.include "global.inc"
.include "src/snes.inc"

.export LoadSamples, SampleDirectory, AddedSamples
.import StopPlayback
.import SAMPLE_DATA

.segment "CODE7"
Name: .byte "Samples",$ff

.segment "BSS7E"
	
	; Code-friendly reference to sample ROM data, global for tracker, shared between songs
	; 8 bytes: PPPxSSAA, PPP is a 24bit pointer to sample start, SS is ~size in kilobytes, AA is the pitch adjustment
	SampleDirectory: .res 8*1000+3

.segment "ZEROPAGE"

	DirectoryIndexToMatch: .res 2

.segment "BSS"

	CursorRow: .res 2
	AddedSamples: .res (2*64)+2 ; Ordered list of 16bit offsets into SampleDirectory. Unique to current song, and loaded by checking Sample Name references
	NumberOfSamples: .res 2 ; That's a lotta samples :O :O :O
	SamplesUpdated: .res 2
	TotalSampleDataSizeInSpc: .res 2
	TilemapOffset: .res 2
	
.segment "CODE6"

.export Samples_Init = Init
Init:
	stz IsPlaying ; Prevents trying to stop non-existant playback before transfering samples
	jsl PrepareSampleEdit
	jsl LoadSamplesIntoSpcDuringInit
rtl

.export Samples_PrepareSampleEdit = PrepareSampleEdit
PrepareSampleEdit:
	jsl LoadSamples
	ldx #$ffff
	stx AddedSamples
	jsl LoadSongData

	ldx #0
	stx SamplesUpdated
rtl

.export Samples_FocusView = FocusView
FocusView:

	jsr LoadView

	Bind Input_CustomHandler, HandleInput
	Bind Input_NavigateIn, NoAction
	Bind Input_NavigateBack, NavigateBack
	
	ldy #.loword(Name)
	jsl WriteTilemapHeader

	ldx #0
	stx CursorRow
	stx SamplesUpdated
	jsl ShowCursor_long
rts

.export Samples_LoadView = LoadView
LoadView:
	ldx z:LoadView_TilemapOffset
	stx TilemapOffset

	jsl PrepareSampleEdit ; Might as well load samples again in case someone loaded a savestate after patching rom with new samples
		
	jsl WriteTilemapBuffer
	
	ldx #0
	jsl LoadGuiMap
rts

LoadSamples:
@samplePointer = 0 ; 3 bytes!
@titleLength = 3

	ldx #.loword(SAMPLE_DATA)
	lda #^SAMPLE_DATA
	stx @samplePointer+0
	sta @samplePointer+2

	ldx #0
	stx NumberOfSamples
	@sampleLoop:
		ldy #0
		@nameLoop:
			lda [@samplePointer],Y
			iny	
			cmp #$ff
		bne @nameLoop
		seta16
		lda [@samplePointer],Y ; read sample size
		beq @end ; Size = 0 - no more samples
		
		sty @titleLength
		inc NumberOfSamples
		pha
		
		; Write reference to sample in directory
		clc
		adc #512
		bcc :+
			lda #$ffff
		:
		xba
		and #$00ff
		lsr
		lsr ; Divide by 1024
		sta f:SampleDirectory+4,X
		
		iny
		iny
		lda [@samplePointer],Y ; read pitch adjust
		sta f:SampleDirectory+6,X
		
		lda @samplePointer
		sta f:SampleDirectory,X
		lda @samplePointer+2
		and #$00FF
		sta f:SampleDirectory+2,X
		inx
		inx
		inx
		inx
		inx
		inx
		inx
		inx
		
		; Get pointer for next sample
		pla
		clc
		adc #6 ; Add the 2 bytes for sample size, 2 for pitch adjust, and 2 for loop offset
		adc @titleLength ; don't expect any carry at this point if max sample size is well below $10000
		adc @samplePointer
		sta @samplePointer
		seta8
		bcc :+
			inc @samplePointer+2 ; If carry, increase bank pointer by 1
		:
	bra @sampleLoop		
	
	@end:
	seta8
	lda #$ff
	sta f:SampleDirectory+2,x ; Write empty ref in last slot

	
rtl

LoadSongData:
seta16
	ldx #0
	ldy #0
	:
		lda f:SAMPLES,X
		; TODO: Name-check if not $FFFF
		sta AddedSamples,Y
		iny
		iny
		txa
		clc
		adc #16
		tax
		cpy #(2*64)
	bne :-
	lda #$ffff ; Always $ffff after end of list
	sta AddedSamples,Y
seta8	
rtl

.a16
UpdateSongData:
	ldx #0
	ldy #0
	:
		lda AddedSamples,Y
		sta f:SAMPLES,X
		cmp #$ffff
		beq @end
		; TODO: Save first 14 bytes of name
		iny
		iny
		txa
		clc
		adc #16
		tax
		cpy #128
	bne :-
	@end:
rtl
.a8

WriteTilemapBuffer:
@samplePointer = 0
@tilemapOffset = 3
@directoryOffset = 5
@tilemap = TilemapBuffer+$100 ; Add a $100 buffer so we can target "previous row" without wrapping banks

seta16
	lda TilemapOffset
	sec
	sbc #$100
	clc
	adc #$10
	sta @tilemapOffset
seta8

	phb
	lda #^TilemapBuffer
	pha
	plb

	ldx #0
	stx @directoryOffset
	@loop:
		ldx @directoryOffset
		lda f:SampleDirectory+2,X
		cmp #$ff
		bne :+
			jmp @endLoop
		:
		sta z:@samplePointer+2
		seta16
		lda f:SampleDirectory+0,X
		sta z:@samplePointer+0
		lda f:SampleDirectory+4,X
		stx z:DirectoryIndexToMatch
		seta8
		pha ; Store sample size
		seta16
		txa
		clc
		adc #8
		sta @directoryOffset
		
		jsr FindAddedSample ; Detect if the sample is already added to song, and store index on stack
		lsr
		pha
		
		lda @tilemapOffset
		tax
		clc
		adc #$40
		sta @tilemapOffset

@sizeTextOffset = @tilemap+26
@selectedMarkOffset = @tilemap-6

		pla
		cmp #$7fff
		seta8
		bne :+
			lda #'_'
			sta @selectedMarkOffset+0,X
			sta @selectedMarkOffset+2,X
			bra :++
		:
			PrintHexNumber @selectedMarkOffset
		:
		ldy #0 ; Loop through sample name
		@nameLoop:
			lda [@samplePointer],Y
			cmp #$ff
			beq :+
			
			sta @tilemap,X
			inx
			inx
			iny
			cpy #13 ; Max name length
		bne @nameLoop
		:
		ldx @tilemapOffset
		pla
		PrintHexNumberNoPadding @sizeTextOffset-$40
		lda #'k'
		sta @sizeTextOffset-$40+4,X
		
	jmp @loop
		
	
	@endLoop:
	
	plb
rtl

.a16
FindAddedSample:
	ldx #0 ; Check if sample is already added, and print its index
	:
		lda f:AddedSamples,X
		cmp #$ffff
		beq @noMatch
		cmp z:DirectoryIndexToMatch
		beq @match
		inx
		inx
	bne :-
	@match:
		txa ; Sample's index into AddedSamples
		bra :+
	@noMatch:
		;lda #$ffff ; $ffff is already in A. Indicates nothing found
	:
rts
FindAddedSample_long: jsr FindAddedSample
rtl
.a8

.segment "CODE7"
NoAction: rts
NavigateBack:
	jsr RefreshSamplesInSpc
jmp NavigateToPreviousScreen

.export Samples_RefreshSamplesInSpc = RefreshSamplesInSpc
RefreshSamplesInSpc:
	lda SamplesUpdated
	beq :+
		; TODO: Display friendly message on screen
		jsr LoadSamplesIntoSpc
	:
rts

ShowCursor_long: jsr ShowCursor
rtl
ShowCursor:
	ldx TilemapOffset
	stx CursorOffset
	lda #24
	sta HighlightLength
	
	lda #5
	sta CursorX
	lda CursorRow
	sta CursorY
	stz CursorSize
	lda #0
jmp UpdateCursorSpriteAndHighlight

HandleInput:

	lda ButtonStates+1
	bit #>KEY_Y
	beq :+
		lda #1
		bra :++
	:
		lda #0
	:
	sta EditMode

	lda ButtonPushed+1
	bit #>KEY_DOWN
	beq :+
		jmp MoveCursorDown
	:
	bit #>KEY_UP
	beq :+
		jmp MoveCursorUp
	:
	bit #>KEY_Y
	beq :+
		;jsr ToggleAddedSample
	:

jmp ShowCursor

MoveCursorDown:
	seta16
	lda CursorRow
	inc
	cmp NumberOfSamples
	bcs :+
		sta CursorRow
	:
	seta8
jmp ShowCursor

MoveCursorUp:
	seta16
	lda CursorRow
	beq :+
		dec CursorRow
	:
	seta8
jmp ShowCursor

.export Samples_RemoveAddedSample = RemoveAddedSample
RemoveAddedSample:
	seta16
	and #$00ff
	asl
	tax
	jsr removeSampleFromIndex
	seta8
rts

.export Samples_IsSampleAdded = IsSampleAdded
IsSampleAdded: .a16
	sta DirectoryIndexToMatch
	jsl FindAddedSample_long
	inc a
rts
.a8
.export Samples_TryAddSample = TryAddSample
TryAddSample: .a16
	; Gets AddedSample index if exists, otherwise adds to sample list
	sta DirectoryIndexToMatch
	jsl FindAddedSample_long
	cmp #$ffff
	bne :+
		jmp tryAddSampleToIndex
	:
rts
.a8

ToggleAddedSample:
	seta16
	jsr GetHighlightedSample
	sta DirectoryIndexToMatch
	jsl FindAddedSample_long
	cmp #$ffff
	beq :+
		jsr removeSampleFromIndex
		bra @end
	:
		jsr tryAddSampleToIndex
	@end:
	seta8
	jsl WriteTilemapBuffer
rts
.a16
removeSampleFromIndex:
	lda #$ffff
	sta AddedSamples,X
	:
		;loop through and move every subsequent non-empty entry back 1 to enable lazy look through the list (searching until $ffff
		inx
		inx
		cpx #(2*64)
		beq @endOfList
		
			lda AddedSamples,X
			cmp #$ffff
			beq @endOfList
			
			sta AddedSamples-2,X
	bra :-
	@endOfList:
	lda #$ffff
	sta AddedSamples-2,X
jmp MarkSampleListUpdated

tryAddSampleToIndex:
	cpx #(64*2) ; Enforce maximum number of added sample
	bcc :+; Also: Add up sizes of all added samples to check enforce a max sample size limit
		seta8
		jsr PlayMosaic
		seta16
		lda #$ffff
		rts
	:
	lda DirectoryIndexToMatch
	sta AddedSamples,X
	phx
	jsr MarkSampleListUpdated
	pla
rts


MarkSampleListUpdated:
	ldx #1
	stx SamplesUpdated
	jsl UpdateSongData
rts

.a16
GetHighlightedSample:
	lda CursorRow
	asl
	asl
	asl ;x8
rts
.a8

.importzp BrewsicTransferDestination
.import BrewsicTransfer

LoadSamplesIntoSpc:
	jsr CopySamplesToSpcBuffer
	jsr PrepareSpcTransfer
	
	; Disable NMI interrupts to avoid transfer being stalled by NMI.
	; Will cause issues because the SPC code always assumes the CPU code is faster, and doesn't wait for data
	stz PPUNMI
	jsr BrewsicTransfer
	lda #VBLANK_NMI|AUTOREAD
	sta PPUNMI
rts
LoadSamplesIntoSpcDuringInit:
	; Same thing but without modifying NMI
	jsr CopySamplesToSpcBuffer
	jsr PrepareSpcTransfer
	jsr BrewsicTransfer
rtl

PrepareSpcTransfer:
	ldy TotalSampleDataSizeInSpc

	ldx #(SampleDirectoryAddress-1)
	stx z:BrewsicTransferDestination ; Destination in SPC memory

	lda #^CompiledPattern
	ldx #.loword(CompiledPattern)
rts

CopySamplesToSpcBuffer:
.import SampleDirectoryAddress
@CurrentSampleDirectoryOffset = 0
@CurrentSampleDataOffset = 2
@CurrentSampleStartAddress = 4
@CurrentTrackDirectoryOffset = 6
@CurrentAddedSampleIndex = 8
@SampleSizeCounter = 10
@CurrentSamplePointer = 12 ; 3 bytes!!

	lda IsPlaying
	beq :+
		jsr StopPlayback
	:
	
	seta16
	lda #$ffff
	sta z:DirectoryIndexToMatch
	jsl FindAddedSample_long ; By searching for $ffff, X will give us half the length of the SPC's sample directory (added samples are 2 bytes per sample, sample dir is 4)
	txa
	ldx #0
	stx z:@CurrentSampleDirectoryOffset
	inc a
	inc a ; Add one empty sample reference in the end of the directory to use for test samples
	sta f:CompiledPattern,X ; First byte is the length of the sample directory (in # of samples)

	asl ; Directory size (in bytes)
	sta z:@CurrentTrackDirectoryOffset
	clc
	adc #2+1 ; Track directory is always 2 bytes, because we always have only 1 available track to play + 1 byte for the initial sampledir length
	sta z:@CurrentSampleDataOffset
	adc #SampleDirectoryAddress-1
	sta z:@CurrentSampleStartAddress ; Address in SPC memory to be written into the directory
	
	inc z:@CurrentTrackDirectoryOffset ; Account for the one byte before the sample directory, telling the size of it
	
	seta8
	lda #^CompiledPattern
	phb
	pha
	plb
	seta16
	
	; Empty sample at the start
	ldx z:@CurrentSampleDirectoryOffset
	inx ; Account for the one byte before the sample directory, telling the size of it
	stz CompiledPattern,X
	inx
	inx
	stz CompiledPattern,X
	inx
	inx
	stx z:@CurrentSampleDirectoryOffset
	
	ldx #0
	stx @CurrentAddedSampleIndex
	@sampleLoop:
		ldx @CurrentAddedSampleIndex
		lda f:AddedSamples,X
		cmp #$ffff
		beq @endSampleLoop
		inx
		inx
		stx @CurrentAddedSampleIndex
		
		; Find sample data source from global sample directory
		tax
		lda	f:SampleDirectory,X
		sta z:@CurrentSamplePointer
		lda	f:SampleDirectory+1,X
		sta z:@CurrentSamplePointer+1
		
		; Read sample data from Far-address pointer reference
		seta8
		ldy #0 ; Skip sample name to find where to read the size and loop offset for sample
		: lda [@CurrentSamplePointer],Y
		  iny
		  cmp #$ff
		bne :-
		seta16
		lda [@CurrentSamplePointer],Y
		iny
		iny
		iny ; INY twice more to skip pitch adjust value. Only need that for instruments
		iny
		sta @SampleSizeCounter

		ldx @CurrentSampleDirectoryOffset
		lda @CurrentSampleStartAddress
		sta CompiledPattern,X
		inx
		inx
		clc
		adc [@CurrentSamplePointer],Y ; Add loop offset
		iny
		iny
		sta CompiledPattern,X
		inx
		inx
		stx @CurrentSampleDirectoryOffset
		
		ldx @CurrentSampleDataOffset
		@sampleCopyLoop:
			; We are using a 16bit operation to copy samples that aren't necessary an even number of bytes, so be aware of one too many byte overwritten in the end
			lda [@CurrentSamplePointer],Y
			sta CompiledPattern,X
			iny
			iny
			inx
			dec z:@SampleSizeCounter
			beq @endCopyLoop
			inx
			dec z:@SampleSizeCounter
		bne @sampleCopyLoop
		@endCopyLoop:
		stx z:@CurrentSampleDataOffset
		txa
		clc
		adc #SampleDirectoryAddress-1
		sta z:@CurrentSampleStartAddress
	
	bra @sampleLoop
	@endSampleLoop:
	
	ldx @CurrentTrackDirectoryOffset

	lda z:@CurrentSampleStartAddress
	sta CompiledPattern,X			; After sample directory, write track address (2 byte track directory)
	sta f:TrackDataAddressInSpc
	
	lda z:@CurrentSampleDataOffset
	sta f:TotalSampleDataSizeInSpc
	seta8
	
	plb
rts
