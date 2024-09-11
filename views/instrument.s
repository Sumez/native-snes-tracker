.include "global.inc"
.include "src/snes.inc"
.smart
.import AddedSamples, SampleDirectory
.import Pattern_GetCurrentNote
.import StopPlayback, PrepareTestPatternPlayback, SwitchToSingleNoteMode, PlaySingleNote

.segment "CODE7"
Name_Existing: .byte "Instrument____-_no_name",$ff
Name_New: .byte "New_instrument",$ff

.segment "BSS"

;SampleIndex = INSTRUMENTS+0
;PitchAdjust	= INSTRUMENTS+1
;Volume		= INSTRUMENTS+3
Envelope 	= INSTRUMENTS+4
FadeOut		= INSTRUMENTS+6
UNUSED		= INSTRUMENTS+7

;Loaded when view loads:
CursorPosition: .res 1
CurrentInstrumentIndex: .res 1
CurrentInstrumentOffset: .res 2
SampleDirectoryIndex: .res 2
PreviousAddedSampleIndex: .res 2
TilemapOffset: .res 2
WasChanged: .res 1
CustomPitchAdjust: .res 2
Volume: .res 2
TestNote: .res 1

.export Instrument_CurrentSampleLoopOffset = CurrentSampleLoopOffset
CurrentSampleSize: .res 2
CurrentSamplePitchAdjust: .res 2
CurrentSampleLoopOffset: .res 2
CurrentSampleDataAddress: .res 3

IsTransfering: .res 1
BufferPreviewSound: .res 1

.segment "ZEROPAGE"
CurrentSampleAddress: .res 3

.segment "CODE6"

.export Instrument_Init = Init
Init:
rtl

.export Instrument_FocusView = FocusView
.import Samples_PrepareSampleEdit
FocusView:
	jsr LoadView
	stz IsTransfering
	stz BufferPreviewSound
	jsl PrepareTestPatternPlayback
	jsl BufferSamplePlayback
	jsl Samples_PrepareSampleEdit
	
	ldy #.loword(Name_New)
	jsl WriteTilemapHeader

	lda PreviousAddedSampleIndex
	cmp #$ff
	beq :+
		ldy #.loword(Name_Existing)
		jsl WriteTilemapHeader
		ldx #$9E+$40
		lda CurrentInstrumentIndex
		jsl WriteTilemapHeaderId
	:

	Bind Input_StartPlayback, NoAction
	Bind Input_CustomHandler, HandleInput
	Bind Input_NavigateIn, NoAction
	Bind Input_NavigateBack, SaveChanges
	Bind OnPlaybackStopped, SwitchToSingleNoteMode

	stz CursorPosition
	jsl ShowCursor_long
rts

.export Instrument_LoadView = LoadView
LoadView:
seta16
	lda z:LoadView_TilemapOffset
	sta TilemapOffset
	
	stz CustomPitchAdjust
seta8
	lda #$50
	sta Volume
	jsr Pattern_GetCurrentNote
	sta TestNote
	
	tya
	cmp #$ff
	beq :++
		sta CurrentInstrumentIndex
		seta16
		tya
		asl
		asl
		asl
		tax
		stx CurrentInstrumentOffset
		lda f:INSTRUMENTS+0,X
		and #$00ff
		sta PreviousAddedSampleIndex
		cmp #$ff
		beq @noSample
			asl
			pha
			lda f:INSTRUMENTS+3,X
			sta Volume
			
			plx
			lda f:AddedSamples,X
			bra :+
		@noSample:
			lda #$ffff
		:
		sta SampleDirectoryIndex
		
		seta8
	:

	stz WasChanged
	jsl WriteBaseTilemapBuffer
	jsl UpdateTilemapBuffer
	
	ldx #4
	jsl LoadGuiMap
rts

WriteBaseTilemapBuffer:

	phb
	lda #^TilemapBuffer
	pha
	plb

	
	ldx #0
	:
		phx
		seta16
		lda f:MenuLines,X
		clc
		adc TilemapOffset
		tay
		lda f:MenuItems,X
		tax
		seta8
		jsr PrintText
		plx
		inx
		inx
		cpx #NumberOfMenuItems*2
	bne :-
	plb
rtl

SelectSampleText: .byte "Select_sample_",$ff
UpdateTilemapBuffer:
.assert ^TilemapBuffer = ^SampleDirectory, error
Tilemap_SampleName = TilemapBuffer+MenuOffset0
Tilemap_TestNote = TilemapBuffer+MenuOffset1

	phb
	lda #^TilemapBuffer
	pha
	plb

	ldx SampleDirectoryIndex
	cpx #$ffff
	bne :+
		lda #^SelectSampleText
		sta z:CurrentSampleAddress+2
		ldx #.loword(SelectSampleText)
		stx z:CurrentSampleAddress

		ldx TilemapOffset
		lda #$1D
		sta Tilemap_SampleName+2,X
		lda #$1F
		sta Tilemap_SampleName+4,X
		
		jsr PrintSampleName ; Print "select sample"

		bra:++
	:
		lda SampleDirectory,X
		sta z:CurrentSampleAddress
		lda SampleDirectory+1,X
		sta z:CurrentSampleAddress+1
		lda SampleDirectory+2,X
		sta z:CurrentSampleAddress+2

		ldx TilemapOffset
		lda #'_'
		sta Tilemap_SampleName+2,X
		sta Tilemap_SampleName+4,X
		
		jsr PrintSampleName
	:
	
	ldx TilemapOffset
	seta16
	lda CustomPitchAdjust
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr ; Divide by 64 (64 pitch adjustments = one semitone)
	seta8
	PrintHexNumber TilemapBuffer+2+MenuOffset3
	lda #'+'
	sta TilemapBuffer+MenuOffset3,X
	lda CustomPitchAdjust+1
	and #63
	PrintHexNumber TilemapBuffer+2+MenuOffset4
	lda #'+'
	sta TilemapBuffer+MenuOffset4,X

	ldy #'_'
	lda Volume
	bpl :+
		ldy #'-'
		eor #$ff
		adc #1
	:
	PrintHexNumber TilemapBuffer+2+MenuOffset2
	tya
	sta TilemapBuffer+0+MenuOffset2,X
	
	lda #0
	xba
	lda TestNote
	ldy #0
	sec
	:
		sbc #12
		bcc :+
			iny
			bra :-
	:
	adc #12
	pha
	lda Octaves,Y
	sta Tilemap_TestNote+4,x
	pla
	tay
	lda Notes,Y
	sta Tilemap_TestNote+0,x
	lda NotesSharp,Y
	sta Tilemap_TestNote+2,x
	
	
	plb
rtl

PrintSampleName:
	ldy #0 ; Loop through sample name
	@nameLoop:
		lda [CurrentSampleAddress],Y
		cmp #$ff
		beq :+
		
		sta Tilemap_SampleName,X
		inx
		inx
		iny
		cpy #14 ; Max name length
	bne @nameLoop
	:
	lda #'_'
	@padEndLoop:
		sta Tilemap_SampleName,X
		inx
		inx
		iny
		cpy #15 ; Max name length+1
	bne @padEndLoop
rts

PrintText:
@loop:
	lda f:Text,X
	cmp #$ff
	bne :+
		rts
	:
	sta TilemapBuffer+10,Y
	lda #$20
	sta TilemapBuffer+11,Y
	iny
	iny
	inx
bra @loop


.segment "CODE7"

.import Samples_IsSampleAdded
BufferSamplePlayback:
	ldx SampleDirectoryIndex
	cpx #$ffff
	bne :+
		rtl
	:
	; TODO: If sample is aleady Added to song/SPC memory, just use that sample instead of transfering anything
	; Should prevent issues with too large samples getting added twice
	;seta16
	;txa
	;jsr Samples_IsSampleAdded
	;beq :+
	;	jsr UseExistingSampleInsteadOfTestSampleOrWhatever
	;	jsr UpdateTestInstrument
	;:
	;seta8

	ldy #0
	@nameLoop:
		lda [CurrentSampleAddress],Y
		iny
		inc a
	bne @nameLoop
	
	seta16
	lda [CurrentSampleAddress],Y
	sta CurrentSampleSize
	iny
	iny
	lda [CurrentSampleAddress],Y
	sta CurrentSamplePitchAdjust
	iny
	iny
	lda [CurrentSampleAddress],Y
	sta CurrentSampleLoopOffset
	iny
	iny
	
	tya
	clc
	adc CurrentSampleAddress
	tax
	seta8
	lda #0
	adc CurrentSampleAddress+2
	stx CurrentSampleDataAddress
	sta CurrentSampleDataAddress+2
	
	; !!!!!!!!!!!!!!!!!! IMPORTANT: TODO: Either buffer transfer until current playback stopped, or find a way to seamlessly change sample ref during playback !!!!!!
	ldy CurrentSampleSize
	jsr BeginFragmentedTransfer

	jsr UpdateTestInstrument
	lda #1
	sta IsTransfering
rtl

.export Instrument_OnSampleTransferDone = OnSampleTransferDone
OnSampleTransferDone:
	stz IsTransfering
;	lda BufferPreviewSound
;	beq :+
;		jmp PreviewInstrument
;	:
rts

UpdateTestInstrument:
	ldx SampleDirectoryIndex
	inx
	bne :+
		rts
	:
	seta16
	lda CurrentSamplePitchAdjust
	clc
	adc CustomPitchAdjust
	tax
	lda Volume
	and #$00ff
	tay
	seta8
	lda CurrentInstrumentIndex
jmp PointInstrumentToTestSample

NoAction: rts
ShowCursor_long: jsr ShowCursor
rtl
ShowCursor:
	ldx TilemapOffset
	stx CursorOffset

	lda #0
	xba
	lda CursorPosition
	tax
	
	lda #24
	sta HighlightLength

	stz CursorX
	
	lda CursorYOffsets,X
	sta CursorY

	;lda CursorSizes,X
	lda #1
	sta CursorSize

	lda #0
jmp UpdateCursorSpriteAndHighlight


Text:
Text_Empty = * - Text
.byte $ff
Text_TestInstrument = * - Text
.byte "Test_note",$ff
Text_PitchAdjust = * - Text
.byte "Shift_semitone",$ff
Text_FineTune = * - Text
.byte "Fine-tune_pitch",$ff
Text_Volume = * - Text
.byte "Volume",$ff


NumberOfMenuItems = 5

MenuRow0 = 0
MenuRow1 = 3
MenuRow2 = 6
MenuRow3 = 8
MenuRow4 = 9

MenuOffset0 = MenuRow0*$40
MenuOffset1 = MenuRow1*$40
MenuOffset2 = MenuRow2*$40
MenuOffset3 = MenuRow3*$40
MenuOffset4 = MenuRow4*$40

MenuItems:
.addr Text_Empty, Text_TestInstrument, Text_Volume, Text_PitchAdjust, Text_FineTune
MenuLines:
.addr MenuOffset0,MenuOffset1,MenuOffset2,MenuOffset3,MenuOffset4
CursorYOffsets:
.byte MenuRow0,MenuRow1,MenuRow2,MenuRow3,MenuRow4

PreviewInstrument:
	stz BufferPreviewSound
	ldx SampleDirectoryIndex
	inx
	bne :+ ; If current sample = $ffff, don't do anything
		rts
	:
	lda IsPlaying
	beq :+ ; If currently playing, just don't do anything
		rts
	:
	lda IsTransfering
	beq :+ ; If transfering sample data, buffer until done
		sta BufferPreviewSound
		rts
	:
	jsr CutCurrentlyPlayingNote
	lda CurrentInstrumentIndex
	xba
	lda TestNote
jmp PlaySingleNote
CutCurrentlyPlayingNote:
	lda IsPlaying
	bne :+ ; If currently playing pattern, chain or song, just don't do anything
		jsr StopPlayback
	:
rts

HandleInput:
	jsr @handleInput
	
	lda IsTransfering
	bne :+
	lda BufferPreviewSound
	beq :+
		jmp PreviewInstrument
	:
rts

@handleInput:

	lda ButtonStates+1
	bit #>KEY_Y
	beq :+
		lda EditMode
		bne @EditMode
			jsr PreviewInstrument
			lda #1
			bra :++
	:
		lda EditMode
		beq @Navigation
			jsr CutCurrentlyPlayingNote
			lda #0
	:
	sta EditMode
	jsr ShowCursor
	beq @Navigation

@EditMode:

	lda CursorPosition
	bne @afterSample
		; Select sample
		lda ButtonPushed+1
		bit #>KEY_LEFT
		beq :+
			jmp DecreaseSampleOffset
		:
		bit #>KEY_RIGHT
		beq :+
			jmp IncreaseSampleOffset
		:
		rts
	@afterSample:
	cmp #1
	bne @afterTestNote
		; Alter test note
		lda ButtonPushed+1
		bit #>KEY_LEFT
		beq :+
			lda #$ff
			jmp ChangeTestNote
		:
		bit #>KEY_RIGHT
		beq :+
			lda #$01
			jmp ChangeTestNote
		:
		bit #>KEY_DOWN
		beq :+
			lda #($100-12)
			jmp ChangeTestNote
		:
		bit #>KEY_UP
		beq :+
			lda #12
			jmp ChangeTestNote
		:
		rts
	@afterTestNote:
	cmp #2
	bne @afterVolume
		; Select sample
		lda ButtonPushed+1
		bit #>KEY_LEFT
		beq :+
			lda #$ff
			jmp ChangeVolume
		:
		bit #>KEY_RIGHT
		beq :+
			lda #1
			jmp ChangeVolume
		:
		bit #>KEY_UP
		beq :+
			lda #$10
			jmp ChangeVolume
		:
		bit #>KEY_DOWN
		beq :+
			lda #$f0
			jmp ChangeVolume
		:
		rts
	@afterVolume:
rts
@Navigation:

	lda ButtonPushed+1
	bit #>KEY_DOWN
	beq :+
		jmp MoveCursorDown
	:
	bit #>KEY_UP
	beq :+
		jmp MoveCursorUp
	:
rts

MoveCursorDown:
	lda CursorPosition
	inc
	cmp #NumberOfMenuItems
	bcc :++
		lda ButtonPushed_Actual+1
		bit #>KEY_DOWN
		bne :+
			rts
		:
		lda #0
	:
	sta CursorPosition
jmp ShowCursor

MoveCursorUp:
	lda CursorPosition
	bne :++
		lda ButtonPushed_Actual+1
		bit #>KEY_UP
		bne :+
			rts
		:
		lda #NumberOfMenuItems
		sta CursorPosition
	:
	dec CursorPosition
jmp ShowCursor

ChangeTestNote:
	clc
	adc TestNote
	bmi :+
	cmp #12*9
	bcs :+
		sta TestNote
		jsl UpdateTilemapBuffer
	:
jmp PreviewInstrument

ChangeVolume:
	clc
	adc Volume
	bvc @noOverflow
		; Overflow (value changed from positive to negative or vice versa)
		bmi :+
			; Clamp negative value at $80
			lda #$80
			bra @noOverflow
		:
		; And positive at $7F
		lda #$7F
	@noOverflow:
	sta Volume

	lda #1
	sta WasChanged

	jsl UpdateTilemapBuffer
	jsr UpdateTestInstrument
jmp PreviewInstrument

DecreaseSampleOffset: .a8
	seta16
	lda SampleDirectoryIndex
	bne :+
		@block:
		seta8
		jmp PlayMosaic
	: .a16
	cmp #$ffff
	beq @block
	sec
	sbc #8
	tax
	bra SetNewSampleDirectoryIndex
IncreaseSampleOffset:
	seta16
	lda SampleDirectoryIndex
	cmp #$ffff
	bne :+
		lda #0
		bra :++
	:
	clc
	adc #8
	:
	seta8
	tax
	lda f:SampleDirectory+2,X
	cmp #$ff
	bne :+
		jmp PlayMosaic
	:
	seta16
SetNewSampleDirectoryIndex:
	stx SampleDirectoryIndex

	seta8
	lda #1
	sta WasChanged
	
	jsl UpdateTilemapBuffer
	jsl BufferSamplePlayback
	jsr PreviewInstrument
rts

.import Samples_TryAddSample, Samples_RemoveAddedSample, Samples_RefreshSamplesInSpc
SaveChanges:

	seta16
	lda SampleDirectoryIndex
	cmp #$ffff
	bne :+
		seta8
		jmp NavigateBack
		.a16
	:
	jsr Samples_TryAddSample
	seta8
	
	ldx CurrentInstrumentOffset
	lsr
	sta f:INSTRUMENTS+0,X
	lda PreviousAddedSampleIndex
	jsr RemoveAddedSampleIfUnused
	
	ldx CurrentInstrumentOffset
	lda Volume
	sta f:INSTRUMENTS+3,X
	
	jsr Samples_RefreshSamplesInSpc ; Refreshes SPC only if new sample was added
	
	NavigateBack:
	; Return to pattern view  / chain view
	lda #1
	ldy #$ff ; Reuse already loaded phrase / chain
jmp NavigateToScreen

RemoveAddedSampleIfUnused:
	ldx #0
	@loop:
		cmp f:INSTRUMENTS+0,X
		beq @found
		inx
		inx
		inx
		inx
		inx
		inx
		inx
		inx
		cpx #MAX_INSTRUMENTS*8
	bne @loop
	@notFound: ; No instrument found using the sample. Remove it from AddedSamples
@removedSample = 0
	sta @removedSample
	jsr Samples_RemoveAddedSample
	ldx #0
	@loop2:
		lda f:INSTRUMENTS+0,X
		cmp #$ff
		beq :+
			cmp @removedSample
			bcc :+
				dec
				sta f:INSTRUMENTS+0,X
		:
		inx
		inx
		inx
		inx
		inx
		inx
		inx
		inx
		cpx #MAX_INSTRUMENTS*8
	bne @loop2
	
	@found: ; An instrument uses the sample. Do nothing
rts