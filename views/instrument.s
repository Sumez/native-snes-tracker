.include "global.inc"
.include "src/snes.inc"
.smart
.import AddedSamples, SampleDirectory

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
PitchAdjust: .res 2
CustomPitchAdjust: .res 2
Volume: .res 1

.segment "CODE6"

.export Instrument_Init = Init
Init:
rtl

.export Instrument_FocusView = FocusView
.import Samples_PrepareSampleEdit
FocusView:
	jsr LoadView
	jsl Samples_PrepareSampleEdit
	
	ldy #.loword(Name_New)
	jsl WriteTilemapHeader

	lda PreviousAddedSampleIndex
	cmp #$ff
	beq :+
		ldy #.loword(Name_Existing)
		jsl WriteTilemapHeader
		ldx #$9E
		lda CurrentInstrumentIndex
		jsl WriteTilemapHeaderId
	:

	Bind Input_StartPlayback, NoAction
	Bind Input_CustomHandler, HandleInput
	Bind Input_NavigateIn, SaveChanges
	Bind Input_NavigateBack, NavigateBack
	Bind OnPlaybackStopped, NoAction

	stz CursorPosition
	jsl ShowCursor_long
rts

.export Instrument_LoadView = LoadView
LoadView:
seta16
	lda z:LoadView_TilemapOffset
	sta TilemapOffset
	
	stz CustomPitchAdjust
	lda #$20
	sta Volume
seta8
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
rts

WriteBaseTilemapBuffer:

	phb
	lda #^TilemapBuffer
	pha
	plb

	seta16
	lda TilemapOffset
	clc
	adc #$8A
	tay
	seta8
	ldx #Text_PitchAdjust
	jsr PrintText

	seta16
	lda TilemapOffset
	clc
	adc #$10A
	tay
	seta8
	ldx #Text_Volume
	jsr PrintText
	
	seta16
	lda TilemapOffset
	clc
	adc #$206
	tay
	seta8
	ldx #Text_SaveChanges
	jsr PrintText
	
	plb
rtl

SelectSampleText: .byte "Select_sample_",$ff
UpdateTilemapBuffer:
.assert ^TilemapBuffer = ^SampleDirectory, error
@sampleData = 0

	phb
	lda #^TilemapBuffer
	pha
	plb

	ldx SampleDirectoryIndex
	cpx #$ffff
	bne :+
		lda #^SelectSampleText
		sta z:@sampleData+2
		ldx #.loword(SelectSampleText)
		stx z:@sampleData

		ldx TilemapOffset
		lda #$1D
		sta TilemapBuffer+2,X
		lda #$1F
		sta TilemapBuffer+4,X

		bra:++
	:
		lda SampleDirectory,X
		sta @sampleData
		lda SampleDirectory+1,X
		sta @sampleData+1
		lda SampleDirectory+2,X
		sta @sampleData+2

		ldx TilemapOffset
		lda #'_'
		sta TilemapBuffer+2,X
		sta TilemapBuffer+4,X
	:
	ldy #0 ; Loop through sample name
	@nameLoop:
		lda [@sampleData],Y
		cmp #$ff
		beq :+
		
		sta TilemapBuffer+10,X
		inx
		inx
		iny
		cpy #14 ; Max name length
	bne @nameLoop
	:
	lda #'_'
	:	sta TilemapBuffer+10,X
		inx
		inx
		iny
		cpy #15 ; Max name length+1
	bne :-
	
	ldx TilemapOffset
	lda CustomPitchAdjust+0
	PrintHexNumber TilemapBuffer+2+$80
	ldy #'_'
	lda Volume
	bpl :+
		ldy #'-'
		eor #$ff
		adc #1
	:
	PrintHexNumber TilemapBuffer+2+$100
	tya
	sta TilemapBuffer+0+$100,X
	
	
	; "Save changes" palette:
	ldx #$00
	lda WasChanged
	bne :+
		;ldx #2<<2 ; TODO: Dim line palette
	:
	txa
	ldx #16
	ldy TilemapOffset
	:
		sta TilemapBuffer+$207,Y
		iny
		iny
		dex
	bne :-

	
	plb
rtl

PrintText:
@loop:
	lda f:Text,X
	cmp #$ff
	bne :+
		rts
	:
	sta TilemapBuffer,Y
	iny
	iny
	inx
bra @loop

Text:
Text_PitchAdjust = * - Text
.byte "Pitch_adjust",$ff
Text_Volume = * - Text
.byte "Volume",$ff
Text_SaveChanges = * - Text
.byte "OK__Save_changes",$ff


.segment "CODE7"

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
	
	lda #20
	sta HighlightLength

	ldy #1
	cpx #3
	bne :+
		ldy #3
	:
	tya
	sta CursorX
	
	lda CursorYOffsets,X
	sta CursorY

	lda CursorSizes,X
	sta CursorSize

	lda #0
jmp UpdateCursorSpriteAndHighlight
CursorYOffsets:
.byte 0,2,4,8
CursorSizes:
.byte 0,0,0,0

HandleInput:
	jsr @handleInput
jmp ShowCursor

@handleInput:
	lda ButtonStates+1
	bit #>KEY_Y
	beq :+
		lda #1
		bra :++
	:
		lda #0
	:
	sta EditMode
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

	cmp #3
	bne @afterSave

		lda ButtonPushed+1
		bit #>KEY_Y
		beq @afterSave
			jmp SaveChanges

	@afterSave:
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

jmp ShowCursor

MoveCursorDown:
	lda CursorPosition
	inc
	cmp #4
	bcs :+
		sta CursorPosition
	:
jmp ShowCursor

MoveCursorUp:
	lda CursorPosition
	beq :+
		dec CursorPosition
	:
jmp ShowCursor

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
rts

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
@samplePointer = 0
	stx SampleDirectoryIndex
	lda f:SampleDirectory,X
	sta @samplePointer
	seta8
	lda f:SampleDirectory+2,X
	sta @samplePointer+2
	ldy #0
	@nameLoop:
		lda [@samplePointer],Y
		iny
		inc
	bne @nameLoop
	iny
	iny
	seta16
	lda [@samplePointer],Y
	sta PitchAdjust	
	seta8
	lda #1
	sta WasChanged
	jsl UpdateTilemapBuffer
rts

.import Samples_TryAddSample, Samples_RemoveAddedSample, Samples_RefreshSamplesInSpc
SaveChanges:

	seta16
	lda SampleDirectoryIndex
	cmp #$ffff
	bne :+
		seta8
		jmp PlayMosaic
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