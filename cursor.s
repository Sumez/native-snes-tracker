.include "global.inc"
.include "src/snes.inc"
.smart


.segment "BSS"
EditMode: .res 1
HighlightLength: .res 1
CursorX: .res 1
CursorY: .res 1
CursorSize: .res 1
CursorOffset: .res 2
PrevCursorPositionOffset: .res 4


.segment "CODE7"

Init: .export Cursor_Init = Init
	ldx #0
	stx PrevCursorPositionOffset
	stz EditMode
rtl

SetPaletteValues:
	sta 0
	lda HighlightLength
	pha
	lda 0
	:
		sta f:TilemapBuffer+01,x
		inx
		inx
		dec HighlightLength
	bne :-
	pla
	sta HighlightLength
rts

UpdateCursorSpriteAndHighlight:

; TODO: On song view, highlight the whole column instead of the whole row, because the rows aren't tied together

	seta16
	and #2 ; Only cursor ID 0 or 2 allowed so far
	tay
	seta8
	lda #0
	ldx PrevCursorPositionOffset,Y
	jsr SetPaletteValues

	lda CursorY
	seta16
	and #$00ff
	xba
	lsr
	lsr
	clc
	adc CursorOffset
	sta CursorOffset
	sta PrevCursorPositionOffset,Y
	tax
	seta8
	lda #1<<2
	jsr SetPaletteValues
	
	
	.import RefreshOam
	seta16
	lda CursorOffset
	lsr
	lsr
	lsr
	seta8
	and #$F8
	sec
	sbc #3
	sta OamBuffer+1
	sta OamBuffer+1+4

	lda CursorOffset
	lsr
	clc
	adc CursorX
	asl
	asl
	asl
	sec
	sbc #3
	sta OamBuffer+0
	tax
	lda CursorSize
	beq :+
		; Wide cursor. Add 16 pixels
		txa
		clc
		adc #16
		bra :++
	:
		; Small cursor. Add 8 pixels
		txa
		clc
		adc #8
	:
	sta OamBuffer+0+4

	lda #2 ; Sprite tile index
	sta OamBuffer+2
	lda #4 ; Sprite tile index
	sta OamBuffer+2+4
	ldy #%00000000
	lda EditMode
	beq :+
		ldy #(%00110000|2) ; High priority + light palette
	:
	tya
	sta OamBuffer+3
	sta OamBuffer+3+4
	lda OamBuffer+$200
	and #%11110000
	sta OamBuffer+$200
	
	lda #1
	sta RefreshOam
	
rts