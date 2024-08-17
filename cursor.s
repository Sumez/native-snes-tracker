.include "global.inc"
.include "src/snes.inc"
.smart


.segment "BSS"
EditMode: .res 1
CursorX: .res 1
CursorY: .res 1
CursorSize: .res 1

PrevCursorPositionOffset: .res 2


.segment "CODE7"

Init: .export Cursor_Init = Init
	ldx #0
	stx PrevCursorPositionOffset
	stz EditMode
rtl

SetPaletteValues:
sta 0
:
	lda 0
	sta f:TilemapBuffer+01,x
	sta f:TilemapBuffer+03,x
	sta f:TilemapBuffer+05,x
	sta f:TilemapBuffer+07,x
	sta f:TilemapBuffer+09,x
	sta f:TilemapBuffer+11,x
	sta f:TilemapBuffer+13,x
	sta f:TilemapBuffer+15,x

	seta16
	txa
	clc
	adc #$10
	tax
	bit #$39
	seta8
	bne :-
rts

UpdateCursorSpriteAndHighlight:

; TODO: On song view, highlight the whole column instead of the whole row, because the rows aren't tied together

	lda #0
	ldx PrevCursorPositionOffset
	jsr SetPaletteValues

	lda CursorY
	seta16
	and #$00ff
	xba
	lsr
	lsr
	tax
	seta8
	lda #1<<2
	stx PrevCursorPositionOffset
	jsr SetPaletteValues
	
	
	.import RefreshOam
	lda CursorY
	asl
	asl
	asl
	sec
	sbc #3
	sta OamBuffer+1
	sta OamBuffer+1+4

	lda CursorX
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