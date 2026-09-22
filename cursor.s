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
	lda #(4<<2)|$20
	ldx PrevCursorPositionOffset,Y
	jsr SetPaletteValues ; Reset highlight from previous position

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
	lda #(5<<2)|$20
	jsr SetPaletteValues ; Set highlight for current position
	
	
	.import RefreshOam
	seta16
	lda CursorOffset
	lsr
	lsr
	lsr
	seta8
	and #$F8
	sec
	sbc #5
	sta OamBuffer+1
	sta OamBuffer+1+4 ; Y coords. Always the same
	clc
	adc #8
	sta OamBuffer+1+8 ; If 8 pixel end bracket, use a third 8x8 sprite to draw that below

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
	clc
	adc #16
	sta OamBuffer+0+4
	sta OamBuffer+0+8

	lda CursorSize
	cmp #1
	beq :++
		lda #$02 ; Sprite tile index
		sta OamBuffer+2
		lda CursorSize
		beq :+
			; Largest cursor
			lda #$04 ; Sprite tile index
			sta OamBuffer+2+4
			bra :+++
		:
		lda #$05 ; Sprite tile index
		sta OamBuffer+2+4
		lda #$15 ; Sprite tile index
		sta OamBuffer+2+8
		bra :++
	:
		; Smalles cursor
		lda #10 ; Sprite tile index
		sta OamBuffer+2
	:
	ldy #(%00110000)
	lda EditMode
	beq :+
		ldy #(%00110000|2) ; High priority + light palette
	:
	tya
	sta OamBuffer+3
	sta OamBuffer+3+4
	sta OamBuffer+3+8

	lda CursorSize
	beq :+
		lda #%00001010 ; Large sprite 1 and 2
		bra :++
	:
		lda #%00000010 ; Large sprite 1, small sprite 2 and 3
	:
	sta OamBuffer+$200
	
	lda CursorSize
	beq :++
		cmp #1
		bne :+
			; Smallest. Only one sprite
			lda #224
			sta OamBuffer+1+4
		:
			; Biggest. Two sprites
			lda #224
			sta OamBuffer+1+8
	:
	
	lda #1
	sta RefreshOam
	
rts


MarkSelectionArea:
	lda #Backdrop_MARKED
	jmp fillSelectionArea
ClearSelectionArea:
	lda #Backdrop_CLEAR
fillSelectionArea:

	pha ; Store A (tile index) until later

	stz 4+1 ; Ensure 8bit value before loading it into 16bit Y
	clc
	asl 4 ; Double width because each tile is 2 bytes
	
	lda #^BackdropTilemapBuffer
	sta 9 ; 7,8,9 = Indirect address to tilemap buffer
	seta16
	lda 0 ; X coord
	and #$ff
	asl
	sta 7
	lda 2 ; Y coord
	and #$ff
	xba
	lsr
	lsr ; Row start coordinate
	adc #(.loword(BackdropTilemapBuffer) - 2) ; Subtract 2 because Y register rabges from #2-(width*2)
	adc CursorOffset
	adc 7
	
	sta 7 ; 7,8,9 = Indirect address to tilemap buffer
	seta8
	
	pla	; Recover tile index
	@loopY:
		ldy 4 ; width
		@loopX:
			sta [7],Y
			dey
			dey
		bne @loopX
		pha
			; Add 64 to pointer (next row)
			seta16
			lda 7
			clc
			adc #64
			sta 7
			seta8
		pla
		dec 6 ; height
	bne @loopY
rts