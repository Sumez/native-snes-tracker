.include "global.inc"
.include "snes.inc"

.segment "BSS"
.export RefreshOam
RefreshOam: .res 1

.segment "ZEROPAGE"

.exportzp SpritePointer, TopPointer
SpritePointer: .res 2
TopMask: .res 1
TopMaskI: .res 1
TopPointer: .res 2

.segment "CODE7"

ResetSprites:
	ldy #0
	sty SpritePointer
	sty TopPointer
	stz RefreshOam

CleanOamBuffer:
	ldx #(WMDATA << 8 & $ff00) ; 69 cycles
	stx DMAMODE|$10
	ldx #.loword(CleanSprite)
	stx DMAADDR|$10
	lda #^CleanSprite
	sta DMAADDRBANK|$10
	ldx #$220
	;ldx #$200 ; TODO: Not necessary to reset top bytes, since ultimately only Y coord matters?
	stx DMALEN|$10

	ldx #.loword(OamBuffer)
	stx WMADDL
	stz WMADDH ; lowram
	
	lda #%00000010 ; TODO reserve channel 0 for mid-frame DMA instead of 1
	sta COPYSTART
rts

.segment "RODATA1"
CleanSprite:
.repeat $200
.byte 224
.endrepeat
.repeat $20
.byte %01010101
.endrepeat
.segment "CODE7"


FinalizeSprites:
	ldx TopPointer
	lda #0
	xba
	lda SpritePointer
	lsr
	lsr
	and #$3
	beq @end
	tay
	:
		lsr OamBuffer+$200,X
		lsr OamBuffer+$200,X
		iny
		cpy #4
	bne :-
	@end:
	
	; TODO: Indicate shadow OAM is ready, and check in NMI

	.ifdef FRP
	lda #1
	sta RefreshOam
	.endif
rts

TopMaskTable:
.byte %00000011,0,0,0
.byte %00001100,0,0,0
.byte %00110000,0,0,0
.byte %11000000
