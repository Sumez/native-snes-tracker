.include "global.inc"
.include "src/snes.inc"

.segment "BSS"
BopTimer: .res 1
MosaicTimer: .res 1
HighlightRowIndex: .res 2
ChannelHighlights: .res 8
ChainHighlight: .res 1
ShowBg3: .res 1
Bg3Offset: .res 2

.segment "CODE7"
.export Vfx_Init = Init, Vfx_ResetOnNavigation = Reset, Vfx_Update = Update, Vfx_Update_Vblank = Update_Vblank

Init:
	stz ScrollY
	stz ScrollY+1
	stz BopTimer
	stz ShowBg3

	lda #$ff
	ldy #7
	:	sta ChannelHighlights,Y
		dey
	bpl :-

	lda #0
	xba
	ldx #$08 ; index of first row-highlight sprite
	:		
		@palette = 1
		@priority = 0
		lda #(@palette<<1|@priority<<4)
		sta OamBuffer+3,X
		sta OamBuffer+3+4,X
		txa
		clc
		adc #8
		tax
		cpx #$50
	bne :-
		
	stz OamBuffer+$200
	stz OamBuffer+$201
	stz OamBuffer+$202
	stz OamBuffer+$203
	stz OamBuffer+$204

Reset:
	stz MosaicTimer
	ldx #$ffff
	stx HighlightRowIndex
	lda #$ff
	sta ChainHighlight
	
rtl

Update_Vblank:
@beatRowDark = rgb(1,5,7)
@beatRowPlaying = rgb(1,6,10)
.macro writeColor value
	lda #<value
	sta CGDATA
	lda #>value
	sta CGDATA
.endmacro

	lda IsPlaying
	bne :+

		stz CGADDR
		stz CGDATA
		stz CGDATA

		lda #$09
		sta CGADDR
		writeColor @beatRowDark
		bra :++
	:
		stz CGADDR
		writeColor rgb(1,3,4)

		lda #$09
		sta CGADDR
		writeColor @beatRowPlaying
		
		ldx HighlightRowIndex
		bmi :+
		LoadBlockToOffsetVRAM HighlightTiles, HighlightRowIndex, $18
	:
	
	lda BopTimer
	beq :+
		dec
		sta BopTimer
		lsr
		sta ScrollY
	
	:
	
	lda ShowBg3
	beq :+
		lda #%00010110
		bra :++
	:
		lda #%00010010
	:
	sta BLENDMAIN

rts

HighlightRow:
	seta16
	txa
	bmi :+
		clc
		adc #Bg3TileMapBase-2
		lsr
	:
	sta HighlightRowIndex
	seta8
rts
HighlightChannel:
	; TODO: Subtract song scroll value(?) currently assuming it was done before calling this routine
	cmp #$ff
	beq :+
		asl
		asl
		asl
		sbc #01
	:
	sta ChannelHighlights,X
rts
HighlightChainRow:
	cmp #$ff
	beq :+
		asl
		asl
		asl
		sbc #01
	:
	sta ChainHighlight
rts

rts

HighlightTiles:
.repeat $20
.word $08de
.endrepeat
rts

Bop:
	lda #5
	sta BopTimer
rts
PlayMosaic:
	lda #$08
	sta MosaicTimer
rts

Update:
	jsr UpdateHighlights
	jsr UpdateMosaic
rts

UpdateHighlights:

	ldx #8 ; OamOffset
	lda #30
	sta z:HighlightX
	lda CurrentScreen
	cmp #1
	bne :+
		lda IsPlaying
		lsr
		beq :+
			lda ChainHighlight
			bra :++
	:
		lda #$ff ; temp hotfix
	:
	jsr DrawHighlightSprite
	lda #0
	xba ; Prepare for X-addition with 8-bit A
	ldy #7
	:
		txa
		clc
		adc #8
		tax
		lda ChannelXCoords,Y
		sta z:HighlightX
		lda CurrentScreen
		bne :+
			lda IsPlaying
			beq :+
				lda ChannelHighlights,Y
				bra :++
		:
			lda #$ff ; temp hotfix
		:
		jsr DrawHighlightSprite
		dey
	bpl :---
	
	.import RefreshOam
	lda #1
	sta RefreshOam
rts
	
ChannelXCoords:
.byte 30, 54, 78, 102, 126, 150, 174, 198
	
HighlightX = 0
DrawHighlightSprite:

	sta OamBuffer+1,X
	sta OamBuffer+1+4,X
	cmp #$ff
	beq :+
		lda #6 ; Sprite tile index
		bra :++
	:
		lda #0 ; Invisible sprite
	:
	sta OamBuffer+2,X
	sta OamBuffer+2+4,X
	
	lda HighlightX
	sta OamBuffer+0,X
	clc
	adc #3
	sta OamBuffer+0+4,X
rts

UpdateMosaic:
	lda MosaicTimer
	bne :+
		rts
	:
	dec
	sta MosaicTimer
	bne :+
		stz MOSAIC
		rts
	:
	cmp #4
	bcc :+
		eor #$FF
		sec
		adc #$08
	:
	asl
	asl
	asl
	asl
	ora #$0F
	sta MOSAIC
rts