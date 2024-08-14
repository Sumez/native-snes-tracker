.include "global.inc"
.include "src/snes.inc"
.smart

.segment "BSS"
MosaicTimer: .res 1

.segment "CODE7"
.export Vfx_Init = Init, Vfx_Update = Update

Init:
	stz MosaicTimer
rtl

PlayMosaic:
	lda #$08
	sta MosaicTimer
rts
Update:
	jsr UpdateMosaic
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