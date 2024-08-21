.include "global.inc"
.include "src/snes.inc"
.smart
.export main:far

.segment "PROGRAMTITLE"
  ; Limit: 21 characters
  .byte "SNES "
  .byte GAME_TITLE


.segment "CODE7"

main:
	seta8
	setxy16
	phk
	plb

; Create noise pattern for palette, to make sure any graphics data is always visible
stz CGADDR
ldx #$0100
:
	clc
	adc #3
	sta CGDATA
	sta CGDATA
	dex
bne :-	

jsr InitSnesmod
;jsr InitializeBitMask

.importzp seed
ldx #$F83B
stx seed
ldx #$06A2
stx seed+2

.import InitEditor, LoadSamples

jsl LoadSamples
jmp InitEditor





InitSnesmod:
	.import BrewsicInit, BrewsicPlayTrack, BrewsicPlaySound
	jsr BrewsicInit
	lda #0
	;jsr BrewsicPlayTrack
rts

;(ClearRangeValue) = Value to store
;A:X = Target address
;Y = Data length
ClearRange:
	stx DMAADDR|$10
	sta DMAADDRBANK|$10
	
	ldx #(WMDATA << 8 & $ff00)|DMA_CONST
	stx DMAMODE|$10

	sty DMALEN|$10
	
	lda #%00000010 ; TODO reserve channel 0 for mid-frame DMA instead of 1
	sta COPYSTART
rts
ResetRange:
	stx DMAADDR|$10
	sta DMAADDRBANK|$10
	
	ldx #(WMDATA << 8 & $ff00)|DMA_00
	stx DMAMODE|$10

	sty DMALEN|$10
	
	lda #%00000010 ; TODO reserve channel 0 for mid-frame DMA instead of 1
	sta COPYSTART
rts