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
	
.macro writeColor value
	lda #<value
	sta CGDATA
	lda #>value
	sta CGDATA
.endmacro


	lda #$0
	sta CGADDR
	writeColor rgb(0,60,60) ; Background color
	;writeColor rgb(60,188,252) ; Background color
	;writeColor rgb(60,136,50) ; Background color
	; Index 1
	writeColor rgb(136,136,136)
	writeColor rgb(50,50,50)

	lda #$5
	sta CGADDR
	writeColor rgb(70,70,70)
	writeColor rgb(20,20,20)

;TODO: Test tile
lda #$f
sta CGADDR
writeColor rgb(60,20,0) ; Background color



nop
nop
nop
nop
nop
nop
nop
nop
nop

jsr InitSnesmod
;jsr InitializeBitMask

.importzp seed
ldx #$F83B
stx seed
ldx #$06A2
stx seed+2

.import InitEditor
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