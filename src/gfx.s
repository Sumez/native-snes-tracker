.include "global.inc"
.include "src/snes.inc"

.segment "RODATA7"
PatternPalette:
.incbin "gfx/font.inc.0.pal"
PatternPaletteHighlight:
.incbin "gfx/font.inc.1.pal"
PatternPaletteDim:
.incbin "gfx/font.inc.2.pal"


.export LoadVRAM, LoadOAM
.segment "CODE7"
;============================================================================
;LoadPalette - Macro that loads palette information into CGRAM
;----------------------------------------------------------------------------
; In: SRC_ADDR -- 24 bit address of source data,
;     START -- Color # to start on,
;     SIZE -- # of COLORS to copy
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: A,X
; Requires: mem/A = 8 bit, X/Y = 16 bit
;----------------------------------------------------------------------------
.macro LoadPalette srcBank, srcAddr, startIndex, count
    lda #startIndex
    sta CGADDR       ; Start at START color
    lda #:srcAddr        ; Using : before the parameter gets its bank.
    ldx #srcAddr         ; Not using : gets the offset address.
    ldy #(count * 2)   ; 2 bytes for every color
    jsr DMAPalette
.endmacro

;============================================================================
; DMAPalette -- Load entire palette using DMA
;----------------------------------------------------------------------------
; In: A:X  -- points to the data
;      Y   -- Size of data
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: none
;----------------------------------------------------------------------------
DMAPalette:
    phb
    php         ; Preserve Registers

    stx DMAADDR   ; Store data offset into DMA source offset
    sta DMAADDRBANK   ; Store data bank into DMA source bank
    sty DMALEN   ; Store size of data block

    stz DMAMODE   ; Set DMA Mode (byte, normal increment)
    ;lda #$22    ; Set destination register ($2122 - CGRAM Write)
	lda #<CGDATA
    sta DMAPPUREG
    lda #$01    ; Initiate DMA transfer
    sta COPYSTART

    plp
    plb
 rts         ; return from subroutine

.macro LoadBlockToMode7Chr srcAddr, size
    lda #$80
    sta PPUCTRL       ; Set VRAM transfer mode to word-access, increment by 1
    ldx #$0000         ; DEST
    stx PPUADDR       ; $2116: Word address for accessing VRAM.
    lda #^srcAddr        ; SRCBANK
    ldx #(srcAddr & $ffff)         ; SRCOFFSET
    ldy #size         ; SIZE
    jsr LoadMode7Chr
.endmacro

;============================================================================
; LoadVRAM -- Load data into VRAM
;----------------------------------------------------------------------------
; In: A:X  -- points to the data
;     Y     -- Number of bytes to copy (0 to 65535)  (assumes 16-bit index)
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: none
;----------------------------------------------------------------------------
; Notes:  Assumes VRAM address has been previously set!!
;----------------------------------------------------------------------------
LoadVRAM:
	phb
	php         ; Preserve Registers

	stx $4302   ; Store Data offset into DMA source offset
	sta $4304   ; Store data Bank into DMA source bank
	sty $4305   ; Store size of data block

	lda #$01
	sta DMAMODE   ; Set DMA mode (word, normal increment)
	;lda #$18    ; Set the destination register (VRAM write register)
	lda #<PPUDATA
	sta DMAPPUREG
	lda #$01    ; Initiate DMA transfer (channel 1)
	sta $420B

	plp         ; restore registers
	plb
	rts         ; return

LoadOAM:
	phb
	php         ; Preserve Registers

    stz OAMADDR
    stz OAMADDR+1

	stx $4312   ; Store Data offset into DMA source offset
	sta $4314   ; Store data Bank into DMA source bank
	sty $4315   ; Store size of data block

	lda #$00
	sta DMAMODE|$10   ; Set DMA mode (byte, normal increment)
	;lda #$18    ; Set the destination register (VRAM write register)
	lda #<OAMDATA
	sta DMAPPUREG|$10
	lda #%00000010    ; Initiate DMA transfer (channel 2)
	sta $420B

	plp         ; restore registers
	plb
	rtl         ; return
	
.segment "CODE7"

InitOam:

	lda #(^OamBuffer)
	pha
	plb

	jsr CleanOamBuffer

	LoadBlockToOAM OamBuffer, 544

rts

.export loadPalette
; A = palette index, X = color count in bytes (*2), Y = palette source address, Uses $00-$01
loadPalette:
	sta CGADDR
	sty 0
	ldy #0
	:
		lda (0),Y
		sta CGDATA
		iny
		dex
	bne :-
rts

LoadPalettes:

	lda #$00 ; 10, 20, etc
	ldx #$20
	ldy #.loword(PatternPalette)
	jsr loadPalette

	lda #$10 ; 10, 20, etc
	ldx #$20
	ldy #.loword(PatternPaletteHighlight)
	jsr loadPalette

	lda #$80 ; 10, 20, etc
	ldx #$20
	ldy #.loword(PatternPaletteDim)
	jsr loadPalette

	lda #$90 ; 10, 20, etc
	ldx #$20
	ldy #.loword(PatternPaletteHighlight)
	jsr loadPalette

rts


.segment "BSS"
OamBuffer: .res $220
