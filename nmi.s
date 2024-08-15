.include "global.inc"
.include "src/snes.inc"
.p816
.smart

.segment "BSS"
ButtonStates: .res 2
ButtonPushed_Actual: .res 2
Timer_Up: .res 2
Timer_Down: .res 2
Timer_Left: .res 2
Timer_Right: .res 2
ButtonPushed: .res 2
ScrollY: .res 2
ColorBlend: .res 1

NMITEST: .res 2

.segment "CODE7"

.export nmi_handler
.import AsciiGfx
.importzp playerFrameIndex
.import LoadFromTileBuffer
.import RefreshOam
nmi_handler:
	phb
	phk
	plb
	seta16
	setxy16
	pha
	txa
	pha
	tya
	pha
	seta8

stz NMITEST

	lda #FORCEBLANK
	sta PPUBRIGHT

	lda IsPlaying
	asl
	asl
	asl
	asl
	asl
	stz CGADDR
	stz CGDATA
	sta CGDATA
	bne :+
		lda #$09
		sta CGADDR
		lda #$42
		sta CGDATA
		lda #$10
		sta CGDATA
		bra :++
	:
		lda #$09
		sta CGADDR
		lda #$84
		sta CGDATA
		lda #$20
		sta CGDATA
	:


.import CopyEntireTilemap, CopyTilemapToUiLayer
jsl CopyEntireTilemap ; TODO: just queue updated rows?
;jsl CopyTilemapToUiLayer

	lda RefreshOam
	beq :+
		stz RefreshOam
		LoadBlockToOAM OamBuffer, 544
	:
	
;	.import CopySpriteGraphics
;	jsr CopySpriteGraphics


lda ScrollY
sta BG1SCROLLY
lda ScrollY+1
sta BG1SCROLLY
seta16
lda #0
seta8
sta BG2SCROLLY
xba
sta BG2SCROLLY


lda #$0F
sta PPUBRIGHT

	lda ColorBlend
	sta CGADSUB



;Tests vblank overflow
;lda SLHV
;lda OPVCT
;xba
;lda OPVCT
;xba
;seta16
;and #$1FF
;cmp #200
;bcs :+
;	lda #0
;:


.import BrewsicTickStream
;jsr BrewsicTickStream

	;bit NMISTATUS
	seta16
	pla
	tay
	pla
	tax
	pla
	plb
rti

UpdateInputStates:
@DasCharge = 20
@DasRepeat = 3
	seta16
	lda ButtonStates
	eor JOY1CUR
	and JOY1CUR
	sta ButtonPushed_Actual ; XOR, AND with new state, preserves bits of only bytes that were changed
	sta ButtonPushed
	
	ldy #@DasCharge
	bit #KEY_DOWN
	beq :+
		sty Timer_Down
	:
	bit #KEY_LEFT
	beq :+
		sty Timer_Left
	:
	bit #KEY_UP
	beq :+
		sty Timer_Up
	:
	bit #KEY_RIGHT
	beq :+
		sty Timer_Right
	:

	ldy #@DasRepeat
	lda JOY1CUR
	sta ButtonStates
	bit #KEY_DOWN
	beq :+
		dec Timer_Down
		bne :+
			sty Timer_Down
			lda #KEY_DOWN
			ora ButtonPushed_Actual
			sta ButtonPushed
			lda ButtonStates
	:
	bit #KEY_LEFT
	beq :+
		dec Timer_Left
		bne :+
			sty Timer_Left
			lda #KEY_LEFT
			ora ButtonPushed_Actual
			sta ButtonPushed
			lda ButtonStates
	:
	bit #KEY_UP
	beq :+
		dec Timer_Up
		bne :+
			sty Timer_Up
			lda #KEY_UP
			ora ButtonPushed_Actual
			sta ButtonPushed
			lda ButtonStates
	:
	bit #KEY_RIGHT
	beq :+
		dec Timer_Right
		bne :+
			sty Timer_Right
			lda #KEY_RIGHT
			ora ButtonPushed_Actual
			sta ButtonPushed
			;lda ButtonStates
	:

	
	seta8
rts


