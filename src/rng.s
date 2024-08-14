.include "global.inc"
.include "src/snes.inc"
.smart

.exportzp seed
.segment "ZEROPAGE"
seed: .res 4       ; initialize 16-bit seed to any value except 0

.segment "CODE7"
RNG:
	ldy #8     ; iteration count (generates 8 bits)
	lda seed+0;,X
:
	asl        ; shift the register
	rol seed+1;,X
	bcc :+
	eor #$2D   ; apply XOR feedback whenever a 1 bit is shifted out
:
	dey
	bne :--
	sta seed+0;,X
;	cmp #0     ; reload flags
rts