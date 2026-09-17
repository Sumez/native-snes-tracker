.include "global.inc"
.include "src/snes.inc"
.smart

.segment "CODE5"

StoreCurrentSong:

	jsr StoreSongIntoBuffer

	.import Samples_Init
	jsl Samples_Init ; Rebuilds the compiled song buffer that we've ruined with the storage buffer
rtl


TESTDATA:
.byte $12,$13,$14,$15
.repeat 10
.byte $16
.endrepeat
.byte $17
.repeat 5
.byte $18
.endrepeat
.repeat 20
.byte $19
.endrepeat
.repeat 150
.byte $20
.endrepeat
.repeat 150
.byte $21,$22
.endrepeat


;TestRleCompression:
;.export TestRleCompression
;ldx #0
;	lda #^TESTDATA
;	ldy #.loword(TESTDATA)
;	sty z:@sourcePointer
;	sta z:@sourcePointer+2
;	jsr @RleCompress
;rtl

.export StoreSongIntoBuffer
StoreSongIntoBuffer:
.assert ^HEADER = ^TITLE && ^TITLE = ^AUTHOR && ^AUTHOR = ^SONG, error, "Header and song base data must be stored in the same bank"
.assert ((SONG - HEADER) & 1) = 0, error, "Header size must be divisible by 2"
@sourcePointer = 0;1,2
@sameCount = 3;4
@diffCount = 5
@lastValue = 6
@blockStart = 7;,8


	ldx #0

	phb
	lda #^HEADER
	pha
	plb
	
	seta16
	ldy #0
	:
		lda HEADER,y
		sta f:StorageBuffer,X
		iny
		iny
		inx
		inx
		cpy #.loword(SONG - HEADER)
	bne :-
	
	seta8
	lda #^SONG
	ldy #.loword(SONG)
	sty @sourcePointer
	sta @sourcePointer+2
	jsr @RleCompress
	
	plb
rts

@RleCompress:
@repeatThreshold = 4; Only treat 4+ successive "same" values as a block, otherwise the compression would be wasteful
	ldy #0
	sty @blockStart
	stz @sameCount+1 ; Prepare for 16-bit addition

	@startBlock:
		stz @sameCount
		stz @diffCount
		; Initialize @lastValue with a fake value to ensure new start
		ldy @blockStart
		lda [@sourcePointer],Y
		inc a
		sta @lastValue
		
	@loop:
		lda [@sourcePointer],Y
		cmp @lastValue
		beq @sameValue
			sta @lastValue
			lda @sameCount
			cmp #(@repeatThreshold-1)
			bcs @storeBlocks ; Encountered diff-value after 4+ same-values
				sec ; Add to existing diff + 1
				adc @diffCount
				sta @diffCount
				stz @sameCount
				cmp #$80 ; Encountered 128+ diff-values
				bcs @storeBlock
				bra :+
		@sameValue:
			inc @sameCount
			lda @sameCount
			cmp #$7E
			beq @storeBlocks
		:
		iny
		cpy #$2000
	bne @loop
	; TODO: Store the remaining blocks
		
rts
@storeBlocks:
	; Entered due to "same value" hit 4-$7E, so convert one "same" value into a diff value
	dec @diffCount
	inc @sameCount
@storeBlock:
	ldy @blockStart
	lda @diffCount
	beq :+++
		bpl :+
			lda #$7F
			sta @diffCount
		:
		sta f:StorageBuffer,X
		inx
		inc a ; Count to 0
		:
			lda [@sourcePointer],Y
			sta f:StorageBuffer,X
			inx
			iny
			dec @diffCount
		bne :-
		sty @blockStart
	:
	
	lda @sameCount
	beq :+
		seta16
		tya
		clc
		adc @sameCount
		sta @blockStart
		seta8
		lda @sameCount
		ora #$80
		sta f:StorageBuffer,X
		inx
		lda [@sourcePointer],Y
		sta f:StorageBuffer,X
		inx
	:
	
jmp @startBlock
