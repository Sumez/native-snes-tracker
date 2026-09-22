.include "global.inc"
.include "src/snes.inc"
.smart

.exportzp BufferedVwfTiles
.export CopyVwfTiles

.segment "RODATA"
TextAddresses:
DialogChr:
.incbin "gfx/font2.chr"
.res $400 ; TODO: Just put the UI chr somewhere else and free up space
.incbin "gfx/ui.chr"
DialogChrEnd:

CursorSpriteChr:
.incbin "gfx/cursor.chr"
CursorSpriteChrEnd:

.segment TilemapBufferSegment
.segment "BSS"
VwfBuffer: .res $200 ; TODO: Find realistic size limit
VwfBuffer_End:
;.res 16 ; Extra buffer for garbage at the end (unnecessary because VwfText is always overwritten at the end)
VwfText: .res $17
StringBuffer: .res $30
LastByteOffset: .res 2
LastByteOffset_saved: .res 2
LastTileIndex: .res 2
LastTileIndex_saved: .res 2
WriteStringPosition: .res 2
BufferStartAddress: .res 2
ChrStartAddress: .res 2
;NextBufferStartAddress: .res 2
;NextChrStartAddress: .res 2

.segment "ZEROPAGE"
BufferedVwfTiles: .res 2

.segment "RODATA"
CharSizes:
.byte 4,4,4,4,3,3,4,4,2,4,4,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4 ; A-Z
.byte 2,3,4,4,4,4
.byte 4,4,4,4,4,3,4,4,2,3,4,3,4,4,4,4,4,3,4,4,4,4,4,4,4,4 ; a-z
.byte 2,3,4,4,4,4
.byte 4,3,4,4,4,4,4,4,4,4 ; 0-9
.byte 3,3 ; (,)


.segment "CODE7"
TEST: .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8A,$8B,$8C,$8D,$8E,$8F
.byte $ff

VwfChr:
.incbin "gfx/vwf.chr",0,$300
VwfChrEnd:

.segment "CODE7"
.export LoadTextGraphics = LoadChr
.export ResetVwfText

LoadChr:
	LoadBlockToVRAM DialogChr, Bg3ChrBase, (DialogChrEnd - DialogChr)
	LoadBlockToVRAM CursorSpriteChr, (SpriteChrBase), (CursorSpriteChrEnd - CursorSpriteChr)
rtl

Vwf_RecordRestorePoint:
	ldx LastTileIndex
	stx LastTileIndex_saved
	ldx LastByteOffset
	stx LastByteOffset_saved
rtl

Vwf_ReturnToRestorePoint:
	ldx LastTileIndex_saved
	stx LastTileIndex
	ldx LastByteOffset_saved
	stx LastByteOffset
jmp resetVwfText2

ResetVwfText:
	ldx #0
	stx LastByteOffset
	stx LastTileIndex
	resetVwfText2:
	seta16 ; It's ok to write two bytes at a time with an arbitrary start because we have a garbage buffer at the end
	lda #0
	:
		sta f:VwfBuffer,X
		inx
		inx
		cpx #.loword(VwfBuffer_End - VwfBuffer)
	bcc :-
	
	;lda #.loword(VwfBuffer)
	;sta BufferStartAddress
	;lda #(Bg3ChrBase+$800)>>1
	;sta ChrStartAddress
	seta8
rtl

BufferNewString:
	ldx #0
	stx WriteStringPosition
BufferString:
	@stringPointer = 0

	ldx WriteStringPosition
	sty @stringPointer
	ldy #0
	:
		lda (@stringPointer),y
		sta StringBuffer,x
		bmi @end
		inx
		iny
	bra :-
		
	@end:
	stx WriteStringPosition
rtl
;BufferNewStringFromLongAddress:
;	ldx #0
;	stx WriteStringPosition
;BufferStringFromLongAddress:
;	@stringPointer = 0
;
;	ldx WriteStringPosition
;	ldy #0
;	:
;		lda [@stringPointer],y
;		sta StringBuffer,x
;		cmp #$ff
;		beq @end
;		inx
;		iny
;	bra :-
;		
;	@end:
;	stx WriteStringPosition
;rtl
PrintBufferedString:
	ldy #.loword(StringBuffer)
WriteTextToHeader:
	jsl bufferText
jml WriteTilemapHeader

WriteTextToTilemapIndex:
	phx
	jsl bufferText
	plx
jml WriteTilemapText



bufferText:
@stringPointer = 0;,1
@pixelOffset = 2;,3
@targetByteOffset = 4;,5
@sourceByteOffset = 6;,7
@bitMask = 8;,9
@planeCount = 9 ; Can borrow byte 9, because it always gets set after bitmask is written to

	sty @stringPointer
	ldy LastByteOffset
	sty @targetByteOffset
	ldy #0
	phy
	seta16
	stz @pixelOffset
	@charLoop:
		ply
		lda #$f0
		sta @bitMask
		lda (@stringPointer),y
		and #$ff
		cmp #$ff
		beq @endChr
		
		iny
		phy ; Pull at the start of next loop
		pha ; Pull into X to get character size later

		lsr ; Half to get odd/even from carry
		bcc :+
			pha
			lda #$0f
			sta @bitMask
			lda @pixelOffset
			ora #$08
			sta @pixelOffset
			pla
		:
		asl
		asl
		asl
		asl ; x8 to bet the byte index of the character (4px chars of 2bpp)
		sta @sourceByteOffset
		lda @pixelOffset
		asl
		tax
		jmp (.loword(@CopyRoutines),x)
		@afterCopy:
		
		seta8
		plx
		lda @pixelOffset
		and #7
		clc
		adc f:CharSizes,X
		bit #8
		beq :+
			pha
			seta16
			lda @targetByteOffset
			clc
			adc #16
			sta @targetByteOffset
			seta8
			pla
		:
		and #7
		sta @pixelOffset
		seta16
		
	bra @charLoop

	@endChr:
	
	lda @targetByteOffset
	sta LastByteOffset
	lsr
	lsr
	lsr
	lsr
	tay
	lda @pixelOffset
	and #$7
	beq :+
		iny
		clc
		lda LastByteOffset
		adc #16
		sta LastByteOffset
	:
	lda #$80
	clc
	adc LastTileIndex
	sty LastTileIndex
	seta8
	ldx #0
	:
		sta VwfText,X
		inc a
		inx
		dey
	bne :-
	lda #$ff
	sta VwfText,X
	
	lda #1
	sta BufferedVwfTiles

	ldy #.loword(VwfText)
rtl

@CopyRoutines:
.addr @CopyDirect, @CopyPlus1, @CopyPlus2, @CopyPlus3
.addr @CopyPlus4, @CopyPlus5, @CopyPlus6, @CopyPlus7
.addr @CopyMinus4, @CopyMinus3, @CopyMinus2, @CopyMinus1
.addr @CopyDirect, @CopyPlus1, @CopyPlus2, @CopyPlus3

.macro vwfCopyRoutine pixelShift, negativeShift
	seta8
	ldy @sourceByteOffset
	ldx @targetByteOffset
	lda #8
	sta @planeCount
	:
		lda VwfChr,y
		and @bitMask
		.if .paramcount > 1
			.repeat negativeShift
				asl
			.endrepeat
		.else
			.repeat pixelShift
				lsr
				ror VwfBuffer+16,x
			.endrepeat
		.endif
		ora VwfBuffer,x
		sta VwfBuffer,x
		lda VwfChr+1,y
		and @bitMask
		.if .paramcount > 1
			.repeat negativeShift
				asl
			.endrepeat
		.else
			.repeat pixelShift
				lsr
				ror VwfBuffer+17,x
			.endrepeat
		.endif
		ora VwfBuffer+1,x
		sta VwfBuffer+1,x
		iny
		iny
		inx
		inx
		dec @planeCount
	bne :-
	seta16
	jmp @afterCopy
.endmacro


@CopyMinus4: vwfCopyRoutine 0, 4
@CopyMinus3: vwfCopyRoutine 0, 3
@CopyMinus2: vwfCopyRoutine 0, 2
@CopyMinus1: vwfCopyRoutine 0, 1
@CopyPlus7: vwfCopyRoutine 7
@CopyPlus6: vwfCopyRoutine 6
@CopyPlus5: vwfCopyRoutine 5
@CopyPlus4: vwfCopyRoutine 4
@CopyPlus3: vwfCopyRoutine 3
@CopyPlus2: vwfCopyRoutine 2
@CopyPlus1: vwfCopyRoutine 1
@CopyDirect: vwfCopyRoutine 0

.a8

CopyVwfTiles: ; Called during NMI if tiles are buffered
	stz BufferedVwfTiles
	;LoadOffsetBlockToVRAM ^VwfBuffer, BufferStartAddress, ChrStartAddress, $100
	LoadBlockToVRAM VwfBuffer, (Bg3ChrBase+$800), $200
rtl
