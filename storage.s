.include "global.inc"
.include "src/snes.inc"
.smart

.segment "BSS"
ChainBuffer: .res $20 ; TODO: Just use the chain view's chain data 

.segment "CODE5"

.export StoreCurrentSong
StoreCurrentSong:

	jsr StoreSongIntoBuffer
	
	jsr CopyBufferIntoSram
	
	jsr IntegrityCheck
	beq :+
		BRK ; Integrity check failed! Do not allow the user to clear song or load new
	:

	.import Samples_Init
	jsl Samples_Init ; Rebuilds the compiled song buffer that we've ruined with the storage buffer
rtl

IntegrityCheck:
@blockSize = 9
	jsr LoadSavedSongIntoBuffer
	
	phb

	ldx #0
	ldy #.loword(SAMPLES_END-HEADER)
	sty @blockSize

	lda #^HEADER
	jsr @checkBlock
	bne @error

	ldy #$2000
	sty @blockSize

	lda #^CHAINS
	jsr @checkBlock
	bne @error

	lda #^PHRASES_1
	jsr @checkBlock
	bne @error

	lda #^PHRASES_2
	jsr @checkBlock
	bne @error
	
	ldy #$200
	sty @blockSize

	lda #^INSTRUMENTS
	jsr @checkBlock
	bne @error

	plb
	lda #0
rts
@error:
	plb
@blockError:
	lda #$ff
rts
@checkBlock:
	pha
	plb
	ldy #0
	:
		lda f:StorageBuffer,x
		cmp CHAINS,y
		bne @blockError
		inx
		iny
		cpy @blockSize
	bne :-
	lda #0
rts

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

StoreSongIntoBuffer:
.assert ^HEADER = ^TITLE && ^TITLE = ^AUTHOR && ^AUTHOR = ^SONG, error, "Header and song base data must be stored in the same bank"
.assert (SAMPLES_END - HEADER) < $2001, error, "Start of header until end of samples must all be RLE compressable in a single block"
@sourcePointer = 0;1,2
@compressionTempValues = 3;,4,5,6,7,8
@blockSize = 9;,10
@countingEmptyChains = 11
@countingEmptyPhrases = 11

ldy #0
ldx #0
lda #0
:
	sta f:StorageBuffer,x
	iny
	inx
bne :-

	ldx #0

	phb
	
	lda #^HEADER
	ldy #.loword(HEADER)
	sty @sourcePointer
	sta @sourcePointer+2
	ldy #.loword(SAMPLES_END - HEADER)
	sty @blockSize
	jsr @RleCompress
	
	jsr @CompressChainSegment
	
	lda #^PHRASES_1
	jsr @CompressPhraseSegment
	lda #^PHRASES_2
	jsr @CompressPhraseSegment
	
	plb
rts

@CompressPhraseSegment:
	pha
	plb

	.assert .loword(PHRASES_1) = .loword(PHRASES_2), error, "You know"
	ldy #.loword(PHRASES_1)
	sty @sourcePointer
	sta @sourcePointer+2

	ldy #$40
	sty @blockSize
	
	stz @countingEmptyPhrases
	ldy #0
	@phraseLoop:

		phx
		phy
		jsr @CompressPhrase
		seta16
			pla
			clc
			adc #$40
			tay
		seta8

		jsr CheckIfPhraseWasEmpty ; Check the COMPRESSED phrase to see if it was empty
		bne @notEmptyPhrase
		@emptyPhrase:
			; Phrase was empty. Store as new empty, or increate the counter
			plx ; Reset X to before we stored the compressed phrase
			lda @countingEmptyPhrases
			bne :+
				; Always one $00 before the counter byte
				sta f:StorageBuffer,x
				inx
				inx ; Increase 1 extra because storing the counter in the next step always decreates X by 1
			:
			inc a
			sta @countingEmptyPhrases
			sta f:StorageBuffer-1,x
			jmp @endThisPhrase
			
		@notEmptyPhrase:
			stz @countingEmptyPhrases ; If we were counting empty phrases, reset that counter now
			pla ; Discard the stored X value
			pla
			
		@endThisPhrase:
		cpy #$2000
	beq :+
		jmp @phraseLoop
	:
rts


@CompressChainSegment:
	lda #^ChainBuffer
	ldy #.loword(ChainBuffer)
	sty @sourcePointer
	sta @sourcePointer+2
	ldy #$20
	sty @blockSize
	
	lda #^CHAINS
	pha
	plb
	stz @countingEmptyChains
	ldy #0
	@chainLoop:
		jsr CheckIfChainIsEmpty
		bne @notEmpty
		@empty:
			; Chain was empty. Store as new empty, or increate the counter
			lda @countingEmptyChains
			bne :+
				; Always one $00 before the counter byte
				sta f:StorageBuffer,x
				inx
				inx ; Increase 1 extra because storing the counter in the next step always decreates X by 1
			:
			inc a
			sta @countingEmptyChains
			sta f:StorageBuffer-1,x
			phy
			jmp @endThisChain
			
		@notEmpty:
			stz @countingEmptyChains ; If we were counting empty chains, reset that counter now
			; RLE comrpess the chain and store
			@chainOffset .set 0
			; Convert the chain into a more RLE friendly format first
			.repeat 16
				lda CHAINS+(@chainOffset*2),y
				sta f:ChainBuffer+(@chainOffset)
				lda CHAINS+(@chainOffset*2)+1,y
				sta f:ChainBuffer+@chainOffset+$10
				@chainOffset .set @chainOffset + 1
			.endrepeat
			phy
			jsr @RleCompress
			
		@endThisChain:
		; Add $20 to Y and read the next chain
		seta16
		pla
		clc
		adc #$20
		tay
		seta8
		cpy #$2000
	beq :+
		jmp @chainLoop
	:
rts

@CompressPhrase:
; $01-$10 - 1-16 empty rows
; Byte 7 set: Has note
; Byte 6 set; Has command
; Bytes 0-5: 0-64 - Command if has one, Otherwise instrument
@p_inst = @compressionTempValues + 0
@p_cmd = @compressionTempValues + 1
@p_param = @compressionTempValues + 2
@p_note = @compressionTempValues + 3
@p_countingEmpty = @compressionTempValues + 4
@p_rowCount = @compressionTempValues + 5

	stz @p_countingEmpty
	lda #16+1 ; Decrease before first loop
	sta @p_rowCount
	@phraseRowLoop:
		dec @p_rowCount
		bne :+
			rts
		:

		@readPhraseRow:
		lda PHRASES_1+0,y ; Instrument
		sta @p_inst
		lda PHRASES_1+1,y ; Command
		cmp #$ff
		bne :++
			stz @p_cmd
			lda PHRASES_1+2,y
			sta @p_note
			cmp #$fe
			bcc :+
				; Note is $FE or $FF. Normalize uninitialized instrument data
				lda #$ff
				sta PHRASES_1+0,y
				sta @p_inst
			:
			bra @compressPhraseRow
		:
			sta @p_cmd
			bne :+
			 	; Command is 0. Normalize uninitialized command parameter
				lda #0
				sta PHRASES_1+2,y
			:
			lda PHRASES_1+2,y
			sta @p_param
			lda PHRASES_1+3,y
			sta @p_note
			cmp #$fe
			bcc :+
				; Note is $FE or $FF. Normalize uninitialized instrument data
				lda #$ff
				sta PHRASES_1+0,y
				sta @p_inst
				
			:
			iny ; Increase Y 3 times if cmd was $ff, 4 times if it was anything else
		
		@compressPhraseRow:
		iny
		iny
		iny
		lda @p_cmd
		bne :+
			; Has no command
			lda @p_note
			cmp #$ff
			beq @emptyPhraseRow
				; Has note, but no command:
				sta f:StorageBuffer+1,X
				lda @p_inst
				and #$3F
				ora #$80
				sta f:StorageBuffer,X
				inx
				inx
				stz @p_countingEmpty
				bra @phraseRowLoop
		:
			; Has command
			and #$3F
			ora #$40
			sta f:StorageBuffer,X
			lda @p_param
			sta f:StorageBuffer+1,X
			lda @p_note
			cmp #$ff
			beq :+
				; Has both command AND note
				lda f:StorageBuffer,X
				ora #$80
				sta f:StorageBuffer,X
				lda @p_inst
				sta f:StorageBuffer+2,X
				lda @p_note
				sta f:StorageBuffer+3,X
				inx
				inx
			:
			inx
			inx
			stz @p_countingEmpty
		jmp @phraseRowLoop
		
	@emptyPhraseRow:
	
		lda @p_countingEmpty
		bne :+
			inx ; Increase X by 1 only at the first write
		:
		inc a
		sta @p_countingEmpty
		sta f:StorageBuffer-1,x
		
	jmp @phraseRowLoop
	
rts

@RleCompress:
@sameCount = @compressionTempValues + 0;.1
@diffCount = @compressionTempValues + 2
@lastValue = @compressionTempValues + 3
@blockStart = @compressionTempValues + 4;,5

@repeatThreshold = 4; Only treat 4+ successive "same" values as a block, otherwise the compression would be wasteful
	ldy #0
	sty @blockStart
	stz @sameCount+1 ; Prepare for 16-bit addition

	@startBlock:
		ldy @blockStart
		cpy @blockSize
		bne :+
			rts
		:
		stz @sameCount
		stz @diffCount
		; Initialize @lastValue with a fake value to ensure new start
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
		cpy @blockSize
	bne @loop
	
	; Store the remaining blocks:
	lda @sameCount
	beq @storeBlock ; If sameCount is 0, just store the "diff" block

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

CheckIfPhraseWasEmpty:
	lda f:StorageBuffer-1,X
	cmp #$10 ; If the last byte written to storage buffer was $10, that means the phrase was empty
	beq :+
		; Return with Z=false = not empty
		rts
	:
	; If the phrase was empty, fill it with $ff to avoid failing integrity check
	phx
	phy
	ldx #$40
	lda #$ff
	:
		dey
		sta PHRASES_1,y
		dex
	bne :-
	ply
	plx
	lda #0
rts
	
	

CheckIfChainIsEmpty:
	lda CHAINS+0,y
	and CHAINS+2,y
	and CHAINS+4,y
	and CHAINS+6,y
	and CHAINS+8,y
	and CHAINS+10,y
	and CHAINS+12,y
	and CHAINS+14,y
	and CHAINS+16,y
	and CHAINS+18,y
	and CHAINS+20,y
	and CHAINS+22,y
	and CHAINS+24,y
	and CHAINS+26,y
	and CHAINS+28,y
	and CHAINS+30,y
	cmp #$ff
	bne :+ ; Returns as "not equals"
		; First clear all transpose values to avoid failing integrity check
		lda #0
		sta CHAINS+1,y
		sta CHAINS+3,y
		sta CHAINS+5,y
		sta CHAINS+7,y
		sta CHAINS+9,y
		sta CHAINS+11,y
		sta CHAINS+13,y
		sta CHAINS+15,y
		sta CHAINS+17,y
		sta CHAINS+19,y
		sta CHAINS+21,y
		sta CHAINS+23,y
		sta CHAINS+25,y
		sta CHAINS+27,y
		sta CHAINS+29,y
		sta CHAINS+31,y
		; Z is still set here because A is 0, so return as "equals"
	:
rts

CopyBufferIntoSram:
;TODO: Use DMA?
@songSize = 0;,1
@targetPointer = 2;,3,4

	cpx #$2000
	bcc :+
		;ldx #$2000
		BRK ; Too large song. Break into chunks
	:
	stx @songSize
	
	lda #$3f
	ldy #$6000
	sty @targetPointer
	sta @targetPointer+2

	phb
	lda #^StorageBuffer
	pha
	plb
	
	ldy #0
	:
		lda StorageBuffer,Y
		sta [@targetPointer],Y
		iny
		cpy @songSize
	bne :-
	
	plb
rts

LoadSavedSongIntoBuffer:
@sourcePointer = 0;1,2
@segmentSize = 3;,4
@chainCounter = 5
@phraseCounter = 5
@blockSize = 9;,10
@chainSize = 11
@phraseSize = 11

	lda #$3f
	ldy #$6000
	sty @sourcePointer
	sta @sourcePointer+2

	ldx #0
	stz @blockSize+1
	
	ldy #.loword(SAMPLES_END - HEADER)
	sty @segmentSize

	ldy #0
	jsr @RleDecompress
	
	stz @chainCounter
	@chainLoop:
		lda [@sourcePointer],Y
		bne :+
		@emptyCBlocks:
			iny
			lda [@sourcePointer],Y
			iny
			sta @blockSize
			@loopEmptyChain:
				jsr @AddEmptyChain
				dec @chainCounter
				dec @blockSize
			bne @loopEmptyChain
			lda @chainCounter
			bne @chainLoop
			bra @chainsEnd
		:
		@compressedCBlock:
			lda #$20
			sta @segmentSize		
			stz @segmentSize+1
			jsr @RleDecompress
			jsr @FixLoadedChainData
			
			dec @chainCounter
			bne @chainLoop
	@chainsEnd:
	
	stz @phraseCounter
	@phraseLoop:
		lda [@sourcePointer],Y
		bne :+
		@emptyPBlocks:
			iny
			lda [@sourcePointer],Y
			iny
			sta @blockSize
			@loopEmptyPhrase:
				jsr @AddEmptyPhrase
				dec @phraseCounter
				dec @blockSize
			bne @loopEmptyPhrase
			lda @phraseCounter
			bne @phraseLoop
			bra @phrasesEnd
		:
		@compressedPBlock:
			jsr @DecompressPhrase
			dec @phraseCounter
			bne @phraseLoop
	@phrasesEnd:
rts
@AddEmptyChain:
	lda #16
	sta @chainSize
	:
		lda #$ff
		sta f:StorageBuffer,X
		inx
		lda #$00
		sta f:StorageBuffer,X
		inx
		dec @chainSize
	bne :-
rts
@AddEmptyPhrase:
	lda #16
	sta @phraseSize
	lda #$ff
	:
		sta f:StorageBuffer,X
		inx
		sta f:StorageBuffer,X
		inx
		sta f:StorageBuffer,X
		inx
		sta f:StorageBuffer,X
		inx
		dec @phraseSize
	bne :-
rts
@DecompressPhrase:
; $01-$10 - 1-16 empty rows
; Byte 7 set: Has note
; Byte 6 set; Has command
; Bytes 0-5: 0-64 - Command if has one, Otherwise instrument
	lda #16+1 ; Decrease before first loop
	sta @phraseSize
	@phraseBlockLooop:
	dec @phraseSize
	bne :+
		rts
	:
	; Default to empty row before overwriting with loaded data
	lda #$ff
	sta f:StorageBuffer,X
	sta f:StorageBuffer+3,X
	lda #0
	sta f:StorageBuffer+1,X
	sta f:StorageBuffer+2,X
			
	lda [@sourcePointer],Y
	iny
	cmp #$40
	bcs :+
		; Number of empty rows
		sta @blockSize
		lda @phraseSize
		inc a
		sec
		sbc @blockSize
		sta @phraseSize
		@emptyPhraseRowLoop:
			lda #$ff
			sta f:StorageBuffer,X
			sta f:StorageBuffer+3,X
			lda #0
			sta f:StorageBuffer+1,X
			sta f:StorageBuffer+2,X
			inx
			inx
			inx
			inx
			dec @blockSize
		bne @emptyPhraseRowLoop
		bra @phraseBlockLooop
	:
		bit #%10000000
		beq @hasNoNote
		@hasNote:
			bit #%01000000
			beq :+
				; Has note and command
				and #$3f
				sta f:StorageBuffer+1,X
				lda [@sourcePointer],Y
				iny
				sta f:StorageBuffer+2,X
				lda [@sourcePointer],Y
				iny
				bra :++
			:
				; Note but no command
				and #$3f
				:
				sta f:StorageBuffer+0,X
				lda [@sourcePointer],Y
				iny
				sta f:StorageBuffer+3,X
				cmp #$fe
				bne :+
					; $FE (cut note) instrument is always $FF (no instrument)
					lda #$ff
					sta f:StorageBuffer+0,X
				:
				bra @rowEnd
		@hasNoNote:
			bit #%01000000
			beq @rowEnd
				; Command but no note
				and #$3f
				sta f:StorageBuffer+1,X
				lda [@sourcePointer],Y
				iny
				sta f:StorageBuffer+2,X
	@rowEnd:
	inx
	inx
	inx
	inx
jmp @phraseBlockLooop

@FixLoadedChainData:
	; Takes the RLE-friendly stored chain and rewrites it into the interleaved data format we're using for some reason
	@chainOffset .set 0
	.repeat 16
		lda f:StorageBuffer-$20+@chainOffset,X
		sta ChainBuffer+@chainOffset
		@chainOffset .set @chainOffset+1
	.endrepeat
	@chainOffset .set 0
	.repeat 16
		lda f:StorageBuffer-$10+@chainOffset,X
		sta f:StorageBuffer-$20+1+(@chainOffset*2),X
		@chainOffset .set @chainOffset+1
	.endrepeat
	@chainOffset .set 0
	.repeat 16
		lda ChainBuffer+@chainOffset
		sta f:StorageBuffer-$20+0+(@chainOffset*2),X
		@chainOffset .set @chainOffset+1
	.endrepeat		
rts
	
@ProcessNextBlock:
	seta16
	lda @segmentSize
	seta8
	bpl :+
		BRK ; If value ever underflows to a negative value, something has gone wrong
	:
	bne :+
		rts ; When it hits 0, we're done with the segment
	:
@RleDecompress:
	lda [@sourcePointer],Y
	bpl @diffValues
	@repeatValues:
		iny
		and #$7f
		sta @blockSize
		seta16
		lda @segmentSize
		sec
		sbc @blockSize
		sta @segmentSize
		seta8
		lda [@sourcePointer],Y
		iny
		:
			sta f:StorageBuffer,X
			inx
			dec @blockSize
		bne :-
		bra @ProcessNextBlock
	@diffValues:
		iny
		sta @blockSize
		seta16
		lda @segmentSize
		sec
		sbc @blockSize
		sta @segmentSize
		seta8
		:
			lda [@sourcePointer],Y
			iny
			sta f:StorageBuffer,X
			inx
			dec @blockSize
		bne :-
		bra @ProcessNextBlock
		