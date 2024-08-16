.include "global.inc"
.include "src/snes.inc"
.smart

.importzp BrewsicTransferDestination
.import BrewsicPlayTrack, BrewsicTransfer, BrewsicPlaySound, BrewsicStopTrack

.export PrepareTestPatternPlayback, StartTestPatternPlayback, UpdateNoteInPlayback, NoteDataOffsetInPhrase
.export StopPlayback = BrewsicStopTrack
.export SwitchToSingleNoteMode, TransferSingleNoteToSpcAndPlay


.segment "BSS"
IsPlaying: .res 1
QueuePreparePlayback: .res 1

; TODO: Max of 16 possible phrases at a time when continuous pattern transfer is implemented
; Maybe two lists, so it's quicker to erase the old one when buffering new phrase patterns
PatternOffsetReferences: .res (3 * $ff)+1 ; $FF possible phrases, 3 bytes per entry, 1 added byte for $FF in the very end
NoteDataOffsetInPhrase: .res 1

.segment "ZEROPAGE"
PatternToFind: .res 2
BarCounter = PatternToFind ; Reuse this variable since the two routines always run in sequence and will never conflict
CurrentCompileIndex: .res 2
CurrentPatternOffset: .res 2
EmptyPatternOffset: .res 2
PatternRowCounter: .res 2

SongRowOfChannel: .res $10


DELETEvariables: .res 6


.segment "CODE7"

PrepareTestPatternPlayback:
	lda IsPlaying
	sta QueuePreparePlayback ; If playing, don't prepare now, but enqueue until current playback ended to avoid conflicting with it
	bne :+
		jsr CopyTestPatternsToSpcBuffer
		ldy #$100
		jsr TransferPlaybackBufferToSpc
	:
rts

CopyTestPatternsToSpcBuffer:
	ldx #0
	:
		lda f:TestPatternSource,X
		sta f:CompiledPattern,X
		inx
		cpx #Pattern16Index+18
	bne :-
rts

StartTestPatternPlayback:
	jsl CompilePatternToBuffer
jmp TransferEntirePlaybackBufferToSpcAndPlay

SwitchToSingleNoteMode:

	;lda QueuePreparePlayback
	;beq :+
		jsr PrepareTestPatternPlayback ; Always prepare test pattern playback because full song could have been playing
	;:

	; Switch to playing single note pattern
	seta16
	lda #Pattern01Index
	sta f:CompiledPattern+PatternReferenceOffset

	lda #SilentPatternIndex
	sta f:CompiledPattern+PatternReferenceOffset+2
	sta f:CompiledPattern+PatternReferenceOffset+4
	sta f:CompiledPattern+PatternReferenceOffset+6
	sta f:CompiledPattern+PatternReferenceOffset+8
	sta f:CompiledPattern+PatternReferenceOffset+10
	sta f:CompiledPattern+PatternReferenceOffset+12
	sta f:CompiledPattern+PatternReferenceOffset+14
	
	lda #$ffff
	seta8
	
	jsr TransferPatternReferencesToSpc
rts

PlayFullSong:
	jsl CopyCurrentSongToSpcBuffer
jmp TransferEntirePlaybackBufferToSpcAndPlay

TransferPatternReferencesToSpc:
	ldx #PatternReferenceOffset
	ldy #4
jmp spcTransfer
TransferSingleNoteToSpcAndPlay:
	ldx #Pattern01Offset
	ldy #8
	jsr spcTransfer
	lda #0
jmp BrewsicPlayTrack
TransferEntirePlaybackBufferToSpcAndPlay:
	seta16
	lda CurrentPatternOffset
	clc
	adc #18
	tay
	seta8
	jsr TransferPlaybackBufferToSpc
	lda #0
jmp BrewsicPlayTrack
TransferPlaybackBufferToSpc: ; Requires size to be set in Y ahead of time
	ldx #0
jmp spcTransfer

; X = Offset to transfer from
; Y = Bytes to transfer
spcTransfer:
@patternDataStartInSpcMemory = $6098 ; Needs to be dynamic, determined after transfer of samples
	
	seta16
	txa
	clc
	adc #@patternDataStartInSpcMemory
	sta z:BrewsicTransferDestination ; Destination in SPC memory

	txa
	clc
	adc #.loword(CompiledPattern)
	tax ; X = Source start to copy from
	seta8
	lda #^CompiledPattern
	;ldy #$100 ; Y = Size
	
	; Disable NMI interrupts to avoid transfer being stalled by NMI.
	; Will cause issues because the SPC code always assumes the CPU code is faster, and doesn't wait for data
	stz PPUNMI
	jsr BrewsicTransfer
	lda #VBLANK_NMI|AUTOREAD
	sta PPUNMI
rts


UpdateNoteInPlayback:
	sta PatternToFind
	ldx #0
	@patternFindLoop:
		lda PatternOffsetReferences,X
		cmp #$ff
		bne :+
			; Reached end. Not found. Do nothing
			rts
		:
		cmp PatternToFind
		bne :+
			; Found pattern. Update only relevant bytes
			clc
			lda NoteDataOffsetInPhrase
			seta16
			and #$00ff
			adc PatternOffsetReferences+1,X
			inc ; Skip the pattern's header byte
			tax
			adc #18
			pha ; need full index for SPC transfer later
			seta8
			jsl CompileSingleNoteBar
			; Transfer the updated bytes to SPC as soon as possible
			plx ; Start offset
			ldy #4 ; Size
			jmp spcTransfer
			; Returns
		:
		inx
		inx
		inx
	bra @patternFindLoop



.segment "CODE6"

CompilePatternToBuffer:	
	
	;Update reference list to play back changes while phrase is edited
	sta f:PatternOffsetReferences
	lda #$ff
	sta f:PatternOffsetReferences+3

	phb
	lda #^PHRASES
	pha
	plb
	
	; Switch test pattern's reference list to playing full phrase
	seta16
	lda #Pattern16Index
	sta f:PatternOffsetReferences+1
	sta f:CompiledPattern+PatternReferenceOffset
	
	; Load phrase into test pattern
	tax
	jsr CompileSinglePattern
	
	lda #$0002 ; Insert loop-back commands on final bar ; TODO: This overrides any commands inserted by user, so expand the pattern-length data to have a loop feature instead
	sta f:CompiledPattern+18-3,X ; Edits the last bar in data
	seta8
	
	plb
rtl

CopyCurrentSongToSpcBuffer:
; TODO: Buffer song in parts, frame-by-frame ahead of pushing play, to decrease wait time
; TODO: Move direct page around to optimize performance
@ChainOffsetOfChannel = $00 ; 16 bytes
@PhraseOfChannel = $10 ; 16 bytes

@instrumentSize = CurrentPatternOffset ; instrsize variable only used temporarily before we know the pattern offset

; Below are used for a calculation of values that will be static 
@currentChannelLength = DELETEvariables
@orderReferenceSize = DELETEvariables+2
@orderLengthSize = DELETEvariables+4


	phb
	lda #^SONG ; Song and chains in same bank
	pha
	plb
	
	lda #$ff
	sta f:PatternOffsetReferences ; $FF indicates the first empty entry, just move the $FF every time an entry is added to prevent looping through the whole thing
	; TODO: Just leave room for two orders, buffer the first first "order", and buffer one every time one 16-bar order is about to end for continuous play
	seta16
	stz @orderLengthSize

	; Detect the channel with the longest series of chain rows playing before looping the entire channel
	ldx #0
	@channelLoopX:
		stz @currentChannelLength
		phx

		txa
		xba
		tay
		sty SongRowOfChannel
		@songRowLoopX:
			lda SONG,Y
			and #$ff
			cmp #$ff
			beq @endChannel
			
			asl
			asl
			asl
			asl
			asl
			tay
			ldx #16
			:
				lda CHAINS,Y
				and #$ff
				cmp #$ff
				beq @endChain
				inc @currentChannelLength
				iny
				iny
				dex
			bne :-
			@endChain:
			inc SongRowOfChannel
			lda SongRowOfChannel
			tay
			and #$ff
		bne @songRowLoopX
	
		@endChannel:
		lda @currentChannelLength
		cmp @orderLengthSize
		bcc :+
			sta @orderLengthSize
		:
		
		plx
		inx
		cpx #8
	bne @channelLoopX
	
	
	lda @orderLengthSize
	sta PatternRowCounter
	asl
	asl
	asl
	asl ; Results in 16, 32, 64, etc
	sta @orderReferenceSize
	
	; TODO: use available instruments
	lda #8*6
	sta @instrumentSize

	; Add up all our numbers to find the index where pattern data starts	
	lda #2+1+2 ; all constant numbers from below added up
	clc
	adc @orderReferenceSize
	;adc #2 ; $ffff at the end
	adc @orderLengthSize
	;adc #1 ; $00 $at the end
	;adc #2; $ffff for empty macro dir
	adc @instrumentSize
	
	sta CurrentPatternOffset
	jsr @AddEmptyPattern

	; HEADER
	; TODO: Actually generate 18 byte header based on song settings
	ldx #0
	:
		lda f:TestPatternSource,X
		sta f:CompiledPattern,X
		inx
		cpx #18 ; Header length
	bne :-
	stx CurrentCompileIndex

	; PATTERN REFERENCES
	ldx #0
	:
		jsr @InitiateChannelIndexes
		inx
		inx
		cpx #16
	bne :-
	
	lda PatternRowCounter
	bne :+
		; Empty song
		jmp @endSongLoop
	:
	@rowLoop:
	
		ldx #0
		@channelLoop:
			ldy @ChainOffsetOfChannel,X
			bpl :+
				; Skip entire channel
				lda #$ff
				sta @PhraseOfChannel,X
				jmp @nextChannel
			:
			lda CHAINS,Y
			and #$ff
			cmp #$ff
			; TODO: !!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!! ALSO MOVE TO NEXT SONG ROW AFTER 16 CHAIN ROWS
			bne :++
				; Move to next song row
				inc SongRowOfChannel,X
				ldy SongRowOfChannel,X
				lda SONG,Y
				and #$ff
				cmp #$ff
				bne :+
					; Loop entire channel
					jsr @InitiateChannelIndexes
					bra @channelLoop
				:
				asl
				asl
				asl
				asl
				asl
				sta @ChainOffsetOfChannel,X
				bra @channelLoop
			:
			sta @PhraseOfChannel,X
			inc @ChainOffsetOfChannel,X
			inc @ChainOffsetOfChannel,X
			
			@nextChannel:
			inx
			inx
			cpx #16
		bne @channelLoop
			
		phb
		seta8
		lda #^PHRASES
		pha
		seta16
		plb
		lda @PhraseOfChannel+0
		jsr @AddPhraseToIndex
		lda @PhraseOfChannel+2
		jsr @AddPhraseToIndex
		lda @PhraseOfChannel+4
		jsr @AddPhraseToIndex
		lda @PhraseOfChannel+6
		jsr @AddPhraseToIndex
		lda @PhraseOfChannel+8
		jsr @AddPhraseToIndex
		lda @PhraseOfChannel+10
		jsr @AddPhraseToIndex
		lda @PhraseOfChannel+12
		jsr @AddPhraseToIndex
		lda @PhraseOfChannel+14
		jsr @AddPhraseToIndex
		plb

		dec PatternRowCounter
		beq @endSongLoop
	bne @rowLoop
		
	@endSongLoop:
	
	ldx CurrentCompileIndex
	lda #$ffff ; End of pattern refs
	sta f:CompiledPattern,X
	inx
	inx
	lda #16 ; This tracker only makes 16-bar patterns
	ldy @orderLengthSize
	:
		sta f:CompiledPattern,X
		inx
		dey
	bne :-
	lda #0 ; End of pattern lengths
	sta f:CompiledPattern,X
	inx
	lda #$ffff ; Empty macro table
	sta f:CompiledPattern,X
	inx
	inx
	
	ldy #0 ; 6 hardcoded instruments so far
	:
;$01 ; sample
;$FFF7 ; pitch adjust
;$00 ; fadeout
;$A0 ; volume
;$0000 ; volume envelope address
;0 ; unused
		phx
		tyx
		lda f:DELETEtestinstruments,X
		plx
		and #$00FF
		ora #$F700
		sta f:CompiledPattern+0,X
		lda #$00FF
		sta f:CompiledPattern+2,X
		lda #$00A0
		sta f:CompiledPattern+4,X
		lda #$0000
		sta f:CompiledPattern+6,X
		txa
		clc
		adc #8
		tax
		iny
		cpy #6
	bne :-
	
	stx CurrentCompileIndex
	
	lda CurrentCompileIndex
	sec
	sbc #18 ; subtract header
	cmp EmptyPatternOffset
	beq :+
		; Something went wrong. Our size estimate didn't match what was actually output
		brk
	:
	
	
	seta8
	
	plb
rtl

@InitiateChannelIndexes:
.a16
	txa
	xba
	lsr
	sta SongRowOfChannel,x
	tay	
	lda SONG,Y
	and #$ff
	cmp #$ff
	beq @silence
		asl
		asl
		asl
		asl
		asl
		sta @ChainOffsetOfChannel,x
		tay
		lda CHAINS,Y
		and #$ff
		cmp #$ff
		beq @silence
rts
	@silence:
		lda #$8000 ; Indicates silent channel
		sta @ChainOffsetOfChannel,x
rts

@AddPhraseToIndex:
.a16
	cmp #$ff
	bne :+
		; Load empty pattern reference
		lda EmptyPatternOffset
		bra :++
	:
		jsr @EnsurePatternOffset
	:
	ldx CurrentCompileIndex
	sta f:CompiledPattern,X
	inx
	inx
	stx CurrentCompileIndex
rts

@EnsurePatternOffset:
.a16
	sta PatternToFind
	seta8
	ldx #0
	@patternFindLoop:
		lda f:PatternOffsetReferences,X
		cmp #$ff
		bne :+
			; Reached end. Not found. Insert here, and move $FF end marker
			sta f:PatternOffsetReferences+3,X
			lda PatternToFind
			sta f:PatternOffsetReferences,X
			seta16
			lda CurrentPatternOffset
			sta f:PatternOffsetReferences+1,X
			pha
			lda PatternToFind
			jsr @AddPattern
			pla
			rts
			.a8
		:
		cmp PatternToFind
		bne :+
			; Found pattern already exported, get reference and reuse it
			seta16
			lda f:PatternOffsetReferences+1,X
			rts
			.a8
		:
		inx
		inx
		inx
	bra @patternFindLoop

@AddEmptyPattern:
.a16
	ldx CurrentPatternOffset
	stx EmptyPatternOffset
	
	lda #$80|16 ; 16 silent rows
	sta f:CompiledPattern+18,X ; Remember, when using the pattern offset, add 18 to account for header
	inx
	stx CurrentPatternOffset
rts
@AddPattern:
.a16
	asl
	asl
	asl
	asl
	asl
	asl
	tay

	ldx CurrentPatternOffset
	
CompileSinglePattern:
	lda #15
	sta f:CompiledPattern+18,X ; Remember, when using the pattern offset, add 18 to account for header
	inx

	inc
	sta BarCounter

	@patternLoop:
		lda PHRASES+2,Y
		sta f:CompiledPattern+18+2,X
		and #$ff00 ; Check if note value is $ff
		cmp #$ff00
		bne :+
			lda #$0080
			sta f:CompiledPattern+18,X ; Then store $80 in instrument byte
			bra :++
		:
			lda PHRASES+0,Y
			ora #$0080 ; Always command data (even when 0), ensures constant pattern size
			and #$00ff ; Unset command (TODO: If high byte is $FF)
			sta f:CompiledPattern+18,X
		:
		inx
		inx
		inx
		inx
		iny
		iny
		iny
		iny
		dec BarCounter
	bne @patternLoop
	stx CurrentPatternOffset
rts

CompileSingleNoteBar:
.a8
	phb
	lda #^PHRASES
	pha
	plb
	
seta16
	lda f:CompiledPattern+18+1,X
	pha

	; TODO: Should probably be macro instead of copied code
	lda PHRASES+2,Y
	sta f:CompiledPattern+18+2,X
	and #$ff00 ; Check if note value is $ff
	cmp #$ff00
	bne :+
		lda #$0080
		sta f:CompiledPattern+18,X ; Then store $80 in instrument byte
		bra :++
	:
		lda PHRASES+0,Y
		ora #$0080 ; Always command data (even when 0), ensures constant pattern size
		and #$00ff ; Unset command (TODO: If high byte is $FF)
		sta f:CompiledPattern+18,X
	:

	; Just in case we overwrote the loop command, put it back in
	;lda #$0002 ; Insert loop-back commands on final bar ; TODO: This overrides any commands inserted by user, so expand the pattern-length data to have a loop feature instead
	pla
	cmp #$0002
	bne :+
		sta f:CompiledPattern+18+1,X
	:
seta8

	plb
rtl

.a8


DELETEtestinstruments:
.byte 1,2,3,5,6,0 ; Preprogrammed instrument sample references. Delete when instrument editing becomes a thing
TestPatternSource:
; Preprogrammed pattern used to test single phrase or play a single note

Pattern01Offset = test_pattern_single_note - TestPatternSource
PatternReferenceOffset = test_pattern_trackindex_start - TestPatternSource

Pattern16Index = test_pattern_16_bars - test_pattern_trackindex_start
Pattern01Index = test_pattern_single_note - test_pattern_trackindex_start
SilentPatternIndex = test_pattern_silent - test_pattern_trackindex_start

;HEADER
.byte $06,$50, $60,$20, $60,$20, $60,$20, $60,$20, $60,$20, $60,$20, $60,$20, $60,$20

;ORDERS
test_pattern_trackindex_start:
.addr Pattern01Index,SilentPatternIndex,SilentPatternIndex,SilentPatternIndex,SilentPatternIndex,SilentPatternIndex,SilentPatternIndex,SilentPatternIndex
.addr $FFFF ; ORDER LIST END

;PATTERN LENGTH TABLE (number of rows per pattern)
.byte 16
.byte $00 ; END

;MACRO DIRECTORY
.addr $FFFF ; END

;INSTRUMENTS
.byte $01 ; sample
.word $FFF7 ; pitch adjust
.byte $00 ; fadeout
.byte $A0 ; volume
.addr $0000 ; volume envelope address
.byte 0 ; unused

.byte $02 ; sample
.word $FFF7 ; pitch adjust
.byte $00 ; fadeout
.byte $A0 ; volume
.addr $0000 ; volume envelope address
.byte 0 ; unused

.byte $03 ; sample
.word $FFF7 ; pitch adjust
.byte $00 ; fadeout
.byte $A0 ; volume
.addr $0000 ; volume envelope address
.byte 0 ; unused

.byte $05 ; sample
.word $FFF7 ; pitch adjust
.byte $00 ; fadeout
.byte $A0 ; volume
.addr $0000 ; volume envelope address
.byte 0 ; unused

.byte $06 ; sample
.word $FFF7 ; pitch adjust
.byte $00 ; fadeout
.byte $A0 ; volume
.addr $0000 ; volume envelope address
.byte 0 ; unused

.byte $00 ; sample
.word $FFF7 ; pitch adjust
.byte $00 ; fadeout
.byte $A0 ; volume
.addr $0000 ; volume envelope address
.byte 0 ; unused


;SILENT PATTERN
test_pattern_silent: .byte $80|16 ; 16 silent rows

test_pattern_single_note:
.byte 0; Block header: 1 bar (1 minus 1)
.byte $00 ; Insert instrument here
.byte $00 ; Insert note here
.byte $80|15 ; 15 silent rows

;MY PATTERN
test_pattern_16_bars:
; Instrument 60 = cut, 61 = fase, 62 = off, 63 = nothing happens, 54-59 = macros
.byte 15 ; Block header: Number of bars minus 1
.res 4 * 16 ; Each bar is at most 4 bytes if command is present, so for random access we just add a "null" command to every bar

;.byte $00 ; Instrument 0-52
;.byte 12*4 ; Note byte
;.byte 63 ; silence
;.byte $00 ; Instrument 0-52
;.byte 12*4+1 ; Note byte
;.byte 63 ; silence
