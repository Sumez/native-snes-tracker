.include "global.inc"
.include "src/snes.inc"
.smart

.importzp BrewsicTransferDestination
.import BrewsicPlayTrack, BrewsicTransfer, BrewsicPlaySound, BrewsicStopTrack
.import Song_UpdateHighlight, Chain_UpdateHighlight, Pattern_UpdateBeatHighlight

.export Playback_Update = Update
.export PrepareTestPatternPlayback, UpdateNoteInPlayback, NoteDataOffsetInPhrase
.export PlaySinglePhrase, PlaySingleChain
.export StopPlayback
.export SwitchToSingleNoteMode, TransferSingleNoteToSpcAndPlay


.segment "BSS"
IsPlaying: .res 1
QueuePreparePlayback: .res 1
TrackDataAddressInSpc: .res 2

; TODO: Max of 16 possible phrases at a time when continuous pattern transfer is implemented
; Maybe two lists, so it's quicker to erase the old one when buffering new phrase patterns
PatternOffsetReferences: .res (3 * 8)+1 ; 8 possible phrases on a single song row, 3 bytes per entry, 1 added byte for $FF in the very end
PatternOffsetReferencesAlternating: .res (3 * 8)+1 ; Alternating table to use, to avoid overriding patterns currently played (probably not necessary, but just to be safe)
NoteDataOffsetInPhrase: .res 1

Playback_CurrentBeatRow: .res 1
Playback_CurrentPhraseOfChannel: .res 8 ; Keep this in memory until bar 0 of the next phrase, to avoid referencing the currently *buffered* phrase
;Playback_CurrentChainRowOfChannel: .res 8
Playback_CurrentSongRowOfChannel = SongRowOfChannel

.segment "ZEROPAGE"
PatternToFind: .res 2
BarCounter = PatternToFind ; Reuse this variable since the two routines always run in sequence and will never conflict
CurrentCompileIndex: .res 2
CurrentPatternOffset: .res 2
EmptyPatternOffset: .res 2
FirstRowPatternOffset: .res 2
SecondRowPatternOffset: .res 2
SecondRowPatternEnd: .res 2
PatternIndexOffset: .res 2

SongRowOfChannel: .res $10
ChainOffsetOfChannel: .res $10 ; row that will be read when NEXT phrase starts
PhraseOfChannel: .res $10
Playback_CurrentChainOffsetOfChannel: .res $10 ; Caches the last actually read row
PatternOffsetTablePointer: .res 2

.segment "CODE7"

Update:
@barsRemaining = 0

	lda IsPlaying
	beq @return

	ldx APU2
	dex ; If still playing, and on the last bar, APU2 will read 1, and APU3 will read 0, so one DEX will reach zero
	stx @barsRemaining ; 0-F
	lda #$0F
	sec
	sbc @barsRemaining
	cmp Playback_CurrentBeatRow
	beq @return
		; Row changed
		sta Playback_CurrentBeatRow
		lda @barsRemaining
		bne :+
		
			; Last bar of phrase. Compile and push next phrases to APU memory, unless just looping a phrase
			bit IsPlaying ; if bit 7 is 1, either a song or phrase is playing
			bpl :+
				jsr PrepareNextRowInSong

		:

		lda Playback_CurrentBeatRow
		bne :+
			; First row. Update indicator of active phrase or chain on chain or song views here
			; Copy buffered phrase indexes to active phrase indexes
			ldx #0
			txy
			@smallLoop:
				lda PhraseOfChannel+0,x
				sta Playback_CurrentPhraseOfChannel+0,y
				inx
				inx
				iny
				cpy #8
			bne @smallLoop
			
			jsr Song_UpdateHighlight
			jsr Chain_UpdateHighlight
		:
		jsr Pattern_UpdateBeatHighlight ; Update pattern highlight every beat


		;lda Playback_CurrentBeatRow
		;and #3
		;bne @return
		;	; Every 0, 4, 8, 12
		;	jsr Bop

	@return:
rts


PrepareTestPatternPlayback:
	; Prepare a dummy test pattern so we can initiate a single note playing as quickly as possible
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

PlaySinglePhrase:
	phx
	;Update the "current phrase" for playback visual feedback
	sta PhraseOfChannel
	;Also update reference list to play back changes while phrase is edited
	sta PatternOffsetReferences
	ldy #0
	sty PatternOffsetTablePointer ; And make sure we know to just look in the first table
	lda #$ff
	sta PatternOffsetReferences+3 ; End of offset refs
	
	sta PhraseOfChannel+2 ; Nothing on this channel
	sta PhraseOfChannel+4 ; etc.
	sta PhraseOfChannel+6
	sta PhraseOfChannel+8
	sta PhraseOfChannel+10
	sta PhraseOfChannel+12
	sta PhraseOfChannel+14

	lda #2 ; 2 indicates playing 1 pattern looped
	sta IsPlaying
	ldx #$ffff
	jsr HighlightRow ; Highlight no rows until a view decides to
	plx
	jsl CompilePhraseToBuffer
	jsr TransferEntirePlaybackBufferToSpcAndPlay
rts

PlaySingleChain:
	phx ; Chain offset to play. Need to get this later
	
	lda #$ff ; First tell chain view we're not playing any rows in here
	sta Playback_CurrentChainOffsetOfChannel+1
	
	lda #$80 ; $ff indicates playing full song is playing. $80 indicates just looping one chain
	sta IsPlaying
	ldx #$ffff
	jsr HighlightRow ; Highlight no rows until a view decides to

	; Initialize chains with row requested by caller ([X])
	seta16
	pla
	ora #$8000
	dec ; Decrease by one, as songrow is always increased by 1 as the first thing in a channel loop
	sta z:SongRowOfChannel+0 ; Special command in SongRow indicates always playing this ChainOffset
	stz z:ChainOffsetOfChannel+0 ; Forces "CompileSongRowToBuffer" to load the next song row first
	lda #$ffff
	sta z:ChainOffsetOfChannel+2 ; Silences other channels
	sta z:ChainOffsetOfChannel+4
	sta z:ChainOffsetOfChannel+6
	sta z:ChainOffsetOfChannel+8
	sta z:ChainOffsetOfChannel+10
	sta z:ChainOffsetOfChannel+12
	sta z:ChainOffsetOfChannel+14
	seta8

	jsl CopyCurrentSongToSpcBuffer
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

PrepareNextRowInSong:
	jsl CompileNextRowInSong
	ldy SecondRowPatternEnd ; TODO: Instead of transfering everything, break it into small bits to prevent halting the song for a full 1+ frame
jmp TransferPlaybackBufferToSpc

StopPlayback:
	jsr BrewsicStopTrack
	lda IsPlaying
	beq :+
		stz IsPlaying
		jmp (OnPlaybackStopped)
	:
rts
			
PlayFullSong:
@rowToStartPlayingFrom = 0 ; 2 bytes
	sta z:@rowToStartPlayingFrom
	stz z:@rowToStartPlayingFrom+1
	
	lda #$ff ; $ff indicates playing full song is playing. $80 indicates just looping one chain
	sta IsPlaying
	ldx #$ffff
	jsr HighlightRow ; Highlight no rows until a view decides to

	; Initialize chains with row 0 of each channel in song
	seta16
	ldy #0
	:
		tya
		xba
		lsr
		ora z:@rowToStartPlayingFrom

		tax
		dex ; channel progress always starts at the "end" of a chain, so decrease X once, so when the next song row is loaded, it will be the first
		stx z:SongRowOfChannel,Y
		ldx #0 ; Forces "CompileSongRowToBuffer" to load the next song row first
		stx z:ChainOffsetOfChannel,Y

		iny
		iny
		cpy #16
	bne :-
	seta8

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
;	lda #1 ; #1 indicated playing one note
;	sta IsPlaying
	lda #0
jmp BrewsicPlayTrack
TransferEntirePlaybackBufferToSpcAndPlay:
lda #$ff
sta Playback_CurrentBeatRow ; TODO: Initiate all three Arrow data values depending on how playback was started
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
	
	seta16
	txa
	clc
	adc TrackDataAddressInSpc
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


UpdateNoteInPlayback: ; !!! Needs to preserve [X] into CompileSingleNoteBar

	sta PatternToFind

	ldy PatternOffsetTablePointer ; Only need to look in the latest buffered row
	@patternFindLoop:
		lda PatternOffsetReferences,Y
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
			adc PatternOffsetReferences+1,Y
			inc ; Skip the pattern's header byte
			tay ; Tells compile code where to write to in buffer
			adc #18
			pha ; need full index for SPC transfer later
			seta8
			jsl CompileSingleNoteBar
			; Transfer the updated bytes to SPC as soon as possible
			plx ; Start offset
			ldy #4 ; Size
			jmp spcTransfer
		:
		iny
		iny
		iny
	bra @patternFindLoop



.segment "CODE6"

CompilePhraseToBuffer:	; !!! Needs to preserve [X] into CompileSinglePattern
	
	phb
	lda #^CompiledPattern
	pha
	plb
	
	; Switch test pattern's reference list to playing full phrase
	seta16
	lda #Pattern16Index
	sta f:PatternOffsetReferences+1
	sta f:CompiledPattern+PatternReferenceOffset
	
	; Load phrase into test pattern
	tay
	jsr CompileSinglePhrase
	
	seta8
	plb
rtl

CompileNextRowInSong:

	phb
	lda #^CompiledPattern
	pha
	plb
	seta16

	lda z:PatternOffsetTablePointer
	beq :+
		; 3rd, 5th, 7th row...
		lda #$ff
		sta f:PatternOffsetReferences
		stz z:PatternOffsetTablePointer ; Toggle between 0, and the offset to the alternating table, on alternating rows
		lda z:PatternIndexOffset
		sta z:CurrentCompileIndex
		lda z:FirstRowPatternOffset ; Toggle between first pattern offset, and one the exact size of 8 patterns later, on alternating rows
		sta z:CurrentPatternOffset
		jsr CompileSongRowToBuffer
		bra :++
	:
		; 2nd, 4th, 6th row...
		lda #$ff
		sta f:PatternOffsetReferencesAlternating
		lda #.loword(PatternOffsetReferencesAlternating - PatternOffsetReferences)
		sta z:PatternOffsetTablePointer
		lda z:PatternIndexOffset
			clc
			adc #16
		sta z:CurrentCompileIndex
		lda z:SecondRowPatternOffset
		sta z:CurrentPatternOffset
		jsr CompileSongRowToBuffer
	:
	seta8
	plb
rtl

CopyCurrentSongToSpcBuffer:
@instrumentSize = 0

	phb
	lda #^CompiledPattern
	pha
	plb
	
	lda #$ff
	sta f:PatternOffsetReferences ; $FF indicates the first empty entry, just move the $FF every time an entry is added to prevent looping through the whole thing
	sta f:PatternOffsetReferencesAlternating ; $FF indicates the first empty entry, just move the $FF every time an entry is added to prevent looping through the whole thing
	seta16

	; TODO: use available instruments
	lda #8*6
	sta @instrumentSize

	; Add up all our numbers to find the index where pattern data starts	
	lda #32+2+2+1+2 ; all constant numbers from below added up
	clc
	;adc #32 ; 16 order references (two rows of 8 channels)
	;adc #2 ; $ffff at the end
	;adc #2 ; two rows looping forever
	;adc #1 ; $00 $at the end
	;adc #2; $ffff for empty macro dir
	adc @instrumentSize
	
	tay
	jsr AddEmptyPattern

	; HEADER
	; TODO: Actually generate 18 byte header based on song settings
	ldx #0
	:
		lda f:TestPatternSource,X
		sta CompiledPattern,X
		inx
		cpx #18 ; Header length
	bne :-
	stx z:PatternIndexOffset

	; PATTERN REFERENCES
	lda z:PatternIndexOffset
	sta z:CurrentCompileIndex
	lda z:FirstRowPatternOffset ; Toggle between first pattern offset, and one the exact size of 8 patterns later, on alternating rows
	sta z:CurrentPatternOffset
	clc
	adc #(4*16+1)*8 ; Next pattern offset should be at least the size of 8 uncompressed patterns later
	sta z:SecondRowPatternOffset
	adc #(4*16+1)*8
	sta z:SecondRowPatternEnd
	stz z:PatternOffsetTablePointer ; Toggle between 0, and the offset to the alternating table, on alternating rows
	jsr CompileSongRowToBuffer
	
	
	ldy z:CurrentCompileIndex
	lda #0
	ldx #8 ; Leave room for the next song-row of pattern indexes
	:	sta CompiledPattern,Y ; Can be anything other than $FFFF, really
		iny
		iny
		dex
	bne :-
	lda #$ffff ; End of pattern refs
	sta CompiledPattern,Y
	iny
	iny
	lda #16 ; This tracker only makes 16-bar patterns
	sta CompiledPattern,Y
	iny
	sta CompiledPattern,Y
	iny
	lda #0 ; End of pattern lengths
	sta CompiledPattern,Y
	iny
	lda #$ffff ; Empty macro table
	sta CompiledPattern,Y
	iny
	iny
	
	ldx #0 ; 6 hardcoded instruments so far
	:
;$01 ; sample
;$FFF7 ; pitch adjust
;$00 ; fadeout
;$A0 ; volume
;$0000 ; volume envelope address
;0 ; unused
		txa
		ora #$0000
		sta CompiledPattern+0,Y
		lda #$0000
		sta CompiledPattern+2,Y
		lda #$00A0
		sta CompiledPattern+4,Y
		lda #$0000
		sta CompiledPattern+6,Y
		tya
		clc
		adc #8
		tay
		inx
		cpx #6
	bne :-
	
	sty CurrentCompileIndex
	
	tya
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

InitiateChannelIndexes:
.a16
	tya
	xba
	lsr
	tax
	stx z:SongRowOfChannel,y
	lda f:SONG,X
	and #$ff
	cmp #$ff
	beq @silence
		asl
		asl
		asl
		asl
		asl
		tax
		stx z:ChainOffsetOfChannel,y
		lda f:CHAINS,X
		and #$ff
		cmp #$ff
		beq @silence
rts
	@silence:
		ldx #$8000 ; Indicates silent channel
		stx z:ChainOffsetOfChannel,y
rts

MoveChannelToNextRowOfSong:
	ldx z:SongRowOfChannel,Y
	inx
	bpl @storeChainIndex ; Max valid song row is $800, so a negative value indicated by the highest bit means loop from a fixed chain index instead the loaded song
		
		@customChainOffset:
		txa
		pha
		and #$FFE0 ; Discard index into current chain, so we loop from the start of the chain next time
		tax
		dex
		stx z:SongRowOfChannel,Y
		
		pla
		and #$7FFF ; realistically just a number from 0-$1FFE
		tax
		stx z:ChainOffsetOfChannel,Y
		lda f:CHAINS,X
		and #$ff
		cmp #$ff
		beq @silence
	rts
		@silence:
			ldx #$8000 ; Indicates silent channel
			stx z:ChainOffsetOfChannel,y
	rts

	@storeChainIndex:
		stx z:SongRowOfChannel,Y
		lda f:SONG,X
		and #$ff
		cmp #$ff
		bne :+
			; Loop entire channel
			jmp InitiateChannelIndexes
		:
		asl
		asl
		asl
		asl
		asl
		tax
		stx z:ChainOffsetOfChannel,Y
	rts

CompileSongRowToBuffer:
.a16
	
	ldy #0 ; Channel index (x2)
	@channelLoop:
		; First check if we need to load the next chain
		ldx z:ChainOffsetOfChannel,Y
		bmi @skipChannel ; Already figured out previously that channel is skipped (if it starts on a blank chain, or blank phrase in first chain)

		txa
@rowsInAChain = 16
		and #(@rowsInAChain-1)<<1 ; just the 00-1F index into the channel
		bne :+
			; On first row, or looped through an entire chain - Load next chain in queue for this channel
			jsr MoveChannelToNextRowOfSong
		
		@readChainRow:
		; Then get actual phrase to load
		ldx z:ChainOffsetOfChannel,Y
		bpl :+
			; Skip entire channel
			@skipChannel:
			ldx #$ffff
			stx z:Playback_CurrentChainOffsetOfChannel,Y
			stx z:PhraseOfChannel,Y
			bra @nextChannel
		:
		
		; Not skipping channel, load next entry in chain
		lda f:CHAINS,X
		and #$ff
		cmp #$ff
		bne :+
			; $FF means early end of chain, move to next row in song and restart channel loop
			jsr MoveChannelToNextRowOfSong
			bra @readChainRow
		:
		tax
		stx z:PhraseOfChannel,Y
		ldx z:ChainOffsetOfChannel,Y
		stx z:Playback_CurrentChainOffsetOfChannel,Y
		inx
		inx
		stx z:ChainOffsetOfChannel,Y
				
		@nextChannel:
		iny
		iny
		cpy #16
	bne @channelLoop
		
	lda z:PhraseOfChannel+0
	jsr @AddPhraseToIndex
	lda z:PhraseOfChannel+2
	jsr @AddPhraseToIndex
	lda z:PhraseOfChannel+4
	jsr @AddPhraseToIndex
	lda z:PhraseOfChannel+6
	jsr @AddPhraseToIndex
	lda z:PhraseOfChannel+8
	jsr @AddPhraseToIndex
	lda z:PhraseOfChannel+10
	jsr @AddPhraseToIndex
	lda z:PhraseOfChannel+12
	jsr @AddPhraseToIndex
	lda z:PhraseOfChannel+14
	jsr @AddPhraseToIndex
		
rts

@AddPhraseToIndex:
.a16
	and #$ff
	cmp #$ff
	bne :+
		; Load empty pattern reference
		lda EmptyPatternOffset
		bra :++
	:
		jsr @EnsurePatternOffset
	:
	ldy CurrentCompileIndex
	sta CompiledPattern,y
	iny
	iny
	sty CurrentCompileIndex
rts

@EnsurePatternOffset:
.a16
	sta PatternToFind
	seta8
	ldx z:PatternOffsetTablePointer
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


@AddPattern: ; [A] = Phrase index
.a16
	asl
	asl
	asl
	asl
	asl
	asl
	tax

	ldy CurrentPatternOffset
	
	
	CompileSinglePhrase:
	lda #15 ; block header byte (15 = 16 uncompressed bars)
	sta CompiledPattern+18,Y ; Remember, when using the pattern offset, add 18 to account for header
	iny

	inc
	sta BarCounter

	txa
	and #$2000
	bne :+
		; PHRASES_1 loop
;		txa
;		and #$1fff
;		tax
		@patternLoop1:
			jsr CopyPhraseFromBank1
			iny
			iny
			iny
			iny
			inx
			inx
			inx
			inx
			dec BarCounter
		bne @patternLoop1
		sty CurrentPatternOffset
	rts
	:
		; PHRASES_2 loop
		txa
		and #$1fff
		tax
		@patternLoop2:
			jsr CopyPhraseFromBank2
			iny
			iny
			iny
			iny
			inx
			inx
			inx
			inx
			dec BarCounter
		bne @patternLoop2
		sty CurrentPatternOffset
	rts
	
AddEmptyPattern:
.a16
	sty z:EmptyPatternOffset
	
	lda #$80|16 ; 16 silent rows
	sta CompiledPattern+18,Y ; Remember, when using the pattern offset, add 18 to account for header
	iny
	sty z:FirstRowPatternOffset ; Address after empty pattern is where all our song patterns will be added from
rts
	
.macro CopyNotesFromPhraseToCompiledPattern SOURCE
;+0 = instrument id, ;+1 = command id, ;+2 = command param, ;+3 = note
		lda f:SOURCE+2,X
		sta CompiledPattern+18+2,Y
		and #$ff00 ; Check if note value a note - or special command for tracker playback
		cmp #$fc00
		bcc :+
;			lda #$0080
			lda f:SOURCE+0,X
			and #$ff00
			ora #$0080 ; Always command data (even when 0), ensures constant pattern size
			sta CompiledPattern+18,Y ; Then store $80 in instrument byte
			bra :++
		:
			lda f:SOURCE+0,X
			ora #$0080 ; Always command data (even when 0), ensures constant pattern size
;			and #$00ff ; Unset command (TODO: If high byte is $FF)
			sta CompiledPattern+18,Y
		:
.endmacro
CopyPhraseFromBank1:
	CopyNotesFromPhraseToCompiledPattern PHRASES_1
rts
CopyPhraseFromBank2:
	CopyNotesFromPhraseToCompiledPattern PHRASES_2
rts

CompileSingleNoteBar:
.a8
	phb
	lda #^CompiledPattern
	pha
	plb
	
seta16

	txa ; X has index into PHRASES, $2000 is the boundary that determines bank 1 or 2
	and #$2000
	bne :+
		;txa
		;and #$1fff
		;tax
		jsr CopyPhraseFromBank1
		bra :++
	:
		txa
		and #$1fff
		tax
		jsr CopyPhraseFromBank2
	:

seta8

	plb
rtl

.a8

; !!!!!!!!!!!!!!!!!!!!!! TODO: Only use tiny bits of this, if any, dynamically add instruments using same code as full song and chain
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
.byte $00 ; sample
.word $0000 ; pitch adjust
.byte $00 ; fadeout
.byte $A0 ; volume
.addr $0000 ; volume envelope address
.byte 0 ; unused

.byte $01 ; sample
.word $0000 ; pitch adjust
.byte $00 ; fadeout
.byte $A0 ; volume
.addr $0000 ; volume envelope address
.byte 0 ; unused

.byte $02 ; sample
.word $0000 ; pitch adjust
.byte $00 ; fadeout
.byte $A0 ; volume
.addr $0000 ; volume envelope address
.byte 0 ; unused

.byte $03 ; sample
.word $0000 ; pitch adjust
.byte $00 ; fadeout
.byte $A0 ; volume
.addr $0000 ; volume envelope address
.byte 0 ; unused

.byte $04 ; sample
.word $0000 ; pitch adjust
.byte $00 ; fadeout
.byte $A0 ; volume
.addr $0000 ; volume envelope address
.byte 0 ; unused

.byte $05 ; sample
.word $0000 ; pitch adjust
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
