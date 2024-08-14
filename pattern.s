.include "global.inc"
.include "src/snes.inc"
.smart

.segment "BSS"

PatternBuffer = TilemapBuffer + $100
;Needs init:
Octaves: .res 10 ; Filled with 0->A
Notes: .res 12 ; Filed with C->B
NotesSharp: .res 12 ; Filed with # or -
CursorPositionCol: .res 2
CursorPositionRow: .res 2
LastEditedNote: .res 1
LastEditedInstrument: .res 1

;Loaded when view loads:
PatternNotes: .res 16
PatternInstruments: .res 16
PatternCommands: .res 16
PatternCommandParams: .res 16
CurrentPhraseIndexInGlobalSong: .res 2
QueuePreparePlayback: .res 1

.segment "CODE6"

.export Pattern_Init = Init
Init:
	ldx #0
	stx CursorPositionRow
	ldx #0
	stx CursorPositionCol
	stz IsPlaying
	
	lda #(12*4)
	sta LastEditedNote
	stz LastEditedInstrument
	
	jsl InitiateNoteTileReferences
rtl

.export Pattern_LoadView = LoadView
LoadView:
	tya
	cmp #$ff
	beq :+
		jsl LoadGlobalData
	:

	Bind Input_StartPlayback, StartPlayback
	Bind Input_CustomHandler, HandleInput
	Bind Input_NavigateIn, NoAction
	Bind Input_NavigateBack, ReturnToChainView
	Bind OnPlaybackStopped, SwitchToSingleNoteMode
	
	jsl PreparePlayback_long
	jsl WritePatternTilemapBuffer
	jsl ShowCursor_long
rts

LoadGlobalData:
	; Copy selected phrase data to temp memory
	seta16
	and #$00ff
	; Every Phrase is $40 bytes, so shift left 6 times (x64)
	asl
	asl
	asl
	asl
	asl
	asl
	tax
	seta8
	stx CurrentPhraseIndexInGlobalSong
	ldy #0
	@loop:
		lda f:PHRASES,x
		sta PatternInstruments,y
		lda f:PHRASES+1,x
		sta PatternCommands,y
		lda f:PHRASES+2,x
		sta PatternCommandParams,y
		lda f:PHRASES+3,x
		sta PatternNotes,y
		inx
		inx
		inx
		inx
		iny
		cpy #$10
	bne @loop	
rtl
UpdateGlobalData:
	; Copy temp memory to selected phrase
	; TODO: Update only currently selected bar
	ldx CurrentPhraseIndexInGlobalSong
	ldy #0
	@loop:
		lda PatternInstruments,y
		sta f:PHRASES,x
		lda PatternCommands,y
		sta f:PHRASES+1,x
		lda PatternCommandParams,y
		sta f:PHRASES+2,x
		lda PatternNotes,y
		sta f:PHRASES+3,x
		inx
		inx
		inx
		inx
		iny
		cpy #$10
	bne @loop
rtl

InitiateNoteTileReferences:
	lda #$40 ;0
	ldx #0
	:
		sta Octaves,x
		inc
		inx
		cpx #11
	bne :-
	lda #$02
	sta Notes+0
	sta Notes+1
	lda #$03
	sta Notes+2
	sta Notes+3
	lda #$04
	sta Notes+4
	lda #$05
	sta Notes+5
	sta Notes+6
	lda #$06
	sta Notes+7
	sta Notes+8
	lda #$00
	sta Notes+9
	sta Notes+10
	lda #$01
	sta Notes+11
	lda #$1C ; #
	sta NotesSharp+1
	sta NotesSharp+3
	sta NotesSharp+6
	sta NotesSharp+8
	sta NotesSharp+10
	lda #$3B ; -
	sta NotesSharp+0
	sta NotesSharp+2
	sta NotesSharp+4
	sta NotesSharp+5
	sta NotesSharp+7
	sta NotesSharp+9
	sta NotesSharp+11
rtl

TestData:
	lda #$ff
	ldx #15
	:
		sta PatternNotes,x
		dex
	bpl :-
	
	lda #$00
	ldx #7
	:
		sta PatternInstruments,x
		dex
	bpl :-
	lda #$02
	ldx #7
	:
		sta PatternInstruments+8,x
		dex
	bpl :-
	
	
	lda #12*6
	sta PatternNotes
	lda #12*6+1
	sta PatternNotes+2
	lda #12*6+2
	sta PatternNotes+4
	lda #12*6+3
	sta PatternNotes+6
	lda #12*6
	sta PatternNotes+8
	lda #12*6+1
	sta PatternNotes+10
	lda #12*6+2
	sta PatternNotes+12
	lda #12*6+3
	sta PatternNotes+14
rts

WritePatternTilemapBuffer:


@currentNote = 0
xba
lda #0 ; Ensure $00 in register B for TAYs
xba

ldx #0
ldy #0

@rowLoop:
	sty @currentNote
	
	;NOTES
	lda PatternNotes,Y
	cmp #$ff
	beq @noNote
	ldy #0
	sec
	:
		sbc #12
		bcc :+
			iny
			bra :-
	:
	adc #12 ; Carry always clear at this point due to the branch that took us here
	pha
	; now A=Note and Y=Octave
	lda Octaves,Y
	sta f:PatternBuffer+12,x
	pla
	tay
	lda Notes,Y
	sta f:PatternBuffer+8,x
	lda NotesSharp,Y
	sta f:PatternBuffer+10,x
	
	ldy @currentNote
	lda PatternInstruments,Y
	pha
	and #$F0
	lsr
	lsr
	lsr
	lsr
	ora #$40
	sta f:PatternBuffer+16,x
	pla
	and #$0F
	ora #$40
	sta f:PatternBuffer+18,x
	
	bra :+
	@noNote:
	
		;Note
		lda #$1d
		sta f:PatternBuffer+8,x
		lda #$1e
		sta f:PatternBuffer+10,x
		lda #$1f
		sta f:PatternBuffer+12,x
	
		;Instrument
		lda #$1d
		sta f:PatternBuffer+16,x
		lda #$1f
		sta f:PatternBuffer+18,x
	:
	
	
	;COMMANDS
		lda #$1d
		sta f:PatternBuffer+22,x
		lda #$1e
		sta f:PatternBuffer+24,x
		lda #$1f
		sta f:PatternBuffer+26,x
		
	seta16
	txa
	clc
	adc #$40
	tax
	lda #0
	seta8
	ldy @currentNote
	iny
	cpy #16

beq :+
jmp @rowLoop
:

rtl


.segment "CODE7"

NoteWasChanged:
	jsl WritePatternTilemapBuffer
	jsl UpdateGlobalData
rts

PlayCurrentNote:
	; Catch-all - if a single note is playing for any reason, it should be stored to be repeated anywhere a new note is inserted
	ldx CursorPositionRow
	lda PatternNotes,x
	sta LastEditedNote
	lda PatternInstruments,x
	sta LastEditedInstrument
	
	lda IsPlaying
	bne :+ ; If currently playing, just don't do anything
		jsr CompileCurrentNoteToBuffer
		jsr TransferSingleNoteToSpc
		lda #0
		jsr BrewsicPlayTrack
	:
rts
CutCurrentlyPlayingNote:
	lda IsPlaying
	bne :+ ; If currently playing, just don't do anything
		jsr BrewsicStopTrack
	:
rts

NoAction: rts
ReturnToChainView:
	lda #1
	ldy #$ff ; Reuse already loaded pattern
jmp NavigateToScreen

HandleInput:

	lda ButtonPushed
	bit #<KEY_X ; Key X removes current note
	beq :+

			ldx CursorPositionRow
			lda PatternNotes,x
			cmp #$ff
			beq :+ ; Don't do anything if bar is already empty
				; TODO: If KEYOFF, don't store as most recent
				
				sta LastEditedNote
				lda PatternInstruments,x
				sta LastEditedInstrument
				
				lda #$ff
				sta PatternNotes,x
				stz EditMode
				jsr NoteWasChanged
				jmp ShowCursor
				;RETURNS - no more inputs read this frame
	:
	
	lda ButtonStates+1 ; Key Y enters edit mode while held
	bit #>KEY_Y
	bne :+
		lda EditMode
		beq @continue
			stz EditMode
			jsr CutCurrentlyPlayingNote
			jsr ShowCursor
			bra @continue
	:
		lda EditMode
		bne @continue

			lda #1
			sta EditMode
			
			; Check if note exists, and if not, place the last one edited
			ldx CursorPositionRow
			lda PatternNotes,x
			cmp #$ff
			bne :+
				; No note exists. Use last edited
				lda LastEditedNote
				sta PatternNotes,x
				lda LastEditedInstrument
				sta PatternInstruments,x
				jsr NoteWasChanged
			:
			jsr PlayCurrentNote
			jsr ShowCursor

	@continue:
	
	; If in edit mode, branch to input relevant to that. if not, branch to internal navigation
	; If L button held, branch to global navigation (handle in editor.s?)
	lda EditMode
	beq @Navigation

@EditMode:

	lda ButtonPushed+1
	ldx CursorPositionCol
	bne @EditModeInstrumentCol

@EditModeNodeCol:
	bit #>KEY_DOWN
	beq :+
		lda #(256-12)
		jmp ChangeCurrentNote
	:
	bit #>KEY_UP
	beq :+
		lda #12
		jmp ChangeCurrentNote
	:
	bit #>KEY_LEFT
	beq :+
		lda #(256-1)
		jmp ChangeCurrentNote
	:
	bit #>KEY_RIGHT
	beq :+
		lda #1
		jmp ChangeCurrentNote
	:
rts

@EditModeInstrumentCol:
	bit #>KEY_DOWN
	beq :+
		lda #(256-$10)
		jmp ChangeCurrentInstrument
	:
	bit #>KEY_UP
	beq :+
		lda #$10
		jmp ChangeCurrentInstrument
	:
	bit #>KEY_LEFT
	beq :+
		lda #(256-1)
		jmp ChangeCurrentInstrument
	:
	bit #>KEY_RIGHT
	beq :+
		lda #1
		jmp ChangeCurrentInstrument
	:
rts

@Navigation:

	lda ButtonPushed+1
	bit #>KEY_DOWN
	beq :+
		jmp MoveCursorDown
	:
	bit #>KEY_UP
	beq :+
		jmp MoveCursorUp
	:
	
	bit #>KEY_LEFT
	beq :+
		jmp SetNoteCol
	:
	bit #>KEY_RIGHT
	beq :+
		jmp SetInstrumentCol
	:
	
	
rts

ChangeCurrentNote:
	ldx CursorPositionRow
	clc
	adc PatternNotes,x
	bmi :+
	cmp #12*10
	bcs :+
		sta PatternNotes,x
		jsr NoteWasChanged
	:
jmp PlayCurrentNote

ChangeCurrentInstrument:
	ldx CursorPositionRow
	clc
	adc PatternInstruments,x
	bpl :+
		lda #0
		bra :++
	:
	cmp #6 ; TODO: Number of instruments
	bcc :+
		lda #6
		dec
	:
	sta PatternInstruments,x
	jsr NoteWasChanged
jmp PlayCurrentNote

MoveCursorDown:
	lda CursorPositionRow
	inc
	sta CursorPositionRow
	cmp #$10
	bne :+
		; TODO: Move down in current chain
		stz CursorPositionRow
	:
jmp ShowCursor

MoveCursorUp:
	dec CursorPositionRow
	bpl :+
		; TODO: Move up in current chain
		lda #$f
		sta CursorPositionRow
	:
jmp ShowCursor

SetNoteCol:
	stz CursorPositionCol
jmp ShowCursor

SetInstrumentCol:
	lda #1
	sta CursorPositionCol
jmp ShowCursor

ShowCursor_long: jsr ShowCursor
rtl
ShowCursor:

	lda CursorPositionRow
	clc
	adc #4
	sta CursorY
	lda CursorPositionCol
	bne :+
		lda #4
		sta CursorX
		lda #1
		bra :++
	:
		lda #8
		sta CursorX
		lda #0
	:
	sta CursorSize
	
jmp UpdateCursorSpriteAndHighlight

PreparePlayback_long: jsr PreparePlayback
rtl
PreparePlayback:
	lda IsPlaying
	sta QueuePreparePlayback ; If playing, don't prepare now, but enqueue until current playback ended to avoid conflicting with it
	bne :+
		jsr CopyTestPatternToSpcBuffer
		jsr TransferEntirePlaybackBufferToSpc
	:
rts

.import BrewsicPlayTrack, BrewsicStopTrack
StartPlayback:
	jsr CompilePatternToBuffer
	jsr TransferEntirePlaybackBufferToSpc
	lda #0
	jsr BrewsicPlayTrack
rts

CopyTestPatternToSpcBuffer:
	ldx #0
	:
		;.import TEST_PATTERN
		;lda f:TEST_PATTERN+$5099,X
		lda f:TEST_PATTERN2,X
		sta f:CompiledPattern,X
		inx
		cpx #Pattern16Offset
	bne :-
rts

SwitchToSingleNoteMode:

	lda QueuePreparePlayback
	beq :+
		jsr PreparePlayback ; If prepare was queued, do it now
	:

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

CompileCurrentNoteToBuffer:
	ldx #Pattern01Offset
	ldy CursorPositionRow
	
	lda PatternInstruments,Y
	sta f:CompiledPattern+1,X
	
	lda PatternNotes,Y
	sta f:CompiledPattern+2,X
rts

CompilePatternToBuffer:
	; Switch to playing full phrase
	lda #<Pattern16Index
	sta f:CompiledPattern+PatternReferenceOffset
	lda #>Pattern16Index
	sta f:CompiledPattern+PatternReferenceOffset+1

	; Load phrase into test pattern
	ldx #Pattern16Offset
	lda #15
	sta f:CompiledPattern,X
	inx
	ldy #0
	@rowLoop:
		lda PatternNotes,Y
		cmp #$ff
		bne :+
			lda #63
			sta f:CompiledPattern,X
			bra :++
		:
			sta f:CompiledPattern+1,X
			lda PatternInstruments,Y ; Instrument/header
			sta f:CompiledPattern,X
			inx
		:
		inx
		iny
		cpy #15
	bne @rowLoop
	; Final row with loop effect added
	lda PatternNotes,Y
	cmp #$ff
	bne :+
		lda #63|$80
		sta f:CompiledPattern,X
		lda #2 ; Loop effect
		sta f:CompiledPattern+1,X
		lda #0
		sta f:CompiledPattern+2,X
		bra :++
	:
		sta f:CompiledPattern+3,X
		lda #0|$80
		sta f:CompiledPattern,X
		lda #2 ; Loop effect
		sta f:CompiledPattern+1,X
		lda #0
		sta f:CompiledPattern+2,X

	:
rts

TransferPatternReferencesToSpc:
	ldx #PatternReferenceOffset
	ldy #4
jmp spcTransfer

TransferSingleNoteToSpc:
	ldx #Pattern01Offset
	ldy #8
jmp spcTransfer


TEST_PATTERN2:
Pattern16Offset = test_pattern_16_bars - TEST_PATTERN2
Pattern01Offset = test_pattern_single_note - TEST_PATTERN2
PatternReferenceOffset = test_pattern_trackindex_start - TEST_PATTERN2

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
.byte $00 ; Instrument 0-52
.byte 12*4 ; Note byte
.byte 63 ; silence
.byte $00 ; Instrument 0-52
.byte 12*4+1 ; Note byte
.byte 63 ; silence
.byte $00 ; Instrument 0-52
.byte 12*4+2 ; Note byte
.byte 63 ; silence
.byte $00 ; Instrument 0-52
.byte 12*4+3 ; Note byte
.byte 63 ; silence
.byte $00 ; Instrument 0-52
.byte 12*4 ; Note byte
.byte 63 ; silence
.byte $00 ; Instrument 0-52
.byte 12*4 ; Note byte
.byte 63 ; silence
.byte $00 ; Instrument 0-52
.byte 12*4 ; Note byte
.byte 63 ; silence
.byte $00 ; Instrument 0-52
.byte 12*4 ; Note byte
.byte 63 ; silence

