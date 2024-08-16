.include "global.inc"
.include "src/snes.inc"
.smart
.import StartTestPatternPlayback, PrepareTestPatternPlayback, StopPlayback, SwitchToSingleNoteMode, TransferSingleNoteToSpcAndPlay
.import UpdateNoteInPlayback, NoteDataOffsetInPhrase

.segment "CODE7"
Name: .byte "Phrase_-_",$ff

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
CurrentPhraseIndex: .res 1
CurrentPhraseIndexInSongData: .res 2
SourcePointer: .res 3 ; far address source pointer necessary because phrase data is spread across multiple banks

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
		; $ff when navigating backwards from instrument
		jsl LoadGlobalData
	:

	Bind Input_StartPlayback, StartPlayback
	Bind Input_CustomHandler, HandleInput
	Bind Input_NavigateIn, NoAction
	Bind Input_NavigateBack, ReturnToChainView
	Bind OnPlaybackStopped, SwitchToSingleNoteMode
	
	jsl PreparePlayback_long
	ldy #.loword(Name)
	jsl WriteTilemapHeader
	lda CurrentPhraseIndex
	jsl WriteTilemapHeaderId
	jsl WritePatternTilemapBuffer
	jsl ShowCursor_long

	;TODO: DELETE
	wai
	lda #%00010110
	sta BLENDMAIN

rts

LoadGlobalData:
@sourcePointer = 0
	sta CurrentPhraseIndex
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
	sta CurrentPhraseIndexInSongData
	and #$1FFF
	pha
	ldy #^PHRASES_1
	ldx #.loword(PHRASES_1)
	lda CurrentPhraseIndexInSongData
	and #$2000
	beq :+
		ldy #^PHRASES_2
		ldx #.loword(PHRASES_2)
	:
	seta8
	tya
	stx @sourcePointer
	sta @sourcePointer+2
	stx SourcePointer
	sta SourcePointer+2
	ldx #0
	ply
	@loop:
		lda [@sourcePointer],y
		sta PatternInstruments,x
		iny		
		lda [@sourcePointer],y
		sta PatternCommands,x
		iny
		lda [@sourcePointer],y
		sta PatternCommandParams,x
		iny
		lda [@sourcePointer],y
		sta PatternNotes,x
		iny
		inx
		cpx #$10
	bne @loop	
rtl
UpdateGlobalData:
@sourcePointer = 0
	; Copy temp memory to selected phrase
	; TODO: Update only currently selected bar
	ldx SourcePointer
	lda SourcePointer+2
	stx @sourcePointer
	sta @sourcePointer+2
	
	ldx #0
	seta16
	lda CurrentPhraseIndexInSongData
	and #$1FFF
	tay
	seta8
	@loop:
		lda PatternInstruments,x
		sta [@sourcePointer],y
		iny
		lda PatternCommands,x
		sta [@sourcePointer],y
		iny
		lda PatternCommandParams,x
		sta [@sourcePointer],y
		iny
		lda PatternNotes,x
		sta [@sourcePointer],y
		iny
		inx
		cpx #$10
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
	jsl UpdateGlobalData
	lda IsPlaying
	beq :+
		lda CursorPositionRow
		asl
		asl ; x4. Max 64, so 8bit A is fine yet
		sta NoteDataOffsetInPhrase
		seta16
		and #$00ff
		adc CurrentPhraseIndexInSongData ; should point to the block of 4 relevant bytes
		tax ; [X] tells where to read data from
		seta8
		lda CurrentPhraseIndex ; [A] tells which phrase to look for in compiled song data
		jsr UpdateNoteInPlayback
	:
	jsl WritePatternTilemapBuffer
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
		jsr TransferSingleNoteToSpcAndPlay
	:
rts
CutCurrentlyPlayingNote:
	lda IsPlaying
	bne :+ ; If currently playing, just don't do anything
		jsr StopPlayback
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

PreparePlayback_long: jsr PrepareTestPatternPlayback
rtl

CompileCurrentNoteToBuffer:
	ldx #Pattern01Offset
	ldy CursorPositionRow
	
	lda PatternInstruments,Y
	sta f:CompiledPattern+1,X
	
	lda PatternNotes,Y
	sta f:CompiledPattern+2,X
rts

StartPlayback:
	lda CurrentPhraseIndex
	ldx CurrentPhraseIndexInSongData
	jsr StartTestPatternPlayback
rts
