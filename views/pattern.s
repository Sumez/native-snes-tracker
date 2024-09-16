.include "global.inc"
.include "src/snes.inc"
.smart
.import PlaySinglePhrase, PrepareTestPatternPlayback, StopPlayback, SwitchToSingleNoteMode, PlaySingleNote
.import UpdateNoteInPlayback, NoteDataOffsetInPhrase

.segment "CODE7"
Name: .byte "Phrase_-_",$ff

.segment "BSS"

;Needs init:
Octaves: .res 10 ; Filled with 0->A
Notes: .res 12 ; Filed with C->B
NotesSharp: .res 12 ; Filed with # or -
CursorPositionCol: .res 2
CursorPositionRow: .res 2
LastEditedNote: .res 1
LastEditedInstrument: .res 1
LastEditedCommand: .res 1
LastEditedCommandParam: .res 1

;Loaded when view loads:
PatternNotes: .res 16
PatternInstruments: .res 16
PatternCommands: .res 16
PatternCommandParams: .res 16
CurrentPhraseIndex: .res 1
CurrentPhraseIndexInSongData: .res 2
SourcePointer: .res 3 ; far address source pointer necessary because phrase data is spread across multiple banks
TilemapOffset: .res 2
FirstUnusedInstrument: .res 1

.segment "CODE6"

.export Pattern_Init = Init
Init:
	ldx #0
	stx CursorPositionRow
	ldx #0
	stx CursorPositionCol
	
	lda #(12*4)
	sta LastEditedNote
	stz LastEditedInstrument
	stz LastEditedCommand
	stz LastEditedCommandParam
	
	jsl InitiateNoteTileReferences
rtl

.export Pattern_FocusView = FocusView
FocusView:
	jsr LoadView

	jsl PrepareTestPatternPlayback
	ldy #.loword(Name)
	jsl WriteTilemapHeader
	lda CurrentPhraseIndex
	jsl WriteTilemapHeaderId

	Bind Input_StartPlayback, StartPlayback
	Bind Input_CustomHandler, HandleInput
	Bind Input_NavigateIn, NavigateFromCursorPosition
	Bind Input_NavigateBack, ReturnToChainView
	Bind OnPlaybackStopped, SwitchToSingleNoteMode

	jsl ShowCursor_long
rts

.export Pattern_LoadView = LoadView
LoadView:
	ldx z:LoadView_TilemapOffset
	stx TilemapOffset
	tya
	cmp #$ff
	beq :+
		; $ff when navigating backwards from instrument
		jsl LoadGlobalData
	:

	jsl WritePatternTilemapBuffer

	;TODO: do better
	seta16
	lda TilemapOffset
	and #$3C
	clc
	adc #Bg3TileMapBase
	lsr
	sta Bg3Offset
	seta8
	lda #1
	sta ShowBg3
	
	jsr GetFirstUnusedInstrument	
	jsl UpdateHighlight_long
rts
.export Pattern_HideView = HideView
HideView:
	jsl WriteEmptyTilemapBuffer
	stz ShowBg3
rts

.export GetFirstUnusedInstrumentOffset
GetFirstUnusedInstrumentOffset:
	ldx #0
	@loop:
		lda f:INSTRUMENTS,X
		cmp #$ff
		beq @store
		seta16
			txa
			clc
			adc #8
			tax
		seta8
		cpx #MAX_INSTRUMENTS*8
	bne @loop
	@store:
rts
GetFirstUnusedInstrument:
	jsr GetFirstUnusedInstrumentOffset
	seta16
	txa
	lsr
	lsr
	lsr
	seta8
	sta FirstUnusedInstrument
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
		cmp #$ff
		beq :+
			sta PatternCommands,x
			iny
			lda [@sourcePointer],y
			sta PatternCommandParams,x
			bra :++
		:
			stz PatternCommands,x
			stz PatternCommandParams,x
		:
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
	lda #'#'
	sta NotesSharp+1
	sta NotesSharp+3
	sta NotesSharp+6
	sta NotesSharp+8
	sta NotesSharp+10
	lda #'-'
	sta NotesSharp+0
	sta NotesSharp+2
	sta NotesSharp+4
	sta NotesSharp+5
	sta NotesSharp+7
	sta NotesSharp+9
	sta NotesSharp+11
rtl

WriteEmptyTilemapBuffer:

	ldx TilemapOffset

	phb
	lda #^TilemapBuffer
	pha
	plb

	ldy #16
	:
		lda #'_'
		sta TilemapBuffer+0,X
		sta TilemapBuffer+2,X
		sta TilemapBuffer+4,X

		sta TilemapBuffer+8,X
		sta TilemapBuffer+10,X

		sta TilemapBuffer+14,X
		sta TilemapBuffer+16,X
		sta TilemapBuffer+18,X
		seta16
		txa
		clc
		adc #$40
		tax
		seta8
		dey
	bne :-
	plb
rtl

WritePatternTilemapBuffer:

@currentNote = 0
xba
lda #0 ; Ensure $00 in register B for TAYs
xba

ldx TilemapOffset
ldy #0

@rowLoop:
	sty @currentNote
	
	;NOTES
	lda PatternNotes,Y
	cmp #$ff
	beq @noNote
	cmp #$fe
	beq @cutNote
	
		; First get octave tile
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
		sta f:TilemapBuffer+4,x
		pla
		tay
		lda Notes,Y
		sta f:TilemapBuffer+0,x
		lda NotesSharp,Y
		sta f:TilemapBuffer+2,x
		
		ldy @currentNote
		lda PatternInstruments,Y
		tay
		PrintHexNumber TilemapBuffer+8
		
		lda UnusedInstruments, Y
		bne :+
			lda #2<<2
			bra :++
		:
			lda #0
		:
		sta f:TilemapBuffer+9,X
		sta f:TilemapBuffer+11,X
		ldy @currentNote
		
	bra :+
	@cutNote:
	
		;Note
		lda #'~'
		sta f:TilemapBuffer+2,x
		lda #'_'
		sta f:TilemapBuffer+0,x
		sta f:TilemapBuffer+4,x
		bra @noInstrument
		
	@noNote:
	
		;Note
		lda #$1d
		sta f:TilemapBuffer+0,x
		lda #$1e
		sta f:TilemapBuffer+2,x
		lda #$1f
		sta f:TilemapBuffer+4,x
	
		@noInstrument:
		;Instrument
		lda #$1d
		sta f:TilemapBuffer+8,x
		lda #$1f
		sta f:TilemapBuffer+10,x

	:
	phx
	lda PatternCommands,Y
	beq @emptyCommand
		tax
		lda f:CommandCharacter,x
		plx
		sta f:TilemapBuffer+14,x
	
		lda PatternCommandParams,Y
		PrintHexNumber TilemapBuffer+16
		bra :+

	@emptyCommand:
		plx
		lda #$1d
		sta f:TilemapBuffer+14,x
		lda #$1e
		sta f:TilemapBuffer+16,x
		lda #$1f
		sta f:TilemapBuffer+18,x

	:

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
CommandCharacter:
.byte 0,"TxXSpPVXxA"

.export Pattern_GetCurrentNote = GetCurrentNote
GetCurrentNote:
	ldx CursorPositionRow
	lda PatternNotes,x
rts


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
		jsr CutCurrentlyPlayingNote
		
		ldy CursorPositionRow
		lda #0
		xba
		lda PatternInstruments,Y
		tax
		lda UnusedInstruments,X ; Don't play note if no instrument sample set
		beq :+
			lda PatternInstruments,Y
			xba
			lda PatternNotes,Y
			jmp PlaySingleNote
	:
rts
CutCurrentlyPlayingNote:
	lda IsPlaying
	bne :+ ; If currently playing pattern, chain or song, just don't do anything
		jsr StopPlayback
	:
rts

NoAction: rts
.import Chain_ChildViewInFocus
ReturnToChainView:
	lda #1
	stz Chain_ChildViewInFocus
	ldy #$ff ; Reuse already loaded chain
jmp NavigateToScreen

NavigateFromCursorPosition:
	lda CursorPositionCol
	cmp #1
	bne @break
	ldx CursorPositionRow
	lda PatternNotes,x
	cmp #$ff
	beq @break
	lda PatternInstruments,x
	cmp #$ff
	beq @break
		tay
		lda #3
		jmp NavigateToScreen
	rts
	@break:
jmp PlayMosaic


HandleInput:

	lda ButtonPushed
	bit #<KEY_X ; Key X removes current note
	beq @noX
	
			ldx CursorPositionRow
			lda CursorPositionCol
			cmp #2
			bcc :++
				
				lda PatternCommands,X
				beq :+
					sta LastEditedCommand
					lda PatternCommandParams,X
					sta LastEditedCommandParam
				:
				stz PatternCommandParams,X
				stz PatternCommands,X
				jmp NoteWasChanged
			:

			lda PatternNotes,x
			cmp #$ff
			beq @isEmpty
				
				cmp #$fe
				beq :+
					; Not empty, and not note-off, save note+instr in memory
					sta LastEditedNote
					lda PatternInstruments,x
					sta LastEditedInstrument
				:
				
				lda #$ff
				bra :+
			@isEmpty:
				; If bar is already empty, add KEYOFF
				lda #$fe
			:
			sta PatternNotes,x
			stz EditMode
			jsr NoteWasChanged
			jmp ShowCursor
			;RETURNS - no more inputs read this frame
	@noX:
	
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

			ldx CursorPositionRow
			lda CursorPositionCol
			cmp #2
			bcc :++
			 	; If command or command param column, don't insert or play notes, but do load or save latest from/to memory
				lda PatternCommands,x
				beq :+
					sta LastEditedCommand
					lda PatternCommandParams,x
					sta LastEditedCommandParam
					jsr NoteWasChanged
					bra @showCursor
				:
					lda LastEditedCommand
					sta PatternCommands,x
					lda LastEditedCommandParam
					sta PatternCommandParams,x
					jsr NoteWasChanged
					bra @showCursor
			:
			; Check if note exists, and if not, place the last one edited
			lda PatternNotes,x
			cmp #$fc
			bcc @playNote
			
				; No note exists. Use last edited
				lda LastEditedNote
				sta PatternNotes,x
				lda LastEditedInstrument
				sta PatternInstruments,x
				jsr NoteWasChanged
					
			@playNote:
			jsr PlayCurrentNote
			@showCursor:
			jsr ShowCursor

	@continue:
	
	; If in edit mode, branch to input relevant to that. if not, branch to internal navigation
	; If L button held, branch to global navigation (handle in editor.s?)
	lda EditMode
	bne :+
		jmp @Navigation
	:

@EditMode:

	lda ButtonPushed+1
	ldx CursorPositionCol
	bne @EditModeInstrumentCol

; TODO: Make this a lot simpler. Maybe use an indirect jump depending on which column is active?
@EditModeNoteCol:
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
	cpx #1
	bne @EditModeCommandIdCol
	
	ldx CursorPositionRow
	lda PatternNotes,X
	cmp #$fc ; Only allow change instrument if valid note on bar
	bcs @return
	
	lda ButtonPushed+1
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
@return: rts

@EditModeCommandIdCol:
	cpx #2
	bne @EditModeCommandParamCol
	
	bit #>KEY_DOWN
	beq :+
		lda #(256-1)
		jmp ChangeCurrentCommand
	:
	bit #>KEY_UP
	beq :+
		lda #1
		jmp ChangeCurrentCommand
	:
	bit #>KEY_LEFT
	beq :+
		lda #(256-1)
		jmp ChangeCurrentCommand
	:
	bit #>KEY_RIGHT
	beq :+
		lda #1
		jmp ChangeCurrentCommand
	:
rts

@EditModeCommandParamCol:
	bit #>KEY_DOWN
	beq :+
		lda #(256-$10)
		jmp ChangeCurrentCommandParam
	:
	bit #>KEY_UP
	beq :+
		lda #$10
		jmp ChangeCurrentCommandParam
	:
	bit #>KEY_LEFT
	beq :+
		lda #(256-1)
		jmp ChangeCurrentCommandParam
	:
	bit #>KEY_RIGHT
	beq :+
		lda #1
		jmp ChangeCurrentCommandParam
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
	cmp #12*9
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
		bra @continue
	:
	;cmp FirstUnusedInstrument
	;bcc @continue
	;	lda FirstUnusedInstrument
	;	bra @continue

	cmp #MAX_INSTRUMENTS
	bcc @continue
		lda #MAX_INSTRUMENTS
		dec

	@continue:
	sta PatternInstruments,x
	jsr NoteWasChanged
jmp PlayCurrentNote

ChangeCurrentCommand:
	ldx CursorPositionRow
	clc
	adc PatternCommands,x
	bpl :+
		lda #0
		bra :++
	:
	cmp #11 ; TODO: Number of supported commands
	bcc :+
		lda #11
		dec
	:
	sta PatternCommands,x
	beq :+
		sta LastEditedCommand
		lda PatternCommandParams,x
		sta LastEditedCommandParam
	:
jmp NoteWasChanged

ChangeCurrentCommandParam:
	ldx CursorPositionRow
	clc
	adc PatternCommandParams,x
	sta PatternCommandParams,x

	lda PatternCommands,x
	beq :+
		sta LastEditedCommand
		lda PatternCommandParams,x
		sta LastEditedCommandParam
	:
jmp NoteWasChanged

.import Chain_MovePhraseUp, Chain_MovePhraseDown
MoveCursorDown:
	lda CursorPositionRow
	inc
	sta CursorPositionRow
	cmp #$10
	beq :+
		jmp ShowCursor
	:
	
	jsr Chain_MovePhraseDown
	beq :+
		stz CursorPositionRow
		bra :++
	:
		lda #$0f
		sta CursorPositionRow
	:
jmp ShowCursor

MoveCursorUp:
	dec CursorPositionRow
	bmi :+
		jmp ShowCursor
	:
	jsr Chain_MovePhraseUp
	bne :+
		stz CursorPositionRow
		bra :++
	:
		lda #$0f
		sta CursorPositionRow
	:
jmp ShowCursor

SetNoteCol:
	dec CursorPositionCol
	bpl :+
		stz CursorPositionCol
	:
jmp ShowCursor

SetInstrumentCol:
	inc CursorPositionCol
	lda CursorPositionCol
	cmp #$4
	bne :+
		dec CursorPositionCol
	:
jmp ShowCursor

ShowCursor_long: jsr ShowCursor
rtl
ShowCursor:

	ldx TilemapOffset
	stx CursorOffset
	lda #11
	sta HighlightLength

	lda CursorPositionRow
	sta CursorY
	lda CursorPositionCol
	bne @notnote
		lda #0
		sta CursorX
		lda #1
		bra :++
	@notnote:
		cmp #1
		bne @notins
		lda #4
		sta CursorX
		bra :+
	@notins:
		cmp #2
		bne @notcmdId
		lda #7
		sta CursorX
		lda #1
		bra :++
	@notcmdId:
		lda #8
		sta CursorX
		:
		lda #0
	:
	sta CursorSize
	
	lda #2
jmp UpdateCursorSpriteAndHighlight

StartPlayback:
	lda CurrentPhraseIndex
	ldx CurrentPhraseIndexInSongData
	jsr PlaySinglePhrase
rts

.export Pattern_UpdateBeatHighlight = UpdateBeatHighlight
UpdateHighlight_long: jsr UpdateBeatHighlight
rtl
UpdateBeatHighlight:
	lda IsPlaying
	bcc @removeHighlight
	ldy #0
	lda CurrentPhraseIndex
	:
		cmp Playback_CurrentPhraseOfChannel,y
		bne :+
			lda Playback_CurrentBeatRow
			seta16
			and #$ff
			xba
			lsr
			lsr
			clc
			adc TilemapOffset
			tax
			seta8
			jmp HighlightRow
		:
		iny
		cpy #8
	bne :--
	@removeHighlight:
	ldx #$ffff
jmp HighlightRow