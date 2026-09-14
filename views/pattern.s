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
SelectionStartCol: .res 2
SelectionStartRow: .res 2
SelectionEndCol: .res 2
SelectionEndRow: .res 2
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

	ldx #$00ff
	stx SelectionStartRow
	stx SelectionStartCol
	stx SelectionEndRow
	stx SelectionEndCol

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

	Bind Input_Erase, Erase
	Bind Input_StartSelection, StartSelection
	Bind Input_EndSelection, EndSelection
	Bind Input_CopySelection, Copy
	Bind Input_CutSelection, Cut
	Bind Input_Paste, Paste

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
	jsr ShowPatternBackdrop
	
	jsr GetFirstUnusedInstrument	
	jsl UpdateHighlight_long
rts
.export Pattern_HideView = HideView
HideView:
	; Hides the pattern bars on the right side, if cursor is over en empty entry on the chain view on the left
	jsl WriteEmptyTilemapBuffer
	;stz ShowBg3
	jsr ShowClearBackdrop
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
	;phx
	lda PatternCommands,Y
	beq @emptyCommand
		;tax
		;lda f:CommandCharacter,x
		;plx
		ora #$50
		sta f:TilemapBuffer+14,x
	
		lda PatternCommandParams,Y
		PrintHexNumber TilemapBuffer+16
		bra :+

	@emptyCommand:
		;plx
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


StartSelection:
	lda CursorPositionRow
	sta SelectionStartRow
	sta SelectionEndRow
	lda CursorPositionCol
	sta SelectionStartCol
	sta SelectionEndCol
jmp ShowCursor

Erase:
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

EndSelection:
	ldx TilemapOffset
	stx CursorOffset
	jsr GetSelectionCoordinates
	jsr ClearSelectionArea
	lda #$ff
	sta SelectionStartRow
rts

HandleInput:

	lda ButtonStates+1 ; Key Y enters edit mode while held
	bit #>KEY_Y
	bne :+
		lda EditMode
		beq @continue
			; Let go of Y while in edit mode
			stz EditMode
			jsr CutCurrentlyPlayingNote
			jsr ShowCursor
			bra @continue
	:
	lda ButtonPushed+1 ; Key Y enters edit mode while held
	bit #>KEY_Y
	beq @continue
		lda EditMode
		bne @continue
			; Pushed Y while not in edit mode
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
@signCheck = 0
	sta @signCheck
	ldx CursorPositionRow
	lda #0
	xba ; Ensure 8bit value in 16bit Y
	lda PatternCommands,x
	tay

	lda @signCheck
	clc
	adc PatternCommandParams,x
	sta PatternCommandParams,x

	bit @signCheck
	bpl :+
		; Negative delta
		clc
		sbc CommandParamMaxValues,Y
		clc
		adc CommandParamRanges,Y
		bcs @noOverflow

			lda CommandParamMinValues,Y
			sta PatternCommandParams,X
			bra @noOverflow

	:
		; Positive delta
		sec
		sbc CommandParamMinValues,Y
		cmp CommandParamRanges,Y
		bcc @noOverflow
			
			lda CommandParamMaxValues,Y
			sta PatternCommandParams,X
	
	@noOverflow:

	lda PatternCommands,x
	beq :+
		sta LastEditedCommand
		lda PatternCommandParams,x
		sta LastEditedCommandParam
	:
jmp NoteWasChanged

;None,Tempo,GainDown,GainUp,GainSet,PitchDown,PitchUp,Arp,Pan,ChVolume,Echo,SampleOffset	.addr NoEffect
CommandParamMinValues:
.byte 0,$01,$00,$00,$00,$00,$00,0,$e0,$80,0,$00
CommandParamMaxValues:
.byte 0,$40,$1f,$1f,$7f,$ff,$ff,$ff,$20,$7f,1,$ff
CommandParamRanges:
.byte 0,$3F,$1F,$1F,$7F,$ff,$ff,$ff,$41,$ff,1,$ff

.import Chain_MovePhraseUp, Chain_MovePhraseDown
MoveCursorDown:
	lda CursorPositionRow
	inc
	sta CursorPositionRow
	cmp #$10
	beq :+
		jmp ShowCursor
	:
	
	bit SelectionStartRow ; Don't move to next phrase if currently making a selection
	bpl :+
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
	
	bit SelectionStartRow ; Don't move to next phrase if currently making a selection
	bpl :+
		jsr Chain_MovePhraseUp
		beq :+
			lda #$0f
			sta CursorPositionRow
			bra :++
		:
			stz CursorPositionRow
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

	bit SelectionStartRow
	bmi :+
		jsr GetSelectionCoordinates
		jsr ClearSelectionArea

		lda CursorPositionRow
		sta SelectionEndRow
		lda CursorPositionCol
		sta SelectionEndCol
		jsr GetSelectionCoordinates
		jsr MarkSelectionArea
	:

	lda CursorPositionRow
	sta CursorY
	ldx CursorPositionCol
	lda f:CursorPositions,x
	sta CursorX
	lda f:CursorTypes,x
	sta CursorSize
	
	lda #2
jmp UpdateCursorSpriteAndHighlight

CursorPositions:
.byte 0,4,7,8
CursorTypes:
.byte 1,0,1,0
CursorWidths:
.byte 3,2,1,2

GetSelectionIndexes:
	;0 = X, 2 = Y, 4 = Width, 6 = Height
	lda SelectionEndRow
	sta Selection_Y
	sec
	sbc SelectionStartRow
	bcs :+
		; Start was higher than End, so reverse the subtraction but keep the value stored in 2
		lda SelectionStartRow
		sec
		sbc SelectionEndRow
		sta Selection_Height
		bra :++
	:
		sta Selection_Height
		lda SelectionStartRow
		sta Selection_Y
	:

	lda SelectionEndCol
	cmp SelectionStartCol
	bcs :+
		; Start was higher than End
		sta Selection_X
		lda SelectionStartCol
		sta Selection_Width ; Use width as "EndX" for now
		bra :++
	:
		sta Selection_Width
		lda SelectionStartCol
		sta Selection_X
	:
rts
GetSelectionCoordinates:
	jsr GetSelectionIndexes
	; Get tile size from LUTs
	stz Selection_X+1
	ldx Selection_X
	lda f:CursorPositions,X
	sta Selection_X

	stz Selection_Width+1
	ldx Selection_Width
	lda f:CursorPositions,X
	clc
	adc f:CursorWidths,X
	sec
	sbc Selection_X ; Subtract X from XEnd to give width in tiles
	sta Selection_Width ; width

	inc Selection_Height ; Add 1 (selection includes last cell)
rts

Copy:
@lastCol = 0
@rowCount = 3
@startX = 4;+5
@colCount = 6

	phb
	lda #^Clipboard
	pha
	plb

	lda #3 ; Phrase data
	sta Clipboard
	
	jsr GetSelectionIndexes
	lda Selection_Height
	inc a
	sta Clipboard+1
	lda Selection_Width
	sec
	sbc Selection_X
	inc
	sta Clipboard+2
	sta @colCount
	
	ldy #3
	lda #0
	xba ; Keep upper 8 bits of A clean
	lda Selection_Y
	tax
	stx @startX ; X is necessary for the jump table, so use X as the read index, since it's reset in every inner loop
	lda Selection_X ; each column index is the copy/paste "mode"
	asl
	
	@colLoop:
		sta @lastCol
		sta Clipboard,Y
		tax
		iny
		lda Clipboard+1
		sta @rowCount
		jsr (.loword(@CopyMethods),X)
	
		lda @lastCol
		inc a
		inc a
		dec @colCount
	bne @colLoop
	
	plb
rts
.macro CopyRoutine Source
.local @rowLoop
	ldx @startX
	@rowLoop:
		lda Source,X
		inx
		sta Clipboard,Y
		iny
		dec @rowCount
	bne @rowLoop
rts
.endmacro
@CopyMethods: .addr @CopyNotes, @CopyInstruments, @CopyCommands, @CopyCommandValues
@CopyNotes: CopyRoutine PatternNotes
@CopyInstruments: CopyRoutine PatternInstruments
@CopyCommands: CopyRoutine PatternCommands
@CopyCommandValues: CopyRoutine PatternCommandParams

Paste:
@rowCount = 3
@colCount = 6


	lda f:Clipboard
	cmp #3 ; Phrase data
	beq :+
		rts
	:

	phb
	lda #^Clipboard
	pha
	plb

	lda #0
	xba ; Clean top 8 bytes of AB
	lda Clipboard+2
	sta @colCount
	ldy #3
	@colLoop:
		lda Clipboard,Y
		tax
		iny
		lda Clipboard+1
		sta @rowCount
		jsr (.loword(@PasteMethods),X)
		dec @colCount
	bne @colLoop

	plb
jmp NoteWasChanged ; TODO: Call changed event for each pasted row, but only redraw the tiles once, so playback plays correctly

.macro PasteRoutine Target, ClearRow
.local @rowLoop
	ldx CursorPositionRow
	@rowLoop:
		lda Clipboard,Y
		iny
		sta Target,X
		.if ClearRow = 1
			stz PatternInstruments,X
			stz PatternCommands,X
			stz PatternCommandParams,X
		.endif
		inx
		dec @rowCount
	bne @rowLoop
rts
.endmacro
@PasteMethods: .addr @PasteNotes, @PasteInstruments, @PasteCommands, @PasteCommandValues
@PasteNotes: PasteRoutine PatternNotes, 1
@PasteInstruments: PasteRoutine PatternInstruments, 0
@PasteCommands: PasteRoutine PatternCommands, 0
@PasteCommandValues: PasteRoutine PatternCommandParams, 0


Cut:
@rowCount = 3
@startX = 4;+5 ; Reused from Copy
@colCount = 6
	jsr Copy

	lda #0
	xba ; Clean top 8 bytes of AB
	lda f:Clipboard+2
	sta @colCount
	ldy #3
	@colLoop:
		tyx
		lda f:Clipboard,X
		tax
		iny
		lda f:Clipboard+1
		sta @rowCount
		jsr (.loword(@ClearMethods),X)
		dec @colCount
	bne @colLoop

jmp NoteWasChanged ; TODO: Call changed event for each pasted row, but only redraw the tiles once, so playback plays correctly

@ClearMethods: .addr @ClearNotes, @ClearInstruments, @ClearCommands, @ClearCommandValues
@ClearNotes:
 	ldx @startX
 	lda #$ff
	@noteRowLoop:
		sta PatternNotes,X
		stz PatternInstruments,X
		stz PatternCommands,X
		stz PatternCommandParams,X
		inx
		iny
		dec @rowCount
	bne @noteRowLoop
rts
@ClearInstruments:
	@instrumentRowLoop:
		iny
		dec @rowCount
	bne @instrumentRowLoop
rts
@ClearCommands:
@ClearCommandValues:
 	ldx @startX
	@commandRowLoop:
		stz PatternCommands,X
		stz PatternCommandParams,X
		inx
		iny
		dec @rowCount
	bne @commandRowLoop
rts

jmp NoteWasChanged
	

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
		cmp Playback_CurrentPhraseOfChannel,y ; Check if a channel (Y) is playing the current phrase
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