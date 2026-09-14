.include "global.inc"
.include "src/snes.inc"
.smart
.import PlaySingleChain

.segment "CODE7"
Name: .byte "Chain_-_",$ff

.segment UnusedItemsSegment
UnusedPhrases: .res $100

.segment "BSS"

;Needs init:
CursorRow: .res 2
CursorColumn: .res 2
SelectionStart: .res 1
SelectionEnd: .res 1
LastEditedPhrase: .res 1
LastEditedTranspose: .res 1

;Loaded when view loads:
PhraseIndexes: .res $10
TransposeValues: .res $10
CurrentChainIndex: .res 1
CurrentChainIndexInGlobalSong: .res 2
TilemapOffset: .res 2
ChildTilemapOffset: .res 2
ChildViewInFocus: .res 1
.export Chain_ChildViewInFocus = ChildViewInFocus

.segment "CODE6"

.export Chain_Init = Init
Init:
	stz LastEditedPhrase
	stz LastEditedTranspose
	
	ldx #0
	stx CursorRow
	stx CursorColumn
rtl

.export Chain_FocusView = FocusView
FocusView:
	jsr LoadView
	
	lda #$ff
	sta SelectionStart

	ldy #.loword(Name)
	jsl WriteTilemapHeader
	lda CurrentChainIndex
	jsl WriteTilemapHeaderId

	Bind Input_StartPlayback, StartPlayback
	Bind Input_CustomHandler, HandleInput
	Bind Input_NavigateIn, NavigateToPhrase
	Bind Input_NavigateBack, NavigateToSong
	Bind OnPlaybackStopped, NoAction
	
	Bind Input_Erase, Erase
	Bind Input_Clone, Clone
	Bind Input_StartSelection, StartSelection
	Bind Input_EndSelection, EndSelection
	Bind Input_CopySelection, Copy
	Bind Input_CutSelection, Cut
	Bind Input_Paste, Paste
	
	jsl UpdateHighlight_long
	jsl ShowCursor_long

	lda ChildViewInFocus
	beq :+
		jsl NavigateToPhrase_long
	:
rts

.export Chain_LoadView = LoadView
LoadView:
seta16
	lda z:LoadView_TilemapOffset
	sta TilemapOffset
	clc
	adc #28
	sta ChildTilemapOffset
seta8
	tya
	cmp #$ff
	beq :+
		jsl LoadGlobalData
		stz ChildViewInFocus
	:
		
	jsl WriteTilemapBuffer
	ldx #2
	jsl LoadGuiMap
rts

.import Pattern_LoadView, Pattern_HideView, Pattern_FocusView
UpdateChildView:

	ldx ChildTilemapOffset
	stx z:LoadView_TilemapOffset
	ldx CursorRow
	lda PhraseIndexes,X
	cmp #$ff
	beq :+
		tay
		jsr Pattern_LoadView
rtl
	:
		jsr Pattern_HideView
rtl
JumpToChildView:
	ldx ChildTilemapOffset
	stx z:LoadView_TilemapOffset
	jsr Pattern_FocusView
rtl

LoadGlobalData:
	; Copy selected chain data to temp memory
	sta CurrentChainIndex
	seta16
	and #$00ff
	; Every Chain is $20 bytes, so shift left 5 times (x32)
	asl
	asl
	asl
	asl
	asl
	tax
	seta8
	stx CurrentChainIndexInGlobalSong
	ldy #0
	@loop:
		lda f:CHAINS,x
		sta PhraseIndexes,y
		lda f:CHAINS+1,x
		sta TransposeValues,y
		inx
		inx
		iny
		cpy #$10
	bne @loop	
rtl
UpdateGlobalData:
	; Copy temp memory to selected chain
	; TODO: Update only currently selected phrase entry (more if cut/paste)
	ldx CurrentChainIndexInGlobalSong
	ldy #0
	@loop:
		lda PhraseIndexes,y
		sta f:CHAINS,x
		lda TransposeValues,y
		sta f:CHAINS+1,x
		inx
		inx
		iny
		cpy #$10
	bne @loop
rtl
.export UpdateUnusedPhrasesGlobal
UpdateUnusedPhrasesGlobal:
; This takes almost 1 frame, so only run it while loading song
	phb
	lda #^UnusedPhrases
	pha
	plb
	setxy8
	ldx #0
	:
		stz UnusedPhrases,X
		inx
	bne :-

	lda #^CHAINS
	pha
	plb
	ldy #0
	lda #$ff
	:
		@offset .set $000
		.repeat $20
			ldx CHAINS+@offset,y
			sta f:UnusedPhrases,X
			@offset .set @offset + $100
		.endrepeat
		iny
		iny
	beq :+
		jmp :-
	:
	setxy16	
	ldx #@offset
	plb
rtl
UpdateUnusedPhrases:
	setxy8
	ldy #$0f
	lda #$ff
	:
		ldx PhraseIndexes,y
		sta f:UnusedPhrases,X
		dey
	bpl :-
	setxy16
rtl

WriteTilemapBuffer:

	ldy #0
	ldx TilemapOffset
	@rowLoop:
		
		lda PhraseIndexes,Y
		cmp #$ff
		beq :+
			; Write the number
			PrintHexNumber TilemapBuffer
			bra :++
		:
			; Empty cell	
			lda #$1d
			sta f:TilemapBuffer,x
			lda #$1f
			sta f:TilemapBuffer+2,x
		:
		
		lda TransposeValues,Y
		beq :+
			; TODO: Write number with +/- notation
			PrintHexNumber TilemapBuffer+6
			bra :++
		:
			; Empty cell	
			lda #$3F
			sta f:TilemapBuffer+6,x
			sta f:TilemapBuffer+8,x
		:
		

		seta16
		txa
		clc
		adc #$40
		tax
		seta8
		
		iny
		cpy #$10
	bne @rowLoop
rtl

.segment "CODE7"

PreparePlayback:
	;jsr CopyCurrentSongToSpcBuffer
	; TODO: If currently playing, don't transfer yet, wait till song stops
	;jsr TransferEntirePlaybackBufferToSpc
rts

StartPlayback:

	; Get current index into chain data and store in [X], to tell playback code to start from there
	seta16
	lda CursorRow
	asl
	ora CurrentChainIndexInGlobalSong
	tax
	seta8
	
	jsr PlaySingleChain
rts

PhraseIndexWasChanged:
;TODO: optimize!!!
	jsl WriteTilemapBuffer
	jsl UpdateGlobalData
rts

NoAction: rts
NavigateToSong:
	lda #0
jmp NavigateToScreen
NavigateToPhrase_long: jsr NavigateToPhrase
rtl
NavigateToPhrase:
	ldx CursorRow
	lda PhraseIndexes,x
	cmp #$ff
	beq :+
		tay
		lda #1
		sta ChildViewInFocus
		jsl JumpToChildView
		rts
	:
jmp PlayMosaic

GetNextUnusedPhrase:

	lda #0
	xba
	lda PhraseIndexes,X ; Start looking from the currently selected chain index
	tax
	inx ; Always start checking the next index
	
	phb

	lda #^PHRASES_1
	pha
	plb
	@loop1:
		cpx #$080
		bcs @nextLoop ; Look in the phrase_2 loop if X is $80 or higher
		lda f:UnusedPhrases,x
		bne :+
			; Seems unused. Read values of chain to check if it is
			jsr @ReturnIfPhraseHasData
			; If routine returns normally, proceed to check next chain
		:
		inx
	bra @loop1
	@nextLoop:
	
	lda #^PHRASES_2
	pha
	plb
	@loop2:
		cpx #$100
		beq @nothingFound
		lda f:UnusedPhrases,x
		bne :+
			; Seems unused. Read values of chain to check if it is
			jsr @ReturnIfPhraseHasData
			; If routine returns normally, proceed to check next chain
		:
		inx
	bra @loop2
	
	@nothingFound:
	lda #$ff
	plb
rts
@ReturnIfPhraseHasData:
	seta16
	txa
	and #$00ff
	; Every Phrase is $40 bytes, so shift left 6 times (x64)
	asl
	asl
	asl
	asl
	asl
	asl
	and #$1FFF ; Only used the lower $2000 bytes. If the top bit is set (128+), current DB index should already be changed to PHRASES_2
	tay
	seta8
	lda #$ff
	.assert .loword(PHRASES_1) = .loword(PHRASES_2), error, "Code would be unable to read PHRASES_2 using this method" ; Requirement for this trick to work
	@offset .set 0
	.repeat 16
		and PHRASES_1+@offset+1,Y 
		and PHRASES_1+@offset+2,Y
	@offset .set @offset + 3
	.endrepeat
	cmp #$ff
	beq :+
		; Had data, so return and check next
		rts
	:	
	; Return value of X in the A register, reset index sizes, and jump up one subroutine before returning, to stop looping through the unused index
	txa
	plx ; Dummy pull from stack to skip parent routine
	plb
rts

Erase:
	ldx CursorRow
	lda PhraseIndexes,x
	cmp #$ff
	beq :+ ; Don't do anything if cell is already empty
		sta LastEditedPhrase
		lda TransposeValues,x
		sta LastEditedTranspose
		lda #$ff
		sta PhraseIndexes,x
		stz TransposeValues,x
		stz EditMode
		jsr PhraseIndexWasChanged
		jmp ShowCursor
	:
rts

Clone:
	ldx CursorRow
	lda PhraseIndexes,x
	cmp #$ff
	beq :+ ; Don't do anything if cell is already empty
		sta 0
		jsr GetNextUnusedPhrase
		cmp #$ff
		beq :+ ; No unused phrase found
		
			ldx CursorRow
			sta f:PhraseIndexes,x
			sta LastEditedPhrase
			stz ExpectDoubleTap
			jsr Clone0IntoA
			jmp PhraseIndexWasChanged
	:
rts

Clone0IntoA: ; Clone data from the chain index stored in $00 into the one stored in A
; 0-1-2: Source (long) address
; 3-4-5: Target (long) address
	seta16
	and #$00ff
	asl
	asl
	asl
	asl
	asl
	asl
	ldx #(^PHRASES_1<<8)
	bit #$2000
	beq :+
		ldx #(^PHRASES_2<<8)
	:
	stx 4 ; Stores the bank byte in 5
	and #$1fff
	clc
	adc #.loword(PHRASES_1)
	sta 3

	.assert .loword(PHRASES_1) = .loword(PHRASES_2), error, "Phrases 1 and 2 have different short addresses, so the code needs to account for that"
	
	lda 0
	and #$00ff
	asl
	asl
	asl
	asl
	asl
	asl
	ldx #(^PHRASES_1<<8)
	bit #$2000
	beq :+
		ldx #(^PHRASES_2<<8)
	:
	stx 1 ; Stores the bank byte in 2
	and #$1fff
	clc
	adc #.loword(PHRASES_1)
	sta 0
	seta8
	
	ldy #0
	@loop:
		lda [0],Y
		sta [3],Y
		iny
		cpy #64
	bne @loop
rts

StartSelection:
	lda CursorRow
	sta SelectionStart
	sta SelectionEnd
jmp ShowCursor

EndSelection:
	ldx TilemapOffset
	stx CursorOffset
	jsr GetSelectionCoordinates
	jsr ClearSelectionArea
	lda #$ff
	sta SelectionStart
rts
			
; TODO: Set pointer variables to all the individual handlers like moveup, down, left, right, when loading the view
HandleInput:

	lda ButtonStates+1 ; Key Y enters edit mode while held
	bit #>KEY_Y
	bne :+
		lda EditMode
		beq @continue
			stz EditMode
			jsl UpdateUnusedPhrases
			jsr ShowCursor
			bra @continue
	:
	lda ButtonPushed+1
	bit #>KEY_Y ; Activate edit mode on Y push, not if it's held while exiting another mode
	beq @continue
		lda EditMode
		bne @continue ; Already in edit mode

			lda #1
			sta EditMode
			
			; Check if phrase exists, and if not, place the last one edited
			ldx CursorRow
			lda PhraseIndexes,x
			cmp #$ff
			beq :++
				; Phrase index exists. Don't do anything yet, but listen for doubletap and store the index to recall later
				bit ExpectDoubleTap
				bmi :+
					sta LastEditedPhrase
					lda TransposeValues,x
					sta LastEditedTranspose
					bra @showCursor
				:

				jsr GetNextUnusedPhrase ; This can take a while
				cmp #$ff
				beq @showCursor ; No unused phrase found (wow)
				
				ldx CursorRow
				sta PhraseIndexes,x
				sta LastEditedPhrase
				lda TransposeValues,x
				sta LastEditedTranspose
				stz ExpectDoubleTap
				jsr PhraseIndexWasChanged
				bra @showCursor
				
			:
				; No phrase index exists. Use last edited
				dec ExpectDoubleTap ; listen for double tap on next Y push
				lda LastEditedPhrase
				sta PhraseIndexes,x
				lda LastEditedTranspose
				sta TransposeValues,x
				jsr PhraseIndexWasChanged
			@showCursor:
			jsr ShowCursor

	@continue:
	
	; If in edit mode, branch to input relevant to that. if not, branch to internal navigation
	; If L button held, branch to global navigation (handle in editor.s?)
	lda EditMode
	beq @Navigation

@EditMode:

	lda ButtonPushed+1
	bit #>KEY_DOWN|>KEY_UP|>KEY_LEFT|>KEY_RIGHT
	bne :+
		rts ; No navigation pushed
	:
	stz ExpectDoubleTap ; If any value edited, reset doubletap wait

	bit #>KEY_DOWN
	beq :+
		lda #(256-$10)
		jmp DecreaseCurrentPhrase
	:
	bit #>KEY_UP
	beq :+
		lda #$10
		jmp IncreaseCurrentPhrase
	:
	bit #>KEY_LEFT
	beq :+
		lda #(256-1)
		jmp DecreaseCurrentPhrase
	:
	bit #>KEY_RIGHT
	beq :+
		lda #1
		jmp IncreaseCurrentPhrase
	:
rts

@Navigation:

	lda ButtonPushed+1
	bit #>KEY_DOWN|>KEY_UP|>KEY_LEFT|>KEY_RIGHT
	bne :+
		rts ; No navigation pushed
	:
	stz ExpectDoubleTap ; If any navigation pushed, reset doubletap wait

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
		jmp MoveCursorLeft
	:
	bit #>KEY_RIGHT
	beq :+
		jmp MoveCursorRight
	:

rts

DecreaseCurrentPhrase:
	ldx CursorRow
	clc
	adc PhraseIndexes,x
	bcs:+
		lda #0
	:
	bra storeNewPhraseIndex
rts
IncreaseCurrentPhrase:
	ldx CursorRow
	clc
	adc PhraseIndexes,x
	bcc:+
		lda #$FE
	:
	cmp #$FF
	bne :+
		dec
	:
	storeNewPhraseIndex:
	sta PhraseIndexes,x
	sta LastEditedPhrase
	lda TransposeValues,x
	sta LastEditedTranspose
	jsr PhraseIndexWasChanged
rts

MoveCursorDown:
	lda CursorRow
	inc
	sta CursorRow
	cmp #$10
	bne :+
		; TODO: Move down in current song
		stz CursorRow
	:
jmp ShowCursor
MoveCursorUp:
	dec CursorRow
	bpl :+
		; TODO: Move up in current song
		lda #$f
		sta CursorRow
	:
jmp ShowCursor
MoveCursorRight:
	lda CursorColumn
	beq :+
		rts
	:
	inc CursorColumn
jmp ShowCursor
MoveCursorLeft:
	dec CursorColumn
	bpl :+
		inc CursorColumn
		rts
	:
jmp ShowCursor

.export Chain_MovePhraseDown = MovePhraseDown, Chain_MovePhraseUp = MovePhraseUp
MovePhraseDown:
	ldx CursorRow
	cpx #$10
	beq @no
	lda PhraseIndexes+1,X
	cmp #$ff
	beq @no
@yes:
	jsr MoveCursorDown
	lda #1
rts
@no:
	; TODO: Ask Song
	lda #0
rts
MovePhraseUp:
	ldx CursorRow
	beq @no
	lda PhraseIndexes-1,X
	cmp #$ff
	beq @no
@yes:
	jsr MoveCursorUp
	lda #1
rts
@no:
	; TODO: Ask Song
	lda #0
rts

ShowCursor_long: jsr ShowCursor
rtl
ShowCursor:

	jsl UpdateChildView

	ldx TilemapOffset
	stx CursorOffset
	
	lda #2
	sta HighlightLength

	lda SelectionStart
	bmi :+
		jsr GetSelectionCoordinates
		jsr ClearSelectionArea

		lda CursorRow
		sta SelectionEnd
		jsr GetSelectionCoordinates
		jsr MarkSelectionArea
	:

	lda CursorColumn
	beq :+
		lda #3
	:
	sta CursorX
	
	lda CursorRow
	sta CursorY
	
	stz CursorSize

	lda #0
jmp UpdateCursorSpriteAndHighlight

GetSelectionCoordinates:
	;0 = X, 2 = Y, 4 = Width, 6 = Height
	lda SelectionEnd
	sta 2
	sec
	sbc SelectionStart
	bcs :+
		; Start was higher than End, so reverse the subtraction but keep the value stored in 2
		lda SelectionStart
		sec
		sbc SelectionEnd
		sta 6
		bra :++
	:
		sta 6
		lda SelectionStart
		sta 2
	:

	stz 0
	lda #5
	sta 4
	inc 6
rts

Copy:
	@rowCount = 0
	; Selection_Y is 2-3 and must be kept unaltered for the DeleteCopied routine

	jsr GetSelectionCoordinates
	stz Selection_Y+1
	ldy Selection_Y

	lda #2 ; 2 = Chain data
	sta f:Clipboard
	lda Selection_Height
	sta f:Clipboard+1
	sta @rowCount
	ldx #2

	@rowLoop:
		lda PhraseIndexes,Y
		sta f:Clipboard,X
		inx
		lda TransposeValues,Y
		sta f:Clipboard,X
		inx
		iny
		dec @rowCount
	bne @rowLoop
rts

DeleteCopied:
	@rowCount = 0 ; Expect @rowCount(0-1) and Selection_Y (2-3) to remain unaltered since the copy routine!
	
	lda f:Clipboard+1
	sta @rowCount
	ldy Selection_Y
	@rowLoop:
		lda #$ff
		sta PhraseIndexes,Y
		lda #0
		sta TransposeValues,Y
		iny
		dec @rowCount
	bne @rowLoop
rts


Cut:
	jsr Copy
	jsr DeleteCopied
jmp PhraseIndexWasChanged

Paste:
	@colCount = 0
	@rowCount = 2
	@yTemp = 8

	lda f:Clipboard
	cmp #2 ; Chain data
	beq :+
		rts
	:

	lda f:Clipboard+1
	sta @rowCount

	ldy CursorRow
	ldx #2
	@rowLoop:
		lda f:Clipboard,X
		sta PhraseIndexes,Y
		inx
		lda f:Clipboard,X
		sta TransposeValues,Y
		inx
		iny
		dec @rowCount
	bne @rowLoop
	jsl UpdateUnusedPhrases
jmp PhraseIndexWasChanged


CopyCurrentSongToSpcBuffer:
rts

.export Chain_UpdateHighlight = UpdateHighlight
UpdateHighlight_long: jsr UpdateHighlight
rtl
UpdateHighlight:
	lda IsPlaying
	asl
	bcc @removeHighlight ; Not playing chain or song

	seta16
	ldy #0
	:
		lda Playback_CurrentChainOffsetOfChannel,y
		and #$FFE0 ; Get start of the current chain
		cmp CurrentChainIndexInGlobalSong
		bne :+
			seta8
			lda Playback_CurrentChainOffsetOfChannel,y ; Just use the lower byte to find the row in chain
			and #$1F
			lsr
			clc
			adc #4
			jmp HighlightChainRow
			.a16
		:
		iny
		iny
		cpy #16
	bne :--
	seta8
	@removeHighlight:
	lda #$ff
jmp HighlightChainRow