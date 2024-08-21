.include "global.inc"
.include "src/snes.inc"
.smart
.import PlaySingleChain

.segment "CODE7"
Name: .byte "Chain_-_",$ff

.segment "BSS"

;Needs init:
CursorRow: .res 2
LastEditedPhrase: .res 1
LastEditedTranspose: .res 1

;Loaded when view loads:
PhraseIndexes: .res $10
TransposeValues: .res $10
CurrentChainIndex: .res 1
CurrentChainIndexInGlobalSong: .res 2
TilemapOffset: .res 2

.segment "CODE6"

.export Chain_Init = Init
Init:
	stz LastEditedPhrase
	stz LastEditedTranspose
	
	ldx #0
	stx CursorRow
rtl

.export Chain_FocusView = FocusView
FocusView:
	jsr LoadView
	
	ldy #.loword(Name)
	jsl WriteTilemapHeader
	lda CurrentChainIndex
	jsl WriteTilemapHeaderId

	Bind Input_StartPlayback, StartPlayback
	Bind Input_CustomHandler, HandleInput
	Bind Input_NavigateIn, NavigateToPhrase
	Bind Input_NavigateBack, NavigateToSong
	Bind OnPlaybackStopped, NoAction
	
	jsl UpdateHighlight_long
	jsl ShowCursor_long
rts

.export Chain_LoadView = LoadView
LoadView:
	ldx z:LoadView_TilemapOffset
	stx TilemapOffset
	tya
	cmp #$ff
	beq :+
		jsl LoadGlobalData
	:
		
	jsl WriteTilemapBuffer

rts

.import Pattern_LoadView, Pattern_HideView
UpdateChildView:

	seta16
	lda TilemapOffset
	clc
	adc #28
	sta z:LoadView_TilemapOffset
	seta8
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
	; TODO: Update only currently selected phrase entry
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
NavigateToPhrase:
	ldx CursorRow
	lda PhraseIndexes,x
	cmp #$ff
	beq :+
		tay
		lda #2
		jmp NavigateToScreen
	:
jmp PlayMosaic

; TODO: Set pointer variables to all the individual handlers like moveup, down, left, right, when loading the view
HandleInput:

	lda ButtonPushed
	bit #<KEY_X ; Key X removes current phrase
	beq :+
		
		ldx CursorRow
		lda PhraseIndexes,x
		cmp #$ff
		beq :+ ; Don't do anything if cell is already empty
			
			sta LastEditedPhrase
			lda TransposeValues,x
			sta LastEditedTranspose
			lda #$ff
			sta PhraseIndexes,x
			stz EditMode
			jsr PhraseIndexWasChanged
			jmp ShowCursor
			;RETURNS - no more inputs read this frame
	:
	
	lda ButtonStates+1 ; Key Y enters edit mode while held
	bit #>KEY_Y
	bne :+
		lda EditMode
		beq @continue
			stz EditMode
			jsr ShowCursor
			bra @continue
	:
		lda EditMode
		bne @continue

			lda #1
			sta EditMode
			
			; Check if phrase exists, and if not, place the last one edited
			ldx CursorRow
			lda PhraseIndexes,x
			cmp #$ff
			bne :+
				; No phrase exists. Use last edited
				lda LastEditedPhrase
				sta PhraseIndexes,x
				lda LastEditedTranspose
				sta TransposeValues,x
				jsr PhraseIndexWasChanged
			:
			jsr ShowCursor

	@continue:
	
	; If in edit mode, branch to input relevant to that. if not, branch to internal navigation
	; If L button held, branch to global navigation (handle in editor.s?)
	lda EditMode
	beq @Navigation

@EditMode:

	lda ButtonPushed+1
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
;		jmp MoveCursorLeft
	:
	bit #>KEY_RIGHT
	beq :+
;		jmp MoveCursorRight
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

ShowCursor_long: jsr ShowCursor
rtl
ShowCursor:

	jsl UpdateChildView

	ldx TilemapOffset
	stx CursorOffset
	
	lda #2
	sta HighlightLength

	lda #0
	sta CursorX
	
	lda CursorRow
	sta CursorY
	
	stz CursorSize

jmp UpdateCursorSpriteAndHighlight


CopyCurrentSongToSpcBuffer:
rts

.export Chain_UpdateHighlight = UpdateHighlight
UpdateHighlight_long: jsr UpdateHighlight
rtl
UpdateHighlight:
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
	lda #$ff
jmp HighlightChainRow