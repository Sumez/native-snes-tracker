.include "global.inc"
.include "src/snes.inc"
.smart

.segment "BSS"

SongTilesBuffer = TilemapBuffer + $100
ChainIndexes = SONG

CursorPosition: .res 2
LastEditedChain: .res 8

.segment "CODE6"

.export Song_Init = Init
Init:
	ldx #$00
	;ldx #$ff TODO: If $ff, find first unused chain ID and use that
	stx LastEditedChain
	stx LastEditedChain+2
	stx LastEditedChain+4
	stx LastEditedChain+6
	
	ldx #0
	stx CursorPosition
rtl

.export Song_LoadView = LoadView
LoadView:

	Bind Input_StartPlayback, NoAction
	Bind Input_CustomHandler, HandleInput
	Bind Input_NavigateIn, NavigateToChain
	Bind Input_NavigateBack, NoAction
	Bind OnPlaybackStopped, NoAction
		
	jsl WriteTilemapBuffer
	jsl ShowCursor_long
rts

WriteTilemapBuffer:

	phb
	lda #^ChainIndexes
	pha
	plb
	ldy #0
	ldx #8
	@rowLoop:
		
		@colLoop:
			lda ChainIndexes,Y
			cmp #$ff
			beq :+
				; Write the number
				pha
				and #$F0
				lsr
				lsr
				lsr
				lsr
				ora #$40
				sta f:SongTilesBuffer,x
				pla
				and #$0F
				ora #$40
				sta f:SongTilesBuffer+2,x
				bra :++
			:
				; Empty cell	
				lda #$1d
				sta f:SongTilesBuffer,x
				lda #$1f
				sta f:SongTilesBuffer+2,x
			:
			inx
			inx
			inx
			inx
			inx
			inx
			iny
			tya
			and #7
		bne @colLoop
		seta16
		txa
		clc
		adc #($40-48)
		tax
		seta8
		
		cpy #168
	bne @rowLoop
	plb
rtl

.segment "CODE7"

PreparePlayback:
	jsr CopyCurrentSongToSpcBuffer
	; TODO: If currently playing, don't transfer yet, wait till song stops
	jsr TransferEntirePlaybackBufferToSpc
rts

ChainIndexWasChanged:
	jsl WriteTilemapBuffer
rts

GetActiveIndexesFromCursor:
	seta16
	lda CursorPosition
	tax ; Selected chain entry in X
	and #7
	tay ; Active column in Y
	seta8
rts

NoAction: rts
NavigateToChain:
	jsr GetActiveIndexesFromCursor
	lda f:ChainIndexes,x
	cmp #$ff
	beq :+
		tay
		lda #1
		jmp NavigateToScreen
	:
jmp PlayMosaic

; TODO: Set pointer variables to all the individual handlers like moveup, down, left, right, when loading the view
HandleInput:

	lda ButtonPushed
	bit #<KEY_X ; Key X removes current chain
	beq :+
		
		jsr GetActiveIndexesFromCursor
		lda f:ChainIndexes,x
		cmp #$ff
		beq :+ ; Don't do anything if cell is already empty
			
			sta LastEditedChain,y
			lda #$ff
			sta f:ChainIndexes,x
			stz EditMode
			jsr ChainIndexWasChanged
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
			
			; Check if chain exists, and if not, place the last one edited
			jsr GetActiveIndexesFromCursor
			lda f:ChainIndexes,x
			cmp #$ff
			bne :+
				; No chain exists. Use last edited
				lda LastEditedChain,y
				sta f:ChainIndexes,x
				jsr ChainIndexWasChanged
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
		jmp DecreaseCurrentChain
	:
	bit #>KEY_UP
	beq :+
		lda #$10
		jmp IncreaseCurrentChain
	:
	bit #>KEY_LEFT
	beq :+
		lda #(256-1)
		jmp DecreaseCurrentChain
	:
	bit #>KEY_RIGHT
	beq :+
		lda #1
		jmp IncreaseCurrentChain
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
		jmp MoveCursorLeft
	:
	bit #>KEY_RIGHT
	beq :+
		jmp MoveCursorRight
	:

rts

DecreaseCurrentChain:
	pha
	jsr GetActiveIndexesFromCursor
	pla
	clc
	adc f:ChainIndexes,x
		bcs:+
		lda #0
	:
	bra storeNewChainIndex
rts
IncreaseCurrentChain:
	pha
	jsr GetActiveIndexesFromCursor
	pla
	clc
	adc f:ChainIndexes,x
	bcc:+
		lda #$FE
	:
	cmp #$FF
	bne :+
		dec
	:
	storeNewChainIndex:
	sta f:ChainIndexes,x
	sta LastEditedChain,y
	jsr ChainIndexWasChanged
rts

MoveCursorUp:
	seta16
	lda CursorPosition
	sec
	sbc #8
	bcc :+
		sta CursorPosition
	:
	seta8
jmp ShowCursor
MoveCursorDown:
	seta16
	lda CursorPosition
	clc
	adc #8
	cmp #21*8
	bcs :+
		sta CursorPosition
	:
	seta8
jmp ShowCursor
MoveCursorLeft:
	lda CursorPosition
	bit #7
	beq :+
		dec CursorPosition
	:
jmp ShowCursor
MoveCursorRight:
	lda CursorPosition
	and #7
	cmp #7
	beq :+
		inc CursorPosition
	:
jmp ShowCursor

ShowCursor_long: jsr ShowCursor
rtl
ShowCursor:

	lda CursorPosition
	and #7
	sta 0
	asl
	adc 0 ; ASL+self = multiply by 3
	adc #4
	sta CursorX
	
	lda CursorPosition
	lsr
	lsr
	lsr ; Just divide by 8 to get row count
	clc
	adc #4
	sta CursorY
	
	stz CursorSize

jmp UpdateCursorSpriteAndHighlight


CopyCurrentSongToSpcBuffer:
rts