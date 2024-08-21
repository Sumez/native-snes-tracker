.include "global.inc"
.include "src/snes.inc"
.smart

.segment "CODE7"
Name: .byte "Song",$ff

.segment "BSS"

SongTilesBuffer = TilemapBuffer + $100
ChainIndexes = SONG

CursorPosition: .res 2
LastEditedChain: .res 8
TilemapOffset: .res 2

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

.export Song_FocusView = FocusView
FocusView:
	jsr LoadView

	Bind Input_StartPlayback, PlaySongFromSelectedRow
	Bind Input_CustomHandler, HandleInput
	Bind Input_NavigateIn, NavigateToChain
	Bind Input_NavigateBack, NoAction
	Bind OnPlaybackStopped, NoAction
	
	ldy #.loword(Name)
	jsl WriteTilemapHeader
	jsl ShowCursor_long
rts

.export Song_LoadView = LoadView
LoadView:
	ldx z:LoadView_TilemapOffset
	stx TilemapOffset
	jsl WriteTilemapBuffer
rts

WriteTilemapBuffer:

	phb
	lda #^ChainIndexes
	pha
	plb
	ldy #0
	ldx #8
	@colLoop:
		
		@rowLoop:
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
			seta16
			txa
			clc
			adc #$40
			tax
			seta8
			iny
			tya
			and #31 ; TODO: dynamically keep Y inside scroll window, maybe use indirect indexed to load?
		bne @rowLoop
		seta16
		tya
		clc
		adc #($100-32)
		tay
		
		txa
		sec
		sbc #(($40*32)-6)
		tax
		seta8
		
		cpy #$800
	bne @rowLoop
	plb
rtl

.segment "CODE7"

PreparePlayback:
	;jsl CopyCurrentSongToSpcBuffer
	; TODO: If currently playing, don't transfer yet, wait till song stops
	;jsr TransferEntirePlaybackBufferToSpc
rts

PlaySongFromSelectedRow:
	lda CursorPosition ; Low byte tells which row we are one
jmp PlayFullSong

ChainIndexWasChanged:
	jsl WriteTilemapBuffer
rts

GetActiveIndexesFromCursor:
	seta16
	lda CursorPosition
	tax ; Selected chain entry in X
	xba ; CursorPosition is $0xyy, where x = channel and yy = song row
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
	lda CursorPosition
	beq :+
		dec CursorPosition
	:
jmp ShowCursor
MoveCursorDown:
	lda CursorPosition
	cmp #21
	beq :+
		inc CursorPosition
	:
jmp ShowCursor
MoveCursorLeft:
	lda CursorPosition+1
	beq :+
		dec CursorPosition+1
	:
jmp ShowCursor
MoveCursorRight:
	lda CursorPosition+1
	cmp #7
	beq :+
		inc CursorPosition+1
	:
jmp ShowCursor

ShowCursor_long: jsr ShowCursor
rtl
ShowCursor:

	ldx TilemapOffset
	stx CursorOffset
	lda #24
	sta HighlightLength

	lda CursorPosition+1 ; Column/channel
	sta 0
	asl
	adc 0 ; ASL+self = multiply by 3
	sta CursorX
	
	lda CursorPosition ; Row
	sta CursorY
	
	stz CursorSize

jmp UpdateCursorSpriteAndHighlight

.export Song_UpdateHighlight = UpdateHighlight
UpdateHighlight:
	ldy #0
	ldx #0
	@loop:
		lda Playback_CurrentChainOffsetOfChannel+1,y ; Negative value if silent channel
		bmi :+
			lda Playback_CurrentSongRowOfChannel+1,y
			inc
			bmi :+ ; If high byte is negative after one addition, this means only one chain keeps looping, don't show anything in this view
				lda Playback_CurrentSongRowOfChannel,y ; Just the lower byte tells the row
				clc
				adc #4 ; TODO: Account for inner Y "scroll" of rows
				bra :++
		:
			lda #$ff
		:
		; [X] Has actual channel number
		jsr HighlightChannel
		iny
		iny
		inx
		cpx #8
	bne @loop
rts