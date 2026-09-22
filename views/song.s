.include "global.inc"
.include "src/snes.inc"
.smart

.segment "CODE7"
Name: .byte "(Untitled_song)",$ff

.segment UnusedItemsSegment
UnusedChains: .res $100

.segment "BSS"

ChainIndexes = SONG
VisibleRows = 19

CursorPosition: .res 2
LastEditedChain: .res 8
TilemapOffset: .res 2
SelectionStart: .res 2
SelectionEnd: .res 2
SongScroll: .res 2

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
	stx SongScroll
rtl

.export Song_FocusView = FocusView
FocusView:
	jsr LoadView

	Bind Input_StartPlayback, PlaySongFromSelectedRow
	Bind Input_CustomHandler, HandleInput
	Bind Input_NavigateIn, NavigateToChain
	Bind Input_NavigateBack, NoAction

	Bind Input_Erase, Erase
	Bind Input_StartSelection, StartSelection
	Bind Input_EndSelection, EndSelection
	Bind Input_CopySelection, Copy
	Bind Input_CutSelection, Cut
	Bind Input_Paste, Paste
	Bind Input_Clone, Clone

	Bind OnPlaybackStopped, NoAction
	
	ldy #$ffff
	sty SelectionStart
	
	ldy #.loword(Name)
	jsl WriteTextToHeader
	jsl ShowCursor_long
	jsl SongScrolled_long
rts

.export Song_LoadView = LoadView
LoadView:
	ldx z:LoadView_TilemapOffset
	stx TilemapOffset
	jsl WriteTilemapBuffer
	ldx #0
	jsl LoadGuiMap
	jsl UpdateUnusedChainsGlobal
rts

WriteTilemapBuffer:
@remainingRows = 0
@remainingCols = 1

	phb
	lda #^ChainIndexes
	pha
	plb
	ldy SongScroll
	ldx TilemapOffset
	lda #8
	sta @remainingCols
	@colLoop:
		lda #VisibleRows
		sta @remainingRows
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
				sta f:TilemapBuffer,x
				pla
				and #$0F
				ora #$40
				sta f:TilemapBuffer+2,x
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
			dec @remainingRows
			;tya
			;and #31
		bne @rowLoop
		seta16
		tya
		clc
		adc #($100-VisibleRows)
		tay
		
		txa
		sec
		sbc #(($40*VisibleRows)-6)
		tax
		seta8
		
		dec @remainingCols
		;cpy #$800
	bne @colLoop
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

UpdateUnusedChains:
	lda #0
	xba
	ldx CursorPosition
	lda f:SONG,x
	tax
	lda #$ff
	sta f:UnusedChains,X
rts

UpdateUnusedChainsGlobal:
; This takes half a frame, so we only run it on paste, cut and view load
	phb
	lda #^UnusedChains
	pha
	plb
	setxy8
	ldx #0
	:
		stz UnusedChains,X
		inx
	bne :-

	lda #^SONG
	pha
	plb
	ldy #0
	lda #$ff
	:
		ldx SONG+$000,y
		sta f:UnusedChains,X
		ldx SONG+$100,y
		sta f:UnusedChains,X
		ldx SONG+$200,y
		sta f:UnusedChains,X
		ldx SONG+$300,y
		sta f:UnusedChains,X
		ldx SONG+$400,y
		sta f:UnusedChains,X
		ldx SONG+$500,y
		sta f:UnusedChains,X
		ldx SONG+$600,y
		sta f:UnusedChains,X
		ldx SONG+$700,y
		sta f:UnusedChains,X
		iny
	bne :-
	setxy16
	
	plb
rtl

GetNextUnusedChain:

	phb
	lda #^CHAINS
	pha
	plb
	
	lda #0
	xba
	lda f:SONG,X ; Start looking from the currently selected chain index
	tax
	@loop:
		inx ; Always start checkinx the next index (if it's not $100)
		cpx #$100
		beq @nothingFound
		lda f:UnusedChains,x
		bne @loop
			; Seems unused. Read values of chain to check if it is
			jsr @ReturnIfChainHasData
			; If routine returns normally, proceed to check next chain
	bra @loop
	
	@nothingFound:
	lda #$ff
	plb
rts
@ReturnIfChainHasData:
	seta16
	txa
	; Every Chain is $20 bytes, so shift left 5 times (x32)
	asl
	asl
	asl
	asl
	asl
	tay
	seta8
	lda CHAINS+0,Y
	and CHAINS+2,Y
	and CHAINS+4,Y
	and CHAINS+6,Y
	and CHAINS+8,Y
	and CHAINS+10,Y
	and CHAINS+12,Y
	and CHAINS+14,Y
	and CHAINS+16,Y
	and CHAINS+18,Y
	and CHAINS+20,Y
	and CHAINS+22,Y
	and CHAINS+24,Y
	and CHAINS+26,Y
	and CHAINS+28,Y
	and CHAINS+30,Y
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


StartSelection:
	ldx CursorPosition ; Load both 8 bit values of CursorPosition (X/Y) and store as both selection start and end, without breakin the A register
	stx SelectionStart
	stx SelectionEnd
jmp ShowCursor

Erase:
	jsr GetActiveIndexesFromCursor
	lda f:ChainIndexes,x
	cmp #$ff
	beq :+ ; Don't do anything if cell is already empty
		
		sta LastEditedChain,y
		lda #$ff
		sta f:ChainIndexes,x
		stz EditMode
		jsr ChainIndexWasChanged
	:
jmp ShowCursor

Clone:
	jsr GetActiveIndexesFromCursor
	lda f:ChainIndexes,x
	cmp #$ff
	beq :++ ; Don't do anything if cell is already empty
		sta 0
		phx
		phy
		jsr GetNextUnusedChain
		cmp #$ff
		beq :+ ; No unused chain found
		
			ply
			plx
			sta f:ChainIndexes,x
			sta LastEditedChain,y
			stz ExpectDoubleTap
			jsr Clone0IntoA
			jmp ChainIndexWasChanged
		:
		ply
		plx
	:
rts

Clone0IntoA: ; Clone data from the chain index stored in $00 into the one stored in A
	seta16
	and #$00ff
	asl
	asl
	asl
	asl
	asl
	tax
	
	lda 0
	and #$00ff
	asl
	asl
	asl
	asl
	asl
	tay
	seta8
	
	phb
	lda #^CHAINS
	pha
	plb
	
	lda #32
	sta 0 ; 0 has been read and handled, now we can use 0 as the loop counter
	@loop:
		lda CHAINS,Y
		sta CHAINS,X
		iny
		inx
		dec 0
	bne @loop
	plb
rts

EndSelection:
	ldx TilemapOffset
	stx CursorOffset
	jsr GetSelectionCoordinates
	jsr ClearSelectionArea
	ldx #$ffff
	stx SelectionStart
rts

ColOffsets:
.word 0, $100, $200, $300, $400, $500, $600, $700
Copy:
	@colCount = 0
	@rowCount = 2
	@yTemp = 8
	@yStart = 10

	jsr GetSelectionIndexes
	stz Selection_X+1
	stz Selection_Y+1
	
	seta16
	lda Selection_X
	asl
	tax
	lda Selection_Y
	clc
	adc ColOffsets,X
	sta @yStart
	tay
	seta8

	phb
	lda #^ChainIndexes
	pha
	plb

	lda #1 ; 1 = Song data
	sta f:Clipboard
	lda Selection_Width
	inc a
	sta f:Clipboard+1
	sta @colCount
	lda Selection_Height
	inc a
	sta f:Clipboard+2
	sta @rowCount
	ldx #3

	@colLoop:
		sty @yTemp
		@rowLoop:
			lda ChainIndexes,Y
			sta f:Clipboard,X
			inx
			iny
			dec @rowCount
		bne @rowLoop
		seta16
		lda @yTemp
		clc
		adc #$100
		tay
		seta8
		lda f:Clipboard+2
		sta @rowCount
		dec @colCount
	bne @colLoop
	
	plb
rts

DeleteCopied:
	; Reuse temp values from the copy routine
	@colCount = 0
	@rowCount = 2
	@xTemp = 8
	@xStart = 10

	ldx @xStart
	lda f:Clipboard+1
	sta @colCount
	lda f:Clipboard+2
	sta @rowCount
	
	@colLoop:
		lda #$ff
		stx @xTemp
		@rowLoop:
			sta f:ChainIndexes,X
			inx
			dec @rowCount
		bne @rowLoop
		seta16
		lda @xTemp
		clc
		adc #$100
		tax
		seta8
		lda f:Clipboard+2
		sta @rowCount
		dec @colCount
	bne @colLoop
rts

Cut:
	jsr Copy
	jsr DeleteCopied
	jsl UpdateUnusedChainsGlobal
jmp ChainIndexWasChanged

Paste:
	@colCount = 0
	@rowCount = 2
	@yTemp = 8

	lda f:Clipboard
	cmp #1 ; Song data
	beq :+
		rts
	:

	lda f:Clipboard+1
	sta @colCount
	lda f:Clipboard+2
	sta @rowCount

	phb
	lda #^ChainIndexes
	pha
	plb

	ldy CursorPosition
	ldx #3
	@colLoop:
		sty @yTemp
		@rowLoop:
			lda f:Clipboard,X
			sta ChainIndexes,Y
			inx
			iny
			dec @rowCount
		bne @rowLoop
		seta16
		lda @yTemp
		clc
		adc #$100
		tay
		seta8
			cpy #$800
			bcs @break ;If the copied block spans more columns than we can insert due to current cursor position, ignore the rest
		lda f:Clipboard+2
		sta @rowCount
		dec @colCount
	bne @colLoop
	@break:
	
	plb
	jsl UpdateUnusedChainsGlobal
jmp ChainIndexWasChanged

; TODO: Set pointer variables to all the individual handlers like moveup, down, left, right, when loading the view
HandleInput:

	lda ButtonStates+1
	bit #>KEY_Y ; Key Y exits edit mode when not held
	bne :+
		lda EditMode
		beq @continue
			stz EditMode
			jsr UpdateUnusedChains
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
			
			; Check if chain exists, and if not, place the last one edited
			jsr GetActiveIndexesFromCursor
			lda f:ChainIndexes,x
			cmp #$ff
			beq :++
				; Chain exists. Don't do anything yet, but listen for doubletap and store the index to recall later
				bit ExpectDoubleTap
				bmi :+
					sta LastEditedChain,y
					bra @showCursor
				:

				phx
				phy
				jsr GetNextUnusedChain ; This can take a while
				ply
				plx
				cmp #$ff
				beq @showCursor ; No unused chain found (wow)

				sta f:ChainIndexes,x
				sta LastEditedChain,y
				stz ExpectDoubleTap
				jsr ChainIndexWasChanged
				bra @showCursor
			:
				; No chain exists. Use last edited
				dec ExpectDoubleTap ; listen for double tap on next Y push
				lda LastEditedChain,y
				sta f:ChainIndexes,x
				jsr ChainIndexWasChanged
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
		lda CursorPosition
		cmp SongScroll
		bcs :+
		
			; Scrolled above lowest visible row
			dec SongScroll
			jsr SongScrolled
	:
jmp ShowCursor
MoveCursorDown:
	lda CursorPosition
	cmp #$ff
	beq :+
		inc CursorPosition
		lda CursorPosition
		sec
		sbc SongScroll
		cmp #VisibleRows
		bcc :+
		
			; Scrolled below lowest visible row
			inc SongScroll
			jsr SongScrolled
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

LineNumberYCoords:
@ycoord .set 53
.repeat VisibleRows
	.byte @ycoord
	@ycoord .set @ycoord+8
.endrepeat
.importzp LineNumberSpriteStartIndex
SongScrolled_long: jsr SongScrolled
rtl
SongScrolled: ; When scrolled, update the line number sprites
	ldx #LineNumberSpriteStartIndex*4
	ldy	#0
	lda #0
	xba
	lda SongScroll
	@loop:
		bit #$03
		bne :+
			sta 0
			lsr
			lsr
			lsr
			lsr
			ora #$20
			sta OamBuffer+2+0,X
			lda 0
			and #$0f
			ora #$20
			sta OamBuffer+2+4,X
			lda LineNumberYCoords,Y
			sta OamBuffer+1+0,X
			sta OamBuffer+1+4,X
			txa
			clc
			adc #8
			tax
			lda 0
		:
		inc a
		iny
		cpy #VisibleRows
	bne @loop
	cpx #(LineNumberSpriteStartIndex*4)+(8*4)
	bne :+
		lda #224
		sta OamBuffer+1+0,X
		sta OamBuffer+1+4,X
	:
	jsl WriteTilemapBuffer
rts

ShowCursor_long: jsr ShowCursor
rtl
ShowCursor:
	
	ldx TilemapOffset
	stx CursorOffset
	lda #24
	sta HighlightLength

	ldx SelectionStart
	bmi :+
		jsr GetSelectionCoordinates
		jsr ClearSelectionArea

		ldx CursorPosition
		stx SelectionEnd
		jsr GetSelectionCoordinates
		jsr MarkSelectionArea
	:

	lda CursorPosition+1 ; Column/channel
	sta 0
	asl
	adc 0 ; ASL+self = multiply by 3
	sta CursorX
	
	lda CursorPosition ; Row
	sec
	sbc SongScroll
	sta CursorY
	
	stz CursorSize

	lda #0
jmp UpdateCursorSpriteAndHighlight

GetSelectionIndexes:
	;0 = X, 2 = Y, 4 = Width, 6 = Height
	lda SelectionEnd
	sta Selection_Y
	sec
	sbc SelectionStart
	bcs :+
		; Start was higher than End, so reverse the subtraction but keep the value stored in 2
		lda SelectionStart
		sec
		sbc SelectionEnd
		sta Selection_Height
		bra :++
	:
		sta Selection_Height
		lda SelectionStart
		sta Selection_Y
	:

	lda SelectionEnd+1
	sta Selection_X
	sec
	sbc SelectionStart+1
	bcs :+
		; Start was higher than End, so reverse the subtraction but keep the value stored in 2
		lda SelectionStart+1
		sec
		sbc SelectionEnd+1
		sta Selection_Width
		bra :++
	:
		sta Selection_Width
		lda SelectionStart+1
		sta Selection_X
	:
rts
GetSelectionCoordinates:
	jsr GetSelectionIndexes	; First get the selection indexes, and then convert to tile coordinates, also accounting for scroll
	; Multiply X and Width by 3
	lda Selection_X
	asl
	adc Selection_X
	sta Selection_X ; x

	lda Selection_Width
	inc a ; Add 1 (selection includes last cell)
	asl
	adc Selection_Width
	sta Selection_Width ; width

	lda Selection_Y ; Subtract scroll from Y coord, but minimum 0
	sec
	sbc SongScroll
	bcs :+
		adc Selection_Height
		sta Selection_Height ; Subtract the difference from the displayed height
		lda #0
	:
	sta Selection_Y
	
	inc Selection_Height ; Add 1 (selection includes last cell)
rts


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