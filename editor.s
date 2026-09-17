.include "global.inc"
.include "src/snes.inc"
.smart

.import Vfx_Update, Vfx_ResetOnNavigation, Vfx_Init, Playback_Update, StopPlayback
.exportzp LineNumberSpriteStartIndex
LineNumberSpriteStartIndex = 4


.segment TilemapBufferSegment

GuiTilemapBuffer:
.res 32*28*2
TilemapBuffer:
.res 32*32*2
.res $200 ; TODO: (remove) buffer because our song view currently overflows the tilemap buffer...
BackdropTilemapBuffer:
.res 32*32*2

.segment ClipboardSegment

Clipboard:
.res $803 	; Max clipboard contents is a completely selected song (+ header) for no reason.
			; Should be no issue reducing the max select size in song view to save ram space when we need it

.ifdef HIROM

	.segment "SRAM1"
	.assert * = $306000, error, "Song data must start at $0000 in sram for compatibility"
	HEADER: .res $30 ; Bytes to spare
	TITLE: .res $40
	AUTHOR: .res $40
	SONG: .res $800 ; $0-FF, 8 channels per row
	SAMPLES: .res 16*64 ; 64 entries, with an index and 14-char name-check each
	.segment "SRAM2"
	CHAINS: .res $2000 ; 2 bytes (phrase ref and transpose), 16 rows, $100 different chains of $20 each
	.segment "SRAM3"
	PHRASES_1: .res $2000 ; 4 bytes (note, instr, command, cmdparam) * 16 rows. $80 different phrases of $40 each
	.segment "SRAM4"
	PHRASES_2: .res $2000 ; 4 bytes (note, instr, command, cmdparam) * 16 rows. $80 different phrases of $40 each
	.segment "SRAM5"
	INSTRUMENTS: .res $200 ; 64 (max 53) instruments of 8 bytes each
	META: .res $800 ; Future feature: custom meta data for song - colors to help with navigation etc.
	.segment "SRAM6"
	PHRASES_3: .res $2000 ; For future expansion
	.segment "SRAM7"
	PHRASES_4: .res $2000

.else

	.segment SongDataSegment
	.assert * = $700000, error, "Song data must start at $0000 in sram for compatibility"
	HEADER: .res $30 ; Bytes to spare
	TITLE: .res $40
	AUTHOR: .res $40
	SONG: .res $800 ; $0-FF, 8 channels per row
	CHAINS: .res $2000 ; 2 bytes (phrase ref and transpose), 16 rows, $100 different chains of $20 each
	PHRASES_1: .res $2000 ; 4 bytes (note, instr, command, cmdparam) * 16 rows. $100 different phrases of $40 each
	PHRASES_2: .res $2000 ; 4 bytes (note, instr, command, cmdparam) * 16 rows. $100 different phrases of $40 each
	INSTRUMENTS: .res $200 ; ??
	META: .res $800 ; Future feature: custom meta data for song - colors to help with navigation etc.
	PHRASES_3: .res $2000
	PHRASES_4: .res $2000

.endif

.segment "ZEROPAGE"
; Input handler pointers
Input_StartPlayback: .res 2
Input_CustomHandler: .res 2
Input_NavigateIn: .res 2
Input_NavigateBack: .res 2
Input_MoveUp: .res 2
Input_MoveDown: .res 2
Input_MoveLeft: .res 2
Input_MoveRight: .res 2
Input_StartSelection: .res 2
Input_EndSelection: .res 2
Input_CopySelection: .res 2
Input_CutSelection: .res 2
Input_Paste: .res 2
Input_Erase: .res 2
Input_Clone: .res 2
OnPlaybackStopped: .res 2

.segment "BSS"
CurrentScreen: .res 1
PreviousScreen: .res 1
ResetStack: .res 2
SelectingActive: .res 1
ExpectDoubleTap: .res 1

.segment CompiledPlaybackDataSegment
; Share the same generic RAM space for both the storage handler and compiled playback
CompiledPattern:
StorageBuffer:
.res $8000
.res $8000


.segment "CODE7"

InitMidScreen:
	wai
	stz PPUNMI
	lda #FORCEBLANK
	sta PPUBRIGHT

InitEditor:
;.import TestRleCompression
;jsl TestRleCompression
.import StoreSongIntoBuffer
jsl StoreSongIntoBuffer


.export InitEditor
.import LoadTextGraphics, LoadPalettes
.import Instrument_Init, Cursor_Init, Pattern_Init, Chain_Init, Song_Init, Samples_Init

	; SONG LOAD - TODO: Handle this BEFORE going into the editor
	jsr LoadSong
	.import UpdateUnusedPhrasesGlobal
	jsl UpdateUnusedPhrasesGlobal
	; SONG LOAD
	
	; INIT RAM VALUES - Initiate each editor once. Anything that needs to be reset whenever navigating there should be in LoadView
jsr ResetSprites
jsr FinalizeSprites
LoadBlockToOAM OamBuffer, 544

	stz Clipboard
	jsl Instrument_Init
	jsl Samples_Init
	jsl Cursor_Init
	jsl Vfx_Init
	jsl Song_Init
	jsl Chain_Init
	jsl Pattern_Init

.import FragmentedRemainingBytes
ldx #0
stx FragmentedRemainingBytes ; TODO: Init routine for playback handler
	
	; MESS WITH VRAM AFTER THIS POINT
	stz $420C ; Halt any potential HDMA, that might interfer with our VRAM access
	jsl LoadTextGraphics
	jsl LoadGuiGraphics
	
	phk
	plb
	

	jsr LoadPalettes
	
	jsl LoadBackdropUi
	ldx #Bg3TileMapBase>>1
	stx Bg3Offset
	jsl CopyBackdropTilemap


ldx #2
stx ScrollY ; TODO: Dedicated GUI handler?

	
	lda #0
	sta CurrentScreen
	jsl LoadView
	
	jsl CopyEntireTilemap

	stz PPUBRIGHT

	lda #1 | %00001000 ; High priority background layer
	sta BGMODE       ; Set Mode to 1

	lda #Bg1TileMapBase >> 9 | 0
	sta NTADDR       ; Set BG1's Tile Map VRAM offset to $0800 (word address) and the Tile Map size to 32x32tiles
	lda #Bg2TileMapBase >> 9 | 2
	sta NTADDR+1       ; Set BG2's Tile Map VRAM offset to $f000 (word address) and the Tile Map size to 32x64tiles
	lda #Bg3TileMapBase >> 9 | 2
	sta NTADDR+2       ; Set BG3's Tile Map VRAM offset to $f000 (word address) and the Tile Map size to 32x64tiles

	lda #Bg1ChrBase >> 13 | Bg2ChrBase >> 9
	sta BGCHRADDR       ; Set BG1's Character VRAM offset to $0000 (word address)
	lda #Bg3ChrBase >> 13 | Bg4ChrBase >> 9
	sta BGCHRADDR+1       ; Set BG1's Character VRAM offset to $0000 (word address)

	; (alternate between $f000 and $f800

	lda #(SpriteChrBase >> 14) | OBSIZE_8_16
	sta OBSEL



	;lda #%00000111
	;lda #%00010100
	lda #%00010011
	sta BLENDMAIN
	lda #%00000000
	sta BLENDSUB
	
	lda #%00110000
	sta CGWSEL
	lda #%00000000
	sta ColorBlend
	sta CGADSUB

	lda #VBLANK_NMI|AUTOREAD
	sta PPUNMI
	wai

	ldx	#0

FadeSpeed = 2
setxy8
lda #$ff
ldy #0
ldx #FadeSpeed
sec
:
	sty PPUBRIGHT
	sta MOSAIC
	wai
.ifdef HALT
@stay: bra @stay
.endif
	dex
	bne :-
	ldx #FadeSpeed
	iny
	sbc #$10
	cmp #$0f
bne :-
sty PPUBRIGHT
stz MOSAIC
setxy16

	cli ; Allow interrupts
	
tsx
stx ResetStack ; Store neutral stack pointer to restore on navigation, preventing stack overflow (shouldn't be an issue normally since stack just wraps, but it can be helpful in case we need manipulate the stack space for other purposes in the future)
MainLoop:


	:
		wai
		lda NMISTATUS
	bpl :-	

	jsr Playback_Update
	jsr Vfx_Update
	jsr UpdateInputStates
	jsr HandleInput

jmp MainLoop

NavigateToScreen:
	cmp CurrentScreen
	bne :+
		; Can't navigate to "modal" view if it's already open. Main views don't have that limitation due to multi-view support
		cmp #$10
		bcs	@return
	:
	
	pha
	lda CurrentScreen
	sta PreviousScreen
	pla
	sta CurrentScreen
	jsl Vfx_ResetOnNavigation
	jsl LoadView
	wai
	;jsl CopyEntireTilemap ; TODO: add this if the call to this every NMI gets removed
@return:
	ldx ResetStack
	txs
jmp MainLoop

NavigateToPreviousScreen:
	lda PreviousScreen
	ldy #$ff ; Reuse what was already loaded if relevant
jmp NavigateToScreen

LoadSong:
@HeaderVerificationCode = $EFCD ; Constant, indicates Initialized SRAM data
@SaveBreakingBuildVersion = 2;TODO! 	; Increased every time a public build breaks older save data. If feeling nice, create code to convert from one version to another
	phb
	lda #^HEADER
	pha
	plb

	ldx HEADER
	lda HEADER+2
	cpx #@HeaderVerificationCode
	bne @resetData
	cmp #@SaveBreakingBuildVersion
	; TODO: Give user a nice message asking them if they want to reset the data, or give them a chance to back it up first
	bne :+
		plb
		rts ; Valid song already present, use sram as-is
	:
		
	; Valid song data but old version:
	cmp #0
	bne :+
		jmp @resetSamples ; 0->1/2 = Add default "addedsamples"
	:
	cmp #1
	bne :+
		jmp @resetSamples ; 1->2 = Fill more empty instrument data. Also: updated samples :\
	:
	@resetData:	
	lda #$ff
	ldx #0
	:	sta f:SONG,X
		inx
		cpx #$800
	bne :-

	lda #$ff
	ldx #0
	:	sta f:CHAINS,X
		inx
		cpx #$2000
	bne :-

	lda #$ff
	ldx #0
	:	sta f:PHRASES_1,X
		inx
		cpx #$2000
	bne :-

	lda #$ff
	ldx #0
	:	sta f:PHRASES_2,X
		inx
		cpx #$2000
	bne :-

	lda #$ff
	ldx #0
	:	sta f:PHRASES_3,X
		inx
		cpx #$2000
	bne :-

	lda #$ff
	ldx #0
	:	sta f:PHRASES_4,X
		inx
		cpx #$2000
	bne :-
	
	lda #$00
	ldx #0
	:	sta f:META,X
		inx
		cpx #$800
	bne :-
	
	@resetSamples: .import DefaultSamples, DefaultSamplesEnd
	ldx #0
	:
		lda f:DefaultSamples,X
		sta f:SAMPLES,X
		inx
		cpx #(DefaultSamplesEnd-DefaultSamples)
	bne :-
	lda #$ff
	:
		sta f:SAMPLES,X
		inx
		cpx #(16*64)
	bne :-
	
	@resetInstruments:
	lda #$ff
	ldx #0
	:
		sta f:INSTRUMENTS,X
		inx
		cpx #$200
	bne :-
		
	
	ldx #@HeaderVerificationCode
	stx HEADER
	lda #@SaveBreakingBuildVersion
	sta HEADER+2

	plb
	
rts

.macro jumpTable TableReference
	lda #0
	xba
	lda CurrentScreen
	asl
	tax
	jsr (.loword(TableReference), X)
.endmacro

.export CopyEntireTilemap, CopyBackdropTilemap
CopyEntireTilemap:
	LoadBlockToVRAM TilemapBuffer, Bg2TileMapBase, 32*29*2
rtl
CopyBackdropTilemap:
	LoadBlockToOffsetVRAM BackdropTilemapBuffer, Bg3Offset, 32*29*2
rtl

.segment "CODE6"

LoadBackdropUi:
	seta16
	;lda #($3f*2) ; Blank space - * 2 because it's 2bpp
	lda #$08df
	ldx #((32*32*2)-2)
	:
		sta f:BackdropTilemapBuffer,x
		dex
		dex
	bpl :-
	seta8
rtl
ShowPatternBackdrop:
	lda #1
	sta ShowBg3
	lda #$04
	ldx #$0FE+$C0+$24
	:
		sta f:BackdropTilemapBuffer+1,x
		inx
		inx
		cpx #$116+$C0+$24
	bne :-
	ldx #$1FE+$C0+$24
	:
		sta f:BackdropTilemapBuffer+1,x
		inx
		inx
		cpx #$216+$C0+$24
	bne :-
	ldx #$2FE+$C0+$24
	:
		sta f:BackdropTilemapBuffer+1,x
		inx
		inx
		cpx #$316+$C0+$24
	bne :-
	ldx #$3FE+$C0+$24
	:
		sta f:BackdropTilemapBuffer+1,x
		inx
		inx
		cpx #$416+$C0+$24
	bne :-
rts
ShowClearBackdrop:
	lda ShowBg3
	bne :+
		; Already cleared, no reason to do more
		rts
	:
	stz ShowBg3
	;lda #($3f*2) ; Blank space - * 2 because it's 2bpp
	lda #$08
	ldx #$0FE+$C0+$24
	:
		sta f:BackdropTilemapBuffer+1,x
		inx
		inx
		cpx #$116+$C0+$24
	bne :-
	ldx #$1FE+$C0+$24
	:
		sta f:BackdropTilemapBuffer+1,x
		inx
		inx
		cpx #$216+$C0+$24
	bne :-
	ldx #$2FE+$C0+$24
	:
		sta f:BackdropTilemapBuffer+1,x
		inx
		inx
		cpx #$316+$C0+$24
	bne :-
	ldx #$3FE+$C0+$24
	:
		sta f:BackdropTilemapBuffer+1,x
		inx
		inx
		cpx #$416+$C0+$24
	bne :-
rts

ClearTilemap:
	seta16
	lda #'_' ; Blank space
	ldx #((32*32*2)-2)
	:
		sta f:TilemapBuffer,x
		dex
		dex
	bpl :-
	seta8
rts

WriteTilemapHeader:
	sty 0
	ldy #0
	ldx #$C8
	:
		lda (0),y
		bpl :+
			rtl
		:
		sta f:TilemapBuffer,x
		iny
		inx
		inx
	bra :--
	
WriteTilemapHeaderId:
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
rtl


LoadView:
	jsr ClearTilemap
	;stz ShowBg3
	jsr ShowClearBackdrop
	stz SelectingActive
	stz ExpectDoubleTap
	ldx #$1C8
	stx z:LoadView_TilemapOffset
	
	Bind Input_Erase, NoAction
	Bind Input_StartSelection, NoAction
	Bind Input_EndSelection, NoAction
	Bind Input_CopySelection, NoAction
	Bind Input_CutSelection, NoAction
	Bind Input_Paste, NoAction
	Bind Input_Clone, NoAction
	
	jsl LoadLineSprites
	jumpTable ViewLoaders
rtl
ViewLoaders: .import Song_FocusView, Chain_FocusView, Pattern_FocusView, Instrument_FocusView, Samples_FocusView
.addr Song_FocusView, Chain_FocusView, Pattern_FocusView, Instrument_FocusView, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.addr 0, Samples_FocusView

.segment "CODE7"

NoAction: rts
HandleInput:

	lda ButtonPushed+1
	bit #>KEY_START
	beq @didNotPushStart
	
		lda IsPlaying
		bne :++
			lda ButtonStates+1
			bit #>KEY_SELECT
			beq :+
				lda #0
				jmp PlayFullSong
			:			
			jmp (Input_StartPlayback)
			;RETURNS - no more inputs read this frame
		rts
		:
			jmp StopPlayback
			;RETURNS - no more inputs read this frame
	
	@didNotPushStart:

	lda ButtonPushed
	and #KEY_R
	beq :+
		lda #$11
		jmp NavigateToScreen
	:

	lda ButtonStates+1
	and #>KEY_SELECT
	ora ButtonPushed+1
	and #>KEY_SELECT|>KEY_RIGHT
	cmp #>KEY_SELECT|>KEY_RIGHT
	bne :+
		jmp (Input_NavigateIn)
		;RETURNS - no more inputs read this frame
	:

	lda ButtonStates+1
	and #>KEY_SELECT
	ora ButtonPushed+1
	and #>KEY_SELECT|>KEY_Y
	cmp #>KEY_SELECT|>KEY_Y
	bne :+
		jmp (Input_Clone)
		;RETURNS - no more inputs read this frame
	:

	lda ButtonStates+1
	and #>KEY_SELECT
	ora ButtonPushed+1
	and #>KEY_SELECT|>KEY_LEFT
	cmp #>KEY_SELECT|>KEY_LEFT
	bne :+
		jmp (Input_NavigateBack)
		;RETURNS - no more inputs read this frame
	:
	
	;lda ButtonStates
	;bit #<KEY_R
	;beq :+
	;	lda ButtonPushed+1
	;	bit #>KEY_Y
	;	beq :+
	;		jmp (Input_Paste)
	;:
	lda ButtonPushed
	bit #<KEY_L
	beq :+
		lda #$ff
		sta SelectingActive
		jmp (Input_StartSelection)
		;RETURNS - no more inputs read this frame
	:
	
	lda ButtonStates
	bit #<KEY_L
	bne :+
		lda SelectingActive
		bpl @continue
			; When L isn't held and there's a selection, forget selection
			jmp EndSelection
	:
		; L key held
		lda ButtonPushed+1
		bit #>KEY_B
		beq :+
			jsr CopySelection
			jmp EndSelection
		:
		bit #>KEY_Y
		beq :+
			stz ExpectDoubleTap
			jmp (Input_Paste)
		:
		
		lda ButtonPushed
		bit #<KEY_X
		beq :+
			jsr CutSelection
			jmp EndSelection
		:

		; TOOD: While selecting with L button, only allow cursor movements or combination inputs
		; jmp HandleCursorInput	
	@continue:

	lda ButtonPushed+1
	bit #>KEY_B
	beq :+
		jmp (Input_NavigateBack)
		;RETURNS - no more inputs read this frame
	:
	lda ButtonPushed
	bit #<KEY_A
	beq :+
		jmp (Input_NavigateIn)
		;RETURNS - no more inputs read this frame
	:
	bit #<KEY_X ; Key X removes current chain
	beq :+
		jmp (Input_Erase)
		;RETURNS - no more inputs read this frame
	:

HandleCursorInput:

jmp (Input_CustomHandler)

CutSelection: jmp (Input_CutSelection)
CopySelection: jmp (Input_CopySelection)
EndSelection:
	stz SelectingActive
jmp (Input_EndSelection)

; TODO: Dedicated GUI handler?

LoadGuiMap:
	
	ldy MapDataAddresses, X
	ldx #0
	:
		lda MapData,Y
		sta f:GuiTilemapBuffer,X
		lda #2<<2
		sta f:GuiTilemapBuffer+1,X
		inx
		inx
		iny
		cpx #32*28*2
	bne :-
	lda #1
	sta UpdateGui
rtl

.export CopyGuiTilemap
CopyGuiTilemap:
	stz UpdateGui
	LoadBlockToVRAM GuiTilemapBuffer, Bg1TileMapBase, 32*28*2
rtl


LoadGuiGraphics:
	LoadBlockToVRAM GuiChr, Bg1ChrBase, (GuiChrEnd - GuiChr)
rtl

MapDataAddresses:
.addr SongGuiMap, ChainGuiMap, InstrumentGuiMap
MapData:
.incbin "gfx/gui.inc"
SongGuiMap = 4
ChainGuiMap = 4 + 32*30
InstrumentGuiMap = 4 + 32*30*2

LoadLineSprites:
	phy
	lda #0
	xba
	lda CurrentScreen
	asl
	tax
	cpx #4
	bcc :+
		ldx #4
	:
	ldy LineNumberSprites, X
	ldx #LineNumberSpriteStartIndex*4
	:
		lda LineNumbers,Y
		sta OamBuffer,X
		inx
		iny
		cpx #(LineNumberSpriteStartIndex*4) + (16*4)
	bne :-
	ply
rtl

LineNumberSprites:
.addr SongLineNumbers-LineNumbers, ChainAndPhraseLineNumbers-LineNumbers, NoLineNumbers-LineNumbers
LineNumbers:
; TODO: This seems like a massive waste of space, considering all the repetition
ChainAndPhraseLineNumbers:
.byte 12,53,$20,%00110000|2
.byte 17,53,$20,%00110000|2
.byte 12,85,$20,%00110000|2
.byte 17,85,$24,%00110000|2
.byte 12,117,$20,%00110000|2
.byte 17,117,$28,%00110000|2
.byte 12,149,$20,%00110000|2
.byte 17,149,$2C,%00110000|2

.byte 124,53,$20,%00110000|2
.byte 129,53,$20,%00110000|2
.byte 124,85,$20,%00110000|2
.byte 129,85,$24,%00110000|2
.byte 124,117,$20,%00110000|2
.byte 129,117,$28,%00110000|2
.byte 124,149,$20,%00110000|2
.byte 129,149,$2C,%00110000|2

SongLineNumbers:
.byte 12,53,$20,%00110000|2
.byte 17,53,$20,%00110000|2
.byte 12,85,$20,%00110000|2
.byte 17,85,$24,%00110000|2
.byte 12,117,$20,%00110000|2
.byte 17,117,$28,%00110000|2
.byte 12,149,$20,%00110000|2
.byte 17,149,$2C,%00110000|2
.byte 12,181,$21,%00110000|2
.byte 17,181,$20,%00110000|2

NoLineNumbers:
.repeat 16
.byte 224,224,0,224
.endrepeat

.segment "RODATA"
GuiChr:
.incbin "gfx/gui.chr"
GuiChrEnd:

.segment "BSS"
.export UpdateGui
UpdateGui: .res 1