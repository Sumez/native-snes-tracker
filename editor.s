.include "global.inc"
.include "src/snes.inc"
.smart

.import Vfx_Update, Vfx_ResetOnNavigation, Vfx_Init, Playback_Update


.segment TilemapBufferSegment

TilemapBuffer:
.res 32*32*2
.res $200 ; TODO: (remove) buffer because our song view currently overflows the tilemap buffer...
UiTilemapBuffer:
.res 32*32*2

.ifdef HIROM

	.segment "SRAM1"
	.assert * = $306000, error, "Song data must start at $0000 in sram for compatibility"
	HEADER: .res $30 ; Bytes to spare
	TITLE: .res $40
	AUTHOR: .res $40
	SONG: .res $800 ; $0-FF, 8 channels per row
	.segment "SRAM2"
	CHAINS: .res $2000 ; 2 bytes (phrase ref and transpose), 16 rows, $80 different chains of $20 each
	.segment "SRAM3"
	PHRASES_1: .res $2000 ; 4 bytes (note, instr, command, cmdparam) * 16 rows. $80 different phrases of $40 each
	.segment "SRAM4"
	PHRASES_2: .res $2000 ; 4 bytes (note, instr, command, cmdparam) * 16 rows. $80 different phrases of $40 each
	.segment "SRAM5"
	INSTRUMENTS: .res $100 ; ??
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
	INSTRUMENTS: .res $100 ; ??
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
OnPlaybackStopped: .res 2

.segment "BSS"
CurrentScreen: .res 1
ResetStack: .res 2

.segment CompiledPlaybackDataSegment
CompiledPattern: .res $8000


.segment "CODE7"

InitMidScreen:
	wai
	stz PPUNMI
	lda #FORCEBLANK
	sta PPUBRIGHT

InitEditor:
.export InitEditor
.import LoadTextGraphics, LoadPalettes
.import Cursor_Init, Pattern_Init, Chain_Init, Song_Init;, Chain_Init, Pattern_Init

	jsr LoadSong ; TODO: Handle this BEFORE going into the editor
	
	; INIT RAM VALUES - Initiate each editor once. Anything that needs to be reset whenever navigating there should be in LoadView
jsr ResetSprites
jsr FinalizeSprites
LoadBlockToOAM OamBuffer, 544

	jsl Cursor_Init
	jsl Vfx_Init
	jsl Song_Init
	jsl Chain_Init
	jsl Pattern_Init
	
	; MESS WITH VRAM AFTER THIS POINT
	stz $420C ; Halt any potential HDMA, that might interfer with our VRAM access
	jsl LoadTextGraphics
	
	phk
	plb
	

	jsr LoadPalettes
	
	jsl LoadBackgroundUi
	jsl CopyTilemapToUiLayer

	
	lda #0
	sta CurrentScreen
	jsl LoadView
	jsl CopyEntireTilemap

	stz PPUBRIGHT

	lda #1 | %00001000 ; High priority background layer
	sta BGMODE       ; Set Mode to 1

	lda #Bg1TileMapBase >> 9 | 2
	sta NTADDR       ; Set BG1's Tile Map VRAM offset to $0800 (word address) and the Tile Map size to 32x64tiles
	lda #Bg2TileMapBase >> 9 | 2
	sta NTADDR+1       ; Set BG2's Tile Map VRAM offset to $f000 (word address) and the Tile Map size to 32x64tiles
	lda #Bg3TileMapBase >> 9 | 2
	sta NTADDR+2       ; Set BG3's Tile Map VRAM offset to $f000 (word address) and the Tile Map size to 32x64tiles

	lda #Bg1ChrBase >> 13 | Bg2ChrBase >> 9
	sta BGCHRADDR       ; Set BG1's Character VRAM offset to $0000 (word address)
	lda #Bg3ChrBase >> 13 | Bg4ChrBase >> 9
	sta BGCHRADDR+1       ; Set BG1's Character VRAM offset to $0000 (word address)

	; (alternate between $f000 and $f800

	lda #(SpriteChrBase >> 14) | OBSIZE_16_32
	sta OBSEL



	;lda #%00000111
	;lda #%00010100
	lda #%00010010
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

	sta CurrentScreen
	jsl Vfx_ResetOnNavigation
	jsl LoadView
	wai
	;jsl CopyEntireTilemap ; TODO: add this if the call to this every NMI gets removed
	
	ldx ResetStack
	txs
jmp MainLoop


LoadSong:
@HeaderVerificationCode = $EFCD ; Constant, indicates Initialized SRAM data
@SaveBreakingBuildVersion = 0 	; Increased every time a public build breaks older save data. If feeling nice, create code to convert from one version to another
	phb
	lda #^HEADER
	pha
	plb

	ldx HEADER
	lda HEADER+2
	cpx #@HeaderVerificationCode
	bne :+
		cmp #@SaveBreakingBuildVersion
		; TODO: Give user a nice message asking them if they want to reset the data, or give them a change to back it up first
		bne :+
			plb
			rts ; Valid song already present, use sram as-is
	:
	
	lda #$ff
	ldx #0
	:
		sta f:SONG,X
		inx
		cpx #$800
	bne :-

	lda #$ff
	ldx #0
	:
		sta f:CHAINS,X
		inx
		cpx #$2000
	bne :-

	lda #$ff
	ldx #0
	:
		sta f:PHRASES_1,X
		inx
		cpx #$2000
	bne :-

	lda #$ff
	ldx #0
	:
		sta f:PHRASES_2,X
		inx
		cpx #$2000
	bne :-

	lda #$ff
	ldx #0
	:
		sta f:PHRASES_3,X
		inx
		cpx #$2000
	bne :-

	lda #$ff
	ldx #0
	:
		sta f:PHRASES_4,X
		inx
		cpx #$2000
	bne :-
	
	lda #$ff
	ldx #0
	:
		sta f:INSTRUMENTS,X
		inx
		cpx #$100
	bne :-
	
	lda #$00
	ldx #0
	:
		sta f:META,X
		inx
		cpx #$800
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

.export CopyEntireTilemap, CopyTilemapToUiLayer
CopyEntireTilemap:
	LoadBlockToVRAM TilemapBuffer, Bg2TileMapBase, 32*32*2
rtl
CopyTilemapToUiLayer:
	LoadBlockToVRAM UiTilemapBuffer, Bg3TileMapBase, 32*32*2
rtl

.segment "CODE6"

LoadBackgroundUi:
	jsr ClearUiTilemap
	seta16
	lda #$08df
	ldx #$106
	:
		sta f:UiTilemapBuffer,x
		inx
		inx
		cpx #$11e
	bne :-
	ldx #$206
	:
		sta f:UiTilemapBuffer,x
		inx
		inx
		cpx #$21e
	bne :-
	ldx #$306
	:
		sta f:UiTilemapBuffer,x
		inx
		inx
		cpx #$31e
	bne :-
	ldx #$406
	:
		sta f:UiTilemapBuffer,x
		inx
		inx
		cpx #$41e
	bne :-
	seta8
rtl
ClearUiTilemap:
	seta16
	lda #($3f*2) ; Blank space - * 2 because it's 2bpp
	ldx #((32*32*2)-2)
	:
		sta f:UiTilemapBuffer,x
		dex
		dex
	bpl :-
	seta8
rts

ClearTilemap:
	seta16
	lda #$3f ; Blank space
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
	ldx #$88
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
	jumpTable ViewLoaders
rtl
ViewLoaders: .import Song_LoadView, Chain_LoadView, Pattern_LoadView
.addr Song_LoadView, Chain_LoadView, Pattern_LoadView

.segment "CODE7"
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
			stz IsPlaying
			.import BrewsicStopTrack
			jsr BrewsicStopTrack
			jmp (OnPlaybackStopped)
			;RETURNS - no more inputs read this frame
	
	@didNotPushStart:

	lda ButtonStates+1
	and #>KEY_SELECT
	ora ButtonPushed+1
	and #>KEY_SELECT|>KEY_RIGHT
	cmp #>KEY_SELECT|>KEY_RIGHT
	bne :+
		jmp (Input_NavigateIn)
		;RETURNS - no more inputs read this frame
	:

	lda ButtonPushed
	bit #<KEY_A
	beq :+
		jmp (Input_NavigateIn)
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
	
	lda ButtonPushed+1
	bit #>KEY_B
	beq :+
		jmp (Input_NavigateBack)
		;RETURNS - no more inputs read this frame
	:

jmp (Input_CustomHandler)
