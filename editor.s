.include "global.inc"
.include "src/snes.inc"
.smart

.import Vfx_Update, Vfx_Init


.segment TilemapBufferSegment

TilemapBuffer:
.res 32*32*2
.res $200 ; TODO: (remove) buffer because our song view currently overflows the tilemap buffer...
UiTilemapBuffer:
.res 32*32*2

.segment SongDataSegment
.assert * = $700000, error, "Song data must start at $0000 in sram for compatibility"
;.segment "SRAM1"
HEADER: .res $30 ; Bytes to spare
TITLE: .res $40
AUTHOR: .res $40
SONG: .res $800 ; $0-FF, 8 channels per row
;.segment "SRAM2"
CHAINS: .res $2000 ; 2 bytes (phrase ref and transpose), 16 rows, $100 different chains of $20 each
;.segment "SRAM3"
PHRASES: .res $4000 ; 4 bytes (note, instr, command, cmdparam) * 16 rows. $100 different phrases of $40 each
;PHRASES: .res $2000 ; 4 bytes (note, instr, command, cmdparam) * 16 rows. $100 different phrases of $40 each
;.segment "SRAM4"
;PHRASESp2: .res $2000
;.segment "SRAM5"
INSTRUMENTS: .res $100 ; ??
META: .res $800 ; Future feature: custom meta data for song - colors to help with navigation etc.

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
	
jsr ResetSprites
jsr FinalizeSprites
LoadBlockToOAM OamBuffer, 544

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

	jsr UpdateInputStates
	jsr Vfx_Update
	jsr HandleInput

jmp MainLoop

NavigateToScreen:

	sta CurrentScreen
	jsl LoadView
	wai
	;jsl CopyEntireTilemap ; TODO: add this if the call to this every NMI gets removed
	
	ldx ResetStack
	txs
jmp MainLoop


LoadSong:

	phb
	lda #^HEADER
	pha
	plb

	ldx HEADER
	cpx #$ABCD
	bne :+
		plb
		rts ; Valid song already present, use sram as-is
	:
	
	lda #$ff
	ldx #0
	:
		sta SONG,X
		inx
		cpx #$800
	bne :-

	lda #$ff
	ldx #0
	:
		sta CHAINS,X
		inx
		cpx #$2000
	bne :-

	lda #$ff
	ldx #0
	:
		sta PHRASES,X
		inx
		cpx #$4000
	bne :-
	
	lda #$ff
	ldx #0
	:
		sta INSTRUMENTS,X
		inx
		cpx #$100
	bne :-
	
	lda #$00
	ldx #0
	:
		sta META,X
		inx
		cpx #$800
	bne :-
	
	ldx #$ABCD
	stx HEADER

	plb
rts
	; Doesn't work in SRAM
	ldx #((WMDATA << 8 & $ff00)|DMA_CONST)
	stx DMAMODE
	ldx #.loword(DefaultChain)
	stx DMAADDR
	lda #^DefaultChain
	sta DMAADDRBANK
	ldx #$800
	stx DMALEN

	ldx #.loword(SONG)
	stx WMADDL
	lda #^SONG
	sta WMADDH
	
	lda #%00000001
	sta COPYSTART
rts
DefaultChain: .byte $FF

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
			lda #1
			sta IsPlaying
			lda ButtonStates+1
			bit #>KEY_SELECT
			beq :+
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
