.global PatternNotes,PatternInstruments
.define GAME_TITLE "SuperTrax"

.define TilemapBufferSegment "BSS7F"
.define CompiledPlaybackDataSegment "BSS7E"
.define SongDataSegment "SRAM"

; Memory addresses
.global SONG, CHAINS, PHRASES_1, PHRASES_2, INSTRUMENTS
.globalzp Input_CustomHandler, Input_NavigateIn, Input_NavigateBack, Input_MoveLeft, Input_MoveUp, Input_MoveRight, Input_MoveDown
.globalzp Input_StartPlayback, OnPlaybackStopped
.global CurrentScreen, IsPlaying, CompiledPattern, TilemapBuffer, UiTilemapBuffer, EditMode
.global Pattern01Offset ; Unsure about these

; Global routines
.global NavigateToScreen, WriteTilemapHeader, WriteTilemapHeaderId

; Playback stuff
.global PlayFullSong
.global Playback_CurrentBeatRow, Playback_CurrentPhraseOfChannel
.globalzp Playback_CurrentChainOffsetOfChannel, Playback_CurrentSongRowOfChannel

; Cursor stuff
.global CursorX, CursorY, CursorSize, UpdateCursorSpriteAndHighlight

; Fx stuff
.global HighlightRow, HighlightChainRow, HighlightChannel
.global PlayMosaic, Bop


.macro Bind pointer, routine
	ldx #.loword(routine)
	stx z:pointer
.endmacro


.global RNG
.global InitStage, InitStageMidScreen, UpdateInputStates, InitMode7

.global InitBulletLayer, InitPlayerSprites, InitOam, PrepareLightGradients, InitializeWavyHdma, PrepareWavyHdma, LoadPalettes, UpdateHdmaTables, InitBgLayer, PushBuffer, PrepareBuffer, UpdateBullets, LoadOAM, OamBuffer, LoadBigSprite

.global PlayerJumpGravityCounter, playerJumpAdjustmentCounter, playerJumpDirection
.global objectState, objectX, objectY, objectScreenX, objectScreenY, objectDirection, objectAnimation, objectAnimationFrame, objectAnimationTimer, objectTimer, objectVelocityX, objectVelocityY, objectOffsetY
.global UpdateObjects, SpawnObject, ApplyVelocityX, AdvanceAnimation

.globalzp Object, ObjectStatusPointer

.global AddBigSprite, AddWideSprite, AddSmallSprite, ResetSprites, FinalizeSprites, CleanOamBuffer
.globalzp SpritePalette, SpriteSize

.global HdmaRoutine, ButtonStates, ButtonPushed, ColorBlend
.global LoadVRAM, LoadMode7TileMap, ClearRange, ResetRange

; Stage data
.global ScrollY, LoadStageData, LoadLightMap, LoadRowUp, LoadRowDown, LoadRow, LoadObjects
.global LoadMapToMode7
.globalzp CurrentStageData, CurrentStageCollisionData, StageHeight

.global InitPlayerData, UpdatePlayer, UpdatePlayerSprite, ClearObjectData, DamagePlayer

.global VoidHeightOnScreen

; HDMA tables
.global Hdma1Ref, Hdma2Ref, Hdma3Ref, Hdma4Ref, HdmaBuffers
.global Stage1Hdma

; Collisions:
.globalzp velocity, coordinate, baseX, baseY
.global CalcCollisionDown, CalcCollisionRight, GetTilesBelow, GetTileInFront, InitializeBitMask


.define rgb(red,green, blue) (red / 8) | ((green/8) << 5 )| ((blue/8) << 10)

State_Falling = %00000001
State_Jumping = %00000010
State_Damaged = %00000100
State_Attacking = %00001000
State_Running = %00010000
State_Landing = %00100000
State_Dead = %11111111

SpriteChrBase = $4000
Bg1TileMapBase = $1000 ; -$2000 ; Should be an "even" number, so we cna easily "and" the address pointer
Bg2TileMapBase = $8000 ; -$6800
Bg3TileMapBase = $f000 ; -$6800
Bg1ChrBase = $2000
Bg2ChrBase = $a000
Bg3ChrBase = Bg2ChrBase
;Bg3ChrBase = $0000
Bg4ChrBase = $0000


.i16
.a8

.macro CleanData StartLabel, EndLabel
	ldx #0
	lda #0
	:
		sta f:StartLabel, X
		inx
		cpx #(EndLabel - StartLabel)
	bne :-
.endmacro

; Faster version:
.macro ClearBlock SourceAddress, TargetAddress, TargetLength
	ldx #.loword(TargetAddress)
	lda #^TargetAddress
	stx WMADDL
	sta WMADDH
	ldx #.loword(SourceAddress)
	lda #^SourceAddress
	ldy #TargetLength
	jsr ClearRange
.endmacro

; Faster version:
.macro ResetBlock SourceAddress, TargetAddress, TargetLength
	ldx #.loword(TargetAddress)
	lda #^TargetAddress
	stx WMADDL
	sta WMADDH
	ldx #.loword(SourceAddress)
	lda #^SourceAddress
	ldy #TargetLength
	jsr ResetRange
.endmacro


;============================================================================
; LoadBlockToVRAM -- Macro that simplifies calling LoadVRAM to copy data to VRAM
;----------------------------------------------------------------------------
; In: SRC_ADDR -- 24 bit address of source data
;     DEST -- VRAM address to write to (WORD address!!)
;     SIZE -- number of BYTEs to copy
;----------------------------------------------------------------------------

; Out: None
;----------------------------------------------------------------------------
; Modifies: A, X, Y
;----------------------------------------------------------------------------

;LoadBlockToVRAM SRC_ADDRESS, DEST, SIZE
;   requires:  mem/A = 8 bit, X/Y = 16 bit
.macro LoadBlockToVRAM srcAddr, destination, size
    lda #INC_DATAHI
    sta PPUCTRL       ; Set VRAM transfer mode to word-access, increment by 1
    ldx #(destination >> 1)         ; DEST
    stx PPUADDR       ; $2116: Word address for accessing VRAM.
    lda #^srcAddr        ; SRCBANK
    ldx #(srcAddr & $ffff)         ; SRCOFFSET
    ldy #size         ; SIZE
    jsr LoadVRAM
.endmacro
.macro LoadBlockToVRAMDown srcAddr, destination, size
    lda #INC_DATAHI|VRAM_DOWN
    sta PPUCTRL       ; Set VRAM transfer mode to word-access, increment by 1
    ldx #(destination >> 1)         ; DEST
    stx PPUADDR       ; $2116: Word address for accessing VRAM.
    lda #^srcAddr        ; SRCBANK
    ldx #(srcAddr & $ffff)         ; SRCOFFSET
    ldy #size         ; SIZE
    jsr LoadVRAM
.endmacro

.macro LoadBlockToOffsetVRAM srcAddr, destinationPointer, size
    lda #$80
    sta PPUCTRL       ; Set VRAM transfer mode to word-access, increment by 1
    ldx destinationPointer         ; DEST
    stx PPUADDR       ; $2116: Word address for accessing VRAM.
    lda #^srcAddr        ; SRCBANK
    ldx #(srcAddr & $ffff)         ; SRCOFFSET
    ldy #size         ; SIZE
    jsr LoadVRAM
.endmacro

.macro LoadBlockToOAM srcAddr, size
    lda #^srcAddr        ; SRCBANK
    ldx #(srcAddr & $ffff)         ; SRCOFFSET
    ldy #size         ; SIZE
    jsl LoadOAM
.endmacro

.macro LoadBlockToMode7TileMap srcAddr, size
    lda #$00
    sta PPUCTRL       ; Set VRAM transfer mode to word-access, increment by 1
    ldx #$0000
    stx PPUADDR       ; $2116: Word address for accessing VRAM.
    lda #^srcAddr        ; SRCBANK
    ldx #(srcAddr & $ffff)         ; SRCOFFSET
    ldy #size         ; SIZE
    jsr LoadMode7TileMap
.endmacro



.charmap 'C',$02
.charmap 'P',$0f
.charmap 'S',$12
.charmap 'a',$20
.charmap 'e',$24
.charmap 'g',$26
.charmap 'h',$27
.charmap 'i',$28
.charmap 'n',$2d
.charmap 'o',$2e
.charmap 'r',$31
.charmap 's',$32
.charmap '-',$3b
.charmap '_',$3f

.smart

