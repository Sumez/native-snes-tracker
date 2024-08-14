.include "global.inc"
.include "src/snes.inc"
.smart

.segment TextBank
TextAddresses:
DialogChr:
.incbin "gfx/font.chr"
DialogChrEnd:
CursorSpriteChr:
.incbin "gfx/cursor.chr"
CursorSpriteChrEnd:


.segment "CODE7"
.export LoadTextGraphics = LoadChr

LoadChr:
	LoadBlockToVRAM DialogChr, Bg2ChrBase, (DialogChrEnd - DialogChr)
	LoadBlockToVRAM CursorSpriteChr, (SpriteChrBase), (CursorSpriteChrEnd - CursorSpriteChr)
rtl
