.p816
.include "src/snes.inc"
.import nmi_handler, main:far

MAPPER_LOROM = $20    ; a15 skipped
MAPPER_HIROM = $21    ; C00000-FFFFFF skipping a22, a23
MAPPER_EXHIROM = $25  ; skipping a22, inverting a23
ROMSPEED_200NS = $00
ROMSPEED_120NS = $10
MEMSIZE_NONE  = $00
MEMSIZE_2KB   = $01
MEMSIZE_4KB   = $02
MEMSIZE_8KB   = $03
MEMSIZE_16KB  = $04
MEMSIZE_32KB  = $05
MEMSIZE_64KB  = $06
MEMSIZE_128KB = $07  ; values from here up are for SRAM
MEMSIZE_256KB = $08  ; values from here down are for ROM
MEMSIZE_512KB = $09  ; Super Mario World
MEMSIZE_1MB   = $0A  ; Mario Paint
MEMSIZE_2MB   = $0B  ; Mega Man X, Super Mario All-Stars, original SF2
MEMSIZE_4MB   = $0C  ; Turbo/Super SF2, all Donkey Kong Country
MEMSIZE_8MB   = $0D  ; ExHiROM only: Tales of Phantasia, SF Alpha 2

.segment "REGDATA"

.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

.segment "SNESHEADER"

map_mode:
;.byte MAPPER_LOROM|ROMSPEED_200NS
.byte MAPPER_LOROM|ROMSPEED_120NS
.byte $02   ; 00: no extra RAM; 02: RAM with battery
;.byte MEMSIZE_256KB  ; ROM size (08-0C typical)
.byte MEMSIZE_1MB  ; ROM size (08-0C typical)
.byte MEMSIZE_16KB                  ; SRAM size
.byte $01                   ; $01 = U.S.  $00 = Japan, that's all I know
.byte $33              ; Just use $00 ; Publisher ID
.byte $00                   ; $00 = 1.00, $01 = 1.01, etc.

  .word $0000 ; sum of all bytes will be poked here after linking
  .word $0000 ; $FFFF minus above sum will also be poked here
  .res 4  ; unused vectors


  ; clcxce mode vectors
  ; reset unused because reset switches to 6502 mode
  .addr cop_handler, brk_handler, abort_handler
  .addr nmistub, $FFFF, irqstub
  .res 4  ; more unused vectors
  ; 6502 mode vectors
  ; brk unused because 6502 mode uses irq handler and pushes the
  ; X flag clear for /IRQ or set for BRK
  .addr ecop_handler, $FFFF, eabort_handler
  .addr enmi_handler, resetstub, eirq_handler


.segment "CODE"

nmistub:
	jml nmi_handler
irqstub:
	jml irq_handler
cop_handler:
brk_handler:
abort_handler:
rti
irq_handler:
rti

ecop_handler:
eabort_handler:
enmi_handler:
eirq_handler:
rti

.import __ZEROPAGE_RUN__
.smart
; Mask off low byte to allow use of $000000-$00000F as local variables
ZEROPAGE_BASE   = __ZEROPAGE_RUN__ & $FF00

; Make sure these conform to the linker script (e.g. lorom256.cfg).
STACK_BASE      = $0100
STACK_SIZE      = $0100
LAST_STACK_ADDR = STACK_BASE + STACK_SIZE - 1

PPU_BASE        = $2100
CPUIO_BASE      = $4200

; MMIO is mirrored into $21xx, $42xx, and $43xx of all banks $00-$3F
; and $80-$BF.  To make it work no matter the current data bank, we
; can use a long address in a nonzero bank.
; Bit 0 of MEMSEL enables fast ROM access above $808000.
MEMSEL2          = $80420D

; A tiny stub in bank $00 needs to set interrupt priority to 1,
; leave 6502 emulation mode, and long jump to the rest of init code
; in another bank. This should set 16-bit mode, turn off decimal
; mode, set the stack pointer, load a predictable state into writable
; MMIO ports of the S-PPU and S-CPU, and set the direct page base.
; For explanation of the values that this writes, see docs/init.txt
;
; For advanced users: Long stretches of STZ are a useful place to
; shuffle code when watermarking your binary.

.segment "CODE"
.proc resetstub
  sei                ; turn off IRQs
  clc
  xce                ; turn off 6502 emulation mode
  cld                ; turn off decimal ADC/SBC
  jml reset_fastrom  ; Bank $00 is not fast, but its mirror $80 is
.endproc

.segment "CODE7"
.proc reset_fastrom
  rep #$30         ; 16-bit AXY
  ldx #LAST_STACK_ADDR
  txs              ; set the stack pointer

  ; Initialize the CPU I/O registers to predictable values
  lda #CPUIO_BASE
  tad              ; temporarily move direct page to S-CPU I/O area
  lda #$FF00
  sta $00
  stz $02
  stz $04
  stz $06
  stz $08
  stz $0A
  stz $0C

  ; Initialize the PPU registers to predictable values
  lda #PPU_BASE
  tad              ; temporarily move direct page to PPU I/O area

  ; first clear the regs that take a 16-bit write
  lda #$0080
  sta $00     ; Enable forced blank
  stz $02
  stz $05
  stz $07
  stz $09
  stz $0B
  stz $16
  stz $24
  stz $26
  stz $28
  stz $2A
  stz $2C
  stz $2E
  ldx #$0030
  stx $30     ; Disable color math
  ldy #$00E0
  sty $32     ; Clear red, green, and blue components of COLDATA

  ; now clear the regs that need 8-bit writes
  sep #$20
  sta $15     ; still $80: Inc VRAM pointer after high byte write
  stz $1A
  stz $21
  stz $23

  ; The scroll registers $210D-$2114 need double 8-bit writes
  .repeat 8, I
    stz $0D+I
    stz $0D+I
  .endrepeat

  ; As do the mode 7 registers, which we set to the identity matrix
  ; [ $0100  $0000 ]
  ; [ $0000  $0100 ]
  lda #$01
  stz $1B
  sta $1B
  stz $1C
  stz $1C
  stz $1D
  stz $1D
  stz $1E
  sta $1E
  stz $1F
  stz $1F
  stz $20
  stz $20

  ; Set fast ROM if the internal header so requests
  lda map_mode
  and #$10
  beq not_fastrom
  lda #$01
  sta MEMSEL2
not_fastrom:

  rep #$20
  lda #ZEROPAGE_BASE
  tad                 ; return direct page to real zero page

  ; Unlike on the NES, we don't have to wait 2 vblanks to do
  ; any of the following remaining tasks.
  ; * Fill or clear areas of VRAM that will be used
  ; * Clear areas of WRAM that will be used
  ; * Load palette data into CGRAM
  ; * Fill shadow OAM and then copy it to OAM
  ; * Boot the S-SMP
  ; The main routine can do these in any order.

  jml main
.endproc

