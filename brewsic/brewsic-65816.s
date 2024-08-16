.a8
.i16
.smart

.export BrewsicInit = Init, BrewsicPlayTrack = PlayTrack, BrewsicStopTrack = StopTrack, BrewsicPlaySound = PlaySound, BrewsicTransfer = TransferBlock
.export BrewsicInitStream = InitStream, BrewsicTickStream = TickStream
.exportzp BrewsicTransferDestination = TransferDestination

.import SpcImage:far, SpcImageEnd:far
SpcImageSize = SpcImageEnd-SpcImage

.export SampleDirectoryAddress
SampleDirectoryAddress = $1100

SpcEntryPoint = $0200

SfxOffset = $8000

PORT0 = $2140
PORT1 = $2141
PORT2 = $2142
PORT3 = $2143

Command_Transfer = 1
Command_PlayTrack = 2
Command_StopTrack = 3
Command_SoundEffect = 4
Command_SoundEffectH = 5
Command_SoundEffectQ = 6
Command_TransferEnd = $ff

.zeropage
Temp: .res 6
.bss
TransferState: .res 1
LastCommByte: .res 1
StreamPosition: .res 3
StreamSize: .res 3
StreamCounter: .res 1
StreamChunk: .res 2
Test2: .res 1


.segment "CODE7"

.define seta8 sep #$20
.define seta16 rep #$20

Fake:
rts

Init:
	ldx #0
	stx StreamSize
	jsr InitiateIPLTransfer
	
	ldx #.loword(SpcImage)
	lda #^SpcImage
	ldy #.loword(SpcImageSize)
	jsr IPLTransferBlock

	jsr RunSpcCode

	ldx #(SampleDirectoryAddress-1)
	stx z:TransferDestination
	lda #^Image1
	ldx #.loword(Image1)
	ldy #.loword(Image1End-Image1) ; Size
	jsr TransferBlock

rts


InitStream:
rts
	ldx #300 ; NTSC
	lda $213F
	and #%00010000 ; PAL/NTSC bit
	beq :+
		ldx #360 ; PAL
	:
	stx StreamChunk


	ldx #SfxOffset
	stx z:TransferDestination
	
	lda #^StreamSrc
	sta StreamPosition+2
	ldx #.loword(StreamSrc)
	stx StreamPosition
	ldy StreamChunk
	stz StreamCounter
	
	phb
	pha
	plb
	
	jsr TransferBlock ; TODO: Fix new 3-byte transfer source logi
	plb
	ldy #SfxOffset
	jsr PlaySound

	seta16
	lda #$8000
	sec
	sbc StreamChunk
	sta StreamSize
	seta8
rts

TickStream:
	lda Test2
	and #1
	eor #1
	sta Test2
	beq :+
		;rts
	:

	seta16
	lda StreamSize
	bne :+
		seta8
		rts
		.a16
	:
	sec
	sbc StreamChunk
	sta StreamSize
	bcs :+
		;stz StreamSize
		; TODO: 3 byte stream size, also prevent loop bit when on the last block
	:

	
	lda StreamPosition
	clc
	adc StreamChunk
	cmp #($7E90+$8000)
	bne :+
		inc StreamPosition+2
		lda #$8000
	:
	sta StreamPosition
	; TODO: handle carry and increase bank - but how to handle the seam between two banks?
	seta8
	
	lda StreamPosition+2
	
	phb
	pha
	plb
	
	ldx #SfxOffset
	lda $213F
	and #%00010000
	bne @pal ; PAL timing skips the three-chunk 300b buffer entirely, and plays using a single 360b one
	
		inc StreamCounter
		lda StreamCounter	
		cmp #1 ; Every third tick, force a loop bit
		bne :+
			ldx #(SfxOffset+300)
			lda #0
		:
		cmp #2
		bne :+
			ldx #(SfxOffset+600)
			lda #$ff
			sta StreamCounter
		:
	@pal:
	stx z:TransferDestination
	ldx StreamPosition
	ldy StreamChunk
	
	jsr transfer

	plb
rts




TransferDestination = 0
TransferBlock:
@sourceAddress = Temp
	sta @sourceAddress+2
	lda #0

transfer:
@sourceAddress = Temp
@sourceSize = Temp+3
@customLoop = Temp+5
	
.ifdef FASTTRANSFER
; Fast transfer relies on the CPU code being faster than SPC.
; This should be no issue even on slowrom (needs confirmation),
; but it's important to make sure the transfer isn't interrupted by IRQs!!

	stx @sourceAddress

	ldx #1 ; Y will never match 1, so loop bit will never be set
	cmp #0
	beq :+
		ldx #290 ; NTSC
		lda $213F
		and #%00010000 ; PAL/NTSC bit
		beq :+
			ldx #350 ; PAL
	:
	stx @customLoop

	:	ldx PORT0
		cpx #$ABCD ; Wait for SPC code to be ready for commands
	bne :-
	
	ldx TransferDestination
	stx PORT2

	;ldx #$0101 ; Begin transfer, start counting from 1, as it is the same as the command

	: inc LastCommByte
	lda LastCommByte
	cmp #$AB ; Since the ready code is ABCD, this is the one value we don't allow on port1 when beginning transfer
	beq :-
	cmp #$AA ; TODO: Or maybe this since it's being increased? Uh. Or maybe just start from a static value
	beq :-

xba
lda #Command_Transfer ; Transfer command
tax ; X now contains both transfer command, *and* the initial PORT1 check value (SPC code reads this, increments and sends back to tell when it's ready for the next block of 4 bytes)

	stx PORT0
	txa
xba
tax
	inx
	:	cmp PORT1 ; wait for SPC to count 01
	bne :-

	iny
	iny
	iny
	sty @sourceSize
	sty PORT2
	inc a
	sta PORT1
	ldy #0
	seta16
	@transferLoop:
		lda [@sourceAddress],Y
		;cpy @customLoop
		;bne :+
		;	ora #$0300 ; Set this header byte to end+loop
		;:
		pha
		iny
		iny
		lda [@sourceAddress],Y
		cpy @customLoop
		bne :+
			ora #$0300 ; Set this header byte to end+loop
		:
		pha
		iny
		iny
		
		seta8
		txa
		inx
		:	cmp PORT1 ; wait for SPC to count 02,03,04,etc. to confirm both bytes have been read
		bne :-
		seta16
		pla
		sta PORT2
		pla
		sta PORT0

		cpy @sourceSize
	bcc @transferLoop
	@endTransfer:
	seta8
	inc a
	xba
	lda #Command_TransferEnd
	seta16
	sta PORT0
	seta8
	xba
	sta LastCommByte

.else

	stx @sourceAddress
	sty @sourceSize
	ldy #0
	
	:	ldx PORT0
		cpx #$ABCD ; Wait for SPC code to be ready for commands
	bne :-
	
	ldx #(SampleDirectoryAddress-1)
	stx PORT2
	ldx #$0101 ; Begin transfer, start counting from 1, as it is the same as the command
	stx PORT0
	txa
	
	@transferLoop:
		inx
		:	cmp PORT1
		bne :-
		cpy @sourceSize
		bcs @endTransfer
		seta16
		lda (@sourceAddress),Y
		sta PORT2
		seta8
		txa
		sta PORT1
		iny
		iny
	bra @transferLoop
	@endTransfer:
	inc a
	xba
	lda #Command_TransferEnd
	seta16
	sta PORT0
	seta8
	xba
	sta LastCommByte

.endif
rts



PlaySound:

	:	ldx PORT0
		cpx #$ABCD ; Wait for SPC code to be ready for commands
	bne :-
	sty PORT2
	inc LastCommByte
	lda LastCommByte
	xba
	lda #Command_SoundEffect ; Play track command
	tax
	stx PORT0

rts

StopTrack:
	:	ldx PORT0
		cpx #$ABCD ; Wait for SPC code to be ready for commands
	bne :-
	inc LastCommByte
	lda LastCommByte
	xba
	lda #Command_StopTrack ; Stop track command
	tax
	stx PORT0
rts

PlayTrack:

	:	ldx PORT0
		cpx #$ABCD ; Wait for SPC code to be ready for commands
	bne :-
	sta PORT2
	inc LastCommByte
	lda LastCommByte
	xba
	lda #Command_PlayTrack ; Play track command
	tax
	stx PORT0
rts

InitiateIPLTransfer:

:	ldx	PORT0	; wait for 'ready signal from SPC
	cpx	#$bbaa
	bne	:-		;--------------------------------------
	stx	PORT1	; start transfer:
	ldx	#SpcEntryPoint	; port1 = !0
	stx	PORT2	; port2,3 = transfer address
	lda	#0CCh		; port0 = 0CCh
	sta	PORT0	;--------------------------------------
:	cmp	PORT0	; wait for SPC
	bne	:-		;

	stz TransferState
rts

IPLTransferBlock:
@sourceAddress = Temp
@sourceSize = Temp+2
	sty @sourceSize
	stx @sourceAddress
	pha
	plb

	ldy #0
	lda	(@sourceAddress),Y	; read first byte
	xba			;
	lda	TransferState
	ldy	#1		;
	bra	@sb_start	;
;----------------------------------------------------------------------
; transfer data
;----------------------------------------------------------------------
@sb_send:
;----------------------------------------------------------------------
	xba			; swap DATA into A
	lda	(@sourceAddress),Y; read next byte
	iny			; swap DATA into B
	xba			;--------------------------------------
:	cmp	PORT0	; wait for SPC
	bne	:-		;--------------------------------------
	ina			; increment counter (port0 data)
;----------------------------------------------------------------------
@sb_start:
;----------------------------------------------------------------------
	rep	#20h		; write port0+port1 data
	sta	PORT0	;
	sep	#20h		;--------------------------------------
	cpy @sourceSize
	bcc	@sb_send				;
	
:	cmp	PORT0	; wait for SPC
	bne	:-		;--------------------------------------
	ina
	sta TransferState
rts

RunSpcCode:
;----------------------------------------------------------------------
; all bytes transferred
;----------------------------------------------------------------------
	lda TransferState
	ina ; Add one more to indicate end of transfer			;--------------------------------------
				; mask data so invalid 80h message wont get sent
	stz	PORT1	; port1=0
	ldx	#SpcEntryPoint	; port2,3 = entry point
	stx	PORT2	;
	sta	PORT0	; write P0 data
				;--------------------------------------
:	cmp	PORT0	; final sync
	bne	:-		;--------------------------------------
	stz	PORT0
	
	;stz	spc_v		; reset V
	;stz	spc_q		; reset Q
	;stz	spc_fwrite	; reset command fifo
	;stz	spc_fread	;
	;stz	spc_sfx_next	;
	
	;stz	spc_pr+0
	;stz	spc_pr+1
	;stz	spc_pr+2
	;stz	spc_pr+3
;----------------------------------------------------------------------
; driver installation successful
;----------------------------------------------------------------------
	rts			; return
;----------------------------------------------------------------------








; Old (faster) code:
;----------------------------------------------------------------------
:	ldx	PORT0	; wait for 'ready signal from SPC
	cpx	#$bbaa
	bne	:-		;--------------------------------------
	stx	PORT1	; start transfer:
	ldx	#SpcEntryPoint	; port1 = !0
	stx	PORT2	; port2,3 = transfer address
	lda	#0CCh		; port0 = 0CCh
	sta	PORT0	;--------------------------------------
:	cmp	PORT0	; wait for SPC
	bne	:-		;
;----------------------------------------------------------------------
; ready to transfer
;----------------------------------------------------------------------
	lda	f:SpcImage	; read first byte
	xba			;
	lda	#0		;
	ldx	#1		;
	bra	sb_start	;
;----------------------------------------------------------------------
; transfer data
;----------------------------------------------------------------------
sb_send:
;----------------------------------------------------------------------
	xba			; swap DATA into A
	lda	f:SpcImage, x; read next byte
	inx			; swap DATA into B
	xba			;--------------------------------------
:	cmp	PORT0	; wait for SPC
	bne	:-		;--------------------------------------
	ina			; increment counter (port0 data)
;----------------------------------------------------------------------
sb_start:
;----------------------------------------------------------------------
	rep	#20h		; write port0+port1 data
	sta	PORT0	;
	sep	#20h		;--------------------------------------
	cpx #.loword(SpcImageSize)
	bcc	sb_send				;
;----------------------------------------------------------------------
; all bytes transferred
;----------------------------------------------------------------------
:	cmp	PORT0	; wait for SPC
	bne	:-		;--------------------------------------
	ina			; add 2 or so...
	ina			;--------------------------------------
				; mask data so invalid 80h message wont get sent
	stz	PORT1	; port1=0
	ldx	#SpcEntryPoint	; port2,3 = entry point
	stx	PORT2	;
	sta	PORT0	; write P0 data
				;--------------------------------------
:	cmp	PORT0	; final sync
	bne	:-		;--------------------------------------
	stz	PORT0
	
	;stz	spc_v		; reset V
	;stz	spc_q		; reset Q
	;stz	spc_fwrite	; reset command fifo
	;stz	spc_fread	;
	;stz	spc_sfx_next	;
	
	;stz	spc_pr+0
	;stz	spc_pr+1
	;stz	spc_pr+2
	;stz	spc_pr+3
;----------------------------------------------------------------------
; driver installation successful
;----------------------------------------------------------------------
	rts			; return
;----------------------------------------------------------------------


.segment "RODATA5"
Image1:
.incbin "music/lucky.bin"
Image1End:

.segment "ROM12"
StreamSrc:
;.incbin "sfx/warning.brr",$7E90*0,$7E90
.segment "ROM13"
;.incbin "sfx/warning.brr",$7E90*1,$7E90
.segment "ROM14"
;.incbin "sfx/warning.brr",$7E90*2,$7E90
.segment "ROM15"
;.incbin "sfx/warning.brr",$7E90*3