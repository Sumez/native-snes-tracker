.include "src/spc-ca65.inc"

Channels = 8
DirAddress = $1000

.segment "SPCZEROPAGE":zeropage
Temp: .res $10
TrackDirectoryPointer: .res 2
CurrentTrackPointer: .res 2
CurrentPatternPointer: .res 2
ChClearCounter: .res Channels
ChRowCounter: .res Channels
CurrentChannelRegister: .res 1
PatternLengthTable: .res 2
MacroIndex: .res 2
InstrumentTable: .res 2
PatternRowCounter: .res 1
CatchUp: .res 1
TickCountDown: .res 1
TrackSpeed: .res 1
ChEffect: .res Channels
ChEffectParam: .res Channels
CurrentChannelVolumeAdjust: .res 1
TrackTempo: .res 1
TickCountUp: .res 1
EffectCounter: .res 1
ChPatternPositionMemory: .res Channels
ChEnvelopePointer: .res Channels ; Keep in ZP to load into Y
ChVolumeEnvelopeH: .res Channels ; Keep in ZP for quicker CMP
ChVolumeEnvelopeCounter: .res Channels

;Channel buffer
ChVolume: .res Channels
ChPan: .res Channels
ChPitchL: .res Channels
ChPitchH: .res Channels
ChSample: .res Channels
ChFadeOutL: .res Channels
ChFadeOutH: .res Channels

ChBaseVol: .res Channels
ChBasePan: .res Channels

Com_LastReceived: .res 1
Com_TransferAddress: .res 2
OneTwo: .res 1


.segment "SPCBSS":absolute
ChPatternPosition: .res Channels
CurrentOrderIndex: .res 1
ChPatternPointers: .res Channels * 2
NoteStates: .res 1
ChLastPitchAdjust: .res Channels ; TODO: Reset these?
ChLastVolumeAdjust: .res Channels
ChLastNote: .res Channels
ChLastInstrument: .res Channels
ChInstrumentFadeAmount: .res Channels
ChCurrentFadeAmount: .res Channels
ChCurrentVEnvelopeStepL: .res Channels
ChCurrentVEnvelopeStepH: .res Channels
ChInMacro: .res Channels
ChMacroAddressL: .res Channels
ChMacroAddressH: .res Channels
ChLastArpeggio: .res Channels
ChOrigPitchL: .res Channels
ChOrigPitchH: .res Channels
ChCurrentInstrumentVolume: .res Channels
ChVolumeEnvelopeL: .res Channels
NoteOff: .res 1

.segment "RODATA4"

;*****************************************************************************************
; registers
;*****************************************************************************************
SPC_TEST	=0F0h ; undocumented
SPC_CONTROL	=0F1h ; control register
SPC_DSP		=0F2h
SPC_DSPA	=0F2h
SPC_DSPD	=0F3h
SPC_PORT0	=0F4h ; i/o port0
SPC_PORT1	=0F5h ; i/o port1
SPC_PORT2	=0F6h ; i/o port2
SPC_PORT3	=0F7h ; i/o port3
SPC_FLAGS	=0F8h ; custom flags
SPC_TIMER0	=0FAh ; timer0 setting
SPC_TIMER1	=0FBh ; timer1 setting
SPC_TIMER2	=0FCh ; timer2 setting
SPC_COUNTER0	=0FDh ; timer0 counter
SPC_COUNTER1	=0FEh ; timer1 counter
SPC_COUNTER2	=0FFh ; timer2 counter

DEBUG_P0 = SPC_PORT0
DEBUG_P2 = SPC_PORT2

;*****************************************************************************************
; dsp registers
;*****************************************************************************************
DSPV_VOL	=00h
DSPV_VOLR	=01h
DSPV_PL		=02h
DSPV_PH		=03h
DSPV_SRCN	=04h
DSPV_ADSR1	=05h
DSPV_ADSR2	=06h
DSPV_GAIN	=07h
DSPV_ENVX	=08h
DSPV_OUTX	=09h

DSP_MVOL	=0Ch
DSP_MVOLR	=1Ch
DSP_EVOL	=2Ch
DSP_EVOLR	=3Ch
DSP_KON		=4Ch
DSP_KOF		=5Ch
DSP_FLG		=6Ch
DSP_ENDX	=7Ch

DSP_EFB		=0Dh
DSP_PMON	=2Dh
DSP_NON		=3Dh
DSP_EON		=4Dh
DSP_DIR		=5Dh
DSP_ESA		=6Dh
DSP_EDL		=7Dh

DSP_C0		=0Fh
DSP_C1		=1Fh
DSP_C2		=2Fh
DSP_C3		=3Fh
DSP_C4		=4Fh
DSP_C5		=5Fh
DSP_C6		=6Fh
DSP_C7		=7Fh

FLG_RESET	=80h
FLG_MUTE	=40h
FLG_ECEN	=20h


.macro dsp addr, value
mov SPC_DSPA, #addr
mov SPC_DSPD, #value
.endmacro


SpcImage:
.org $0200
bra DriverEntryPoint

SpcFileEntryPoint:
	call !LoadTrackDirectory
	mov a, #0
jmp !PlayMusic

DriverEntryPoint:

dsp $10, $00
dsp $11, $00
dsp $20, $00
dsp $21, $00
dsp $30, $00
dsp $31, $00
dsp $40, $00
dsp $41, $00
dsp $50, $00
dsp $51, $00
dsp $60, $00
dsp $61, $00
dsp $70, $00
dsp $71, $00


dsp DSP_KOF, $00
dsp DSP_DIR, (DirAddress >> 8)

call !ResetDsp
dsp DSP_FLG, $20

mov OneTwo, #0
;TODO: Clear communication ram
; Initialization done, wait for commands in a tight loop
mov Com_LastReceived, #0
mov SPC_CONTROL,#%00110000 ; Reset input values
mov SPC_PORT3, #0
mov SPC_PORT2, #0
mov SPC_PORT1, #$AB ; Tell CPU we are ready for the next command
mov SPC_PORT0, #$CD
:	call !CheckCpuCommunication
bra :-

ResetDsp:
	mov Temp, #$70
	setc
	@initChannelLoop:
		mov SPC_DSPA, DSPV_VOL
		or SPC_DSPA, Temp
		mov SPC_DSPD, #$00 ; Vx VolL
		inc SPC_DSPA
		mov SPC_DSPD, #$00 ; Vx VolR
		inc SPC_DSPA
		mov SPC_DSPD, #$00; Vx PitchL
		inc SPC_DSPA
		mov SPC_DSPD, #$10; Vx PitchH
		inc SPC_DSPA
		mov SPC_DSPD, #$00; Vx Sample
		inc SPC_DSPA
		mov SPC_DSPD, #$00; Vx ADSR1
		inc SPC_DSPA
		inc SPC_DSPA
		mov SPC_DSPD, #$1F; Vx Gain
		;mov SPC_DSPD, #$dc; Vx Gain
		sbc Temp, #$10
	bpl @initChannelLoop
	
	dsp DSP_MVOL, $60;60
	dsp DSP_MVOLR, $60;60
	dsp DSP_EVOL, $00
	dsp DSP_EVOLR, $00
	dsp DSP_EFB, $00
	dsp DSP_PMON, $00
	dsp DSP_NON, $00
	dsp DSP_EON, $00
	dsp DSP_ESA, $00
	dsp DSP_EDL, $00
	dsp DSP_C0, $00
	dsp DSP_C1, $00
	dsp DSP_C2, $00
	dsp DSP_C3, $00
	dsp DSP_C4, $00
	dsp DSP_C5, $00
	dsp DSP_C6, $00
	dsp DSP_C7, $00
ret

PlayMusic:
	mov x, #$ED ; Hacky method of making sure we're not playing a track "within another track"
	mov sp, x
	call !LoadTrack
	
	mov SPC_TIMER0,TrackTempo
	mov SPC_CONTROL,#$01
	mov a,SPC_COUNTER0

	; First tick, engage immediately to prevent "start lag"
	mov a, #1
	mov TickCountDown,a
	call !TickModule
	
	@wait:
		call !CheckCpuCommunication
		mov a, SPC_COUNTER0
		beq @wait
		mov CatchUp, a
		@repeatTickImmediately:
			eor OneTwo, #1 ; Halves set tempo of song, allows us to support lower tempo settings
			beq :+
			call !TickModule
			:
			dec CatchUp
		bne @repeatTickImmediately

		; TODO: Check S-CPU communications
bra @wait

PlaySound: 
@sfxIndexPointer = Temp+2
@pitch = Temp+4
@channel = Temp+5

	mov @pitch, a
	mov @channel, y
	mov a, SPC_PORT2
	mov y, SPC_PORT3
; YA = Address of SFX sample

	movw Temp, ya
	movw ya, TrackDirectoryPointer
	setc
	sbc a, #4 ; Last sample index (SFX index) is located 4 bytes before the track directory
	bcs :+
		dec y
	:
	movw @sfxIndexPointer,ya
	mov y, #0
	mov a, Temp
	mov [@sfxIndexPointer]+Y, a
	inc y
inc y
mov [@sfxIndexPointer]+Y, a
dec y
	mov a, Temp+1
	mov [@sfxIndexPointer]+Y, a
inc y
inc y
mov [@sfxIndexPointer]+Y, a
	
	mov a, !(DirAddress-1)
	lsr a ; = sample count
	dec a ; sfx sample index

	mov y, @channel ; $70 or $60
	mov SPC_DSPA, y ; ChX Volume L
	mov SPC_DSPD, #$80
	
	inc SPC_DSPA ; Volume R
	mov SPC_DSPD, #$80
	
	inc SPC_DSPA ; Pitch low byte
	mov SPC_DSPD, #$00;#$52

	inc SPC_DSPA ; Pitch high byte
	;mov SPC_DSPD, #$10
	mov y, @pitch
	mov SPC_DSPD, y

	inc SPC_DSPA ; Sample ID
	mov SPC_DSPD, a ; sfx index

	inc SPC_DSPA
	inc SPC_DSPA
	inc SPC_DSPA ; Gain
	mov SPC_DSPD, #$7f

	
	mov SPC_DSPA, #DSP_KON
	
	mov a, #$80 ; channel 7
	cmp @channel, #$70
	beq :+
		lsr a ; channel 6
	:
	mov SPC_DSPD, a
ret

Command_Transfer = 1
Command_PlayTrack = 2
Command_StopTrack = 3
Command_SoundEffect = 4
Command_SoundEffectH = 5
Command_SoundEffectQ = 6
Command_TransferEnd = $ff

CheckCpuCommunication:
	cmp Com_LastReceived, SPC_PORT1 ; Port 1 indicates new message
	beq @return
	nop
	cmp SPC_PORT0, #Command_Transfer ; Port 0 indicates message type
	bne :+
		jmp !BeginTransfer
	:

	mov Com_LastReceived, SPC_PORT1 ; Even if we didn't recognize the command, we still need to look for the next
	cmp SPC_PORT0, #Command_PlayTrack
	bne :+
		mov a, SPC_PORT2
		jmp !PlayMusic
	:
	
	cmp SPC_PORT0, #Command_StopTrack
	bne :+
		jmp !StopTrack
	:
	
;	cmp SPC_PORT0, #Command_SoundEffect
;	bne :+
;		mov a, #$10
;		mov y, #$60
;		jmp !PlaySound
;	:
;	cmp SPC_PORT0, #Command_SoundEffectH
;	bne :+
;		mov a, #$08
;		mov y, #$70
;		jmp !PlaySound
;	:
;	cmp SPC_PORT0, #Command_SoundEffectQ
;	bne :+
;		mov a, #$04
;		mov y, #$60
;		jmp !PlaySound
;	:
	@return:
ret

.ifdef FASTTRANSFER

BeginTransfer:
@transferSize = Temp

	mov a, SPC_PORT2
	mov y, SPC_PORT3
	movw Com_TransferAddress, ya
	mov a, SPC_PORT1 ; Read again to ensure it's wasn't a dirty read
	mov SPC_PORT1, a ; Tell S-CPU we've received the message and are ready for the next word of data
	inc a
	mov Com_LastReceived, a ; Store n+1, because that's the next number we're expecting
	mov y, #0

	: cmp Com_LastReceived, SPC_PORT1
	bne :-
	
	mov a, SPC_PORT2
	mov @transferSize+1, SPC_PORT3
	mov SPC_PORT1, Com_LastReceived
	
	lsr @transferSize+1
	ror a
	lsr @transferSize+1
	ror a
	mov @transferSize, a

	@loop:
			inc Com_LastReceived
			mov a, SPC_PORT0
			mov [Com_TransferAddress]+y, a
			inc y
			mov a, SPC_PORT1
			mov [Com_TransferAddress]+y, a
			inc y
			mov a, SPC_PORT2
			mov [Com_TransferAddress]+y, a
			mov a, SPC_PORT3
			mov SPC_PORT1, Com_LastReceived ; Since we're done reading all ports, we might as well send our "ready" message early
			inc y
			mov [Com_TransferAddress]+y, a
			
			decw @transferSize
			beq @endTransfer
			
			inc y
	
		bne @loop
		; when y is zero, add $100 to our transfer address and continue
		inc Com_TransferAddress+1
	bra @loop

	@endTransfer:
	
.else

BeginTransfer:
	mov a, SPC_PORT2
	mov y, SPC_PORT3
	movw Com_TransferAddress, ya
	mov a, SPC_PORT1 ; Read again to ensure it's wasn't a dirty read
	mov SPC_PORT1, a ; Tell S-CPU we've received the message and are ready for the next word of data
	inc a
	mov Com_LastReceived, a ; Store n+1, because that's the next number we're expecting
	mov y, #0

	@wait:
		cmp Com_LastReceived, SPC_PORT1
		bne @wait
		cmp SPC_PORT0, #Command_TransferEnd
		beq @endTransfer
	
			mov a, SPC_PORT2
			mov [Com_TransferAddress]+y, a
			mov a, SPC_PORT3
			mov SPC_PORT1, Com_LastReceived ; Since we're done reading all ports, we might as well send our "ready" message early
			inc y
			mov [Com_TransferAddress]+y, a
			inc Com_LastReceived
			inc y

		bne @wait
		; when y is zero, add $100 to our transfer address and continue
		inc Com_TransferAddress+1
	bra @wait

	@endTransfer:

.endif

	call !LoadTrackDirectory

	mov SPC_PORT1, #$AB ; Tell CPU we are ready for the next command
	mov SPC_PORT0, #$CD
	
ret

LoadTrackDirectory:
	mov a, #DirAddress & $ff
	mov y, #DirAddress >> 8
	movw <TrackDirectoryPointer, ya
	mov a, !(DirAddress-1)
	asl a
	mov y, #0
	addw ya, TrackDirectoryPointer
	movw TrackDirectoryPointer, ya
ret

LoadTrack:
	asl a
	mov y, a
	mov a, [TrackDirectoryPointer]+y
	push a
	inc y
	mov a, [TrackDirectoryPointer]+y
	mov y, a
	pop a
	movw CurrentTrackPointer, ya
;	movw @fixOffset, ya ; TODO: Fix offsets first time a track is loaded (save flags in track header?), set @fixOffset to 0 after then
	
	; Analyze track headers to get the addresses of Pattern data, Instrument data, and Envelope data
	
	mov y, #0 ; Get header data and advance pointer to start of pattern index
	mov a, [CurrentTrackPointer]+y
	mov TrackSpeed, a
	incw CurrentTrackPointer
	mov a, [CurrentTrackPointer]+y
	mov TrackTempo, a
	incw CurrentTrackPointer
	mov x, #0
	:
		mov a, [CurrentTrackPointer]+y
		mov ChBaseVol+x, a
		incw CurrentTrackPointer
		mov a, [CurrentTrackPointer]+y
		mov ChBasePan+x, a
		incw CurrentTrackPointer
		inc x
		cmp x, #8
	bne :-
		
	movw ya, CurrentTrackPointer
	movw PatternLengthTable, ya
	mov y, #0
	:	; Find end of Pattern index
		mov a, [PatternLengthTable]+y
		incw PatternLengthTable
		and a, [PatternLengthTable]+y
		incw PatternLengthTable
		cmp a, #$ff
	bne :-
	
	movw ya, PatternLengthTable
	movw MacroIndex, ya
	mov y, #0
	:	; Find end of Pattern length table
		mov a, [MacroIndex]+y
		incw MacroIndex
		cmp a, #$00
	bne :-

	movw ya, MacroIndex
	movw InstrumentTable, ya
	mov y, #0
	:	; Find end of Macro index
		mov a, [InstrumentTable]+y
		incw InstrumentTable
		and a, [InstrumentTable]+y
		incw InstrumentTable
		cmp a, #$ff
	bne :-
	
	mov a, #0 ; Always start at the beginning
	mov !CurrentOrderIndex, a
	call !ResetPattern
ret

ResetPattern:
	mov TickCountDown, TrackSpeed

	mov y, !CurrentOrderIndex
.ifdef TESTPATTERN
	mov y, #TESTPATTERN
.endif
	mov a, [PatternLengthTable]+y
	cmp a, #$00 ; TODO: Loop value?
	bne :+
		; Reached end of patterns
		jmp !StopTrack
	:
	inc a
	mov PatternRowCounter, a
	
	mov a, #16
	mul ya
	addw ya, CurrentTrackPointer ; Current pattern index * 16 + Start of address table = Current pattern address (every entry is 16 bytes, two address bytes per 8 channels)
	movw Temp+0, ya

	mov x, #15
	mov y, #15
	@channelLoop:
		mov a, [Temp+0]+y
		mov !ChPatternPointers+x, a
		dec y
		dec x
	bpl @channelLoop
	

	mov x, #7
	@channelLoop2:
		mov a, #0
		mov !ChPatternPosition+x, a
		mov ChClearCounter+x, a
		mov ChRowCounter+x, a
		mov !ChInMacro+x, a

		mov a, ChBasePan+x ; Only reset panning on pattern start?
		mov ChPan+x, a
		dec x
	bpl @channelLoop2
ret

StopTrack:
	;call !ResetDsp
mov a, #$ff ; All keys off. No reason to reset dsp (just results in cut-off pops)
mov SPC_DSPA, #DSP_KOF
mov SPC_DSPD, a
	mov x, #$ED ; force jumping back to regular wait loop
	mov sp, x
ret



TickModule:

	mov x, #6
.ifdef TESTCHANNEL
	mov x, #TESTCHANNEL
.endif
	@updateChannelLoop:
		call !UpdateDspChannel
		dec x
.ifndef TESTCHANNEL
	bpl @updateChannelLoop
.endif

	mov a, !NoteStates
	mov SPC_DSPA, #DSP_KON
	mov SPC_DSPD, a

	; Undo key off only for notes that need to be on
	eor a, #$ff
	and a, !NoteOff
	mov !NoteOff, a
	mov SPC_DSPA, #DSP_KOF
	mov SPC_DSPD, a
	
	
	mov a, #0
	mov !NoteStates, a

	inc EffectCounter
	dec TickCountDown
	bne @applyEffects

		dec PatternRowCounter
		bne :+
			inc !CurrentOrderIndex
			call !ResetPattern
		:
		mov TickCountDown, TrackSpeed
	
		mov EffectCounter, #1
		mov x, #7; Channel #
.ifdef TESTCHANNEL
		mov x, #TESTCHANNEL
.endif
		:
			call !BufferChannelRow
			dec x
.ifndef TESTCHANNEL
		bpl :-
.endif


		
	@applyEffects:
	mov y, #7; Channel #
	@effectChannelLoop:
		mov x, ChEffectParam+y
		mov a, x
		mov x, ChEffect+y ; Effect routine offset
		cmp x, #SupportedRoutines
		bcs :+
			call !ExecuteEffect
		:
		dec y
	bpl @effectChannelLoop

	mov a, !NoteOff
	or a, !NoteStates
	mov SPC_DSPA, #DSP_KOF
	mov SPC_DSPD, a

ret

BufferChannelRow:
	
	push x
	mov a, !ChInMacro+x
	beq :+
		mov a, !ChMacroAddressH+x
		mov y, a
		mov a, !ChMacroAddressL+x
		bra @calculateTrackPointer
	:
		mov a, x
		asl a
		mov x, a
		mov a, !(ChPatternPointers+1)+x
		mov y, a
		mov a, !ChPatternPointers+x
	@calculateTrackPointer:
	addw ya, CurrentTrackPointer
	movw CurrentPatternPointer, ya
	pop x

	mov a, #0
	mov ChEffect+x, a
	mov ChEffectParam+x, a
	mov a, #$ff
	mov CurrentChannelVolumeAdjust, a ; Default to $ff (no adjustment) on new notes if no volume column was read

	mov a, ChClearCounter+x
	bne @decClearCounter

	mov a, !ChPatternPosition+x
	mov y, a
	
	mov a, ChRowCounter+X ; Counts row of current block. If 0, read next header byte
	bne @readRow
	
	mov a, [CurrentPatternPointer]+y ; Read block header byte
	bpl @notClear
		; bit 7 is set = number of clear rows
		cmp a, #$ff
		bne :++
			; TODO: Optimize this mess
			mov Temp, a
			mov a, !ChInMacro+x
			beq :+
				mov a, #0
				mov !ChInMacro+x, a
				mov a, !ChPatternPositionMemory+x
				mov !ChPatternPosition+x, a
				jmp !BufferChannelRow
			:
			mov a, Temp
		:		
		and a, #%01111111
		mov ChClearCounter+x, a
		inc y
		mov a, y
		mov !ChPatternPosition+x, a
		@decClearCounter:
		dec ChClearCounter+x
		ret
		
	@notClear:
	cmp a, #$40
	bcc @notMacro
		; Bit 6 is set = macro
		mov !ChInMacro+x, a
		inc y
		mov ChPatternPositionMemory+x, y

		and a, #%00111111
		asl a
		mov y, a
		mov a, [MacroIndex]+y
		mov !ChMacroAddressL+x, a
		inc y
		mov a, [MacroIndex]+y
		mov !ChMacroAddressH+x, a
		
		mov a, #0
		mov !ChPatternPosition+x, a
		
		jmp !BufferChannelRow
		
	@notMacro:
	inc y
	and a, #%00111111 ; = number of rows in block
	inc a
	mov ChRowCounter+x, a ; Block row counter (not full pattern)
	@readRow:
		dec ChRowCounter+x
		mov a, [CurrentPatternPointer]+y ; Read row header byte (instrument, effects, etc)
		inc y
		mov Temp, a ; store
		asl a
		bpl :+
			; has volume effect
			mov a, [CurrentPatternPointer]+y ; Read volume effect byte
			inc y
			mov CurrentChannelVolumeAdjust, a
			mov ChVolume+x, a ; Store in volume info also, in case no new note is read
		:
		mov a, Temp
		bpl :+
			; Has effect/command
			mov a, [CurrentPatternPointer]+y ; Read effect byte
			inc y
			asl a
			mov ChEffect+x, a
			mov a, [CurrentPatternPointer]+y ; Read effect parameter
			inc y
			mov ChEffectParam+x, a
		:
		mov a, Temp
		and a, #%00111111 ; Istrument ID
		cmp a, #60 ; "Instruments" 60+ are special commands (cut/silent/etc)
		bcc @readNote
		
			cmp a, #63
			bne :+ ; TODO: Invert if nothing else happens on 63?
				; No new note (63)
				bra @end
			:
			cmp a, #60
			bcc @readNote

				bne :+
					; Cut note (60)
					mov a, !ChannelBits+x
					or a, !NoteOff
					mov !NoteOff, a
					;mov a, #0
					;mov ChVolume+x, a
					bra @end
				:
				; Fade/off note (61/62) (currently treated as the same)
				mov a, !ChInstrumentFadeAmount+x
				mov !ChCurrentFadeAmount+x, a
				bra @end

		@readNote:
			call !ReadNote
	@end:
	mov a, y
	mov !ChPatternPosition+x, a
ret

ExecuteEffect:
jmp [!EffectRoutines+x]
NoEffect:
ret
EffectRoutines:
	.addr NoEffect
	.addr SetSpeed			;A
	.addr JumpToPattern		;B
	.addr JumpToNextPattern	;C
	.addr VolumeSlide		;D
	.addr PitchDown			;E
	.addr PitchUp			;F
	.addr NoEffect	;G
	.addr NoEffect	;H
	.addr NoEffect	;I
	.addr Arpeggio			;J
	.addr NoEffect	;K
	.addr NoEffect	;L
	.addr NoEffect	;M
	.addr NoEffect	;N
	.addr NoEffect	;O
	.addr NoEffect	;P
	.addr NoEffect	;Q
	.addr NoEffect	;R
	.addr Special	;S
EffectRoutinesEnd:
SupportedRoutines = EffectRoutinesEnd - EffectRoutines
Special:
	cmp EffectCounter, #1
	beq @return
	; Ignore on first tick
	
	mov x, a
	and a, #$f0
	cmp a, #$c0
	bne @return
	; Note-cut effect. When more effects are supported, use jump table
	
	mov a, x
	and a, #$0f
	push y
	pop x
	cmp a, EffectCounter
	bcs @return
	; If current counter is eq or higher, end note
	
	mov a, #0
	mov ChVolume+x, a
	
	@return:
ret

Arpeggio:
	push y
	pop x
	cmp a, #0
	beq :+
		mov !ChLastArpeggio+x, a
	:
	mov a, EffectCounter
	and a, #3
	cmp a, #2
	bcc @abort
		beq @firstJump

		@secondJump:
			mov a, !ChLastArpeggio+x
			and a, #$f
			bra @apply

		@firstJump:
			mov a, !ChLastArpeggio+x
			lsr a
			lsr a
			lsr a
			lsr a

		@apply:
			mov y, #64
			mul ya
			clrc
			adc a, !ChOrigPitchL+x
			mov ChPitchL+x, a
			mov a, y
			adc a, !ChOrigPitchH+x
			mov ChPitchH+x, a
			
			push x
			pop y
	@abort:
ret

GetPitchAdjustment:
	push y
	pop x
	cmp a, #0
	bne :+
		mov a, !ChLastPitchAdjust+x
	:
	mov !ChLastPitchAdjust+x, a

	mov Temp+1, #0
	cmp a, #$e0
	bcc @regular
		cmp a, #$f0
		bcs @fine
			; $Ex
			and a, #$0f
			bra @extraFine
		@fine:
			; $Fx, Only on first tick
			cmp EffectCounter, #1
			bne @abort
			and a, #$0f
			asl a
			asl a
			bra @adjust
	@regular:
	asl a
	rol Temp+1
	asl a ; x 4 (1/16 of a semitone)
	rol Temp+1

	@extraFine:
	cmp EffectCounter, #1 ; All ticks except first
	beq @abort
	
	@adjust:
	mov Temp, a
ret
	@abort:
	pop a
	pop a
ret

PitchUp:
	call !GetPitchAdjustment
	
	clrc
	mov a, ChPitchL+x
	adc a, Temp
	mov ChPitchL+x, a
	mov a, ChPitchH+x
	adc a, Temp+1
	mov ChPitchH+x, a
	bcc :+
		; Overflow
		mov a, #$ff
		mov ChPitchL+x, a
		mov ChPitchH+x, a
	:
ret
PitchDown:
	call !GetPitchAdjustment
	
	setc
	mov a, ChPitchL+x
	sbc a, Temp
	mov ChPitchL+x, a
	mov a, ChPitchH+x
	sbc a, Temp+1
	mov ChPitchH+x, a
	bcs :+
		; Underflow
		mov a, #0
		mov ChPitchL+x, a
		mov ChPitchH+x, a
	:
ret
VolumeSlide:
@sub = Temp
	push y
	pop x
	cmp a, #0
	bne :+
		mov a, !ChLastVolumeAdjust+x
	:
	mov !ChLastVolumeAdjust+x, a

	push a
	cmp a, #$0f
	beq @decrease ; $0F decreases on every tick
	
; TODO: Test everything past this, test module only had D0F effects
	cmp EffectCounter, #1
	bne @otherTicks
		; First tick
		cmp a, #$f0
		beq @return
		cmp a, #$ff
		beq @return
		cmp a, #$f0
		bcc :+
			; $Fx, Fine decrease
			@decrease:
			pop a
			and a, #$0F
			mov @sub, a
			mov a, ChVolume+x
			setc
			sbc a, @sub
			bcs @noUnderflow
				mov a, #0
			@noUnderflow:
			mov ChVolume+x, a
			ret
		:
		and a, #$0f
		cmp a, #$0f
		bne @return
			; $xF, Fine increase
			@increase:
			pop a
			lsr a
			lsr a
			lsr a
			lsr a
			clrc
			adc a, ChVolume+x
			cmp a, #64
			bcc @noOverflow
				mov a, #64
			@noOverflow:
			mov ChVolume+x, a
			ret
			
	@otherTicks:
	;TEST
		cmp a, #$10
		bcs @decrease
			; Increase?
			and a, #$0f
			cmp a, #$00
			beq @increase

	@return:
	pop a
ret
SetSpeed:
	cmp a, #0
	beq :+
		mov TrackSpeed, a
	:
ret
JumpToPattern:
	cmp TickCountDown, #1
	bne :+
		dec a
		mov !CurrentOrderIndex, a
		mov PatternRowCounter, #1
	:
ret
JumpToNextPattern:
	cmp TickCountDown, #1
	bne :+
		mov PatternRowCounter, #1
	:
ret

NoteProgressionMacros:
.byte 0,0,0,0,0,0
.byte 253 ; 54 (-3)
.byte 254 ; 55 (-2)
.byte 255 ; 56 (-1)
.byte 244 ; 57 (-12)
.byte 12 ; 58 (+12)
.byte 0 ; 59 (+0)
; A = instrument, X = channel #, Y = read pointer
ReadNote:
@channel = Temp
@instrumentAddress = Temp+1

	mov @channel, x ; We need to free the X register for a few lookup tables
	cmp a, #54
	bcc :+
		; Known note progression
		and a, #$0f
		mov x, a
		mov a, !NoteProgressionMacros+x
		mov x, @channel
		clrc
		adc a, !ChLastNote+x

		bra :++
	:
		mov !ChLastInstrument+x, a ; Remember instrument (in case of macros?)
		mov a, [CurrentPatternPointer]+y ; Read note value
		inc y
	:
	push y ; We need to free the Y register for the next few calculations

	mov !ChLastNote+x, a
	mov @instrumentAddress, #0
	mov a, !ChLastInstrument+x
	asl a
	rol @instrumentAddress
	asl a
	rol @instrumentAddress
	asl a ; instrument data size = 8 bytes
	rol @instrumentAddress
	mov y, @instrumentAddress
	addw ya, InstrumentTable
	movw @instrumentAddress, ya
	mov y, #0
	mov a, [@instrumentAddress]+y ; Read instrument sample
	mov ChSample+x, a
	inc y
	mov a, [@instrumentAddress]+y ; Read instrument pitch adjustment LoByte
	mov ChPitchL+x, a
	inc y
	mov a, [@instrumentAddress]+y ; Read instrument pitch adjustment HiByte
	mov ChPitchH+x, a
	inc y
	mov a, [@instrumentAddress]+y ; Read fadeout speed
	mov !ChInstrumentFadeAmount+x, a
	mov a, #0
	mov !ChCurrentFadeAmount+x, a
	mov !ChCurrentVEnvelopeStepL+x, a
	mov !ChCurrentVEnvelopeStepH+x, a
	inc y
	mov a, [@instrumentAddress]+y ; Read instrument volume
	mov !ChCurrentInstrumentVolume+x, a
	inc y
	
@EnvelopeAddress = Temp
	mov a, [@instrumentAddress]+y ; Read envelope low byte
	mov !ChVolumeEnvelopeL+x, a
	inc y
	mov a, [@instrumentAddress]+y ; Read envelope high byte
	mov ChVolumeEnvelopeH+x, a
	beq :+
		clrc
		mov a, !ChVolumeEnvelopeL+x
		adc a, CurrentTrackPointer
		mov !ChVolumeEnvelopeL+x, a
		mov @EnvelopeAddress, a

		mov a, ChVolumeEnvelopeH+x
		adc a, CurrentTrackPointer+1
		mov ChVolumeEnvelopeH+x, a
		mov @EnvelopeAddress+1, a
	:

	mov a, !ChLastNote+x
	
	mov	y, #64 ; Fine frequency slide works on a step of 1/64 of a semitone, so multiply by 64 to be able to adjust gradually
	mul	ya
	clrc
	adc a, ChPitchL+x ; Add note pitch to instrument adjustment, 16 bit operation with carry
	mov ChPitchL+x, a
	mov !ChOrigPitchL+x, a
	mov a, y
	adc a, ChPitchH+x
	mov ChPitchH+x, a
	mov !ChOrigPitchH+x, a
	
	mov a, CurrentChannelVolumeAdjust
	cmp a, #$ff
	bne :+
		mov a, !ChCurrentInstrumentVolume+x
	:
	mov ChVolume+x, a

	mov y, #0
	mov a, ChVolumeEnvelopeH+x
	bne :+
		; Default fade amount ($400 = not faded)
		mov y, #$04
		mov a, #$00
		bra :++
	:
		mov a, [@EnvelopeAddress]+y
		mov Temp, y
		inc y
		mov ChEnvelopePointer+x, y
		mov ChVolumeEnvelopeCounter+x, y
		asl a
		rol Temp
		asl a
		rol Temp
		mov y, Temp
	:
	mov ChFadeOutL+x, a
	mov ChFadeOutH+x, y
	
	mov a, !ChannelBits+x
	or a, !NoteStates
	mov !NoteStates, a

	pop y
ret


.macro applyVolumeEnvelope
	dec ChVolumeEnvelopeCounter+x
	bne @envelopeEnd

	mov @envelopeAddress+1, a
	mov a, !ChVolumeEnvelopeL+x
	mov @envelopeAddress, a
	mov y, ChEnvelopePointer+x
	
	@loopBack:
	mov a, [@envelopeAddress]+y
	bne :+
		; Envelope ends, apply instrument fadeout
		mov a, !ChInstrumentFadeAmount+x
		mov !ChCurrentFadeAmount+x, a
		bra @stopEnvelope
	:
	bpl :++
		; Envelope loop
		cmp a, #$80
		bne :+
			; Loop in place - ie. don't adjust volume until an explicit note off/fade command
			@stopEnvelope:
			mov a, #0
			mov !ChCurrentVEnvelopeStepL+x, a
			mov !ChCurrentVEnvelopeStepH+x, a
			mov ChVolumeEnvelopeH+x, a
			bra @envelopeEnd
		:
			; Loop back to index
			and a, #$7f
			mov y, a
			bra @loopBack
	:
	; Progress envelope as normal
	mov ChVolumeEnvelopeCounter+x, a
	inc y
	mov @envelopeDeltaFix, #$00
	mov a, [@envelopeAddress]+y
	push y
	bpl :+
		mov @envelopeDeltaFix, #$f8
	:
	mov y, #8
	mul ya
	mov !ChCurrentVEnvelopeStepL+x, a
	mov a, y
	or a, @envelopeDeltaFix
	mov !ChCurrentVEnvelopeStepH+x, a
	
	pop y
	inc y
	mov ChEnvelopePointer+x, y

	@envelopeEnd:
.endmacro


UpdateDspChannel:

	mov a, x
	asl a
	asl a
	asl a
	asl a
	mov CurrentChannelRegister, a ; $0x, $1x, $2x, etc.

	; Get DSP pitch from look-up table
			mov a, ChPitchL+x
			mov y, ChPitchH+x
			cmp y, #$20 ; Clip at higest possible octave
			bcc :+
				mov a, #$ff
				mov y, #$1f
			:
			movw Temp+0, ya ; Note*64
			
			mov	a, !LUT_DIV3+y
			mov	Temp+2, a ; Octave
			
			asl	a
			adc	a, Temp+2
			mov	Temp+3, a
			mov	a, Temp+1
			setc
			sbc	a, Temp+3
			
			asl	Temp+0				; m3 = m3*2 + LUT_FTAB base
			rol	a				;
			adc	Temp+0, #(LUT_FTAB&0FFh)		;
			adc	a, #(LUT_FTAB>>8)			; 
			mov	Temp+1, a				;
			mov	y, #0
			mov	a, [Temp]+y
			mov	Temp+4, a				;
			inc	y				;
			mov	a, [Temp]+y			;
			push	a				;
			
	mov	a, #8				; y = 8-oct
	setc					;
	sbc	a, Temp+2				;
	mov	y, a				;

	pop	a				; a,m4 = ftab value
	beq	@no_pitch_shift			; skip shift if 0
	
	lsr	a				; shift by (8-oct)
	ror	Temp+4				;
	dec	y				;
	beq	@no_pitch_shift			;
	lsr	a				;
	ror	Temp+4				;
	dec	y				;
	beq	@no_pitch_shift			;
	lsr	a				;
	ror	Temp+4				;
	dec	y				;
	beq	@no_pitch_shift			;
	lsr	a				;
	ror	Temp+4				;
	dec	y				;
	beq	@no_pitch_shift			;
	lsr	a				;	
	ror	Temp+4				;	
	dec	y				;
	beq	@no_pitch_shift			;
	lsr	a				;
	ror	Temp+4				;
	dec	y				;
	beq	@no_pitch_shift			;
	lsr	a				;
	ror	Temp+4				;
	dec	y				;
	beq	@no_pitch_shift			;
	lsr	a				;
	ror	Temp+4				;
	
	@no_pitch_shift:
	mov	Temp+5, a

	@volumeAdjustment:
@channelVolume = Temp+6
@fadeOutH = Temp+7
@envelopeAddress = Temp+8
@envelopeDeltaFix = Temp+10
@fadeDelta = Temp+8
@envelopeDelta = Temp+10
;	mov y, ChBaseVol+x
	;mov a, !ChCurrentInstrumentVolume+x
	;mul ya
;	mov @channelVolume, y
	mov a, ChBaseVol+x
	mov @channelVolume, a
	mov y, ChVolume+x
	cmp y, #65
	bcs :++ ; We don't understand any volume values >= 64 (0-64 is volume adjust)
		cmp a, #64 ; If A = 64, use 100% of adjustment value
		beq :+
			; Adjust Base vol (0-63) to (0-255) range
			asl a
			asl a
			; After multiplying, high byte should be the new channel volume (0-63)
			mul ya
		:
		mov @channelVolume, y
	:
	
	mov a, ChVolumeEnvelopeH+x
	beq @noEnvelope
		applyVolumeEnvelope
	@noEnvelope:
	mov a, !ChCurrentFadeAmount+x
	mov y, #$00
	movw @fadeDelta, ya
	mov a, !ChCurrentVEnvelopeStepH+x
	mov y, a
	mov a, !ChCurrentVEnvelopeStepL+x ; TODO: only use temp values here
	movw @envelopeDelta, ya
	
	mov a, ChFadeOutH+x
	mov y, a
	mov a, ChFadeOutL+x
	
	subw ya, @fadeDelta
	bcs :+
		; Don't allow fade to underflow
		mov a, #0
		mov y, #0
	:
	addw ya, @envelopeDelta
	bcc :+
		cmp @envelopeDelta+1, #$80
		bcs @noUnderflow
		mov a, #0
		mov y, #0
	:
		cmp @envelopeDelta+1, #$80
		bcc @noOverflow
		mov a, #$00
		mov y, #$04
	@noUnderflow:
	@noOverflow:
	mov ChFadeOutL+x, a
	mov ChFadeOutH+x, y
	
	mov @fadeOutH, y
	cmp y, #$04 ; Fade $400 = max volume
	bcs :+
		mov a, ChFadeOutL+x
		lsr @fadeOutH
		ror a
		lsr @fadeOutH
		ror a
		mov y, @channelVolume
		mul ya
		mov @channelVolume, y
	:


	@pan:
@volumeReduction = Temp+7
@panAmount = Temp+8
	mov a, ChPan+x
	mov @panAmount, a
	setc
	sbc a, #32
	bcs :+
		; Pan left, invert value
		eor a, #$ff
		adc a, #1
	:
	mov @volumeReduction, a
	
	mov a, #32
	cmp @panAmount, #33
	bcc :+
		setc
		sbc a, @volumeReduction ; If panning right, adjust left speaker
		bcs :+
			mov a, #0
	:
	mov SPC_DSPA, CurrentChannelRegister ; ChX Volume L
	mov SPC_DSPD, a
	
	mov a, #32
	cmp @panAmount, #32
	bcs :+
		setc
		sbc a, @volumeReduction ; If panning left, adjust right speaker
		bcs :+
			mov a, #0
	:
	inc SPC_DSPA ; Volume R
	mov SPC_DSPD, a
	
	inc SPC_DSPA ; Pitch low byte
	mov SPC_DSPD, Temp+4

	inc SPC_DSPA ; Pitch high byte
	mov SPC_DSPD, Temp+5

	mov a, ChSample+x
	inc SPC_DSPA ; Sample ID
	mov SPC_DSPD, a ; Instrument byte

	inc SPC_DSPA
	inc SPC_DSPA
	inc SPC_DSPA ; Gain
	mov a, @channelVolume
	bne :+
		mov a, #$9c ; Make gain slide down instead of popping out
	:
	mov SPC_DSPD, a ;@channelVolume

ret

ChannelBits:
	.byte %1
	.byte %10
	.byte %100
	.byte %1000
	.byte %10000
	.byte %100000
	.byte %1000000
	.byte %10000000

LUT_DIV3:
	.byte 0, 0, 0, 1, 1, 1, 2, 2, 2
	.byte 3, 3, 3, 4, 4, 4, 5, 5, 5
	.byte 6, 6, 6, 7, 7, 7, 8, 8, 8
	.byte 9, 9, 9,10,10
	
LUT_FTAB:
        .word 02174h, 0217Bh, 02183h, 0218Bh, 02193h, 0219Ah, 021A2h, 021AAh, 021B2h, 021BAh, 021C1h, 021C9h, 021D1h, 021D9h, 021E1h, 021E8h
        .word 021F0h, 021F8h, 02200h, 02208h, 02210h, 02218h, 0221Fh, 02227h, 0222Fh, 02237h, 0223Fh, 02247h, 0224Fh, 02257h, 0225Fh, 02267h
        .word 0226Fh, 02277h, 0227Fh, 02287h, 0228Fh, 02297h, 0229Fh, 022A7h, 022AFh, 022B7h, 022BFh, 022C7h, 022CFh, 022D7h, 022DFh, 022E7h
        .word 022EFh, 022F7h, 022FFh, 02307h, 0230Fh, 02317h, 0231Fh, 02328h, 02330h, 02338h, 02340h, 02348h, 02350h, 02358h, 02361h, 02369h
        .word 02371h, 02379h, 02381h, 0238Ah, 02392h, 0239Ah, 023A2h, 023AAh, 023B3h, 023BBh, 023C3h, 023CBh, 023D4h, 023DCh, 023E4h, 023EDh
        .word 023F5h, 023FDh, 02406h, 0240Eh, 02416h, 0241Fh, 02427h, 0242Fh, 02438h, 02440h, 02448h, 02451h, 02459h, 02462h, 0246Ah, 02472h
        .word 0247Bh, 02483h, 0248Ch, 02494h, 0249Dh, 024A5h, 024AEh, 024B6h, 024BEh, 024C7h, 024CFh, 024D8h, 024E0h, 024E9h, 024F2h, 024FAh
        .word 02503h, 0250Bh, 02514h, 0251Ch, 02525h, 0252Dh, 02536h, 0253Fh, 02547h, 02550h, 02559h, 02561h, 0256Ah, 02572h, 0257Bh, 02584h
        .word 0258Ch, 02595h, 0259Eh, 025A7h, 025AFh, 025B8h, 025C1h, 025C9h, 025D2h, 025DBh, 025E4h, 025ECh, 025F5h, 025FEh, 02607h, 0260Fh
        .word 02618h, 02621h, 0262Ah, 02633h, 0263Ch, 02644h, 0264Dh, 02656h, 0265Fh, 02668h, 02671h, 0267Ah, 02682h, 0268Bh, 02694h, 0269Dh
        .word 026A6h, 026AFh, 026B8h, 026C1h, 026CAh, 026D3h, 026DCh, 026E5h, 026EEh, 026F7h, 02700h, 02709h, 02712h, 0271Bh, 02724h, 0272Dh
        .word 02736h, 0273Fh, 02748h, 02751h, 0275Ah, 02763h, 0276Dh, 02776h, 0277Fh, 02788h, 02791h, 0279Ah, 027A3h, 027ACh, 027B6h, 027BFh
        .word 027C8h, 027D1h, 027DAh, 027E4h, 027EDh, 027F6h, 027FFh, 02809h, 02812h, 0281Bh, 02824h, 0282Eh, 02837h, 02840h, 0284Ah, 02853h
        .word 0285Ch, 02865h, 0286Fh, 02878h, 02882h, 0288Bh, 02894h, 0289Eh, 028A7h, 028B0h, 028BAh, 028C3h, 028CDh, 028D6h, 028E0h, 028E9h
        .word 028F2h, 028FCh, 02905h, 0290Fh, 02918h, 02922h, 0292Bh, 02935h, 0293Eh, 02948h, 02951h, 0295Bh, 02965h, 0296Eh, 02978h, 02981h
        .word 0298Bh, 02995h, 0299Eh, 029A8h, 029B1h, 029BBh, 029C5h, 029CEh, 029D8h, 029E2h, 029EBh, 029F5h, 029FFh, 02A08h, 02A12h, 02A1Ch
        .word 02A26h, 02A2Fh, 02A39h, 02A43h, 02A4Dh, 02A56h, 02A60h, 02A6Ah, 02A74h, 02A7Eh, 02A87h, 02A91h, 02A9Bh, 02AA5h, 02AAFh, 02AB9h
        .word 02AC3h, 02ACCh, 02AD6h, 02AE0h, 02AEAh, 02AF4h, 02AFEh, 02B08h, 02B12h, 02B1Ch, 02B26h, 02B30h, 02B3Ah, 02B44h, 02B4Eh, 02B58h
        .word 02B62h, 02B6Ch, 02B76h, 02B80h, 02B8Ah, 02B94h, 02B9Eh, 02BA8h, 02BB2h, 02BBCh, 02BC6h, 02BD1h, 02BDBh, 02BE5h, 02BEFh, 02BF9h
        .word 02C03h, 02C0Dh, 02C18h, 02C22h, 02C2Ch, 02C36h, 02C40h, 02C4Bh, 02C55h, 02C5Fh, 02C69h, 02C74h, 02C7Eh, 02C88h, 02C93h, 02C9Dh
        .word 02CA7h, 02CB2h, 02CBCh, 02CC6h, 02CD1h, 02CDBh, 02CE5h, 02CF0h, 02CFAh, 02D04h, 02D0Fh, 02D19h, 02D24h, 02D2Eh, 02D39h, 02D43h
        .word 02D4Dh, 02D58h, 02D62h, 02D6Dh, 02D77h, 02D82h, 02D8Ch, 02D97h, 02DA1h, 02DACh, 02DB7h, 02DC1h, 02DCCh, 02DD6h, 02DE1h, 02DECh
        .word 02DF6h, 02E01h, 02E0Bh, 02E16h, 02E21h, 02E2Bh, 02E36h, 02E41h, 02E4Bh, 02E56h, 02E61h, 02E6Ch, 02E76h, 02E81h, 02E8Ch, 02E97h
        .word 02EA1h, 02EACh, 02EB7h, 02EC2h, 02ECCh, 02ED7h, 02EE2h, 02EEDh, 02EF8h, 02F03h, 02F0Eh, 02F18h, 02F23h, 02F2Eh, 02F39h, 02F44h
        .word 02F4Fh, 02F5Ah, 02F65h, 02F70h, 02F7Bh, 02F86h, 02F91h, 02F9Ch, 02FA7h, 02FB2h, 02FBDh, 02FC8h, 02FD3h, 02FDEh, 02FE9h, 02FF4h
        .word 02FFFh, 0300Ah, 03015h, 03020h, 0302Ch, 03037h, 03042h, 0304Dh, 03058h, 03063h, 0306Eh, 0307Ah, 03085h, 03090h, 0309Bh, 030A7h
        .word 030B2h, 030BDh, 030C8h, 030D4h, 030DFh, 030EAh, 030F5h, 03101h, 0310Ch, 03117h, 03123h, 0312Eh, 0313Ah, 03145h, 03150h, 0315Ch
        .word 03167h, 03173h, 0317Eh, 03189h, 03195h, 031A0h, 031ACh, 031B7h, 031C3h, 031CEh, 031DAh, 031E5h, 031F1h, 031FCh, 03208h, 03213h
        .word 0321Fh, 0322Bh, 03236h, 03242h, 0324Dh, 03259h, 03265h, 03270h, 0327Ch, 03288h, 03293h, 0329Fh, 032ABh, 032B7h, 032C2h, 032CEh
        .word 032DAh, 032E5h, 032F1h, 032FDh, 03309h, 03315h, 03320h, 0332Ch, 03338h, 03344h, 03350h, 0335Ch, 03367h, 03373h, 0337Fh, 0338Bh
        .word 03397h, 033A3h, 033AFh, 033BBh, 033C7h, 033D3h, 033DFh, 033EBh, 033F7h, 03403h, 0340Fh, 0341Bh, 03427h, 03433h, 0343Fh, 0344Bh
        .word 03457h, 03463h, 0346Fh, 0347Bh, 03488h, 03494h, 034A0h, 034ACh, 034B8h, 034C4h, 034D1h, 034DDh, 034E9h, 034F5h, 03502h, 0350Eh
        .word 0351Ah, 03526h, 03533h, 0353Fh, 0354Bh, 03558h, 03564h, 03570h, 0357Dh, 03589h, 03595h, 035A2h, 035AEh, 035BAh, 035C7h, 035D3h
        .word 035E0h, 035ECh, 035F9h, 03605h, 03612h, 0361Eh, 0362Bh, 03637h, 03644h, 03650h, 0365Dh, 03669h, 03676h, 03683h, 0368Fh, 0369Ch
        .word 036A8h, 036B5h, 036C2h, 036CEh, 036DBh, 036E8h, 036F4h, 03701h, 0370Eh, 0371Bh, 03727h, 03734h, 03741h, 0374Eh, 0375Ah, 03767h
        .word 03774h, 03781h, 0378Eh, 0379Ah, 037A7h, 037B4h, 037C1h, 037CEh, 037DBh, 037E8h, 037F5h, 03802h, 0380Eh, 0381Bh, 03828h, 03835h
        .word 03842h, 0384Fh, 0385Ch, 03869h, 03876h, 03884h, 03891h, 0389Eh, 038ABh, 038B8h, 038C5h, 038D2h, 038DFh, 038ECh, 038FAh, 03907h
        .word 03914h, 03921h, 0392Eh, 0393Bh, 03949h, 03956h, 03963h, 03970h, 0397Eh, 0398Bh, 03998h, 039A6h, 039B3h, 039C0h, 039CEh, 039DBh
        .word 039E8h, 039F6h, 03A03h, 03A11h, 03A1Eh, 03A2Bh, 03A39h, 03A46h, 03A54h, 03A61h, 03A6Fh, 03A7Ch, 03A8Ah, 03A97h, 03AA5h, 03AB2h
        .word 03AC0h, 03ACEh, 03ADBh, 03AE9h, 03AF6h, 03B04h, 03B12h, 03B1Fh, 03B2Dh, 03B3Bh, 03B48h, 03B56h, 03B64h, 03B72h, 03B7Fh, 03B8Dh
        .word 03B9Bh, 03BA9h, 03BB6h, 03BC4h, 03BD2h, 03BE0h, 03BEEh, 03BFCh, 03C09h, 03C17h, 03C25h, 03C33h, 03C41h, 03C4Fh, 03C5Dh, 03C6Bh
        .word 03C79h, 03C87h, 03C95h, 03CA3h, 03CB1h, 03CBFh, 03CCDh, 03CDBh, 03CE9h, 03CF7h, 03D05h, 03D13h, 03D21h, 03D2Fh, 03D3Eh, 03D4Ch
        .word 03D5Ah, 03D68h, 03D76h, 03D85h, 03D93h, 03DA1h, 03DAFh, 03DBDh, 03DCCh, 03DDAh, 03DE8h, 03DF7h, 03E05h, 03E13h, 03E22h, 03E30h
        .word 03E3Eh, 03E4Dh, 03E5Bh, 03E6Ah, 03E78h, 03E86h, 03E95h, 03EA3h, 03EB2h, 03EC0h, 03ECFh, 03EDDh, 03EECh, 03EFAh, 03F09h, 03F18h
        .word 03F26h, 03F35h, 03F43h, 03F52h, 03F61h, 03F6Fh, 03F7Eh, 03F8Dh, 03F9Bh, 03FAAh, 03FB9h, 03FC7h, 03FD6h, 03FE5h, 03FF4h, 04002h
        .word 04011h, 04020h, 0402Fh, 0403Eh, 0404Dh, 0405Bh, 0406Ah, 04079h, 04088h, 04097h, 040A6h, 040B5h, 040C4h, 040D3h, 040E2h, 040F1h
        .word 04100h, 0410Fh, 0411Eh, 0412Dh, 0413Ch, 0414Bh, 0415Ah, 04169h, 04178h, 04188h, 04197h, 041A6h, 041B5h, 041C4h, 041D3h, 041E3h
        .word 041F2h, 04201h, 04210h, 04220h, 0422Fh, 0423Eh, 0424Eh, 0425Dh, 0426Ch, 0427Ch, 0428Bh, 0429Ah, 042AAh, 042B9h, 042C9h, 042D8h

DspChannels:
	.byte $00,$10,$20,$30,$40,$50,$60,$70

.align $100
.assert * = DirAddress, error, "Unaligned track data"

.reloc