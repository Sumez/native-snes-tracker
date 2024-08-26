.include "src/spc-ca65.inc"

Channels = 8
SampleDirectoryAddress = $1100

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
CurrentChannelVolumeOverride: .res 1
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
LoopToPattern: .res 1

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
SPC_PORT2	=0F6h ; i/o port2 - Reads command params, Writes current bar in current pattern
SPC_PORT3	=0F7h ; i/o port3 - Reads command params, Writes 0xFF(?) after track is done playing (nothing set before first playback)
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
dsp DSP_DIR, (SampleDirectoryAddress >> 8)

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

	; tell PORT3 that playback is active
	mov SPC_PORT3, #0

	; First tick, engage immediately to prevent "start lag",
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
	
	mov a, !(SampleDirectoryAddress-1)
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
	mov a, SPC_PORT1 ; Read again to ensure it wasn't a dirty read
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
	mov a, #SampleDirectoryAddress & $ff
	mov y, #SampleDirectoryAddress >> 8
	movw <TrackDirectoryPointer, ya
	mov a, !(SampleDirectoryAddress-1)
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
	; TODO: Define loop by a loop-start index instead of a loop command in the end (just go back to alway $00 in the end)
	mov !LoopToPattern, a
	call !ResetPattern
	inc PatternRowCounter ; Usually a new track is loaded AFTER counting down one row, so add +1 on the first pattern to acocunt for that
ret

ResetPattern:
	mov TickCountDown, TrackSpeed

.ifdef TESTPATTERN
	mov y, #TESTPATTERN
.else
	mov y, !CurrentOrderIndex
.endif
	mov a, [PatternLengthTable]+y
	cmp a, #$00
	bne :++
		; Reached end of patterns
		mov a, !LoopToPattern
		cmp a, #$ff
		bne :+
			; No looping, end playback
			jmp !StopTrack
		:
		mov !CurrentOrderIndex, a
		jmp !ResetPattern
	:
	mov PatternRowCounter, a
	
	mov a, #16
	mul ya
	addw ya, CurrentTrackPointer ; Current pattern index * 16 + Start of address table = Current pattern address (every entry is 16 bytes, two address bytes per 8 channels)
	movw Temp+0, ya

	mov x, #15
	mov y, #15
	@channelLoop: ; Set pattern address for each channel
		mov a, [Temp+0]+y
		mov !ChPatternPointers+x, a
		dec y
		dec x
	bpl @channelLoop
	

	mov x, #7
	@channelLoop2: ; Init all channels from data that shouldn't carry over between patterns
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
	mov SPC_PORT3, a ; Also set PORT3 to tell we are done playing
	mov SPC_DSPA, #DSP_KOF
	mov SPC_DSPD, a
	mov x, #$ED ; force jumping back to regular wait loop
	mov sp, x
ret



TickModule:

.ifdef SkipLastChannelForSFX
	mov x, #6
.elseif .defined(TESTCHANNEL)
	mov x, #TESTCHANNEL
.else
	mov x, #7
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


		@newBeat:
		dec PatternRowCounter
		bne :+
			inc !CurrentOrderIndex
			call !ResetPattern
		:
		mov a, PatternRowCounter
		mov SPC_PORT2, a ; Tell active bar/beat to CPU, for CPU-controlled playback
		
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

	; init everything that's related to reading a new note bar
	mov a, #0
	mov ChEffect+x, a
	mov ChEffectParam+x, a
	mov a, #$ff
	mov CurrentChannelVolumeOverride, a ; Default to $ff (no adjustment) on new notes if no volume column was read

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
			mov CurrentChannelVolumeOverride, a
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
					call !CutNote
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
	.addr NoEffect
	;.addr JumpToPattern		;B
	.addr NoEffect
	;.addr JumpToNextPattern	;C
	.addr VolumeSlide		;D
	.addr PitchDown			;E
	.addr PitchUp			;F
	.addr VolumeAdjust
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
VolumeAdjust:
	push y
	pop x
	mov ChVolume+x, a
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
return: ret

CutNote:
	mov a, !ChannelBits+x
	or a, !NoteOff
	mov !NoteOff, a
ret

NoteProgressionMacros:
.byte 0,0,0,0,0,0 ; Inaccessible! (instrument IDs up to 53 are accepted)
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
		mov !ChLastInstrument+x, a ; Remember instrument (in case of note progression macros)
		mov a, [CurrentPatternPointer]+y ; Read note value
		inc y
		cmp a, #$ff ; TRK: Ineffecient way to indicate no note, but ensures constant pattern size for live edits in tracker
		beq return
		cmp a, #$fe ; TRK: Cut note
		bne :+
			jmp !CutNote
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
	mov !ChOrigPitchL+x, a ; Stored for use in arp commands
	mov a, y
	adc a, ChPitchH+x
	mov ChPitchH+x, a
	mov !ChOrigPitchH+x, a ; Stored for use in arp commands
	
	mov a, CurrentChannelVolumeOverride
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
			movw Temp+0, ya ; Note*64, meaning every (64*12) 768th increment is an octave.
			
			mov	a, !LUT_DIV3+y ; 768 is 3 times 256, so if we divide the high byte by 3 we get the target octave
			mov	Temp+2, a ; Octave
			
			asl	a
			adc	a, Temp+2 ; Multiply by 3 again, shedding any remainder value (TODO: use a LUT for this?)
			mov	Temp+3, a
			mov	a, Temp+1
			setc
			sbc	a, Temp+3 ; Get that remainder value... seriously there must be a better way to do this
			; A<<8 | Temp+0 is now a spot on the octave (~0-11.984), multiplied by 64 (0-767)
			
			; Roll left once because the table is 16bit values (TODO: just split the table into one two tables of high bytes and low bytes)
			asl	Temp+0
			rol	a
			; The LUT is too large to look up using an index register, so just add the address (TODO: just split into three LUTs and use a CMP to branch to the right one. Maybe even don't divide by three, and use a binary-friendly approach from the start?
			adc	Temp+0, #<LUT_FTAB
			adc	a, #>LUT_FTAB
			mov	Temp+1, a ; Now Temp0+1 have a 16 bit address to the target frequency on the highest octave
			mov	y, #0
			mov	a, [Temp+0]+y
			mov	Temp+4, a
			inc	y
			mov	a, [Temp+0]+y
			push a
			
	mov	a, #8
	setc
	sbc	a, Temp+2
	mov	y, a ; Y is now 8 minus octave ($8,7,6,5,4,3,2,1,0,FF)

	pop	a				; A<<8 | Temp4 = ftab value
	beq	@no_pitch_shift			; Z flag is carried from the mov to Y. Skip shift if 0
	;Half the frequency for every octave step
	:
		lsr	a
		ror	Temp+4				;
		dec	y				;
	bne :-
	
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
Octave8:
; Octave randing from $2000 to $4000 pitch (the highest a SNES can play correctly)
; JS to generate data:
; const notes = []; for (let i = 0; i < 768; i++) notes.push(Math.round(0x2000 * Math.pow(Math.pow(2, 1/768),i))); console.log(notes.map(n => `$${n.toString(16)}`).join(','));
.word $2000,$2007,$200f,$2016,$201e,$2025,$202c,$2034,$203b,$2043,$204a,$2052,$2059,$2061,$2068,$2070,$2077,$207f,$2086,$208e,$2095,$209d,$20a4,$20ac,$20b3,$20bb,$20c3,$20ca,$20d2,$20d9,$20e1,$20e8,$20f0,$20f8,$20ff,$2107,$210f,$2116,$211e,$2125,$212d,$2135,$213c,$2144,$214c,$2154,$215b,$2163,$216b,$2172,$217a,$2182,$218a,$2191,$2199,$21a1,$21a9,$21b0,$21b8,$21c0,$21c8,$21d0,$21d7,$21df,$21e7,$21ef,$21f7,$21ff,$2207,$220e,$2216,$221e,$2226,$222e,$2236,$223e,$2246,$224e,$2255,$225d,$2265,$226d,$2275,$227d,$2285,$228d,$2295,$229d,$22a5,$22ad,$22b5,$22bd,$22c5,$22cd,$22d5,$22dd,$22e5,$22ee,$22f6,$22fe,$2306,$230e,$2316,$231e,$2326,$232e,$2336,$233f,$2347,$234f,$2357,$235f,$2367,$2370,$2378,$2380,$2388,$2390,$2399,$23a1,$23a9,$23b1,$23ba,$23c2,$23ca,$23d2,$23db,$23e3,$23eb,$23f4,$23fc,$2404,$240c,$2415,$241d,$2425,$242e,$2436,$243f,$2447,$244f,$2458,$2460,$2469,$2471,$2479,$2482,$248a,$2493,$249b,$24a4,$24ac,$24b5,$24bd,$24c6,$24ce,$24d7,$24df,$24e8,$24f0,$24f9,$2501,$250a,$2512,$251b,$2523,$252c,$2535,$253d,$2546,$254e,$2557,$2560,$2568,$2571,$257a,$2582,$258b,$2594,$259c,$25a5,$25ae,$25b6,$25bf,$25c8,$25d1,$25d9,$25e2,$25eb,$25f4,$25fc,$2605,$260e,$2617,$2620,$2628,$2631,$263a,$2643,$264c,$2655,$265d,$2666,$266f,$2678,$2681,$268a,$2693,$269c,$26a5,$26ae,$26b6,$26bf,$26c8,$26d1,$26da,$26e3,$26ec,$26f5,$26fe,$2707,$2710,$2719,$2722,$272b,$2735,$273e,$2747,$2750,$2759,$2762,$276b,$2774,$277d,$2786,$278f,$2799,$27a2,$27ab,$27b4,$27bd,$27c6,$27d0,$27d9,$27e2,$27eb,$27f5,$27fe,$2807,$2810,$281a,$2823,$282c,$2835,$283f,$2848,$2851,$285b,$2864,$286d,$2877,$2880,$2889,$2893,$289c,$28a5,$28af,$28b8,$28c2,$28cb,$28d5,$28de,$28e7,$28f1,$28fa,$2904,$290d,$2917,$2920,$292a,$2933,$293d,$2946,$2950,$2959,$2963,$296d,$2976,$2980,$2989,$2993,$299d,$29a6,$29b0,$29b9,$29c3,$29cd,$29d6,$29e0,$29ea,$29f3,$29fd,$2a07,$2a11,$2a1a,$2a24,$2a2e,$2a37,$2a41,$2a4b,$2a55,$2a5f,$2a68,$2a72,$2a7c,$2a86,$2a90,$2a99,$2aa3,$2aad,$2ab7,$2ac1,$2acb,$2ad5,$2adf,$2ae8,$2af2,$2afc,$2b06,$2b10,$2b1a,$2b24,$2b2e,$2b38,$2b42,$2b4c,$2b56,$2b60,$2b6a,$2b74,$2b7e,$2b88,$2b92,$2b9c,$2ba6,$2bb1,$2bbb,$2bc5,$2bcf,$2bd9,$2be3,$2bed,$2bf7,$2c02,$2c0c,$2c16,$2c20,$2c2a,$2c35,$2c3f,$2c49,$2c53,$2c5d,$2c68,$2c72,$2c7c,$2c87,$2c91,$2c9b,$2ca5,$2cb0,$2cba,$2cc4,$2ccf,$2cd9,$2ce4,$2cee,$2cf8,$2d03,$2d0d,$2d17,$2d22,$2d2c,$2d37,$2d41,$2d4c,$2d56,$2d61,$2d6b,$2d76,$2d80,$2d8b,$2d95,$2da0,$2daa,$2db5,$2dbf,$2dca,$2dd5,$2ddf,$2dea,$2df4,$2dff,$2e0a,$2e14,$2e1f,$2e2a,$2e34,$2e3f,$2e4a,$2e54,$2e5f,$2e6a,$2e74,$2e7f,$2e8a,$2e95,$2e9f,$2eaa,$2eb5,$2ec0,$2ecb,$2ed5,$2ee0,$2eeb,$2ef6,$2f01,$2f0c,$2f17,$2f21,$2f2c,$2f37,$2f42,$2f4d,$2f58,$2f63,$2f6e,$2f79,$2f84,$2f8f,$2f9a,$2fa5,$2fb0,$2fbb,$2fc6,$2fd1,$2fdc,$2fe7,$2ff2,$2ffd,$3008,$3013,$301f,$302a,$3035,$3040,$304b,$3056,$3061,$306d,$3078,$3083,$308e,$3099,$30a5,$30b0,$30bb,$30c6,$30d2,$30dd,$30e8,$30f4,$30ff,$310a,$3116,$3121,$312c,$3138,$3143,$314e,$315a,$3165,$3171,$317c,$3187,$3193,$319e,$31aa,$31b5,$31c1,$31cc,$31d8,$31e3,$31ef,$31fa,$3206,$3212,$321d,$3229,$3234,$3240,$324c,$3257,$3263,$326e,$327a,$3286,$3291,$329d,$32a9,$32b5,$32c0,$32cc,$32d8,$32e3,$32ef,$32fb,$3307,$3313,$331e,$332a,$3336,$3342,$334e,$335a,$3365,$3371,$337d,$3389,$3395,$33a1,$33ad,$33b9,$33c5,$33d1,$33dd,$33e9,$33f5,$3401,$340d,$3419,$3425,$3431,$343d,$3449,$3455,$3461,$346d,$3479,$3486,$3492,$349e,$34aa,$34b6,$34c2,$34cf,$34db,$34e7,$34f3,$34ff,$350c,$3518,$3524,$3531,$353d,$3549,$3555,$3562,$356e,$357a,$3587,$3593,$35a0,$35ac,$35b8,$35c5,$35d1,$35de,$35ea,$35f7,$3603,$3610,$361c,$3629,$3635,$3642,$364e,$365b,$3667,$3674,$3680,$368d,$369a,$36a6,$36b3,$36c0,$36cc,$36d9,$36e6,$36f2,$36ff,$370c,$3718,$3725,$3732,$373f,$374b,$3758,$3765,$3772,$377f,$378b,$3798,$37a5,$37b2,$37bf,$37cc,$37d9,$37e6,$37f2,$37ff,$380c,$3819,$3826,$3833,$3840,$384d,$385a,$3867,$3874,$3881,$388e,$389b,$38a9,$38b6,$38c3,$38d0,$38dd,$38ea,$38f7,$3904,$3912,$391f,$392c,$3939,$3947,$3954,$3961,$396e,$397c,$3989,$3996,$39a3,$39b1,$39be,$39cb,$39d9,$39e6,$39f4,$3a01,$3a0e,$3a1c,$3a29,$3a37,$3a44,$3a52,$3a5f,$3a6d,$3a7a,$3a88,$3a95,$3aa3,$3ab0,$3abe,$3acb,$3ad9,$3ae7,$3af4,$3b02,$3b0f,$3b1d,$3b2b,$3b38,$3b46,$3b54,$3b62,$3b6f,$3b7d,$3b8b,$3b98,$3ba6,$3bb4,$3bc2,$3bd0,$3bdd,$3beb,$3bf9,$3c07,$3c15,$3c23,$3c31,$3c3f,$3c4d,$3c5a,$3c68,$3c76,$3c84,$3c92,$3ca0,$3cae,$3cbc,$3cca,$3cd8,$3ce7,$3cf5,$3d03,$3d11,$3d1f,$3d2d,$3d3b,$3d49,$3d58,$3d66,$3d74,$3d82,$3d90,$3d9f,$3dad,$3dbb,$3dc9,$3dd8,$3de6,$3df4,$3e03,$3e11,$3e1f,$3e2e,$3e3c,$3e4a,$3e59,$3e67,$3e76,$3e84,$3e92,$3ea1,$3eaf,$3ebe,$3ecc,$3edb,$3ee9,$3ef8,$3f07,$3f15,$3f24,$3f32,$3f41,$3f50,$3f5e,$3f6d,$3f7b,$3f8a,$3f99,$3fa8,$3fb6,$3fc5,$3fd4,$3fe2,$3ff1

DspChannels:
	.byte $00,$10,$20,$30,$40,$50,$60,$70

nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
.align $100
.assert * = SampleDirectoryAddress, error, "Unaligned track data - Update directory address in both SPC and CPU code. Compiled track images will also not work anymore"

.reloc