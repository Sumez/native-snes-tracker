.include "global.inc"

.macro AddSample name, source
.local BrrStart, BrrEnd
	.byte name,$ff
	.word .loword(BrrEnd-BrrStart-2)
	.word $0000
	BrrStart: .incbin source
	BrrEnd:
.endmacro
.macro AddNoLoopSample name, source
.local BrrStart, BrrEnd
	.byte name,$ff
	.word .loword(BrrEnd-BrrStart)
	.word $0000
	.word $0000
	BrrStart: 
		.incbin source
	BrrEnd:
.endmacro

.segment "RODATA"

.export SAMPLE_DATA
SAMPLE_DATA:
.align $100
.incbin "bin/samples2.brrp"
;.incbin "bin/rgsh.brrp"
AddSample "Sample_0", "music/sample0.brr"
AddSample "Sample_1", "music/sample1.brr"
AddSample "Sample_2", "music/sample2.brr"
AddSample "Sample_3", "music/sample3.brr"
AddSample "Sample_4", "music/sample4.brr"
AddSample "Sample_5", "music/sample5.brr"
AddSample "Sample_6", "music/sample6.brr"
AddNoLoopSample "bass_elec", "samples/bass_elec.brr"
AddNoLoopSample "perc_hat_closed", "samples/perc_hat_closed.brr"
AddNoLoopSample "perc_snare", "samples/perc_snare.brr"
SampleInsertionPoint: .byte $ff,$00,$00

.segment "CODE7"

.export DefaultSamples, DefaultSamplesEnd
DefaultSamples:
.align 16,$ff
.addr 1*8
.byte"Sample_1"
.align 16,$ff
.addr 2*8
.byte"Sample_2"
.align 16,$ff
.addr 3*8
.byte"Sample_3"
.align 16,$ff
.addr 5*8
.byte"Sample_5"
.align 16,$ff
.addr 6*8
.byte"Sample_6"
.align 16,$ff
.addr 0*8
.byte"Sample_0"
.align 16,$ff
DefaultSamplesEnd:
