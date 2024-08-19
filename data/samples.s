.include "global.inc"

.macro AddSample name, source
.local BrrStart, BrrEnd
	.byte name,$ff
	.word .loword(BrrEnd-BrrStart-2)
	BrrStart: .incbin source
	BrrEnd:
.endmacro

.segment "RODATA"

.export SAMPLE_DATA
SAMPLE_DATA:
.align $100
S5: AddSample "Sample_0", "music/sample0.brr"
S0: AddSample "Sample_1", "music/sample1.brr"
S1: AddSample "Sample_2", "music/sample2.brr"
S2: AddSample "Sample_3", "music/sample3.brr"
AddSample "Sample_4", "music/sample4.brr"
S3: AddSample "Sample_5", "music/sample5.brr"
S4:AddSample "Sample_6", "music/sample6.brr"
AddSample "Sample_7", "music/sample7.brr"
AddSample "Sample_8", "music/sample8.brr"
AddSample "Sample_9", "music/sample9.brr"
AddSample "Sample_10", "music/sample10.brr"
AddSample "Sample_11", "music/sample11.brr"
AddSample "Sample_12", "music/sample12.brr"
AddSample "Sample_13", "music/sample13.brr"
AddSample "Sample_14", "music/sample14.brr"
.byte $ff,$00,$00

.segment "CODE7"

.export DefaultSamples, DefaultSamplesEnd
DefaultSamples:
.align 16,$ff
.addr 1*6
.byte"Sample_1"
.align 16,$ff
.addr 2*6
.byte"Sample_2"
.align 16,$ff
.addr 3*6
.byte"Sample_3"
.align 16,$ff
.addr 5*6
.byte"Sample_5"
.align 16,$ff
.addr 6*6
.byte"Sample_6"
.align 16,$ff
.addr 0*6
.byte"Sample_0"
.align 16,$ff
DefaultSamplesEnd:
