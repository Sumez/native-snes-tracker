.export SpcImage:far, SpcImageEnd:far

;.include "brewsic-spc.s"

.segment "RODATA4"
SpcImage:
;.incbin "brewsic.spc"
;.incbin "brewsic/frp.sfc.spc"
.incbin "obj/bsdj.sfc.spc"
SpcImageEnd:
