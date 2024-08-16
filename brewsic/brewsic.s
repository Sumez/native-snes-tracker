.export SpcImage:far, SpcImageEnd:far

;.include "brewsic-spc.s"

.segment "SPCIMAGE"
SpcImage:
;.incbin "brewsic.spc"
;.incbin "brewsic/frp.sfc.spc"
.incbin "bin/bsdj.sfc.spc"
SpcImageEnd:
