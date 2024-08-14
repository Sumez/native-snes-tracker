; Super Nintendo SPC-700 program assembly using ca65

; Copyright (C) 2013-2014 Shay Green <gblargg@gmail.com>
; Copying and distribution of this file, with or without modification, are
; permitted in any medium without royalty provided the copyright notice and
; this notice are preserved. This file is offered as-is, without any warranty.

;.ifndef SPC_CA65_INC ; ca65 still barfs on file second time through with this, ugh
SPC_CA65_INC = 1

.setcpu "none"

.ifndef DEFAULT_ABS
	DEFAULT_ABS = 0
.endif

.ifndef WARN_DEFAULT_ABS
	WARN_DEFAULT_ABS = 0
.endif

.if WARN_DEFAULT_ABS
	.define _op_warn_default_abs .assert 0, warning, "Defaulting to absolute addressing"
.else
	.define _op_warn_default_abs
.endif

; Handle * properly
.macro _op_tb op, val
	.local @target
	@target = val
	.byte op, @target
.endmacro

.macro _op_tb2 op, val, val2
	.local @target, @target2
	@target = val
	@target2 = val2
	.byte op, @target, @target2
.endmacro

.macro _op_tw op, val
	.local @target
	@target = val
	.byte op
	.word @target
.endmacro


;**** OP a, src
;**** OP dest, a

; d, !a, (X), [d+X], 
.macro _op_a op, val
	.if .xmatch (.left (1, {val}), !)
		.if .xmatch (.right (2, {val}), +x)
			_op_tw $15+op, .mid (1, .tcount ({val})-3, {val}) ; !a+X
		.elseif .xmatch (.right (2, {val}), +y)
			_op_tw $16+op, .mid (1, .tcount ({val})-3, {val}) ; !a+Y
		.else
			_op_tw $05+op, .right (.tcount ({val})-1, {val}) ; !a
		.endif
	.elseif .xmatch (.left (1, {val}), [)
		.if .xmatch (.right (3, {val}), +x])
			_op_tb $07+op, .mid (1, .tcount ({val})-4, {val}) ; [d+X]
		.elseif .xmatch (.right (3, {val}), ]+y)
			_op_tb $17+op, .mid (1, .tcount ({val})-4, {val}) ; [d]+Y
		.else
			.assert 0, error, "unrecognized [] addressing mode"
		.endif
	.elseif .xmatch ({val}, {(x)})
		.byte $06+op ; (X)
	.elseif ::DEFAULT_ABS && (!.xmatch (.left (1, {val}), <))
		_op_warn_default_abs
		.if .xmatch (.right (2, {val}), +x)
			_op_tw $15+op, .left (.tcount ({val})-2, {val}) ; a+X
		.elseif .xmatch (.right (2, {val}), +y)
			_op_tw $16+op, .left (.tcount ({val})-2, {val}) ; a+Y
		.else
			_op_tw $05+op, val ; a
		.endif
	.elseif .xmatch (.right (2, {val}), +x)
		_op_tb $14+op, .left (.tcount ({val})-2, {val}) ; d+X
	.else
		_op_tb $04+op, val ; d
	.endif
.endmacro

.macro _op_imm op, val
	.if .xmatch (.left (1, {val}), #)
		_op_tb $08+op, .right (.tcount ({val})-1, {val}) ; #
	.else
		_op_a op, {val}
	.endif
.endmacro

.macro _arith_a op, dest, src
	.if .xmatch (.left (1, {dest}), a)
		_op_imm op, src ; a, src
	.elseif .xmatch (.left (1, {src}), #)
		_op_tb2 $18+op, .right (.tcount ({src})-1, {src}), dest ; d, #
	.elseif .xmatch ({dest}, {(x)}) && .xmatch ({src}, {(y)})
		.byte $19+op ; (x), (y)
	.else
		_op_tb2 $09+op, src, dest ; dd, ds
	.endif
.endmacro

.define or   _arith_a $00, ; dest, src
.define and_ _arith_a $20, ; dest, src
.define eor_ _arith_a $40, ; dest, src
.define adc_ _arith_a $80, ; dest, src
.define sbc_ _arith_a $A0, ; dest, src

;**** MOV dest, src

.macro _mov_a op, op_xplus, val
	.if .xmatch ({val}, {(x)+})
		.byte op_xplus ; (X)+
	.elseif .xmatch ({val}, x)
		.byte op-$63 ; x
	.elseif .xmatch ({val}, y)
		.byte op-$03 ; y
	.else
		_op_imm op, {val}
	.endif
.endmacro

.macro _mov_y op, val
	.if .xmatch (.left (1, {val}), !)
		_op_tw $ec-op, .right (.tcount ({val})-1, {val})
	.elseif .xmatch (.right (2, {val}), +x)
		_op_tb $fb-op, .left (.tcount ({val})-2, {val})
	.elseif ::DEFAULT_ABS && (!.xmatch (.left (1, {val}), <))
		_op_warn_default_abs
		_op_tw $ec-op, val
	.else
		_op_tb $eb-op, val
	.endif
.endmacro

.macro _mov_x op, val
	.if .xmatch (.left (1, {val}), !)
		_op_tw $e9-op, .right (.tcount ({val})-1, {val})
	.elseif .xmatch (.right (2, {val}), +y)
		_op_tb $f9-op, .left (.tcount ({val})-2, {val})
	.elseif ::DEFAULT_ABS && (!.xmatch (.left (1, {val}), <))
		_op_warn_default_abs
		_op_tw $e9-op, val
	.else
		_op_tb $f8-op, val
	.endif
.endmacro

.macro mov dest, src
	.if .xmatch ({dest}, y)
		.if .xmatch (.left (1, {src}), #)
			_op_tb $8d, .right (.tcount ({src})-1, {src})
		.elseif .xmatch ({src}, a)
			.byte $fd ; a (inconsistent encoding)
		.else
			_mov_y 0, {src}
		.endif
	.elseif .xmatch ({dest}, a)
		_mov_a $e0, $BF, src ; MOV A, src
	.elseif .xmatch ({src}, a)
		_mov_a $c0, $AF, dest ; MOV dest, A
	.elseif .xmatch ({dest}, x)
		.if .xmatch (.left (1, {src}), #)
			_op_tb $cd, .right (.tcount ({src})-1, {src})
		.elseif .xmatch ({src}, sp)
			.byte $9d
		.else
			_mov_x 0, src
		.endif
	.elseif .xmatch ({src}, x)
		.if .xmatch ({dest}, sp)
			.byte $BD
		.else
			_mov_x $20, dest
		.endif
	.elseif .xmatch ({src}, y)
		_mov_y $20, {dest}
	.elseif .xmatch (.left (1, {src}), #)
		_op_tb2 $8f, .right (.tcount ({src})-1, {src}), dest
	.else
		_op_tb2 $fa, src, dest
	.endif
.endmacro

;**** CMP src1, src2

.macro _cmp_xy op, immop, val
	.if .xmatch (.left (1, {val}), #)
		_op_tb immop, .right (.tcount ({val})-1, {val}) ; #
	.elseif .xmatch (.left (1, {val}), !)
		_op_tw $1e+op, .right (.tcount ({val})-1, {val}) ; !a
	.elseif ::DEFAULT_ABS && (!.xmatch (.left (1, {val}), <))
		_op_warn_default_abs
		_op_tw $1e+op, val ; a
	.else
		_op_tb $3e+op, val ; d
	.endif
.endmacro

.macro cmp_ dest, src
	.if .xmatch ({dest}, x)
		_cmp_xy 0, $c8, {src}
	.elseif .xmatch ({dest}, y)
		_cmp_xy $40, $ad, {src}
	.else
		_arith_a $60, dest, src
	.endif
.endmacro

;**** RMW dest

.macro _op_shift op, val
	.if .xmatch ({val}, a)
		.byte $1c+op ; A
	.elseif .xmatch (.left (1, {val}), !)
		_op_tw $0c+op, .right (.tcount ({val})-1, {val}) ; !a
	.elseif .xmatch (.right (2, {val}), +x)
		_op_tb $1b+op, .left (.tcount ({val})-2, {val})
	.elseif ::DEFAULT_ABS && (!.xmatch (.left (1, {val}), <))
		_op_warn_default_abs
		_op_tw $0c+op, val
	.else
		_op_tb $0b+op, val
	.endif
.endmacro

.macro _inc_dec op, val
	.if .xmatch ({val}, x)
		.byte $1d+op ; x
	.elseif .xmatch ({val}, y)
		.byte $dc+op ; y
	.else
		_op_shift $80+op, {val}
	.endif
.endmacro

.define dec_ _inc_dec  $00, ; val
.define inc_ _inc_dec  $20, ; val
.define lsr_ _op_shift $40, ; val
.define asl_ _op_shift $00, ; val
.define rol_ _op_shift $20, ; val
.define ror_ _op_shift $60, ; val

;**** PUSH/POP

.macro _push_pop op, val
	.if .xmatch ({val}, a)
		.byte $2d+op
	.elseif .xmatch ({val}, x)
		.byte $4d+op
	.elseif .xmatch ({val}, y)
		.byte $6d+op
	.elseif .xmatch ({val}, psw)
		.byte $0d+op
	.else
		.assert 0, error, "invalid register"
	.endif
.endmacro

.define push _push_pop $00,
.define pop  _push_pop $81,

;**** SET1/CLR1

.macro _op_bit op, bitval, val
	.local @begin
@begin:
	.repeat .tcount ({val}), i
		.if .xmatch (.mid (i, 1, {val}), .)
			_op_tb op + (bitval * .right (.tcount ({val})-(i+1), {val})), .left (i, {val})
		.endif
	.endrepeat
	; TODO: report error during assembly rather than linking
	.assert (*-@begin) = 2, error, "unsupported addressing mode"
.endmacro

.define set1 _op_bit $02, $20,
.define clr1 _op_bit $12, $20,

;**** Branch

.macro _branch_offset instr, target
	.local @distance, @next
	@distance = (target) - @next
	instr
	.assert @distance >= -128 && @distance <= 127, error, "branch out of range"
	.byte <@distance
@next:
.endmacro

.macro _op_branch inst, target
	_branch_offset {.byte inst}, target
.endmacro

.macro bbs val, target
	_branch_offset {_op_bit $03, $20, val}, target
.endmacro

.macro bbc val, target
	_branch_offset {_op_bit $13, $20, val}, target
.endmacro

.macro dbnz val, target
	.if .xmatch ({val}, y)
		_op_branch $fe, target
	.else
		_op_branch {$6e, val}, (target)
	.endif
.endmacro

.macro cbne val, target
	.if .xmatch (.right (2, {val}), +x)
		_branch_offset {_op_tb $de, .left (.tcount ({val})-2, {val})}, target
	.else
		_branch_offset {_op_tb $2e, val}, target
	.endif
.endmacro

.define bpl _op_branch $10, ; target
.define bra _op_branch $2f, ; target
.define bmi _op_branch $30, ; target
.define bvc _op_branch $50, ; target
.define bvs _op_branch $70, ; target
.define bcc _op_branch $90, ; target
.define bcs _op_branch $B0, ; target
.define bne _op_branch $D0, ; target
.define beq _op_branch $f0, ; target

;**** OP !abs

.macro _op_abs op, val; ****
	.local @target ; evaluate * below before opcode for correct addr
	.if .xmatch (.left (1, {val}), !)
		@target = .right (.tcount ({val})-1, {val})
		_op_tw op, @target
	.elseif ::DEFAULT_ABS && (!.xmatch (.left (1, {val}), <))
		_op_warn_default_abs
		@target = val
		_op_tw op, @target
	.else
		.assert 0, error, "unsupported addressing mode"
	.endif
.endmacro

.define tset1 _op_abs $0E, ; abs
.define tclr1 _op_abs $4E, ; abs
.define call  _op_abs $3F, ; abs

.macro jmp_ val; ****
	.if .xmatch (.left (2, {val}), [!) && .xmatch (.right (3, {val}), +x])
		_op_tw $1f, .mid (2, .tcount ({val})-5, {val})
	.elseif ::DEFAULT_ABS && .xmatch (.left (1, {val}), [) && .xmatch (.right (3, {val}), +x])
		_op_warn_default_abs
		_op_tw $1f, .mid (1, .tcount ({val})-4, {val})
	.else
		_op_abs $5f, val
	.endif
.endmacro

;**** $1FFF.bit

.macro _op_mbit op, val
	.local @begin, @addr
@begin:
	.repeat .tcount ({val}), i
		.if .xmatch (.mid (i, 1, {val}), .)
			@addr = .left (i, {val})
			.assert 0 <= @addr && @addr <= $1FFF, error, "address exceeds 13 bits"
			_op_tw op, (.right (.tcount ({val})-(i+1), {val}))*$2000 + @addr
		.endif
	.endrepeat
	; TODO: report error during assembly rather than linking
	.assert (*-@begin) = 3, error, "unsupported addressing mode"
.endmacro

.macro _op_mbit_c op, carry, val
	.if .xmatch (carry, c)
		_op_mbit op, val
	.else
		.assert 0, error, "destination must be C"
	.endif
.endmacro

.macro _op_mbit_inv op, carry, val
	.if .xmatch (.left (1, {val}), /)
		_op_mbit_c op+$20, carry, .right (.tcount ({val})-1, {val})
	.else
		_op_mbit_c op, carry, val
	.endif
.endmacro

.define not1 _op_mbit     $EA, ; abs.bit
.define or1  _op_mbit_inv $0A, ; abs.bit
.define and1 _op_mbit_inv $4A, ; abs.bit
.define eor1 _op_mbit_inv $8A, ; abs.bit

.macro mov1 dest, src
	.if .xmatch ({src}, c)
		_op_mbit_c $CA, src, dest
	.else
		_op_mbit_c $AA, dest, src
	.endif
.endmacro

;**** OP dp

.macro _op_dp op, dp
	_op_tb op, (dp)
.endmacro

.define decw _op_dp $1a, ; dp
.define incw _op_dp $3a, ; dp

;**** OP reg

.macro _op_one_reg op, reg, err, val
	.if .xmatch ({val}, reg)
		.byte op
	.else
		.assert 0, error, err
	.endif
.endmacro

.macro _op_w op, reg, val
	_op_one_reg op, ya, "only supports ya", reg
	.byte val
.endmacro

.define cmpw _op_w $5A, ; dp
.define addw _op_w $7A, ; dp
.define subw _op_w $9A, ; dp

.macro movw dest, src
	.if .xmatch ({src}, ya)
		_op_w $DA, src, dest
	.else
		_op_w $BA, dest, src
	.endif
.endmacro

.macro div dest, src
	.if .xmatch ({dest}, ya) && .xmatch ({src}, x)
		.byte $9e
	.else
		.assert 0, error, "only supports ya, x"
	.endif
.endmacro

.define xcn _op_one_reg $9f, a, "only supports a",
.define das _op_one_reg $BE, a, "only supports a",
.define daa _op_one_reg $DF, a, "only supports a",
.define mul _op_one_reg $CF, ya, "only supports ya",

;**** Unique

.macro tcall val
	.assert 0 <= (val) && (val) <= 15, error, "invalid value"
	.byte (val)*$10 + $01
.endmacro

.macro pcall val
	_op_tb $4f, (val)
.endmacro

;**** Implied

.macro _op_implied op
	.byte op
.endmacro

.define nop		_op_implied $00
.define brk_	_op_implied $0f
.define clrp	_op_implied $20
.define setp	_op_implied $40
.define clrc	_op_implied $60
.define ret		_op_implied $6f
.define reti	_op_implied $7f
.define setc	_op_implied $80
.define ei		_op_implied $A0
.define di		_op_implied $C0
.define clrv	_op_implied $E0
.define notc	_op_implied $ED
.define sleep	_op_implied $ef
.define stop	_op_implied $ff

.ifndef CASPC_65XX
	.define and and_
	.define eor eor_
	.define adc adc_
	.define sbc sbc_
	.define cmp cmp_
	
	.define dec dec_
	.define inc inc_
	.define lsr lsr_
	.define asl asl_
	.define rol rol_
	.define ror ror_
	
	.define jmp jmp_
	.define brk brk_
.endif

;.endif
