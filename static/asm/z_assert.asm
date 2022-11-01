include z_mem
include z_math
include z_debug

a.reg.trc:
dw 0
a.reg.adr:
dw 0
a.reg.min:
dw 0
a.reg.val:
dw 0
a.reg.max:
dw 0
a.regi macro n,v
 push hl
 ld hl,&v
 ld (a.reg.&n),hl
 pop hl
endm
a.regr macro n,v
 ld (a.reg.&n),&v
endm

a.dummy macro
#local
#endlocal
endm


assert.eq macro o
 storelastpc
 pushall
 a.regi val, &o
 ld de,(a.reg.val)
 ld(a.reg.val),hl
 ld(a.reg.min),de
 ld(a.reg.max),de
 cpde
 jp nz,assert.fail
 popall
endm

assert.do macro nx
 storelastpc
 pushall
 call to
 popall
 jr &nx
 to:
endm

storelastpc macro
 push hl
 call getpc
 ld (lastpc),hl
 pop hl
endm
lastpc:
 dw 0

getpc:
 pop hl
 push hl
 ret

assert.fail:
 ld hl,0deadh
 show hl
 showm a.reg.trc
 showm a.reg.min
 showm a.reg.val
 showm a.reg.max
 showm a.reg.adr
 showm lastpc
 call freeze
assert.meqw macro ad,val
 a.regi adr,&ad
 push hl
 ld hl,(&ad)
 assert.eq &val
 pop hl
endm
