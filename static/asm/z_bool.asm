include z_math
include z_ctrl
true equ -1
false equ 0

ret.true.if macro flg
#local
 iff &flg ,tru
  ld hl,true
  ret
 tru:
#endlocal
endm

ret.false.if macro flg
#local
 iff &flg ,tru
  ret.false
 tru:
#endlocal
endm

ret.false macro
 ld hl,false
 ret
endm

hleqde:
 subhl de
 ret.true.if z
 ret.false

hlnede:
 subhl de
 ret.true.if nz
 ret.false

hlgtde:
 subhl de
 ret.false.if z
 bit 7,h
 ret.true.if z
 ret.false

hlltde:
 subhl de
 bit 7,h
 ret.true.if nz
 ret.false

hlgede:
 subhl de
 ret.true.if z
 bit 7,h
 ret.true.if z
 ret.false

hllede:
 subhl de
 ret.true.if z
 bit 7,h
 ret.true.if nz
 ret.false

ziffalse:
#local
 ld (resa-1),a
 call ziffalse.a
 ld A,0
 resa:
 ret
ziffalse.a:
 ld a,0
 cp h
 ret nz
 cp l
 ret
#endlocal

jpf macro to
 call ziffalse
 jp z,&to
endm

andand macro fls
 jpf &fls
endm
oror macro tru
 call ziffalse
 jp nz,&tru
endm

bitand.a macro xx
 ; hl&=de
 ld a,h
 and d
 ld h,a
 ld a,l
 and e
 ld l,a
endm

bitor.a macro xx
 ; hl|=de
 ld a,h
 or d
 ld h,a
 ld a,l
 or e
 ld l,a
endm


flagtobool macro fl
#local
 jr &fl, yes
 ld hl,false
 jr skp
 yes:
 ld hl,true
 skp:
#endlocal
endm

defsub nothl
#local
 jpf fls
 ld hl, false
 ret
 fls:
 ld hl, true
#endlocal
endsub nothl
