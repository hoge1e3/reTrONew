include z_mem
include z_th
include z_assert

;a2 a1  oldpc oldix lcl1 lcl2
argidx equ 2
getarg macro n
 ld l,(ix+argidx+&n*2)
 ld h,(ix+argidx+&n*2+1)
endm
setarg macro n
 ld (ix+argidx+&n*2),l
 ld (ix+argidx+&n*2+1),h
endm

setlcl macro n
 ld (ix-(&n*2-1)),h
 ld (ix-&n*2),l
endm

getlcl macro n
 ld h,(ix-(&n*2-1))
 ld l,(ix-&n*2)
endm

addarg macro
 push hl
; hl=arg  stktp=af
;ex (sp),hl
;ld a,h
;push af
endm



pusharg macro n
 getarg &n
 push HL
endm

pushlcl macro n
 getlcl &n
 push HL
endm


pops macro n
 rept &n*2
  inc sp
endm
endm



pushthis macro xx
 getthis &xx
 push af
endm
popthis macro xx
 popa &xx
 ld (this),a
endm


invoketg.a macro fld
; pushthis before arg push
; hl=target
 ld a,h
 ld (this),a
 getfld &fld
 callhl
; pops args
; popthis after
endm

invoke macro fld
 getfld &fld
 callhl
; pops args
 getthis 0
endm

getfld macro n
#local
 ld (ad-1),a
 ld hl,(&n)
 ad:
#endlocal
endm

setfld macro n
#local
 ld (ad-1),a
 ld (&n),hl
 ad:
#endlocal
endm

getfldtg macro n
;hl=tg
 ld l,&n
 peekw hl,hl
endm

setfldtg macro n
; stk=val hl=tg
 ld l,&n
 pop de
 pokew hl,de
endm

getfldtg_botu macro n
; hl=target
 ld d,h
 ld e,&n
 peekw HL,de
endm

tgconst macro n
 ld (&n-1),a
endm
tgconst.g macro r16,n,fld
 ld &r16,(&fld)
 &n:
endm
tgconst.s macro n,fld,r16
 ld (&fld),&r16
 &n:
endm


curth2this macro
 ld a,(th.cur+1)
 ld (this),a
endm
getthis macro xx
 ld a,(this)
endm

new macro Class,flds,st,en
 th.new.range th.start+&st*th.size, th.start+&en*th.size
 pushi &flds, bc
 pushi &Class, bc
 call o.new
endm

defsub o.new
#local
 ; {val .f} n &initbl retad
 pop hl;retad
 ld (retad-2),hl
 ; set initbl for th.with
 pop hl;&initbl
 th.with.setdst hl
 ; save this
 ld (svthis-1),a
 ; allocate thread
 call th.new
 jr nc, allocfail
 push hl; thread address
 call th.with.s; call &initbl
 pop hl; thread address
 ld a,h; set this as thread
 ; init fields
 pop bc; n of {val .f}
 inc c
 loop:
  dec c
  jr z,lpend
  pop hl; .f
  ld h,a
  ld (w-2),hl
  pop hl; val
  ld (w),hl
  w:
 jr loop
 lpend:
 ; return h as this
 ld h,a
 finally:
  ;restore a before call o.new
  ld a,0
  svthis:
  ;return
  jp 0
  retad:
 allocfail:
  ; drop {val .f}
  pop bc; n of {val .f}
  ld b,c
  inc c
  lp2:
   dec c
   jr z, lp2end
   pop hl
   pop hl
  jr lp2
  lp2end:
  ld hl,null;  todo null
  jr finally
#endlocal
endsub o.new

new.arg.i macro n,v
 ld hl,&v
 new.arg &n
endm
new.arg macro n
 push hl
 pushi &n,bc
endm

o.assert.eq macro fld,v
#local
 assert.do aa
  getfld &fld
  assert.eq &v
  ret
 aa:
#endlocal
endm

this:
db 0

fld.def macro n
 &n equ fldidx
 fldidx:defl fldidx-2
endm
class macro Class,super
 unreach "c"
 marker.b 0
 dw &super
 fldidx:defl fld.top
 &Class:
  fld .class,&Class
endm
fld.bottom macro Class
 if 0
  if bottomOf&Class ne fldidx
   .error bottom ne fldidx
  endif
 else
 bottomOf&Class:defl fldidx
 endif
endm
fld macro n,v
 if 0
  if &n ne fldidx
   .error &n ne fldidx
  else
   fldidx:defl fldidx-2
  endif
 else
  fld.def &n
 endif
 pushi &v,bc
endm
unuse macro xx
 fldidx:defl fldidx-2
 pushi 0,bc
endm
meth macro Class,n
 fld .&n, &Class.&n
endm
met2 macro Class,n
 fld &n, &Class&n
endm

class Object,null
 fld .main,null
 fld.bottom Object
 marker.e Object


defsub o.boot
 curth2this
 invoke .main
endsub o.boot


yield macro
 pushthis 0
 push ix
 call th.yield
 pop ix
 popthis 0
endm

def.noent: defl 0
def macro n,args,lcls
head &n
 def.args:defl &args
 def.locals:defl &lcls
 if def.noent eq 0
  enter &lcls
 endif
endm
enter macro locals
 ;  arg2 arg1 retad oldix lcl1 lcl2
 push ix
 ld ix,0
 add ix,sp
 rept &locals
  push HL
endm
endm
enddef macro n
 if def.noent eq 0
  exit def.args
 else
  ret
 endif
 marker.e &n
endm
exit macro n
 ld sp,ix
 pop ix
 if &n!=0
  exx
  pop bc
  pops &n
  push bc
  exx
 endif
 ret
endm


defsub isobj.a
 ;hl=obj?
 ;cy=true
 ld a,h
 cp th.start.h
 jr c,notobj
 cp th.end.h
 jr nc,notobj
 scf
 ret
 notobj:
 and a
endsub isobj.a

defsub instanceof
 ; a=this de=Class
 ; z: true
 getfld .class
 jp is.subclass.a
endsub instanceof

defsub get.superclass
 ; hl=Class
 dec hl
 dec hl
 peekw hl,hl
endsub get.superclass

defsub is.subclass.a
#local
 ; hl=Subclass
 ; de=Superclass
 ; z:true
 top:
 cpde.a 0
 ret z
 call get.superclass
 push de
 ld de,null
 cpde.a 0
 pop de
 jr nz,top
 cpde.a 0
#endlocal
endsub is.subclass.a
