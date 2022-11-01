
include z_spr
include z_bool
include z_key

;.onUpdate equ .c-2
;.update equ .onUpdate-2
;.screenOut equ .update-2
;.die equ .screenOut-2
;.updateEx equ .die-2

end.const macro n
 pushi RActor.wait,bc
 pushi o.boot,bc
 th.with.ret 0
 marker.e &n
endm

RActor.noovr macro Class
 meth &Class,main
 fld.bottom Object
 fld .x, 0
 fld .y, -1024
 fld .p, 0
 fld .c, 3
 fld.bottom Sprite
 meth RActor,onUpdate
 meth RActor,update
 meth RActor,screenOut
 meth RActor,die
 meth RActor,updateEx
 meth RActor,crashTo
 fld.bottom RActor
endm

class RActor,Sprite
 RActor.noovr RActor
 end.const RActor
RActor.main:
 enter 0
 exit 0
RActor.update:
 invoke .onUpdate
 yield
 ret
RActor.onUpdate:
 ret
RActor.screenOut:
#local
 getfld .x
 bit 1,h
 jr nz, true
 getfld .y
 ld de,192*2
 cpde.a 0
 getthis 0
 jr nc,true
 ld hl,0
 xor a
 ret
 true:
 ld hl,1
 scf
 ret
#endlocal
RActor.wait:
#local
 lbl:
 invoke .update
 jr lbl
#endlocal
def RActor.die,0,0
 ld h,a
 push af
 call th.kill.a
 pop af
enddef RActor.die

def RActor.updateEx,1,0
#local
; enter 0
 getarg 1
 ld b,h
 ld c,l
 reptbc n
  invoke .update
  continue
 n:
#endlocal
enddef RActor.updateEx

crashTo.size equ 8<<spr.scale


defsub crashTo.setXY
 getfld .x
 const cr.gx, hl
 getfld .y
 const cr.gy, hl
endsub crashTo.setXY


def RActor.crashTo,1,0
#local
 call crashTo.setXY
 getarg 1
 ;const cr.class,hl
 call isobj.a
 jr c, cr1
  unreach "c"
  ; "Cannot call target.crashTo(Class) "
 cr1:
  getthis 0
  call crashTo1
  flagtobool c
#endlocal
enddef RActor.crashTo

crashToClass macro Class,st,en
#local
 ; a=this
 call crashTo.setXY
 foreach.a &Class,&st,&en,nx
  call crashTo1
  break.if c
  continue
 nx:
 getthis 0
 jr c, found
  ld hl,null
 found:
#endlocal
endm

foreach.a macro Class,st,en,nxt
 th.for.a &nxt, &st, &en
  all.skip &Class
endm


all.skip.blank.self macro xx
 ; skip blank
  ; TODO th.ofs.stp
  call th.isblank.a
  continue.if z
  ; skip hl==this
  getthis 0
  cp h
  continue.if z
endm
all.skip.isnot macro Class
  ; skip object not instance of *Class*
  push hl
  ld a,h
  ld de,&Class
  call instanceof
  getthis 0
  pop hl
  continue.if nz
endm
all.skip macro Class
 all.skip.blank.self 0
 all.skip.isnot &Class
endm

defsub crashTo1
 ; call crashTo.setXY before
 ;hl=tg
 ;cy:true
 ;hl is used
 push af
 ld a,h
 tgconst cr.t1
 tgconst cr.t2
 pop af
 tgconst.g hl, cr.t1, .x
 ldconst bc, cr.gx
 subhl bc
 call abs
 ld bc,crashTo.size
 subhl bc
 ret nc

 tgconst.g hl, cr.t2, .y
 ldconst bc, cr.gy
 subhl bc
 call abs
 ld bc,crashTo.size
 subhl bc
endsub crashTo1




tnu.run macro Main,st,en
 unreach.new.init 0
 ld sp,sp.ini
 call screen1
 call screen2

 showsp
 showlb endusr
 call spr.inipat
 call bg.inipat

 ld hl,th.start
 ld (hl),0cdh
 ld de,th.start+1
 ld bc,th.size*th.count-1
 ldir

 call th.init
 ;call mus.ini
 new &Main, 0,&st ,&en
 movw (h.thlop),spr.puts
 movw (h.thent),keyall
 jp th.loop
endm

defsub all.die
#local
 pushthis 0
 th.for.a nxt, 0, th.count
  call th.isblank.a
  continue.if z
  invoketg.a .die
  continue
 nxt:
 popthis 0
#endlocal
endsub all.die

loadPage macro Class,args,st,en
call clearInterval
call all.die
new &Class,&args,&st,&en
endm

