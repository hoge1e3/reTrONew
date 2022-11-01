include z_const
include z_th
include z_mem
include z_oop
include z_sub

class Sprite,Object
 fld .main, 0
 fld.bottom Object
 fld .x, 100
 fld .y, 100
 fld .p, 0
 fld .c, 2
 fld.bottom Sprite
 marker.e Sprite

outwrt macro n
  out (98h),a
endm


spr.unscale macro n
 ; HL -> A
 rept spr.scale
  srlhl 0
endm
 LD A,L
endm

defsub spr.puts_old
#local
 ld hl, 1b00h
 call SETWRT
 th.for.a sprl, 0, th.count
  ld a,h
  tgconst t1
  tgconst t2
  tgconst t3
  tgconst t4

  tgconst.g hl,t1,.y
  spr.unscale 0
  sub 8
  outwrt 0

  tgconst.g hl,t2,.x
  spr.unscale 0
  sub 8
  outwrt 0

  tgconst.g a,t3,.p
  sla a
  sla a
  outwrt 0

  tgconst.g a,t4,.c
  outwrt 0
  continue
 sprl:
#endlocal
endsub spr.puts

defsub spr.puts
#local
 call spr.putslp
 ld a,(spr.puts.s)
 cp 1
 jr nz,rdir
  ld a, th.count-1
  ld (spr.puts.s),a
  ret
 rdir:
 ld a,1
 ld (spr.puts.s),a

 ld a,(spr.puts.b)
 inc a
 cp th.end.h
 jr c,rsta
  sub th.count
 rsta:
 ld (spr.puts.b),a
#endlocal
endsub spr.puts_new

defwork spr.puts.b
 db th.start.h
endwork spr.puts.b

defwork spr.puts.s
 db 1
endwork spr.puts.s

defsub spr.putslp
#local
 ld hl, 1b00h
 call SETWRT
 ld a, (spr.puts.b)
 ld b, th.count
 sprl:
  call spr.put1
  ld c,a
  ld a,(spr.puts.s)
  add a,c
  cp th.end.h
  jr c,nrst
   sub th.count
  nrst:
 djnz sprl
 ld a,208
 outwrt 0
#endlocal
endsub spr.putslp


defsub spr.put1
; a: th.id
#local
  push af

  tgconst t1
  tgconst t2
  tgconst t3
  tgconst t4
  ld h,a
  call th.isblank.a
  jr z,nothave

  tgconst.g hl,t1,.y
  spr.unscale 0
  sub 8
  outwrt 0

  tgconst.g hl,t2,.x
  spr.unscale 0
  sub 8
  outwrt 0

  tgconst.g a,t3,.p
  sla a
  sla a
  outwrt 0

  tgconst.g a,t4,.c
  outwrt 0
  nothave:
  pop af
#endlocal
endsub spr.put1

