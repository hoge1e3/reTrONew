include z_const
freeze:
halt
jr freeze

for macro lbend
 ; uses a
 ; c: breaked
#local
  lb:
  call dcompr; uses a
  jp nc,&lbend
  push HL
  push de
  push bc
  call s
  pop bc
  pop de
  pop HL
  jp c,&lbend
  add HL,bc
  jr lb
  s:
#endlocal
endm

repti macro n,lbend
#local
  push bc
  ld b,&n
  lb:
  push bc
  call s
  pop bc
  jr c,lbend2
  djnz lb
  lbend2:
  pop bc
  jp &lbend
  s:
#endlocal
endm


reptb macro lbend
#local
 inc b
 djnz lb
 jp &lbend
 lb:
  push bc
  call s
  pop bc
  jp c,&lbend
 djnz lb
 jp &lbend
 s:
#endlocal
endm



callsva macro pp
#local
 ld (sva-1),a
 call &pp
 ld a,0
 sva:
#endlocal
endm
bcis0:
 callsva bcis0.a
 ret
bcis0.a:
 ld a,b
 and a
 ret nz
 ld a,c
 and a
 ret

reptbc macro lbend
#local
 call bcis0
 jp z,&lbend
 lb:
  push bc
  call s
  pop bc
  jp c,&lbend
  dec bc
  call bcis0
 jr nz, lb
 jp &lbend
 s:
#endlocal
endm


iff.nz equ 0
iff.z  equ 1
iff.nc equ 2
iff.c  equ 3

iff macro cnd,to
 if iff.&cnd eq iff.nz
  jr z,&to
 endif
 if iff.&cnd eq iff.z
  jr nz,&to
 endif
 if iff.&cnd eq iff.nc
  jr c,&to
 endif
 if iff.&cnd eq iff.c
  jr nc,&to
 endif
 ;jr cnd, skip
 ;jr to
 ;skip:
endm

break macro
 scf
 ret
endm
break.if macro cnd
#local
 iff &cnd ,jj
  break
 jj:
#endlocal
endm

continue macro
 or a
 ret
endm
continue.if macro cnd
#local
 iff &cnd,jj
  continue
 jj:
#endlocal
endm


djnzr macro reg,j
 dec &reg
 jr nz,&j
endm

callhl macro
#local
 ld (LCD-2),HL
 call LCD
 LCD:
#endlocal
endm

stride macro lim,to
 if lo($)<&lim
  exitm
 endif
 ds 256+&to-lo($),0cdh
endm


unreach macro mesg
 ;trace mesg
 call h.unreach
endm
head macro lb
 unreach &lb
 marker.b &lb
 &lb:
endm

defsub macro n
 head &n
endm
endsub macro n
 ret
 marker.e &n
endm
defwork macro n
 head &n
endm
endwork macro n
 marker.e &n
endm

defsub h.unreach
 ld a,'u'
 ld hl,1800h
 call 4dh
 dw 0x18,0xfe
endsub h.unreach