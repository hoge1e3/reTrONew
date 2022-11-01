include z_math
include z_ctrl

;debug
show macro reg
 ld (hexval),&reg
 call showhex
endm
showm macro ad
 push hl
 ld HL,(&ad)
 show HL
 pop HL
endm
showlb macro lb
 push hl
 ld hl,&lb
 ld (hexval),hl
 call showhex
 pop hl
endm

trace macro v
 push af
 ld a,&v
 ld (trad),a
 pop af
 call trace.s
endm

defwork hexval
 dw 0
endwork hexval

defsub showhex
#local
 push af
 push bc
 push HL
 ld hl,(hexval)
 ld b,4
 loop:
  xor a
  rept 4
   slhl 0
   rla
endm
  call showhex1
 djnz loop
 ld a,32
 call wrt
 pop HL
 pop bc
 pop af
 ret
#endlocal
endsub showhex

defsub showhex1
#local
 cp 10
 jp nc, els
 add a,48
 jp wrt
 els:
 add a,65-10
 jp wrt
#endlocal
endsub showhex1


defsub abort
 call wrt
 db 018h,0feh
endsub abort

defsub trace.s
 push af
 push hl
 ld a,(trad)
 ld hl,1ae0h
 call wrt
 call 4dh
 inc a
 ld (trad),a
 ld a,32
 call wrt
 pop hl
 pop af
endsub trace.s
defwork trad
 db 65
endwork trace.s

defsub showz
 push af
 jr z,showz.s
 ld a,'N'
 call wrt
 showz.s:
 ld a,'z'
 call wrt
 ld a,32
 call wrt
 pop af
endsub showz


defsub showc
 push af
 jr c,showc.s
 ld a,'N'
 call wrt
 showc.s:
 ld a,'c'
 call wrt
 ld a,32
 call wrt
 pop af
endsub showc

defwork cursor
 dw 1800h
endwork cursor

defsub wrt
#local
 push hl
 push af
 ld hl,(cursor)
 call 4dh
 inc hl
 ld a,h
 cp 1bh
 jr c,sk
  ld h,18h
 sk:
 ld (cursor),hl
 pop af
 pop hl
 ret
#endlocal
endsub wrt

defsub print.a
 ld a,(hl)
 cp 0
 ret z
 call wrt
 inc hl
 jr print.a
endsub print.a

defsub unreach.new
 ld hl,unreach.m
 call print.a
 pop hl
 show hl
 db 18h,0feh
 unreach.m:
 db "unreach"
 db 0
endsub unreach.new
unreach.new.init macro xx
ld hl,h.unreach
ld (hl),0c3h
ld hl,unreach.new
ld (h.unreach+1),hl
endm