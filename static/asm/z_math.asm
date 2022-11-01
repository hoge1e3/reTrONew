include z_ctrl

;16bit shifts
slhl macro xx
 sla l
 rl h
endm
srahl macro xx
 sra h
 rr l
endm
srlhl macro xx
 srl h
 rr l
endm
slde macro xx
 sla e
 rl d
endm
srade macro xx
 sra d
 rr e
endm
srlde macro xx
 srl d
 rr e
endm
slbc macro xx
 sla c
 rl b
endm
srabc macro xx
 sra b
 rr c
endm
srlbc macro xx
 srl b
 rr c
endm


; for xrnd
sldehl macro n
#local
  ld b,&n
 loop:
  sla d
  rl e
  rl h
  rl l
  djnz loop
#endlocal
endm
srdehl macro n
#local
 ld b,&n
 loop:
  srl l
  rr h
  rr e
  rr d
 djnz loop
#endlocal
endm

xorrm macro re,me
  ld A,(&me)
  xor &re
  ld (&me),a
endm

subhl macro rp
 and a
 sbc hl,&rp
endm

cpde.a macro n
 rst dcompr
endm


defsub xrnd.a
#local
 ; s[0] ^= s[0] << 13
 call rdhlde
 sldehl 13
 call wrtxor
 ; s[0] ^= s[0] >> 17
 call rdhlde
 srdehl 17
 call wrtxor
 ; s[0] ^= s[0] << 5;
 call rdhlde
 sldehl 5
 call wrtxor
 ret

 rdhlde:
  ld hl,1234
 rhl:
  ld de,5678
 rde:
  ret

 wrtxor:
  xorrm h,rhl-1
  xorrm l,rhl-2
  xorrm d,rde-1
  xorrm e,rde-2
  ret
#endlocal
endsub xrnd.a


defsub rnd
 push af
 call rnd.a
 pop af
endsub rnd

defsub rnd.a
 ex de,hl
 ld hl,07fffh
 call udiv.a
 inc bc
 push bc
 call xrnd.a
 res 7,h
 pop de
 call udiv.a
 ld h,b
 ld l,c
endsub rnd.a

defsub abs
 bit 7,h
 ret z
neghl:
 ld de,0
 ex de,hl
 subhl de
 ret
endsub abs

defsub clamp
#local
 ; hl value
 ; bc min
 ; de max
 push hl
 subhl bc
 bit 7,h
 pop hl
 jr z, notmin
  ld h,b
  ld l,c
  ret
 notmin:
 push hl
 subhl de
 bit 7,h
 pop hl
 ret nz
 ex de,hl
 ret
#endlocal
endsub clamp

defsub mul.a
 ; hl*=de
#local
 ld b,h
 ld c,l
 
 ld hl,0
loop:
 srlbc 0
 jr nc,notad
 add hl,de
notad:
 slde 0
 ld a,b
 or c
 jr nz,loop
 
#endlocal
endsub mul.a

defsub log.a
#local
 push hl
 ld a,16
loop:
 dec A
 slhl 0
 jr nc,loop
 pop hl
#endlocal
endsub log.a

neghl.sv macro xx
 push de
 call neghl
 pop de
endm
negde.sv macro xx
 ex de,hl
 neghl.sv 0
 ex de,hl
endm
defsub div.a
; hl/=de with signed
; de=remainder
#local
 ld a,0
 ; hl=abs(hl), 
 ; a++ if hl<0
 bit 7,h
 jr z, hposi
 xor 3
 neghl.sv 0
hposi:
 ; de=abs(de), 
 ; a++ if de<0
 bit 7,d
 jr z, dposi
 xor 1
 negde.sv 0
dposi:
 push af
 call udiv.a
 ex de,hl; remainder
 ld h,b
 ld l,c
 pop af
 bit 0,a
 jp z, nnq
; negate quotient
 neghl.sv 0
nnq:
 bit 1,a
 jp z, nnr
; negate remainder
 negde.sv 0
nnr:
 
#endlocal
endsub div.a

defsub udiv.a
 ; bc=hl/de
 ; hl=hl%de
#local
 ld a,d
 or e
 ret z; /0 error
 
 ld b,0
bitc.lp:
 slde 0
 inc b
 rst dcompr
 jr nc, bitc.lp
bitc.br:
 srlde 0
 ld a,b
 ld bc,0
 ;ld (hexval),de
 ;call showhex
put.lp:
 subhl de
 bit 7,h
 jr z, put1
put0:
 add hl,de
 and a
 jr puten
put1:
 and a
 scf  
puten:
 rl c
 rl b
 srlde 0
 dec a
 jr nz, put.lp 
put.br:

#endlocal
endsub udiv.a

