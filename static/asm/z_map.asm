include z_sub
include z_math
include z_tnu

defsub map.adr
 ; hl=chipx
 ; de=chipy
 rept 5
  slde 0
endm
 add hl,de
 ld de,1800h
 add hl,de
endsub map.adr

defsub map.set.a
 ;  a=data
 call map.adr
 call 4dh
endsub map.set.a

defsub map.get.a
 call map.adr
 call 4ah
endsub map.get.a

defsub map.adrat.a
 ; hl=spr_x
 ; de=spr_y
 spr.unscale 0
 srl a
 srl a
 srl a
 push af
 ex de,hl
 spr.unscale 0
 srl a
 srl a
 srl a
 ld d,0
 ld e,a
 pop hl
 ld l,h
 ld h,0
 call map.adr
endsub map.adrat.a

defsub map.getat.a
 call map.adrat.a
 call 4ah
endsub map.getat.a

defsub map.setat.a
 ; a=data
 push af
 call map.adrat.a
 pop af
 call 4dh
endsub map.setat.a

defsub locate
 ; hl=chipx
 ; de=chipy
 call map.adr
 ld (cursor),hl
endsub locate
