;https://www.msx.org/wiki/VDP_Status_Registers
;st 0 bit 7
;read 1

;https://www.msx.org/wiki/VDP_Mode_Registers
;ctrl 1 bit 5 set 0
include z_const
include z_ctrl
;include th

defsub susint
 ld a,(RG1SAV)
 res 5,a
 ld b,A
 ld c,1
 jp WRTVDP
endsub susint
defsub rstint
 ld a,(RG1SAV)
 set 5,a
 ld b,A
 ld c,1
 jp WRTVDP
endsub rstint
defsub inted
 call RDVDP
 bit 7,a
endsub inted
doint:
 call inted
 jr z, norst
 ld hl,timecnt
 inc (hl)
 call h.tntimi
 norst:
 ;call rstint
 ret
h.tntimi:
 call int.nop
 ret
defsub setInterval
 ld (h.tntimi+1), hl
endsub setInterval
defsub clearInterval
 ld hl, int.nop
 jp setInterval
endsub clearInterval

defsub int.nop
ret
endsub int.nop

timecnt:
db 0
vdptest macro
#local
stk1:
 ds 256,35
stk2:
 ds 256,42
stk3:

vl:
 call susint
 ld sp,stk2
 ld hl,stk1
 ld de,1800h
 ld bc,256
 call LDIRVM


 ld sp,stk3
 call doint
 ld hl,stk2
 ld de,1900h
 ld bc,256
 call LDIRVM
 jp vl
#endlocal
endm

screen1:
 ld a,1
 call CHGMOD
 ld a,(RG1SAV)
 set 1,a
 ld b,A
 ld c,1
 call WRTVDP
 ret

defsub screen2
 ;ld a,2
 ;call CHGMOD
 call 72h
 ld a,(RG1SAV)
 set 1,a
 ld b,A
 ld c,1
 call WRTVDP
endsub screen2
