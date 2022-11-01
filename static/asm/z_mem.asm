include z_const
;
rdslt:
 ex de,hl
 rept 5
 srl d;page*2
endm
 CALL RSLREG
 ld e,d
 rdslt1:
  RRCA
  dec e
  jp nz,rdslt1
 AND    00000011B
 LD c,A;000000Pr
 LD B,0
 LD HL,EXPTBL
 ADD HL,BC
 LD c,A;000000Pr
 LD A,(HL)
 AND 80H;expand flag
 OR c
 LD c,A;e00000Pr
 rept 4;const
 INC HL
endm
 LD A,(HL);exp reg
 ld e,d
 rdslt2:
  srl a
  dec e
  jp nz,rdslt2
;000000Ex
 sla a
 sla a
 ;    0000Ex00
 and  00001100b
 OR c;e000ExPr
 ret
memini:
 CALL RSLREG
 rept 4
  RRCA
endm
 AND    00000011B
 LD c,A;000000Pr
 LD B,0
 LD HL,EXPTBL
 ADD HL,BC
 LD c,A;000000Pr
 LD A,(HL)
 AND 80H;expand flag
 OR c
 LD c,A;e00000Pr
 rept 4;const
 INC HL
endm
 LD A,(HL);exp reg
 rept 4; page*2
 srl a
endm;000000Ex
 sla a
 sla a
 ;    0000Ex00
 and  00001100b
 OR c;e000ExPr
 LD Hl,04000H
 jp ENASLT

peekw macro regv,regm
#local
  ld (w-2),&regm
  ld &regv,(0)
  w:
#endlocal
endm

pokew macro regm,regv
#local
  ld (w-2),&regm
  ld (0),&regv
  w:
#endlocal
endm
movw macro dst,src
 push hl
 ld hl,&src
 ld &dst,hl
 pop hl
endm

popa macro xx
  ex (sp),hl
  ld a,h
  pop HL
endm

pushall macro
 push af
 push bc
 push de
 push hl
endm
popall macro
 pop hl
 pop de
 pop bc
 pop af
endm


pushi macro n,rp
 ld &rp,&n
 push &rp
endm
const macro n,reg
 ld (&n-2),&reg
endm
ldconst macro reg,n
 ld &reg,0
 &n:
endm
peekconst macro reg,n
 ld &reg,(0)
 &n:
endm
