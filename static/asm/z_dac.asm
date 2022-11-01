
;jp main
include z_const
include z_ctrl
include z_math
include z_debug
include z_sub
include z_mem
include z_th


DECSUB equ 268CH;DAC ← DAC-ARG
DECADD equ 269AH;DAC ← DAC+ARG
DECNRM equ 26FAH;DAC を正規化する (*1)
DECROU equ 273CH;DAC を四捨五入する
DECMUL equ 27E6H;DAC ← DAC*DAC
DECDIV equ 289FH;DAC ← DAC/DAC
MAF equ 2C4DH;ARG ← DAC
MAM equ 2C50H;ARG ← [HL]
MOV8DH equ 2C53H;[DE] ← [HL]
MFA equ 2C59H;DAC ← ARG
MFM equ 2C5CH;[HL] ← DAC
MMF equ 2C67H;[HL] ← DAC
MOV8HD equ 2C6AH;[HL] ← [DE]
XTF equ 2C6FH;[SP] ←→ DAC
PHA equ 2CC7H;ARG → [SP]
PHF equ 2CCCH;DAC → [SP]
PPA equ 2CDCH;[SP] → ARG
PPF equ 2CE1H;[SP] → DAC
PUSHF equ 2EB1H;DAC → [SP]
MOVFM equ 2EBEH;DAC ← [HL]
MOVFR equ 2EC1H;DAC ← (CBED)
MOVRF equ 2ECCH;(CBED) ← DAC
MOVRMI equ 2ED6H;(CBDE) ← [HL]
MOVRM equ 2EDFH;(BCDE) ← [HL]
MOVMF equ 2EE8H;[HL] ← DAC
MOVE equ 2EEBH;[HL] ← [DE]
VMOVAM equ 2EEFH;ARG ← [HL]
MOVVFM equ 2EF2H;[DE] ← [HL]
VMOVE equ 2EF3H;[HL] ← [DE]
VMOVFA equ 2F05H;DAC ← ARG
VMOVFM equ 2F08H;DAC ← [HL]
VMOVAF equ 2F0DH;ARG ← DAC
VMOVMF equ 2F10H;[HL] ← DAC

VALTYP equ 0F663H;1
DAC equ 0F7F6H;16
ARG equ 0F847H;16
FOUT equ 3425H
PUFOUT equ 3426H

defsub int2dac
 push af
 ld a,2
 ld (VALTYP),a
 ld (DAC+2),HL
 pop af
endsub int2dac
;===your code

#local
main:
ld hl,12345
call int2dac
ld hl,str
call FOUT

ld b,10
reptb nxt
 ld a,(hl)
 cp 0
 break.if z
 call wrt
 inc hl
 continue
nxt:
ret
str:
#endlocal
defsub myfout.a
#local
 xor a
 ld (strbuf.i+1),a
 bit 7,h
 jr z, posi
nega:
 neghl.sv 0
 ld a, '-'
 ld (strbuf.i),a
 jr loop
posi:
 ld a, ' '
 ld (strbuf.i),a
loop:
 ld de,10
 call udiv.a
 ;show bc
 ; bc=q hl=r
 ld a,l
 push bc
 call unshift.sbuf
 pop hl
 add a,'0'
 ld (strbuf.i+1),a
 ld a,h
 or l
 jr nz, loop
 
#endlocal
endsub myfout.a

defsub printi.a
 ;call int2dac
 call myfout.a
 ld hl,strbuf.i
 ;ld a,128
 ;ld b,0
 ;call FOUT
 call print.a
endsub printi.a
; 0123456
; s12345
;       
defsub unshift.sbuf
 ld hl, strbuf.i+5
 ld de, strbuf.i+6
 ld bc, 5
 lddr
endsub unshift.sbuf

defwork strbuf.i
ds 16, 0
endwork strbuf.i
