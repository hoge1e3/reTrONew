
;wrt equ 0a2h
dcompr equ 0020H

sp.ini.h  equ 0dch
sp.ini    equ sp.ini.h*256
stksize.h equ 2
stksize   equ stksize.h*256

th.size.h   equ 1
th.size     equ th.size.h*256
th.count    equ 20
th.end.h    equ sp.ini.h-stksize.h
th.end      equ th.end.h*256
th.start.h  equ th.end.h-th.count*th.size.h
th.start    equ th.start.h*256


th.bottom equ 0

spr.scale equ 1
spr.xmax equ 256<<spr.scale
spr.ymax equ 192<<spr.scale

ENASLT EQU 0024H
RSLREG EQU 0138H
EXPTBL EQU 0FCC1H
SETWRT equ 0053H
LDIRVM equ 005CH
WRTVDP equ 0047H
RG1SAV equ 0F3E0H
RDVDP  equ 013EH
SNSMAT.a equ 0141h

CHGMOD equ 005FH

IMULT.a equ 3193H;HL ← DE*HL
IDIV.a equ 31E6H;HL ← DE/HL
IMOD.a equ 323AH;HL ← DE mod HL (DE ← DE/HL)

WRTPSG  equ 0093H

CSRY equ 0F3DCH
CSRX equ 0F3DDH

null equ 0

marker.b macro n
 ;last.marker: defl $
endm
marker.e macro n
 ;len.##n: defl $-last.marker
endm
