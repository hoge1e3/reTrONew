include z_mem
include z_debug
sp.get macro
 ld HL,0
 ADD hl, sp
endm
sp.set macro
 ld sp,hl
endm
mem2sp macro
 ;(hl)->sp
#local
 ld (rs-2),hl
 ld sp,(0)
 rs:
#endlocal
endm
sp2mem macro
 ;sp->(hl)
#local
 ld (spad-2),hl
 ld (0),sp
 spad:
#endlocal
endm

showsp macro
 ld (sptmp),sp
 showm sptmp
endm
sptmp:
dw 0
showstk macro
 showsp
 ld (sva),a
 ld a,":"
 call wrt
 ld a,(sva)
 ex (sp),hl
 show hl
 ex (sp),hl
endm
sva: db 0