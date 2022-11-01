 org 04000h
 db 41h, 42h
 dw 04010h
 ds 12,0
ramtop equ 8000h
init:
 call cpslt
 jp ramtop 
include z_mem
include z_ctrl

cpwork equ 0c000h
cplen equ 01000h
rom equ cpwork+cplen
ram equ rom+1
defsub cpslt
#local
 ld hl,4000h
 call rdslt
 ld (rom),a
 ld hl,cpwork
 call rdslt
 ld (ram),a
 ld hl,ramtop 
 loop:
  push hl
  ld a,(rom)
  ld hl,ramtop 
  call ENASLT
  pop hl
  push hl
  ld bc,cplen
  ld de,cpwork
  ldir
  ld a,(ram)
  ld hl,ramtop 
  call ENASLT
  ld hl,cpwork
  pop de
  ld bc,cplen
  ldir
  ld a,h
  cp 0c0h
  jr c,loop
 ret
#endlocal 
endsub 0

