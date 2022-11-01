include z_debug

defsub keyall
;show hl
#local
 ld hl,keymat1
 ld de,keymat2
 ld bc,13
 ldir
 ld a,0
 ld hl,keymat1
 lp:
 push af
 call SNSMAT.a
 xor 255
 ld (hl),a
 pop af
 inc hl
 inc a
 cp 11
 jr c,lp
 ld a,1
 call snstk.a
 ld (hl),d
 inc hl
 ld a,2
 call snstk.a
 ld (hl),d
#endlocal
endsub keyall
defsub snstk.a
#local
 push hl
  ld e,a
  push de
   call 0d5h
  pop de
  ld c,a
  ld b,0
  ld hl,trig2mat
  add hl,bc
  ld d,(hl)
  ld a,e
  push de
   call 0d8h
  pop de
  cp 0
  jr z,skp1
  set 0,d
  skp1:
  ld a,e
  inc a
  inc a
  push de
   call 0d8h
  pop de
  cp 0
  jr z,skp2
  set 1,d
  skp2:
 pop hl
#endlocal
endsub snstk.a

defwork keymat1
ds 13, 0
endwork keymat1
defwork keymat2
ds 13, 0
endwork keymat2
defwork trig2mat
db 0

;  rdul
db 00100000b
db 10100000b
db 10000000b
db 11000000b
db 01000000b
db 01010000b
db 00010000b
db 00110000b

endwork trig2mat


defsub getkey.a
ex de,hl
ld hl,keymat1
call chkmat
ld hl,0
ret z
ld hl,keymat2
call chkmat
ld hl,1
ret z
inc hl
endsub getkey.a

defsub chkmat
push de
ld a,d
ld d,0
add hl,de
and (hl)
pop de
endsub chkmat

defsub getkey
push af
call getkey.a
pop af
endsub getkey

defsub stickX.a
#local
; a :stickid
; hl:  vec norm
 cp 0
 jr nz,nx1
  ld a,(keymat1+8)
  bit 7,a
  ret nz
  bit 4,a
  jp nz,neghl
  ld hl,0
  ret
 nx1:
 cp 1
 jr nz,nx2
  ld a,(keymat1+11)
  bit 7,a
  ret nz
  bit 4,a
  jp nz,neghl
  ld hl,0
  ret
 nx2:
 cp 2
 jr nz,nx3
  ld a,(keymat1+12)
  bit 7,a
  ret nz
  bit 4,a
  jp nz,neghl
  ld hl,0
  ret
 nx3:
 ; w 5-b4  a 2-b6  s 5-b0  d 3-b1
  ld a,(keymat1+3);d
  bit 1,a
  ret nz
  ld a,(keymat1+2);a
  bit 6,a
  jp nz,neghl
  ld hl,0
#endlocal
endsub stickX.a


defsub stickY.a
#local
; a :stickid
; hl:  vec norm
 cp 0
 jr nz,nx1
  ld a,(keymat1+8)
  bit 6,a
  ret nz
  bit 5,a
  jp nz,neghl
  ld hl,0
  ret
 nx1:
 cp 1
 jr nz,nx2
  ld a,(keymat1+11)
  bit 6,a
  ret nz
  bit 5,a
  jp nz,neghl
  ld hl,0
  ret
 nx2:
 cp 2
 jr nz,nx3
  ld a,(keymat1+12)
  bit 6,a
  ret nz
  bit 5,a
  jp nz,neghl
  ld hl,0
  ret
 nx3:
 ; w 5-b4  a 2-b6  s 5-b0  d 3-b1
  ld a,(keymat1+5);s
  bit 0,a
  ret nz
  bit 4,a ; w
  jp nz,neghl
  ld hl,0
#endlocal
endsub stickY.a


