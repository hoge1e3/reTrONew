include z_mem
include z_debug
mus.ini:
 di
 ld hl,0fd9fh
 ld (hl),0c3h
 movw (0fd9fh+1),mus
 ei
 ret
mus:
#local
 push af
 push de
 ld a,(we-1)
 xor 15
 ld (we-1),a
 ld a,8
 ld e,15
 we:
 call WRTPSG
 pop af
 pop de
 ret
#endlocal

defsub mus.scale.a
 ; a: 0,1,2 ch
 ; l: 0-95
 sla a
 sla l
 ld h,0
 ld bc,scltbl
 add hl,bc
 ld e,(hl)
 call WRTPSG
 inc hl
 ld e,(hl)
 inc a
 call WRTPSG
endsub mus.scale.a

defwork scltbl
dw 0d5dh, 0c9ch, 0be7h, 0b3ch, 0a9bh, 0a02h, 0973h, 08ebh, 086bh, 07f2h, 0780h, 0714h, 06afh, 064eh, 05f4h, 059eh, 054eh, 0501h, 04bah, 0476h, 0436h, 03f9h, 03c0h, 038ah, 0357h, 0327h, 02fah, 02cfh, 02a7h, 0281h, 025dh, 023bh, 021bh, 01fdh, 01e0h, 01c5h, 01ach, 0194h, 017dh, 0168h, 0153h, 0140h, 012eh, 011dh, 010dh, 0feh, 0f0h, 0e3h, 0d6h, 0cah, 0beh, 0b4h, 0aah, 0a0h, 097h, 08fh, 087h, 07fh, 078h, 071h, 06bh, 065h, 05fh, 05ah, 055h, 050h, 04ch, 047h, 043h, 040h, 03ch, 039h, 035h, 032h, 030h, 02dh, 02ah, 028h, 026h, 024h, 022h, 020h, 01eh, 01ch, 01bh, 019h, 018h, 016h, 015h, 014h, 013h, 012h, 011h, 010h, 0fh, 0eh
endwork scltbl