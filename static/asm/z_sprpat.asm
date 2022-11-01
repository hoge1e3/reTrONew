include z_const

;aaa
spr.inipat:
 ld de,3800h
 ld hl,spr.pat
 ld bc,128
 jp LDIRVM
bg.inipat:
 ret
spr.pat:
; --- Slot 0 cat fstand
; color 9
DB $0C,$0E,$0F,$4F,$3D,$1D,$7F,$1B
DB $0C,$3F,$7F,$7F,$6F,$0F,$06,$0C
DB $18,$38,$F8,$F9,$DE,$DC,$7F,$6C
DB $98,$FC,$FE,$FE,$F6,$F0,$60,$70
;
; --- Slot 1 cat fwalk1
; color 9
DB $0C,$0E,$0F,$4F,$3D,$1D,$7F,$1B
DB $0C,$3F,$7F,$7F,$EF,$EF,$06,$06
DB $18,$38,$F8,$F9,$DE,$DC,$7F,$6C
DB $98,$FC,$FE,$FE,$D4,$78,$F0,$00
;
; --- Slot 2 cat fwalk2
; color 9
DB $18,$1C,$1F,$9F,$7B,$3B,$FE,$36
DB $19,$3F,$7F,$7F,$2B,$1E,$0F,$00
DB $30,$70,$F0,$F2,$BC,$B8,$FE,$D8
DB $30,$FC,$FE,$FE,$F7,$F7,$60,$60
;
; --- Slot 3 cat omg
; color 9
DB $2C,$8E,$0F,$4B,$3D,$11,$7F,$1D
DB $CA,$FF,$7F,$3F,$15,$1F,$0E,$00
DB $1C,$39,$F8,$E9,$DE,$C4,$7F,$5C
DB $AB,$FF,$FF,$FE,$AC,$F8,$70,$00

ds 60*32, 0
