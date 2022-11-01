org 09000h
; TODO
; frameCount
; strig

include z_ctrl
include z_math
include z_debug
include z_sub
include z_mem
include z_sp
include z_vdp
include z_th


;===your code

main:
ld a,5
lp:
dec b
show a
iff nc,nx
jr lp
nx:

ret
end main