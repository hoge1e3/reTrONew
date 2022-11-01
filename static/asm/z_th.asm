include z_ctrl
include z_sp
include z_vdp
include z_mem
include z_math
include z_debug

th.ofs.stp equ 256-4
th.ofs.sp equ th.ofs.stp+1
th.ofs.spini equ th.ofs.stp
fld.top equ th.ofs.spini-2
th.st.blank equ 0c9h
th.st.active equ 31h

;macro th.for,lb
; ld HL,th.start
; ld de,th.end
; ld bc,th.size
; for lb
;endm
th.for.a macro nx,st,en
#local
 ld a,&st+(th.start.h)
 loop:
  cp &en+(th.start.h)
  jp nc, &nx
  push af
  ld h,a
  ld l,0
  call do
  popa 0
  ld h,a
  ld l,0
  jp c, &nx
  inc a
 jr loop
 do:
#endlocal
endm

th.new.range macro st,en
 ld bc,&st
 ld(th.new.start),bc
 ld bc,&en
 ld(th.new.end),bc
endm

defsub th.isblank.a
 ; h= thread
 ; z if true
 ld l, th.ofs.stp
 ld a,(hl)
 cp th.st.blank
endsub th.isblank.a

 defwork th.new.start
 dw th.start
 endwork th.new.start
 defwork th.new.end
 dw th.end
 endwork th.new.end

defsub th.find.blank
#local
 ld a,(th.new.start+1)
 ld h,a
 ld l,th.ofs.stp
loop:
 ld a,(hl)
 cp th.st.blank
 ret z
 ld a,(th.new.end+1)
 inc h
 cp h
 jr nz,loop
 inc a; reset z
#endlocal
endsub 0

defsub th.new
; nc for alloc fail
#local
 ld hl,(th.new.start)
 ld de,(th.new.end)
 ld bc,th.size
 for lbend
  ; TODO th.ofs.stp
  call th.isblank.a
  break.if z
  continue
 lbend:
 ret nc
 ; TODO th.ofs.stp
 ld L,th.ofs.stp
 ld (HL),31h
 inc HL
 ld (HL),th.ofs.spini
 ld a,h
 inc HL
 ld (hl),a
 inc HL
 ld (HL),0c9h
 ld l,th.bottom
 scf
 ret
#endlocal
endsub th.new

defsub th.init
#local
 th.for.a lbend, 0, th.count
  ; TODO th.ofs.stp
  ld L, th.ofs.stp
  ld (HL),th.st.blank
  continue
 lbend:
 ; disable timer
 ld HL,0fd9fh
 ld (hl),0c9h
 call susint
 ret
#endlocal
endsub th.init

defsub th.stepall
 th.for.a thnx, 0, th.count
  ;todo th.ofs.stp
  ld (th.cur),hl
  call th.isblank.a
  continue.if z
  call th.step

  ; chk reserved kill
  ld hl,0
  ld (th.cur),hl
  ld a,(th.kill.reserve)
  cp 0
  continue.if z
  ld h,a
  call th.kill.a
  xor a
  ld (th.kill.reserve),a
  continue
 thnx:
endsub th.stepall

defsub th.kill.a
; th.term = self kill
; th.kill = kill from either external or internal
#local
 ; h: th
 ld a,(th.cur+1)
 cp h
 jr z, reserve
  ld l, th.ofs.stp
  ld (hl), th.st.blank
  ret
 reserve:
  ld (th.kill.reserve),a
#endlocal
endsub th.kill.a

defwork th.kill.reserve
db 0
endwork th.kill.reserve

defsub th.step
 ld (adrssp+1),sp
 ld HL,(th.cur)
 ld l,th.ofs.stp
 ;call susint
 jp (hl)
endsub th.step

defsub th.yield
 ld hl,(th.cur)
 ld l,th.ofs.sp
 sp2mem
 adrssp:
 ld sp,0
 jp doint
endsub th.yield

defsub th.term
 ld hl,(th.cur)
 ; TODO th.ofs.stp
 ld L,th.ofs.stp
 ld (hl),th.st.blank
 jr adrssp
endsub th.term

th.with.do macro to
#local
 th.with pr
 jr &to
 pr:
#endlocal
endm

th.with.setdst macro reg
 ld (th.jpdest-2),&reg
endm
th.with macro pr
 movw (th.jpdest-2), &pr
 call th.with.s
endm
th.with.ret macro n
 jp th.ewith
endm

defsub th.with.s
 ld (th.wrssp-2),sp
 ld l, th.ofs.sp
 ld (th.updsp-2),hl
 mem2sp
 jp 0
 th.jpdest:
th.ewith:
 ld (0),sp
 th.updsp:
 ld sp,0
 th.wrssp:
endsub th.with.s




defsub th.push
 ;push bc to thread hl
 th.with tpsbc
 ret
 tpsbc:
  push bc
  th.with.ret 0
endsub th.push


defwork th.cur
 dw 0
endwork th.cur

defsub th.loop
 ; hook before stepall
 db 0cdh
 h.thent:
 dw int.nop
 ; save prev timecnt
 ld a,(timecnt)
 push af
 ; Do stepall
 call th.stepall
 ; hook after stepall
 db 0cdh
 h.thlop:
 dw int.nop
 ; wait until timecnt changes
 pop af
 bwat:
  ld hl,timecnt
  cp (hl)
  jr nz,bbwat
  push af
  call doint
  pop af
  jr bwat
 bbwat:
 ; repeat
 jr th.loop
endsub th.loop



th.pushi macro val
 ld bc,&val
 call th.push
endm

