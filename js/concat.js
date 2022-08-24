Tonyu.klass.define({
  fullName: 'user.Asms',
  shortName: 'Asms',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.TObject,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_Asms_main() {
        "use strict";
        var _this=this;
        
        _this.data = {'main': ['org 09000h\r\n\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude sp\r\ninclude vdp\r\ninclude th\r\n\r\n\r\n;===your code \r\n\r\nmain:\r\nld a,5\r\nlp:\r\ndec b\r\nshow a\r\niff nc,nx\r\njr lp\r\nnx:\r\n\r\nret\r\nend main'].join(''),'math': ['include ctrl\r\n\r\n;16bit shifts\r\nmacro slhl\r\n sla l\r\n rl h\r\nendm\r\nmacro srahl\r\n sra h\r\n rr l\r\nendm\r\nmacro srlhl\r\n srl h\r\n rr l\r\nendm\r\nmacro slde\r\n sla e\r\n rl d\r\nendm\r\nmacro srade\r\n sra d\r\n rr e\r\nendm\r\nmacro srlde\r\n srl d\r\n rr e\r\nendm\r\nmacro slbc\r\n sla c\r\n rl b\r\nendm\r\nmacro srabc\r\n sra b\r\n rr c\r\nendm\r\nmacro srlbc\r\n srl b\r\n rr c\r\nendm\r\n\r\n\r\n; for xrnd\r\nmacro sldehl,n\r\n  local loop\r\n  ld b,n\r\n loop:\r\n  sla d\r\n  rl e\r\n  rl h\r\n  rl l\r\n  djnz loop\r\nendm\r\nmacro srdehl,n\r\n local loop\r\n ld b,n\r\n loop:\r\n  srl l\r\n  rr h\r\n  rr e\r\n  rr d\r\n djnz loop\r\nendm\r\n \r\nmacro xorrm,re,me\r\n  ld A,(me)\r\n  xor re\r\n  ld (me),a\r\nendm\r\n\r\nmacro subhl,rp\r\n and a\r\n sbc hl,rp\r\nendm\r\n\r\nmacro cpde.a\r\n rst dcompr\r\nendm\r\n\r\n\r\nmarker.b xrnd.a\r\nxrnd.a:\r\nproc \r\n local rhl,rde,rdhlde\r\n ; s[0] ^= s[0] << 13\r\n call rdhlde\r\n sldehl 13\r\n call wrtxor\r\n ; s[0] ^= s[0] >> 17\r\n call rdhlde\r\n srdehl 17\r\n call wrtxor\r\n ; s[0] ^= s[0] << 5;\r\n call rdhlde\r\n sldehl 5\r\n call wrtxor\r\n ret\r\n \r\n rdhlde:\r\n  ld hl,1234\r\n rhl:\r\n  ld de,5678\r\n rde:\r\n  ret\r\n \r\n wrtxor:\r\n  xorrm h,rhl-1\r\n  xorrm l,rhl-2\r\n  xorrm d,rde-1\r\n  xorrm e,rde-2\r\n  ret\r\nendp\r\nmarker.e xrnd.a\r\n\r\n\r\nmarker.b rnd\r\nrnd:\r\n push af\r\n call rnd.a\r\n pop af\r\n ret\r\nmarker.e rnd\r\nmarker.b rnd.a\r\nrnd.a:\r\n ld de,07fffh\r\n call IDIV.a\r\n push hl\r\n call xrnd.a\r\n res 7,h\r\n ex de,hl\r\n pop hl\r\n inc hl\r\n call IDIV.a\r\n ret\r\nmarker.e rnd.a\r\n\r\nmarker.b abs\r\nabs:\r\n bit 7,h\r\n ret z\r\nneghl:\r\n ld de,0 \r\n ex de,hl \r\n subhl de\r\n ret\r\nmarker.e abs\r\n\r\n '].join(''),'bool': ['include math\r\ninclude ctrl\r\ntrue equ -1\r\nfalse equ 0\r\n\r\nmacro rethl,val,flg\r\n local tru\r\n if not nul flg\r\n  iff flg ,tru\r\n endif\r\n ld hl,val\r\n ret\r\n tru:\r\nendm\r\n\r\nhleqde:\r\n subhl de\r\n rethl true,z\r\n rethl false\r\n\r\nhlnede:\r\n subhl de\r\n rethl true,nz\r\n rethl false\r\n \r\nhlgtde:\r\n subhl de\r\n rethl false,z\r\n bit 7,h\r\n rethl true,z\r\n rethl false\r\n\r\nhlltde:\r\n subhl de\r\n bit 7,h\r\n rethl true,nz\r\n rethl false\r\n \r\nhlgede:\r\n subhl de\r\n rethl true,z\r\n bit 7,h\r\n rethl true,z\r\n rethl false\r\n\r\nhllede:\r\n subhl de\r\n rethl true,z\r\n bit 7,h\r\n rethl true,nz\r\n rethl false\r\n \r\nproc\r\nziffalse:\r\n local resa\r\n ld (resa-1),a\r\n call ziffalse.a\r\n ld A,0\r\n resa:\r\n ret\r\nziffalse.a:\r\n ld a,0\r\n cp h\r\n ret nz\r\n cp l\r\n ret\r\nendp\r\n\r\nmacro jpf,to\r\n call ziffalse\r\n jp z,to\r\nendm\r\n\r\nmacro andand,fls\r\n jpf fls\r\nendm\r\nmacro oror,tru\r\n call ziffalse\r\n jp nz,tru\r\nendm\r\n\r\nmacro flagtobool,fl\r\n local yes,skp\r\n jr fl, yes\r\n ld hl,false\r\n jr skp\r\n yes:\r\n ld hl,true\r\n skp: \r\nendm'].join(''),'mem': ['include const\r\n;\r\nrdslt:\r\n ex de,hl\r\n rept 5\r\n srl d;page*2\r\n endm\r\n CALL RSLREG\r\n ld e,d\r\n rdslt1:\r\n  RRCA\r\n  dec e\r\n  jp nz,rdslt1\r\n AND    00000011B\r\n LD C,A;000000Pr\r\n LD B,0\r\n LD HL,EXPTBL\r\n ADD HL,BC\r\n LD C,A;000000Pr\r\n LD A,(HL)\r\n AND 80H;expand flag\r\n OR C\r\n LD C,A;e00000Pr\r\n rept 4;const\r\n INC HL\r\n endm\r\n LD A,(HL);exp reg\r\n ld e,d\r\n rdslt2:\r\n  srl a\r\n  dec e\r\n  jp nz,rdslt2\r\n;000000Ex\r\n sla a\r\n sla a\r\n ;    0000Ex00\r\n and  00001100b\r\n OR C;e000ExPr\r\n ret\r\nmemini:\r\n CALL RSLREG\r\n rept 4\r\n  RRCA\r\n endm\r\n AND    00000011B\r\n LD C,A;000000Pr\r\n LD B,0\r\n LD HL,EXPTBL\r\n ADD HL,BC\r\n LD C,A;000000Pr\r\n LD A,(HL)\r\n AND 80H;expand flag\r\n OR C\r\n LD C,A;e00000Pr\r\n rept 4;const\r\n INC HL\r\n endm\r\n LD A,(HL);exp reg\r\n rept 4; page*2\r\n srl a\r\n endm;000000Ex\r\n sla a\r\n sla a\r\n ;    0000Ex00\r\n and  00001100b\r\n OR C;e000ExPr\r\n LD Hl,04000H\r\n jp ENASLT\r\n\r\nmacro peekw ,regv,regm\r\n  local w\r\n  ld (w-2),regm\r\n  ld regv,(0)\r\n  w:\r\nendm\r\n\r\nmacro pokew ,regm,regv\r\n  local w\r\n  ld (w-2),regm\r\n  ld (0),regv\r\n  w:\r\nendm\r\nmacro movw,dst,src,rp\r\n if nul rp\r\n  push hl\r\n  movw dst,src,hl\r\n  pop hl\r\n else\r\n  ld rp,src\r\n  ld dst,rp\r\n endif \r\nendm\r\n\r\nmacro popa\r\n  ex (sp),hl\r\n  ld a,h\r\n  pop HL\r\nendm\r\n\r\nmacro pushall\r\n push af\r\n push bc\r\n push de\r\n push hl\r\nendm\r\nmacro popall\r\n pop hl\r\n pop de\r\n pop bc\r\n pop af\r\nendm\r\n \r\n\r\nmacro pushi, n,rp\r\n local rrr\r\n if nul rp\r\n  ld (rrr-2),hl\r\n  ld hl,n\r\n  push hl\r\n  ld hl,0\r\n  rrr:\r\n else\r\n  ld rp,n\r\n  push rp\r\n endif\r\nendm\r\nmacro const,n,reg\r\n ld (n-2),reg\r\nendm\r\nmacro ldconst,reg,n\r\n ld reg,0\r\n n:\r\nendm\r\nmacro peekconst,reg,n\r\n ld reg,(0)\r\n n:\r\nendm\r\n'].join(''),'const': ['\r\n;wrt equ 0a2h\r\ndcompr equ 0020H\r\nsp.ini equ 0dc00h\r\nstksize equ 512\r\n\r\nth.size equ 256\r\nth.count equ 20\r\nth.start equ th.end-th.size*th.count\r\nth.end equ sp.ini-stksize\r\n\r\nth.bottom equ 0\r\n\r\nspr.scale equ 1\r\nspr.xmax equ 256<<spr.scale\r\nspr.ymax equ 192<<spr.scale\r\n\r\nENASLT EQU 0024H\r\nRSLREG EQU 0138H\r\nEXPTBL EQU 0FCC1H\r\nSETWRT equ 0053H\r\nLDIRVM equ 005CH\r\nWRTVDP equ 0047H\r\nRG1SAV equ 0F3E0H\r\nRDVDP  equ 013EH\r\nSNSMAT.a equ 0141h\r\n\r\nCHGMOD equ 005FH\r\n\r\nIMULT.a equ 3193H;HL ← DE*HL\r\nIDIV.a equ 31E6H;HL ← DE/HL\r\nIMOD.a equ 323AH;HL ← DE mod HL (DE ← DE/HL) \r\n\r\nWRTPSG  equ 0093H\r\n\r\nCSRY equ 0F3DCH\r\nCSRX equ 0F3DDH\r\n\r\nnull equ 0\r\n\r\nmacro marker.b, n\r\n last.marker: defl $\r\nendm\r\nmacro marker.e, n\r\n len.##n: defl $-last.marker\r\nendm\r\n'].join(''),'ctrl': ['include const\r\nfreeze:\r\nhalt\r\njr freeze\r\n\r\nmacro for ,lbend\r\n ; uses a\r\n ; c: breaked\r\n proc\r\n  local s,lb\r\n  lb:\r\n  call dcompr; uses a\r\n  jp nc,lbend\r\n  push HL\r\n  push de\r\n  push bc\r\n  call s\r\n  pop bc\r\n  pop de\r\n  pop HL\r\n  jp c,lbend\r\n  add HL,bc\r\n  jr lb\r\n  s:\r\n endp\r\nendm\r\n\r\nmacro repti ,n,lbend\r\n proc\r\n  local s,lb, lbend2\r\n  push bc\r\n  ld b,n\r\n  lb:\r\n  push bc\r\n  call s\r\n  pop bc\r\n  jr c,lbend2\r\n  djnz lb\r\n  lbend2:\r\n  pop bc\r\n  jp lbend \r\n  s:\r\n endp\r\nendm\r\n\r\n\r\nmacro reptb ,lbend\r\n  local s,lb\r\n inc b\r\n djnz lb\r\n jp lbend\r\n lb:\r\n  push bc\r\n  call s\r\n  pop bc\r\n  jp c,lbend\r\n djnz lb\r\n jp lbend \r\n s:\r\nendm\r\n\r\n\r\n\r\nmacro callsva,pp\r\n local sva\r\n ld (sva-1),a\r\n call pp\r\n ld a,0\r\n sva:\r\nendm\r\nbcis0:\r\n callsva bcis0.a\r\n ret\r\nbcis0.a:\r\n ld a,b\r\n and a\r\n ret nz\r\n ld a,c\r\n and a\r\n ret\r\n\r\nmacro reptbc ,lbend\r\n local s,lb\r\n call bcis0\r\n jp z,lbend \r\n lb:\r\n  push bc\r\n  call s\r\n  pop bc\r\n  jp c,lbend\r\n  dec bc\r\n  call bcis0\r\n jr nz, lb\r\n jp lbend \r\n s:\r\nendm\r\n\r\n\r\niff.NZ equ 0\r\niff.Z  equ 1\r\niff.NC equ 2\r\niff.C  equ 3\r\n\r\nmacro iff,cnd,to\r\n local iff.\r\n if iff.##cnd eq iff.NZ\r\n  jr z,to\r\n endif\r\n if iff.##cnd eq iff.Z\r\n  jr nz,to\r\n endif\r\n if iff.##cnd eq iff.NC\r\n  jr c,to\r\n endif\r\n if iff.##cnd eq iff.C\r\n  jr nc,to\r\n endif\r\n ;jr cnd, skip\r\n ;jr to\r\n ;skip:\r\nendm\r\n\r\nmacro break,cnd\r\n if NUL cnd\r\n  scf\r\n  ret\r\n else\r\n  proc \r\n   local jj\r\n   iff cnd ,jj\r\n   break\r\n  jj:\r\n  endp\r\n endif\r\nendm\r\nmacro continue,cnd\r\n if NUL cnd \r\n  or a\r\n  ret\r\n else\r\n  proc \r\n   local jj\r\n   iff cnd,jj\r\n   continue\r\n  jj:\r\n  endp\r\n endif\r\nendm\r\n\r\n\r\nmacro djnzr,reg, j\r\n dec reg\r\n jr NZ,j\r\nendm\r\n\r\nmacro callhl\r\n local LCD\r\n ld (LCD-2),HL\r\n call LCD\r\n LCD:\r\nendm\r\n\r\nmacro stride,lim,to\r\n if (low $)<lim\r\n  exitm\r\n endif\r\n ds 256+to-(low $),0cdh\r\nendm'].join(''),'th': ['include ctrl\r\ninclude sp\r\ninclude vdp\r\ninclude mem\r\ninclude math\r\ninclude debug\r\n\r\nth.ofs.stp equ 256-4\r\nth.ofs.sp equ th.ofs.stp+1\r\nth.ofs.spini equ th.ofs.stp\r\nfld.top equ th.ofs.spini-2\r\nth.st.blank equ 0c9h\r\nth.st.active equ 31h\r\n\r\nmacro th.for,lb\r\n ld HL,th.start\r\n ld de,th.end\r\n ld bc,th.size\r\n for lb\r\nendm\r\n\r\nmacro th.new.range,st,en\r\n ld bc,st\r\n ld(th.new.start),bc\r\n ld bc,en\r\n ld(th.new.end),bc\r\nendm\r\n\r\ndefsub th.isblank.a\r\n ; h= thread\r\n ; z if true\r\n ld l, th.ofs.stp\r\n ld a,(hl)\r\n cp th.st.blank\r\nendsub th.isblank.a\r\n\r\ndefsub th.new\r\n; nc for alloc fail\r\nproc \r\n local lbend\r\n db 21h\r\n th.new.start:\r\n dw th.start\r\n db 11h\r\n th.new.end:\r\n dw th.end\r\n ld bc,th.size\r\n for lbend\r\n  ; TODO th.ofs.stp\r\n  call th.isblank.a\r\n  break z\r\n  continue\r\n lbend:\r\n ret nc\r\n ; TODO th.ofs.stp\r\n ld L,th.ofs.stp\r\n ld (HL),31h\r\n inc HL\r\n ld (HL),th.ofs.spini\r\n ld a,h\r\n inc HL\r\n ld (hl),a\r\n inc HL\r\n ld (HL),0c9h\r\n ld l,th.bottom\r\n scf\r\n ret\r\nendp\r\nendsub th.new\r\n\r\ndefsub th.init\r\nproc\r\n local lbend\r\n th.for lbend\r\n  ; TODO th.ofs.stp\r\n  ld L, th.ofs.stp\r\n  ld (HL),th.st.blank\r\n  continue\r\n lbend:\r\n ; disable timer\r\n ld HL,0fd9fh\r\n ld (hl),0c9h\r\n call susint\r\n ret\r\nendp\r\nendsub th.init\r\n\r\ndefsub th.stepall\r\n th.for thnx\r\n  ;todo th.ofs.stp\r\n  ld (th.cur),hl\r\n  call th.isblank.a\r\n  continue z\r\n  call th.step\r\n  continue\r\n thnx:\r\nendsub th.stepall\r\n\r\ndefsub th.step\r\n sp2mem adrssp+1\r\n ld HL,(th.cur)\r\n ld l,th.ofs.stp\r\n ;call susint\r\n jp (hl)\r\nendsub th.step\r\n\r\ndefsub th.yield\r\n ld hl,(th.cur)\r\n ld l,th.ofs.sp\r\n sp2mem\r\n adrssp:\r\n ld sp,0\r\n jp doint\r\nendsub th.yield\r\n\r\ndefsub th.term\r\n ld hl,(th.cur)\r\n ; TODO th.ofs.stp\r\n ld L,th.ofs.stp\r\n ld (hl),th.st.blank\r\n jr adrssp\r\nendsub th.term\r\n\r\nmacro th.with.do, to\r\n local pr\r\n th.with pr\r\n jr to\r\n pr:\r\nendm\r\n\r\nmacro th.with.setdst, reg\r\n ld (th.jpdest-2),reg\r\nendm\r\nmacro th.with,pr\r\n movw (th.jpdest-2), pr\r\n call th.with.s\r\nendm\r\nmacro th.with.ret\r\n jp th.ewith\r\nendm\r\n\r\ndefsub th.with.s\r\n sp2mem th.wrssp-2\r\n ld l, th.ofs.sp\r\n ld (th.updsp-2),hl\r\n mem2sp\r\n jp 0\r\n th.jpdest:\r\nth.ewith:\r\n ld (0),sp\r\n th.updsp:\r\n ld sp,0\r\n th.wrssp:\r\nendsub th.with.s\r\n \r\n\r\n \r\n \r\ndefsub th.push\r\n ;push bc to thread hl\r\n th.with tpsbc\r\n ret\r\n tpsbc:\r\n  push bc\r\n  th.with.ret 0\r\nendsub th.push\r\n\r\n\r\ndefwork th.cur\r\n dw 0\r\nendwork th.cur\r\n\r\ndefsub th.loop\r\n ; hook before stepall\r\n db 0cdh\r\n h.thent:\r\n dw th.nop\r\n ; save prev timecnt\r\n ld a,(timecnt)\r\n push af\r\n ; Do stepall\r\n call th.stepall\r\n ; hook after stepall\r\n db 0cdh\r\n h.thlop:\r\n dw th.nop\r\n ; wait until timecnt changes\r\n pop af\r\n bwat:\r\n  ld hl,timecnt\r\n  cp (hl)\r\n  jr nz,bbwat\r\n  push af\r\n  call doint\r\n  pop af\r\n  jr bwat\r\n bbwat:\r\n ; repeat\r\n jr th.loop\r\nendsub th.loop\r\n\r\nth.nop:\r\n ret\r\n\r\n\r\nmacro th.pushi, val\r\n ld bc,val\r\n call th.push\r\nendm\r\n\r\n'].join(''),'sub': [].join(''),'debug': ['include math\r\n;debug\r\nmacro show,reg\r\n ld (hexval+1),reg\r\n call showhex\r\nendm\r\nmacro showm ,ad\r\n push hl\r\n ld HL,(ad)\r\n show HL\r\n pop HL\r\nendm\r\nmacro showlb,lb\r\n push hl\r\n ld hl,lb\r\n ld (hexval+1),hl\r\n call showhex\r\n pop hl\r\nendm\r\nshowhex:\r\nproc\r\n local loop\r\n push af\r\n push bc\r\n push HL\r\n hexval:\r\n ld hl,0\r\n ld b,4\r\n loop:\r\n  xor a\r\n  rept 4\r\n   slhl\r\n   rla\r\n  endm\r\n  call showhex1\r\n djnz loop\r\n ld a,32\r\n call wrt\r\n pop HL\r\n pop bc\r\n pop af\r\n ret\r\nendp\r\nshowhex1:\r\nproc\r\n local els\r\n cp 10\r\n jp nc, els\r\n add a,48\r\n jp wrt\r\n els:\r\n add a,65-10\r\n jp wrt\r\nendp\r\nabort:\r\n call wrt\r\n db 018h,0feh\r\nret\r\n\r\nmacro trace,v\r\n if not nul v\r\n  push af\r\n  ld a,v\r\n  ld (trad),a\r\n  pop af\r\n endif\r\n call trace.s\r\nendm\r\ntrace.s:\r\n push af\r\n push hl\r\n ld a,(trad)\r\n ld hl,1ae0h\r\n call wrt\r\n call 4dh\r\n inc a\r\n ld (trad),a\r\n ld a,32\r\n call wrt \r\n pop hl\r\n pop af\r\n ret\r\ntrad:\r\n db 65\r\n\r\nshowz:\r\n push af\r\n jr z,showz.s\r\n ld a,"N"\r\n call wrt\r\n showz.s:\r\n ld a,"Z"\r\n call wrt\r\n ld a,32\r\n call wrt\r\n pop af\r\n ret\r\n \r\n\r\nshowc:\r\n push af\r\n jr c,showc.s\r\n ld a,"N"\r\n call wrt\r\n showc.s:\r\n ld a,"C"\r\n call wrt\r\n ld a,32\r\n call wrt\r\n pop af\r\n ret\r\n \r\n\r\n\r\n\r\n\r\n\r\nmacro unreach, mesg\r\n trace mesg\r\n dw 0x18,0xfe\r\nendm\r\nmacro head, lb\r\n unreach lb\r\n marker.b lb\r\n lb:\r\nendm\r\n\r\nmacro defsub, n\r\n head n\r\nendm\r\nmacro endsub, n\r\n ret\r\n marker.e n\r\nendm\r\nmacro defwork, n\r\n head n\r\nendm\r\nmacro endwork, n\r\n marker.e n\r\nendm\r\n\r\ndefsub wrt\r\nproc\r\n local sk\r\n push hl\r\n push af\r\n ld hl,1800h\r\n cursor:\r\n call 4dh\r\n inc hl\r\n ld a,h\r\n cp 1bh\r\n jr c,sk\r\n  ld h,18h\r\n sk:\r\n ld (cursor-2),hl\r\n pop af\r\n pop hl\r\n ret\r\nendp\r\nendsub wrt\r\n'].join(''),'sp': ['include mem\r\ninclude debug\r\nmacro sp.get\r\n ld HL,0\r\n ADD hl, sp\r\nendm\r\nmacro sp.set\r\n ld sp,hl\r\nendm\r\nmacro mem2sp,ad\r\n local rs\r\n if nul ad\r\n  ld (rs-2),hl\r\n  ld sp,(0)\r\n  rs:\r\n else\r\n  ld sp,(ad)\r\n endif\r\nendm\r\nmacro sp2mem,ad\r\n local spad\r\n if nul ad\r\n  ld (spad-2),hl\r\n  ld (0),sp\r\n  spad:\r\n else\r\n  ld (ad),sp\r\n endif\r\nendm\r\n\r\nmacro showsp\r\n ld (sptmp),sp\r\n showm sptmp\r\nendm\r\nsptmp:\r\ndw 0\r\nmacro showstk\r\n showsp\r\n ld (sva),a\r\n ld a,":"\r\n call wrt\r\n ld a,(sva)\r\n ex (sp),hl\r\n show hl\r\n ex (sp),hl\r\nendm\r\nsva: db 0'].join(''),'oop': ['include mem\r\ninclude th\r\ninclude assert\r\n\r\n;a2 a1  oldpc oldix lcl1 lcl2\r\nargidx equ 2\r\nmacro getarg ,n\r\n ld l,(ix+argidx+n*2)\r\n ld h,(ix+argidx+n*2+1)\r\nendm\r\n\r\nmacro setlcl ,n\r\n ld (IX-(n*2-1)),h\r\n ld (ix-n*2),l\r\nendm\r\n\r\nmacro getlcl ,n\r\n ld h,(IX-(n*2-1))\r\n ld l,(ix-n*2)\r\nendm\r\n\r\nmacro addarg\r\n push hl\r\n; hl=arg  stktp=af\r\n;ex (sp),hl\r\n;ld a,h\r\n;push af\r\nendm\r\n\r\n\r\n\r\nmacro pusharg ,n\r\n getarg n\r\n push HL\r\nendm\r\n\r\nmacro pushlcl ,n\r\n getlcl n\r\n push HL\r\nendm\r\n\r\nmacro enter ,locals\r\n push ix\r\n ld ix,0\r\n add ix,sp\r\n rept locals\r\n  push HL\r\n endm\r\nendm\r\n\r\nmacro pops ,n\r\n rept n*2\r\n  inc sp\r\n endm\r\nendm\r\n\r\n\r\nmacro exit,n\r\n ld sp,ix\r\n pop ix\r\n if n!=0\r\n  exx\r\n  pop bc\r\n  pops n\r\n  push bc\r\n  exx\r\n endif\r\n ret\r\nendm\r\n\r\nmacro pushthis\r\n getthis\r\n push af\r\nendm\r\nmacro popthis\r\n popa\r\n ld (this),a\r\nendm\r\n\r\n\r\nmacro invoketg.a,fld,args\r\n; pushthis before arg push\r\n; hl=target \r\n ld a,h\r\n ld (this),a\r\n getfld fld\r\n callhl\r\n; pops args\r\n; popthis after \r\nendm\r\n\r\nmacro invoke,fld\r\n getfld fld\r\n callhl\r\n; pops args\r\n getthis\r\nendm\r\n\r\nmacro getfld, n\r\n local ad\r\n ld (ad-1),a\r\n ld hl,(n)\r\n ad:\r\nendm\r\n\r\nmacro setfld, n\r\n local ad\r\n ld (ad-1),a\r\n ld (n),hl\r\n ad:\r\nendm\r\n\r\nmacro getfldtg,n\r\n;hl=tg\r\n ld l,n\r\n peekw hl,hl\r\nendm\r\n\r\nmacro setfldtg,n\r\n; stk=val hl=tg\r\n ld l,n\r\n pop de\r\n pokew hl,de\r\nendm\r\n\r\nmacro getfldtg, n\r\n; hl=target\r\n ld d,h\r\n ld e,n\r\n peekw HL,de\r\nendm\r\n\r\nmacro tgconst,n\r\n ld (n-1),a\r\nendm\r\nmacro tgconst.g ,r16,n,fld\r\n ld r16,(fld)\r\n n:\r\nendm\r\nmacro tgconst.s ,n,fld,r16\r\n ld (fld),r16\r\n n:\r\nendm\r\n\r\n\r\nmacro curth2this\r\n ld a,(th.cur+1)\r\n ld (this),a\r\nendm\r\nmacro getthis\r\n ld a,(this)\r\nendm\r\n\r\nmacro new,Class,flds,st,en\r\n if nul st\r\n  th.new.range th.start, th.end\r\n else\r\n  th.new.range th.start+st*th.size, th.start+en*th.size\r\n endif\r\n pushi flds, bc\r\n pushi Class, bc\r\n call o.new\r\nendm\r\n\r\ndefsub o.new\r\nproc\r\n local retad,svthis,svsp,loop,lpend, w,allocfail,finally,lp2,lp2end\r\n ; {val .f} n &initbl retad\r\n pop hl;retad\r\n ld (retad-2),hl\r\n ; set initbl for th.with\r\n pop hl;&initbl\r\n th.with.setdst hl\r\n ; save this\r\n ld (svthis-1),a\r\n ; allocate thread\r\n call th.new\r\n jr nc, allocfail\r\n push hl; thread address\r\n call th.with.s; call &initbl\r\n pop hl; thread address\r\n ld a,h; set this as thread\r\n ; init fields\r\n pop bc; n of {val .f}\r\n inc c\r\n loop:\r\n  dec c\r\n  jr z,lpend\r\n  pop hl; .f\r\n  ld h,a\r\n  ld (w-2),hl\r\n  pop hl; val\r\n  ld (w),hl\r\n  w:\r\n jr loop\r\n lpend:\r\n ; return h as this\r\n ld h,a\r\n finally:\r\n  ;restore a before call o.new\r\n  ld a,0\r\n  svthis:\r\n  ;return \r\n  jp 0\r\n  retad:\r\n allocfail:\r\n  ; drop {val .f}\r\n  pop bc; n of {val .f}\r\n  ld b,c\r\n  inc c\r\n  lp2:\r\n   dec c\r\n   jr z, lp2end\r\n   pop hl\r\n   pop hl\r\n  jr lp2\r\n  lp2end:\r\n  ld hl,null;  todo null\r\n  jr finally\r\nendp\r\nendsub o.new\r\n\r\nmacro new.arg, n, v\r\n if not nul v\r\n  ld hl,v\r\n endif\r\n push hl\r\n pushi n,bc\r\nendm\r\n \r\nmacro o.assert.eq,fld, v\r\n local aa\r\n assert.do aa\r\n  getfld fld\r\n  assert.eq v\r\n  ret\r\n aa:\r\nendm\r\n\r\nthis:\r\ndb 0\r\n\r\nmacro fld.def,n\r\n n equ fldidx\r\n fldidx:defl fldidx-2\r\nendm\r\nmacro class,Class,super\r\n unreach "c"\r\n marker.b 0\r\n dw super\r\n fldidx:defl fld.top; todo fld.top\r\n Class:\r\n  fld .class,Class\r\nendm\r\nmacro fld.bottom,Class\r\n if defined Class##.bottom \r\n  if Class##.bottom ne fldidx\r\n   .error bottom ne fldidx\r\n  endif\r\n else\r\n Class##.bottom:defl fldidx\r\n endif\r\nendm \r\nmacro fld,n,v\r\n if defined n\r\n  if n ne fldidx\r\n   .error n ne fldidx\r\n  else \r\n   fldidx:defl fldidx-2\r\n  endif\r\n else\r\n  fld.def n\r\n endif\r\n pushi v,bc\r\nendm\r\nmacro unuse\r\n fldidx:defl fldidx-2\r\n pushi 0,bc\r\nendm\r\nmacro meth,Class,n\r\n fld .##n, Class##.##n\r\nendm\r\nmacro met2,Class,n\r\n fld n, Class##n\r\nendm\r\n\r\nclass Object,null\r\n fld .main,null\r\n fld.bottom Object\r\n marker.e Object\r\n\r\n\r\ndefsub o.boot\r\n curth2this\r\n invoke .main,0\r\nendsub o.boot\r\n\r\n\r\nmacro yield\r\n pushthis\r\n push ix\r\n call th.yield\r\n pop ix\r\n popthis\r\nendm\r\n\r\nmacro def,n,args,lcls\r\nhead n\r\n def.args:defl args\r\n def.locals:defl lcls\r\n if args>0 or lcls>0\r\n  enter lcls\r\n endif\r\nendm\r\nmacro enddef,n\r\n if def.args>0 or def.locals>0\r\n  exit def.args\r\n else\r\n  ret\r\n endif\r\n marker.e n\r\nendm\r\n\r\ndefsub isobj.a\r\n ;hl=obj?\r\n ;cy=true\r\n ld a,h\r\n cp high th.start\r\n jr c,notobj\r\n cp high th.end\r\n jr nc,notobj\r\n scf\r\n ret\r\n notobj:\r\n and a\r\nendsub isobj.a\r\n\r\ndefsub instanceof\r\n ; a=this de=Class\r\n ; z: true\r\n getfld .class\r\n jp is.subclass.a\r\nendsub instanceof\r\n\r\ndefsub get.superclass\r\n ; hl=Class\r\n dec hl\r\n dec hl\r\n peekw hl,hl\r\nendsub get.superclass\r\n\r\ndefsub is.subclass.a\r\nproc \r\n local top\r\n ; hl=Subclass\r\n ; de=Superclass\r\n ; z:true\r\n top:\r\n cpde.a 0\r\n ret z\r\n call get.superclass\r\n push de\r\n ld de,null\r\n cpde.a 0\r\n pop de\r\n jr nz,top\r\n cpde.a 0\r\nendp\r\nendsub is.subclass.a\r\n '].join(''),'spr': ['include const\r\ninclude th\r\ninclude mem\r\ninclude oop\r\ninclude sub\r\n\r\nclass Sprite,Object\r\n fld .main, 0\r\n fld.bottom Object\r\n fld .x, 100\r\n fld .y, 100\r\n fld .p, 0\r\n fld .c, 2\r\n fld.bottom Sprite\r\n marker.e Sprite\r\n \r\nmacro outwrt\r\n  out (98h),a\r\nendm\r\n\r\n\r\nmacro spr.unscale\r\n ; HL -> A\r\n rept spr.scale\r\n  srlhl\r\n endm\r\n LD A,L\r\n sub 8 \r\nendm\r\n\r\ndefsub spr.puts\r\nproc\r\n local t1,t2,t3,t4\r\n ld hl, 1b00h\r\n call SETWRT\r\n th.for sprl\r\n  ld a,h\r\n  tgconst t1\r\n  tgconst t2\r\n  tgconst t3\r\n  tgconst t4\r\n\r\n  tgconst.g hl,t1,.y \r\n  spr.unscale 0\r\n  outwrt 0\r\n  \r\n  tgconst.g hl,t2,.x \r\n  spr.unscale 0\r\n  outwrt 0\r\n  \r\n  tgconst.g a,t3,.p \r\n  sla a\r\n  sla a\r\n  outwrt 0\r\n  \r\n  tgconst.g a,t4,.c \r\n  outwrt 0\r\n  continue\r\n sprl:\r\nendp\r\nendsub spr.puts\r\n \r\n '].join(''),'sprpat': ['include const\r\n\r\n;aaa\r\nspr.inipat:\r\n ld de,3800h\r\n ld hl,spr.pat\r\n ld bc,128\r\n jp LDIRVM\r\nbg.inipat:\r\n ret\r\nspr.pat:\r\n; --- Slot 0 cat fstand\r\n; color 9\r\nDB $0C,$0E,$0F,$4F,$3D,$1D,$7F,$1B\r\nDB $0C,$3F,$7F,$7F,$6F,$0F,$06,$0C\r\nDB $18,$38,$F8,$F9,$DE,$DC,$7F,$6C\r\nDB $98,$FC,$FE,$FE,$F6,$F0,$60,$70\r\n; \r\n; --- Slot 1 cat fwalk1\r\n; color 9\r\nDB $0C,$0E,$0F,$4F,$3D,$1D,$7F,$1B\r\nDB $0C,$3F,$7F,$7F,$EF,$EF,$06,$06\r\nDB $18,$38,$F8,$F9,$DE,$DC,$7F,$6C\r\nDB $98,$FC,$FE,$FE,$D4,$78,$F0,$00\r\n; \r\n; --- Slot 2 cat fwalk2\r\n; color 9\r\nDB $18,$1C,$1F,$9F,$7B,$3B,$FE,$36\r\nDB $19,$3F,$7F,$7F,$2B,$1E,$0F,$00\r\nDB $30,$70,$F0,$F2,$BC,$B8,$FE,$D8\r\nDB $30,$FC,$FE,$FE,$F7,$F7,$60,$60\r\n; \r\n; --- Slot 3 cat omg\r\n; color 9\r\nDB $2C,$8E,$0F,$4B,$3D,$11,$7F,$1D\r\nDB $CA,$FF,$7F,$3F,$15,$1F,$0E,$00\r\nDB $1C,$39,$F8,$E9,$DE,$C4,$7F,$5C\r\nDB $AB,$FF,$FF,$FE,$AC,$F8,$70,$00\r\n\r\nds 60*32\r\n'].join(''),'tnu': ['\r\ninclude spr\r\ninclude bool\r\ninclude key\r\n\r\n;.onUpdate equ .c-2\r\n;.update equ .onUpdate-2\r\n;.screenOut equ .update-2\r\n;.die equ .screenOut-2\r\n;.updateEx equ .die-2\r\n\r\nmacro end.const, n\r\n pushi RActor.wait,bc\r\n pushi o.boot,bc\r\n th.with.ret 0 \r\n marker.e n\r\nendm\r\n\r\nmacro RActor.noovr,Class\r\n meth Class,main\r\n fld.bottom Object\r\n fld .x, 0\r\n fld .y, -1024\r\n fld .p, 0\r\n fld .c, 3\r\n fld.bottom Sprite\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut \r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld.bottom RActor\r\nendm\r\n\r\nclass RActor,Sprite\r\n RActor.noovr RActor\r\n end.const RActor\r\nRActor.main:\r\n enter 0\r\n exit 0\r\nRActor.update:\r\n invoke .onUpdate\r\n yield\r\n ret \r\nRActor.onUpdate:\r\n ret\r\nRActor.screenOut:\r\nproc\r\n local true\r\n getfld .x\r\n bit 1,h\r\n jr nz, true\r\n getfld .y\r\n ld de,192*2\r\n cpde.a\r\n getthis\r\n jr nc,true\r\n ld hl,0\r\n xor a\r\n ret\r\n true:\r\n ld hl,1\r\n scf\r\n ret\r\nendp\r\nRActor.wait:\r\nproc\r\n local lbl\r\n lbl:\r\n invoke .update\r\n jr lbl\r\nendp\r\ndef RActor.die,0,0\r\n ld h,a\r\n ld l,th.ofs.stp\r\n ld (hl),th.st.blank\r\n ld hl, 0\r\n setfld .c\r\nenddef RActor.die\r\n\r\ndef RActor.updateEx,1,0\r\nproc \r\n local n\r\n; enter 0\r\n getarg 1\r\n ld b,h\r\n ld c,l\r\n reptbc n\r\n  invoke .update\r\n  continue\r\n n:\r\nendp\r\nenddef RActor.updateEx\r\n\r\ncrashTo.size equ 8<<spr.scale\r\n\r\nproc\r\n local gx,gy,t1,t2\r\n local endc,cr1\r\n local fe\r\n\r\ndefsub crashTo.setXY\r\n getfld .x\r\n const gx,hl\r\n getfld .y\r\n const gy,hl\r\nendsub crashTo.setXY\r\n\r\n\r\ndef RActor.crashTo,1,0\r\n call crashTo.setXY\r\n getarg 1\r\n const cr.class,hl\r\n call isobj.a\r\n jr c, cr1\r\n  ld hl, th.start\r\n  ld de, th.end\r\n  call crashTo1\r\n  jr endc\r\n cr1:\r\n  getthis 0\r\n  call crashTo1\r\n  flagtobool c\r\n endc:\r\nenddef RActor.crashTo\r\n\r\nmacro crashToClass,Class,st,en\r\n ; a=this\r\n call crashTo.setXY\r\n ld hl,Class\r\n const cr.class,hl\r\n ld hl,th.start+st*th.size\r\n ld de,th.start+en*th.size\r\n call crashToC\r\nendm\r\n\r\nmacro foreach, Class,st,en,nxt\r\n;uses a\r\n ld hl,th.start+st*th.size\r\n ld de,th.start+en*th.size\r\n ld bc,th.size\r\n for nxt\r\n  all.skip Class\r\nendm\r\n\r\n\r\nmacro all.skip.blank.self\r\n ; skip blank\r\n  ; TODO th.ofs.stp\r\n  call th.isblank.a\r\n  continue z\r\n  ; skip hl==this\r\n  getthis 0\r\n  cp h\r\n  continue z\r\nendm\r\nmacro all.skip.isnot,Class\r\n  ; skip object not instance of *Class*\r\n  push hl\r\n  ld a,h\r\n  ld de,Class\r\n  call instanceof\r\n  getthis 0\r\n  pop hl\r\n  continue nz\r\nendm\r\nmacro all.skip, Class\r\n all.skip.blank.self 0\r\n all.skip.isnot Class\r\nendm\r\n\r\ndefsub crashToC\r\n ;before:\r\n ; call crashTo.setXY\r\n ; const cr.class,Class\r\n ; hl start\r\n ; de end\r\n ld bc,th.size\r\n for fe\r\n  all.skip.blank.self 0\r\n  ; skip object not instance of *Class*\r\n  push hl\r\n  ld a,h\r\n  ldconst de,cr.class\r\n  call instanceof\r\n  pop hl\r\n  continue nz\r\n  ; do crashTo1\r\n  getthis 0\r\n  call crashTo1\r\n  break c\r\n  continue\r\n fe:\r\n getthis 0\r\n ret c\r\n ld hl,null\r\nendsub all\r\n\r\ndefsub crashTo1\r\n ;hl=tg\r\n ;cy:true\r\n ;hl is used\r\n push af\r\n ld a,h\r\n tgconst t1\r\n tgconst t2\r\n pop af\r\n tgconst.g hl,t1,.x\r\n ldconst bc,gx\r\n subhl bc\r\n call abs\r\n ld bc,crashTo.size\r\n subhl bc\r\n ret nc\r\n\r\n tgconst.g hl,t2,.y\r\n ldconst bc,gy\r\n subhl bc\r\n call abs\r\n ld bc,crashTo.size\r\n subhl bc\r\nendsub crashTo1\r\n\r\nendp\r\n\r\n\r\nmacro tnu.run,Main\r\n ld sp,sp.ini\r\n call screen2\r\n \r\n showsp\r\n showlb endusr\r\n call spr.inipat\r\n call bg.inipat\r\n\r\n ld hl,th.start\r\n ld (hl),0cdh\r\n ld de,th.start+1\r\n ld bc,th.size*th.count-1\r\n ldir\r\n \r\n call th.init\r\n ;call mus.ini\r\n new Main, 0\r\n movw (h.thlop),spr.puts\r\n movw (h.thent),keyall\r\n jp th.loop\r\nendm\r\n\r\n;aaaa'].join(''),'key': ['include debug\r\n\r\ndefsub keyall\r\nproc\r\n;show hl\r\n local lp\r\n ld hl,keymat1\r\n ld de,keymat2\r\n ld bc,11\r\n ldir\r\n ld a,0\r\n ld hl,keymat1\r\n lp:\r\n push af\r\n call SNSMAT.a\r\n xor 255\r\n ld (hl),a\r\n pop af\r\n inc hl\r\n inc a\r\n cp 11\r\n jr c,lp\r\nendp\r\nendsub keyall\r\n\r\ndefwork keymat1\r\nds 11\r\nendwork keymat1\r\ndefwork keymat2\r\nds 11\r\nendwork keymat2\r\n\r\n\r\nproc\r\ndefsub getkey.a\r\nlocal chkmat\r\nex de,hl\r\nld hl,keymat1\r\ncall chkmat\r\nld hl,0\r\nret z\r\nld hl,keymat2\r\ncall chkmat\r\nld hl,1\r\nret z\r\ninc hl\r\nendsub getkey.a\r\n\r\ndefsub chkmat\r\npush de\r\nld a,d\r\nld d,0\r\nadd hl,de\r\nand (hl)\r\npop de\r\nendsub chkmat\r\n\r\ndefsub getkey\r\npush af\r\ncall getkey.a\r\npop af\r\nendsub getkey\r\n\r\nendp'].join(''),'map': ['include sub\r\ninclude math\r\ninclude tnu\r\n\r\ndefsub map.adr\r\n ; hl=chipx\r\n ; de=chipy\r\n rept 5\r\n  slde 0\r\n endm\r\n add hl,de\r\n ld de,1800h\r\n add hl,de\r\nendsub map.adr\r\n\r\ndefsub map.set.a\r\n ;  a=data\r\n call map.adr\r\n call 4dh\r\nendsub map.set.a\r\n\r\ndefsub map.get.a\r\n call map.adr\r\n call 4ah \r\nendsub map.get.a\r\n\r\ndefsub map.adrat.a\r\n ; hl=spr_x\r\n ; de=spr_y\r\n spr.unscale 0\r\n srl a\r\n srl a\r\n srl a\r\n push af\r\n ex de,hl\r\n spr.unscale 0\r\n srl a\r\n srl a\r\n srl a\r\n ld d,0\r\n ld e,a\r\n pop hl\r\n ld l,h\r\n ld h,0\r\n inc hl\r\n inc de\r\n call map.adr\r\nendsub map.adrat.a\r\n\r\ndefsub map.getat.a\r\n call map.adrat.a\r\n call 4ah\r\nendsub map.getat.a\r\n\r\ndefsub map.setat.a\r\n ; a=data\r\n push af\r\n call map.adrat.a\r\n pop af\r\n call 4dh\r\nendsub map.setat.a\r\n\r\ndefsub locate\r\n ; hl=chipx\r\n ; de=chipy\r\n call map.adr\r\n ld (cursor-2),hl\r\nendsub locate\r\n'].join(''),'maze': [].join(''),'t1': ['org 09000h\r\njp main\r\ninclude const\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude tnu\r\ninclude sp\r\n\r\n;===your code \r\n\r\nright:dw 0\r\n\r\nmain:\r\ntnu.run Main\r\ndef Main.main,0,0\r\nnew.arg .vx,1\r\nnew.arg .vy,0\r\nnew.arg .x,0\r\nnew.arg .y,100\r\nnew Cat,4\r\n\r\nnew.arg .x,100\r\nnew.arg .y,100\r\nnew Target,2\r\n\r\nld (right),hl\r\nld a,h\r\nld de,RActor\r\ncall instanceof\r\ncall showz\r\n\r\nld a,(right+1)\r\nld de,Target\r\ncall instanceof\r\ncall showz\r\n\r\nld a,(right+1)\r\nld de,Cat\r\ncall instanceof\r\ncall showz\r\n\r\n\r\nld hl,1\r\nsetfld .c\r\nenddef 0\r\n\r\nclass Main,RActor\r\n RActor.noovr Main\r\n end.const Main\r\nclass Target,RActor\r\n RActor.noovr Target\r\n met2 Target,.push\r\n end.const Target\r\ndef Target.main,0,0\r\nenddef\r\nclass Cat,RActor\r\n RActor.noovr Cat\r\n fld .vy, 0\r\n fld .vx, 0\r\n fld.bottom Cat\r\n end.const Cat\r\ndef Cat.main,0,0\r\n blp:\r\n  ld hl,0108h\r\n  call getkey\r\n  jpf nomov\r\n  getthis\r\n  ;x+=vx\r\n  getfld .x\r\n  ex de, hl\r\n  getfld .vx\r\n  add hl,de\r\n  setfld .x\r\n  nomov:\r\n  ; y+=vy\r\n  getfld .y\r\n  ex de, hl\r\n  getfld .vy\r\n  add hl,de\r\n  setfld .y\r\n  ld hl,(right)\r\n  push hl\r\n  invoke .crashTo\r\n  jpf cr\r\n   ; r.x+=10\r\n   ld hl,(right)\r\n   getfldtg .x\r\n   ld de,10\r\n   add hl,de\r\n   push hl\r\n   ld hl,(right)\r\n   setfldtg .x\r\n   ; r.push()\r\n   pushthis 0\r\n   ld hl,(right)\r\n   invoketg.a .push\r\n   popthis 0\r\n  cr:\r\n  invoke .update\r\n jp blp\r\nenddef\r\n; test t1\r\ndef Target.push,0,0\r\n ld hl,3\r\n setfld .p\r\n repti 30,pse\r\n  getfld .x\r\n  inc hl\r\n  setfld .x\r\n  invoke .update\r\n  continue\r\n pse:\r\n ld hl,0\r\n setfld .p\r\nenddef\r\n\r\nendusr: \r\ninclude sprpat\r\n\r\nend main\r\nhttps://msxpen.com/codes/-N6DDfMvZq9aUeJ9JLpN\r\nhttps://msxpen.com/codes/-N6QGYk-rr5iDuTtHpF7'].join(''),'t2': ['org 08000h\r\ninclude tnu\r\ninclude bool\r\ninclude map\r\n\r\nmain:\r\ntnu.run Main\r\n;range 0-5\r\nclass EBullet,RActor\r\n meth EBullet,main\r\n fld .x,0\r\n fld .y,0\r\n fld .p,0\r\n fld .c,0\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut\r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld .vx,0\r\n fld .vy,0\r\n end.const 0\r\ndef EBullet.main,0,0\r\n ;p=7;\r\n ld hl,7\r\n setfld .p\r\n ;c=15;\r\n ld hl,15\r\n setfld .c\r\n lb1:\r\n ld hl,true\r\n jpf lb2\r\n ;x+=vx;\r\n getfld .vx\r\n push hl\r\n getfld .x\r\n pop de\r\n add hl, de\r\n setfld .x\r\n ;y+=vy;\r\n getfld .vy\r\n push hl\r\n getfld .y\r\n pop de\r\n add hl, de\r\n setfld .y\r\n invoke .screenOut\r\n jpf lb4\r\n ;die();\r\n invoke .die\r\n jp lb3\r\n lb4:\r\n lb3:\r\n ;update();\r\n invoke .update\r\n jp lb1\r\n lb2:\r\n ret\r\n;range 5-15\r\nclass Enemy,RActor\r\n meth Enemy,main\r\n fld .x,0\r\n fld .y,0\r\n fld .p,0\r\n fld .c,0\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut\r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld .vx,0\r\n fld .bvx,0\r\n fld .bvy,0\r\n fld .bdist,0\r\n meth Enemy,fire\r\n end.const 0\r\ndef Enemy.main,0,0\r\n lb5:\r\n ld hl,200\r\n push hl\r\n getfld .y\r\n pop de\r\n call hlltde\r\n jpf lb6\r\n ;y+=2;\r\n ld hl,2\r\n push hl\r\n getfld .y\r\n pop de\r\n add hl, de\r\n setfld .y\r\n ;update();\r\n invoke .update\r\n jp lb5\r\n lb6:\r\n ld hl,(gbl_player)\r\n getfldtg .x\r\n push hl\r\n getfld .x\r\n pop de\r\n call hlgtde\r\n jpf lb8\r\n ;vx=0-2;\r\n ld hl,2\r\n push hl\r\n ld hl,0\r\n pop de\r\n subhl de\r\n setfld .vx\r\n jp lb7\r\n lb8:\r\n ;vx=2;\r\n ld hl,2\r\n setfld .vx\r\n lb7:\r\n ;fire();\r\n invoke .fire\r\n lb9:\r\n ld hl,true\r\n jpf lb10\r\n invoke .screenOut\r\n jpf lb12\r\n ;die();\r\n invoke .die\r\n jp lb11\r\n lb12:\r\n lb11:\r\n ;y+=2;\r\n ld hl,2\r\n push hl\r\n getfld .y\r\n pop de\r\n add hl, de\r\n setfld .y\r\n ;x+=vx;\r\n getfld .vx\r\n push hl\r\n getfld .x\r\n pop de\r\n add hl, de\r\n setfld .x\r\n ;update();\r\n invoke .update\r\n jp lb9\r\n lb10:\r\n ret\r\ndef Enemy.fire,0,0\r\n ;bvx=$player.x-x;\r\n getfld .x\r\n push hl\r\n ld hl,(gbl_player)\r\n getfldtg .x\r\n pop de\r\n subhl de\r\n setfld .bvx\r\n ;bvy=$player.y-y;\r\n getfld .y\r\n push hl\r\n ld hl,(gbl_player)\r\n getfldtg .y\r\n pop de\r\n subhl de\r\n setfld .bvy\r\n ;bdist=abs(bvx)+abs(bvy);\r\n getfld .bvy\r\n call abs\r\n push hl\r\n getfld .bvx\r\n call abs\r\n pop de\r\n add hl, de\r\n setfld .bdist\r\n ;bdist/=8;\r\n pushthis\r\n getfld .bdist\r\n push hl\r\n ld hl,8\r\n pop de\r\n call IDIV.a\r\n popthis\r\n setfld .bdist\r\n ;bvx/=bdist;\r\n pushthis\r\n getfld .bvx\r\n push hl\r\n getfld .bdist\r\n pop de\r\n call IDIV.a\r\n popthis\r\n setfld .bvx\r\n ;bvy/=bdist;\r\n pushthis\r\n getfld .bvy\r\n push hl\r\n getfld .bdist\r\n pop de\r\n call IDIV.a\r\n popthis\r\n setfld .bvy\r\n ;new EBullet{        x,y,vx:bvx,vy:bvy    };\r\n getfld .x\r\n new.arg .x\r\n getfld .y\r\n new.arg .y\r\n getfld .bvx\r\n new.arg .vx\r\n getfld .bvy\r\n new.arg .vy\r\n new EBullet,4,0,5\r\n ret\r\n;range 0-5\r\nclass Main,RActor\r\n meth Main,main\r\n fld .x,0\r\n fld .y,0\r\n fld .p,0\r\n fld .c,0\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut\r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld .i,0\r\n end.const 0\r\ndef Main.main,0,0\r\n ;$player=new Player{x:256, y: 300, p:$pat_spr+4, c:15};\r\n ld hl,256\r\n new.arg .x\r\n ld hl,300\r\n new.arg .y\r\n ld hl,4\r\n push hl\r\n ld hl,(gbl_pat_spr)\r\n pop de\r\n add hl, de\r\n new.arg .p\r\n ld hl,15\r\n new.arg .c\r\n new Player,4,0,5\r\n ld (gbl_player),hl\r\n ld hl,0\r\n setfld .i\r\n lb13:\r\n ld hl,20\r\n push hl\r\n getfld .i\r\n pop de\r\n call hlltde\r\n jpf lb14\r\n ;new Enemy{x:rnd(512), y:0, p:5, c:7};\r\n ld hl,512\r\n call rnd\r\n new.arg .x\r\n ld hl,0\r\n new.arg .y\r\n ld hl,5\r\n new.arg .p\r\n ld hl,7\r\n new.arg .c\r\n new Enemy,4,5,15\r\n ;updateEx(30);\r\n ld hl,30\r\n push hl\r\n invoke .updateEx\r\n jp lb13\r\n lb14:\r\n ret\r\n;range 15-20\r\nclass PBullet,RActor\r\n meth PBullet,main\r\n fld .x,0\r\n fld .y,0\r\n fld .p,0\r\n fld .c,0\r\n meth PBullet,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut\r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld .e,0\r\n end.const 0\r\ndef PBullet.main,0,0\r\n ;p=6;\r\n ld hl,6\r\n setfld .p\r\n ;c=8;\r\n ld hl,8\r\n setfld .c\r\n lb15:\r\n ld hl,true\r\n jpf lb16\r\n invoke .screenOut\r\n jpf lb18\r\n ;die();\r\n invoke .die\r\n jp lb17\r\n lb18:\r\n lb17:\r\n ;y-=6;\r\n ld hl,6\r\n push hl\r\n getfld .y\r\n pop de\r\n subhl de\r\n setfld .y\r\n crashToClass Enemy, 5, 15\r\n setfld .e\r\n getfld .e\r\n jpf lb20\r\n ;e.die();\r\n pushthis 0\r\n getfld .e\r\n invoketg.a .die\r\n popthis 0\r\n ;die();\r\n invoke .die\r\n jp lb19\r\n lb20:\r\n lb19:\r\n ;update();\r\n invoke .update\r\n jp lb15\r\n lb16:\r\n ret\r\ndef PBullet.onUpdate,0,0\r\n ;c+=1;\r\n ld hl,1\r\n push hl\r\n getfld .c\r\n pop de\r\n add hl, de\r\n setfld .c\r\n ld hl,14\r\n push hl\r\n getfld .c\r\n pop de\r\n call hlgtde\r\n jpf lb22\r\n ;c=0;\r\n ld hl,0\r\n setfld .c\r\n jp lb21\r\n lb22:\r\n lb21:\r\n ret\r\n;range 0-5\r\nclass Player,RActor\r\n meth Player,main\r\n fld .x,0\r\n fld .y,0\r\n fld .p,0\r\n fld .c,0\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut\r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n end.const 0\r\ndef Player.main,0,0\r\n lb23:\r\n ld hl,3\r\n ld de,10\r\n call locate\r\n getfld .y\r\n ld a,l\r\n call wrt\r\n ld a,h\r\n call wrt\r\n getthis 0\r\n \r\n ex de,hl\r\n getfld .x\r\n ld a,35\r\n call map.setat.a\r\n getthis 0\r\n ld hl,true\r\n jpf lb24\r\n ld hl, 4104\r\n call getkey\r\n jpf lb26\r\n ;x-=3;\r\n ld hl,3\r\n push hl\r\n getfld .x\r\n pop de\r\n subhl de\r\n setfld .x\r\n jp lb25\r\n lb26:\r\n lb25:\r\n ld hl, 32776\r\n call getkey\r\n jpf lb28\r\n ;x+=3;\r\n ld hl,3\r\n push hl\r\n getfld .x\r\n pop de\r\n add hl, de\r\n setfld .x\r\n jp lb27\r\n lb28:\r\n lb27:\r\n ld hl, 8200\r\n call getkey\r\n jpf lb30\r\n ;y-=3;\r\n ld hl,3\r\n push hl\r\n getfld .y\r\n pop de\r\n subhl de\r\n setfld .y\r\n jp lb29\r\n lb30:\r\n lb29:\r\n ld hl, 16392\r\n call getkey\r\n jpf lb32\r\n ;y+=3;\r\n ld hl,3\r\n push hl\r\n getfld .y\r\n pop de\r\n add hl, de\r\n setfld .y\r\n jp lb31\r\n lb32:\r\n lb31:\r\n crashToClass Enemy, 5, 15\r\n jpf lb34\r\n ;die();\r\n invoke .die\r\n jp lb33\r\n lb34:\r\n lb33:\r\n crashToClass EBullet, 0, 5\r\n jpf lb36\r\n ;die();\r\n invoke .die\r\n jp lb35\r\n lb36:\r\n lb35:\r\n ld hl,1\r\n push hl\r\n ld hl, 264\r\n call getkey\r\n pop de\r\n call hleqde\r\n jpf lb38\r\n ;new PBullet{x,y};\r\n getfld .x\r\n new.arg .x\r\n getfld .y\r\n new.arg .y\r\n new PBullet,2,15,20\r\n foreach Enemy,5,14,nxx\r\n  ld a,h\r\n  ld hl,8\r\n  setfld .c\r\n  continue \r\n nxx:\r\n getthis 0\r\n \r\n \r\n jp lb37\r\n lb38:\r\n lb37:\r\n ;update();\r\n invoke .update\r\n jp lb23\r\n lb24:\r\n ret\r\nenddef 0\r\nendusr:\r\ngbl_player:dw 0\r\ngbl_pat_spr:dw 0\r\nspr.inipat:\r\n ld de,3800h\r\n ld hl,spr.pat\r\n ld bc,2048\r\n jp LDIRVM\r\nspr.pat:\r\ndb $c,$e,$f,$4f,$3d,$1d,$7f,$1b,$c,$3f,$7f,$7f,$6f,$f,$6,$c\r\ndb $18,$38,$f8,$f9,$de,$dc,$7f,$6c,$98,$fc,$fe,$fe,$f6,$f0,$60,$70\r\ndb $c,$e,$f,$4f,$3d,$1d,$7f,$1b,$c,$3f,$7f,$7f,$ef,$ef,$6,$6\r\ndb $18,$38,$f8,$f9,$de,$dc,$7f,$6c,$98,$fc,$fe,$fe,$d4,$78,$f0,$0\r\ndb $18,$1c,$1f,$9f,$7b,$3b,$fe,$36,$19,$3f,$7f,$7f,$2b,$1e,$f,$0\r\ndb $30,$70,$f0,$f2,$bc,$b8,$fe,$d8,$30,$fc,$fe,$fe,$f7,$f7,$60,$60\r\ndb $2c,$8e,$f,$4b,$3d,$11,$7f,$1d,$ca,$ff,$7f,$3f,$15,$1f,$e,$0\r\ndb $1c,$39,$f8,$e9,$de,$c4,$7f,$5c,$ab,$ff,$ff,$fe,$ac,$f8,$70,$0\r\ndb $1,$1,$1,$2,$3,$92,$96,$f6,$f6,$fe,$fe,$fe,$ff,$af,$26,$0\r\ndb $80,$80,$80,$40,$c0,$49,$69,$6f,$6f,$7f,$7f,$7f,$ff,$f5,$64,$0\r\ndb $8,$10,$30,$30,$18,$f,$3f,$37,$7b,$7d,$3f,$1f,$37,$60,$70,$c\r\ndb $8,$4,$6,$6,$c,$f8,$fe,$f6,$ef,$df,$fe,$fc,$f6,$3,$7,$18\r\ndb $0,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$0,$0\r\ndb $80,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$80,$0\r\ndb $0,$0,$0,$0,$3,$f,$f,$1f,$1f,$1f,$1f,$f,$f,$3,$0,$0\r\ndb $0,$0,$0,$0,$c0,$f0,$f0,$f8,$f8,$f8,$f8,$f0,$f0,$c0,$0,$0\r\nbg.inipat:\r\n ret\r\n\r\n\r\nend main'].join(''),'t3': ['org 09000h\r\n\r\ninclude tnu\r\ninclude mus\r\ninclude sprpat\r\n\r\n;===your code \r\n\r\nmain:\r\nld sp,(8000h)\r\n\r\ncall screen1\r\n\r\nshowsp\r\nshowlb endusr\r\n\r\nld hl,8000h\r\nld (hl),0cdh\r\nld de,8001h\r\nld bc,th.size*th.count-1\r\nldir\r\n\r\n\r\ncall th.init\r\ncall spr.inipat\r\n;call mus.ini\r\n\r\n\r\nnew Main, 0\r\nshow hl\r\n\r\n\r\nmovw (h.thlop),spr.puts\r\njp th.loop\r\n\r\nclass Main, RActor\r\n meth Main,main\r\n fld.bottom Object\r\n fld .x,100\r\n fld .y,300\r\n fld .p,0\r\n fld .c,3\r\n fld.bottom Sprite\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut \r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld.bottom RActor\r\n fld.bottom Main\r\n end.const\r\n\r\nMain.main:\r\n olp:\r\n  getthis\r\n  invoke .update\r\n  call xrnd.a\r\n  ld a,h\r\n  and 15\r\n  jr nz,doap\r\n   getthis\r\n   getfld .x\r\n   new.arg .x\r\n   getfld .y\r\n   new.arg .y\r\n   ld hl,7\r\n   call rnd.a\r\n   ld de,3\r\n   sbc hl,de\r\n   new.arg .vx\r\n   ld hl,5\r\n   call rnd.a\r\n   ld de,15\r\n   sbc hl,de\r\n   new.arg .vy\r\n   new Bullet, 4\r\n   call dstk\r\n  doap:\r\n  ld a,8\r\n  call SNSMAT.a\r\n  and 1\r\n  jr z,golf\r\n  \r\n  getthis\r\n  getfld .x\r\n  inc hl\r\n  inc hl\r\n  setfld .x\r\n  ld de,400\r\n  cpde.a\r\n  jp c, olp\r\n  golf:\r\n  ld hl,0\r\n  getthis\r\n  setfld .x\r\n jp olp\r\n\r\n\r\nclass Bullet,RActor\r\n meth Bullet,main\r\n fld.bottom Object\r\n fld .x, 0\r\n fld .y, 0\r\n fld .p, 2\r\n fld .c, 15\r\n fld.bottom Sprite\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut \r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld.bottom RActor\r\n fld .vy, -10\r\n fld .vx, 0\r\n fld.bottom Bullet\r\n end.const \r\n \r\nBullet.main:\r\n blp:\r\n  getthis\r\n  ;x+=vx\r\n  getfld .x\r\n  ex de, hl\r\n  getfld .vx\r\n  add hl,de\r\n  setfld .x\r\n  ; y+=vy\r\n  getfld .y\r\n  ex de, hl\r\n  getfld .vy\r\n  add hl,de\r\n  setfld .y\r\n  getfld .vy\r\n  inc hl\r\n  setfld .vy\r\n\r\n  invoke .update\r\n  invoke .screenOut\r\n  jp c, bdie\r\n  getfld .vy\r\n  bit 7,h\r\n  jr nz,blp\r\n  ld de,5\r\n  cpde.a\r\n  jr c,blp\r\n \r\n  call dstk\r\n  getthis\r\n  ld hl,3\r\n  setfld .p\r\n  pushi 10,bc\r\n  invoke .updateEx\r\n\r\n bleft:\r\n  getthis\r\n  ld hl,2\r\n  setfld .p\r\n  getfld .x\r\n  dec hl\r\n  dec hl\r\n  setfld .x\r\n  getfld .y\r\n  dec hl\r\n  setfld .y\r\n  invoke .update\r\n  invoke .screenOut\r\n  jr c, bdie\r\n  jr bleft\r\n bdie:\r\n  invoke .die\r\n  ret \r\n\r\n  \r\ndstk:\r\n push af\r\n ld hl,th.start+256*3\r\n getthis\r\n ld h,a\r\n ld de,1900h\r\n ld bc,256\r\n call LDIRVM\r\n pop af\r\n ret\r\n \r\nendusr:\r\nend main'].join(''),'t4': ['org 09000h\r\njp main\r\ninclude const\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude tnu\r\ninclude sp\r\n\r\n;===your code \r\n\r\nright:dw 0\r\n\r\nmain:\r\ntnu.run Main\r\ndef Main.main,0,0\r\nnew.arg .vx,1\r\nnew.arg .vy,0\r\nnew.arg .x,0\r\nnew.arg .y,100\r\nnew Cat,4\r\n\r\nnew.arg .x,100\r\nnew.arg .y,100\r\nnew Target,2\r\n\r\nnew.arg .x,200\r\nnew.arg .y,100\r\nnew Target,2\r\n\r\n\r\nnew.arg .x,150\r\nnew.arg .y,100\r\nnew.arg .c,8\r\nnew NTarget,3\r\n\r\nld (right),hl\r\nld a,h\r\nld de,Actor\r\ncall instanceof\r\ncall showz\r\n\r\nld a,(right+1)\r\nld de,Target\r\ncall instanceof\r\ncall showz\r\n\r\nld a,(right+1)\r\nld de,Cat\r\ncall instanceof\r\ncall showz\r\n\r\n\r\nld hl,1\r\nsetfld .c\r\nenddef 0\r\n\r\nclass Main,Actor\r\n Actor.noovr Main\r\n end.const 0\r\nclass Target,Actor\r\n Actor.noovr Target\r\n met2 Target,.push\r\n end.const 0\r\nclass NTarget,Actor\r\n Actor.noovr NTarget\r\n end.const 0\r\ndef NTarget.main,0,0\r\n ret\r\nenddef\r\n\r\ndef Target.main,0,0\r\nenddef\r\nclass Cat,Actor\r\n Actor.noovr Cat\r\n fld .vy, 0\r\n fld .vx, 0\r\n fld.bottom Cat\r\n end.const 0\r\ndef Cat.main,0,0\r\n blp:\r\n  getthis\r\n  ;x+=vx\r\n  getfld .x\r\n  ex de, hl\r\n  getfld .vx\r\n  add hl,de\r\n  setfld .x\r\n  ; y+=vy\r\n  getfld .y\r\n  ex de, hl\r\n  getfld .vy\r\n  add hl,de\r\n  setfld .y\r\n  ld hl,Target\r\n  push hl\r\n  invoke .crashTo\r\n  jpf cr\r\n   ; r.x+=10\r\n   const setg,hl\r\n   getfldtg .y\r\n   ld de,30\r\n   add hl,de\r\n   push hl\r\n   ldconst hl,setg\r\n   setfldtg .y\r\n  cr:\r\n  invoke .update\r\n jp blp\r\nenddef\r\ndef Target.push,0,0\r\n ld hl,3\r\n setfld .p\r\n repti 30,pse\r\n  getfld .y\r\n  inc hl\r\n  setfld .y\r\n  invoke .update\r\n  continue\r\n pse:\r\n ld hl,0\r\n setfld .p\r\nenddef\r\n\r\nendusr: \r\nend main\r\nhttps://msxpen.com/codes/-N6DDfMvZq9aUeJ9JLpN\r\nhttps://msxpen.com/codes/-N6QGYk-rr5iDuTtHpF7'].join(''),'t5': ['org 9000h\r\n\r\n\r\ninclude key\r\n\r\nmain:\r\ncall keyall\r\nld hl,0108h\r\ncall getkey\r\nshow hl\r\nld hl,0107h\r\ncall getkey\r\nshow hl\r\n\r\n\r\nhalt\r\njp main'].join(''),'gen': ['org 09000h\r\ninclude tnu\r\ninclude bool\r\n\r\nmain:\r\ntnu.run Main\r\nclass Main,Actor\r\n Actor.noovr Main\r\n end.const 0\r\ndef Main.main,0,0\r\n\r\n showlb .main\r\n showlb .crashTo\r\nenddef 0\r\nendusr:\r\nend main'].join(''),'dac': ['org 09000h\r\njp main\r\ninclude const\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude th\r\n\r\n\r\nDECSUB equ 268CH;DAC ← DAC-ARG\r\nDECADD equ 269AH;DAC ← DAC+ARG\r\nDECNRM equ 26FAH;DAC を正規化する (*1)\r\nDECROU equ 273CH;DAC を四捨五入する\r\nDECMUL equ 27E6H;DAC ← DAC*DAC\r\nDECDIV equ 289FH;DAC ← DAC/DAC\r\nMAF equ 2C4DH;ARG ← DAC\r\nMAM equ 2C50H;ARG ← [HL]\r\nMOV8DH equ 2C53H;[DE] ← [HL]\r\nMFA equ 2C59H;DAC ← ARG\r\nMFM equ 2C5CH;[HL] ← DAC\r\nMMF equ 2C67H;[HL] ← DAC\r\nMOV8HD equ 2C6AH;[HL] ← [DE]\r\nXTF equ 2C6FH;[SP] ←→ DAC\r\nPHA equ 2CC7H;ARG → [SP]\r\nPHF equ 2CCCH;DAC → [SP]\r\nPPA equ 2CDCH;[SP] → ARG\r\nPPF equ 2CE1H;[SP] → DAC\r\nPUSHF equ 2EB1H;DAC → [SP]\r\nMOVFM equ 2EBEH;DAC ← [HL]\r\nMOVFR equ 2EC1H;DAC ← (CBED)\r\nMOVRF equ 2ECCH;(CBED) ← DAC\r\nMOVRMI equ 2ED6H;(CBDE) ← [HL]\r\nMOVRM equ 2EDFH;(BCDE) ← [HL]\r\nMOVMF equ 2EE8H;[HL] ← DAC\r\nMOVE equ 2EEBH;[HL] ← [DE]\r\nVMOVAM equ 2EEFH;ARG ← [HL]\r\nMOVVFM equ 2EF2H;[DE] ← [HL]\r\nVMOVE equ 2EF3H;[HL] ← [DE]\r\nVMOVFA equ 2F05H;DAC ← ARG\r\nVMOVFM equ 2F08H;DAC ← [HL]\r\nVMOVAF equ 2F0DH;ARG ← DAC\r\nVMOVMF equ 2F10H;[HL] ← DAC\r\n\r\nVALTYP equ 0F663H;1\r\nDAC equ 0F7F6H;16\r\nARG equ 0F847H;16\r\nFOUT equ 3425H\r\n\r\ndefsub int2dac\r\n push af\r\n ld a,2\r\n ld (VALTYP),a\r\n ld (DAC+2),HL\r\n pop af\r\nendsub int2dac\r\n;===your code \r\n\r\nmain:\r\nld hl,12345\r\ncall int2dac\r\nld hl,str\r\ncall FOUT\r\n\r\nld b,10\r\nreptb nxt\r\n ld a,(hl)\r\n cp 0\r\n break z\r\n call wrt2\r\n inc hl\r\n continue\r\nnxt:\r\nret\r\nstr:\r\n\r\n\r\n'].join(''),'setvrm': ['org 09000h\r\njp main\r\ninclude const\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude th\r\n\r\n;===your code \r\n\r\nmain:\r\nld hl,1800h\r\ncall SETWRT\r\nld a,35\r\nrepti 5, ed\r\ninc a\r\nout (98h),a\r\ncontinue\r\ned:\r\nret'].join(''),'assert': ['include mem\r\ninclude math\r\ninclude debug\r\n\r\na.reg.trc:\r\ndw 0\r\na.reg.adr:\r\ndw 0\r\na.reg.min:\r\ndw 0\r\na.reg.val:\r\ndw 0\r\na.reg.max:\r\ndw 0\r\nmacro a.regi,n,v\r\n push hl\r\n ld hl,v\r\n ld (a.reg.##n),hl\r\n pop hl\r\nendm\r\nmacro a.regr,n,v\r\n ld (a.reg.##n),v\r\nendm\r\n\r\nmacro a.dummy\r\n local a.reg.,trc,adr,min,val,nax\r\nendm\r\n \r\n\r\nmacro assert.eq,o\r\n storelastpc\r\n pushall \r\n if not nul o\r\n  a.regi val, o\r\n endif\r\n ld de,(a.reg.val)\r\n ld(a.reg.val),hl\r\n ld(a.reg.min),de\r\n ld(a.reg.max),de\r\n cpde\r\n jp nz,assert.fail\r\n popall\r\nendm\r\n\r\nmacro assert.do,nx\r\n storelastpc\r\n pushall\r\n call to\r\n popall\r\n jr nx\r\n to:\r\nendm\r\n\r\nmacro storelastpc\r\n push hl\r\n call getpc\r\n ld (lastpc),hl\r\n pop hl\r\nendm\r\nlastpc:\r\n dw 0\r\n \r\ngetpc:\r\n pop hl\r\n push hl\r\n ret\r\n\r\nassert.fail:\r\n ld hl,0deadh\r\n show hl\r\n showm a.reg.trc\r\n showm a.reg.min\r\n showm a.reg.val\r\n showm a.reg.max\r\n showm a.reg.adr\r\n showm lastpc\r\n call freeze\r\nmacro assert.meqw,ad,val\r\n a.regi adr,ad\r\n push hl\r\n ld hl,(ad)\r\n assert.eq val\r\n pop hl\r\nendm\r\n '].join(''),'stksz': ['org 09000h\r\njp main\r\ninclude const\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude th\r\n\r\n\r\n;===your code \r\n\r\nsz equ 256\r\n  \r\nmain:\r\nld hl,0fd9fh\r\nld (hl),0c9h\r\n rept sz/2\r\n  push hl\r\n endm\r\n rept sz/2\r\n  pop hl\r\n endm\r\n\r\nloop:\r\n getsp\r\n ld de,-sz\r\n add hl,de\r\n ld de,1800h\r\n ld bc,sz\r\n call LDIRVM\r\n ld hl,0\r\n halt\r\n jp loop\r\n \r\n \r\n '].join(''),'vdp': [';https://www.msx.org/wiki/VDP_Status_Registers\r\n;st 0 bit 7\r\n;read 1\r\n\r\n;https://www.msx.org/wiki/VDP_Mode_Registers\r\n;ctrl 1 bit 5 set 0\r\ninclude const\r\n\r\nsusint:\r\n ld a,(RG1SAV)\r\n res 5,a\r\n ld b,A\r\n ld c,1\r\n jp WRTVDP\r\n;rstint:\r\n ld a,(RG1SAV)\r\n set 5,a\r\n ld b,A\r\n ld c,1\r\n jp WRTVDP\r\ninted:\r\n call RDVDP\r\n bit 7,a\r\n ret\r\ndoint:\r\n call inted\r\n jr z, norst\r\n ld hl,timecnt\r\n inc (hl)\r\n call h.tntimi\r\n norst:\r\n ;call rstint\r\n ret\r\nh.tntimi:\r\n ld A,(timecnt)\r\n ld hl,1ae0h\r\n call 4dh\r\n ret\r\n ds 16\r\n \r\ntimecnt:\r\ndb 0\r\nmacro vdptest\r\nlocal stk1,stk2,stk3,vl\r\nstk1:\r\n ds 256,35\r\nstk2:\r\n ds 256,42\r\nstk3:\r\n\r\nvl:\r\n call susint\r\n ld sp,stk2\r\n ld hl,stk1\r\n ld de,1800h\r\n ld bc,256\r\n call LDIRVM\r\n \r\n \r\n ld sp,stk3\r\n call doint\r\n ld hl,stk2\r\n ld de,1900h\r\n ld bc,256\r\n call LDIRVM\r\n jp vl\r\nendm\r\n \r\nscreen1:\r\n ld a,1\r\n call CHGMOD\r\n ld a,(RG1SAV)\r\n set 1,a\r\n ld b,A\r\n ld c,1\r\n call WRTVDP\r\n ret\r\n\r\ndefsub screen2\r\n ld a,2\r\n call CHGMOD\r\n ld a,(RG1SAV)\r\n set 1,a\r\n ld b,A\r\n ld c,1\r\n call WRTVDP\r\nendsub screen2\r\n '].join(''),'mus': ['include mem\r\nmus.ini:\r\n di\r\n ld hl,0fd9fh\r\n ld (hl),0c3h\r\n movw (0fd9fh+1),mus\r\n ei\r\n ret\r\nmus:\r\nproc\r\nlocal we\r\n push af\r\n push de\r\n ld a,(we-1)\r\n xor 15\r\n ld (we-1),a \r\n ld a,8\r\n ld e,15\r\n we:\r\n call WRTPSG\r\n pop af \r\n pop de\r\n ret\r\nendp'].join('')};
        
      },
      fiber$main :function* _trc_Asms_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.data = {'main': ['org 09000h\r\n\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude sp\r\ninclude vdp\r\ninclude th\r\n\r\n\r\n;===your code \r\n\r\nmain:\r\nld a,5\r\nlp:\r\ndec b\r\nshow a\r\niff nc,nx\r\njr lp\r\nnx:\r\n\r\nret\r\nend main'].join(''),'math': ['include ctrl\r\n\r\n;16bit shifts\r\nmacro slhl\r\n sla l\r\n rl h\r\nendm\r\nmacro srahl\r\n sra h\r\n rr l\r\nendm\r\nmacro srlhl\r\n srl h\r\n rr l\r\nendm\r\nmacro slde\r\n sla e\r\n rl d\r\nendm\r\nmacro srade\r\n sra d\r\n rr e\r\nendm\r\nmacro srlde\r\n srl d\r\n rr e\r\nendm\r\nmacro slbc\r\n sla c\r\n rl b\r\nendm\r\nmacro srabc\r\n sra b\r\n rr c\r\nendm\r\nmacro srlbc\r\n srl b\r\n rr c\r\nendm\r\n\r\n\r\n; for xrnd\r\nmacro sldehl,n\r\n  local loop\r\n  ld b,n\r\n loop:\r\n  sla d\r\n  rl e\r\n  rl h\r\n  rl l\r\n  djnz loop\r\nendm\r\nmacro srdehl,n\r\n local loop\r\n ld b,n\r\n loop:\r\n  srl l\r\n  rr h\r\n  rr e\r\n  rr d\r\n djnz loop\r\nendm\r\n \r\nmacro xorrm,re,me\r\n  ld A,(me)\r\n  xor re\r\n  ld (me),a\r\nendm\r\n\r\nmacro subhl,rp\r\n and a\r\n sbc hl,rp\r\nendm\r\n\r\nmacro cpde.a\r\n rst dcompr\r\nendm\r\n\r\n\r\nmarker.b xrnd.a\r\nxrnd.a:\r\nproc \r\n local rhl,rde,rdhlde\r\n ; s[0] ^= s[0] << 13\r\n call rdhlde\r\n sldehl 13\r\n call wrtxor\r\n ; s[0] ^= s[0] >> 17\r\n call rdhlde\r\n srdehl 17\r\n call wrtxor\r\n ; s[0] ^= s[0] << 5;\r\n call rdhlde\r\n sldehl 5\r\n call wrtxor\r\n ret\r\n \r\n rdhlde:\r\n  ld hl,1234\r\n rhl:\r\n  ld de,5678\r\n rde:\r\n  ret\r\n \r\n wrtxor:\r\n  xorrm h,rhl-1\r\n  xorrm l,rhl-2\r\n  xorrm d,rde-1\r\n  xorrm e,rde-2\r\n  ret\r\nendp\r\nmarker.e xrnd.a\r\n\r\n\r\nmarker.b rnd\r\nrnd:\r\n push af\r\n call rnd.a\r\n pop af\r\n ret\r\nmarker.e rnd\r\nmarker.b rnd.a\r\nrnd.a:\r\n ld de,07fffh\r\n call IDIV.a\r\n push hl\r\n call xrnd.a\r\n res 7,h\r\n ex de,hl\r\n pop hl\r\n inc hl\r\n call IDIV.a\r\n ret\r\nmarker.e rnd.a\r\n\r\nmarker.b abs\r\nabs:\r\n bit 7,h\r\n ret z\r\nneghl:\r\n ld de,0 \r\n ex de,hl \r\n subhl de\r\n ret\r\nmarker.e abs\r\n\r\n '].join(''),'bool': ['include math\r\ninclude ctrl\r\ntrue equ -1\r\nfalse equ 0\r\n\r\nmacro rethl,val,flg\r\n local tru\r\n if not nul flg\r\n  iff flg ,tru\r\n endif\r\n ld hl,val\r\n ret\r\n tru:\r\nendm\r\n\r\nhleqde:\r\n subhl de\r\n rethl true,z\r\n rethl false\r\n\r\nhlnede:\r\n subhl de\r\n rethl true,nz\r\n rethl false\r\n \r\nhlgtde:\r\n subhl de\r\n rethl false,z\r\n bit 7,h\r\n rethl true,z\r\n rethl false\r\n\r\nhlltde:\r\n subhl de\r\n bit 7,h\r\n rethl true,nz\r\n rethl false\r\n \r\nhlgede:\r\n subhl de\r\n rethl true,z\r\n bit 7,h\r\n rethl true,z\r\n rethl false\r\n\r\nhllede:\r\n subhl de\r\n rethl true,z\r\n bit 7,h\r\n rethl true,nz\r\n rethl false\r\n \r\nproc\r\nziffalse:\r\n local resa\r\n ld (resa-1),a\r\n call ziffalse.a\r\n ld A,0\r\n resa:\r\n ret\r\nziffalse.a:\r\n ld a,0\r\n cp h\r\n ret nz\r\n cp l\r\n ret\r\nendp\r\n\r\nmacro jpf,to\r\n call ziffalse\r\n jp z,to\r\nendm\r\n\r\nmacro andand,fls\r\n jpf fls\r\nendm\r\nmacro oror,tru\r\n call ziffalse\r\n jp nz,tru\r\nendm\r\n\r\nmacro flagtobool,fl\r\n local yes,skp\r\n jr fl, yes\r\n ld hl,false\r\n jr skp\r\n yes:\r\n ld hl,true\r\n skp: \r\nendm'].join(''),'mem': ['include const\r\n;\r\nrdslt:\r\n ex de,hl\r\n rept 5\r\n srl d;page*2\r\n endm\r\n CALL RSLREG\r\n ld e,d\r\n rdslt1:\r\n  RRCA\r\n  dec e\r\n  jp nz,rdslt1\r\n AND    00000011B\r\n LD C,A;000000Pr\r\n LD B,0\r\n LD HL,EXPTBL\r\n ADD HL,BC\r\n LD C,A;000000Pr\r\n LD A,(HL)\r\n AND 80H;expand flag\r\n OR C\r\n LD C,A;e00000Pr\r\n rept 4;const\r\n INC HL\r\n endm\r\n LD A,(HL);exp reg\r\n ld e,d\r\n rdslt2:\r\n  srl a\r\n  dec e\r\n  jp nz,rdslt2\r\n;000000Ex\r\n sla a\r\n sla a\r\n ;    0000Ex00\r\n and  00001100b\r\n OR C;e000ExPr\r\n ret\r\nmemini:\r\n CALL RSLREG\r\n rept 4\r\n  RRCA\r\n endm\r\n AND    00000011B\r\n LD C,A;000000Pr\r\n LD B,0\r\n LD HL,EXPTBL\r\n ADD HL,BC\r\n LD C,A;000000Pr\r\n LD A,(HL)\r\n AND 80H;expand flag\r\n OR C\r\n LD C,A;e00000Pr\r\n rept 4;const\r\n INC HL\r\n endm\r\n LD A,(HL);exp reg\r\n rept 4; page*2\r\n srl a\r\n endm;000000Ex\r\n sla a\r\n sla a\r\n ;    0000Ex00\r\n and  00001100b\r\n OR C;e000ExPr\r\n LD Hl,04000H\r\n jp ENASLT\r\n\r\nmacro peekw ,regv,regm\r\n  local w\r\n  ld (w-2),regm\r\n  ld regv,(0)\r\n  w:\r\nendm\r\n\r\nmacro pokew ,regm,regv\r\n  local w\r\n  ld (w-2),regm\r\n  ld (0),regv\r\n  w:\r\nendm\r\nmacro movw,dst,src,rp\r\n if nul rp\r\n  push hl\r\n  movw dst,src,hl\r\n  pop hl\r\n else\r\n  ld rp,src\r\n  ld dst,rp\r\n endif \r\nendm\r\n\r\nmacro popa\r\n  ex (sp),hl\r\n  ld a,h\r\n  pop HL\r\nendm\r\n\r\nmacro pushall\r\n push af\r\n push bc\r\n push de\r\n push hl\r\nendm\r\nmacro popall\r\n pop hl\r\n pop de\r\n pop bc\r\n pop af\r\nendm\r\n \r\n\r\nmacro pushi, n,rp\r\n local rrr\r\n if nul rp\r\n  ld (rrr-2),hl\r\n  ld hl,n\r\n  push hl\r\n  ld hl,0\r\n  rrr:\r\n else\r\n  ld rp,n\r\n  push rp\r\n endif\r\nendm\r\nmacro const,n,reg\r\n ld (n-2),reg\r\nendm\r\nmacro ldconst,reg,n\r\n ld reg,0\r\n n:\r\nendm\r\nmacro peekconst,reg,n\r\n ld reg,(0)\r\n n:\r\nendm\r\n'].join(''),'const': ['\r\n;wrt equ 0a2h\r\ndcompr equ 0020H\r\nsp.ini equ 0dc00h\r\nstksize equ 512\r\n\r\nth.size equ 256\r\nth.count equ 20\r\nth.start equ th.end-th.size*th.count\r\nth.end equ sp.ini-stksize\r\n\r\nth.bottom equ 0\r\n\r\nspr.scale equ 1\r\nspr.xmax equ 256<<spr.scale\r\nspr.ymax equ 192<<spr.scale\r\n\r\nENASLT EQU 0024H\r\nRSLREG EQU 0138H\r\nEXPTBL EQU 0FCC1H\r\nSETWRT equ 0053H\r\nLDIRVM equ 005CH\r\nWRTVDP equ 0047H\r\nRG1SAV equ 0F3E0H\r\nRDVDP  equ 013EH\r\nSNSMAT.a equ 0141h\r\n\r\nCHGMOD equ 005FH\r\n\r\nIMULT.a equ 3193H;HL ← DE*HL\r\nIDIV.a equ 31E6H;HL ← DE/HL\r\nIMOD.a equ 323AH;HL ← DE mod HL (DE ← DE/HL) \r\n\r\nWRTPSG  equ 0093H\r\n\r\nCSRY equ 0F3DCH\r\nCSRX equ 0F3DDH\r\n\r\nnull equ 0\r\n\r\nmacro marker.b, n\r\n last.marker: defl $\r\nendm\r\nmacro marker.e, n\r\n len.##n: defl $-last.marker\r\nendm\r\n'].join(''),'ctrl': ['include const\r\nfreeze:\r\nhalt\r\njr freeze\r\n\r\nmacro for ,lbend\r\n ; uses a\r\n ; c: breaked\r\n proc\r\n  local s,lb\r\n  lb:\r\n  call dcompr; uses a\r\n  jp nc,lbend\r\n  push HL\r\n  push de\r\n  push bc\r\n  call s\r\n  pop bc\r\n  pop de\r\n  pop HL\r\n  jp c,lbend\r\n  add HL,bc\r\n  jr lb\r\n  s:\r\n endp\r\nendm\r\n\r\nmacro repti ,n,lbend\r\n proc\r\n  local s,lb, lbend2\r\n  push bc\r\n  ld b,n\r\n  lb:\r\n  push bc\r\n  call s\r\n  pop bc\r\n  jr c,lbend2\r\n  djnz lb\r\n  lbend2:\r\n  pop bc\r\n  jp lbend \r\n  s:\r\n endp\r\nendm\r\n\r\n\r\nmacro reptb ,lbend\r\n  local s,lb\r\n inc b\r\n djnz lb\r\n jp lbend\r\n lb:\r\n  push bc\r\n  call s\r\n  pop bc\r\n  jp c,lbend\r\n djnz lb\r\n jp lbend \r\n s:\r\nendm\r\n\r\n\r\n\r\nmacro callsva,pp\r\n local sva\r\n ld (sva-1),a\r\n call pp\r\n ld a,0\r\n sva:\r\nendm\r\nbcis0:\r\n callsva bcis0.a\r\n ret\r\nbcis0.a:\r\n ld a,b\r\n and a\r\n ret nz\r\n ld a,c\r\n and a\r\n ret\r\n\r\nmacro reptbc ,lbend\r\n local s,lb\r\n call bcis0\r\n jp z,lbend \r\n lb:\r\n  push bc\r\n  call s\r\n  pop bc\r\n  jp c,lbend\r\n  dec bc\r\n  call bcis0\r\n jr nz, lb\r\n jp lbend \r\n s:\r\nendm\r\n\r\n\r\niff.NZ equ 0\r\niff.Z  equ 1\r\niff.NC equ 2\r\niff.C  equ 3\r\n\r\nmacro iff,cnd,to\r\n local iff.\r\n if iff.##cnd eq iff.NZ\r\n  jr z,to\r\n endif\r\n if iff.##cnd eq iff.Z\r\n  jr nz,to\r\n endif\r\n if iff.##cnd eq iff.NC\r\n  jr c,to\r\n endif\r\n if iff.##cnd eq iff.C\r\n  jr nc,to\r\n endif\r\n ;jr cnd, skip\r\n ;jr to\r\n ;skip:\r\nendm\r\n\r\nmacro break,cnd\r\n if NUL cnd\r\n  scf\r\n  ret\r\n else\r\n  proc \r\n   local jj\r\n   iff cnd ,jj\r\n   break\r\n  jj:\r\n  endp\r\n endif\r\nendm\r\nmacro continue,cnd\r\n if NUL cnd \r\n  or a\r\n  ret\r\n else\r\n  proc \r\n   local jj\r\n   iff cnd,jj\r\n   continue\r\n  jj:\r\n  endp\r\n endif\r\nendm\r\n\r\n\r\nmacro djnzr,reg, j\r\n dec reg\r\n jr NZ,j\r\nendm\r\n\r\nmacro callhl\r\n local LCD\r\n ld (LCD-2),HL\r\n call LCD\r\n LCD:\r\nendm\r\n\r\nmacro stride,lim,to\r\n if (low $)<lim\r\n  exitm\r\n endif\r\n ds 256+to-(low $),0cdh\r\nendm'].join(''),'th': ['include ctrl\r\ninclude sp\r\ninclude vdp\r\ninclude mem\r\ninclude math\r\ninclude debug\r\n\r\nth.ofs.stp equ 256-4\r\nth.ofs.sp equ th.ofs.stp+1\r\nth.ofs.spini equ th.ofs.stp\r\nfld.top equ th.ofs.spini-2\r\nth.st.blank equ 0c9h\r\nth.st.active equ 31h\r\n\r\nmacro th.for,lb\r\n ld HL,th.start\r\n ld de,th.end\r\n ld bc,th.size\r\n for lb\r\nendm\r\n\r\nmacro th.new.range,st,en\r\n ld bc,st\r\n ld(th.new.start),bc\r\n ld bc,en\r\n ld(th.new.end),bc\r\nendm\r\n\r\ndefsub th.isblank.a\r\n ; h= thread\r\n ; z if true\r\n ld l, th.ofs.stp\r\n ld a,(hl)\r\n cp th.st.blank\r\nendsub th.isblank.a\r\n\r\ndefsub th.new\r\n; nc for alloc fail\r\nproc \r\n local lbend\r\n db 21h\r\n th.new.start:\r\n dw th.start\r\n db 11h\r\n th.new.end:\r\n dw th.end\r\n ld bc,th.size\r\n for lbend\r\n  ; TODO th.ofs.stp\r\n  call th.isblank.a\r\n  break z\r\n  continue\r\n lbend:\r\n ret nc\r\n ; TODO th.ofs.stp\r\n ld L,th.ofs.stp\r\n ld (HL),31h\r\n inc HL\r\n ld (HL),th.ofs.spini\r\n ld a,h\r\n inc HL\r\n ld (hl),a\r\n inc HL\r\n ld (HL),0c9h\r\n ld l,th.bottom\r\n scf\r\n ret\r\nendp\r\nendsub th.new\r\n\r\ndefsub th.init\r\nproc\r\n local lbend\r\n th.for lbend\r\n  ; TODO th.ofs.stp\r\n  ld L, th.ofs.stp\r\n  ld (HL),th.st.blank\r\n  continue\r\n lbend:\r\n ; disable timer\r\n ld HL,0fd9fh\r\n ld (hl),0c9h\r\n call susint\r\n ret\r\nendp\r\nendsub th.init\r\n\r\ndefsub th.stepall\r\n th.for thnx\r\n  ;todo th.ofs.stp\r\n  ld (th.cur),hl\r\n  call th.isblank.a\r\n  continue z\r\n  call th.step\r\n  continue\r\n thnx:\r\nendsub th.stepall\r\n\r\ndefsub th.step\r\n sp2mem adrssp+1\r\n ld HL,(th.cur)\r\n ld l,th.ofs.stp\r\n ;call susint\r\n jp (hl)\r\nendsub th.step\r\n\r\ndefsub th.yield\r\n ld hl,(th.cur)\r\n ld l,th.ofs.sp\r\n sp2mem\r\n adrssp:\r\n ld sp,0\r\n jp doint\r\nendsub th.yield\r\n\r\ndefsub th.term\r\n ld hl,(th.cur)\r\n ; TODO th.ofs.stp\r\n ld L,th.ofs.stp\r\n ld (hl),th.st.blank\r\n jr adrssp\r\nendsub th.term\r\n\r\nmacro th.with.do, to\r\n local pr\r\n th.with pr\r\n jr to\r\n pr:\r\nendm\r\n\r\nmacro th.with.setdst, reg\r\n ld (th.jpdest-2),reg\r\nendm\r\nmacro th.with,pr\r\n movw (th.jpdest-2), pr\r\n call th.with.s\r\nendm\r\nmacro th.with.ret\r\n jp th.ewith\r\nendm\r\n\r\ndefsub th.with.s\r\n sp2mem th.wrssp-2\r\n ld l, th.ofs.sp\r\n ld (th.updsp-2),hl\r\n mem2sp\r\n jp 0\r\n th.jpdest:\r\nth.ewith:\r\n ld (0),sp\r\n th.updsp:\r\n ld sp,0\r\n th.wrssp:\r\nendsub th.with.s\r\n \r\n\r\n \r\n \r\ndefsub th.push\r\n ;push bc to thread hl\r\n th.with tpsbc\r\n ret\r\n tpsbc:\r\n  push bc\r\n  th.with.ret 0\r\nendsub th.push\r\n\r\n\r\ndefwork th.cur\r\n dw 0\r\nendwork th.cur\r\n\r\ndefsub th.loop\r\n ; hook before stepall\r\n db 0cdh\r\n h.thent:\r\n dw th.nop\r\n ; save prev timecnt\r\n ld a,(timecnt)\r\n push af\r\n ; Do stepall\r\n call th.stepall\r\n ; hook after stepall\r\n db 0cdh\r\n h.thlop:\r\n dw th.nop\r\n ; wait until timecnt changes\r\n pop af\r\n bwat:\r\n  ld hl,timecnt\r\n  cp (hl)\r\n  jr nz,bbwat\r\n  push af\r\n  call doint\r\n  pop af\r\n  jr bwat\r\n bbwat:\r\n ; repeat\r\n jr th.loop\r\nendsub th.loop\r\n\r\nth.nop:\r\n ret\r\n\r\n\r\nmacro th.pushi, val\r\n ld bc,val\r\n call th.push\r\nendm\r\n\r\n'].join(''),'sub': [].join(''),'debug': ['include math\r\n;debug\r\nmacro show,reg\r\n ld (hexval+1),reg\r\n call showhex\r\nendm\r\nmacro showm ,ad\r\n push hl\r\n ld HL,(ad)\r\n show HL\r\n pop HL\r\nendm\r\nmacro showlb,lb\r\n push hl\r\n ld hl,lb\r\n ld (hexval+1),hl\r\n call showhex\r\n pop hl\r\nendm\r\nshowhex:\r\nproc\r\n local loop\r\n push af\r\n push bc\r\n push HL\r\n hexval:\r\n ld hl,0\r\n ld b,4\r\n loop:\r\n  xor a\r\n  rept 4\r\n   slhl\r\n   rla\r\n  endm\r\n  call showhex1\r\n djnz loop\r\n ld a,32\r\n call wrt\r\n pop HL\r\n pop bc\r\n pop af\r\n ret\r\nendp\r\nshowhex1:\r\nproc\r\n local els\r\n cp 10\r\n jp nc, els\r\n add a,48\r\n jp wrt\r\n els:\r\n add a,65-10\r\n jp wrt\r\nendp\r\nabort:\r\n call wrt\r\n db 018h,0feh\r\nret\r\n\r\nmacro trace,v\r\n if not nul v\r\n  push af\r\n  ld a,v\r\n  ld (trad),a\r\n  pop af\r\n endif\r\n call trace.s\r\nendm\r\ntrace.s:\r\n push af\r\n push hl\r\n ld a,(trad)\r\n ld hl,1ae0h\r\n call wrt\r\n call 4dh\r\n inc a\r\n ld (trad),a\r\n ld a,32\r\n call wrt \r\n pop hl\r\n pop af\r\n ret\r\ntrad:\r\n db 65\r\n\r\nshowz:\r\n push af\r\n jr z,showz.s\r\n ld a,"N"\r\n call wrt\r\n showz.s:\r\n ld a,"Z"\r\n call wrt\r\n ld a,32\r\n call wrt\r\n pop af\r\n ret\r\n \r\n\r\nshowc:\r\n push af\r\n jr c,showc.s\r\n ld a,"N"\r\n call wrt\r\n showc.s:\r\n ld a,"C"\r\n call wrt\r\n ld a,32\r\n call wrt\r\n pop af\r\n ret\r\n \r\n\r\n\r\n\r\n\r\n\r\nmacro unreach, mesg\r\n trace mesg\r\n dw 0x18,0xfe\r\nendm\r\nmacro head, lb\r\n unreach lb\r\n marker.b lb\r\n lb:\r\nendm\r\n\r\nmacro defsub, n\r\n head n\r\nendm\r\nmacro endsub, n\r\n ret\r\n marker.e n\r\nendm\r\nmacro defwork, n\r\n head n\r\nendm\r\nmacro endwork, n\r\n marker.e n\r\nendm\r\n\r\ndefsub wrt\r\nproc\r\n local sk\r\n push hl\r\n push af\r\n ld hl,1800h\r\n cursor:\r\n call 4dh\r\n inc hl\r\n ld a,h\r\n cp 1bh\r\n jr c,sk\r\n  ld h,18h\r\n sk:\r\n ld (cursor-2),hl\r\n pop af\r\n pop hl\r\n ret\r\nendp\r\nendsub wrt\r\n'].join(''),'sp': ['include mem\r\ninclude debug\r\nmacro sp.get\r\n ld HL,0\r\n ADD hl, sp\r\nendm\r\nmacro sp.set\r\n ld sp,hl\r\nendm\r\nmacro mem2sp,ad\r\n local rs\r\n if nul ad\r\n  ld (rs-2),hl\r\n  ld sp,(0)\r\n  rs:\r\n else\r\n  ld sp,(ad)\r\n endif\r\nendm\r\nmacro sp2mem,ad\r\n local spad\r\n if nul ad\r\n  ld (spad-2),hl\r\n  ld (0),sp\r\n  spad:\r\n else\r\n  ld (ad),sp\r\n endif\r\nendm\r\n\r\nmacro showsp\r\n ld (sptmp),sp\r\n showm sptmp\r\nendm\r\nsptmp:\r\ndw 0\r\nmacro showstk\r\n showsp\r\n ld (sva),a\r\n ld a,":"\r\n call wrt\r\n ld a,(sva)\r\n ex (sp),hl\r\n show hl\r\n ex (sp),hl\r\nendm\r\nsva: db 0'].join(''),'oop': ['include mem\r\ninclude th\r\ninclude assert\r\n\r\n;a2 a1  oldpc oldix lcl1 lcl2\r\nargidx equ 2\r\nmacro getarg ,n\r\n ld l,(ix+argidx+n*2)\r\n ld h,(ix+argidx+n*2+1)\r\nendm\r\n\r\nmacro setlcl ,n\r\n ld (IX-(n*2-1)),h\r\n ld (ix-n*2),l\r\nendm\r\n\r\nmacro getlcl ,n\r\n ld h,(IX-(n*2-1))\r\n ld l,(ix-n*2)\r\nendm\r\n\r\nmacro addarg\r\n push hl\r\n; hl=arg  stktp=af\r\n;ex (sp),hl\r\n;ld a,h\r\n;push af\r\nendm\r\n\r\n\r\n\r\nmacro pusharg ,n\r\n getarg n\r\n push HL\r\nendm\r\n\r\nmacro pushlcl ,n\r\n getlcl n\r\n push HL\r\nendm\r\n\r\nmacro enter ,locals\r\n push ix\r\n ld ix,0\r\n add ix,sp\r\n rept locals\r\n  push HL\r\n endm\r\nendm\r\n\r\nmacro pops ,n\r\n rept n*2\r\n  inc sp\r\n endm\r\nendm\r\n\r\n\r\nmacro exit,n\r\n ld sp,ix\r\n pop ix\r\n if n!=0\r\n  exx\r\n  pop bc\r\n  pops n\r\n  push bc\r\n  exx\r\n endif\r\n ret\r\nendm\r\n\r\nmacro pushthis\r\n getthis\r\n push af\r\nendm\r\nmacro popthis\r\n popa\r\n ld (this),a\r\nendm\r\n\r\n\r\nmacro invoketg.a,fld,args\r\n; pushthis before arg push\r\n; hl=target \r\n ld a,h\r\n ld (this),a\r\n getfld fld\r\n callhl\r\n; pops args\r\n; popthis after \r\nendm\r\n\r\nmacro invoke,fld\r\n getfld fld\r\n callhl\r\n; pops args\r\n getthis\r\nendm\r\n\r\nmacro getfld, n\r\n local ad\r\n ld (ad-1),a\r\n ld hl,(n)\r\n ad:\r\nendm\r\n\r\nmacro setfld, n\r\n local ad\r\n ld (ad-1),a\r\n ld (n),hl\r\n ad:\r\nendm\r\n\r\nmacro getfldtg,n\r\n;hl=tg\r\n ld l,n\r\n peekw hl,hl\r\nendm\r\n\r\nmacro setfldtg,n\r\n; stk=val hl=tg\r\n ld l,n\r\n pop de\r\n pokew hl,de\r\nendm\r\n\r\nmacro getfldtg, n\r\n; hl=target\r\n ld d,h\r\n ld e,n\r\n peekw HL,de\r\nendm\r\n\r\nmacro tgconst,n\r\n ld (n-1),a\r\nendm\r\nmacro tgconst.g ,r16,n,fld\r\n ld r16,(fld)\r\n n:\r\nendm\r\nmacro tgconst.s ,n,fld,r16\r\n ld (fld),r16\r\n n:\r\nendm\r\n\r\n\r\nmacro curth2this\r\n ld a,(th.cur+1)\r\n ld (this),a\r\nendm\r\nmacro getthis\r\n ld a,(this)\r\nendm\r\n\r\nmacro new,Class,flds,st,en\r\n if nul st\r\n  th.new.range th.start, th.end\r\n else\r\n  th.new.range th.start+st*th.size, th.start+en*th.size\r\n endif\r\n pushi flds, bc\r\n pushi Class, bc\r\n call o.new\r\nendm\r\n\r\ndefsub o.new\r\nproc\r\n local retad,svthis,svsp,loop,lpend, w,allocfail,finally,lp2,lp2end\r\n ; {val .f} n &initbl retad\r\n pop hl;retad\r\n ld (retad-2),hl\r\n ; set initbl for th.with\r\n pop hl;&initbl\r\n th.with.setdst hl\r\n ; save this\r\n ld (svthis-1),a\r\n ; allocate thread\r\n call th.new\r\n jr nc, allocfail\r\n push hl; thread address\r\n call th.with.s; call &initbl\r\n pop hl; thread address\r\n ld a,h; set this as thread\r\n ; init fields\r\n pop bc; n of {val .f}\r\n inc c\r\n loop:\r\n  dec c\r\n  jr z,lpend\r\n  pop hl; .f\r\n  ld h,a\r\n  ld (w-2),hl\r\n  pop hl; val\r\n  ld (w),hl\r\n  w:\r\n jr loop\r\n lpend:\r\n ; return h as this\r\n ld h,a\r\n finally:\r\n  ;restore a before call o.new\r\n  ld a,0\r\n  svthis:\r\n  ;return \r\n  jp 0\r\n  retad:\r\n allocfail:\r\n  ; drop {val .f}\r\n  pop bc; n of {val .f}\r\n  ld b,c\r\n  inc c\r\n  lp2:\r\n   dec c\r\n   jr z, lp2end\r\n   pop hl\r\n   pop hl\r\n  jr lp2\r\n  lp2end:\r\n  ld hl,null;  todo null\r\n  jr finally\r\nendp\r\nendsub o.new\r\n\r\nmacro new.arg, n, v\r\n if not nul v\r\n  ld hl,v\r\n endif\r\n push hl\r\n pushi n,bc\r\nendm\r\n \r\nmacro o.assert.eq,fld, v\r\n local aa\r\n assert.do aa\r\n  getfld fld\r\n  assert.eq v\r\n  ret\r\n aa:\r\nendm\r\n\r\nthis:\r\ndb 0\r\n\r\nmacro fld.def,n\r\n n equ fldidx\r\n fldidx:defl fldidx-2\r\nendm\r\nmacro class,Class,super\r\n unreach "c"\r\n marker.b 0\r\n dw super\r\n fldidx:defl fld.top; todo fld.top\r\n Class:\r\n  fld .class,Class\r\nendm\r\nmacro fld.bottom,Class\r\n if defined Class##.bottom \r\n  if Class##.bottom ne fldidx\r\n   .error bottom ne fldidx\r\n  endif\r\n else\r\n Class##.bottom:defl fldidx\r\n endif\r\nendm \r\nmacro fld,n,v\r\n if defined n\r\n  if n ne fldidx\r\n   .error n ne fldidx\r\n  else \r\n   fldidx:defl fldidx-2\r\n  endif\r\n else\r\n  fld.def n\r\n endif\r\n pushi v,bc\r\nendm\r\nmacro unuse\r\n fldidx:defl fldidx-2\r\n pushi 0,bc\r\nendm\r\nmacro meth,Class,n\r\n fld .##n, Class##.##n\r\nendm\r\nmacro met2,Class,n\r\n fld n, Class##n\r\nendm\r\n\r\nclass Object,null\r\n fld .main,null\r\n fld.bottom Object\r\n marker.e Object\r\n\r\n\r\ndefsub o.boot\r\n curth2this\r\n invoke .main,0\r\nendsub o.boot\r\n\r\n\r\nmacro yield\r\n pushthis\r\n push ix\r\n call th.yield\r\n pop ix\r\n popthis\r\nendm\r\n\r\nmacro def,n,args,lcls\r\nhead n\r\n def.args:defl args\r\n def.locals:defl lcls\r\n if args>0 or lcls>0\r\n  enter lcls\r\n endif\r\nendm\r\nmacro enddef,n\r\n if def.args>0 or def.locals>0\r\n  exit def.args\r\n else\r\n  ret\r\n endif\r\n marker.e n\r\nendm\r\n\r\ndefsub isobj.a\r\n ;hl=obj?\r\n ;cy=true\r\n ld a,h\r\n cp high th.start\r\n jr c,notobj\r\n cp high th.end\r\n jr nc,notobj\r\n scf\r\n ret\r\n notobj:\r\n and a\r\nendsub isobj.a\r\n\r\ndefsub instanceof\r\n ; a=this de=Class\r\n ; z: true\r\n getfld .class\r\n jp is.subclass.a\r\nendsub instanceof\r\n\r\ndefsub get.superclass\r\n ; hl=Class\r\n dec hl\r\n dec hl\r\n peekw hl,hl\r\nendsub get.superclass\r\n\r\ndefsub is.subclass.a\r\nproc \r\n local top\r\n ; hl=Subclass\r\n ; de=Superclass\r\n ; z:true\r\n top:\r\n cpde.a 0\r\n ret z\r\n call get.superclass\r\n push de\r\n ld de,null\r\n cpde.a 0\r\n pop de\r\n jr nz,top\r\n cpde.a 0\r\nendp\r\nendsub is.subclass.a\r\n '].join(''),'spr': ['include const\r\ninclude th\r\ninclude mem\r\ninclude oop\r\ninclude sub\r\n\r\nclass Sprite,Object\r\n fld .main, 0\r\n fld.bottom Object\r\n fld .x, 100\r\n fld .y, 100\r\n fld .p, 0\r\n fld .c, 2\r\n fld.bottom Sprite\r\n marker.e Sprite\r\n \r\nmacro outwrt\r\n  out (98h),a\r\nendm\r\n\r\n\r\nmacro spr.unscale\r\n ; HL -> A\r\n rept spr.scale\r\n  srlhl\r\n endm\r\n LD A,L\r\n sub 8 \r\nendm\r\n\r\ndefsub spr.puts\r\nproc\r\n local t1,t2,t3,t4\r\n ld hl, 1b00h\r\n call SETWRT\r\n th.for sprl\r\n  ld a,h\r\n  tgconst t1\r\n  tgconst t2\r\n  tgconst t3\r\n  tgconst t4\r\n\r\n  tgconst.g hl,t1,.y \r\n  spr.unscale 0\r\n  outwrt 0\r\n  \r\n  tgconst.g hl,t2,.x \r\n  spr.unscale 0\r\n  outwrt 0\r\n  \r\n  tgconst.g a,t3,.p \r\n  sla a\r\n  sla a\r\n  outwrt 0\r\n  \r\n  tgconst.g a,t4,.c \r\n  outwrt 0\r\n  continue\r\n sprl:\r\nendp\r\nendsub spr.puts\r\n \r\n '].join(''),'sprpat': ['include const\r\n\r\n;aaa\r\nspr.inipat:\r\n ld de,3800h\r\n ld hl,spr.pat\r\n ld bc,128\r\n jp LDIRVM\r\nbg.inipat:\r\n ret\r\nspr.pat:\r\n; --- Slot 0 cat fstand\r\n; color 9\r\nDB $0C,$0E,$0F,$4F,$3D,$1D,$7F,$1B\r\nDB $0C,$3F,$7F,$7F,$6F,$0F,$06,$0C\r\nDB $18,$38,$F8,$F9,$DE,$DC,$7F,$6C\r\nDB $98,$FC,$FE,$FE,$F6,$F0,$60,$70\r\n; \r\n; --- Slot 1 cat fwalk1\r\n; color 9\r\nDB $0C,$0E,$0F,$4F,$3D,$1D,$7F,$1B\r\nDB $0C,$3F,$7F,$7F,$EF,$EF,$06,$06\r\nDB $18,$38,$F8,$F9,$DE,$DC,$7F,$6C\r\nDB $98,$FC,$FE,$FE,$D4,$78,$F0,$00\r\n; \r\n; --- Slot 2 cat fwalk2\r\n; color 9\r\nDB $18,$1C,$1F,$9F,$7B,$3B,$FE,$36\r\nDB $19,$3F,$7F,$7F,$2B,$1E,$0F,$00\r\nDB $30,$70,$F0,$F2,$BC,$B8,$FE,$D8\r\nDB $30,$FC,$FE,$FE,$F7,$F7,$60,$60\r\n; \r\n; --- Slot 3 cat omg\r\n; color 9\r\nDB $2C,$8E,$0F,$4B,$3D,$11,$7F,$1D\r\nDB $CA,$FF,$7F,$3F,$15,$1F,$0E,$00\r\nDB $1C,$39,$F8,$E9,$DE,$C4,$7F,$5C\r\nDB $AB,$FF,$FF,$FE,$AC,$F8,$70,$00\r\n\r\nds 60*32\r\n'].join(''),'tnu': ['\r\ninclude spr\r\ninclude bool\r\ninclude key\r\n\r\n;.onUpdate equ .c-2\r\n;.update equ .onUpdate-2\r\n;.screenOut equ .update-2\r\n;.die equ .screenOut-2\r\n;.updateEx equ .die-2\r\n\r\nmacro end.const, n\r\n pushi RActor.wait,bc\r\n pushi o.boot,bc\r\n th.with.ret 0 \r\n marker.e n\r\nendm\r\n\r\nmacro RActor.noovr,Class\r\n meth Class,main\r\n fld.bottom Object\r\n fld .x, 0\r\n fld .y, -1024\r\n fld .p, 0\r\n fld .c, 3\r\n fld.bottom Sprite\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut \r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld.bottom RActor\r\nendm\r\n\r\nclass RActor,Sprite\r\n RActor.noovr RActor\r\n end.const RActor\r\nRActor.main:\r\n enter 0\r\n exit 0\r\nRActor.update:\r\n invoke .onUpdate\r\n yield\r\n ret \r\nRActor.onUpdate:\r\n ret\r\nRActor.screenOut:\r\nproc\r\n local true\r\n getfld .x\r\n bit 1,h\r\n jr nz, true\r\n getfld .y\r\n ld de,192*2\r\n cpde.a\r\n getthis\r\n jr nc,true\r\n ld hl,0\r\n xor a\r\n ret\r\n true:\r\n ld hl,1\r\n scf\r\n ret\r\nendp\r\nRActor.wait:\r\nproc\r\n local lbl\r\n lbl:\r\n invoke .update\r\n jr lbl\r\nendp\r\ndef RActor.die,0,0\r\n ld h,a\r\n ld l,th.ofs.stp\r\n ld (hl),th.st.blank\r\n ld hl, 0\r\n setfld .c\r\nenddef RActor.die\r\n\r\ndef RActor.updateEx,1,0\r\nproc \r\n local n\r\n; enter 0\r\n getarg 1\r\n ld b,h\r\n ld c,l\r\n reptbc n\r\n  invoke .update\r\n  continue\r\n n:\r\nendp\r\nenddef RActor.updateEx\r\n\r\ncrashTo.size equ 8<<spr.scale\r\n\r\nproc\r\n local gx,gy,t1,t2\r\n local endc,cr1\r\n local fe\r\n\r\ndefsub crashTo.setXY\r\n getfld .x\r\n const gx,hl\r\n getfld .y\r\n const gy,hl\r\nendsub crashTo.setXY\r\n\r\n\r\ndef RActor.crashTo,1,0\r\n call crashTo.setXY\r\n getarg 1\r\n const cr.class,hl\r\n call isobj.a\r\n jr c, cr1\r\n  ld hl, th.start\r\n  ld de, th.end\r\n  call crashTo1\r\n  jr endc\r\n cr1:\r\n  getthis 0\r\n  call crashTo1\r\n  flagtobool c\r\n endc:\r\nenddef RActor.crashTo\r\n\r\nmacro crashToClass,Class,st,en\r\n ; a=this\r\n call crashTo.setXY\r\n ld hl,Class\r\n const cr.class,hl\r\n ld hl,th.start+st*th.size\r\n ld de,th.start+en*th.size\r\n call crashToC\r\nendm\r\n\r\nmacro foreach, Class,st,en,nxt\r\n;uses a\r\n ld hl,th.start+st*th.size\r\n ld de,th.start+en*th.size\r\n ld bc,th.size\r\n for nxt\r\n  all.skip Class\r\nendm\r\n\r\n\r\nmacro all.skip.blank.self\r\n ; skip blank\r\n  ; TODO th.ofs.stp\r\n  call th.isblank.a\r\n  continue z\r\n  ; skip hl==this\r\n  getthis 0\r\n  cp h\r\n  continue z\r\nendm\r\nmacro all.skip.isnot,Class\r\n  ; skip object not instance of *Class*\r\n  push hl\r\n  ld a,h\r\n  ld de,Class\r\n  call instanceof\r\n  getthis 0\r\n  pop hl\r\n  continue nz\r\nendm\r\nmacro all.skip, Class\r\n all.skip.blank.self 0\r\n all.skip.isnot Class\r\nendm\r\n\r\ndefsub crashToC\r\n ;before:\r\n ; call crashTo.setXY\r\n ; const cr.class,Class\r\n ; hl start\r\n ; de end\r\n ld bc,th.size\r\n for fe\r\n  all.skip.blank.self 0\r\n  ; skip object not instance of *Class*\r\n  push hl\r\n  ld a,h\r\n  ldconst de,cr.class\r\n  call instanceof\r\n  pop hl\r\n  continue nz\r\n  ; do crashTo1\r\n  getthis 0\r\n  call crashTo1\r\n  break c\r\n  continue\r\n fe:\r\n getthis 0\r\n ret c\r\n ld hl,null\r\nendsub all\r\n\r\ndefsub crashTo1\r\n ;hl=tg\r\n ;cy:true\r\n ;hl is used\r\n push af\r\n ld a,h\r\n tgconst t1\r\n tgconst t2\r\n pop af\r\n tgconst.g hl,t1,.x\r\n ldconst bc,gx\r\n subhl bc\r\n call abs\r\n ld bc,crashTo.size\r\n subhl bc\r\n ret nc\r\n\r\n tgconst.g hl,t2,.y\r\n ldconst bc,gy\r\n subhl bc\r\n call abs\r\n ld bc,crashTo.size\r\n subhl bc\r\nendsub crashTo1\r\n\r\nendp\r\n\r\n\r\nmacro tnu.run,Main\r\n ld sp,sp.ini\r\n call screen2\r\n \r\n showsp\r\n showlb endusr\r\n call spr.inipat\r\n call bg.inipat\r\n\r\n ld hl,th.start\r\n ld (hl),0cdh\r\n ld de,th.start+1\r\n ld bc,th.size*th.count-1\r\n ldir\r\n \r\n call th.init\r\n ;call mus.ini\r\n new Main, 0\r\n movw (h.thlop),spr.puts\r\n movw (h.thent),keyall\r\n jp th.loop\r\nendm\r\n\r\n;aaaa'].join(''),'key': ['include debug\r\n\r\ndefsub keyall\r\nproc\r\n;show hl\r\n local lp\r\n ld hl,keymat1\r\n ld de,keymat2\r\n ld bc,11\r\n ldir\r\n ld a,0\r\n ld hl,keymat1\r\n lp:\r\n push af\r\n call SNSMAT.a\r\n xor 255\r\n ld (hl),a\r\n pop af\r\n inc hl\r\n inc a\r\n cp 11\r\n jr c,lp\r\nendp\r\nendsub keyall\r\n\r\ndefwork keymat1\r\nds 11\r\nendwork keymat1\r\ndefwork keymat2\r\nds 11\r\nendwork keymat2\r\n\r\n\r\nproc\r\ndefsub getkey.a\r\nlocal chkmat\r\nex de,hl\r\nld hl,keymat1\r\ncall chkmat\r\nld hl,0\r\nret z\r\nld hl,keymat2\r\ncall chkmat\r\nld hl,1\r\nret z\r\ninc hl\r\nendsub getkey.a\r\n\r\ndefsub chkmat\r\npush de\r\nld a,d\r\nld d,0\r\nadd hl,de\r\nand (hl)\r\npop de\r\nendsub chkmat\r\n\r\ndefsub getkey\r\npush af\r\ncall getkey.a\r\npop af\r\nendsub getkey\r\n\r\nendp'].join(''),'map': ['include sub\r\ninclude math\r\ninclude tnu\r\n\r\ndefsub map.adr\r\n ; hl=chipx\r\n ; de=chipy\r\n rept 5\r\n  slde 0\r\n endm\r\n add hl,de\r\n ld de,1800h\r\n add hl,de\r\nendsub map.adr\r\n\r\ndefsub map.set.a\r\n ;  a=data\r\n call map.adr\r\n call 4dh\r\nendsub map.set.a\r\n\r\ndefsub map.get.a\r\n call map.adr\r\n call 4ah \r\nendsub map.get.a\r\n\r\ndefsub map.adrat.a\r\n ; hl=spr_x\r\n ; de=spr_y\r\n spr.unscale 0\r\n srl a\r\n srl a\r\n srl a\r\n push af\r\n ex de,hl\r\n spr.unscale 0\r\n srl a\r\n srl a\r\n srl a\r\n ld d,0\r\n ld e,a\r\n pop hl\r\n ld l,h\r\n ld h,0\r\n inc hl\r\n inc de\r\n call map.adr\r\nendsub map.adrat.a\r\n\r\ndefsub map.getat.a\r\n call map.adrat.a\r\n call 4ah\r\nendsub map.getat.a\r\n\r\ndefsub map.setat.a\r\n ; a=data\r\n push af\r\n call map.adrat.a\r\n pop af\r\n call 4dh\r\nendsub map.setat.a\r\n\r\ndefsub locate\r\n ; hl=chipx\r\n ; de=chipy\r\n call map.adr\r\n ld (cursor-2),hl\r\nendsub locate\r\n'].join(''),'maze': [].join(''),'t1': ['org 09000h\r\njp main\r\ninclude const\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude tnu\r\ninclude sp\r\n\r\n;===your code \r\n\r\nright:dw 0\r\n\r\nmain:\r\ntnu.run Main\r\ndef Main.main,0,0\r\nnew.arg .vx,1\r\nnew.arg .vy,0\r\nnew.arg .x,0\r\nnew.arg .y,100\r\nnew Cat,4\r\n\r\nnew.arg .x,100\r\nnew.arg .y,100\r\nnew Target,2\r\n\r\nld (right),hl\r\nld a,h\r\nld de,RActor\r\ncall instanceof\r\ncall showz\r\n\r\nld a,(right+1)\r\nld de,Target\r\ncall instanceof\r\ncall showz\r\n\r\nld a,(right+1)\r\nld de,Cat\r\ncall instanceof\r\ncall showz\r\n\r\n\r\nld hl,1\r\nsetfld .c\r\nenddef 0\r\n\r\nclass Main,RActor\r\n RActor.noovr Main\r\n end.const Main\r\nclass Target,RActor\r\n RActor.noovr Target\r\n met2 Target,.push\r\n end.const Target\r\ndef Target.main,0,0\r\nenddef\r\nclass Cat,RActor\r\n RActor.noovr Cat\r\n fld .vy, 0\r\n fld .vx, 0\r\n fld.bottom Cat\r\n end.const Cat\r\ndef Cat.main,0,0\r\n blp:\r\n  ld hl,0108h\r\n  call getkey\r\n  jpf nomov\r\n  getthis\r\n  ;x+=vx\r\n  getfld .x\r\n  ex de, hl\r\n  getfld .vx\r\n  add hl,de\r\n  setfld .x\r\n  nomov:\r\n  ; y+=vy\r\n  getfld .y\r\n  ex de, hl\r\n  getfld .vy\r\n  add hl,de\r\n  setfld .y\r\n  ld hl,(right)\r\n  push hl\r\n  invoke .crashTo\r\n  jpf cr\r\n   ; r.x+=10\r\n   ld hl,(right)\r\n   getfldtg .x\r\n   ld de,10\r\n   add hl,de\r\n   push hl\r\n   ld hl,(right)\r\n   setfldtg .x\r\n   ; r.push()\r\n   pushthis 0\r\n   ld hl,(right)\r\n   invoketg.a .push\r\n   popthis 0\r\n  cr:\r\n  invoke .update\r\n jp blp\r\nenddef\r\n; test t1\r\ndef Target.push,0,0\r\n ld hl,3\r\n setfld .p\r\n repti 30,pse\r\n  getfld .x\r\n  inc hl\r\n  setfld .x\r\n  invoke .update\r\n  continue\r\n pse:\r\n ld hl,0\r\n setfld .p\r\nenddef\r\n\r\nendusr: \r\ninclude sprpat\r\n\r\nend main\r\nhttps://msxpen.com/codes/-N6DDfMvZq9aUeJ9JLpN\r\nhttps://msxpen.com/codes/-N6QGYk-rr5iDuTtHpF7'].join(''),'t2': ['org 08000h\r\ninclude tnu\r\ninclude bool\r\ninclude map\r\n\r\nmain:\r\ntnu.run Main\r\n;range 0-5\r\nclass EBullet,RActor\r\n meth EBullet,main\r\n fld .x,0\r\n fld .y,0\r\n fld .p,0\r\n fld .c,0\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut\r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld .vx,0\r\n fld .vy,0\r\n end.const 0\r\ndef EBullet.main,0,0\r\n ;p=7;\r\n ld hl,7\r\n setfld .p\r\n ;c=15;\r\n ld hl,15\r\n setfld .c\r\n lb1:\r\n ld hl,true\r\n jpf lb2\r\n ;x+=vx;\r\n getfld .vx\r\n push hl\r\n getfld .x\r\n pop de\r\n add hl, de\r\n setfld .x\r\n ;y+=vy;\r\n getfld .vy\r\n push hl\r\n getfld .y\r\n pop de\r\n add hl, de\r\n setfld .y\r\n invoke .screenOut\r\n jpf lb4\r\n ;die();\r\n invoke .die\r\n jp lb3\r\n lb4:\r\n lb3:\r\n ;update();\r\n invoke .update\r\n jp lb1\r\n lb2:\r\n ret\r\n;range 5-15\r\nclass Enemy,RActor\r\n meth Enemy,main\r\n fld .x,0\r\n fld .y,0\r\n fld .p,0\r\n fld .c,0\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut\r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld .vx,0\r\n fld .bvx,0\r\n fld .bvy,0\r\n fld .bdist,0\r\n meth Enemy,fire\r\n end.const 0\r\ndef Enemy.main,0,0\r\n lb5:\r\n ld hl,200\r\n push hl\r\n getfld .y\r\n pop de\r\n call hlltde\r\n jpf lb6\r\n ;y+=2;\r\n ld hl,2\r\n push hl\r\n getfld .y\r\n pop de\r\n add hl, de\r\n setfld .y\r\n ;update();\r\n invoke .update\r\n jp lb5\r\n lb6:\r\n ld hl,(gbl_player)\r\n getfldtg .x\r\n push hl\r\n getfld .x\r\n pop de\r\n call hlgtde\r\n jpf lb8\r\n ;vx=0-2;\r\n ld hl,2\r\n push hl\r\n ld hl,0\r\n pop de\r\n subhl de\r\n setfld .vx\r\n jp lb7\r\n lb8:\r\n ;vx=2;\r\n ld hl,2\r\n setfld .vx\r\n lb7:\r\n ;fire();\r\n invoke .fire\r\n lb9:\r\n ld hl,true\r\n jpf lb10\r\n invoke .screenOut\r\n jpf lb12\r\n ;die();\r\n invoke .die\r\n jp lb11\r\n lb12:\r\n lb11:\r\n ;y+=2;\r\n ld hl,2\r\n push hl\r\n getfld .y\r\n pop de\r\n add hl, de\r\n setfld .y\r\n ;x+=vx;\r\n getfld .vx\r\n push hl\r\n getfld .x\r\n pop de\r\n add hl, de\r\n setfld .x\r\n ;update();\r\n invoke .update\r\n jp lb9\r\n lb10:\r\n ret\r\ndef Enemy.fire,0,0\r\n ;bvx=$player.x-x;\r\n getfld .x\r\n push hl\r\n ld hl,(gbl_player)\r\n getfldtg .x\r\n pop de\r\n subhl de\r\n setfld .bvx\r\n ;bvy=$player.y-y;\r\n getfld .y\r\n push hl\r\n ld hl,(gbl_player)\r\n getfldtg .y\r\n pop de\r\n subhl de\r\n setfld .bvy\r\n ;bdist=abs(bvx)+abs(bvy);\r\n getfld .bvy\r\n call abs\r\n push hl\r\n getfld .bvx\r\n call abs\r\n pop de\r\n add hl, de\r\n setfld .bdist\r\n ;bdist/=8;\r\n pushthis\r\n getfld .bdist\r\n push hl\r\n ld hl,8\r\n pop de\r\n call IDIV.a\r\n popthis\r\n setfld .bdist\r\n ;bvx/=bdist;\r\n pushthis\r\n getfld .bvx\r\n push hl\r\n getfld .bdist\r\n pop de\r\n call IDIV.a\r\n popthis\r\n setfld .bvx\r\n ;bvy/=bdist;\r\n pushthis\r\n getfld .bvy\r\n push hl\r\n getfld .bdist\r\n pop de\r\n call IDIV.a\r\n popthis\r\n setfld .bvy\r\n ;new EBullet{        x,y,vx:bvx,vy:bvy    };\r\n getfld .x\r\n new.arg .x\r\n getfld .y\r\n new.arg .y\r\n getfld .bvx\r\n new.arg .vx\r\n getfld .bvy\r\n new.arg .vy\r\n new EBullet,4,0,5\r\n ret\r\n;range 0-5\r\nclass Main,RActor\r\n meth Main,main\r\n fld .x,0\r\n fld .y,0\r\n fld .p,0\r\n fld .c,0\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut\r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld .i,0\r\n end.const 0\r\ndef Main.main,0,0\r\n ;$player=new Player{x:256, y: 300, p:$pat_spr+4, c:15};\r\n ld hl,256\r\n new.arg .x\r\n ld hl,300\r\n new.arg .y\r\n ld hl,4\r\n push hl\r\n ld hl,(gbl_pat_spr)\r\n pop de\r\n add hl, de\r\n new.arg .p\r\n ld hl,15\r\n new.arg .c\r\n new Player,4,0,5\r\n ld (gbl_player),hl\r\n ld hl,0\r\n setfld .i\r\n lb13:\r\n ld hl,20\r\n push hl\r\n getfld .i\r\n pop de\r\n call hlltde\r\n jpf lb14\r\n ;new Enemy{x:rnd(512), y:0, p:5, c:7};\r\n ld hl,512\r\n call rnd\r\n new.arg .x\r\n ld hl,0\r\n new.arg .y\r\n ld hl,5\r\n new.arg .p\r\n ld hl,7\r\n new.arg .c\r\n new Enemy,4,5,15\r\n ;updateEx(30);\r\n ld hl,30\r\n push hl\r\n invoke .updateEx\r\n jp lb13\r\n lb14:\r\n ret\r\n;range 15-20\r\nclass PBullet,RActor\r\n meth PBullet,main\r\n fld .x,0\r\n fld .y,0\r\n fld .p,0\r\n fld .c,0\r\n meth PBullet,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut\r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld .e,0\r\n end.const 0\r\ndef PBullet.main,0,0\r\n ;p=6;\r\n ld hl,6\r\n setfld .p\r\n ;c=8;\r\n ld hl,8\r\n setfld .c\r\n lb15:\r\n ld hl,true\r\n jpf lb16\r\n invoke .screenOut\r\n jpf lb18\r\n ;die();\r\n invoke .die\r\n jp lb17\r\n lb18:\r\n lb17:\r\n ;y-=6;\r\n ld hl,6\r\n push hl\r\n getfld .y\r\n pop de\r\n subhl de\r\n setfld .y\r\n crashToClass Enemy, 5, 15\r\n setfld .e\r\n getfld .e\r\n jpf lb20\r\n ;e.die();\r\n pushthis 0\r\n getfld .e\r\n invoketg.a .die\r\n popthis 0\r\n ;die();\r\n invoke .die\r\n jp lb19\r\n lb20:\r\n lb19:\r\n ;update();\r\n invoke .update\r\n jp lb15\r\n lb16:\r\n ret\r\ndef PBullet.onUpdate,0,0\r\n ;c+=1;\r\n ld hl,1\r\n push hl\r\n getfld .c\r\n pop de\r\n add hl, de\r\n setfld .c\r\n ld hl,14\r\n push hl\r\n getfld .c\r\n pop de\r\n call hlgtde\r\n jpf lb22\r\n ;c=0;\r\n ld hl,0\r\n setfld .c\r\n jp lb21\r\n lb22:\r\n lb21:\r\n ret\r\n;range 0-5\r\nclass Player,RActor\r\n meth Player,main\r\n fld .x,0\r\n fld .y,0\r\n fld .p,0\r\n fld .c,0\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut\r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n end.const 0\r\ndef Player.main,0,0\r\n lb23:\r\n ld hl,3\r\n ld de,10\r\n call locate\r\n getfld .y\r\n ld a,l\r\n call wrt\r\n ld a,h\r\n call wrt\r\n getthis 0\r\n \r\n ex de,hl\r\n getfld .x\r\n ld a,35\r\n call map.setat.a\r\n getthis 0\r\n ld hl,true\r\n jpf lb24\r\n ld hl, 4104\r\n call getkey\r\n jpf lb26\r\n ;x-=3;\r\n ld hl,3\r\n push hl\r\n getfld .x\r\n pop de\r\n subhl de\r\n setfld .x\r\n jp lb25\r\n lb26:\r\n lb25:\r\n ld hl, 32776\r\n call getkey\r\n jpf lb28\r\n ;x+=3;\r\n ld hl,3\r\n push hl\r\n getfld .x\r\n pop de\r\n add hl, de\r\n setfld .x\r\n jp lb27\r\n lb28:\r\n lb27:\r\n ld hl, 8200\r\n call getkey\r\n jpf lb30\r\n ;y-=3;\r\n ld hl,3\r\n push hl\r\n getfld .y\r\n pop de\r\n subhl de\r\n setfld .y\r\n jp lb29\r\n lb30:\r\n lb29:\r\n ld hl, 16392\r\n call getkey\r\n jpf lb32\r\n ;y+=3;\r\n ld hl,3\r\n push hl\r\n getfld .y\r\n pop de\r\n add hl, de\r\n setfld .y\r\n jp lb31\r\n lb32:\r\n lb31:\r\n crashToClass Enemy, 5, 15\r\n jpf lb34\r\n ;die();\r\n invoke .die\r\n jp lb33\r\n lb34:\r\n lb33:\r\n crashToClass EBullet, 0, 5\r\n jpf lb36\r\n ;die();\r\n invoke .die\r\n jp lb35\r\n lb36:\r\n lb35:\r\n ld hl,1\r\n push hl\r\n ld hl, 264\r\n call getkey\r\n pop de\r\n call hleqde\r\n jpf lb38\r\n ;new PBullet{x,y};\r\n getfld .x\r\n new.arg .x\r\n getfld .y\r\n new.arg .y\r\n new PBullet,2,15,20\r\n foreach Enemy,5,14,nxx\r\n  ld a,h\r\n  ld hl,8\r\n  setfld .c\r\n  continue \r\n nxx:\r\n getthis 0\r\n \r\n \r\n jp lb37\r\n lb38:\r\n lb37:\r\n ;update();\r\n invoke .update\r\n jp lb23\r\n lb24:\r\n ret\r\nenddef 0\r\nendusr:\r\ngbl_player:dw 0\r\ngbl_pat_spr:dw 0\r\nspr.inipat:\r\n ld de,3800h\r\n ld hl,spr.pat\r\n ld bc,2048\r\n jp LDIRVM\r\nspr.pat:\r\ndb $c,$e,$f,$4f,$3d,$1d,$7f,$1b,$c,$3f,$7f,$7f,$6f,$f,$6,$c\r\ndb $18,$38,$f8,$f9,$de,$dc,$7f,$6c,$98,$fc,$fe,$fe,$f6,$f0,$60,$70\r\ndb $c,$e,$f,$4f,$3d,$1d,$7f,$1b,$c,$3f,$7f,$7f,$ef,$ef,$6,$6\r\ndb $18,$38,$f8,$f9,$de,$dc,$7f,$6c,$98,$fc,$fe,$fe,$d4,$78,$f0,$0\r\ndb $18,$1c,$1f,$9f,$7b,$3b,$fe,$36,$19,$3f,$7f,$7f,$2b,$1e,$f,$0\r\ndb $30,$70,$f0,$f2,$bc,$b8,$fe,$d8,$30,$fc,$fe,$fe,$f7,$f7,$60,$60\r\ndb $2c,$8e,$f,$4b,$3d,$11,$7f,$1d,$ca,$ff,$7f,$3f,$15,$1f,$e,$0\r\ndb $1c,$39,$f8,$e9,$de,$c4,$7f,$5c,$ab,$ff,$ff,$fe,$ac,$f8,$70,$0\r\ndb $1,$1,$1,$2,$3,$92,$96,$f6,$f6,$fe,$fe,$fe,$ff,$af,$26,$0\r\ndb $80,$80,$80,$40,$c0,$49,$69,$6f,$6f,$7f,$7f,$7f,$ff,$f5,$64,$0\r\ndb $8,$10,$30,$30,$18,$f,$3f,$37,$7b,$7d,$3f,$1f,$37,$60,$70,$c\r\ndb $8,$4,$6,$6,$c,$f8,$fe,$f6,$ef,$df,$fe,$fc,$f6,$3,$7,$18\r\ndb $0,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$0,$0\r\ndb $80,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$80,$0\r\ndb $0,$0,$0,$0,$3,$f,$f,$1f,$1f,$1f,$1f,$f,$f,$3,$0,$0\r\ndb $0,$0,$0,$0,$c0,$f0,$f0,$f8,$f8,$f8,$f8,$f0,$f0,$c0,$0,$0\r\nbg.inipat:\r\n ret\r\n\r\n\r\nend main'].join(''),'t3': ['org 09000h\r\n\r\ninclude tnu\r\ninclude mus\r\ninclude sprpat\r\n\r\n;===your code \r\n\r\nmain:\r\nld sp,(8000h)\r\n\r\ncall screen1\r\n\r\nshowsp\r\nshowlb endusr\r\n\r\nld hl,8000h\r\nld (hl),0cdh\r\nld de,8001h\r\nld bc,th.size*th.count-1\r\nldir\r\n\r\n\r\ncall th.init\r\ncall spr.inipat\r\n;call mus.ini\r\n\r\n\r\nnew Main, 0\r\nshow hl\r\n\r\n\r\nmovw (h.thlop),spr.puts\r\njp th.loop\r\n\r\nclass Main, RActor\r\n meth Main,main\r\n fld.bottom Object\r\n fld .x,100\r\n fld .y,300\r\n fld .p,0\r\n fld .c,3\r\n fld.bottom Sprite\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut \r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld.bottom RActor\r\n fld.bottom Main\r\n end.const\r\n\r\nMain.main:\r\n olp:\r\n  getthis\r\n  invoke .update\r\n  call xrnd.a\r\n  ld a,h\r\n  and 15\r\n  jr nz,doap\r\n   getthis\r\n   getfld .x\r\n   new.arg .x\r\n   getfld .y\r\n   new.arg .y\r\n   ld hl,7\r\n   call rnd.a\r\n   ld de,3\r\n   sbc hl,de\r\n   new.arg .vx\r\n   ld hl,5\r\n   call rnd.a\r\n   ld de,15\r\n   sbc hl,de\r\n   new.arg .vy\r\n   new Bullet, 4\r\n   call dstk\r\n  doap:\r\n  ld a,8\r\n  call SNSMAT.a\r\n  and 1\r\n  jr z,golf\r\n  \r\n  getthis\r\n  getfld .x\r\n  inc hl\r\n  inc hl\r\n  setfld .x\r\n  ld de,400\r\n  cpde.a\r\n  jp c, olp\r\n  golf:\r\n  ld hl,0\r\n  getthis\r\n  setfld .x\r\n jp olp\r\n\r\n\r\nclass Bullet,RActor\r\n meth Bullet,main\r\n fld.bottom Object\r\n fld .x, 0\r\n fld .y, 0\r\n fld .p, 2\r\n fld .c, 15\r\n fld.bottom Sprite\r\n meth RActor,onUpdate\r\n meth RActor,update\r\n meth RActor,screenOut \r\n meth RActor,die\r\n meth RActor,updateEx\r\n meth RActor,crashTo\r\n fld.bottom RActor\r\n fld .vy, -10\r\n fld .vx, 0\r\n fld.bottom Bullet\r\n end.const \r\n \r\nBullet.main:\r\n blp:\r\n  getthis\r\n  ;x+=vx\r\n  getfld .x\r\n  ex de, hl\r\n  getfld .vx\r\n  add hl,de\r\n  setfld .x\r\n  ; y+=vy\r\n  getfld .y\r\n  ex de, hl\r\n  getfld .vy\r\n  add hl,de\r\n  setfld .y\r\n  getfld .vy\r\n  inc hl\r\n  setfld .vy\r\n\r\n  invoke .update\r\n  invoke .screenOut\r\n  jp c, bdie\r\n  getfld .vy\r\n  bit 7,h\r\n  jr nz,blp\r\n  ld de,5\r\n  cpde.a\r\n  jr c,blp\r\n \r\n  call dstk\r\n  getthis\r\n  ld hl,3\r\n  setfld .p\r\n  pushi 10,bc\r\n  invoke .updateEx\r\n\r\n bleft:\r\n  getthis\r\n  ld hl,2\r\n  setfld .p\r\n  getfld .x\r\n  dec hl\r\n  dec hl\r\n  setfld .x\r\n  getfld .y\r\n  dec hl\r\n  setfld .y\r\n  invoke .update\r\n  invoke .screenOut\r\n  jr c, bdie\r\n  jr bleft\r\n bdie:\r\n  invoke .die\r\n  ret \r\n\r\n  \r\ndstk:\r\n push af\r\n ld hl,th.start+256*3\r\n getthis\r\n ld h,a\r\n ld de,1900h\r\n ld bc,256\r\n call LDIRVM\r\n pop af\r\n ret\r\n \r\nendusr:\r\nend main'].join(''),'t4': ['org 09000h\r\njp main\r\ninclude const\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude tnu\r\ninclude sp\r\n\r\n;===your code \r\n\r\nright:dw 0\r\n\r\nmain:\r\ntnu.run Main\r\ndef Main.main,0,0\r\nnew.arg .vx,1\r\nnew.arg .vy,0\r\nnew.arg .x,0\r\nnew.arg .y,100\r\nnew Cat,4\r\n\r\nnew.arg .x,100\r\nnew.arg .y,100\r\nnew Target,2\r\n\r\nnew.arg .x,200\r\nnew.arg .y,100\r\nnew Target,2\r\n\r\n\r\nnew.arg .x,150\r\nnew.arg .y,100\r\nnew.arg .c,8\r\nnew NTarget,3\r\n\r\nld (right),hl\r\nld a,h\r\nld de,Actor\r\ncall instanceof\r\ncall showz\r\n\r\nld a,(right+1)\r\nld de,Target\r\ncall instanceof\r\ncall showz\r\n\r\nld a,(right+1)\r\nld de,Cat\r\ncall instanceof\r\ncall showz\r\n\r\n\r\nld hl,1\r\nsetfld .c\r\nenddef 0\r\n\r\nclass Main,Actor\r\n Actor.noovr Main\r\n end.const 0\r\nclass Target,Actor\r\n Actor.noovr Target\r\n met2 Target,.push\r\n end.const 0\r\nclass NTarget,Actor\r\n Actor.noovr NTarget\r\n end.const 0\r\ndef NTarget.main,0,0\r\n ret\r\nenddef\r\n\r\ndef Target.main,0,0\r\nenddef\r\nclass Cat,Actor\r\n Actor.noovr Cat\r\n fld .vy, 0\r\n fld .vx, 0\r\n fld.bottom Cat\r\n end.const 0\r\ndef Cat.main,0,0\r\n blp:\r\n  getthis\r\n  ;x+=vx\r\n  getfld .x\r\n  ex de, hl\r\n  getfld .vx\r\n  add hl,de\r\n  setfld .x\r\n  ; y+=vy\r\n  getfld .y\r\n  ex de, hl\r\n  getfld .vy\r\n  add hl,de\r\n  setfld .y\r\n  ld hl,Target\r\n  push hl\r\n  invoke .crashTo\r\n  jpf cr\r\n   ; r.x+=10\r\n   const setg,hl\r\n   getfldtg .y\r\n   ld de,30\r\n   add hl,de\r\n   push hl\r\n   ldconst hl,setg\r\n   setfldtg .y\r\n  cr:\r\n  invoke .update\r\n jp blp\r\nenddef\r\ndef Target.push,0,0\r\n ld hl,3\r\n setfld .p\r\n repti 30,pse\r\n  getfld .y\r\n  inc hl\r\n  setfld .y\r\n  invoke .update\r\n  continue\r\n pse:\r\n ld hl,0\r\n setfld .p\r\nenddef\r\n\r\nendusr: \r\nend main\r\nhttps://msxpen.com/codes/-N6DDfMvZq9aUeJ9JLpN\r\nhttps://msxpen.com/codes/-N6QGYk-rr5iDuTtHpF7'].join(''),'t5': ['org 9000h\r\n\r\n\r\ninclude key\r\n\r\nmain:\r\ncall keyall\r\nld hl,0108h\r\ncall getkey\r\nshow hl\r\nld hl,0107h\r\ncall getkey\r\nshow hl\r\n\r\n\r\nhalt\r\njp main'].join(''),'gen': ['org 09000h\r\ninclude tnu\r\ninclude bool\r\n\r\nmain:\r\ntnu.run Main\r\nclass Main,Actor\r\n Actor.noovr Main\r\n end.const 0\r\ndef Main.main,0,0\r\n\r\n showlb .main\r\n showlb .crashTo\r\nenddef 0\r\nendusr:\r\nend main'].join(''),'dac': ['org 09000h\r\njp main\r\ninclude const\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude th\r\n\r\n\r\nDECSUB equ 268CH;DAC ← DAC-ARG\r\nDECADD equ 269AH;DAC ← DAC+ARG\r\nDECNRM equ 26FAH;DAC を正規化する (*1)\r\nDECROU equ 273CH;DAC を四捨五入する\r\nDECMUL equ 27E6H;DAC ← DAC*DAC\r\nDECDIV equ 289FH;DAC ← DAC/DAC\r\nMAF equ 2C4DH;ARG ← DAC\r\nMAM equ 2C50H;ARG ← [HL]\r\nMOV8DH equ 2C53H;[DE] ← [HL]\r\nMFA equ 2C59H;DAC ← ARG\r\nMFM equ 2C5CH;[HL] ← DAC\r\nMMF equ 2C67H;[HL] ← DAC\r\nMOV8HD equ 2C6AH;[HL] ← [DE]\r\nXTF equ 2C6FH;[SP] ←→ DAC\r\nPHA equ 2CC7H;ARG → [SP]\r\nPHF equ 2CCCH;DAC → [SP]\r\nPPA equ 2CDCH;[SP] → ARG\r\nPPF equ 2CE1H;[SP] → DAC\r\nPUSHF equ 2EB1H;DAC → [SP]\r\nMOVFM equ 2EBEH;DAC ← [HL]\r\nMOVFR equ 2EC1H;DAC ← (CBED)\r\nMOVRF equ 2ECCH;(CBED) ← DAC\r\nMOVRMI equ 2ED6H;(CBDE) ← [HL]\r\nMOVRM equ 2EDFH;(BCDE) ← [HL]\r\nMOVMF equ 2EE8H;[HL] ← DAC\r\nMOVE equ 2EEBH;[HL] ← [DE]\r\nVMOVAM equ 2EEFH;ARG ← [HL]\r\nMOVVFM equ 2EF2H;[DE] ← [HL]\r\nVMOVE equ 2EF3H;[HL] ← [DE]\r\nVMOVFA equ 2F05H;DAC ← ARG\r\nVMOVFM equ 2F08H;DAC ← [HL]\r\nVMOVAF equ 2F0DH;ARG ← DAC\r\nVMOVMF equ 2F10H;[HL] ← DAC\r\n\r\nVALTYP equ 0F663H;1\r\nDAC equ 0F7F6H;16\r\nARG equ 0F847H;16\r\nFOUT equ 3425H\r\n\r\ndefsub int2dac\r\n push af\r\n ld a,2\r\n ld (VALTYP),a\r\n ld (DAC+2),HL\r\n pop af\r\nendsub int2dac\r\n;===your code \r\n\r\nmain:\r\nld hl,12345\r\ncall int2dac\r\nld hl,str\r\ncall FOUT\r\n\r\nld b,10\r\nreptb nxt\r\n ld a,(hl)\r\n cp 0\r\n break z\r\n call wrt2\r\n inc hl\r\n continue\r\nnxt:\r\nret\r\nstr:\r\n\r\n\r\n'].join(''),'setvrm': ['org 09000h\r\njp main\r\ninclude const\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude th\r\n\r\n;===your code \r\n\r\nmain:\r\nld hl,1800h\r\ncall SETWRT\r\nld a,35\r\nrepti 5, ed\r\ninc a\r\nout (98h),a\r\ncontinue\r\ned:\r\nret'].join(''),'assert': ['include mem\r\ninclude math\r\ninclude debug\r\n\r\na.reg.trc:\r\ndw 0\r\na.reg.adr:\r\ndw 0\r\na.reg.min:\r\ndw 0\r\na.reg.val:\r\ndw 0\r\na.reg.max:\r\ndw 0\r\nmacro a.regi,n,v\r\n push hl\r\n ld hl,v\r\n ld (a.reg.##n),hl\r\n pop hl\r\nendm\r\nmacro a.regr,n,v\r\n ld (a.reg.##n),v\r\nendm\r\n\r\nmacro a.dummy\r\n local a.reg.,trc,adr,min,val,nax\r\nendm\r\n \r\n\r\nmacro assert.eq,o\r\n storelastpc\r\n pushall \r\n if not nul o\r\n  a.regi val, o\r\n endif\r\n ld de,(a.reg.val)\r\n ld(a.reg.val),hl\r\n ld(a.reg.min),de\r\n ld(a.reg.max),de\r\n cpde\r\n jp nz,assert.fail\r\n popall\r\nendm\r\n\r\nmacro assert.do,nx\r\n storelastpc\r\n pushall\r\n call to\r\n popall\r\n jr nx\r\n to:\r\nendm\r\n\r\nmacro storelastpc\r\n push hl\r\n call getpc\r\n ld (lastpc),hl\r\n pop hl\r\nendm\r\nlastpc:\r\n dw 0\r\n \r\ngetpc:\r\n pop hl\r\n push hl\r\n ret\r\n\r\nassert.fail:\r\n ld hl,0deadh\r\n show hl\r\n showm a.reg.trc\r\n showm a.reg.min\r\n showm a.reg.val\r\n showm a.reg.max\r\n showm a.reg.adr\r\n showm lastpc\r\n call freeze\r\nmacro assert.meqw,ad,val\r\n a.regi adr,ad\r\n push hl\r\n ld hl,(ad)\r\n assert.eq val\r\n pop hl\r\nendm\r\n '].join(''),'stksz': ['org 09000h\r\njp main\r\ninclude const\r\ninclude ctrl\r\ninclude math\r\ninclude debug\r\ninclude sub\r\ninclude mem\r\ninclude th\r\n\r\n\r\n;===your code \r\n\r\nsz equ 256\r\n  \r\nmain:\r\nld hl,0fd9fh\r\nld (hl),0c9h\r\n rept sz/2\r\n  push hl\r\n endm\r\n rept sz/2\r\n  pop hl\r\n endm\r\n\r\nloop:\r\n getsp\r\n ld de,-sz\r\n add hl,de\r\n ld de,1800h\r\n ld bc,sz\r\n call LDIRVM\r\n ld hl,0\r\n halt\r\n jp loop\r\n \r\n \r\n '].join(''),'vdp': [';https://www.msx.org/wiki/VDP_Status_Registers\r\n;st 0 bit 7\r\n;read 1\r\n\r\n;https://www.msx.org/wiki/VDP_Mode_Registers\r\n;ctrl 1 bit 5 set 0\r\ninclude const\r\n\r\nsusint:\r\n ld a,(RG1SAV)\r\n res 5,a\r\n ld b,A\r\n ld c,1\r\n jp WRTVDP\r\n;rstint:\r\n ld a,(RG1SAV)\r\n set 5,a\r\n ld b,A\r\n ld c,1\r\n jp WRTVDP\r\ninted:\r\n call RDVDP\r\n bit 7,a\r\n ret\r\ndoint:\r\n call inted\r\n jr z, norst\r\n ld hl,timecnt\r\n inc (hl)\r\n call h.tntimi\r\n norst:\r\n ;call rstint\r\n ret\r\nh.tntimi:\r\n ld A,(timecnt)\r\n ld hl,1ae0h\r\n call 4dh\r\n ret\r\n ds 16\r\n \r\ntimecnt:\r\ndb 0\r\nmacro vdptest\r\nlocal stk1,stk2,stk3,vl\r\nstk1:\r\n ds 256,35\r\nstk2:\r\n ds 256,42\r\nstk3:\r\n\r\nvl:\r\n call susint\r\n ld sp,stk2\r\n ld hl,stk1\r\n ld de,1800h\r\n ld bc,256\r\n call LDIRVM\r\n \r\n \r\n ld sp,stk3\r\n call doint\r\n ld hl,stk2\r\n ld de,1900h\r\n ld bc,256\r\n call LDIRVM\r\n jp vl\r\nendm\r\n \r\nscreen1:\r\n ld a,1\r\n call CHGMOD\r\n ld a,(RG1SAV)\r\n set 1,a\r\n ld b,A\r\n ld c,1\r\n call WRTVDP\r\n ret\r\n\r\ndefsub screen2\r\n ld a,2\r\n call CHGMOD\r\n ld a,(RG1SAV)\r\n set 1,a\r\n ld b,A\r\n ld c,1\r\n call WRTVDP\r\nendsub screen2\r\n '].join(''),'mus': ['include mem\r\nmus.ini:\r\n di\r\n ld hl,0fd9fh\r\n ld (hl),0c3h\r\n movw (0fd9fh+1),mus\r\n ei\r\n ret\r\nmus:\r\nproc\r\nlocal we\r\n push af\r\n push de\r\n ld a,(we-1)\r\n xor 15\r\n ld (we-1),a \r\n ld a,8\r\n ld e,15\r\n we:\r\n call WRTPSG\r\n pop af \r\n pop de\r\n ret\r\nendp'].join('')};
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"data":{}}}
});
Tonyu.klass.define({
  fullName: 'user.FldIdx',
  shortName: 'FldIdx',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.TObject,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_FldIdx_main() {
        "use strict";
        var _this=this;
        
        _this.idx = 256-4-2;
        
        _this._n2idx = {};
        
        _this._n2seq = {};
        
        _this.confli = {};
        
        _this.fldidx();
        _this.build();
      },
      fiber$main :function* _trc_FldIdx_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.idx = 256-4-2;
        
        _this._n2idx = {};
        
        _this._n2seq = {};
        
        _this.confli = {};
        
        (yield* _this.fiber$fldidx(_thread));
        (yield* _this.fiber$build(_thread));
        
      },
      fldidx :function _trc_FldIdx_fldidx() {
        "use strict";
        var _this=this;
        
        let def = ['Actor.noovr,Class\r\n fld .class,Class\r\n meth Class,main\r\n fld.bottom Object\r\n fld .x, 0\r\n fld .y, -1024\r\n fld .p, 0\r\n fld .c, 3\r\n fld.bottom Sprite\r\n meth Actor,onUpdate\r\n meth Actor,update\r\n meth Actor,screenOut \r\n meth Actor,die\r\n meth Actor,updateEx\r\n meth Actor,crashTo\r\n fld.bottom Actor\r\nendm\r\n'].join('');
        
        let fldp = /fld \.(\w+)/;
        
        let metp = /meth.*,(\w+)/;
        
        _this.idx=256-4-2;
        let res = {};
        
        for (let [ln] of Tonyu.iterator2(def.split("\n"),1)) {
          let m = fldp.exec(ln)||metp.exec(ln);
          
          if (m) {
            res[m[1]]=_this.idx;
            _this.n2seq(m[1]);
            _this.idx-=2;
            
          }
          
        }
        _this._n2idx=res;
        return res;
      },
      fiber$fldidx :function* _trc_FldIdx_f_fldidx(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let def = ['Actor.noovr,Class\r\n fld .class,Class\r\n meth Class,main\r\n fld.bottom Object\r\n fld .x, 0\r\n fld .y, -1024\r\n fld .p, 0\r\n fld .c, 3\r\n fld.bottom Sprite\r\n meth Actor,onUpdate\r\n meth Actor,update\r\n meth Actor,screenOut \r\n meth Actor,die\r\n meth Actor,updateEx\r\n meth Actor,crashTo\r\n fld.bottom Actor\r\nendm\r\n'].join('');
        
        let fldp = /fld \.(\w+)/;
        
        let metp = /meth.*,(\w+)/;
        
        _this.idx=256-4-2;
        let res = {};
        
        for (let [ln] of Tonyu.iterator2(def.split("\n"),1)) {
          let m = fldp.exec(ln)||metp.exec(ln);
          
          if (m) {
            res[m[1]]=_this.idx;
            (yield* _this.fiber$n2seq(_thread, m[1]));
            _this.idx-=2;
            
          }
          
        }
        _this._n2idx=res;
        return res;
        
      },
      build :function _trc_FldIdx_build() {
        "use strict";
        var _this=this;
        
        for (let [n] of Tonyu.iterator2(Object.keys(_this._n2seq),1)) {
          if (! _this._n2idx[n]) {
            _this._n2idx[n]=_this.idx;
          }
          
        }
        while (true) {
          Tonyu.checkLoop();
          let cont = false;
          
          for (let [k, i] of Tonyu.iterator2(_this.confli,2)) {
            k=k.split("\t");
            if (_this._n2idx[k[0]]==_this._n2idx[k[1]]) {
              _this._n2idx[k[1]]-=2;
              cont=true;
              
            }
            
          }
          if (! cont) {
            break;
            
          }
          
        }
      },
      fiber$build :function* _trc_FldIdx_f_build(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        for (let [n] of Tonyu.iterator2(Object.keys(_this._n2seq),1)) {
          if (! _this._n2idx[n]) {
            _this._n2idx[n]=_this.idx;
          }
          
        }
        while (true) {
          yield null;
          let cont = false;
          
          for (let [k, i] of Tonyu.iterator2(_this.confli,2)) {
            k=k.split("\t");
            if (_this._n2idx[k[0]]==_this._n2idx[k[1]]) {
              _this._n2idx[k[1]]-=2;
              cont=true;
              
            }
            
          }
          if (! cont) {
            break;
            
          }
          
        }
        
      },
      n2idx :function _trc_FldIdx_n2idx(n) {
        "use strict";
        var _this=this;
        
        let r = _this._n2idx[n];
        
        if (r) {
          return r;
        }
        throw new Error([n,' is not found'].join(''));
        
      },
      fiber$n2idx :function* _trc_FldIdx_f_n2idx(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let r = _this._n2idx[n];
        
        if (r) {
          return r;
        }
        throw new Error([n,' is not found'].join(''));
        
        
      },
      n2seq :function _trc_FldIdx_n2seq(n) {
        "use strict";
        var _this=this;
        
        let r = _this._n2seq[n];
        
        if (r!=null) {
          return r;
        }
        r=Object.keys(_this._n2seq).length;
        _this._n2seq[n]=r;
        return r;
      },
      fiber$n2seq :function* _trc_FldIdx_f_n2seq(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let r = _this._n2seq[n];
        
        if (r!=null) {
          return r;
        }
        r=Object.keys(_this._n2seq).length;
        _this._n2seq[n]=r;
        return r;
        
      },
      idx2n :function _trc_FldIdx_idx2n(i) {
        "use strict";
        var _this=this;
        
        for (let [k, v] of Tonyu.iterator2(_this._n2idx,2)) {
          if (v==i) {
            return k;
          }
          
        }
        return null;
      },
      fiber$idx2n :function* _trc_FldIdx_f_idx2n(_thread,i) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        for (let [k, v] of Tonyu.iterator2(_this._n2idx,2)) {
          if (v==i) {
            return k;
          }
          
        }
        return null;
        
      },
      addConfli :function _trc_FldIdx_addConfli(a,b) {
        "use strict";
        var _this=this;
        
        if (a==b) {
          return _this;
        }
        if (_this.n2seq(a)>_this.n2seq(b)) {
          return _this.addConfli(b,a);
          
        }
        _this.confli[a+"\t"+b]=true;
      },
      fiber$addConfli :function* _trc_FldIdx_f_addConfli(_thread,a,b) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        if (a==b) {
          return _this;
        }
        if (_this.n2seq(a)>_this.n2seq(b)) {
          return yield* _this.fiber$addConfli(_thread, b, a);
          
          
        }
        _this.confli[a+"\t"+b]=true;
        
      },
      isConfli :function _trc_FldIdx_isConfli(a,b) {
        "use strict";
        var _this=this;
        
        if (a==b) {
          return true;
        }
        if (_this.n2seq(a)>_this.n2seq(b)) {
          return _this.isConfli(b,a);
          
        }
        return _this.confli[a+"\t"+b]=true;
      },
      fiber$isConfli :function* _trc_FldIdx_f_isConfli(_thread,a,b) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        if (a==b) {
          return true;
        }
        if (_this.n2seq(a)>_this.n2seq(b)) {
          return yield* _this.fiber$isConfli(_thread, b, a);
          
          
        }
        return _this.confli[a+"\t"+b]=true;
        
      },
      addMembers :function _trc_FldIdx_addMembers(names) {
        "use strict";
        var _this=this;
        
        for (let [name] of Tonyu.iterator2(names,1)) {
          for (let [name2] of Tonyu.iterator2(names,1)) {
            _this.addConfli(name,name2);
            
          }
          
        }
      },
      fiber$addMembers :function* _trc_FldIdx_f_addMembers(_thread,names) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        for (let [name] of Tonyu.iterator2(names,1)) {
          for (let [name2] of Tonyu.iterator2(names,1)) {
            (yield* _this.fiber$addConfli(_thread, name, name2));
            
          }
          
        }
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"fldidx":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"build":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"n2idx":{"nowait":false,"isMain":false,"vtype":{"params":["String"],"returnValue":null}},"n2seq":{"nowait":false,"isMain":false,"vtype":{"params":["String"],"returnValue":null}},"idx2n":{"nowait":false,"isMain":false,"vtype":{"params":["Number"],"returnValue":null}},"addConfli":{"nowait":false,"isMain":false,"vtype":{"params":["String","String"],"returnValue":null}},"isConfli":{"nowait":false,"isMain":false,"vtype":{"params":["String","String"],"returnValue":null}},"addMembers":{"nowait":false,"isMain":false,"vtype":{"params":[{"element":"String"}],"returnValue":null}}},"fields":{"idx":{},"_n2idx":{},"_n2seq":{},"confli":{}}}
});
Tonyu.klass.define({
  fullName: 'user.Buf',
  shortName: 'Buf',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.TObject,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_Buf_main() {
        "use strict";
        var _this=this;
        
        _this.indentBuf = "";
        
        _this.indentStr = " ";
        
        _this.buf = new Tonyu.classes.user.StringBuilder;
        
        
      },
      fiber$main :function* _trc_Buf_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.indentBuf = "";
        
        _this.indentStr = " ";
        
        _this.buf = new Tonyu.classes.user.StringBuilder;
        
        
        
      },
      initModule :function _trc_Buf_initModule() {
        "use strict";
        var _this=this;
        
        _this.indentBuf="";
        _this.indentStr=" ";
        _this.buf=new Tonyu.classes.user.StringBuilder;
        _this.visitor=_this;
      },
      fiber$initModule :function* _trc_Buf_f_initModule(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.indentBuf="";
        _this.indentStr=" ";
        _this.buf=new Tonyu.classes.user.StringBuilder;
        _this.visitor=_this;
        
      },
      printf :function _trc_Buf_printf() {
        "use strict";
        var _this=this;
        function shiftArg(nullable) {
          
          let res = ap.shift();
          
          if (res==null&&! nullable) {
            console.log(ap);
            throw new Error("null param: fmt="+fmt);
            
            
          }
          return res;
        }
        let ap = new Tonyu.classes.kernel.ArgParser(arguments);
        
        
        let fmt = shiftArg();
        
        while (true) {
          Tonyu.checkLoop();
          let i = fmt.indexOf("%");
          
          if (i<0) {
            _this.printraw(fmt);
            break;
            
            
          }
          _this.printraw(fmt.substring(0,i));
          i++;
          let fstr = fmt.charAt(i);
          
          if (fstr=="s") {
            let str = shiftArg();
            
            if (typeof  str=="string"||typeof  str=="number") {
              
              
            } else {
              if (str==null) {
                str="null";
              } else {
                if (str.text) {
                  _this.addMapping(str);
                  str=str.text;
                  
                }
              }
            }
            _this.printraw(str);
            i++;
            
          } else {
            if (fstr=="d") {
              let n = shiftArg();
              
              if (typeof  n!="number") {
                throw new Error(n+" is not a number: fmt="+fmt);
                
              }
              _this.printraw(n);
              i++;
              
            } else {
              if (fstr=="n") {
                _this.ln();
                i++;
                
              } else {
                if (fstr=="{") {
                  _this.indent();
                  i++;
                  
                } else {
                  if (fstr=="}") {
                    _this.dedent();
                    i++;
                    
                  } else {
                    if (fstr=="%") {
                      _this.printraw("%");
                      i++;
                      
                    } else {
                      if (fstr=="f") {
                        shiftArg()(Tonyu.globals.$);
                        i++;
                        
                      } else {
                        if (fstr=="v") {
                          let a = shiftArg();
                          
                          if (! a) {
                            throw new Error("Null %v");
                            
                          }
                          if (typeof  a!="object") {
                            throw new Error("nonobject %v:"+a);
                            
                          }
                          _this.addMapping(a);
                          _this.visitor.visit(a);
                          i++;
                          
                        } else {
                          if (fstr=="j") {
                            let sp_node = shiftArg();
                            
                            let sp = sp_node[0];
                            
                            let node = sp_node[1];
                            
                            let sep = false;
                            
                            if (! node||! node.forEach) {
                              console.log(node);
                              throw new Error(node+" is not array. cannot join fmt:"+fmt);
                              
                              
                            }
                            for (let [n] of Tonyu.iterator2(node,1)) {
                              if (sep) {
                                _this.printf(sp);
                              }
                              sep=true;
                              _this.visitor.visit(n);
                              
                            }
                            i++;
                            
                          } else {
                            if (fstr=="D") {
                              shiftArg(true);
                              i++;
                              
                            } else {
                              i+=2;
                              
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          fmt=fmt.substring(i);
          
        }
      },
      fiber$printf :function* _trc_Buf_f_printf(_thread) {
        "use strict";
        var _this=this;
        var _arguments=Tonyu.A(arguments);
        function shiftArg(nullable) {
          
          let res = ap.shift();
          
          if (res==null&&! nullable) {
            console.log(ap);
            throw new Error("null param: fmt="+fmt);
            
            
          }
          return res;
        }
        let ap = new Tonyu.classes.kernel.ArgParser(_arguments);
        
        
        let fmt = shiftArg();
        
        while (true) {
          yield null;
          let i = fmt.indexOf("%");
          
          if (i<0) {
            (yield* _this.fiber$printraw(_thread, fmt));
            break;
            
            
          }
          (yield* _this.fiber$printraw(_thread, fmt.substring(0,i)));
          i++;
          let fstr = fmt.charAt(i);
          
          if (fstr=="s") {
            let str = shiftArg();
            
            if (typeof  str=="string"||typeof  str=="number") {
              
              
            } else {
              if (str==null) {
                str="null";
              } else {
                if (str.text) {
                  (yield* _this.fiber$addMapping(_thread, str));
                  str=str.text;
                  
                }
              }
            }
            (yield* _this.fiber$printraw(_thread, str));
            i++;
            
          } else {
            if (fstr=="d") {
              let n = shiftArg();
              
              if (typeof  n!="number") {
                throw new Error(n+" is not a number: fmt="+fmt);
                
              }
              (yield* _this.fiber$printraw(_thread, n));
              i++;
              
            } else {
              if (fstr=="n") {
                (yield* _this.fiber$ln(_thread));
                i++;
                
              } else {
                if (fstr=="{") {
                  (yield* _this.fiber$indent(_thread));
                  i++;
                  
                } else {
                  if (fstr=="}") {
                    (yield* _this.fiber$dedent(_thread));
                    i++;
                    
                  } else {
                    if (fstr=="%") {
                      (yield* _this.fiber$printraw(_thread, "%"));
                      i++;
                      
                    } else {
                      if (fstr=="f") {
                        shiftArg()(Tonyu.globals.$);
                        i++;
                        
                      } else {
                        if (fstr=="v") {
                          let a = shiftArg();
                          
                          if (! a) {
                            throw new Error("Null %v");
                            
                          }
                          if (typeof  a!="object") {
                            throw new Error("nonobject %v:"+a);
                            
                          }
                          (yield* _this.fiber$addMapping(_thread, a));
                          _this.visitor.visit(a);
                          i++;
                          
                        } else {
                          if (fstr=="j") {
                            let sp_node = shiftArg();
                            
                            let sp = sp_node[0];
                            
                            let node = sp_node[1];
                            
                            let sep = false;
                            
                            if (! node||! node.forEach) {
                              console.log(node);
                              throw new Error(node+" is not array. cannot join fmt:"+fmt);
                              
                              
                            }
                            for (let [n] of Tonyu.iterator2(node,1)) {
                              if (sep) {
                                (yield* _this.fiber$printf(_thread, sp));
                              }
                              sep=true;
                              _this.visitor.visit(n);
                              
                            }
                            i++;
                            
                          } else {
                            if (fstr=="D") {
                              shiftArg(true);
                              i++;
                              
                            } else {
                              i+=2;
                              
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          fmt=fmt.substring(i);
          
        }
        
      },
      ln :function _trc_Buf_ln() {
        "use strict";
        var _this=this;
        
        _this.printraw("\n"+_this.indentBuf);
      },
      fiber$ln :function* _trc_Buf_f_ln(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        (yield* _this.fiber$printraw(_thread, "\n"+_this.indentBuf));
        
      },
      indent :function _trc_Buf_indent() {
        "use strict";
        var _this=this;
        
        _this.indentBuf+=_this.indentStr;
        _this.printraw("\n"+_this.indentBuf);
      },
      fiber$indent :function* _trc_Buf_f_indent(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.indentBuf+=_this.indentStr;
        (yield* _this.fiber$printraw(_thread, "\n"+_this.indentBuf));
        
      },
      dedent :function _trc_Buf_dedent() {
        "use strict";
        var _this=this;
        
        let len = _this.indentStr.length;
        
        if (! _this.buf.last(len).match(/^\s*$/)) {
          console.log(_this.buf);
          throw new Error("Non-space truncated ");
          
          
        }
        _this.buf.truncate(len);
        _this.indentBuf=_this.indentBuf.substring(0,_this.indentBuf.length-len);
      },
      fiber$dedent :function* _trc_Buf_f_dedent(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let len = _this.indentStr.length;
        
        if (! _this.buf.last(len).match(/^\s*$/)) {
          console.log(_this.buf);
          throw new Error("Non-space truncated ");
          
          
        }
        _this.buf.truncate(len);
        _this.indentBuf=_this.indentBuf.substring(0,_this.indentBuf.length-len);
        
      },
      addMapping :function _trc_Buf_addMapping() {
        "use strict";
        var _this=this;
        
      },
      fiber$addMapping :function* _trc_Buf_f_addMapping(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
      },
      printraw :function _trc_Buf_printraw(s) {
        "use strict";
        var _this=this;
        
        _this.buf.append(s);
      },
      fiber$printraw :function* _trc_Buf_f_printraw(_thread,s) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.buf.append(s);
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"initModule":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"printf":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"ln":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"indent":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"dedent":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"addMapping":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"printraw":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}}},"fields":{"indentBuf":{"vtype":"String"},"indentStr":{"vtype":"String"},"buf":{"vtype":"user.StringBuilder"},"visitor":{"vtype":"user.Visitor"}}}
});
Tonyu.klass.define({
  fullName: 'user.Context',
  shortName: 'Context',
  namespace: 'user',
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_Context_main() {
        "use strict";
        var _this=this;
        
      },
      fiber$main :function* _trc_Context_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
      },
      enter :function _trc_Context_enter(newval,act) {
        "use strict";
        var _this=this;
        
        let sv = {};
        
        let curval = _this;
        
        for (let [k] of Tonyu.iterator2(newval,1)) {
          sv[k]=curval[k];
          curval[k]=newval[k];
          
        }
        let res = act(_this);
        
        for (let [k] of Tonyu.iterator2(sv,1)) {
          curval[k]=sv[k];
          
        }
        return res;
      },
      fiber$enter :function* _trc_Context_f_enter(_thread,newval,act) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let sv = {};
        
        let curval = _this;
        
        for (let [k] of Tonyu.iterator2(newval,1)) {
          sv[k]=curval[k];
          curval[k]=newval[k];
          
        }
        let res = act(_this);
        
        for (let [k] of Tonyu.iterator2(sv,1)) {
          curval[k]=sv[k];
          
        }
        return res;
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"enter":{"nowait":false,"isMain":false,"vtype":{"params":[null,null],"returnValue":null}}},"fields":{}}
});
Tonyu.klass.define({
  fullName: 'user.StringBuilder',
  shortName: 'StringBuilder',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.TObject,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_StringBuilder_main() {
        "use strict";
        var _this=this;
        
        _this.buf = [""];
        
        _this.bufSize = 1024;
        
      },
      fiber$main :function* _trc_StringBuilder_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.buf = [""];
        
        _this.bufSize = 1024;
        
        
      },
      rest :function _trc_StringBuilder_rest(lastIdx) {
        "use strict";
        var _this=this;
        
        return _this.bufSize-_this.buf[lastIdx].length;
      },
      fiber$rest :function* _trc_StringBuilder_f_rest(_thread,lastIdx) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return _this.bufSize-_this.buf[lastIdx].length;
        
      },
      validate :function _trc_StringBuilder_validate() {
        "use strict";
        var _this=this;
        
        for (let i = 0;
         i<_this.buf.length-1 ; i++) {
          Tonyu.checkLoop();
          {
            if (_this.buf[i].length!==_this.bufSize) {
              console.log(_this.buf);
              throw new Error("NO!");
              
              
            }
          }
        }
      },
      fiber$validate :function* _trc_StringBuilder_f_validate(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        for (let i = 0;
         i<_this.buf.length-1 ; i++) {
          yield null;
          {
            if (_this.buf[i].length!==_this.bufSize) {
              console.log(_this.buf);
              throw new Error("NO!");
              
              
            }
          }
        }
        
      },
      append :function _trc_StringBuilder_append(content) {
        "use strict";
        var _this=this;
        
        content=content+"";
        while (content) {
          Tonyu.checkLoop();
          let lastIdx = _this.buf.length-1;
          
          let r = _this.rest(lastIdx);
          
          if (content.length<=r) {
            _this.buf[lastIdx]+=content;
            break;
            
            
          } else {
            _this.buf[lastIdx]+=content.substring(0,r);
            _this.buf.push("");
            content=content.substring(r);
            
          }
          
        }
        _this.validate();
      },
      fiber$append :function* _trc_StringBuilder_f_append(_thread,content) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        content=content+"";
        while (content) {
          yield null;
          let lastIdx = _this.buf.length-1;
          
          let r=yield* _this.fiber$rest(_thread, lastIdx);
          
          if (content.length<=r) {
            _this.buf[lastIdx]+=content;
            break;
            
            
          } else {
            _this.buf[lastIdx]+=content.substring(0,r);
            _this.buf.push("");
            content=content.substring(r);
            
          }
          
        }
        (yield* _this.fiber$validate(_thread));
        
      },
      rowcol :function _trc_StringBuilder_rowcol(index) {
        "use strict";
        var _this=this;
        
        let row = Math.floor(index/_this.bufSize);
        
        let col = index%_this.bufSize;
        
        return {row: row,col: col};
      },
      fiber$rowcol :function* _trc_StringBuilder_f_rowcol(_thread,index) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let row = Math.floor(index/_this.bufSize);
        
        let col = index%_this.bufSize;
        
        return {row: row,col: col};
        
      },
      replace :function _trc_StringBuilder_replace(index,replacement) {
        "use strict";
        var _this=this;
        
        replacement=replacement+"";
        if (replacement.length>_this.bufSize) {
          throw new Error("Cannot replace over len="+_this.bufSize);
          
          
        }
        let start = _this.rowcol(index);
        
        let end = _this.rowcol(index+replacement.length);
        
        if (start.row===end.row) {
          let line = _this.buf[start.row];
          
          _this.buf[start.row]=line.substring(0,start.col)+replacement+line.substring(end.col);
          
        } else {
          let line1 = _this.buf[start.row];
          
          let line2 = _this.buf[end.row];
          
          let len1 = _this.bufSize-start.col;
          
          let len2 = replacement.length-len1;
          
          _this.buf[start.row]=line1.substring(0,start.col)+replacement.substring(0,len1);
          _this.buf[end.row]=replacement.substring(len1)+line2.substring(len2);
          
        }
        _this.validate();
      },
      fiber$replace :function* _trc_StringBuilder_f_replace(_thread,index,replacement) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        replacement=replacement+"";
        if (replacement.length>_this.bufSize) {
          throw new Error("Cannot replace over len="+_this.bufSize);
          
          
        }
        let start=yield* _this.fiber$rowcol(_thread, index);
        
        let end=yield* _this.fiber$rowcol(_thread, index+replacement.length);
        
        if (start.row===end.row) {
          let line = _this.buf[start.row];
          
          _this.buf[start.row]=line.substring(0,start.col)+replacement+line.substring(end.col);
          
        } else {
          let line1 = _this.buf[start.row];
          
          let line2 = _this.buf[end.row];
          
          let len1 = _this.bufSize-start.col;
          
          let len2 = replacement.length-len1;
          
          _this.buf[start.row]=line1.substring(0,start.col)+replacement.substring(0,len1);
          _this.buf[end.row]=replacement.substring(len1)+line2.substring(len2);
          
        }
        (yield* _this.fiber$validate(_thread));
        
      },
      truncate :function _trc_StringBuilder_truncate(length) {
        "use strict";
        var _this=this;
        
        while (true) {
          Tonyu.checkLoop();
          let lastIdx = _this.buf.length-1;
          
          let dec = _this.buf[lastIdx].length-length;
          
          if (dec>=0) {
            _this.buf[lastIdx]=_this.buf[lastIdx].substring(0,dec);
            break;
            
            
          } else {
            _this.buf.pop();
            length=- dec;
            
          }
          
        }
        _this.validate();
      },
      fiber$truncate :function* _trc_StringBuilder_f_truncate(_thread,length) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        while (true) {
          yield null;
          let lastIdx = _this.buf.length-1;
          
          let dec = _this.buf[lastIdx].length-length;
          
          if (dec>=0) {
            _this.buf[lastIdx]=_this.buf[lastIdx].substring(0,dec);
            break;
            
            
          } else {
            _this.buf.pop();
            length=- dec;
            
          }
          
        }
        (yield* _this.fiber$validate(_thread));
        
      },
      getLength :function _trc_StringBuilder_getLength() {
        "use strict";
        var _this=this;
        
        let lastIdx = _this.buf.length-1;
        
        return _this.bufSize*lastIdx+_this.buf[lastIdx].length;
      },
      fiber$getLength :function* _trc_StringBuilder_f_getLength(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let lastIdx = _this.buf.length-1;
        
        return _this.bufSize*lastIdx+_this.buf[lastIdx].length;
        
      },
      last :function _trc_StringBuilder_last(len) {
        "use strict";
        var _this=this;
        
        if (len>_this.bufSize) {
          throw new Error("Cannot replace over len="+_this.bufSize);
          
          
        }
        let lastIdx = _this.buf.length-1;
        
        let deced = _this.buf[lastIdx].length-len;
        
        if (deced>=0) {
          return _this.buf[lastIdx].substring(deced);
          
        } else {
          return _this.buf[lastIdx-1].substring(_this.bufSize+deced)+_this.buf[lastIdx];
          
        }
      },
      fiber$last :function* _trc_StringBuilder_f_last(_thread,len) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        if (len>_this.bufSize) {
          throw new Error("Cannot replace over len="+_this.bufSize);
          
          
        }
        let lastIdx = _this.buf.length-1;
        
        let deced = _this.buf[lastIdx].length-len;
        
        if (deced>=0) {
          return _this.buf[lastIdx].substring(deced);
          
        } else {
          return _this.buf[lastIdx-1].substring(_this.bufSize+deced)+_this.buf[lastIdx];
          
        }
        
      },
      toString :function _trc_StringBuilder_toString() {
        "use strict";
        var _this=this;
        
        return _this.buf.join("");
      },
      fiber$toString :function* _trc_StringBuilder_f_toString(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return _this.buf.join("");
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"rest":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"validate":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"append":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"rowcol":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"replace":{"nowait":false,"isMain":false,"vtype":{"params":[null,null],"returnValue":null}},"truncate":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"getLength":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"last":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"toString":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}}},"fields":{"buf":{"vtype":{"element":"String"}},"bufSize":{"vtype":"Number"}}}
});
Tonyu.klass.define({
  fullName: 'user.VisitorBase',
  shortName: 'VisitorBase',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.Actor,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_VisitorBase_main() {
        "use strict";
        var _this=this;
        
      },
      fiber$main :function* _trc_VisitorBase_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
      },
      visit :function _trc_VisitorBase_visit(n) {
        "use strict";
        var _this=this;
        
        let k = n&&"v_"+n.type;
        
        if (n&&typeof  _this[k]=="function") {
          return _this[k](n);
          
        }
        return _this.def(n);
      },
      fiber$visit :function* _trc_VisitorBase_f_visit(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let k = n&&"v_"+n.type;
        
        if (n&&typeof  _this[k]=="function") {
          return _this[k](n);
          
        }
        return yield* _this.fiber$def(_thread, n);
        
        
      },
      def :function _trc_VisitorBase_def(o) {
        "use strict";
        var _this=this;
        
        if (! o) {
          return _this;
        }
        for (let [k, v] of Tonyu.iterator2(o,2)) {
          _this.visit(o);
          
        }
      },
      fiber$def :function* _trc_VisitorBase_f_def(_thread,o) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        if (! o) {
          return _this;
        }
        for (let [k, v] of Tonyu.iterator2(o,2)) {
          (yield* _this.fiber$visit(_thread, o));
          
        }
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"visit":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"def":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}}},"fields":{}}
});
Tonyu.klass.define({
  fullName: 'user.Includer',
  shortName: 'Includer',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.TObject,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_Includer_main() {
        "use strict";
        var _this=this;
        
        _this.asms = new Tonyu.classes.user.Asms;
        
        
        _this.cmtp = /;([^\n]+)/gi;
        
        _this.inclp = /^include ([\d\w]+)/;
        
      },
      fiber$main :function* _trc_Includer_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.asms = new Tonyu.classes.user.Asms;
        
        
        _this.cmtp = /;([^\n]+)/gi;
        
        _this.inclp = /^include ([\d\w]+)/;
        
        
      },
      gensrc :function _trc_Includer_gensrc(src) {
        "use strict";
        var _this=this;
        
        _this.incled={};
        return _this.writeConfig(_this.incl(src));
      },
      fiber$gensrc :function* _trc_Includer_f_gensrc(_thread,src) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.incled={};
        return yield* _this.fiber$writeConfig(_thread, _this.incl(src));
        
        
      },
      writeConfig :function _trc_Includer_writeConfig(src) {
        "use strict";
        var _this=this;
        
        for (let [k, v] of Tonyu.iterator2(Tonyu.globals.$config,2)) {
          let r = new RegExp(k.replace(/\./g,"\\.")+"\\s+equ\\s+[^\\n]+");
          
          src=src.replace(r,[k,' equ ',v].join(''));
          
        }
        return src;
      },
      fiber$writeConfig :function* _trc_Includer_f_writeConfig(_thread,src) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        for (let [k, v] of Tonyu.iterator2(Tonyu.globals.$config,2)) {
          let r = new RegExp(k.replace(/\./g,"\\.")+"\\s+equ\\s+[^\\n]+");
          
          src=src.replace(r,[k,' equ ',v].join(''));
          
        }
        return src;
        
      },
      incl :function _trc_Includer_incl(src) {
        "use strict";
        var _this=this;
        
        return src.split("\n").map(Tonyu.bindFunc(_this,_this.inclf)).join("\n");
      },
      fiber$incl :function* _trc_Includer_f_incl(_thread,src) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return src.split("\n").map(Tonyu.bindFunc(_this,_this.inclf)).join("\n");
        
      },
      inclf :function _trc_Includer_inclf(line) {
        "use strict";
        var _this=this;
        var m;
        var f;
        
        m = _this.inclp.exec(line);
        
        if (! m) {
          return line;
        }
        f = m[1];
        
        if (_this.incled[f]) {
          return "";
        }
        _this.incled[f]=1;
        return ";include "+f+"\n"+_this.incl(_this.asms.data[f])+"\n"+";end of include "+f;
      },
      fiber$inclf :function* _trc_Includer_f_inclf(_thread,line) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        var m;
        var f;
        
        m = _this.inclp.exec(line);
        
        if (! m) {
          return line;
        }
        f = m[1];
        
        if (_this.incled[f]) {
          return "";
        }
        _this.incled[f]=1;
        return ";include "+f+"\n"+_this.incl(_this.asms.data[f])+"\n"+";end of include "+f;
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"gensrc":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"writeConfig":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"incl":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"inclf":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}}},"fields":{"asms":{"vtype":"user.Asms"},"incled":{},"cmtp":{},"inclp":{}}}
});
Tonyu.klass.define({
  fullName: 'user.MBoot',
  shortName: 'MBoot',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.Boot,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_MBoot_main() {
        "use strict";
        var _this=this;
        
        __superClass.prototype.main.apply( _this, []);
      },
      fiber$main :function* _trc_MBoot_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        (yield* __superClass.prototype.fiber$main.apply( _this, [_thread]));
        
      },
      config :function _trc_MBoot_config() {
        "use strict";
        var _this=this;
        
        Tonyu.globals.$config={"spr.scale": 1,"obj.limits": {PBullet: 5,Enemy: 10},"th.count": 20,"defdbl": ["dbl","$dbl",/^f_/,/^\$f_/],"arrays": {"$mem": 50,"$ary": [1,10,100],"$mat": [[1,3,5],[10,12]],"$f_ary": [1.2,2.3,4.5]},"strings": {"$mesg": "Hello, world","$mesgs": ["Hello","World!!"]}};
      },
      fiber$config :function* _trc_MBoot_f_config(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        Tonyu.globals.$config={"spr.scale": 1,"obj.limits": {PBullet: 5,Enemy: 10},"th.count": 20,"defdbl": ["dbl","$dbl",/^f_/,/^\$f_/],"arrays": {"$mem": 50,"$ary": [1,10,100],"$mat": [[1,3,5],[10,12]],"$f_ary": [1.2,2.3,4.5]},"strings": {"$mesg": "Hello, world","$mesgs": ["Hello","World!!"]}};
        
      },
      createMainObject :function _trc_MBoot_createMainObject() {
        "use strict";
        var _this=this;
        
        _this.config();
        _this.getMainClass();
        Tonyu.globals.$excludeFromAll=Tonyu.globals.$Screen.all();
        Tonyu.globals.$Screen.resize(512,512);
        Tonyu.globals.$MScreen=new Tonyu.classes.kernel.Screen({x: 256,y: 224,width: 256,height: 192,scaleX: 2});
        Tonyu.globals.$MLayer=Tonyu.globals.$MScreen.addLayer();
        Tonyu.globals.$crashTo_size=8<<Tonyu.globals.$config["spr.scale"];
        Tonyu.globals.$RSprPat=new Tonyu.classes.user.RSprPat;
        Tonyu.globals.$MScreen.setBGColor(Tonyu.globals.$RSprPat.palette[3]);
        Tonyu.globals.$Screen.setBGColor(Tonyu.globals.$RSprPat.palette[6]);
        Tonyu.globals.$pat_font_orig=Tonyu.globals.$pat_font;
        Tonyu.globals.$pat_font=0;
        Tonyu.globals.$map=new Tonyu.classes.kernel.Map2({chipWidth: 8,chipHeight: 8,row: 24,col: 32,layer: Tonyu.globals.$MLayer,x: 0,y: 0});
        Tonyu.globals.$exporter=new Tonyu.classes.user.GenAsm;
        let ide = Tonyu.globals.$Boot.getIDE();
        
        if (ide&&Tonyu.globals.$Navigator.isMobile()) {
          ide.runDialog.resize({left: 10,top: 300,width: 400,height: 400});
        }
        if (Tonyu.globals.$editButton) {
          new Tonyu.classes.kernel.Button({top: 480,text: "Edit Page",height: 30,fillStyle: Tonyu.globals.$RSprPat.palette[4],onClick: (function anonymous_2114() {
            
            Tonyu.globals.$editButton.openEditor();
          })});
          
        }
        Tonyu.globals.$mainClassName=_this.mainClass.meta.shortName;
        new _this.mainClass;
      },
      fiber$createMainObject :function* _trc_MBoot_f_createMainObject(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        (yield* _this.fiber$config(_thread));
        (yield* _this.fiber$getMainClass(_thread));
        Tonyu.globals.$excludeFromAll=Tonyu.globals.$Screen.all();
        Tonyu.globals.$Screen.resize(512,512);
        Tonyu.globals.$MScreen=new Tonyu.classes.kernel.Screen({x: 256,y: 224,width: 256,height: 192,scaleX: 2});
        Tonyu.globals.$MLayer=Tonyu.globals.$MScreen.addLayer();
        Tonyu.globals.$crashTo_size=8<<Tonyu.globals.$config["spr.scale"];
        Tonyu.globals.$RSprPat=new Tonyu.classes.user.RSprPat;
        Tonyu.globals.$MScreen.setBGColor(Tonyu.globals.$RSprPat.palette[3]);
        Tonyu.globals.$Screen.setBGColor(Tonyu.globals.$RSprPat.palette[6]);
        Tonyu.globals.$pat_font_orig=Tonyu.globals.$pat_font;
        Tonyu.globals.$pat_font=0;
        Tonyu.globals.$map=new Tonyu.classes.kernel.Map2({chipWidth: 8,chipHeight: 8,row: 24,col: 32,layer: Tonyu.globals.$MLayer,x: 0,y: 0});
        Tonyu.globals.$exporter=new Tonyu.classes.user.GenAsm;
        let ide = Tonyu.globals.$Boot.getIDE();
        
        if (ide&&Tonyu.globals.$Navigator.isMobile()) {
          ide.runDialog.resize({left: 10,top: 300,width: 400,height: 400});
        }
        if (Tonyu.globals.$editButton) {
          new Tonyu.classes.kernel.Button({top: 480,text: "Edit Page",height: 30,fillStyle: Tonyu.globals.$RSprPat.palette[4],onClick: (function anonymous_2114() {
            
            Tonyu.globals.$editButton.openEditor();
          })});
          
        }
        Tonyu.globals.$mainClassName=_this.mainClass.meta.shortName;
        new _this.mainClass;
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"config":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"createMainObject":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}}},"fields":{}}
});
Tonyu.klass.define({
  fullName: 'user.MemberScan',
  shortName: 'MemberScan',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.TObject,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_MemberScan_main() {
        "use strict";
        var _this=this;
        
        
        _this.anodes = _this.genasm.anodes;
        
        _this.Za = (_this.anodes.filter((function anonymous_817(e) {
          
          return e.shortName==="RActor";
        }))[0]);
        
        _this.fld = new Tonyu.classes.user.FldIdx;
        
        _this.ks = _this.anodes.filter((function anonymous_906(klass) {
          
          return _this.inherits(klass,_this.Za)&&klass!==_this.Za;
        }));
        
        _this.obj_limits = _this.initLimitRange(Tonyu.globals.$config["obj.limits"]||{});
        
        _this.name2klass = {};
        
        for (let [klass] of Tonyu.iterator2(_this.ks,1)) {
          _this.name2klass[klass.shortName]=klass;
          _this.proc(klass);
          
        }
        _this.fld.build();
      },
      fiber$main :function* _trc_MemberScan_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        _this.anodes = _this.genasm.anodes;
        
        _this.Za = (_this.anodes.filter((function anonymous_817(e) {
          
          return e.shortName==="RActor";
        }))[0]);
        
        _this.fld = new Tonyu.classes.user.FldIdx;
        
        _this.ks = _this.anodes.filter((function anonymous_906(klass) {
          
          return _this.inherits(klass,_this.Za)&&klass!==_this.Za;
        }));
        
        _this.obj_limits=yield* _this.fiber$initLimitRange(_thread, Tonyu.globals.$config["obj.limits"]||{});
        
        _this.name2klass = {};
        
        for (let [klass] of Tonyu.iterator2(_this.ks,1)) {
          _this.name2klass[klass.shortName]=klass;
          (yield* _this.fiber$proc(_thread, klass));
          
        }
        _this.fld.build();
        
      },
      u :function _trc_MemberScan_u(obj) {
        "use strict";
        var _this=this;
        
        return Object.assign({},obj);
      },
      fiber$u :function* _trc_MemberScan_f_u(_thread,obj) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return Object.assign({},obj);
        
      },
      getMembers :function _trc_MemberScan_getMembers(klass) {
        "use strict";
        var _this=this;
        
        let d = klass.decls;
        
        let f = (d.fields);
        let m = (d.methods);
        
        let s = klass.superclass;
        
        let res = {};
        
        if (klass.shortName!="RActor"&&s) {
          res=_this.getMembers(s);
        }
        for (let [n, v] of Tonyu.iterator2(f,2)) {
          res[n]={type: "fld",declin: klass};
          
        }
        for (let [n, v] of Tonyu.iterator2(m,2)) {
          if (n=="new") {
            continue;
            
          }
          res[n]={type: "meth",declin: klass};
          
        }
        return res;
      },
      fiber$getMembers :function* _trc_MemberScan_f_getMembers(_thread,klass) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let d = klass.decls;
        
        let f = (d.fields);
        let m = (d.methods);
        
        let s = klass.superclass;
        
        let res = {};
        
        if (klass.shortName!="RActor"&&s) {
          res=(yield* _this.fiber$getMembers(_thread, s));
        }
        for (let [n, v] of Tonyu.iterator2(f,2)) {
          res[n]={type: "fld",declin: klass};
          
        }
        for (let [n, v] of Tonyu.iterator2(m,2)) {
          if (n=="new") {
            continue;
            
          }
          res[n]={type: "meth",declin: klass};
          
        }
        return res;
        
      },
      inherits :function _trc_MemberScan_inherits(klass,parent) {
        "use strict";
        var _this=this;
        
        if (klass===parent) {
          return true;
        }
        if (! klass.superclass) {
          return false;
        }
        return _this.inherits(klass.superclass,parent);
      },
      fiber$inherits :function* _trc_MemberScan_f_inherits(_thread,klass,parent) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        if (klass===parent) {
          return true;
        }
        if (! klass.superclass) {
          return false;
        }
        return yield* _this.fiber$inherits(_thread, klass.superclass, parent);
        
        
      },
      initLimitRange :function _trc_MemberScan_initLimitRange(lims) {
        "use strict";
        var _this=this;
        
        let res = {};
        
        let base = {};
        
        let allCnt = Tonyu.globals.$config["th.count"];
        let cur = allCnt;
        
        for (let [klass, num] of Tonyu.iterator2(lims,2)) {
          res[klass]=[cur-num,cur];
          cur-=num;
          
        }
        if (cur<0) {
          throw new Error(['Config error: obj.limit exceeded th.count(',allCnt,')'].join(''));
          
          
        }
        res["*"]=[0,cur];
        return res;
      },
      fiber$initLimitRange :function* _trc_MemberScan_f_initLimitRange(_thread,lims) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let res = {};
        
        let base = {};
        
        let allCnt = Tonyu.globals.$config["th.count"];
        let cur = allCnt;
        
        for (let [klass, num] of Tonyu.iterator2(lims,2)) {
          res[klass]=[cur-num,cur];
          cur-=num;
          
        }
        if (cur<0) {
          throw new Error(['Config error: obj.limit exceeded th.count(',allCnt,')'].join(''));
          
          
        }
        res["*"]=[0,cur];
        return res;
        
      },
      objRange :function _trc_MemberScan_objRange(klass) {
        "use strict";
        var _this=this;
        
        for (let [klassEnt, num] of Tonyu.iterator2(_this.obj_limits,2)) {
          klassEnt=_this.name2klass[klassEnt];
          if (_this.inherits(klass,klassEnt)) {
            return num;
          }
          
        }
        return _this.obj_limits["*"];
      },
      fiber$objRange :function* _trc_MemberScan_f_objRange(_thread,klass) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        for (let [klassEnt, num] of Tonyu.iterator2(_this.obj_limits,2)) {
          klassEnt=_this.name2klass[klassEnt];
          if (_this.inherits(klass,klassEnt)) {
            return num;
          }
          
        }
        return _this.obj_limits["*"];
        
      },
      proc :function _trc_MemberScan_proc(klass) {
        "use strict";
        var _this=this;
        
        let r = _this.getMembers(klass);
        
        _this.fld.addMembers(Object.keys(r));
      },
      fiber$proc :function* _trc_MemberScan_f_proc(_thread,klass) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let r=yield* _this.fiber$getMembers(_thread, klass);
        
        _this.fld.addMembers(Object.keys(r));
        
      },
      header :function _trc_MemberScan_header(klass) {
        "use strict";
        var _this=this;
        
        let r = _this.getMembers(klass);
        
        let min = 256;
        let max = 0;
        
        let rev = {};
        
        _this.printf("class %s,%s%{",klass.shortName,klass.superclass.shortName);
        for (let [k, v] of Tonyu.iterator2(r,2)) {
          let i = _this.fld.n2idx(k);
          
          if (i<min) {
            min=i;
          }
          if (i>max) {
            max=i;
          }
          rev[i]=k;
          
        }
        for (let i = max;
         i>=min ; i-=2) {
          Tonyu.checkLoop();
          {
            let k = rev[i];
            
            if (! k) {
              _this.printf("unused%n");
              continue;
              
              
            }
            let ri = r[k];
            
            if (ri.type=="fld") {
              _this.printf("fld .%s,0%n",k);
              
            } else {
              _this.printf('meth %s,%s%n',ri.declin.shortName,k);
              
            }
          }
        }
        _this.printf("end.const %s%n%}",klass.shortName);
      },
      fiber$header :function* _trc_MemberScan_f_header(_thread,klass) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let r=yield* _this.fiber$getMembers(_thread, klass);
        
        let min = 256;
        let max = 0;
        
        let rev = {};
        
        (yield* _this.fiber$printf(_thread, "class %s,%s%{", klass.shortName, klass.superclass.shortName));
        for (let [k, v] of Tonyu.iterator2(r,2)) {
          let i = _this.fld.n2idx(k);
          
          if (i<min) {
            min=i;
          }
          if (i>max) {
            max=i;
          }
          rev[i]=k;
          
        }
        for (let i = max;
         i>=min ; i-=2) {
          yield null;
          {
            let k = rev[i];
            
            if (! k) {
              (yield* _this.fiber$printf(_thread, "unused%n"));
              continue;
              
              
            }
            let ri = r[k];
            
            if (ri.type=="fld") {
              (yield* _this.fiber$printf(_thread, "fld .%s,0%n", k));
              
            } else {
              (yield* _this.fiber$printf(_thread, 'meth %s,%s%n', ri.declin.shortName, k));
              
            }
          }
        }
        (yield* _this.fiber$printf(_thread, "end.const %s%n%}", klass.shortName));
        
      },
      printf :function _trc_MemberScan_printf() {
        "use strict";
        var _this=this;
        
        _this.genasm.printf.apply(_this.genasm,arguments);
      },
      fiber$printf :function* _trc_MemberScan_f_printf(_thread) {
        "use strict";
        var _this=this;
        var _arguments=Tonyu.A(arguments);
        
        _this.genasm.printf.apply(_this.genasm,_arguments);
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"u":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"getMembers":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"inherits":{"nowait":false,"isMain":false,"vtype":{"params":[null,null],"returnValue":null}},"initLimitRange":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"objRange":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"proc":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"header":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"printf":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}}},"fields":{"genasm":{"vtype":"user.GenAsm"},"anodes":{},"Za":{},"fld":{"vtype":"user.FldIdx"},"ks":{},"obj_limits":{},"name2klass":{}}}
});
Tonyu.klass.define({
  fullName: 'user.Catches',
  shortName: 'Catches',
  namespace: 'user',
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_Catches_main() {
        "use strict";
        var _this=this;
        
      },
      fiber$main :function* _trc_Catches_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{}}
});
Tonyu.klass.define({
  fullName: 'user.C_Meta',
  shortName: 'C_Meta',
  namespace: 'user',
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_C_Meta_main() {
        "use strict";
        var _this=this;
        
        
        
        
        
        
      },
      fiber$main :function* _trc_C_Meta_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"decls":{},"superclass":{"vtype":"user.C_Meta"},"src":{},"node":{"vtype":"user.Program"},"annotation":{}}}
});
Tonyu.klass.define({
  fullName: 'user.ForHead',
  shortName: 'ForHead',
  namespace: 'user',
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_ForHead_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_ForHead_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"type":{"vtype":"String"}}}
});
Tonyu.klass.define({
  fullName: 'user.FuncInfo',
  shortName: 'FuncInfo',
  namespace: 'user',
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_FuncInfo_main() {
        "use strict";
        var _this=this;
        
        
        
        
        
        
        
        
        
        
        
        
        
        
      },
      fiber$main :function* _trc_FuncInfo_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"klass":{"vtype":"user.C_Meta"},"node":{"vtype":"user.FuncDecl"},"head":{"vtype":"user.FuncDeclHead"},"ftype":{"vtype":"String"},"name":{"vtype":"String"},"isMain":{"vtype":"Boolean"},"stmts":{"vtype":{"element":"user.Stmt"}},"locals":{"vtype":"user.Locals"},"params":{"vtype":{"element":"user.ParamDecl"}},"scope":{},"useArgs":{"vtype":"Boolean"},"paramTypes":{},"returnType":{}}}
});
Tonyu.klass.define({
  fullName: 'user.Locals',
  shortName: 'Locals',
  namespace: 'user',
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_Locals_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_Locals_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"varDecls":{},"subFuncDecls":{}}}
});
Tonyu.klass.define({
  fullName: 'user.ObjOrFuncArg',
  shortName: 'ObjOrFuncArg',
  namespace: 'user',
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_ObjOrFuncArg_main() {
        "use strict";
        var _this=this;
        
      },
      fiber$main :function* _trc_ObjOrFuncArg_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{}}
});
Tonyu.klass.define({
  fullName: 'user.Stmt',
  shortName: 'Stmt',
  namespace: 'user',
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_Stmt_main() {
        "use strict";
        var _this=this;
        
      },
      fiber$main :function* _trc_Stmt_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{}}
});
Tonyu.klass.define({
  fullName: 'user.TNode',
  shortName: 'TNode',
  namespace: 'user',
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_TNode_main() {
        "use strict";
        var _this=this;
        
        
        
        
        
        
        
      },
      fiber$main :function* _trc_TNode_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"type":{"vtype":"String"},"row":{"vtype":"Number"},"col":{"vtype":"Number"},"pos":{"vtype":"Number"},"len":{"vtype":"Number"},"text":{"vtype":"String"}}}
});
Tonyu.klass.define({
  fullName: 'user.Try',
  shortName: 'Try',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_Try_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_Try_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"stmt":{"vtype":"user.Stmt"},"catches":{"vtype":{"element":"user.Catches"}}}}
});
Tonyu.klass.define({
  fullName: 'user.TypeDecl',
  shortName: 'TypeDecl',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_TypeDecl_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_TypeDecl_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"vtype":{"vtype":"user.TypeExpr"}}}
});
Tonyu.klass.define({
  fullName: 'user.TypeExpr',
  shortName: 'TypeExpr',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_TypeExpr_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_TypeExpr_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"name":{"vtype":"user.Token"}}}
});
Tonyu.klass.define({
  fullName: 'user.VarDecl',
  shortName: 'VarDecl',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_VarDecl_main() {
        "use strict";
        var _this=this;
        
        
        
        
      },
      fiber$main :function* _trc_VarDecl_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"name":{"vtype":"user.Token"},"typeDecl":{"vtype":"user.TypeDecl"},"value":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.VarsDecl',
  shortName: 'VarsDecl',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_VarsDecl_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_VarsDecl_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"decls":{"vtype":{"element":"user.VarDecl"}},"declPrefix":{"vtype":"user.Token"}}}
});
Tonyu.klass.define({
  fullName: 'user.While',
  shortName: 'While',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_While_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_While_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"cond":{"vtype":"user.Expression"},"loop":{"vtype":"user.Stmt"}}}
});
Tonyu.klass.define({
  fullName: 'user.Visitor',
  shortName: 'Visitor',
  namespace: 'user',
  superclass: Tonyu.classes.user.VisitorBase,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_Visitor_main() {
        "use strict";
        var _this=this;
        
      },
      fiber$main :function* _trc_Visitor_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
      },
      v_String :function _trc_Visitor_v_String(n) {
        "use strict";
        var _this=this;
        
        let text = n.text;
        
        _this.def(n);
      },
      fiber$v_String :function* _trc_Visitor_f_v_String(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let text = n.text;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_prefix :function _trc_Visitor_v_prefix(n) {
        "use strict";
        var _this=this;
        
        let op = n.op;
        
        let right = n.right;
        
        _this.def(n);
      },
      fiber$v_prefix :function* _trc_Visitor_f_v_prefix(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let op = n.op;
        
        let right = n.right;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_postfix :function _trc_Visitor_v_postfix(n) {
        "use strict";
        var _this=this;
        
        let left = n.left;
        
        let op = n.op;
        
        _this.def(n);
      },
      fiber$v_postfix :function* _trc_Visitor_f_v_postfix(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let left = n.left;
        
        let op = n.op;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_infix :function _trc_Visitor_v_infix(n) {
        "use strict";
        var _this=this;
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        _this.def(n);
      },
      fiber$v_infix :function* _trc_Visitor_f_v_infix(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_trifix :function _trc_Visitor_v_trifix(n) {
        "use strict";
        var _this=this;
        
        let left = n.left;
        
        let op1 = n.op1;
        
        let mid = n.mid;
        
        let op2 = n.op2;
        
        let right = n.right;
        
        _this.def(n);
      },
      fiber$v_trifix :function* _trc_Visitor_f_v_trifix(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let left = n.left;
        
        let op1 = n.op1;
        
        let mid = n.mid;
        
        let op2 = n.op2;
        
        let right = n.right;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_arrayElem :function _trc_Visitor_v_arrayElem(n) {
        "use strict";
        var _this=this;
        
        let subscript = n.subscript;
        
        _this.def(n);
      },
      fiber$v_arrayElem :function* _trc_Visitor_f_v_arrayElem(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let subscript = n.subscript;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_argList :function _trc_Visitor_v_argList(n) {
        "use strict";
        var _this=this;
        
        let args = n.args;
        
        _this.def(n);
      },
      fiber$v_argList :function* _trc_Visitor_f_v_argList(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let args = n.args;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_member :function _trc_Visitor_v_member(n) {
        "use strict";
        var _this=this;
        
        let name = n.name;
        
        _this.def(n);
      },
      fiber$v_member :function* _trc_Visitor_f_v_member(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let name = n.name;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_parenExpr :function _trc_Visitor_v_parenExpr(n) {
        "use strict";
        var _this=this;
        
        let expr = n.expr;
        
        _this.def(n);
      },
      fiber$v_parenExpr :function* _trc_Visitor_f_v_parenExpr(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let expr = n.expr;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_varAccess :function _trc_Visitor_v_varAccess(n) {
        "use strict";
        var _this=this;
        
        let name = n.name;
        
        _this.def(n);
      },
      fiber$v_varAccess :function* _trc_Visitor_f_v_varAccess(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let name = n.name;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_funcExprArg :function _trc_Visitor_v_funcExprArg(n) {
        "use strict";
        var _this=this;
        
        let obj = n.obj;
        
        _this.def(n);
      },
      fiber$v_funcExprArg :function* _trc_Visitor_f_v_funcExprArg(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let obj = n.obj;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_objlitArg :function _trc_Visitor_v_objlitArg(n) {
        "use strict";
        var _this=this;
        
        let obj = n.obj;
        
        _this.def(n);
      },
      fiber$v_objlitArg :function* _trc_Visitor_f_v_objlitArg(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let obj = n.obj;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_call :function _trc_Visitor_v_call(n) {
        "use strict";
        var _this=this;
        
        let args = n.args;
        
        _this.def(n);
      },
      fiber$v_call :function* _trc_Visitor_f_v_call(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let args = n.args;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_scall :function _trc_Visitor_v_scall(n) {
        "use strict";
        var _this=this;
        
        let args = n.args;
        
        _this.def(n);
      },
      fiber$v_scall :function* _trc_Visitor_f_v_scall(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let args = n.args;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_newExpr :function _trc_Visitor_v_newExpr(n) {
        "use strict";
        var _this=this;
        
        let klass = n.klass;
        
        let params = n.params;
        
        _this.def(n);
      },
      fiber$v_newExpr :function* _trc_Visitor_f_v_newExpr(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let klass = n.klass;
        
        let params = n.params;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_superExpr :function _trc_Visitor_v_superExpr(n) {
        "use strict";
        var _this=this;
        
        let name = n.name;
        
        let params = n.params;
        
        _this.def(n);
      },
      fiber$v_superExpr :function* _trc_Visitor_f_v_superExpr(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let name = n.name;
        
        let params = n.params;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_exprstmt :function _trc_Visitor_v_exprstmt(n) {
        "use strict";
        var _this=this;
        
        let expr = n.expr;
        
        _this.def(n);
      },
      fiber$v_exprstmt :function* _trc_Visitor_f_v_exprstmt(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let expr = n.expr;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_compound :function _trc_Visitor_v_compound(n) {
        "use strict";
        var _this=this;
        
        let stmts = n.stmts;
        
        _this.def(n);
      },
      fiber$v_compound :function* _trc_Visitor_f_v_compound(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let stmts = n.stmts;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_return :function _trc_Visitor_v_return(n) {
        "use strict";
        var _this=this;
        
        let value = n.value;
        
        _this.def(n);
      },
      fiber$v_return :function* _trc_Visitor_f_v_return(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let value = n.value;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_if :function _trc_Visitor_v_if(n) {
        "use strict";
        var _this=this;
        
        let cond = n.cond;
        
        let then = n.then;
        
        let _else = n._else;
        
        _this.def(n);
      },
      fiber$v_if :function* _trc_Visitor_f_v_if(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let cond = n.cond;
        
        let then = n.then;
        
        let _else = n._else;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_forin :function _trc_Visitor_v_forin(n) {
        "use strict";
        var _this=this;
        
        let isVar = n.isVar;
        
        let vars = n.vars;
        
        let inof = n.inof;
        
        let set = n.set;
        
        _this.def(n);
      },
      fiber$v_forin :function* _trc_Visitor_f_v_forin(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let isVar = n.isVar;
        
        let vars = n.vars;
        
        let inof = n.inof;
        
        let set = n.set;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_normalFor :function _trc_Visitor_v_normalFor(n) {
        "use strict";
        var _this=this;
        
        let init = n.init;
        
        let cond = n.cond;
        
        let next = n.next;
        
        _this.def(n);
      },
      fiber$v_normalFor :function* _trc_Visitor_f_v_normalFor(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let init = n.init;
        
        let cond = n.cond;
        
        let next = n.next;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_for :function _trc_Visitor_v_for(n) {
        "use strict";
        var _this=this;
        
        let inFor = n.inFor;
        
        let loop = n.loop;
        
        _this.def(n);
      },
      fiber$v_for :function* _trc_Visitor_f_v_for(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let inFor = n.inFor;
        
        let loop = n.loop;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_while :function _trc_Visitor_v_while(n) {
        "use strict";
        var _this=this;
        
        let cond = n.cond;
        
        let loop = n.loop;
        
        _this.def(n);
      },
      fiber$v_while :function* _trc_Visitor_f_v_while(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let cond = n.cond;
        
        let loop = n.loop;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_do :function _trc_Visitor_v_do(n) {
        "use strict";
        var _this=this;
        
        let loop = n.loop;
        
        let cond = n.cond;
        
        _this.def(n);
      },
      fiber$v_do :function* _trc_Visitor_f_v_do(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let loop = n.loop;
        
        let cond = n.cond;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_case :function _trc_Visitor_v_case(n) {
        "use strict";
        var _this=this;
        
        let value = n.value;
        
        let stmts = n.stmts;
        
        _this.def(n);
      },
      fiber$v_case :function* _trc_Visitor_f_v_case(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let value = n.value;
        
        let stmts = n.stmts;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_default :function _trc_Visitor_v_default(n) {
        "use strict";
        var _this=this;
        
        let stmts = n.stmts;
        
        _this.def(n);
      },
      fiber$v_default :function* _trc_Visitor_f_v_default(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let stmts = n.stmts;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_switch :function _trc_Visitor_v_switch(n) {
        "use strict";
        var _this=this;
        
        let value = n.value;
        
        let cases = n.cases;
        
        let defs = n.defs;
        
        _this.def(n);
      },
      fiber$v_switch :function* _trc_Visitor_f_v_switch(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let value = n.value;
        
        let cases = n.cases;
        
        let defs = n.defs;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_break :function _trc_Visitor_v_break(n) {
        "use strict";
        var _this=this;
        
        let brk = n.brk;
        
        _this.def(n);
      },
      fiber$v_break :function* _trc_Visitor_f_v_break(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let brk = n.brk;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_continue :function _trc_Visitor_v_continue(n) {
        "use strict";
        var _this=this;
        
        let cont = n.cont;
        
        _this.def(n);
      },
      fiber$v_continue :function* _trc_Visitor_f_v_continue(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let cont = n.cont;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_finally :function _trc_Visitor_v_finally(n) {
        "use strict";
        var _this=this;
        
        let stmt = n.stmt;
        
        _this.def(n);
      },
      fiber$v_finally :function* _trc_Visitor_f_v_finally(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let stmt = n.stmt;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_catch :function _trc_Visitor_v_catch(n) {
        "use strict";
        var _this=this;
        
        let name = n.name;
        
        let stmt = n.stmt;
        
        _this.def(n);
      },
      fiber$v_catch :function* _trc_Visitor_f_v_catch(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let name = n.name;
        
        let stmt = n.stmt;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_try :function _trc_Visitor_v_try(n) {
        "use strict";
        var _this=this;
        
        let stmt = n.stmt;
        
        let catches = n.catches;
        
        _this.def(n);
      },
      fiber$v_try :function* _trc_Visitor_f_v_try(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let stmt = n.stmt;
        
        let catches = n.catches;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_throw :function _trc_Visitor_v_throw(n) {
        "use strict";
        var _this=this;
        
        let ex = n.ex;
        
        _this.def(n);
      },
      fiber$v_throw :function* _trc_Visitor_f_v_throw(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let ex = n.ex;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_typeExpr :function _trc_Visitor_v_typeExpr(n) {
        "use strict";
        var _this=this;
        
        let name = n.name;
        
        _this.def(n);
      },
      fiber$v_typeExpr :function* _trc_Visitor_f_v_typeExpr(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let name = n.name;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_typeDecl :function _trc_Visitor_v_typeDecl(n) {
        "use strict";
        var _this=this;
        
        let vtype = n.vtype;
        
        _this.def(n);
      },
      fiber$v_typeDecl :function* _trc_Visitor_f_v_typeDecl(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let vtype = n.vtype;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_varDecl :function _trc_Visitor_v_varDecl(n) {
        "use strict";
        var _this=this;
        
        let name = n.name;
        
        let typeDecl = n.typeDecl;
        
        let value = n.value;
        
        _this.def(n);
      },
      fiber$v_varDecl :function* _trc_Visitor_f_v_varDecl(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let name = n.name;
        
        let typeDecl = n.typeDecl;
        
        let value = n.value;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_varsDecl :function _trc_Visitor_v_varsDecl(n) {
        "use strict";
        var _this=this;
        
        let decls = n.decls;
        
        _this.def(n);
      },
      fiber$v_varsDecl :function* _trc_Visitor_f_v_varsDecl(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let decls = n.decls;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_paramDecl :function _trc_Visitor_v_paramDecl(n) {
        "use strict";
        var _this=this;
        
        let name = n.name;
        
        let typeDecl = n.typeDecl;
        
        _this.def(n);
      },
      fiber$v_paramDecl :function* _trc_Visitor_f_v_paramDecl(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let name = n.name;
        
        let typeDecl = n.typeDecl;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_paramDecls :function _trc_Visitor_v_paramDecls(n) {
        "use strict";
        var _this=this;
        
        let params = n.params;
        
        _this.def(n);
      },
      fiber$v_paramDecls :function* _trc_Visitor_f_v_paramDecls(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let params = n.params;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_setterDecl :function _trc_Visitor_v_setterDecl(n) {
        "use strict";
        var _this=this;
        
        let value = n.value;
        
        _this.def(n);
      },
      fiber$v_setterDecl :function* _trc_Visitor_f_v_setterDecl(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let value = n.value;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_funcDeclHead :function _trc_Visitor_v_funcDeclHead(n) {
        "use strict";
        var _this=this;
        
        let _nowait = n._nowait;
        
        let ftype = n.ftype;
        
        let name = n.name;
        
        let setter = n.setter;
        
        let params = n.params;
        
        let rtype = n.rtype;
        
        _this.def(n);
      },
      fiber$v_funcDeclHead :function* _trc_Visitor_f_v_funcDeclHead(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let _nowait = n._nowait;
        
        let ftype = n.ftype;
        
        let name = n.name;
        
        let setter = n.setter;
        
        let params = n.params;
        
        let rtype = n.rtype;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_funcDecl :function _trc_Visitor_v_funcDecl(n) {
        "use strict";
        var _this=this;
        
        let head = n.head;
        
        let body = n.body;
        
        _this.def(n);
      },
      fiber$v_funcDecl :function* _trc_Visitor_f_v_funcDecl(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let head = n.head;
        
        let body = n.body;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_nativeDecl :function _trc_Visitor_v_nativeDecl(n) {
        "use strict";
        var _this=this;
        
        let name = n.name;
        
        _this.def(n);
      },
      fiber$v_nativeDecl :function* _trc_Visitor_f_v_nativeDecl(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let name = n.name;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_ifWait :function _trc_Visitor_v_ifWait(n) {
        "use strict";
        var _this=this;
        
        let then = n.then;
        
        let _else = n._else;
        
        _this.def(n);
      },
      fiber$v_ifWait :function* _trc_Visitor_f_v_ifWait(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let then = n.then;
        
        let _else = n._else;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_empty :function _trc_Visitor_v_empty(n) {
        "use strict";
        var _this=this;
        
        _this.def(n);
      },
      fiber$v_empty :function* _trc_Visitor_f_v_empty(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_funcExprHead :function _trc_Visitor_v_funcExprHead(n) {
        "use strict";
        var _this=this;
        
        let name = n.name;
        
        let params = n.params;
        
        _this.def(n);
      },
      fiber$v_funcExprHead :function* _trc_Visitor_f_v_funcExprHead(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let name = n.name;
        
        let params = n.params;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_funcExpr :function _trc_Visitor_v_funcExpr(n) {
        "use strict";
        var _this=this;
        
        let head = n.head;
        
        let body = n.body;
        
        _this.def(n);
      },
      fiber$v_funcExpr :function* _trc_Visitor_f_v_funcExpr(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let head = n.head;
        
        let body = n.body;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_jsonElem :function _trc_Visitor_v_jsonElem(n) {
        "use strict";
        var _this=this;
        
        let key = n.key;
        
        let value = n.value;
        
        _this.def(n);
      },
      fiber$v_jsonElem :function* _trc_Visitor_f_v_jsonElem(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let key = n.key;
        
        let value = n.value;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_objlit :function _trc_Visitor_v_objlit(n) {
        "use strict";
        var _this=this;
        
        let elems = n.elems;
        
        _this.def(n);
      },
      fiber$v_objlit :function* _trc_Visitor_f_v_objlit(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let elems = n.elems;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_arylit :function _trc_Visitor_v_arylit(n) {
        "use strict";
        var _this=this;
        
        let elems = n.elems;
        
        _this.def(n);
      },
      fiber$v_arylit :function* _trc_Visitor_f_v_arylit(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let elems = n.elems;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_extends :function _trc_Visitor_v_extends(n) {
        "use strict";
        var _this=this;
        
        let superclassName = n.superclassName;
        
        _this.def(n);
      },
      fiber$v_extends :function* _trc_Visitor_f_v_extends(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let superclassName = n.superclassName;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_includes :function _trc_Visitor_v_includes(n) {
        "use strict";
        var _this=this;
        
        let includeClassNames = n.includeClassNames;
        
        _this.def(n);
      },
      fiber$v_includes :function* _trc_Visitor_f_v_includes(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let includeClassNames = n.includeClassNames;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      v_program :function _trc_Visitor_v_program(n) {
        "use strict";
        var _this=this;
        
        let ext = n.ext;
        
        let incl = n.incl;
        
        let stmts = n.stmts;
        
        _this.def(n);
      },
      fiber$v_program :function* _trc_Visitor_f_v_program(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let ext = n.ext;
        
        let incl = n.incl;
        
        let stmts = n.stmts;
        
        (yield* _this.fiber$def(_thread, n));
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"v_String":{"nowait":false,"isMain":false,"vtype":{"params":["user.Token"],"returnValue":null}},"v_prefix":{"nowait":false,"isMain":false,"vtype":{"params":["user.Prefix"],"returnValue":null}},"v_postfix":{"nowait":false,"isMain":false,"vtype":{"params":["user.Postfix"],"returnValue":null}},"v_infix":{"nowait":false,"isMain":false,"vtype":{"params":["user.Infix"],"returnValue":null}},"v_trifix":{"nowait":false,"isMain":false,"vtype":{"params":["user.Trifix"],"returnValue":null}},"v_arrayElem":{"nowait":false,"isMain":false,"vtype":{"params":["user.ArrayElem"],"returnValue":null}},"v_argList":{"nowait":false,"isMain":false,"vtype":{"params":["user.ArgList"],"returnValue":null}},"v_member":{"nowait":false,"isMain":false,"vtype":{"params":["user.Member"],"returnValue":null}},"v_parenExpr":{"nowait":false,"isMain":false,"vtype":{"params":["user.ParenExpr"],"returnValue":null}},"v_varAccess":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarAccess"],"returnValue":null}},"v_funcExprArg":{"nowait":false,"isMain":false,"vtype":{"params":["user.FuncExprArg"],"returnValue":null}},"v_objlitArg":{"nowait":false,"isMain":false,"vtype":{"params":["user.ObjlitArg"],"returnValue":null}},"v_call":{"nowait":false,"isMain":false,"vtype":{"params":["user.Call"],"returnValue":null}},"v_scall":{"nowait":false,"isMain":false,"vtype":{"params":["user.Scall"],"returnValue":null}},"v_newExpr":{"nowait":false,"isMain":false,"vtype":{"params":["user.NewExpr"],"returnValue":null}},"v_superExpr":{"nowait":false,"isMain":false,"vtype":{"params":["user.SuperExpr"],"returnValue":null}},"v_exprstmt":{"nowait":false,"isMain":false,"vtype":{"params":["user.Exprstmt"],"returnValue":null}},"v_compound":{"nowait":false,"isMain":false,"vtype":{"params":["user.Compound"],"returnValue":null}},"v_return":{"nowait":false,"isMain":false,"vtype":{"params":["user.Return"],"returnValue":null}},"v_if":{"nowait":false,"isMain":false,"vtype":{"params":["user.If"],"returnValue":null}},"v_forin":{"nowait":false,"isMain":false,"vtype":{"params":["user.Forin"],"returnValue":null}},"v_normalFor":{"nowait":false,"isMain":false,"vtype":{"params":["user.NormalFor"],"returnValue":null}},"v_for":{"nowait":false,"isMain":false,"vtype":{"params":["user.For"],"returnValue":null}},"v_while":{"nowait":false,"isMain":false,"vtype":{"params":["user.While"],"returnValue":null}},"v_do":{"nowait":false,"isMain":false,"vtype":{"params":["user.Do"],"returnValue":null}},"v_case":{"nowait":false,"isMain":false,"vtype":{"params":["user.Case"],"returnValue":null}},"v_default":{"nowait":false,"isMain":false,"vtype":{"params":["user.Default"],"returnValue":null}},"v_switch":{"nowait":false,"isMain":false,"vtype":{"params":["user.Switch"],"returnValue":null}},"v_break":{"nowait":false,"isMain":false,"vtype":{"params":["user.Break"],"returnValue":null}},"v_continue":{"nowait":false,"isMain":false,"vtype":{"params":["user.Continue"],"returnValue":null}},"v_finally":{"nowait":false,"isMain":false,"vtype":{"params":["user.Finally"],"returnValue":null}},"v_catch":{"nowait":false,"isMain":false,"vtype":{"params":["user.Catch"],"returnValue":null}},"v_try":{"nowait":false,"isMain":false,"vtype":{"params":["user.Try"],"returnValue":null}},"v_throw":{"nowait":false,"isMain":false,"vtype":{"params":["user.Throw"],"returnValue":null}},"v_typeExpr":{"nowait":false,"isMain":false,"vtype":{"params":["user.TypeExpr"],"returnValue":null}},"v_typeDecl":{"nowait":false,"isMain":false,"vtype":{"params":["user.TypeDecl"],"returnValue":null}},"v_varDecl":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarDecl"],"returnValue":null}},"v_varsDecl":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarsDecl"],"returnValue":null}},"v_paramDecl":{"nowait":false,"isMain":false,"vtype":{"params":["user.ParamDecl"],"returnValue":null}},"v_paramDecls":{"nowait":false,"isMain":false,"vtype":{"params":["user.ParamDecls"],"returnValue":null}},"v_setterDecl":{"nowait":false,"isMain":false,"vtype":{"params":["user.SetterDecl"],"returnValue":null}},"v_funcDeclHead":{"nowait":false,"isMain":false,"vtype":{"params":["user.FuncDeclHead"],"returnValue":null}},"v_funcDecl":{"nowait":false,"isMain":false,"vtype":{"params":["user.FuncDecl"],"returnValue":null}},"v_nativeDecl":{"nowait":false,"isMain":false,"vtype":{"params":["user.NativeDecl"],"returnValue":null}},"v_ifWait":{"nowait":false,"isMain":false,"vtype":{"params":["user.IfWait"],"returnValue":null}},"v_empty":{"nowait":false,"isMain":false,"vtype":{"params":["user.Empty"],"returnValue":null}},"v_funcExprHead":{"nowait":false,"isMain":false,"vtype":{"params":["user.FuncExprHead"],"returnValue":null}},"v_funcExpr":{"nowait":false,"isMain":false,"vtype":{"params":["user.FuncExpr"],"returnValue":null}},"v_jsonElem":{"nowait":false,"isMain":false,"vtype":{"params":["user.JsonElem"],"returnValue":null}},"v_objlit":{"nowait":false,"isMain":false,"vtype":{"params":["user.Objlit"],"returnValue":null}},"v_arylit":{"nowait":false,"isMain":false,"vtype":{"params":["user.Arylit"],"returnValue":null}},"v_extends":{"nowait":false,"isMain":false,"vtype":{"params":["user.Extends"],"returnValue":null}},"v_includes":{"nowait":false,"isMain":false,"vtype":{"params":["user.Includes"],"returnValue":null}},"v_program":{"nowait":false,"isMain":false,"vtype":{"params":["user.Program"],"returnValue":null}}},"fields":{}}
});
Tonyu.klass.define({
  fullName: 'user.OutBG',
  shortName: 'OutBG',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.Actor,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_OutBG_main() {
        "use strict";
        var _this=this;
        
        _this.pa = new Tonyu.classes.kernel.Panel({x: 16,y: 16,width: 8,height: 8,scaleX: 1});
        
        Tonyu.globals.$RSprPat=Tonyu.globals.$RSprPat||new Tonyu.classes.user.RSprPat;
        _this.buf = '';
        _this.count = 0;
        
        _this.patgen = [];
        _this.coltbl = [];
        
        for (let i = 0;
         i<Tonyu.globals.$imageList.length ; i++) {
          Tonyu.checkLoop();
          {
            let l = Tonyu.globals.$imageList[i];
            
            if (l.width!=8) {
              continue;
              
            }
            if (l.height!=8) {
              continue;
              
            }
            if (_this.count>=256) {
              break;
              
            }
            _this.count++;
            _this.pa.clearRect();
            _this.pa.drawSprite(4,4,i);
            let d = _this.pa.getImageData(0,0,8,8);
            
            d=d.data;
            _this.out(d);
            if (i%8==0) {
              _this.updateEx(1);
            }
          }
        }
        _this.pa.die();
        _this.fireEvent("complete");
      },
      fiber$main :function* _trc_OutBG_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.pa = new Tonyu.classes.kernel.Panel({x: 16,y: 16,width: 8,height: 8,scaleX: 1});
        
        Tonyu.globals.$RSprPat=Tonyu.globals.$RSprPat||new Tonyu.classes.user.RSprPat;
        _this.buf = '';
        _this.count = 0;
        
        _this.patgen = [];
        _this.coltbl = [];
        
        for (let i = 0;
         i<Tonyu.globals.$imageList.length ; i++) {
          yield null;
          {
            let l = Tonyu.globals.$imageList[i];
            
            if (l.width!=8) {
              continue;
              
            }
            if (l.height!=8) {
              continue;
              
            }
            if (_this.count>=256) {
              break;
              
            }
            _this.count++;
            _this.pa.clearRect();
            _this.pa.drawSprite(4,4,i);
            let d = _this.pa.getImageData(0,0,8,8);
            
            d=d.data;
            (yield* _this.fiber$out(_thread, d));
            if (i%8==0) {
              (yield* _this.fiber$updateEx(_thread, 1));
            }
          }
        }
        _this.pa.die();
        _this.fireEvent("complete");
        
      },
      out :function _trc_OutBG_out(d) {
        "use strict";
        var _this=this;
        
        let pals = Tonyu.globals.$RSprPat.palette;
        
        if (! pals) {
          throw new Error("No pals");
          
        }
        for (let y = 0;
         y<8 ; y++) {
          Tonyu.checkLoop();
          {
            let line = [];
            let colc = {};
            
            for (let x = 0;
             x<8 ; x++) {
              Tonyu.checkLoop();
              {
                let i = (y*8+x)*4;
                
                let c = new Tonyu.classes.kernel.Color(d[i],d[i+1],d[i+2]);
                
                let min = 100000;
                let minc = 0;
                
                for (let [i, pal] of Tonyu.iterator2(pals,2)) {
                  let di = c.distHSLA(pal);
                  
                  if (di<min) {
                    minc=i;
                    min=di;
                    
                  }
                  
                }
                minc++;
                line.push(minc);
                colc[minc]=colc[minc]||0;
                colc[minc]++;
              }
            }
            let cols = Object.keys(colc).map((function anonymous_1324(e) {
              
              return e-0;
            })).sort((function anonymous_1368(a,b) {
              
              return colc[b]-colc[a];
            }));
            
            if (cols.length<2) {
              _this.coltbl.push(cols[0]);
              _this.patgen.push(0);
              continue;
              
              
            }
            let bgc = pals[cols[0]-1];
            
            let fgc = pals[cols[1]-1];
            
            _this.coltbl.push(cols[1]*16+cols[0]);
            if (cols[1]*16+cols[0]>=256) {
              throw new Error(cols+" "+(cols[1]*16+cols[0]));
              
              
            }
            let bits = "0b";
            
            for (let [dot] of Tonyu.iterator2(line,1)) {
              let bit = cols.indexOf(dot);
              
              if (bit<0) {
                throw new Error(cols+"indexOf "+dot);
                
              }
              if (bit>=2) {
                let c = pals[cols[bit]-1];
                
                let df = fgc.distHSLA(c);
                
                let db = bgc.distHSLA(c);
                
                bit=(df<db?1:0);
                
              }
              bits+=bit;
              
            }
            _this.patgen.push(bits-0);
          }
        }
      },
      fiber$out :function* _trc_OutBG_f_out(_thread,d) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let pals = Tonyu.globals.$RSprPat.palette;
        
        if (! pals) {
          throw new Error("No pals");
          
        }
        for (let y = 0;
         y<8 ; y++) {
          yield null;
          {
            let line = [];
            let colc = {};
            
            for (let x = 0;
             x<8 ; x++) {
              yield null;
              {
                let i = (y*8+x)*4;
                
                let c = new Tonyu.classes.kernel.Color(d[i],d[i+1],d[i+2]);
                
                let min = 100000;
                let minc = 0;
                
                for (let [i, pal] of Tonyu.iterator2(pals,2)) {
                  let di = c.distHSLA(pal);
                  
                  if (di<min) {
                    minc=i;
                    min=di;
                    
                  }
                  
                }
                minc++;
                line.push(minc);
                colc[minc]=colc[minc]||0;
                colc[minc]++;
              }
            }
            let cols = Object.keys(colc).map((function anonymous_1324(e) {
              
              return e-0;
            })).sort((function anonymous_1368(a,b) {
              
              return colc[b]-colc[a];
            }));
            
            if (cols.length<2) {
              _this.coltbl.push(cols[0]);
              _this.patgen.push(0);
              continue;
              
              
            }
            let bgc = pals[cols[0]-1];
            
            let fgc = pals[cols[1]-1];
            
            _this.coltbl.push(cols[1]*16+cols[0]);
            if (cols[1]*16+cols[0]>=256) {
              throw new Error(cols+" "+(cols[1]*16+cols[0]));
              
              
            }
            let bits = "0b";
            
            for (let [dot] of Tonyu.iterator2(line,1)) {
              let bit = cols.indexOf(dot);
              
              if (bit<0) {
                throw new Error(cols+"indexOf "+dot);
                
              }
              if (bit>=2) {
                let c = pals[cols[bit]-1];
                
                let df = fgc.distHSLA(c);
                
                let db = bgc.distHSLA(c);
                
                bit=(df<db?1:0);
                
              }
              bits+=bit;
              
            }
            _this.patgen.push(bits-0);
          }
        }
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"out":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}}},"fields":{"pa":{"vtype":"kernel.Panel"},"buf":{"vtype":"String"},"count":{"vtype":"Number"},"patgen":{},"coltbl":{}}}
});
Tonyu.klass.define({
  fullName: 'user.OutPat',
  shortName: 'OutPat',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.TObject,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_OutPat_main() {
        "use strict";
        var _this=this;
        
        _this.pa = new Tonyu.classes.kernel.Panel({x: 200,y: 200,width: 16,height: 16,scaleX: 10});
        
        _this.buf = '';
        _this.count = 0;
        
        for (let i = 0;
         i<Tonyu.globals.$imageList.length ; i++) {
          Tonyu.checkLoop();
          {
            let l = Tonyu.globals.$imageList[i];
            
            if (l.width!=16) {
              continue;
              
            }
            if (l.height!=16) {
              continue;
              
            }
            if (_this.count>=64) {
              break;
              
            }
            _this.count++;
            _this.pa.clearRect();
            _this.pa.drawSprite(8,8,i);
            let d = _this.pa.getImageData(0,0,8,16);
            
            d=d.data;
            _this.out(d);
            d=_this.pa.getImageData(8,0,8,16);
            d=d.data;
            _this.out(d);
          }
        }
        _this.pa.die();
      },
      fiber$main :function* _trc_OutPat_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.pa = new Tonyu.classes.kernel.Panel({x: 200,y: 200,width: 16,height: 16,scaleX: 10});
        
        _this.buf = '';
        _this.count = 0;
        
        for (let i = 0;
         i<Tonyu.globals.$imageList.length ; i++) {
          yield null;
          {
            let l = Tonyu.globals.$imageList[i];
            
            if (l.width!=16) {
              continue;
              
            }
            if (l.height!=16) {
              continue;
              
            }
            if (_this.count>=64) {
              break;
              
            }
            _this.count++;
            _this.pa.clearRect();
            _this.pa.drawSprite(8,8,i);
            let d = _this.pa.getImageData(0,0,8,16);
            
            d=d.data;
            (yield* _this.fiber$out(_thread, d));
            d=_this.pa.getImageData(8,0,8,16);
            d=d.data;
            (yield* _this.fiber$out(_thread, d));
          }
        }
        _this.pa.die();
        
      },
      out :function _trc_OutPat_out(d) {
        "use strict";
        var _this=this;
        
        let c = "0b";
        
        let db = [];
        
        for (let i = 3;
         i<d.length ; i+=4) {
          Tonyu.checkLoop();
          {
            c+=(d[i]>128?1:0);
            if (c.length>=10) {
              c=(c-0).toString(16);
              c="0"+c+"h";
              db.push(c);
              c="0b";
              
            }
          }
        }
        _this.buf+="db "+db+"\n";
      },
      fiber$out :function* _trc_OutPat_f_out(_thread,d) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let c = "0b";
        
        let db = [];
        
        for (let i = 3;
         i<d.length ; i+=4) {
          yield null;
          {
            c+=(d[i]>128?1:0);
            if (c.length>=10) {
              c=(c-0).toString(16);
              c="0"+c+"h";
              db.push(c);
              c="0b";
              
            }
          }
        }
        _this.buf+="db "+db+"\n";
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"out":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}}},"fields":{"pa":{"vtype":"kernel.Panel"},"buf":{"vtype":"String"},"count":{"vtype":"Number"}}}
});
Tonyu.klass.define({
  fullName: 'user.RSprite',
  shortName: 'RSprite',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.Actor,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_RSprite_main() {
        "use strict";
        var _this=this;
        
        
        
        _this.dotAlign = 1;
        
      },
      fiber$main :function* _trc_RSprite_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        _this.dotAlign = 1;
        
        
      },
      __getter__spr_scale :function _trc_RSprite___getter__spr_scale() {
        "use strict";
        var _this=this;
        
        return Tonyu.globals.$config["spr.scale"];
      },
      initialize :function _trc_RSprite_initialize(params) {
        "use strict";
        var _this=this;
        
        __superClass.apply( _this, [params]);
        if (! Tonyu.globals.$RSprPat) {
          Tonyu.globals.$RSprPat=new Tonyu.classes.user.RSprPat;
        }
        _this.sprpat=_this.sprpat||Tonyu.globals.$RSprPat;
        _this.dotAlign=_this.dotAlign||1;
        _this.width=Tonyu.globals.$crashTo_size;
        _this.height=Tonyu.globals.$crashTo_size;
      },
      setPalette :function _trc_RSprite_setPalette(n,c) {
        "use strict";
        var _this=this;
        
        _this.sprpat.palette[n-1]=c;
      },
      fiber$setPalette :function* _trc_RSprite_f_setPalette(_thread,n,c) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.sprpat.palette[n-1]=c;
        
      },
      map_setAt :function _trc_RSprite_map_setAt(x,y,p) {
        "use strict";
        var _this=this;
        
        Tonyu.globals.$map.set(x>>(3+_this.spr_scale),y>>(3+_this.spr_scale),Tonyu.globals.$pat_font_orig+p);
      },
      fiber$map_setAt :function* _trc_RSprite_f_map_setAt(_thread,x,y,p) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        Tonyu.globals.$map.set(x>>(3+_this.spr_scale),y>>(3+_this.spr_scale),Tonyu.globals.$pat_font_orig+p);
        
      },
      map_getAt :function _trc_RSprite_map_getAt(x,y) {
        "use strict";
        var _this=this;
        
        return Tonyu.globals.$map.get(x>>(3+_this.spr_scale),y>>(3+_this.spr_scale))-Tonyu.globals.$pat_font_orig;
      },
      fiber$map_getAt :function* _trc_RSprite_f_map_getAt(_thread,x,y) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return Tonyu.globals.$map.get(x>>(3+_this.spr_scale),y>>(3+_this.spr_scale))-Tonyu.globals.$pat_font_orig;
        
      },
      map_set :function _trc_RSprite_map_set(x,y,p) {
        "use strict";
        var _this=this;
        
        Tonyu.globals.$map.set(x,y,Tonyu.globals.$pat_font_orig+p);
      },
      fiber$map_set :function* _trc_RSprite_f_map_set(_thread,x,y,p) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        Tonyu.globals.$map.set(x,y,Tonyu.globals.$pat_font_orig+p);
        
      },
      map_get :function _trc_RSprite_map_get(x,y) {
        "use strict";
        var _this=this;
        
        return Tonyu.globals.$map.get(x,y)-Tonyu.globals.$pat_font_orig;
      },
      fiber$map_get :function* _trc_RSprite_f_map_get(_thread,x,y) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return Tonyu.globals.$map.get(x,y)-Tonyu.globals.$pat_font_orig;
        
      },
      div :function _trc_RSprite_div(x,y) {
        "use strict";
        var _this=this;
        
        return _this.floor(x/y);
      },
      fiber$div :function* _trc_RSprite_f_div(_thread,x,y) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return _this.floor(x/y);
        
      },
      draw :function _trc_RSprite_draw(ctx) {
        "use strict";
        var _this=this;
        
        let pa = _this.sprpat.get(_this.p,_this.c);
        
        if (pa==null) {
          return _this;
        }
        ctx.save();
        let sx = _this.x;
        let sy = _this.y;
        
        _this.x=_this.floor(_this.x/_this.dotAlign)*_this.dotAlign>>_this.spr_scale;
        _this.y=_this.floor(_this.y/_this.dotAlign)*_this.dotAlign>>_this.spr_scale;
        _this.performTransform(ctx);
        ctx.drawImage(pa.image,- _this.floor(pa.width/2),- _this.floor(pa.height/2));
        _this.x=sx;
        _this.y=sy;
        ctx.restore();
      },
      __getter__defaultLayer :function _trc_RSprite___getter__defaultLayer() {
        "use strict";
        var _this=this;
        
        return Tonyu.globals.$MLayer;
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"__getter__spr_scale":{"nowait":true,"isMain":false,"vtype":{"params":[],"returnValue":null}},"new":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"setPalette":{"nowait":false,"isMain":false,"vtype":{"params":[null,null],"returnValue":null}},"map_setAt":{"nowait":false,"isMain":false,"vtype":{"params":[null,null,null],"returnValue":null}},"map_getAt":{"nowait":false,"isMain":false,"vtype":{"params":[null,null],"returnValue":null}},"map_set":{"nowait":false,"isMain":false,"vtype":{"params":[null,null,null],"returnValue":null}},"map_get":{"nowait":false,"isMain":false,"vtype":{"params":[null,null],"returnValue":null}},"div":{"nowait":false,"isMain":false,"vtype":{"params":[null,null],"returnValue":null}},"draw":{"nowait":true,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"__getter__defaultLayer":{"nowait":true,"isMain":false,"vtype":{"params":[],"returnValue":null}}},"fields":{"sprpat":{},"p":{},"c":{},"dotAlign":{"vtype":"Number"}}}
});
Tonyu.klass.define({
  fullName: 'user.RSprPat',
  shortName: 'RSprPat',
  namespace: 'user',
  superclass: Tonyu.classes.kernel.TObject,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_RSprPat_main() {
        "use strict";
        var _this=this;
        
        _this.palette = _this.palette||[[0,0,0],[64,183,74],[117,207,126],[89,86,215],[128,119,239],[184,95,81],[102,219,238],[217,102,90],[253,138,126],[204,195,96],[222,208,136],[59,161,66],[182,103,180],[204,204,204],[255,255,255]].map((function anonymous_243(rgb) {
          
          return new Tonyu.classes.kernel.Color(rgb[0],rgb[1],rgb[2]);
        }));
        
        _this.imgs = {};
        
      },
      fiber$main :function* _trc_RSprPat_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.palette = _this.palette||[[0,0,0],[64,183,74],[117,207,126],[89,86,215],[128,119,239],[184,95,81],[102,219,238],[217,102,90],[253,138,126],[204,195,96],[222,208,136],[59,161,66],[182,103,180],[204,204,204],[255,255,255]].map((function anonymous_243(rgb) {
          
          return new Tonyu.classes.kernel.Color(rgb[0],rgb[1],rgb[2]);
        }));
        
        _this.imgs = {};
        
        
      },
      get :function _trc_RSprPat_get(p,c) {
        "use strict";
        var _this=this;
        
        c=_this.amod(c,(_this.palette.length+1));
        if (c<=0) {
          return null;
        }
        let key = [p,'\\t',c].join('');
        
        if (_this.imgs[key]) {
          return _this.imgs[key];
        }
        let ls = Tonyu.globals.$imageList[p];
        
        if (! ls) {
          return null;
        }
        let res = new Tonyu.classes.kernel.Panel({width: ls.width,height: ls.height});
        
        _this.imgs[key]=res;
        res.context.drawImage(ls.image,ls.x,ls.y,ls.width,ls.height,0,0,ls.width,ls.height);
        res.replaceColor(0,0,ls.width,ls.height,_this.palette[c-1]);
        res.die();
        return res;
      },
      fiber$get :function* _trc_RSprPat_f_get(_thread,p,c) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        c=_this.amod(c,(_this.palette.length+1));
        if (c<=0) {
          return null;
        }
        let key = [p,'\\t',c].join('');
        
        if (_this.imgs[key]) {
          return _this.imgs[key];
        }
        let ls = Tonyu.globals.$imageList[p];
        
        if (! ls) {
          return null;
        }
        let res = new Tonyu.classes.kernel.Panel({width: ls.width,height: ls.height});
        
        _this.imgs[key]=res;
        res.context.drawImage(ls.image,ls.x,ls.y,ls.width,ls.height,0,0,ls.width,ls.height);
        res.replaceColor(0,0,ls.width,ls.height,_this.palette[c-1]);
        res.die();
        return res;
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"get":{"nowait":false,"isMain":false,"vtype":{"params":["Number","Number"],"returnValue":null}}},"fields":{"palette":{},"imgs":{}}}
});
Tonyu.klass.define({
  fullName: 'user.GenAsm',
  shortName: 'GenAsm',
  namespace: 'user',
  superclass: Tonyu.classes.user.Visitor,
  includes: [Tonyu.classes.user.Buf,Tonyu.classes.user.Context],
  methods: function (__superClass) {
    return {
      main :function _trc_GenAsm_main() {
        "use strict";
        var _this=this;
        
        _this.initModule();
        _this.IDEPrj = Tonyu.globals.$currentProject.compiler;
        
        _this.anodes = _this.waitFor(_this.IDEPrj.serializeAnnotatedNodes());
        
        _this.mem = new Tonyu.classes.user.MemberScan({genasm: _this});
        
        _this.lval = false;
        
        _this.symSeq = 1;
        
        _this.ide = Tonyu.globals.$Boot.getIDE();
        
        
        _this.problems = [];
        
        _this.builtins = {getkey: "getkey",rnd: "rnd"};
        
        _this.globals = {};
        
        _this.keynames = {space: 264,right: 32776,down: 16392,up: 8200,left: 4104};
        
        _this.opmap = {"*": "call IMULT.a%n","/": "call IDIV.a%n","%": "call IMOD.a%n","+": "add hl, de%n","-": "subhl de%n"};
        
        _this.printf(['org 08000h\r\ninclude tnu\r\ninclude map\r\ninclude bool\r\n\r\nmain:\r\ntnu.run ',Tonyu.globals.$mainClassName,'\r\n'].join(''));
        
        _this.x=256;
        _this.y=20;
        _this.fillStyle="black";
        for ([_this.klass] of Tonyu.iterator2(_this.mem.ks,1)) {
          let r = _this.mem.objRange(_this.klass);
          
          _this.text=_this.klass.shortName;
          _this.printf(";range %d-%d%n",r[0],r[1]);
          _this.mem.header(_this.klass);
          _this.klassSrc=_this.klass.src.tonyu.text();
          let methods = _this.klass.decls.methods;
          
          _this.anode=(_this.klass.annotation);
          for (let [mname, me] of Tonyu.iterator2(methods,2)) {
            let method = me;
            
            _this.text=_this.klass.shortName+"."+mname;
            if (method.params.length>0) {
              _this.unsup(method.params[0],"Parameters is not yet supported");
              
            }
            _this.printf("def %s.%s,%d,0%{",_this.klass.shortName,mname,method.params.length);
            for (let [s] of Tonyu.iterator2(method.stmts,1)) {
              _this.visit(s);
            }
            _this.printf("enddef %s.%s%n",_this.klass.shortName,mname);
            _this.printf("%}");
            _this.update();
            
          }
          _this.update();
          
        }
        _this.text="";
        _this.outp = new Tonyu.classes.user.OutPat;
        
        _this.outbg = new Tonyu.classes.user.OutBG;
        
        _this.waitEvent(_this.outbg,"complete");
        _this.printf(['\r\nendusr:\r\n',Object.keys(_this.globals).map((function anonymous_1790(k) {
          
          return _this.globalLabel(k)+":dw 0";
        })).join("\n"),'\r\nmacro inivrm, n, dst\r\n ld de,dst\r\n ld hl,n\r\n ld bc,end.##n-n\r\n call LDIRVM\r\nendm\r\n\r\nspr.inipat:\r\n inivrm spr.pat, 3800h\r\n ret\r\n\r\nspr.pat:\r\n',_this.outp.buf,'\r\nend.spr.pat:\r\n\r\nbg.inipat:\r\n inivrm bg.gen, 0000h\r\n inivrm bg.gen, 0800h\r\n inivrm bg.gen, 1000h\r\n inivrm bg.col, 2000h\r\n inivrm bg.col, 2800h\r\n inivrm bg.col, 3000h\r\n inivrm bg.name, 1800h\r\n ret\r\n\r\nbg.gen:\r\n',_this.toDB(_this.outbg.patgen),'\r\nend.bg.gen:\r\n\r\nbg.col:\r\n',_this.toDB(_this.outbg.coltbl),'\r\nend.bg.col:\r\n\r\nbg.name:\r\n ds 256*3,32\r\nend.bg.name:\r\n\r\nend main'].join(''));
        _this.url = "https://msxpen.com/codes/-N8klu22ZKY0trVaYX66";
        
        new Tonyu.classes.kernel.Button({top: 420,text: "to MSXPen",onClick: Tonyu.bindFunc(_this,_this.showDiag),fillStyle: Tonyu.globals.$RSprPat.palette[2]});
      },
      fiber$main :function* _trc_GenAsm_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        (yield* _this.fiber$initModule(_thread));
        _this.IDEPrj = Tonyu.globals.$currentProject.compiler;
        
        _this.anodes=yield* _this.fiber$waitFor(_thread, _this.IDEPrj.serializeAnnotatedNodes());
        
        _this.mem = new Tonyu.classes.user.MemberScan({genasm: _this});
        
        _this.lval = false;
        
        _this.symSeq = 1;
        
        _this.ide = Tonyu.globals.$Boot.getIDE();
        
        
        _this.problems = [];
        
        _this.builtins = {getkey: "getkey",rnd: "rnd"};
        
        _this.globals = {};
        
        _this.keynames = {space: 264,right: 32776,down: 16392,up: 8200,left: 4104};
        
        _this.opmap = {"*": "call IMULT.a%n","/": "call IDIV.a%n","%": "call IMOD.a%n","+": "add hl, de%n","-": "subhl de%n"};
        
        (yield* _this.fiber$printf(_thread, ['org 08000h\r\ninclude tnu\r\ninclude map\r\ninclude bool\r\n\r\nmain:\r\ntnu.run ',Tonyu.globals.$mainClassName,'\r\n'].join('')));
        
        _this.x=256;
        _this.y=20;
        _this.fillStyle="black";
        for ([_this.klass] of Tonyu.iterator2(_this.mem.ks,1)) {
          let r = _this.mem.objRange(_this.klass);
          
          _this.text=_this.klass.shortName;
          (yield* _this.fiber$printf(_thread, ";range %d-%d%n", r[0], r[1]));
          _this.mem.header(_this.klass);
          _this.klassSrc=_this.klass.src.tonyu.text();
          let methods = _this.klass.decls.methods;
          
          _this.anode=(_this.klass.annotation);
          for (let [mname, me] of Tonyu.iterator2(methods,2)) {
            let method = me;
            
            _this.text=_this.klass.shortName+"."+mname;
            if (method.params.length>0) {
              (yield* _this.fiber$unsup(_thread, method.params[0], "Parameters is not yet supported"));
              
            }
            (yield* _this.fiber$printf(_thread, "def %s.%s,%d,0%{", _this.klass.shortName, mname, method.params.length));
            for (let [s] of Tonyu.iterator2(method.stmts,1)) {
              (yield* _this.fiber$visit(_thread, s));
            }
            (yield* _this.fiber$printf(_thread, "enddef %s.%s%n", _this.klass.shortName, mname));
            (yield* _this.fiber$printf(_thread, "%}"));
            (yield* _this.fiber$update(_thread));
            
          }
          (yield* _this.fiber$update(_thread));
          
        }
        _this.text="";
        _this.outp = new Tonyu.classes.user.OutPat;
        
        _this.outbg = new Tonyu.classes.user.OutBG;
        
        (yield* _this.fiber$waitEvent(_thread, _this.outbg, "complete"));
        (yield* _this.fiber$printf(_thread, ['\r\nendusr:\r\n',Object.keys(_this.globals).map((function anonymous_1790(k) {
          
          return _this.globalLabel(k)+":dw 0";
        })).join("\n"),'\r\nmacro inivrm, n, dst\r\n ld de,dst\r\n ld hl,n\r\n ld bc,end.##n-n\r\n call LDIRVM\r\nendm\r\n\r\nspr.inipat:\r\n inivrm spr.pat, 3800h\r\n ret\r\n\r\nspr.pat:\r\n',_this.outp.buf,'\r\nend.spr.pat:\r\n\r\nbg.inipat:\r\n inivrm bg.gen, 0000h\r\n inivrm bg.gen, 0800h\r\n inivrm bg.gen, 1000h\r\n inivrm bg.col, 2000h\r\n inivrm bg.col, 2800h\r\n inivrm bg.col, 3000h\r\n inivrm bg.name, 1800h\r\n ret\r\n\r\nbg.gen:\r\n',_this.toDB(_this.outbg.patgen),'\r\nend.bg.gen:\r\n\r\nbg.col:\r\n',_this.toDB(_this.outbg.coltbl),'\r\nend.bg.col:\r\n\r\nbg.name:\r\n ds 256*3,32\r\nend.bg.name:\r\n\r\nend main'].join('')));
        _this.url = "https://msxpen.com/codes/-N8klu22ZKY0trVaYX66";
        
        new Tonyu.classes.kernel.Button({top: 420,text: "to MSXPen",onClick: Tonyu.bindFunc(_this,_this.showDiag),fillStyle: Tonyu.globals.$RSprPat.palette[2]});
        
      },
      annotation :function _trc_GenAsm_annotation(n) {
        "use strict";
        var _this=this;
        
        let res = _this.anode[n._id];
        
        if (! res) {
          return {};
          
        }
        return res;
      },
      fiber$annotation :function* _trc_GenAsm_f_annotation(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let res = _this.anode[n._id];
        
        if (! res) {
          return {};
          
        }
        return res;
        
      },
      toDB :function _trc_GenAsm_toDB(bytes) {
        "use strict";
        var _this=this;
        
        let buf = "";
        let c = [];
        
        while (bytes.length) {
          Tonyu.checkLoop();
          let b = bytes.shift();
          
          c.push("0"+b.toString(16)+"h");
          if (c.length>=8) {
            buf+="db "+c.join(", ")+"\n";
            c=[];
            
          }
          
        }
        if (c.length) {
          buf+="db "+c.join(", ")+"\n";
        }
        return buf;
      },
      fiber$toDB :function* _trc_GenAsm_f_toDB(_thread,bytes) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let buf = "";
        let c = [];
        
        while (bytes.length) {
          yield null;
          let b = bytes.shift();
          
          c.push("0"+b.toString(16)+"h");
          if (c.length>=8) {
            buf+="db "+c.join(", ")+"\n";
            c=[];
            
          }
          
        }
        if (c.length) {
          buf+="db "+c.join(", ")+"\n";
        }
        return buf;
        
      },
      showDiag :function _trc_GenAsm_showDiag() {
        "use strict";
        var _this=this;
        
        if (_this.problems.length) {
          let tx = new Tonyu.classes.kernel.HTMLUI({content: ["div",{style: 'background: #fee;'},["h2","Problem(s) found"],["ul"].concat(_this.problems.map((function anonymous_3275(p) {
            
            return ["li",["a",{href: "javascript:;",onclick: (function anonymous_3391() {
              
              _this.ide.jump(p.file,p.row,p.col);
            })},p.file.name(),":",p.row,":",p.col," - ",p.mesg]];
          }))),["button",{onclick: (function anonymous_3630() {
            
            tx.die();
          })},"Close"]],left: 10,top: 20,width: 300,height: 400});
          
          return _this;
          
        }
        let tx = new Tonyu.classes.kernel.HTMLUI({content: ["div",{style: 'background: #eee;'},["h2","Code copied!"],["ul",["li","Open ",["a",{target: "pen",href: _this.url},"this MSXpen page "],"."],["li","Paste the copied code"," to 'Asm' tab."]],["textarea",{rows: "10",cols: "30",name: "val"},"test\ndesu"],["button",{onclick: (function anonymous_4203() {
          
          tx.die();
        })},"Close"]],left: 10,top: 20,width: 300,height: 400});
        
        if (_this.getkey("n")) {
          tx.setValue("val",_this.buf+"");
          
        } else {
          let cg = new Tonyu.classes.user.Includer;
          
          tx.setValue("val",cg.gensrc(_this.buf+""));
          
        }
        tx.copyToClipboard("val");
      },
      fiber$showDiag :function* _trc_GenAsm_f_showDiag(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        if (_this.problems.length) {
          let tx = new Tonyu.classes.kernel.HTMLUI({content: ["div",{style: 'background: #fee;'},["h2","Problem(s) found"],["ul"].concat(_this.problems.map((function anonymous_3275(p) {
            
            return ["li",["a",{href: "javascript:;",onclick: (function anonymous_3391() {
              
              _this.ide.jump(p.file,p.row,p.col);
            })},p.file.name(),":",p.row,":",p.col," - ",p.mesg]];
          }))),["button",{onclick: (function anonymous_3630() {
            
            tx.die();
          })},"Close"]],left: 10,top: 20,width: 300,height: 400});
          
          return _this;
          
        }
        let tx = new Tonyu.classes.kernel.HTMLUI({content: ["div",{style: 'background: #eee;'},["h2","Code copied!"],["ul",["li","Open ",["a",{target: "pen",href: _this.url},"this MSXpen page "],"."],["li","Paste the copied code"," to 'Asm' tab."]],["textarea",{rows: "10",cols: "30",name: "val"},"test\ndesu"],["button",{onclick: (function anonymous_4203() {
          
          tx.die();
        })},"Close"]],left: 10,top: 20,width: 300,height: 400});
        
        if (_this.getkey("n")) {
          tx.setValue("val",_this.buf+"");
          
        } else {
          let cg = new Tonyu.classes.user.Includer;
          
          tx.setValue("val",cg.gensrc(_this.buf+""));
          
        }
        tx.copyToClipboard("val");
        
      },
      def :function _trc_GenAsm_def(n) {
        "use strict";
        var _this=this;
        
        if (! n) {
          _this.print(n);
          return _this;
          
        }
        _this.print(n.type,":",Object.keys(n));
        _this.unsup(n);
      },
      fiber$def :function* _trc_GenAsm_f_def(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        if (! n) {
          _this.print(n);
          return _this;
          
        }
        _this.print(n.type,":",Object.keys(n));
        (yield* _this.fiber$unsup(_thread, n));
        
      },
      unsup :function _trc_GenAsm_unsup(n,mesg) {
        "use strict";
        var _this=this;
        
        mesg=mesg||"Unsupported syntax";
        let file = _this.klass.src.tonyu;
        
        let rc = Tonyu.TError.calcRowCol(_this.klassSrc,n.pos);
        
        let p = {file: file,row: rc.row,col: rc.col,mesg: mesg};
        
        _this.problems.push(p);
        _this.print(p.file.name(),":",p.row,":",p.col," - ",p.mesg);
      },
      fiber$unsup :function* _trc_GenAsm_f_unsup(_thread,n,mesg) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        mesg=mesg||"Unsupported syntax";
        let file = _this.klass.src.tonyu;
        
        let rc = Tonyu.TError.calcRowCol(_this.klassSrc,n.pos);
        
        let p = {file: file,row: rc.row,col: rc.col,mesg: mesg};
        
        _this.problems.push(p);
        _this.print(p.file.name(),":",p.row,":",p.col," - ",p.mesg);
        
      },
      v_program :function _trc_GenAsm_v_program(n) {
        "use strict";
        var _this=this;
        
        let ext = n.ext;
        
        let incl = n.incl;
        
        let stmts = n.stmts;
        
        for (let [s] of Tonyu.iterator2(stmts,1)) {
          _this.visit(s);
          
        }
        _this.printf("ret%n");
      },
      fiber$v_program :function* _trc_GenAsm_f_v_program(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let ext = n.ext;
        
        let incl = n.incl;
        
        let stmts = n.stmts;
        
        for (let [s] of Tonyu.iterator2(stmts,1)) {
          (yield* _this.fiber$visit(_thread, s));
          
        }
        (yield* _this.fiber$printf(_thread, "ret%n"));
        
      },
      v_parenExpr :function _trc_GenAsm_v_parenExpr(n) {
        "use strict";
        var _this=this;
        
        _this.visit(n.expr);
      },
      fiber$v_parenExpr :function* _trc_GenAsm_f_v_parenExpr(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        (yield* _this.fiber$visit(_thread, n.expr));
        
      },
      extractSrc :function _trc_GenAsm_extractSrc(n) {
        "use strict";
        var _this=this;
        
        return _this.klassSrc.substring(n.pos,n.pos+n.len);
      },
      fiber$extractSrc :function* _trc_GenAsm_f_extractSrc(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return _this.klassSrc.substring(n.pos,n.pos+n.len);
        
      },
      v_exprstmt :function _trc_GenAsm_v_exprstmt(n) {
        "use strict";
        var _this=this;
        
        let expr = n.expr;
        
        _this.printf(";%s%n",_this.extractSrc(n).replace(/[\r\n]/g,""));
        _this.visit(expr);
      },
      fiber$v_exprstmt :function* _trc_GenAsm_f_v_exprstmt(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let expr = n.expr;
        
        (yield* _this.fiber$printf(_thread, ";%s%n", _this.extractSrc(n).replace(/[\r\n]/g,"")));
        (yield* _this.fiber$visit(_thread, expr));
        
      },
      v_infix :function _trc_GenAsm_v_infix(n) {
        "use strict";
        var _this=this;
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        switch (op.text) {
        case "=":
          _this.assign(n);
          break;
          
        case "+":
          
        case "-":
          
        case "*":
          
        case "/":
          
        case "%":
          _this.arith(n);
          break;
          
        case "==":
          
        case "!=":
          
        case "<=":
          
        case ">=":
          
        case "<":
          
        case ">":
          _this.cmp(n);
          break;
          
        case "+=":
          
        case "-=":
          
        case "*=":
          
        case "/=":
          
        case "%=":
          _this.arithEq(n);
          break;
          
        case "&&":
          _this.andand(left,right);
          break;
          
        case "||":
          _this.oror(left,right);
          break;
          
        default:
          _this.unsup(n,['Unsupported infix operator \'',op.text,'\''].join(''));
        }
      },
      fiber$v_infix :function* _trc_GenAsm_f_v_infix(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        switch (op.text) {
        case "=":
          (yield* _this.fiber$assign(_thread, n));
          break;
          
        case "+":
          
        case "-":
          
        case "*":
          
        case "/":
          
        case "%":
          (yield* _this.fiber$arith(_thread, n));
          break;
          
        case "==":
          
        case "!=":
          
        case "<=":
          
        case ">=":
          
        case "<":
          
        case ">":
          (yield* _this.fiber$cmp(_thread, n));
          break;
          
        case "+=":
          
        case "-=":
          
        case "*=":
          
        case "/=":
          
        case "%=":
          (yield* _this.fiber$arithEq(_thread, n));
          break;
          
        case "&&":
          (yield* _this.fiber$andand(_thread, left, right));
          break;
          
        case "||":
          (yield* _this.fiber$oror(_thread, left, right));
          break;
          
        default:
          (yield* _this.fiber$unsup(_thread, n, ['Unsupported infix operator \'',op.text,'\''].join('')));
        }
        
      },
      andand :function _trc_GenAsm_andand(left,right) {
        "use strict";
        var _this=this;
        
        let n = _this.genSym();
        
        _this.visit(left);
        _this.printf("andand %s%n",n);
        _this.visit(right);
        _this.printf("%s:%n",n);
      },
      fiber$andand :function* _trc_GenAsm_f_andand(_thread,left,right) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let n=yield* _this.fiber$genSym(_thread);
        
        (yield* _this.fiber$visit(_thread, left));
        (yield* _this.fiber$printf(_thread, "andand %s%n", n));
        (yield* _this.fiber$visit(_thread, right));
        (yield* _this.fiber$printf(_thread, "%s:%n", n));
        
      },
      oror :function _trc_GenAsm_oror(left,right) {
        "use strict";
        var _this=this;
        
        let n = _this.genSym();
        
        _this.visit(left);
        _this.printf("oror %s%n",n);
        _this.visit(right);
        _this.printf("%s:%n",n);
      },
      fiber$oror :function* _trc_GenAsm_f_oror(_thread,left,right) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let n=yield* _this.fiber$genSym(_thread);
        
        (yield* _this.fiber$visit(_thread, left));
        (yield* _this.fiber$printf(_thread, "oror %s%n", n));
        (yield* _this.fiber$visit(_thread, right));
        (yield* _this.fiber$printf(_thread, "%s:%n", n));
        
      },
      isMemberRef :function _trc_GenAsm_isMemberRef(m) {
        "use strict";
        var _this=this;
        
        if (m.type!=="postfix") {
          return null;
        }
        let tgme = m;
        
        if (tgme.op.type!=="member") {
          return null;
        }
        let mem = tgme.op;
        
        return [tgme.left,mem.name];
      },
      fiber$isMemberRef :function* _trc_GenAsm_f_isMemberRef(_thread,m) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        if (m.type!=="postfix") {
          return null;
        }
        let tgme = m;
        
        if (tgme.op.type!=="member") {
          return null;
        }
        let mem = tgme.op;
        
        return [tgme.left,mem.name];
        
      },
      v_postfix :function _trc_GenAsm_v_postfix(n) {
        "use strict";
        var _this=this;
        
        let left = n.left;
        
        let op = n.op;
        
        if (! _this.lval) {
          let tgme = _this.isMemberRef(n);
          
          if (tgme) {
            _this.visit(tgme[0]);
            _this.printf("getfldtg .%s%n",tgme[1]);
            return _this;
            
          } else {
            if (op.type==="call") {
              if (left.type==="varAccess") {
                return _this.myMeth(left,op);
                
              } else {
                let tgme = _this.isMemberRef(left);
                
                if (tgme) {
                  _this.tgMeth(tgme[0],tgme[1],op);
                  return _this;
                  
                }
                _this.unsup(n,['Not a member Ref '].join(''));
                
              }
              
            }
          }
          
        } else {
          let tgme = _this.isMemberRef(n);
          
          if (tgme) {
            _this.printf("push hl%n");
            _this.enter({lval: false},(function anonymous_7446() {
              
              _this.visit(tgme[0]);
            }));
            _this.printf("setfldtg .%s%n",tgme[1]);
            return _this;
            
          }
          
        }
        _this.unsup(n,['Unsupported postfix operator \'',op.text,'\' (',op.type,')'].join(''));
      },
      fiber$v_postfix :function* _trc_GenAsm_f_v_postfix(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let left = n.left;
        
        let op = n.op;
        
        if (! _this.lval) {
          let tgme=yield* _this.fiber$isMemberRef(_thread, n);
          
          if (tgme) {
            (yield* _this.fiber$visit(_thread, tgme[0]));
            (yield* _this.fiber$printf(_thread, "getfldtg .%s%n", tgme[1]));
            return _this;
            
          } else {
            if (op.type==="call") {
              if (left.type==="varAccess") {
                return yield* _this.fiber$myMeth(_thread, left, op);
                
                
              } else {
                let tgme=yield* _this.fiber$isMemberRef(_thread, left);
                
                if (tgme) {
                  (yield* _this.fiber$tgMeth(_thread, tgme[0], tgme[1], op));
                  return _this;
                  
                }
                (yield* _this.fiber$unsup(_thread, n, ['Not a member Ref '].join('')));
                
              }
              
            }
          }
          
        } else {
          let tgme=yield* _this.fiber$isMemberRef(_thread, n);
          
          if (tgme) {
            (yield* _this.fiber$printf(_thread, "push hl%n"));
            (yield* _this.fiber$enter(_thread, {lval: false}, (function anonymous_7446() {
              
              _this.visit(tgme[0]);
            })));
            (yield* _this.fiber$printf(_thread, "setfldtg .%s%n", tgme[1]));
            return _this;
            
          }
          
        }
        (yield* _this.fiber$unsup(_thread, n, ['Unsupported postfix operator \'',op.text,'\' (',op.type,')'].join('')));
        
      },
      tgMeth :function _trc_GenAsm_tgMeth(target,name,op) {
        "use strict";
        var _this=this;
        
        _this.printf("pushthis 0%n");
        let args = op.args;
        
        for (let [a] of Tonyu.iterator2(args,1)) {
          _this.visit(a);
          _this.printf("push hl%n");
          
        }
        _this.visit(target);
        _this.printf("invoketg.a .%s%n",name);
        _this.printf("popthis 0%n");
      },
      fiber$tgMeth :function* _trc_GenAsm_f_tgMeth(_thread,target,name,op) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        (yield* _this.fiber$printf(_thread, "pushthis 0%n"));
        let args = op.args;
        
        for (let [a] of Tonyu.iterator2(args,1)) {
          (yield* _this.fiber$visit(_thread, a));
          (yield* _this.fiber$printf(_thread, "push hl%n"));
          
        }
        (yield* _this.fiber$visit(_thread, target));
        (yield* _this.fiber$printf(_thread, "invoketg.a .%s%n", name));
        (yield* _this.fiber$printf(_thread, "popthis 0%n"));
        
      },
      myMeth :function _trc_GenAsm_myMeth(left,op) {
        "use strict";
        var _this=this;
        
        let args = op.args;
        
        let mname = left.name.text;
        
        if (mname==="getkey") {
          if (! args[0]) {
            _this.unsup(left,"'getkey' should give a string constant.");
            
          } else {
            let kn = args[0];
            
            if (kn.type==="literal") {
              let lit = kn;
              
              let s = lit.text;
              
              s=s.substring(1,s.length-1);
              if (! _this.keynames[s]) {
                _this.unsup(kn,['Undefined key name \'',s,'\'.'].join(''));
                
              }
              _this.printf("ld hl, %s%n",_this.keynames[s]);
              _this.printf("call getkey%n");
              
            } else {
              _this.unsup(kn,['\'',kn.type,'\' is not a string constant '].join(''));
              
            }
            
          }
          
        } else {
          if (mname==="rnd") {
            if (! args[0]) {
              _this.unsup(left,"'rnd' should give a number.");
              
            }
            _this.visit(args[0]);
            _this.printf("call rnd%n");
            
          } else {
            if (mname==="abs") {
              if (! args[0]) {
                _this.unsup(left,"'abs' should give a number.");
                
              }
              _this.visit(args[0]);
              _this.printf("call abs%n");
              
            } else {
              if (mname.match(/^map_set(At)?/)) {
                if (args.length!==3) {
                  _this.unsup(left,"'map_set(At) should give (x,y,p)");
                  
                }
                _this.visit(args[1]);
                _this.printf("push hl%n");
                _this.visit(args[0]);
                _this.printf("push hl%n");
                _this.visit(args[2]);
                _this.printf("ld a,l%n");
                _this.printf("pop hl%n");
                _this.printf("pop de%n");
                _this.printf("call map.set%s.a%n",mname.match(/At/)?"at":"");
                _this.printf("getthis 0%n");
                
              } else {
                if (mname.match(/^map_get(At)?/)) {
                  if (args.length!==2) {
                    _this.unsup(left,"'map_get(At) should give (x,y)");
                    
                  }
                  _this.visit(args[1]);
                  _this.printf("push hl%n");
                  _this.visit(args[0]);
                  _this.printf("pop de%n");
                  _this.printf("call map.get%s.a%n",mname.match(/At/)?"at":"");
                  _this.printf("ld h,0%n");
                  _this.printf("ld l,a%n");
                  _this.printf("getthis 0%n");
                  
                } else {
                  if (mname==="crashTo") {
                    let tg = args[0];
                    
                    if (! tg) {
                      _this.unsup(left,"'crashTo' should give a object or Class.");
                      
                    }
                    let klass = (tg.type==="varAccess"&&_this.isClassConst(tg));
                    
                    if (klass) {
                      let range = _this.mem.objRange(klass);
                      
                      _this.printf("crashToClass %s, %d, %d%n",klass.shortName,range[0],range[1]);
                      
                    } else {
                      _this.visit(tg);
                      _this.printf("call crashTo1%n");
                      _this.printf("flagtobool c%n");
                      
                    }
                    
                  } else {
                    if (mname==="div") {
                      if (args.length!==2) {
                        return _this.unsup("'div' should supply 2 numbers");
                        
                      }
                      _this.arith2(args[0],"/",args[1]);
                      
                    } else {
                      for (let [a] of Tonyu.iterator2(args,1)) {
                        _this.visit(a);
                        _this.printf("push hl%n");
                        
                      }
                      _this.printf("invoke .%s%n",mname);
                      
                    }
                  }
                }
              }
            }
          }
        }
      },
      fiber$myMeth :function* _trc_GenAsm_f_myMeth(_thread,left,op) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let args = op.args;
        
        let mname = left.name.text;
        
        if (mname==="getkey") {
          if (! args[0]) {
            (yield* _this.fiber$unsup(_thread, left, "'getkey' should give a string constant."));
            
          } else {
            let kn = args[0];
            
            if (kn.type==="literal") {
              let lit = kn;
              
              let s = lit.text;
              
              s=s.substring(1,s.length-1);
              if (! _this.keynames[s]) {
                (yield* _this.fiber$unsup(_thread, kn, ['Undefined key name \'',s,'\'.'].join('')));
                
              }
              (yield* _this.fiber$printf(_thread, "ld hl, %s%n", _this.keynames[s]));
              (yield* _this.fiber$printf(_thread, "call getkey%n"));
              
            } else {
              (yield* _this.fiber$unsup(_thread, kn, ['\'',kn.type,'\' is not a string constant '].join('')));
              
            }
            
          }
          
        } else {
          if (mname==="rnd") {
            if (! args[0]) {
              (yield* _this.fiber$unsup(_thread, left, "'rnd' should give a number."));
              
            }
            (yield* _this.fiber$visit(_thread, args[0]));
            (yield* _this.fiber$printf(_thread, "call rnd%n"));
            
          } else {
            if (mname==="abs") {
              if (! args[0]) {
                (yield* _this.fiber$unsup(_thread, left, "'abs' should give a number."));
                
              }
              (yield* _this.fiber$visit(_thread, args[0]));
              (yield* _this.fiber$printf(_thread, "call abs%n"));
              
            } else {
              if (mname.match(/^map_set(At)?/)) {
                if (args.length!==3) {
                  (yield* _this.fiber$unsup(_thread, left, "'map_set(At) should give (x,y,p)"));
                  
                }
                (yield* _this.fiber$visit(_thread, args[1]));
                (yield* _this.fiber$printf(_thread, "push hl%n"));
                (yield* _this.fiber$visit(_thread, args[0]));
                (yield* _this.fiber$printf(_thread, "push hl%n"));
                (yield* _this.fiber$visit(_thread, args[2]));
                (yield* _this.fiber$printf(_thread, "ld a,l%n"));
                (yield* _this.fiber$printf(_thread, "pop hl%n"));
                (yield* _this.fiber$printf(_thread, "pop de%n"));
                (yield* _this.fiber$printf(_thread, "call map.set%s.a%n", mname.match(/At/)?"at":""));
                (yield* _this.fiber$printf(_thread, "getthis 0%n"));
                
              } else {
                if (mname.match(/^map_get(At)?/)) {
                  if (args.length!==2) {
                    (yield* _this.fiber$unsup(_thread, left, "'map_get(At) should give (x,y)"));
                    
                  }
                  (yield* _this.fiber$visit(_thread, args[1]));
                  (yield* _this.fiber$printf(_thread, "push hl%n"));
                  (yield* _this.fiber$visit(_thread, args[0]));
                  (yield* _this.fiber$printf(_thread, "pop de%n"));
                  (yield* _this.fiber$printf(_thread, "call map.get%s.a%n", mname.match(/At/)?"at":""));
                  (yield* _this.fiber$printf(_thread, "ld h,0%n"));
                  (yield* _this.fiber$printf(_thread, "ld l,a%n"));
                  (yield* _this.fiber$printf(_thread, "getthis 0%n"));
                  
                } else {
                  if (mname==="crashTo") {
                    let tg = args[0];
                    
                    if (! tg) {
                      (yield* _this.fiber$unsup(_thread, left, "'crashTo' should give a object or Class."));
                      
                    }
                    let klass = (tg.type==="varAccess"&&_this.isClassConst(tg));
                    
                    if (klass) {
                      let range = _this.mem.objRange(klass);
                      
                      (yield* _this.fiber$printf(_thread, "crashToClass %s, %d, %d%n", klass.shortName, range[0], range[1]));
                      
                    } else {
                      (yield* _this.fiber$visit(_thread, tg));
                      (yield* _this.fiber$printf(_thread, "call crashTo1%n"));
                      (yield* _this.fiber$printf(_thread, "flagtobool c%n"));
                      
                    }
                    
                  } else {
                    if (mname==="div") {
                      if (args.length!==2) {
                        return yield* _this.fiber$unsup(_thread, "'div' should supply 2 numbers");
                        
                        
                      }
                      (yield* _this.fiber$arith2(_thread, args[0], "/", args[1]));
                      
                    } else {
                      for (let [a] of Tonyu.iterator2(args,1)) {
                        (yield* _this.fiber$visit(_thread, a));
                        (yield* _this.fiber$printf(_thread, "push hl%n"));
                        
                      }
                      (yield* _this.fiber$printf(_thread, "invoke .%s%n", mname));
                      
                    }
                  }
                }
              }
            }
          }
        }
        
      },
      cmp :function _trc_GenAsm_cmp(n) {
        "use strict";
        var _this=this;
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        let ops = {"==": "eq","!=": "ne","<=": "le",">=": "ge","<": "lt",">": "gt"};
        
        _this.visit(right);
        _this.printf("push hl%n");
        _this.visit(left);
        _this.printf("pop de%n");
        _this.printf("call hl%sde%n",ops[op.text]);
      },
      fiber$cmp :function* _trc_GenAsm_f_cmp(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        let ops = {"==": "eq","!=": "ne","<=": "le",">=": "ge","<": "lt",">": "gt"};
        
        (yield* _this.fiber$visit(_thread, right));
        (yield* _this.fiber$printf(_thread, "push hl%n"));
        (yield* _this.fiber$visit(_thread, left));
        (yield* _this.fiber$printf(_thread, "pop de%n"));
        (yield* _this.fiber$printf(_thread, "call hl%sde%n", ops[op.text]));
        
      },
      arith :function _trc_GenAsm_arith(n) {
        "use strict";
        var _this=this;
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        _this.arith2(left,op.text,right);
      },
      fiber$arith :function* _trc_GenAsm_f_arith(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        (yield* _this.fiber$arith2(_thread, left, op.text, right));
        
      },
      arith2 :function _trc_GenAsm_arith2(left,opr,right) {
        "use strict";
        var _this=this;
        
        switch (opr) {
        case "*":
          
        case "/":
          
        case "%":
          _this.printf("pushthis%n");
          _this.visit(left);
          _this.printf("push hl%n");
          _this.visit(right);
          _this.printf("pop de%n");
          _this.printf(_this.opmap[opr]);
          _this.printf("popthis%n");
          break;
          
        case "+":
          
        case "-":
          _this.visit(right);
          _this.printf("push hl%n");
          _this.visit(left);
          _this.printf("pop de%n");
          _this.printf(_this.opmap[opr]);
          break;
          
        default:
          _this.unsup(left,['Invalid op ',opr].join(''));
        }
      },
      fiber$arith2 :function* _trc_GenAsm_f_arith2(_thread,left,opr,right) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        switch (opr) {
        case "*":
          
        case "/":
          
        case "%":
          (yield* _this.fiber$printf(_thread, "pushthis%n"));
          (yield* _this.fiber$visit(_thread, left));
          (yield* _this.fiber$printf(_thread, "push hl%n"));
          (yield* _this.fiber$visit(_thread, right));
          (yield* _this.fiber$printf(_thread, "pop de%n"));
          (yield* _this.fiber$printf(_thread, _this.opmap[opr]));
          (yield* _this.fiber$printf(_thread, "popthis%n"));
          break;
          
        case "+":
          
        case "-":
          (yield* _this.fiber$visit(_thread, right));
          (yield* _this.fiber$printf(_thread, "push hl%n"));
          (yield* _this.fiber$visit(_thread, left));
          (yield* _this.fiber$printf(_thread, "pop de%n"));
          (yield* _this.fiber$printf(_thread, _this.opmap[opr]));
          break;
          
        default:
          (yield* _this.fiber$unsup(_thread, left, ['Invalid op ',opr].join('')));
        }
        
      },
      arithEq :function _trc_GenAsm_arithEq(n) {
        "use strict";
        var _this=this;
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        _this.arith2(left,op.text.substring(0,op.text.length-1),right);
        _this.enter({lval: true},(function anonymous_12503() {
          
          _this.visit(left);
        }));
      },
      fiber$arithEq :function* _trc_GenAsm_f_arithEq(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        (yield* _this.fiber$arith2(_thread, left, op.text.substring(0,op.text.length-1), right));
        (yield* _this.fiber$enter(_thread, {lval: true}, (function anonymous_12503() {
          
          _this.visit(left);
        })));
        
      },
      assign :function _trc_GenAsm_assign(n) {
        "use strict";
        var _this=this;
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        _this.visit(right);
        _this.enter({lval: true},(function anonymous_13358() {
          
          _this.visit(left);
        }));
      },
      fiber$assign :function* _trc_GenAsm_f_assign(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let left = n.left;
        
        let op = n.op;
        
        let right = n.right;
        
        (yield* _this.fiber$visit(_thread, right));
        (yield* _this.fiber$enter(_thread, {lval: true}, (function anonymous_13358() {
          
          _this.visit(left);
        })));
        
      },
      v_reservedConst :function _trc_GenAsm_v_reservedConst(n) {
        "use strict";
        var _this=this;
        
        if (n.text==="true"||n.text==="false") {
          _this.printf("ld hl,%s%n",n.text);
          
        } else {
          if (n.text==="this") {
            _this.printf("ld h,a%n");
            _this.printf("ld l,0%n");
            return _this;
            
          } else {
            _this.unsup(n,['Unsupported reserved word \'',n.text,'\'.'].join(''));
            
          }
        }
      },
      fiber$v_reservedConst :function* _trc_GenAsm_f_v_reservedConst(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        if (n.text==="true"||n.text==="false") {
          (yield* _this.fiber$printf(_thread, "ld hl,%s%n", n.text));
          
        } else {
          if (n.text==="this") {
            (yield* _this.fiber$printf(_thread, "ld h,a%n"));
            (yield* _this.fiber$printf(_thread, "ld l,0%n"));
            return _this;
            
          } else {
            (yield* _this.fiber$unsup(_thread, n, ['Unsupported reserved word \'',n.text,'\'.'].join('')));
            
          }
        }
        
      },
      v_number :function _trc_GenAsm_v_number(n) {
        "use strict";
        var _this=this;
        
        _this.printf("ld hl,%d%n",n.text-0);
      },
      fiber$v_number :function* _trc_GenAsm_f_v_number(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        (yield* _this.fiber$printf(_thread, "ld hl,%d%n", n.text-0));
        
      },
      globalLabel :function _trc_GenAsm_globalLabel(n) {
        "use strict";
        var _this=this;
        
        return ['gbl_',n.replace(/\$/g,"")].join('');
      },
      fiber$globalLabel :function* _trc_GenAsm_f_globalLabel(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return ['gbl_',n.replace(/\$/g,"")].join('');
        
      },
      v_varAccess :function _trc_GenAsm_v_varAccess(n) {
        "use strict";
        var _this=this;
        
        let name = n.name;
        
        let a = _this.annotation(n);
        
        switch (a.scopeInfo.type) {
        case "field":
          if (_this.lval) {
            _this.printf("setfld .%s%n",name);
            
          } else {
            _this.printf("getfld .%s%n",name);
            
          }
          break;
          
        case "class":
          if (_this.lval) {
            _this.unsup(n,['Cannot assign to class \'',name,'\'.'].join(''));
            
          } else {
            _this.printf("ld hl,%s%n",name);
            
          }
          break;
          
        case "global":
          _this.globals[name.text]=1;
          if (_this.lval) {
            _this.printf("ld (%s),hl%n",_this.globalLabel(name.text));
            
          } else {
            _this.printf("ld hl,(%s)%n",_this.globalLabel(name.text));
            
          }
          break;
          
        default:
          _this.unsup(n,[name.text,': Unsupported variable type \'',a.scopeInfo.type,'\''].join(''));
        }
      },
      fiber$v_varAccess :function* _trc_GenAsm_f_v_varAccess(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let name = n.name;
        
        let a=yield* _this.fiber$annotation(_thread, n);
        
        switch (a.scopeInfo.type) {
        case "field":
          if (_this.lval) {
            (yield* _this.fiber$printf(_thread, "setfld .%s%n", name));
            
          } else {
            (yield* _this.fiber$printf(_thread, "getfld .%s%n", name));
            
          }
          break;
          
        case "class":
          if (_this.lval) {
            (yield* _this.fiber$unsup(_thread, n, ['Cannot assign to class \'',name,'\'.'].join('')));
            
          } else {
            (yield* _this.fiber$printf(_thread, "ld hl,%s%n", name));
            
          }
          break;
          
        case "global":
          _this.globals[name.text]=1;
          if (_this.lval) {
            (yield* _this.fiber$printf(_thread, "ld (%s),hl%n", _this.globalLabel(name.text)));
            
          } else {
            (yield* _this.fiber$printf(_thread, "ld hl,(%s)%n", _this.globalLabel(name.text)));
            
          }
          break;
          
        default:
          (yield* _this.fiber$unsup(_thread, n, [name.text,': Unsupported variable type \'',a.scopeInfo.type,'\''].join('')));
        }
        
      },
      genSym :function _trc_GenAsm_genSym() {
        "use strict";
        var _this=this;
        
        return "lb"+(_this.symSeq++);
      },
      fiber$genSym :function* _trc_GenAsm_f_genSym(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return "lb"+(_this.symSeq++);
        
      },
      v_compound :function _trc_GenAsm_v_compound(n) {
        "use strict";
        var _this=this;
        
        for (let [s] of Tonyu.iterator2(n.stmts,1)) {
          _this.visit(s);
          
        }
      },
      fiber$v_compound :function* _trc_GenAsm_f_v_compound(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        for (let [s] of Tonyu.iterator2(n.stmts,1)) {
          (yield* _this.fiber$visit(_thread, s));
          
        }
        
      },
      v_while :function _trc_GenAsm_v_while(n) {
        "use strict";
        var _this=this;
        
        let cond = n.cond;
        
        let loop = n.loop;
        
        let sh = _this.genSym();
        
        let se = _this.genSym();
        
        _this.printf("%s:%n",sh);
        _this.visit(cond);
        _this.printf("jpf %s%n",se);
        _this.visit(loop);
        _this.printf("jp %s%n",sh);
        _this.printf("%s:%n",se);
      },
      fiber$v_while :function* _trc_GenAsm_f_v_while(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let cond = n.cond;
        
        let loop = n.loop;
        
        let sh=yield* _this.fiber$genSym(_thread);
        
        let se=yield* _this.fiber$genSym(_thread);
        
        (yield* _this.fiber$printf(_thread, "%s:%n", sh));
        (yield* _this.fiber$visit(_thread, cond));
        (yield* _this.fiber$printf(_thread, "jpf %s%n", se));
        (yield* _this.fiber$visit(_thread, loop));
        (yield* _this.fiber$printf(_thread, "jp %s%n", sh));
        (yield* _this.fiber$printf(_thread, "%s:%n", se));
        
      },
      v_for :function _trc_GenAsm_v_for(n) {
        "use strict";
        var _this=this;
        
        let inFor = n.inFor;
        
        let loop = n.loop;
        
        if (inFor.type==="normalFor") {
          let h = inFor;
          
          let init = h.init;
          
          let cond = h.cond;
          
          let next = h.next;
          
          let sh = _this.genSym();
          
          let se = _this.genSym();
          
          _this.visit(init);
          _this.printf("%s:%n",sh);
          _this.visit(cond);
          _this.printf("jpf %s%n",se);
          _this.visit(loop);
          _this.visit(next);
          _this.printf("jp %s%n",sh);
          _this.printf("%s:%n",se);
          
        } else {
          let h = inFor;
          
          let isVar = h.isVar;
          
          let vars = h.vars;
          
          let inof = h.inof;
          
          let set = h.set;
          
          let klass = _this.isAll(set);
          
          if (! klass) {
            _this.unsup(set,"only 'for (e of all(Class))' is allowed ");
            return _this;
            
          }
          let range = _this.mem.objRange(klass);
          
          let nx = _this.genSym();
          
          _this.printf("foreach %s, %d, %d, %s%{",klass.shortName,range[0],range[1],nx);
          let va = _this.annotation(vars[0]);
          
          if (va.scopeInfo.type==="field") {
            _this.printf("setfld .%s%n",vars[0].text);
            
          } else {
            _this.unsup(vars,"only field supports");
            
          }
          _this.visit(loop);
          _this.printf("continue%n");
          _this.printf("%}%s:%n",nx);
          _this.printf("getthis 0%n");
          
        }
      },
      fiber$v_for :function* _trc_GenAsm_f_v_for(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let inFor = n.inFor;
        
        let loop = n.loop;
        
        if (inFor.type==="normalFor") {
          let h = inFor;
          
          let init = h.init;
          
          let cond = h.cond;
          
          let next = h.next;
          
          let sh=yield* _this.fiber$genSym(_thread);
          
          let se=yield* _this.fiber$genSym(_thread);
          
          (yield* _this.fiber$visit(_thread, init));
          (yield* _this.fiber$printf(_thread, "%s:%n", sh));
          (yield* _this.fiber$visit(_thread, cond));
          (yield* _this.fiber$printf(_thread, "jpf %s%n", se));
          (yield* _this.fiber$visit(_thread, loop));
          (yield* _this.fiber$visit(_thread, next));
          (yield* _this.fiber$printf(_thread, "jp %s%n", sh));
          (yield* _this.fiber$printf(_thread, "%s:%n", se));
          
        } else {
          let h = inFor;
          
          let isVar = h.isVar;
          
          let vars = h.vars;
          
          let inof = h.inof;
          
          let set = h.set;
          
          let klass=yield* _this.fiber$isAll(_thread, set);
          
          if (! klass) {
            (yield* _this.fiber$unsup(_thread, set, "only 'for (e of all(Class))' is allowed "));
            return _this;
            
          }
          let range = _this.mem.objRange(klass);
          
          let nx=yield* _this.fiber$genSym(_thread);
          
          (yield* _this.fiber$printf(_thread, "foreach %s, %d, %d, %s%{", klass.shortName, range[0], range[1], nx));
          let va=yield* _this.fiber$annotation(_thread, vars[0]);
          
          if (va.scopeInfo.type==="field") {
            (yield* _this.fiber$printf(_thread, "setfld .%s%n", vars[0].text));
            
          } else {
            (yield* _this.fiber$unsup(_thread, vars, "only field supports"));
            
          }
          (yield* _this.fiber$visit(_thread, loop));
          (yield* _this.fiber$printf(_thread, "continue%n"));
          (yield* _this.fiber$printf(_thread, "%}%s:%n", nx));
          (yield* _this.fiber$printf(_thread, "getthis 0%n"));
          
        }
        
      },
      isAll :function _trc_GenAsm_isAll(set) {
        "use strict";
        var _this=this;
        
        if (set.type!=="postfix") {
          return false;
        }
        let p = set;
        
        if (p.op.type!=="call") {
          return false;
        }
        let c = p.op;
        
        if (p.left.type!=="varAccess") {
          return false;
        }
        let a = p.left;
        
        if (a.name.text!=="all") {
          return false;
        }
        if (c.args.length!==1) {
          return false;
        }
        let k = c.args[0];
        
        return k.type==="varAccess"&&_this.isClassConst(k);
      },
      fiber$isAll :function* _trc_GenAsm_f_isAll(_thread,set) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        if (set.type!=="postfix") {
          return false;
        }
        let p = set;
        
        if (p.op.type!=="call") {
          return false;
        }
        let c = p.op;
        
        if (p.left.type!=="varAccess") {
          return false;
        }
        let a = p.left;
        
        if (a.name.text!=="all") {
          return false;
        }
        if (c.args.length!==1) {
          return false;
        }
        let k = c.args[0];
        
        return k.type==="varAccess"&&_this.isClassConst(k);
        
      },
      v_if :function _trc_GenAsm_v_if(n) {
        "use strict";
        var _this=this;
        
        let cond = n.cond;
        
        let then = n.then;
        
        let _else = n._else;
        
        let send = _this.genSym();
        
        let sels = _this.genSym();
        
        _this.visit(cond);
        _this.printf("jpf %s%n",sels);
        _this.visit(then);
        _this.printf("jp %s%n",send);
        _this.printf("%s:%n",sels);
        if (_else) {
          _this.visit(_else);
        }
        _this.printf("%s:%n",send);
      },
      fiber$v_if :function* _trc_GenAsm_f_v_if(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let cond = n.cond;
        
        let then = n.then;
        
        let _else = n._else;
        
        let send=yield* _this.fiber$genSym(_thread);
        
        let sels=yield* _this.fiber$genSym(_thread);
        
        (yield* _this.fiber$visit(_thread, cond));
        (yield* _this.fiber$printf(_thread, "jpf %s%n", sels));
        (yield* _this.fiber$visit(_thread, then));
        (yield* _this.fiber$printf(_thread, "jp %s%n", send));
        (yield* _this.fiber$printf(_thread, "%s:%n", sels));
        if (_else) {
          (yield* _this.fiber$visit(_thread, _else));
        }
        (yield* _this.fiber$printf(_thread, "%s:%n", send));
        
      },
      v_varDecl :function _trc_GenAsm_v_varDecl(n) {
        "use strict";
        var _this=this;
        
        let name = n.name;
        
        let typeDecl = n.typeDecl;
        
        let value = n.value;
        
        let a = _this.annotation(n);
        
        if (! a.varInMain) {
          _this.unsup(n,"Local Variables are not yet supported.");
          return _this;
          
        }
        if (! value) {
          return _this;
        }
        _this.visit(value);
        _this.printf("setfld .%s%n",name);
      },
      fiber$v_varDecl :function* _trc_GenAsm_f_v_varDecl(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let name = n.name;
        
        let typeDecl = n.typeDecl;
        
        let value = n.value;
        
        let a=yield* _this.fiber$annotation(_thread, n);
        
        if (! a.varInMain) {
          (yield* _this.fiber$unsup(_thread, n, "Local Variables are not yet supported."));
          return _this;
          
        }
        if (! value) {
          return _this;
        }
        (yield* _this.fiber$visit(_thread, value));
        (yield* _this.fiber$printf(_thread, "setfld .%s%n", name));
        
      },
      v_varsDecl :function _trc_GenAsm_v_varsDecl(n) {
        "use strict";
        var _this=this;
        
        let decls = n.decls;
        
        if (n.declPrefix.text!=="var") {
          _this.unsup(n,"let or const are not supported.");
          
        }
        for (let [d] of Tonyu.iterator2(decls,1)) {
          _this.visit(d);
          
        }
      },
      fiber$v_varsDecl :function* _trc_GenAsm_f_v_varsDecl(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let decls = n.decls;
        
        if (n.declPrefix.text!=="var") {
          (yield* _this.fiber$unsup(_thread, n, "let or const are not supported."));
          
        }
        for (let [d] of Tonyu.iterator2(decls,1)) {
          (yield* _this.fiber$visit(_thread, d));
          
        }
        
      },
      rangeToAd :function _trc_GenAsm_rangeToAd(r) {
        "use strict";
        var _this=this;
        
        return ['th.start+',r,'*th.size'].join('');
      },
      fiber$rangeToAd :function* _trc_GenAsm_f_rangeToAd(_thread,r) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return ['th.start+',r,'*th.size'].join('');
        
      },
      isClassConst :function _trc_GenAsm_isClassConst(klass) {
        "use strict";
        var _this=this;
        
        let a = _this.annotation(klass);
        
        if (a.scopeInfo.type!=="class") {
          return null;
          
        }
        return a.scopeInfo.info;
      },
      fiber$isClassConst :function* _trc_GenAsm_f_isClassConst(_thread,klass) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let a=yield* _this.fiber$annotation(_thread, klass);
        
        if (a.scopeInfo.type!=="class") {
          return null;
          
        }
        return a.scopeInfo.info;
        
      },
      v_newExpr :function _trc_GenAsm_v_newExpr(n) {
        "use strict";
        var _this=this;
        
        let klass = n.klass;
        
        let params = n.params;
        
        if (params.args.length==1&&params.args[0].type=="objlit") {
          let objlit = params.args[0];
          
          let elems = objlit.elems;
          
          for (let [elem] of Tonyu.iterator2(elems,1)) {
            if (elem.value) {
              _this.visit(elem.value);
              
            } else {
              _this.printf("getfld .%s%n",elem.key);
              
            }
            _this.printf("new.arg .%s%n",elem.key);
            
          }
          let resKlass = _this.isClassConst(klass);
          
          if (! resKlass) {
            _this.unsup(klass,['Only class name is allowed'].join(''));
            
          }
          let range = _this.mem.objRange(resKlass);
          
          _this.printf("new %s,%d,%s,%s%n",klass.name,elems.length,range[0],range[1]);
          return _this;
          
        }
        _this.unsup(n,['\'new ',klass.name,'\' can be followed by only {key:value...}'].join(''));
      },
      fiber$v_newExpr :function* _trc_GenAsm_f_v_newExpr(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        let klass = n.klass;
        
        let params = n.params;
        
        if (params.args.length==1&&params.args[0].type=="objlit") {
          let objlit = params.args[0];
          
          let elems = objlit.elems;
          
          for (let [elem] of Tonyu.iterator2(elems,1)) {
            if (elem.value) {
              (yield* _this.fiber$visit(_thread, elem.value));
              
            } else {
              (yield* _this.fiber$printf(_thread, "getfld .%s%n", elem.key));
              
            }
            (yield* _this.fiber$printf(_thread, "new.arg .%s%n", elem.key));
            
          }
          let resKlass=yield* _this.fiber$isClassConst(_thread, klass);
          
          if (! resKlass) {
            (yield* _this.fiber$unsup(_thread, klass, ['Only class name is allowed'].join('')));
            
          }
          let range = _this.mem.objRange(resKlass);
          
          (yield* _this.fiber$printf(_thread, "new %s,%d,%s,%s%n", klass.name, elems.length, range[0], range[1]));
          return _this;
          
        }
        (yield* _this.fiber$unsup(_thread, n, ['\'new ',klass.name,'\' can be followed by only {key:value...}'].join('')));
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"annotation":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"toDB":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"showDiag":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"def":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"unsup":{"nowait":false,"isMain":false,"vtype":{"params":["user.TNode","String"],"returnValue":null}},"v_program":{"nowait":false,"isMain":false,"vtype":{"params":["user.Program"],"returnValue":null}},"v_parenExpr":{"nowait":false,"isMain":false,"vtype":{"params":["user.ParenExpr"],"returnValue":null}},"extractSrc":{"nowait":false,"isMain":false,"vtype":{"params":["user.TNode"],"returnValue":null}},"v_exprstmt":{"nowait":false,"isMain":false,"vtype":{"params":["user.Exprstmt"],"returnValue":null}},"v_infix":{"nowait":false,"isMain":false,"vtype":{"params":["user.Infix"],"returnValue":null}},"andand":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression","user.Expression"],"returnValue":null}},"oror":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression","user.Expression"],"returnValue":null}},"isMemberRef":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression"],"returnValue":null}},"v_postfix":{"nowait":false,"isMain":false,"vtype":{"params":["user.Postfix"],"returnValue":null}},"tgMeth":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression","user.Token","user.Call"],"returnValue":null}},"myMeth":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarAccess","user.Call"],"returnValue":null}},"cmp":{"nowait":false,"isMain":false,"vtype":{"params":["user.Infix"],"returnValue":null}},"arith":{"nowait":false,"isMain":false,"vtype":{"params":["user.Infix"],"returnValue":null}},"arith2":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression","String","user.Expression"],"returnValue":null}},"arithEq":{"nowait":false,"isMain":false,"vtype":{"params":["user.Infix"],"returnValue":null}},"assign":{"nowait":false,"isMain":false,"vtype":{"params":["user.Infix"],"returnValue":null}},"v_reservedConst":{"nowait":false,"isMain":false,"vtype":{"params":["user.Token"],"returnValue":null}},"v_number":{"nowait":false,"isMain":false,"vtype":{"params":["user.Token"],"returnValue":null}},"globalLabel":{"nowait":false,"isMain":false,"vtype":{"params":["String"],"returnValue":null}},"v_varAccess":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarAccess"],"returnValue":null}},"genSym":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"v_compound":{"nowait":false,"isMain":false,"vtype":{"params":["user.Compound"],"returnValue":null}},"v_while":{"nowait":false,"isMain":false,"vtype":{"params":["user.While"],"returnValue":null}},"v_for":{"nowait":false,"isMain":false,"vtype":{"params":["user.For"],"returnValue":null}},"isAll":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression"],"returnValue":null}},"v_if":{"nowait":false,"isMain":false,"vtype":{"params":["user.If"],"returnValue":null}},"v_varDecl":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarDecl"],"returnValue":null}},"v_varsDecl":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarsDecl"],"returnValue":null}},"rangeToAd":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"isClassConst":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarAccess"],"returnValue":null}},"v_newExpr":{"nowait":false,"isMain":false,"vtype":{"params":["user.NewExpr"],"returnValue":null}}},"fields":{"IDEPrj":{},"anodes":{},"mem":{"vtype":"user.MemberScan"},"lval":{},"symSeq":{"vtype":"Number"},"ide":{},"klass":{},"klassSrc":{},"problems":{},"builtins":{},"globals":{},"keynames":{},"opmap":{},"anode":{},"outp":{"vtype":"user.OutPat"},"outbg":{"vtype":"user.OutBG"},"url":{"vtype":"String"}}}
});
Tonyu.klass.define({
  fullName: 'user.ArgList',
  shortName: 'ArgList',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_ArgList_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_ArgList_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"args":{"vtype":{"element":"user.Expression"}}}}
});
Tonyu.klass.define({
  fullName: 'user.ArrayElem',
  shortName: 'ArrayElem',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_ArrayElem_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_ArrayElem_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"subscript":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.Break',
  shortName: 'Break',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_Break_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Break_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"brk":{"vtype":"user.Token"}}}
});
Tonyu.klass.define({
  fullName: 'user.Call',
  shortName: 'Call',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Call_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Call_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"args":{"vtype":{"element":"user.Expression"}}}}
});
Tonyu.klass.define({
  fullName: 'user.Case',
  shortName: 'Case',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Case_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_Case_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"value":{"vtype":"user.Expression"},"stmts":{"vtype":{"element":"user.Stmt"}}}}
});
Tonyu.klass.define({
  fullName: 'user.Catch',
  shortName: 'Catch',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Catch_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_Catch_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"name":{"vtype":"user.Token"},"stmt":{"vtype":"user.Stmt"}}}
});
Tonyu.klass.define({
  fullName: 'user.Compound',
  shortName: 'Compound',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_Compound_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Compound_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"stmts":{"vtype":{"element":"user.Stmt"}}}}
});
Tonyu.klass.define({
  fullName: 'user.Continue',
  shortName: 'Continue',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_Continue_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Continue_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"cont":{"vtype":"user.Token"}}}
});
Tonyu.klass.define({
  fullName: 'user.Default',
  shortName: 'Default',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Default_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Default_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"stmts":{"vtype":{"element":"user.Stmt"}}}}
});
Tonyu.klass.define({
  fullName: 'user.Do',
  shortName: 'Do',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_Do_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_Do_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"loop":{"vtype":"user.Stmt"},"cond":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.Empty',
  shortName: 'Empty',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_Empty_main() {
        "use strict";
        var _this=this;
        
      },
      fiber$main :function* _trc_Empty_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{}}
});
Tonyu.klass.define({
  fullName: 'user.Expr',
  shortName: 'Expr',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Expr_main() {
        "use strict";
        var _this=this;
        
      },
      fiber$main :function* _trc_Expr_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{}}
});
Tonyu.klass.define({
  fullName: 'user.Expression',
  shortName: 'Expression',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Expression_main() {
        "use strict";
        var _this=this;
        
      },
      fiber$main :function* _trc_Expression_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{}}
});
Tonyu.klass.define({
  fullName: 'user.Exprstmt',
  shortName: 'Exprstmt',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_Exprstmt_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Exprstmt_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"expr":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.Extends',
  shortName: 'Extends',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Extends_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Extends_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"superclassName":{"vtype":"user.Token"}}}
});
Tonyu.klass.define({
  fullName: 'user.Finally',
  shortName: 'Finally',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Finally_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Finally_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"stmt":{"vtype":"user.Stmt"}}}
});
Tonyu.klass.define({
  fullName: 'user.For',
  shortName: 'For',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_For_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_For_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"inFor":{"vtype":"user.ForHead"},"loop":{"vtype":"user.Stmt"}}}
});
Tonyu.klass.define({
  fullName: 'user.Forin',
  shortName: 'Forin',
  namespace: 'user',
  superclass: Tonyu.classes.user.ForHead,
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Forin_main() {
        "use strict";
        var _this=this;
        
        
        
        
        
      },
      fiber$main :function* _trc_Forin_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"isVar":{"vtype":"user.Token"},"vars":{"vtype":{"element":"user.Token"}},"inof":{"vtype":"user.Token"},"set":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.FuncDecl',
  shortName: 'FuncDecl',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_FuncDecl_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_FuncDecl_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"head":{"vtype":"user.FuncDeclHead"},"body":{"vtype":"user.Compound"}}}
});
Tonyu.klass.define({
  fullName: 'user.FuncDeclHead',
  shortName: 'FuncDeclHead',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_FuncDeclHead_main() {
        "use strict";
        var _this=this;
        
        
        
        
        
        
        
      },
      fiber$main :function* _trc_FuncDeclHead_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"_nowait":{"vtype":"user.Token"},"ftype":{"vtype":"user.Token"},"name":{"vtype":"user.Token"},"setter":{"vtype":"user.SetterDecl"},"params":{"vtype":"user.ParamDecls"},"rtype":{"vtype":"user.TypeDecl"}}}
});
Tonyu.klass.define({
  fullName: 'user.FuncExprArg',
  shortName: 'FuncExprArg',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.ObjOrFuncArg],
  methods: function (__superClass) {
    return {
      main :function _trc_FuncExprArg_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_FuncExprArg_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"obj":{"vtype":"user.FuncExpr"}}}
});
Tonyu.klass.define({
  fullName: 'user.FuncExprHead',
  shortName: 'FuncExprHead',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_FuncExprHead_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_FuncExprHead_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"name":{"vtype":"user.Token"},"params":{"vtype":"user.ParamDecls"}}}
});
Tonyu.klass.define({
  fullName: 'user.If',
  shortName: 'If',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_If_main() {
        "use strict";
        var _this=this;
        
        
        
        
      },
      fiber$main :function* _trc_If_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"cond":{"vtype":"user.Expression"},"then":{"vtype":"user.Stmt"},"_else":{"vtype":"user.Stmt"}}}
});
Tonyu.klass.define({
  fullName: 'user.IfWait',
  shortName: 'IfWait',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_IfWait_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_IfWait_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"then":{"vtype":"user.Stmt"},"_else":{"vtype":"user.Stmt"}}}
});
Tonyu.klass.define({
  fullName: 'user.Includes',
  shortName: 'Includes',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Includes_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Includes_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"includeClassNames":{"vtype":{"element":"user.Token"}}}}
});
Tonyu.klass.define({
  fullName: 'user.Infix',
  shortName: 'Infix',
  namespace: 'user',
  includes: [Tonyu.classes.user.Expression],
  methods: function (__superClass) {
    return {
      main :function _trc_Infix_main() {
        "use strict";
        var _this=this;
        
        
        
        
      },
      fiber$main :function* _trc_Infix_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"left":{"vtype":"user.Expression"},"op":{"vtype":"user.Token"},"right":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.JsonElem',
  shortName: 'JsonElem',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_JsonElem_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_JsonElem_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"key":{"vtype":"user.Token"},"value":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.Member',
  shortName: 'Member',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Member_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Member_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"name":{"vtype":"user.Token"}}}
});
Tonyu.klass.define({
  fullName: 'user.NativeDecl',
  shortName: 'NativeDecl',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_NativeDecl_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_NativeDecl_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"name":{"vtype":"user.Token"}}}
});
Tonyu.klass.define({
  fullName: 'user.NormalFor',
  shortName: 'NormalFor',
  namespace: 'user',
  superclass: Tonyu.classes.user.ForHead,
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_NormalFor_main() {
        "use strict";
        var _this=this;
        
        
        
        
      },
      fiber$main :function* _trc_NormalFor_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"init":{"vtype":"user.Stmt"},"cond":{"vtype":"user.Expression"},"next":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.ObjlitArg',
  shortName: 'ObjlitArg',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.ObjOrFuncArg],
  methods: function (__superClass) {
    return {
      main :function _trc_ObjlitArg_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_ObjlitArg_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"obj":{"vtype":"user.Objlit"}}}
});
Tonyu.klass.define({
  fullName: 'user.ParamDecl',
  shortName: 'ParamDecl',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_ParamDecl_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_ParamDecl_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"name":{"vtype":"user.Token"},"typeDecl":{"vtype":"user.TypeDecl"}}}
});
Tonyu.klass.define({
  fullName: 'user.ParamDecls',
  shortName: 'ParamDecls',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_ParamDecls_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_ParamDecls_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"params":{"vtype":{"element":"user.ParamDecl"}}}}
});
Tonyu.klass.define({
  fullName: 'user.Postfix',
  shortName: 'Postfix',
  namespace: 'user',
  includes: [Tonyu.classes.user.Expression],
  methods: function (__superClass) {
    return {
      main :function _trc_Postfix_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_Postfix_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"left":{"vtype":"user.Expression"},"op":{"vtype":"user.TNode"}}}
});
Tonyu.klass.define({
  fullName: 'user.Prefix',
  shortName: 'Prefix',
  namespace: 'user',
  includes: [Tonyu.classes.user.Expression],
  methods: function (__superClass) {
    return {
      main :function _trc_Prefix_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_Prefix_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"op":{"vtype":"user.Token"},"right":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.Program',
  shortName: 'Program',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Program_main() {
        "use strict";
        var _this=this;
        
        
        
        
      },
      fiber$main :function* _trc_Program_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"ext":{"vtype":"user.Extends"},"incl":{"vtype":"user.Includes"},"stmts":{"vtype":{"element":"user.Stmt"}}}}
});
Tonyu.klass.define({
  fullName: 'user.Return',
  shortName: 'Return',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_Return_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Return_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"value":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.Scall',
  shortName: 'Scall',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_Scall_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Scall_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"args":{"vtype":{"element":"user.Expression"}}}}
});
Tonyu.klass.define({
  fullName: 'user.SetterDecl',
  shortName: 'SetterDecl',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode],
  methods: function (__superClass) {
    return {
      main :function _trc_SetterDecl_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_SetterDecl_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"value":{"vtype":"user.ParamDecl"}}}
});
Tonyu.klass.define({
  fullName: 'user.Switch',
  shortName: 'Switch',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_Switch_main() {
        "use strict";
        var _this=this;
        
        
        
        
      },
      fiber$main :function* _trc_Switch_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"value":{"vtype":"user.Expression"},"cases":{"vtype":{"element":"user.Case"}},"defs":{"vtype":"user.Default"}}}
});
Tonyu.klass.define({
  fullName: 'user.Throw',
  shortName: 'Throw',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Stmt],
  methods: function (__superClass) {
    return {
      main :function _trc_Throw_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Throw_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"ex":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.Trifix',
  shortName: 'Trifix',
  namespace: 'user',
  includes: [Tonyu.classes.user.Expression],
  methods: function (__superClass) {
    return {
      main :function _trc_Trifix_main() {
        "use strict";
        var _this=this;
        
        
        
        
        
        
      },
      fiber$main :function* _trc_Trifix_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"left":{"vtype":"user.Expression"},"op1":{"vtype":"user.Token"},"mid":{"vtype":"user.Expression"},"op2":{"vtype":"user.Token"},"right":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.RActor',
  shortName: 'RActor',
  namespace: 'user',
  superclass: Tonyu.classes.user.RSprite,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_RActor_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_RActor_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      die :function _trc_RActor_die() {
        "use strict";
        var _this=this;
        
        __superClass.prototype.die.apply( _this, []);
      },
      crashTo :function _trc_RActor_crashTo(e) {
        "use strict";
        var _this=this;
        
        return __superClass.prototype.crashTo.apply( _this, [e]);
      },
      update :function _trc_RActor_update() {
        "use strict";
        var _this=this;
        
        __superClass.prototype.update.apply( _this, []);
      },
      fiber$update :function* _trc_RActor_f_update(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        (yield* __superClass.prototype.fiber$update.apply( _this, [_thread]));
        
      },
      updateEx :function _trc_RActor_updateEx(n) {
        "use strict";
        var _this=this;
        
        __superClass.prototype.updateEx.apply( _this, [n]);
      },
      fiber$updateEx :function* _trc_RActor_f_updateEx(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        (yield* __superClass.prototype.fiber$updateEx.apply( _this, [_thread, n]));
        
      },
      onUpdate :function _trc_RActor_onUpdate() {
        "use strict";
        var _this=this;
        
      },
      screenOut :function _trc_RActor_screenOut() {
        "use strict";
        var _this=this;
        
        return _this.x<0||_this.x>Tonyu.globals.$MScreen.width<<_this.spr_scale||_this.y<0||_this.y>Tonyu.globals.$MScreen.height<<_this.spr_scale;
      },
      fiber$screenOut :function* _trc_RActor_f_screenOut(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        return _this.x<0||_this.x>Tonyu.globals.$MScreen.width<<_this.spr_scale||_this.y<0||_this.y>Tonyu.globals.$MScreen.height<<_this.spr_scale;
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"die":{"nowait":true,"isMain":false,"vtype":{"params":[],"returnValue":null}},"crashTo":{"nowait":true,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"update":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"updateEx":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"onUpdate":{"nowait":true,"isMain":false,"vtype":{"params":[],"returnValue":null}},"screenOut":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}}},"fields":{"x":{},"y":{},"p":{},"c":{}}}
});
Tonyu.klass.define({
  fullName: 'user.EBullet',
  shortName: 'EBullet',
  namespace: 'user',
  superclass: Tonyu.classes.user.RActor,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_EBullet_main() {
        "use strict";
        var _this=this;
        
        
        _this.p=7;
        _this.c=15;
        while (true) {
          Tonyu.checkLoop();
          _this.x+=_this.vx;
          _this.y+=_this.vy;
          if (_this.screenOut()) {
            _this.die();
          }
          if (_this.map_getAt(_this.x,_this.y)==1) {
            _this.map_setAt(_this.x,_this.y,32);
            _this.die();
            
          }
          _this.update();
          
        }
      },
      fiber$main :function* _trc_EBullet_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        _this.p=7;
        _this.c=15;
        while (true) {
          yield null;
          _this.x+=_this.vx;
          _this.y+=_this.vy;
          if (_this.screenOut()) {
            _this.die();
          }
          if (_this.map_getAt(_this.x,_this.y)==1) {
            (yield* _this.fiber$map_setAt(_thread, _this.x, _this.y, 32));
            _this.die();
            
          }
          (yield* _this.fiber$update(_thread));
          
        }
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"vx":{},"vy":{}}}
});
Tonyu.klass.define({
  fullName: 'user.Enemy',
  shortName: 'Enemy',
  namespace: 'user',
  superclass: Tonyu.classes.user.RActor,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_Enemy_main() {
        "use strict";
        var _this=this;
        
        
        while (_this.y<200) {
          Tonyu.checkLoop();
          _this.y+=2;
          _this.update();
          
        }
        if (_this.x>Tonyu.globals.$player.x) {
          _this.vx=0-2;
        } else {
          _this.vx=2;
        }
        _this.fire();
        while (true) {
          Tonyu.checkLoop();
          if (_this.screenOut()) {
            _this.die();
          }
          _this.y+=2;
          _this.x+=_this.vx;
          _this.map_setAt(_this.x,_this.y,32);
          _this.update();
          
        }
        
      },
      fiber$main :function* _trc_Enemy_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        while (_this.y<200) {
          yield null;
          _this.y+=2;
          (yield* _this.fiber$update(_thread));
          
        }
        if (_this.x>Tonyu.globals.$player.x) {
          _this.vx=0-2;
        } else {
          _this.vx=2;
        }
        (yield* _this.fiber$fire(_thread));
        while (true) {
          yield null;
          if (_this.screenOut()) {
            _this.die();
          }
          _this.y+=2;
          _this.x+=_this.vx;
          (yield* _this.fiber$map_setAt(_thread, _this.x, _this.y, 32));
          (yield* _this.fiber$update(_thread));
          
        }
        
        
      },
      fire :function _trc_Enemy_fire() {
        "use strict";
        var _this=this;
        
        _this.bvx=Tonyu.globals.$player.x-_this.x;
        _this.bvy=Tonyu.globals.$player.y-_this.y;
        _this.bdist=_this.abs(_this.bvx)+_this.abs(_this.bvy);
        _this.bdist/=8;
        if (_this.bdist>0) {
          _this.bvx/=_this.bdist;
          _this.bvy/=_this.bdist;
          new Tonyu.classes.user.EBullet({x: _this.x,y: _this.y,vx: _this.bvx,vy: _this.bvy});
          
        }
      },
      fiber$fire :function* _trc_Enemy_f_fire(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.bvx=Tonyu.globals.$player.x-_this.x;
        _this.bvy=Tonyu.globals.$player.y-_this.y;
        _this.bdist=_this.abs(_this.bvx)+_this.abs(_this.bvy);
        _this.bdist/=8;
        if (_this.bdist>0) {
          _this.bvx/=_this.bdist;
          _this.bvy/=_this.bdist;
          new Tonyu.classes.user.EBullet({x: _this.x,y: _this.y,vx: _this.bvx,vy: _this.bvy});
          
        }
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"fire":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}}},"fields":{"vx":{},"bvx":{},"bvy":{},"bdist":{}}}
});
Tonyu.klass.define({
  fullName: 'user.Main',
  shortName: 'Main',
  namespace: 'user',
  superclass: Tonyu.classes.user.RActor,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_Main_main() {
        "use strict";
        var _this=this;
        
        Tonyu.globals.$player=new Tonyu.classes.user.Player({x: 256,y: 300,p: Tonyu.globals.$pat_spr+4,c: 15});
        _this.i = 0;
        
        Tonyu.globals.$score=0;
        while (true) {
          Tonyu.checkLoop();
          new Tonyu.classes.user.Enemy({x: _this.rnd(512),y: 0,p: 5,c: 7});
          _this.updateEx(30);
          _this.showScore();
          
        }
      },
      fiber$main :function* _trc_Main_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        Tonyu.globals.$player=new Tonyu.classes.user.Player({x: 256,y: 300,p: Tonyu.globals.$pat_spr+4,c: 15});
        _this.i = 0;
        
        Tonyu.globals.$score=0;
        while (true) {
          yield null;
          new Tonyu.classes.user.Enemy({x: _this.rnd(512),y: 0,p: 5,c: 7});
          (yield* _this.fiber$updateEx(_thread, 30));
          (yield* _this.fiber$showScore(_thread));
          
        }
        
      },
      showScore :function _trc_Main_showScore() {
        "use strict";
        var _this=this;
        
        _this.tmps=Tonyu.globals.$score;
        for (_this.i=5; _this.i>1 ; _this.i-=1) {
          Tonyu.checkLoop();
          {
            _this.map_set(_this.i,1,_this.tmps%10+48);
            _this.tmps=_this.div(_this.tmps,10);
          }
        }
      },
      fiber$showScore :function* _trc_Main_f_showScore(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.tmps=Tonyu.globals.$score;
        for (_this.i=5; _this.i>1 ; _this.i-=1) {
          yield null;
          {
            (yield* _this.fiber$map_set(_thread, _this.i, 1, _this.tmps%10+48));
            _this.tmps=(yield* _this.fiber$div(_thread, _this.tmps, 10));
          }
        }
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"showScore":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}}},"fields":{"i":{"vtype":"Number"},"tmps":{}}}
});
Tonyu.klass.define({
  fullName: 'user.PBullet',
  shortName: 'PBullet',
  namespace: 'user',
  superclass: Tonyu.classes.user.RActor,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_PBullet_main() {
        "use strict";
        var _this=this;
        
        _this.p=6;
        _this.c=8;
        while (true) {
          Tonyu.checkLoop();
          if (_this.screenOut()) {
            _this.die();
          }
          _this.y-=6;
          _this.e = _this.crashTo(Tonyu.classes.user.Enemy);
          
          if (_this.e) {
            _this.e.die();
            _this.die();
            Tonyu.globals.$score+=10;
            
          }
          if (_this.map_getAt(_this.x,_this.y-16)==1) {
            _this.map_setAt(_this.x,_this.y-16,32);
            _this.die();
            
          }
          _this.update();
          
        }
      },
      fiber$main :function* _trc_PBullet_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.p=6;
        _this.c=8;
        while (true) {
          yield null;
          if (_this.screenOut()) {
            _this.die();
          }
          _this.y-=6;
          _this.e = _this.crashTo(Tonyu.classes.user.Enemy);
          
          if (_this.e) {
            _this.e.die();
            _this.die();
            Tonyu.globals.$score+=10;
            
          }
          if (_this.map_getAt(_this.x,_this.y-16)==1) {
            (yield* _this.fiber$map_setAt(_thread, _this.x, _this.y-16, 32));
            _this.die();
            
          }
          (yield* _this.fiber$update(_thread));
          
        }
        
      },
      onUpdate :function _trc_PBullet_onUpdate() {
        "use strict";
        var _this=this;
        
        _this.c+=1;
        if (_this.c>14) {
          _this.c=0;
        }
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"onUpdate":{"nowait":true,"isMain":false,"vtype":{"params":[],"returnValue":null}}},"fields":{"e":{}}}
});
Tonyu.klass.define({
  fullName: 'user.Player',
  shortName: 'Player',
  namespace: 'user',
  superclass: Tonyu.classes.user.RActor,
  includes: [],
  methods: function (__superClass) {
    return {
      main :function _trc_Player_main() {
        "use strict";
        var _this=this;
        
        while (true) {
          Tonyu.checkLoop();
          _this.map_setAt(_this.x,_this.y,Tonyu.globals.$pat_font+1);
          if (_this.getkey("left")) {
            _this.x-=3;
          }
          if (_this.getkey("right")) {
            _this.x+=3;
          }
          if (_this.getkey("up")) {
            _this.y-=3;
          }
          if (_this.getkey("down")) {
            _this.y+=3;
          }
          if (_this.crashTo(Tonyu.classes.user.Enemy)) {
            _this.die();
            
          }
          if (_this.crashTo(Tonyu.classes.user.EBullet)) {
            _this.die();
            
          }
          if (_this.getkey("space")==1) {
            new Tonyu.classes.user.PBullet({x: _this.x,y: _this.y});
            
            _this.r = _this.rnd(10)+1;
            
            for ([_this.e] of Tonyu.iterator2(_this.all(Tonyu.classes.user.Enemy),1)) {
              _this.e.c=_this.r;
              
            }
            
          }
          _this.update();
          
        }
      },
      fiber$main :function* _trc_Player_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        while (true) {
          yield null;
          (yield* _this.fiber$map_setAt(_thread, _this.x, _this.y, Tonyu.globals.$pat_font+1));
          if (_this.getkey("left")) {
            _this.x-=3;
          }
          if (_this.getkey("right")) {
            _this.x+=3;
          }
          if (_this.getkey("up")) {
            _this.y-=3;
          }
          if (_this.getkey("down")) {
            _this.y+=3;
          }
          if (_this.crashTo(Tonyu.classes.user.Enemy)) {
            _this.die();
            
          }
          if (_this.crashTo(Tonyu.classes.user.EBullet)) {
            _this.die();
            
          }
          if (_this.getkey("space")==1) {
            new Tonyu.classes.user.PBullet({x: _this.x,y: _this.y});
            
            _this.r = _this.rnd(10)+1;
            
            for ([_this.e] of Tonyu.iterator2(_this.all(Tonyu.classes.user.Enemy),1)) {
              _this.e.c=_this.r;
              
            }
            
          }
          (yield* _this.fiber$update(_thread));
          
        }
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"e":{},"r":{}}}
});
Tonyu.klass.define({
  fullName: 'user.Elem',
  shortName: 'Elem',
  namespace: 'user',
  includes: [Tonyu.classes.user.Expression],
  methods: function (__superClass) {
    return {
      main :function _trc_Elem_main() {
        "use strict";
        var _this=this;
        
      },
      fiber$main :function* _trc_Elem_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{}}
});
Tonyu.klass.define({
  fullName: 'user.FuncExpr',
  shortName: 'FuncExpr',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Elem],
  methods: function (__superClass) {
    return {
      main :function _trc_FuncExpr_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_FuncExpr_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"head":{"vtype":"user.FuncExprHead"},"body":{"vtype":"user.Compound"}}}
});
Tonyu.klass.define({
  fullName: 'user.NewExpr',
  shortName: 'NewExpr',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Elem],
  methods: function (__superClass) {
    return {
      main :function _trc_NewExpr_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_NewExpr_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"klass":{"vtype":"user.VarAccess"},"params":{"vtype":"user.Call"}}}
});
Tonyu.klass.define({
  fullName: 'user.Objlit',
  shortName: 'Objlit',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Elem],
  methods: function (__superClass) {
    return {
      main :function _trc_Objlit_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Objlit_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"elems":{"vtype":{"element":"user.JsonElem"}}}}
});
Tonyu.klass.define({
  fullName: 'user.ParenExpr',
  shortName: 'ParenExpr',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Elem],
  methods: function (__superClass) {
    return {
      main :function _trc_ParenExpr_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_ParenExpr_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"expr":{"vtype":"user.Expression"}}}
});
Tonyu.klass.define({
  fullName: 'user.SuperExpr',
  shortName: 'SuperExpr',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Elem],
  methods: function (__superClass) {
    return {
      main :function _trc_SuperExpr_main() {
        "use strict";
        var _this=this;
        
        
        
      },
      fiber$main :function* _trc_SuperExpr_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"name":{"vtype":"user.Token"},"params":{"vtype":"user.Scall"}}}
});
Tonyu.klass.define({
  fullName: 'user.Token',
  shortName: 'Token',
  namespace: 'user',
  includes: [Tonyu.classes.user.Elem],
  methods: function (__superClass) {
    return {
      main :function _trc_Token_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Token_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"text":{"vtype":"String"}}}
});
Tonyu.klass.define({
  fullName: 'user.VarAccess',
  shortName: 'VarAccess',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Elem],
  methods: function (__superClass) {
    return {
      main :function _trc_VarAccess_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_VarAccess_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"name":{"vtype":"user.Token"}}}
});
Tonyu.klass.define({
  fullName: 'user.Arylit',
  shortName: 'Arylit',
  namespace: 'user',
  includes: [Tonyu.classes.user.TNode,Tonyu.classes.user.Elem],
  methods: function (__superClass) {
    return {
      main :function _trc_Arylit_main() {
        "use strict";
        var _this=this;
        
        
      },
      fiber$main :function* _trc_Arylit_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"elems":{"vtype":{"element":"user.Expression"}}}}
});

//# sourceMappingURL=concat.js.map