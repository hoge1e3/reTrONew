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
        
        _this.data = {'main': ['org 09000h\n\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude sp\ninclude vdp\ninclude th\n\n\n;===your code \n\nmain:\nld a,5\nlp:\ndec b\nshow a\niff nc,nx\njr lp\nnx:\n\nret\nend main'].join(''),'math': ['include ctrl\n\n;16bit shifts\nmacro slhl\n sla l\n rl h\nendm\nmacro srahl\n sra h\n rr l\nendm\nmacro srlhl\n srl h\n rr l\nendm\nmacro slde\n sla e\n rl d\nendm\nmacro srade\n sra d\n rr e\nendm\nmacro srlde\n srl d\n rr e\nendm\nmacro slbc\n sla c\n rl b\nendm\nmacro srabc\n sra b\n rr c\nendm\nmacro srlbc\n srl b\n rr c\nendm\n\n\n; for xrnd\nmacro sldehl,n\n  local loop\n  ld b,n\n loop:\n  sla d\n  rl e\n  rl h\n  rl l\n  djnz loop\nendm\nmacro srdehl,n\n local loop\n ld b,n\n loop:\n  srl l\n  rr h\n  rr e\n  rr d\n djnz loop\nendm\n \nmacro xorrm,re,me\n  ld A,(me)\n  xor re\n  ld (me),a\nendm\n\nmacro subhl,rp\n and a\n sbc hl,rp\nendm\n\nmacro cpde.a\n rst dcompr\nendm\n\n\nmarker.b xrnd.a\nxrnd.a:\nproc \n local rhl,rde,rdhlde\n ; s[0] ^= s[0] << 13\n call rdhlde\n sldehl 13\n call wrtxor\n ; s[0] ^= s[0] >> 17\n call rdhlde\n srdehl 17\n call wrtxor\n ; s[0] ^= s[0] << 5;\n call rdhlde\n sldehl 5\n call wrtxor\n ret\n \n rdhlde:\n  ld hl,1234\n rhl:\n  ld de,5678\n rde:\n  ret\n \n wrtxor:\n  xorrm h,rhl-1\n  xorrm l,rhl-2\n  xorrm d,rde-1\n  xorrm e,rde-2\n  ret\nendp\nmarker.e xrnd.a\n\n\nmarker.b rnd\nrnd:\n push af\n call rnd.a\n pop af\n ret\nmarker.e rnd\nmarker.b rnd.a\nrnd.a:\n ld de,07fffh\n call IDIV.a\n push hl\n call xrnd.a\n res 7,h\n ex de,hl\n pop hl\n inc hl\n call IDIV.a\n ret\nmarker.e rnd.a\n\nmarker.b abs\nabs:\n bit 7,h\n ret z\nneghl:\n ld de,0 \n ex de,hl \n subhl de\n ret\nmarker.e abs\n\n '].join(''),'bool': ['include math\ninclude ctrl\ntrue equ -1\nfalse equ 0\n\nmacro rethl,val,flg\n local tru\n if not nul flg\n  iff flg ,tru\n endif\n ld hl,val\n ret\n tru:\nendm\n\nhleqde:\n subhl de\n rethl true,z\n rethl false\n\nhlnede:\n subhl de\n rethl true,nz\n rethl false\n \nhlgtde:\n subhl de\n rethl false,z\n bit 7,h\n rethl true,z\n rethl false\n\nhlltde:\n subhl de\n bit 7,h\n rethl true,nz\n rethl false\n \nhlgede:\n subhl de\n rethl true,z\n bit 7,h\n rethl true,z\n rethl false\n\nhllede:\n subhl de\n rethl true,z\n bit 7,h\n rethl true,nz\n rethl false\n \nproc\nziffalse:\n local resa\n ld (resa-1),a\n call ziffalse.a\n ld A,0\n resa:\n ret\nziffalse.a:\n ld a,0\n cp h\n ret nz\n cp l\n ret\nendp\n\nmacro jpf,to\n call ziffalse\n jp z,to\nendm\n\nmacro andand,fls\n jpf fls\nendm\nmacro oror,tru\n call ziffalse\n jp nz,tru\nendm\n\nmacro flagtobool,fl\n local yes,skp\n jr fl, yes\n ld hl,false\n jr skp\n yes:\n ld hl,true\n skp: \nendm'].join(''),'mem': ['include const\n;\nrdslt:\n ex de,hl\n rept 5\n srl d;page*2\n endm\n CALL RSLREG\n ld e,d\n rdslt1:\n  RRCA\n  dec e\n  jp nz,rdslt1\n AND    00000011B\n LD C,A;000000Pr\n LD B,0\n LD HL,EXPTBL\n ADD HL,BC\n LD C,A;000000Pr\n LD A,(HL)\n AND 80H;expand flag\n OR C\n LD C,A;e00000Pr\n rept 4;const\n INC HL\n endm\n LD A,(HL);exp reg\n ld e,d\n rdslt2:\n  srl a\n  dec e\n  jp nz,rdslt2\n;000000Ex\n sla a\n sla a\n ;    0000Ex00\n and  00001100b\n OR C;e000ExPr\n ret\nmemini:\n CALL RSLREG\n rept 4\n  RRCA\n endm\n AND    00000011B\n LD C,A;000000Pr\n LD B,0\n LD HL,EXPTBL\n ADD HL,BC\n LD C,A;000000Pr\n LD A,(HL)\n AND 80H;expand flag\n OR C\n LD C,A;e00000Pr\n rept 4;const\n INC HL\n endm\n LD A,(HL);exp reg\n rept 4; page*2\n srl a\n endm;000000Ex\n sla a\n sla a\n ;    0000Ex00\n and  00001100b\n OR C;e000ExPr\n LD Hl,04000H\n jp ENASLT\n\nmacro peekw ,regv,regm\n  local w\n  ld (w-2),regm\n  ld regv,(0)\n  w:\nendm\n\nmacro pokew ,regm,regv\n  local w\n  ld (w-2),regm\n  ld (0),regv\n  w:\nendm\nmacro movw,dst,src,rp\n if nul rp\n  push hl\n  movw dst,src,hl\n  pop hl\n else\n  ld rp,src\n  ld dst,rp\n endif \nendm\n\nmacro popa\n  ex (sp),hl\n  ld a,h\n  pop HL\nendm\n\nmacro pushall\n push af\n push bc\n push de\n push hl\nendm\nmacro popall\n pop hl\n pop de\n pop bc\n pop af\nendm\n \n\nmacro pushi, n,rp\n local rrr\n if nul rp\n  ld (rrr-2),hl\n  ld hl,n\n  push hl\n  ld hl,0\n  rrr:\n else\n  ld rp,n\n  push rp\n endif\nendm\nmacro const,n,reg\n ld (n-2),reg\nendm\nmacro ldconst,reg,n\n ld reg,0\n n:\nendm\nmacro peekconst,reg,n\n ld reg,(0)\n n:\nendm\n'].join(''),'const': ['\n;wrt equ 0a2h\ndcompr equ 0020H\nsp.ini equ 0dc00h\nstksize equ 512\n\nth.size equ 256\nth.count equ 20\nth.start equ th.end-th.size*th.count\nth.end equ sp.ini-stksize\n\nth.bottom equ 0\n\nspr.scale equ 1\nspr.xmax equ 256<<spr.scale\nspr.ymax equ 192<<spr.scale\n\nENASLT EQU 0024H\nRSLREG EQU 0138H\nEXPTBL EQU 0FCC1H\nSETWRT equ 0053H\nLDIRVM equ 005CH\nWRTVDP equ 0047H\nRG1SAV equ 0F3E0H\nRDVDP  equ 013EH\nSNSMAT.a equ 0141h\n\nCHGMOD equ 005FH\n\nIMULT.a equ 3193H;HL ← DE*HL\nIDIV.a equ 31E6H;HL ← DE/HL\nIMOD.a equ 323AH;HL ← DE mod HL (DE ← DE/HL) \n\nWRTPSG  equ 0093H\n\nCSRY equ 0F3DCH\nCSRX equ 0F3DDH\n\nnull equ 0\n\nmacro marker.b, n\n last.marker: defl $\nendm\nmacro marker.e, n\n len.##n: defl $-last.marker\nendm\n'].join(''),'ctrl': ['include const\nfreeze:\nhalt\njr freeze\n\nmacro for ,lbend\n ; uses a\n ; c: breaked\n proc\n  local s,lb\n  lb:\n  call dcompr; uses a\n  jp nc,lbend\n  push HL\n  push de\n  push bc\n  call s\n  pop bc\n  pop de\n  pop HL\n  jp c,lbend\n  add HL,bc\n  jr lb\n  s:\n endp\nendm\n\nmacro repti ,n,lbend\n proc\n  local s,lb, lbend2\n  push bc\n  ld b,n\n  lb:\n  push bc\n  call s\n  pop bc\n  jr c,lbend2\n  djnz lb\n  lbend2:\n  pop bc\n  jp lbend \n  s:\n endp\nendm\n\n\nmacro reptb ,lbend\n  local s,lb\n inc b\n djnz lb\n jp lbend\n lb:\n  push bc\n  call s\n  pop bc\n  jp c,lbend\n djnz lb\n jp lbend \n s:\nendm\n\n\n\nmacro callsva,pp\n local sva\n ld (sva-1),a\n call pp\n ld a,0\n sva:\nendm\nbcis0:\n callsva bcis0.a\n ret\nbcis0.a:\n ld a,b\n and a\n ret nz\n ld a,c\n and a\n ret\n\nmacro reptbc ,lbend\n local s,lb\n call bcis0\n jp z,lbend \n lb:\n  push bc\n  call s\n  pop bc\n  jp c,lbend\n  dec bc\n  call bcis0\n jr nz, lb\n jp lbend \n s:\nendm\n\n\niff.NZ equ 0\niff.Z  equ 1\niff.NC equ 2\niff.C  equ 3\n\nmacro iff,cnd,to\n local iff.\n if iff.##cnd eq iff.NZ\n  jr z,to\n endif\n if iff.##cnd eq iff.Z\n  jr nz,to\n endif\n if iff.##cnd eq iff.NC\n  jr c,to\n endif\n if iff.##cnd eq iff.C\n  jr nc,to\n endif\n ;jr cnd, skip\n ;jr to\n ;skip:\nendm\n\nmacro break,cnd\n if NUL cnd\n  scf\n  ret\n else\n  proc \n   local jj\n   iff cnd ,jj\n   break\n  jj:\n  endp\n endif\nendm\nmacro continue,cnd\n if NUL cnd \n  or a\n  ret\n else\n  proc \n   local jj\n   iff cnd,jj\n   continue\n  jj:\n  endp\n endif\nendm\n\n\nmacro djnzr,reg, j\n dec reg\n jr NZ,j\nendm\n\nmacro callhl\n local LCD\n ld (LCD-2),HL\n call LCD\n LCD:\nendm\n\nmacro stride,lim,to\n if (low $)<lim\n  exitm\n endif\n ds 256+to-(low $),0cdh\nendm'].join(''),'th': ['include ctrl\ninclude sp\ninclude vdp\ninclude mem\ninclude math\ninclude debug\n\nth.ofs.stp equ 256-4\nth.ofs.sp equ th.ofs.stp+1\nth.ofs.spini equ th.ofs.stp\nfld.top equ th.ofs.spini-2\nth.st.blank equ 0c9h\nth.st.active equ 31h\n\n;macro th.for,lb\n; ld HL,th.start\n; ld de,th.end\n; ld bc,th.size\n; for lb\n;endm\nmacro th.for.a, nx, st, en\n if nul st\n  th.for.a nx, 0, th.count\n  exitm\n endif\n local do, loop\n ld a,st+(high th.start)\n loop:\n  cp en+(high th.start)\n  jp nc, nx\n  push af\n  ld h,a\n  ld l,0\n  call do\n  popa\n  ld h,a\n  ld l,0\n  jp c, nx\n  inc a\n jr loop\n do:\nendm\n\nmacro th.new.range,st,en\n ld bc,st\n ld(th.new.start),bc\n ld bc,en\n ld(th.new.end),bc\nendm\n\ndefsub th.isblank.a\n ; h= thread\n ; z if true\n ld l, th.ofs.stp\n ld a,(hl)\n cp th.st.blank\nendsub th.isblank.a\n\ndefsub th.new\n; nc for alloc fail\nproc \n local lbend\n db 21h\n th.new.start:\n dw th.start\n db 11h\n th.new.end:\n dw th.end\n ld bc,th.size\n for lbend\n  ; TODO th.ofs.stp\n  call th.isblank.a\n  break z\n  continue\n lbend:\n ret nc\n ; TODO th.ofs.stp\n ld L,th.ofs.stp\n ld (HL),31h\n inc HL\n ld (HL),th.ofs.spini\n ld a,h\n inc HL\n ld (hl),a\n inc HL\n ld (HL),0c9h\n ld l,th.bottom\n scf\n ret\nendp\nendsub th.new\n\ndefsub th.init\nproc\n local lbend\n th.for.a lbend\n  ; TODO th.ofs.stp\n  ld L, th.ofs.stp\n  ld (HL),th.st.blank\n  continue\n lbend:\n ; disable timer\n ld HL,0fd9fh\n ld (hl),0c9h\n call susint\n ret\nendp\nendsub th.init\n\ndefsub th.stepall\n th.for.a thnx\n  ;todo th.ofs.stp\n  ld (th.cur),hl\n  call th.isblank.a\n  continue z\n  call th.step\n  continue\n thnx:\nendsub th.stepall\n\ndefsub th.step\n sp2mem adrssp+1\n ld HL,(th.cur)\n ld l,th.ofs.stp\n ;call susint\n jp (hl)\nendsub th.step\n\ndefsub th.yield\n ld hl,(th.cur)\n ld l,th.ofs.sp\n sp2mem\n adrssp:\n ld sp,0\n jp doint\nendsub th.yield\n\ndefsub th.term\n ld hl,(th.cur)\n ; TODO th.ofs.stp\n ld L,th.ofs.stp\n ld (hl),th.st.blank\n jr adrssp\nendsub th.term\n\nmacro th.with.do, to\n local pr\n th.with pr\n jr to\n pr:\nendm\n\nmacro th.with.setdst, reg\n ld (th.jpdest-2),reg\nendm\nmacro th.with,pr\n movw (th.jpdest-2), pr\n call th.with.s\nendm\nmacro th.with.ret\n jp th.ewith\nendm\n\ndefsub th.with.s\n sp2mem th.wrssp-2\n ld l, th.ofs.sp\n ld (th.updsp-2),hl\n mem2sp\n jp 0\n th.jpdest:\nth.ewith:\n ld (0),sp\n th.updsp:\n ld sp,0\n th.wrssp:\nendsub th.with.s\n \n\n \n \ndefsub th.push\n ;push bc to thread hl\n th.with tpsbc\n ret\n tpsbc:\n  push bc\n  th.with.ret 0\nendsub th.push\n\n\ndefwork th.cur\n dw 0\nendwork th.cur\n\ndefsub th.loop\n ; hook before stepall\n db 0cdh\n h.thent:\n dw th.nop\n ; save prev timecnt\n ld a,(timecnt)\n push af\n ; Do stepall\n call th.stepall\n ; hook after stepall\n db 0cdh\n h.thlop:\n dw th.nop\n ; wait until timecnt changes\n pop af\n bwat:\n  ld hl,timecnt\n  cp (hl)\n  jr nz,bbwat\n  push af\n  call doint\n  pop af\n  jr bwat\n bbwat:\n ; repeat\n jr th.loop\nendsub th.loop\n\nth.nop:\n ret\n\n\nmacro th.pushi, val\n ld bc,val\n call th.push\nendm\n\n'].join(''),'sub': [].join(''),'debug': ['include math\n;debug\nmacro show,reg\n ld (hexval+1),reg\n call showhex\nendm\nmacro showm ,ad\n push hl\n ld HL,(ad)\n show HL\n pop HL\nendm\nmacro showlb,lb\n push hl\n ld hl,lb\n ld (hexval+1),hl\n call showhex\n pop hl\nendm\nshowhex:\nproc\n local loop\n push af\n push bc\n push HL\n hexval:\n ld hl,0\n ld b,4\n loop:\n  xor a\n  rept 4\n   slhl\n   rla\n  endm\n  call showhex1\n djnz loop\n ld a,32\n call wrt\n pop HL\n pop bc\n pop af\n ret\nendp\nshowhex1:\nproc\n local els\n cp 10\n jp nc, els\n add a,48\n jp wrt\n els:\n add a,65-10\n jp wrt\nendp\nabort:\n call wrt\n db 018h,0feh\nret\n\nmacro trace,v\n if not nul v\n  push af\n  ld a,v\n  ld (trad),a\n  pop af\n endif\n call trace.s\nendm\ntrace.s:\n push af\n push hl\n ld a,(trad)\n ld hl,1ae0h\n call wrt\n call 4dh\n inc a\n ld (trad),a\n ld a,32\n call wrt \n pop hl\n pop af\n ret\ntrad:\n db 65\n\nshowz:\n push af\n jr z,showz.s\n ld a,"N"\n call wrt\n showz.s:\n ld a,"Z"\n call wrt\n ld a,32\n call wrt\n pop af\n ret\n \n\nshowc:\n push af\n jr c,showc.s\n ld a,"N"\n call wrt\n showc.s:\n ld a,"C"\n call wrt\n ld a,32\n call wrt\n pop af\n ret\n \n\n\n\n\n\nmacro unreach, mesg\n trace mesg\n dw 0x18,0xfe\nendm\nmacro head, lb\n unreach lb\n marker.b lb\n lb:\nendm\n\nmacro defsub, n\n head n\nendm\nmacro endsub, n\n ret\n marker.e n\nendm\nmacro defwork, n\n head n\nendm\nmacro endwork, n\n marker.e n\nendm\n\ndefsub wrt\nproc\n local sk\n push hl\n push af\n ld hl,1800h\n cursor:\n call 4dh\n inc hl\n ld a,h\n cp 1bh\n jr c,sk\n  ld h,18h\n sk:\n ld (cursor-2),hl\n pop af\n pop hl\n ret\nendp\nendsub wrt\n'].join(''),'sp': ['include mem\ninclude debug\nmacro sp.get\n ld HL,0\n ADD hl, sp\nendm\nmacro sp.set\n ld sp,hl\nendm\nmacro mem2sp,ad\n local rs\n if nul ad\n  ld (rs-2),hl\n  ld sp,(0)\n  rs:\n else\n  ld sp,(ad)\n endif\nendm\nmacro sp2mem,ad\n local spad\n if nul ad\n  ld (spad-2),hl\n  ld (0),sp\n  spad:\n else\n  ld (ad),sp\n endif\nendm\n\nmacro showsp\n ld (sptmp),sp\n showm sptmp\nendm\nsptmp:\ndw 0\nmacro showstk\n showsp\n ld (sva),a\n ld a,":"\n call wrt\n ld a,(sva)\n ex (sp),hl\n show hl\n ex (sp),hl\nendm\nsva: db 0'].join(''),'oop': ['include mem\ninclude th\ninclude assert\n\n;a2 a1  oldpc oldix lcl1 lcl2\nargidx equ 2\nmacro getarg ,n\n ld l,(ix+argidx+n*2)\n ld h,(ix+argidx+n*2+1)\nendm\n\nmacro setlcl ,n\n ld (IX-(n*2-1)),h\n ld (ix-n*2),l\nendm\n\nmacro getlcl ,n\n ld h,(IX-(n*2-1))\n ld l,(ix-n*2)\nendm\n\nmacro addarg\n push hl\n; hl=arg  stktp=af\n;ex (sp),hl\n;ld a,h\n;push af\nendm\n\n\n\nmacro pusharg ,n\n getarg n\n push HL\nendm\n\nmacro pushlcl ,n\n getlcl n\n push HL\nendm\n\nmacro enter ,locals\n push ix\n ld ix,0\n add ix,sp\n rept locals\n  push HL\n endm\nendm\n\nmacro pops ,n\n rept n*2\n  inc sp\n endm\nendm\n\n\nmacro exit,n\n ld sp,ix\n pop ix\n if n!=0\n  exx\n  pop bc\n  pops n\n  push bc\n  exx\n endif\n ret\nendm\n\nmacro pushthis\n getthis\n push af\nendm\nmacro popthis\n popa\n ld (this),a\nendm\n\n\nmacro invoketg.a,fld,args\n; pushthis before arg push\n; hl=target \n ld a,h\n ld (this),a\n getfld fld\n callhl\n; pops args\n; popthis after \nendm\n\nmacro invoke,fld\n getfld fld\n callhl\n; pops args\n getthis\nendm\n\nmacro getfld, n\n local ad\n ld (ad-1),a\n ld hl,(n)\n ad:\nendm\n\nmacro setfld, n\n local ad\n ld (ad-1),a\n ld (n),hl\n ad:\nendm\n\nmacro getfldtg,n\n;hl=tg\n ld l,n\n peekw hl,hl\nendm\n\nmacro setfldtg,n\n; stk=val hl=tg\n ld l,n\n pop de\n pokew hl,de\nendm\n\nmacro getfldtg, n\n; hl=target\n ld d,h\n ld e,n\n peekw HL,de\nendm\n\nmacro tgconst,n\n ld (n-1),a\nendm\nmacro tgconst.g ,r16,n,fld\n ld r16,(fld)\n n:\nendm\nmacro tgconst.s ,n,fld,r16\n ld (fld),r16\n n:\nendm\n\n\nmacro curth2this\n ld a,(th.cur+1)\n ld (this),a\nendm\nmacro getthis\n ld a,(this)\nendm\n\nmacro new,Class,flds,st,en\n if nul st\n  th.new.range th.start, th.end\n else\n  th.new.range th.start+st*th.size, th.start+en*th.size\n endif\n pushi flds, bc\n pushi Class, bc\n call o.new\nendm\n\ndefsub o.new\nproc\n local retad,svthis,svsp,loop,lpend, w,allocfail,finally,lp2,lp2end\n ; {val .f} n &initbl retad\n pop hl;retad\n ld (retad-2),hl\n ; set initbl for th.with\n pop hl;&initbl\n th.with.setdst hl\n ; save this\n ld (svthis-1),a\n ; allocate thread\n call th.new\n jr nc, allocfail\n push hl; thread address\n call th.with.s; call &initbl\n pop hl; thread address\n ld a,h; set this as thread\n ; init fields\n pop bc; n of {val .f}\n inc c\n loop:\n  dec c\n  jr z,lpend\n  pop hl; .f\n  ld h,a\n  ld (w-2),hl\n  pop hl; val\n  ld (w),hl\n  w:\n jr loop\n lpend:\n ; return h as this\n ld h,a\n finally:\n  ;restore a before call o.new\n  ld a,0\n  svthis:\n  ;return \n  jp 0\n  retad:\n allocfail:\n  ; drop {val .f}\n  pop bc; n of {val .f}\n  ld b,c\n  inc c\n  lp2:\n   dec c\n   jr z, lp2end\n   pop hl\n   pop hl\n  jr lp2\n  lp2end:\n  ld hl,null;  todo null\n  jr finally\nendp\nendsub o.new\n\nmacro new.arg, n, v\n if not nul v\n  ld hl,v\n endif\n push hl\n pushi n,bc\nendm\n \nmacro o.assert.eq,fld, v\n local aa\n assert.do aa\n  getfld fld\n  assert.eq v\n  ret\n aa:\nendm\n\nthis:\ndb 0\n\nmacro fld.def,n\n n equ fldidx\n fldidx:defl fldidx-2\nendm\nmacro class,Class,super\n unreach "c"\n marker.b 0\n dw super\n fldidx:defl fld.top; todo fld.top\n Class:\n  fld .class,Class\nendm\nmacro fld.bottom,Class\n if defined Class##.bottom \n  if Class##.bottom ne fldidx\n   .error bottom ne fldidx\n  endif\n else\n Class##.bottom:defl fldidx\n endif\nendm \nmacro fld,n,v\n if defined n\n  if n ne fldidx\n   .error n ne fldidx\n  else \n   fldidx:defl fldidx-2\n  endif\n else\n  fld.def n\n endif\n pushi v,bc\nendm\nmacro unuse\n fldidx:defl fldidx-2\n pushi 0,bc\nendm\nmacro meth,Class,n\n fld .##n, Class##.##n\nendm\nmacro met2,Class,n\n fld n, Class##n\nendm\n\nclass Object,null\n fld .main,null\n fld.bottom Object\n marker.e Object\n\n\ndefsub o.boot\n curth2this\n invoke .main,0\nendsub o.boot\n\n\nmacro yield\n pushthis\n push ix\n call th.yield\n pop ix\n popthis\nendm\n\nmacro def,n,args,lcls\nhead n\n def.args:defl args\n def.locals:defl lcls\n if args>0 or lcls>0\n  enter lcls\n endif\nendm\nmacro enddef,n\n if def.args>0 or def.locals>0\n  exit def.args\n else\n  ret\n endif\n marker.e n\nendm\n\ndefsub isobj.a\n ;hl=obj?\n ;cy=true\n ld a,h\n cp high th.start\n jr c,notobj\n cp high th.end\n jr nc,notobj\n scf\n ret\n notobj:\n and a\nendsub isobj.a\n\ndefsub instanceof\n ; a=this de=Class\n ; z: true\n getfld .class\n jp is.subclass.a\nendsub instanceof\n\ndefsub get.superclass\n ; hl=Class\n dec hl\n dec hl\n peekw hl,hl\nendsub get.superclass\n\ndefsub is.subclass.a\nproc \n local top\n ; hl=Subclass\n ; de=Superclass\n ; z:true\n top:\n cpde.a 0\n ret z\n call get.superclass\n push de\n ld de,null\n cpde.a 0\n pop de\n jr nz,top\n cpde.a 0\nendp\nendsub is.subclass.a\n '].join(''),'spr': ['include const\ninclude th\ninclude mem\ninclude oop\ninclude sub\n\nclass Sprite,Object\n fld .main, 0\n fld.bottom Object\n fld .x, 100\n fld .y, 100\n fld .p, 0\n fld .c, 2\n fld.bottom Sprite\n marker.e Sprite\n \nmacro outwrt\n  out (98h),a\nendm\n\n\nmacro spr.unscale\n ; HL -> A\n rept spr.scale\n  srlhl\n endm\n LD A,L\n sub 8 \nendm\n\ndefsub spr.puts\nproc\n local t1,t2,t3,t4\n ld hl, 1b00h\n call SETWRT\n th.for.a sprl\n  ld a,h\n  tgconst t1\n  tgconst t2\n  tgconst t3\n  tgconst t4\n\n  tgconst.g hl,t1,.y \n  spr.unscale 0\n  outwrt 0\n  \n  tgconst.g hl,t2,.x \n  spr.unscale 0\n  outwrt 0\n  \n  tgconst.g a,t3,.p \n  sla a\n  sla a\n  outwrt 0\n  \n  tgconst.g a,t4,.c \n  outwrt 0\n  continue\n sprl:\nendp\nendsub spr.puts\n \n '].join(''),'sprpat': ['include const\n\n;aaa\nspr.inipat:\n ld de,3800h\n ld hl,spr.pat\n ld bc,128\n jp LDIRVM\nbg.inipat:\n ret\nspr.pat:\n; --- Slot 0 cat fstand\n; color 9\nDB $0C,$0E,$0F,$4F,$3D,$1D,$7F,$1B\nDB $0C,$3F,$7F,$7F,$6F,$0F,$06,$0C\nDB $18,$38,$F8,$F9,$DE,$DC,$7F,$6C\nDB $98,$FC,$FE,$FE,$F6,$F0,$60,$70\n; \n; --- Slot 1 cat fwalk1\n; color 9\nDB $0C,$0E,$0F,$4F,$3D,$1D,$7F,$1B\nDB $0C,$3F,$7F,$7F,$EF,$EF,$06,$06\nDB $18,$38,$F8,$F9,$DE,$DC,$7F,$6C\nDB $98,$FC,$FE,$FE,$D4,$78,$F0,$00\n; \n; --- Slot 2 cat fwalk2\n; color 9\nDB $18,$1C,$1F,$9F,$7B,$3B,$FE,$36\nDB $19,$3F,$7F,$7F,$2B,$1E,$0F,$00\nDB $30,$70,$F0,$F2,$BC,$B8,$FE,$D8\nDB $30,$FC,$FE,$FE,$F7,$F7,$60,$60\n; \n; --- Slot 3 cat omg\n; color 9\nDB $2C,$8E,$0F,$4B,$3D,$11,$7F,$1D\nDB $CA,$FF,$7F,$3F,$15,$1F,$0E,$00\nDB $1C,$39,$F8,$E9,$DE,$C4,$7F,$5C\nDB $AB,$FF,$FF,$FE,$AC,$F8,$70,$00\n\nds 60*32\n'].join(''),'tnu': ['\ninclude spr\ninclude bool\ninclude key\n\n;.onUpdate equ .c-2\n;.update equ .onUpdate-2\n;.screenOut equ .update-2\n;.die equ .screenOut-2\n;.updateEx equ .die-2\n\nmacro end.const, n\n pushi RActor.wait,bc\n pushi o.boot,bc\n th.with.ret 0 \n marker.e n\nendm\n\nmacro RActor.noovr,Class\n meth Class,main\n fld.bottom Object\n fld .x, 0\n fld .y, -1024\n fld .p, 0\n fld .c, 3\n fld.bottom Sprite\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut \n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld.bottom RActor\nendm\n\nclass RActor,Sprite\n RActor.noovr RActor\n end.const RActor\nRActor.main:\n enter 0\n exit 0\nRActor.update:\n invoke .onUpdate\n yield\n ret \nRActor.onUpdate:\n ret\nRActor.screenOut:\nproc\n local true\n getfld .x\n bit 1,h\n jr nz, true\n getfld .y\n ld de,192*2\n cpde.a\n getthis\n jr nc,true\n ld hl,0\n xor a\n ret\n true:\n ld hl,1\n scf\n ret\nendp\nRActor.wait:\nproc\n local lbl\n lbl:\n invoke .update\n jr lbl\nendp\ndef RActor.die,0,0\n ld h,a\n ld l,th.ofs.stp\n ld (hl),th.st.blank\n ld hl, 0\n setfld .c\nenddef RActor.die\n\ndef RActor.updateEx,1,0\nproc \n local n\n; enter 0\n getarg 1\n ld b,h\n ld c,l\n reptbc n\n  invoke .update\n  continue\n n:\nendp\nenddef RActor.updateEx\n\ncrashTo.size equ 8<<spr.scale\n\nproc\n local gx,gy,t1,t2\n local endc,cr1\n local fe\n\ndefsub crashTo.setXY\n getfld .x\n const gx,hl\n getfld .y\n const gy,hl\nendsub crashTo.setXY\n\n\ndef RActor.crashTo,1,0\n call crashTo.setXY\n getarg 1\n const cr.class,hl\n call isobj.a\n jr c, cr1\n  unreach "C"\n  ; "Cannot call target.crashTo(Class) "\n  ;ld hl, th.start\n  ;ld de, th.end\n  ;call crashTo1\n  ;jr endc\n cr1:\n  getthis 0\n  call crashTo1\n  flagtobool c\n endc:\nenddef RActor.crashTo\n\nmacro crashToClass,Class,st,en\n local nx,found\n ; a=this\n call crashTo.setXY\n foreach.a Class,st,en,nx\n  call crashTo1\n  break c\n  continue\n nx:\n getthis 0\n jr c, found\n  ld hl,null\n found:\nendm\n\nmacro foreach.a, Class,st,en,nxt\n th.for.a nxt, st, en\n  all.skip Class\nendm\n\n\nmacro all.skip.blank.self\n ; skip blank\n  ; TODO th.ofs.stp\n  call th.isblank.a\n  continue z\n  ; skip hl==this\n  getthis 0\n  cp h\n  continue z\nendm\nmacro all.skip.isnot,Class\n  ; skip object not instance of *Class*\n  push hl\n  ld a,h\n  ld de,Class\n  call instanceof\n  getthis 0\n  pop hl\n  continue nz\nendm\nmacro all.skip, Class\n all.skip.blank.self 0\n all.skip.isnot Class\nendm\n\ndefsub crashToC.abolished\n ;before:\n ; call crashTo.setXY\n ; const cr.class,Class\n ; hl start\n ; de end\n ld bc,th.size\n for fe\n  all.skip.blank.self 0\n  ; skip object not instance of *Class*\n  push hl\n  ld a,h\n  ldconst de,cr.class\n  call instanceof\n  pop hl\n  continue nz\n  ; do crashTo1\n  getthis 0\n  call crashTo1\n  break c\n  continue\n fe:\n getthis 0\n ret c\n ld hl,null\nendsub crashToC.abolished\n\ndefsub crashTo1\n ; call crashTo.setXY before\n ;hl=tg\n ;cy:true\n ;hl is used\n push af\n ld a,h\n tgconst t1\n tgconst t2\n pop af\n tgconst.g hl,t1,.x\n ldconst bc,gx\n subhl bc\n call abs\n ld bc,crashTo.size\n subhl bc\n ret nc\n\n tgconst.g hl,t2,.y\n ldconst bc,gy\n subhl bc\n call abs\n ld bc,crashTo.size\n subhl bc\nendsub crashTo1\n\nendp\n\n\nmacro tnu.run,Main\n ld sp,sp.ini\n call screen2\n \n showsp\n showlb endusr\n call spr.inipat\n call bg.inipat\n\n ld hl,th.start\n ld (hl),0cdh\n ld de,th.start+1\n ld bc,th.size*th.count-1\n ldir\n \n call th.init\n ;call mus.ini\n new Main, 0\n movw (h.thlop),spr.puts\n movw (h.thent),keyall\n jp th.loop\nendm\n\n;aaaa'].join(''),'key': ['include debug\n\ndefsub keyall\nproc\n;show hl\n local lp\n ld hl,keymat1\n ld de,keymat2\n ld bc,11\n ldir\n ld a,0\n ld hl,keymat1\n lp:\n push af\n call SNSMAT.a\n xor 255\n ld (hl),a\n pop af\n inc hl\n inc a\n cp 11\n jr c,lp\nendp\nendsub keyall\n\ndefwork keymat1\nds 11\nendwork keymat1\ndefwork keymat2\nds 11\nendwork keymat2\n\n\nproc\ndefsub getkey.a\nlocal chkmat\nex de,hl\nld hl,keymat1\ncall chkmat\nld hl,0\nret z\nld hl,keymat2\ncall chkmat\nld hl,1\nret z\ninc hl\nendsub getkey.a\n\ndefsub chkmat\npush de\nld a,d\nld d,0\nadd hl,de\nand (hl)\npop de\nendsub chkmat\n\ndefsub getkey\npush af\ncall getkey.a\npop af\nendsub getkey\n\nendp'].join(''),'map': ['include sub\ninclude math\ninclude tnu\n\ndefsub map.adr\n ; hl=chipx\n ; de=chipy\n rept 5\n  slde 0\n endm\n add hl,de\n ld de,1800h\n add hl,de\nendsub map.adr\n\ndefsub map.set.a\n ;  a=data\n call map.adr\n call 4dh\nendsub map.set.a\n\ndefsub map.get.a\n call map.adr\n call 4ah \nendsub map.get.a\n\ndefsub map.adrat.a\n ; hl=spr_x\n ; de=spr_y\n spr.unscale 0\n srl a\n srl a\n srl a\n push af\n ex de,hl\n spr.unscale 0\n srl a\n srl a\n srl a\n ld d,0\n ld e,a\n pop hl\n ld l,h\n ld h,0\n inc hl\n inc de\n call map.adr\nendsub map.adrat.a\n\ndefsub map.getat.a\n call map.adrat.a\n call 4ah\nendsub map.getat.a\n\ndefsub map.setat.a\n ; a=data\n push af\n call map.adrat.a\n pop af\n call 4dh\nendsub map.setat.a\n\ndefsub locate\n ; hl=chipx\n ; de=chipy\n call map.adr\n ld (cursor-2),hl\nendsub locate\n'].join(''),'maze': [].join(''),'t1': ['org 09000h\njp main\ninclude const\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude tnu\ninclude sp\n\n;===your code \n\nright:dw 0\n\nmain:\ntnu.run Main\ndef Main.main,0,0\nnew.arg .vx,1\nnew.arg .vy,0\nnew.arg .x,0\nnew.arg .y,100\nnew Cat,4\n\nnew.arg .x,100\nnew.arg .y,100\nnew Target,2\n\nld (right),hl\nld a,h\nld de,RActor\ncall instanceof\ncall showz\n\nld a,(right+1)\nld de,Target\ncall instanceof\ncall showz\n\nld a,(right+1)\nld de,Cat\ncall instanceof\ncall showz\n\n\nld hl,1\nsetfld .c\nenddef 0\n\nclass Main,RActor\n RActor.noovr Main\n end.const Main\nclass Target,RActor\n RActor.noovr Target\n met2 Target,.push\n end.const Target\ndef Target.main,0,0\nenddef\nclass Cat,RActor\n RActor.noovr Cat\n fld .vy, 0\n fld .vx, 0\n fld.bottom Cat\n end.const Cat\ndef Cat.main,0,0\n blp:\n  ld hl,0108h\n  call getkey\n  jpf nomov\n  getthis\n  ;x+=vx\n  getfld .x\n  ex de, hl\n  getfld .vx\n  add hl,de\n  setfld .x\n  nomov:\n  ; y+=vy\n  getfld .y\n  ex de, hl\n  getfld .vy\n  add hl,de\n  setfld .y\n  ld hl,(right)\n  push hl\n  invoke .crashTo\n  jpf cr\n   ; r.x+=10\n   ld hl,(right)\n   getfldtg .x\n   ld de,10\n   add hl,de\n   push hl\n   ld hl,(right)\n   setfldtg .x\n   ; r.push()\n   pushthis 0\n   ld hl,(right)\n   invoketg.a .push\n   popthis 0\n  cr:\n  invoke .update\n jp blp\nenddef\n; test t1\ndef Target.push,0,0\n ld hl,3\n setfld .p\n repti 30,pse\n  getfld .x\n  inc hl\n  setfld .x\n  invoke .update\n  continue\n pse:\n ld hl,0\n setfld .p\nenddef\n\nendusr: \ninclude sprpat\n\nend main\nhttps://msxpen.com/codes/-N6DDfMvZq9aUeJ9JLpN\nhttps://msxpen.com/codes/-N6QGYk-rr5iDuTtHpF7'].join(''),'t2': ['org 08000h\ninclude tnu\ninclude bool\ninclude map\n\nmain:\ntnu.run Main\n;range 0-5\nclass EBullet,RActor\n meth EBullet,main\n fld .x,0\n fld .y,0\n fld .p,0\n fld .c,0\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut\n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld .vx,0\n fld .vy,0\n end.const 0\ndef EBullet.main,0,0\n ;p=7;\n ld hl,7\n setfld .p\n ;c=15;\n ld hl,15\n setfld .c\n lb1:\n ld hl,true\n jpf lb2\n ;x+=vx;\n getfld .vx\n push hl\n getfld .x\n pop de\n add hl, de\n setfld .x\n ;y+=vy;\n getfld .vy\n push hl\n getfld .y\n pop de\n add hl, de\n setfld .y\n invoke .screenOut\n jpf lb4\n ;die();\n invoke .die\n jp lb3\n lb4:\n lb3:\n ;update();\n invoke .update\n jp lb1\n lb2:\n ret\n;range 5-15\nclass Enemy,RActor\n meth Enemy,main\n fld .x,0\n fld .y,0\n fld .p,0\n fld .c,0\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut\n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld .vx,0\n fld .bvx,0\n fld .bvy,0\n fld .bdist,0\n meth Enemy,fire\n end.const 0\ndef Enemy.main,0,0\n lb5:\n ld hl,200\n push hl\n getfld .y\n pop de\n call hlltde\n jpf lb6\n ;y+=2;\n ld hl,2\n push hl\n getfld .y\n pop de\n add hl, de\n setfld .y\n ;update();\n invoke .update\n jp lb5\n lb6:\n ld hl,(gbl_player)\n getfldtg .x\n push hl\n getfld .x\n pop de\n call hlgtde\n jpf lb8\n ;vx=0-2;\n ld hl,2\n push hl\n ld hl,0\n pop de\n subhl de\n setfld .vx\n jp lb7\n lb8:\n ;vx=2;\n ld hl,2\n setfld .vx\n lb7:\n ;fire();\n invoke .fire\n lb9:\n ld hl,true\n jpf lb10\n invoke .screenOut\n jpf lb12\n ;die();\n invoke .die\n jp lb11\n lb12:\n lb11:\n ;y+=2;\n ld hl,2\n push hl\n getfld .y\n pop de\n add hl, de\n setfld .y\n ;x+=vx;\n getfld .vx\n push hl\n getfld .x\n pop de\n add hl, de\n setfld .x\n ;update();\n invoke .update\n jp lb9\n lb10:\n ret\ndef Enemy.fire,0,0\n ;bvx=$player.x-x;\n getfld .x\n push hl\n ld hl,(gbl_player)\n getfldtg .x\n pop de\n subhl de\n setfld .bvx\n ;bvy=$player.y-y;\n getfld .y\n push hl\n ld hl,(gbl_player)\n getfldtg .y\n pop de\n subhl de\n setfld .bvy\n ;bdist=abs(bvx)+abs(bvy);\n getfld .bvy\n call abs\n push hl\n getfld .bvx\n call abs\n pop de\n add hl, de\n setfld .bdist\n ;bdist/=8;\n pushthis\n getfld .bdist\n push hl\n ld hl,8\n pop de\n call IDIV.a\n popthis\n setfld .bdist\n ;bvx/=bdist;\n pushthis\n getfld .bvx\n push hl\n getfld .bdist\n pop de\n call IDIV.a\n popthis\n setfld .bvx\n ;bvy/=bdist;\n pushthis\n getfld .bvy\n push hl\n getfld .bdist\n pop de\n call IDIV.a\n popthis\n setfld .bvy\n ;new EBullet{        x,y,vx:bvx,vy:bvy    };\n getfld .x\n new.arg .x\n getfld .y\n new.arg .y\n getfld .bvx\n new.arg .vx\n getfld .bvy\n new.arg .vy\n new EBullet,4,0,5\n ret\n;range 0-5\nclass Main,RActor\n meth Main,main\n fld .x,0\n fld .y,0\n fld .p,0\n fld .c,0\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut\n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld .i,0\n end.const 0\ndef Main.main,0,0\n ;$player=new Player{x:256, y: 300, p:$pat_spr+4, c:15};\n ld hl,256\n new.arg .x\n ld hl,300\n new.arg .y\n ld hl,4\n push hl\n ld hl,(gbl_pat_spr)\n pop de\n add hl, de\n new.arg .p\n ld hl,15\n new.arg .c\n new Player,4,0,5\n ld (gbl_player),hl\n ld hl,0\n setfld .i\n lb13:\n ld hl,20\n push hl\n getfld .i\n pop de\n call hlltde\n jpf lb14\n ;new Enemy{x:rnd(512), y:0, p:5, c:7};\n ld hl,512\n call rnd\n new.arg .x\n ld hl,0\n new.arg .y\n ld hl,5\n new.arg .p\n ld hl,7\n new.arg .c\n new Enemy,4,5,15\n ;updateEx(30);\n ld hl,30\n push hl\n invoke .updateEx\n jp lb13\n lb14:\n ret\n;range 15-20\nclass PBullet,RActor\n meth PBullet,main\n fld .x,0\n fld .y,0\n fld .p,0\n fld .c,0\n meth PBullet,onUpdate\n meth RActor,update\n meth RActor,screenOut\n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld .e,0\n end.const 0\ndef PBullet.main,0,0\n ;p=6;\n ld hl,6\n setfld .p\n ;c=8;\n ld hl,8\n setfld .c\n lb15:\n ld hl,true\n jpf lb16\n invoke .screenOut\n jpf lb18\n ;die();\n invoke .die\n jp lb17\n lb18:\n lb17:\n ;y-=6;\n ld hl,6\n push hl\n getfld .y\n pop de\n subhl de\n setfld .y\n crashToClass Enemy, 5, 15\n setfld .e\n getfld .e\n jpf lb20\n ;e.die();\n pushthis 0\n getfld .e\n invoketg.a .die\n popthis 0\n ;die();\n invoke .die\n jp lb19\n lb20:\n lb19:\n ;update();\n invoke .update\n jp lb15\n lb16:\n ret\ndef PBullet.onUpdate,0,0\n ;c+=1;\n ld hl,1\n push hl\n getfld .c\n pop de\n add hl, de\n setfld .c\n ld hl,14\n push hl\n getfld .c\n pop de\n call hlgtde\n jpf lb22\n ;c=0;\n ld hl,0\n setfld .c\n jp lb21\n lb22:\n lb21:\n ret\n;range 0-5\nclass Player,RActor\n meth Player,main\n fld .x,0\n fld .y,0\n fld .p,0\n fld .c,0\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut\n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n end.const 0\ndef Player.main,0,0\n lb23:\n ld hl,3\n ld de,10\n call locate\n getfld .y\n ld a,l\n call wrt\n ld a,h\n call wrt\n getthis 0\n \n ex de,hl\n getfld .x\n ld a,35\n call map.setat.a\n getthis 0\n ld hl,true\n jpf lb24\n ld hl, 4104\n call getkey\n jpf lb26\n ;x-=3;\n ld hl,3\n push hl\n getfld .x\n pop de\n subhl de\n setfld .x\n jp lb25\n lb26:\n lb25:\n ld hl, 32776\n call getkey\n jpf lb28\n ;x+=3;\n ld hl,3\n push hl\n getfld .x\n pop de\n add hl, de\n setfld .x\n jp lb27\n lb28:\n lb27:\n ld hl, 8200\n call getkey\n jpf lb30\n ;y-=3;\n ld hl,3\n push hl\n getfld .y\n pop de\n subhl de\n setfld .y\n jp lb29\n lb30:\n lb29:\n ld hl, 16392\n call getkey\n jpf lb32\n ;y+=3;\n ld hl,3\n push hl\n getfld .y\n pop de\n add hl, de\n setfld .y\n jp lb31\n lb32:\n lb31:\n crashToClass Enemy, 5, 15\n jpf lb34\n ;die();\n invoke .die\n jp lb33\n lb34:\n lb33:\n crashToClass EBullet, 0, 5\n jpf lb36\n ;die();\n invoke .die\n jp lb35\n lb36:\n lb35:\n ld hl,1\n push hl\n ld hl, 264\n call getkey\n pop de\n call hleqde\n jpf lb38\n ;new PBullet{x,y};\n getfld .x\n new.arg .x\n getfld .y\n new.arg .y\n new PBullet,2,15,20\n foreach.a Enemy,5,14,nxx\n  ld a,h\n  ld hl,8\n  setfld .c\n  continue \n nxx:\n getthis 0\n \n \n jp lb37\n lb38:\n lb37:\n ;update();\n invoke .update\n jp lb23\n lb24:\n ret\nenddef 0\nendusr:\ngbl_player:dw 0\ngbl_pat_spr:dw 0\nspr.inipat:\n ld de,3800h\n ld hl,spr.pat\n ld bc,2048\n jp LDIRVM\nspr.pat:\ndb $c,$e,$f,$4f,$3d,$1d,$7f,$1b,$c,$3f,$7f,$7f,$6f,$f,$6,$c\ndb $18,$38,$f8,$f9,$de,$dc,$7f,$6c,$98,$fc,$fe,$fe,$f6,$f0,$60,$70\ndb $c,$e,$f,$4f,$3d,$1d,$7f,$1b,$c,$3f,$7f,$7f,$ef,$ef,$6,$6\ndb $18,$38,$f8,$f9,$de,$dc,$7f,$6c,$98,$fc,$fe,$fe,$d4,$78,$f0,$0\ndb $18,$1c,$1f,$9f,$7b,$3b,$fe,$36,$19,$3f,$7f,$7f,$2b,$1e,$f,$0\ndb $30,$70,$f0,$f2,$bc,$b8,$fe,$d8,$30,$fc,$fe,$fe,$f7,$f7,$60,$60\ndb $2c,$8e,$f,$4b,$3d,$11,$7f,$1d,$ca,$ff,$7f,$3f,$15,$1f,$e,$0\ndb $1c,$39,$f8,$e9,$de,$c4,$7f,$5c,$ab,$ff,$ff,$fe,$ac,$f8,$70,$0\ndb $1,$1,$1,$2,$3,$92,$96,$f6,$f6,$fe,$fe,$fe,$ff,$af,$26,$0\ndb $80,$80,$80,$40,$c0,$49,$69,$6f,$6f,$7f,$7f,$7f,$ff,$f5,$64,$0\ndb $8,$10,$30,$30,$18,$f,$3f,$37,$7b,$7d,$3f,$1f,$37,$60,$70,$c\ndb $8,$4,$6,$6,$c,$f8,$fe,$f6,$ef,$df,$fe,$fc,$f6,$3,$7,$18\ndb $0,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$0,$0\ndb $80,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$80,$0\ndb $0,$0,$0,$0,$3,$f,$f,$1f,$1f,$1f,$1f,$f,$f,$3,$0,$0\ndb $0,$0,$0,$0,$c0,$f0,$f0,$f8,$f8,$f8,$f8,$f0,$f0,$c0,$0,$0\nbg.inipat:\n ret\n\n\nend main'].join(''),'t3': ['org 09000h\n\ninclude tnu\ninclude mus\ninclude sprpat\n\n;===your code \n\nmain:\nld sp,(8000h)\n\ncall screen1\n\nshowsp\nshowlb endusr\n\nld hl,8000h\nld (hl),0cdh\nld de,8001h\nld bc,th.size*th.count-1\nldir\n\n\ncall th.init\ncall spr.inipat\n;call mus.ini\n\n\nnew Main, 0\nshow hl\n\n\nmovw (h.thlop),spr.puts\njp th.loop\n\nclass Main, RActor\n meth Main,main\n fld.bottom Object\n fld .x,100\n fld .y,300\n fld .p,0\n fld .c,3\n fld.bottom Sprite\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut \n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld.bottom RActor\n fld.bottom Main\n end.const\n\nMain.main:\n olp:\n  getthis\n  invoke .update\n  call xrnd.a\n  ld a,h\n  and 15\n  jr nz,doap\n   getthis\n   getfld .x\n   new.arg .x\n   getfld .y\n   new.arg .y\n   ld hl,7\n   call rnd.a\n   ld de,3\n   sbc hl,de\n   new.arg .vx\n   ld hl,5\n   call rnd.a\n   ld de,15\n   sbc hl,de\n   new.arg .vy\n   new Bullet, 4\n   call dstk\n  doap:\n  ld a,8\n  call SNSMAT.a\n  and 1\n  jr z,golf\n  \n  getthis\n  getfld .x\n  inc hl\n  inc hl\n  setfld .x\n  ld de,400\n  cpde.a\n  jp c, olp\n  golf:\n  ld hl,0\n  getthis\n  setfld .x\n jp olp\n\n\nclass Bullet,RActor\n meth Bullet,main\n fld.bottom Object\n fld .x, 0\n fld .y, 0\n fld .p, 2\n fld .c, 15\n fld.bottom Sprite\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut \n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld.bottom RActor\n fld .vy, -10\n fld .vx, 0\n fld.bottom Bullet\n end.const \n \nBullet.main:\n blp:\n  getthis\n  ;x+=vx\n  getfld .x\n  ex de, hl\n  getfld .vx\n  add hl,de\n  setfld .x\n  ; y+=vy\n  getfld .y\n  ex de, hl\n  getfld .vy\n  add hl,de\n  setfld .y\n  getfld .vy\n  inc hl\n  setfld .vy\n\n  invoke .update\n  invoke .screenOut\n  jp c, bdie\n  getfld .vy\n  bit 7,h\n  jr nz,blp\n  ld de,5\n  cpde.a\n  jr c,blp\n \n  call dstk\n  getthis\n  ld hl,3\n  setfld .p\n  pushi 10,bc\n  invoke .updateEx\n\n bleft:\n  getthis\n  ld hl,2\n  setfld .p\n  getfld .x\n  dec hl\n  dec hl\n  setfld .x\n  getfld .y\n  dec hl\n  setfld .y\n  invoke .update\n  invoke .screenOut\n  jr c, bdie\n  jr bleft\n bdie:\n  invoke .die\n  ret \n\n  \ndstk:\n push af\n ld hl,th.start+256*3\n getthis\n ld h,a\n ld de,1900h\n ld bc,256\n call LDIRVM\n pop af\n ret\n \nendusr:\nend main'].join(''),'t4': ['org 09000h\njp main\ninclude const\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude tnu\ninclude sp\n\n;===your code \n\nright:dw 0\n\nmain:\ntnu.run Main\ndef Main.main,0,0\nnew.arg .vx,1\nnew.arg .vy,0\nnew.arg .x,0\nnew.arg .y,100\nnew Cat,4\n\nnew.arg .x,100\nnew.arg .y,100\nnew Target,2\n\nnew.arg .x,200\nnew.arg .y,100\nnew Target,2\n\n\nnew.arg .x,150\nnew.arg .y,100\nnew.arg .c,8\nnew NTarget,3\n\nld (right),hl\nld a,h\nld de,Actor\ncall instanceof\ncall showz\n\nld a,(right+1)\nld de,Target\ncall instanceof\ncall showz\n\nld a,(right+1)\nld de,Cat\ncall instanceof\ncall showz\n\n\nld hl,1\nsetfld .c\nenddef 0\n\nclass Main,Actor\n Actor.noovr Main\n end.const 0\nclass Target,Actor\n Actor.noovr Target\n met2 Target,.push\n end.const 0\nclass NTarget,Actor\n Actor.noovr NTarget\n end.const 0\ndef NTarget.main,0,0\n ret\nenddef\n\ndef Target.main,0,0\nenddef\nclass Cat,Actor\n Actor.noovr Cat\n fld .vy, 0\n fld .vx, 0\n fld.bottom Cat\n end.const 0\ndef Cat.main,0,0\n blp:\n  getthis\n  ;x+=vx\n  getfld .x\n  ex de, hl\n  getfld .vx\n  add hl,de\n  setfld .x\n  ; y+=vy\n  getfld .y\n  ex de, hl\n  getfld .vy\n  add hl,de\n  setfld .y\n  ld hl,Target\n  push hl\n  invoke .crashTo\n  jpf cr\n   ; r.x+=10\n   const setg,hl\n   getfldtg .y\n   ld de,30\n   add hl,de\n   push hl\n   ldconst hl,setg\n   setfldtg .y\n  cr:\n  invoke .update\n jp blp\nenddef\ndef Target.push,0,0\n ld hl,3\n setfld .p\n repti 30,pse\n  getfld .y\n  inc hl\n  setfld .y\n  invoke .update\n  continue\n pse:\n ld hl,0\n setfld .p\nenddef\n\nendusr: \nend main\nhttps://msxpen.com/codes/-N6DDfMvZq9aUeJ9JLpN\nhttps://msxpen.com/codes/-N6QGYk-rr5iDuTtHpF7'].join(''),'t5': ['org 9000h\n\n\ninclude key\n\nmain:\ncall keyall\nld hl,0108h\ncall getkey\nshow hl\nld hl,0107h\ncall getkey\nshow hl\n\n\nhalt\njp main'].join(''),'gen': ['org 09000h\ninclude tnu\ninclude bool\n\nmain:\ntnu.run Main\nclass Main,Actor\n Actor.noovr Main\n end.const 0\ndef Main.main,0,0\n\n showlb .main\n showlb .crashTo\nenddef 0\nendusr:\nend main'].join(''),'dac': ['org 09000h\njp main\ninclude const\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude th\n\n\nDECSUB equ 268CH;DAC ← DAC-ARG\nDECADD equ 269AH;DAC ← DAC+ARG\nDECNRM equ 26FAH;DAC を正規化する (*1)\nDECROU equ 273CH;DAC を四捨五入する\nDECMUL equ 27E6H;DAC ← DAC*DAC\nDECDIV equ 289FH;DAC ← DAC/DAC\nMAF equ 2C4DH;ARG ← DAC\nMAM equ 2C50H;ARG ← [HL]\nMOV8DH equ 2C53H;[DE] ← [HL]\nMFA equ 2C59H;DAC ← ARG\nMFM equ 2C5CH;[HL] ← DAC\nMMF equ 2C67H;[HL] ← DAC\nMOV8HD equ 2C6AH;[HL] ← [DE]\nXTF equ 2C6FH;[SP] ←→ DAC\nPHA equ 2CC7H;ARG → [SP]\nPHF equ 2CCCH;DAC → [SP]\nPPA equ 2CDCH;[SP] → ARG\nPPF equ 2CE1H;[SP] → DAC\nPUSHF equ 2EB1H;DAC → [SP]\nMOVFM equ 2EBEH;DAC ← [HL]\nMOVFR equ 2EC1H;DAC ← (CBED)\nMOVRF equ 2ECCH;(CBED) ← DAC\nMOVRMI equ 2ED6H;(CBDE) ← [HL]\nMOVRM equ 2EDFH;(BCDE) ← [HL]\nMOVMF equ 2EE8H;[HL] ← DAC\nMOVE equ 2EEBH;[HL] ← [DE]\nVMOVAM equ 2EEFH;ARG ← [HL]\nMOVVFM equ 2EF2H;[DE] ← [HL]\nVMOVE equ 2EF3H;[HL] ← [DE]\nVMOVFA equ 2F05H;DAC ← ARG\nVMOVFM equ 2F08H;DAC ← [HL]\nVMOVAF equ 2F0DH;ARG ← DAC\nVMOVMF equ 2F10H;[HL] ← DAC\n\nVALTYP equ 0F663H;1\nDAC equ 0F7F6H;16\nARG equ 0F847H;16\nFOUT equ 3425H\n\ndefsub int2dac\n push af\n ld a,2\n ld (VALTYP),a\n ld (DAC+2),HL\n pop af\nendsub int2dac\n;===your code \n\nmain:\nld hl,12345\ncall int2dac\nld hl,str\ncall FOUT\n\nld b,10\nreptb nxt\n ld a,(hl)\n cp 0\n break z\n call wrt2\n inc hl\n continue\nnxt:\nret\nstr:\n\n\n'].join(''),'setvrm': ['org 09000h\njp main\ninclude const\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude th\n\n;===your code \n\nmain:\nld hl,1800h\ncall SETWRT\nld a,35\nrepti 5, ed\ninc a\nout (98h),a\ncontinue\ned:\nret'].join(''),'assert': ['include mem\ninclude math\ninclude debug\n\na.reg.trc:\ndw 0\na.reg.adr:\ndw 0\na.reg.min:\ndw 0\na.reg.val:\ndw 0\na.reg.max:\ndw 0\nmacro a.regi,n,v\n push hl\n ld hl,v\n ld (a.reg.##n),hl\n pop hl\nendm\nmacro a.regr,n,v\n ld (a.reg.##n),v\nendm\n\nmacro a.dummy\n local a.reg.,trc,adr,min,val,nax\nendm\n \n\nmacro assert.eq,o\n storelastpc\n pushall \n if not nul o\n  a.regi val, o\n endif\n ld de,(a.reg.val)\n ld(a.reg.val),hl\n ld(a.reg.min),de\n ld(a.reg.max),de\n cpde\n jp nz,assert.fail\n popall\nendm\n\nmacro assert.do,nx\n storelastpc\n pushall\n call to\n popall\n jr nx\n to:\nendm\n\nmacro storelastpc\n push hl\n call getpc\n ld (lastpc),hl\n pop hl\nendm\nlastpc:\n dw 0\n \ngetpc:\n pop hl\n push hl\n ret\n\nassert.fail:\n ld hl,0deadh\n show hl\n showm a.reg.trc\n showm a.reg.min\n showm a.reg.val\n showm a.reg.max\n showm a.reg.adr\n showm lastpc\n call freeze\nmacro assert.meqw,ad,val\n a.regi adr,ad\n push hl\n ld hl,(ad)\n assert.eq val\n pop hl\nendm\n '].join(''),'stksz': ['org 09000h\njp main\ninclude const\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude th\n\n\n;===your code \n\nsz equ 256\n  \nmain:\nld hl,0fd9fh\nld (hl),0c9h\n rept sz/2\n  push hl\n endm\n rept sz/2\n  pop hl\n endm\n\nloop:\n getsp\n ld de,-sz\n add hl,de\n ld de,1800h\n ld bc,sz\n call LDIRVM\n ld hl,0\n halt\n jp loop\n \n \n '].join(''),'vdp': [';https://www.msx.org/wiki/VDP_Status_Registers\n;st 0 bit 7\n;read 1\n\n;https://www.msx.org/wiki/VDP_Mode_Registers\n;ctrl 1 bit 5 set 0\ninclude const\n\nsusint:\n ld a,(RG1SAV)\n res 5,a\n ld b,A\n ld c,1\n jp WRTVDP\n;rstint:\n ld a,(RG1SAV)\n set 5,a\n ld b,A\n ld c,1\n jp WRTVDP\ninted:\n call RDVDP\n bit 7,a\n ret\ndoint:\n call inted\n jr z, norst\n ld hl,timecnt\n inc (hl)\n call h.tntimi\n norst:\n ;call rstint\n ret\nh.tntimi:\n ld A,(timecnt)\n ld hl,1ae0h\n call 4dh\n ret\n ds 16\n \ntimecnt:\ndb 0\nmacro vdptest\nlocal stk1,stk2,stk3,vl\nstk1:\n ds 256,35\nstk2:\n ds 256,42\nstk3:\n\nvl:\n call susint\n ld sp,stk2\n ld hl,stk1\n ld de,1800h\n ld bc,256\n call LDIRVM\n \n \n ld sp,stk3\n call doint\n ld hl,stk2\n ld de,1900h\n ld bc,256\n call LDIRVM\n jp vl\nendm\n \nscreen1:\n ld a,1\n call CHGMOD\n ld a,(RG1SAV)\n set 1,a\n ld b,A\n ld c,1\n call WRTVDP\n ret\n\ndefsub screen2\n ld a,2\n call CHGMOD\n ld a,(RG1SAV)\n set 1,a\n ld b,A\n ld c,1\n call WRTVDP\nendsub screen2\n '].join(''),'mus': ['include mem\nmus.ini:\n di\n ld hl,0fd9fh\n ld (hl),0c3h\n movw (0fd9fh+1),mus\n ei\n ret\nmus:\nproc\nlocal we\n push af\n push de\n ld a,(we-1)\n xor 15\n ld (we-1),a \n ld a,8\n ld e,15\n we:\n call WRTPSG\n pop af \n pop de\n ret\nendp'].join('')};
        
      },
      fiber$main :function* _trc_Asms_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.data = {'main': ['org 09000h\n\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude sp\ninclude vdp\ninclude th\n\n\n;===your code \n\nmain:\nld a,5\nlp:\ndec b\nshow a\niff nc,nx\njr lp\nnx:\n\nret\nend main'].join(''),'math': ['include ctrl\n\n;16bit shifts\nmacro slhl\n sla l\n rl h\nendm\nmacro srahl\n sra h\n rr l\nendm\nmacro srlhl\n srl h\n rr l\nendm\nmacro slde\n sla e\n rl d\nendm\nmacro srade\n sra d\n rr e\nendm\nmacro srlde\n srl d\n rr e\nendm\nmacro slbc\n sla c\n rl b\nendm\nmacro srabc\n sra b\n rr c\nendm\nmacro srlbc\n srl b\n rr c\nendm\n\n\n; for xrnd\nmacro sldehl,n\n  local loop\n  ld b,n\n loop:\n  sla d\n  rl e\n  rl h\n  rl l\n  djnz loop\nendm\nmacro srdehl,n\n local loop\n ld b,n\n loop:\n  srl l\n  rr h\n  rr e\n  rr d\n djnz loop\nendm\n \nmacro xorrm,re,me\n  ld A,(me)\n  xor re\n  ld (me),a\nendm\n\nmacro subhl,rp\n and a\n sbc hl,rp\nendm\n\nmacro cpde.a\n rst dcompr\nendm\n\n\nmarker.b xrnd.a\nxrnd.a:\nproc \n local rhl,rde,rdhlde\n ; s[0] ^= s[0] << 13\n call rdhlde\n sldehl 13\n call wrtxor\n ; s[0] ^= s[0] >> 17\n call rdhlde\n srdehl 17\n call wrtxor\n ; s[0] ^= s[0] << 5;\n call rdhlde\n sldehl 5\n call wrtxor\n ret\n \n rdhlde:\n  ld hl,1234\n rhl:\n  ld de,5678\n rde:\n  ret\n \n wrtxor:\n  xorrm h,rhl-1\n  xorrm l,rhl-2\n  xorrm d,rde-1\n  xorrm e,rde-2\n  ret\nendp\nmarker.e xrnd.a\n\n\nmarker.b rnd\nrnd:\n push af\n call rnd.a\n pop af\n ret\nmarker.e rnd\nmarker.b rnd.a\nrnd.a:\n ld de,07fffh\n call IDIV.a\n push hl\n call xrnd.a\n res 7,h\n ex de,hl\n pop hl\n inc hl\n call IDIV.a\n ret\nmarker.e rnd.a\n\nmarker.b abs\nabs:\n bit 7,h\n ret z\nneghl:\n ld de,0 \n ex de,hl \n subhl de\n ret\nmarker.e abs\n\n '].join(''),'bool': ['include math\ninclude ctrl\ntrue equ -1\nfalse equ 0\n\nmacro rethl,val,flg\n local tru\n if not nul flg\n  iff flg ,tru\n endif\n ld hl,val\n ret\n tru:\nendm\n\nhleqde:\n subhl de\n rethl true,z\n rethl false\n\nhlnede:\n subhl de\n rethl true,nz\n rethl false\n \nhlgtde:\n subhl de\n rethl false,z\n bit 7,h\n rethl true,z\n rethl false\n\nhlltde:\n subhl de\n bit 7,h\n rethl true,nz\n rethl false\n \nhlgede:\n subhl de\n rethl true,z\n bit 7,h\n rethl true,z\n rethl false\n\nhllede:\n subhl de\n rethl true,z\n bit 7,h\n rethl true,nz\n rethl false\n \nproc\nziffalse:\n local resa\n ld (resa-1),a\n call ziffalse.a\n ld A,0\n resa:\n ret\nziffalse.a:\n ld a,0\n cp h\n ret nz\n cp l\n ret\nendp\n\nmacro jpf,to\n call ziffalse\n jp z,to\nendm\n\nmacro andand,fls\n jpf fls\nendm\nmacro oror,tru\n call ziffalse\n jp nz,tru\nendm\n\nmacro flagtobool,fl\n local yes,skp\n jr fl, yes\n ld hl,false\n jr skp\n yes:\n ld hl,true\n skp: \nendm'].join(''),'mem': ['include const\n;\nrdslt:\n ex de,hl\n rept 5\n srl d;page*2\n endm\n CALL RSLREG\n ld e,d\n rdslt1:\n  RRCA\n  dec e\n  jp nz,rdslt1\n AND    00000011B\n LD C,A;000000Pr\n LD B,0\n LD HL,EXPTBL\n ADD HL,BC\n LD C,A;000000Pr\n LD A,(HL)\n AND 80H;expand flag\n OR C\n LD C,A;e00000Pr\n rept 4;const\n INC HL\n endm\n LD A,(HL);exp reg\n ld e,d\n rdslt2:\n  srl a\n  dec e\n  jp nz,rdslt2\n;000000Ex\n sla a\n sla a\n ;    0000Ex00\n and  00001100b\n OR C;e000ExPr\n ret\nmemini:\n CALL RSLREG\n rept 4\n  RRCA\n endm\n AND    00000011B\n LD C,A;000000Pr\n LD B,0\n LD HL,EXPTBL\n ADD HL,BC\n LD C,A;000000Pr\n LD A,(HL)\n AND 80H;expand flag\n OR C\n LD C,A;e00000Pr\n rept 4;const\n INC HL\n endm\n LD A,(HL);exp reg\n rept 4; page*2\n srl a\n endm;000000Ex\n sla a\n sla a\n ;    0000Ex00\n and  00001100b\n OR C;e000ExPr\n LD Hl,04000H\n jp ENASLT\n\nmacro peekw ,regv,regm\n  local w\n  ld (w-2),regm\n  ld regv,(0)\n  w:\nendm\n\nmacro pokew ,regm,regv\n  local w\n  ld (w-2),regm\n  ld (0),regv\n  w:\nendm\nmacro movw,dst,src,rp\n if nul rp\n  push hl\n  movw dst,src,hl\n  pop hl\n else\n  ld rp,src\n  ld dst,rp\n endif \nendm\n\nmacro popa\n  ex (sp),hl\n  ld a,h\n  pop HL\nendm\n\nmacro pushall\n push af\n push bc\n push de\n push hl\nendm\nmacro popall\n pop hl\n pop de\n pop bc\n pop af\nendm\n \n\nmacro pushi, n,rp\n local rrr\n if nul rp\n  ld (rrr-2),hl\n  ld hl,n\n  push hl\n  ld hl,0\n  rrr:\n else\n  ld rp,n\n  push rp\n endif\nendm\nmacro const,n,reg\n ld (n-2),reg\nendm\nmacro ldconst,reg,n\n ld reg,0\n n:\nendm\nmacro peekconst,reg,n\n ld reg,(0)\n n:\nendm\n'].join(''),'const': ['\n;wrt equ 0a2h\ndcompr equ 0020H\nsp.ini equ 0dc00h\nstksize equ 512\n\nth.size equ 256\nth.count equ 20\nth.start equ th.end-th.size*th.count\nth.end equ sp.ini-stksize\n\nth.bottom equ 0\n\nspr.scale equ 1\nspr.xmax equ 256<<spr.scale\nspr.ymax equ 192<<spr.scale\n\nENASLT EQU 0024H\nRSLREG EQU 0138H\nEXPTBL EQU 0FCC1H\nSETWRT equ 0053H\nLDIRVM equ 005CH\nWRTVDP equ 0047H\nRG1SAV equ 0F3E0H\nRDVDP  equ 013EH\nSNSMAT.a equ 0141h\n\nCHGMOD equ 005FH\n\nIMULT.a equ 3193H;HL ← DE*HL\nIDIV.a equ 31E6H;HL ← DE/HL\nIMOD.a equ 323AH;HL ← DE mod HL (DE ← DE/HL) \n\nWRTPSG  equ 0093H\n\nCSRY equ 0F3DCH\nCSRX equ 0F3DDH\n\nnull equ 0\n\nmacro marker.b, n\n last.marker: defl $\nendm\nmacro marker.e, n\n len.##n: defl $-last.marker\nendm\n'].join(''),'ctrl': ['include const\nfreeze:\nhalt\njr freeze\n\nmacro for ,lbend\n ; uses a\n ; c: breaked\n proc\n  local s,lb\n  lb:\n  call dcompr; uses a\n  jp nc,lbend\n  push HL\n  push de\n  push bc\n  call s\n  pop bc\n  pop de\n  pop HL\n  jp c,lbend\n  add HL,bc\n  jr lb\n  s:\n endp\nendm\n\nmacro repti ,n,lbend\n proc\n  local s,lb, lbend2\n  push bc\n  ld b,n\n  lb:\n  push bc\n  call s\n  pop bc\n  jr c,lbend2\n  djnz lb\n  lbend2:\n  pop bc\n  jp lbend \n  s:\n endp\nendm\n\n\nmacro reptb ,lbend\n  local s,lb\n inc b\n djnz lb\n jp lbend\n lb:\n  push bc\n  call s\n  pop bc\n  jp c,lbend\n djnz lb\n jp lbend \n s:\nendm\n\n\n\nmacro callsva,pp\n local sva\n ld (sva-1),a\n call pp\n ld a,0\n sva:\nendm\nbcis0:\n callsva bcis0.a\n ret\nbcis0.a:\n ld a,b\n and a\n ret nz\n ld a,c\n and a\n ret\n\nmacro reptbc ,lbend\n local s,lb\n call bcis0\n jp z,lbend \n lb:\n  push bc\n  call s\n  pop bc\n  jp c,lbend\n  dec bc\n  call bcis0\n jr nz, lb\n jp lbend \n s:\nendm\n\n\niff.NZ equ 0\niff.Z  equ 1\niff.NC equ 2\niff.C  equ 3\n\nmacro iff,cnd,to\n local iff.\n if iff.##cnd eq iff.NZ\n  jr z,to\n endif\n if iff.##cnd eq iff.Z\n  jr nz,to\n endif\n if iff.##cnd eq iff.NC\n  jr c,to\n endif\n if iff.##cnd eq iff.C\n  jr nc,to\n endif\n ;jr cnd, skip\n ;jr to\n ;skip:\nendm\n\nmacro break,cnd\n if NUL cnd\n  scf\n  ret\n else\n  proc \n   local jj\n   iff cnd ,jj\n   break\n  jj:\n  endp\n endif\nendm\nmacro continue,cnd\n if NUL cnd \n  or a\n  ret\n else\n  proc \n   local jj\n   iff cnd,jj\n   continue\n  jj:\n  endp\n endif\nendm\n\n\nmacro djnzr,reg, j\n dec reg\n jr NZ,j\nendm\n\nmacro callhl\n local LCD\n ld (LCD-2),HL\n call LCD\n LCD:\nendm\n\nmacro stride,lim,to\n if (low $)<lim\n  exitm\n endif\n ds 256+to-(low $),0cdh\nendm'].join(''),'th': ['include ctrl\ninclude sp\ninclude vdp\ninclude mem\ninclude math\ninclude debug\n\nth.ofs.stp equ 256-4\nth.ofs.sp equ th.ofs.stp+1\nth.ofs.spini equ th.ofs.stp\nfld.top equ th.ofs.spini-2\nth.st.blank equ 0c9h\nth.st.active equ 31h\n\n;macro th.for,lb\n; ld HL,th.start\n; ld de,th.end\n; ld bc,th.size\n; for lb\n;endm\nmacro th.for.a, nx, st, en\n if nul st\n  th.for.a nx, 0, th.count\n  exitm\n endif\n local do, loop\n ld a,st+(high th.start)\n loop:\n  cp en+(high th.start)\n  jp nc, nx\n  push af\n  ld h,a\n  ld l,0\n  call do\n  popa\n  ld h,a\n  ld l,0\n  jp c, nx\n  inc a\n jr loop\n do:\nendm\n\nmacro th.new.range,st,en\n ld bc,st\n ld(th.new.start),bc\n ld bc,en\n ld(th.new.end),bc\nendm\n\ndefsub th.isblank.a\n ; h= thread\n ; z if true\n ld l, th.ofs.stp\n ld a,(hl)\n cp th.st.blank\nendsub th.isblank.a\n\ndefsub th.new\n; nc for alloc fail\nproc \n local lbend\n db 21h\n th.new.start:\n dw th.start\n db 11h\n th.new.end:\n dw th.end\n ld bc,th.size\n for lbend\n  ; TODO th.ofs.stp\n  call th.isblank.a\n  break z\n  continue\n lbend:\n ret nc\n ; TODO th.ofs.stp\n ld L,th.ofs.stp\n ld (HL),31h\n inc HL\n ld (HL),th.ofs.spini\n ld a,h\n inc HL\n ld (hl),a\n inc HL\n ld (HL),0c9h\n ld l,th.bottom\n scf\n ret\nendp\nendsub th.new\n\ndefsub th.init\nproc\n local lbend\n th.for.a lbend\n  ; TODO th.ofs.stp\n  ld L, th.ofs.stp\n  ld (HL),th.st.blank\n  continue\n lbend:\n ; disable timer\n ld HL,0fd9fh\n ld (hl),0c9h\n call susint\n ret\nendp\nendsub th.init\n\ndefsub th.stepall\n th.for.a thnx\n  ;todo th.ofs.stp\n  ld (th.cur),hl\n  call th.isblank.a\n  continue z\n  call th.step\n  continue\n thnx:\nendsub th.stepall\n\ndefsub th.step\n sp2mem adrssp+1\n ld HL,(th.cur)\n ld l,th.ofs.stp\n ;call susint\n jp (hl)\nendsub th.step\n\ndefsub th.yield\n ld hl,(th.cur)\n ld l,th.ofs.sp\n sp2mem\n adrssp:\n ld sp,0\n jp doint\nendsub th.yield\n\ndefsub th.term\n ld hl,(th.cur)\n ; TODO th.ofs.stp\n ld L,th.ofs.stp\n ld (hl),th.st.blank\n jr adrssp\nendsub th.term\n\nmacro th.with.do, to\n local pr\n th.with pr\n jr to\n pr:\nendm\n\nmacro th.with.setdst, reg\n ld (th.jpdest-2),reg\nendm\nmacro th.with,pr\n movw (th.jpdest-2), pr\n call th.with.s\nendm\nmacro th.with.ret\n jp th.ewith\nendm\n\ndefsub th.with.s\n sp2mem th.wrssp-2\n ld l, th.ofs.sp\n ld (th.updsp-2),hl\n mem2sp\n jp 0\n th.jpdest:\nth.ewith:\n ld (0),sp\n th.updsp:\n ld sp,0\n th.wrssp:\nendsub th.with.s\n \n\n \n \ndefsub th.push\n ;push bc to thread hl\n th.with tpsbc\n ret\n tpsbc:\n  push bc\n  th.with.ret 0\nendsub th.push\n\n\ndefwork th.cur\n dw 0\nendwork th.cur\n\ndefsub th.loop\n ; hook before stepall\n db 0cdh\n h.thent:\n dw th.nop\n ; save prev timecnt\n ld a,(timecnt)\n push af\n ; Do stepall\n call th.stepall\n ; hook after stepall\n db 0cdh\n h.thlop:\n dw th.nop\n ; wait until timecnt changes\n pop af\n bwat:\n  ld hl,timecnt\n  cp (hl)\n  jr nz,bbwat\n  push af\n  call doint\n  pop af\n  jr bwat\n bbwat:\n ; repeat\n jr th.loop\nendsub th.loop\n\nth.nop:\n ret\n\n\nmacro th.pushi, val\n ld bc,val\n call th.push\nendm\n\n'].join(''),'sub': [].join(''),'debug': ['include math\n;debug\nmacro show,reg\n ld (hexval+1),reg\n call showhex\nendm\nmacro showm ,ad\n push hl\n ld HL,(ad)\n show HL\n pop HL\nendm\nmacro showlb,lb\n push hl\n ld hl,lb\n ld (hexval+1),hl\n call showhex\n pop hl\nendm\nshowhex:\nproc\n local loop\n push af\n push bc\n push HL\n hexval:\n ld hl,0\n ld b,4\n loop:\n  xor a\n  rept 4\n   slhl\n   rla\n  endm\n  call showhex1\n djnz loop\n ld a,32\n call wrt\n pop HL\n pop bc\n pop af\n ret\nendp\nshowhex1:\nproc\n local els\n cp 10\n jp nc, els\n add a,48\n jp wrt\n els:\n add a,65-10\n jp wrt\nendp\nabort:\n call wrt\n db 018h,0feh\nret\n\nmacro trace,v\n if not nul v\n  push af\n  ld a,v\n  ld (trad),a\n  pop af\n endif\n call trace.s\nendm\ntrace.s:\n push af\n push hl\n ld a,(trad)\n ld hl,1ae0h\n call wrt\n call 4dh\n inc a\n ld (trad),a\n ld a,32\n call wrt \n pop hl\n pop af\n ret\ntrad:\n db 65\n\nshowz:\n push af\n jr z,showz.s\n ld a,"N"\n call wrt\n showz.s:\n ld a,"Z"\n call wrt\n ld a,32\n call wrt\n pop af\n ret\n \n\nshowc:\n push af\n jr c,showc.s\n ld a,"N"\n call wrt\n showc.s:\n ld a,"C"\n call wrt\n ld a,32\n call wrt\n pop af\n ret\n \n\n\n\n\n\nmacro unreach, mesg\n trace mesg\n dw 0x18,0xfe\nendm\nmacro head, lb\n unreach lb\n marker.b lb\n lb:\nendm\n\nmacro defsub, n\n head n\nendm\nmacro endsub, n\n ret\n marker.e n\nendm\nmacro defwork, n\n head n\nendm\nmacro endwork, n\n marker.e n\nendm\n\ndefsub wrt\nproc\n local sk\n push hl\n push af\n ld hl,1800h\n cursor:\n call 4dh\n inc hl\n ld a,h\n cp 1bh\n jr c,sk\n  ld h,18h\n sk:\n ld (cursor-2),hl\n pop af\n pop hl\n ret\nendp\nendsub wrt\n'].join(''),'sp': ['include mem\ninclude debug\nmacro sp.get\n ld HL,0\n ADD hl, sp\nendm\nmacro sp.set\n ld sp,hl\nendm\nmacro mem2sp,ad\n local rs\n if nul ad\n  ld (rs-2),hl\n  ld sp,(0)\n  rs:\n else\n  ld sp,(ad)\n endif\nendm\nmacro sp2mem,ad\n local spad\n if nul ad\n  ld (spad-2),hl\n  ld (0),sp\n  spad:\n else\n  ld (ad),sp\n endif\nendm\n\nmacro showsp\n ld (sptmp),sp\n showm sptmp\nendm\nsptmp:\ndw 0\nmacro showstk\n showsp\n ld (sva),a\n ld a,":"\n call wrt\n ld a,(sva)\n ex (sp),hl\n show hl\n ex (sp),hl\nendm\nsva: db 0'].join(''),'oop': ['include mem\ninclude th\ninclude assert\n\n;a2 a1  oldpc oldix lcl1 lcl2\nargidx equ 2\nmacro getarg ,n\n ld l,(ix+argidx+n*2)\n ld h,(ix+argidx+n*2+1)\nendm\n\nmacro setlcl ,n\n ld (IX-(n*2-1)),h\n ld (ix-n*2),l\nendm\n\nmacro getlcl ,n\n ld h,(IX-(n*2-1))\n ld l,(ix-n*2)\nendm\n\nmacro addarg\n push hl\n; hl=arg  stktp=af\n;ex (sp),hl\n;ld a,h\n;push af\nendm\n\n\n\nmacro pusharg ,n\n getarg n\n push HL\nendm\n\nmacro pushlcl ,n\n getlcl n\n push HL\nendm\n\nmacro enter ,locals\n push ix\n ld ix,0\n add ix,sp\n rept locals\n  push HL\n endm\nendm\n\nmacro pops ,n\n rept n*2\n  inc sp\n endm\nendm\n\n\nmacro exit,n\n ld sp,ix\n pop ix\n if n!=0\n  exx\n  pop bc\n  pops n\n  push bc\n  exx\n endif\n ret\nendm\n\nmacro pushthis\n getthis\n push af\nendm\nmacro popthis\n popa\n ld (this),a\nendm\n\n\nmacro invoketg.a,fld,args\n; pushthis before arg push\n; hl=target \n ld a,h\n ld (this),a\n getfld fld\n callhl\n; pops args\n; popthis after \nendm\n\nmacro invoke,fld\n getfld fld\n callhl\n; pops args\n getthis\nendm\n\nmacro getfld, n\n local ad\n ld (ad-1),a\n ld hl,(n)\n ad:\nendm\n\nmacro setfld, n\n local ad\n ld (ad-1),a\n ld (n),hl\n ad:\nendm\n\nmacro getfldtg,n\n;hl=tg\n ld l,n\n peekw hl,hl\nendm\n\nmacro setfldtg,n\n; stk=val hl=tg\n ld l,n\n pop de\n pokew hl,de\nendm\n\nmacro getfldtg, n\n; hl=target\n ld d,h\n ld e,n\n peekw HL,de\nendm\n\nmacro tgconst,n\n ld (n-1),a\nendm\nmacro tgconst.g ,r16,n,fld\n ld r16,(fld)\n n:\nendm\nmacro tgconst.s ,n,fld,r16\n ld (fld),r16\n n:\nendm\n\n\nmacro curth2this\n ld a,(th.cur+1)\n ld (this),a\nendm\nmacro getthis\n ld a,(this)\nendm\n\nmacro new,Class,flds,st,en\n if nul st\n  th.new.range th.start, th.end\n else\n  th.new.range th.start+st*th.size, th.start+en*th.size\n endif\n pushi flds, bc\n pushi Class, bc\n call o.new\nendm\n\ndefsub o.new\nproc\n local retad,svthis,svsp,loop,lpend, w,allocfail,finally,lp2,lp2end\n ; {val .f} n &initbl retad\n pop hl;retad\n ld (retad-2),hl\n ; set initbl for th.with\n pop hl;&initbl\n th.with.setdst hl\n ; save this\n ld (svthis-1),a\n ; allocate thread\n call th.new\n jr nc, allocfail\n push hl; thread address\n call th.with.s; call &initbl\n pop hl; thread address\n ld a,h; set this as thread\n ; init fields\n pop bc; n of {val .f}\n inc c\n loop:\n  dec c\n  jr z,lpend\n  pop hl; .f\n  ld h,a\n  ld (w-2),hl\n  pop hl; val\n  ld (w),hl\n  w:\n jr loop\n lpend:\n ; return h as this\n ld h,a\n finally:\n  ;restore a before call o.new\n  ld a,0\n  svthis:\n  ;return \n  jp 0\n  retad:\n allocfail:\n  ; drop {val .f}\n  pop bc; n of {val .f}\n  ld b,c\n  inc c\n  lp2:\n   dec c\n   jr z, lp2end\n   pop hl\n   pop hl\n  jr lp2\n  lp2end:\n  ld hl,null;  todo null\n  jr finally\nendp\nendsub o.new\n\nmacro new.arg, n, v\n if not nul v\n  ld hl,v\n endif\n push hl\n pushi n,bc\nendm\n \nmacro o.assert.eq,fld, v\n local aa\n assert.do aa\n  getfld fld\n  assert.eq v\n  ret\n aa:\nendm\n\nthis:\ndb 0\n\nmacro fld.def,n\n n equ fldidx\n fldidx:defl fldidx-2\nendm\nmacro class,Class,super\n unreach "c"\n marker.b 0\n dw super\n fldidx:defl fld.top; todo fld.top\n Class:\n  fld .class,Class\nendm\nmacro fld.bottom,Class\n if defined Class##.bottom \n  if Class##.bottom ne fldidx\n   .error bottom ne fldidx\n  endif\n else\n Class##.bottom:defl fldidx\n endif\nendm \nmacro fld,n,v\n if defined n\n  if n ne fldidx\n   .error n ne fldidx\n  else \n   fldidx:defl fldidx-2\n  endif\n else\n  fld.def n\n endif\n pushi v,bc\nendm\nmacro unuse\n fldidx:defl fldidx-2\n pushi 0,bc\nendm\nmacro meth,Class,n\n fld .##n, Class##.##n\nendm\nmacro met2,Class,n\n fld n, Class##n\nendm\n\nclass Object,null\n fld .main,null\n fld.bottom Object\n marker.e Object\n\n\ndefsub o.boot\n curth2this\n invoke .main,0\nendsub o.boot\n\n\nmacro yield\n pushthis\n push ix\n call th.yield\n pop ix\n popthis\nendm\n\nmacro def,n,args,lcls\nhead n\n def.args:defl args\n def.locals:defl lcls\n if args>0 or lcls>0\n  enter lcls\n endif\nendm\nmacro enddef,n\n if def.args>0 or def.locals>0\n  exit def.args\n else\n  ret\n endif\n marker.e n\nendm\n\ndefsub isobj.a\n ;hl=obj?\n ;cy=true\n ld a,h\n cp high th.start\n jr c,notobj\n cp high th.end\n jr nc,notobj\n scf\n ret\n notobj:\n and a\nendsub isobj.a\n\ndefsub instanceof\n ; a=this de=Class\n ; z: true\n getfld .class\n jp is.subclass.a\nendsub instanceof\n\ndefsub get.superclass\n ; hl=Class\n dec hl\n dec hl\n peekw hl,hl\nendsub get.superclass\n\ndefsub is.subclass.a\nproc \n local top\n ; hl=Subclass\n ; de=Superclass\n ; z:true\n top:\n cpde.a 0\n ret z\n call get.superclass\n push de\n ld de,null\n cpde.a 0\n pop de\n jr nz,top\n cpde.a 0\nendp\nendsub is.subclass.a\n '].join(''),'spr': ['include const\ninclude th\ninclude mem\ninclude oop\ninclude sub\n\nclass Sprite,Object\n fld .main, 0\n fld.bottom Object\n fld .x, 100\n fld .y, 100\n fld .p, 0\n fld .c, 2\n fld.bottom Sprite\n marker.e Sprite\n \nmacro outwrt\n  out (98h),a\nendm\n\n\nmacro spr.unscale\n ; HL -> A\n rept spr.scale\n  srlhl\n endm\n LD A,L\n sub 8 \nendm\n\ndefsub spr.puts\nproc\n local t1,t2,t3,t4\n ld hl, 1b00h\n call SETWRT\n th.for.a sprl\n  ld a,h\n  tgconst t1\n  tgconst t2\n  tgconst t3\n  tgconst t4\n\n  tgconst.g hl,t1,.y \n  spr.unscale 0\n  outwrt 0\n  \n  tgconst.g hl,t2,.x \n  spr.unscale 0\n  outwrt 0\n  \n  tgconst.g a,t3,.p \n  sla a\n  sla a\n  outwrt 0\n  \n  tgconst.g a,t4,.c \n  outwrt 0\n  continue\n sprl:\nendp\nendsub spr.puts\n \n '].join(''),'sprpat': ['include const\n\n;aaa\nspr.inipat:\n ld de,3800h\n ld hl,spr.pat\n ld bc,128\n jp LDIRVM\nbg.inipat:\n ret\nspr.pat:\n; --- Slot 0 cat fstand\n; color 9\nDB $0C,$0E,$0F,$4F,$3D,$1D,$7F,$1B\nDB $0C,$3F,$7F,$7F,$6F,$0F,$06,$0C\nDB $18,$38,$F8,$F9,$DE,$DC,$7F,$6C\nDB $98,$FC,$FE,$FE,$F6,$F0,$60,$70\n; \n; --- Slot 1 cat fwalk1\n; color 9\nDB $0C,$0E,$0F,$4F,$3D,$1D,$7F,$1B\nDB $0C,$3F,$7F,$7F,$EF,$EF,$06,$06\nDB $18,$38,$F8,$F9,$DE,$DC,$7F,$6C\nDB $98,$FC,$FE,$FE,$D4,$78,$F0,$00\n; \n; --- Slot 2 cat fwalk2\n; color 9\nDB $18,$1C,$1F,$9F,$7B,$3B,$FE,$36\nDB $19,$3F,$7F,$7F,$2B,$1E,$0F,$00\nDB $30,$70,$F0,$F2,$BC,$B8,$FE,$D8\nDB $30,$FC,$FE,$FE,$F7,$F7,$60,$60\n; \n; --- Slot 3 cat omg\n; color 9\nDB $2C,$8E,$0F,$4B,$3D,$11,$7F,$1D\nDB $CA,$FF,$7F,$3F,$15,$1F,$0E,$00\nDB $1C,$39,$F8,$E9,$DE,$C4,$7F,$5C\nDB $AB,$FF,$FF,$FE,$AC,$F8,$70,$00\n\nds 60*32\n'].join(''),'tnu': ['\ninclude spr\ninclude bool\ninclude key\n\n;.onUpdate equ .c-2\n;.update equ .onUpdate-2\n;.screenOut equ .update-2\n;.die equ .screenOut-2\n;.updateEx equ .die-2\n\nmacro end.const, n\n pushi RActor.wait,bc\n pushi o.boot,bc\n th.with.ret 0 \n marker.e n\nendm\n\nmacro RActor.noovr,Class\n meth Class,main\n fld.bottom Object\n fld .x, 0\n fld .y, -1024\n fld .p, 0\n fld .c, 3\n fld.bottom Sprite\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut \n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld.bottom RActor\nendm\n\nclass RActor,Sprite\n RActor.noovr RActor\n end.const RActor\nRActor.main:\n enter 0\n exit 0\nRActor.update:\n invoke .onUpdate\n yield\n ret \nRActor.onUpdate:\n ret\nRActor.screenOut:\nproc\n local true\n getfld .x\n bit 1,h\n jr nz, true\n getfld .y\n ld de,192*2\n cpde.a\n getthis\n jr nc,true\n ld hl,0\n xor a\n ret\n true:\n ld hl,1\n scf\n ret\nendp\nRActor.wait:\nproc\n local lbl\n lbl:\n invoke .update\n jr lbl\nendp\ndef RActor.die,0,0\n ld h,a\n ld l,th.ofs.stp\n ld (hl),th.st.blank\n ld hl, 0\n setfld .c\nenddef RActor.die\n\ndef RActor.updateEx,1,0\nproc \n local n\n; enter 0\n getarg 1\n ld b,h\n ld c,l\n reptbc n\n  invoke .update\n  continue\n n:\nendp\nenddef RActor.updateEx\n\ncrashTo.size equ 8<<spr.scale\n\nproc\n local gx,gy,t1,t2\n local endc,cr1\n local fe\n\ndefsub crashTo.setXY\n getfld .x\n const gx,hl\n getfld .y\n const gy,hl\nendsub crashTo.setXY\n\n\ndef RActor.crashTo,1,0\n call crashTo.setXY\n getarg 1\n const cr.class,hl\n call isobj.a\n jr c, cr1\n  unreach "C"\n  ; "Cannot call target.crashTo(Class) "\n  ;ld hl, th.start\n  ;ld de, th.end\n  ;call crashTo1\n  ;jr endc\n cr1:\n  getthis 0\n  call crashTo1\n  flagtobool c\n endc:\nenddef RActor.crashTo\n\nmacro crashToClass,Class,st,en\n local nx,found\n ; a=this\n call crashTo.setXY\n foreach.a Class,st,en,nx\n  call crashTo1\n  break c\n  continue\n nx:\n getthis 0\n jr c, found\n  ld hl,null\n found:\nendm\n\nmacro foreach.a, Class,st,en,nxt\n th.for.a nxt, st, en\n  all.skip Class\nendm\n\n\nmacro all.skip.blank.self\n ; skip blank\n  ; TODO th.ofs.stp\n  call th.isblank.a\n  continue z\n  ; skip hl==this\n  getthis 0\n  cp h\n  continue z\nendm\nmacro all.skip.isnot,Class\n  ; skip object not instance of *Class*\n  push hl\n  ld a,h\n  ld de,Class\n  call instanceof\n  getthis 0\n  pop hl\n  continue nz\nendm\nmacro all.skip, Class\n all.skip.blank.self 0\n all.skip.isnot Class\nendm\n\ndefsub crashToC.abolished\n ;before:\n ; call crashTo.setXY\n ; const cr.class,Class\n ; hl start\n ; de end\n ld bc,th.size\n for fe\n  all.skip.blank.self 0\n  ; skip object not instance of *Class*\n  push hl\n  ld a,h\n  ldconst de,cr.class\n  call instanceof\n  pop hl\n  continue nz\n  ; do crashTo1\n  getthis 0\n  call crashTo1\n  break c\n  continue\n fe:\n getthis 0\n ret c\n ld hl,null\nendsub crashToC.abolished\n\ndefsub crashTo1\n ; call crashTo.setXY before\n ;hl=tg\n ;cy:true\n ;hl is used\n push af\n ld a,h\n tgconst t1\n tgconst t2\n pop af\n tgconst.g hl,t1,.x\n ldconst bc,gx\n subhl bc\n call abs\n ld bc,crashTo.size\n subhl bc\n ret nc\n\n tgconst.g hl,t2,.y\n ldconst bc,gy\n subhl bc\n call abs\n ld bc,crashTo.size\n subhl bc\nendsub crashTo1\n\nendp\n\n\nmacro tnu.run,Main\n ld sp,sp.ini\n call screen2\n \n showsp\n showlb endusr\n call spr.inipat\n call bg.inipat\n\n ld hl,th.start\n ld (hl),0cdh\n ld de,th.start+1\n ld bc,th.size*th.count-1\n ldir\n \n call th.init\n ;call mus.ini\n new Main, 0\n movw (h.thlop),spr.puts\n movw (h.thent),keyall\n jp th.loop\nendm\n\n;aaaa'].join(''),'key': ['include debug\n\ndefsub keyall\nproc\n;show hl\n local lp\n ld hl,keymat1\n ld de,keymat2\n ld bc,11\n ldir\n ld a,0\n ld hl,keymat1\n lp:\n push af\n call SNSMAT.a\n xor 255\n ld (hl),a\n pop af\n inc hl\n inc a\n cp 11\n jr c,lp\nendp\nendsub keyall\n\ndefwork keymat1\nds 11\nendwork keymat1\ndefwork keymat2\nds 11\nendwork keymat2\n\n\nproc\ndefsub getkey.a\nlocal chkmat\nex de,hl\nld hl,keymat1\ncall chkmat\nld hl,0\nret z\nld hl,keymat2\ncall chkmat\nld hl,1\nret z\ninc hl\nendsub getkey.a\n\ndefsub chkmat\npush de\nld a,d\nld d,0\nadd hl,de\nand (hl)\npop de\nendsub chkmat\n\ndefsub getkey\npush af\ncall getkey.a\npop af\nendsub getkey\n\nendp'].join(''),'map': ['include sub\ninclude math\ninclude tnu\n\ndefsub map.adr\n ; hl=chipx\n ; de=chipy\n rept 5\n  slde 0\n endm\n add hl,de\n ld de,1800h\n add hl,de\nendsub map.adr\n\ndefsub map.set.a\n ;  a=data\n call map.adr\n call 4dh\nendsub map.set.a\n\ndefsub map.get.a\n call map.adr\n call 4ah \nendsub map.get.a\n\ndefsub map.adrat.a\n ; hl=spr_x\n ; de=spr_y\n spr.unscale 0\n srl a\n srl a\n srl a\n push af\n ex de,hl\n spr.unscale 0\n srl a\n srl a\n srl a\n ld d,0\n ld e,a\n pop hl\n ld l,h\n ld h,0\n inc hl\n inc de\n call map.adr\nendsub map.adrat.a\n\ndefsub map.getat.a\n call map.adrat.a\n call 4ah\nendsub map.getat.a\n\ndefsub map.setat.a\n ; a=data\n push af\n call map.adrat.a\n pop af\n call 4dh\nendsub map.setat.a\n\ndefsub locate\n ; hl=chipx\n ; de=chipy\n call map.adr\n ld (cursor-2),hl\nendsub locate\n'].join(''),'maze': [].join(''),'t1': ['org 09000h\njp main\ninclude const\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude tnu\ninclude sp\n\n;===your code \n\nright:dw 0\n\nmain:\ntnu.run Main\ndef Main.main,0,0\nnew.arg .vx,1\nnew.arg .vy,0\nnew.arg .x,0\nnew.arg .y,100\nnew Cat,4\n\nnew.arg .x,100\nnew.arg .y,100\nnew Target,2\n\nld (right),hl\nld a,h\nld de,RActor\ncall instanceof\ncall showz\n\nld a,(right+1)\nld de,Target\ncall instanceof\ncall showz\n\nld a,(right+1)\nld de,Cat\ncall instanceof\ncall showz\n\n\nld hl,1\nsetfld .c\nenddef 0\n\nclass Main,RActor\n RActor.noovr Main\n end.const Main\nclass Target,RActor\n RActor.noovr Target\n met2 Target,.push\n end.const Target\ndef Target.main,0,0\nenddef\nclass Cat,RActor\n RActor.noovr Cat\n fld .vy, 0\n fld .vx, 0\n fld.bottom Cat\n end.const Cat\ndef Cat.main,0,0\n blp:\n  ld hl,0108h\n  call getkey\n  jpf nomov\n  getthis\n  ;x+=vx\n  getfld .x\n  ex de, hl\n  getfld .vx\n  add hl,de\n  setfld .x\n  nomov:\n  ; y+=vy\n  getfld .y\n  ex de, hl\n  getfld .vy\n  add hl,de\n  setfld .y\n  ld hl,(right)\n  push hl\n  invoke .crashTo\n  jpf cr\n   ; r.x+=10\n   ld hl,(right)\n   getfldtg .x\n   ld de,10\n   add hl,de\n   push hl\n   ld hl,(right)\n   setfldtg .x\n   ; r.push()\n   pushthis 0\n   ld hl,(right)\n   invoketg.a .push\n   popthis 0\n  cr:\n  invoke .update\n jp blp\nenddef\n; test t1\ndef Target.push,0,0\n ld hl,3\n setfld .p\n repti 30,pse\n  getfld .x\n  inc hl\n  setfld .x\n  invoke .update\n  continue\n pse:\n ld hl,0\n setfld .p\nenddef\n\nendusr: \ninclude sprpat\n\nend main\nhttps://msxpen.com/codes/-N6DDfMvZq9aUeJ9JLpN\nhttps://msxpen.com/codes/-N6QGYk-rr5iDuTtHpF7'].join(''),'t2': ['org 08000h\ninclude tnu\ninclude bool\ninclude map\n\nmain:\ntnu.run Main\n;range 0-5\nclass EBullet,RActor\n meth EBullet,main\n fld .x,0\n fld .y,0\n fld .p,0\n fld .c,0\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut\n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld .vx,0\n fld .vy,0\n end.const 0\ndef EBullet.main,0,0\n ;p=7;\n ld hl,7\n setfld .p\n ;c=15;\n ld hl,15\n setfld .c\n lb1:\n ld hl,true\n jpf lb2\n ;x+=vx;\n getfld .vx\n push hl\n getfld .x\n pop de\n add hl, de\n setfld .x\n ;y+=vy;\n getfld .vy\n push hl\n getfld .y\n pop de\n add hl, de\n setfld .y\n invoke .screenOut\n jpf lb4\n ;die();\n invoke .die\n jp lb3\n lb4:\n lb3:\n ;update();\n invoke .update\n jp lb1\n lb2:\n ret\n;range 5-15\nclass Enemy,RActor\n meth Enemy,main\n fld .x,0\n fld .y,0\n fld .p,0\n fld .c,0\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut\n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld .vx,0\n fld .bvx,0\n fld .bvy,0\n fld .bdist,0\n meth Enemy,fire\n end.const 0\ndef Enemy.main,0,0\n lb5:\n ld hl,200\n push hl\n getfld .y\n pop de\n call hlltde\n jpf lb6\n ;y+=2;\n ld hl,2\n push hl\n getfld .y\n pop de\n add hl, de\n setfld .y\n ;update();\n invoke .update\n jp lb5\n lb6:\n ld hl,(gbl_player)\n getfldtg .x\n push hl\n getfld .x\n pop de\n call hlgtde\n jpf lb8\n ;vx=0-2;\n ld hl,2\n push hl\n ld hl,0\n pop de\n subhl de\n setfld .vx\n jp lb7\n lb8:\n ;vx=2;\n ld hl,2\n setfld .vx\n lb7:\n ;fire();\n invoke .fire\n lb9:\n ld hl,true\n jpf lb10\n invoke .screenOut\n jpf lb12\n ;die();\n invoke .die\n jp lb11\n lb12:\n lb11:\n ;y+=2;\n ld hl,2\n push hl\n getfld .y\n pop de\n add hl, de\n setfld .y\n ;x+=vx;\n getfld .vx\n push hl\n getfld .x\n pop de\n add hl, de\n setfld .x\n ;update();\n invoke .update\n jp lb9\n lb10:\n ret\ndef Enemy.fire,0,0\n ;bvx=$player.x-x;\n getfld .x\n push hl\n ld hl,(gbl_player)\n getfldtg .x\n pop de\n subhl de\n setfld .bvx\n ;bvy=$player.y-y;\n getfld .y\n push hl\n ld hl,(gbl_player)\n getfldtg .y\n pop de\n subhl de\n setfld .bvy\n ;bdist=abs(bvx)+abs(bvy);\n getfld .bvy\n call abs\n push hl\n getfld .bvx\n call abs\n pop de\n add hl, de\n setfld .bdist\n ;bdist/=8;\n pushthis\n getfld .bdist\n push hl\n ld hl,8\n pop de\n call IDIV.a\n popthis\n setfld .bdist\n ;bvx/=bdist;\n pushthis\n getfld .bvx\n push hl\n getfld .bdist\n pop de\n call IDIV.a\n popthis\n setfld .bvx\n ;bvy/=bdist;\n pushthis\n getfld .bvy\n push hl\n getfld .bdist\n pop de\n call IDIV.a\n popthis\n setfld .bvy\n ;new EBullet{        x,y,vx:bvx,vy:bvy    };\n getfld .x\n new.arg .x\n getfld .y\n new.arg .y\n getfld .bvx\n new.arg .vx\n getfld .bvy\n new.arg .vy\n new EBullet,4,0,5\n ret\n;range 0-5\nclass Main,RActor\n meth Main,main\n fld .x,0\n fld .y,0\n fld .p,0\n fld .c,0\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut\n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld .i,0\n end.const 0\ndef Main.main,0,0\n ;$player=new Player{x:256, y: 300, p:$pat_spr+4, c:15};\n ld hl,256\n new.arg .x\n ld hl,300\n new.arg .y\n ld hl,4\n push hl\n ld hl,(gbl_pat_spr)\n pop de\n add hl, de\n new.arg .p\n ld hl,15\n new.arg .c\n new Player,4,0,5\n ld (gbl_player),hl\n ld hl,0\n setfld .i\n lb13:\n ld hl,20\n push hl\n getfld .i\n pop de\n call hlltde\n jpf lb14\n ;new Enemy{x:rnd(512), y:0, p:5, c:7};\n ld hl,512\n call rnd\n new.arg .x\n ld hl,0\n new.arg .y\n ld hl,5\n new.arg .p\n ld hl,7\n new.arg .c\n new Enemy,4,5,15\n ;updateEx(30);\n ld hl,30\n push hl\n invoke .updateEx\n jp lb13\n lb14:\n ret\n;range 15-20\nclass PBullet,RActor\n meth PBullet,main\n fld .x,0\n fld .y,0\n fld .p,0\n fld .c,0\n meth PBullet,onUpdate\n meth RActor,update\n meth RActor,screenOut\n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld .e,0\n end.const 0\ndef PBullet.main,0,0\n ;p=6;\n ld hl,6\n setfld .p\n ;c=8;\n ld hl,8\n setfld .c\n lb15:\n ld hl,true\n jpf lb16\n invoke .screenOut\n jpf lb18\n ;die();\n invoke .die\n jp lb17\n lb18:\n lb17:\n ;y-=6;\n ld hl,6\n push hl\n getfld .y\n pop de\n subhl de\n setfld .y\n crashToClass Enemy, 5, 15\n setfld .e\n getfld .e\n jpf lb20\n ;e.die();\n pushthis 0\n getfld .e\n invoketg.a .die\n popthis 0\n ;die();\n invoke .die\n jp lb19\n lb20:\n lb19:\n ;update();\n invoke .update\n jp lb15\n lb16:\n ret\ndef PBullet.onUpdate,0,0\n ;c+=1;\n ld hl,1\n push hl\n getfld .c\n pop de\n add hl, de\n setfld .c\n ld hl,14\n push hl\n getfld .c\n pop de\n call hlgtde\n jpf lb22\n ;c=0;\n ld hl,0\n setfld .c\n jp lb21\n lb22:\n lb21:\n ret\n;range 0-5\nclass Player,RActor\n meth Player,main\n fld .x,0\n fld .y,0\n fld .p,0\n fld .c,0\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut\n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n end.const 0\ndef Player.main,0,0\n lb23:\n ld hl,3\n ld de,10\n call locate\n getfld .y\n ld a,l\n call wrt\n ld a,h\n call wrt\n getthis 0\n \n ex de,hl\n getfld .x\n ld a,35\n call map.setat.a\n getthis 0\n ld hl,true\n jpf lb24\n ld hl, 4104\n call getkey\n jpf lb26\n ;x-=3;\n ld hl,3\n push hl\n getfld .x\n pop de\n subhl de\n setfld .x\n jp lb25\n lb26:\n lb25:\n ld hl, 32776\n call getkey\n jpf lb28\n ;x+=3;\n ld hl,3\n push hl\n getfld .x\n pop de\n add hl, de\n setfld .x\n jp lb27\n lb28:\n lb27:\n ld hl, 8200\n call getkey\n jpf lb30\n ;y-=3;\n ld hl,3\n push hl\n getfld .y\n pop de\n subhl de\n setfld .y\n jp lb29\n lb30:\n lb29:\n ld hl, 16392\n call getkey\n jpf lb32\n ;y+=3;\n ld hl,3\n push hl\n getfld .y\n pop de\n add hl, de\n setfld .y\n jp lb31\n lb32:\n lb31:\n crashToClass Enemy, 5, 15\n jpf lb34\n ;die();\n invoke .die\n jp lb33\n lb34:\n lb33:\n crashToClass EBullet, 0, 5\n jpf lb36\n ;die();\n invoke .die\n jp lb35\n lb36:\n lb35:\n ld hl,1\n push hl\n ld hl, 264\n call getkey\n pop de\n call hleqde\n jpf lb38\n ;new PBullet{x,y};\n getfld .x\n new.arg .x\n getfld .y\n new.arg .y\n new PBullet,2,15,20\n foreach.a Enemy,5,14,nxx\n  ld a,h\n  ld hl,8\n  setfld .c\n  continue \n nxx:\n getthis 0\n \n \n jp lb37\n lb38:\n lb37:\n ;update();\n invoke .update\n jp lb23\n lb24:\n ret\nenddef 0\nendusr:\ngbl_player:dw 0\ngbl_pat_spr:dw 0\nspr.inipat:\n ld de,3800h\n ld hl,spr.pat\n ld bc,2048\n jp LDIRVM\nspr.pat:\ndb $c,$e,$f,$4f,$3d,$1d,$7f,$1b,$c,$3f,$7f,$7f,$6f,$f,$6,$c\ndb $18,$38,$f8,$f9,$de,$dc,$7f,$6c,$98,$fc,$fe,$fe,$f6,$f0,$60,$70\ndb $c,$e,$f,$4f,$3d,$1d,$7f,$1b,$c,$3f,$7f,$7f,$ef,$ef,$6,$6\ndb $18,$38,$f8,$f9,$de,$dc,$7f,$6c,$98,$fc,$fe,$fe,$d4,$78,$f0,$0\ndb $18,$1c,$1f,$9f,$7b,$3b,$fe,$36,$19,$3f,$7f,$7f,$2b,$1e,$f,$0\ndb $30,$70,$f0,$f2,$bc,$b8,$fe,$d8,$30,$fc,$fe,$fe,$f7,$f7,$60,$60\ndb $2c,$8e,$f,$4b,$3d,$11,$7f,$1d,$ca,$ff,$7f,$3f,$15,$1f,$e,$0\ndb $1c,$39,$f8,$e9,$de,$c4,$7f,$5c,$ab,$ff,$ff,$fe,$ac,$f8,$70,$0\ndb $1,$1,$1,$2,$3,$92,$96,$f6,$f6,$fe,$fe,$fe,$ff,$af,$26,$0\ndb $80,$80,$80,$40,$c0,$49,$69,$6f,$6f,$7f,$7f,$7f,$ff,$f5,$64,$0\ndb $8,$10,$30,$30,$18,$f,$3f,$37,$7b,$7d,$3f,$1f,$37,$60,$70,$c\ndb $8,$4,$6,$6,$c,$f8,$fe,$f6,$ef,$df,$fe,$fc,$f6,$3,$7,$18\ndb $0,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$0,$0\ndb $80,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$80,$0\ndb $0,$0,$0,$0,$3,$f,$f,$1f,$1f,$1f,$1f,$f,$f,$3,$0,$0\ndb $0,$0,$0,$0,$c0,$f0,$f0,$f8,$f8,$f8,$f8,$f0,$f0,$c0,$0,$0\nbg.inipat:\n ret\n\n\nend main'].join(''),'t3': ['org 09000h\n\ninclude tnu\ninclude mus\ninclude sprpat\n\n;===your code \n\nmain:\nld sp,(8000h)\n\ncall screen1\n\nshowsp\nshowlb endusr\n\nld hl,8000h\nld (hl),0cdh\nld de,8001h\nld bc,th.size*th.count-1\nldir\n\n\ncall th.init\ncall spr.inipat\n;call mus.ini\n\n\nnew Main, 0\nshow hl\n\n\nmovw (h.thlop),spr.puts\njp th.loop\n\nclass Main, RActor\n meth Main,main\n fld.bottom Object\n fld .x,100\n fld .y,300\n fld .p,0\n fld .c,3\n fld.bottom Sprite\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut \n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld.bottom RActor\n fld.bottom Main\n end.const\n\nMain.main:\n olp:\n  getthis\n  invoke .update\n  call xrnd.a\n  ld a,h\n  and 15\n  jr nz,doap\n   getthis\n   getfld .x\n   new.arg .x\n   getfld .y\n   new.arg .y\n   ld hl,7\n   call rnd.a\n   ld de,3\n   sbc hl,de\n   new.arg .vx\n   ld hl,5\n   call rnd.a\n   ld de,15\n   sbc hl,de\n   new.arg .vy\n   new Bullet, 4\n   call dstk\n  doap:\n  ld a,8\n  call SNSMAT.a\n  and 1\n  jr z,golf\n  \n  getthis\n  getfld .x\n  inc hl\n  inc hl\n  setfld .x\n  ld de,400\n  cpde.a\n  jp c, olp\n  golf:\n  ld hl,0\n  getthis\n  setfld .x\n jp olp\n\n\nclass Bullet,RActor\n meth Bullet,main\n fld.bottom Object\n fld .x, 0\n fld .y, 0\n fld .p, 2\n fld .c, 15\n fld.bottom Sprite\n meth RActor,onUpdate\n meth RActor,update\n meth RActor,screenOut \n meth RActor,die\n meth RActor,updateEx\n meth RActor,crashTo\n fld.bottom RActor\n fld .vy, -10\n fld .vx, 0\n fld.bottom Bullet\n end.const \n \nBullet.main:\n blp:\n  getthis\n  ;x+=vx\n  getfld .x\n  ex de, hl\n  getfld .vx\n  add hl,de\n  setfld .x\n  ; y+=vy\n  getfld .y\n  ex de, hl\n  getfld .vy\n  add hl,de\n  setfld .y\n  getfld .vy\n  inc hl\n  setfld .vy\n\n  invoke .update\n  invoke .screenOut\n  jp c, bdie\n  getfld .vy\n  bit 7,h\n  jr nz,blp\n  ld de,5\n  cpde.a\n  jr c,blp\n \n  call dstk\n  getthis\n  ld hl,3\n  setfld .p\n  pushi 10,bc\n  invoke .updateEx\n\n bleft:\n  getthis\n  ld hl,2\n  setfld .p\n  getfld .x\n  dec hl\n  dec hl\n  setfld .x\n  getfld .y\n  dec hl\n  setfld .y\n  invoke .update\n  invoke .screenOut\n  jr c, bdie\n  jr bleft\n bdie:\n  invoke .die\n  ret \n\n  \ndstk:\n push af\n ld hl,th.start+256*3\n getthis\n ld h,a\n ld de,1900h\n ld bc,256\n call LDIRVM\n pop af\n ret\n \nendusr:\nend main'].join(''),'t4': ['org 09000h\njp main\ninclude const\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude tnu\ninclude sp\n\n;===your code \n\nright:dw 0\n\nmain:\ntnu.run Main\ndef Main.main,0,0\nnew.arg .vx,1\nnew.arg .vy,0\nnew.arg .x,0\nnew.arg .y,100\nnew Cat,4\n\nnew.arg .x,100\nnew.arg .y,100\nnew Target,2\n\nnew.arg .x,200\nnew.arg .y,100\nnew Target,2\n\n\nnew.arg .x,150\nnew.arg .y,100\nnew.arg .c,8\nnew NTarget,3\n\nld (right),hl\nld a,h\nld de,Actor\ncall instanceof\ncall showz\n\nld a,(right+1)\nld de,Target\ncall instanceof\ncall showz\n\nld a,(right+1)\nld de,Cat\ncall instanceof\ncall showz\n\n\nld hl,1\nsetfld .c\nenddef 0\n\nclass Main,Actor\n Actor.noovr Main\n end.const 0\nclass Target,Actor\n Actor.noovr Target\n met2 Target,.push\n end.const 0\nclass NTarget,Actor\n Actor.noovr NTarget\n end.const 0\ndef NTarget.main,0,0\n ret\nenddef\n\ndef Target.main,0,0\nenddef\nclass Cat,Actor\n Actor.noovr Cat\n fld .vy, 0\n fld .vx, 0\n fld.bottom Cat\n end.const 0\ndef Cat.main,0,0\n blp:\n  getthis\n  ;x+=vx\n  getfld .x\n  ex de, hl\n  getfld .vx\n  add hl,de\n  setfld .x\n  ; y+=vy\n  getfld .y\n  ex de, hl\n  getfld .vy\n  add hl,de\n  setfld .y\n  ld hl,Target\n  push hl\n  invoke .crashTo\n  jpf cr\n   ; r.x+=10\n   const setg,hl\n   getfldtg .y\n   ld de,30\n   add hl,de\n   push hl\n   ldconst hl,setg\n   setfldtg .y\n  cr:\n  invoke .update\n jp blp\nenddef\ndef Target.push,0,0\n ld hl,3\n setfld .p\n repti 30,pse\n  getfld .y\n  inc hl\n  setfld .y\n  invoke .update\n  continue\n pse:\n ld hl,0\n setfld .p\nenddef\n\nendusr: \nend main\nhttps://msxpen.com/codes/-N6DDfMvZq9aUeJ9JLpN\nhttps://msxpen.com/codes/-N6QGYk-rr5iDuTtHpF7'].join(''),'t5': ['org 9000h\n\n\ninclude key\n\nmain:\ncall keyall\nld hl,0108h\ncall getkey\nshow hl\nld hl,0107h\ncall getkey\nshow hl\n\n\nhalt\njp main'].join(''),'gen': ['org 09000h\ninclude tnu\ninclude bool\n\nmain:\ntnu.run Main\nclass Main,Actor\n Actor.noovr Main\n end.const 0\ndef Main.main,0,0\n\n showlb .main\n showlb .crashTo\nenddef 0\nendusr:\nend main'].join(''),'dac': ['org 09000h\njp main\ninclude const\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude th\n\n\nDECSUB equ 268CH;DAC ← DAC-ARG\nDECADD equ 269AH;DAC ← DAC+ARG\nDECNRM equ 26FAH;DAC を正規化する (*1)\nDECROU equ 273CH;DAC を四捨五入する\nDECMUL equ 27E6H;DAC ← DAC*DAC\nDECDIV equ 289FH;DAC ← DAC/DAC\nMAF equ 2C4DH;ARG ← DAC\nMAM equ 2C50H;ARG ← [HL]\nMOV8DH equ 2C53H;[DE] ← [HL]\nMFA equ 2C59H;DAC ← ARG\nMFM equ 2C5CH;[HL] ← DAC\nMMF equ 2C67H;[HL] ← DAC\nMOV8HD equ 2C6AH;[HL] ← [DE]\nXTF equ 2C6FH;[SP] ←→ DAC\nPHA equ 2CC7H;ARG → [SP]\nPHF equ 2CCCH;DAC → [SP]\nPPA equ 2CDCH;[SP] → ARG\nPPF equ 2CE1H;[SP] → DAC\nPUSHF equ 2EB1H;DAC → [SP]\nMOVFM equ 2EBEH;DAC ← [HL]\nMOVFR equ 2EC1H;DAC ← (CBED)\nMOVRF equ 2ECCH;(CBED) ← DAC\nMOVRMI equ 2ED6H;(CBDE) ← [HL]\nMOVRM equ 2EDFH;(BCDE) ← [HL]\nMOVMF equ 2EE8H;[HL] ← DAC\nMOVE equ 2EEBH;[HL] ← [DE]\nVMOVAM equ 2EEFH;ARG ← [HL]\nMOVVFM equ 2EF2H;[DE] ← [HL]\nVMOVE equ 2EF3H;[HL] ← [DE]\nVMOVFA equ 2F05H;DAC ← ARG\nVMOVFM equ 2F08H;DAC ← [HL]\nVMOVAF equ 2F0DH;ARG ← DAC\nVMOVMF equ 2F10H;[HL] ← DAC\n\nVALTYP equ 0F663H;1\nDAC equ 0F7F6H;16\nARG equ 0F847H;16\nFOUT equ 3425H\n\ndefsub int2dac\n push af\n ld a,2\n ld (VALTYP),a\n ld (DAC+2),HL\n pop af\nendsub int2dac\n;===your code \n\nmain:\nld hl,12345\ncall int2dac\nld hl,str\ncall FOUT\n\nld b,10\nreptb nxt\n ld a,(hl)\n cp 0\n break z\n call wrt2\n inc hl\n continue\nnxt:\nret\nstr:\n\n\n'].join(''),'setvrm': ['org 09000h\njp main\ninclude const\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude th\n\n;===your code \n\nmain:\nld hl,1800h\ncall SETWRT\nld a,35\nrepti 5, ed\ninc a\nout (98h),a\ncontinue\ned:\nret'].join(''),'assert': ['include mem\ninclude math\ninclude debug\n\na.reg.trc:\ndw 0\na.reg.adr:\ndw 0\na.reg.min:\ndw 0\na.reg.val:\ndw 0\na.reg.max:\ndw 0\nmacro a.regi,n,v\n push hl\n ld hl,v\n ld (a.reg.##n),hl\n pop hl\nendm\nmacro a.regr,n,v\n ld (a.reg.##n),v\nendm\n\nmacro a.dummy\n local a.reg.,trc,adr,min,val,nax\nendm\n \n\nmacro assert.eq,o\n storelastpc\n pushall \n if not nul o\n  a.regi val, o\n endif\n ld de,(a.reg.val)\n ld(a.reg.val),hl\n ld(a.reg.min),de\n ld(a.reg.max),de\n cpde\n jp nz,assert.fail\n popall\nendm\n\nmacro assert.do,nx\n storelastpc\n pushall\n call to\n popall\n jr nx\n to:\nendm\n\nmacro storelastpc\n push hl\n call getpc\n ld (lastpc),hl\n pop hl\nendm\nlastpc:\n dw 0\n \ngetpc:\n pop hl\n push hl\n ret\n\nassert.fail:\n ld hl,0deadh\n show hl\n showm a.reg.trc\n showm a.reg.min\n showm a.reg.val\n showm a.reg.max\n showm a.reg.adr\n showm lastpc\n call freeze\nmacro assert.meqw,ad,val\n a.regi adr,ad\n push hl\n ld hl,(ad)\n assert.eq val\n pop hl\nendm\n '].join(''),'stksz': ['org 09000h\njp main\ninclude const\ninclude ctrl\ninclude math\ninclude debug\ninclude sub\ninclude mem\ninclude th\n\n\n;===your code \n\nsz equ 256\n  \nmain:\nld hl,0fd9fh\nld (hl),0c9h\n rept sz/2\n  push hl\n endm\n rept sz/2\n  pop hl\n endm\n\nloop:\n getsp\n ld de,-sz\n add hl,de\n ld de,1800h\n ld bc,sz\n call LDIRVM\n ld hl,0\n halt\n jp loop\n \n \n '].join(''),'vdp': [';https://www.msx.org/wiki/VDP_Status_Registers\n;st 0 bit 7\n;read 1\n\n;https://www.msx.org/wiki/VDP_Mode_Registers\n;ctrl 1 bit 5 set 0\ninclude const\n\nsusint:\n ld a,(RG1SAV)\n res 5,a\n ld b,A\n ld c,1\n jp WRTVDP\n;rstint:\n ld a,(RG1SAV)\n set 5,a\n ld b,A\n ld c,1\n jp WRTVDP\ninted:\n call RDVDP\n bit 7,a\n ret\ndoint:\n call inted\n jr z, norst\n ld hl,timecnt\n inc (hl)\n call h.tntimi\n norst:\n ;call rstint\n ret\nh.tntimi:\n ld A,(timecnt)\n ld hl,1ae0h\n call 4dh\n ret\n ds 16\n \ntimecnt:\ndb 0\nmacro vdptest\nlocal stk1,stk2,stk3,vl\nstk1:\n ds 256,35\nstk2:\n ds 256,42\nstk3:\n\nvl:\n call susint\n ld sp,stk2\n ld hl,stk1\n ld de,1800h\n ld bc,256\n call LDIRVM\n \n \n ld sp,stk3\n call doint\n ld hl,stk2\n ld de,1900h\n ld bc,256\n call LDIRVM\n jp vl\nendm\n \nscreen1:\n ld a,1\n call CHGMOD\n ld a,(RG1SAV)\n set 1,a\n ld b,A\n ld c,1\n call WRTVDP\n ret\n\ndefsub screen2\n ld a,2\n call CHGMOD\n ld a,(RG1SAV)\n set 1,a\n ld b,A\n ld c,1\n call WRTVDP\nendsub screen2\n '].join(''),'mus': ['include mem\nmus.ini:\n di\n ld hl,0fd9fh\n ld (hl),0c3h\n movw (0fd9fh+1),mus\n ei\n ret\nmus:\nproc\nlocal we\n push af\n push de\n ld a,(we-1)\n xor 15\n ld (we-1),a \n ld a,8\n ld e,15\n we:\n call WRTPSG\n pop af \n pop de\n ret\nendp'].join('')};
        
        
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
        
        Tonyu.globals.$config={"spr.scale": 1,"obj.limits": {PBullet: 12,Player: 4},"th.count": 20,"defdbl": ["dbl","$dbl",/^f_/,/^\$f_/],"arrays": {"$mem": 50,"$ary": [1,10,100],"$mat": [[1,3,5],[10,12]],"$f_ary": [1.2,2.3,4.5]},"strings": {"$mesg": "Hello, world","$mesgs": ["Hello","World!!"]}};
      },
      fiber$config :function* _trc_MBoot_f_config(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        Tonyu.globals.$config={"spr.scale": 1,"obj.limits": {PBullet: 12,Player: 4},"th.count": 20,"defdbl": ["dbl","$dbl",/^f_/,/^\$f_/],"arrays": {"$mem": 50,"$ary": [1,10,100],"$mat": [[1,3,5],[10,12]],"$f_ary": [1.2,2.3,4.5]},"strings": {"$mesg": "Hello, world","$mesgs": ["Hello","World!!"]}};
        
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
          new Tonyu.classes.kernel.Button({top: 480,text: "Edit Page",height: 30,fillStyle: Tonyu.globals.$RSprPat.palette[4],onClick: (function anonymous_2115() {
            
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
          new Tonyu.classes.kernel.Button({top: 480,text: "Edit Page",height: 30,fillStyle: Tonyu.globals.$RSprPat.palette[4],onClick: (function anonymous_2115() {
            
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
              _this.printf("unuse 0%n");
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
              (yield* _this.fiber$printf(_thread, "unuse 0%n"));
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
        
        _this.printf(['org 08000h\ninclude tnu\ninclude map\ninclude bool\n\nmain:\ntnu.run ',Tonyu.globals.$mainClassName,'\n'].join(''));
        
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
        _this.printf(['\nendusr:\n',Object.keys(_this.globals).map((function anonymous_1733(k) {
          
          return _this.globalLabel(k)+":dw 0";
        })).join("\n"),'\nmacro inivrm, n, dst\n ld de,dst\n ld hl,n\n ld bc,end.##n-n\n call LDIRVM\nendm\n\nspr.inipat:\n inivrm spr.pat, 3800h\n ret\n\nspr.pat:\n',_this.outp.buf,'\nend.spr.pat:\n\nbg.inipat:\n inivrm bg.gen, 0000h\n inivrm bg.gen, 0800h\n inivrm bg.gen, 1000h\n inivrm bg.col, 2000h\n inivrm bg.col, 2800h\n inivrm bg.col, 3000h\n inivrm bg.name, 1800h\n ret\n\nbg.gen:\n',_this.toDB(_this.outbg.patgen),'\nend.bg.gen:\n\nbg.col:\n',_this.toDB(_this.outbg.coltbl),'\nend.bg.col:\n\nbg.name:\n ds 256*3,32\nend.bg.name:\n\nend main'].join(''));
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
        
        (yield* _this.fiber$printf(_thread, ['org 08000h\ninclude tnu\ninclude map\ninclude bool\n\nmain:\ntnu.run ',Tonyu.globals.$mainClassName,'\n'].join('')));
        
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
        (yield* _this.fiber$printf(_thread, ['\nendusr:\n',Object.keys(_this.globals).map((function anonymous_1733(k) {
          
          return _this.globalLabel(k)+":dw 0";
        })).join("\n"),'\nmacro inivrm, n, dst\n ld de,dst\n ld hl,n\n ld bc,end.##n-n\n call LDIRVM\nendm\n\nspr.inipat:\n inivrm spr.pat, 3800h\n ret\n\nspr.pat:\n',_this.outp.buf,'\nend.spr.pat:\n\nbg.inipat:\n inivrm bg.gen, 0000h\n inivrm bg.gen, 0800h\n inivrm bg.gen, 1000h\n inivrm bg.col, 2000h\n inivrm bg.col, 2800h\n inivrm bg.col, 3000h\n inivrm bg.name, 1800h\n ret\n\nbg.gen:\n',_this.toDB(_this.outbg.patgen),'\nend.bg.gen:\n\nbg.col:\n',_this.toDB(_this.outbg.coltbl),'\nend.bg.col:\n\nbg.name:\n ds 256*3,32\nend.bg.name:\n\nend main'].join('')));
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
          let tx = new Tonyu.classes.kernel.HTMLUI({content: ["div",{style: 'background: #fee;'},["h2","Problem(s) found"],["ul"].concat(_this.problems.map((function anonymous_3143(p) {
            
            return ["li",["a",{href: "javascript:;",onclick: (function anonymous_3256() {
              
              _this.ide.jump(p.file,p.row,p.col);
            })},p.file.name(),":",p.row,":",p.col," - ",p.mesg]];
          }))),["button",{onclick: (function anonymous_3488() {
            
            tx.die();
          })},"Close"]],left: 10,top: 20,width: 300,height: 400});
          
          return _this;
          
        }
        let tx = new Tonyu.classes.kernel.HTMLUI({content: ["div",{style: 'background: #eee;'},["h2","Code copied!"],["ul",["li","Open ",["a",{target: "pen",href: _this.url},"this MSXpen page "],"."],["li","Paste the copied code"," to 'Asm' tab."]],["textarea",{rows: "10",cols: "30",name: "val"},"test\ndesu"],["button",{onclick: (function anonymous_4040() {
          
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
          let tx = new Tonyu.classes.kernel.HTMLUI({content: ["div",{style: 'background: #fee;'},["h2","Problem(s) found"],["ul"].concat(_this.problems.map((function anonymous_3143(p) {
            
            return ["li",["a",{href: "javascript:;",onclick: (function anonymous_3256() {
              
              _this.ide.jump(p.file,p.row,p.col);
            })},p.file.name(),":",p.row,":",p.col," - ",p.mesg]];
          }))),["button",{onclick: (function anonymous_3488() {
            
            tx.die();
          })},"Close"]],left: 10,top: 20,width: 300,height: 400});
          
          return _this;
          
        }
        let tx = new Tonyu.classes.kernel.HTMLUI({content: ["div",{style: 'background: #eee;'},["h2","Code copied!"],["ul",["li","Open ",["a",{target: "pen",href: _this.url},"this MSXpen page "],"."],["li","Paste the copied code"," to 'Asm' tab."]],["textarea",{rows: "10",cols: "30",name: "val"},"test\ndesu"],["button",{onclick: (function anonymous_4040() {
          
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
              
            } else {
              if (op.text==="++"||op.text==="--") {
                _this.visit(left);
                _this.printf(op.text==="++"?"inc hl%n":"dec hl%n");
                _this.enter({lval: true},(function anonymous_7185() {
                  
                  _this.visit(left);
                }));
                return _this;
                
              }
            }
          }
          
        } else {
          let tgme = _this.isMemberRef(n);
          
          if (tgme) {
            _this.printf("push hl%n");
            _this.enter({lval: false},(function anonymous_7391() {
              
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
              
            } else {
              if (op.text==="++"||op.text==="--") {
                (yield* _this.fiber$visit(_thread, left));
                (yield* _this.fiber$printf(_thread, op.text==="++"?"inc hl%n":"dec hl%n"));
                (yield* _this.fiber$enter(_thread, {lval: true}, (function anonymous_7185() {
                  
                  _this.visit(left);
                })));
                return _this;
                
              }
            }
          }
          
        } else {
          let tgme=yield* _this.fiber$isMemberRef(_thread, n);
          
          if (tgme) {
            (yield* _this.fiber$printf(_thread, "push hl%n"));
            (yield* _this.fiber$enter(_thread, {lval: false}, (function anonymous_7391() {
              
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
        
        if (name.text==="crashTo") {
          _this.unsup(name,"target.crashTo( ) is not supported");
          return _this;
          
        }
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
        
        if (name.text==="crashTo") {
          (yield* _this.fiber$unsup(_thread, name, "target.crashTo( ) is not supported"));
          return _this;
          
        }
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
                      _this.printf("call crashTo.setXY%n");
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
                      (yield* _this.fiber$printf(_thread, "call crashTo.setXY%n"));
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
        _this.enter({lval: true},(function anonymous_12437() {
          
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
        (yield* _this.fiber$enter(_thread, {lval: true}, (function anonymous_12437() {
          
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
        _this.enter({lval: true},(function anonymous_13252() {
          
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
        (yield* _this.fiber$enter(_thread, {lval: true}, (function anonymous_13252() {
          
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
        _this.enter({closestBrk: se},(function anonymous_14839() {
          
          _this.visit(loop);
        }));
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
        (yield* _this.fiber$enter(_thread, {closestBrk: se}, (function anonymous_14839() {
          
          _this.visit(loop);
        })));
        (yield* _this.fiber$printf(_thread, "jp %s%n", sh));
        (yield* _this.fiber$printf(_thread, "%s:%n", se));
        
      },
      v_break :function _trc_GenAsm_v_break(n) {
        "use strict";
        var _this=this;
        
        if (! _this.closestBrk) {
          _this.unsup(n,"break outside loop ");
          
        }
        if (_this.closestBrk==="BREAK") {
          _this.printf("break%n");
          
        } else {
          _this.printf("jp %s%n",_this.closestBrk);
          
        }
      },
      fiber$v_break :function* _trc_GenAsm_f_v_break(_thread,n) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        if (! _this.closestBrk) {
          (yield* _this.fiber$unsup(_thread, n, "break outside loop "));
          
        }
        if (_this.closestBrk==="BREAK") {
          (yield* _this.fiber$printf(_thread, "break%n"));
          
        } else {
          (yield* _this.fiber$printf(_thread, "jp %s%n", _this.closestBrk));
          
        }
        
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
          _this.enter({closestBrk: se},(function anonymous_15551() {
            
            _this.visit(loop);
          }));
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
          
          _this.printf("foreach.a %s, %d, %d, %s%{",klass.shortName,range[0],range[1],nx);
          let va = _this.annotation(vars[0]);
          
          if (va.scopeInfo.type==="field") {
            _this.printf("setfld .%s%n",vars[0].text);
            
          } else {
            _this.unsup(vars,"only field supports");
            
          }
          _this.enter({closestBrk: "BREAK"},(function anonymous_16432() {
            
            _this.visit(loop);
          }));
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
          (yield* _this.fiber$enter(_thread, {closestBrk: se}, (function anonymous_15551() {
            
            _this.visit(loop);
          })));
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
          
          (yield* _this.fiber$printf(_thread, "foreach.a %s, %d, %d, %s%{", klass.shortName, range[0], range[1], nx));
          let va=yield* _this.fiber$annotation(_thread, vars[0]);
          
          if (va.scopeInfo.type==="field") {
            (yield* _this.fiber$printf(_thread, "setfld .%s%n", vars[0].text));
            
          } else {
            (yield* _this.fiber$unsup(_thread, vars, "only field supports"));
            
          }
          (yield* _this.fiber$enter(_thread, {closestBrk: "BREAK"}, (function anonymous_16432() {
            
            _this.visit(loop);
          })));
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
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}},"annotation":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"toDB":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"showDiag":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"def":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"unsup":{"nowait":false,"isMain":false,"vtype":{"params":["user.TNode","String"],"returnValue":null}},"v_program":{"nowait":false,"isMain":false,"vtype":{"params":["user.Program"],"returnValue":null}},"v_parenExpr":{"nowait":false,"isMain":false,"vtype":{"params":["user.ParenExpr"],"returnValue":null}},"extractSrc":{"nowait":false,"isMain":false,"vtype":{"params":["user.TNode"],"returnValue":null}},"v_exprstmt":{"nowait":false,"isMain":false,"vtype":{"params":["user.Exprstmt"],"returnValue":null}},"v_infix":{"nowait":false,"isMain":false,"vtype":{"params":["user.Infix"],"returnValue":null}},"andand":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression","user.Expression"],"returnValue":null}},"oror":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression","user.Expression"],"returnValue":null}},"isMemberRef":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression"],"returnValue":null}},"v_postfix":{"nowait":false,"isMain":false,"vtype":{"params":["user.Postfix"],"returnValue":null}},"tgMeth":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression","user.Token","user.Call"],"returnValue":null}},"myMeth":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarAccess","user.Call"],"returnValue":null}},"cmp":{"nowait":false,"isMain":false,"vtype":{"params":["user.Infix"],"returnValue":null}},"arith":{"nowait":false,"isMain":false,"vtype":{"params":["user.Infix"],"returnValue":null}},"arith2":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression","String","user.Expression"],"returnValue":null}},"arithEq":{"nowait":false,"isMain":false,"vtype":{"params":["user.Infix"],"returnValue":null}},"assign":{"nowait":false,"isMain":false,"vtype":{"params":["user.Infix"],"returnValue":null}},"v_reservedConst":{"nowait":false,"isMain":false,"vtype":{"params":["user.Token"],"returnValue":null}},"v_number":{"nowait":false,"isMain":false,"vtype":{"params":["user.Token"],"returnValue":null}},"globalLabel":{"nowait":false,"isMain":false,"vtype":{"params":["String"],"returnValue":null}},"v_varAccess":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarAccess"],"returnValue":null}},"genSym":{"nowait":false,"isMain":false,"vtype":{"params":[],"returnValue":null}},"v_compound":{"nowait":false,"isMain":false,"vtype":{"params":["user.Compound"],"returnValue":null}},"v_while":{"nowait":false,"isMain":false,"vtype":{"params":["user.While"],"returnValue":null}},"v_break":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"v_for":{"nowait":false,"isMain":false,"vtype":{"params":["user.For"],"returnValue":null}},"isAll":{"nowait":false,"isMain":false,"vtype":{"params":["user.Expression"],"returnValue":null}},"v_if":{"nowait":false,"isMain":false,"vtype":{"params":["user.If"],"returnValue":null}},"v_varDecl":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarDecl"],"returnValue":null}},"v_varsDecl":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarsDecl"],"returnValue":null}},"rangeToAd":{"nowait":false,"isMain":false,"vtype":{"params":[null],"returnValue":null}},"isClassConst":{"nowait":false,"isMain":false,"vtype":{"params":["user.VarAccess"],"returnValue":null}},"v_newExpr":{"nowait":false,"isMain":false,"vtype":{"params":["user.NewExpr"],"returnValue":null}}},"fields":{"IDEPrj":{},"anodes":{},"mem":{"vtype":"user.MemberScan"},"lval":{},"symSeq":{"vtype":"Number"},"ide":{},"klass":{},"klassSrc":{},"problems":{},"builtins":{},"globals":{},"keynames":{},"closestBrk":{},"opmap":{},"anode":{},"outp":{"vtype":"user.OutPat"},"outbg":{"vtype":"user.OutBG"},"url":{"vtype":"String"}}}
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
        
        new Tonyu.classes.user.Player({x: 32,y: 32,p: 16,c: 8,pc: 3});
        new Tonyu.classes.user.Player({x: 480,y: 300,p: 17,c: 7,pc: 4});
        new Tonyu.classes.user.Player({x: 32,y: 32,p: 18,c: 3,pc: 5});
        new Tonyu.classes.user.Player({x: 480,y: 300,p: 19,c: 10,pc: 6});
        _this.p=0;
        _this.c=15;
        while (true) {
          Tonyu.checkLoop();
          if (_this.rnd(40)==0) {
            _this.x=_this.rnd(32);
            _this.y=_this.rnd(24);
            _this.c=_this.map_get(_this.x,_this.y);
            if (_this.c>=3&&_this.c<=6) {
              new Tonyu.classes.user.PBullet({x: _this.x*16+8,y: _this.y*16+8,pc: _this.c});
              
            }
            
          }
          _this.update();
          
        }
      },
      fiber$main :function* _trc_Main_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        new Tonyu.classes.user.Player({x: 32,y: 32,p: 16,c: 8,pc: 3});
        new Tonyu.classes.user.Player({x: 480,y: 300,p: 17,c: 7,pc: 4});
        new Tonyu.classes.user.Player({x: 32,y: 32,p: 18,c: 3,pc: 5});
        new Tonyu.classes.user.Player({x: 480,y: 300,p: 19,c: 10,pc: 6});
        _this.p=0;
        _this.c=15;
        while (true) {
          yield null;
          if (_this.rnd(40)==0) {
            _this.x=_this.rnd(32);
            _this.y=_this.rnd(24);
            _this.c=(yield* _this.fiber$map_get(_thread, _this.x, _this.y));
            if (_this.c>=3&&_this.c<=6) {
              new Tonyu.classes.user.PBullet({x: _this.x*16+8,y: _this.y*16+8,pc: _this.c});
              
            }
            
          }
          (yield* _this.fiber$update(_thread));
          
        }
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{}}
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
        
        _this.p=7;
        _this.vx = 0;
        
        _this.vy = 0;
        
        
        if (_this.pc==3) {
          _this.bc=8;
        }
        if (_this.pc==4) {
          _this.bc=7;
        }
        if (_this.pc==5) {
          _this.bc=3;
        }
        if (_this.pc==6) {
          _this.bc=10;
        }
        for ([_this.target] of Tonyu.iterator2(_this.all(Tonyu.classes.user.Player),1)) {
          if (_this.target.pc==_this.pc) {
            break;
            
          }
          
        }
        _this.i = 0;
        
        while (true) {
          Tonyu.checkLoop();
          if (_this.i%20==0) {
            _this.c=15;
          } else {
            _this.c=_this.bc;
          }
          if (_this.crashTo(_this.target)) {
            break;
            
          }
          _this.i++;
          _this.update();
          
        }
        _this.i=0-1;
        while (true) {
          Tonyu.checkLoop();
          _this.map_setAt(_this.x,_this.y,_this.pc);
          if (_this.target.defeatBy) {
            _this.die();
            
          }
          _this.foe = _this.crashTo(Tonyu.classes.user.Player);
          
          if (_this.foe&&_this.foe.pc!=_this.pc) {
            _this.foe.defeatBy=_this.pc;
            _this.die();
            
          }
          if (_this.screenOut()) {
            _this.die();
          }
          if (_this.target.x>_this.x) {
            _this.vx+=1;
          }
          if (_this.target.x<_this.x) {
            _this.vx-=1;
          }
          if (_this.target.y>_this.y) {
            _this.vy+=1;
          }
          if (_this.target.y<_this.y) {
            _this.vy-=1;
          }
          _this.x+=_this.vx;
          _this.y+=_this.vy;
          _this.update();
          
        }
      },
      fiber$main :function* _trc_PBullet_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.p=7;
        _this.vx = 0;
        
        _this.vy = 0;
        
        
        if (_this.pc==3) {
          _this.bc=8;
        }
        if (_this.pc==4) {
          _this.bc=7;
        }
        if (_this.pc==5) {
          _this.bc=3;
        }
        if (_this.pc==6) {
          _this.bc=10;
        }
        for ([_this.target] of Tonyu.iterator2(_this.all(Tonyu.classes.user.Player),1)) {
          if (_this.target.pc==_this.pc) {
            break;
            
          }
          
        }
        _this.i = 0;
        
        while (true) {
          yield null;
          if (_this.i%20==0) {
            _this.c=15;
          } else {
            _this.c=_this.bc;
          }
          if (_this.crashTo(_this.target)) {
            break;
            
          }
          _this.i++;
          (yield* _this.fiber$update(_thread));
          
        }
        _this.i=0-1;
        while (true) {
          yield null;
          (yield* _this.fiber$map_setAt(_thread, _this.x, _this.y, _this.pc));
          if (_this.target.defeatBy) {
            _this.die();
            
          }
          _this.foe = _this.crashTo(Tonyu.classes.user.Player);
          
          if (_this.foe&&_this.foe.pc!=_this.pc) {
            _this.foe.defeatBy=_this.pc;
            _this.die();
            
          }
          if (_this.screenOut()) {
            _this.die();
          }
          if (_this.target.x>_this.x) {
            _this.vx+=1;
          }
          if (_this.target.x<_this.x) {
            _this.vx-=1;
          }
          if (_this.target.y>_this.y) {
            _this.vy+=1;
          }
          if (_this.target.y<_this.y) {
            _this.vy-=1;
          }
          _this.x+=_this.vx;
          _this.y+=_this.vy;
          (yield* _this.fiber$update(_thread));
          
        }
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"vx":{"vtype":"Number"},"vy":{"vtype":"Number"},"pc":{},"bc":{},"target":{},"i":{"vtype":"Number"},"foe":{}}}
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
        
        _this.tx = _this.rnd(512);
        
        _this.ty = _this.rnd(384);
        
        
        while (true) {
          Tonyu.checkLoop();
          _this.map_setAt(_this.x,_this.y,_this.pc);
          if (_this.c==8) {
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
            
          } else {
            if (_this.x>_this.tx) {
              _this.x-=3;
            }
            if (_this.x<_this.tx) {
              _this.x+=3;
            }
            if (_this.y>_this.ty) {
              _this.y-=3;
            }
            if (_this.y<_this.ty) {
              _this.y+=3;
            }
            if (_this.rnd(20)==0) {
              _this.tx=_this.rnd(512);
              _this.ty=_this.rnd(384);
              for ([_this.s] of Tonyu.iterator2(_this.all(Tonyu.classes.user.PBullet),1)) {
                if (_this.s.pc==_this.pc&&_this.s.i>=0) {
                  _this.tx=_this.s.x;
                  _this.ty=_this.s.y;
                  
                }
                
              }
              
            }
            
          }
          
          if (_this.defeatBy) {
            _this.i = 0;
            
            while (_this.i<120) {
              Tonyu.checkLoop();
              _this.x+=8;
              _this.update();
              _this.x-=8;
              _this.update();
              _this.i+=1;
              
            }
            _this.defeatBy=0;
            _this.i=0;
            
          }
          _this.update();
          
        }
      },
      fiber$main :function* _trc_Player_f_main(_thread) {
        "use strict";
        var _this=this;
        //var _arguments=Tonyu.A(arguments);
        
        _this.tx = _this.rnd(512);
        
        _this.ty = _this.rnd(384);
        
        
        while (true) {
          yield null;
          (yield* _this.fiber$map_setAt(_thread, _this.x, _this.y, _this.pc));
          if (_this.c==8) {
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
            
          } else {
            if (_this.x>_this.tx) {
              _this.x-=3;
            }
            if (_this.x<_this.tx) {
              _this.x+=3;
            }
            if (_this.y>_this.ty) {
              _this.y-=3;
            }
            if (_this.y<_this.ty) {
              _this.y+=3;
            }
            if (_this.rnd(20)==0) {
              _this.tx=_this.rnd(512);
              _this.ty=_this.rnd(384);
              for ([_this.s] of Tonyu.iterator2(_this.all(Tonyu.classes.user.PBullet),1)) {
                if (_this.s.pc==_this.pc&&_this.s.i>=0) {
                  _this.tx=_this.s.x;
                  _this.ty=_this.s.y;
                  
                }
                
              }
              
            }
            
          }
          
          if (_this.defeatBy) {
            _this.i = 0;
            
            while (_this.i<120) {
              yield null;
              _this.x+=8;
              (yield* _this.fiber$update(_thread));
              _this.x-=8;
              (yield* _this.fiber$update(_thread));
              _this.i+=1;
              
            }
            _this.defeatBy=0;
            _this.i=0;
            
          }
          (yield* _this.fiber$update(_thread));
          
        }
        
      },
      __dummy: false
    };
  },
  decls: {"methods":{"main":{"nowait":false,"isMain":true,"vtype":{"params":[],"returnValue":null}}},"fields":{"tx":{},"ty":{},"pc":{},"s":{},"defeatBy":{},"i":{"vtype":"Number"}}}
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