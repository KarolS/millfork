package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedIntel8080Run, EmuUnoptimizedIntel8085Run, EmuUnoptimizedR800Run, EmuUnoptimizedSharpRun, EmuUnoptimizedZ80NextRun, EmuUnoptimizedZ80Run}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class Z80AssemblySuite extends FunSuite with Matchers {

  test("Common I80 instructions (Zilog syntax)") {
    EmuUnoptimizedIntel8080Run(
      """
        | #pragma zilog_syntax
        | asm void main () {
        |   ret
        |
        |   nop
        |   ld bc,$101
        |   ld (bc),a
        |   inc bc
        |   inc b
        |   dec b
        |   ld b,6
        |   rlca
        |   add hl,bc
        |   ld a,(bc)
        |   dec bc
        |   inc c
        |   dec c
        |   ld c,$e
        |   rrca
        |
        |   ld de,$1111
        |   ld (de),a
        |   inc de
        |   inc d
        |   dec d
        |   ld d,$16
        |   rla
        |   add hl,de
        |   ld a,(de)
        |   dec de
        |   inc e
        |   dec e
        |   ld e,$1e
        |   rra
        |
        |   ld hl,$2121
        |   inc hl
        |   inc h
        |   dec h
        |   ld h,$26
        |   daa
        |   add hl,hl
        |   dec hl
        |   inc l
        |   dec l
        |   ld l,$2e
        |   cpl
        |
        |   ld sp,$3131
        |   ld ($fffe),a
        |   inc sp
        |   inc (hl)
        |   dec (hl)
        |   ld (hl),$36
        |   scf
        |   add hl,sp
        |   ld a,($fffe)
        |   dec sp
        |   inc a
        |   dec a
        |   ld a,$3e
        |   ccf
        |   
        |   ld b,b
        |   ld b,c
        |   ld b,d
        |   ld b,e
        |   ld b,h
        |   ld b,l
        |   ld b,(hl)
        |   ld b,a
        |   
        |   ld c,b
        |   ld c,c
        |   ld c,d
        |   ld c,e
        |   ld c,h
        |   ld c,l
        |   ld c,(hl)
        |   ld c,a
        |   
        |   ld d,b
        |   ld d,c
        |   ld d,d
        |   ld d,e
        |   ld d,h
        |   ld d,l
        |   ld d,(hl)
        |   ld d,a
        |   
        |   ld e,b
        |   ld e,c
        |   ld e,d
        |   ld e,e
        |   ld e,h
        |   ld e,l
        |   ld e,(hl)
        |   ld e,a
        |   
        |   ld h,b
        |   ld h,c
        |   ld h,d
        |   ld h,e
        |   ld h,h
        |   ld h,l
        |   ld h,(hl)
        |   ld h,a
        |   
        |   ld l,b
        |   ld l,c
        |   ld l,d
        |   ld l,e
        |   ld l,h
        |   ld l,l
        |   ld l,(hl)
        |   ld l,a
        |   
        |   ld (hl),b
        |   ld (hl),c
        |   ld (hl),d
        |   ld (hl),e
        |   ld (hl),h
        |   ld (hl),l
        |   halt
        |   ld (hl),a
        |   
        |   ld a,b
        |   ld a,c
        |   ld a,d
        |   ld a,e
        |   ld a,h
        |   ld a,l
        |   ld a,(hl)
        |   ld a,a
        |   
        |   add a,b
        |   add a,c
        |   add a,d
        |   add a,e
        |   add a,h
        |   add a,l
        |   add a,(hl)
        |   add a,a
        |   
        |   adc a,b
        |   adc a,c
        |   adc a,d
        |   adc a,e
        |   adc a,h
        |   adc a,l
        |   adc a,(hl)
        |   adc a,a
        |   
        |   sub b
        |   sub c
        |   sub d
        |   sub e
        |   sub h
        |   sub l
        |   sub (hl)
        |   sub a
        |   
        |   sbc a,b
        |   sbc a,c
        |   sbc a,d
        |   sbc a,e
        |   sbc a,h
        |   sbc a,l
        |   sbc a,(hl)
        |   sbc a,a
        |   
        |   and b
        |   and c
        |   and d
        |   and e
        |   and h
        |   and l
        |   and (hl)
        |   and a
        |   
        |   xor b
        |   xor c
        |   xor d
        |   xor e
        |   xor h
        |   xor l
        |   xor (hl)
        |   xor a
        |   
        |   or b
        |   or c
        |   or d
        |   or e
        |   or h
        |   or l
        |   or (hl)
        |   or a
        |   
        |   cp b
        |   cp c
        |   cp d
        |   cp e
        |   cp h
        |   cp l
        |   cp (hl)
        |   cp a
        |
        |   ret nz
        |   pop bc
        |   jp nz,main
        |   jp main
        |   call nz,main
        |   push bc
        |   add a,1
        |   rst 0
        |
        |   ret z
        |   ret
        |   jp z,main
        |   call z,main
        |   call main
        |   adc a,1
        |   rst 8
        |
        |   ret nc
        |   pop de
        |   jp nc,main
        |   call nc,main
        |   push de
        |   sub 1
        |   rst $10
        |
        |   ret c
        |   jp c,main
        |   call c,main
        |   sbc a,1
        |   rst $18
        |
        |   pop hl
        |   ex (sp),hl
        |   push hl
        |   and 1
        |   rst $20
        |
        |   jp (hl)
        |   xor 1
        |   rst $28
        |
        |   pop af
        |   di
        |   push af
        |   or 1
        |   rst $30
        |
        |   ld sp,hl
        |   ei
        |   cp 1
        |   rst $38
        |
        |   ret
        | }
      """.stripMargin)
  }

  test("Intel hex syntax disambiguation") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Intel8080)(
      """
        | #pragma intel_syntax
        | asm void main () {
        |   lxi h, 0c000h
        |   mvi m, 0b1h
        |   mvi m, 0b1_h
        |   mvi m, 0b_1_h
        |   inx h
        |   mvi m, 0b1
        |   mvi m, 0b_1
        |   ret
        |   }
        |   """.stripMargin){m =>
      m.readByte(0xc000) should equal(0xb1)
      m.readByte(0xc001) should equal(1)
    }
  }

  test("Common I80 instructions (without RST, Intel syntax)") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Intel8080, Cpu.Intel8086)(
      """
        | #pragma intel_syntax
        | asm void main () {
        |   ret
        |
        |   nop
        |   lxi b, 0101h
        |   stax b
        |   inx b
        |   inr b
        |   dcr b
        |   mvi b,6h
        |   rlc
        |   dad b
        |   ldax b
        |   dcx b
        |   inr c
        |   dcr c
        |   mvi c,0e_h
        |   rrc
        |
        |   lxi d,11_11h
        |   stax d
        |   inx d
        |   inr d
        |   dcr d
        |   mvi d,16h
        |   ral
        |   dad d
        |   ldax d
        |   dcx d
        |   inr d
        |   dcr d
        |   mvi e, 1eh
        |   rar
        |   
        |   lxi h, 2121h
        |   inx h
        |   inr h
        |   dcr h
        |   mvi h,26h
        |   daa
        |   dad h
        |   dcx h
        |   inr l
        |   dcr l
        |   mvi l,2eh
        |   cma
        |
        |   lxi sp, 3131h
        |   sta 0fffeh
        |   inx sp
        |   inr m
        |   dcr m
        |   mvi m, 36h
        |   stc
        |   dad sp
        |   lda 0fffeh
        |   dcx sp
        |   inr a
        |   dcr a
        |   mvi a, 3eh
        |   cmc
        |
        |   mov b,b
        |   mov b,c
        |   mov b,d
        |   mov b,e
        |   mov b,h
        |   mov b,l
        |   mov b,m
        |   mov b,a
        |
        |   mov c,b
        |   mov c,c
        |   mov c,d
        |   mov c,e
        |   mov c,h
        |   mov c,l
        |   mov c,m
        |   mov c,a
        |
        |   mov d,b
        |   mov d,c
        |   mov d,d
        |   mov d,e
        |   mov d,h
        |   mov d,l
        |   mov d,m
        |   mov d,a
        |
        |   mov e,b
        |   mov e,c
        |   mov e,d
        |   mov e,e
        |   mov e,h
        |   mov e,l
        |   mov e,m
        |   mov e,a
        |
        |   mov h,b
        |   mov h,c
        |   mov h,d
        |   mov h,e
        |   mov h,h
        |   mov h,l
        |   mov h,m
        |   mov h,a
        |
        |   mov l,b
        |   mov l,c
        |   mov l,d
        |   mov l,e
        |   mov l,h
        |   mov l,l
        |   mov l,m
        |   mov l,a
        |
        |   mov m,b
        |   mov m,c
        |   mov m,d
        |   mov m,e
        |   mov m,h
        |   mov m,l
        |   hlt
        |   mov m,a
        |
        |   mov a,b
        |   mov a,c
        |   mov a,d
        |   mov a,e
        |   mov a,h
        |   mov a,l
        |   mov a,m
        |   mov a,a
        |
        |   add b
        |   add c
        |   add d
        |   add e
        |   add h
        |   add l
        |   add m
        |   add a
        |
        |   adc b
        |   adc c
        |   adc d
        |   adc e
        |   adc h
        |   adc l
        |   adc m
        |   adc a
        |
        |   sub b
        |   sub c
        |   sub d
        |   sub e
        |   sub h
        |   sub l
        |   sub m
        |   sub a
        |
        |   sbb b
        |   sbb c
        |   sbb d
        |   sbb e
        |   sbb h
        |   sbb l
        |   sbb m
        |   sbb a
        |
        |   ana b
        |   ana c
        |   ana d
        |   ana e
        |   ana h
        |   ana l
        |   ana m
        |   ana a
        |
        |   xra b
        |   xra c
        |   xra d
        |   xra e
        |   xra h
        |   xra l
        |   xra m
        |   xra a
        |
        |   ora b
        |   ora c
        |   ora d
        |   ora e
        |   ora h
        |   ora l
        |   ora m
        |   ora a
        |
        |   cmp b
        |   cmp c
        |   cmp d
        |   cmp e
        |   cmp h
        |   cmp l
        |   cmp m
        |   cmp a
        |
        |   rnz
        |   pop b
        |   jnz main
        |   jmp main
        |   cnz main
        |   push b
        |   adi 1_h
        |
        |   rz
        |   ret
        |   jz main
        |   cz main
        |   call main
        |   aci 1
        |
        |   rnc
        |   pop d
        |   jnc main
        |   cnc main
        |   push d
        |   sui 1
        |
        |   rc
        |   jc main
        |   cc main
        |   sbi 1
        |
        |   pop h
        |   xthl
        |   push h
        |   ani 1
        |
        |   pchl
        |   xri 1
        |
        |   pop psw
        |   di
        |   push psw
        |   ori 1
        |
        |   sphl
        |   ei
        |   cpi 1
        |
        |   ret
        | }
      """.stripMargin){ m => }
  }

  test("Intel 8080 instructions (Zilog syntax)") {
    EmuUnoptimizedIntel8080Run(
      """
        | #pragma zilog_syntax
        | asm void main () {
        |   ret
        |   ld ($fffe),hl
        |   ld hl,($fffe)
        |   out (1),a
        |   in a,(1)
        |   ret po
        |   jp po,main
        |   call po,main
        |   ret pe
        |   jp pe,main
        |   ex de,hl
        |   call pe,main
        |   ret p
        |   jp p,main
        |   call p,main
        |   ret m
        |   jp m,main
        |   call m,main
        |
        |   ret
        | }
      """.stripMargin)
  }

  test("Intel 8080 instructions (Intel syntax)") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Intel8080, Cpu.Intel8086)(
      """
        | #pragma intel_syntax
        | asm void main () {
        |   ret
        |   shld 0fffeh
        |   lhld 0fffeh
        |   out 1
        |   in 1
        |   rpo
        |   jpo main
        |   cpo main
        |   rpe
        |   jpe main
        |   xchg
        |   cpe main
        |   rp
        |   jp main
        |   cp main
        |   rm
        |   jm main
        |   cm main
        |
        |   ret
        | }
      """.stripMargin){ m => }
  }

  test("Intel 8080 RST instructions (Intel syntax)") {
    EmuUnoptimizedIntel8080Run(
      """
        | #pragma intel_syntax
        | asm void main () {
        |   ret
        |   rst 0
        |   rst 1
        |   rst 2
        |   rst 3
        |   rst 4
        |   rst 5
        |   rst 6
        |   rst 7
        |
        |   ret
        | }
      """.stripMargin)
  }

  test("Extended I80 instructions") {
    EmuUnoptimizedZ80Run(
      """
        | asm void main () {
        |   ret
        |
        |   reti
        |
        |   jr main
        |   jr nz,main
        |   jr z,main
        |   jr nc,main
        |   jr c,main
        |   ld (34),sp
        |   
        |   rlc b
        |   rlc c
        |   rlc d
        |   rlc e
        |   rlc h
        |   rlc l
        |   rlc (hl)
        |   rlc a
        |   
        |   rrc b
        |   rrc c
        |   rrc d
        |   rrc e
        |   rrc h
        |   rrc l
        |   rrc (hl)
        |   rrc a
        |   
        |   rl b
        |   rl c
        |   rl d
        |   rl e
        |   rl h
        |   rl l
        |   rl (hl)
        |   rl a
        |   
        |   rr b
        |   rr c
        |   rr d
        |   rr e
        |   rr h
        |   rr l
        |   rr (hl)
        |   rr a
        |   
        |   sla b
        |   sla c
        |   sla d
        |   sla e
        |   sla h
        |   sla l
        |   sla (hl)
        |   sla a
        |   
        |   sra b
        |   sra c
        |   sra d
        |   sra e
        |   sra h
        |   sra l
        |   sra (hl)
        |   sra a
        |   
        |   srl b
        |   srl c
        |   srl d
        |   srl e
        |   srl h
        |   srl l
        |   srl (hl)
        |   srl a
        |
        |   bit 1,a
        |   res 1,a
        |   set 1,a
        |   bit 1,(hl)
        |   res 1,(hl)
        |   set 1,(hl)
        |
        |   ret
        | }
      """.stripMargin)
  }

  test("Z80 instructions with IX") {
    EmuUnoptimizedZ80Run(
      """
        | asm void main () {
        |   ret
        |   add a,ix(1)
        |   adc a,ix(2)
        |   sub ix(3)
        |   sbc a,ix(0)
        |   and ix(0)
        |   xor ix(0)
        |   or ix(0)
        |   cp ix(0)
        |
        |   rrc ix(0)
        |   rr ix(0)
        |   rlc ix(0)
        |   rl ix(0)
        |   sla ix(0)
        |   sra ix(0)
        |   srl ix(0)
        |   sll ix(0)
        |
        |   pop ix
        |   push ix
        |   add ix,sp
        |   add ix,ix
        |   add ix,de
        |   add ix,bc
        |   inc ix
        |   dec ix
        |   ld ix,3
        |   ld ix,(3)
        |   ld (3),ix
        |   ex (sp),ix
        |   jp (ix)
        |   ld sp,ix
        |   ld a,ix(2)
        |   ld ix(2),a
        |
        |   ret
        | }
      """.stripMargin)
  }

  test("Z80 instructions with IY") {
    EmuUnoptimizedZ80Run(
      """
        | asm void main () {
        |   ret
        |   add a,iy(1)
        |   adc a,iy(2)
        |   sub iy(3)
        |   sbc a,iy(0)
        |   and iy(0)
        |   xor iy(0)
        |   or iy(0)
        |   cp iy(0)
        |
        |   rrc iy(0)
        |   rr iy(0)
        |   rlc iy(0)
        |   rl iy(0)
        |   sla iy(0)
        |   sra iy(0)
        |   srl iy(0)
        |   sll iy(0)
        |
        |   pop iy
        |   push iy
        |   add iy,sp
        |   add iy,iy
        |   add iy,de
        |   add iy,bc
        |   inc iy
        |   dec iy
        |   ld iy,3
        |   ld iy,(3)
        |   ld (3),iy
        |   ex (sp),iy
        |   jp (iy)
        |   ld sp,iy
        |   ld a,iy(2)
        |   ld iy(2),a
        |
        |   ret
        | }
      """.stripMargin)
  }

  test("Other Z80 instructions") {
    EmuUnoptimizedZ80Run(
      """
        | asm void main () {
        |   ret
        |
        |   djnz main
        |   ex af,af'
        |   exx
        |
        |   sll b
        |   sll c
        |   sll d
        |   sll e
        |   sll h
        |   sll l
        |   sll (hl)
        |   sll a
        |
        |   in b,(c)
        |   out(c),b
        |   sbc hl,bc
        |   ld (34),bc
        |   neg
        |   retn
        |   im 0
        |   ld i,a
        |   in c,(c)
        |   out (c),c
        |   adc hl,bc
        |   ld bc,(7)
        |   ld r,a
        |   in d,(c)
        |   out (c),d
        |   sbc hl,de
        |   ld (55),de
        |   im 1
        |   ld a,i
        |   in e,(c)
        |   out (c),e
        |   adc hl,de
        |   ld de,(33)
        |   im 2
        |   ld a,r
        |
        |   in h,(c)
        |   out (c),h
        |   sbc hl,hl
        |   rrd
        |   in l,(c)
        |   out (c),l
        |   adc hl,hl
        |   rld
        |   sbc hl,sp
        |   in a,(c)
        |   out (c),a
        |   adc hl,sp
        |   ld sp,(345)
        |
        |   ldi
        |   cpi
        |   ini
        |   outi
        |   ldd
        |   cpd
        |   ind
        |   outd
        |   ldir
        |   cpir
        |   inir
        |   otir
        |   lddr
        |   cpdr
        |   indr
        |   otdr
        |
        |   ret
        | }
      """.stripMargin)
  }

  test("Extended I80 instructions (Intel syntax)") {
    EmuUnoptimizedZ80Run(
      """
        | #pragma intel_syntax
        | asm void main () {
        |   ret
        |
        |   reti
        |
        |   jmpr main
        |   jrnz main
        |   jrz main
        |   jrnc main
        |   jrc main
        |   lspd 34
        |
        |   rlcr b
        |   rlcr c
        |   rlcr d
        |   rlcr e
        |   rlcr h
        |   rlcr l
        |   rlcr m
        |   rlcr a
        |
        |   rrcr b
        |   rrcr c
        |   rrcr d
        |   rrcr e
        |   rrcr h
        |   rrcr l
        |   rrcr m
        |   rrcr a
        |
        |   ralr b
        |   ralr c
        |   ralr d
        |   ralr e
        |   ralr h
        |   ralr l
        |   ralr m
        |   ralr a
        |
        |   rarr b
        |   rarr c
        |   rarr d
        |   rarr e
        |   rarr h
        |   rarr l
        |   rarr m
        |   rarr a
        |
        |   slar b
        |   slar c
        |   slar d
        |   slar e
        |   slar h
        |   slar l
        |   slar m
        |   slar a
        |
        |   srar b
        |   srar c
        |   srar d
        |   srar e
        |   srar h
        |   srar l
        |   srar m
        |   srar a
        |
        |   srlr b
        |   srlr c
        |   srlr d
        |   srlr e
        |   srlr h
        |   srlr l
        |   srlr m
        |   srlr a
        |
        |   bit 1,a
        |   res 1,a
        |   setb 1,a
        |   set 1,a
        |   bit 1,m
        |   res 1,m
        |   setb 1,m
        |   set 1,m
        |
        |   ret
        | }
      """.stripMargin)
  }

  test("Z80 instructions with IX (Intel syntax)") {
    EmuUnoptimizedZ80Run(
      """
        | #pragma intel_syntax
        | asm void main () {
        |   ret
        |   addx 0
        |   adcx 0
        |   subx 0
        |   sbcx 0
        |   andx 0
        |   xorx 0
        |   orx 0
        |   cmpx 0
        |
        |   rrcx 0
        |   rarx 0
        |   rlcx 0
        |   ralx 0
        |   slax 0
        |   srax 0
        |   srlx 0
        |   sllx 0
        |
        |   popix
        |   pushix
        |   pop ix
        |   push ix
        |   dadx sp
        |   dadx ix
        |   dadx d
        |   dadx b
        |   inxix
        |   dcxix
        |   inx ix
        |   dcx ix
        |   lxix 3
        |   lixd 3
        |   sixd 3
        |   xtix
        |   pcix
        |   spix
        |   ldx a,0
        |   stx a,0
        |
        |   ret
        | }
      """.stripMargin)
  }

  test("Z80 instructions with IY (Intel syntax)") {
    EmuUnoptimizedZ80Run(
      """
        | #pragma intel_syntax
        | asm void main () {
        |   ret
        |   addy 0
        |   adcy 0
        |   suby 0
        |   sbcy 0
        |   andy 0
        |   xory 0
        |   ory 0
        |   cmpy 0
        |
        |   rrcy 0
        |   rary 0
        |   rlcy 0
        |   raly 0
        |   slay 0
        |   sray 0
        |   srly 0
        |   slly 0
        |
        |   popiy
        |   pushiy
        |   pop iy
        |   push iy
        |   dady sp
        |   dady iy
        |   dady d
        |   dady b
        |   inxiy
        |   dcxiy
        |   inx iy
        |   dcx iy
        |   lxiy 3
        |   liyd 3
        |   siyd 3
        |   xtiy
        |   pciy
        |   spiy
        |   ldy a,0
        |   sty a,0
        |
        |   ret
        | }
      """.stripMargin)
  }

  test("Other Z80 instructions (Intel syntax)") {
    EmuUnoptimizedZ80Run(
      """
        | #pragma intel_syntax
        | asm void main () {
        |   ret
        |
        |   djnz main
        |   exaf
        |   exx
        |
        |   sllr b
        |   sllr c
        |   sllr d
        |   sllr e
        |   sllr h
        |   sllr l
        |   sllr m
        |   sllr a
        |
        |   inp b
        |   outp b
        |   dsbc b
        |   lbcd 34
        |   neg
        |   retn
        |   im0
        |   stai
        |   inp c
        |   outp c
        |   dadc b
        |   lbcd 7
        |   star
        |   inp d
        |   outp d
        |   dsbc d
        |   lded 55
        |   im1
        |   ldai
        |   inp e
        |   outp e
        |   dadc d
        |   lded 33
        |   im2
        |   ldar
        |
        |   inp h
        |   outp h
        |   dsbc h
        |   rrd
        |   inp l
        |   outp l
        |   dadc h
        |   rld
        |   dsbc sp
        |   inp a
        |   outp a
        |   dadc sp
        |   lspd 345
        |
        |   ldi
        |   cci
        |   ini
        |   outi
        |   ldd
        |   ccd
        |   ind
        |   outd
        |   ldir
        |   ccir
        |   inir
        |   outir
        |   lddr
        |   ccdr
        |   indr
        |   outdr
        |
        |   ret
        | }
      """.stripMargin)
  }

  test("Gameboy instructions") {
    EmuUnoptimizedSharpRun(
      """
        | asm void main () {
        |   ret
        |   ld (8),sp
        |   stop
        |   ld (hli),a
        |   ld (hld),a
        |   ld a,(hli)
        |   ld a,(hld)
        |   reti
        |   add sp,3
        |   ld hl,sp+3
        |   swap a
        |   swap b
        |   swap c
        |   swap(hl)
        |
        |   ldh a,(9)
        |   ldh (9),a
        |   ldh a,(c)
        |   ldh (c),a
        |   ret
        | }
      """.stripMargin)
  }


  test("Intel 8085 instructions (Zilog syntax)") {
    EmuUnoptimizedIntel8085Run(
      """
        | #pragma zilog_syntax
        | asm void main () {
        |   ret
        |   rim
        |   sim
        |   ret
        | }
      """.stripMargin)
  }


  test("Intel 8085 instructions (Intel syntax)") {
    EmuUnoptimizedIntel8085Run(
      """
        | #pragma intel_syntax
        | asm void main () {
        |   ret
        |   rim
        |   sim
        |   ret
        | }
      """.stripMargin)
  }

  test("Illegal Intel 8085 instructions (Intel syntax)") {
    EmuUnoptimizedIntel8085Run(
      """
        | #pragma intel_syntax
        | asm void main () {
        |   ret
        |   lhlx
        |   shlx
        |   arhl
        |   rlde
        |   ldhi 5
        |   ldsi 6
        |   rstv
        |   ovrst8
        |   jk main
        |   jnk main
        |   jx5 main
        |   jnx5 main
        |   dsub
        |   ret
        | }
      """.stripMargin)
  }

  test("Illegal Intel 8085 instructions (Zilog syntax)") {
    EmuUnoptimizedIntel8085Run(
      """
        | #pragma zilog_syntax
        | asm void main () {
        |   ret
        |   ld hl,(de)
        |   ld (de), hl
        |   sra hl
        |   rl de
        |   ld de,hl+5
        |   ld de,sp+6
        |   rstv
        |   jp k, main
        |   jp nk, main
        |   jp x5, main
        |   jp nx5, main
        |   dsub
        |   ret
        | }
      """.stripMargin)
  }

  test("Z80 Next instructions (Zilog syntax)") {
    EmuUnoptimizedZ80NextRun(
      """
        | #pragma zilog_syntax
        | asm void main () {
        |   ret
        |   ldix
        |   ldws
        |   ldirx
        |   lddx
        |   lddrx
        |   ldpirx
        |   outinb
        |   mul
        |   mul d,e
        |   add hl,a
        |   add de,a
        |   add bc,a
        |   add hl,1
        |   add de,2
        |   add bc,3
        |   swapnib
        |   mirror
        |   mirror a
        |   push $5555
        |   nextreg 1,2
        |   nextreg 1,a
        |   pixeldn
        |   pixelad
        |   setae
        |   test 8
        |   ret
        | }
    """.stripMargin)
  }

  test("Z80 halves of index registers") {
    EmuUnoptimizedZ80NextRun(
      """
        | #pragma zilog_syntax
        | asm void main () {
        |   ret
        |   inc ixh
        |   inc ixl
        |   inc iyh
        |   inc iyl
        |   dec ixh
        |   dec ixl
        |   dec iyh
        |   dec iyl
        |   ld a,ixh
        |   ld a,ixl
        |   ld iyh,a
        |   ld iyl,a
        |   add a,iyl
        |   adc a,iyl
        |   sub iyl
        |   sbc a,iyl
        |   or iyl
        |   xor iyl
        |   and iyl
        |   cp iyl
        |   ld ixh,0
        |   ret
        | }
    """.stripMargin)
  }

  test("R800 stuff") {
    EmuUnoptimizedR800Run(
      """
        | #pragma zilog_syntax
        | asm void main () {
        |   ret
        |   mulub a,b
        |   mulub a,c
        |   mulub a,d
        |   mulub a,e
        |   muluw hl,bc
        |   muluw hl,sp
        |   inc ixh
        |   inc ixl
        |   inc iyh
        |   inc iyl
        |   dec ixh
        |   dec ixl
        |   dec iyh
        |   dec iyl
        |   ld a,ixh
        |   ld a,ixl
        |   ld iyh,a
        |   ld iyl,a
        |   add a,iyl
        |   adc a,iyl
        |   sub iyl
        |   sbc a,iyl
        |   or iyl
        |   xor iyl
        |   and iyl
        |   cp iyl
        |   ld ixh,0
        |   ret
        | }
    """.stripMargin)
  }

}
