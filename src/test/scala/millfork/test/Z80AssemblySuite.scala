package millfork.test

import millfork.test.emu.{EmuUnoptimizedIntel8080Run, EmuUnoptimizedSharpRun, EmuUnoptimizedZ80Run}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class Z80AssemblySuite extends FunSuite with Matchers {

  test("Common I80 instructions") {
    EmuUnoptimizedIntel8080Run(
      """
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
        |   ld hl,$2121
        |   ld ($fffe),a
        |   inc sp
        |   inc (hl)
        |   dec (hl)
        |   ld h,$26
        |   scf
        |   add hl,sp
        |   ld a,($fffe)
        |   dec sp
        |   inc a
        |   dec a
        |   ld l,$2e
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

  test("Intel 8080 instructions") {
    EmuUnoptimizedIntel8080Run(
      """
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
        |   add a,ix(0)
        |   adc a,ix(0)
        |   sub ix(0)
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
        |   ld a,ix(0)
        |   ld ix(0),a
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
        |   add a,iy(0)
        |   adc a,iy(0)
        |   sub iy(0)
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
        |   ld a,iy(0)
        |   ld iy(0),a
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
        |   ret
        | }
      """.stripMargin)
  }
}