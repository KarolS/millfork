package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCmosRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class Issue11Test extends FunSuite with Matchers {

  test("Test issue 11") {
    val src =
      """
        |import c64_basic
        |import stdio
        |
        |struct Box {
        |    byte x,
        |    byte y,
        |    byte width,
        |    byte height
        |}
        |
        |struct Phys_Obj {
        |    Box pos,
        |    byte xfrac,
        |    byte yfrac,
        |    sbyte xvel,
        |    sbyte yvel,
        |    byte xaccel,
        |    byte yaccel,
        |    byte xspeed,
        |    bool on_ground,
        |    bool jumping,
        |    bool can_jump
        |}
        |
        |Phys_Obj player1
        |byte output @$c000
        |
        |inline pointer get_metatile_column(word metatiles_column) {
        |    //This is the function where the original player pointer
        |    //gets corrupted.
        |    return pointer(metatiles_column)
        |}
        |
        |byte get_tile(byte column, byte row) {
        |    byte tile
        |    pointer metatiles_column
        |
        |    metatiles_column = get_metatile_column($0000)
        |    tile = $00
        |    return tile
        |}
        |
        |void check_background_collis(byte screenx, byte screeny, byte spritewidth, byte spriteheight) {
        |    byte new_column1
        |    byte new_column2
        |    byte new_row1
        |    byte new_row2
        |    byte i
        |    byte limit
        |    byte current_tile
        |
        |    current_tile = get_tile(0,0)
        |}
        |
        |void check_player_collis_and_update_player_loc(pointer.Phys_Obj player) {
        |    sbyte temp_vel
        |    byte old_playerx
        |    byte old_playerx_frac
        |    byte old_playery
        |    byte old_playery_frac
        |
        |    old_playerx = player1.pos.x
        |    old_playery = player1.pos.y
        |    old_playerx_frac = player1.xfrac
        |    old_playery_frac = player1.yfrac
        |
        |    check_background_collis(player1.pos.x, player1.pos.y, player1.pos.width, player1.pos.height)
        |}
        |
        |inline void game_logic(pointer.Phys_Obj player) {
        |    player->pos.x = 15
        |
        |    //comment out this function call and the player
        |    //pointer should still point to the correct location
        |    check_player_collis_and_update_player_loc(player)
        |    //after the function call, player points to garbage
        |
        |    //expected val: 15
        |    //if player no longer points to player1: garbage
        |    output = player->pos.x
        |}
        |
        |void main() {
        |    pointer.Phys_Obj player
        |    player = pointer.Phys_Obj(player1.addr)
        |    game_logic(player)
        |}
        |""".stripMargin

    EmuCrossPlatformBenchmarkRun(Cpu.Mos)(src) { m =>
      m.readByte(0xc000) should equal(15)
    }
  }
}
