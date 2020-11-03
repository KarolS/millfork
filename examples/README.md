# Examples

The examples showcased here are designed to compile with a compiler built from newest sources.
If you are using a release version of the compiler, consider browsing the older versions of the examples:

* [for version 0.3.22](https://github.com/KarolS/millfork/tree/v0.3.22/examples)

* [for version 0.3.18](https://github.com/KarolS/millfork/tree/v0.3.18/examples)

* [for version 0.3.16](https://github.com/KarolS/millfork/tree/v0.3.16/examples)

* [for version 0.3.14](https://github.com/KarolS/millfork/tree/v0.3.14/examples)

* [for version 0.3.12](https://github.com/KarolS/millfork/tree/v0.3.12/examples)

* [for version 0.3.10](https://github.com/KarolS/millfork/tree/v0.3.10/examples)

## Cross-platform examples

* [Hello world](crossplatform/hello_world.mfk) (C64/C16/PET/VIC-20/Atari/Apple II/BBC Micro/ZX Spectrum/PC-88/Armstrad CPC/MSX/Z1013) – simple text output

* [Fizzbuzz](crossplatform/fizzbuzz.mfk) (C64/C16/PET/VIC-20/PET/Atari/Apple II/BBC Micro/ZX Spectrum/PC-88/Armstrad CPC/MSX/X16) – everyone's favourite programming task

* [Fizzbuzz 2](crossplatform/fizzbuzz2.mfk) (C64/C16/PET/VIC-20/PET/Atari/Apple II/BBC Micro/ZX Spectrum/PC-88/Armstrad CPC/MSX/CoCo) – an alternative, more extensible implemententation of fizzbuzz

* [Fizzbuzz JP](crossplatform/fizzbuzz_jp.mfk) (PC-88/Japanese C64) – Fizzbuzz, but in Japanese

* [Text encodings](crossplatform/text_encodings.mfk) (C64/ZX Spectrum) – examples of text encoding features

* [Echo](crossplatform/echo.mfk) (C64/C16/VIC-20/Apple II/ZX Spectrum/PC-88/MSX)– simple text input and output

* [Calculator](crossplatform/calculator.mfk) (C64/C16/VIC-20/Apple II/ZX Spectrum/PC-88/MSX/TRS-80) – simple numeric input and output

* [Guessing game](crossplatform/guess.mfk) (C64/C16/VIC-20/Apple II/ZX Spectrum/PC-88/MSX/TRS-80/Z1013) – a guess-a-number game

* [Fire effect](crossplatform/fire.mfk) (C64/C16/ZX Spectrum) – a simple fire effect

* [`readkey` test](crossplatform/readkeytest.mfk) (C64/C16/PET/VIC-20/Atari/Apple II/Armstrad CPC/ZX Spectrum/PC-88/TRS-80/Z1013) – keyboard reading test

* [Screen encoding test](crossplatform/screnctest.mfk) (C64/C16) – default-to-screen encoding conversion test

* [Bell](crossplatform/bell.mfk) (Apple II/ZX Spectrum) – a program that goes \*ding!\*

* [Life](crossplatform/life.mfk) (C64/C16/Atari/ZX Spectrum) – Conway's game of life

* [Reg dump](crossplatform/regdump.mfk) (C64/C16/ZX Spectrum/CoCo) – a program that simply prints the initial values of CPU registers

* [Test suite](tests) (C64/C16/Atari/Apple II/BBC Micro/Armstrad CPC/ZX Spectrum/PC-88/CoCo) – the semi-official test-suite for Millfork

## Commodore 64 examples

### Graphical examples

* [Rasterbar](c64/rasterbar.mfk) – simple rasterbar effect

* [Softscrolling](c64/softscroll.mfk) – soft-scrolling a single line of text

* [Galencia starfield](c64/galencia.mfk) – a port of the starfield effect from the game *Galencia*

* [Space Poker \[external link\]](https://github.com/KarolS/spacepoker) – a game made for the 2018 Reset C64 Craptastic 4KB Game Competition

### Other examples

* Multifile ([source code](c64/multifile.mfk), [platform definition](c64/multifile.ini)) –
how to create a program made of multiple files loaded on demand

* [Panic](c64/panic_test.mfk) – how panic works on C64, showing the address of where it happened

## Famicom/NES examples

* [NES 101 tutorial example](nes/nestest.mfk) – a port of the tutorial example from the NES 101 tutorial by Michael Martin

* [MMC4 example](nes/nestest_mmc4.mfk) – the same thing as above, but uses a MMC4 mapper just to test bankswitching

* [Pong example](nes/pong.mfk) – simple pong example based off pong1.asm by bunnyboy of the nintendoage.com forums

## Atari Lynx examples

* [Lynx demo example](atari_lynx/atari_lynx_demo.mfk) – a simple sprite demo

## Atari 8-bit examples

### Hardware specific examples

* [System Off example](a8/systemoff_example.mfk) – programming with ROM off

* [DLI example](a8/dli_example.mfk) – simple display list and display list interrupt example

* [Horizontal scroll example](a8/endless_scroll.mfk) – simple horizontal scroll example

* [Vertical scroll example](a8/vertical_scroll.mfk) – simple vertical scroll example

* [Horizontal stars example](a8/horizontal_stars.mfk) – horizontal stars done on one missile

### Music

* [CMC Player](a8/cmcplayer.mfk) – CMC player with sample music

* [MPT Player](a8/mptplayer.mfk) – MPT player with sample music

### Benchmarks

* [Grand Theft Antic](a8/grand_theft_antic.mfk) – ANTIC impact on CPU depending on the used graphic mode

* [GR.8 Chessboard Benchmark](a8/gr8_chessboard_benchmark.mfk) – chessboard drawing benchmark in GR.8

* [FOR Countdown Benchmark](a8/countdown_for_benchmark.mfk) – countdown from 1,999,999 to 0 (FOR loop)

* [WHILE Countdown Benchmark](a8/countdown_while_benchmark.mfk) – countdown from 1,999,999 to 0 (WHILE loop)

* [Sieve of Eratosthenes (1899) Benchmark](a8/sieve1899.mfk) – sieve of Eratosthenes, 1899 primes algorithm

* [Monte Carlo PI approximation Benchmark](a8/montecarlo_pi_benchmark.mfk) – measures the efficiency of multiplication

* [Bubble Sort Benchmark](a8/bubble_sort.mfk) – sort 255 elements

### Other examples

* [Test OS module](a8/a8_os_test.mfk) – quick test for a8_os.mfk module

* [Rainbow example](a8/rainbow.mfk) – simple scrolling rasterbars

* [Quatari Landscape](a8/landscape.mfk) – part of Quatari 256B intro

## Game Boy examples

* [GB test example](gb/gbtest.mfk) – a partial port of the NES example, with a rudimentary experimental text output implementation

## Atari 2600 examples

* [Colors](vcs/colors.mfk) – simple static rasterbars

## MSX examples

* [Encoding test](msx/encoding_test.mfk) – text encoding test; displays three lines of text in three different languages,
no more one of which will display correctly depending on the default font of your computer.

## Commander X16 examples

* [Palette](x16/palette.mfk) – displays the default 256-colour palette.

* [Balls](x16/balls.mfk) – 16 sprites using 240 colours.

* [Joy demo](x16/joydemo.mfk) – simple joystick demo.
