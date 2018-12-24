[< back to index](../index.md)

# Getting started

## Hello world example

Save the following as `hello_world.mfk`:

```
import stdio

void main(){
    putstrz("hello world"z)
    while(true){}
}
```

Compile it using the following commandline:

```
java -jar millfork.jar hello_world.mfk -o hello_world -t c64
```

Run the output executable (here using the VICE emulator):

```
x64 hello_world.prg
```

## Basic command-line usage

The following options are obligatory when compiling your sources:

* `-o FILENAME` – specifies the base name for your output file, an appropriate file extension will be appended (`prg` for Commodore, `xex` for Atari computers, `a2` for Apple, `asm` for assembly output, `lbl` for label file, `inf` for BBC file metadata, `dsk` for PC-88, `tap` for ZX Spectrum, `nes` for Famicom, `bin` for Atari 2600)

* `-t PLATFORM` – specifies the target platform. Each platform is defined in an `.ini` file in the include directory. For the list of supported platforms, see [Supported platforms](target-platforms.md)

You may be also interested in the following:

* `-O`, `-O2`, `-O3`, `-O4` – enable optimization (various levels)

* `-finline` – automatically inline functions for better optimization

* `-fipo` – enable interprocedural optimization

* `-s` – additionally generate assembly output
(if targeting Intel 8080, use `--syntax=intel` or `--syntax=zilog` to choose the preferred assembly syntax)

* `-fsource-in-asm` – show original Millfork source in the assembly output

* `-g` – additionally generate a label file, in format compatible with VICE emulator

* `-r PROGRAM` – automatically launch given program after successful compilation

* `-Wall` – enable all warnings

* `--help` – list all commandline options

For the comprehensive list of command-line options, see [Command-line options](./command-line.md).
