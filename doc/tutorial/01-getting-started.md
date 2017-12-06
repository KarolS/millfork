# Getting started

## Hello world example

Save the following as `hello_world.mfk`:

```
import stdio

array hello_world = "hello world" petscii

void main(){
    putstr(hello_world, hello_world.length)
    while(true){}
}
```

Compile is using the following commandline:

```
java millfork.jar hello_world.mfk -o hello_world -t c64 -I path_to_millfork\include
```

Run the output executable (here using the VICE emulator):

```
x64 hello_world.prg
```

## Basic commandline usage

The following options are crucial when compiling your sources:

* `-o FILENAME` – specifies the base name for your output file, an appropriate file extension will be appended (`prg` for Commodore, `xex` for Atari, `asm` for assembly output, `lbl` for label file)

* `-I DIR;DIR;DIR;...` – specifies the paths to directories with modules to include.  

* `-t PLATFORM` – specifies the target platform (`c64` is the default). Each platform is defined in an `.ini` file in the include directory. For the list of supported platforms, see [Supported platforms](../target-platforms.md)

You may be also interested in the following:

* `-O`, `-O2`, `-O3` – enable optimization (various levels)

* `--detailed-flow` – use more resource-consuming but more precise flow analysis engine for better optimization

* `-s` – additionally generate assembly output

* `-g` – additionally generate a label file, in format compatible with VICE emulator

* `-r PROGRAM` – automatically launch given program after successful compilation

* `-Wall` – enable all warnings

* `--help` – list all commandline options