[< back to index](../doc_index.md)

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

or if you're using a Windows native build:

```
millfork hello_world.mfk -o hello_world -t c64
```

Run the output executable (here using the VICE emulator):

```
x64 hello_world.prg
```

## Basic command-line usage

The following options are obligatory when compiling your sources:

* `-o <FILENAME>` – specifies the base name for your output file, an appropriate file extension will be appended.

* `-t <PLATFORM>` – specifies the target platform.
Each platform is defined in an `.ini` file in the include directory.
For the list of supported platforms, see [Supported platforms](target-platforms.md)

You may be also interested in the following:

* `-O`, `-O2`, `-O3`, `-O4` – enable optimization (various levels)

* `-finline` – automatically inline functions for better optimization

* `-fipo` – enable interprocedural optimization

* `-s` – additionally generate assembly output
(if targeting Intel 8080/8085, use `--syntax=intel` or `--syntax=zilog` to choose the preferred assembly syntax)

* `-fsource-in-asm` – show original Millfork source in the assembly output

* `-g` – additionally generate a label file

* `-r <PROGRAM>` – automatically launch given program after successful compilation; you can supply extra params for it with `-R <PARAM>`

* `-Wall` – enable all warnings

* `--help` – list all commandline options

For the comprehensive list of command-line options, see [Command-line options](./command-line.md).
