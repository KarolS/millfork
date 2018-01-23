# Command-line options

## General options

* `--version` – Display the version number and quit.

* `--help` – Displays help the command line option.

* `--` – End of the options, all the following parameters will be treated as input files, even if they look like options.

## I/O options

*  `-o <file>` – Output filename, without extension. Extension will be added automatically, `.prg` for Commodore, `.a2` for Apple and `.xex` for Atari.

* `-s` – Generate also the assembly output. It is not compatible with any assembler, but it serves purely informational purpose. The file has the same nam as the output file and the extension is `.asm`.

* `-s` – Generate also the label file. The label file contains labels with their addresses, with duplicates removed. It can be loaded into the monitor of the Vice emulator for debugging purposes. The file has the same nam as the output file and the extension is `.lbl`.

* `-I  <dir>;<dir>` – The include directories. The current working directory is also an include directory. Those directories are searched for modules and platform definitions.
 
* `-t <platform>` – Target platform. It is loaded from an `.ini` file found in any of the include directories. See also [this document](target-platforms.md).

* `-r <program>` – Run given program after successful compilation. Useful for automatically launching emulators without any external scripting.


## Verbosity options

 * `-q` – Supress all messages except for errors.

* `-v`, `-vv`, `-vvv` – Increase verbosity, various levels.

## Code generation options

* `-fcmos-ops`, `-fno-cmos-ops` – Whether should emit CMOS opcodes. `.ini` equivalent: `emit_cmos`.

* `-fillegals`, `-fno-illegals` – Whether should emit illegal (undocumented) NMOS opcodes. `.ini` equivalent: `emit_illegals`.

* `-fjmp-fix`, `-fno-jmp-fix` – Whether should prevent indirect JMP bug on page boundary. `.ini` equivalent: `prevent_jmp_indirect_bug`.
                    
* `-fdecimal-mode`, `-fno-decimal-mode` – Whether decimal mode should be available. `.ini` equivalent: `decimal_mode`.

* `-fvariable-overlap`, `-fno-variable-overlap` – Whether variables should overlap if their scopes do not intersect. Default: yes.

* `-fbounds-checking`, `-fnobounds-checking` – Whether should insert bounds checking on array access. Default: no.

## Optimization options

* `-O0` – Disable all optimizations.

* `-O`, `-O2`, `-O3` – Optimize code, various levels.

* `-O9` – Optimize code using superoptimizer (experimental). Computationally expensive, decent results.

* `--inline` – Inline functions automatically (experimental). See the [documentation about inlining](../abi/inlining.md). Computationally easy, can give decent gains.

* `--detailed-flow` – Use detailed flow analysis (experimental). Very computationally expensive and not that great.

* `--dangerous-optimizations` – Use dangerous optimizations (experimental). Dangerous optimizations are more likely to result in broken code.

## Warning options

* `-Wall` – Enable extra warnings.

* `-Wfatal` – Treat warnings as errors.
