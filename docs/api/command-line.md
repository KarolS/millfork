[< back to index](../doc_index.md)

# Command-line options

## General options

* `--version` – Display the version number and quit.

* `--help` – Displays help the command line option.

* `--` – End of the options, all the following parameters will be treated as input files, even if they look like options.

## I/O options

*  `-o <file>` – Output filename, without extension.
Extension will be added automatically,
`.prg` for Commodore (including LUnix/LNG),
`.crt` for Commodore cartridges,
`.xex` for Atari computers,
`.a2` for Apple,
`.dsk` for PC-88 disk images,
`.tap` for ZX-Spectrum tape images,
`.rom` for MSX cartridge images,
`.com` for CP/M executables,
`.nes` for NES/Famicom,
`.bin` for Atari 2600,
no extension for BBC micro program file,
`.inf` for BBC Micro file metadata,
`.asm` for assembly output,
`.lbl`, `.nl`, `.fns`, or `.sym` for label file.

* `-s` – Generate also the assembly output. It is not compatible with any assembler, but it serves purely informational purpose. The file has the same nam as the output file and the extension is `.asm`.

* `-g` – Generate also the label file. The label file contains labels with their addresses, with duplicates removed.
It can be loaded into the monitor of the emulator for debugging purposes.
The file has the same name as the output file.
The extension and the file format are platform-dependent. 

* `-G <format>` – The same as `-g`, but with the specified format:

    * `-G vice` – format compatible with the Vice emulator. The extension is `.lbl`.
    
    * `-G nesasm` – format used by the NESASM assembler. The extension is `.fns`.
    
    * `-G sym` – format used by the WLA DX assembler. The extension is `.sym`.
    
    * `-G fceux` – multi-file format used by the FCEUX emulator. The extension is `.nl`.

* `-I  <dir>;<dir>` – The include directories.
Those directories are searched for modules and platform definitions.
When searching for modules, the directory containing the file currently being compiled is also searched.
When searching for platform definitions, the current working directory is also searched.
If not given, the compiler will try to detect the default include directory.
If given, then the compiler will NOT try to detect the default include directory and you will have to add it to the list yourself.

* `-i <dir>` – Add a directory to the include directories.
Unlike `-I`, this does not replace the default include directory and allows using directories with semicolons in their names. 
 
* `-t <platform>` – Target platform. It is loaded from an `.ini` file found in any of the include directories. See also [this document](target-platforms.md).

* `-r <program>` – Run given program after successful compilation. Useful for automatically launching emulators without any external scripting.

* `-R <param>` – Adds a parameter to the command line of the program run with `-r`. All `-R` options are added in order, before the output file name.

* `-D <feature>=<value>` – Defines a feature value for the preprocessor.

* `-finput_intel_syntax`, `-finput_zilog_syntax` –
Choose syntax for assembly sources on 8080-like targets.
Can be overridden by the source file itself using `#pragma`.
`.ini` equivalent: `input_intel_syntax`. Default: Intel (true) on Intel 8080/8085, Zilog (false) otherwise.

* `-foutput_intel_syntax`, `-foutput_zilog_syntax` –
Choose syntax for assembly output on 8080-like targets.
`.ini` equivalent: `output_intel_syntax`. Default: Intel (true) on Intel 8080/8085, Zilog (false) otherwise.

* `--syntax=intel`, `--syntax=zilog` – sets both previous options at once

* `-fline-numbers`, `-fno-line-numbers` – Whether should show source line numbers in assembly output. Slightly slows down the compilation. Default: false.

* `-fsource-in-asm`, `-fno-source-in-asm` – Whether should show the original source in assembly output. Implies `-fline-numbers`. Default: false.

## Verbosity options

 * `-q` – Suppress all messages except for errors.

* `-v`, `-vv`, `-vvv` – Increase verbosity, various levels.

## Code generation options

#### For all targets

* `-fvariable-overlap`, `-fno-variable-overlap` – Whether variables should overlap if their scopes do not intersect.  
Default: yes.

* `-fbounds-checking`, `-fno-bounds-checking` – Whether should insert bounds checking on array access.  
Default: no.

* `-fcompact-dispatch-params`, `-fno-compact-dispatch-params` – 
Whether parameter values in return dispatch statements may overlap other objects. 
This may cause problems if the parameter table is stored next to a hardware register that has side effects when reading.  
`.ini` equivalent: `compact_dispatch_params`. Default: yes.

* `-flenient-encoding`, `-fno-lenient-encoding` – 
Whether the compiler should allow for invalid characters in string/character literals that use the default encodings and replace them with alternatives.
`.ini` equivalent: `lenient_encoding`. Default: yes on Apple II, no otherwise.

* `-fillegals`, `-fno-illegals` – Whether should emit illegal (undocumented) NMOS 6502, Intel 8085 or Z80 opcodes.  
`.ini` equivalent: `emit_illegals`.
Default: no.

#### 6502-related

* `-fcmos-ops`, `-fno-cmos-ops` – Whether should emit 65C02 opcodes.  
`.ini` equivalent: `emit_cmos`.
Default: yes if targeting a 65C02-compatible architecture, no otherwise.

* `-f65ce02-ops`, `-fno-65ce02-ops` – Whether should emit 65CE02 opcodes.  
`.ini` equivalent: `emit_65ce026`. 
Default: yes if targeting 65CE02, no otherwise.

* `-fhuc6280-ops`, `-fno-huc6280-ops` – Whether should emit HuC6280 opcodes.  
`.ini` equivalent: `emit_huc6280`. 
Default: yes if targeting HuC6280, no otherwise.

* `-fno-65816-ops`, `-femulation-65816-ops`, `-fnative-65816-ops` – Which subset of 65816 instructions to support. 
`-fnative-65816-ops` is required to use any 16-bit operations. 
Currently, there is not much support in the compiler for the native mode.
`.ini` equivalent: `emit_65816`. 
Default: native if targeting 65816, no otherwise.

* `-fjmp-fix`, `-fno-jmp-fix` – Whether should prevent indirect JMP bug on page boundary.  
`.ini` equivalent: `prevent_jmp_indirect_bug`.
Default: no if targeting a 65C02-compatible architecture or a non-6502 architecture, yes otherwise.

* `-fzp-register=[0-15]`, `-fno-zp-register` – Sets the size of the zeropage pseudoregister.
Increases language features on 6502-based targets.
Recommended values: 2 or 4. Values higher than 4 are pointless.  
`.ini` equivalent: `zeropage_register`.
Default: 4 if targeting a 6502-based architecture, 0 otherwise.
                    
* `-fdecimal-mode`, `-fno-decimal-mode` –
Whether hardware decimal mode should be used (6502 only).
If disabled, a software decimal mode will be used.  
`.ini` equivalent: `decimal_mode`.
Default: no if targeting Ricoh, yes otherwise.

* `-fsoftware-stack`, `-fno-software-stack` –
Use a software stack for stack variables.
`.ini` equivalent: `software_stack`. Default: no.

#### 8080/Z80-related

* `-f8085-ops`, `-fno-8085-ops` – Whether should emit Intel 8085 opcodes.  
`.ini` equivalent: `emit_8085`.
Default: yes if targeting Intel 8085 architecture, no otherwise.

* `-fz80-ops`, `-fno-z80-ops` – Whether should emit Z80 opcodes.   
`.ini` equivalents: `emit_z80` and `emit_x80`.
Default: yes if targeting a Z80-compatible architecture, no otherwise.

* `-fshadow-irq`, `-fno-shadow-irq` –
Whether the interrupt routines should make use of Z80 shadow registers.
`.ini` equivalent: `use_shadow_registers_for_irq`. Default: yes on Z80, no otherwise.

* `-fuse-ix-for-stack`, `-fuse-iy-for-stack`, `-fno-use-index-for-stack` –
Which of Z80 index registers should be used for accessing stack variables, if any. 
`.ini` equivalent: `ix_stack` and `iy_stack`. Default: IX on Z80, no otherwise.

* `-fuse-ix-for-scratch`, `-fno-use-ix-for-scratch` –
Allow using the IX register for other purposes.
`.ini` equivalent: `ix_scratch`. Default: no.

* `-fuse-iy-for-scratch`, `-fno-use-iy-for-scratch` –
Allow using the IY register for other purposes.
`.ini` equivalent: `iy_scratch`. Default: no.

#### 8086-related

Compiling to 8086 is based on translating from a mix of 8085 and Z80 instructions to 8086.
See [the 8086 support disclaimer](./../lang/x86disclaimer.md).

## Optimization options

* `-O0` – Disable all optimizations except unused global symbol removal.

* `-O`, `-O2` ... `-O8` – Optimize code, various levels. For most code, anything above `-O4` doesn't improve it anymore. 

* `-O9` – Optimize code using superoptimizer (experimental). Computationally very expensive, decent results.

* `-finline`, `-fno-inline` – Whether should inline functions automatically.
See the [documentation about inlining](../abi/inlining.md). Computationally easy, can give decent gains.
`.ini` equivalent: `inline`.
Default: no

* `-fipo`, `-fno-ipo` – Whether should perform interprocedural optimization.
It enables certain optimization similar to what inlining would enable, but without actual inlining.
`.ini` equivalent: `ipo`.
Default: no. 

* `-fregister-variables`, `-fno-register-variables` – Whether should allow moving local variables into CPU registers.
Default: yes. 

* `-foptimize-stdlib`, `-fno-optimize-stdlib` – 
Whether should replace some standard library calls with constant parameters with more efficient variants.
Currently affects `putstrz` and `strzlen`, but may affect more functions in the future.
`.ini` equivalent: `optimize_stdlib`.
Default: no.

* `-ffunction-fallthrough`, `-fno-function-fallthrough` – 
Whether should replace a tail call by simply putting one function after another.
`.ini` equivalent: `function_fallthrough`.
Default: yes. 

* `-ffunction-deduplication`, `-fno-function-deduplication` – 
Whether identical functions should be merged into one function.
`.ini` equivalent: `function_deduplication`.
Default: yes. 

* `-fsubroutine-extraction`, `-fno-subroutine-extraction` – 
Whether identical fragments of functions should be extracted into subroutines (experimental).
Makes the code smaller. Computationally very expensive.
`.ini` equivalent: `subroutine_extraction`.
Default: no. 

* `-Os`, `--size` – Optimize for size, sacrificing some speed (experimental).

* `-Of`, `--fast` – Optimize for speed, even if it increases the size a bit (experimental).
Also enables `-finline`. 

* `-Ob`, `--blast-processing` – Optimize for speed, even if it increases the size a lot (experimental).
Also enables `-finline`. 

* `-Og`, `--optimize-debugging` – Disables optimizations that make debugging harder.
Sets 
`-fno-optimize-stdlib`,
`-fno-variable-overlap`,
`-fno-register-variables`,
`-fno-function-fallthrough`,
`-fno-function-deduplication`,
`-fno-subroutine-extraction`
 automatically. 

* `-fdangerous-optimizations` – Use dangerous optimizations (experimental).
Dangerous optimizations are more likely to result in broken code.
Also enables `-fipo` and `-foptimize-stdlib` automatically.
`.ini` equivalent: `dangerous_optimizations`. 

Note: for compatibility with versions 0.3.0 and earlier,
command line options `--inline`, `--dangerous-optimizations` `--fipo` and `--fno-ipo` are still recognized, but discouraged.

## Warning options

* `-Wall` – Enable extra warnings.

* `-Wfatal` – Treat warnings as errors.
