[< back to index](../doc_index.md)

# Optimization guidelines

## Command-line options

* The default options provide literally no optimizations.
Consider using at least `-O1` for quick compilation and `-O4` for release builds.

* Inlining can drastically improve performance. Add `-finline` to the command line.

* If you're not using self-modifying code or code generation,
enabling interprocedural optimizations (`-fipo`) and stdlib optimizations (`-foptimize-stdlib`) can also help.

* For convenience, all options useful for debug builds can be enabled with `-Xd`,
and for release builds with `-Xr`.

* 6502 only: If you are sure the target will have a CPU that supports so-called illegal/undocumented 6502 instructions,
consider adding the `-fillegals` option. Good examples of such targets are NES and C64.

## Alignment

* Consider adding `align(fast)` or even `align(256)` to arrays which you want to access quickly.

* 6502 only: Consider adding `align(fast)` to the hottest functions.

* If you have an array of structs, consider adding `align(X)` to the definition of the struct,
where `X` is a power of two. Even if this makes the struct 12 bytes instead of 11, it can still improve performance.

## Variables

* Use the smallest type you need. Note that Millfork supports integers of any size from 1 to 16 bytes.

* Consider using multiple arrays instead of arrays of structs.

* Avoid reusing temporary variables.
It makes it easier for the optimizer to eliminate the variable entirely.   

## Functions

* Write many functions with no parameters and use `-finline`.
This will simplify the job for the optimizer and increase the chances of certain powerful optimizations to apply.

* Avoid passing many parameters to functions.
Try to minimize the number of bytes passed as parameters and returned as return values.

## Loops

* For `for` loops that use a byte-sized variable and whose body does not involve function calls or further loops,
use a unique iteration variable. Such variable will have a bigger chance of being stored in a CPU register.  
For example:

        byte i
        byte j
        for i,0,until,30 { .... }
        for j,0,until,40 { .... }

    is usually better than:
    
        byte i
        for i,0,until,30 { .... }
        for i,0,until,40 { .... }

* 8080/Z80 only: The previous tip applies also for loops using word-sized variables.

* When the iteration order is not important, use `paralleluntil` or `parallelto`.
The compiler will try to choose the optimal iteration order.

* Since 0.3.18: When the iteration order is not important,
use `for ix,ptr:array` to iterate over arrays of structs.

* 6502 only: When iterating over an array larger than 256 bytes, whose element count is a composite number,
consider splitting it into less-than-256-byte sized slices and use them within the same iteration.
For example, instead of:

        word i
        for i,0,paralleluntil,1000 {
           screen[i] = ' 'scr
        }

    consider:
            
        byte i
        for i,0,paralleluntil,250 { 
            screen[i+000] = ' 'scr
            screen[i+250] = ' 'scr
            screen[i+500] = ' 'scr
            screen[i+750] = ' 'scr
        }
        
    Note that the compiler might do this optimization automatically
    for simpler loops with certain iteration ranges, but it is not guaranteed.

# Arithmetic

* Avoid 16-bit arithmetic. Try to keep calculations 8-bit for as long as you can.
If you can calculate the upper and lower byte of a 16-bit value separately, it's usually better to do so.

* Avoid arithmetic larger than 16-bit.

* Use `nonet` if you are sure that the result of shifting will fit into 9 bits.
Use `nonet` when doing byte addition that you want to promote to a word.

