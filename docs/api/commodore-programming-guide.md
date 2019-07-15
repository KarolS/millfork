[< back to index](../doc_index.md)

### A note about Commodore 64

#### Multifile programs

A multifile program is a program stored on a disk that consists of the main program file that is executed first
and several other files that can be loaded into memory on demand.
This allows for creating programs that are larger than 64 kilobytes.

Millfork allows building such programs, but leaves several things to the programmer:

* tracking of which parts of the program are currently loaded and which are not

* whether memory ranges of various parts overlap or not

* whether loading succeeded or not

##### Writing multifile programs

You will need to create a platform definition file with multiple segments.
The default segment should start at $80D.
Example:

    segments=default,extra
    ; the first file will contain the initial code:
    default_code_segment=default
    segment_default_start=$80D
    segment_default_codeend=$7fff
    segment_default_datastart=after_code
    segment_default_end=$7fff
    ; the second file will contain the extra code:
    segment_extra_start=$8000
    segment_extra_codeend=$9fff
    segment_extra_datastart=after_code
    segment_extra_end=$cfff

You also need to set `style=per_segment` in the `[output]` section.

Annotate things you want to place in that file with the `segment` keyword:

    segment(extra)
    void extra_function () {
        //
    }

Then in your code you need to load the file:

    load_file(last_used_device(), "eee"z)
    if errno == err_ok {
        extra_function()
    } else {
        // handle error
    }

(Prefer `last_used_device()` instead of hardcoded `8`, so your program will work when loaded from any disk drive.)

Compiling the program with `-o OUTPUT` will yield several PRG files.
The default segment will be in `OUTPUT.prg`, the segment called `extra` in `OUTPUT.extra.prg` and so on.

##### Packaging multifile programs

The Millfork compiler does not create Commodore disk images.

You can use a variety of tools to perform that task,
for example the `c1531` tool shipped with [the VICE emulator](http://vice-emu.sourceforge.net/).

To create a new disk image for the last example, use:

    c1541 -format "example,01" d64 example.d64 -write OUTPUT.prg start -write OUTPUT.extra.prg eee
