[< back to index](../doc_index.md)

# Apple II-oriented modules

## apple2_prodos module

This module provides basic support for issuing ProDOS calls.
It assumes ProDOS has been loaded normally before your program has started.
The API closely follows the ProDOS machine language interface.
See the 
[ProDOS 8 Technical Reference Manual](http://www.easy68k.com/paulrsm/6502/PDOS8TRM.HTM)
for more details, such as the error code values returned.
The following functions are currently implemented:

#### void prodos_read_block(byte unit, pointer data_buffer, word block_number)

Read the specified block from device `unit` into `data_buffer`.
`data_buffer` must be page-aligned.

#### void prodos_write_block(byte unit, pointer data_buffer, word block_number)

Write the specified block from device `unit` into `data_buffer`.
`data_buffer` must be page-aligned.

#### void prodos_close(byte rnum)

Close file referred to by reference `rnum`.
ProDOS will free the associated buffers and flush all changes to disk if necessary.

#### void prodos_flush(byte rnum)

Flush any changes to disk for file referred to by reference `rnum`.

#### void prodos_get_prefix(pointer filename)

Takes a pointer to a 64-byte buffer.
ProDOS will fill this with the current path prefix as a Pascal-style string.

#### void prodos_set_prefix(pointer filename)

Sets or modifies the ProDOS prefix.
Takes a pointer to a Pascal-style string.

#### void prodos_create(pointer filename, byte filetype)

Create a file.
`filename` is a pointer to a Pascal-style string.
`filetype` is one of the standard ProDOS file types.
See the ProDOS manual for more details.
This *must* be called before a file can be opened or written.
ProDOS does not create files implicitly.

#### void prodos_destroy(pointer filename)

Delete a file.

#### void prodos_rename(pointer filename, pointer new_filename)

Rename a file.

#### byte prodos_open (pointer filename, pointer buffer)

Open a file.
`filename` is a pointer to a Pascal-style string containing the path of file to be opened.
Buffer is a 1024 byte I/O buffer used internally by ProDOS.
The buffer must be page-aligned.
This call returns a byte which is the ProDOS file handle. 
This handle is used by other calls.

A minimal example:

    byte file_handle
    array io_buffer [1024] align (256)
    file_handle = prodos_open("myfile"p, io_buffer)
    prodos_close(file_handle)

Files must exist to be opened.
Use the `prodos_create` call to create a file first if necessary.

#### void prodos_newline(byte rnum, byte mask, byte newline_char)

Set the ProDOS newline character and mask.
See ProDOS manual for details.

#### void prodos_read(byte rnum, pointer data_buffer, word read_count)

Read the number of bytes specified by `read_count` into `data_buffer` from file handle `rnum`.

#### void prodos_write(byte rnum, pointer data_buffer, word write_count)

Write the number of bytes specified by `write_count` from `data_buffer` to file handle `rnum`.