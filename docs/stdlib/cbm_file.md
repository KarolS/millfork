[< back to index](../doc_index.md)

## cbm_file

The `cbm_file` module provides support for loading and saving files to tape and disk.
Currently, it works only for Commodore 64 targets, although support for more targets is coming.
It uses Kernal routines, so it requires the Kernal ROM to be enabled.

#### byte last_used_device()

Returns the last device number, or 8 if none.

#### void load_file(byte device, pointer name)

Loads a PRG file with the given null-terminated name to the address specified in the file.
Sets `errno`.

#### void load_file_at(byte device, pointer name, pointer addr)

Loads a PRG file with the given null-terminated name to the given address.
Sets `errno`.

#### void save_file(byte device, pointer name, pointer start, word length)

Saves `length` bytes starting from `start` to a PRG file named `name` on device `device`.
Sets `errno`.

#### void exec_disk(byte device, pointer command)

Executes a CBM DOS command on the given drive.

#### void delete_file(byte device, pointer name)

Deletes given file in the given drive. (`S0:`)

#### void rename_file(byte device, pointer old_name, pointer new_name)

Renames given file in the given drive. (`R0:`)

#### void copy_file(byte device, pointer old_name, pointer new_name)

Copies given file in the given drive. (`C0:`)

#### void initialize_disk(byte device)

Reinitialized disk status in the given drive. (`I0`)

#### void validate_disk(byte device)

Validates disk status in the given drive. (`V0`)

#### void format_disk(byte device)

Formats disk status in the given drive. (`N0:`)

#### void open_file(byte device, pointer name, byte fd, byte mode)

Opens a file.
Sets `errno`.   
TODO: buggy.

#### const byte MODE_READ = 0
#### const byte MODE_WRITE = 1

#### void close_file(byte fd)

Closes the given file.
Sets `errno`.   
TODO: buggy.

#### byte getbyte_safe()

Reads a byte from file.
Sets `errno` to `err_eof` when reading the last byte, so don't abort reading when getting `errno != err_ok`.  
TODO: buggy.

#### void putbyte_safe(byte b)

Writes a byte from file.
Sets `errno`.   
TODO: buggy.
