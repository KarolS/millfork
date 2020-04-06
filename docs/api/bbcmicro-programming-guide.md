[< back to index](../doc_index.md)

### A note about BBC Micro

The default configuration file puts the start address for the program at $0E00.

The compiler outputs two files: a raw machine code file without an extension and a `.inf` file with file metadata.
To use the file, you need to put it on a disk or a disk image.
You can for example use tools like BBC Disk Explorer (GUI) or MkImg (command line):

    MkImg disk_image.ssd output_directory

After putting it on a disk, the file can be run with:

    *RUN "FILENAME"
    
Currently, multipart BBC Micro programs are not supported.
