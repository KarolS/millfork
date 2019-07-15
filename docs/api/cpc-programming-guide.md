[< back to index](../doc_index.md)

### A note about Amstrad CPC

The compiler output is a raw machine code file, which then has to be put on a disk. 
You can do it using [CPCDiskXP](http://www.cpcwiki.eu/index.php/CPCDiskXP), 
[ManageDsk](http://www.cpcwiki.eu/index.php/ManageDsk), 
[iDSK](http://www.cpcwiki.eu/index.php/IDSK), 
or some other tool.

The file has to be loaded from $0400. An example how to put such file onto a disk using CPCDiskXP:

    CPCDiskXP -File FILENAME -AddAmsdosHeader 0400 -AddToNewDsk disk_image.dsk


After putting it on a disk, the file can be run with:

    RUN "!FILENAME"
