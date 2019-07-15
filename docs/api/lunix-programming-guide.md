[< back to index](../doc_index.md)

### A note about LUnix

LUnix uses relocatable code, which means that object addresses (`.addr`) are not constants. 
To help with this problem, new constants are defined, with a `.rawaddr` suffix. 
They are not relocated, so to use them, you need to manually relocate them 
by adding `relocation_offset` to their high bytes:

    pointer p
    p = variable.rawaddr
    p.hi += relocation_offset
