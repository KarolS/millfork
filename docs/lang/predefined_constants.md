[< back to index](../doc_index.md)

# Predefined constants

* `byte nullchar` – the null terminator for strings in the default encoding, equivalent to `""z[0]`

* `null$ nullptr` – the invalid pointer value; the value of the `NULLPTR` feature

* `bool true`, `bool false` – boolean constants

* `pointer segment.N.start` – the value of `segment_N_start` from the platform definition

* `pointer segment.N.end` – the value of `segment_N_end` from the platform definition

* `pointer segment.N.heapstart` – the address of the first byte in the `N` segment that was not automatically allocated

* `word segment.N.length` – the number of byte locations between `segment_N_start` and `segment_N_end`, inclusive

* `byte segment.N.bank` – the value of `segment_N_bank` from the platform definition
