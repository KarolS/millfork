[< back to index](../doc_index.md)

# Interfacing with external code 

## Calling external functions at a static address

To call an external function, you need to declare it as `asm extern`. For example:

```
asm void putchar(byte a) @$FFD2 extern
```

The function parameter will be passed via the accumulator,
the function itself is located in ROM at $FFD2. A call like this:

```
putchar(13)
```

will be compiled to something like this on 6502:

```
LDA #13
JSR $FFD2
```

For more details about how to pass parameters to `asm` functions, see:

* for 6502: [Using 6502 assembly within Millfork programs#Assembly functions](./assembly.md#assembly-functions).

* for Z80: [Using Z80 assembly within Millfork programs#Assembly functions](./assemblyz80.md#assembly-functions).

## Calling external functions at a dynamic address

To call a function that has its address calculated dynamically, 
you just need to do the same as what you would do in assembly; 6502 example:

```
asm void call_function(byte a) {
    JMP (function_address)
}
```

where `function_address` is a variable that contains the address of the function to call.