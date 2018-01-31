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

will be compiled to something like this:

```
LDA #13
JSR $FFD2
```

For more details about how to pass parameters to `asm` functions,
see [Using assembly within Millfork programs#Assembly functions](./assembly.md#assembly-functions).

## Calling external functions at a dynamic address

To call a function that has its address calculated dynamically, 
you just need to do the same as what you would do in assembly:  

```
asm void call_function(byte a) {
    JMP (function_address)
}
```

where `function_address` is a variable that contains the address of the function to call.