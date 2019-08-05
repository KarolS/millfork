[< back to index](../doc_index.md)

# Function definitions

Syntax:

`[segment (<segment>)] [<modifiers>] <return_type> <name> ( <params> ) [align ( <alignment> )] [@ <address>] { <body> }`

`[segment (<segment>)] [<modifiers>] <return_type> <name> ( <params> ) [align ( <alignment> )] [@ <address>] = <expression>`

`[segment (<segment>)] asm <return_type> <name> ( <params> ) @ <address> extern`

Examples:

    void do_nothing() { }
    inline byte two() = 2
    extern asm void chkout(byte a) @ $FFD2
    segment(prgrom0) void main_loop(word w, byte x) align(fast) { // body omitted
    

* `<segment>`: segment name; if absent, then defaults to `default_code_segment` as defined for the platform (usually `default`)

* `<modifiers>`: zero or more of the following:

    * `asm` – the function is written in assembly, not in Millfork (obligatory for `extern` functions), 
    see [Using 6502 assembly within Millfork programs#Assembly functions](./assembly.md#assembly-functions)
    or [Using 8080/LR35902/Z80 assembly within Millfork programs#Assembly functions](./assemblyz80.md#assembly-functions);  
    for 8086, see the [8086 support disclaimer](./x86disclaimer.md).
    
    * `macro` – the function is a macro, 
    see [Macros_and inlining#Macros](../abi/inlining.md#macros)
    
    * `inline` – the function should preferably be inlined
    see [Macros_and inlining#Inlining](../abi/inlining.md#automatic-inlining)
    
    * `noinline` – the function should never be inlined
    
    * `interrupt` – the function is a hardware interrupt handler.
    You are not allowed to call such functions directly.
    The function cannot have parameters and the return type should be `void`.
    
    * `kernal_interrupt` – the function is an interrupt handler called from a generic vendor-provider hardware interrupt handler.
    The hardware instruction handler is assumed to have preserved the CPU registers,
    so this function only has to preserve the zeropage pseudoregisters.
    An example is the Commodore 64 interrupt handler that calls the function at an address read from $314/$315.
    Unlike hardware handlers with `interrupt`, you can treat functions with `kernal_interrupt` like normal functions.  
    On non-6502-based targets, functions marked as `kernal_interrupt` don't differ from normal functions.
    
* `<return_type>` is a valid return type, see [Types](./types.md)

* `<params>` is a comma-separated list of parameters, in form `type name`. Allowed types are the same as for local variables.
For assembly functions, certain parameter names are interpreted as CPU registers.

* `<alignment>` is either a numeric literal that is a power of 2, or keyword `fast`.
    The function will be allocated at the address divisible by alignment.
    `fast` means different things depending on the target platform:

    * on 6502, it means that the function will not cross a page boundary if possible
    * on Z80, it is ignored   

* `<address>` is a constant expression that defines where in the memory the function is or will be located.

* `extern` is a keyword than marks functions that are not defined in the current program, 
but are likely to be available at certain address in memory. 
Such functions should be marked as written in assembly and should have their parameters passed through registers.

* `<body>` is a newline-separated list of either Millfork or assembly statements

* `<expression>` is an expression. It is equivalent to a function body of form `{ return <expression> }`.  

The address of an non-macro function `f` is a constant `f.addr`. 

Non-macro, non-interrupt functions which have max one parameter of size max 2 bytes
and return `void` or a value of size max 2 bytes,
can be accessed via a pointer.

    void f() {}
    
    function.void.to.void p = f.pointer
    
    call(p)

The value of the pointer `f.pointer` may not be the same as the value of the function address `f.addr`. 
