# Function definitions

Syntax:

`[<modifiers>] <return_type> <name> ( <params> ) [@ <address>] { <body> }`

`asm <return_type> <name> ( <params> ) @ <address> extern`

* `<modifiers>`: zero or more of the following:

    * `asm` – the function is written in assembly, not in Millfork (obligatory for `extern` functions), 
    see [Using assembly within Millfork programs#Assembly functions](./assembly.md#assembly-functions)
    
    * `macro` – the function is a macro, 
    see [Macros_and inlining#Macros](../abi/inlining.md#macros)
    
    * `inline` – the function should preferably be inlined
    see [Macros_and inlining#Inlining](../abi/inlining.md#automatic_inlining.md)
    
    * `noinline` – the function should never be inlined
    
    * `interrupt` – the function is a hardware interrupt handler.
    You are not allowed to call such functions directly.
    The function cannot have parameters and the retrn type should be `void`.
    
    * `kernal_interrupt` – the function is an interrupt handler called from a generic vendor-provider hardware interrupt handler.
    The hardware instruction handler is assumed to have preserved the CPU registers,
    so this function only has to preserve the zeropage pseudoregisters.
    An example is the Commodore 64 interrupt handler that calls the function at an address read from $314/$315.
    Unline hardware handlers with `interrupt`, you can treat functions with `kernal_interrupt` like normal functions. 
    
* `<return_type>` is a valid return type, see [Types](./types.md)

* `<params>` is a comma-separated list of parameters, in form `type name`. Allowed types are the same as for local variables.

* `<address>` is a constant expression that defines where in the memory the function is or will be located.

* `extern` is a keyword than marks functions that are not defined in the current program, 
but are likely to be available at certain address in memory. 
Such functions should be marked as written in assembly and should have their parameters passed through registers.

* `<body>` is a newline-separated list of either Millfork or assembly statements

