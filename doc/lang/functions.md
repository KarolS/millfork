# Function definitions

Syntax:

`[<modifiers>] <return_type> <name> ( <params> ) [@ <address>] { <body> }`

`asm <return_type> <name> ( <params> ) @ <address> extern`

* `<modifiers>`: zero or more of the following:

    * `asm` – the function is written in assembly, not in Millfork (doesn't matter for `extern` functions), 
    see [Using assembly within Millfork programs#Assembly functions](./assembly.md#assembly-functions)
    
    * `inline` – the function should be always inlined, 
    see [Function inlining#Explicit inlining](../abi/inlining.md#explicit-inlining)
    
    * `interrupt` – the function is a hardware interrupt handler
    
* `<return_type>` is a valid return type, see [Types](./types.md)

* `<params>` is a comma-separated list of parameters, in form `type name`. Allowed types are the same as for local variables.

* `<address>` is a constant expression that defines where in the memory the function is or will be located.

* `extern` is a keyword than marks functions that are not defined in the current program, 
but are likely to be available at certain address in memory. 
Such functions should be marked as written in assembly and should have their parameters passed through registers.

* `<body>` is a newline-separated list of either Millfork or assembly statements

