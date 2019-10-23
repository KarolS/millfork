[< back to index](../doc_index.md)

# List of keywords

## Normal keywords

### Top level keywords

These keywords can occur at the top level of the file:

    import segment
    array struct union enum alias

### Declaration modifiers

These keywords occur within variable, array and function declarations:

* Variable declaration modifiers: `const volatile register static stack` 

* Function declaration modifiers: `asm inline noinline interrupt kernal_interrupt macro reentrant extern`

* Array declaration modifiers: `const`

* Common modifiers: `segment align`
    
### Statement keywords

These keywords occur within function bodies:

    if else for while do
    goto label
    asm
    return

### Sub-statement keywords

These keywords occur within bodies of certain other statements:

* for-loop directions: `to until downto parallelto paralleluntil`

* special return-dispatch branch: `default`

* loop control flow: `break continue`

## Contextual keywords

These are not considered keywords, but have special meaning in certain syntactical locations:

* special alignment constant: `fast`

* special array function: `file`

* text encoding names: `z defaultz scr scrz`; for the rest, see [the list of text encodings](./text.md)  
`default` is also a name of a text encoding, but it's also a keyword

## Reserved identifiers

Most of reserved identifiers are considered keywords (i.e. you cannot override them, even locally).
Some are not, but overriding them is strongly discouraged and for highest code clarity should also be considered keywords.

The keyword status of those identifiers may change in the future.

### Built-in types

The following identifiers are considered keywords:

    byte ubyte sbyte word long
    pointer void bool     
    set_carry set_zero set_overflow set_negative    
    clear_carry clear_zero clear_overflow clear_negative    
    int16 int24 int32 int40 int 48 int56 int64 int72 int80 int88 int96 int104 int112 int120 int128    
    unsigned8 signed8

`farword` is a deprecated alias for `int24` and it is not a keyword. It will be removed in the future.
    
### Built-in constants

The following identifiers are considered keywords:

    true false
    
The following identifiers are not considered keywords:

    nullptr nullchar
    
### Built-in functions and operators

The following identifiers are considered keywords:

    not hi lo nonet sizeof
    
The following identifiers are not considered keywords:

    sin cos tan call

## Reserved field names

It is not allowed to define a struct or a union with a field with any of the following names:

    pointer addr rawaddr return

## Preprocesor keywords

### Directives

    #if #elseif #else #endif
    #define #use
    #pragma
    #fatal #error #warn #info #infoeval
    
### Built-in functions

    not hi lo defined if