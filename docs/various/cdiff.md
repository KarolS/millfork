[< back to index](../doc_index.md)

# Differences from C

## Syntax

* Block comments `/* */` are not supported, use line comments `//`.

* You cannot put multiple statements on one line.  
Semicolons are allowed at the end of the line, but no code can follow them.

* There are no `++` or `--` operators. Use `+= 1` and `-= 1`.

* Pointer types are declared using the `pointer.` prefix, not `*` suffix. To dereference them, use `p[0]` instead of `*p`.

* There is no unary `&` operator.
Pointers to an object are acquired using the `.pointer` suffix.
Raw addresses are acquired using the `.addr` suffix.
The numeric values of the pointer and of the raw address may differ.

* Operator precedence works differently.
Bitwise and bitshift operators have the same precedence as arithmetic operators,
and mixing different operators with the same precedence is usually forbidden.
This prevents most ambiguities in bit-twiddling code, but requires care when porting code from or to C. 

* There is no `!` operator. The negation is expressed as the `not` function.

* The modulo operator is written `%%`. As with `/`, it's only defined for unsigned division. 

* The `for` loops are range-based. Arbitrary, C-like `for` loops are not supported.

* Variable declaration and initialization have to be separate.

* Integer literals starting with zero and containing just digits are decimal, not octal.
For octal literals, use the `0o` prefix.

* String literals are not null-terminated by default. Use the `z` suffix for null-terminated strings.

* In `if`, `do/while`, `while` and `for` statements, parentheses are not required, but braces are.
The `else` branch also requires braces, unless the only statement in the `else` block is an `if` statement.

* There has to be a space between many operators and character literals,
as the parser may treat the apostrophe as a part of the operator.

## Preprocessor

* The preprocessor cannot expand symbols to something more complex than an identifier or a literal.

* The preprocessor cannot define symbols for use in other files.

* The closest to C's `#define` is `#use`.
Unlike `#define`, such definitions are not visible within the preprocessor.

* Directives like `#use` and `#pragma` affect the entire file, not just the code after the directive.

* The preprocessor cannot include files.

## Semantics

* There is no automatic integer promotion. An operation of two bytes will yield a byte. For example:

        byte a
        byte b
        word w
        w = a * b

    may yield unexpected results. To prevent this, cast at least one argument:
    
        w = word(a) * b

    This issue applies mostly to the `*` and `<<` operators.

* There is no padding in structs.


    

  
