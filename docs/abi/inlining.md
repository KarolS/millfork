[< back to index](../doc_index.md)

# Macros and inlining

## Macros

Functions defined with the `macro` keyword are not actual functions, but they are used as syntax replacements.

It implies the following:

* macros must return `void`

* cannot be `inline`, `noinline` or `extern`

* cannot contain variable or array declarations

* can be `asm` - in this case, they should **not** end with a return instruction

* do not have an address

* their invocations cannot be used as expressions

* in case of `asm` macros, the parameters:

    * must be defined as either `const` (compile-time constants), `ref` (variables) or `register(XX)` (registers, where XX is the register you want to use)
    
    * at most one parameter can be defined as a register

* in case of non-`asm` macros, the parameters

    * must be defined as either `ref` (variables; default, may be omitted) `const` (compile-time constants), or `call` (expressions, which are evaluated every time they are used)
    
    * `ref` parameters exceptionally can have their type declared as `void`; such parameters accept variables of any type
    
    * `call` parameters exceptionally can have their type declared as `void`;
    such parameters accept expressions of any type, including `void`, however, you cannot assign from those expressions

* macros do not have their own scope (they reuse the scope from their invocations) – exceptions: the parameters and the local labels defined in assembly

* control-flow statements (`break`, `continue`, `return`, `goto`, `label`) are run as if places in the caller function

When invoking a macro, you need to pass variables as arguments to parameters annotated with `ref` and constants as arguments annotated with `const`.

Invoking a non-`asm` macro requires the types of variables via `ref` parameters to match precisely.
No type conversions are performed.
Exception: parameters of type `void` can accept a variable of any type.

For parameters defined as `const`, `register(XX)` or `call`, the usual type conversions are performed.

You can invoke a macro from assembly, by preceding the invocation with `+`

Examples:

    macro void inc_x() {
        x += 1
    }
    byte add_two_1(byte x) {
        inc_x()
        inc_x()
        return x
    }
    
    macro void inc(byte b) {
        b += 1
    }
    byte add_two_2(byte x) {
        inc(x)
        inc(x)
        return x
    }
    macro void perform_twice(void call f) {
        f
        f
    }
    byte add_two_3(byte x) {
        perform_twice(inc(x))
        return x
    }
    
    macro void add(byte b, byte v) {
        b += v
    }
    macro void retu(byte result) {
        return result
    }
    byte add_two_4(byte x) {
        add(x, 2)
        retu(x)
    }
    
    macro asm byte add_asm(byte ref b, byte const v) {
        LDA b
        CLC
        ADC #v
        STA b
        // no RTS!
    }
    byte add_two_5(byte x) {
        add_asm(x, 2)
        return x
    }
    
    macro asm byte add_asm_2(byte ref b, byte register(x) v) {
        TXA
        CLC
        ADC b
        STA b
        // no RTS!
    }
    byte add_two_6(byte x) {
        add_asm_2(x, 2)
        return x
    }
    

## Automatic inlining

You can control inlining behavior in several ways:

* functions declared with the `const` keyword called with constant arguments will always be inlined,
with the whole invocation being converted into a single constant, regardless of `inline` and `noinline` keywords;
calls with non-constant arguments are subject to the regular rules.

* functions declared with the `inline` keyword will be inlined if possible

* functions declared with the `noinline` keyword will never be inlined

* the remaining functions may be inlined only if the `-finline` command-line option is enabled
and the compiler decides the function is worth doing

## Automatic subroutine extraction

Subroutine extraction is the opposite of inlining.

When given the `-fsubroutine-extraction`, the compiler will attempt to extract common code fragments to new subroutines.
The code will get smaller and slower.

Generally, when using `-fsubroutine-extraction`, it's recommended to also use `-finline`.
This allows the compiler to first inline and optimize code and then extract it back when appropriate.
