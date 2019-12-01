[< back to index](../doc_index.md)

# Macros and inlining

## Macros

`macro` keyword

## Automatic inlining

You can control inlining behavior in several ways:

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
