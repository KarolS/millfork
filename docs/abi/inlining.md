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

