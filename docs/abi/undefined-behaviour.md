[< back to index](../doc_index.md)

# Undefined behaviour

Since Millfork is only a middle-level programming language and attempts to eschew runtime checks in favour of performance, 
there are many situation when the program may not behave as expected. 
In the following list, "undefined value" means an arbitrary value that cannot be relied upon, 
and "undefined behaviour" means arbitrary and unpredictable behaviour that may lead to anything, 
even up to hardware damage. 

* array overruns: indexing past the end of an array leads to undefined behaviour 

* writing to arrays defined as `const`

* stray pointers: indexing a pointer that doesn't point to a valid object or indexing it past the end of the pointed object leads to undefined behaviour

* reading uninitialized variables: will return undefined values and, if the type is `bool`, may put the program in an invalid state

* reading variables used by return dispatch statements but not assigned a value: will return undefined values

* reading a loop variable after the loop without initializing it again: will return undefined values

* returning a value from a function by return dispatch to a function of different return type: will return undefined values

* passing an index out of range for a return dispatch statement

* stack overflow: exhausting the hardware stack due to excess recursion, excess function calls or excess stack-allocated variables

* on ROM-based platforms: writing to arrays

* on ROM-based platforms: using global variables with an initial value (they will not be initialized!)

* violating the safe assembly rules ([6502](../lang/assembly.md), [8080/LR35902/Z80](../lang/assemblyz80.md))

* violating the [safe reentrancy rules](../lang/reentrancy.md)

* when using modifying operators: calling non-pure functions in the left-hand-side index expression (like in `a[f()] += b`).
Currently, such functions may be evaluated either once or twice. This might be fixed in the future.

* when using modifying operators: calling functions on the right-hand-side index expression than modify any of the variables used on the left hand side

* when using `for` loops operators: calling non-pure functions in the range limits (like in `for i,f(),to,g()`).
Currently, such functions may be evaluated any number of times. This might be fixed in the future.

* jumping across the scope of for loop that uses a fixed list or across functions

* division by zero and modulo by zero

* decimal addition and subtraction of values that are not binary-coded decimals

The above list is not exhaustive.
