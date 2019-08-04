# PatternT

Symbolic calculator / Macros processor  
Reduces expressions using pattern-based rules  

This is a library, [example compiler](https://github.com/Donaim/hacalc) and [tests](https://github.com/Donaim/PatternT-tests) have their own respository  
The library expects from compiler to add builtin rules that cannot be expressed as patterns, for example addition of numerical symbols  

## Examples

rules:  
```
a or a -> a
a and a -> a
a or (not a) -> 1
a and (not a) -> 0
1 or x -> 1
1 and x -> x
0 or x -> x
0 and x -> 0
not (not a) -> a
not 0 -> 1
not 1 -> 0
```
produce:
```
x or x
>>> x

0 and x
>>> 0

(0 and x) or 1
>>> 1

0 and (x or x)
>>> 0

(0 and x) and y
>>> 0

not ((0 and x) and y)
>>> 1

(not ((0 and x) and x)) and x
>>> x

y or ((not ((0 and x) and y)) and x)
>>> y or x
```

For more examples, see match tests: https://github.com/Donaim/PatternT-tests/blob/master/examples/test1  
But they also include compiler specific cases  

## Tests

Tests are being developed separately: https://github.com/Donaim/PatternT-tests  
