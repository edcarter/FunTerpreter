# FunTerpreter
Interpreter for a LISP like language, based off of a project for a functional programming class

## Primitive Functions
```
(if x y z)
(null x)
(atom x)
(eq x y)
(first x)
(rest x)
(cons x y)
(equal x y)
(isnumber x)
(+ x y)
(- x y)
(* x y)
(> x y) 
(< x y) 
(= x y) 
(and x y)
(or x y)
(not x)
```

## Calling Convention
```
(fl-interp [program] [user_functions])

[user_functions] is a list of sub-programs,
for example: ( (multiply x y = (* x y)) (double x = (multiply x 2)) )
```

## Examples
```lisp
(fl-interp '(+ 2 5) nil) -> 7
(fl-interp '(isnumber 10) -> T
(fl-interp '(square 5) '((square x = (* x x)))) -> 25
```
