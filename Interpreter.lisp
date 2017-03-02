; Top level function for the fl language interpreter.
; The interpreter will evaluate an expression 'E' using
; built-in functions as well as user defined functions in the list 'P'.
;
; This function was started using the provided 'Skeleton' function found on e-class 
;
; ex:
;	(fl-interp '(+ 10 5) nil) -> 15
;	(fl-interp '(square 4) '((square x = (* x x)))) -> 16
;
;	Note that the interpreter will select the correct function based on the number of arguments
;	(fl-interp '(f 1)   '((f x y = (t)) (f x = (nil)))) -> (nil)
;	(fl-interp '(f 1 2) '((f x y = (t)) (f x = (nil)))) -> (t)
;
;	The function body can be an atom as well
;	(fl-interp '(f 1) '((f x = 5))) -> 5
;
(defun fl-interp (E P)
	(cond 
		((atom E) E)   ; this includes the case where expr is nil
        	(t
           		(let ( (f (car E))  (arg (cdr E)) (fn-def (find-fn (car E) (cdr E) P)) )
	      		(cond
                		; handle built-in functions
				((eq f '+)         (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((eq f '-)         (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((eq f '*)         (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((eq f '>)         (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((eq f '<)         (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((eq f '=)         (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((eq f 'first)     (car (fl-interp (car arg) P)))
				((eq f 'rest)      (cdr (fl-interp (car arg) P)))
				((eq f 'cons)      (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((eq f 'null)      (null (fl-interp (car arg) P)))
				((eq f 'atom)      (atom (fl-interp (car arg) P)))
				((eq f 'eq)        (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((eq f 'equal)     (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((eq f 'isnumber)  (numberp (fl-interp (car arg) P)))
				((eq f 'not)       (not (fl-interp (car arg) P)))

				;short circuit and if the first argument evaluates to false
				((eq f 'and)
					(if (eq t (fl-interp (car arg) P))
						(fl-interp (cadr arg) P)
						nil
				   	)
				)

				;short circuit or if the first agrument evaluates to true
				((eq f 'or)
					(if (eq t (fl-interp (car arg) P))
						t
						(fl-interp (cadr arg) P)
					)
				)

				;evaluate if condition first
				((eq f 'if)
					(if (eq t (fl-interp (car arg) P)) 
						(fl-interp (cadr arg) P)
						(fl-interp (cadr (cdr arg)) P)
					)
				)

				; if f is a user-defined function,
				;    then evaluate the parameters 
				;         and apply f to the evaluated parameters 
				;             (applicative order reduction) 
				((not (null fn-def))
					(fl-interp
						(replace-fn-args
							(get-vars fn-def) ; get function variable names
							(mapcar (function (lambda (z) (fl-interp z P))) arg) ; evaluate function params
							(get-body fn-def) ; get function body
						)
						P
					)
				)

				; otherwise f is undefined; in this case,
				; E is returned as if it is quoted in lisp
				(t E)

			) ; end cond
			) ; end let
		) ; end t
	) ; end cond
) ; end fl-interp

; find a function in the P list with a specific name N and same argument count as ARG
; ex:
;	(find-fn 'subtract '(1 2) '((add x y = (+ x y)) (subtract x y = (- x y)))) -> (subtract x y = (- x y))
;
(defun find-fn (N ARG P)
	(if (null P)
		nil
		(if
			(and
				(eq N (car (car P)))  ; function name matches
				(eq (xcount ARG) (xcount (get-vars (car P)))) ; arg count matches
			)
			(car P) ; return funtion declaration
			(find-fn N ARG (cdr P)) ; otherwise look at next function definition
		)
	)
)

; get variables from funciton definition
; ex:
;	(get-vars '(add x y = (+ x y))) -> (x y)
;
(defun get-vars (F)
	; we dont want the function name as a variable, so we skip it with cdr
	(inner-get-vars (cdr F) nil)
)

; inner function for geting variables from function definition
; ex:
;	(inner-get-vars '(x y = (+ x y)) nil) -> (x y)
; 
(defun inner-get-vars (F L)
	(if (eq (car F) '=)
		L ; if we find the "=" sign in the fn-definition, we have consumed all variables
		(inner-get-vars (cdr F) (append L (list (car F))))
	)
)

; get function body from function definition
; ex:
;	(get-body '(add x y = (+ x y))) -> (+ x y)
;
(defun get-body (F)
	; the body should always be the last element in the function definition,
	; so we just iterate through the list until we find the end.
	(if (null (cdr F))
		(car F)
		(get-body (cdr F))
	)
)

; replace function arguments in the body with the passed in parameters
; we assume length(ARGS) == length(PARAMS)
; ex:
;	(replace-args '(x y) '(1 2) '(+ x y)) -> (+ 1 2)
;
(defun replace-fn-args (ARGS PARAMS BODY)
	(if (null ARGS)
		BODY ; we have no args left to replace, return the body
		(replace-fn-args
			(cdr ARGS)
			(cdr PARAMS)
			(replace-arg (car ARGS) (car PARAMS) BODY)
		)
	)
)

; Replace argument (A) in function body (B) with the value (E)
; ex:
;	(replace-arg 'X 4 '(+ X 3)) -> (+ 4 3)
;	(replace-arg 'Y 5 '(+ Y (- Y 3)) -> (+ 5 (- 5 3))
;
(defun replace-arg (A E B)
	(cond
		((null B) nil)
		((atom B)
			; if the body is an atom, we want to replace it with a variable value
			; if it matches, otherwise we return the body atom
			(if (eq B A)
				E
				B
			)
		)
		((eq (car B) A)
			; we found a match! replace the variable with our value
			(cons E (replace-arg A E (cdr B))))
		((not (atom (car B)))
			; we found a sub list, we must drill into it to replace any variables inside
			(cons (replace-arg A E (car B)) (replace-arg A E (cdr B)))
		)
	        (t 
			; didn't find the variable, continue
			(cons (car B) (replace-arg A E (cdr B)))
		)
	)
)

; count number of elements in a list
; ex:
;	(xcount '(1 2 3)) -> 3
;	(xcount nil) -> 0
;
(defun xcount (L)
	(if (null L)
		0
		(+ (xcount (cdr L)) 1)
	)
)




	
