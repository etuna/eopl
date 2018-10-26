#lang eopl
; Add to the defined language (LET) a facility that adds a cond expression.
; Expression ::= cond {Expression ==> Expression}* end
; 
; In this expression, the expressions on the left-hand sides of the '==>'s 
; are evaluated in order until one of them returns a true value. Then the value
; of the entire expression is the value of the corresponding right-hand expression.
; If none of the tests succeeds, the expression should report an error.
;
; Here are the steps for adding this expression:
; 1- define the grammar (inside the definition -a-)
;
; Expression ::= cond Variants end
; Variants ::== {Expression ==> Expression}Variants | null
;
; 2- add the syntax variant (to the definition -b-)
; 3- add the behavior variant (to the definition -c-)
;
; For testing: Add the following expressions to the end of this file:
; (eopl:printf "~a\n" (number->string (expval->num (run "let x = 6 in let y = 5 in cond {zero?(-(x,6)) ==> -(x,1)}{zero?(-(y,80)) ==> -(y,34)} end"))))
; (eopl:printf "~a\n" (number->string (expval->num (run "let x = 6 in let y = 5 in cond {zero?(-(x,5)) ==> -(x,1)}{zero?(-(y,5)) ==> -(y,34)} end"))))
; (eopl:printf "~a\n" (number->string (expval->num (run "let x = 8 in let y = cond {zero?(-(2,3)) ==> 8}{zero?(-(1,1)) ==> 9} end in -(x,y)"))))
; (eopl:printf "~a\n" (number->string (expval->num (run "cond {zero?(-(6,6)) ==> let x = 9 in cond {zero?(1) ==> -(x,4)}{zero?(-(x,9)) ==> -(3,x)}{zero?(-(x,3)) ==> -(-(x,2),1)} end} end"))))
;
; Good luck.

(define-datatype program program?
  (a-program
   (exp1 expression?)))

; -b- syntax definition
(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var symbol?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (eopl:printf "Expression value extraction error: num")))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (eopl:printf "Expression value extraction error: bool")))))

(define scanner-spec-let
  '((comment ("%" (arbno (not #\newline))) skip)
    (symbol (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

; -a- grammar definition
(define grammar-let
  '((program
     (expression)
     a-program)
    (expression
     (number)
     const-exp)
    (expression
     ("-(" expression "," expression ")")
     diff-exp)
    (expression
     ("zero?(" expression ")")
     zero?-exp)
    (expression
     ("if " expression " then " expression " else " expression)
     if-exp)
    (expression
     (symbol)
     var-exp)
    (expression
     ("let " symbol " = " expression " in " expression)
     let-exp)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-let grammar-let))

(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (eopl:printf "No binding found for ~s" search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (eopl:printf "Invalid environment!")))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (empty-env))))))

; -c- behavior definition
(define value-of 
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp1 body)
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                  (extend-env var val1 env)))))))

;; a few outputs for testing
(eopl:printf "Trying to make some calculations...\n")
; 6
(eopl:printf "~a\n" (number->string (expval->num (run "-(8,2)"))))
; 3
(eopl:printf "~a\n" (number->string (expval->num (run "let i = 1 in let v = 5 in let x = 10 in -(-(x,3),-(v,i))"))))
; 18
(eopl:printf "~a\n" (number->string (expval->num (run "let x = 33 in let y = 22 in if zero?(-(x,11)) then -(y,2) else -(y,4)"))))
; -5
(eopl:printf "~a\n" (number->string (expval->num (run "let x = 7 in let y = 2 in let y = let x = -(x,1) in -(x,y) in -(-(x,8),y)"))))

