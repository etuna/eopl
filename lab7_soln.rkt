#lang eopl

; Extend the lexical address translator and interpreter to support the following expressions:
; (Please note that this interpreter uses lexical address translator.)
;
; *(a,b) -> returns  a x b
; /(a,b) -> returns a / b
; +(a,b) -> returns a + b
; equal?(a,b) -> returns if a = b as boolean
; less?(a,b) -> returns if a < b as boolean
; greater?(a,b) -> returns if a > b as boolean
; print(x) -> prints out x and returns 1

; env

(define (empty-senv)
  '())

(define (extend-senv var senv)
  (cons var senv))

(define (apply-senv senv var)
  (cond ((null? senv) (eopl:error 'apply-senv "Unbound variable: ~a" var))
        ((eqv? var (car senv)) 0)
        (else (+ 1 (apply-senv (cdr senv) var)))))

(define (nameless-environment? x)
  ((list-of expval?) x))

(define (empty-nameless-env)
  '())

(define (extend-nameless-env val nameless-env)
  (cons val nameless-env))

(define (apply-nameless-env nameless-env n)
  (list-ref nameless-env n))

; parser

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (minuend expression?)
   (subtrahend expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (div-exp
   (exp1 expression?)
   (exp2 expression?))
  (mult-exp
   (exp1 expression?)
   (exp2 expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (print-exp
   (exp1 expression?))
  (zero?-exp
   (expr expression?))
  (if-exp
   (predicate expression?)
   (consequent expression?)
   (alternative expression?))
  (var-exp
   (var symbol?))
  (let-exp
   (var symbol?)
   (value expression?)
   (body expression?))
  (proc-exp
   (var symbol?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (nameless-var-exp
   (num integer?))
  (nameless-let-exp
   (exp1 expression?)
   (body expression?))
  (nameless-proc-exp
   (body expression?)))

(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("*" "(" expression "," expression ")") mult-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("print" "(" expression ")") print-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("(" expression expression ")") call-exp)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

; The evaluator
(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-nameless-env nameless-environment?)))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (body saved-nameless-env)
               (value-of body (extend-nameless-env val saved-nameless-env)))))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))

(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (eopl:error 'expval->num "Invalid number: ~s" val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (eopl:error 'expval->bool "Invalid boolean: ~s" val))))

(define (expval->proc val)
  (cases expval val
    (proc-val (proc) proc)
    (else (eopl:error 'expval->proc "Invalid procedure: ~s" val))))

(define (translation-of expr senv)
  (cases expression expr
    (const-exp (num) (const-exp num))
    (diff-exp (minuend subtrahend)
              (diff-exp
               (translation-of minuend senv)
               (translation-of subtrahend senv)))
    (add-exp (exp1 exp2)
             (add-exp
              (translation-of exp1 senv)
              (translation-of exp2 senv)))
    (mult-exp (exp1 exp2)
              (mult-exp
               (translation-of exp1 senv)
               (translation-of exp2 senv)))
    (div-exp (exp1 exp2)
             (div-exp
              (translation-of exp1 senv)
              (translation-of exp2 senv)))
    (equal?-exp (exp1 exp2)
                (equal?-exp
                 (translation-of exp1 senv)
                 (translation-of exp2 senv)))
    (less?-exp (exp1 exp2)
               (less?-exp
                (translation-of exp1 senv)
                (translation-of exp2 senv)))
    (greater?-exp (exp1 exp2)
                  (greater?-exp
                   (translation-of exp1 senv)
                   (translation-of exp2 senv)))
    (print-exp (exp1)
               (print-exp (translation-of exp1 senv)))
    (zero?-exp (arg)
               (zero?-exp (translation-of arg senv)))
    (if-exp (predicate consequent alternative)
            (if-exp
             (translation-of predicate senv)
             (translation-of consequent senv)
             (translation-of alternative senv)))
    (var-exp (var)
             (nameless-var-exp (apply-senv senv var)))
    (let-exp (var value-exp body)
             (nameless-let-exp
              (translation-of value-exp senv)
              (translation-of body (extend-senv var senv))))
    (proc-exp (var body)
              (nameless-proc-exp
               (translation-of body (extend-senv var senv))))
    (call-exp (rator rand)
              (call-exp
               (translation-of rator senv)
               (translation-of rand senv)))
    (else
     (eopl:error 'translation-of "Cannot translate ~a" expr))))

(define (value-of expr nenv)
  (cases expression expr
    (const-exp (num) (num-val num))
    (diff-exp (minuend subtrahend)
              (let ((minuend-val (value-of minuend nenv))
                    (subtrahend-val (value-of subtrahend nenv)))
                (let ((minuend-num (expval->num minuend-val))
                      (subtrahend-num (expval->num subtrahend-val)))
                  (num-val
                   (- minuend-num subtrahend-num)))))
    (add-exp (exp1 exp2)
             (let ((exp1-val (value-of exp1 nenv))
                   (exp2-val (value-of exp2 nenv)))
               (let ((exp1-num (expval->num exp1-val))
                     (exp2-num (expval->num exp2-val)))
                 (num-val
                  (+ exp1-num exp2-num)))))
    (mult-exp (exp1 exp2)
              (let ((exp1-val (value-of exp1 nenv))
                    (exp2-val (value-of exp2 nenv)))
                (let ((exp1-num (expval->num exp1-val))
                      (exp2-num (expval->num exp2-val)))
                  (num-val
                   (* exp1-num exp2-num)))))
    (div-exp (exp1 exp2)
             (let ((exp1-val (value-of exp1 nenv))
                   (exp2-val (value-of exp2 nenv)))
               (let ((exp1-num (expval->num exp1-val))
                     (exp2-num (expval->num exp2-val)))
                 (num-val
                  (/ exp1-num exp2-num)))))
    (equal?-exp (exp1 exp2)
                (let ((exp1-val (value-of exp1 nenv))
                      (exp2-val (value-of exp2 nenv)))
                  (let ((exp1-num (expval->num exp1-val))
                        (exp2-num (expval->num exp2-val)))
                    (bool-val
                     (= exp1-num exp2-num)))))
    (less?-exp (exp1 exp2)
               (let ((exp1-val (value-of exp1 nenv))
                     (exp2-val (value-of exp2 nenv)))
                 (let ((exp1-num (expval->num exp1-val))
                       (exp2-num (expval->num exp2-val)))
                   (bool-val
                    (< exp1-num exp2-num)))))
    (greater?-exp (exp1 exp2)
                  (let ((exp1-val (value-of exp1 nenv))
                        (exp2-val (value-of exp2 nenv)))
                    (let ((exp1-num (expval->num exp1-val))
                          (exp2-num (expval->num exp2-val)))
                      (bool-val
                       (> exp1-num exp2-num)))))
    (print-exp (exp1)
               (let ((exp1-val (value-of exp1 nenv)))
                 (cases expval exp1-val
                   (num-val (num)
                            (eopl:printf "~s\n" num)
                            (num-val 1))
                   (bool-val (bool)
                             (if bool
                                 (eopl:printf "True\n")
                                 (eopl:printf "False\n"))
                             (num-val 1))
                   (proc-val (proc)
                             (eopl:printf "<Procedure>\n")
                             (num-val 1))
                   (else (eopl:error 'value-of "Incorrect value\n")))))
    (zero?-exp (arg)
               (let ((value (value-of arg nenv)))
                 (let ((number (expval->num value)))
                   (if (zero? number)
                       (bool-val #t)
                       (bool-val #f)))))
    (if-exp (predicate consequent alternative)
            (let ((value (value-of predicate nenv)))
              (if (expval->bool value)
                  (value-of consequent nenv)
                  (value-of alternative nenv))))
    (call-exp (rator rand)
              (let ((proc (expval->proc (value-of rator nenv)))
                    (arg (value-of rand nenv)))
                (apply-procedure proc arg)))
    (nameless-var-exp (n)
                      (apply-nameless-env nenv n))
    (nameless-let-exp (value-exp body)
                      (let ((val (value-of value-exp nenv)))
                        (value-of body
                                  (extend-nameless-env val nenv))))
    (nameless-proc-exp (body)
                       (proc-val
                        (procedure body nenv)))
    (else
     (eopl:error 'value-of "Cannot evaluate ~a" expr))))


;; runner
(define run
  (lambda (string)
    (value-of
     (translation-of
      (scan&parse string) (empty-senv)) (empty-nameless-env))))

;; some examples
(define result1 (run "let x = 5 in -(x,1)"))
(eopl:printf "~s\n" result1)
(define result2 (run "let x = 5 in let y = 6 in -(y,x)"))
(eopl:printf "~s\n" result2)
(define result3 (run "let x = 5 in let f = proc (x) -(x,10) in (f x)"))
(eopl:printf "~s\n" result3)
; some other examples
(define result4 (run "let x = 5 in let y = 4 in +(x,y)"))
(eopl:printf "~s\n" result4)
(define result5 (run "let x = 5 in let y = 4 in *(x,y)"))
(eopl:printf "~s\n" result5)
(define result6 (run "let x = 5 in let y = 4 in /(x,y)"))
(eopl:printf "~s\n" result6)
(define result7 (run "let x = 5 in let y = 4 in *(/(x,y),y)"))
(eopl:printf "~s\n" result7)
(define result8 (run "let b = 40 in print(b)"))
(eopl:printf "~s\n" result8)
(define result9 (run "let b = 40 in equal?(+(b,1),41)"))
(eopl:printf "~s\n" result9)
(define result10 (run "let b = 40 in less?(+(b,1),42)"))
(eopl:printf "~s\n" result10)
(define result11 (run "let b = 40 in greater?(+(b,3),42)"))
(eopl:printf "~s\n" result11)
(define result12 (run "let a = 3 in let isPositive = proc (x) greater?(x,0) in if (isPositive a) then -(a,3) else +(a,3)"))
(eopl:printf "~s\n" result12)
(define result13 (run "let a = 3 in let isPositive = proc (x) greater?(x,0) in print(isPositive)"))
(eopl:printf "~s\n" result13)
(define result14 (run "let a = 3 in let isPositive = proc (x) greater?(x,0) in print((isPositive a))"))
(eopl:printf "~s\n" result14)
