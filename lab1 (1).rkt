#lang racket

; A1
(define (sumlist x)
(if (null? x) 0
      (+ (car x) (sumlist (cdr x))))
  )

(sumlist '(3 4 5))
(sumlist '())
(sumlist '(2))
(sumlist '(5 4 5 4 5 4))

; A2

(define (my-append x y)
  (if (null? x) (list y)
      (cons (car x) (my-append (cdr x) y))))

(my-append '(4 5 6) 7)
(my-append '() 7)
(my-append '(3 2 1) 'a)

; B1

(define (my-map func alist)
   (if (null? alist)
       '()
       (cons (func (car alist)) (my-map func (cdr alist)))))

(my-map (lambda (x) (+ 1 x)) '(10 20 30))
(my-map (lambda (x) (* x x)) '(1 2 3 4 5))

; B2

(define mult
  (lambda (a)
    (lambda (b)
      (* a b))))

((mult 3) 3)
((mult 10) 2)

(define (curry2 f)
  (lambda(x)
    (lambda(y)
      (f x y))))

(define (curry3 f)
  (lambda(x)
    (lambda(y)
      (lambda(z)
       (f x y z)))))

(((curry2 *) 2) 3)
(((curry2 +) 2) 3)

((((curry3 *) -1) 3) 2)
((((curry3 +) -1) 1) 2)

(require racket/trace)
(trace sumlist)
(sumlist `(3 2 3 4))