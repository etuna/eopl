#lang racket

;; Lab 1 Answers

;; Question 1

(define (duple n sexp)
  (if (zero? n)
    '()
    (cons sexp (duple (- n 1) sexp))))


(duple 2 3) ;; (3 3)
(duple 4 '(ha ha)) ;; ((ha ha) (ha ha) (ha ha) (ha ha))
(duple 0 '(blah))  ;; ()


;; Question 2

(define (count-occurrences s slist)
  (if (null? slist)
    0
    (+ (count-occurrences-in-s-sexp s (car slist))
       (count-occurrences s (cdr slist)))))

(define (count-occurrences-in-s-sexp s sexp)
  (if (symbol? sexp)
    (if (eqv? sexp s) 1 0)
    (count-occurrences s sexp)))

(count-occurrences 'x '((f x) y (((x z) x)))) ;; 3
(count-occurrences 'x '((f x) y (((x z) () x)))) ;; 3
(count-occurrences 'w '((f x) y (((x z) x)))) ;; 0


;; Question 3

(define (flatten slist)
  (cond [(null? slist) '()]
        [(list? (car slist)) (append (flatten (car slist))
                                     (flatten (cdr slist)))]
        [else (cons (car slist)
                    (flatten (cdr slist)))]))

(flatten '(a b c)) ;; (a b c)
(flatten '((a) () (b ()) () (c))) ;; (a b c)
(flatten '((a b) c (((d)) e))) ;; (a b c d e)
(flatten '(a b (() (c)))) ;; (a b c)