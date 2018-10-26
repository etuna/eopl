;; Lab 1 Questions

;; Question 1
;; (duple n x) returns a list containing n copies of x.

;; Your definition here.

;; Test cases
;; (duple 2 3) ;; (3 3)
;; (duple 4 '(ha ha)) ;; ((ha ha) (ha ha) (ha ha) (ha ha))
;; (duple 0 '(blah))  ;; ()


;; Question 2
;; (count-occurrences s slist) returns the number of occurrences of s in slist.

;; Your definition here.

;; Test cases
;; (count-occurrences 'x '((f x) y (((x z) x)))) ;; 3
;; (count-occurrences 'x '((f x) y (((x z) () x)))) ;; 3
;; (count-occurrences 'w '((f x) y (((x z) x)))) ;; 0


;; Question 3
;; (flatten slist) returns a list of the symbols contained in slist in the order in which they occur when slist is printed. Intuitively, flatten removes all the inner parentheses from its argument. 

;; You definition here

;; Test cases
;; (flatten '(a b c)) ;; (a b c)
;; (flatten '((a) () (b ()) () (c))) ;; (a b c)
;; (flatten '((a b) c (((d)) e))) ;; (a b c d e)
;; (flatten '(a b (() (c)))) ;; (a b c)