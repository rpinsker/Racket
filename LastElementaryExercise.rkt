;;Last Elementary Exercise
;;by Rachel Pinsker and Matt Myers
;;9/20/12

;;1.) a is an element of a set
(define (element-of num list1)
  (cond ((null? list1) false)
        ((= num (car list1)) true)
        (else (element-of num (cdr list1)))))

;;2.) subset of
(define (subset list1 list2)
  (cond ((null? list1) true)
        ((element-of (car list1) list2) (subset (cdr list1) list2))
        (else false)))

;;3.) intersection
(define (intersection list1 list2)
  (aux-intersection list1 list2 '()))

(define (aux-intersection list1 list2 list3)
  (cond ((null? list1) list3)
        ((element-of (car list1) list2) (aux-intersection (cdr list1) list2 (append list3 (list (car list1)))))
        (else (aux-intersection (cdr list1) list2 list3))))

;;4.) union
(define (union list1 list2)
  (aux-union list1 list2))

(define (aux-union list1 list2)
  (cond ((null? list2) list1)
        ((not (element-of (car list2) list1)) (aux-union (append list1 (list (car list2))) (cdr list2)))
        (else (aux-union list1 (cdr list2))))) 
