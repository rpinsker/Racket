;;First List Exercises
;; by Rachel Pinsker and Matt Myers
;; 9/14/12

;;1.)
(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))

;;2.) 
(define (my-list-ref list1 index)
  (cond ((null? list1) null)
        ((= 0 index) (car list1))
        (else (my-list-ref (cdr list1) (- index 1)))))

;;3.)
(define (my-length list1)
  (if (null? list1)
      0
      (+ 1 (my-length (cdr list1)))))

;;4.)
(define (my-last-pair list1)
  (if (null? (cdr list1))
      (car list1)
      (last-pair (cdr list1))))

;;5.) 
(define (my-reverse list1)
  (cond ((null? (cdr list1)) list1)
      (else (append (my-reverse (cdr list1)) (list (car list1)))))) 

;;6.) 
(define (my-count-leaves list1)
  (cond ((null? list1) 0)
        ((not (pair? list1)) 1)
      (else (+ (my-count-leaves (car list1)) (my-count-leaves (cdr list1))))))

  
  
        
  
      