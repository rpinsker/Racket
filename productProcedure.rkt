;; product procedure
;; by Rachel Pinsker and Matt Myers
;; 9/12/12

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial x)
  (define (next x) (+ x 1))
  (product (lambda (x) x) 1 next x))