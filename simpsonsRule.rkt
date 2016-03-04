;; simpson's-rule
;; by Rachel Pinsker and Matt Myers
;; 9/4/12

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) 
  (* x x x))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h)))) 
  (define (inc k) (+ k 2.0))
  (define (term k) (+ (* 2.0 (y k)) (* 4.0 (y (+ k 1)))))
  (* (/ h 3.0) (+ (sum term 2 inc (- n 1)) (y 0) (y n) (* 4.0 (y 1)))))


  
                          
                    