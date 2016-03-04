;; cube-root-calculator
;; by Rachel Pinsker
;; 8/29/12
(define (improve guess a)
  (/ (+ (* 2 guess) (/ a (* guess guess))) 3))

(define (close-enough? guess a)
  (< (abs (- (* guess guess guess) a)) 0.001))

(define (my-aux-root guess a)
  (if (close-enough? guess a)
      guess
      (my-aux-root (improve guess a) a)))

;;wrapper function
(define (my-cube-root a)
  (my-aux-root 1.0 a))


  