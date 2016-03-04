;;Fibonacci-number-generator
;;by Rachel Pinsker
;;8/29/12
(define (fib n)
  (if (or (= n 1) (= n 2)) 1
      (+ (fib (- n 1)) (fib (- n 2)))))