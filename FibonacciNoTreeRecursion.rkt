;;Fibonacci-number-generator (no tree recursion)
;;by Rachel Pinsker
;;8/29/12

;;wrapper function
(define (fib n)
  (fib-aux 1 1 n))

(define (fib-aux num1 num2 n)
  (if (= n 2) num1
      (fib-aux (+ num1 num2) num1 (- n 1))))

