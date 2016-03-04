;;prime-number-checker
;;by Rachel Pinsker
;;8/29/12

(define (aux-prime? factor n)
  (if (> n factor)
      (if (= (remainder n factor) 0) factor
      (aux-prime? (+ factor 1) n))
      1))
 
  
      
      
 (define (prime? n)
   (aux-prime? 2 n))