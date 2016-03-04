;;pascal-triangle
;;by Rachel Pinsker
;;8/29/12

(define (pascal n k)
  (if (or (= k 0) (= n k)) 1
      (+ (pascal (- n 1) (- k 1)) (pascal (- n 1) k))))
          