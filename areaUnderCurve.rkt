;;area-under-curve
;;by Rachel Pinsker
;;8/29/12

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))
  

(define (approx-area f a b dx)
  (define (inc a) (+ a dx))
  (* dx (sum f (+ a (/ dx 2)) inc b)))


  
  