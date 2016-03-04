;;Differentiations and Simplify (simplify only works sometimes)
;;by Rachel Pinsker and Matt Myers
;;10/3/12

(define (simplify-deriv exp var)
  (simplify (deriv exp var) var))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((eq? exp var) 1)
        ((eq? (car exp) '^) (list '* (list '* (caddr exp)
                                           (list '^ (cadr exp)
                                                 (list '- (caddr exp) 1)))
                                  (deriv (cadr exp) var)))
        ((eq? (car exp) '*) (list '+ (list '* (cadr exp) 
                                           (deriv (caddr exp) var)) 
                                  (list '* (deriv (cadr exp) var) (caddr exp))))
        ((eq? (car exp) '/) (list '/ (list '- (list '* (caddr exp) (deriv (cadr exp) var))
                                           (list '* (deriv (caddr exp) var) (cadr exp))) 
                                  (list '* (caddr exp) (caddr exp))))
        ((eq? (car exp) '+) (list '+ (deriv (cadr exp) var) (deriv (caddr exp) var)))
        ((eq? (car exp) '-) (list '- (deriv (cadr exp) var) (deriv (caddr exp) var)))
        ((eq? (car exp) 'my-exp) (list '* (list 'my-exp (cadr exp)) (deriv (cadr exp) var)))
        ((eq? (car exp) 'ln) (list '* (list '/ 1 (cadr exp)) (deriv (cadr exp) var)))
        ((eq? (car exp) 'sin) (list '* (list 'cos (cadr exp)) (deriv (cadr exp) var)))
        ((eq? (car exp) 'cos) (list '* -1 (list 'sin (cadr exp)) (deriv (cadr exp) var)))
        ((eq? (car exp) 'sec) (list '* (list 'sec (cadr exp)) 
                                    (list 'tan (cadr exp)) 
                                    (deriv (cadr exp) var)))
        ((eq? (car exp) 'csc) (list '* -1 (list 'csc (cadr exp)) 
                                    (list 'cot (cadr exp)) 
                                    (deriv (cadr exp) var)))
        ((eq? (car exp) 'tan) (list '* (list 'sec (cadr exp))
                                    (list 'sec (cadr exp))
                                    (deriv (cadr exp) var)))
        ((eq? (car exp) 'cot) (list '* -1 (list 'csc (cadr exp))
                                    (list 'csc (cadr exp))
                                    (deriv (cadr exp) var)))
        ((eq? (car exp) 'arcsin) (list '* (list '/ 1 (list '^ (list '- 1 (list '^ (cadr exp) 2)) (/ 1 2)))
                                       (deriv (cadr exp) var)))
        ((eq? (car exp) 'arctan) (list '* (list '/ 1 (list '+ (list '* (cadr exp) (cadr exp)) 1))
                                        (deriv (cadr exp) var)))))


(define (simplify exp var)
  (cond ((eq? (car exp) '+) (simplify-add exp var))
        ((eq? (car exp) '*) (simplify-mult exp var))
        (else exp)))

(define (simplify-mult exp var)
   (is-length-two-mult (cons (car exp) (aux-simplify-mult (cdr exp) var)) var))

(define (aux-simplify-mult exp var)
  (cond ((and (null? (cdr exp)) (eq? (car exp) 1)) '())  ;;last in list and is 1
        ((and (null? (cdr exp)) (or (eq? (car exp) var)  (number? (car exp)))) exp)  ;;last in list and is not another function 
        ((null? (cdr exp)) (cons (simplify (car exp) var) (aux-simplify-mult (cdr exp) var))) ;;last in list and is a function
        ((eq? (car exp) 1) (aux-simplify-mult (cdr exp) var)) ;;not last in list and is 1
        ((and (not (eq? (car exp) 1)) (or (eq? (car exp) var) (number? (car exp)))) ;;not last in list and is not another function 
         (cons (car exp) (aux-simplify-mult (cdr exp) var)))
        ((or (eq? (car (car exp)) '*) (eq? (car exp) '+)) (cons (simplify (car exp) var) (aux-simplify-mult (cdr exp) var)))
        (else (cons (simplify (car exp) var) (aux-simplify-mult (cdr exp) var)))))
    
(define (is-length-two-mult exp var)
  (cond ((eq? (length exp) 2) (cdr exp))
        (else exp)))

(define (simplify-add exp var)
   (is-length-two-add (cons (car exp) (aux-simplify-add (cdr exp) var)) var))

(define (aux-simplify-add exp var)
  (cond ((and (null? (cdr exp)) (eq? (car exp) 0)) '())  ;;last in list and is 1
        ((and (null? (cdr exp)) (or (eq? (car exp) var)  (number? (car exp)))) exp)  ;;last in list and is not another function 
        ((null? (cdr exp)) (cons (simplify (car exp) var) (aux-simplify-add (cdr exp) var))) ;;last in list and is a function
        ((eq? (car exp) 0) (aux-simplify-add (cdr exp) var)) ;;not last in list and is 1
        ((and (not (eq? (car exp) 0)) (or (eq? (car exp) var) (number? (car exp)))) ;;not last in list and is not another function 
         (cons (car exp) (aux-simplify-add (cdr exp) var)))
        ((or (eq? (car (car exp)) '+) (eq? (car exp) '*)) (cons (simplify (car exp) var) (aux-simplify-add (cdr exp) var)))
        (else (cons (simplify (car exp) var) (aux-simplify-add (cdr exp) var)))))
    
(define (is-length-two-add exp var)
  (cond ((eq? (length exp) 2) (cdr exp))
        (else exp)))
                            
                        




  
