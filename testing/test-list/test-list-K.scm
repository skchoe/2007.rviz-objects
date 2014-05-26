#lang scheme

(define pair1 (cons 1 (cons 2 (cons 3  (cons 4 empty)))))

(define in-number
  (lambda (n pair)
    (call/cc
     (lambda (exit)
       (if (null? pair) 
           #f
           (begin
             (printf "pair-elt = ~s\n" (car pair))
             (if (equal? n (car pair))
                 (exit #t)
                 (in-number n (cdr pair)))))))))

(printf "~s \n" (in-number 1
                           pair1))
           