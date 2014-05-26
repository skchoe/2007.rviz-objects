#lang scheme

(define a1 (cons 1 empty))
(define a2 (cons 1 empty))

(if (eq? a1 a2)
    (display 'true)
    (display 'false))

(newline)

(if (memq (cons 1 empty) (list a1 a2))
    (display 'm-true)
    (display 'm-false))
(newline)

(define b1 1)
(define b2 2)
(define b3 3)
(define new-list (append (list b1) (list b1 b2 b2)))
(append (list b3) (remq b2 (remq b1 new-list)))
new-list
(newline)


(printf "....................\n")
(define s1 #f)
(define s2 #f)
(define s3 #f)
(if (memq b1 new-list)
    (begin
      (set! s1 456)
      (set! s2 new-list)
      (display 'there-is)(newline)
      (set! s3 (append (list b3) (remq b1 new-list)))
      new-list
      (append (list b2) new-list))
    (display 'not-exist)
    )

s1
s2
s3


(printf "....................\n")
(define new-list-1 (cons b1 (cons b1 (cons b2 (cons b2 empty)))))

new-list-1
(append (list b3) new-list-1)
(remq b2 new-list-1)



;(define sum
;  (lambda (a b) (+ a b)))
;new-list
;(newline)
;(define num (length new-list))
;num
;(define dummy (build-list num (lambda (x) (* x 0))))
;(newline)
;dummy
;(map sum new-list dummy)
;
;
;
;
;
;(define l-100 (build-list 100 (lambda (x) x)))
;(define l-101 (map (lambda (x) (+ 1 x)) l-100))
;
;(define add-lists 
;  (lambda (a b)
;    (+ a b)))
;
;(define (list-op list1 list2)
;  (map add-lists list1 list2))
;  
;(list-op l-100 l-101)