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
(define new-real-list (list b1 b1 b2 b2))
(define new-pair (cons b1 (cons b1 (cons b2 (cons b2 empty)))))
(define new-vector (vector b1 b1 b2  b2))
(append (list b3) (remq b2 (remq b1 new-list)))
new-list

(define prinT 
  (lambda (num)
    (printf "====>~s \n" num)))

(for-each prinT new-list)
(for-each prinT new-real-list)
(for-each prinT new-pair)
(for-each prinT (vector->list new-vector))

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

(display (cons 1 (cons 2 empty)))
