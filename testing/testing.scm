#|;; (define-type shape
;;   [square (side number?)]
;;   [rect (height number?) ( (define-type shape

;; http://www.scheme.com/tspl3/ The Scheme programming language

;#module slideshow
(define const 5)
(define (circle r) const)
(circle 4)

(car '(a b c))

(load "reciprocal.ss") 
(reciprocal 4)

'(/ (* 2 -1) 3)

(quote hello)

(car '((a b) (c d)))


(let ((x 2) (y 3)) (+ x y))

(let ((list1 '(a b c)) (list2 '(d e f)))
  (cons (cons (car list1)
              (car list2))
        (cons (car (cdr list1))
              (car (cdr list2)))))


(lambda (x) (+ x x))
(let ((double (lambda (x) (+ x x))))
  (list (double (* 3 4))
        (double (/ 99 11))
        (double (- 2 7))))

(let ((x 'a))
  (let ((f (lambda (y) (list x y))))
    (f 'b)))

(let ((f (lambda x x)))
  (f 1 2 3 4 5 6 7 8))

(let ((g (lambda (x . y) (list x y))))
  (g 1 2 3 4))

(let ((h (lambda (x y . z) (list x y z))))
  (h 'a 'b 'c 'd))

;; section 2.6
(define cadr
  (lambda (x)
    (car (cdr x)))) 

(define sign
  (lambda (n)
    (cond
      ((< n 0) -1)
      ((> n 0) +1)
      (else 0)))) 

(define abs-all
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (abs (car ls))
              (abs-all (cdr ls)))))) 

(define abs-all
  (lambda (ls)
    (map abs ls))) 

(map (lambda (x) (* x x))
     '(1 -3 -5 7))
(map cons '(a b c) '(1 2 3))

(define map1
  (lambda (p ls)
    (if (null? ls)
        '()
        (cons (p (car ls))
              (map1 p (cdr ls)))))) 


(map1 abs '(1 -2 3 -4 5 -6))

;; section 2.9

;;(define abc '(a b c))

(define quadratic-formula
  (lambda (a b c)
    (let ((root1 0) (root2 0) (minusb 0) (radical 0) (divisor 0))
      (set! minusb (- 0 b))
      (set! radical (sqrt (- (* b b) (* 4 (* a c)))))
      (set! divisor (* 2 a))
      (set! root1 (/ (+ minusb radical) divisor))
      (set! root2 (/ (- minusb radical) divisor))
      (cons root1 root2)))) 

(define quadratic-formula
  (lambda (a b c)
    (let ((minusb (- 0 b))
          (radical (sqrt (- (* b b) (* 4 (* a c)))))
          (divisor (* 2 a)))
      (let ((root1 (/ (+ minusb radical) divisor))
            (root2 (/ (- minusb radical) divisor)))
        (cons root1 root2))))) 

(let ((sum (lambda (sum ls)
             (if (null? ls)
                 0
                 (+ (car ls) (sum sum (cdr ls)))))))
  (sum sum '(1 2 3 4 5)))

;; chap 3.1
(letrec ((sum (lambda (ls)
                (if (null? ls)
                    0
                    (+ (car ls) (sum (cdr ls)))))))
  (sum '(1 2 3 4 5)))

(letrec ((even?
          (lambda (x)
            (or (= x 0)
                (odd? (- x 1)))))
         (odd?
          (lambda (x)
            (and (not (= x 0))
                 (even? (- x 1))))))
  (list (even? 20) (odd? 20))) 

(define list?
  (lambda (x)
    (letrec ((race
              (lambda (h t)
                (if (pair? h)
                    (let ((h (cdr h)))
                      (if (pair? h)
                          (and (not (eq? h t))
                               (race (cdr h) (cdr t)))
                          (null? h)))
                    (null? h)))))
      (race x x)))) 

(define list?
  (lambda (x)
    (let race ((h x) (t x))
      (if (pair? h)
          (let ((h (cdr h)))
            (if (pair? h)
                (and (not (eq? h t))
                     (race (cdr h) (cdr t)))
                (null? h)))
          (null? h))))) 

(define factorial
  (lambda (n)
    (let fact ((i n))
      (if (= i 0)
          1
          (* i (fact (- i 1))))))) 

;; section 3.4

;; chap 4

;; section 4.1

list
(define x 'a)
(let ((x 'b)) (list x x))

(let ((let 'let)) let)

(define f
  (lambda (x)
    (g x)))
(define g
  (lambda (x)
    (+ x x)))
(f 3)

;; section 4.3

(let ((x (* 3.0 3.0)) (y (* 4.0 4.0)))
  (sqrt (+ x y)))


;; another chapter Vector.
(vector)

(make-vector 0)
(make-vector 0 'a)
;(vector-ref
 (vector 3 'a 'b 'c)
(vector-ref (make-vector 2 'a) 0)
(make-vector 10)

;(define-syntax let
;  (syntax-rules ()
;    ((_ ((x v) ...) e1 e2 ...)
;     ((lambda (x ...) e1 e2 ...) v ...)))) 

(let ((v1 'a) (v2 'b)) (vector v1 v2))
(let ((v (vector 'a 'b))) v)
(let ((v (vector 'a 'b))) (let ((v (vector-set! v 0 'c))) v))

(let ((v (vector 'a 'b)))
(vector-set! v 0 'c)
v)

(define vector-fill!
  (lambda (v x)
    (let ((n (vector-length v)))
      (do ((i 0 (+ i 1)))
          ((= i n))
          (vector-set! v i x))))) 

(let ((v (vector 1 2 3)))
  (vector-fill! v 0)
  v)


  (define (map f l)
   (cond
     [(null? l) '()]
     [else (cons (f (car l)) (map f (cdr l)))]))

  (define (increment x) (+ x 1))
|#
(values)
(values 1)
(values 1 2 3)