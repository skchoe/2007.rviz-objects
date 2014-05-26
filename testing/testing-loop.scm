;#lang scheme
(module testing-loop scheme
  (require (lib "mred.ss" "mred")
           (lib "match.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "foreign.ss"))
  
(define width 5)
(define height 3)
(define i 0)
(define j 0)

(let ((k 100))
  k)

(let fac ([n 10])
  (if (zero? n)
      1
      (* n (fac (sub1 n)))))

(define store0 (build-list (* (* width height) 3) (lambda (x) x)))
(display store0) (newline)
  
(define convert 
  (lambda (s t width height) 
  (+ s (* (- (- height 1) t) width))))
 
;(define my-quotient/remainder
;  (lambda (denom nom)
;    (let-values (( (x y) (quotient/remainder denom nom) ))
;      
                         
    
(printf "~s\n" (convert 0 2 5 3))
  
(define y 10)
  y
  
(let-values (((x y) (quotient/remainder y height)))
  (display x)(newline)
  (display y)(newline))
  (display '---------------)(newline)
(for* ([t (build-list height (lambda (x) x))]
       [s (build-list width (lambda (x) x))])

      (display (list t s))
      (let ((x (+ s (* t width))))
        (printf "  org idx = ~s\t" x)
        (printf " values = ~s ~s ~s\t" (list-ref store0 (* 3 x))
                (list-ref store0 (+ 1 (* 3 x)))
                (list-ref store0 (+ 2 (* 3 x))))
        (let ((y (convert s t width height)))
          (printf "converted idx = ~s\t" y)
          (let-values (((dh dw) (quotient/remainder y width)))
            (printf "position dw = ~s \t dh = ~s\n" dw dh)
            (let ((x1 (convert dw dh width height)))
              (printf "converted = ~s\n" x1)
              (printf "values(~s) =  ~s ~s ~s\n" x1
                        (list-ref store0 (* 3 x1))
                        (list-ref store0 (+ 1 (* 3 x1)))
                        (list-ref store0 (+ 2 (* 3 x1)))))))))

     
;(let loop ((s 0)
;           (t 0)
;           (cnt 0))
;  (if (< s width)
;      (begin
;        (if (< t height)
;            (begin (printf "s, t = ~s ~s\n" s t)
;                   (set! t (+ 1 t))
;                   (loop s t (+ cnt 1)))
;            (set! t 0))
;        (set! s (+ 1 s))
;        (loop s t (+ cnt 1)))
;      (printf  "cnt(~s)s, t are greater than height, width\n" cnt)))
;          
)