(module test-max-posn mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "math.ss")
           (lib "list.ss"))
  
  (define-struct posn (x y))
  (define-struct min-max-posn (min-posn max-posn))
  
  (define what-is-min-max-posn 
    (lambda (p0 p1)
      (let* ((x0 (posn-x p0))
             (y0 (posn-y p0))
             (x1 (posn-x p1))
             (y1 (posn-y p1)))
        (cond 
          [(and (< x0 x1) (< y0 y1)) 
           (make-min-max-posn (make-posn x0 y0) (make-posn x1 y1))]
          [(and (< x0 x1) (> y0 y1))
           (make-min-max-posn (make-posn x0 y1) (make-posn x1 y0))]
          [(and (> x0 x1) (< y0 y1)) 
           (make-min-max-posn (make-posn x1 y0) (make-posn x0 y1))]
          [(and (> x0 x1) (> y0 y1))
           (make-min-max-posn (make-posn x1 y1) (make-posn x0 y0))]))))
  
  (define mmp (what-is-min-max-posn
   (make-posn 1 0) (make-posn 0 1)))
  
  (define min- (min-max-posn-min-posn mmp))
  (define max- (min-max-posn-max-posn mmp))
  
  (printf "~s ~s\n" (posn-x min-) (posn-y min-))
  (printf "~s ~s\n" (posn-x max-) (posn-y max-))
  
  (printf "~s\n" (and #t #t #t #t))
  )

