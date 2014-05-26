(module test-for_fold (lib "mzscheme");mzscheme
  (require (only-in scheme for/fold))
(for/fold ((sum 0)
           [rev-roots null])
          ([i '(1 2 3 4)])
          (begin 
            (let ((a (+ sum i))
                  (b (cons (sqrt i) rev-roots)))
              (printf "~s ~s\n" a b)
              (values a b))))

(for/fold ((result empty)
           (a 1) (b 2) (c 3) (d 4)) 
          ((i (list 10 11 12 13 14)))
          (let* ((A 0) (B 0) (C 0) (D 0)
                 (a1 (+ a i)) (b1 (+ b i)) (c1 (+ c i)) (d1 (+ d i)))
            (printf "~s ~s ~s ~s\n" a1 b1 c1 d1)
            (values (append result (list a1 b1 c1 d1)) a1 b1 c1 d1)))

;(display (for/fold ((a 888)
;                    (k 0))
;                   (i '(in-list (list 1 2 3)))
;                   (+ a (list-ref i k))))

(define print-them
  (lambda (a b)
    (printf "~s ~s\n" a b)))

(call-with-values (lambda () (values 1 2)) print-them)

(for/fold ([lst (list 1 2 3 4)])
          ((input-lists (list empty (list 1) (list 45 56) empty (list 4999))))
          (append lst input-lists))
)

