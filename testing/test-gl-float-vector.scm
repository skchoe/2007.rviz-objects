(module test-gl-float-vector mzscheme
(require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "math.ss")
           (lib "list.ss")
           sgl/gl-vectors)
  (define v1 (vector 1 2 3 4))
  (display (vector-ref v1 0))
  (display (eq? #t (eq? (vector-length v1) 4)))
  (define v1.5 (gl-float-vector 1 2 3 4  5 6 7 8))
  (define v2 (gl-vector->vector v1.5))
  (display (vector-length v2))
  (newline)
  (display (vector-ref v2 0))
  (newline)
  (display (vector-ref v2 1))
  (newline)
  (display (vector-ref v2 2))
  (newline)
  (display (vector-ref v2 3))
  (newline)
  (display (vector-ref v2 4))
  (newline)
  
  (define v3 (gl-vector->list v1.5))
  (define traverse-list
    (lambda (l)
      (cond [(eq? l empty) #f]
            [else
             (display (car l))
             (newline)
             (traverse-list (cdr l))])))
  
  (traverse-list v3))
