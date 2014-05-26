(module struct-test mzscheme
  (require (lib "mred.ss" "mred")
           "../module_canvas/math/def.scm")
  
  (define-struct cons-cell (car cdr))
  (define x (make-cons-cell 1 2))
  (define-struct (tagged-cons-cell cons-cell) (tag))
  (define z (make-tagged-cons-cell 3 4 't))
  (cons-cell? z) ; => #t
  (display (cons-cell-cdr z))
  (tagged-cons-cell? z) ; => #t
  (tagged-cons-cell? x) ; => #f
  (cons-cell-car z) ; => 3
  (tagged-cons-cell-tag z) ; => 't

  (define-struct a (name))
  (define-struct (A a) (age))

  (make-a 'test)
  (make-A 'test1 12)

  (define-struct fish (kind size lake position))
  (define test (make-fish 'black 8.0 'utah (make-point3 0.0 5.0 0.0)))

  ;(a-name)
  ;(A-age)
  )