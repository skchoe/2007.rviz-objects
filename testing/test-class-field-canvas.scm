#lang scheme

(require (lib "mred.ss" "mred")
         (lib "framework.ss" "framework")
         (lib "etc.ss")
         (lib "class.ss"))

(define child-canvas%
  (class canvas%
    (init-field test)
    (super-instantiate () (style '(gl no-autoclear)))))

;; frame-object: (void) -> frame-object 
(define frame-object 
  (lambda ()
    (make-object frame% "testing sgl-toolkit" #f)))

;; viz-class-object: frame -> viz-class-object
(define viz-class-object 
  (lambda (pos-expr) 
    (new child-canvas% (parent pos-expr) 
      [min-width 800] 
      [min-height 650]
      [test 10])))
;[pick-id-table (init-pick-id-table)])))

(define (main)
  (let* ((frm (frame-object))
           (viz (viz-class-object frm)))
    (send frm show #t)))

(main)
