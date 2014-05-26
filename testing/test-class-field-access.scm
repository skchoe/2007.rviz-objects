#lang mzscheme

(require (lib "mred.ss" "mred")
         (lib "framework.ss" "framework")
         (lib "etc.ss")
         (lib "class.ss"))

(define child-object%
  (class object%
    (init size)
    (define member1 size)
    (super-new)    
    (define/public set-member1
      (lambda (cn)
        (set! member1 cn)))
    (define/public get-member1
      (lambda () member1))
    (set! member1 100)))

(define fish%
  (class object%
    (init-field size weight)  ; initialization argument
    (init-field ht)
    
    (super-new)                ; superclass initialization
    
    ;; methods
    (define/public set-size
      (lambda (sz)
        (set! size sz)))
    (define/public (get-size)
      size)
    (define/public (grow amt)
      (set! size (+ amt size)))

    (define/public set-weight
      (lambda (wt)
        (set! weight wt)))
    (define/public (get-weight)
      weight)
    
    (define/public (get-ht)
      ht)

    
    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))
  
(define init-ht
  (lambda ()
    (make-hash-table 'equal)))

(define charlie (instantiate fish% () [size 10] [weight 100] [ht (init-ht)]))
(define viz-class (new child-object% [size 100]))

(define (main)
  (let* ((c charlie)
         (d viz-class))
    
    (send c grow 1000)
    (send c set-weight 200)
    (display (send c get-size)) (newline)
    (display (send c get-weight)) (newline)
    (send d set-member1 200)
    (display (send d get-member1))
  ))

(main)

         