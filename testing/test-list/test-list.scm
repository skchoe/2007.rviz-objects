(module test-list mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss"))
  
  (define r<%> (interface () render))
  
  ; super class
  (define render-class% 
    (class* object% (r<%>)
      (public render)
      (define (render)
        (display 'r_top))
      (super-new)))
  
  ; child class 0
  (define render-obj0%
    (class render-class%
      (override render)
      (define (render)
        (display 'r0))
      (super-new)))
  
  ; child class 1
  (define render-obj1%
    (class render-class%
      (override render)
      (define (render)
        (display 'r1))
      (super-new)))
  
  (define (app0)
    (let* ((c_top (make-object render-class%))
           (c_0 (make-object render-obj0%))
           (c_1 (make-object render-obj1%)) 
           )
      (send c_top render)))
  
  (define (app1)
    (define obj-list empty)
    (let* ((c_top (make-object render-class%))
           (c_0 (make-object render-obj0%))
           (c_1 (make-object render-obj1%)) )

      (let* ((l_top (cons c_top obj-list))
             (l_0 (cons c_0 l_top))
             (l_1 (cons c_1 l_0)))
        (let* ((first (car l_1))
               (rest (car (cdr l_1))))
          (begin
            (send first render)
            (newline)
            (send rest render))))))
    (app0) (newline)
    (app1))