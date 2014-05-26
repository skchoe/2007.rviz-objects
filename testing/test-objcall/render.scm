(module render mzscheme
  (require (lib "class.ss"))

  (define r<%> (interface () render))
    
  (define render%
    (class* object% (r<%>)
      (public render)
      (define (render)
        (display 'render_on_parent_called))
     
      (super-new)))
  
  (define (app)
    (let* ((c (make-object render%)))
      (send c render)))
  
(provide render%))