(module render mzscheme
  (require (lib "class.ss"))

  (define r<%> (interface () render))
    
  (define renderer%
    (class* object% (r<%>)
      (public render)
      (define (render)
        (display 'render_on_parent_called))
     
      (super-new)))
  
  (define (app)
    (let* ((c (make-object renderer%)))
      (send c render)))
  
(provide renderer%))