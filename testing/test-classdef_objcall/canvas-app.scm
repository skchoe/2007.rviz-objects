(module canvas-app mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           "canvas.scm")
  
  (define vis-class%
    (class canvas% ()
      
      (define (f in out) 
        (draw-canvas in out))
        
      ))
  
  (define (F)
    (begin
      (display 'ttt)))
  
  (display 'TEST)
  
  (F))