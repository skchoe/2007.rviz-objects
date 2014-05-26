(module static_function mzscheme  
  (define (draw-canvas inner outer)
    (begin
      (display inner)
      (newline)
      (display outer))
    )

  
  (provide draw-canvas))
