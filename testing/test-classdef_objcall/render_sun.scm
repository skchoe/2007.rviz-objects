(module render_sun mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           "render.scm")

  (define render_sun%
    (class renderer% ()

      ; private member: object to render
      (define sun null)
      
      ; set object from outside
      (public set-object)
      (define (set-object obj)
        (set! sun obj))
      
      ;Declare interface methods
      (override render)
      ;Method implementations
      (define (render)
        (render-fp sun))
        
      (define (render-fp obj)
        (display obj))
        
      (super-new)))
  

  (provide render_sun%))