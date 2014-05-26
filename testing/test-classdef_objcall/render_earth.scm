(module render_earth mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           "render.scm")

  (define render_earth%
    (class renderer% ()
      
      ; private member: object to render
      (define earth null)
      
      ; set object from outside
      (public set-object)
      (define (set-object obj)
        (set! earth obj))
      
      ;Declare interface methods
      (override render)
      ;Method implementations
      (define (render)
        (render-fp earth))
        
      ; obj -> void
      (define (render-fp obj)
        (display obj))
        
      (super-new)))
  

  (provide render_earth%))