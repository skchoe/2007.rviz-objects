(module render_sun mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           "render.scm")

  (define render_sun%
    (class render% ()
      ;Declare interface methods
      (override render)
      ;Method implementations
      (define (render)
        (display 'TESTTEST))
      (super-new)))
  
  (send (make-object render_sun%) render)
  
  (provide render_sun%))