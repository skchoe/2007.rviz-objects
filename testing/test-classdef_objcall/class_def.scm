(module class_def mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           "static_function.scm")

  
  (define class_1%
    (class* object% ()
      
      ; private member of class_1
      (define renderer-list empty)
      
      ; add-renderer
      (public add-renderer)
      (define (add-renderer renderer)
        (set! renderer-list (add-renderer-fp renderer-list renderer)))
      ; add-renderer-fp
      (define (add-renderer-fp renderer-list renderer)
        (cons renderer renderer-list))
      
      ; remove-renderer
      ; remove a renderer from renderer-list
      (public remove-renderer)
      (define (remove-renderer renderer)
        (set! renderer-list (remove-renderer-fp renderer-list renderer)))
      ; remove-renderer-fp
      (define (remove-renderer-fp renderer-list renderer)
        (cond
          [(empty? renderer-list) empty]
          [(equal? renderer (car renderer-list)) (cdr renderer-list)]
          [else (cons (car renderer-list) 
                      (remove-renderer-fp (cdr renderer-list)
                                          renderer))]))

      ; remove-all
      (public remove-all-renderer)
      (define (remove-all-renderer)
        (set! renderer-list empty))
      
      ; call render in each renderer in renderer-list
      (public render-all)
      (define (render-all)
        (render renderer-list))
      
      ; renderer-list -> void
      ; traverse renderer object in renderer-list and call render
      (define (render l)
        (cond
          [(empty? l) void]
          [else (begin
                  (send (car l) render)
                  (render (cdr l)))]))
         
      ; public decl.
      (public display_inout)
      ; public impl.
      (define (display_inout in out) 
        (draw-canvas in out))
      (super-new)))

  (provide class_1%))