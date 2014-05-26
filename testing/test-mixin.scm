(module test-mixin mzscheme
(require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "math.ss")
           (lib "list.ss")
           (prefix gl- (lib "sgl.ss" "sgl"))
         (lib "gl-vectors.ss" "sgl"))
  
     
  (define fish%
    (class object% ()
      (init size)
      (define current-size size)
      (super-new)
      (define/public (grow amt)
        (set! current-size (+ amt current-size)))))
  
  (define (picky-mixin %)
    (class % (super-new)
      (define/override (grow amt) (super grow (* 3/4 amt)))))

  (define picky-fish% (picky-mixin fish%))
  
  
  
  (define obj-color-fish%
    (class fish% ()
      (super-new)
      (init struct-root fish-tree)
      ;...
      (define/public (object->renderer obj)
        (printf "obj->render\n"))
      
      (define/public (convert-struct root new-root) 
        (printf "convert-struct\n"))
      ))
    
  (define obj-file-fish%
    (class fish% ()
      (super-new)
      (init struct-root flat-list)
      ;...
      (define/public (object->renderer obj)
        (printf "obj->render\n"))
      
      (define/public (convert-struct root new-root)
        (printf "convert-struct\n"))

      (define update-output #f)
      ))

  (define color-fish-obj (new obj-color-fish% 
                              [size 10] 
                              [struct-root null] 
                              [fish-tree null]))
  (send color-fish-obj object->renderer color-fish-obj)
  (send color-fish-obj convert-struct color-fish-obj color-fish-obj)

  (define render-interface (interface () object->render convert-struct))

  (define (render-graph-mixin %)

;    (unless (implementation? % render-interface)
;      (error "render-mixin:not render interface class\n"))
    
    (class % (render-interface)
      (super-new)
      (define/override (object->renderer obj) #f)
      (define/override (convert-sruct root new-root) #f)))
  
  (define (is-render? o) (is-a? o render-interface))
  (define gfx-obj-fish% (render-graph-mixin obj-file-fish%))
  
  (define gfx-color-fish% (render-graph-mixin obj-color-fish%))
  
  
  

      
  )
