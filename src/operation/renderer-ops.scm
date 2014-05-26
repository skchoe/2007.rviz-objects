;; definitions on linear pair of renderers
;; definitions on scene-tree
(module renderer-ops scheme
  (require (lib "class.ss") ; scheme/class
           "../render/render.scm"
           "../render/primitives.scm"
           "../render/line.scm"
           "../render/cube.scm"
           "../render/disk.scm"
           "../render/sphere.scm"
           "../render/cylinder.scm"
           "../render/triangle.scm"
           "../render/quad.scm"           
           "../render/surface.scm")

  (provide construct-renderer-object 
           initialize-renderer
           )
  
  ;; ------------------------------------------------ ;;
  ;; ---------  linear pairs of renderers ----------- ;;
  
  ; obj -> renderer
  (define (construct-renderer-object obj)
    (cond
      [(line? obj) (new line-renderer%)]
      [(cube? obj) (new cube-renderer%)]
      [(disk? obj) (new disk-renderer%)]
      [(sphere? obj) (new sphere-renderer%)]
      [(cylinder? obj) (new cylinder-renderer%)]
      [(triangle? obj) (new triangle-renderer%)]
      [(quad? obj) (new quad-renderer%)]
      [(surface? obj)  (new surface-renderer%)]
      [else null]))
  
  ; obj, material, transform -> renderer
  (define initialize-renderer
    (lambda (prim material transform) 
      (let* ([r (construct-renderer-object prim)])
        (unless (null? r)
          (send r set-primitive prim)
          (send r set-material material)
          (send r set-transform transform)
          (send r compute-bounding-box prim))
        r)))

;  (provide/contract 
;   [initialize-renderer (->* (primitive?)
;                             (#:m material? 
;                              #:x vector?)
;1.                             (or/c null? object?))])
;2.                             (or/c null? (is-a?/c renderer%)))])
  
;  (define initialize-renderer
;    (lambda (prim 
;             #:m [material (default-material)]
;             #:x [transform (default-transform)]) 
;      (let* ([r (construct-renderer-object prim)])
;        (unless (null? r)
;          (send r set-primitive prim)
;          (send r set-material material)
;          (send r set-transform transform)
;          (send r compute-bounding-box prim))
;        r)))

;  (provide/contract 
;   [initialize-renderer (->* (primitive?)
;                             (#:m material? 
;                              #:x vector?)
;                             (or/c null? (subclass?/c renderer%)))])
  

)