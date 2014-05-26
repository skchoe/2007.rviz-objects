(module line mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           sgl
           sgl/gl
           sgl/gl-vectors
           "primitives.scm"
           "render.scm"
           "../math/def.scm"
           "../math/calc.scm")
    
  (provide line-renderer%)
  (provide render-line)
  (provide pick-line)
  
  (define line-renderer%
    (class renderer% ()
      
      ;; primitive sphere:primitive
      (define line null)
      ; set-primitive
      (public set-primitive)
      (define (set-primitive ln)
        (set! line ln))
      
      ;; field xform: inherit from renderer%
      (inherit-field xform)
      ; set-transform: inherit from renderer%
      (inherit set-transform)
      
      ;; field material: inherit from renderer%
      (inherit-field material)
      ; set-material:
      (inherit set-material)
      
      ;; field dl-id: inherit from renderer%
      (inherit-field dl-id)
      ; set-dl-id:
      (inherit set-dl-id)
      ; get-dl-id:
      (inherit get-dl-id)

      ;; compute-bounding-box: from renderer%::compute-bounding-box
      ;; field bbox: inherit from renderer%
      (inherit-field bbox)
      ; set-transform: inherit from renderer%
      (override compute-bounding-box)
      (define (compute-bounding-box obj)
        (let* ((e 0.001)
               (p-b (line-pt_begin obj))
               (p-e (line-pt_end obj))
               (b-x (point3-x p-b)) (b-y (point3-y p-b)) (b-z (point3-z p-b))
               (e-x (point3-x p-e)) (e-y (point3-y p-e)) (e-z (point3-z p-e)))
          (set! bbox
                (make-bounding-box (make-point3 (if (equal? b-x e-x) (* -1 e) (if (< b-x e-x) b-x e-x))
                                                (if (equal? b-y e-y) (* -1 e) (if (< b-y e-y) b-y e-y))
                                                (if (equal? b-z e-z) (* -1 e) (if (< b-z e-z) b-z e-z)))
                                   (make-point3 (if (equal? b-x e-x) e (if (< b-x e-x) e-x b-x))
                                                (if (equal? b-y e-y) e (if (< b-y e-y) e-y b-y))
                                                (if (equal? b-z e-z) e (if (< b-z e-z) e-z b-z)))))))
      
      ; get-bounding-box:
      (inherit get-bounding-box)

         

      ;; render: from renderer%::render
      (override render)
      (define (render)
        (render-fp material xform line))
      
      ;; pick: from renderer%::render
      (override pick)
      (define (pick)
        (pick-fp xform line))
      
      (super-new)))
  
  ; define: sphere -> (void)
  (define (render-fp mat xf ln)
    (render-line mat xf (line-pt_begin ln) (line-pt_end ln)))
  
  ;(define (render-sphere radius)   ; radius of sphere
  (define (render-line mat xform pt_begin  pt_end)
    (gl-push-attrib 'all-attrib-bits)
      ;material
      (set-material-property mat)
    
    (gl-push-matrix)

    ;transfomation
    (gl-mult-matrix (vector->gl-float-vector xform))
    
    (gl-begin 'lines)
    (gl-vertex (point3-x pt_begin) (point3-y pt_begin) (point3-z pt_begin))
    (gl-vertex (point3-x pt_end) (point3-y pt_end) (point3-z pt_end))
    (gl-end)
      
    (gl-pop-matrix)

    (gl-pop-attrib)
    )
  
  ; define: line -> (void)
  (define (pick-fp xf ln)
    (pick-line xf (line-pt_begin ln) (line-pt_end ln)))
  
  ;(define (pick-line radius)   ; radius of sphere
  (define (pick-line xform pt_begin  pt_end)
    (gl-push-matrix)
    ;transfomation
    (gl-mult-matrix (vector->gl-float-vector xform))
    
    (gl-begin 'lines)
    (gl-vertex (point3-x pt_begin) (point3-y pt_begin) (point3-z pt_begin))
    (gl-vertex (point3-x pt_end) (point3-y pt_end) (point3-z pt_end))
    (gl-end)
      
    (gl-pop-matrix))

)