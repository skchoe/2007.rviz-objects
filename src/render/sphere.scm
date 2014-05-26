(module sphere mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           sgl
           sgl/gl
           sgl/gl-vectors
           "primitives.scm"
           "render.scm"
           "../math/def.scm"
           "../math/calc.scm")

  (provide sphere-renderer%)
  (provide render-sphere)
  (provide pick-sphere)
  
  (define sphere-renderer%
    (class renderer% ()
      
      ;; primitive sphere:primitive
      (define sphere null)
      ; set-primitive
      (public set-primitive)
      (define (set-primitive sp)
        (set! sphere sp))
      
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
        (let ((r (sphere-radius obj)))
          (set! bbox
                (make-bounding-box (make-point3 (* -1 r) (* -1 r) (* -1 r))
                                   (make-point3 r r r)))))
      
      ; get-bounding-box:
      (inherit get-bounding-box)

         

      ;; render: from renderer%::render
      (override render)
      (define (render)
        (when (null? (get-dl-id))
          (set-dl-id (gl-gen-lists 1))
          (gl-new-list (get-dl-id) 'compile)
          (render-fp material xform sphere)
          (gl-end-list))
        (gl-call-list (get-dl-id)))

            
      ;; pick: from renderer%::render
      (override pick)
      (define (pick)
        (pick-fp xform sphere))
      
      (super-new)))
  
  ; define: sphere -> (void)
  (define (render-fp mat xf sph)
    (render-sphere mat xf (sphere-radius sph)))
  
  ;(define (render-sphere radius)   ; radius of sphere
  (define (render-sphere mat xform radius)
    (let* ((q (gl-new-quadric)))
      (gl-push-attrib 'all-attrib-bits)
      ;material
      (set-material-property mat)
    
      (gl-push-matrix)
      ;transfomation
      (gl-mult-matrix (vector->gl-float-vector xform))

      (gl-sphere q radius 20 20)
      
      (gl-pop-matrix)
      (gl-pop-attrib)
      ))
    ; define: sphere -> (void)
  (define (pick-fp xf sph)
    (pick-sphere xf (sphere-radius sph)))
  
  ;(define (render-sphere radius)   ; radius of sphere
  (define (pick-sphere xform radius)
    (let* ((q (gl-new-quadric)))
      (gl-push-matrix)
      ;transfomation
      (gl-mult-matrix (vector->gl-float-vector xform))

      (gl-sphere q radius 10 10)
      
      (gl-pop-matrix)
      ))

)