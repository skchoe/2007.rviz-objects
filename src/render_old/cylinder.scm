(module cylinder mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           sgl
           sgl/gl
           sgl/gl-vectors
           "primitives.scm"
           "render.scm"
           "../math/def.scm"
           "../math/calc.scm")
  
  (provide cylinder-renderer%)
  (provide render-cylinder)
  (provide pick-cylinder)
  
  (define cylinder-renderer%
    (class renderer% ()
      
      ;; field cylinder:primitive
      (define cylinder null)
      ; set-primitive      
      (public set-primitive)
      (define (set-primitive cld)
        (set! cylinder cld))
      
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
        (let* ((r-b (cylinder-base_radius obj))
               (r-t (cylinder-top_radius obj))
               (h (cylinder-height obj))
               (big-r (if (< r-b r-t) r-t r-b)))
          (set! bbox
                (make-bounding-box (make-point3 (* -1 big-r) (* -1 big-r) 0)
                                   (make-point3 big-r big-r h)))))
      ; get-bounding-box:
      (inherit get-bounding-box)

         
      
      ;; render: from renderer%::render
      (override render)
      (define (render)
        (when (null? (get-dl-id))
          (set-dl-id (gl-gen-lists 1))
          (gl-new-list (get-dl-id) 'compile)
          (render-fp material xform cylinder)
          (gl-end-list))
        (gl-call-list (get-dl-id)))      
      
      ;; pick: from renderer%::render
      (override pick)
      (define (pick)
        (pick-fp xform cylinder))
      
      (super-new)))

  ; render cylinder -> (void)
  (define (render-fp mat xform cld)
    (render-cylinder mat xform 
                     (cylinder-base_radius cld)
                     (cylinder-top_radius cld)
                     (cylinder-height cld)))
  
  ;; render-cylinder radius_base radius_top height -> (void)
  (define (render-cylinder mat xform base_radius top_radius height)

    (let* ((q (gl-new-quadric)))
      
      (gl-push-attrib 'all-attrib-bits)
        (set-material-property mat)
    
      (gl-push-matrix)

      ;transformation
      (gl-mult-matrix (vector->gl-float-vector xform))
    
      (gl-cylinder q base_radius top_radius height 50 50)
      
      (gl-pop-matrix)
      (gl-pop-attrib)
      )
      )
  
  ; pick cylinder -> (void)
  (define (pick-fp xform cld)
    (pick-cylinder xform 
                   (cylinder-base_radius cld)
                   (cylinder-top_radius cld)
                   (cylinder-height cld)))
  
  ;; pick-cylinder radius_base radius_top height -> (void)
  (define (pick-cylinder xform base_radius top_radius height)
    (let* ((q (gl-new-quadric)))
      
      (gl-push-matrix)
      ;transformation
      (gl-mult-matrix (vector->gl-float-vector xform))
    
      (gl-cylinder q base_radius top_radius height 50 50)
      
      (gl-pop-matrix)))

)