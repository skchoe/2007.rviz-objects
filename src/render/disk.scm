(module disk mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           sgl
           sgl/gl
           sgl/gl-vectors
           "primitives.scm"
           "render.scm"
           "../math/def.scm"
           "../math/calc.scm")
  
  (provide disk-renderer%)
  (provide render-disk)
  (provide pick-disk)
  
  (define disk-renderer%
    (class renderer% ()
      
      ;; field disk:primitive
      (define disk null)
      ; set-primitive      
      (public set-primitive)
      (define (set-primitive dsk)
        (set! disk dsk))
      
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
               (out-r (disk-outer_radius obj)))
          (set! bbox
                (make-bounding-box (make-point3 (* -1 out-r) (* -1 out-r) (* -1 e))
                                   (make-point3 out-r out-r e)))))
      ; get-bounding-box:
      (inherit get-bounding-box)

         

      ;; render: from renderer%::render
      (override render)
      (define (render)
        (when (null? (get-dl-id))
          (set-dl-id (gl-gen-lists 1))
          (gl-new-list (get-dl-id) 'compile)
          (render-fp material xform disk)
          (gl-end-list))
        (gl-call-list (get-dl-id)))          
      
      ;; pick: from renderer%::render
      (override pick)
      (define (pick)
        (pick-fp xform disk))
      
      (super-new)))

  ;; render disk -> (void)
  (define (render-fp mat xform dsk)
    (render-disk mat xform 
                 (disk-inner_radius dsk)
                 (disk-outer_radius dsk)))

  ;; render-disk radius_inner radius_outer -> (void)
  (define (render-disk mat xform inner outer)
    (let* ((q (gl-new-quadric)))

      (gl-push-attrib 'all-attrib-bits)
        ;material
        (set-material-property mat)
      

    
      (gl-push-matrix)

      ;transformation
      (gl-mult-matrix (vector->gl-float-vector xform))
      
      (gl-disk q inner outer 20 20)
      
      (gl-pop-matrix)

      (gl-pop-attrib)
      ))
  
  ;; pick disk -> (void)
  (define (pick-fp  xform dsk)
    (pick-disk xform 
               (disk-inner_radius dsk)
               (disk-outer_radius dsk)))

  ;; pick-disk radius_inner radius_outer -> (void)
  (define (pick-disk xform inner outer)
    (let* ((q (gl-new-quadric)))
      (gl-push-matrix)
      ;transformation
      (gl-mult-matrix (vector->gl-float-vector xform))
      
      (gl-disk q inner outer 10 10)
      
      (gl-pop-matrix)))

      
)