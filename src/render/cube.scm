(module cube mzscheme
    (require (lib "mred.ss" "mred")
             (lib "class.ss")
             (lib "math.ss")
             sgl
             sgl/gl
             sgl/gl-vectors
             "primitives.scm"
             "../math/def.scm"
             "../math/calc.scm"
             "render.scm")

  (provide cube-renderer%)
  (provide render-cube)
  (provide pick-cube)
  
  (define cube-renderer%
    (class renderer% ()

      ;; field cube:primitive
      (define cube null)
      ; set-primitive
      (public set-primitive)
      (define (set-primitive cb)
        (set! cube cb))

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
      ; set-bounding-box:
      (inherit set-bounding-box)
      
      ; get-bounding-box:
      (inherit get-bounding-box)

      (override compute-bounding-box)
      (define (compute-bounding-box obj)
        (let* ((szx (cube-szx obj))
               (szy (cube-szy obj))
               (szz (cube-szz obj)))
          (set! bbox
                (make-bounding-box (make-point3 (* -1 (/ szx 2)) (* -1 (/ szy 2)) (* -1 (/ szz 2)))
                                   (make-point3 (/ szx 2) (/ szy 2) (/ szz 2))
                                   ))))

         
      
      ;; render: from renderer%::render
      (override render)
      (define (render)
;        (when (null? (get-dl-id))
;          (set-dl-id (gl-gen-lists 1))
;          (gl-new-list (get-dl-id) 'compile)
;          (render-fp material xform cube)
;          (gl-end-list)
;        (gl-call-list (get-dl-id))))

        (render-fp material xform cube))

      ;; pick: from renderer%::pick
      (override pick)
      (define (pick)
        (pick-fp xform cube))

         
         
      (super-new)))

  ; render: cube -> (void)
  (define (render-fp mat xform cb)
    (let* ((halfx (/ (cube-szx cb) 2.0))
           (halfy (/ (cube-szy cb) 2.0))
           (halfz (/ (cube-szz cb) 2.0)))
      (render-cube mat xform
                   (* -1 halfx) halfx
                   (* -1 halfy) halfy
                   (* -1 halfz) halfz))) 

  ; render-box: 
  ; x-range
  ; y-range          -------> (void)
  ; z-range
  (define (render-cube mat xform xmin xmax ymin ymax zmin zmax) 

    (gl-shade-model 'flat)
        
    (gl-push-attrib 'all-attrib-bits)
      (set-material-property mat)
    
    (gl-push-matrix)
      (gl-mult-matrix (vector->gl-float-vector xform))
      (cube-geometry xmin xmax ymin ymax zmin zmax)
    (gl-pop-matrix)
    
    (gl-pop-attrib)
    )

  
  
  ; pick: cube -> (void)
  (define (pick-fp ;table 
           xform cb)
    (let* ((halfx (/ (cube-szx cb) 2.0))
           (halfy (/ (cube-szy cb) 2.0))
           (halfz (/ (cube-szz cb) 2.0)))
      (pick-cube ;table 
                 xform
                 (* -1 halfx) halfx
                 (* -1 halfy) halfy
                 (* -1 halfz) halfz))) 
  

  ; render-box: 
  ; x-range
  ; y-range          -------> (void)
  ; z-range
  (define (pick-cube xform xmin xmax ymin ymax zmin zmax)
    
    (gl-push-matrix)
    (gl-mult-matrix (vector->gl-float-vector xform))

    (cube-geometry xmin xmax ymin ymax zmin zmax)
    
    (gl-pop-matrix))

 
  (define cube-geometry
    (lambda (xmin xmax ymin ymax zmin zmax)
      ; z-(-)s
      (gl-begin 'quads)
      (gl-normal 0.0 0.0 -1.0)
      (gl-vertex xmin ymin zmin)
      (gl-vertex xmin ymax zmin)
      (gl-vertex xmax ymax zmin)                
      (gl-vertex xmax ymin zmin)
      (gl-end)
      
      ; z-(+)
      (gl-begin 'quads)
      (gl-normal 0.0 0.0 1.0)
      (gl-vertex xmin ymin zmax)
      (gl-vertex xmax ymin zmax)
      (gl-vertex xmax ymax zmax)                
      (gl-vertex xmin ymax zmax)
      (gl-end)
      
      ; x-(-)
      (gl-begin 'quads)
      (gl-normal -1.0 0.0 0.0)
      (gl-vertex xmin ymin zmin)
      (gl-vertex xmin ymin zmax)
      (gl-vertex xmin ymax zmax)                
      (gl-vertex xmin ymax zmin)
      (gl-end)
      
      ; x-(+)
      (gl-begin 'quads)
      (gl-normal 1.0 0.0 0.0)
      (gl-vertex xmax ymin zmin)
      (gl-vertex xmax ymax zmin)
      (gl-vertex xmax ymax zmax)                
      (gl-vertex xmax ymin zmax)
      (gl-end)
      
      ; y-(-)
      
      (gl-begin 'quads)
      (gl-normal 0.0 -1.0 0.0)
      (gl-vertex xmin ymin zmin)
      (gl-vertex xmax ymin zmin)
      (gl-vertex xmax ymin zmax)                
      (gl-vertex xmin ymin zmax)
      (gl-end)
      
      ; y-(+)
      
      (gl-begin 'quads)
      (gl-normal 0.0 1.0 0.0)
      (gl-vertex xmin ymax zmin)
      (gl-vertex xmin ymax zmax)
      (gl-vertex xmax ymax zmax)                
      (gl-vertex xmax ymax zmin)
      (gl-end)))

)