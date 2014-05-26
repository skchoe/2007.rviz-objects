(module triangle mzscheme
    (require (lib "mred.ss" "mred")
             (lib "class.ss")
             (lib "math.ss")
             sgl
             sgl/gl
             sgl/gl-vectors
             "primitives.scm"
             "render.scm"
             "../math/def.scm"
             "../math/calc.scm")
   
  (provide triangle-renderer%)
  (provide render-triangle)
  (provide pick-triangle)
  
  (define triangle-renderer%
    (class renderer% ()
      
      ;; field triangle:primitive
      (define triangle null)
      ; set-primitive      
      (public set-primitive)
      (define (set-primitive tg)
        (set! triangle tg))
      
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
               (vtx0 (triangle-vertex0 obj))
               (vtx1 (triangle-vertex1 obj))
               (vtx2 (triangle-vertex2 obj))
               (pt0 (vertex-position vtx0))
               (pt1 (vertex-position vtx1))
               (pt2 (vertex-position vtx2))
               (xx (max (point3-x pt0) (point3-x pt1) (point3-x pt2)))
               (xy (max (point3-y pt0) (point3-y pt1) (point3-y pt2)))
               (xz (max (point3-z pt0) (point3-z pt1) (point3-z pt2)))
               (nx (min (point3-x pt0) (point3-x pt1) (point3-x pt2)))
               (ny (min (point3-y pt0) (point3-y pt1) (point3-y pt2)))
               (nz (min (point3-z pt0) (point3-z pt1) (point3-z pt2))))

          (set! bbox
                (make-bounding-box (make-point3 (if (equal? xx nx) (+ xx e) xx)
                                                (if (equal? xy ny) (+ xy e) xy)
                                                (if (equal? xz nz) (+ xz e) xz))
                                   (make-point3 (if (equal? xx nx) (- xx e) nx)
                                                (if (equal? xy ny) (- xy e) ny)
                                                (if (equal? xz nz) (- xz e) nz))))))
      ; get-bounding-box:
      (inherit get-bounding-box)

         

      ;; render: from renderer%::render
      (override render)
      (define (render)
        (when (null? (get-dl-id))
          (set-dl-id (gl-gen-lists 1))
          (gl-new-list (get-dl-id) 'compile)
          (render-fp material xform triangle)
          (gl-end-list))
        (gl-call-list (get-dl-id)))
      
      ;; pick: from renderer%::render
      (override pick)
      (define (pick)
        (pick-fp xform triangle))
      
      (super-new)))
  
  ; render triangle -> (void)
  (define (render-fp mat xf trgl)
    (render-triangle mat xf 
                     (triangle-vertex0 trgl)
                     (triangle-vertex1 trgl)
                     (triangle-vertex2 trgl)))
    
  (define (render-triangle mat xform vertex0 vertex1 vertex2)
    
    (gl-shade-model 'flat)

    (gl-push-attrib 'all-attrib-bits)
      ;material
      (set-material-property mat)
    
    (gl-push-matrix)

    (gl-mult-matrix (vector->gl-float-vector xform))
    
    (let* ((normal0 (vertex-normal vertex0))
           (normal1 (vertex-normal vertex1))
           (normal2 (vertex-normal vertex2))
           (position0 (vertex-position vertex0))
           (position1 (vertex-position vertex1))
           (position2 (vertex-position vertex2))
           (color0 (vertex-color vertex0))
           (color1 (vertex-color vertex1))
           (color2 (vertex-color vertex2)))

      (gl-begin 'triangles)

      ; vertex0
      (unless (eq? null color0)
        (gl-material-v 'front-and-back
                       'ambient-and-diffuse
                       (vector->gl-float-vector 
                        (vector (point4-x color0) (point4-y color0) 
                                (point4-z color0) (point4-w color0)))))
      (gl-normal (point3-x normal0) (point3-y normal0) (point3-z normal0))
      (gl-vertex (point3-x position0) (point3-y position0) (point3-z position0))

      ; vertex1
      (unless (eq? null color1)
        (gl-material-v 'front-and-back
                       'ambient-and-diffuse
                       (vector->gl-float-vector 
                        (vector (point4-x color1) (point4-y color1) 
                                (point4-z color1) (point4-w color1)))))
      (gl-normal (point3-x normal1) (point3-y normal1) (point3-z normal1))
      (gl-vertex (point3-x position1) (point3-y position1) (point3-z position1))

      ; vertex2
      (unless (eq? null color2)
        (gl-material-v 'front-and-back
                       'ambient-and-diffuse
                       (vector->gl-float-vector 
                        (vector (point4-x color2) (point4-y color2) 
                                (point4-z color2) (point4-w color2)))))
      (gl-normal (point3-x normal2) (point3-y normal2) (point3-z normal2))
      (gl-vertex (point3-x position2) (point3-y position2) (point3-z position2))

      (gl-end)
      
      (gl-pop-matrix)

      (gl-pop-attrib)))
    
    ;;(gl-shade-model 'smooth)
    ;; Draw inside radius cylinder.
  ; pick triangle -> (void)
  (define (pick-fp xf trgl)
    (pick-triangle xf 
                   (triangle-vertex0 trgl)
                   (triangle-vertex1 trgl)
                   (triangle-vertex2 trgl)))
    
  (define (pick-triangle xform vertex0 vertex1 vertex2)
    (gl-push-matrix)
    (gl-mult-matrix (vector->gl-float-vector xform))
    
    (let* ((position0 (vertex-position vertex0))
           (position1 (vertex-position vertex1))
           (position2 (vertex-position vertex2)))

      (gl-begin 'triangles)
      (gl-vertex (point3-x position0) (point3-y position0) (point3-z position0))
      (gl-vertex (point3-x position1) (point3-y position1) (point3-z position1))
      (gl-vertex (point3-x position2) (point3-y position2) (point3-z position2))
      (gl-end)
      
      (gl-pop-matrix)))

)