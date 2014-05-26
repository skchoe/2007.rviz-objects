(module quad mzscheme
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
   
  (provide quad-renderer%)
  (provide render-quad)
  (provide pick-quad)
  
  (define quad-renderer%
    (class renderer% ()
      
      ;; field quad:primitive
      (define quad null)
      ; set-primitive      
      (public set-primitive)
      (define (set-primitive qd)
        (set! quad qd))
      
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
               (vtx0 (quad-vertex0 obj))
               (vtx1 (quad-vertex1 obj))
               (vtx2 (quad-vertex2 obj))
               (vtx3 (quad-vertex3 obj))
               (pt0 (vertex-position vtx0))
               (pt1 (vertex-position vtx1))
               (pt2 (vertex-position vtx2))
               (pt3 (vertex-position vtx3))
               (xx (max (point3-x pt0) (point3-x pt1) (point3-x pt2) (point3-x pt3)))
               (xy (max (point3-y pt0) (point3-y pt1) (point3-y pt2) (point3-y pt3)))
               (xz (max (point3-z pt0) (point3-z pt1) (point3-z pt2) (point3-z pt3)))
               (nx (min (point3-x pt0) (point3-x pt1) (point3-x pt2) (point3-x pt3)))
               (ny (min (point3-y pt0) (point3-y pt1) (point3-y pt2) (point3-y pt3)))
               (nz (min (point3-z pt0) (point3-z pt1) (point3-z pt2) (point3-z pt3))))

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
        (render-fp material xform quad))
      
      ;; pick: from renderer%::render
      (override pick)
      (define (pick)
        (pick-fp xform quad))
      
      (super-new)))
  
  ; render triangle -> (void)
  (define (render-fp mat xf qd)
    (render-quad mat xf 
                     (quad-vertex0 qd)
                     (quad-vertex1 qd)
                     (quad-vertex2 qd)
                     (quad-vertex3 qd)))
    
  (define (render-quad mat xform vertex0 vertex1 vertex2 vertex3)
    
    (gl-shade-model 'flat)

    (gl-push-attrib 'all-attrib-bits)
      ;material
      (set-material-property mat)
    
    (gl-push-matrix)
    (gl-mult-matrix (vector->gl-float-vector xform))
    
    (let* ((normal0 (vertex-normal vertex0))
           (normal1 (vertex-normal vertex1))
           (normal2 (vertex-normal vertex2))
           (normal3 (vertex-normal vertex3))
           (position0 (vertex-position vertex0))
           (position1 (vertex-position vertex1))
           (position2 (vertex-position vertex2))
           (position3 (vertex-position vertex3))
           (color0 (vertex-color vertex0))
           (color1 (vertex-color vertex1))
           (color2 (vertex-color vertex2))
           (color3 (vertex-color vertex3)))

      (gl-begin 'quads)

      ; 3 2
      ; 0 1
      ; vertex0
      (set-color-material-amb-dif color0)
      (gl-normal (point3-x normal0) (point3-y normal0) (point3-z normal0))
      (glTexCoord2i 0 0)
      (gl-vertex (point3-x position0) (point3-y position0) (point3-z position0))

      ; vertex1
      (set-color-material-amb-dif color1)
      (gl-normal (point3-x normal1) (point3-y normal1) (point3-z normal1))
      (glTexCoord2i 1 0)      
      (gl-vertex (point3-x position1) (point3-y position1) (point3-z position1))

      ; vertex2
      (set-color-material-amb-dif color2)
      (gl-normal (point3-x normal2) (point3-y normal2) (point3-z normal2))
      (glTexCoord2i 1 1)      
      (gl-vertex (point3-x position2) (point3-y position2) (point3-z position2))

      ; vertex3
      (set-color-material-amb-dif color3)
      (gl-normal (point3-x normal3) (point3-y normal3) (point3-z normal3))
      (glTexCoord2i 0 1)      
      (gl-vertex (point3-x position3) (point3-y position3) (point3-z position3))

      (gl-end)
      
      (gl-pop-matrix)

      (gl-pop-attrib)))
    
    ;;(gl-shade-model 'smooth)
    ;; Draw inside radius cylinder.
  ; pick quad -> (void)
  (define (pick-fp xf qd)
    (pick-quad xf 
               (quad-vertex0 qd)
               (quad-vertex1 qd)
               (quad-vertex2 qd)
               (quad-vertex3 qd)
               ))
    
  (define (pick-quad xform vertex0 vertex1 vertex2 vertex3)
    (gl-push-matrix)
    (gl-mult-matrix (vector->gl-float-vector xform))
    
    (let* ((position0 (vertex-position vertex0))
           (position1 (vertex-position vertex1))
           (position2 (vertex-position vertex2))
           (position3 (vertex-position vertex3)))

      (gl-begin 'quads)
      (gl-vertex (point3-x position0) (point3-y position0) (point3-z position0))
      (gl-vertex (point3-x position1) (point3-y position1) (point3-z position1))
      (gl-vertex (point3-x position2) (point3-y position2) (point3-z position2))
      (gl-vertex (point3-x position3) (point3-y position3) (point3-z position3))
      (gl-end)
      
      (gl-pop-matrix)))

)