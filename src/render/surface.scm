(module surface mzscheme
    (require (lib "mred.ss" "mred")
             (lib "class.ss")
             (lib "math.ss")
             (lib "list.ss")
             sgl
             sgl/gl
             sgl/gl-vectors
             "primitives.scm"
             "render.scm"
             "gl-texture-helpers.ss"
             "../math/def.scm"
             "../math/calc.scm")
  
  
  (provide surface-renderer%)
  (provide render-surface)
  ;  (provide pick-triangle)
  
  (define surface-renderer%
    (class renderer% ()
      
      ;; field lst-triangle:primitive
      (define lst-triangle null)
      ; set-primitive      
      (public set-primitive)
      (define (set-primitive ltg)
        (set! lst-triangle ltg))
      
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
        (let* ((lst-triangle (surface-lst-triangle obj))
               (lst-pts (lst-triangle->lst-points lst-triangle)))
          (set! bbox (calc-bounding-box lst-pts))))
      ; get-bounding-box:
      (inherit get-bounding-box)

          
         

      ;; render: from renderer%::render
      (override render)
      (define (render)
        (when (null? (get-dl-id))
          (set-dl-id (gl-gen-lists 1))
          (gl-new-list (get-dl-id) 'compile)
          (render-fp material xform lst-triangle)
          (gl-end-list))
        (gl-call-list (get-dl-id)))
      
      ;; pick: from renderer%::render
      (override pick)
      (define (pick)
        (pick-fp xform lst-triangle))
      
      (super-new)))
  
  ; render triangle -> (void)
  (define render-fp 
    (lambda (mat xf srf)
      (render-surface mat xf srf)))
  
  
  
  (define render-surface
    (lambda (mat xform srf)
      ;(printf "size of triangle set = ~s\n" (length (surface-lst-triangle srf)))
      
      (gl-shade-model 'smooth)

      (gl-push-attrib 'all-attrib-bits)
        ;material
        (set-material-property mat)
    
        (gl-push-matrix)
        (gl-mult-matrix (vector->gl-float-vector xform))
        (render-faces (surface-lst-triangle srf))  
        (gl-pop-matrix)
        (gl-pop-attrib)))
  
  
  ;; face info -> rendering
  (define render-faces 
    (lambda (lst-tg)
      
      (gl-begin 'triangles)

      (for-each render-triangle lst-tg)
      
      (gl-end)))
  
  (define render-triangle 
    (lambda (triangle)
    (let* ((vertex0 (triangle-vertex0 triangle))
           (vertex1 (triangle-vertex1 triangle))
           (vertex2 (triangle-vertex2 triangle))
           
           (position0 (vertex-position vertex0))
           (position1 (vertex-position vertex1))
           (position2 (vertex-position vertex2))
           (normal0 (vertex-normal vertex0))
           (normal1 (vertex-normal vertex1))
           (normal2 (vertex-normal vertex2))
           (texture0 (vertex-texture vertex0))
           (texture1 (vertex-texture vertex1))
           (texture2 (vertex-texture vertex2))
           (color0 (vertex-color vertex0))
           (color1 (vertex-color vertex1))
           (color2 (vertex-color vertex2)))


        ; vertex0
        (set-color-material-amb-dif color0)
        (gl-normal (point3-x normal0) (point3-y normal0) (point3-z normal0))
        (glTexCoord2d (point2-s texture0) (point2-t texture0))
        (gl-vertex (point3-x position0) (point3-y position0) (point3-z position0))

        ; vertex1
        (set-color-material-amb-dif color1)
        (gl-normal (point3-x normal1) (point3-y normal1) (point3-z normal1))
        (glTexCoord2d (point2-s texture1) (point2-t texture1))
        (gl-vertex (point3-x position1) (point3-y position1) (point3-z position1))

        ; vertex2
        (set-color-material-amb-dif color2)
        (gl-normal (point3-x normal2) (point3-y normal2) (point3-z normal2))
        (glTexCoord2d (point2-s texture2) (point2-t texture2))
        (gl-vertex (point3-x position2) (point3-y position2) (point3-z position2))


      ;; normal vector drawing.
      (when #f
        (render-normal normal0 position0)
        (render-normal normal1 position1)
        (render-normal normal2 position2)))))

    

  (define render-normal
    (lambda (direction position)
      (let* ((destx (+ (point3-x position) (point3-x direction)))
             (desty (+ (point3-y position) (point3-y direction)))
             (destz (+ (point3-z position) (point3-z direction))))
        (render-line (make-point3 destx desty destz) position)
        )))
    
  (define render-line
    (lambda (v0 v1)
      (gl-begin 'lines)
      (gl-vertex (point3-x v0) (point3-y v0) (point3-z v0))
      (gl-vertex (point3-x v1) (point3-y v1) (point3-z v1))
      (gl-end)))
      
                
  
    ;;(gl-shade-model 'smooth)
    ;; Draw inside radius cylinder.
  ; pick triangle -> (void)
  (define pick-fp 
    (lambda (xf srf)
      (pick-surface xf srf)))

  (define pick-surface 
    (lambda (xform srf)
      (gl-push-matrix)
      (gl-mult-matrix (vector->gl-float-vector xform))
      (pick-faces (surface-lst-triangle srf))
      (gl-pop-matrix)))
      
  (define pick-faces
    (lambda (lst-tg)
      (for-each pick-triangle lst-tg)))
             
  (define pick-triangle 
    (lambda (tg)
      (let* ((vertex0 (triangle-vertex0 tg))
             (vertex1 (triangle-vertex1 tg))
             (vertex2 (triangle-vertex2 tg))
             (position0 (vertex-position vertex0))
             (position1 (vertex-position vertex1))
             (position2 (vertex-position vertex2)))

      (gl-begin 'triangles)
      (gl-vertex (point3-x position0) (point3-y position0) (point3-z position0))
      (gl-vertex (point3-x position1) (point3-y position1) (point3-z position1))
      (gl-vertex (point3-x position2) (point3-y position2) (point3-z position2))
      (gl-end))))
        
  (define lst-triangle->lst-points 
    (lambda (lst-triangle)
      (let* ((lst-points empty))
        (foldl triangle->list-points lst-points lst-triangle))))
        
  (define triangle->list-points
    (lambda (trgl lst-points)
      (let* ((vtx0 (triangle-vertex0 trgl))
             (vtx1 (triangle-vertex1 trgl))
             (vtx2 (triangle-vertex2 trgl)))
        (append lst-points 
                (list (vertex-position vtx0) (vertex-position vtx1) (vertex-position vtx2))))))
  
  (define calc-bounding-box 
    (lambda (lst-pts)
      (let ((bbox null))
        (foldl update-bounding-box bbox lst-pts))))
  
  (define update-bounding-box
    (lambda (p3 bbox)
      (if (null? bbox)
          (let* ((e 0.001))
            (make-bounding-box (make-point3 (- (point3-x p3) e)
                                            (- (point3-y p3) e)
                                            (- (point3-z p3) e))
                               (make-point3 (+ (point3-x p3) e)
                                            (+ (point3-y p3) e)
                                            (+ (point3-z p3) e))))
          (let* ((min-pos (bounding-box-min-posn3 bbox))
                 (max-pos (bounding-box-min-posn3 bbox))
                 (min-max-list (update-min-max min-pos max-pos p3)))
;            (printf "srf-bbox-min = (~s ~s ~s) max = (~s ~s ~s)\n" 
;                    (point3-x min-pos) (point3-y min-pos) (point3-z min-pos)
;                    (point3-x max-pos) (point3-y max-pos) (point3-z max-pos))
            (make-bounding-box (list-ref min-max-list 0)
                               (list-ref min-max-list 1))))))
      
      
)