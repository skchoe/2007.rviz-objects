(module primitives scheme
  (require (lib "class.ss")
           "../math/def.scm")

  
  (provide
   (struct-out vertex); (position normal texture color))
   (struct-out material); (ambient diffuse specular shininess texture-object))
   
   (struct-out gfx_primitive); ())
   (struct-out line); (pt_begin pt_end))
   (struct-out cube); (szx szy szz))
   (struct-out cylinder); (base_radius top_radius height))
   (struct-out disk); (inner_radius outer_radius))
   (struct-out sphere); (radius))
   (struct-out triangle); (vertex0 vertex1 vertex2))
   (struct-out quad); (vertex0 vertex1 vertex2 vertex3))
   (struct-out surface); (lst-triangle))
   
   new-triangle
   new-quad
   new-quad-size
  
  (struct-out bounding-box); (min-posn3 max-posn3))
  print-bounding-box)
  
  ;  position: point3
  ;  normal  : point3
  ;  texture : point2
  (define-struct vertex (position normal texture color))

  (provide/contract 
   [build-vertex (->* (point3?)
                      (#:n point3? 
                       #:t point2?
                       #:c point4?)
                      vertex?)])

  
  (define build-vertex 
    (lambda (position 
             #:n [normal (make-point3 0.0 0.0 1.0)]
             #:t [texture (make-point2 0.5 0.5)]
             #:c [color (make-point4 0.5 0.5 0.5 1.0)])
      (make-vertex position normal texture color)))
                                 
  
  ; point4 point4 point4 number (list width:int height:int cvector)
  (define-struct material (ambient diffuse specular shininess texture-object))

  ;; graphics primitive objects
  (define-struct gfx_primitive ())
  (define-struct (line gfx_primitive) (pt_begin pt_end)) ;(point3 point3)
  (define-struct (cube gfx_primitive) (szx szy szz))
  (define-struct (cylinder gfx_primitive) (base_radius top_radius height))
  (define-struct (disk gfx_primitive) (inner_radius outer_radius))
  (define-struct (sphere gfx_primitive) (radius))
  (define-struct (triangle gfx_primitive) (vertex0 vertex1 vertex2))
  (define-struct (quad gfx_primitive) (vertex0 vertex1 vertex2 vertex3))
  (define-struct (surface gfx_primitive) (lst-triangle))
  
  (define (new-triangle size)
    (let* ((halfv (/ size 2.0))
           (c null)
           (v (make-point3 0.0 0.0 1.0))
           (p0 (make-point3 (* -1 halfv) 0.0 0.0))
           (p1 (make-point3 halfv 0.0 0.0))
           (p2 (make-point3 0.0 size 0.0))
           (vtx0 (make-vertex p0 v null c))
           (vtx1 (make-vertex p1 v null c))
           (vtx2 (make-vertex p2 v null c)))
      (make-triangle vtx0 vtx1 vtx2)))
  
  (define (new-quad vtx0 vtx1 vtx2 vtx3)
    (make-quad vtx0 vtx1 vtx2 vtx3))
  
  (define (new-quad-size size)
    (let* ((halfv (/ size 2.0))
           (-halfv (* -1 halfv))
           (c null)
           (v (make-point3 0.0 0.0 1.0))
           (p0 (make-point3 -halfv -halfv 0.0))
           (p1 (make-point3 halfv -halfv 0.0))
           (p2 (make-point3 halfv halfv 0.0))
           (p3 (make-point3 -halfv halfv 0.0))
           (vt0 (make-point2 0.0 0.0))
           (vt1 (make-point2 0.0 1.0))
           (vt2 (make-point2 1.0 1.0))
           (vt3 (make-point2 1.0 0.0))
           (vtx0 (make-vertex p0 v vt0 c))
           (vtx1 (make-vertex p1 v vt1 c))
           (vtx2 (make-vertex p2 v vt2 c))
           (vtx3 (make-vertex p3 v vt3 c))
           )
      (new-quad vtx0 vtx1 vtx2 vtx3)))
  
  
  (define-struct bounding-box (min-posn3 max-posn3))
  
      
  (define print-bounding-box 
    (lambda (bbx)
      (if (null? bbx) 
          (printf "print-bbx - bbox is null\n")
          (let* ((min-posn3 (bounding-box-min-posn3 bbx))
                 (max-posn3 (bounding-box-max-posn3 bbx)))
            (printf "print-bbx - min(~s ~s ~s) \t max(~s ~s ~s)\n"
                    (point3-x min-posn3) (point3-y min-posn3) (point3-z min-posn3)
                    (point3-x max-posn3) (point3-y max-posn3) (point3-z max-posn3))))))
  
)