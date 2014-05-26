(module init-lst-renderers scheme
  (require (lib "mred.ss" "mred")
           (lib "list.ss")
           (lib "class.ss")
           (lib "graphics.ss" "graphics")
           (lib "etc.ss")
           "../src/math/def.scm"
           "../src/math/calc.scm"
           "../src/render/render.scm"
           "../src/render/primitives.scm"
           "../src/operation/renderer-ops.scm")
  
  (provide construct-lst-renderers
           gen-renderer)
  
  (define construct-lst-renderers
    (lambda ()
      (let* ((cubes (gen-lst-renderers 'cube 50))
             (cylinders (gen-lst-renderers 'cylinder 30))
             (disks (gen-lst-renderers 'disk 20))
             (quads (gen-lst-renderers 'quad 20))
             (spheres (gen-lst-renderers 'sphere 30))
             (triangles (gen-lst-renderers 'triangle 10)))
        (append cubes cylinders disks quads spheres triangles))))

  (define gen-lst-renderers 
    (lambda (geo-kind num)
      (for/fold([lst empty])
               ([i (in-range 1 num)])
               (cons (gen-renderer geo-kind #t empty) lst))))
  
  (define gen-renderer
    (lambda (geo-kind random? lst)
      (let* ([prim (if (empty? lst)
                      (match geo-kind
                        ['cube (make-cube .5 .5 .5)]
                        ['cylinder (make-cylinder .3 .3 1)]
                        ['disk (make-disk .2 .5)]
                        ['quad (gen-unit-quad)]
                        ['sphere (make-sphere .5)]
                        ['triangle (gen-unit-triangle)]
                        ['line (gen-unit-line)]
                        [_ null])
                      (match geo-kind
                        ['cube (make-cube (list-ref lst 0) 
                                          (list-ref lst 1) 
                                          (list-ref lst 2))]
                        ['cylinder (make-cylinder (list-ref lst 0) 
                                                  (list-ref lst 1)
                                                  (list-ref lst 2))]
                        ['disk (make-disk (list-ref lst 0) 
                                          (list-ref lst 1))]
                        ['quad (make-quad (list-ref lst 0) 
                                          (list-ref lst 1)
                                          (list-ref lst 2) 
                                          (list-ref lst 3))]
                        ['sphere (make-sphere (list-ref lst 0))]
                        ['triangle (make-triangle (list-ref lst 0) 
                                                  (list-ref lst 1)
                                                  (list-ref lst 2))]
                        ['line (make-line (list-ref lst 0) (list-ref lst 1))]
                        [_ null]))]
                        
            [mat (if random? (random-material) (default-material))]
            [xfm (if random? (random-pos-xform 100) (default-transform))])
        (unless (null? prim)
          (initialize-renderer prim mat xfm)))))

  
  (define random-material
    (lambda ()
      (make-material (make-point4 (random) (random) (random) 1.0)
                     (make-point4 (random) (random) (random) 1.0)
                     (make-point4  0.0 0.0 0.0 1.0)
                     0.0
                     empty)))
  
  (define random-pos-xform
    (lambda (r)
      (let* ([rx (- (random r) (/ r 2.0))]
             [ry (- (random r) (/ r 2.0))]
             [rz (- (random r) (/ r 2.0))])
      (vector 1.0 0.0 0.0 0.0
              0.0 1.0 0.0 0.0
              0.0 0.0 1.0 0.0
              rx ry rz 1.0))))
    
  (define gen-unit-quad
    (lambda ()
      (let* ([vtx0 (make-vertex (make-point3 1 -1 0) 
                                (make-point3 0 0 1) 
                                (make-point2 1 1) 
                                (make-point4 1 0 0 1))]
             [vtx1 (make-vertex (make-point3  1  1 0) 
                                (make-point3 0 0 1) 
                                (make-point2 1 0) 
                                (make-point4 0 1 0 1))]
             [vtx2 (make-vertex (make-point3 -1  1 0) 
                                (make-point3 0 0 1) 
                                (make-point2 0 0) 
                                (make-point4 0 0 1 1))]
             [vtx3 (make-vertex (make-point3 -1 -1 0) 
                                (make-point3 0 0 1) 
                                (make-point2 0 1) 
                                (make-point4 1 1 1 1))])
        (make-quad vtx0 vtx1 vtx2 vtx3))))
  
  (define gen-line
    (lambda (p0 p1)
      (make-line p0 p1)))
      
  (define gen-unit-line
    (lambda ()
      (make-line (make-point3 1 0 0) (make-point3 -1 0 0))))

             
             
             
  (define gen-unit-triangle
    (lambda ()
      (let* ([vtx0 (make-vertex (make-point3   1 0 0) (make-point3 0 0 1) (make-point2 1 1) (make-point4 1 0 0 1))]
             [vtx1 (make-vertex (make-point3  -1 0 0) (make-point3 0 0 1) (make-point2 1 0) (make-point4 0 1 0 1))]
             [vtx2 (make-vertex (make-point3   0 1 0) (make-point3 0 0 1) (make-point2 0 0) (make-point4 0 0 1 1))])
        (make-triangle vtx0 vtx1 vtx2))))
  )
      
