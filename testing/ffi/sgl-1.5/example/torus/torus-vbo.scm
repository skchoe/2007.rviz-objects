(module torus-vbo scheme
  (require mred
           mzlib/class
           scheme/foreign
           sgl
           sgl/gl
           sgl/gl-vectors
           sgl/gl-types
           "../../gl-1.5.scm"
           )
  
  (unsafe!)
  
  (define controls? #t)
  
  (define vbo-init #f)
  
  (define v-vbo-id '())
  
  (define-cstruct _s-vertex ([x _gl-float]
                             [y _gl-float]
                             [z _gl-float]
                             [nx _gl-float]
                             [ny _gl-float]
                             [nz _gl-float]                             
                             [r _gl-float]
                             [g _gl-float]
                             [b _gl-float]))

  (define Rds 1.0)
  (define rds 0.5)
  (define num_row 16)
  (define num_col 16)
  (define draw-in-line #f)
  (define num_vtx (*  num_row num_col))

  (define (define-torus-vertices Rds rds num_row num_col)
    (let* ([vertices (malloc _s-vertex num_vtx)]
           [pi 3.1415265359])
      (for ([r (in-range num_row)])
        (for ([c (in-range num_col)])
          (let* ([u (* (/ r num_row) (* pi 2.0) )]
                 [v (* (/ c num_col) (* pi 2.0))]
                 [coeffv (+ Rds (* rds (cos v)))]

                 [x (* coeffv (cos u))]
                 [y (* coeffv (sin u))]
                 [z (* rds (sin v))]

                 [nx (* coeffv (- (cos u) 1))]
                 [ny (* coeffv (- (sin u) 1))]
                 [nz 0]
                 
                 [R (/ r num_row)]
                 [G (/ c num_col)]
                 [B 0.7]
                 
                 [offset (+ (* r num_col) c)]
                 )
            ; now the s-vertex structure
            (ptr-set! vertices
                      _s-vertex
                      offset
                      (make-s-vertex x y z nx ny nz R G B)))))

      vertices))

  ;; vertex-set, num_vtx known
  (define vertices->geom-cpointer
    (lambda (num_row num_col vtx-set)
      (let* ([length (* 3 num_vtx)]
             [vg-set (malloc _gl-float length)])
        (for ([i (in-range num_vtx)])
          (let* ([idxcnt (* i 3)]
                 [current-vtx (ptr-ref vtx-set _s-vertex i)])
            (ptr-set! vg-set _gl-float idxcnt (s-vertex-x current-vtx))
            (ptr-set! vg-set _gl-float (+ 1 idxcnt) (s-vertex-y current-vtx))
            (ptr-set! vg-set _gl-float (+ 2 idxcnt) (s-vertex-z current-vtx))))
        
        (values vg-set length))))
  
  ;; normal-set, num_vtx known
  (define vertices->norm-cpointer
    (lambda (num_row num_col vtx-set)
      (let* ([length (* 3 num_vtx)]
             [vn-set (malloc _gl-float length)])
        (for ([i (in-range num_vtx)])
          (let* ([idxcnt (* i 3)]
                 [current-vtx (ptr-ref vtx-set _s-vertex i)])
            (ptr-set! vn-set _gl-float idxcnt (s-vertex-nx current-vtx))
            (ptr-set! vn-set _gl-float (+ 1 idxcnt) (s-vertex-ny current-vtx))
            (ptr-set! vn-set _gl-float (+ 2 idxcnt) (s-vertex-nz current-vtx))))
        
        (values vn-set length))))
  
  
  ;; vertex-set, num_vtx known
  (define vertices->color-cpointer
    (lambda (num_row num_col vtx-set)
      (let* ([length (* 3 num_vtx)]
             [vc-set (malloc _gl-float length)])
        (for ([i (in-range num_vtx)])
          (let* ([idxcnt (* i 3)]
                 [current-vtx (ptr-ref vtx-set _s-vertex i)])
            (ptr-set! vc-set _gl-float idxcnt (s-vertex-r current-vtx))
            (ptr-set! vc-set _gl-float (+ 1 idxcnt) (s-vertex-g current-vtx))
            (ptr-set! vc-set _gl-float (+ 2 idxcnt) (s-vertex-b current-vtx))))
        
        (values vc-set length))))
  
  (define (copy-ptr-list num _type ptr)
    (let* ([new-list (malloc _type num)])
      (for ([i (in-range num)])
        (let ([elt (ptr-ref ptr _type i)])
          (ptr-set! new-list _type i elt)
          ))
      new-list))
  
  (define (print-ptr-list num _type ptr-list)
    (for ([i (in-range num)])
      (printf "\t ~s" (ptr-ref ptr-list _type i)))
    (printf "\n"))

  (define (push-back lst val)
    (append lst (list val)))
  
  (define (define-torus-indices num_row num_col)
    (let* ([idx-list (build-list 0 (lambda (x) 0))])
      
      (for ([r0 (in-range (- num_row 1))])
        (let ([r1 (+ r0 1)])
          (for ([c0 (in-range (- num_col 1))])
            (let ([c1 (+ c0 1)])
              (set! idx-list (push-back idx-list (+ (* r0 num_col) c1)))
              (set! idx-list (push-back idx-list (+ (* r0 num_col) c0)))

              (set! idx-list (push-back idx-list (+ (* r1 num_col) c0)))
              
              (set! idx-list (push-back idx-list (+ (* r1 num_col) c0)))
              (set! idx-list (push-back idx-list (+ (* r1 num_col) c1)))

              (set! idx-list (push-back idx-list (+ (* r0 num_col) c1)))
              )
            )

          (set! idx-list (push-back idx-list (+ (* r0 num_col) (- num_col 1))))
          (set! idx-list (push-back idx-list (+ (* r1 num_col) (- num_col 1))))

          (set! idx-list (push-back idx-list (* r0 num_col)))
          

          (set! idx-list (push-back idx-list (* r1 num_col) ))
          (set! idx-list (push-back idx-list (* r0 num_col) ))

          (set! idx-list (push-back idx-list (+ (* r1 num_col)
                                                (- num_col 1))))
          )
      )

      (let* ([r0 (- num_row 1)][r1 0])
        (for ([c0 (in-range (- num_col 1))])
          (let ([c1 (+ c0 1)])
            (set! idx-list (push-back idx-list (+ (* r0 num_col) c0)))
            (set! idx-list (push-back idx-list (+ (* r1 num_col) c0)))
            (set! idx-list (push-back idx-list (+ (* r0 num_col) c1)))

            
            (set! idx-list (push-back idx-list (+ (* r1 num_col) c1)))
            
            (set! idx-list (push-back idx-list (+ (* r0 num_col) c1)))
            (set! idx-list (push-back idx-list (+ (* r1 num_col) c0)))
            )
          )
        (set! idx-list (push-back idx-list (+ (* r0 num_col) (- num_col 1))))
        (set! idx-list (push-back idx-list (+ (* r1 num_col) (- num_col 1))))

        (set! idx-list (push-back idx-list (* r0 num_col)))
        
        (set! idx-list (push-back idx-list (* r1 num_col) ))
        (set! idx-list (push-back idx-list (* r0 num_col) ))

        (set! idx-list (push-back idx-list (+ (* r1 num_col) 
                                              (- num_col 1))))
        )
      
      (list->cvector idx-list _uint)))

  (define (cvector->pointer cvc)
    (let* ([num (cvector-length cvc)]
           [_type (cvector-type cvc)]
           [ptr (malloc _type num)])
      (for ([i (in-range num)])
        (ptr-set! ptr _type i (cvector-ref cvc i)))
      (values ptr _type  num)))

  (define-values (index-set _idx-type num-idx)
    (let ([cv (define-torus-indices num_row num_col)])
      (cvector->pointer cv)))
  
  
  ;; window frustum
  (define aspect 1)
  (define size 1)
  (define near 3)
  (define far 10000)
  
  ;; projection
  (define is_perspective #t)
  
  ;; lookat
  (define eye_x 0)
  (define eye_y 0)
  (define eye_z 10)
  
  
  (define vertex-set (define-torus-vertices Rds rds num_row num_col))
  (define-values (vertex-geom-set num-vtx) 
    (vertices->geom-cpointer num_row num_col vertex-set))
  (define-values (vertex-norm-set num-nml) 
    (vertices->norm-cpointer num_row num_col vertex-set))
  (define-values (vertex-color-set num-clr) 
    (vertices->color-cpointer num_row num_col vertex-set))
  
  
  ;;class gears-canvas
  (define gears-canvas%
    (class* canvas% ()
      
      (inherit refresh with-gl-context swap-gl-buffers get-parent)
      (super-instantiate () (style '(gl no-autoclear)))
      
      (define rotation 0.0)
      
      (define view-rotx 0.0)
      (define view-roty 0.0)
      (define view-rotz 0.0)
      
      (define step? #f)
      
      (define/public (run)
        (set! step? #t)
        (refresh))
      
      (define/public (move-left rdn)
        (set! view-roty (+ view-roty rdn))
        (refresh))
      
      (define/public (move-right rdn)
        (set! view-roty (- view-roty rdn))
        (refresh))
      
      (define/public (move-up rdn)
        (set! view-rotx (+ view-rotx rdn))
        (refresh))
      
      (define/public (move-down rdn)
        (set! view-rotx (- view-rotx rdn))
        (refresh))
      
      (define/override (on-size width height)
        (with-gl-context
         (lambda ()
           (unless vbo-init
             
             (set! v-vbo-id (glGenBuffers 4))
             
             ; ffi sgl-1.5
             (glBindBuffer GL_ARRAY_BUFFER (cvector-ref v-vbo-id 0))
             (glBufferData GL_ARRAY_BUFFER
                           (* num-vtx (ctype-sizeof _gl-float))
                           vertex-geom-set
                           GL_STATIC_DRAW)
           
             (glBindBuffer GL_ARRAY_BUFFER (cvector-ref v-vbo-id 1))
             (glBufferData GL_ARRAY_BUFFER
                           (* num-nml (ctype-sizeof _gl-float))
                           vertex-norm-set
                           GL_STATIC_DRAW)
             
             (glBindBuffer GL_ARRAY_BUFFER (cvector-ref v-vbo-id 2))
             (glBufferData GL_ARRAY_BUFFER
                           (* num-clr (ctype-sizeof _gl-float))
                           vertex-color-set
                           GL_STATIC_DRAW)
             
             (glBindBuffer GL_ELEMENT_ARRAY_BUFFER (cvector-ref v-vbo-id 3))
             (glBufferData GL_ELEMENT_ARRAY_BUFFER 
                           (* num-idx (ctype-sizeof _gl-uint))
                           index-set
                           GL_STATIC_DRAW)
             
             (set! vbo-init #t)
             )
           
;           (gl-matrix-mode 'projection)
;           (gl-load-identity)
;           (let ((h (/ height width)))
;             (gl-frustum -1.0 1.0 (- h) h 5.0 60.0)
;             )
           (gl-viewport 0 0 width height)
           
           ))
        (refresh))
      

      
    ; frustum constuction
    (define on-paint-frustum
      (lambda ()
        (let* (
               (left (* -1 size))
               (right size)         
               (top size)
               (bottom (* -1 size))
               (ortho-size 5))
          
          (if (> aspect 1) 
              (let* ((l (* left aspect)) 
                     (r (* right aspect))
                     )
                (if is_perspective
                    (gl-frustum l r bottom top near far)
                    (gl-ortho (* left ortho-size) (* right ortho-size) 
                              (* bottom ortho-size) (* top ortho-size) near far)))
              (let* ((t (/ top aspect)) (b (/ bottom aspect)))
                (if is_perspective
                    (gl-frustum left right b t near far)
                    (gl-ortho (* left ortho-size) (* right ortho-size) 
                              (* top ortho-size) (* bottom ortho-size) near far)))))))
            
      
      (define/override (on-paint)
        (with-gl-context
         (lambda ()
           
           (gl-clear-color 0.0 0.0 0.0 0.0)
           (gl-clear 'color-buffer-bit 'depth-buffer-bit)
           (glPolygonMode GL_FRONT_AND_BACK GL_FILL)
           (glPointSize 5.0)           
           (gl-shade-model 'smooth)
           (gl-enable 'depth-test)
           (gl-enable 'lighting)

           (gl-color-material 'front-and-back 'ambient-and-diffuse)
           (gl-enable 'color-material)

           

           ;; projection
           (gl-matrix-mode 'projection)
           (gl-load-identity)
           
           (on-paint-frustum)
           
           
           ;; model-view
           (gl-matrix-mode 'modelview)
           (gl-load-identity)

           ;; eye, light
           (let* ((look_x 0) (look_y 0) (look_z 0)
                             (up_x 0) (up_y 1) (up_z 0))
           
           ;; gluLookAt()
           (gl-look-at eye_x eye_y eye_z 
                       look_x look_y look_z 
                       up_x up_y up_z))
           
           (gl-enable 'cull-face)
           (gl-enable 'lighting)
           (gl-enable 'depth-test)

           (gl-light-v 'light0 'position (vector->gl-float-vector
                                          (vector eye_x eye_y (* 5 eye_z) 0.0)))
           (gl-light-v 'light1 'position (vector->gl-float-vector
                                          (vector eye_x eye_y (* -5 eye_z) 0.0)))
           (gl-light-v 'light2 'position (vector->gl-float-vector
                                          (vector eye_x (* 5 eye_y) eye_z 0.0)))
           (gl-light-v 'light3 'position (vector->gl-float-vector
                                          (vector eye_x (* -5 eye_y) eye_z 0.0)))
           (gl-light-v 'light4 'position (vector->gl-float-vector
                                          (vector (* 5 eye_x) eye_y eye_z 0.0)))
           (gl-light-v 'light5 'position (vector->gl-float-vector
                                          (vector (* -5 eye_x) eye_y eye_z 0.0)))
           (gl-enable 'light0)
           (gl-enable 'light1)
           (gl-enable 'light2)
           (gl-enable 'light3)
           (gl-enable 'light4)
           (gl-enable 'light5)
           
           (gl-push-matrix)
           
           (gl-push-matrix)
           (gl-rotate view-rotx 1.0 0.0 0.0)
           (gl-rotate view-roty 0.0 1.0 0.0)
           (gl-rotate view-rotz 0.0 0.0 1.0)
           
           (glEnableClientState GL_VERTEX_ARRAY)
           (glEnableClientState GL_NORMAL_ARRAY)
           (glEnableClientState GL_COLOR_ARRAY)
           (glEnableClientState GL_INDEX_ARRAY)

           (glBindBuffer GL_ARRAY_BUFFER (cvector-ref v-vbo-id 1))
           (glNormalPointer GL_FLOAT 0 0)

           (glBindBuffer GL_ARRAY_BUFFER (cvector-ref v-vbo-id 2))
           (glColorPointer 3 GL_FLOAT 0 0)

           (glBindBuffer GL_ARRAY_BUFFER (cvector-ref v-vbo-id 0))
           (glVertexPointer 3 GL_FLOAT 0 0)           

           (glBindBuffer GL_ELEMENT_ARRAY_BUFFER (cvector-ref v-vbo-id 3))
           (glIndexPointer GL_UNSIGNED_INT 0 0)
           
           (if (not draw-in-line)
                 (glDrawElements GL_TRIANGLES
                                 num-idx
                                 GL_UNSIGNED_INT
                                 0)
                 
                 (glDrawArrays GL_POINTS
                                 0
                                 num_vtx)
                 )

           (glDisableClientState GL_INDEX_ARRAY)
           (glDisableClientState GL_COLOR_ARRAY)
           (glDisableClientState GL_NORMAL_ARRAY)
           (glDisableClientState GL_VERTEX_ARRAY)
           
           (glBindBuffer GL_ARRAY_BUFFER 0);
           (glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0);
           
           (gl-pop-matrix)
           (gl-pop-matrix)
           
           (swap-gl-buffers)
           (gl-flush)))
        )
      ))
  
  (define (run)
    (let* (
           [fm (make-object frame% "gears-bufferobject" #f)]
           [cvs (instantiate gears-canvas%
                  (fm) (min-width 300) (min-height 300))]
           )
      (when controls?
        (let ([h (instantiate horizontal-panel% (fm)
                   (alignment '(center center)) (stretchable-height #f))])
          (let ([h (instantiate horizontal-panel% (h)
                     (alignment '(center center)))])
            (instantiate button% ("Left" h
                                         (lambda x (send cvs move-left 10)))
              (stretchable-width #t))
            (let ([v (instantiate vertical-panel% (h)
                       (alignment '(center center)) (stretchable-width #f))])
              (instantiate button% ("Up" v
                                         (lambda x (send cvs move-up 10)))
                (stretchable-width #t))
              (instantiate button% ("Down" v
                                           (lambda x (send cvs move-down 10)))
                (stretchable-width #t)))
            (instantiate button% ("Right" h
                                          (lambda x (send cvs move-right 10)))
              (stretchable-width #t))
            )))
      (send fm show #t)
      
      cvs)
    )
  
  (define cvs (run))
  
  (thread
   (lambda ()
     (let loop ()
       (sync (system-idle-evt))
       ;(queue-callback (lambda x (send cvs move-left .5)))
       (loop))))
)
