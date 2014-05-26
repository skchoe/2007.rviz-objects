(module cylinder-vao scheme
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
  
  (define vao-init #f)
  
  (define-cstruct _s-vertex ([x _gl-float]
                             [y _gl-float]
                             [z _gl-float]
                             [nx _gl-float]
                             [ny _gl-float]
                             [nz _gl-float]
                             [r _gl-float]
                             [g _gl-float]
                             [b _gl-float]))

  (define num_row 16)
  (define num_col 16)
  (define draw-in-line #f)
  (define num_vtx (+ (* (+ num_row 1) num_col) 2)) ; wall, 2 end centers

  (define (define-vertices radius height num_row num_col)
    (let* ([vertices (malloc _s-vertex num_vtx)]
           [pi 3.1415265359])
      (for ([r0 (in-range (+ num_row 1))])
        (for ([c (in-range num_col)])
          (let* ([r1 (+ r0 1)]
                 
                 [y (if ( < 0 num_row )
                        (* (- (/ r0 num_row) .5) height)
                        0)]
                 [x (* radius (sin (* (/ c num_col) (* pi 2.0) )))]
                 [z (* radius (cos (* (/ c num_col) (* pi 2.0) )))]

                 [ny 0]
                 [nx x]
                 [nz z]

                 [r 0.7]
                 [g (/ r0 num_row)]
                 [b (/ c num_col)]
                 
                 [offset (+ (* r0 num_col) c)]
                 )
            ; now the s-vertex structure
            (ptr-set! vertices
                      _s-vertex
                      offset
                      (make-s-vertex x y z nx ny nz r g b)))))
      
      (let* ([lid0 (* (+ num_row 1) num_col)]
             [lid1 (+ 1 lid0)]
             [height/2 (/ height 2.0)]
             [lvtx0 (make-s-vertex 0.0 (* -1 height/2) 0.0 0.0 -1.0 0.0 1.0 0.7 1.0)]
             [lvtx1 (make-s-vertex 0.0 height/2 0.0 0.0 1.0 0.0 1.0 0.7 1.0)]
             )

        (ptr-set! vertices _s-vertex lid0 lvtx0)
        (ptr-set! vertices _s-vertex lid1 lvtx1))
      
      vertices))

  ;; vertex-set, num_vtx known
  (define vertices->geom-cpointer
    (lambda (num_row num_col vtx-set)
      (let* ([vg-set (malloc _gl-float (* 3 num_vtx))])
        (for ([i (in-range num_vtx)])
          (let* ([idxcnt (* i 3)]
                 [current-vtx (ptr-ref vtx-set _s-vertex i)])
            (ptr-set! vg-set _gl-float idxcnt (s-vertex-x current-vtx))
            (ptr-set! vg-set _gl-float (+ 1 idxcnt) (s-vertex-y current-vtx))
            (ptr-set! vg-set _gl-float (+ 2 idxcnt) (s-vertex-z current-vtx))))
        
        vg-set)))

  (define vertices->norm-cpointer
    (lambda (num_row num_col vtx-set)
      (let* ([vn-set (malloc _gl-float (* 3 num_vtx))])
        (for ([i (in-range num_vtx)])
          (let* ([idxcnt (* i 3)]
                 [current-vtx (ptr-ref vtx-set _s-vertex i)])
            (ptr-set! vn-set _gl-float idxcnt (s-vertex-nx current-vtx))
            (ptr-set! vn-set _gl-float (+ 1 idxcnt) (s-vertex-ny current-vtx))
            (ptr-set! vn-set _gl-float (+ 2 idxcnt) (s-vertex-nz current-vtx))))
        
        vn-set)))
    
  (define vertices->color-cpointer
    (lambda (num_row num_col vtx-set)
      (let* ([vc-set (malloc _gl-float (* 3 num_vtx))])
        (for ([i (in-range num_vtx)])
          (let* ([idxcnt (* i 3)]
                 [current-vtx (ptr-ref vtx-set _s-vertex i)])
            (ptr-set! vc-set _gl-float idxcnt (s-vertex-r current-vtx))
            (ptr-set! vc-set _gl-float (+ 1 idxcnt) (s-vertex-g current-vtx))
            (ptr-set! vc-set _gl-float (+ 2 idxcnt) (s-vertex-b current-vtx))))
        
        vc-set)))
  
  (define vertex-set (define-vertices 1.0 4.0 num_row num_col))
  (define vertex-geom-set (vertices->geom-cpointer num_row num_col vertex-set))
  (define vertex-norm-set (vertices->norm-cpointer num_row num_col vertex-set))
  (define vertex-color-set (vertices->color-cpointer num_row num_col vertex-set))
  
  (define (copy-ptr-list num _type ptr)
    (let* ([new-list (malloc _type num)])
      (for ([i (in-range num)])
        (let ([elt (ptr-ref ptr _type i)])
          (ptr-set! new-list _type i elt)
          (printf "~s(1)\t" elt)))
      (for ([i (in-range num)])
        (let ([elt (ptr-ref new-list _type i)])
          (printf "~s(2)\t" elt)))
      (printf "\n")
      new-list))
  
 
  (define (print-ptr-list num _type ptr-list)
    (for ([i (in-range num)])
      (printf "\t ~s" (ptr-ref ptr-list _type i)))
    (printf "\n"))

  (define (push-back lst val)
    (append lst (list val)))
   
  ; row=0
  ; col=3
  ;      *
  ;    * * * (0)
  ;      *
  ;
  ; row=1
  ; col=3
  ;     *
  ;   * * * ( .5)
  ;   * * * (-.5)
  ;     *
    
  (define (define-indices num_row num_col)
    (let* ([idx-list (build-list 0 (lambda (x) 0))])
      
      (for ([r0 (in-range num_row)])
        (let* ([r1 (+ r0 1)])
          (for ([c (in-range (- num_col 1))])
            (set! idx-list (push-back idx-list (+ (* r0 num_col) c)))
            (set! idx-list (push-back idx-list (+ (* r1 num_col) c)))
            (set! idx-list (push-back idx-list (+ (* r0 num_col) (+ c 1))))
            
            (set! idx-list (push-back idx-list (+ (* r0 num_col) (+ c 1))))
            (set! idx-list (push-back idx-list (+ (* r1 num_col) c)))
            (set! idx-list (push-back idx-list (+ (* r1 num_col) (+ c 1))))
            )

          (set! idx-list (push-back idx-list (+ (* r0 num_col) (- num_col 1))))
          (set! idx-list (push-back idx-list (+ (* r1 num_col) (- num_col 1))))
          (set! idx-list (push-back idx-list (* r0 num_col)))
          
          (set! idx-list (push-back idx-list (* r0 num_col) ))
          (set! idx-list (push-back idx-list (+ (* r1 num_col) (- num_col 1))))
          (set! idx-list (push-back idx-list (* r1 num_col) ))
          )
        )

      ; The triangles to the highest and deepest vertices:
      (for ([c (in-range (- num_col 1))])
        (set! idx-list (push-back idx-list c))
        (set! idx-list (push-back idx-list (+ c 1)))
        (set! idx-list (push-back idx-list (* (+ num_row 1) num_col)))
        )
      (set! idx-list (push-back idx-list 0))
      (set! idx-list (push-back idx-list (* (+ num_row 1) num_col)))
      (set! idx-list (push-back idx-list (- num_col 1)))
      
      (for ([c (in-range (- num_col 1))])
        (set! idx-list (push-back idx-list (+ (* num_row num_col) c 1)))
        (set! idx-list (push-back idx-list (+ (* num_row num_col) c)))
        (set! idx-list (push-back idx-list (+ (* (+ num_row 1) num_col) 1)))
        )

      (set! idx-list (push-back idx-list (+ (* num_row num_col) (- num_col 1))))
      (set! idx-list (push-back idx-list (+ (* (+ num_row 1) num_col) 1)))
      (set! idx-list (push-back idx-list (* num_row num_col)))
      
      (list->cvector idx-list _uint)))

  (define index-set (define-indices num_row num_col))
  
  ;;class gears-canvas
  (define gears-canvas%
    (class* canvas% ()
      
      (inherit refresh with-gl-context swap-gl-buffers get-parent)
      (super-instantiate () (style '(gl no-autoclear)))
      
      (define rotation 0.0)
      
      (define view-rotx 0.0)
      (define view-roty 0.0)
      (define view-rotz 0.0)
      
      (define aspect 1)
      (define size 2)
      (define near .003)
      (define far 1000)
      
      ;; projection
      (define is_perspective #t)
      
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
           (unless vao-init
             
             (glVertexPointer 3 ;3 components per vertex (x,y,z)
                              GL_FLOAT
                              0
                              vertex-geom-set)

             (glNormalPointer GL_FLOAT
                              0
                              vertex-norm-set)

             (glColorPointer 3  ;3 components per vertex (r,g,b)
                             GL_FLOAT
                             0
                             vertex-color-set);  //cvector to the color-set

             (set! vao-init #t)
             )
           
           (set! aspect (/ width height))
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
                 [ortho-size 2]
                 )
            
            (if (> aspect 1) 
                (let* ([l (* left aspect)] 
                       [r (* right aspect)]
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
           (glPointSize 2.0)           
           (gl-shade-model 'smooth)
           (gl-enable 'depth-test)
           (gl-enable 'lighting)
           
           (gl-color-material 'front-and-back 'ambient-and-diffuse)
           (gl-enable 'color-material)
           
           ;; projection matrix setup
           (gl-matrix-mode 'projection)
           (gl-load-identity)

           ;; frustum
           (on-paint-frustum)           
           
           (gl-matrix-mode 'modelview)
           (gl-load-identity)
           
           ;; eye, light
           (let ([eye_x 0] [eye_y 0] [eye_z 3]
                           [look_x 0] [look_y 0] [look_z 0]
                           [up_x 0] [up_y 1] [up_z 0])
           
           ;; gluLookAt()
             (gl-look-at eye_x eye_y eye_z 
                         look_x look_y look_z 
                         up_x up_y up_z))
           
           (gl-light-v 'light0 'position (vector->gl-float-vector
                                          (vector 0.0 0.0 50.0 0.0)))
           (gl-enable 'light0)
           
           (gl-push-matrix)
           (gl-rotate view-rotx 1.0 0.0 0.0)
           (gl-rotate view-roty 0.0 1.0 0.0)
           (gl-rotate view-rotz 0.0 0.0 1.0)
           
           (glEnableClientState GL_VERTEX_ARRAY)
           (glEnableClientState GL_NORMAL_ARRAY)
           (glEnableClientState GL_COLOR_ARRAY)
           (glEnableClientState GL_INDEX_ARRAY)
           
           (if (not draw-in-line)
                 (glDrawElements GL_TRIANGLES
                                 (cvector-length index-set)
                                 GL_UNSIGNED_INT
                                 index-set)
                 (glDrawArrays GL_POINTS
                                 0
                                 num_vtx)
                 )

           (glEnableClientState GL_INDEX_ARRAY)
           (glDisableClientState GL_COLOR_ARRAY)
           (glDisableClientState GL_NORMAL_ARRAY)
           (glDisableClientState GL_VERTEX_ARRAY)
           
           (gl-pop-matrix)
           
           (swap-gl-buffers)
           (gl-flush)))
        )
      ))
  
  (define (run)
    (let* (
           [fm (make-object frame% "gears-bufferobject" #f)]
           [cvs (instantiate gears-canvas%
                  (fm) (min-width 400) (min-height 300))]
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
