(module sphere-vao scheme
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
                             [r _gl-float]
                             [g _gl-float]
                             [b _gl-float]))

  (define num_row 32)
  (define num_col 32)
  (define draw-in-line #f)
  
  (define (define-sphere-vertices num_row num_col)
    (let* ([num_vtx (+ (* (- num_row 2) num_col) 2)]
           [vertices (malloc _s-vertex num_vtx)]
           [pi 3.1415265359])
      (for ([r0 (in-range (- num_row 2))])
        (for ([c (in-range num_col)])
          (let* ([r1 (+ r0 1)]
                 [y (- 1 (* (/ r1 (- num_row 1)) 2.0))]
                 [radius (sin (acos y))]
                 [x (* radius (sin (* (/ c num_col) (* pi 2.0) )))]
                 [z (* radius (cos (* (/ c num_col) (* pi 2.0) )))]
                 [offset (+ (* r0 num_col) c)]

                 [r (/ r0 num_row)]
                 [g 0.7]
                 [b (/ c num_col)]
                 )
            ; now the s-vertex structure
            (ptr-set! vertices
                      _s-vertex
                      offset
                      (make-s-vertex x y z r g b)))))
      
      (let* ([lid0 (* (- num_row 2) num_col)]
             [lid1 (+ 1 lid0)]
             [lvtx0 (make-s-vertex 0.0 1.0 0.0 1.0 0.7 1.0)]
             [lvtx1 (make-s-vertex 0.0 -1.0 0.0 1.0 0.7 1.0)])
        (ptr-set! vertices _s-vertex lid0 lvtx0)
        (ptr-set! vertices _s-vertex lid1 lvtx1))
      
      vertices))

  ;; vertex-set, num_vtx known
  (define vertices->geom-cpointer
    (lambda (num_row num_col vtx-set)
      (let* ([num_vtx (+ (* (- num_row 2) num_col) 2)]
             [vg-set (malloc _gl-float (* 3 num_vtx))])
        (for ([i (in-range num_vtx)])
          (let* ([idxcnt (* i 3)]
                 [current-vtx (ptr-ref vtx-set _s-vertex i)])
            (ptr-set! vg-set _gl-float idxcnt (s-vertex-x current-vtx))
            (ptr-set! vg-set _gl-float (+ 1 idxcnt) (s-vertex-y current-vtx))
            (ptr-set! vg-set _gl-float (+ 2 idxcnt) (s-vertex-z current-vtx))))
        
        vg-set)))
  
  ;; vertex-set, num_vtx known)))
  
  (define vertices->color-cpointer
    (lambda (num_row num_col vtx-set)
      (let* ([num_vtx (+ (* (- num_row 2) num_col) 2)]
             [vc-set (malloc _gl-float (* 3 num_vtx))])
        (for ([i (in-range num_vtx)])
          (let* ([idxcnt (* i 3)]
                 [current-vtx (ptr-ref vtx-set _s-vertex i)])
            (ptr-set! vc-set _gl-float idxcnt (s-vertex-r current-vtx))
            (ptr-set! vc-set _gl-float (+ 1 idxcnt) (s-vertex-g current-vtx))
            (ptr-set! vc-set _gl-float (+ 2 idxcnt) (s-vertex-b current-vtx))))
        
        vc-set)))
  
  (define vertex-set (define-sphere-vertices num_row num_col))
  (define vertex-geom-set (vertices->geom-cpointer num_row num_col vertex-set))
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
  
  (define (define-sphere-indices num_row num_col)
    (let* ([idx-list (build-list 0 (lambda (x) 0))])
      
      (for ([r0 (in-range (- num_row 3))])
        (let* ([r1 (+ r0 1)])
          (for ([c (in-range (- num_col 1))])
            (set! idx-list (push-back idx-list (+ (* r0 num_col) c)))
            (set! idx-list (push-back idx-list (+ (* r0 num_col) (+ c 1))))
            (set! idx-list (push-back idx-list (+ (* r1 num_col) c)))

            (set! idx-list (push-back idx-list (+ (* r0 num_col) (+ c 1))))
            (set! idx-list (push-back idx-list (+ (* r1 num_col) (+ c 1))))
            (set! idx-list (push-back idx-list (+ (* r1 num_col) c)))
            )

          (set! idx-list (push-back idx-list (+ (* r0 num_col) (- num_col 1))))
          (set! idx-list (push-back idx-list (* r0 num_col)))
          (set! idx-list (push-back idx-list (+ (* r1 num_col) (- num_col 1))))

          (set! idx-list (push-back idx-list (* r0 num_col) ))
          (set! idx-list (push-back idx-list (* r1 num_col) ))
          (set! idx-list (push-back idx-list (+ (* r1 num_col) 
                                                (- num_col 1)))))
      )

      ; The triangles to the highest and deepest vertices:
      (for ([c (in-range (- num_col 1))])
        (set! idx-list (push-back idx-list (+ c 1)))
        (set! idx-list (push-back idx-list c))
        (set! idx-list (push-back idx-list (* (- num_row 2) num_col)))
        )
      (set! idx-list (push-back idx-list 0))
      (set! idx-list (push-back idx-list (- num_col 1)))
      (set! idx-list (push-back idx-list (* (- num_row 2) num_col)))

      (for ([c (in-range (- num_col 1))])
        (set! idx-list (push-back idx-list (+ (* (- num_row 3) num_col) c)))
        (set! idx-list (push-back idx-list (+ (* (- num_row 3) num_col) c 1)))
        (set! idx-list (push-back idx-list (+ (* (- num_row 2) num_col) 1)))
        )

      (set! idx-list (push-back idx-list (+ (* (- num_row 3) num_col)
                                            (- num_col 1))))
      (set! idx-list (push-back idx-list (* (- num_row 3) num_col)))
      (set! idx-list (push-back idx-list (+ (* (- num_row 2) num_col) 1)))

      (list->cvector idx-list _uint)))

  (define index-set (define-sphere-indices num_row num_col))
  
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
           (unless vao-init
             (glVertexPointer 3 ;3 components per vertex (x,y,z)
                              GL_FLOAT
                              0
                              vertex-geom-set)
             (glNormalPointer GL_FLOAT
                              0
                              vertex-geom-set)

             (glColorPointer 3  ;3 components per vertex (r,g,b)
                             GL_FLOAT
                             0
                             vertex-color-set);  //cvector to the color-set

             (set! vao-init #t)
             )
           
           (gl-matrix-mode 'projection)
           (gl-load-identity)
           (let ((h (/ height width)))
             (gl-frustum -1.0 1.0 (- h) h 5.0 60.0)
             )
           (gl-viewport 0 0 width height)
           
           ))
        (refresh))
      
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
           
           (gl-matrix-mode 'modelview)
           (gl-load-identity)

           (gl-light-v 'light0 'position (vector->gl-float-vector
                                          (vector 0.0 0.0 -50.0 0.0)))
           (gl-enable 'cull-face)
           (gl-enable 'lighting)
           (gl-enable 'light0)
           (gl-enable 'depth-test)

           (gl-push-matrix)

           (gl-translate 0.0 0.0 -5.0)

           (gl-push-matrix)
           (gl-rotate view-rotx 1.0 0.0 0.0)
           (gl-rotate view-roty 0.0 1.0 0.0)
           (gl-rotate view-rotz 0.0 0.0 1.0)

           (glEnableClientState GL_VERTEX_ARRAY)
           (glEnableClientState GL_NORMAL_ARRAY)
           (glEnableClientState GL_COLOR_ARRAY)
           
           (if (not draw-in-line)
                 (glDrawElements GL_TRIANGLES
                                 (cvector-length index-set)
                                 GL_UNSIGNED_INT
                                 index-set)
                 (let ([num_vtx (+ (* (- num_row 2) num_col) 2)])
                   (glDrawArrays GL_POINTS
                                 0
                                 num_vtx))
                 )
           
           (glDisableClientState GL_COLOR_ARRAY)
           (glDisableClientState GL_NORMAL_ARRAY)
           (glDisableClientState GL_VERTEX_ARRAY)
           
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
