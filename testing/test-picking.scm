

(module test-picking mzscheme
(require (lib "mred.ss" "mred")
         (lib "list.ss")
         (lib "class.ss")
         (lib "math.ss")
         sgl
         sgl/gl
         sgl/gl-vectors)
         
         
(define controls? #t)

(define gears-canvas%
  (class* canvas% ()

    (inherit refresh with-gl-context swap-gl-buffers get-parent)

    (define rotation 0.0)

    (define view-rotx 0.0)
    (define view-roty 0.0)
    (define view-rotz 0.0)

    (define gear1 #f)

    (define step? #f)

    (define/public (run)
      (set! step? #t)
      (refresh))

    (define/public (move-left)
      (set! view-roty (+ view-roty 5.0))
      (refresh))

    (define/public (move-right)
      (set! view-roty (- view-roty 5.0))
      (refresh))

    (define/public (move-up)
      (set! view-rotx (+ view-rotx 5.0))
      (refresh))

    (define/public (move-down)
      (set! view-rotx (- view-rotx 5.0))
      (refresh))

    (define (build-gear inner-radius    ; radius of hole at center
                        outer-radius    ; radius at center of teeth
                        width           ; width of gear
                        teeth           ; number of teeth
                        tooth-depth)    ; depth of tooth
      (let* ((r0             inner-radius)
             (r1             (- outer-radius (/ tooth-depth 2.0)))
             (r2             (+ outer-radius (/ tooth-depth 2.0)))
             (da             (/ (* 2.0 pi) teeth 4.0))
             (da2            (* da 2))
             (da3            (* da 3))
             (half-width     (* width 0.5))
             (neg-half-width (- half-width)))

        ;; TODO: Generalize away some more redundant program text.
        (gl-shade-model 'flat)
        (gl-normal 0.0 0.0 1.0)
	;; Draw front face.
        (gl-begin 'quad-strip)
          (do ((i 0 (+ 1 i))) ((> i teeth))
            (let* ((angle     (/ (* i 2.0 pi) teeth))
                   (cos-angle (cos angle))
                   (sin-angle (sin angle)))
              (gl-vertex (* r0 cos-angle) (* r0 sin-angle) half-width)
              (gl-vertex (* r1 cos-angle) (* r1 sin-angle) half-width)
              (when (< i teeth)
                (gl-vertex (* r0 cos-angle)
                           (* r0 sin-angle)
                           (* half-width))
                (gl-vertex (* r1 (cos (+ angle da3)))
                           (* r1 (sin (+ angle da3)))
                           half-width))))
        (gl-end)))

    (define frame-aspect 0)
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda () 
         (set! frame-aspect (/ height width))
         (gl-viewport 0 0 width height)
      (refresh))))

    (define sec (current-seconds))
    (define frames 0)
    
    (define/override (on-paint)

	(with-gl-context
	 (lambda ()

           (if pick-mode-on
               (pick-an-object mouse-px mouse-py 0.5 0.5 0 0))
               
	   (gl-clear-color 0.0 0.0 0.0 0.0)
	   (gl-clear 'color-buffer-bit 'depth-buffer-bit)

           (gl-matrix-mode 'projection)
           (gl-load-identity)
           ;; frustum part
           (gl-frustum -1.0 1.0 
                       (* -1 frame-aspect) frame-aspect 
                       5.0 60.0)
           (gl-matrix-mode 'modelview)
           (gl-load-identity)


           (gl-light-v 'light0 'position (vector->gl-float-vector
                                          (vector 5.0 5.0 10.0 0.0)))
           (gl-enable 'cull-face)
           (gl-enable 'lighting)
           (gl-enable 'light0)
           (gl-enable 'depth-test)

           ; model 1
           (gl-material-v 'front
                          'ambient-and-diffuse
                          (vector->gl-float-vector (vector 0.8 0.4 0.0 1.0)))
             (gl-push-matrix)
             (gl-translate -3.0 0.0 -40.0)
             (build-gear 1.0 4.0 1.0 20 0.7)
             (gl-pop-matrix)

           ; model 2
           (gl-material-v 'front
                          'ambient-and-diffuse
                          (vector->gl-float-vector (vector 0.0 0.4 0.8 1.0)))
             (gl-push-matrix)
             (gl-translate 3.0 0.0 -35.0)
             (build-gear 1.0 4.0 1.0 20 0.7)
             (gl-pop-matrix)
           
           ; model 3
           (gl-material-v 'front
                          'ambient-and-diffuse
                          (vector->gl-float-vector (vector 0.8 0.0 0.6 1.0)))
             (gl-push-matrix)
             (gl-translate 0.0 3.0 -30.0)
             (build-gear 1.0 4.0 1.0 20 0.7)
             (gl-pop-matrix)

           ; model 4
           (gl-material-v 'front
                          'ambient-and-diffuse
                          (vector->gl-float-vector (vector 0.0 0.9 0.3 1.0)))
             (gl-push-matrix)
             (gl-translate 0.0 -3.0 -25.0)
             (build-gear 1.0 4.0 1.0 20 0.7)
             (gl-pop-matrix)
           
           
	   (swap-gl-buffers)
	   (gl-flush)
           )))
;	(when step?
;	  (set! step? #f)
;	  (queue-callback (lambda x (send this run))))))
    
    
    (define mouse-px 0) 
    (define mouse-py 0)
    (define pick-mode-on #f)
    
    ;; Action on each mouse events
    (define/override (on-event e)
      (let* ((m-down (send e button-down?))
             (m-up (send e button-up?))
             (m-drag (send e dragging?)))
        
        (when m-down
          (set! mouse-px (send e get-x))
          (set! mouse-py (send e get-y))
          ;(printf "--------dx: ~s, dy: ~s\n" mouse-px mouse-py)
          )
        (when m-up
          (let* ((cx (send e get-x))
                 (cy (send e get-y)))
            (if (is-mouse-pick mouse-px mouse-py cx cy)
                (begin 
                  (set! pick-mode-on #t)
                  (refresh)))))))
                

    (define is-mouse-pick 
      (lambda (ox oy cx cy); original position (ox oy), current position (cx, cy)
        (let ((delta 5))
          (if (and (<= (- ox delta) cx) (<= cx (+ ox delta)))
              (if (and (<= (- oy delta) cy) (<= cy (+ oy delta)))
                  #t 
                  #f)
              #f))))

    (define process-hits
      (lambda ()
        (display '_picking_starts)
        (printf "~s ~s\n" mouse-px mouse-py)))
    
    (define pick-an-object
      (lambda (px py w h offx offy)
        (let* ((vp (glGetIntegerv GL_VIEWPORT 4))
               (prj (glGetDoublev GL_PROJECTION_MATRIX 16))
               (mdl (glGetDoublev GL_MODELVIEW_MATRIX 16))
               ;;report ogl the selection buffer
               (select-buffer (glSelectBuffer 1024)))
          ;; selection
          (gl-render-mode 'select)
          (gl-init-names)

          
          ;; projection - update projection matrix
          (gl-matrix-mode 'projection)
          (gl-load-identity)
          (gl-pick-matrix (+ px offx)
                          (- (vector-ref (gl-vector->vector vp) 3) 
                             (+ py offy))
                          w 
                          h 
                          vp)

          ;; make-frustum
          (gl-frustum -1.0 1.0 
                      (* -1 frame-aspect) frame-aspect 
                      5.0 60.0)

          (gl-matrix-mode 'modelview)
          (gl-load-identity)

          ;; name 1

          (gl-push-matrix)
          (gl-translate -3.0 0.0 -40.0)
          (gl-push-name 87)
          (build-gear 1.0 4.0 1.0 20 0.7)
          (gl-pop-name)
          (gl-pop-matrix)          

          
          ;; name 2

          (gl-push-matrix)
          (gl-translate 3.0 0.0 -35.0)
          (gl-push-name 65)
          (build-gear 1.0 4.0 1.0 20 0.7)
          (gl-pop-name)
          (gl-pop-matrix)          

          
          ;; name 3

          (gl-push-matrix)
          (gl-translate 0.0 3.0 -30.0)
          (gl-push-name 43)
          (build-gear 1.0 4.0 1.0 20 0.7)
          (gl-pop-name)
          (gl-pop-matrix)


          ;; name 4

          (gl-push-matrix)
          (gl-translate 0.0 -3.0 -25.0)
          (gl-push-name 21)
          (build-gear 1.0 4.0 1.0 20 0.7)
          (gl-pop-name)
          (gl-pop-matrix)


          (gl-flush)

          (let* ((hits (gl-render-mode 'render))
                 (selected-uint-list (select-buffer->gl-uint-vector select-buffer))
                 (selected-list (my-process-selection 
                                 selected-uint-list
                                 hits))
                 (results (sort selected-list
                                (lambda (a b)
                                  (< (my-selection-record-min-z a)
                                     (my-selection-record-min-z b))))))
            
;            (printf "hits = ~s\n" hits)
;            ;; print selectbuffer
;            (let* ((svec (gl-vector->vector selected-uint-list)))
;              (printf "s-b ~s \t ~s ~s ~s\n ~s ~s ~s ~s\n " (vector-ref svec 0) (vector-ref svec 1) (vector-ref svec 2) (vector-ref svec 3)
;                      (vector-ref svec 4) (vector-ref svec 5) (vector-ref svec 6) (vector-ref svec 7)))
            
            (look-into-selection-buffer results)
            
            #f))))

    (define look-into-selection-buffer
      (lambda (buffer)
        (cond
              ((null? buffer) (begin (display 'nothing_seleected) #f))
              (else 
               (let ((index (car (my-selection-record-stack (car buffer)))))
                 (printf "index = ~s\n" index))))))

    ;; (make-selection-record number number (listof positive-int))
    (define-struct my-selection-record (min-z max-z stack))  

    ;; process-selection : gl-uint-vector int -> (listof selection-record)
    (define (my-process-selection v hits)
      (unless (gl-uint-vector? v)
        (raise-type-error 'process-selection
                          "gl-uint-vector"
                          0 v hits))
      (let ((index 0))
        (let loop ((hit 0))
          (cond
            ((>= hit hits) null)
            (else
             (let ((stack-size (gl-vector-ref v index)))
               (cons (make-my-selection-record 
                      (gl-vector-ref v (add1 index))
                      (gl-vector-ref v (+ index 2))
                      (begin
                        (printf "~s ~s\n" index hits)
                        (set! index (+ 3 index))
                        (let loop ((j 0))
                          (cond
                            ((< j stack-size)
                             (cons (gl-vector-ref v index)
                                   (begin
                                     (set! index (add1 index))
                                     (loop (add1 j)))))
                            (else null)))))
                     (loop (add1 hit)))))))))
    
    
    (display '_picking_starts)
    (printf "~s ~s\n" mouse-px mouse-py)
     
    (super-instantiate () (style '(gl no-autoclear)))))
    
    
  (define (f)
    (let* ((f (make-object frame% "gears.ss" #f))
           (c (instantiate gears-canvas% (f) (min-width 300) (min-height 300))))
      (send f create-status-line)
      (when controls?
        (let ((h (instantiate horizontal-panel% (f)
                   (alignment '(center center)) (stretchable-height #f))))
          (instantiate button%
            ("Start" h (lambda (b e) (send b enable #f) (send c run)))
            (stretchable-width #t) (stretchable-height #t))
          (let ((h (instantiate horizontal-panel% (h)
                     (alignment '(center center)))))
            (instantiate button% ("Left" h (lambda x (send c move-left)))
              (stretchable-width #t))
            (let ((v (instantiate vertical-panel% (h)
                       (alignment '(center center)) (stretchable-width #f))))
              (instantiate button% ("Up" v (lambda x (send c move-up)))
                (stretchable-width #t))
              (instantiate button% ("Down" v (lambda x (send c move-down)))
                (stretchable-width #t)))
            (instantiate button% ("Right" h (lambda x (send c move-right)))
              (stretchable-width #t)))))
      (send f show #t))) 
  (f)
  )
;;eof
