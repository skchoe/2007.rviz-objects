
(module test-mouse-drag mzscheme
(require mred
         mzlib/class
         mzlib/math
         sgl
         sgl/gl
         sgl/gl-vectors
         (lib "foreign.ss")
         (lib "list.ss"))


(define gears-canvas%
  (class* canvas% ()

    (inherit refresh with-gl-context swap-gl-buffers get-parent)

    (define aspect 1)
    (define size 1)
    (define near 3)
    (define far 10000)
    
    (define rotation 0.0)

    (define view-rotx 20.0)
    (define view-roty 30.0)
    (define view-rotz 0.0)

    (define gear1 #f)
    (define gear2 #f)
    (define gear3 #f)
    
    ;; translation-center:point3, mov-x mov-y -> vector (16)
    (define translation->transform
      (lambda (dx dy dz)
        (vector 1 0 0 0
                0 1 0 0
                0 0 1 0
                dx dy dz 1)))
    (define xform-global (translation->transform 0.0 0.0 0.0))
    (define xform-group1 (translation->transform 0.0 0.0 0.0))
    (define xform-group23 (translation->transform 1.0 1.0 0.0))

    (define init-pick-id-table (lambda () (make-hash-table 'equal)))    
    ;; pick-id-table (used in wide veriety of place to manage unique pick-id)
    (define pick-id-table (init-pick-id-table))

    ;; pick-id-table -> pick-id: randomly/uniquly generated
    ;; update pick-id-table
    (define generate-pick-id
      (lambda (pi-table) ; pick-id-table
        (let loop ()
          (let ((pick-id (generate-random-number 10000000)))
            (if (hash-table-get pi-table pick-id #f)
                (loop)
                (begin
                  ;(printf "PICK-ID: ~s is assigned\n" pick-id)
                  pick-id))))))
    
    (define register-pick-id
      (lambda (pi-table pick-id s-group)
        (hash-table-put! pi-table pick-id s-group)))
    
    (define generate-random-number
      (lambda(k) (random k)))
    
    (define gear1-name (generate-pick-id pick-id-table))
    (register-pick-id pick-id-table gear1-name xform-group1)
    (define gear23-name (generate-pick-id pick-id-table))
    (register-pick-id pick-id-table gear23-name xform-group23)

    (define mouse-px 0) 
    (define mouse-py 0)
    (define mouse-cx 0) 
    (define mouse-cy 0)
    
    (define pick-mode-on #f)
    (define selected-scene-node-list empty) ; list form (car, cdr, cons)
    
    (define (build-gear pick-id         ; picking id
                        inner-radius    ; radius of hole at center
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

        (unless (null? pick-id) (gl-push-name pick-id))
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
        (gl-end)

        ;; Draw front sides of teeth.
        (gl-begin 'quads)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let ((angle (/ (* i 2.0 pi) teeth)))
            (gl-vertex (* r1 (cos angle))
                       (* r1 (sin angle))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       half-width)
            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       half-width)))
        (gl-end)

        (gl-normal 0.0 0.0 -1.0)

        ;; Draw back face.
        (gl-begin 'quad-strip)
        (do ((i 0 (+ 1 i))) ((> i teeth))
          (let* ((angle     (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))
            (gl-vertex (* r1 cos-angle) (* r1 sin-angle) neg-half-width)
            (gl-vertex (* r0 cos-angle) (* r0 sin-angle) neg-half-width)
            (when (< i teeth)
              (gl-vertex (* r1 (cos (+ angle da3)))
                         (* r1 (sin (+ angle da3)))
                         neg-half-width)
              (gl-vertex (* r0 cos-angle)
                         (* r0 sin-angle)
                         neg-half-width))))
        (gl-end)

        ;; Draw back sides of teeth.
        (gl-begin 'quads)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let ((angle (/ (* i 2.0 pi) teeth)))
            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       neg-half-width)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       neg-half-width)
            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       neg-half-width)
            (gl-vertex (* r1 (cos angle))
                       (* r1 (sin angle))
                       neg-half-width)))
        (gl-end)

        ;; Draw outward faces of teeth.
        (gl-begin 'quad-strip)
        (do ((i 0 (+ 1 i))) ((= i teeth))
          (let* ((angle     (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))

            (gl-vertex (* r1 cos-angle) (* r1 sin-angle) half-width)
            (gl-vertex (* r1 cos-angle) (* r1 sin-angle) neg-half-width)

            (let* ((u   (- (* r2 (cos (+ angle da))) (* r1 cos-angle)))
                   (v   (- (* r2 (sin (+ angle da))) (* r1 sin-angle)))
                   (len (sqrt (+ (* u u) (* v v)))))
              (gl-normal (/ v len) (- (/ u len)) 0.0))

            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da)))
                       (* r2 (sin (+ angle da)))
                       neg-half-width)
            (gl-normal cos-angle sin-angle 0.0)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       half-width)
            (gl-vertex (* r2 (cos (+ angle da2)))
                       (* r2 (sin (+ angle da2)))
                       neg-half-width)

            (let ((u (- (* r1 (cos (+ angle da3)))
                        (* r2 (cos (+ angle da2)))))
                  (v (- (* r1 (sin (+ angle da3)))
                        (* r2 (sin (+ angle da2))))))
              (gl-normal v (- u) 0.0))

            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       half-width)
            (gl-vertex (* r1 (cos (+ angle da3)))
                       (* r1 (sin (+ angle da3)))
                       neg-half-width)
            (gl-normal cos-angle sin-angle 0.0)))

        (gl-vertex (* r1 (cos 0)) (* r1 (sin 0)) half-width)
        (gl-vertex (* r1 (cos 0)) (* r1 (sin 0)) neg-half-width)
        (gl-end)

        (gl-shade-model 'smooth)

        ;; Draw inside radius cylinder.
        (gl-begin 'quad-strip)
        (do ((i 0 (+ 1 i))) ((> i teeth))
          (let* ((angle     (/ (* i 2.0 pi) teeth))
                 (cos-angle (cos angle))
                 (sin-angle (sin angle)))
            (gl-normal (- cos-angle) (- sin-angle) 0.0)
            (gl-vertex (* r0 cos-angle) (* r0 sin-angle) neg-half-width)
            (gl-vertex (* r0 cos-angle) (* r0 sin-angle) half-width)))
        (gl-end)
        
        (unless (null? pick-id) (gl-pop-name))
        ))
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (set! aspect (/ height width))
         (gl-viewport 0 0 width height)))
       (refresh))
    
    (define default-transform 
      (lambda ()
        (vector 1.0 0.0 0.0 0.0
                0.0 1.0 0.0 0.0
                0.0 0.0 1.0 0.0
                0.0 0.0 0.0 1.0)))
    
    (define-struct point3 (x y z))
    
    (define (xrotation->transform x-angle)
      (let* ((cosx (cos x-angle))
             (sinx (sin x-angle)))
        
        (vector 1.0 0.0 0.0 0.0
                0.0 cosx (* -1 sinx) 0.0
                0.0 sinx cosx 0.0
                0.0 0.0 0.0 1.0)))
    
    (define (yrotation->transform y-angle)
      (let* ((cosy (cos y-angle))
             (siny (sin y-angle)))
        
        (vector cosy 0.0 siny 0.0
                0.0 1.0  0.0 0.0
                (* -1 siny) 0.0 cosy 0.0
                0.0 0.0 0.0 1.0)))
    
    (define (zrotation->transform z-angle)
      (let* ((cosz (cos z-angle))
             (sinz (sin z-angle)))
        
        (vector cosz (* -1 sinz) 0.0 0.0
                sinz cosz 0.0 0.0
                0.0 0.0 1.0 0.0
                0.0 0.0 0.0 1.0)))
    
    (define mtx-mult-3-3
      (lambda (mat1 mat2)
        (let* ((mat1-row-0 (vector (vector-ref mat1 0)
                                   (vector-ref mat1 4)
                                   (vector-ref mat1 8)))
               (mat1-row-1 (vector (vector-ref mat1 1)
                                   (vector-ref mat1 5)
                                   (vector-ref mat1 9)))
               (mat1-row-2 (vector (vector-ref mat1 2)
                                   (vector-ref mat1 6)
                                   (vector-ref mat1 10)))
               (mat2-col-0 (vector (vector-ref mat2 0)
                                   (vector-ref mat2 1)
                                   (vector-ref mat2 2)))
               (mat2-col-1 (vector (vector-ref mat2 4)
                                   (vector-ref mat2 5)
                                   (vector-ref mat2 6)))
               (mat2-col-2 (vector (vector-ref mat2 8)
                                   (vector-ref mat2 9)
                                   (vector-ref mat2 10))))
          
          ; 0 4 8  0
          ; 1 5 9  0
          ; 2 6 10 0
          ; 0 0 0  1
          (vector (dot-product-3 mat1-row-0 mat2-col-0)
                  (dot-product-3 mat1-row-1 mat2-col-0)
                  (dot-product-3 mat1-row-2 mat2-col-0)
                  0
                  (dot-product-3 mat1-row-0 mat2-col-1)
                  (dot-product-3 mat1-row-1 mat2-col-1)
                  (dot-product-3 mat1-row-2 mat2-col-1)
                  0
                  (dot-product-3 mat1-row-0 mat2-col-2)
                  (dot-product-3 mat1-row-1 mat2-col-2)
                  (dot-product-3 mat1-row-2 mat2-col-2)
                  0 
                  0 0 0 1))))
    
  ;; (mult A(4x4) B(4x4)) -> 4x4
  ;; w = v * A * B (A is old, B is new transform):v [x y z w] |-> w [x' y' z' w'] 
  ;; 
  (define mtx-mult-4-4
    (lambda (mat1 mat2)
      (let* ((mat1-row-0 (vector (vector-ref mat1 0)
                                 (vector-ref mat1 4)
                                 (vector-ref mat1 8)
                                 (vector-ref mat1 12)))
             (mat1-row-1 (vector (vector-ref mat1 1)
                                 (vector-ref mat1 5)
                                 (vector-ref mat1 9)
                                 (vector-ref mat1 13)))
             (mat1-row-2 (vector (vector-ref mat1 2)
                                 (vector-ref mat1 6)
                                 (vector-ref mat1 10)
                                 (vector-ref mat1 14)))
             (mat1-row-3 (vector (vector-ref mat1 3)
                                 (vector-ref mat1 7)
                                 (vector-ref mat1 11)
                                 (vector-ref mat1 15)))
             (mat2-col-0 (vector (vector-ref mat2 0)
                                 (vector-ref mat2 1)
                                 (vector-ref mat2 2)
                                 (vector-ref mat2 3)))
             (mat2-col-1 (vector (vector-ref mat2 4)
                                 (vector-ref mat2 5)
                                 (vector-ref mat2 6)
                                 (vector-ref mat2 7)))
             (mat2-col-2 (vector (vector-ref mat2 8)
                                 (vector-ref mat2 9)
                                 (vector-ref mat2 10)
                                 (vector-ref mat2 11)))
             (mat2-col-3 (vector (vector-ref mat2 12)
                                 (vector-ref mat2 13)
                                 (vector-ref mat2 14)
                                 (vector-ref mat2 15))))
             
        ; 0 4  8 12
        ; 1 5  9 13
        ; 2 6 10 14
        ; 3 7 11 15
        (vector (dot-product-4 mat1-row-0 mat2-col-0)
                (dot-product-4 mat1-row-1 mat2-col-0)
                (dot-product-4 mat1-row-2 mat2-col-0)
                (dot-product-4 mat1-row-3 mat2-col-0)
                (dot-product-4 mat1-row-0 mat2-col-1)
                (dot-product-4 mat1-row-1 mat2-col-1)
                (dot-product-4 mat1-row-2 mat2-col-1)
                (dot-product-4 mat1-row-3 mat2-col-1)
                (dot-product-4 mat1-row-0 mat2-col-2)
                (dot-product-4 mat1-row-1 mat2-col-2)
                (dot-product-4 mat1-row-2 mat2-col-2)
                (dot-product-4 mat1-row-3 mat2-col-2)
                (dot-product-4 mat1-row-0 mat2-col-3)
                (dot-product-4 mat1-row-1 mat2-col-3)
                (dot-product-4 mat1-row-2 mat2-col-3)
                (dot-product-4 mat1-row-3 mat2-col-3)))))
    
    ;; vec1: (eq? vector-length 3)
    ;; vec2: (eq? vector-length 3)
    ;; vec1, vec2 -> float
    (define dot-product-3
      (lambda (vec1 vec2)
        (let ((v0 (* (vector-ref vec1 0)
                     (vector-ref vec2 0)))
              (v1 (* (vector-ref vec1 1)
                     (vector-ref vec2 1)))
              (v2 (* (vector-ref vec1 2)
                     (vector-ref vec2 2))))
          (+ (+ v0 v1) v2))))
    
    (define dot-product-4
      (lambda (vec1 vec2)
        (let ((v0 (* (vector-ref vec1 0)
                     (vector-ref vec2 0)))
              (v1 (* (vector-ref vec1 1)
                     (vector-ref vec2 1)))
              (v2 (* (vector-ref vec1 2)
                     (vector-ref vec2 2)))
              (v3 (* (vector-ref vec1 3)
                     (vector-ref vec2 3))))
          
          (+ (+ (+ v0 v1) v2) v3))))
    
    ;; rotation-center:point3, rot-x rot-y -> vector (16)
    (define rotation->transform
      (lambda (center dx dy dz)
        (let* ((mat-to-origin (vector 1 0 0 0
                                      0 1 0 0
                                      0 0 1 0
                                      (* -1 (point3-x center))
                                      (* -1 (point3-y center))
                                      (* -1 (point3-z center)) 1))
               (mat-from-origin (vector 1 0 0 0
                                        0 1 0 0
                                        0 0 1 0
                                        (* -1 (point3-x center))
                                        (* -1 (point3-y center))
                                        (* -1 (point3-z center)) 1))
               (mat-rot-x (xrotation->transform dx))
               (mat-rot-y (yrotation->transform dy))
               (mat-rot-z (yrotation->transform dz)))
          (mtx-mult-3-3 mat-from-origin
                        (mtx-mult-3-3 mat-rot-z
                                      (mtx-mult-3-3 mat-rot-y
                                                    (mtx-mult-3-3 mat-rot-x
                                                                  mat-to-origin)))))))
    
    (define rotation->transform-w-axis
      (lambda (center theta x y z)
        (let* ((mat-to-origin (vector 1 0 0 0
                                      0 1 0 0
                                      0 0 1 0
                                      (* -1 (point3-x center))
                                      (* -1 (point3-y center))
                                      (* -1 (point3-z center)) 1))
               (mat-from-origin (vector 1 0 0 0
                                        0 1 0 0
                                        0 0 1 0
                                        (* -1 (point3-x center))
                                        (* -1 (point3-y center))
                                        (* -1 (point3-z center)) 1))
               (c-t (cos theta))
               (s-t (sin theta))
               (l-c-t (- 1 c-t))
               (l-s-t (- 1 s-t))
               (m00 (+ (* x x) (* (- 1 (* x x)) c-t)))
               (m01 (- (* x y l-c-t) (* z s-t)))
               (m02 (+ (* x z l-c-t) (* y s-t)))
               (m10 (+ (* x y l-c-t) (* z s-t)))
               (m11 (+ (* y y) (* (- 1 (* y y)) c-t)))
               (m12 (- (* y z l-c-t) (* x s-t)))
               (m20 (- (* x z l-c-t) (* y s-t)))
               (m21 (+ (* y z l-c-t) (* x s-t)))
               (m22 (+ (* z z) (* (- 1 (* z z)) c-t)))
              (rot-matrix (vector m00 m10 m20 0.0 m01 m11 m21 0.0 m02 m12 m22 0.0 0.0 0.0 0.0 1.0)))
          ;     (rot-matrix (vector m00 m01 m02 0.0 m10 m11 m12 0.0 m20 m21 m22 0.0 0.0 0.0 0.0 1.0)))

          (mtx-mult-4-4 mat-from-origin
                        (mtx-mult-4-4 rot-matrix mat-to-origin)))))
    
    (define/override (on-paint)
 
      (with-gl-context
	 (lambda ()
           ;;____________P_I_C_K______________
           (when pick-mode-on
             (begin
               (printf "pick-mode-on\n")
               (set! pick-mode-on #f)
               (pick-on-paint mouse-px mouse-py 0.5 0.5 0 0)
               ))

           ;;__________R_E_N_D_E_R____________
	   (gl-clear-color 0.8 0.8 0.8 1.0)
	   (gl-clear 'color-buffer-bit 'depth-buffer-bit)

           ;; projection - update projection matrix
           (gl-matrix-mode 'projection)
           (gl-load-identity)
           ;; make-frustum
           (on-paint-frustum)
           ;; modelview matrix setup
           (gl-matrix-mode 'modelview)
           (gl-load-identity)
           (gl-look-at 0.0 0.0 40.0 0.0 0.0 0.0 0.0 1.0 0.0)
           ;(gl-translate 0.0 0.0 -40.0)
           
           (gl-light-v 'light0 'position (vector->gl-float-vector
                                          (vector 5.0 5.0 10.0 0.0)))
           (gl-enable 'cull-face)
           (gl-enable 'lighting)
           (gl-enable 'light0)
           (gl-enable 'depth-test)

           (render-pick)
           
	   (gl-flush)
        )))
    
        ; frustum constuction
    (define (on-paint-frustum)
      (gl-frustum -1.0 1.0 (- aspect) aspect 5.0 60.0))


    (define render-pick
      (lambda ()
        (unless (or gear1 gear2 gear3)
          
          (set! gear1 (gl-gen-lists 1))
          (gl-new-list gear1 'compile)
          (gl-material-v 'front
                         'ambient-and-diffuse
                         (vector->gl-float-vector (vector 0.8 0.1 0.0 1.0)))
          (build-gear gear1-name 1.0 4.0 1.0 20 0.7)
          (gl-end-list)
          
          (set! gear2 (gl-gen-lists 1))
          (gl-new-list gear2 'compile)
          (gl-material-v 'front
                         'ambient-and-diffuse
                         (vector->gl-float-vector (vector 0.0 0.8 0.2 1.0)))
          (build-gear gear23-name 0.5 2.0 2.0 10 0.7)
          (gl-end-list)
          
          (set! gear3 (gl-gen-lists 1))
          (gl-new-list gear3 'compile)
          (gl-material-v 'front
                         'ambient-and-diffuse
                         (vector->gl-float-vector (vector 0.2 0.2 1.0 1.0)))
          (build-gear gear23-name 1.3 2.0 0.5 10 0.7)
          (gl-end-list))
        
        (gl-enable 'normalize)  
        
        (gl-push-matrix)
        
        (let* ((mv-g (glGetDoublev GL_MODELVIEW_MATRIX 16)))
          (gl-load-identity)
          (gl-load-matrix (vector->gl-float-vector xform-global))
          (gl-mult-matrix mv-g))
        
        (gl-push-matrix)
        
        ;        (gl-translate -3.0 -2.0 0.0)
        ;        (gl-rotate rotation 0.0 0.0 1.0)
        (let* ((mvm (glGetDoublev GL_MODELVIEW_MATRIX 16)))
          ;          (gl-load-identity)
          
          ;          (printf "mvm = ~s ~s ~s ~s    ~s ~s ~s ~s    ~s ~s ~s ~s   ~s ~s ~s ~s\n"
          ;                  (cvector-ref mvm 0) (cvector-ref mvm 1) (cvector-ref mvm 2)
          ;                  (cvector-ref mvm 3) (cvector-ref mvm 4) (cvector-ref mvm 5) 
          ;                  (cvector-ref mvm 6) (cvector-ref mvm 7) (cvector-ref mvm 8) 
          ;                  (cvector-ref mvm 9) (cvector-ref mvm 10) (cvector-ref mvm 11) 
          ;                  (cvector-ref mvm 12) (cvector-ref mvm 13) (cvector-ref mvm 14) 
          ;                  (cvector-ref mvm 15))
          
          (gl-mult-matrix (vector->gl-float-vector 
                           xform-group1))
          ;
          ;          (gl-mult-matrix mvm)          
          ;          
          (gl-call-list gear1)
          
          
          (gl-pop-matrix))
        
        (gl-push-matrix)
        (gl-mult-matrix (vector->gl-float-vector xform-group23))
        (gl-push-matrix)
        (gl-translate 3.1 -2.0 0.0)
        (gl-rotate (- (* -2.0 rotation) 9.0) 0.0 0.0 1.0)
        (gl-call-list gear2)
        (gl-pop-matrix)
        
        (gl-push-matrix)
        (gl-translate -3.1 4.2 0.0)
        (gl-rotate (- (* -2.0 rotation) 25.0) 0.0 0.0 1.0)
        (gl-call-list gear3)
        (gl-pop-matrix)
        (gl-pop-matrix)
        
        (gl-pop-matrix)
        
        
        (swap-gl-buffers)))
    
    
    ;; Action on each mouse events
    (define/override (on-event e)
      (let* ((m-down  (send e button-down?))
             (m-up    (send e button-up?))
             (m-drag  (send e dragging?))
             (ka-down (send e get-alt-down))
             (ks-down (send e get-shift-down))) 
        
        (when m-down
          (set! mouse-px (send e get-x))
          (set! mouse-py (send e get-y))

          (if ka-down ;; picking 
                      ;; -> define rotation center and object to be rotated
              (begin
                (set! pick-mode-on #t)
                (refresh))
              )) 

        ; rotation===================
        ; drag with left mouse button
        (when (and m-drag (send e get-left-down) (not ks-down))
          (set! mouse-cx (send e get-x))
          (set! mouse-cy (send e get-y))
          
          (let* ((dx (- mouse-cx mouse-px))
                 (dy (- mouse-py mouse-cy))
                 (theta (* 0.1 (sqrt (+ (* dx dx) (* dy dy)))))
;                 (rot-xform (rotation->transform-w-axis (make-point3 0.0 0.0 0.0)
;                                                        theta (- dy) dx 0.0)))
                 (rot-xform (rotation->transform (make-point3 0.0 0.0 0.0)
                                                 (* 0.1 dy) (* 0.1 (- dx)) 0.0)))
            (set! mouse-px mouse-cx)
            (set! mouse-py mouse-cy)

            ;; ctrl dwn? => (pick object, rotate it) (rotate whole scene)
            (if ka-down
                ;; picking and rotation of picked scene-node(s)
                (let* ((picked-xform (if (empty? selected-scene-node-list)
                                         xform-global
                                         (car selected-scene-node-list))))
                  (printf "picked xform = ~s\n" picked-xform)
                  (printf "rotation xform = ~s\n" rot-xform)
                  (cond
                    [(eq? picked-xform xform-group1)
                     (let* ((xform-new (mtx-mult-4-4 xform-group1 rot-xform)))
                       (set! selected-scene-node-list (list xform-new))
                       (update-pick-id-table gear1-name xform-new)
                       (set! xform-group1  xform-new))]
                    [(eq? picked-xform xform-group23)
                     (let* ((xform-new (mtx-mult-4-4 xform-group23 rot-xform)))
                       (set! selected-scene-node-list (list xform-new))
                       (update-pick-id-table gear23-name xform-new)
                       (set! xform-group23 xform-new))]
                    [else #f]))
                ;; rotation of top object (whole object)
                (set! xform-global (mtx-mult-4-4 rot-xform xform-global)))
;                (set! xform-global (mtx-mult-4-4 xform-global rot-xform)))

            (refresh)))
        
        ; x-y translation===================
        ; drag with right mouse button
        (when (and m-drag (send e get-right-down) (not ks-down))
          (set! mouse-cx  (send e get-x))
          (set! mouse-cy  (send e get-y))
          (let* ((dx (- mouse-px mouse-cx))
                 (dy (- mouse-py mouse-cy)))

            (set! mouse-px mouse-cx)
            (set! mouse-py mouse-cy)
            
            ;; Alt -down and translations
            (if ka-down
                (begin
                  #f)

                (begin
                  (let* ((xln-xform (translation->transform 
                                     (* -0.05 dx) (* 0.05 dy) 0)))
                    (set! xform-global (mtx-mult-4-4 xln-xform xform-global)))))

            (refresh)))


        ; z - translation===================
        ; shift add transform from/to eye(not z) direction
        (when (and m-drag  ks-down);(send e get-shift-down))
          (set! mouse-cx  (send e get-x))
          (set! mouse-cy  (send e get-y))
          (let* ((dy (- mouse-cy mouse-py)))
                                
            (set! mouse-px mouse-cx)
            (set! mouse-py mouse-cy)

            ;; Alt -down and translations
            (if ka-down
                (begin
                  #f)

                (begin
                  (let* ((direction (point3-normalize (make-point3 0.0 0.0 1.0) (* 0.1 dy)))
                         (zm-xform (translation->transform (point3-x direction)
                                                           (point3-y direction)
                                                           (point3-z direction))))
                    (set! xform-global (mtx-mult-4-4 zm-xform xform-global)))))

            (refresh)))
        ))
    
    (define update-pick-id-table 
      (lambda (key new-value)
        (let* ((v (hash-table-get pick-id-table key)))
          (unless (null? v)
            (begin
              (hash-table-remove! pick-id-table key)
              (hash-table-put! pick-id-table key new-value))))))

  (define point3-length
    (lambda (v)
      (let* ((vx (point3-x v))
             (vy (point3-y v))
             (vz (point3-z v))
             (length (sqrt (+ (* vx vx) (* vy vy) (* vz vz)))))
        length)))
    
    ; normalization
    (define point3-normalize 
      (lambda (v scale)
        (let* ((vx (point3-x v))
               (vy (point3-y v))
               (vz (point3-z v))
               (length (point3-length v))
               (unit-v (make-point3 (* (/ vx length) scale)
                                    (* (/ vy length) scale)
                                    (* (/ vz length) scale))))
          unit-v)))
    
    (define look-into-selection-buffer
      (lambda (buffer)
        (if (empty? buffer)
            (begin 
              (printf "selection buffer is now empty\n") #f)
            ;((equal? empty buffer) (begin (printf "nothing_seleected-2\n") #f))
            (begin 
;              (printf "MY STACK = \t")
;              (display (my-selection-record-stack (car buffer))) (newline)
            
              (let* ((selection (car buffer))
                    (selected-obj (my-selection-record-stack selection)))
                (if (not (null? selected-obj))
                    (car selected-obj)
                    null)
                )))))

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
    
    ;; pickingroutine
    (define pick-on-paint
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
                             (+ py offy)) w h vp)
          ;; make-frustum
          (on-paint-frustum)
          ;; modelview matrix setup
          (gl-matrix-mode 'modelview)
          (gl-load-identity)
          (gl-look-at 0.0 0.0 40.0 0.0 0.0 0.0 0.0 1.0 0.0)
          ;(gl-translate 0.0 0.0 -40.0)

          ;;______________________________________________________
          ;; pick all objects defined in renderer-list (member)
          (render-pick)

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

            (printf "hits = ~s\n" hits)

            ; print selectbuffer
            (let* ((svec (gl-vector->vector selected-uint-list)))
              (printf "s-b ~s \t ~s ~s ~s\n ~s ~s ~s ~s\n " 
                      (vector-ref svec 0) (vector-ref svec 1) (vector-ref svec 2)
                      (vector-ref svec 3) (vector-ref svec 4) (vector-ref svec 5)
                      (vector-ref svec 6) (vector-ref svec 7)))

            (let ((picked-id (look-into-selection-buffer results)))
              (if (null? picked-id) 
                  (printf "selection buffer don't include given pick-id ~s\n" picked-id)
                  (begin ;; obtain scene-node from pick-id-table
                    (printf "picked-id = ~s\n" picked-id)
;                    (hash-table-for-each pick-id-table 
;                                         (lambda (k v) (printf "k = ~s , v's name = ~s\n" k v)))
                    (let* ((s-node (hash-table-get pick-id-table picked-id null)))

                      (if (null? s-node)
                          (printf "pick-id table doesn't have s-node w/ pick-id\n")
                          (begin
                            (printf "~s is selected" s-node)
                            ; update selected-scene-node-list
                            (set! selected-scene-node-list (list s-node))
;                                  (add-remove-group selected-scene-node-list s-node))
                            (print-selected-scene-node-list selected-scene-node-list)
                            ))
                      )))
              #f)))))
    
    (define print-selected-scene-node-list
      (lambda (sn-list)
        (newline)
        (if (null? sn-list) 
            (printf "selected-scene-node-list is null")
            (for-each (lambda (node)
                   (printf "current content of selected-scene-node-list= ~s \n" node))
                 sn-list))
        ))
    

    (super-instantiate () (style '(gl no-autoclear)))))
  
  
(define (main)
  (let* ((f (make-object frame% "gears.ss" #f))
         (c (instantiate gears-canvas% (f) (min-width 600) (min-height 600))))

    (send f show #t)))
  (main)
)
;;eof
