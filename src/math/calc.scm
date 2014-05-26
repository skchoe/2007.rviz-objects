(module calc mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "match.ss")
           "def.scm")
  
  (provide (all-defined))
  
  ; 4x4 matrix x:vector, 4x4 matrix y:vector
  ; x * y -> 4 x 4 matrix
  ; assume mat1, mat2 are 4x4 float-valued vectors
  ; rotation part only
  ; 0 4 8  12
  ; 1 5 9  13    -> [row-0|
  ; 2 6 10 14
  ; 3 7 11 15
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
  
  ;; vector-16 -> (void)
  (define display-matrix-4-4
    (lambda (mtx)
      (printf "~s ~s ~s ~s\n~s ~s ~s ~s\n~s ~s ~s ~s\n~s ~s ~s ~s\n"
              (vector-ref mtx 0) (vector-ref mtx 4) (vector-ref mtx 8)  (vector-ref mtx 12)
              (vector-ref mtx 1) (vector-ref mtx 5) (vector-ref mtx 9)  (vector-ref mtx 13)
              (vector-ref mtx 2) (vector-ref mtx 6) (vector-ref mtx 10) (vector-ref mtx 14)
              (vector-ref mtx 3) (vector-ref mtx 7) (vector-ref mtx 11) (vector-ref mtx 15))))
    
  ;; vector of size 3-> (void)
  (define display-vector3
    (lambda (v)
      (printf "~s ~s ~s \n" 
              (vector-ref v 0)
              (vector-ref v 1)
              (vector-ref v 2))))
 
; example
;  (display-matrix-4-4
;   (mtx-mult-3-3 
;    (vector 1 2 3 0 1 2 3 0 1 2 3 0 0 0 0 1)
;    (vector 1 2 3 0 1 2 3 0 1 2 3 0 0 0 0 1)))
  
  
  (define (test)
    (let* ((v1 (vector 1 2 3 0 1 2 3 0 1 2 3 0 0 0 0 1)))
      (display (vector-ref v1 0))
      (newline)
      (let* ((v2 (vector (vector-ref v1 0) (vector-ref v1 1))))
        (display (vector-length v2)))))

  
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
  
  
  ;; translation-center:point3, mov-x mov-y -> vector (16)
  (define translation->transform
    (lambda (dx dy dz)
      (vector 1 0 0 0
              0 1 0 0
              0 0 1 0
              dx dy dz 1)))

  
  ;; transform v[16] -> point3
  (define transform->translation-3
    (lambda (xform)
      (make-point3 (vector-ref xform 12)
                   (vector-ref xform 13)
                   (vector-ref xform 14))))

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
             
  ;; component wise division
  (define point3-comp-div
    (lambda (dvd dvr)
      (make-point3 (/ (point3-x dvd) (point3-x dvr))
                   (/ (point3-y dvd) (point3-y dvr))
                   (/ (point3-z dvd) (point3-z dvr)))))
  
  ; arg0:point3
  ; arg1:point3 
  ; component wise multiplication -> point3
  (define point3-comp-mult
    (lambda (arg0 arg1)
      (make-point3 (* (point3-x arg0) (point3-x arg1))
                   (* (point3-y arg0) (point3-y arg1))
                   (* (point3-z arg0) (point3-z arg1)))))
    
  (define point3-inner-product
    (lambda (arg0 arg1)
      (+ (* (point3-x arg0) (point3-x arg1))
         (* (point3-y arg0) (point3-y arg1))
         (* (point3-z arg0) (point3-z arg1)))))
  
  (define point3-cross-product
    (lambda (arg0 arg1)
      (let* ((x0 (point3-x arg0))
             (y0 (point3-y arg0))
             (z0 (point3-z arg0))
             (x1 (point3-x arg1))
             (y1 (point3-y arg1))
             (z1 (point3-z arg1)))
        (make-point3 (- (* y0 z1) (* z0 y1))
                     (- (* z0 x1) (* x0 z1))
                     (- (* x0 y1) (* y0 x1))))))
      
  (define print-point2
    (lambda (p2)
      (printf "~s ~s\n" (point2-s p2) (point2-t p2))))
      
  ;; point3, point3 to add -> new point3
  (define point3-add
    (lambda (org val)
;      (printf "~s ~s ~s\t+\t ~s ~s ~s\n"  
;              (point3-x org) (point3-y org) (point3-z org)
;              (point3-x val) (point3-y val) (point3-z val))
      (make-point3 (+ (point3-x org) (point3-x val))
                   (+ (point3-y org) (point3-y val))
                   (+ (point3-z org) (point3-z val)))))

  (define add-point3-list
    (lambda (list-point3)
      (foldl point3-add (make-point3 0 0 0) list-point3)))
  
  (define print-point3-list
    (lambda (p3-list)
      (for-each print-point3 p3-list)))
      
  (define print-point3
    (lambda (p)
      (printf "~s ~s ~s\n" (point3-x p) (point3-y p) (point3-z p))))
  
  (define point3-diff
    (lambda (from to)
      (make-point3 (- (point3-x to) (point3-x from))
                   (- (point3-y to) (point3-y from))
                   (- (point3-z to) (point3-z from)))))
  
  (define point3-abs-diff
    (lambda (v0 v1)
      (let* ((diff (point3-diff v0 v1)))
        (make-point3 (abs (point3-x diff))
                     (abs (point3-y diff))
                     (abs (point3-z diff))))))
  
  (define update-min-max
    (lambda (min-pos max-pos new-pt3)
      (let* (
             (min-x (point3-x min-pos))
             (min-y (point3-y min-pos))
             (min-z (point3-z min-pos))
             (max-x (point3-x max-pos))
             (max-y (point3-y max-pos))
             (max-z (point3-z max-pos))
             (x (point3-x new-pt3))
             (y (point3-y new-pt3))
             (z (point3-z new-pt3))
             )

        (cond
          ;(-1 -1 -1)
          [(and (< x min-x) (< y min-y) (< z min-z)) 
           (list (make-point3 x y z) 
                 (make-point3 max-x max-y max-z))]
          ;(-1 -1 1)
          [(and (< x min-x) (< y min-y) (> z max-z)) 
           (list (make-point3 x y min-z) 
                 (make-point3 max-x max-y z))]
          ;(-1 -1 0)
          [(and (< x min-x) (< y min-y))
           (list (make-point3 x y min-z)
                 (make-point3 max-x max-y max-z))]
          ;(-1 1 -1)
          [(and (< x min-x) (> y max-y) (< z min-z))
           (list (make-point3 x min-y z)
                 (make-point3 max-x y max-z))]
          ;(-1 1 1)
          [(and (< x min-x) (> y max-y) (> z max-z))
           (list (make-point3 x min-y min-z)
                 (make-point3 max-x y z))]
          ;(-1 1 0)
          [(and (< x min-x) (> y max-y))
           (list (make-point3 x min-y min-z)
                 (make-point3 max-x y max-z))]
          ;(-1 0 -1)
          [(and (< x min-x) (< z min-z))
           (list (make-point3 x min-y z)
                 (make-point3 max-x max-y max-z))]
          ;(-1 0 1)
          [(and (< x min-x) (> z max-z)) 
           (list (make-point3 x min-y min-z)
                 (make-point3 max-x max-y z))]
          ;(-1 0 0)
          [(and (< x min-x)) 
           (list (make-point3 x min-y min-z)
                 (make-point3 max-x max-y max-z))]
          ;(1 -1 -1)
          [(and (> x max-x) (< y min-y) (< z min-z))
           (list (make-point3 min-x y z)
                 (make-point3 x max-y max-z))]
          ;(1 -1 1)
          [(and (> x max-x) (< y min-y) (> z max-z))
           (list (make-point3 min-x y min-z)
                 (make-point3 x max-y z))]
          ;(1 -1 0)
          [(and (> x max-x) (< y min-y)) 
           (list (make-point3 min-x y min-z)
                 (make-point3 x max-y max-z))]
          ;(1 1 -1)
          [(and (> x max-x) (> y max-y) (< z min-z))
           (list (make-point3 min-x min-y z)
                 (make-point3 x y max-z))]
          ;(1 1 1)
          [(and (> x max-x) (> y max-y) (> z max-z))
           (list (make-point3 min-x min-y min-z)
                 (make-point3 x y z))]
          ;(1 1 0)
          [(and (> x max-x) (> y max-y))
           (list (make-point3 min-x min-y min-z)
                 (make-point3 x y max-z))]
          ;(1 0 -1)
          [(and (> x max-x) (< z min-z)) 
           (list (make-point3 min-x min-y z)
                 (make-point3 x max-y max-z))]
          ;(1 0 1)
          [(and (> x max-x) (> z max-z)) 
           (list (make-point3 min-x min-y min-z)
                 (make-point3 x max-y z))]
          ;(1 0 0)
          [(and (> x max-x)) 
           (list (make-point3 min-x min-y min-z)
                 (make-point3 x max-y max-z))]
          ;(0 -1 -1)
          [(and (< y min-y) (< z min-z)) 
           (list (make-point3 min-x y z)
                 (make-point3 max-x max-y max-z))]
          ;(0 -1 1)
          [(and (< y min-y) (> z max-z))
           (list (make-point3 min-x y min-z)
                 (make-point3 max-x max-y z))]
          ;(0 -1 0)
          [(and (< y min-y)) 
           (list (make-point3 min-x y min-z)
                 (make-point3 max-x max-y max-z))]
          ;(0 1 -1)
          [(and (> y max-y) (< z min-z)) 
           (list (make-point3 min-x min-y z)
                 (make-point3 max-x y max-z))]
          ;(0 1 1)
          [(and (> y max-y) (> z max-z))
           (list (make-point3 min-x min-y min-z)
                 (make-point3 max-x y z))]
          ;(0 1 0)
          [(and (> y max-y))
           (list (make-point3 min-x min-y min-z)
                 (make-point3 max-x y max-z))]
          ;(0 0 -1)
          [(and (< z min-z))
           (list (make-point3 min-x min-y z)
                 (make-point3 max-x max-y max-z))]
          ;(0 0 1)
          [(and (> z max-z)) 
           (list (make-point3 min-x min-y min-z)
                 (make-point3 max-x max-y z))]
          ;(0 0 0)
          [else (list min-pos max-pos)]))))  
)