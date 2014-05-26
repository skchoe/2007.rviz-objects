(module mtl-read scheme
  (require 
           (lib "mred.ss" "mred")
           (lib "match.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "foreign.ss")
           (lib "etc.ss")
           mzlib/for
           sgl/gl
           sgl/gl-vectors
           "../render/gl-texture-helpers.ss"
           "../math/def.scm"
           "flat-obj.scm")
  
  (provide read-mtl
           topdown->bottomup
           print-mtl)

  ;; reading mtl stream
  (define read-mtl-stream 
    (lambda (in-stream)
      (let ((mat-table (make-hash))
            (current-material null))
        (let next-line ()
          (let* ((lit (read in-stream)))

            (unless (eof-object? lit)
              (begin
                (let* ((str (symbol->string lit)))
                  ;(printf "current=String = ~s\n" str)
                  (cond
                    [(equal? str "newmtl")
                     (begin
                       (set! current-material (symbol->string (read in-stream)))
                       ;(printf "current-material is ~s.\n" current-material)
                       (hash-set! mat-table
                                  current-material
                                  (default-mtl-params))
                       (next-line))]
                    [(equal? str "Ns")
                     (begin 
                       ;(printf "~s ~s\n" "Ns-read" current-material)
                       (let* ((current-params (hash-ref
                                               mat-table current-material))
                              (ns (read in-stream)))
                         ;(printf "input Ns = ~s\n" ns)
                         (hash-remove! mat-table current-material)
                         (hash-set! mat-table 
                                    current-material
                                    (make-mtl-params
                                     ns
                                     (mtl-params-d current-params)
                                     (mtl-params-illum current-params)
                                     (mtl-params-Ka current-params)
                                     (mtl-params-Kd current-params)
                                     (mtl-params-Ks current-params)
                                     (mtl-params-map_Kd current-params))))
                       (next-line))]
                    [(equal? str "d") 
                     (begin 
                       ;(printf "~s\n" "d-read")
                       (let* ((current-params (hash-ref
                                               mat-table current-material))
                              (d (read in-stream)))
                         (hash-remove! mat-table current-material)
                         (hash-set! mat-table 
                                    current-material
                                    (make-mtl-params
                                     (mtl-params-Ns current-params)
                                     d
                                     (mtl-params-illum current-params)
                                     (mtl-params-Ka current-params)
                                     (mtl-params-Kd current-params)
                                     (mtl-params-Ks current-params)
                                     (mtl-params-map_Kd current-params))))
                       (next-line))]
                    [(equal? str "illum") 
                     (begin 
                       ;(printf "~s\n" "illum-read")
                       (let* ((current-params (hash-ref
                                               mat-table current-material))
                              (il (read in-stream)))
                         (hash-remove! mat-table current-material)
                         (hash-set! mat-table 
                                         current-material
                                         (make-mtl-params
                                          (mtl-params-Ns current-params)
                                          (mtl-params-d current-params)
                                          il
                                          (mtl-params-Ka current-params)
                                          (mtl-params-Kd current-params)
                                          (mtl-params-Ks current-params)
                                          (mtl-params-map_Kd current-params))))
                       (next-line))]
                    [(equal? str "Ka") 
                     (begin 
                       ;(printf "~s\n" "Ka-read")
                       (let* ((current-params (hash-ref
                                               mat-table current-material))
                              (ka-r (read in-stream))
                              (ka-g (read in-stream))
                              (ka-b (read in-stream)))
                         (hash-remove! mat-table current-material)
                         (hash-set! mat-table 
                                    current-material
                                    (make-mtl-params
                                     (mtl-params-Ns current-params)
                                     (mtl-params-d current-params)
                                     (mtl-params-illum current-params)
                                     (make-point3 ka-r ka-g ka-b)
                                     (mtl-params-Kd current-params)
                                     (mtl-params-Ks current-params)
                                     (mtl-params-map_Kd current-params))))
                       (next-line))]
                    [(equal? str "Kd") 
                     (begin 
                       ;(printf "~s\n" "Kd-read")
                       (let* ((current-params (hash-ref
                                               mat-table current-material))
                              (kd-r (read in-stream))
                              (kd-g (read in-stream))
                              (kd-b (read in-stream)))
                         (hash-remove! mat-table current-material)
                         (hash-set! mat-table 
                                    current-material
                                    (make-mtl-params
                                     (mtl-params-Ns current-params)
                                     (mtl-params-d current-params)
                                     (mtl-params-illum current-params)
                                     (mtl-params-Ka current-params)
                                     (make-point3 kd-r kd-g kd-b)
                                     (mtl-params-Ks current-params)
                                     (mtl-params-map_Kd current-params))))
                       (next-line))]
                    [(equal? str "Ks") 
                     (begin 
                       ;(printf "~s\n" "Ks-read")
                       (let* ((current-params (hash-ref
                                               mat-table current-material))
                              (ks-r (read in-stream))
                              (ks-g (read in-stream))
                              (ks-b (read in-stream)))
                         (hash-remove! mat-table current-material)
                         (hash-set! mat-table 
                                    current-material
                                    (make-mtl-params
                                     (mtl-params-Ns current-params)
                                     (mtl-params-d current-params)
                                     (mtl-params-illum current-params)
                                     (mtl-params-Ka current-params)
                                     (mtl-params-Kd current-params)
                                     (make-point3 ks-r ks-g ks-b)
                                     (mtl-params-map_Kd current-params))))
                       (next-line))]
                    [(equal? str "map_Kd") 
                     (begin 
                       ;(printf "~s\n" "map_Kd-read")
                       (let* ((current-params (hash-ref
                                               mat-table current-material))
                              (map-name (symbol->string (read in-stream)))
                              (map-obj-top-down-scan (image->gl-vector map-name))
                              (map-obj-bottom-up-scan (topdown->bottomup 
                                                       map-obj-top-down-scan)))
                         ;(printf "map-name(texture) = ~s\n" map-name)
                         
                         (hash-remove! mat-table current-material)
                         (hash-set! mat-table 
                                    current-material
                                    (make-mtl-params
                                     (mtl-params-Ns current-params)
                                     (mtl-params-d current-params)
                                     (mtl-params-illum current-params)
                                     (mtl-params-Ka current-params)
                                     (mtl-params-Kd current-params)
                                     (mtl-params-Ks current-params)
                                     map-obj-bottom-up-scan)))
                       (next-line))]
                    
                    ;; # : NOT parsed
                    [(equal? str "#")
                     (begin
;                       (printf "~s\n" (read in-stream))
                       (next-line))]
                    
                    
                    ))))))
        mat-table)))
            
;  ;; bmp file name->image object
;  (define bmp->image 
;    (lambda (filename)
;      (let* ((in-bmp (open-input-file filename))
;             (bmp (read-bmp-stream in-bmp)))
;        (close-input-port in-bmp)
;        bmp)))
;  
;  ;; read-bmp-stream
;  (define read-bmp-stream
;    (lambda (in-bmp)
;      #f))
  
  ;; reading mtl file
  (define read-mtl
    (lambda (filename)
      (let* ((in-mtl (open-input-file filename))
             (mtl (read-mtl-stream in-mtl)))
        (close-input-port in-mtl)
        mtl)))
  
  
  (define convert-top-bottom
    (lambda (s t width height) 
      (+ s (* (- (- height 1) t) width))))
  
  ; modify vec:gl-vector by bottom up enumeration of the texture image.
  (define topdown->bottomup 
    (lambda (map-obj-td)
      (let* ((width (list-ref map-obj-td 0))
             (height (list-ref map-obj-td 1))
             (vec-bu (make-gl-ubyte-vector (* (* width height) 3)))
             (vec-td (list-ref map-obj-td 2))
             (vec-size (gl-vector-length vec-td)))
        (for* ([t (build-list height (lambda (x) x))]
               [s (build-list width (lambda (x) x))])
              (let* ((x (+ s (* t width)))
                     (y (convert-top-bottom
                         s t width height)))
                (gl-vector-set! vec-bu (* 3 x) (gl-vector-ref vec-td (* 3 y)))
                (gl-vector-set! vec-bu (+ 1 (* 3 x)) (gl-vector-ref vec-td (+ 1 (* 3 y))))
                (gl-vector-set! vec-bu (+ 2 (* 3 x)) (gl-vector-ref vec-td (+ 1 (* 3 y))))))
        (list width height vec-bu))))
                

        
;        (for i = 0 ;  i < height ; i++)
;             (for j = 0 ; j < width ; j++)
;                  int index = i * width + j;
;                  int td-index = (height-1-i) * width + j;
;                  vec(3*index)   = org-vec(3*td-index)
;                  vec(3*index+1) = org-vec(3*td-index+1)
;                  vec(3*index+2) = org-vec(3*td-index+2)
                  
;        (printf "width = ~s\t height = ~s\n, size of vec = ~s\n"
;                width height (cvector-length vec-td)))))


  ;; print each hash
  (define print-mtl-hash
    (lambda (k v)
      (printf "key: ~s\n" k)
      (printf "v-: ~s\n" (mtl-params-Ns v))
      (printf "v-: ~s\n" (mtl-params-d v))
      (printf "v-: ~s\n" (mtl-params-illum v))
      (printf "v-: ~s ~s ~s\n" (point3-x (mtl-params-Ka v))
              (point3-y (mtl-params-Ka v)) (point3-z (mtl-params-Ka v)))
      (printf "v-: ~s ~s ~s\n" (point3-x (mtl-params-Kd v))
              (point3-y (mtl-params-Kd v)) (point3-z (mtl-params-Kd v)))
      (printf "v-: ~s ~s ~s\n" (point3-x (mtl-params-Ks v))
              (point3-y (mtl-params-Ks v)) (point3-z (mtl-params-Ks v)))))
  
  ;; mtl:hash-table -> <void?>
  (define print-mtl 
    (lambda (mtl)
      (unless (empty? mtl)
        (hash-for-each mtl print-mtl-hash)
        (printf "hash-table is null\n"))))
  
  ;; real file parsing
  (define main-mtl
    (lambda ()
      (let* ((mtl (read-mtl "guppybluegrass0.obj.mtl")))
        (print-mtl mtl))))
  
  ;(main-mtl)
)
  