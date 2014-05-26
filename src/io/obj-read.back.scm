(module obj-read.back mzscheme
  (require (lib "mred.ss" "mred")
           (lib "match.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "etc.ss")
           "mtl-read.scm"
           "../math/def.scm"
           "../math/calc.scm"
           "flat-obj.scm")
  
  (provide (all-defined))

  (define read-obj-stream
    (lambda (in-stream)
      (let* ((current-part (make-obj-part #f empty #f))
             (obj-obj (make-obj-object null 
                                       #f 
                                       (make-obj-vertices empty empty empty) 
                                       empty
                                       empty)))

        (let next-line ([current-part current-part][obj-obj obj-obj])
          (let* ((lit (read in-stream)))
            ;(printf "first string/symbol = ~s\n" lit)
            (unless (eof-object? lit)
              (begin

                (let* ((str (if (symbol? lit) 
                                (symbol->string lit)
                                (next-line))))
                  (cond
                    
                    ;; mtllib - unique in the obj-object
                    [(equal? str "mtllib")
                     (let* ((mtl-filename (symbol->string (read in-stream)))
                           (mtl-table (read-mtl mtl-filename)))
                       ;(print-mtl-table mtl-table)
                       (next-line current-part
                                  (make-obj-object (obj-object-dimension obj-obj)
                                                   mtl-filename 
                                                   (obj-object-vertices obj-obj)
                                                   (obj-object-parts obj-obj)
                                                   mtl-table)))]
                    
                    ;; g
                    [(equal? str "g")
                     (begin
                       (let ((part-name (read in-stream)))
;                         (printf "part: ~s\n" part-name)
                         (if (is-new-part-name-in-object obj-obj part-name)
                             (begin ;; make new part and add to part list
;                               (printf "New part\n")
                               (next-line
                                (make-obj-part part-name empty #f)
                                (make-obj-object 
                                 (obj-object-dimension obj-obj)
                                 (obj-object-mtllib obj-obj)
                                 (obj-object-vertices obj-obj)
                                 (cons current-part
                                       (obj-object-parts obj-obj))
                                 (obj-object-materials obj-obj))
                                )
                               ;(print-part-names (obj-object-parts obj-obj))
                               )
                             (begin
;                               (printf "Not a new part\n")
                               (next-line
                                (find-part-in-object obj-obj part-name)
                                obj-obj)))))]
                    
                    ;; v
                    [(equal? str "v")
                     (begin
;                       (printf "part: ~s \n" (obj-part-name current-part))
                       (let* ((points (obj-vertices-points (obj-object-vertices obj-obj)))
                              (x (read in-stream))
                              (y (read in-stream))
                              (z (read in-stream))
                              (pt (make-point3 x y z)))

                         ; one point
                         (next-line
                          current-part
                          (make-obj-object
                           (calc-obj-dimension (make-point3 x y z) 
                                               (obj-object-dimension obj-obj))
                           (obj-object-mtllib obj-obj)
                           (make-obj-vertices 
                            (append points (list pt))
                            (obj-vertices-normals (obj-object-vertices obj-obj))
                            (obj-vertices-texture-coords (obj-object-vertices obj-obj)))
                           (obj-object-parts obj-obj)
                           (obj-object-materials obj-obj)))))]
                         
                    ;; vn
                    [(equal? str "vn")
                     (begin
                       (let* ((normals (obj-vertices-normals (obj-object-vertices obj-obj)))
                              (xn (read in-stream))
                              (yn (read in-stream))
                              (zn (read in-stream))
                              (nm (make-point3 xn yn zn)))
;                         (printf "~s ~s ~s \n" xn yn zn)
                         ; one normal
                         (next-line
                          current-part
                          (make-obj-object
                           (obj-object-dimension obj-obj)
                           (obj-object-mtllib obj-obj)
                           (make-obj-vertices
                            (obj-vertices-points (obj-object-vertices obj-obj))
                            (append normals (list nm))
                            (obj-vertices-texture-coords (obj-object-vertices obj-obj)))
                           (obj-object-parts obj-obj)
                           (obj-object-materials obj-obj)))))]

                    ;; vt
                    [(equal? str "vt")
                     (begin
                       (let* ((texture-coords (obj-vertices-texture-coords
                                               (obj-object-vertices obj-obj)))
                              (st (read in-stream))
                              (tt (read in-stream))
                              (tc (make-point2 st tt)))
;                         (printf "~s ~s \n" st tt)
                         ; one texture-coord
                         (next-line
                          current-part
                          (make-obj-object
                           (obj-object-dimension obj-obj)
                           (obj-object-mtllib obj-obj)
                           (make-obj-vertices
                            (obj-vertices-points (obj-object-vertices obj-obj))
                            (obj-vertices-normals (obj-object-vertices obj-obj))
                            (append texture-coords (list tc)))
                           (obj-object-parts obj-obj)
                           (obj-object-materials obj-obj)))))]

                    
                    ;; f
                    [(equal? str "f")
                     (begin
                       (let* ((faces (obj-part-faces current-part))
                              (old-part current-part)
                              (f0 (symbol->string (read in-stream)))
                              (f1 (symbol->string (read in-stream)))
                              (f2 (symbol->string (read in-stream))))
;                         (printf "~s ~s ~s \n" f0 f1 f2)
                         (let* ((F0 (vertex->p-n-t f0))
                                (F1 (vertex->p-n-t f1))
                                (F2 (vertex->p-n-t f2)))
;                           ; one face
;                           (printf "point: ~s ~s ~s\n" 
;                                   (string->number (point3-x F0))
;                                   (string->number (point3-x F1))
;                                   (string->number (point3-x F2)))
;                           (printf "normal: ~s ~s ~s\n" 
;                                   (string->number (point3-y F0)) 
;                                   (string->number (point3-y F1)) 
;                                   (string->number (point3-y F2)))
;                           (printf "tex-coord: ~s ~s ~s\n" 
;                                   (string->number (point3-z F0))
;                                   (string->number (point3-z F1)) 
;                                   (string->number (point3-z F2)))
                           
                           (let ((new-face (make-obj-face (make-point3 
                                           (sub1 (string->number (point3-x F0)))
                                           (sub1 (string->number (point3-x F1)))
                                           (sub1 (string->number (point3-x F2))))
                                          (make-point3 
                                           (sub1 (string->number (point3-y F0)))
                                           (sub1 (string->number (point3-y F1)))
                                           (sub1 (string->number (point3-y F2))))
                                          (make-point3 
                                           (sub1 (string->number (point3-z F0)))
                                           (sub1 (string->number (point3-z F1)))
                                           (sub1 (string->number (point3-z F2)))))))
                             
                             (next-line
                              (make-obj-part
                               (obj-part-name current-part)
                               (append (obj-part-faces current-part) (list new-face))
                               (obj-part-material current-part))
                              (make-obj-object
                               (obj-object-dimension obj-obj)
                               (obj-object-mtllib obj-obj)
                               (obj-object-vertices obj-obj)
                               (append (remq old-part
                                             (obj-object-parts obj-obj))
                                       (list current-part))
                               (obj-object-materials obj-obj)))))))]
                         
                    
                    ;; # : NOT parsed
                    [(equal? str "#")
                     (begin
;                       (printf "~s\n" (read in-stream))
                       (next-line
                        current-part obj-obj))]
                    
                    
                    ;; s : NOT parsed
                    [(equal? str "s")
                     (begin
                       (let* (( s-val (read in-stream) ))
                         #f)
                       (next-line current-part obj-obj))]
                    
                    ;; usemtl
                    [(equal? str "usemtl")
                     (begin
                       (let* ((old-part current-part)
                              (mat-name (symbol->string (read in-stream))))
                         ;(printf "use-mtl name = ~s\n" mat-name)
                         (next-line 
                          (make-obj-part
                           (obj-part-name current-part)
                           (obj-part-faces current-part)
                           mat-name)
                          (make-obj-object
                           (obj-object-dimension obj-obj)
                           (obj-object-mtllib obj-obj)
                           (obj-object-vertices obj-obj)
                           (append (remq old-part
                                         (obj-object-parts obj-obj))
                                   (list current-part))
                           (obj-object-materials obj-obj)))))]
                         
                    [else (next-line current-part obj-obj)])
                  )))
            obj-obj)))))
    
  (define vertex->p-n-t
    (lambda (fid)
      (let* ((list (regexp-split #rx"/" fid)))
        (make-point3 (list-ref list 0)
                     (list-ref list 1)
                     (list-ref list 2)))))

  (define is-new-part-name-in-object 
    (lambda (obj-obj part-name)
      (let ((obj-parts (obj-object-parts obj-obj)))
        (is-new-part-name-in-part obj-parts part-name))))
  
  (define is-new-part-name-in-part
    (lambda (obj-parts part-name)
      (if (not (empty? obj-parts))
          (begin ;(printf "O---gname ~s <-> ~s\n" 
               ;        (obj-part-name (car obj-parts)) part-name)
               (call/cc
                (lambda (exit)
                  (if (equal? part-name (obj-part-name (car obj-parts)))
                      (exit #f)
                      (is-new-part-name-in-part
                       (cdr obj-parts) part-name)))))
          #t)))

  (define find-part-in-object 
    (lambda (obj-obj name)
      (let ((obj-parts (obj-object-parts obj-obj)))
        (find-part-of-name-in-part obj-parts name))))
    
  (define find-part-of-name-in-part 
    (lambda (obj-parts name)
      (if (empty? obj-parts)
          #f
          (if (equal? name (obj-part-name (car obj-parts)))
              (car obj-parts)
              (find-part-of-name-in-part (cdr obj-parts) name)))))


  
  
  ;;--------------------------------------------------------------------
  (define add-a-face-to-vi-fl-table 
    (lambda (face vi-fl-table)
      (let* ((point (obj-face-point face))
             (vi0 (point3-x point))
             (vi1 (point3-y point))
             (vi2 (point3-z point))
             (fl0 (hash-table-get vi-fl-table vi0 (lambda () empty)))
             (fl1 (hash-table-get vi-fl-table vi1 (lambda () empty)))
             (fl2 (hash-table-get vi-fl-table vi2 (lambda () empty))))
        (hash-table-put! vi-fl-table vi0 (append fl0 (list face)))
        (hash-table-put! vi-fl-table vi1 (append fl1 (list face)))
        (hash-table-put! vi-fl-table vi2 (append fl2 (list face)))
        vi-fl-table)))
    
  (define add-faces-to-vi-fl-table
    (lambda (part vi-fl-table)
      (let* ((faces (obj-part-faces part)))
        (foldl add-a-face-to-vi-fl-table vi-fl-table faces))))
      
  ;vi-f-table:hash-table: vertex-index -> list of faces
  (define collect-neighbor-faces
    (lambda (obj)
      (let* ((parts (obj-object-parts obj)))
        (foldl add-faces-to-vi-fl-table (make-hash-table 'equal) parts))))
  
  ;; --------------------------------
  ;; structure for normal vector calc.
  (define-struct hash-w-points (h-table points))
  
  (define calc-face-normal-one
    (lambda (face points)
      (let* ((point (obj-face-point face))
             (iv0 (point3-x point))
             (iv1 (point3-y point))
             (iv2 (point3-z point))
             (p0 (list-ref points iv0))
             (p1 (list-ref points iv1))
             (p2 (list-ref points iv2))
             (v01 (point3-diff p0 p1))
             (v02 (point3-diff p0 p2))
             (result (point3-cross-product v01 v02)))
;        (printf "cross-result = ~s ~s ~s\n" 
;                (point3-x result) (point3-y result) (point3-z result))
        result)))
                  
  (define calc-face-normal-face
    (lambda (face hash-points)
      (let* ((h-table (hash-w-points-h-table hash-points))
             (points (hash-w-points-points hash-points)))
        
;        (if (null? h-table) 
;            (printf "h table is null\n")
;            (if (hash-table? h-table)
;                (printf "h-table is hashtable\n")
;                (printf "h-table isn't hashtable\n")))
;        
        (hash-table-put! h-table face (calc-face-normal-one face points))
        (make-hash-w-points h-table points))))
  
  (define calc-face-normal-part 
    (lambda (part hash-points)
      (let* ((faces (obj-part-faces part))
             (h-table (hash-w-points-h-table hash-points)))
        
;        (printf "name of part = ~s\n" (obj-part-name part))

        (foldl calc-face-normal-face hash-points faces))))
  
  (define calc-face-normals
    (lambda (obj)
      (let* ((vertices (obj-object-vertices obj))
             (points (obj-vertices-points vertices))
             (parts (obj-object-parts obj))
             [f-fn-hash-table (make-hash-table 'equal)])

        (hash-w-points-h-table 
         (foldl calc-face-normal-part (make-hash-w-points f-fn-hash-table points) parts)))))


  (define print-key-value
    (lambda (k v)
      (printf "hash-k = ~s, \t hash-v = ~s \n" k v)))
  
  (define-struct normal-m-struct (new-normals f-n-table vi-fl-table))
  
  ;; normals:list-of-existing-normals
  ;; f-n-table: face-facenormal hash-table
  ;; vi-fl-table: v-idx-(list of faces) hash-table
  (define calc-point-normals
    (lambda (index-list f-n-table vi-fl-table)
      (let* ((new-normals empty)
             (normal-m-struct (make-normal-m-struct new-normals f-n-table vi-fl-table)))
        (normal-m-struct-new-normals (foldl calc-a-normal normal-m-struct index-list)))))


  
  ;; normal: existing normal (ordered)
  ;; n-m-struct: struct w/ new-normal in it.
  ;;output : normal-m-struct
  (define calc-a-normal
    (lambda (v-index n-m-struct)
      (let* ((normal-list (normal-m-struct-new-normals n-m-struct))
             (f-n-table (normal-m-struct-f-n-table n-m-struct))
             (vi-fl-table (normal-m-struct-vi-fl-table n-m-struct))
             (f-list (hash-table-get vi-fl-table v-index))
             (face-normal-list (normals-from-table-normal-list 
                                (face-list->face-normal-list f-list f-n-table)))
             (f-n-list-size (length face-normal-list))
             (new-added (add-face-normal-list face-normal-list))
             (new-normal (point3-comp-div 
                          new-added 
                          (make-point3 f-n-list-size f-n-list-size f-n-list-size)))
             (new-unit-normal (point3-normalize new-normal 1.0))
             (new-normal-list (append normal-list (list new-unit-normal))))
        
;        (printf "new-point-normal = ~s ~s ~s\n"
;                (point3-x new-normal) (point3-y new-normal) (point3-z new-normal))
        
        (make-normal-m-struct new-normal-list
                              f-n-table
                              vi-fl-table))))
  
    (define add-face-normal-list
      (lambda (f-n-list)
        (add-point3-list f-n-list)))

  
  (define-struct normals-from-table (normal-list f-n-table))
  
  (define face-list->face-normal-list 
    (lambda (face-list face-normal-table)
      (foldl face->face-normal (make-normals-from-table empty face-normal-table) face-list)))
    
  (define face->face-normal
    (lambda (face normal-table)
      (let* ((f-n-table (normals-from-table-f-n-table normal-table))
             (normal-list (normals-from-table-normal-list normal-table)))

        (make-normals-from-table (append normal-list (list (hash-table-get f-n-table face)))
                                 f-n-table))))
  
  ;; calc normal vector for each vertex
  ;; obj-object -> obj-object with valied normal
  (define add-unit-normal
    (lambda (obj)
             ;; storage of faces per vertex: start empty
      (let* ((vertices (obj-object-vertices obj))
             (parts (obj-object-parts obj))
             (points (obj-vertices-points vertices))
             (f-n-table (calc-face-normals obj))
             (vi-fl-table (collect-neighbor-faces obj))
             (index-list (build-list (length points) (lambda (n) n)))
             (new-normals (calc-point-normals index-list
                                              f-n-table
                                              vi-fl-table))
             (new-vertices (make-obj-vertices (obj-vertices-points vertices)
                                              new-normals
                                              (obj-vertices-texture-coords vertices))))
        (printf "face table info = # of faces = ~s\n" (hash-table-count f-n-table))
;        (print-point3-list new-normals)

          (make-obj-object (obj-object-dimension obj)
                           (obj-object-mtllib obj)
                           new-vertices
                           (obj-object-parts obj)
                           (obj-object-materials obj)))))
  
  
  
  
  
  
  
  (define obj-object->vertex-size
    (lambda (obj)
      (length (obj-vertices-points (obj-object-vertices obj)))))
  
  ;; calc dimension from given obj-object
  (define calc-obj-dimension-from-points
    (lambda (pts)
      (let ((dim null))
        (foldl calc-obj-dimension dim pts))))
        
  ; dim:dimesion
  ; p3: point3
  (define calc-obj-dimension
    (lambda (p3 dim)
      (if (null? dim)
          (make-dimension (make-point3 (point3-x p3)
                                       (point3-y p3)
                                       (point3-z p3))
                          (make-point3 (point3-x p3)
                                       (point3-y p3)
                                       (point3-z p3)))
          (let* ((min-pos (dimension-min-position dim))
                 (max-pos (dimension-max-position dim))
                 (min-max-list (update-min-max min-pos max-pos p3)))
            (make-dimension (list-ref min-max-list 0)
                            (list-ref min-max-list 1))))))
  



  ; mtl-table: hash-table (material-name, mtl-params)
  (define print-mtl-table
    (lambda (mtl-table)
      (printf "hash-table of size = ~s\n" (hash-table-count mtl-table))
      (hash-table-for-each mtl-table print-mtl-param)))
  
  ;input key, value of mtl-hash-table
  (define print-mtl-param
    (lambda (k v)
      (printf "key of mtl-hash-table = ~s \n" k)
      (let* ((Ns (mtl-params-Ns v))
             (d (mtl-params-d v))
             (illum (mtl-params-illum v))
             (Ka (mtl-params-Ka v))
             (Kd (mtl-params-Kd v))
             (Ks (mtl-params-Ks v))
             (map_Kd (mtl-params-map_Kd v)))
        (printf "Ns = ~s\t" Ns) (printf "d = ~s\t" d) (printf "illum = ~s\n" illum)
        (print-point Ka)
        (print-point Kd)
        (print-point Ks)
        (printf "map_Kd = ~s\n" map_Kd))))
  
  ;; obj-object -> stdout: part names
  (define print-part-names 
    (lambda (parts)
      (map (lambda (gr) (printf "part-name: ~s\n" (obj-part-name gr))) parts)))
  
  ;; print all contents of obj-object 
  (define print-obj
    (lambda (obj-obj)
      (printf "mtllib = ~s\n" (obj-object-mtllib obj-obj))
;      (let ((vertices (obj-object-vertices obj-obj)))
;        (map print-point (obj-vertices-points vertices))
;        (map print-normal (obj-vertices-normals vertices))
;        (map print-texture (obj-vertices-texture-coords vertices)))
      (let ((parts (obj-object-parts obj-obj)))
        (map print-part parts))))

    
  ;; print vertex
  (define print-vertex
    (lambda (vtx)
;      (let* ((points (obj-vertices-points vtx))
;             (normals (obj-vertices-normals vtx))
;             (textures (obj-vertices-texture-coords vtx)))
;        ;; point printing
;        (map print-point points)
;        ;; normals printing
;        (map print-normal normals)
;        ;; textures printing
;        (map print-texture textures))))
      #f))
        
        
  ;; print part elements
  (define print-part 
    (lambda (part)
      (printf "part-name = ~s\n" (obj-part-name part))
      (let* ((mat (obj-part-material part))
             (faces (obj-part-faces part)))
        ;; material
        (printf "material = ~s\n" mat)
        ;; face-point-index printing
        (map print-face faces)
        )))
  
  ;; point
  (define print-point
    (lambda (point)
      (printf "pnt : ~s ~s ~s\n" (point3-x point) (point3-y point) (point3-z point))
      ))
      
  ;; normals
  (define print-normal
    (lambda (normals)
      (printf "nml : ~s ~s ~s\n" 
              (point3-x normals) (point3-y normals) (point3-z normals))
      ))
        
  ;; texture
  (define print-texture
    (lambda (tex)
      (printf "txc : ~s ~s \n" (point2-s tex) (point2-t tex))
      ))

  (define print-face 
    (lambda (face)
;      (let ((idx-pnt (obj-face-point  face))
;            (idx-nml (obj-face-normal face))
;            (idx-tex (obj-face-texture-coord face)))
;
;        (printf "PT-IDX: ~s ~s ~s\n" 
;                (point3-x idx-pnt) 
;                (point3-y idx-pnt) 
;                (point3-z idx-pnt))
;        
;        (printf "NL-IDX: ~s ~s ~s\n" 
;                (point3-x idx-nml) 
;                (point3-y idx-nml) 
;                (point3-z idx-nml))
;
;        (printf "TX-IDX: ~s ~s ~s\n" 
;                (point3-x idx-tex) 
;                (point3-y idx-tex) 
;                (point3-z idx-tex)))))
      #f))
  
  (define read-obj
    (lambda (filename)
      (let* ((in-obj (open-input-file filename))
             (obj (read-obj-stream in-obj)))
        (close-input-port in-obj)
        obj)))


  ;; real file parsing
  (define main-obj
    (lambda ()
      (let* ((obj (read-obj "GuppyBlueGrass0.obj")))
        (print-obj obj)
        )))
  
  
  ;(main-obj)

  )
