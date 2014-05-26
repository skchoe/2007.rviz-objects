(module init-flat mzscheme
  (require (lib "mred.ss" "mred")
           (lib "list.ss")
           (lib "class.ss")
           (lib "graphics.ss" "graphics")
           (lib "etc.ss")
           "../src/math/def.scm"
           "../src/math/calc.scm"
           "../src/render/triangle.scm"
           "../src/render/primitives.scm"
           "../src/io/flat-obj.scm"
           "../src/io/obj-read.scm"
           "../src/io/file-channel.scm"
           "../src/io/mtl-read.scm")
  
  (provide (all-defined))
  
  (define construct-fish-set-flat
    (lambda ()
      (let* (
             (filename0 "data/GuppyBlueGrass0.obj")
             (filename1 "data/delfin_low.obj")
             (filename2 "data/LeopardShark0.obj")
             (filename3 "data/Kumanomi0.obj")
             (filename4 "data/AngelShark0.obj")

             (range-value0 3.0)
             (range-value1 7.0)
             (group-range-1-max range-value0)
             (group-range-1-min (* -1 range-value0))
             (x-1-range (make-point2 group-range-1-min group-range-1-max))
             (y-1-range (make-point2 group-range-1-min group-range-1-max))
             (z-1-range (make-point2 group-range-1-min group-range-1-max))

             (group-range-2-max range-value1)
             (group-range-2-min (* -1 range-value1))
             (x-2-range (make-point2 group-range-2-min group-range-2-max))
             (y-2-range (make-point2 group-range-2-min group-range-2-max))
             (z-2-range (make-point2 group-range-2-min group-range-2-max))

             (num-fish0 10);7)
             (num-fish1 7);3)
             (num-fish2 10)
             (num-fish3 10)
             (num-fish4 5)
             
             
             ;; send read request all 5 kinds
             ;; receive file content all at once 
             
             (fish0 (if (zero? num-fish0) null (read-a-fish filename0)))
             (fish1 (if (zero? num-fish1) null (read-a-fish filename1)))
             (fish2 (if (zero? num-fish2) null (read-a-fish filename2)))
             (fish3 (if (zero? num-fish3) null (read-a-fish filename3)))
             (fish4 (if (zero? num-fish4) null (read-a-fish filename4)))
             
             (fish-scaled0 (if (null? fish0) null (obj-scale fish0 20)))
             (fish-scaled1 (if (null? fish1) null (obj-scale fish1 10)))
             (fish-scaled2 (if (null? fish2) null (obj-scale fish2 40)))
             (fish-scaled3 (if (null? fish3) null (obj-scale fish3 10)))
             (fish-scaled4 (if (null? fish4) null (obj-scale fish4 30)))
             
             (fish0s (if (null? fish-scaled0)
                         empty
                         (construct-obj-scene-fish-list-w-random-position-in-range
                          fish-scaled0
                          num-fish0
                          (make-point3 0.0 0.0 0.0) ; uniform orientations
                          x-1-range y-1-range z-1-range   ; separate positions
                          )))
             (fish1s (if (null? fish-scaled1)
                         empty
                         (construct-obj-scene-fish-list-w-random-position-in-range
                          fish-scaled1
                          num-fish1
                          (make-point3 0.0 0.0 0.0) ; uniform orientations
                          x-2-range y-2-range z-2-range   ; separate positions
                          )))
             (fish2s (if (null? fish-scaled2)
                         empty
                         (construct-obj-scene-fish-list-w-random-position-in-range
                          fish-scaled2
                          num-fish2
                          (make-point3 0.0 0.0 0.0) ; uniform orientations
                          x-2-range y-2-range z-2-range   ; separate positions
                          )))
             (fish3s (if (null? fish-scaled3) 
                         empty
                         (construct-obj-scene-fish-list-w-random-position-in-range
                          fish-scaled3
                          num-fish3
                          (make-point3 0.0 0.0 0.0) ; uniform orientations
                          x-1-range y-1-range z-1-range   ; separate positions
                          )))
             (fish4s (if (null? fish-scaled4)
                         empty
                         (construct-obj-scene-fish-list-w-random-position-in-range
                          fish-scaled4
                          num-fish4
                          (make-point3 0.0 0.0 0.0) ; uniform orientations
                          x-2-range y-2-range z-2-range   ; separate positions
                          )))

             (fg0 (if (empty? fish0s)
                      null
                      (make-fish-group (string->symbol filename0)
                                       #f
                                       (make-point3 0.0 5.0 0.0)  ;position
                                       (make-point3 1.57 0.0 0.0) ;(make-point3 -1.57 0.0 1.57); orientation
                                       fish0s)))
             
             (fg1 (if (empty? fish1s)
                      null
                      (make-fish-group (string->symbol filename1)
                                       #f
                                       (make-point3 0.0 -2.0 0.0)
                                       (make-point3 -1.57 0.0 -1.57)
                                       fish1s)))
             
             (fg2 (if (empty? fish2s)
                      null
                      (make-fish-group (string->symbol filename2)
                                       #f
                                       (make-point3 -3.0 -10.0 0.0)
                                       (make-point3 0.0 0.0 0.0)
                                       fish2s)))
             
             (fg3 (if (empty? fish3s)
                      null
                      (make-fish-group (string->symbol filename3)
                                       #f
                                       (make-point3 3.0 -10.0 0.0)
                                       (make-point3 0.0 0.0 0.0)
                                       fish3s)))
             
             (fg4 (if (empty? fish4s)
                      null
                      (make-fish-group (string->symbol filename4)
                                       #f
                                       (make-point3 -3.0 10.0 0.0)
                                       (make-point3 0.0 0.0 0.0)
                                       fish4s)))
             
             (fish-set (append (list-form fg0) (list-form fg1) 
                               (list-form fg2) (list-form fg3) (list-form fg4)))

             )
        
        (for-each 
         (lambda (fg) (printf "fish-group ~s initalized\n" (fish-group-name fg))) 
         fish-set)
        
        (printf "** data loading completes\n")
        
        fish-set)))

  
  (define list-form
    (lambda (g)
      (if (null? g) 
          empty
          (list g))))
  
          
  
  (define read-a-fish-size
    (lambda (filename size)
             ;; file reading
      (let* ((obj (obj-read-thread filename))
             ;; setting center and scaling
             (obj-centered-scaled (obj-set-center-scale 
                                   obj
                                   (make-point3 0.0 0.0 0.0) size))
             ;; normal testing
             (obj-w-normal (add-unit-normal obj-centered-scaled)))

        (printf "vertices = ~s\n" (length (obj-vertices-points (obj-object-vertices obj))))
        
        obj-w-normal)))

  ;; read file and position to center
  (define read-a-fish
    (lambda (filename)
             ;; file reading
      (let* ((obj (read-file-obj filename))
             ;; setting center to be origin
             (obj-centered (if (null? obj) null (obj-move-to obj (make-point3 0.0 0.0 0.0))))
             ;; normal testing
             (obj-w-normal (if (null? obj-centered) null (add-unit-normal obj-centered))))

        (printf "vertices = ~s\n" (length (obj-vertices-points (obj-object-vertices obj))))
        
        obj-w-normal)))
              

  ;; random position, 
  ;; given orientation(rotation)
  ;; filename: obj file name: string
  ;; num: size of fish-list
  ;; size: fish size:number
  ;; orientation: rotation factor for each coordinate: point3
  ;; in-x in-y in-z:point2 interval for range of x y z position, respectively.
  (define construct-obj-scene-fish-list-w-random-position-in-range
    (lambda (fishobj num orientation in-x in-y in-z)
      (if (null? fishobj) 
          empty
          (if (zero? num) 
              null
              (build-list num 
                          (lambda (x) 
                            (let* ((position 
                                    (make-point3 (random-in-range (point2-s in-x) (point2-t in-x))
                                                 (random-in-range (point2-s in-y) (point2-t in-y))
                                                 (random-in-range (point2-s in-z) (point2-t in-z)))))
                              (make-obj-scene-object fishobj
                                                     position
                                                     orientation))))))))

  
  ; any random number in [min max]
  (define random-in-range
    (lambda (min max)
      (+ (* (random) (- max min) min))))
  
  
  ;; obj-object, size -> scale factor
  (define get-scale-factor 
    (lambda (obj size)
      (let* ((dim (obj-object-dimension obj))
             (org-size (dimension->size dim))
             (center (dimension->center dim)))
      (/ size (max (point3-x org-size) (point3-y org-size) (point3-z org-size))))))

  (define obj-move-to
    (lambda (obj center)
        (obj-translate obj (point3-comp-mult (obj-distance-to obj center) (make-point3 -1 -1 -1)))))

  ;; x y z distance from cener to target-position
  (define obj-distance-to
    (lambda (obj target-pos)
      (let* ((dim (obj-object-dimension obj))
             (cc (dimension->center dim))
             (vtx (obj-object-vertices obj))
             (pts (obj-vertices-points vtx))
             (diff (point3-diff cc target-pos)))
        diff)))
      
      
  (define obj-set-center-scale
    (lambda (obj center size)
      (let* ((scalef (get-scale-factor obj size))
             (dim (obj-object-dimension obj))
             (cc (dimension->center dim))
             (vtx (obj-object-vertices obj))
             (pts (obj-vertices-points vtx))
             (diff (point3-diff cc center)))
        
        (let* ((obj2 (obj-translate obj diff))
               (obj3 (obj-scale obj2 scalef)))

          obj3))))

  (define dim-translate
    (lambda (dmsn d)
      (make-dimension (point3-add (dimension-min-position dmsn) d)
                      (point3-add (dimension-max-position dmsn) d))))
  

  ;----------------------------------------------------
  ;; obj: obj-object
  ;; var: point3
  ; -> new obj-object
  (define obj-translate
    (lambda (obj var)
      (let* ((vtx (obj-object-vertices obj))
             (pts (obj-vertices-points vtx))
             (new-pts (points-add pts var))
             (new-dim (calc-obj-dimension-from-points new-pts)))
        
;        (print-point3 (list-ref pts 0))
;        (print-point3 (list-ref new-pts 0))
;                      
;        (printf "old\n")
;        (print-dimension (obj-object-dimension obj))
;        (printf "new\n")
;        (print-dimension new-dim)
        
        (make-obj-object
         new-dim
         (obj-object-mtllib obj)
         (make-obj-vertices new-pts
                            (obj-vertices-normals vtx)
                            (obj-vertices-texture-coords vtx))
         (obj-object-parts obj)
         (obj-object-materials obj)))))
  
  
  (define print-vertices
    (lambda (vertices)
      (for-each print-point3 (obj-vertices-points vertices))
      (for-each print-point3 (obj-vertices-normals vertices))
      (for-each print-point2 (obj-vertices-texture-coords vertices))))
  

  ; struct for operation on list with parameter:point3
  (define-struct wrap-opn (pt-list param))
    
  ;;
  (define point-list-add
    (lambda (pt wrap)
      (let* ((var (wrap-opn-param wrap))
             (lst (wrap-opn-pt-list wrap)))
             
        (make-wrap-opn (append lst (list (point3-add pt var))) var))))
  
  ;; list of points, point3 to add-> new list of points
  (define points-add
    (lambda (points var)
      (wrap-opn-pt-list 
       (foldl point-list-add (make-wrap-opn empty var) points))
      ))

  

  ;----------------------------------------------------
  (define obj-scale
    (lambda (obj s-ftr)
      (if (zero? s-ftr) 
          obj
          (let* (
                 (obj-origin (obj-move-to obj (make-point3 0.0 0.0 0.0)))
                 (old-dist (obj-distance-to obj (make-point3 0.0 0.0 0.0)))
                 (new-dist (point3-comp-mult old-dist (make-point3 s-ftr s-ftr s-ftr)))
                 (vtx (obj-object-vertices obj-origin))
                 (pts (obj-vertices-points vtx))
                 (new-scaled-pts (points-scale pts s-ftr))
                 (new-pts (points-add new-scaled-pts (point3-comp-mult new-dist
                                                                       (make-point3 -1 -1 -1))))
                 )
        
            (make-obj-object
             (calc-obj-dimension-from-points pts)
             (obj-object-mtllib obj)
             (make-obj-vertices new-pts
                                (obj-vertices-normals vtx)
                                (obj-vertices-texture-coords vtx))
             (obj-object-parts obj)
             (obj-object-materials obj)))
          )))

  (define points-scale
    (lambda (points var)
      (wrap-opn-pt-list
       (foldl point-list-mult (make-wrap-opn empty var) points))
      ))
  
  (define point-list-mult
    (lambda (pt wrap)
      (let* ((var (wrap-opn-param wrap))
             (lst (wrap-opn-pt-list wrap)))
             
      (make-wrap-opn (append lst 
                             (list (point3-comp-mult pt (make-point3 var var var))))
                     var))))
  
  (define dimension->size
    (lambda (dim)
      (let* ((max3 (dimension-max-position dim))
             (min3 (dimension-min-position dim)))
        (point3-abs-diff max3 min3))))
        
  (define dimension->center
    (lambda (dim)
      (let* ((max3 (dimension-max-position dim))
             (min3 (dimension-min-position dim)))
        
      (make-point3 (/ (+ (point3-x max3) (point3-x min3)) 2.0)
                   (/ (+ (point3-y max3) (point3-y min3)) 2.0)
                   (/ (+ (point3-z max3) (point3-z min3)) 2.0)))))   
  
      
  (define print-dimension 
    (lambda (dim)
      (printf "dimension = min:")
      (print-point3 (dimension-min-position dim))
      (printf "dimension = max:")
      (print-point3 (dimension-max-position dim))))
  
  (define verify-fish-set-flat 
    (lambda (fish-set)
      (printf "--------------------------------------------------------\n")
      (if (empty? fish-set) 
          (printf "-verify-fish-set-flat = fish-set is empty\n")
          (begin
            (verify-fish-group (car fish-set))
            (verify-fish-set-flat (cdr fish-set))))))
  
  (define verify-fish-group 
    (lambda (fish-grp)
      (let* ((name (fish-group-name fish-grp))
             (view-group (fish-group-view-group fish-grp))
             (location (fish-group-location fish-grp))
             (fish-list (fish-group-fish-list fish-grp)))
        (printf "fish-group(name)= ~s\n" name)
        (printf "fish-group(view-group) = ~s\n" view-group)
        (printf "fish-group(location) = ~s ~s ~s\n" (point3-x location)
                (point3-y location) (point3-z location))
        (if (empty? fish-list)
            ()
            (begin 
              (verify-obj-object (car fish-list))
              (verify-obj-object-list (cdr fish-list)))))))

  
  (define verify-obj-object-list 
    (lambda (f-list)
      (if (empty? f-list)
          ()
          (begin
            (verify-obj-object (car f-list))
            (verify-obj-object-list (cdr f-list))))))
  
  (define print-obj-list
    (lambda (obj-list)
      (if (null? obj-list) 
          ()
          (begin 
            (print-obj (car obj-list))
            (print-obj-list (cdr obj-list))))))
  
  (define verify-obj-object
    (lambda (obj-obj)
      (let* ((mtllib (obj-object-mtllib obj-obj))
             (parts (obj-object-parts obj-obj))
             (materials (obj-object-materials obj-obj)))
        (printf "obj-object(mtllib)= ~s\n" mtllib)
        (map print-part parts)
        (print-mtl materials))))
  
  )