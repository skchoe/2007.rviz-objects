(module flat-to-scene scheme

  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "match.ss")
           "flat-obj.scm"
           "../structure/scene-node.scm"
           "../render/primitives.scm"
           "../render/render.scm"
           "../render/triangle.scm"
           "../render/pick.scm"
           "../math/def.scm"
           "../math/calc.scm"
           "../view-env/viz-class.scm"
           "../operation/convert-utils.scm"
           "../operation/renderer-ops.scm"
           "../operation/scene-node-ops.scm")
  
  (provide generate-scene-graph-from-flat)
  
  ;; ------------------------------------------------ ;;
  ;; ---------  scene-tree w/ renderers  ------------ ;;
  
  ;; flat-list = (cons fish-group1 (cons fish-group2 .......)))
  ;;      
  ;; fish-group - name
  ;             - view-group
  ;             - location
  ;             - orientation
  ;             - fish-list: (list fish0:obj-scene-object 
  ;                                fish1:obj-scene-object 
  ;                                fish2:obj-scene-object ...)
  
  ;   obj-scene-object - position
  ;                    - rotation
  ;                    - obj-object
  
  ;;  fishK: obj-object - mtllib
  ;                     - groups (group of parts) - (pair)
  ;                     - materials (hash-table)
  
  
  ;; scene-node = 
  ;;                   root-scene-node              : pair of groups of fishes
  ;;                          |
  ;;             ----------------------------
  ;;             |                |         ...
  ;;         scene-node       scene-node            : group of fishes (fish-group)
  ;;             |
  ;;       -------------------------
  ;;       |               |
  ;;   scene-node      scene-node   ...             : each fishes (obj-scene-object: obj file)
  ;;                       |    
  ;;                ------------------------
  ;;                |            |
  ;;            leaf-node    leaf-node ...          : each parts (different material name) 
  ;;                |
  ;;          ------------------------
  ;;          |             |
  ;;      renderer1     renderer2 ...               : parts of fishes(surface)
  
  
  
 ; parent class for mixin
  (define flat-list->scene-root-node%
    (class* object% (convert-to-scene-graph<%>)
      (super-new)
      
      (define/public object->renderer 
        (lambda (points normals texture-coords faces part-mtl-param)
          (obj-part->renderer points normals texture-coords faces part-mtl-param)))
      
      (define/public convert-to-scene-graph 
        (lambda (flat-list)
          (flat-list->scene-root-node flat-list)))))
     
  
  ; child mixin class
  ; input: structure of flat-list
  ; use global(in this module) for conversion: flat-list->scene-root-node%
  (define generate-scene-graph-from-flat
      (lambda (stc)
      (object-mixin->scene-graph 
       (convert-to-scene-graph-mixin flat-list->scene-root-node%)
       stc)))
  
  
  
  ;; helpers
  ;; fish-list -> root-node
  (define flat-list->scene-root-node
    (lambda (fl) ; (pair of fish-group)
      (if (not (empty? fl))
          (begin
            (let ((root (initialize-scene-node
                         (cons (fish-type->scene-node (car fl) null)
                               (fish-list->scene-node-list (cdr fl) null))
                         #:n (string->symbol "Fish-Set")
                         )))
              (printf "** constructing scene-graph completes\n")
              root))
          (begin
            (printf "__________fish-list is empty\n")
            null))))
  
  (define get-mtl-param-in-ht 
    (lambda (mat-table name)
      (hash-ref mat-table name default-mtl-params)))
    
  (define fish-part->scene-leaf-node 
    (lambda (vertices part mat-hash-table)
      (let* ((name     (obj-part-name part))
             (points   (obj-vertices-points vertices))
             (normals  (obj-vertices-normals vertices))
             (texture-coords (obj-vertices-texture-coords vertices))
             (mat-name (obj-part-material part))
             (faces    (obj-part-faces part))
             (mtl-param (get-mtl-param-in-ht mat-hash-table mat-name))
             (surface (obj-part->renderer 
                       points normals texture-coords faces mtl-param)))

        (initialize-scene-leaf-node 
         (cons surface empty)
         #:n (if (string? name) (string->symbol name) name)))))


  (define fish-part-list->scene-node-list
    (lambda (vertices parts table)
      (if (empty? parts)
          empty
          (cons (fish-part->scene-leaf-node vertices (car parts) table)
                (fish-part-list->scene-node-list vertices (cdr parts) table)))))
            
  ;; obj-scene-object  -> obj-object -> scene-node 
  (define obj-scene-object->scene-node
    (lambda (f-s-object table) ;; table will be ignored and mat-table will be taken.
      (let* (
             (position (obj-scene-object-position f-s-object))
             (orientation (obj-scene-object-orientation f-s-object))
             (f-object (obj-scene-object-obj-obj f-s-object))
             
             (mtllib (obj-object-mtllib f-object))
             (vertices (obj-object-vertices f-object))
             (parts (obj-object-parts f-object))
             (mat-table (obj-object-materials f-object))
             
             (xform (pos_orient->transform position orientation))
             (children (if (empty? parts)
                            empty
                            (cons (fish-part->scene-leaf-node vertices (car parts) mat-table)
                                  (fish-part-list->scene-node-list vertices (cdr parts) mat-table))))
             )
        
        (initialize-scene-node children
                               #:n (if (string? mtllib) (string->symbol mtllib) mtllib)
                               #:p (generate-pick-id pick-id-table)
                               #:x xform
                               ))))
  
  ;; input: fish-group, obj-scene-object, 
  ;; output: scene-node
  (define fish-type->scene-node
    (lambda (f-type mat-table)
      (cond
        [(fish-group? f-type) (fish-group->scene-node f-type mat-table)]
        [(obj-scene-object? f-type) (obj-scene-object->scene-node f-type mat-table)]
        [else #f])))
  
  ;; flat-list->scene-node-list
  (define fish-list->scene-node-list
    (lambda (f-list table)
      (if (empty? f-list)
          empty
          (cons (fish-type->scene-node (car f-list) table)
                (fish-list->scene-node-list (cdr f-list) table)))))
  
           
  (define fish-group->scene-node 
    (lambda (f-group mat-table)
      (let* ((name (fish-group-name f-group))
             (group? (fish-group-view-group f-group))
             (loc (fish-group-location f-group))
             (ornt (fish-group-orientation f-group))
             (f-list (fish-group-fish-list f-group)))
;    (match-lambda ((make-fish-group name group? loc ornt f-list)
;                   mat-table)
             
        (initialize-scene-node
         (if (empty? f-list)
             empty
             (cons (fish-type->scene-node (car f-list) mat-table)
                   (fish-list->scene-node-list (cdr f-list) mat-table)))
         #:n name
         #:p (if group? (generate-pick-id pick-id-table) #f)
         #:x (pos_orient->transform loc ornt)
         )
        )))
          

  (define mtl-params->scene-material
    (lambda (mtl-p)
      (let* ((Ka (mtl-params-Ka mtl-p))
             (Kd (mtl-params-Kd mtl-p))
             (Ks (mtl-params-Ks mtl-p))
             (alpha (mtl-params-d mtl-p))
             (shininess (mtl-params-Ns mtl-p))
             (tex-obj (mtl-params-map_Kd mtl-p)))
        (make-material (make-point4 (point3-x Ka) (point3-y Ka) (point3-z Ka) alpha)
                       (make-point4 (point3-x Kd) (point3-y Kd) (point3-z Kd) alpha)
                       (make-point4 (point3-x Ks) (point3-y Ks) (point3-z Ks) alpha)
                       shininess
                       tex-obj))))
  
  ;; part(group) in obj-file->renderer
  ;; parts -> surface -> surface-renderer w/ calls
  ;; part-mtl-param: mtl-param
  (define obj-part->renderer 
    (lambda (points normals texture-coords faces part-mtl-param)
      ;; 1. defining surface primitive
      (let* ((arg (make-arg-set-obj empty points normals texture-coords))
             (surf-prim (make-surface (obj-part->list-triangle faces arg))))
        
        ;; 2. call methods of renderer 
        (if (not (null? surf-prim))
            (initialize-renderer surf-prim
                                 (mtl-params->scene-material part-mtl-param)
                                 (default-transform))
            null))))
  
  (define-struct arg-set-obj (lst ps ns ts))
  
  ;; f-idx: obj-face
  ;; arg: arg-set-obj
  (define face-arg->triangle 
    (lambda (f-idx arg)
      (let* ((ps (arg-set-obj-ps arg))
             (ns (arg-set-obj-ns arg))
             (ts (arg-set-obj-ts arg))
             (lst (arg-set-obj-lst arg)))
        (make-arg-set-obj
         (cons (face->triangle f-idx ps ns ts)  ; triangle
               lst) 
         ps 
         ns 
         ts))))

  (define face->triangle
    (lambda (f-obj ps ns ts)
      (let* ((pi (obj-face-point f-obj)) ; :point3, 3-indices
             (ni (obj-face-normal f-obj)) ; :point3, 3-indices
             (ti (obj-face-texture-coord f-obj))) ; :point3, 3-indices

;        (printf "pi = ~s ~s ~s\n" (point3-x pi) (point3-y pi) (point3-z pi))
;        (printf "ni = ~s ~s ~s\n" (point3-x ni) (point3-y ni) (point3-z ni))
;        (printf "ti = ~s ~s ~s\n" (point3-x ti) (point3-y ti) (point3-z ti))
;
;        (printf "size:points = ~s\n" (length ps))
;        (printf "size:normal = ~s\n" (length ns))
;        (printf "size:tex-crd = ~s\n" (length ts))
  
        (let* ((p0 (list-ref ps (point3-x pi))) ; :point3, (x0,y0,z0)
               (p1 (list-ref ps (point3-y pi))) ; :point3, (x1,y1,z1)
               (p2 (list-ref ps (point3-z pi))) ; :point3, (x2,y2,z2)
               
               (n0 (list-ref ns (point3-x ni)))
               (n1 (list-ref ns (point3-y ni)))
               (n2 (list-ref ns (point3-z ni)))
               
               (t0 (list-ref ts (point3-x ti))) ; :point2, (s0,t0)
               (t1 (list-ref ts (point3-y ti))) ; :point2, (s1,t1)
               (t2 (list-ref ts (point3-z ti)))) ; :point2, (s2,t2)
        
          (make-triangle (make-vertex p0 n0 t0 null)
                         (make-vertex p1 n1 t1 null)
                         (make-vertex p2 n2 t2 null))))))
                 
  (define obj-part->list-triangle
    (lambda (faces arg)
      (let* ((arg (foldl face-arg->triangle arg faces)))
        (arg-set-obj-lst arg))))
  
  ;--------------------------------
  ;input key, value of mtl-hash-table
  (define print-mtl-param-this-file
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
        (print-point3 Ka)
        (print-point3 Kd)
        (print-point3 Ks)
        (printf "map_Kd = ~s\n" map_Kd)))))