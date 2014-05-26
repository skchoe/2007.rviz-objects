(module viz-class mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           sgl
           sgl/gl
           sgl/gl-vectors
           scheme/bool
           "../render/pick.scm"
           "../render/primitives.scm"
           "../math/def.scm"
           "../math/calc.scm"
           "../structure/scene-node.scm"
           "../operation/scene-node-ops.scm"
          )


  (provide viz-class%)
  
  ;; root node of scene tree
  (define scene-tree-root null)


  ;; scene-tree-bounding-box
  (define scene-bounding-box null)

  
  ;; hash-table
  (define hash-table-empty
    (lambda (ht)
      (hash-table-for-each ht (lambda (k v);(hash-table-get ht k)) 
                                (hash-table-remove! ht k)))))



;; canvas%
(define viz-class%
  (class canvas% ()
    
    ;; from canvas%
    (inherit refresh with-gl-context swap-gl-buffers get-parent)

    ;; init-super
    (super-instantiate () (style '(gl no-autoclear)))

    ;; Viewing Parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (define eye_z 100)

    (define mouse-px 0) 
    (define mouse-py 0)
    (define mouse-cx 0) 
    (define mouse-cy 0)
    (define pick-mode-on #f)
    (define selected-scene-node-list empty) ; list form (car, cdr, cons)

    ;; Action on each mouse events
    (define/override (on-event e)
      (let* ((m-down  (send e button-down?))
             (m-up    (send e button-up?))
             (m-drag  (send e dragging?))
             (km-down (send e get-meta-down))
;                      (case (system-type)
;                        [(windows) (send e get-control-down)]
;                        [(macosx) (send e get-meta-down)]
;                        [else (send e get-control-down)]))
             (ks-down (send e get-shift-down)))
        
        (when m-down
          (set! mouse-px (send e get-x))
          (set! mouse-py (send e get-y))

          (if km-down ;; picking 
                      ;; -> define rotation center and object to be rotated
              (begin 
                ;(printf "--------dx: ~s, dy: ~s\n" mouse-px mouse-py)
                (set! pick-mode-on #t)
                (refresh)
                )))
        
        (when (and m-up (or (not km-down)
                            (not ks-down)))
          (set! selected-scene-node-list empty))
        
        ; rotation===================
        ; drag with left mouse button
        (when (and m-drag (send e get-left-down) (not ks-down))
          (set! mouse-cx (send e get-x))
          (set! mouse-cy (send e get-y))
          (let* ((dy (- mouse-px mouse-cx))
                 (dx (- mouse-py mouse-cy)))


            (set! mouse-px mouse-cx)
            (set! mouse-py mouse-cy)
            
            ;; ctrl dwn? => (pick object, rotate it) (rotate whole scene)
            (if km-down; 
                
                (begin ;; picking and rotation of picked scene-node(s)

                  (unless (empty? selected-scene-node-list)
                    ;; for now, just single(not multiple) selection allowed.
                    (let* (
                           (old-target-node (list-ref selected-scene-node-list 0))
                           (rotation-center (scene-node->center old-target-node))
                           (event-xform (rotation->transform 
                                         (if (null? rotation-center)
                                             (make-point3 0.0 0.0 0.0)
                                             rotation-center)
                                         (* 0.01 dx)
                                         (* 0.01 dy)
                                         0.0))
                           (new-xform (scene-node->new-transform-by-rotation
                                       old-target-node event-xform))
                           (new-target-node (new-transform->scene-node
                                             new-xform
                                             old-target-node))
                           (path (search-scene-node scene-tree-root old-target-node))
                           )
                      ; scene-tree-root (replace by path info)
                      (set! scene-tree-root
                            (in-path-new-node->root-scene-node
                                   scene-tree-root
                                   path
                                   new-target-node
                                   pick-id-table))
                      
                      ;____________________________________________________________
                      ;; env update 1                      
                      ; selected-scene-node-list
                      (set! selected-scene-node-list (list new-target-node))

                      
                      ;; env update 2
                      ; pick-id-table
                      (let* ((val (hash-table-get pick-id-table 
                                                  (scene-node-pick-id new-target-node))))
                        (unless (null? val)
                            (begin
                              (hash-table-remove! pick-id-table 
                                                  (scene-node-pick-id new-target-node))
                              (hash-table-put! pick-id-table 
                                               (scene-node-pick-id new-target-node)
                                               new-target-node))))
                      )))

                (begin ;; rotation of top object (whole object)
                  (let* (
                         (children-nodes (scene-node-list-of-scene-nodes
                                          scene-tree-root))
                         (object-root (car children-nodes))
                         (rotation-center (scene-node->center object-root))
                         (event-xform (rotation->transform 
                                       (if (null? rotation-center)
                                           (make-point3 0 0 0)
                                           rotation-center)
                                       (* 0.01 dx)
                                       (* 0.01 dy)
                                       0.0))
                         (new-xform (scene-node->new-transform-by-rotation
                                     object-root event-xform))
                         
                         (new-object-root
                          (new-transform->scene-node new-xform object-root))
                         
                         (path (search-scene-node scene-tree-root object-root))
                         
                         )

                    ;scene-tree-root
                    (set! scene-tree-root
                          (in-path-new-node->root-scene-node scene-tree-root
                                                             path
                                                             new-object-root
                                                             pick-id-table))
                    )))

            (refresh)
            ))
        
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
            (if km-down
                (begin
                  ;; loop for selected-scene-node-list
                  (unless (empty? selected-scene-node-list)
                    ; for testing one element picked
                    ; TODO need processing multiple selection
                    (let* (
                           (old-target-node (list-ref selected-scene-node-list 0))
                           (event-xform (translation->transform 
                                         (* -0.05 dx) (* 0.05 dy) 0))
                           (new-xform (scene-node->new-transform-by-translation 
                                       old-target-node event-xform))
                           (new-target-node (new-transform->scene-node 
                                             new-xform 
                                             old-target-node))
                           (path (search-scene-node scene-tree-root old-target-node))
                           )

;                      (printf "root-target = ~s\t old-target = ~s\t new-target = ~s\n" 
;                              (eq-hash-code scene-tree-root)
;                              (eq-hash-code old-target-node)
;                              (eq-hash-code new-target-node))
                      
                      ;; update scene-tree-root
                      (set! scene-tree-root 
                            (in-path-new-node->root-scene-node scene-tree-root
                                                             path
                                                             new-target-node
                                                             pick-id-table))

                      ;; ______________________________________________________
                      ;; env update 1
                      ;; update selected-scene-node-list
                      (set! selected-scene-node-list (list new-target-node))
;                            (append (list new-target-node) 
;                                    (remq old-target-node selected-scene-node-list)))


                      ;; env update 2
                      ;; update pick-id-table (hashtablee (pick-id scene-node))
                      (hash-table-remove! pick-id-table 
                                          (scene-node-pick-id new-target-node))
                      (hash-table-put! pick-id-table 
                                       (scene-node-pick-id new-target-node)
                                       new-target-node)
                                            
                      )))

                (begin
                  (let* (
                         (children-nodes (scene-node-list-of-scene-nodes
                                          scene-tree-root))
                         (object-root (car children-nodes))
                         (event-xform (translation->transform 
                                       (* -0.05 dx) (* 0.05 dy) 0))
                         (new-xform (scene-node->new-transform-by-translation 
                                     object-root event-xform))                         
                         
                         (new-obj-root (new-transform->scene-node
                                        new-xform
                                        object-root))
                         
                         (path (search-scene-node scene-tree-root
                                                  object-root))                         
                         )
            
                    (set! scene-tree-root 
                          (in-path-new-node->root-scene-node scene-tree-root
                                                           path
                                                           new-obj-root
                                                           pick-id-table))
                    )))

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
            (if km-down
                (begin
                  ;; loop for selected-scene-node-list
                  (unless (empty? selected-scene-node-list)
                    ; for testing one element picked
                    ; TODO need processin multiple selection
                    (let* ((old-target-node (list-ref selected-scene-node-list 0))
                           (object-center (scene-node->center old-target-node))
                           (direction
                            (point3-normalize 
                             (point3-diff object-center 
                                          (make-point3 eye_x eye_y eye_z)) (* 0.1 dy)))
                           (event-xform (translation->transform (point3-x direction)
                                                                (point3-y direction)
                                                                (point3-z direction)))
                           (new-xform (scene-node->new-transform-by-translation 
                                       old-target-node event-xform))
                           (new-target-node (new-transform->scene-node 
                                             new-xform 
                                             old-target-node))
                           (path (search-scene-node scene-tree-root old-target-node))
                           )
                      
                      ;; update scene-tree-root
                      (set! scene-tree-root 
                            (in-path-new-node->root-scene-node scene-tree-root
                                                             path
                                                             new-target-node
                                                             pick-id-table))

                      ;; __________________________________________________________________
                      ;; env update 1
                      ;; update selected-scene-node-list
                      (set! selected-scene-node-list (list new-target-node))
;                            (append (list new-target-node) 
;                                    (remq old-target-node selected-scene-node-list)))

                      ;; env update 2
                      ;; update pick-id-table (hashtablee (pick-id scene-node))
                      (hash-table-remove! pick-id-table (scene-node-pick-id
                                                         new-target-node))
                      (hash-table-put! pick-id-table 
                                       (scene-node-pick-id new-target-node) 
                                       new-target-node)
                      
                  )))

                (begin
                  (let* (
                         (children-nodes (scene-node-list-of-scene-nodes
                                          scene-tree-root))
                         (object-root (car children-nodes))
                         (object-center (scene-node->center object-root))
                         (direction
                          (point3-normalize 
                           (point3-diff object-center 
                                        (make-point3 eye_x eye_y eye_z)) (* 0.1 dy)))
                         (event-xform (translation->transform (point3-x direction)
                                                              (point3-y direction)
                                                              (point3-z direction)))
                         (new-xform (scene-node->new-transform-by-translation 
                                     object-root event-xform))
                         (new-obj-root (new-transform->scene-node
                                        new-xform
                                        object-root))
                         (path (search-scene-node scene-tree-root 
                                                  object-root))                         
                         )
            
                    (set! scene-tree-root 
                          (in-path-new-node->root-scene-node scene-tree-root
                                                           path
                                                           new-obj-root
                                                           pick-id-table)))))

            (refresh)
        ))
        
        ))

        

    ; scene-tree-root -> scene-tree-root
    ; case 1: global xform -> (list scene-tree-root w/ xform) -> scene-tree-root
    ; case 2: local xform -> (list scene-node(s) w/xform(s)) -> scene-tree-root
    ; ex: (update-scene-transform (list sn1 sn2 ...))
    (define update-scene-transform 
      (lambda (root-node new-sn-list)
        (list 1 2 3)))
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda()
         (set! aspect (/ width height))
         (gl-viewport 0 0 width height)))
       (refresh))

    ; frustum constuction
    (define on-paint-frustum
      (lambda ()
        (let* (
               (left (* -1 size))
               (right size)         
               (top size)
               (bottom (* -1 size))
               )
          
          (if (> aspect 1) 
              (let* ((l (* left aspect)) (r (* right aspect)))
                (if is_perspective
                    (gl-frustum l r bottom top near far)
                    (gl-ortho (* left 10.0) (* right 10.0) 
                              (* bottom 10.0) (* top 10.0) near far)))
              (let* ((t (/ top aspect)) (b (/ bottom aspect)))
                (if is_perspective
                    (gl-frustum left right b t near far)
                    (gl-ortho (* left 10.0) (* right 10.0) 
                              (* top 10.0) (* bottom 10.0) near far)))))))
      
    
    ;; determin eye_x eye_y eye_z from bounds of the scene.
    (define scene-bounding-box->eye-position 
      (lambda (bbx)
        (print-bounding-box bbx)
        (let* ([min3 (bounding-box-min-posn3 bbx)]
               [max3 (bounding-box-max-posn3 bbx)]
               [cx (/ (+ (point3-x max3) (point3-x min3)) 2.0)]
               [cy (/ (+ (point3-y max3) (point3-y min3)) 2.0)]
               [cz (/ (+ (point3-z max3) (point3-z min3)) 2.0)]
               [w (- (point3-x max3) (point3-x min3))]
               [h (- (point3-y max3) (point3-y min3))]
               [d (- (point3-z max3) (point3-z min3))]
               [mx (max w h)])
          (printf "mx dim = ~s\n" mx)
          (set! eye_x cx)
          (set! eye_y cy)
          (set! eye_z (+ (* (/ mx 2.0) 3.33) cz d 30)))
        ))
    
    
    ;; on-paint: call displaylist if they exist, 
    ;; else construct it first.
    (define/override (on-paint)
      (with-gl-context
       (lambda () 

         ;; apply scene bounds to eye position
         (scene-bounding-box->eye-position scene-bounding-box)
         
         ;;____________P_I_C_K______________
         (if pick-mode-on
             (begin
               (set! pick-mode-on #f)
               (pick-on-paint mouse-px mouse-py 0.5 0.5 0 0)))

         ;;__________R_E_N_D_E_R____________
         ;; env setup
         (gl-clear-color 0.9 0.9 0.9 1.0)
         (gl-clear 'color-buffer-bit 'depth-buffer-bit)
         (gl-enable 'depth-test)
         (gl-enable 'cull-face)
         
         ;; projection matrix setup
         (gl-matrix-mode 'projection)
         (gl-load-identity)

         ;; frustum
         (on-paint-frustum)
           
         ;; modelview matrix setup
         (gl-matrix-mode 'modelview)
         (gl-load-identity)
         
         ;; eye, light
         (let* ((look_x 0) (look_y 0) (look_z 0)
                (up_x 0) (up_y 1) (up_z 0))
           
           ;; gluLookAt()
           (gl-look-at eye_x eye_y eye_z 
                       look_x look_y look_z 
                       up_x up_y up_z)
           
           ;; light settings need to be moved to initialization routine.
           ;; light0
           (gl-light-v 'light0 'position 
                       (vector->gl-float-vector
                        (vector eye_x eye_y eye_z 0.0)))
           (gl-enable 'lighting)
           (gl-enable 'light0)
           ;; light1
           (gl-light-v 'light1 'position 
                       (vector->gl-float-vector
                        (vector eye_x eye_y (* -1 eye_z) 0.0)))
           (gl-enable 'light1))
         
         ;;______________________________________________________
         ;; render all objects defined in renderer-list (member)
         (render-all-objects)

         ;; buffering/flush
         (swap-gl-buffers)
         (gl-flush))))
    
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
                             (+ py offy))
                          w h vp)

          ;; make-frustum
          (on-paint-frustum)
          
          ;; modelview matrix setup
          (gl-matrix-mode 'modelview)
          (gl-load-identity)
          
          ;; eye, light
          (let* ((look_x 0) (look_y 0) (look_z 0)
                 (up_x 0) (up_y 1) (up_z 0))
            
            ;; gluLookAt()
            (gl-look-at eye_x eye_y eye_z 
                        look_x look_y look_z 
                        up_x up_y up_z))
           
 
          ;;______________________________________________________
          ;; pick all objects defined in renderer-list (member)
          (pick-all-objects)

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
;            (let* ((svec (gl-vector->vector selected-uint-list)))
;              (printf "s-b ~s \t ~s ~s ~s\n ~s ~s ~s ~s\n " 
;                      (vector-ref svec 0) (vector-ref svec 1) (vector-ref svec 2)
;                      (vector-ref svec 3) (vector-ref svec 4) (vector-ref svec 5)
;                      (vector-ref svec 6) (vector-ref svec 7)))

            (let ((picked-id (look-into-selection-buffer results)))
              (if (null? picked-id) 
                  (printf "selection buffer don't include given pick-id ~s\n" picked-id)
                  (begin ;; obtain scene-node from pick-id-table
;                    (printf "picked-id = ~s\n" picked-id)
;                    (hash-table-for-each pick-id-table 
;                                         (lambda (k v) (printf "k = ~s , v's name = ~s\n" 
;                                                               k (scene-node-name v))))
                    (let* ((s-node (hash-table-get pick-id-table picked-id null)))

                      (if (null? s-node)
                          (printf "pick-id table doesn't have s-node w/ pick-id\n")
                          (begin
;                            (printf "~s is selected" (scene-node-name s-node))
                            ; update selected-scene-node-list
                            (set! selected-scene-node-list (list s-node))
;                                  (add-remove-group selected-scene-node-list s-node))
;                            (print-selected-scene-node-list selected-scene-node-list)
                            ))
                      )))
              #f)))))

    (define print-selected-scene-node-list
      (lambda (sn-list)
        (newline)
        (if (null? sn-list) 
            (printf "selected-scene-node-list is null")
            (for-each (lambda (node)
                        (printf "current content of selected-scene-node-list= ~s \n" 
                                (scene-node-name node)))
                      sn-list))
        ))
    
    (define add-remove-group 
      (lambda (node-list node)
        ;; check if node-vector contains node
        (if (null? node-list) 
            (list node)
            (if (memq node node-list)
                (remq node node-list);remove and return
                (append (list node) node-list) ;add and return
                ))))
        
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

    ;; Render objects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; set-scene-tree-root
    (public set-scene-tree-root)
    (define (set-scene-tree-root root)
      (unless (null? root)
        (begin 
          (set! scene-tree-root root)
          (let* ((bbx (scene-node-bounding-box root)))

            (set! scene-bounding-box bbx)))))
    
    ;____________________render_____________________
    ; call render in each renderer in renderer-list
    (define (render-all-objects)
      ;(check-renderer-list renderer-list)
      (if (null? scene-tree-root)
          (display 'LIST-EMPTY)
          (begin
            (render scene-tree-root))))
    
    ; scene-node -> void
    ; traverse renderer object in scene-tree and call 
    ; /gl-mult-matrix/gl-material/gl-view-mode/render
    (define (render nd)
      (if (null? nd) 
          ()
          (begin
            (gl-push-matrix)
            (gl-mult-matrix (vector->gl-float-vector (scene-node-transform nd)))
            (if (scene-leaf-node? nd)
                  (render-list-of-renderers (scene-leaf-node-list-of-renderers nd))
                  (render-list-of-nodes (scene-node-list-of-scene-nodes nd)))
            (gl-pop-matrix)
            )))

    (define render-list-of-renderers
      (lambda (list-of-renderers)
        (cond
          [(empty? list-of-renderers) empty]
          [else (begin
                  (send (car list-of-renderers) render)
                  (render-list-of-renderers 
                   (cdr list-of-renderers)))])))

    (define render-list-of-nodes
      (lambda (list-of-nodes)
        (cond
          [(empty? list-of-nodes)]
          [else (begin
                  (render (car list-of-nodes))
                  (render-list-of-nodes (cdr list-of-nodes)))])))
    
    ;____________________pick_____________________
    (define pick-all-objects ;    (gl-pop-name)

      (lambda () 
        (pick scene-tree-root)))

    (define pickable?
      (lambda (s-node)
        (if (false? (scene-node-pick-id s-node))
            #f
            #t)))

    (define (pick nd)
      (cond 
        [(null? nd)]
        [(scene-leaf-node? nd)
         (begin (when (pickable? nd) (gl-push-name (scene-node-pick-id nd)))
                (gl-push-matrix)
                (gl-mult-matrix (vector->gl-float-vector (scene-node-transform nd)))
                (pick-list-of-renderers (scene-leaf-node-list-of-renderers nd))
                (gl-pop-matrix)
                (when (pickable? nd) (gl-pop-name)))]
        [else ; scene-node
         (begin (when (pickable? nd) (gl-push-name (scene-node-pick-id nd)))
                (gl-push-matrix)
                (gl-mult-matrix (vector->gl-float-vector (scene-node-transform nd)))
                (pick-list-of-nodes (scene-node-list-of-scene-nodes nd))
                (gl-pop-matrix)
                (when (pickable? nd) (gl-pop-name)))]))
    
    (define pick-list-of-renderers
      (lambda (list-of-renderers)
        (cond
          [(empty? list-of-renderers)]
          [else (begin
                  (send (car list-of-renderers) pick)
                  (pick-list-of-renderers 
                   (cdr list-of-renderers)))])))

    (define pick-list-of-nodes
      (lambda (list-of-nodes)
        (cond
          [(empty? list-of-nodes)]
          [else (begin
                  (pick (car list-of-nodes))
                  (pick-list-of-nodes (cdr list-of-nodes)))])))
    ))
  
  
)