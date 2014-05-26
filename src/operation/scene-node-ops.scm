(module scene-node-ops scheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "math.ss")
           (lib "list.ss")
           "../structure/scene-node.scm"
           "../math/def.scm"
           "../math/calc.scm"
           "../render/pick.scm"
           "../render/render.scm"           
           "../render/primitives.scm"
           )
         

  (provide old-node-new-node->list-of-scene-nodes
           old-node-new-node->root-scene-node
           in-path-new-node->list-of-scene-nodes
           in-path-new-node->root-scene-node
           search-scene-node
           new-transform->scene-node
           scene-node->new-transform-by-rotation
           scene-node->new-transform-by-translation
           print-4-4-transform
           scene-node->center)
  
  (provide/contract 
   [initialize-scene-node (->* (list?)
                               (#:n symbol?
                                #:p (or/c boolean? number?)
                                #:x vector?
                                #:m material?
                                #:v symbol?)
                               scene-node?)]
   [initialize-scene-leaf-node (->* (list?)
                                    (#:n symbol?
                                     #:p (or/c boolean? number?)
                                     #:x vector?
                                     #:m material?
                                     #:v symbol?
                                     #:cn list?)
                                    scene-leaf-node?)])
  
            
  ;; init-scene-node
  (define (initialize-scene-node 
             child-nodes 
             #:n [name 'unnamed]
             #:p [pick-id #f]; #f==unpickable (generate-pick-id pick-id-table)]
             #:x [transform (default-transform)]
             #:m [material (default-material)]
             #:v [view-mode (default-view-mode)])
      (initialize-s-node name pick-id transform material view-mode child-nodes))
    
  (define initialize-s-node
    (lambda (name pick-id transform material view-mode child-nodes)
      (let* ((lst-bbox (lst-scene-node->lst-bounding-box child-nodes))
             (bbox (bounding-box-list->bounding-box lst-bbox))
             (new-node (make-scene-node name
                                        pick-id
                                        transform
                                        material
                                        view-mode
                                        bbox
                                        child-nodes)))
        (unless (false? pick-id)
          (register-pick-id pick-id-table pick-id new-node))
        
        new-node)))  
  
  ;; init-scene-leaf-node
  (define (initialize-scene-leaf-node 
             renderers
             #:n [name 'unnamed]
             #:p [pick-id #f] ;#f==unpickable (generate-pick-id pick-id-table)]
             #:x [transform (default-transform)]
             #:m [material (default-material)]
             #:v [view-mode (default-view-mode)]
             #:cn [child-nodes empty])
      (initialize-sl-node name pick-id transform material view-mode child-nodes renderers))
  
  (define initialize-sl-node
    (lambda (name pick-id transform material view-mode child-nodes renderers)
      (let* ((lst-bbox (begin 
                         ;(printf "scene-leaf creation ~s in ~s renderers\n" name (length renderers))
                         (lst-renderer->lst-bounding-box renderers)))
             (bbox (begin 
                     ;(printf "~s bbx -> 1 bbox\n" (length lst-bbox))
                     (bounding-box-list->bounding-box lst-bbox)))
             (new-node (begin
                         ;(print-bounding-box bbox)
                         (make-scene-leaf-node name
                                               pick-id
                                               transform
                                               material
                                               view-mode
                                               bbox
                                               '()
                                               renderers))))
        (unless (false? pick-id)
          (register-pick-id pick-id-table pick-id new-node))

        new-node)))

  
  (define lst-scene-node->lst-bounding-box
    (lambda (scene-node-list)
      (map (lambda (scene-node) (scene-node-bounding-box scene-node)) scene-node-list)))
  
    
  (define lst-renderer->lst-bounding-box
    (lambda (renderer-list)
      (map (lambda (renderer) (send renderer get-bounding-box)) renderer-list)))

  
  (define bounding-box-list->bounding-box
    (lambda (bbx-list)
      (let* ((non-empty-list (filter (lambda (bbx) (if (null? bbx) #f #t)) bbx-list)))
        (if (empty? non-empty-list)
            (begin 
              ;(printf "bbx-list is empty\n")
              null)
            (begin 
              ;(printf "bbx-list is non-empty\n")
              (let* ((init-bbx (car non-empty-list))
                     (tail-bbx-list (cdr non-empty-list)))
                ;(print-bounding-box init-bbx)
                (if (empty? tail-bbx-list)
                    init-bbx
                    (foldl update-bounding-box init-bbx tail-bbx-list))))))))
  
  (define update-bounding-box 
    (lambda (in-bbx out-bbx)
      (if (not (null? in-bbx))
          (let* ((in-min (bounding-box-min-posn3 in-bbx))
                 (in-max (bounding-box-max-posn3 in-bbx))
                 (out-min (bounding-box-min-posn3 out-bbx))
                 (out-max (bounding-box-max-posn3 out-bbx)))
            (make-bounding-box (make-point3 (min (point3-x in-min) (point3-x out-min))
                                            (min (point3-y in-min) (point3-y out-min))
                                            (min (point3-z in-min) (point3-z out-min)))
                               (make-point3 (max (point3-x in-max) (point3-x out-max))
                                            (max (point3-y in-max) (point3-y out-max))
                                            (max (point3-z in-max) (point3-z out-max)))))
          out-bbx)))

  (define old-node-new-node->list-of-scene-nodes
    (lambda (list-of-scene-nodes old-node new-node pick-id-table)
      (if (empty? list-of-scene-nodes)
          empty
          (begin
            (cons (old-node-new-node->root-scene-node (car list-of-scene-nodes) 
                                                   old-node
                                                   new-node 
                                                   pick-id-table)
                  (old-node-new-node->list-of-scene-nodes (cdr list-of-scene-nodes)
                                                       old-node
                                                       new-node 
                                                       pick-id-table))
            ))))
 

  ;; root, target-node, corresponding xform -> new root
  ;; new-node is a complete substitute for old-node
  ;; call/cc
  (define old-node-new-node->root-scene-node
    (lambda (root-node old-node new-node pick-id-table)
      (if (null? root-node)
          null
          (begin
;            (printf "root-scene-node, old node name = ~s/~s : ~s/~s \n" 
;                    (eq-hash-code root-node) (scene-node-name root-node)
;                    (eq-hash-code old-node) (scene-node-name old-node))
            (if (eq? root-node old-node)
                (begin
;                  (printf "same-node found ~s\n" (scene-node-name root-node))
                  new-node ; just replace  old-node with new and return the new
                  )
                (begin ; search child node
;                  (printf "not found on root-> search from child nodeds\n")
                  (if (scene-leaf-node? root-node)
                      root-node
                      (let* 
                          ((pick-id (scene-node-pick-id root-node))
                           (new-s-node (initialize-scene-node 
                                        ;; insert brand new list-of child nodes
                                        (old-node-new-node->list-of-scene-nodes
                                         (scene-node-list-of-scene-nodes root-node)
                                         old-node
                                         new-node
                                         pick-id-table)
                                        #:n (scene-node-name root-node)
                                        #:p pick-id
                                        #:x (scene-node-transform root-node)
                                        #:m (scene-node-material root-node)
                                        #:v (scene-node-view-mode root-node))))
                        
                        (when (not (false? pick-id))
                          (hash-remove! pick-id-table pick-id)
                          (hash-set! pick-id-table pick-id new-s-node))
                        
                        new-s-node))))))))
  
  (define in-path-new-node->list-of-scene-nodes
    (lambda (list-of-scene-nodes path new-node pick-id-table)
      (cond 
        [(empty? list-of-scene-nodes) empty]
        [(empty? path) list-of-scene-nodes]
        [else
         (cons (in-path-new-node->root-scene-node (car list-of-scene-nodes) 
                                                  path
                                                  new-node 
                                                  pick-id-table)
               (in-path-new-node->list-of-scene-nodes (cdr list-of-scene-nodes)
                                                      path
                                                      new-node 
                                                      pick-id-table))])))
  
  (define in-path-new-node->root-scene-node
    (lambda (root-node path new-scene-node pick-id-table)
      (cond
        [(empty? path) root-node]
        [(not (eq? root-node (car path)))
         (begin
;           (printf "error: root isn't identical to car of path\n ~s ~s\n"
;                   (scene-node-name root-node) (scene-node-name (car path)))
           root-node)]
        [(empty? (cdr path)) new-scene-node] ; (car path) is the node to be replaced
         
        [else ; lower level
         (let* ((pick-id (scene-node-pick-id root-node))
                (new-s-node (initialize-scene-node 
                             ;; insert brand new list-of child nodes
                             (in-path-new-node->list-of-scene-nodes
                              (scene-node-list-of-scene-nodes root-node)
                              (cdr path)
                              new-scene-node
                              pick-id-table)
                             #:n (scene-node-name root-node)
                             #:p pick-id
                             #:x (scene-node-transform root-node)
                             #:m (scene-node-material root-node)
                             #:v (scene-node-view-mode root-node))))
           (when (not (false? pick-id))
             (hash-remove! pick-id-table pick-id)
             (hash-set! pick-id-table pick-id new-s-node))
           new-s-node)])))
           
   
  ;; root of a tree -> node to seek -> path to the node (list root, ... , target-node)
  (define search-scene-node
    (lambda (root-node target-node)

      ;(printf "Searching: from ~s ~s\n" (scene-node-name root-node) (scene-node-name target-node))

      (if (null? root-node)
          empty
          (begin
            ;(printf "now visiting ~s node \n" (scene-node-name root-node))
            (if (eq? root-node target-node)
                (list target-node)
                (begin
                  (if (scene-leaf-node? root-node)
                      empty
                      (begin
;                        (if (list? (scene-node-list-of-scene-nodes root-node))
;                            (for-each (lambda (nd) (printf "list elt = ~s \n" (scene-node-name nd)))
;                                  (scene-node-list-of-scene-nodes root-node))
;                            (printf " given list is not of list type\n"))
                        (let* ((node-list 
                                (search-scene-node-in-list 
                                 target-node
                                 (scene-node-list-of-scene-nodes root-node))))
                          (if (empty? node-list)
                              empty
                              (begin
                               ;(printf "found a node! under ~s node\n" (scene-node-name root-node))
                                (cons root-node node-list))))))))))))
  
  
  (define search-scene-node-in-list 
    (lambda (target-node node-list)
      (let* ((path (for/fold ([lst empty])
                             ([child-node node-list])
                             (append lst
                                     (if (null? child-node)
                                         empty
                                         (search-scene-node child-node target-node))))))

;        (for-each (lambda (nd) (printf " nd(~s) \t" (scene-node-name nd))) path) 
;        (newline)
        path)))

  ;; transform, target-node -> new-scene-node w/the transform
  (define new-transform->scene-node 
    (lambda (transform target-node) 
      (if (null? target-node)
          (begin (printf "target node is null\n")
                 null)
          (begin 
            ;(printf "target isn't null-> check scene-leaf\n")
            (if (scene-leaf-node? target-node)
              (begin
                ;(printf "new-node: scene-leaf-node\n")
                (initialize-scene-leaf-node 
                 (scene-leaf-node-list-of-renderers target-node)
                 #:n (scene-node-name target-node)
                 #:p (scene-node-pick-id target-node)
                 #:x transform ;(scene-node-transform target-node) ;(default-xform)
                 #:m (scene-node-material target-node)
                 #:v (scene-node-view-mode target-node)
                 #:cn '()
                 ))        
              (begin 
                ;(printf "new-node: scene-node\n")
                (initialize-scene-node (scene-node-list-of-scene-nodes target-node)
                                       #:n (scene-node-name target-node)
                                       #:p (scene-node-pick-id target-node)
                                       #:x transform ;(scene-node-transform target-node) ;(default-xform)
                                       #:m (scene-node-material target-node)
                                       #:v (scene-node-view-mode target-node)))
              )))))

  ; target-node -> corresonding xform by rotation
  (define scene-node->new-transform-by-rotation
    (lambda (s-node rotation)
      (let* ((xform (scene-node-transform s-node))
             (ccenter (transform->translation-3 (scene-node-transform s-node)))
             (new-rot-xform (mtx-mult-3-3 rotation xform))
             (new-xform (mtx-mult-4-4
                         (translation->transform (point3-x ccenter)
                                                 (point3-y ccenter)
                                                 (point3-z ccenter))
                         new-rot-xform)))
        new-xform)))
  
  ;; target-node -> corresonding xform by translation
  (define scene-node->new-transform-by-translation
    (lambda (s-node translation)
      (let* ((xform (scene-node-transform s-node))
             (new-xform (mtx-mult-4-4 translation xform)))
        ;(print-4-4-transform translation)
        ;(print-4-4-transform new-xform)
        new-xform)))
  
  
  (define print-4-4-transform
    (lambda (xv) ; vector of 16 elts
      (printf " ~s ~s ~s ~s\n ~s ~s ~s ~s\n ~s ~s ~s ~s\n ~s ~s ~s ~s\n "
              (vector-ref xv 0) (vector-ref xv 4) (vector-ref xv 8) (vector-ref xv 12) 
              (vector-ref xv 1) (vector-ref xv 5) (vector-ref xv 9) (vector-ref xv 13) 
              (vector-ref xv 2) (vector-ref xv 6) (vector-ref xv 10) (vector-ref xv 14) 
              (vector-ref xv 3) (vector-ref xv 7) (vector-ref xv 11) (vector-ref xv 15))))

  (define scene-node->center
    (lambda (nd)
      (if (null? nd) 
          null
          (let* ((bbx (scene-node-bounding-box nd))
                 (min-posn3 (bounding-box-min-posn3 bbx))
                 (max-posn3 (bounding-box-max-posn3 bbx))
                 (center-x (/ (+ (point3-x min-posn3) (point3-x max-posn3)) 2.0))
                 (center-y (/ (+ (point3-y min-posn3) (point3-y max-posn3)) 2.0))
                 (center-z (/ (+ (point3-z min-posn3) (point3-z max-posn3)) 2.0)))
            
            ;(print-bounding-box bbx)
            ;(printf "center of selected = ~s ~s ~s\n" center-x center-y center-z)
            (make-point3 center-x center-y center-z)))
      ))

            


  
)
