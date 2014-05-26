(module tree-to-scene scheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "match.ss")
           "tree-node.scm"
           "../src/structure/scene-node.scm"
           "../src/render/primitives.scm"
           "../src/render/render.scm"
           "../src/render/cube.scm"
           "../src/render/cylinder.scm"
           "../src/render/disk.scm"
           "../src/render/sphere.scm"
           "../src/render/triangle.scm"
           "../src/render/pick.scm"
           "../src/math/def.scm"
           "../src/math/calc.scm"
           "../src/view-env/viz-class.scm"
           "../src/operation/convert-utils.scm"
           "../src/operation/renderer-ops.scm"
           "../src/operation/scene-node-ops.scm")
  
  (provide tree-node->scene-node%
           generate-scene-graph-from-tree
           tree-node->scene-node
           list-of-nodes->list-of-scene-nodes
           color-name->material
           list-of-fishes->list-of-renderers
           color-fish->renderer)

  
  ;; ------------------------------------------------ ;;
  ;; ---------  scene-tree w/ renderers  ----------- ;;
  ; parent class for mixin
  (define tree-node->scene-node%
    (class* object% (convert-to-scene-graph<%>)
      (super-new)
      
      (define/public object->renderer 
        (lambda (object)
          (color-fish->renderer object)))
      
      (define/public convert-to-scene-graph
        (lambda (any-node)
          (tree-node->scene-node any-node)))))
  
  ; child mixin class
  ; input: structure of tree
  ; use global(in this module) for conversion: tree-node->scene-node%
  (define generate-scene-graph-from-tree
    (lambda (stc)
      (object-mixin->scene-graph 
       (convert-to-scene-graph-mixin tree-node->scene-node%)
       stc)))
    
  ;;;;;;;
  ;; helpers 
  ;; direct conversion to scene-node struct
  (define tree-node->scene-node
    (lambda (any-node)
      (cond
        [(null? any-node) (lambda (success-k fail-k) (fail-k))]
        [(leaf-node? any-node)  ; leaf-node
         (let* ((name (node-name any-node))
                (group? (node-view-group any-node))
                (location (node-location any-node))
                (orientation (node-orientation any-node))
                (list-of-fishes (leaf-node-list-of-fishes any-node)))
                
           (let ((new-node
                  (initialize-scene-leaf-node 
                   (map color-fish->renderer list-of-fishes)
                   #:n name
                   #:p (if group? (generate-pick-id pick-id-table) #f)
                   #:x (pos_orient->transform location orientation)
                   #:m (default-material)
                   #:v (default-view-mode)
                   #:cn empty)))

             new-node))] ; leaf node
        
        [else ; node
         (let* ((name (node-name any-node))
                (group? (node-view-group any-node))
                (location (node-location any-node))
                (orientation (node-orientation any-node))
                (list-of-nodes (node-list-of-nodes any-node))
                )

           (let ((new-node (initialize-scene-node
                            (map tree-node->scene-node list-of-nodes)
                            #:n name
                            #:p (if group? (generate-pick-id pick-id-table) #f)
                            #:x (pos_orient->transform location orientation)
                            #:m (default-material)
                            #:v (default-view-mode)
                            )))

             new-node))]
        
        ))) ; node
  
  (define list-of-nodes->list-of-scene-nodes 
    (lambda (node-list)
      (cond
        [(empty? node-list) empty]
        [else (cons (tree-node->scene-node (car node-list))
                    (list-of-nodes->list-of-scene-nodes (cdr node-list)))])))
  
  (define (color-name->material c-name)
    (match c-name
      ['black  (make-material-diffuse(make-point4 0.4 0.4 0.4 1.0))]
      ['yellow (make-material-diffuse(make-point4 0.9 0.9 0.0 1.0))]
      ['purple (make-material-diffuse(make-point4 0.9 0.0 0.9 1.0))]
      ['red    (make-material-diffuse(make-point4 0.9 0.0 0.0 1.0))]
      ['green  (make-material-diffuse(make-point4 0.0 0.9 0.0 1.0))]
      ['blue   (make-material-diffuse(make-point4 0.0 0.0 0.9 1.0))]
      [_       (make-material-diffuse(make-point4 0.2 0.2 0.2 1.0))]))

        
  ;; list-of-fishes -> list-of-renderers
  (define list-of-fishes->list-of-renderers
    (lambda (list-of-fishes)
      (cond
        [(empty? list-of-fishes) empty]
        [else (cons (color-fish->renderer (car list-of-fishes))
                    (list-of-fishes->list-of-renderers
                     (cdr list-of-fishes)))])))
  
  ;; color-fish->renderer
  (define color-fish->renderer
    (lambda (fish-node)
      (let* ((kind (fish-kind fish-node))
             (size (fish-size fish-node))
             (lake (fish-lake fish-node))
             (position (fish-position fish-node))
             (orientation (fish-orientation fish-node)))
        ;;1. primitive definitions
        (let ([prim 
               (match kind
                ['black (make-cube size size size)]
                ;;yellow
                ['yellow (make-cylinder 3 3 size)]
                ;;purple
                ['purple (make-disk 0.1 size)]
                ;;red
                ['red (make-sphere (/ size 2))]
                ;;blue
                ['blue (new-triangle size)]
                ;;green
                ['green (make-cube size size size)]
                ;;else
                [_ #f])])

          ;; 2. call methods of renderer for material and transform
          (if prim
              (initialize-renderer prim
                                   (color-name->material kind)
                                   (pos_orient->transform position orientation))
              null)))))

)