(module lst-renderers-to-scene scheme
  (require (lib "class.ss")
           "../view-env/viz-class.scm"
           "../render/pick.scm"
           "../operation/convert-utils.scm"
           "../operation/scene-node-ops.scm")
  
  (provide lst-renderers->scene-node%
           generate-scene-graph-from-lst-renderers)

  
  ;; ------------------------------------------------ ;;
  ;; ---------  scene-tree w/ renderers  ----------- ;;
  ; parent class for mixin
  (define lst-renderers->scene-node%
    (class* object% (convert-to-scene-graph<%>)
      (super-new)
      
      (define/public object->renderer 
        (lambda (object)
          object))
      
      (define/public convert-to-scene-graph
        (lambda (lst-renderers)
          (lst-renderers->scene-node lst-renderers)))))
  
  ; child mixin class
  ; input: structure of tree
  ; use global(in this module) for conversion: tree-node->scene-node%
  (define generate-scene-graph-from-lst-renderers
    (lambda (lst-renderers)
      (object-mixin->scene-graph 
       (convert-to-scene-graph-mixin lst-renderers->scene-node%)
       lst-renderers)))
    
  ;;;;;;;
  ;; helpers 
  ;; direct conversion to scene-node struct
  (define lst-renderers->scene-node
    (lambda (lst)
      (let ((new-node
             (initialize-scene-leaf-node lst
                                         #:p (generate-pick-id pick-id-table))))
        new-node))) ; leaf node
  
)