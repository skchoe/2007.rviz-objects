(module tree-node mzscheme
  (require (lib "mred.ss" "mred"))
  
  (provide 
   (struct node (name view-group location orientation list-of-nodes))
   (struct leaf-node (list-of-fishes))
   (struct fish (kind size lake position orientation)))
  
  ;;---------------------------------------------------------
  ;; structure for node of tree - domain data side(DATA TREE)
  ; group node
  ; view-group field: boolean
  (define-struct node (name           ; identifier
                       view-group     ; boolean to view in one group in viz
                       location       ; relative position to connected node
                       orientation    ; relative oriention        "
                       list-of-nodes)); list of both group-nodes and leaf-nodes
  ; leaf node
  (define-struct (leaf-node node) (list-of-fishes))
  
  ;; structure of fish
  (define-struct fish (kind
                       size
                       lake
                       position
                       orientation))


)
