(module scene-node mzscheme
  (require (lib "mred.ss" "mred")
           "../render/primitives.scm"
           "../math/def.scm")
  

  (provide verify-scene-node)  
  (provide 
   (struct scene-node (name 
                       pick-id
                       transform 
                       material 
                       view-mode
                       bounding-box
                       list-of-scene-nodes))
   (struct scene-leaf-node (list-of-renderers))
   )
  
  
  
  
  
  ;;-----------------------------------------------------
  ;; structure for node of tree - 3d viz side(SCENE TREE Node)
  ;                             color-fish            
  ;             sn                :root
  ;           /  |  \
  ;         sn  sn  sn            :fish-group
  ;        /    /  \   \
  ;      r1   sln  sn  sn         :fish 
  ;          /    /  \   \
  ;        r2   sln sln   r3      :parts
  ;              |   | \
  ;             r4  r5 r6         :renderers
    
  
  ; group node
  (define-struct scene-node (name
                             pick-id
                             transform 
                             material 
                             view-mode
                             bounding-box
                             list-of-scene-nodes)) ; pair form
  ; leaf node
  (define-struct (scene-leaf-node scene-node) (list-of-renderers))
  
  
  (define verify-scene-node 
    (lambda (node)
      (printf "_____verify_scene_node_____\n")
      (if (scene-node? node)
          (begin
            (let* ((n (scene-node-name node))
                   (id (scene-node-pick-id node))
                   (ln (scene-node-list-of-scene-nodes node)))
              (printf "VERIFY ~s \t" n)
              (if (null? ln)
                  ()
                  (begin
                    (verify-scene-node (car ln))
                    (verify-scene-node-list (cdr ln))))))
          (printf "NOT scene-node form\n"))))
  
  (define verify-scene-node-list 
    (lambda (node-list)
      (if (null? node-list)
          ()
          (begin 
            (verify-scene-node (car node-list))
            (verify-scene-node-list (cdr node-list))))))
  

)