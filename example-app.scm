(module example-app mzscheme
  (require (lib "list.ss")
           "src/view-env/viz-class.scm"
           "src/view-env/view-env.scm"
           "src/io/flat-to-scene.scm"
           "src/operation/lst-renderers-to-scene.scm"
           "src/ui/app-container.scm"
           "object-color/init-tree.scm"
           "object-color/tree-to-scene.scm"
           "object-objfile/init-flat.scm"
           "object-renderers/init-lst-renderers.scm")

  
  ;; data initialization, scenegraph generation
  (define initialize-scene-content
    (lambda ()
      (let* (
             ;; domain 0
             ;; building fish-set structure
             (root-node (construct-fish-set-tree))
             ;; convert to scene-node hierarchy
             (s-node-0 (generate-scene-graph-from-tree root-node))

             ;; domain 1
             (fish-list (construct-fish-set-flat))
             ;; convert to scene-node hierarchy
             (s-node-1 (generate-scene-graph-from-flat fish-list))

             ;; domain 2 : simplest (root is scene-root-node)
             (lst-renderers (construct-lst-renderers))
             ;; convert to scene-leaf-node with renderer-list
             (s-node-2 (generate-scene-graph-from-lst-renderers lst-renderers))

             ;; append scene env:backgroupnd, axes, etc: to the hierarchy
             (valid-node-list (filter (lambda (nd) (if (null? nd) #f #t)) 
                                      (list s-node-0 s-node-1 s-node-2)))
             )

        valid-node-list)))

  ;; main
  (define (main)
    (let* (;; 3d object definitions
           (content-list (initialize-scene-content))
           ;; background added to the top
           (content-top (add-scene-env content-list))
           ;; GUI controlls definitions
           (frm (frame-object "functional api for sgl")))
      
      ;(printf "Seungkeols id = ~s\n" seungkeol)
      
      ;; register the both, init view-controller, and (show)
      (run-view-controller frm 1024 768 content-top)
      ))
  
  (main))