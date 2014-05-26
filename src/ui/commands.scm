(module commands mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")

           "../../object-renderers/init-lst-renderers.scm"
           "../math/def.scm"
           "../render/primitives.scm"
           "../structure/scene-node.scm"
           "../operation/lst-renderers-to-scene.scm"
           "../operation/scene-node-ops.scm"
           "app-container.scm")



  (define scene-node-list->scene-root-node 
    (lambda (node-list)
      (initialize-scene-node node-list)))

;  (define show-cube 
;    (lambda (w h d)
;      (show-geom (list 'cube w h d))))

  (define show-cube
    (lambda (w h d)
      (let* ((object-scene-node 
              (generate-scene-graph-from-lst-renderers 
               (list (gen-renderer 'cube #f (list w h d)))))
             (valid-node-list (filter (lambda (nd) (if (null? nd) #f #t))
                                      (list object-scene-node)))
             (content-top (scene-node-list->scene-root-node valid-node-list))
             (frm (frame-object "cube")))
        (run-view-controller frm 500 500 content-top))))
  
;  (show-cube 10 10 10)

;  (define show-cylinder
;    (lambda (r R h)
;      (show-geom (list 'cylinder (list r R h)))))
  (define show-cylinder
    (lambda (r R h)
      (let* ((object-scene-node 
              (generate-scene-graph-from-lst-renderers 
               (list (gen-renderer 'cylinder #f (list r R h)))))
             (valid-node-list (filter (lambda (nd) (if (null? nd) #f #t))
                                      (list object-scene-node)))
             (content-top (scene-node-list->scene-root-node valid-node-list))
             (frm (frame-object "cylinder")))
        (run-view-controller frm 500 500 content-top))))
  
;  (show-cylinder 10 10 100)
  
  ;  (define show-disk
;    (lambda (r R)
;      (show-geom (list 'disk r R))))
  (define show-disk
    (lambda (r R)
      (let* ((object-scene-node 
              (generate-scene-graph-from-lst-renderers 
               (list (gen-renderer 'disk #f (list r R)))))
             (valid-node-list (filter (lambda (nd) (if (null? nd) #f #t))
                                      (list object-scene-node)))
             (content-top (scene-node-list->scene-root-node valid-node-list))
             (frm (frame-object "disk")))
        (run-view-controller frm 500 500 content-top))))
  
;  (show-disk 5 10)
  
  (define show-line
    (lambda (vB vE)
      (let* ((object-scene-node
              (generate-scene-graph-from-lst-renderers 
               (list (gen-renderer 'line #f (list vB vE)))))
             (valid-node-list (filter (lambda (nd) (if (null? nd) #f #t))
                                      (list object-scene-node)))
             (content-top (scene-node-list->scene-root-node valid-node-list))
             (frm (frame-object "line")))
        (run-view-controller frm 500 500 content-top))))
 
;  (show-line (make-point3 10.0 0.0 0.0)
;             (make-point3 0.0 10.0 0.0))
            
  (define show-quad
    (lambda (v0 v1 v2 v3)
      (let* ((object-scene-node
              (generate-scene-graph-from-lst-renderers 
               (list (gen-renderer 'quad #f (list v0 v1 v2 v3)))))
             (valid-node-list (filter (lambda (nd) (if (null? nd) #f #t))
                                      (list object-scene-node)))
             (content-top (scene-node-list->scene-root-node valid-node-list))
             (frm (frame-object "quad")))
        (run-view-controller frm 500 500 content-top))))

;  (show-quad (build-vertex (make-point3 1 -1 0))
;             (build-vertex (make-point3 1 1 0))
;             (build-vertex (make-point3 -1 1 0))
;             (build-vertex (make-point3 -1 -1 0)))
  
  (define show-sphere
    (lambda (r)
      (let* ((object-scene-node
              (generate-scene-graph-from-lst-renderers 
               (list (gen-renderer 'sphere #f (list r)))))
             (valid-node-list (filter (lambda (nd) (if (null? nd) #f #t))
                                      (list object-scene-node)))
             (content-top (scene-node-list->scene-root-node valid-node-list))
             (frm (frame-object "sphere")))
        (run-view-controller frm 500 500 content-top))))
      
  
;  (show-sphere 10)

      
  (define show-triangle
    (lambda (v0 v1 v2)
      (let* ((object-scene-node
              (generate-scene-graph-from-lst-renderers 
               (list (gen-renderer 'triangle #f (list v0 v1 v2)))))
             (valid-node-list (filter (lambda (nd) (if (null? nd) #f #t))
                                      (list object-scene-node)))
             (content-top (scene-node-list->scene-root-node valid-node-list))
             (frm (frame-object "triangle")))
        (run-view-controller frm 500 500 content-top))))
  
  (show-triangle (build-vertex (make-point3 10.0 0.0 0.0))
                 (build-vertex (make-point3 -10.0 0.0 0.0))
                 (build-vertex (make-point3 0.0 -10.0 0.0)))
      
  
  (provide show-cube 
           show-cylinder 
           show-quad 
           show-disk 
           show-line 
           show-triangle 
           show-sphere)
    
  )
           