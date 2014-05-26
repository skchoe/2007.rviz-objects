(module view-env scheme
  (require (lib "class.ss")
           (lib "list.ss")
           mred
           sgl/gl
           sgl/gl-vectors
           (lib "foreign.ss")
           "../render/primitives.scm"
           "../render/render.scm"
           "../math/def.scm"
           "../structure/scene-node.scm"
           "../render/gl-texture-helpers.ss"
           "../operation/renderer-ops.scm"
           "../operation/scene-node-ops.scm"
           "../io/mtl-read.scm")
  
  (provide add-scene-env)
  
  ;; scene-node -> scene-node w/ old-scene-node + new-scene-node(having axis)
  ;; scene-leaf-node -> (cons new-scene-node old-scene-node)
  ;; WHOLE-ROOT
  ;; -list-of-scene-node : (cons object-node (list-of environments))
  (define add-scene-env
    (lambda (lst-scene-node)
      (let* (
             (object-node (initialize-scene-node lst-scene-node
                                                 #:n 'OBJECTS))

             (env-groups (cons (construct-axis-node) 
                               (cons (construct-backpanel-node 200 -150)
                                     empty)))
             )
        (initialize-scene-node (cons object-node env-groups)
                               #:n 'SCENE-ROOT))))

  ;; (void) -> axis leaf node w/ axis renderer list
  (define construct-axis-node
    (lambda ()
      (let* (
             (xax-renderer
              (initialize-renderer
               (make-line (make-point3 0.0 0.0 0.0)
                          (make-point3 10.0 0.0 0.0))
               (make-material-diffuse (make-point4 1.0 0.0 0.0 1.0))
               (default-transform)))

             (yax-renderer 
              (initialize-renderer
               (make-line (make-point3 0.0 0.0 0.0)
                          (make-point3 0.0 10.0 0.0))
               (make-material-diffuse (make-point4 0.0 1.0 0.0 1.0))
               (default-transform)))
             
             (zax-renderer 
              (initialize-renderer
               (make-line (make-point3 0.0 0.0 0.0)
                          (make-point3 0.0 0.0 10.0))
               (make-material-diffuse (make-point4 0.0 0.0 1.0 1.0))
               (default-transform)))
             
             (axis-renderer-list 
              (cons xax-renderer
                    (cons yax-renderer
                          (cons zax-renderer empty))))
             
             (leaf-node (initialize-scene-leaf-node 
                         axis-renderer-list
                         #:n 'axis-leaf))
                         
             
             (axes-node (initialize-scene-node (cons leaf-node empty)
                                               #:n 'axis))
             )
        
        axes-node)))
  
  (define texture-filename "data/m_Underwater_1024.bmp")
  ;;
  (define make-texture-object
    (lambda (filename)
      (let*((map-obj-top-down-scan (image->gl-vector filename))
            (map-obj-bottom-up-scan (topdown->bottomup 
                                     map-obj-top-down-scan))
            
            (vec-data (list-ref map-obj-bottom-up-scan 2)))
;        (printf "backpanel width = ~s\t" (list-ref map-obj-top-down-scan 0)) 
;        (printf "backpanel height = ~s\n" (list-ref map-obj-top-down-scan 1)) 
;        (printf "length-of backpanel texture = ~s\t, first elt = ~s\n"
;                (cvector-length vec-data) (cvector-ref vec-data 0))
;        (printf "--------------------------------------------\n")
        
        map-obj-bottom-up-scan)))
  
  (define construct-textured-material 
    (lambda (filename)
      (let* ((mat (default-material)))
        (make-material (material-ambient mat)
                       (make-point4 0.2 0.2 1.0 1.0)
                       (material-specular mat)
                       (material-shininess mat)
                       (make-texture-object filename)))))
    
  ;; (void ->background panel
  (define construct-backpanel-node
    (lambda (size z-value)
      (let* (
             (back-renderer
              (initialize-renderer (new-quad-size size)
                                   (construct-textured-material texture-filename)
                                   (default-transform)))
             (back-renderer-list (list back-renderer))
                                  
             (leaf-node (initialize-scene-leaf-node back-renderer-list
                                                    #:n 'back-leaf))

             (back-node (initialize-scene-node (cons leaf-node empty)
                                               #:n 'back
                                               #:x (default-transform-center
                                                     0.0 0.0 z-value)))
             )
        
        back-node)))
  
)