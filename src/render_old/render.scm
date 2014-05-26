(module render mzscheme
  (require mred
           sgl
           sgl/gl
           sgl/gl-vectors
           (lib "class.ss")
           (lib "list.ss")
           (lib "foreign.ss")
           "primitives.scm"
           "../math/def.scm")

  (provide renderer%)
  (provide set-material-property)
  (provide default-transform)
  (provide default-material)
  (provide default-view-mode)
  (provide make-material-diffuse)
  (provide set-color-material-amb-dif)
  (provide default-transform-center)
  
  ;; interface of renderer%
  (define r<%> (interface () render))
    
  (define renderer%
    (class* object% (r<%>)
      
      ;; xform: to be inherited
      (init-field (xform 
                   (vector 1.0 0.0 0.0 0.0
                           0.0 1.0 0.0 0.0
                           0.0 0.0 1.0 0.0
                           0.0 0.0 0.0 1.0)))
      
      ; set-xform
      (public set-transform)
      (define (set-transform transform)
        (set! xform transform))

      ;; material color: to be inherited
      (init-field (material null))
      ; set-material
      (public set-material)
      (define (set-material mtrl)
        (set! material mtrl))
      
;      ;; primitive member(private)
;      (define primitive null)
;      ; set-primitive
;      (define (set-primitive prim)
;        (set! primitive prim))
;      ; get-primitive
;      (define (get-primitive)
;        primitive)

      ;; glGenList(1) gives dl-id
      (init-field (dl-id null))
      (public set-dl-id)
      (define (set-dl-id id)
        (set! dl-id id))
      (public get-dl-id)
      (define (get-dl-id)
        dl-id)

      ;; bounding-box
      (init-field (bbox null))
      (public set-bounding-box)
      (define (set-bounding-box b)
        (set! bbox b))
      (public get-bounding-box)
      (define (get-bounding-box)
        bbox)
      
      ;; compute bbx
      (public compute-bounding-box)
      (define (compute-bounding-box prim)
        (display 'compute-bounding-box_called))
      

      ;; render function for each renderer
      (public render)
      (define (render)
        (display 'render_on_parent_called))
     
      ;; pick function for each renderer
      (public pick)
      (define (pick)
        (display 'pick_on_parent_called))
      
      (super-new)))
  
  (define set-material-property
    (lambda (mat)

      (unless (null? mat)
        (unless (null? (material-ambient mat))
          (let ((amb (material-ambient mat)))
            (gl-material-v 'front-and-back
                           'ambient
                           (vector->gl-float-vector 
                            (vector (point4-x amb) (point4-y amb) 
                                    (point4-z amb) (point4-w amb))))))
        
        (unless (null? (material-diffuse mat))
          (let ((dif (material-diffuse mat)))
            (gl-material-v 'front-and-back
                           'diffuse
                           (vector->gl-float-vector 
                            (vector (point4-x dif) (point4-y dif) 
                                    (point4-z dif) (point4-w dif))))))
        
        (unless (null? (material-specular mat))
          (let ((spc (material-specular mat)))
            (gl-material-v 'front-and-back
                           'specular
                           (vector->gl-float-vector 
                            (vector (point4-x spc) (point4-y spc) 
                                    (point4-z spc) (point4-w spc))))))
        
        (unless (null? (material-shininess mat))
          (let ((shn (material-shininess mat)))
            (gl-material 'front-and-back
                         'shininess
                         shn)))

;        ;; texture setting
;        (when (empty? (material-texture-object mat)) 
;          (printf "textureobject is empty\n"))
          
        (unless (empty? (material-texture-object mat))
          (let* ((tex-obj (material-texture-object mat))
                 (width (list-ref tex-obj 0))
                 (height (list-ref tex-obj 1))
                 (tex-data (list-ref tex-obj 2)))
            
;            (printf "width = ~s, height= ~s\n" width height)
;            (printf "length(texture) = ~s\t first-val = ~s\n" 
;                    (cvector-length tex-data)
;                    (cvector-ref tex-data 0))

            (let* ((texture-list (glGenTextures 1))
                   (texture (gl-vector-ref texture-list 0)))
              
;              (printf "texture id = ~s \n" texture)
              
              (glBindTexture GL_TEXTURE_2D texture)
              
              ;;select modulate to mix texture with color for shading
              (glTexEnvf GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
              
              ; when texture area is small, bilinear filter the closest mipmap
              (glTexParameterf GL_TEXTURE_2D 
                               GL_TEXTURE_MIN_FILTER 
                               GL_LINEAR_MIPMAP_NEAREST)
              
              ; when texture area is large, bilinear filter the original
              (glTexParameterf GL_TEXTURE_2D 
                               GL_TEXTURE_MAG_FILTER 
                               GL_LINEAR)

              ; the texture wraps over at the edges (repeat)
              (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
              (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)

              
              (gluBuild2DMipmaps GL_TEXTURE_2D 3 width height
                                 GL_RGB GL_UNSIGNED_BYTE tex-data)

              (glColor4d 1 1 1 0.2)
              (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
              (glEnable GL_BLEND)

              ;; Standard Init
              (glEnable GL_TEXTURE_2D)

              ;; tex obj binding
              (glBindTexture GL_TEXTURE_2D texture)))))))
      

  
  (define make-material-diffuse
    (lambda (dif-p4)
      (let ((default (default-material)))
        (make-material (material-ambient default)
                       dif-p4
                       (material-specular default)
                       (material-shininess default)
                       (material-texture-object default)))))
               
  
    (define set-color-material-amb-dif 
      (lambda (color)
        (unless (eq? null color)
          (gl-material-v 'front-and-back
                         'ambient-and-diffuse
                         (vector->gl-float-vector 
                          (vector (point4-x color) (point4-y color) 
                                  (point4-z color) (point4-w color)))))))
  
  (define default-transform 
    (lambda ()
      (vector 1.0 0.0 0.0 0.0
              0.0 1.0 0.0 0.0
              0.0 0.0 1.0 0.0
              0.0 0.0 0.0 1.0)))
  
  (define default-transform-center 
    (lambda (x y z)
      (vector 1.0 0.0 0.0 0.0
              0.0 1.0 0.0 0.0
              0.0 0.0 1.0 0.0
              x y z 1.0)))
  
  
  ;; by opengl material values
  (define default-material 
    (lambda () 
      (make-material (make-point4 0.2 0.2 0.2 1.0)
                     (make-point4 0.8 0.8 0.8 1.0)
                     (make-point4 0.0 0.0 0.0 1.0)
                     0.0
                     empty)))
                     
  
  (define default-view-mode 
    (lambda () 'filled))

      
)