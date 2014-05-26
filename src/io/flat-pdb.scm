(module flat-pdb mzscheme
  (require (lib "mred.ss" "mred")
           "../math/def.scm")
  
  ;;---------------------------------------------------------
  ;; structure for flat type : originated from obj file - domain data side
  (provide 
   (all-defined))
;   (struct obj-vertices (points normals texture-coords))
;   (struct obj-face (point normal texture-coord))
;   (struct obj-part (name faces material))
;   (struct obj-object (dimension mtllib vertices parts materials))
;   
;   (struct mtl-params (Ns d illum Ka Kd Ks map_Kd))
;   
;   (struct fish-group (name view-group location orientation fish-list))
;  )
  
  
  ;--------container for obj file content
  ; points: list(point)
  ; normals: list(normal)
  ; tex-coords: list(tex-coord)
  (define-struct obj-vertices (points normals texture-coords))

  ; point:point3: integer index
  ; normal:point3 
  ; tex-coord: point2 
  ; each index stored in obj-face starts from 0: which corresponds 1 in obj file format
  (define-struct obj-face (point normal texture-coord)) 

  ; name: string
  ; faces: list(obj-face)
  ; material: material name
  (define-struct obj-part (name faces material))
  
  ; dimension: sruct (min-position, max-position)
  ; mtllib: string(material file name)
  ; vertices: struct obj-vertices
  ; parts: pair(obj-part)
  ; materials: hash-table(materialname, mtl-params)
  (define-struct obj-object (dimension mtllib vertices parts materials))
  
  ; obj-obj: obj-object
  ; position: point3
  ; orientation: point3
  (define-struct obj-scene-object (obj-obj position orientation))
  
  ; Ns: number(shininess s 0.0)
  ; d:  number(alpha Tr 1.0)
  ; illum: number (n=1,2) (1:flat-no spec highlight, 
  ;                        2: spec-highlight-> Ks is applied)
  ; Ka : point3 (ambient (0.2 0.2 0.2))
  ; Kd : point3 (diffuse (0.8 0.8 0.8))
  ; Ks : point3 (specular (1.0 1.0 1.0))
  ; map_Kd : image-object(file name for texture-map)
  (define-struct mtl-params (Ns d illum Ka Kd Ks map_Kd))
    
  ;; default mtl-params
  (define default-mtl-params
    (lambda ()
      (make-mtl-params 0.0 1.0 2 
                       (make-point3 0.2 0.2 0.2)
                       (make-point3 0.8 0.8 0.8)
                       (make-point3 0.8 0.8 0.8)
                       null)))

  ; name: string
  ; view-group: boolean
  ; location: point3
  ; orientation:point3
  ; fish-list:list, pair
  (define-struct fish-group (name        ; id of the group
                             view-group  ; boolean to view in one group
                             location    ; center of the group
                             orientation ; relative orientation
                             fish-list))); list of n-fishes
