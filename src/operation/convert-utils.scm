(module convert-utils mzscheme
  (require (lib "class.ss")
           "../math/def.scm"
           "../math/calc.scm")
  
  (provide (all-defined))
  
  ; interface 
  (define convert-to-scene-graph<%>
    (interface ()
      object->renderer
      convert-to-scene-graph))
  
  ; mixin
  (define convert-to-scene-graph-mixin
    (lambda (%)
      (unless (implementation? % convert-to-scene-graph<%>)
        (error "convert-mixin:the class doesn't implement convert-to-scene-graph interface\n"))
      (class* % ()
        (super-new))))
  
  ; mixin -> scene-graph: general proc.
  ; in: mixin(child class), custom structure
  ; out: scene-graph
  (define object-mixin->scene-graph
    (lambda (mxn stc)
      (let* ([mxn-obj (new mxn)])
        (send mxn-obj convert-to-scene-graph stc))))
  
  
  
  ;; conversion helpers
  ;
  (define pos_orient->transform 
    (lambda (position orientation)
      (letrec ((px (point3-x position))  ; x pos
             (py (point3-y position))    ; y pos
             (pz (point3-z position))    ; z pos
             (rx (point3-x orientation)) ; rotation around x-axis
             (ry (point3-y orientation)) ; rotation around y-axis
             (rz (point3-z orientation))); rotation around z-axis

        (let* ([mat_rx (xrotation->transform rx)]
               [mat_ry (yrotation->transform ry)]
               [mat_rz (zrotation->transform rz)]
               
               ;  mat_rx * mat_ry * mat_rz
               [mat_rxyz (mtx-mult-3-3 mat_rx (mtx-mult-3-3 mat_ry mat_rz))])

          (vector-set! mat_rxyz 12 px)
          (vector-set! mat_rxyz 13 py)
          (vector-set! mat_rxyz 14 pz)
          
          mat_rxyz))))
          
  
  
        
)