(module def mzscheme

  (provide 
   (struct point2 (s t))
   (struct point3 (x y z))
   (struct point4 (x y z w))
   (struct matrix4 (m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33))

   (struct dimension (min-position max-position)))
  
;  s t: number
  (define-struct point2 (s t))
  
;  x y z : number
  (define-struct point3 (x y z))
  
;  x y z w : number
  (define-struct point4 (x y z w))
  
;  m00 m01 m02 m03 : number
;  m10 m11 m12 m13
;  m20 m21 m22 m23
;  m30 m31 m32 m33
  (define-struct matrix4 (m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33))
  

  
;  min-position:point3
;  max-position:point3
  (define-struct dimension (min-position max-position))
  
)

;--------------------------------------------------------
;(define-type geometry
;  [point4 (x number?)
;          (y number?)
;          (z number?)
;          (w number?)]
;  [vector4 (dx number?)
;           (dy number?)
;           (dz number?)
;           (dw number?)])  
