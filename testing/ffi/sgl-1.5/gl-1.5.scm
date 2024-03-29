(module gl-1.5 scheme
  
  (require scheme/foreign
           (lib "class.ss")
           (lib "math.ss")
           sgl/gl-types
           sgl/gl-vectors
           "gl-types-1.5.scm"
           ;scheme/contract
           )
  
  (unsafe!)
  
  
  ; same definition as gl.ss
  (define gl-lib (case (system-type)
                   [(windows) (ffi-lib "glew32")]
                   [(macosx) (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL")]
                   [else (ffi-lib "libGL")]))
  (define glu-lib (case (system-type)
                    [(windows) (ffi-lib "glu32")]
                    [(macosx) (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGLU")]
                    [else (ffi-lib "libGLU")]))
  
  ;; helper functions in gl.ss  
  (define (unavailable name)
    (lambda ()
      (lambda x
        (error name "unavailable on this system"))))
  
  (define-syntax define-foreign-lib
    (syntax-rules (->)
      ((_ lib name type ... ->)
       (define-foreign-lib lib name type ... -> _void))
      ((_ lib name type ...)
       (begin
         (provide name)
         (define name
           (get-ffi-obj 'name lib (_fun type ...) (unavailable 'name)))))))

;  (define-syntax define-foreign-lib-pointer
;    (syntax-rules (->)
;      ((_ lib name type ... ptr-type ->)
;       (define-foreign-lib lib name type ... ptr-type -> _void))
;      ((_ lib name type ... ptr-type)
;       (begin
;         (provide name)
;         (cond ptr-type
;               [(exact-integer?) 
;                (define name
;                  (get-ffi-obj 'name lib (_fun type ... _gl-int) (unavailable 'name)))]
;               [(exact-integer?) 
;                (define name
;                  (get-ffi-obj 'name lib (_fun type ... _pointer) (unavailable 'name)))]
;               [else (error! "~s not supported \n")])))))
  
  (define-syntax define-foreign
    (syntax-rules ()
      ((_ args ...)
       (define-foreign-lib gl-lib args ...))))
  
  ; 2.8
;  (define-syntax define-foreign-default-pointer 
;    (syntax-rules (->)
;      ((_ name stride ptr-offset ->)
;       (begin
;         (provide name)
;         (define name
;           (cond 
;             [(exact-integer? ptr-offset)
;              (get-ffi-obj 'name gl-lib (_fun stride _gl-int -> _void) 
;                           (unavailable 'name))]
;             [(ctype? ptr-offset)
;              (get-ffi-obj 'name gl-lib (_fun stride _pointer -> _void) 
;                           (unavailable 'name))])
;              )))))
;  
;  (define-syntax define-foreign-sized-pointer 
;    (syntax-rules (->)
;      ((_ name enum stride ptr-offset ->)
;       (begin
;         (provide name)
;         (define name
;           (cond 
;             [(exact-integer? ptr-offset)
;              (get-ffi-obj 'name gl-lib (_fun enum stride _gl-int -> _void) 
;                           (unavailable 'name))]
;             [(ctype? ptr-offset)
;              (get-ffi-obj 'name gl-lib (_fun enum stride _pointer -> _void) 
;                           (unavailable 'name))]))))))
;
;  (define-syntax define-foreign-pointer
;    (syntax-rules (->)
;      ((_ name size enum stride ptr-offset ->)
;       (begin
;         (provide name)
;         (define name
;           (cond 
;             [(exact-integer? ptr-offset)
;              (get-ffi-obj 'name gl-lib (_fun size enum stride _gl-int -> _void) 
;                           (unavailable 'name))]
;             [(ctype? ptr-offset)
;              (get-ffi-obj 'name gl-lib (_fun size enum stride _pointer -> _void) 
;                           (unavailable 'name))]))))))

  
  ;; last formal variable is set to _gl-ptr: okey for vertex object, not vertex buffer
  ;; object because vertex buffer object consider it to be an offset from the beginning
  ;; of vertex buffer.
;;  (define-foreign glNormalPointer _gl-enum _gl-sizei _gl-ptr ->)
;;  (define-foreign glVertexPointer _gl-uint _gl-enum _gl-sizei _gl-ptr ->)
;;
;;  (define-foreign glColorPointer _gl-uint _gl-enum _gl-sizei _gl-ptr ->)
;;  (define-foreign glSecondaryColorPointer
;;                  _gl-uint _gl-enum _gl-sizei _gl-ptr ->)
;;  (define-foreign glIndexPointer _gl-enum _gl-sizei _gl-ptr ->)
;;  (define-foreign glFogCoordPointer _gl-enum _gl-sizei _gl-ptr ->)
;;  (define-foreign glTexCoordPointer _gl-uint _gl-enum _gl-sizei _gl-ptr ->)
;;  (define-foreign glEdgeFlagPointer _gl-sizei _gl-ptr ->)
;  
;  (define-foreign glNormalPointer _gl-enum _gl-sizei _gl-uint ->)
;  (define-foreign glVertexPointer _gl-uint _gl-enum _gl-sizei _gl-uint ->)
;
;  (define-foreign glColorPointer _gl-uint _gl-enum _gl-sizei _gl-uint ->)
;  (define-foreign glSecondaryColorPointer
;                  _gl-uint _gl-enum _gl-sizei _gl-uint ->)
;  (define-foreign glIndexPointer _gl-enum _gl-sizei _gl-uint ->)
;  (define-foreign glFogCoordPointer _gl-enum _gl-sizei _gl-uint ->)
;  (define-foreign glTexCoordPointer _gl-uint _gl-enum _gl-sizei _gl-uint ->)
;  (define-foreign glEdgeFlagPointer _gl-sizei _gl-uint ->)
  
  ; so by using above 3 functions, define-foreign-xxx-pointer, we deal with ptr-offset to
  ; have 2 forms of types: integer? ctype? 
;  (define-foreign-sized-pointer glNormalPointer _gl-enum _gl-sizei ptr ->)
;  (define-foreign-pointer glVertexPointer _gl-uint _gl-enum _gl-sizei ptr ->)

;  (define-foreign-pointer glColorPointer _gl-uint _gl-enum _gl-sizei ptr ->)

  (provide glNormalPointer)
  (define (glNormalPointer enum stride ptr-offset)
    (let ([gl-normal-pointer
           (cond 
             [(exact-integer? ptr-offset)
              (begin
                (get-ffi-obj 'glNormalPointer 
                             gl-lib 
                             (_fun _gl-enum _gl-sizei _gl-int -> _void) 
                             (unavailable 'glNormalPointer)))]
             [(cpointer? ptr-offset)
              (get-ffi-obj 'glNormalPointer 
                           gl-lib 
                           (_fun _gl-enum _gl-sizei _pointer -> _void) 
                           (unavailable 'glNormalPointer))])])
      (gl-normal-pointer enum stride ptr-offset)))
    
  (provide glVertexPointer)
  (define (glVertexPointer size enum stride ptr-offset)
    (let ([gl-vertex-pointer 
           (cond 
             [(exact-integer? ptr-offset)
              (begin
                (get-ffi-obj 'glVertexPointer 
                             gl-lib 
                             (_fun _gl-uint _gl-enum _gl-sizei _gl-int -> _void) 
                             (unavailable 'glVertexPointer)))]
             [(cpointer? ptr-offset)
              (get-ffi-obj 'glVertexPointer 
                           gl-lib 
                           (_fun _gl-uint _gl-enum _gl-sizei _pointer -> _void) 
                           (unavailable 'glVertexPointer))])])
      (gl-vertex-pointer size enum stride ptr-offset)))

  (provide glColorPointer)
  (define (glColorPointer size enum stride ptr-offset)
    (let ([gl-color-pointer 
           (cond 
             [(exact-integer? ptr-offset)
              (begin
                (get-ffi-obj 'glColorPointer 
                             gl-lib 
                             (_fun _gl-uint _gl-enum _gl-sizei _gl-int -> _void) 
                             (unavailable 'glColorPointer)))]
             [(cpointer? ptr-offset)
              (get-ffi-obj 'glColorPointer
                           gl-lib 
                           (_fun _gl-uint _gl-enum _gl-sizei _pointer -> _void) 
                           (unavailable 'glColorPointer))])])
      (gl-color-pointer size enum stride ptr-offset)))

  (provide glIndexPointer)
  (define (glIndexPointer enum size ptr-offset)
    (let ([gl-index-pointer
           (cond 
             [(exact-integer? ptr-offset)
              (begin
                (get-ffi-obj 'glIndexPointer 
                             gl-lib 
                             (_fun _gl-enum _gl-sizei _gl-int -> _void) 
                             (unavailable 'glIndexPointer)))]
             [(cpointer? ptr-offset)
              (get-ffi-obj 'glIndexPointer
                           gl-lib 
                           (_fun _gl-enum _gl-sizei _pointer -> _void) 
                           (unavailable 'glIndexPointer))])])
      (gl-index-pointer enum size ptr-offset)))
  
;  (define-foreign-pointer glSecondaryColorPointer
;                  _gl-uint _gl-enum _gl-sizei ptr ->)
;  (define-foreign-sized-pointer glIndexPointer _gl-enum _gl-sizei ptr ->)
;  (define-foreign-sized-pointer glFogCoordPointer _gl-enum _gl-sizei ptr ->)
;  (define-foreign-pointer glTexCoordPointer _gl-uint _gl-enum _gl-sizei ptr ->)
;  (define-foreign-default-pointer glEdgeFlagPointer _gl-sizei ptr ->)
  
  (define-foreign glEnableClientState _gl-enum ->)
  (define-foreign glDisableClientState _gl-enum ->)
  (define-foreign glClientActiveTexture _gl-enum ->)
  (define-foreign glArrayElement _gl-int ->)
  (define-foreign glDrawArrays _gl-enum _gl-int _gl-sizei ->)
 
  (define-foreign glMultiDrawArrays _gl-enum _gl-intv _gl-sizeiv _gl-sizei ->)
 
  
  
  (provide glDrawElements)
  (define (glDrawElements enumg size enumt ptr-offset)
    (let ([gl-draw-element
           (cond 
             [(exact-integer? ptr-offset)
              (get-ffi-obj 'glDrawElements
                           gl-lib
                           (_fun _gl-enum _gl-sizei _gl-enum _gl-int -> _void)
                           (unavailable 'glDrawElements))]
             [ (cpointer? ptr-offset)
              (get-ffi-obj 'glDrawElements
                           gl-lib
                           (_fun _gl-enum _gl-sizei _gl-enum _pointer -> _void)
                           (unavailable 'glDrawElements))]
             [(cvector? ptr-offset)
              (get-ffi-obj 'glDrawElements
                           gl-lib
                           (_fun _gl-enum _gl-sizei _gl-enum _gl-voidv -> _void)
                           (unavailable 'glDrawElements))])])
              
      (gl-draw-element enumg size enumt ptr-offset)))
  
;  (define-foreign glDrawElements _gl-enum _gl-sizei _gl-enum _gl-voidv ->)
  
  
  
  (define-foreign glMultiDrawElements
                  _gl-enum _gl-sizeiv _gl-enum _gl-voidvv _gl-sizei ->)
  (define-foreign glDrawRangeElements
                  _gl-enum _gl-uint _gl-uint _gl-sizei _gl-enum _gl-voidv ->)
  (define-foreign glInterleavedArrays _gl-enum _gl-sizei _gl-voidv ->)
  
  
  ;; 2.9
  #|
  |#
  (define-foreign glBindBuffer _gl-enum _gl-uint ->)
  (define-foreign glDeleteBuffers _gl-sizei _gl-uintv ->)
  (define-foreign glGenBuffers (n : _gl-sizei) (r : (_cvector o _gl-uint n)) -> _void -> r)

  (define-foreign glBufferData _gl-enum _gl-sizeiptr _gl-ptr _gl-enum ->)
  (define-foreign glBufferSubData _gl-enum _gl-uint _gl-uint _gl-ptr ->)
  (define-foreign glMapBuffer _gl-enum _gl-enum -> _gl-ptr)
  (define-foreign glUnmapBuffer _gl-enum -> _gl-boolean)

  )
