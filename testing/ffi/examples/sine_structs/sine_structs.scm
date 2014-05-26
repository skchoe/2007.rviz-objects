#lang scheme
(require scheme/foreign)
(unsafe!)

(provide* (unsafe sine_array))

(define in-val (list 0.0 1.7 3.1))
(define in-valc (list->cvector in-val _float))

 (define (make-cvector-type t)
   (make-ctype _cvector
              (lambda (sval)
                (unless (cvector? sval)
                    (raise-type-error 'Scheme->C "cvector" sval))
                (unless (eq? (cvector-type sval) t)
                  (error 'Scheme->C "wrong kind of cvector"))
                sval)
              #f))

;; simple value
(define sine-lib (ffi-lib  "sine_structs.so"))
(let* (
       [sv (get-ffi-obj 'sine_value 
			sine-lib
			(_cprocedure (list _float)
				     _float)
			(lambda (x) (printf "NOT found\n")))]
       [out-val (sv 3.14)]
       )
  (printf "outval = ~s\n" out-val))
  
;;; array pointer
;(define value-set (malloc (* (ctype-sizeof _float) 2) 'raw))
;(let ([v (make-cvector* value-set _float 2)])
;  (cvector-set! v 0 1.75)
;  (cvector-set! v 1 3.14)

(define value-seT (cvector _float 1.0 100.0))
(let* ([sa (get-ffi-obj 'sine_array-nptr
                         sine-lib 
                         (_cprocedure (list _uint _cvector)
                                     _cvector)
                         (lambda (x) (printf "NOT found\n"))
                         )]
         [out-valc (sa 2 value-seT)])
    #f
;    (let (;[v1 (make-cvector* out-valc _float 2)])
  (printf "cvector 0: ~s\n" (cvector-ref out-valc _float 0))
  (printf "cvector 1: ~s\n" (cvector-ref out-valc _float 1))
  )


;(define value-vec (make-cvector _float 2))
;(cvector-set! value-vec 0 1.75)
;(cvector-set! value-vec 1 3.14)
;(define value-vec (list->cvector  (list 1.75 3.14) _float))
(define value-vec (malloc (* (ctype-sizeof _float) 2) 'raw))
(ptr-set! value-vec _float 0 1.75)
(ptr-set! value-vec _float 1 3.14)
 
(let* (;[_ftype (make-cvector-type _float)]
       [sa (get-ffi-obj 'sine_array
                       sine-lib
                       (_fun _uint _pointer -> _pointer)
                       (lambda (x) (printf "sin_array not found\n")))]
       [out-valc (sa 2 value-vec)])
  (printf "elt 0 = ~s\n" (ptr-ref out-valc _float 0))
  (printf "elt 1 = ~s\n" (ptr-ref out-valc _float 1)))


; cstruct
;(make-cstruct-type (list _float _float))
(define-cstruct _SineStruct ([in _float][out _float]))

(define t (make-SineStruct 1.1 2.2))
  (if (null? t) (printf "t: ss-null\n") (printf "t: ss-not-null\n"))

(let* (
       [ss (get-ffi-obj 'sine_struct
                       sine-lib
                       (_fun _SineStruct-pointer -> _SineStruct-pointer)
                       (lambda (x) (printf "sine_array:NOT found\n"))
                       )]
       [gens (get-ffi-obj 'gen_sine_struct
                       sine-lib
                       (_fun -> _SineStruct-pointer)
                       (lambda (x) (printf "gen_sine_struct:NOT found\n"))
                       )] 
       [sstr (make-SineStruct 1.7 0.0)]
       ;[sstr (gens)] 
       [out-strt (ss sstr)]
         )
  ;(if (eq? #f sstr) (printf "ssnull\n") (printf "ss-not-null\n"))
  ;(if (SineStruct? sstr) (printf "yes\n") (printf "no\n"))
  ;(if (cpointer? sstr) (printf "yes\n") (printf "no\n"))
   (printf "str in: ~s\n" (SineStruct-in out-strt))
  (printf "str out: ~s\n" (SineStruct-out out-strt))
  )


;array of struct
(printf "----------accessing_ftn_w/ array of pointer of structure\n")
(printf "ctype size of SineStruct = ~s\n" (ctype-sizeof _SineStruct))

(define m0 (malloc _SineStruct 1))
;(ctype-sizeof _SineStruct) 'raw))
(define ms0 (make-SineStruct 0.0 1.0))
(ptr-set! m0 _SineStruct ms0)

(define m1 (malloc _SineStruct 1))
;(ctype-sizeof _SineStruct) 'raw))
(define ms1 (make-SineStruct 1.75 1.0))
(ptr-set! m1 _SineStruct ms1)

(define struct-set (malloc _pointer 2))
;(* (ctype-sizeof _SineStruct-pointer) 2) 'raw))
(ptr-set! struct-set _pointer 0 m0)
(ptr-set! struct-set _pointer 1 m1)

(let* (
       [ssa (get-ffi-obj 'sine_struct_array
                        sine-lib
                        (_fun _uint _pointer -> _pointer)
                        (lambda (x) (printf "sine_struct_array: NOT found\n"))
                        )]
       [out-valc (ssa 2 struct-set)]
       )

    (let* ([ss0 (ptr-ref out-valc _pointer 0)][ss1 (ptr-ref out-valc _pointer 1)]
             [sS0 (ptr-ref ss0 _SineStruct 0)][sS1 (ptr-ref ss1 _SineStruct 0)])
      (printf " 0 ptr in:~s,  out:~s\n" (SineStruct-in sS0) (SineStruct-out sS0))
      (printf " 1 ptr in:~s,  out:~s\n" (SineStruct-in sS1) (SineStruct-out sS1))
      )
    #f
    )

(printf "----------accessing_ftn_w/ array of inlined structure\n")

(define arr-in (malloc _SineStruct 2))
(ptr-set! arr-in _SineStruct 0 (make-SineStruct 1.0 10.0))
(ptr-set! arr-in _SineStruct 1 (make-SineStruct 2.0 20.0))
 
(let* (
      [ssaa (get-ffi-obj 'sine_struct_array_inlined
                         sine-lib
                         (_fun _uint _pointer -> _pointer)
                         (lambda() (printf "sine_struct_array_inlined:NOT found\n"))
                         )]
       [arr-out (ssaa 2 arr-in)]
       ;[arr-out arr-in]
       ) 
  (let* ([s0 (ptr-ref arr-out _SineStruct 0)]
         [s1 (ptr-ref arr-out _SineStruct 1)])
    (printf "0 inlined in ~s, out ~s\n" (SineStruct-in s0) (SineStruct-out s0))
    (printf "1 inlined in:~s, out:~s\n" (SineStruct-in s1) (SineStruct-out s1))
    ))

(display '----------cvector_with_stride)
;(define make-cvector-fields* 
;  (lambda (cptr type length field-type field-name)
;    (let* ([sub-cvector (make-cvector field-type length)])
;      (for ([idx (in-range length)])
;        (cvector-set! sub-cvector idx (type-fieldname (cvector-ref cptr idx))))
;      sub-cvector)))
;  
  






