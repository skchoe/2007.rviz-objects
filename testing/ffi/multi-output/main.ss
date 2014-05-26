#lang scheme/base
(require scheme/foreign)

(define mult-out-lib
(ffi-lib "libmulti_out"))

(unsafe!)
 
(define (tmp)
  (values _int _cvector))
  
(tmp)
  
(define save-char
  (get-ffi-obj 'save_char
               mult-out-lib
               (_fun _int (char-set : _cvector) 
                     -> (status : _int) 
                     -> (list status char-set))
               (lambda () (printf "char:NOT found\n"))))

(define str0 (list->cvector (list "a" "b") _string))
(define str1 (list->cvector (list "c" "d" "e") _string))
(define str2 (list->cvector (list "f") _string))

(define lst-str (make-cvector _cvector 3))
(cvector-set! lst-str 0 str0)
(cvector-set! lst-str 1 str1)
(cvector-set! lst-str 2 str2)
(define ret-char (save-char  3 lst-str))
(printf "return value num = ~s\n" (list-ref ret-char 0))
(printf "return value chars = ~s\n" (list-ref ret-char 1))


(define save-int
  (get-ffi-obj 'save_int
               mult-out-lib
               (_fun _int (int-set : _cvector) 
                     -> (status : _int)
                     -> (list status int-set))
               (lambda () (printf "int not found\n"))))


(define lst-int (cvector _int 0 1 2 3 4))
(define ret-int (save-int (cvector-length lst-int) lst-int))
(printf "~s: return value num = ~s\n" 
        (cvector-length lst-int) 
        (list-ref ret-int 0))
(define new-int (list-ref ret-int 1))
(for-each (lambda (x) (printf "~s\n" x)) (cvector->list new-int))


(define update-int
  (get-ffi-obj 'update_int
               mult-out-lib
               (_fun (state : _pointer)
                     -> (output : _int)
                     -> (list output state))
               (lambda () (printf "update-int unavailable\n"))))

(define int-ptr (malloc _int 1))
(ptr-set! int-ptr _int 123)
(define ret-up (update-int int-ptr))

(define data (ptr-ref (list-ref ret-up 1) _int))

(printf "--~s:~s\n" (list-ref ret-up 0) data)


(define-struct tmp2 ())
(define att (make-tmp2))
(define-cstruct _CUcontext ())
#|
(define delete-context 
  (get-ffi-obj 'deleteContext 
               mult-out-lib
               (_fun (tt : _CUcontext)
                     -> (result : _int))
               (lambda () (printf "deleteContext unavailable\n"))))

(define C (make-CUcontext))
(printf "del-c = ~s\n" (delete-context C))

|#
  
  
