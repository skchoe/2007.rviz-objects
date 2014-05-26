(module test-mixin-accumulator scheme
  (require (lib "class.ss")
           (lib "list.ss")
           mzlib/etc
           )
  
  (define computing-interface<%>
    (interface () 
      object->renderer
      convert-struct
      ))

  
  ;; domain data 1
  (define obj-color-fish%
    (class* object% (computing-interface<%>)
      (super-new)
      ;...
      (define/public (object->renderer num)
        (printf "(to renderer) ~s \n" (+ num 100))
        (+ num 100))
      
      (define (process-elt elt)
        (if (list? elt)
            (if (empty? elt)
                empty 
                (begin 
                  (let* ((a (car elt))
                         (r (cdr elt)))
                    (append (process-elt a)
                            (process-elt r)))))
            (when (number? elt)
              (list (object->renderer elt)))))
      
      
      (define/public (convert-struct root new-root)
        (when (empty? new-root) 
          (set! new-root (build-list 0 identity)))
        (if (not (empty? root))
            (let* ([a (car root)]
                   [r (cdr root)])
              (convert-struct r (append new-root (process-elt a))))
            new-root))
              
      ))
    
  identity
  ;; domain data 2
  (define obj-file-fish%
    (class* object% (computing-interface<%>)
      (super-new)

      (define/public object->renderer
        (lambda (num)
          (printf "(to renderer) ~s\n" (* num 100))
          (* num 100)))
      
      (define/public (convert-struct root new-root)
        (printf "(file-fish)convert-struct\n")
        (map (lambda (x) (object->renderer x)) root))
      ))

  ; structures
  (define simple-tree (list 0 (list 1 2 3) (list 4 5 (list 6 7))))
  (define simple-list (list 0 1 2 3 4 5 6))
  

  ; object testing
  (define color-fish-obj (new obj-color-fish% ))
  (define obj-file-fish-obj (new obj-file-fish%))
                                 

  ;; example mixin
  (define (to-scene-graph-mixin %)
    (unless (implementation? % computing-interface<%>)
      (error "render-mixin:the class doesn't implement computing interface\n"))
    (class* % ()
      (super-new)
      )
    )

  (printf "1.-----------------------------\n")

  ;  (define (is-render? o) (is-a? o render-interface))
  (define sg-color-fish% (to-scene-graph-mixin obj-color-fish%))
  (define sg-obj-fish% (to-scene-graph-mixin obj-file-fish%))

  ; derived class -> 2 different object by 2 different parents
  (define color-child-obj (new sg-color-fish%))
  (define obj-child-obj (new sg-obj-fish%))

 
  (define sg-color (send color-child-obj convert-struct simple-tree null))
  (define sg-obj-file (send obj-child-obj convert-struct simple-list null))
  
  sg-color
  sg-obj-file
 
  (printf "2.-----------------------------\n")
  
  (define object-mixin->sg
    (lambda (mxn stc)
      (let* ([mxn-obj (new mxn)])
        (send mxn-obj convert-struct stc null))))

  (define tree->sg
    (lambda (tree-class% tree)
      (object-mixin->sg (to-scene-graph-mixin tree-class%) tree)))
  
  (define list->sg
    (lambda (list-class% list)
      (object-mixin->sg (to-scene-graph-mixin list-class%) list)))
  
  (define sg-color-1 (tree->sg obj-color-fish% simple-tree))
  (define sg-obj-file-1 (list->sg obj-file-fish% simple-list))
  
  sg-color-1
  sg-obj-file-1
  
  ;; check for arbitray object->render function can be handled in mixin.
  ;; input: domain-dependent mixin w/ object->renderer
  
  
  
  
;  ;; testing accumulator:
;  (define decrease-vector 
;    (lambda (v)
;      (let* ([size (vector-length v)]
;             [v1 (for/fold ([out-list empty])
;                           ([i (build-list (- size 1) (lambda (x) x))])
;                           (cons (vector-ref v i) out-list))])
;        (list->vector (reverse-list v1)))));(vector->list v1))))))
;  
;  (define reverse-list 
;    (lambda (lst)
;      (for/fold ([out-list empty])
;                ([i (build-list (length lst) (lambda (x) x))])
;                  (cons (list-ref lst i) out-list))))
;  
;
;  (define convert-struct-test 
;    (lambda (vect lst)
;      
;      (when (null? lst)
;        (set! lst empty))
;      
;      (if (equal? 0 (vector-length vect))
;          lst
;          (let* ([elt (vector-ref vect (- (vector-length vect) 1))]
;                 [new-V (decrease-vector vect)]
;                 [new-L (cons elt lst)])
;            (convert-struct-test new-V new-L)
;            ))))
;
;  (define initial-struct (vector 1 2 3 4 5 6))
;  (convert-struct-test initial-struct null)
;  
;  (decrease-vector (vector 1 2 3 4))
;  (reverse-list (list  1 2 3 4 5 6 7))

  
  
  
  
  
  )
  