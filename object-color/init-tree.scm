(module init-tree mzscheme
  (require (lib "mred.ss" "mred")
           (lib "list.ss")
           (lib "class.ss")
           (lib "graphics.ss" "graphics")
           (lib "etc.ss")
           "../src/math/def.scm"
           "../src/math/calc.scm"
           "../src/render/cube.scm"
           "../src/render/sphere.scm"
           "../src/render/triangle.scm"
           "../src/render/cylinder.scm"
           "../src/render/disk.scm"
           "../src/render/primitives.scm"
           "tree-node.scm")
  
  
  (provide construct-fish-set-tree)
  
  ;; lake-state info
  ;  state in global -> point3
  ;  lake  in state  -> point3
  
  ;; defining graphics objects from fish object info.
  ;  kind -> color
  ;  kind -> primitive
  ;  size -> size
  ;       -> local point3
  
  ;;; State of Utah (UT) (10.0 0.0 0.0)
  ;; Utah lake (-10.0 0.0 0.0)
  ; - a black_fish (0.0 5.0 0.0)    - cube
  ; - a yellow_fish (0.0 -5.0 0.0)  - cylinder
  
  ;; Bear lake (10.0 0.0 0.0)
  ; - a purple_fish (0.0 0.0 0.0)   - disk
  
  ;;; State of California (CA) (-10.0 0.0 0.0)
  ;; Yosemite lake (0.0 0.0 0.0)
  ; - a red_fish (5.0 0.0 0.0)      - sphere
  ; - a blue_fish (-5.0 0.0 0.0)    - triangle
  ; - a green_fish (0.0 5.0 0.0)    - cube

  (define pos-param 5.0)
  ;; null -> root of tree
  (define (construct-fish-set-tree)
    ; fish construction
    (let* (; fish
           (fish-0 (make-fish 'black  2  'utah     
                              (make-point3 (/ pos-param 2.0) (* -2 pos-param) 0.0)
                              (make-point3 0.0 0.0 0.0)))
           (fish-1 (make-fish 'yellow 3  'utah     
                              (make-point3 (/ pos-param -2.0) (* -2 pos-param) 0.0)
                              (make-point3 0.0 0.0 0.0)))
           (fish-2 (make-fish 'purple 2  'bear     
                              (make-point3 0.0 0.0 0.0)
                              (make-point3 0.0 0.0 0.0)))
           (fish-3 (make-fish 'red    5 'yosemite 
                              (make-point3 pos-param 0.0 0.0)
                              (make-point3 0.0 0.0 0.0)))
           (fish-4 (make-fish 'blue   4 'yosemite 
                              (make-point3 (* -1 pos-param) 0.0 0.0)
                              (make-point3 0.0 0.0 0.0)))
           (fish-5 (make-fish 'green  3 'yosemite 
                              (make-point3 0.0 pos-param 0.0)
                              (make-point3 0.0 0.0 0.0)))
           
           ; list-of-fish
           (list-utah-fishes     (cons fish-0 (cons fish-1 empty)))
           (list-bear-fishes     (cons fish-2 empty))
           (list-yosemite-fishes (cons fish-3 (cons fish-4 (cons fish-5 empty))))
           
           ; leaf-node: lake names
           (utah     (make-leaf-node 'utah
                                     #t
                                     (make-point3 15.0 -5.0 0.0)
                                     (make-point3 0.0 0.0 0.0)
                                     empty
                                     list-utah-fishes))
           (bear     (make-leaf-node 'bear     
                                     #t
                                     (make-point3 15.0 20.0 0.0)  
                                     (make-point3 0.0 0.0 0.0)
                                     empty
                                     list-bear-fishes))
           (yosemite (make-leaf-node 'yosemite 
                                     #t
                                     (make-point3 -20.0 -10.0 0.0)
                                     (make-point3 0.0 0.0 0.0)
                                     empty
                                     list-yosemite-fishes))
           ; list of lake
           (list-UT-lakes (cons utah (cons bear empty)))
           (list-CA-lakes (cons yosemite empty))
           ; group-node: state names
           (UT (make-node 'UT 
                          #f
                          (make-point3 5.0 0.0 0.0) 
                          (make-point3 0.0 0.0 0.0) 
                          list-UT-lakes))
           (CA (make-node 'CA
                          #f
                          (make-point3 -5.0 0.0 0.0) 
                          (make-point3 0.0 0.0 0.0) 
                          list-CA-lakes))
           ; group-node: unique western U.S.
           (W-US (make-node 'W-US 
                            #f
                            (make-point3 0.0 0.0 0.0) 
                            (make-point3 0.0 0.0 0.0)
                            (cons CA (cons UT empty)))))
      W-US))
     
  (define (verify-fish-set)
    (let* ((root (construct-fish-set-tree)))
      (cond
        [(null? root) (display "root_is_null")]
        [else (print-out-field-kind root)])))

  (define (print-out-field-kind nd)
    (cond
      [(leaf-node? nd) (print-fish-list (leaf-node-list-of-fishes nd))]
      [else (print-out-field-kind-list (node-list-of-nodes nd))]))
  
  (define (print-out-field-kind-list lnd)
    (cond 
      [(empty? lnd)]
      [else (begin 
              (print-out-field-kind (car lnd))
              (print-out-field-kind-list (cdr lnd)))]))
  
  (define (print-fish-list fl)
    (cond
      [(empty? fl)]
      [else (begin 
              (display (fish-kind (car fl)))
              (newline)
              (print-fish-list (cdr fl)))]))
       
;  (verify-fish-set)  

  )