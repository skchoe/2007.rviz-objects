(module test-for-each-vector scheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "match.ss"))

  (define-struct combi (num str))

  (define v-combi-1 (make-combi 1 "1"))
  (define v-combi-2 (make-combi 2 "2"))
  (define v-combi-3 (make-combi 3 "3"))
  (define v-combi-4 (make-combi 4 "4"))

  (define lst-combi (list v-combi-1 v-combi-2 v-combi-3 v-combi-4))
  
  (define operation-combi0 
    (lambda (v-combi-k)
      (printf "~s\t ~s\n" (combi-num v-combi-k) (combi-str v-combi-k))))
  
  (define operation-combi1
    (lambda (v-combi-k)
      (make-combi (+ 100 (combi-num v-combi-k))
                  (combi-str v-combi-k))))

  (define operation-combi2
    (lambda (v-combi-k arg)
      (let ((lst (o2-input-lst arg))
            (v0 (o2-input-v0 arg))
            (v1 (o2-input-v1 arg)))
        (make-o2-input
         (cons (make-combi (+ v0 (+ v1 (combi-num v-combi-k)))
                           (combi-str v-combi-k))
               (o2-input-lst arg))
         v0
         v1))))
       
  (define test0
    (lambda ()
      (for-each operation-combi0 lst-combi)))

  (define test-values 
    (lambda (lst)
      (for-each operation-combi0 lst)))
  
  (define test-values2
    (lambda (arg)
      (let* ((lst (o2-input-lst arg)))
        (for-each operation-combi0 lst))))

  (define test1
    (lambda ()
      (let ((new-lst (map operation-combi1 lst-combi)))
        (test-values new-lst))))
  
  (define-struct o2-input (lst v0 v1))
  
  (define test2
    (lambda ()
      (let* ((arg (make-o2-input empty 100 1000))
             (new-lst (foldl operation-combi2 arg lst-combi)))
        (test-values2 new-lst))))
  (test2)
  
  ;; TEST
  (define-struct point3 (x y z))
  (define test-arg0 (list (make-point3 1 2 3) (make-point3 3 4 5) (make-point3 5 6 7)))
  (define test-arg1 (make-point3 1 1 1))
;  (for-each print-point3 (points-add test-arg0 test-arg1))
  
  
  (define gen-lst-numbers
    (lambda (num)
      (for/fold ([lst empty])
                ([i (in-range 0 num)])
                (cons i lst))))

  
  (gen-lst-numbers 10)
  
  )

