(module test-list-copy mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "math.ss")
           (lib "list.ss"))
  
  (define (construct-list-org)
    (let* ((list-org (cons '1 (cons '2 (cons '3 (cons '4 empty))))))
      list-org))
  
  (define list-copy-org
    (lambda (list)
      (cond
        [(empty? list) empty]
        [else 
           (cons (car list) (list-copy-org (cdr list)))])))
  
  (define my-print
    (lambda (list)
      (cond
        [(empty? list) #f]
        [else 
         (begin 
           (display (car list))
           (my-print (cdr list)))])))
  
  
  (list-copy-org (construct-list-org))
  (my-print (list-copy-org (construct-list-org)))
  (newline)
  (display '--------------------------------)
  (newline)
  
  (define list-copy-k
    (lambda (list k)
      (cond
        [(empty? list) (k empty)]
        [else
         (list-copy-k (cdr list)
                      (lambda (v) (k (cons (car list) v))))])))
  
  (define list-copy
    (lambda (list)
      (list-copy-k list (lambda (v) v))))
  
  (my-print (list-copy-k (construct-list-org) (lambda(v) v)))
  
  (define okey #f)
  
  
  (define list0 (list 1 2 3))
;  (define list1 (append (list 100 200) list0))
  (define list1 (append empty list0))
  (define print-them
    (lambda ()
      (map print-each list1)))
  
  (define print-each 
    (lambda (num)
      (printf "~s\n" num)))
  
  (print-them)
  
  )

