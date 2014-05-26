(module test-tree-K mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "match.ss"))


;  (define lc-node (make-tree-leaf 'left-leaf))
;  (define rc-node (make-tree-leaf 'right-leaf))
;  (define top-node (make-struct 'top lc-node rc-node))
;  
  (define hashT (make-hash-table 'weak))
  (hash-table-put! hashT 1 "a")
  (hash-table-put! hashT 2 "b")
  
  (printf "~s \n" (hash-table-get hashT 2))
  
  (let* ((lst (build-list 100 (lambda (n) n))))
    (printf "~s is length of lst\n" (length lst))
    (for-each (lambda (n) (printf "~s \t" n)) lst)))
  

