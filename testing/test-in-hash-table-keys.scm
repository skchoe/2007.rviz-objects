#lang scheme


(if (null? (in-range 4.5)) #t #f)
(sequence? (cons 4 empty))

(define ht (make-hash-table 'equal))
(hash-table-put! ht 1 11)
(hash-table-put! ht 2 12)
(hash-table-put! ht 3 13)
(hash-table-put! ht 4 14)
(hash-table-put! ht 5 15)

(define keys (in-hash-table-keys ht))
(for ((i keys)) (begin (newline)(display i)(newline)))

(define ht2 (make-hash-table 'equal))
(hash-table-put! ht2 "one" 1)
(hash-table-put! ht2 "two" 2)

(printf "hashtable-get ~s\n" (hash-table-get ht2 "one"))

(if false
    (printf "true\n")
    (printf "false\n"))

(define pht
  (lambda (k v)
    (printf "key = ~s, val = ~s\n" k v)))

(hash-table-for-each ht pht)

