#lang mzscheme

(define ht (make-hash-table 'equal))
;(hash-table-put! ht 1 1)
;(hash-table-put! ht 3 3)
;(hash-table-put! ht 4 4)
;(hash-table-put! ht 6 6)
;(hash-table-put! ht 7 7)
;(define result (hash-table-get ht 3 null))
;(null? result)
;result
;
;(define key (random 10))
;key
;(hash-table-get ht key null)
;
;(not #t)
;(define unique -1)
;unique
;(set! unique (do ((key (random 10)))
;                 ((equal? null (hash-table-get ht key null))
;                  (begin (hash-table-put! ht key key)
;                         (printf "~s \n" key)))
;                 (begin 
;                   (display key)
;                   (set! key (random 10))
;                   )))

        
(define generate-random-number
  (lambda(k) (random k)))
  
(define generate-pick-id
  (lambda (pi-table) ; pick-id-table
    (let* ((pick-id (generate-random-number 1000000))) 
      (do ()
        ((eq? null (hash-table-get pi-table pick-id null))
         (hash-table-put! pi-table pick-id pick-id))
        (set! pick-id (generate-random-number 1000000)))
      pick-id)))

(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)
(display (generate-pick-id ht))
(newline)

;(define tell (do ((key 1))
;               ((eq? key 1) (begin 
;                              (printf "~s \t" key)
;                              (set! unique key)))
;               (display "true")))
;
;unique

      