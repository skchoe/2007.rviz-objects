(module pick mzscheme
  (require (lib "class.ss")
           sgl
           sgl/gl
           sgl/gl-vectors
           (lib "list.ss"))

  (provide (all-defined))
  (provide pick-id-table)

  
  ;; pick-id-table -> pick-id: randomly/uniquly generated
  ;; update pick-id-table
  ;; pick-id is non-zero
  (define generate-pick-id
    (lambda (pi-table) ; pick-id-table
      (let loop ()
        (let ((pick-id (generate-random-number 10000000)))
          (if (or (hash-table-get pi-table pick-id #f) (equal? pick-id 0))
              (loop)
              (begin
                ;(printf "PICK-ID: ~s is assigned\n" pick-id)
                pick-id))))))
  
  (define register-pick-id
    (lambda (pi-table pick-id s-group)
      (hash-table-put! pi-table pick-id s-group)))

  (define generate-random-number
    (lambda(k) (random k)))
  
  (define init-pick-id-table 
    (lambda () 
      (make-hash-table 'equal)))
      
    
  ;; pick-id-table (used in wide veriety of place to manage unique pick-id)
  (define pick-id-table (init-pick-id-table))  
)