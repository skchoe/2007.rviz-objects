(module pdb-util mzscheme
  (require (lib "match.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "etc.ss"))

(provide (all-defined))

(define list-copy-N
  (lambda (lst-src pos)
    (let* ([size-src (length lst-src)]
	   [size-dst (- size-src pos)])
      (let loop ([cnt 0]
		 [nlst '()])
	(if (equal? cnt size-dst)
	    nlst
	    (let ([elt (list-ref lst-src (+ pos cnt))])
	      (if (empty? nlst)
		 (loop (add1 cnt) (list elt))
		 (loop (add1 cnt) (append nlst (list elt))))))))))


(define string-trim-front
  (lambda (str)
   (let* ([lst (string->list str)])
     (let loop ([cnt 0]
		[nlst '()])
       ;(printf "in loop (~s:\t ~s)\n" cnt (length nlst))
       (if (equal? 0 (length lst)) 
           nlst
           (let* ([c (list-ref lst cnt)])
             (if (or (char-whitespace? c) (char-blank? c))
                 (loop (add1 cnt) nlst)
                 (list->string (list-copy-N lst cnt)))))))))


(define string-trim-rear
  (lambda (str)
    (let* ([rstr (string-reverse str)]
           [nstr (string-trim-front rstr)])
      (string-reverse nstr))))

(define string-reverse
  (lambda (str)
    (list->string (reverse (string->list str)))))
  
(define string-trim
  (lambda (str)
    (string-trim-front (string-trim-rear str))))
  
(define string-deblank
  (lambda (str)
    (let* ([lst (string->list str)]
           [size (length lst)])
      ;(printf "input size = ~s\n" size)
      (let loop ([cnt 0]
                 [nlst '()])
        ;(printf "in loop (~s:\t ~s)\n" cnt (length nlst))
        (if (eq? cnt size)
            (list->string nlst)
            (let* ([c (list-ref lst cnt)])
              (unless (or (char-whitespace? c) (char-blank? c))
                ;(printf "~s\n" c)
                (if (empty? nlst)
                    (set! nlst (list c))
                    (set! nlst (append nlst (list c)))))
              (loop (add1 cnt) nlst)))))))
  
  
;  (define ex-str "  ssd sdl ")
;  (printf "\n [~s]\n" (string-deblank ex-str))
;  (printf "~s\n" (string-trim  ex-str)) 
  
(define string-size 
  (lambda (str)
    (length (string->list str))))
  
; file id is a part of file name before .extension: (.pdb .ext)
(define file-name->file-id
  (lambda (filename)
    (let* ([len (string-size filename)])
      (if (> len 4)
          (substring filename 0 (- len 4))
          "default"))))
  
)