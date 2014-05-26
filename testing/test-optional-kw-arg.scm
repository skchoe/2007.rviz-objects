(module test-optional-kw-arg scheme

(provide optl-arg-S)
(provide/contract
  [deposit (-> number?
               boolean?)]
  [optl-arg (->* (number?)
                 (#:s number?
                      #:S number?)
                number?)]
  )

(define this 0)
(define deposit
  (lambda (x)
    (and (number? x) (integer? x) (>= x 0))))
;(deposit 3)


(define test-optional-arg 
  (lambda (#:(t 1))
    #f))

;(test-optional-arg)


(define (optl-arg tt #:s (t 1) #:S (T 2))
  (printf "~s optional arg = ~s ~s\n" tt t T)
  t)

(define (optl-arg-S www)
  (optl-arg www #:S www))

;(optl-arg-S 1000)
;
;(optl-arg #:s 3 #:S 4)
;(optl-arg #:s 3)
;(optl-arg)

)