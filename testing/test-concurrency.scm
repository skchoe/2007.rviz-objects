#lang scheme
(require (lib "mred.ss" "mred")
        (lib "class.ss"))
(define frame (new frame% (label "Test Thread") (width 500) (height 100))) 

(define button
  (new button% 
      (parent frame) 
      (label "Click me!")
      (callback (lambda (item event)
                  (message-box "OK" "Now what?")))))

(define radio-box
  (new radio-box% 
      (label "Pick one")
      (choices '("Disable that button there" "Let it be enabled"))
      (parent frame)))

(define but-up-t 
  (thread
   (lambda ()
     (let loop ()
       (sync never-evt);(system-idle-evt))
       (send button enable
            (not (zero? (send radio-box get-selection))))
       (dok 5)
       (sleep 2)
       (loop)
       ))) 
  )

(define dok
  (lambda (k)
    (for ((i (in-range k)))
      (printf ":~s:\t" (* i 1000)))
    (printf "dok dok\n")
    ))

(define dok0
  (lambda ()
    (for ((i (in-range 9)))
      (printf ":~s:\t" (* i 1000)))))


(thread
 (lambda ()
   (let loop()
     (handle-evt always-evt dok);(system-idle-evt) dok)
     (sleep 1)
     (loop))
   ))
 
 
 
;(thread 
; (lambda ()
;   (let loop ()
;     (handle-evt (system-idle-evt)
;                (lambda (x) (printf "idle\t")))
;     (sleep .1)
;     (loop))))
;(define switch #f)
;(thread 
; (lambda ()
;   (let loop ()
;     (if switch 
;        (begin (printf "sec: ~s\t" (current-seconds)) (set! switch #f))
;        (begin (sleep 1) (set! switch #t)))
;     (loop))))

(send frame show #t)