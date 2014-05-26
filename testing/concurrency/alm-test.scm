#lang scheme
(require mred)
;;; thread1 just call alm-evt and wait until it returns value (corresp.evt is ready)
;;; alm-evt return ready/not-ready every moment(turn into ready after the delay)
(define pp-no 1)
(define initv 0)
(define maxv 100)

(define chan-i2o (make-channel))
(define chan-o2i (make-channel))

(define alm-evt 
  (lambda (elap-t)
    (sync (system-idle-evt))
    (alarm-evt (+ (current-inexact-milliseconds) elap-t))
    ))


(define (ping)
  (thread
   (lambda ()
     (channel-put chan-i2o initv)
     (let loop ()
       (let* ([val (channel-get chan-o2i)])
         (printf "ping: ~s \t" (+ val 1))
         (channel-put chan-i2o (+ val 1))

       (loop))))))

(define (pong)
  (thread
   (lambda ()
    (let loop ()
      (let* ([val (channel-get chan-i2o)])
        (printf "pong: ~s\n" (+ val 1))
        (when (< val maxv)
           (begin (channel-put chan-o2i (+ val 1))
                 (loop))))))))
(ping)
(pong)








  