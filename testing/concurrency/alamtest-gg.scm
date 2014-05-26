;; --- 
;; make-resettable-alarm : nat -> (values evt (-> void)) 
;; WARNING: not well-tested 

#lang scheme
(define (make-resettable-alarm delta) 
    (let ([internal-comm-channel (make-channel)] 
          [external-evt (make-channel)]) 
        (thread 
         (λ () 
           (let loop () 
             (sync 
              ;; the first event is for the case where the alarm goes off 
              (handle-evt (alarm-evt (+ (current-inexact-milliseconds) delta)) 
                          (λ (_) (channel-put external-evt external-evt))) 

              ;; the second event handles the case where someone resets the alarm. 
              ;; the strategy is to listen on an internal communications channel 
              ;; for reset requests; if they come in then abandon the original 
              ;; alarm and restart the loop with a new one 
              (handle-evt internal-comm-channel (λ (_) (loop)))))))
        (values 
         external-evt 
         (λ ()
           ;; asynchronously register a reset request. If we don't do this asynchronously 
           ;; then there's a race condition: if the alarm goes off and then somebody 
           ;; tries to reset the counter then the reset attempt will block forever. 
           (thread (λ () (channel-put internal-comm-channel #t)))))))

;; example usage 
(begin
    (define the-time (current-seconds)) 
    (define-values (c r) (make-resettable-alarm (* 16 1000))) 
    (sleep 4) 
    (r) 
    (sync c) 
    (printf "done! elapsed time: ~a seconds\n" (- (current-seconds) the-time))) 

;; after 9 seconds, prints: done! elapsed time: 9 seconds 

;; ---