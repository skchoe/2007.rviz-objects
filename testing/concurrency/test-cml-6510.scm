(module test-cml-6510 mzscheme
  (require (lib "match.ss")
           (lib "list.ss"))

;;; The thread running `serve' is the only one to
;;; touch `balance'
;
;(define balance 0)
;
;(define (deposit amt)
;  (set! balance (+ balance amt)))
;
;
;(define ch (make-channel))
;
;(define (serve)
;  (let ([amt (channel-get ch)])
;    (deposit amt)
;    (serve)))
;
;(thread serve)
;
;(channel-put ch 10)
;
;------------------------------------------------------------
;
;;; Channel value is can now be a request
;;; for the current balance in the form of
;;; c reply channel to receive the balance.

;(define balance 0)
;
;(define (deposit amt)
;  (set! balance (+ balance amt)))
;
;
;(define ch (make-channel))
;(define reply-ch (make-channel))
;  
;(define (serve)
;  (let ([req (channel-get ch)])
;    (if (number? req)
;        (begin (printf "input number = ~s\n" req)
;               (deposit req))
;        (begin (printf "input channel\n")
;               (channel-put req balance)))
;    (serve)))
;
;(thread serve)
;
;(channel-put ch 10)
;(let ()
;  (channel-put ch reply-ch)
;  (printf "chennel-get = ~s\n" (channel-get reply-ch)))
;
;(channel-put ch 20)
;(channel-put ch 50)
;  
;  (let ()
;  (channel-put ch reply-ch)
;  (printf "chennel-get = ~s\n" (channel-get reply-ch)))


;------------------------------------------------------------
;
;;; Step toward general strategy for making
;;; things thread-safe: create a channel,
;;; create a server, and create a safe-ifying
;;; wrapper.

;(define balance 0)
;
;(define (deposit amt)
;  (set! balance (+ balance amt)))
;
;(define ch (make-channel))
;
;(define (serve)
;  (let ([req (channel-get ch)])
;    (match req
;      [(reply-ch . thunk) (channel-put reply-ch (thunk))])
;    (serve)
;    )
;  )
;
;(thread serve)
;
;(define (make-safe thunk)
;  (let ([reply-ch (make-channel)])
;    (channel-put ch (cons reply-ch thunk))
;    (channel-get reply-ch)))
;
;(define (safe-deposit amt)
;  (make-safe (lambda ()
;               (deposit amt))))
;
;(define (safe-balance)
;  (make-safe (lambda () balance)))
;
;  (printf "deposit = ~s\n" (safe-deposit 10))
;  (printf "balance = ~s\n" (safe-balance))
;------------------------------------------------------------
;
;;; Avoid mutation: just pass the current balance
;;; in a loop.
;
;(define ch (make-channel))
;
;(define (serve balance)
;  (let ([req (channel-get ch)])
;    (if (number? req)
;        (serve (+ balance req))
;        (begin
;          (channel-put req balance)
;          (serve balance)))))
;
;(thread (lambda () (serve 0)))
;
;(channel-put ch 10)
;(let ([reply-ch (make-channel)])
;  (channel-put ch reply-ch)
;  (channel-get reply-ch))
;
;------------------------------------------------------------
;
;;; Instead of encoding deposit versus balance in
;;;  the message, encode it in the choice of
;;;  channel, which is more general (and, yes, tends
;;;  to fit type systems better).

;(define deposit-ch (make-channel))
;(define balance-ch (make-channel))
;
;(define (serve balance)
;  (let ([req (sync deposit-ch
;                   balance-ch)])
;    (if (number? req)
;        (serve (+ balance req))
;        (begin
;          (channel-put req balance)
;          (serve balance)))))
;
;(thread (lambda () (serve 0)))
;
;(channel-put deposit-ch 10)
;(let ([reply-ch (make-channel)])
;  (channel-put balance-ch reply-ch)
;  (printf "get = ~s\n" (channel-get reply-ch)))

;------------------------------------------------------------
;
;;; Use `sync' and `handle-evt' to clean up the
;;; dispatch.
;
;(define deposit-ch (make-channel))
;(define balance-ch (make-channel))
;
;(define (serve balance)
;  (sync (handle-evt deposit-ch
;                    (lambda (amt)
;                      (serve (+ amt balance ))))
;        (handle-evt balance-ch
;                    (lambda (reply-ch)
;                      (channel-put reply-ch balance)
;                      (serve balance)))))
;
;(thread (lambda () (serve 0)))
;
;(channel-put deposit-ch 10)
;(let ([reply-ch (make-channel)])
;  (channel-put balance-ch reply-ch)
;  (channel-get reply-ch))

;------------------------------------------------------------
;;; Switch to queue example.
;;;  [Bug: what if the queue is empty?]

;(define enqueue-ch (make-channel))
;(define dequeue-ch (make-channel))
;
;(define (serve items)
;  (sync (handle-evt enqueue-ch
;                    (lambda (item)
;                      (serve (append items
;                                     (list item)))))
;        (handle-evt dequeue-ch
;                    (lambda (reply-ch)
;                      (channel-put reply-ch (first items))
;                      (serve (rest items))))))
;
;(thread (lambda () (serve empty)))
;
;(channel-put enqueue-ch 10)
;(let ([reply-ch (make-channel)])
;  (channel-put dequeue-ch reply-ch)
;  (channel-get reply-ch))

;------------------------------------------------------------
;
;;; Fix bug by ignoring dequeue requests if we
;;; have nothing to supply; use never-evt in that
;;; case.

;(define enqueue-ch (make-channel))
;(define dequeue-ch (make-channel))
;
;(define (serve items)
;  (sync (handle-evt enqueue-ch
;                    (lambda (item)
;                      (serve (append items
;                                     (list item)))))
;        (if (empty? items)
;            never-evt
;            (handle-evt dequeue-ch
;                        (lambda (reply-ch)
;                          (channel-put reply-ch (first items))
;                          (serve (rest items)))))))
;  
;(thread (lambda () (serve empty)))
;
;(channel-put enqueue-ch 10)
;(define (dequeue)
;  (let ([reply-ch (make-channel)])
;    (channel-put dequeue-ch reply-ch)
;    (channel-get reply-ch)))
;(channel-put enqueue-ch 20) 
;(dequeue)
;(dequeue)
;
;------------------------------------------------------------
;
;;; Generalize queue code to support multiple
;;; queues.
;
;(define-struct q (en-ch de-ch))
;
;(define (create-queue)
;  (define enqueue-ch (make-channel))
;  (define dequeue-ch (make-channel))
;  
;  (define (serve items)
;    (sync (handle-evt enqueue-ch
;                      (lambda (item)
;                        (serve (append items
;                                       (list item)))))
;          (if (empty? items)
;              never-evt
;              (handle-evt dequeue-ch
;                          (lambda (reply-ch)
;                            (channel-put reply-ch (first items))
;                            (serve (rest items)))))))
;  
;  (thread (lambda () (serve empty)))
;  
;  (make-q enqueue-ch dequeue-ch))
;
;(define q1 (create-queue))
;(define q2 (create-queue))
;
;(channel-put (q-en-ch q1) 10)
;(channel-put (q-en-ch q2) 20)
;
;(define (dequeue q)
;  (let ([reply-ch (make-channel)])
;    (channel-put (q-de-ch q) reply-ch)
;    (channel-get reply-ch)))
;
;(dequeue q1)
;(dequeue q2)
;
;------------------------------------------------------------
;
;;; Dequeue from q1 or q2, whichever is
;;; ready to supply an item.
;
;(define-struct q (en-ch de-ch))
;
;(define (create-queue)
;  (define enqueue-ch (make-channel))
;  (define dequeue-ch (make-channel))
;  
;  (define (serve items)
;    (sync (handle-evt enqueue-ch
;                      (lambda (item)
;                        (serve (append items
;                                       (list item)))))
;          (if (empty? items)
;              never-evt
;              (handle-evt dequeue-ch
;                          (lambda (reply-ch)
;                            (channel-put reply-ch (first items))
;                            (serve (rest items)))))))
;  
;  (thread (lambda () (serve empty)))
;  
;  (make-q enqueue-ch dequeue-ch))
;
;(define q1 (create-queue))
;(define q2 (create-queue))
;
;(channel-put (q-en-ch q1) 10)
;(channel-put (q-en-ch q2) 20)
;
;(define (dequeue)
;  (let ([reply-ch (make-channel)])
;    (sync (channel-put-evt (q-de-ch q1) reply-ch)
;          (channel-put-evt (q-de-ch q2) reply-ch))
;    (channel-get reply-ch)))
;
;(dequeue)
;(dequeue)
;
;------------------------------------------------------------
;
;;; Switch to an API that exposes events instead of
;;; channels, because that will compose better.
;
;(define-struct q (en-evt de-evt))
;
;(define (create-queue)
;  (define enqueue-ch (make-channel))
;  (define dequeue-ch (make-channel))
;  
;  (define (serve items)
;    (sync (handle-evt enqueue-ch
;                      (lambda (item)
;                        (serve (append items
;                                       (list item)))))
;          (if (empty? items)
;              never-evt
;              (handle-evt dequeue-ch
;                          (lambda (reply-ch)
;                            (channel-put reply-ch (first items))
;                            (serve (rest items)))))))
;  
;  (thread (lambda () (serve empty)))
;  
;  (make-q (lambda (v)
;            (channel-put-evt enqueue-ch v))
;          (lambda (reply-ch)
;            (channel-put-evt dequeue-ch reply-ch))))
;
;(define q1 (create-queue))
;(define q2 (create-queue))
;
;(sync ((q-en-evt q1) 10))
;(sync ((q-en-evt q2) 20))
;
;(define (dequeue)
;  (let ([reply-ch (make-channel)])
;    (sync ((q-de-evt q1) reply-ch)
;          ((q-de-evt q2) reply-ch))
;    (channel-get reply-ch)))
;
;(dequeue)
;(dequeue)
;
;------------------------------------------------------------
;
;;; Now we can implementing a multiplex "queue"
;;; in terms of other queues.
;;; [Beware that the multiplexes one is not actually
;;;  a queue anymore, because it doesn't preserve
;;;  order, but maybe we don't care.]
;
;(define-struct q (en-evt de-evt))
;
;(define (create-queue)
;  (define enqueue-ch (make-channel))
;  (define dequeue-ch (make-channel))
;  
;  (define (serve items)
;    (sync (handle-evt enqueue-ch
;                      (lambda (item)
;                        (serve (append items
;                                       (list item)))))
;          (if (empty? items)
;              never-evt
;              (handle-evt dequeue-ch
;                          (lambda (reply-ch)
;                            (channel-put reply-ch (first items))
;                            (serve (rest items)))))))
;  
;  (thread (lambda () (serve empty)))
;  
;  (make-q (lambda (v)
;            (channel-put-evt enqueue-ch v))
;          (lambda (reply-ch)
;            (channel-put-evt dequeue-ch reply-ch))))
;
;(define q1 (create-queue))
;(define q2 (create-queue))
;
;(define (multiplex-q q1 q2)
;  (make-q (lambda (v)
;            (choice-evt ((q-en-evt q1) v)
;                        ((q-en-evt q2) v)))
;          (lambda (reply-ch)
;            (choice-evt ((q-de-evt q1) reply-ch)
;                        ((q-de-evt q2) reply-ch)))))
;
;(define q1+q2 (multiplex-q q1 q2))
;
;(sync ((q-en-evt q1+q2) 10))
;(sync ((q-en-evt q1+q2) 20))
;
;(define (dequeue q)
;  (let ([reply-ch (make-channel)])
;    (sync ((q-de-evt q) reply-ch))
;    (channel-get reply-ch)))
;
;(dequeue q1+q2)
;(dequeue q1+q2)
;(thread (lambda () (sleep 2) (sync ((q-en-evt q1+q2) 30))))
;(dequeue q1+q2)
;
;------------------------------------------------------------
;
;;; Swap-channel example
;
;(define (swap-ch)
;  (define ch (make-channel))
;  (lambda (v)
;    (sync
;     (choice-evt
;      (let ([reply-ch (make-channel)])
;        (handle-evt (channel-put-evt ch (cons v reply-ch))
;                    (lambda (_)
;                      (channel-get reply-ch))))
;      (handle-evt ch
;                  (lambda (req)
;                    (match req
;                      [(other-v . reply-ch)
;                       (channel-put reply-ch v)
;                       other-v])))))))
;(define s (swap-ch))
;
;(thread (lambda () (s 12)))
;(thread (lambda () (s 13)))
;(thread (lambda () (s 14)))
;(s 10)
;
;
;------------------------------------------------------------
;  
;;; A queue that accepts a predicate before supplying an
;;; item from the queue. The server now needs to keep
;;; a list of requests, and the synchronization point is
;;; the request response, not the request acceptance.
;
;(define-struct q (en-evt de-evt))
;
;(define (create-queue)
;  (define enqueue-ch (make-channel))
;  (define dequeue-ch (make-channel))
;  
;  ;; A req (cons pred reply-ch)
;  
;  (define (serve items reqs)
;    (sync (handle-evt enqueue-ch
;                      (lambda (item)
;                        (serve (append items
;                                       (list item))
;                               reqs)))
;          (handle-evt dequeue-ch
;                      (lambda (req)
;                        (serve items (cons req reqs))))
;          (apply
;           choice-evt
;           (map (lambda (req)
;                  (match req
;                    [(pred . reply-ch)
;                     (let ([ok-items (filter pred items)])
;                       (if (empty? ok-items)
;                           never-evt
;                           (handle-evt (channel-put-evt reply-ch (first ok-items))
;                                       (lambda (_)
;                                         (serve (remq (first ok-items) items)
;                                                (remq req reqs))))))]))
;                reqs))))
;
;  (thread (lambda () (serve empty empty)))
;  
;  (make-q (lambda (v)
;            (channel-put-evt enqueue-ch v))
;          (lambda (pred reply-ch)
;            (guard-evt
;             (lambda ()
;               (channel-put dequeue-ch (cons pred reply-ch))
;               reply-ch)))))
;
;(define q1 (create-queue))
;(define q2 (create-queue))
;
;(define (multiplex-q q1 q2)
;  (make-q (lambda (v)
;            (choice-evt ((q-en-evt q1) v)
;                        ((q-en-evt q2) v)))
;          (lambda (reply-ch pred)
;            (choice-evt ((q-de-evt q1) reply-ch pred)
;                        ((q-de-evt q2) reply-ch pred)))))
;
;(define q1+q2 (multiplex-q q1 q2))
;
;(sync ((q-en-evt q1+q2) 10))
;(sync ((q-en-evt q1+q2) 20))
;
;(define (dequeue q)
;  (let ([reply-ch (make-channel)])
;    (sync ((q-de-evt q) (lambda (x) #t) reply-ch))))
;
;(dequeue q1+q2)
;(dequeue q1+q2)
;(thread (lambda () (sleep 2) (sync ((q-en-evt q1+q2) 30))))
;(dequeue q1+q2)
;
;------------------------------------------------------------
;
;;; Function summary
;;; [The alphas and betas in this file are in UTF-8.]
;
;;; sync            : α-event ... -> α
;
;;; make-channel    : -> α-channel
;;; channet-get     : α-channel -> α
;;; channel-put     : α-channel α -> void
;;; channel-put-evt : α-channel α -> void-event
;;; channel-get-evt : α-channel -> α-event
;;;  ... or use α-channel <: α-event
;
;;; never-evt       : α-event
;;; always-evt      : void-event
;
;;; choice-evt      : α-event ... -> α-event
;
;;; handle-evt      : β-event (β -> α) -> α-event
;
;;; guard-evt       : (-> α-event) -> α-event
;;; nack-guard-evt  : (void-event -> α-event) -> α-event
;
;;; poll-guard-evt  : (bool -> α-event) -> α-event
;
  
;  ;; killing
;  (define-struct q (en-evt de-evt))
;
;(define (create-queue)
;  (define enqueue-ch (make-channel))
;  (define dequeue-ch (make-channel))
;  
;  ;; A req (cons pred reply-ch)
;  
;  (define (serve items reqs)
;    (sync (handle-evt enqueue-ch
;                      (lambda (item)
;                        (serve (append items
;                                       (list item))
;                               reqs)))
;          (handle-evt dequeue-ch
;                      (lambda (req)
;                        (serve items (cons req reqs))))
;          (apply
;           choice-evt
;           (map (lambda (req)
;                  (match req
;                    [(pred reply-ch cancel-evt)
;                     (choice-evt
;                      (handle-evt cancel-evt
;                                  (lambda (_)
;                                    (serve items
;                                           (remq req reqs))))
;                      (let ([ok-items (filter pred items)])
;                        (if (empty? ok-items)
;                            never-evt
;                            (handle-evt (channel-put-evt reply-ch 
;                                                         (first ok-items))
;                                        (lambda (_)
;                                          (serve (remq (first ok-items) items)
;                                                 (remq req reqs)))))))]))
;                reqs))))
;
;  (thread (lambda () (serve empty empty)))
;  
;  (make-q (lambda (v)
;            (channel-put-evt enqueue-ch v))
;          (lambda (pred)
;            (nack-guard-evt
;             (lambda (cancel-evt)
;               (let ([reply-ch (make-channel)])
;                 (channel-put dequeue-ch (list pred reply-ch cancel-evt))
;                 (channel-get-evt reply-ch)))))))
;(define (channel-get-evt ch) ch)
;
;(define q1 (create-queue))
;(define q2 (create-queue))
;
;(define (multiplex-q q1 q2)
;  (make-q (lambda (v)
;            (choice-evt ((q-en-evt q1) v)
;                        ((q-en-evt q2) v)))
;          (lambda (pred)
;            (choice-evt ((q-de-evt q1) pred)
;                        ((q-de-evt q2) pred)))))
;
;(define q1+q2 (multiplex-q q1 q2))
;
;(sync ((q-en-evt q1+q2) 10))
;(sync ((q-en-evt q1+q2) 20))
;
;(define (dequeue q)
;  (sync ((q-de-evt q) (lambda (x) #t))))
;
;(dequeue q1+q2)
;(dequeue q1+q2)
;(thread (lambda () (sleep 2) (sync ((q-en-evt q1+q2) 30))))
;(kill-thread (thread (lambda () (dequeue q1+q2))))

  
  
;; thread init
;; Illustrates the tedios work required to start a thread
;;  that expects to cooperatively exit:
;(define (thread/breakable thunk)
;  (parameterize-break
;   #f
;   (thread (lambda () 
;             (with-handlers ([exn:break?
;                              (lambda (exn) (printf "done\n"))])
;               (parameterize-break
;                #t
;                (thunk)))))))
;(define t (thread/breakable (lambda () (sleep 10))))
;(break-thread t)  
  
;;; Shows how to implement a library function that works ok
;;;  with cooperative termination (i.e., asynchronous break signals).
;
;;; The key is to use `parameterize-break' in the right way, and to
;;;  use `tcp-connect/enable-break' to make breaks allowed at just
;;;  the right time while a connection is attempted.
;
;(define (check-version)
;  (parameterize-break
;   #f
;   (let ([(in out) (tcp-connect/enable-break "www.cs.utah.edu" 80)])
;     (dynamic-wind
;      (lambda () (void))
;      (lambda ()
;        (parameterize-break
;         #t
;         ... (write out) ...
;         ... (read in) ...))
;      (lambda ()
;        (close-input-port in)
;        (close-output-port out)))))) 
)