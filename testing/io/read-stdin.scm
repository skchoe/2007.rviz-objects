#lang scheme
(define flush-input
    (let ([buf (make-bytes 10)])
      (lambda ()
        (let loop ()
          (let ([n (read-bytes-avail!* buf)])
            (unless (or (eof-object? n) (zero? (read-bytes-avail!* buf)))
              (loop)))))))

(define (flush-and- read) (flush-input) (read))
  

(list (flush-and- read)
        (flush-and- read-line)
        (flush-and- read)
        (flush-and- read-line))