(module test-load-thread mzscheme
  (require (lib "match.ss")
           (lib "list.ss"))

  (define path-file "2006-summer-oregon.txt")
  
  (define read-file-obj
    (lambda (filename)
      (unless (null? filename)
        (let* ((in-obj (open-input-file filename)) ; input-port
               (obj (read in-obj)))
          (close-input-port in-obj)
          obj))))


  ;----------------------------------------
  (define obj-read-server
    (lambda (obj-object filename-ch filecontent-ch)
      (sync (handle-evt filename-ch 
                        (lambda (name) 
                          (obj-read-server (read-file-obj name)
                                           filename-ch
                                           filecontent-ch)))
            (handle-evt filecontent-ch
                        (lambda (reply-ch)
                          (channel-put reply-ch obj-object)
                          (obj-read-server obj-object
                                           filename-ch
                                           filecontent-ch))))))
  
  (define init-thread
    (lambda (obj-object filename-ch filecontent-ch)  
      (thread (lambda () 
                (obj-read-server obj-object
                                 filename-ch
                                 filecontent-ch)))))

  (define content-ch->content 
    (lambda (c-ch) ; content channel
      (let ([reply-ch (make-channel)])
        (channel-put c-ch reply-ch)
        (channel-get reply-ch))))

  
  ;LOCALS
  
;  (define obj-read-thread-all
;    (lambda (path-file)
;      (define filename-ch (make-channel))
;      (define filecontent-ch (make-channel))
;  
;      (let ([tload (init-thread null filename-ch filecontent-ch)])
;        (channel-put filename-ch path-file)
;        
;        (let ([content (content-ch->content filecontent-ch)])
;          (kill-thread tload)
;          content))))
  
  (define send-filename
    (lambda (file-path)
      (define filename-ch (make-channel))
      (define filecontent-ch (make-channel))
  
      (let ([tload (init-thread null filename-ch filecontent-ch)])
        (channel-put filename-ch path-file)
        (values filecontent-ch tload))))
  
  (define recv-content
    (lambda (content-ch thread)
        (let ([content (content-ch->content content-ch)])
          (kill-thread thread)
          content)))
      
  (define gen-recv
    (lambda (v0 v1 recv) 
      (call-with-values (lambda () (values v0 v1)) recv)))
  
  (let*-values ([(vs0 vs1) (send-filename path-file)]
                [(vs2 vs3) (send-filename path-file)]
                [(content0) (gen-recv vs0 vs1 recv-content)]
                [(content1) (gen-recv vs2 vs3 recv-content)])
    (printf "xxxx~s\n" content0)
    (printf "xxxx~s\n" content1))
  )