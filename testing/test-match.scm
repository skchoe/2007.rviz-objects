(module test-match mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "match.ss")
           )

  (define a 'aaa)
  (define b 'bbb)

  (display 29)
  
  (match 'ccc
    ('ccc (display #t))
    ('ddd (display #f)))
  
  (define (matching c)
    (match c
      ('aaa (display a))
      ('bbb (display b))))
  
  (matching 'aaa))