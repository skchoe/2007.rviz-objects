   (module test-lexing mzscheme
     (require (prefix : (lib "lex-sre.ss" "parser-tools"))
              (lib "lex.ss" "parser-tools"))
     (provide (all-defined))
     
     (define my-simple-lexer
       (lexer
        [(:+ whitespace) (my-simple-lexer input-port)]
        [(:+ (:~ whitespace)) lexeme]
        [(eof) eof]))
     
     ;; split a string into a list of elements
     (define split
       (lambda (s)
         (let ((input (open-input-string s)))
           (let loop ((token (my-simple-lexer input)))
             (if (not (eof-object? token))
                 (cons token (loop (my-simple-lexer input)))
                 '())))))
     
;     [(:+ whitespace) (my-simple-lexer input-port)]
     )
   
   
   
;   http://hkn.eecs.berkeley.edu/~dyoo/plt/readme.text
;   PLT Scheme's lexer package has a fairly simple interface: we provide
;regular expressions and actions to perform when those expressions
;match, and out pops a lexer function.  This lexer function can then
;take input ports as input and return single tokens.  For example, the
;following is a really silly example of a lexer that breaks a string
;via whitespace:
;
;   (module test-lexing mzscheme
;     (require (prefix : (lib "lex-sre.ss" "parser-tools"))
;              (lib "lex.ss" "parser-tools"))
;     (provide (all-defined))
;
;     (define my-simple-lexer
;        (lexer
;          [(:+ whitespace) (my-simple-lexer input-port)]
;	  [(:+ (:~ whitespace)) lexeme]
;	  [(eof) eof]))
;
;     ;; split a string into a list of elements
;     (define split
;        (lambda (s)
;           (let ((input (open-input-string s)))
;              (let loop ((token (my-simple-lexer input)))
;                 (if (not (eof-object? token))
;                     (cons token (loop (my-simple-lexer input)))
;                     '())))))
;
;Of course, no sane person would use a lexer in such a simple situation
;like this.
;
;
;But anyway, there are a set of predefined regular expression
;"abbreviations" --- *whitespace* being one of them --- and a mechanism
;for defining more abbreviations.  The core of a lexer are the
;regex/action pairs, so when we hit something like:
;
;    [(:+ whitespace) (my-simple-lexer input-port)]
;
;what we are asking the lexer to do is to just skip one or more
;*whitespace* by reapplying the lexer on the rest of the given
;*input-port*.
;
;One thing that's probably so obvious that the documentation doesn't
;mention it is that the regular expression syntax defined in doc.txt
;can't be used outside a lexer macro.  Just something to note.
;
;There's a mechanism for defining different token types: the example
;above just returns strings as tokens, but there's a more general way
;to define specific token types that will work nicely with the rest of
;the PLT parser-tools.