#lang scheme
;file:///Users/Authenticated%20User/Desktop/PLT%20Scheme%20v3.99.0.22/doc/guide/Contracts_on_Functions_in_General.html#(part~20contracts-optional)
(provide/contract

   ; pad the given str left and right with

   ; the (optional) char so that it is centered

   [string-pad-center (->* (string? natural-number/c)

                           (char?)

                           string?)])

  

  (define (string-pad-center str width [pad #\space])

    (define field-width (min width (string-length str)))

    (define rmargin (ceiling (/ (- width field-width) 2)))

    (define lmargin (floor (/ (- width field-width) 2)))

    (string-append (build-string lmargin (λ (x) pad))

                   str

                   (build-string rmargin (λ (x) pad))))
  
  (string-pad-center "test" 10 (lambda (x) x))


;Guide: PLT Scheme
; 
;1  Welcome to PLT Scheme 
;2  Scheme Essentials 
;3  Built-In Datatypes 
;4  Expressions and Definitions 
;5  Programmer-Defined Datatypes 
;6  Modules 
;7  Contracts 
;8  Input and Output 
;9  Regular Expressions 
;10  Exceptions and Control 
;11  Iterations and Comprehensions 
;12  Pattern Matching 
;13  Classes and Objects 
;14  Units (Components) 
;15  Threads 
;16  Reflection and Dynamic Evaluation 
;17  Macros 
;18  Reader Extension 
;19  Security 
;20  Memory Management 
;21  Performance 
;22  Running and Creating Executables 
;23  Configuration and Compilation 
;24  More Libraries 
; Bibliography 
; Index 
;
;On this page:
;7.3.1 Contract error messages that contain “...” 
;7.3.2 Optional arguments 
;7.3.3 Rest arguments 
;7.3.4 Keyword arguments 
;7.3.5 Optional keyword arguments 
;7.3.6 When a function’s result depends on its arguments 
;7.3.7 When contract arguments depend on each other 
;7.3.8 Ensuring that a function properly modifies state 
;7.3.9 Contracts for case-lambda 
;7.3.10 Multiple result values 
;7.3.11 Procedures of some fixed, but statically unknown arity 
;Version: 3.99.0.23contents index ← prev  up  next → 
;
;7.3 Contracts on Functions in General
;7.3.1 Contract error messages that contain “...”
;You wrote your module. You added contracts. You put them into the interface so that client programmers have all the information from interfaces. It’s a piece of art:
;
;  #lang scheme
; 
;  
; 
;  (provide/contract
; 
;   [deposit (-> (lambda (x)
; 
;                  (and (number? x) (integer? x) (>= x 0)))
; 
;                any)])
; 
;  
; 
;  (define this 0)
; 
;  (define (deposit a) ...)
; 
;
;
;Several clients used your module. Others used their modules in turn. And all of a sudden one of them sees this error message:
;
;bank-client broke the contract (-> ??? any) it had with myaccount on deposit; expected <???>, given: -10
;
;Clearly, bank-client is a module that uses myaccount but what is the ??? doing there? Wouldn’t it be nice if we had a name for this class of data much like we have string, number, and so on?
;
;For this situation, PLT Scheme provides flat named contracts. The use of “contract” in this term shows that contracts are first-class values. The “flat” means that the collection of data is a subset of the built-in atomic classes of data; they are described by a predicate that consumes all Scheme values and produces a boolean. The “named” part says what we want to do, which is to name the contract so that error messages become intelligible:
;
;  #lang scheme
; 
;  
; 
;  (define (amount? x) (and (number? x) (integer? x) (>= x 0)))
; 
;  (define amount (flat-named-contract 'amount amount?))
; 
;  
; 
;  (provide/contract
; 
;   [deposit (amount . -> . any)])
; 
;  
; 
;  (define this 0)
; 
;  (define (deposit a) ...)
; 
;
;
;With this little change, the error message becomes all of the sudden quite readable:
;
;bank-client broke the contract (-> amount any) it had with myaccount on deposit; expected <amount>, given: -10
;
;7.3.2 Optional arguments
;Take a look at this excerpt from a string-processing module, inspired by the Scheme cookbook:
;
;  #lang scheme
; 
;  
; 
;  (provide/contract
; 
;   ; pad the given str left and right with
; 
;   ; the (optional) char so that it is centered
; 
;   [string-pad-center (->* (string? natural-number/c)
; 
;                           (char?)
; 
;                           string?)])
; 
;  
; 
;  (define (string-pad-center str width [pad #\space])
; 
;    (define field-width (min width (string-length str)))
; 
;    (define rmargin (ceiling (/ (- width field-width) 2)))
; 
;    (define lmargin (floor (/ (- width field-width) 2)))
; 
;    (string-append (build-string lmargin (λ (x) pad))
; 
;                   str
; 
;                   (build-string rmargin (λ (x) pad))))
; 
;
;
;The module exports string-pad-center, a function that creates a string of a given width with the given string in the center. The default fill character is #\space; if the client module wishes to use a different character, it may call string-pad-center with a third argument, a char, overwriting the default.
;
;The function definition uses optional arguments, which is appropriate for this kind of functionality. The interesting point here is the formulation of the contract for the string-pad-center.
;
;The contract combinator ->*, demands several groups of contracts:
;
;The first one is a parenthesized group of contracts for all required arguments. In this example, we see two: string? and natural-number/c. 
;
;The second one is a parenthesized group of contracts for all optional arguments: char?. 
;
;The last one is a single contract: the result of the function.
;
;Note if a default value does not satisfy a contract, you won’t get a contract error for this interface. In contrast to type systems, we do trust you; if you can’t trust yourself, you need to communicate across boundaries for everything you write.
;
;7.3.3 Rest arguments
;We all know that + in Beginner Scheme is a function that consumes at least two numbers but, in principle, arbitraily many more. Defining the function is easy:
;
;  (define (plus fst snd . rst)
; 
;    (foldr + (+ fst snd) rst))
; 
;
;
;Describing this function via a contract is difficult because of the rest argument (rst).
;
;Here is the contract:
;
;  (provide/contract
; 
;   [plus (->* (number? number?) () #:rest (listof number?) number?)])
; 
;
;
;The ->* contract combinator empowers you to specify functions that consume a variable number of arguments or functions like plus, which consume “at least this number” of arguments but an arbitrary number of additional arguments.
;
;The contracts for the required arguments are enclosed in the first pair of parentheses:
;
;  (number? number?)
;
;For plus they demand two numbers. The empty pair of parenthesis indicates that there are no optional arguments (not counting the rest arguments) and the contract for the rest argument follows #:rest
;
;  (listof number?)
;
;Since the remainder of the actual arguments are collected in a list for a rest parameter such as rst, the contract demands a list of values; in this specific examples, these values must be number.
;
;7.3.4 Keyword arguments
;Sometimes, a function accepts many arguments and remembering their order can be a nightmare. To help with such functions, PLT Scheme has keyword arguments.
;
;For example, consider this function that creates a simple GUI and asks the user a yes-or-no question:
;
;  #lang scheme/gui
; 
;  
; 
  (define (ask-yes-or-no-question #:question question
 
                                  #:default answer
 
                                  #:title title
 
                                  #:width w
 
                                  #:height h)
 
    (define d (new dialog% [label title] [width w] [height h]))
 
    (define msg (new message% [label question] [parent d]))
 
    (define (yes) (set! answer #t) (send d show #f))
 
    (define (no) (set! answer #f) (send d show #f))
 
    (define yes-b (new button%
 
                       [label "Yes"] [parent d]
 
                       [callback (λ (x y) (yes))]
 
                       [style (if answer '(border) '())]))
 
    (define no-b (new button%
 
                      [label "No"] [parent d]
 
                      [callback (λ (x y) (no))]
 
                      [style (if answer '() '(border))]))
 
    (send d show #t)
 
    answer)
  
 
;  (provide/contract
; 
;   [ask-yes-or-no-question
; 
;    (-> #:question string?
; 
;        #:default boolean?
; 
;        #:title string?
; 
;        #:width exact-integer?
; 
;        #:height exact-integer?
; 
;        boolean?)])
; 

;
;Note that if you really want to ask a yes-or-no question via a GUI, you should use message-box/custom (and generally speaking, avoiding the responses “yes” and “no” in your dialog is a good idea, too ...).
;The contract for ask-yes-or-no-question uses our old friend the -> contract combinator. Just like lambda (or define-based functions) use keywords for specifying keyword arguments, it uses keywords for specifying contracts on keyword arguments. In this case, it says that ask-yes-or-no-question must receive five keyword arguments, one for each of the keywords #:question, #:default, #:title, #:width, and #:height. Also, just like in a function definition, the keywords in the -> may appear in any order.
;
;7.3.5 Optional keyword arguments
;Of course, many of the parameters in ask-yes-or-no-question (from the previous question) have reasonable defaults, and should be made optional:
;
;  (define (ask-yes-or-no-question #:question question
; 
;                                  #:default answer
; 
;                                  #:title [title "Yes or No?"]
; 
;                                  #:width [w 400]
; 
;                                  #:height [h 200])
; 
;    ...)
; 
;
;
;To specify this function’s contract, we need to use ->*. It too supports keywords just as you might expect, in both the optional and mandatory argument sections. In this case, we have mandatory keywords #:question and #:default, and optional keywords #:title, #:width, and #:height. So, we write the contract like this:
;
;  (provide/contract
; 
;   [ask-yes-or-no-question
; 
;    (->* (#:question string?
; 
;          #:default boolean?)
; 
;  
; 
;         (#:title string?
; 
;          #:width exact-integer?
; 
;          #:height exact-integer?)
; 
;  
; 
;         boolean?)])
; 
;
;
;putting the mandatory keywords in the first section and the optional ones in the second section.
;
;7.3.6 When a function’s result depends on its arguments
;Here is an excerpt from an imaginary (pardon the pun) numerics module:
;
;  #lang scheme
; 
;  (provide/contract
; 
;   [sqrt.v1 (->d ([argument (>=/c 1)])
; 
;                 ()
; 
;                 [result (<=/c argument)])])
; 
;  ...
; 
;
;
;The contract for the exported function sqrt.v1 uses the ->d rather than -> function contract. The “d” stands for dependent contract, meaning the contract for the function range depends on the value of the argument.
;
;In this particular case, the argument of sqrt.v1 is greater or equal to 1. Hence a very basic correctness check is that the result is smaller than the argument. (Naturally, if this function is critical, one could strengthen this check with additional clauses.)
;
;In general, a dependent function contract looks just like the more general ->* contract, but with names added that can be used elsewhere in the contract.
;
;Yes, there are many other contract combinators such as <=/c and >=/c, and it pays off to look them up in the contract section of the reference manual. They simplify contracts tremendously and make them more accessible to potential clients.
;
;7.3.7 When contract arguments depend on each other
;Eventually bank customers want their money back. Hence, a module that implements a bank account must include a method for withdrawing money. Of course, ordinary accounts don’t let customers withdraw an arbitrary amount of money but only as much as they have in the account.
;
;Suppose the account module provides the following two functions:
;
;  balance : (-> account amount)
; 
;  withdraw : (-> account amount account)
; 
;
;
;Then, informally, the proper precondition for withdraw is that “the balance of the given account is greater than or equal to the given (withdrawal) amount.” The postcondition is similar to the one for deposit: “the balance of the resulting account is larger than (or equal to) than the one of the given account.” You could of course also formulate a full-fledged correctness condition, namely, that the balance of the resulting account is equal to the balance of the given one, plus the given amount.
;
;The following module implements accounts imperatively and specifies the conditions we just discussed:
;
;  #lang scheme
; 
;  
; 
;  ; section 1: the contract definitions
; 
;  (define-struct account (balance))
; 
;  (define amount natural-number/c)
; 
;  
; 
;  (define msg> "account a with balance larger than ~a expected")
; 
;  (define msg< "account a with balance less than ~a expected")
; 
;  
; 
;  (define (mk-account-contract acc amt op msg)
; 
;    (define balance0 (balance acc))
; 
;    (define (ctr a)
; 
;      (and (account? a) (op balance0 (balance a))))
; 
;    (flat-named-contract (format msg balance0) ctr))
; 
;  
; 
;  ; section 2: the exports
; 
;  (provide/contract
; 
;   [create   (amount . -> . account?)]
; 
;   [balance  (account? . -> . amount)]
; 
;   [withdraw (->d ([acc account?]
; 
;                   [amt (and/c amount (<=/c (balance acc)))])
; 
;                  ()
; 
;                  [result (mk-account-contract acc amt > msg>)])]
; 
;   [deposit  (->d ([acc account?]
; 
;                   [amt amount])
; 
;                  ()
; 
;                  [result (mk-account-contract acc amt < msg<)])])
; 
;  
; 
;  ; section 3: the function definitions
; 
;  (define balance account-balance)
; 
;  
; 
;  (define (create amt) (make-account amt))
; 
;  
; 
;  (define (withdraw acc amt)
; 
;    (set-account-balance! acc (- (balance acc) amt))
; 
;    acc)
; 
;  
; 
;  (define (deposit acc amt)
; 
;    (set-account-balance! acc (+ (balance acc) amt))
; 
;    acc)
; 
;
;
;The second section is the export interface: 
;
;create consumes an initial deposit and produces an account. This kind of contract is just like a type in a statically typed language, except that statically typed languages usually don’t support the type “natural numbers” (as a full-fledged subtype of numbers). 
;
;balance consumes an account and computes its current balance.
;
;withdraw consumes an account, named acc, and an amount, amt. In addition to being an amount, the latter must also be less than (balance acc), i.e., the balance of the given account. That is, the contract for amt depends on the value of acc, which is what the ->d contract combinator expresses.
;
;The result contract is formed on the fly: (mk-account-contract acc amt > msg>). It is an application of a contract-producing function that consumes an account, an amount, a comparison operator, and an error message (a format string). The result is a contract.
;
;deposit’s contract has been reformulated using the ->d combinator. 
;
;The code in the first section defines all those pieces that are needed for the formulation of the export contracts: account?, amount, error messages (format strings), and mk-account-contract. The latter is a function that extracts the current balance from the given account and then returns a named contract, whose error message (contract name) is a string that refers to this balance. The resulting contract checks whether an account has a balance that is larger or smaller, depending on the given comparison operator, than the original balance.
;
;7.3.8 Ensuring that a function properly modifies state
;The ->d contract combinator can also ensure that a function only modifies state according to certain constraints. For example, consider this contract (it is a slightly simplified from the function preferences:add-panel in the framework):
;
;  (->d ([parent (is-a?/c area-container-window<%>)])
; 
;        ()
; 
;        [_
; 
;         (let ([old-children (send parent get-children)])
; 
;           (λ (child)
; 
;             (andmap eq?
; 
;                     (append old-children (list child))
; 
;                     (send parent get-children))))])
; 
;
;
;It says that the function accepts a single argument, named parent, and that parent must be an object matching the interface area-container-window<%>.
;
;The range contract ensures that the function only modifies the children of parent by adding a new child to the front of the list. It accomplishes this by using the _ instead of a normal identifier, which tells the contract library that the range contract does not depend on the values of any of the results, and thus the contract library evaluates the expression following the _ when the function is called, instead of when it returns. Therefore the call to the get-children method happens before the function under the contract is called. When the function under contract returns, its result is passed in as child, and the contract ensures that the children after the function return are the same as the children before the function called, but with one more child, at the front of the list.
;
;To see the difference in a toy example that focuses on this point, consider this program
;
;  #lang scheme
; 
;  (define x '())
; 
;  (define (get-x) x)
; 
;  (define (f) (set! x (cons 'f x)))
; 
;  (provide/contract
; 
;   [f (->d () () [_ (begin (set! x (cons 'ctc x)) any/c)])]
; 
;   [get-x (-> (listof symbol?))])
; 
;
;
;If you were to require this module, call f, then the result of get-x would be '(f ctc). In contrast, if the contract for f were
;
;  (->d () () [res (begin (set! x (cons 'ctc x)) any/c)])
;
;(only changing the underscore to res), then the result of get-x would be '(ctc f).
;
;7.3.9 Contracts for case-lambda
;Dybvig, in Chapter 5 of the Chez Scheme User’s Guide, explains the meaning and pragmatics of case-lambda with the following example (among others):
;
;  (define substring1
; 
;    (case-lambda
; 
;      [(s) (substring1 s 0 (string-length s))]
; 
;      [(s start) (substring1 s start (string-length s))]
; 
;      [(s start end) (substring s start end)]))
; 
;
;
;This version of substring has one of the following signature:
;
;just a string, in which case it copies the string;
;
;a string and an index into the string, in which case it extracts the suffix of the string starting at the index; or 
;
;a string a start index and an end index, in which case it extracts the fragment of the string between the two indices. 
;
;The contract for such a function is formed with the case-> combinator, which combines as many functional contracts as needed:
;
;  (provide/contract
; 
;    [substring1
; 
;     (case->
; 
;      (string? . -> . string?)
; 
;      (string? natural-number/c . -> . string?)
; 
;      (string? natural-number/c natural-number/c . -> . string?))])
; 
;
;
;As you can see, the contract for substring1 combines three function contracts, just as many clauses as the explanation of its functionality required.
;
;7.3.10 Multiple result values
;The function split consumes a list of chars and delivers the string that occurs before the first occurrence of #\newline (if any) and the rest of the list:
;
;  (define (split l)
; 
;    (define (split l w)
; 
;      (cond
; 
;        [(null? l) (values (list->string (reverse w)) '())]
; 
;        [(char=? #\newline (car l))
; 
;         (values (list->string (reverse w)) (cdr l))]
; 
;        [else (split (cdr l) (cons (car l) w))]))
; 
;    (split l '()))
; 
;
;
;It is a typical multiple-value function, returning two values by traversing a single list.
;
;The contract for such a function can use the ordinary function arrow ->, since it treats values specially, when it appears as the last result:
;
;  (provide/contract
; 
;   [split (-> (listof char?)
; 
;              (values string? (listof char?)))])
; 
;
;
;The contract for such a function can also be written using ->*, just like plus:
;
;  (provide/contract
; 
;   [split (->* ((listof char?))
; 
;               ()
; 
;               (values string? (listof char?)))])
; 
;
;
;As before the contract for the argument is wrapped in an extra pair of parentheses (and must always be wrapped like that) and the empty pair of parentheses indicates that there are no optoinal arguments. The contracts for the results are inside values: a string and a list of characters.
;
;Now suppose we also want to ensure that the first result of split is a prefix of the given word in list format. In that case, we need to use the ->d contract combinator:
;
;  (define (substring-of? s)
; 
;    (flat-named-contract
; 
;      (format "substring of ~s" s)
; 
;      (lambda (s2)
; 
;        (and (string? s2)
; 
;             (<= (string-length s2) s)
; 
;             (equal? (substring s 0 (string-length s2)) s2)))))
; 
;  
; 
;  (provide/contract
; 
;   [split (->d ([fl (listof char?)])
; 
;               ()
; 
;               (values [s (substring-of (list->string fl))]
; 
;                       [c (listof char?)]))])
; 
;
;
;Like ->*, the ->d combinator uses a function over the argument to create the range contracts. Yes, it doesn’t just return one contract but as many as the function produces values: one contract per value. In this case, the second contract is the same as before, ensuring that the second result is a list of chars. In contrast, the first contract strengthens the old one so that the result is a prefix of the given word.
;
;This contract is expensive to check of course. Here is a slightly cheaper version:
;
;  (provide/contract
; 
;   [split (->d ([fl (listof char?)])
; 
;               ()
; 
;               (values [s (string-len/c (length fl))]
; 
;                       [c (listof char?)]))])
; 
;
;
;Click on string-len/c to see what it does.
;
;7.3.11 Procedures of some fixed, but statically unknown arity
;Imagine yourself writing a contract for a function that accepts some other function and a list of numbers that eventually applies the former to the latter. Unless the arity of the given function matches the length of the given list, your procedure is in trouble.
;
;Consider this n-step function:
;
;  ; (number ... -> (union #f number?)) (listof number) -> void
; 
;  (define (n-step proc inits)
; 
;    (let ([inc (apply proc inits)])
; 
;      (when inc
; 
;        (n-step proc (map (λ (x) (+ x inc)) inits)))))
; 
;
;
;The argument of n-step is proc, a function proc whose results are either numbers or false, and a list. It then applies proc to the list inits. As long as proc returns a number, n-step treats that number as an increment for each of the numbers in inits and recurs. When proc returns false, the loop stops.
;
;Here are two uses:
;
;  ; nat -> nat
; 
;  (define (f x)
; 
;    (printf "~s \n" x)
; 
;    (if (= x 0) #f -1))
; 
;  (n-step f '(2))
; 
;  
; 
;  ; nat nat -> nat
; 
;  (define (g x y)
; 
;    (define z (+ x y))
; 
;    (printf "~s\n" (list x y z))
; 
;    (if (= z 0) #f -1))
; 
;  
; 
;  (n-step g '(1 1))
; 
;
;
;A contract for n-step must specify two aspects of proc’s behavior: its arity must include the number of elements in inits, and it must return either a number or #f. The latter is easy, the former is difficult. At first glance, this appears to suggest a contract that assigns a variable-arity to proc:
;
;  (->* ()
; 
;       (listof any/c)
; 
;       (or/c number? false/c))
; 
;
;
;This contract, however, says that the function must accept any number of arguments, not a specific but undetermined number. Thus, applying n-step to (lambda (x) x) and (list 1) breaks the contract because the given function accepts only one argument.
;
;The correct contract uses the unconstrained-domain-> combinator, which specifies only the range of a function, not its domain. It is then possible to combine this contract with an arity test to specify the correct n-step’s contract:
;
;  (provide/contract
; 
;   [n-step
; 
;    (->d ([proc
; 
;           (and/c (unconstrained-domain->
; 
;                   (or/c false/c number?))
; 
;                  (λ (f) (procedure-arity-includes?
; 
;                          f
; 
;                          (length inits))))]
; 
;          [inits (listof number?)])
; 
;         ()
; 
;         any)])
; 
;
;
; 
;
;contents index ← prev  up  next → 
