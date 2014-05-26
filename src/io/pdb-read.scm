(module pdb-read scheme
(require (lib "match.ss")
	 (lib "list.ss")
	 (lib "string.ss")
	 (lib "etc.ss")
	 "../math/def.scm"
	 "../math/calc.scm"
         "pdb-struct.scm"
         "pdb-util.scm")

#|
//---------------------------------------------------------------------------------------------------------------
//    http://www.wwpdb.org/documentation/format23/sect9.html
//---------------------------------------------------------------------------------------------------------------
//    ATOM
//    Overview
//    The ATOM records present the atomic coordinates for standard residues (see http://deposit.pdb.org/public-component-erf.cif). They also present the occupancy and temperature factor for each atom. Heterogen coordinates use the HETATM record type. The element symbol is always present on each ATOM record; segment identifier and charge are optional.
//
//    Record Format
//
//    COLUMNS      DATA TYPE        FIELD      DEFINITION
//    ------------------------------------------------------
//     1 -  6      Record name      "ATOM    "
//     7 - 11      Integer          serial     Atom serial number.
//    13 - 16      Atom             name       Atom name.
//    17           Character        altLoc     Alternate location indicator.
//    18 - 20      Residue name     resName    Residue name.
//    22           Character        chainID    Chain identifier.
//    23 - 26      Integer          resSeq     Residue sequence number.
//    27           AChar            iCode      Code for insertion of residues.
//    31 - 38      Real(8.3)        x          Orthogonal coordinates for X in
//                                             Angstroms
//    39 - 46      Real(8.3)        y          Orthogonal coordinates for Y in
//                                             Angstroms
//    47 - 54      Real(8.3)        z          Orthogonal coordinates for Z in
//                                             Angstroms
//    55 - 60      Real(6.2)        occupancy  Occupancy.
//    61 - 66      Real(6.2)        tempFactor Temperature factor.
//    77 - 78      LString(2)       element    Element symbol, right-justified.
//    79 - 80      LString(2)       charge     Charge on the atom.


//    HETATM
//    Overview
//    The HETATM records present the atomic coordinate records for atoms within "non-standard" groups. These records are used for water molecules and atoms presented in HET groups (see http://deposit.pdb.org/public-component-erf.cif).
//
//    Record Format
//
//    COLUMNS     DATA TYPE        FIELD         DEFINITION
//    --------------------------------------------------------------
//     1 - 6      Record name      "HETATM"
//     7 - 11     Integer          serial        Atom serial number.
//    13 - 16     Atom             name          Atom name.
//    17          Character        altLoc        Alternate location indicator.
//    18 - 20     Residue name     resName       Residue name.
//    22          Character        chainID       Chain identifier.
//    23 - 26     Integer          resSeq        Residue sequence number.
//    27          AChar            iCode         Code for insertion of residues.
//    31 - 38     Real(8.3)        x             Orthogonal coordinates for X.
//    39 - 46     Real(8.3)        y             Orthogonal coordinates for Y.
//    47 - 54     Real(8.3)        z             Orthogonal coordinates for Z.
//    55 - 60     Real(6.2)        occupancy     Occupancy.
//    61 - 66     Real(6.2)        tempFactor    Temperature factor.
|#
  
  
  
(define (proc-lst-copy lst)
  (let ([size (length lst)])
    (let loop ([cnt 0]
               [nlst '()])
      (if (equal? cnt size)
          nlst
          (let ([elt (list-ref lst cnt)])
            (if (empty? nlst) 
                (loop (add1 cnt) (list elt))
                (loop (add1 cnt) (append nlst (list elt)))))))))
  
(define ex-string (list 1 2 3 4 5))
(printf "~s: ~s\n"  (length ex-string) (length (proc-lst-copy ex-string)))

(define initialize-mols
  (lambda (pdb-name mol-obj)
    (make-mols pdb-name 
               (list mol-obj))))
  
(define initialize-mol-object
  (lambda (pdb-name chn-name agrp-name agrp-no atom)
    (make-mol-object (string-append pdb-name "_01")
                     #hash((chn-name . (initialize-chain chn-name agrp-name agrp-no atom))))))

(define initialize-chain
  (lambda (chn-name agrp-name agrp-no atom)
    (make-chain chn-name 
                #hash((agrp-name . (initialize-agroup agrp-name agrp-no atom))))))
     
(define initialize-agroup
  (lambda (agrp-name agrp-no atom)
    (make-agroup agrp-name agrp-no #hash((atom-sn . atom)))))


(define list-remove 
  (lambda (lst elm)
    (foldl (lambda (x list) (if (eq? elm x) list (cons x list)))
           '()
           lst)))
  
(define update-mols
  (lambda (mol-s chn-name agrp-name agrp-no atom)
    (let ([lst-mol-obj (mols-lst-mol-object mol-s)])
      (if (empty? lst-mol-obj)
          (let ([mol-obj (initialize-mol-object (mols-name mol-s) chn-name agrp-name agrp-no atom)])
            (if (mol-object? mol-obj) (printf "lst-mol-obj empty->mol-object\n") (printf "lst-mol-obj empty-> non-mol-object\n"))
            (initialize-mols (mols-name mol-s) mol-obj))
          (let* ([len (length lst-mol-obj)])
            (printf "length of mol-list = ~s\n" len)
            (if (null? (list-ref lst-mol-obj 0))
                (let ([mol-obj (initialize-mol-object (mols-name mol-s) chn-name agrp-name agrp-no atom)])
                  (initialize-mols (mols-name mol-s) mol-obj))
                (begin
                  (printf "numberof lst-mol-obj = ~s\n" (length lst-mol-obj))
                  (if (mol-object? (list-ref lst-mol-obj 0)) (printf "mol-object type\n") (printf "not mol-object-type\n"))
                (for/first ([mol-obj lst-mol-obj]
                            #:when (equal? (mol-object-name mol-obj) (string-append (mols-name mol-s) "_01")))
                  (let* ([new-mol-obj (update-mol-object mol-obj chn-name agrp-name agrp-no atom)]
                         [lst-elmtd (list-remove lst-mol-obj mol-obj)]
                         [new-lst-mol-obj (begin (printf "size = ~s\n" (length lst-elmtd)) (append lst-elmtd (list new-mol-obj)))])
                    (make-mols (mols-name mol-s) new-lst-mol-obj))))))))))

(define update-mol-object 
  (lambda (mol-obj chn-name agrp-name agrp-no atom)
    #f))
  
(define substring-trim
  (lambda (ln start stop)
    (string-trim (substring ln start stop))))
    
(define atom-name->type
  (lambda (name)
    #f))
  
(define read-line-elements
  (lambda (ln)
    (let ([record-name (substring-trim ln 0 6)])
      (if (equal? record-name "ATOM")
       ;(or (equal? record-name "ATOM") ;; currently ATOM is supported only.
       ;(equal? record-name "HETATM"))
          (let ([sn (substring-trim ln 6 11)]
                [name (substring-trim ln 12 16)]
                [agrp-name (substring-trim ln 17 20)]
                [chn-name (substring-trim ln 21 22)]
                [agrp-no (substring-trim ln 22 26)]
                [pos-x (string->number (substring-trim ln 30 38))]
                [pos-y (string->number (substring-trim ln 38 46))]
                [pos-z (string->number (substring-trim ln 46 54))])
            (values chn-name 
                    agrp-name 
                    agrp-no
                    (make-atom sn name (atom-name->type name) (make-point3 pos-x pos-y pos-z))))
          (values null null null null))
      )))

  
(define process-lst-line
  (lambda (pdb-name lst-line)
    (let* ([size (length lst-line)])
      (let process-line ([cnt 0]
                         [mols (make-mols pdb-name '())])
      (if (equal? cnt size)
          mols
          (let ([pdb-ln (list-ref lst-line cnt)])
            (let-values ([(chn-name agrp-name agrp-no atom) (read-line-elements pdb-ln)])
              (let ([nmols (update-mols mols chn-name agrp-name agrp-no atom)])
                (process-line (add1 cnt) nmols)))))))))
  
(define read-pdb-stream
  (lambda (in-stream)
    (let next-line ([lns '()])
      (let ([ln (read-line in-stream 'linefeed)])
        (if (eof-object? ln)
            lns
            (if (empty? lns)
                (next-line (list ln))
                (next-line (append lns (list ln)))))))))

(define read-file-pdb
  (lambda (filename)
    (let* ((in-pdb (open-input-file filename)) ; input-port
           (lst-ln 'empty)
	   (pdb (read-pdb-stream in-pdb))) ;; pdb is list of strings(each lines)
      (close-input-port in-pdb)
      (unless (empty? pdb)
        (process-lst-line (file-name->file-id filename) pdb))
      )))
 
(define print-pdb
  (lambda (pdb)
    (printf "~s....\n" pdb)))

;; real file parsing
(define main-obj
  (lambda ()
    (let ([pdb (read-file-pdb "./1UZV.pdb")]) 
      #f
      )))

(main-obj)
)