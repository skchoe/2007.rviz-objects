(module pdb-struct mzscheme
  (require (lib "mred.ss" "mred")
           "../math/def.scm")
  
  ;;---------------------------------------------------------
  ;; structure for pdb type : originated from pdb file - domain data side
  (provide 
   (all-defined))
;   (struct atom (sn name type pos))
;   (hash-table atom-table (sn atom))
;   (struct agroup (name no atom-table))
;   (hash-table agroup-table (no agroup))
;   (struct chain (name agroup-table))
;   (hash-table chain-table (name chain))
;;  (struct mol-object (name chain-table)) ; name is from molecule ID(filename)
   
#|
   atom:sn name type pos

   atom-table (sn, atom)


   agroup:name no atom-table

   agroup-table (no agroup)


   chain: name agroup-table

   chain-table (name chain)

   
   mol-object (name chain-table)

   
   mols: name (list mol-object ...)
|#
  ;--------container for obj file content
  ; sn: serial number(integer)
  ; name: chemical name in periodic table
  ; type: atom-type in residue(string)
  ; pos: position (point3)
  (define-struct atom (sn name type pos))
  
;   (hash-table atom-table (sn atom))
  ; name: name of residue (ligand, water) (string)
  ; no: number in file "
  ; atom-table: set of atom(hash-table)
  (define-struct agroup (name no atom-table))

  ;(hash-table agroup-table (no agroup))
  ; name: chain name A B C ..
  ; agroup-table: set of agroup(hash-table)
  (define-struct chain (name agroup-table))

  ;(hash-table chain-table (name chain))
  ; name is from molecule ID(filename) (string)
  ; chain-table: set of chain(hash-table)
  (define-struct mol-object (name chain-table)) 
  
  ; mols
  ; name : filename without extension
  ; lst-mol-object:list of mol-object
  (define-struct mols (name lst-mol-object))
)