
(import (chicken io))
(import (chicken string))
(import srfi-1)

(define inputfile (open-input-file "input.txt"))
(define lines (read-lines inputfile))
(define inps (string-split (car lines) ","))


(define inp (car inps))

(define (hash-red c r)
  ;(print `(hash-red ,r ,c))
  (modulo (* 17 (+ r c)) 256))

(define (hash inp) (foldl hash-red 0 (map char->integer (string->list inp))))

(print (foldl + 0 (map hash inps)))
