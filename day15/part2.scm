
(import (chicken io))
(import (chicken string))
(import srfi-1)

(define inputfile (open-input-file "input.txt"))
(define lines (read-lines inputfile))
(define inps (string-split (car lines) ","))

(define inp (car inps))

(define (hash-red c r)
  (modulo (* 17 (+ r c)) 256))

(define (hash inp) (foldl hash-red 0 (map char->integer (string->list inp))))

(print (foldl + 0 (map hash inps)))
(define (label lens) (car (string-split lens "=-")))
(define (focus lens) (string->number (cadr (string-split lens "=-"))))
(define (op lens) (if (substring-index "=" lens) "=" "-"))

(define (add-lens prev rem lens)
  (if (null? rem) (append prev (list lens))
  (if (string=? (label (car rem)) (label lens)) 
    (append prev (list lens) (cdr rem))
    (add-lens (append prev (list (car rem))) (cdr rem) lens))))

(define (remove-lens prev rem lens)
  (if (null? rem) prev
  (if (string=? (label (car rem)) (label lens)) 
    (append prev (cdr rem))
    (remove-lens (append prev (list (car rem))) (cdr rem) lens))))

(define (proc instr box)
  (if (string=? (op instr) "=") (add-lens '() box instr)
    (remove-lens '() box instr)))

(define boxes (make-vector 256 '()))

(define (add-instr instr)
  (let ((hsh (hash (label instr))))
    (vector-set! boxes hsh (proc instr (vector-ref boxes hsh)))))

(map add-instr inps)

(define (range-rev n)
  (if (eq? n -1) '() (cons n (range-rev (- n 1)))))
(define (range n) (reverse (range-rev (- n 1))))

(define (lensfn e) (* (+ 1 (car e)) (cadr e))) 

(define (score idx)
  (let ((box (vector-ref boxes idx)))
    (* (+ 1 idx) (foldl + 0 (map lensfn (zip (range (length box)) (map focus box)))))))

(print (foldl + 0 (map score (range 256))))






