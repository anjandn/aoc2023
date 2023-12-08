
(import (chicken io))

(define inputfile (open-input-file "input.txt"))
(define data (read-lines inputfile))
(define instr (car data))
(define table (cddr data))

(define (key entry) (substring entry 0 3))
(define (left entry) (substring entry 7 10))
(define (right entry) (substring entry 12 15))


(define (lookup keystr table)
  (cond 
    ((string=? keystr (key (car table))) (car table))
    ((null? (cdr table)) '(not found))
    (else (lookup keystr (cdr table)))))

(define (op c) (if (char=? c #\L) left right))

(define len (string-length instr))
(define (next dir)
  (list (modulo (+ 1 (car dir)) len)
        (op (string-ref instr (car dir)))
	(+ 1 (caddr dir))))

(define start-key "AAA")
(define end-key "ZZZ")
(define start-dir '( 0 'blah 0 )) 

(define (move dir key)
  (let* ((entry (lookup key table))
	 (move-op (cadr (next dir)))
	 (new-key (move-op entry))
	 (new-dir (next dir)))
	(if (string=? new-key end-key)
	  (caddr new-dir)
	  (move new-dir new-key))))

(print (move start-dir start-key))


