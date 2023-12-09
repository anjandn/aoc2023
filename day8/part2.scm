
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

(define start-dir '( 0 'blah 0 )) 

(define (is-start-key key) (char=? (string-ref key 2) #\A))
(define (is-end-key key)   (char=? (string-ref key 2) #\Z))

(define (filter-starts table starts)
  (cond 
    ((is-start-key (key (car table))) 
     (filter-starts (cdr table) 
		    (cons (list (key (car table)) start-dir) starts)))
    ((null? (cdr table)) starts)
    (else (filter-starts (cdr table) starts))))

(define (move-one-step keydir)
  (let* ((key (car keydir))
	 (dir (cadr keydir))
	 (entry (lookup key table))
	 (move-op (cadr (next dir)))
	 (new-key (move-op entry))
	 (new-dir (next dir)))
	  (list new-key new-dir)))

(define all-starts (filter-starts table '()))

(define (num-step-to-end fn x)
  (let* ((end-bool (is-end-key (car x)))
        (cnt (caddr (cadr x))))
  (if end-bool cnt (num-step-to-end fn (fn x)))))

(define (period st) (num-step-to-end move-one-step st))
(print (apply lcm (map period all-starts)))

