
(import (chicken io))
(import (chicken string))
(import srfi-1)
(import srfi-69)

(define inputfile (open-input-file "input.txt"))
(define lines (read-lines inputfile))

(define (is-bind seq) (and (not (null? seq)) (list? (car seq))))
(define (start-bind seq) (cons `(bind ,(- (car seq) 1)) (cdr seq)))
(define (dec-bind seq) (cons `(bind ,(- (cadar seq) 1)) (cdr seq)))
(define (is-zero-bind seq) (eq? (cadar seq) 0))

(define (num-soln pat seq)
  (if (null? pat) 
    (if (or (null? seq) (equal? seq '((bind 0)))) 1 0)
  (case (car pat)
    ((#\.) (if (is-bind seq) 
	     (if (is-zero-bind seq) (cnum-soln (cdr pat) (cdr seq)) 0)
	     (cnum-soln (cdr pat) seq)))
    ((#\#) (cond ((null? seq) 0)
		 ((is-bind seq) 
		   (if (is-zero-bind seq) 0
		     (cnum-soln (cdr pat) (dec-bind seq))))
	    	 (else (cnum-soln (cdr pat) (start-bind seq)))))
    ((#\?) (+ (cnum-soln (cons #\. (cdr pat)) seq)
	      (cnum-soln (cons #\# (cdr pat)) seq))))))

(define (repeat-pat pat n)
  (if (eq? n 1) pat
  (append pat '(#\?) (repeat-pat pat (- n 1)))))

(define (repeat-seq seq n)
  (if (eq? n 0) '()
  (append seq (repeat-seq seq (- n 1)))))

(define cache (make-hash-table))
(define (cget pat seq) (hash-table-ref/default cache `(,pat ,seq) 'fail))
(define (cset pat seq num) (hash-table-set! cache `(,pat ,seq) num))

(define (cnum-soln pat seq)
  (let ((cache-soln (cget pat seq)))
    (if (equal? cache-soln 'fail)
      (let ((fresh-soln (num-soln pat seq)))
	(cset pat seq fresh-soln)
	fresh-soln)
      cache-soln)))

(define n 0)
(define (process2 line)
  (print n " : " line)
  (set! n (+ 1 n))
  (set! cache (make-hash-table))
  (let* ((sline (string-split line " "))
	(pattern (string->list (car sline)))
	(seq (map string->number (string-split (cadr sline) ",")))
    	(pattern5 (repeat-pat pattern 5))
	(seq5 (repeat-seq seq 5)))
    (num-soln pattern5 seq5)))

(print (fold + 0 (map process2 lines)))
