
(import (chicken io))
(import (chicken string))
(import srfi-1)

(define inputfile (open-input-file "input.txt"))
(define lines (read-lines inputfile))

(define line (list-ref lines 0))
(define sline (string-split line " "))
(define pattern (string->list (car sline)))
(define seq (map string->number (string-split (cadr sline) ",")))

(define (is-bind seq) (and (not (null? seq)) (list? (car seq))))
(define (start-bind seq) (cons `(bind ,(- (car seq) 1)) (cdr seq)))
(define (dec-bind seq) (cons `(bind ,(- (cadar seq) 1)) (cdr seq)))
(define (is-zero-bind seq) (eq? (cadar seq) 0))

(define (num-soln pat seq tot)
  ;(print `(num-soln ,pat, seq))
  (if (null? pat) 
    (if (or (null? seq) (equal? seq '((bind 0)))) 1 0)
  (case (car pat)
    ((#\.) (if (is-bind seq) 
	     (if (is-zero-bind seq) (num-soln (cdr pat) (cdr seq) tot) 0)
	     (num-soln (cdr pat) seq tot)))
    ((#\#) (cond ((null? seq) 0)
		 ((is-bind seq) 
		   (if (is-zero-bind seq) 0
		     (num-soln (cdr pat) (dec-bind seq) tot)))
	    	 (else (num-soln (cdr pat) (start-bind seq) tot))))
    ((#\?) (+ (num-soln (cons #\. (cdr pat)) seq tot)
	      (num-soln (cons #\# (cdr pat)) seq tot))))))

(define (process line)
  (let* ((sline (string-split line " "))
	(pattern (string->list (car sline)))
	(seq (map string->number (string-split (cadr sline) ","))))
    (num-soln pattern seq 0)))

(print (fold + 0 (map process lines)))

(define (repeat-pat pat n)
  (if (eq? n 1) pat
  (append pat '(#\?) (repeat-pat pat (- n 1)))))

(define (repeat-seq seq n)
  (if (eq? n 0) '()
  (append seq (repeat-seq seq (- n 1)))))

(define (process2 line)
  (let* ((sline (string-split line " "))
	(pattern (string->list (car sline)))
	(seq (map string->number (string-split (cadr sline) ",")))
    	(pattern5 (repeat-pat pattern 5))
	(seq5 (repeat-seq seq 5)))
    (num-soln pattern5 seq5 0)))

;(print (repeat-pat pattern 5))
;(print (repeat-seq seq 5))
;(print (fold + 0 (map process2 lines)))
