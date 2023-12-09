
(import (chicken io))
(import (chicken string))

(define inputfile (open-input-file "input.txt"))
(define data (read-lines inputfile))

(define (process line) 
  (map string->number (string-split line " ")))

(define (diff-r seq acc)
   (if (null? (cdr seq)) acc 
     (diff-r (cdr seq) (cons (- (cadr seq) (car seq)) acc ))))

(define (diff seq) (reverse (diff-r seq '())))

(define (all-zero seq)
  (cond ((null? seq) #t)
	((eq? (car seq) 0) (all-zero (cdr seq)))
	(else #f)))

(define (rec-diff seq acc)
  (if (all-zero seq) acc 
    (let ((diff-s (diff seq)))
      (rec-diff diff-s (cons diff-s acc)))))

(define (diff-tree seq) (rec-diff seq (list seq))) 

(define (last-elem seq)
  (if (null? (cdr seq)) (car seq) (last-elem (cdr seq))))

(define (predict seq)
  (foldl + 0 (map last-elem (diff-tree seq))))

(define list-of-seqs (map process data))

(print "Part 1: " (foldl + 0 (map predict list-of-seqs)))
(print "Part 2: " (foldl + 0 (map predict (map reverse list-of-seqs))))
