
(import (chicken io))
(import (chicken string))
(import srfi-1)

(define inputfile (open-input-file "input.txt"))
(define lines (read-lines inputfile))

(define (skip lns)
  (if (null? lns) '()
  (if (string=? (car lns) "") (cdr lns)
    (skip (cdr lns)))))

(define (next lns)
  (if (or (null? lns) (string=? (car lns) "")) '()
    (cons (car lns) (next (cdr lns)))))

(define (parse lns)
  (if (null? lns) '()
    (cons (next lns) (parse (skip lns)))))

(define inp (next lines))
(define inps (parse lines))


;---------- Generic Functions -----------
(define (valid x) (not (equal? x 'fail)))

(define (? fn . x)
  (if (all (map valid x)) (apply fn x) 'fail))

(define (all arr)
  (foldl (lambda (x acc) (and acc x)) #t arr))

(define (range-rev n)
  (if (eq? n -1) '() (cons n (range-rev (- n 1)))))
(define (range n) (reverse (range-rev (- n 1))))

(define (tackl elem lst)
  (zip (make-list (length lst) elem) lst))

(define (tackr lst elem)
  (zip lst (make-list (length lst) elem)))

(define (c fn1 fn2) (lambda (arg1) (fn1 (fn2 arg1))))
(define (c< fn arg1) (lambda (arg2) (fn arg1 arg2)))
(define (c<2 fn arg1 arg2) (lambda (arg3) (fn arg1 arg2 arg3)))

(define (transform x . fns)
  (if (null? fns) x (apply transform `(,((car fns) x) ,@(cdr fns)))))

(define (all-but-one arr)
  (let* ((narr (map (lambda (x) (if x 0 1)) arr))
	 (num-mismatch (foldl + 0 narr)))
    (eq? num-mismatch 1)))
;------------------------------------------
(define (score inp)

(define data (list->vector inp))

(define W (string-length (vector-ref data 0)))
(define H (vector-length data))

(define (A x y)
  (if (and (< x W) (>= x 0) (< y H) (>= y 0))
  (string-ref (vector-ref data y) x) 'fail))

(define (Ap loc)
  (A (car loc) (cadr loc)))

(define (row j)
  (tackr (range W) j))

(define (col i)
  (tackl i (range H)))

(define (dim vec) (if (eq? vec row) H W))

(define (valid vec i)
  (let ((L (dim vec)))
    (and (>= i 0) (< i L))))

(define (pair-c pred p)
  (and (pred (car p)) (pred (cadr p)) (< (car p) (cadr p))))

(define (Veq v loc)
  (let ((i (car loc)) (j (cadr loc)))
  (equal? (map Ap (v i)) (map Ap (v j)))))

(define (peq? p) (eq? (car p) (cadr p)))

(define (almost-Veq v loc)
  (let ((i (car loc)) (j (cadr loc)))
  (all-but-one (map peq? (zip (map Ap (v i)) (map Ap (v j)))))))

(define (refl i r)
  `(,i ,(+ (+ (- r i) r) 1)))

(define (failed-refls vec r) 
  (transform (range (dim vec))
	(c< map (lambda (i) (refl i r)))
	(c< filter (c< pair-c (c< valid vec)))
	(c< filter (c not (c< Veq vec)))
))

(define (refl->r p) (/ (+ (car p) (cadr p) -1) 2 ))

(define (almost-syms vec) 
  (transform (range (- (dim vec) 1))
	(c< map (c< failed-refls vec))
	(c< filter (lambda (x) (eq? (length x) 1)))
	(c< map car)
	(c< filter (c< almost-Veq vec))
	(c< map refl->r)
))

(define col-sym (almost-syms col))
(define row-sym (almost-syms row))

(if (null? col-sym) (* 100 (+ 1 (car row-sym)))
		    (+ 1 (car col-sym)))
)

(print (foldl + 0 (map score inps)))






