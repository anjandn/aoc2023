
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

(define (c< fn arg1) (lambda (arg2) (fn arg1 arg2)))
(define (c<2 fn arg1 arg2) (lambda (arg3) (fn arg1 arg2 arg3)))

(define (transform x . fns)
  ;(print `(transform ,x ,fns))
  (if (null? fns) x (apply transform `(,((car fns) x) ,@(cdr fns)))))
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
  (and (pred (car p)) (pred (cadr p))))

(define (Veq v loc)
  (let ((i (car loc)) (j (cadr loc)))
  (equal? (map Ap (v i)) (map Ap (v j)))))

(define (refl i r)
  `(,i ,(+ (+ (- r i) r) 1)))


(define (is-sym vec r)
  (transform (range (dim vec))
	(c< map (lambda (i) (refl i r)))
	(c< filter (c< pair-c (c< valid vec)))
	(c< map (c< Veq vec))
	all))

(define col-sym (filter (c< is-sym col) (range (- W 1))))
(define row-sym (filter (c< is-sym row) (range (- H 1))))

(if (null? col-sym) (* 100 (+ 1 (car row-sym)))
		    (+ 1 (car col-sym))))

(print (foldl + 0 (map score inps)))
			





