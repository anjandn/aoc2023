
(import (chicken io))
(import (chicken string))
(import srfi-1)

(define inputfile (open-input-file "input.txt"))
(define lines (read-lines inputfile))

(define data (list->vector lines))

(define W (string-length (vector-ref data 0)))
(define H (vector-length data))

(define (A x y)
  (if (and (< x W) (>= x 0) (< y H) (>= y 0))
  (string-ref (vector-ref data y) x) 'fail))

(define (Ap loc)
  (A (car loc) (cadr loc)))

;---------- Generic Functions -----------
(define (all arr)
  (foldl (lambda (x acc) (and acc x)) #t arr))

(define (range-rev n)
  (if (eq? n -1) '() (cons n (range-rev (- n 1)))))
(define (range n) (reverse (range-rev (- n 1))))

(define (tackl elem lst)
  (zip (make-list (length lst) elem) lst))

(define (tackr lst elem)
  (zip lst (make-list (length lst) elem)))

(define (cross-product lst1 lst2)
  (if (null? lst1) '() (append
	(tackl (car lst1) lst2)
  	(cross-product (cdr lst1) lst2))))

(define (c< fn arg1) (lambda (arg2) (fn arg1 arg2)))

(define (cum-sum-helper lst tot) 
  (if (null? lst) '() 
  (let ((elem (+ (car lst) tot)))
    (cons elem (cum-sum-helper (cdr lst) elem)))) )
(define (cum-sum lst) (cum-sum-helper lst 0))
;------------------------------------------

(define locs (cross-product (range W) (range H)))

(define (is-galaxy loc) (eqv? (Ap loc) #\#))
(define (not-is-galaxy loc) (not (is-galaxy loc)))

(define (row j)
  (tackr (range W) j))

(define (col i)
  (tackl i (range H)))

(define galaxy-locs (filter is-galaxy locs))
(define (is-empty vec) (all (map not-is-galaxy vec)))

(define (metric vec i)
  (if (is-empty (vec i)) 1000000 1))

(define row-metric (map (c< metric row) (range H)))
(define col-metric (map (c< metric col) (range W)))

(define row-d-vec (list->vector (cum-sum row-metric)))
(define col-d-vec (list->vector (cum-sum col-metric)))
(define (row-d i) (vector-ref row-d-vec i))
(define (col-d i) (vector-ref col-d-vec i))

(define (metric locs)
  (let* ((loc1 (car locs)) (loc2 (cadr locs))
	(x1 (car loc1)) (y1 (cadr loc1))
	(x2 (car loc2)) (y2 (cadr loc2)))
    (+  (abs (- (row-d y1) (row-d y2)))
    	(abs (- (col-d x1) (col-d x2))))))

(print (/ (foldl + 0 (map metric (cross-product galaxy-locs galaxy-locs))) 2))







