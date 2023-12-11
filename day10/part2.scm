

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

;----------
(define (valid x) (not (equal? x 'fail)))

(define (& x y) (and x y))
(define (all x) (foldl & #t x))

(define (? fn . x)
  (if (all (map valid x)) (apply fn x) 'fail))
;----------

(define (Ap loc)
  (A (car loc) (cadr loc)))

(define start 0)

(do ((x 0 (+ 1 x))) ((= x W)) 
  (do ((y 0 (+ 1 y))) ((= y H)) 
    (if (eq? (A x y) #\S) (set! start `(,x ,y)))))

(define (add-to-dir loc dx dy)
  (let ((x (car loc))
	(y (cadr loc)))
	   `( ,(+ x dx) ,(+ y dy))))

(define (move dir loc)
    (case dir
      ((up) (add-to-dir loc 0 -1))
      ((down) (add-to-dir loc 0 1))
      ((left) (add-to-dir loc -1 0))
      ((right) (add-to-dir loc 1 0))))

(define (openings c)
  (case c
    ((#\7) '(left down))
    ((#\J) '(up left))
    ((#\|) '(up down))
    ((#\-) '(left right))
    ((#\L) '(right up))
    ((#\F) '(down right))
    ((#\S) (openings c_start))
    (else 'fail)))

(define (eff-dir c)
  (case c
    ((#\7) 'right)
    ((#\J) 'left)
    ((#\L) 'left)
    ((#\F) 'right)
    ((#\S) (eff-dir c_start))
    (else 'fail)))

(define (is-corner c)
  (case c
    ((#\|) #f)
    ((#\-) #f)
    ((#\S) (is-corner c_start))
    (else  #t)))

(define (mapflow dir pipe)
  (cond ((equal? (car pipe) dir) (cadr pipe))
	((equal? (cadr pipe) dir) (car pipe))
	(else 'fail)))

(define (is-outer dir pipe)
  (cond ((equal? (car pipe) dir) 1)
	((equal? (cadr pipe) dir) 0)
	(else 'fail)))

(define (rev dir)
    (case dir
      ((up) 'down)
      ((down) 'up)
      ((left) 'right)
      ((right) 'left)))

(define (flow-into loc dir)
  (? mapflow (rev dir) (? openings (Ap loc))))

(define (left-right-area loc dir)
    (case dir
      ((up) 0)
      ((down) 0)
      ((right) (cadr loc))
      ((left) (- (+ (cadr loc) 1)))))

(define (delta-area loc dir)
  (let* ((c (Ap loc))
 	 (pipe (openings c)))
     (if (is-corner c) 
	(* (is-outer (rev dir) pipe)
	   (left-right-area loc (eff-dir c)) )
	(left-right-area loc dir))))

(define (propel x)
  (let* ((loc (car x))
	 (dir  (cadr x))
	 (dist (caddr x))
	 (area (cadddr x))
	 (new-loc (move dir loc))
	 (new-dir (flow-into new-loc dir))
	 (area-diff (delta-area new-loc dir)))
	`(,new-loc ,new-dir ,(+ 1 dist) ,(+ area area-diff))))

(define all-dirs '(left right up down))

(define (is-valid-dir dir)
  (valid (flow-into (move dir start) dir)))

(define start-dirs (filter is-valid-dir all-dirs))

(print 'start-dirs " " start-dirs)
; bit of cheating : manually choosing start locations char,
; by looking at above print
(define c_start #\|)

(define n (length start-dirs))
(assert (eq? n 2))

(define start-locdirs (zip (make-list n start)
			   start-dirs
			   (make-list n 0)
			   (make-list n 0)))

(define (is-start x)
  (equal? (caar x) start))

(define (propel-till-meet x)
  (if (is-start x) x (propel-till-meet (map propel x))))

(define res (propel-till-meet (map propel start-locdirs)))

(define area1 (abs (cadddr (car res))))
(define area2 (abs (cadddr (cadr res))))
(define dist (caddr (car res)))
(print dist)
(print area1)
(print area2)
(print "Answer: " (min area1 area2))

