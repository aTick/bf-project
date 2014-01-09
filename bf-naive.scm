(use-modules (srfi srfi-1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (replace-empty-list lst)
  (cond
   ((and (list? lst) (null? lst))
    '(0))
   (else lst)))

(define (display-bf-arr arr)
  (display (append (reverse (cadr arr)) (list (list (car arr))) (caddr arr)))
  (newline))

(define (get-matching-brace-indices str)
  ;; Preprocess the input to determine where we should jump in the input when looping on '[' or ']' characters
  (define (filter-other pred lst1 lst2)
    (define (iter lst1 lst2 acc)
      (cond
       ((null? lst1) acc)
       ((pred (car lst1))
	(iter (cdr lst1) (cdr lst2) (cons (car lst2) acc)))
       (else
	(iter (cdr lst1) (cdr lst2) acc))))
    (iter lst1 lst2 '()))

  (define (get-matching-braces str)
    (let loop ((i (1- (string-length str)))
	       (brace-numbers '())
	       (num-braces 0)
	       (brace-stack '()))
      (if (< i 0)
	  brace-numbers
	  (let ((c (string-ref str i)))
	    (cond
	     ((eqv? c #\])
	      (loop (1- i) (cons (1+ num-braces) brace-numbers) (1+ num-braces) (cons (1+ num-braces) brace-stack)))
	     ((eqv? c #\[)
	      (loop (1- i) (cons (car brace-stack) brace-numbers) num-braces (cdr brace-stack)))
	     (else
	      (loop (1- i) (cons 0 brace-numbers) num-braces brace-stack)))))))

  (define (range low high)
    (fold (lambda (x y) 
	    (if (not (null? y))
		(cons (1- (car y)) y)
		(cons x y)))
	  '()
	  (make-list (1+ (- high low)) high)))

  (let* ((matching-nums (get-matching-braces str))
	 (indices (range 0 (1- (length matching-nums)))))
    (map (lambda (x y)
	   (if (not (= x 0))
	       (car
		(filter (lambda (index) (not (= y index)))
			(filter-other (lambda (val) (= val x)) matching-nums indices)))
	       0))
	 matching-nums
	 indices)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for creating and manipulating the tape
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-new-bf-array)
  '(0 (0) (0)))

(define (move-left bf-array)
  (map replace-empty-list
       (let ((current (car bf-array))
	     (less (cadr bf-array))
	     (greater (caddr bf-array)))
	 (list (car less)
	       (cdr less)
	       (cons current greater)))))

(define (move-right bf-array)
  (map replace-empty-list
       (let ((current (car bf-array))
	     (less (cadr bf-array))
	     (greater (caddr bf-array)))
	 (list (car greater)
	       (cons current less)
	       (cdr greater)))))

(define (inc bf-array)
  (cons (1+ (car bf-array)) (cdr bf-array)))

(define (dec bf-array)
  (cons (1- (car bf-array)) (cdr bf-array)))

(define (print-bf bf-array)
  (display (integer->char (car bf-array)))
  bf-array)

(define (read-bf bf-array)
  (let ((ch (read-char (current-input-port))))
    (cond
     ((eof-object? ch)
      0)
     (else
      (cons (char->integer ch)
	    (cdr bf-array))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-bf str)
  (define br-idx (get-matching-brace-indices str))
  (define (eval-bf-iter arr str i)
    ;;(begin (display-bf-arr arr))
    (let ((c (if (<= i (1- (string-length str)))
		 (string-ref str i)
		 "")))
      (cond
       ((eqv? c #\+)
	(eval-bf-iter (inc arr) str (1+ i)))
       ((eqv? c #\-)
	(eval-bf-iter (dec arr) str (1+ i)))
       ((eqv? c #\<)
	(eval-bf-iter (move-left arr) str (1+ i)))
       ((eqv? c #\>)
	(eval-bf-iter (move-right arr) str (1+ i)))
       ((eqv? c #\.)
	(begin
	  (print-bf arr)
	  (eval-bf-iter arr str (1+ i))))
       ((eqv? c #\,)
	(begin
	  (read-bf arr)
	  (eval-bf-iter arr str (1+ i))))
       ((eqv? c #\[)
	(if (= (car arr) 0)
	    (eval-bf-iter arr str (list-ref br-idx i))
	    (eval-bf-iter arr str (1+ i))
	    ))
       ((eqv? c #\])
	(if (not (= (car arr) 0))
	    (eval-bf-iter arr str (list-ref br-idx i))
	    (eval-bf-iter arr str (1+ i))))
       (else
	arr))))
  (eval-bf-iter (make-new-bf-array) str 0))