(define (encrypt msg n e block)
	    (numencrypt (translate msg block) n e) )

(define (translate msg block) (let ((len (length msg)));;divides msg into segments to translate into numbers
  (cond ((null? msg) '())
	((< len block) (translate msg (- block 1)))
	((= (remainder len block) 0) (cons (helptrans (reverse (nthcdr (- len block) (reverse msg))) block)
						    (translate (nthcdr block msg) block)) )
	(else (cons (helptrans (reverse (nthcdr (- len (remainder len block)) (reverse msg))) (remainder len block))
	       (translate (nthcdr (remainder len block) msg) block)) ) ) ) )

(define (helptrans lst block) ;does the actual translating into numbers
  (cond ((null? lst) 0)
	(else (+ (* (expt 100 (- block 1) ) (position (car lst) alphabet))
	   (helptrans (cdr lst) (- block 1)) )) ) )

(define (numencrypt msg n e)
  (if (null? msg) msg (cons (remainder (expt (car msg) e) n) (numencrypt (cdr msg) n e)) ) )
  
(define (decrypt msg p q e block)
  (turntolet (numencrypt  msg (* p q) (inexact->exact (inverse e (* (- p 1) (- q 1)))) ) block) )

(define (test msg p q e block)
  (decrypt (encrypt msg (* p q) e block) p q e block))

(define (turntolet msg block)
  (if (null? msg) msg (append (turnhelp (car msg) block) (turntolet (cdr msg) block))) )

(define (turnhelp msg block)
  (if (= msg 0) '() (cons (list-ref alphabet (quotient msg (expt 100 (- block 1))))
			  (turnhelp (remainder msg (expt 100 (- block 1))) (- block 1))) ) )

(define (inverse e k)
    (if (= (gcd e k) 1)
	(helpin e 0 1 k 1 0)
	(error "bad E") ) )

(define (helpin root row1 row2 n row21 row22)
  (let* ((n1 (+ (* root row1) (* n row21)))
	(n2 (+ (* root row2) (* n row22)))
	(q (- (floor (/ n1 n2)))) )
    (cond ((> n2 n1) (helpin root row2 row1 n row22 row21))
	  ((= n2 1) (if (> row2 0) row2 (+ row2 n)) )
	  (else (helpin root row2 (+ row1 (* row2 q)) n row22 (+ row21 (* row22 q))) ) ) ) )
    
(define (gcd x y)
  (cond ((= x 0) y)
	((= y 0) x)
	((= x y) x)
	((< x y) (gcd y x))
	(else (gcd y (remainder x y))) ) )

(define ALPHABET '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
