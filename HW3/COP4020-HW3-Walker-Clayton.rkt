#lang racket
(require racket/list)
(define colors(list "red" "green" "blue"))
(define digits (list 0 1 2 3 4 5 6 7 8 9))
(define vowels(list "a" "e" "i" "o" "u"))
(define consonants (list "b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" 
                         "s" "t" "v" "w" "x" "y" "z"))
(define mixed(list 1 2 3 4 5 "a" "b" "c" "d" "e" (cons "a" 1) "a bridge too far"))
(define von (vector 1 2 3 45 55 66 98))
(define two-list(list (list 'a 1)(list 'b 2)(list 'c 3)(list 'd 4)))

;Clayton Walker
;COP 4020-Programming Languages
;HW3
;Due 3/20/14

;Part a
;list-length : List → Int
;usage: (list-length l) = the length of l 

(define list-length
           (lambda (lst)
             (if (null? lst)
0
(+ 1 (list-length (cdr lst))))))

;Part b
;nth-element : List × Int → SchemeVal
;usage: (nth-element lst n) = the n-th element of lst 

(define nth-element
    (lambda (lst n)
      (if (null? lst)
(report-list-too-short n) (if (zero? n)
(car lst)
(nth-element (cdr lst) (- n 1))))))
  (define report-list-too-short
    (lambda (n)
(error 'nth-element
"List too short by ~s elements.~%" (+ n 1))))

;Part c
;remove-first : Sym × Listof(Sym) → Listof(Sym) 

(define remove-first
           (lambda (s los)
             (if (null? los)
               '()
               (if (eqv? (car los) s)
(cdr los)
(cons (car los) (remove-first s (cdr los)))))))

;Part d
;occurs-free? : Sym × LcExp → Bool
;usage: returns #t if the symbol var occurs free
;in exp, otherwise returns #f. 

(define occurs-free?
    (lambda (var exp)
      (cond
((symbol? exp) (eqv? var exp)) ((eqv? (car exp) 'lambda)
(and
(not (eqv? var (car (cadr exp)))) (occurs-free? var (caddr exp))))
(else (or
(occurs-free? var (car exp)) (occurs-free? var (cadr exp)))))))

;Part e
;subst : Sym × Sym × S-list → S-list 

(define subst
    (lambda (new old slist)
      (if (null? slist)
'() (cons
(subst-in-s-exp new old (car slist)) (subst new old (cdr slist))))))

;subst-in-s-exp : Sym × Sym × S-exp → S-exp 

(define subst-in-s-exp
    (lambda (new old sexp)
      (if (symbol? sexp)
(if (eqv? sexp old) new sexp) (subst new old sexp))))

;Part f
;number-elements-from : Listof(SchemeVal) × Int → Listof(List(Int,SchemeVal))
;usage: (number-elements-from ’(v0 v1 v2 ...) n) = ((n v0) (n+1 v1) (n+2 v2) ...)
  
(define number-elements-from
    (lambda (lst n)
      (if (null? lst) '()
        (cons
(list n (car lst))
(number-elements-from (cdr lst) (+ n 1))))))

;number-elements : List → Listof(List(Int, SchemeVal)) 

(define number-elements
(lambda (lst) (number-elements-from lst 0)))

;Part g
;list-sum : Listof(Int) → Int 

(define list-sum
    (lambda (loi)
      (if (null? loi)
0
(+ (car loi)
           (list-sum (cdr loi))))))

;Part h
;partial-vector-sum : Vectorof(Int) × Int → Int
;usage: if 0 ≤ n < length(v), then (partial-vector-sumv n)=∑vi
  
(define partial-vector-sum
    (lambda (v n)
      (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
(partial-vector-sum v (- n 1))))))

;vector-sum : Vectorof(Int) → Int i=length(v)−1
;usage: (vector-sum v) = ∑vi i=0
        
(define vector-sum
          (lambda (v)
(let ((n (vector-length v))) (if (zero? n)
0
(partial-vector-sum v (- n 1))))))

;Part i
;(duple n x) returns a list containing n copies of x

(define	duple 
	(lambda (n x)
		(if (and (integer? n) (> n -1))
		   	(if (= n 0) 	'()
					(cons x (duple (- n 1) x)))
			(write "Error, wrong parameters passed")))) 

;Part j
;(sort/predicate pred loi) returns a list of elements sorted by the predicate.

(define (sort element comparator lst)
  (cond
    [(or (empty? lst) (comparator element (first lst))) (cons element lst)]
    [else (cons (first lst) (sort element comparator (rest lst)))]))

(define (sort/predicate comparator lst)
  (cond
    [(empty? lst) empty]
    [else (sort (first lst) comparator (sort/predicate comparator (rest lst)))]))