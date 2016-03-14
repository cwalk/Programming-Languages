;#lang racket
#lang slideshow
(require racket/list)

;Clayton Walker
;COP 4020-Programming Languages
;HW4
;Due 3/30/14

;Part I

;list-sum : Listof(Int) → Int 
(define list-sum
    (lambda (loi)
      (if (null? loi)
0
(+ (car loi)
           (list-sum (cdr loi))))))

;throw-die: randomly generate and return a number between 1 and 6
(define throw-die
  (lambda ()
    (+ 1 (random 6))))

;throw-dice: call throw-die, and return a list of two numbers, the throw.
(define throw-dice
  (lambda () 
    (list (throw-die) (throw-die))))

;throw-value: call throw-dice, and return the sum of its two numbers
(define throw-value
  (lambda ()
    (list-sum (throw-dice))))

;five “naming” functions: call throw-dice and output either a
;string or a list with the name of that number, 
;with the following associations of number:name
(define namingFunction
  (lambda (sumAdded)
    (cond 
	[(= 7 sumAdded) "-- NATURAL -- "]
	[(= 11 sumAdded) "-- YO -- "]
	[(= 2 sumAdded) "-- SNAKE-EYES -- "]
	[(= 3 sumAdded) "-- ACE-DEUCE -- "]
	[(= 12 sumAdded) "-- BOXCARS -- "]
	(else ""))))

;win: either accept throw or call throw-dice and call throw-value 
;and output either a string or a list declaring "you won!"
(define win
  (lambda (sumAdded)
    (if (or (= 7 sumAdded) (= 11 sumAdded)) 
	"You win!""")))

;lose: either accept throw or call throw-dice and call throw-value 
;and output either a string or a list declaring "you lost!"
(define lose
  (lambda (sumAdded)
    (if (or (= 2 sumAdded) (= 3 sumAdded) (= 12 sumAdded))
	"You lose.""")))

;dice1
(define dice1
  (lambda ()
    (let ([dice (throw-dice)]) dice                 
      (printf "~v~%You threw ~s and ~s which makes a point of ~s ~a~a~a"
	(picgen (list-ref dice 0) (list-ref dice 1))(list-ref dice 0)
        (list-ref dice 1)(list-sum dice)(namingFunction (list-sum dice))(win (list-sum dice))
         (lose (list-sum dice))))))


;Part II

;-----------SAMPLE INVOCATIONS-------------
;You must call dice2 and provide 2 lists, first a list of winners, and then
;a list of losers. An example of this would be:

; (dice2 '(1 2 3 4 5) '(6 7 8 9 10))

;This would make any roll of 1-5 a winning roll, 6-10 a losing roll, and 11 and 12
;neither.

;More examples: 

;Even winners, Odd losers
;(dice2 '(2 4 6 8 10 12) '(1 3 5 7 9 11)) 

;2 3 and 12 are winners, 7 and 11 are losers, everything else regular. 
;(this is opposite from the dice1 example)
; (dice2 '(2 3 12) '(7 11))


;winners: either accept throw or call throw-dice and call throw-value 
;and output either a string or a list declaring "you won!"
(define winners
  (lambda (loi sumAdded)
    (if (member sumAdded loi)
	"You win!""")))

;losers: either accept throw or call throw-dice and call throw-value 
;and output either a string or a list declaring "you lost!"
(define losers
  (lambda (loi sumAdded)
    (if (member sumAdded loi)
	"You lose.""")))

;dice 2
(define dice2
  (lambda (winnerList loserList)
	(let ([dice (throw-dice)]) dice                  
          (printf"~v~%You throw ~s and ~s which makes a point of ~s ~a~a~a"
                 (picgen (list-ref dice 0) (list-ref dice 1))
                 (list-ref dice 0)(list-ref dice 1)(list-sum dice)
                 (namingFunction (list-sum dice))(winners winnerList (list-sum dice))
                 (losers loserList (list-sum dice))))))
  

;Part III
;GUI Implementation
(define c (colorize (disk 25) "black"))
(define c50 (colorize (disk 50) "black"))
(define r (rectangle 100 100))

(define one
  (lambda ()
    (pin-over r 25 25 c50)))

(define two
  (lambda ()
    (pin-over(pin-over r 37.5 10 c) 37.5 65 c)))

(define three
  (lambda ()
    (pin-over(pin-over(pin-over r 37.5 37.5 c) 10 10 c) 65 65 c)))

(define four
  (lambda ()
    (pin-over(pin-over(pin-over
      (pin-over r 10 10 c) 10 65 c) 65 10 c) 65 65 c)))

(define five
  (lambda ()
    (pin-over(pin-over(pin-over(pin-over
      (pin-over r 37.5 37.5 c) 10 10 c) 65 10 c) 10 65 c) 65 65 c)))

(define six
  (lambda ()
    (pin-over(pin-over(pin-over(pin-over(pin-over
      (pin-over r 10 5 c) 10 37.5 c) 10 70 c) 65 5 c) 65 37.5 c) 65 70 c)))

(define pict
  (list one two three four five six))

(define picgen
  (lambda (v1 v2)
    (hc-append 100 ((list-ref pict (- v1 1))) ((list-ref pict (- v2 1))))))
