#lang racket
; ----------- CONVENIENCE-METHODE (kann ignoriert werden)
(define (>> text)
  (begin
    (newline)
    (display text)
    (newline)))

; CODESNIPPETS ------------------------------------------ ;

; Beispiele für cond / if
; A)
(>> "Beispiel A)")

(define wetter-ist-gut? #t)

(if wetter-ist-gut?
    "Flip-Flops"
    "Regenschirm")

; B)
(>> "Beispiel B)")

(if (= 4 10)
    14
    0)

(cond ((= 2 3) 5)
      ((= (* 2 4) 100) 99)
      (else 2))

; C)
(>> "Beispiel C)")

(define praedikat
  (< 4 10))

(define trifft-zu
  "Zahl ist kleiner als 10")

(define trifft-nicht-zu
  "Zahl ist nicht kleiner als 10")

; in der if-Form
(if praedikat
    trifft-zu
    trifft-nicht-zu)

; identischer Inhalt in der cond-Form, wobei ...
(cond (praedikat trifft-zu)
      (else trifft-nicht-zu))
  
; ... die cond-Form vor allem bei mehreren praedikaten nützlich ist
(define praedikat1 #f)
(define praedikat2 (< 2 5))
(cond (praedikat1 trifft-zu)
      (praedikat2 trifft-zu)
      (else trifft-nicht-zu))

; LÖSUNGEN ZU DEN AUFGABEN --------------------------------- ;

; Aufgabe 1 (Einrückung durchaus nach gusto..)
(>> "Aufgabe 1:")
(/ (+ 5 4 (- 2
             (- 3
                (+ 6 4/5))))
   (* 3
      (- 6 2)
      (- 2 7)))

; Aufgabe 2
(define (flaecheninhalt a r)
  (- (* a a) (* pi r r)))

(>> "Aufgabe 2:")
(flaecheninhalt 10 2) ; => 87,43..

; Aufgabe 3
(define (addiere-2 a b)
  (if (> (+ a b) 10)
      -1
      (+ a b)))

(>> "Aufgabe 3:")
(addiere-2 5 6) ; => -1
(addiere-2 3 3) ; => 6

; Aufgabe 4
(define (verrechne a b c)
  (if (< (- a b c) 0)
      (abs (- a b c))
      (- a b c)))

(>> "Aufgabe 4:")
(verrechne 22 5 5) ; => 12
(verrechne 1 10 3) ; => 12