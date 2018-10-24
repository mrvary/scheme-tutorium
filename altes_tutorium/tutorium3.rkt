#lang racket
; Tutorium 3 - verbessern der Aufgaben aus Tut1

; Aufgabe 1 - korrekt
(define (sub-korrekt zahl1 zahl2 zahl3)
  (define summe (- zahl1 zahl2 zahl3))
  (if (< summe 0)
     (* summe -1)
     summe))

; Aufgabe 2 - korrekt
(define (durchschnittsRechner zahl1 zahl2 zahl3 zahl4 zahl5)
  (define summe (/ (+ zahl1 zahl2 zahl3 zahl4 zahl5) 5))
  (/ (round (* summe 100.0)) 100.0))

; Aufgabe 3 - korrekt
(define (groessenVergleich input1 input2 input3)
  (define zahl1 (* input1 input1))
  (define zahl2 (* input2 input2))
  (define zahl3 (* input3 input3))
  (cond ((and (> zahl1 zahl2) (> zahl1 zahl3)) zahl1)
        ((and (> zahl2 zahl1) (> zahl2 zahl3)) zahl2)
        (else zahl3)))

; Aufgabe 4 - korrekt
(define (flaechenberechnung radius laenge)
  (define summe (- (* laenge laenge) (* radius radius pi)))
  (if (or (< laenge (* 2 radius)) (< radius 0) (< laenge 0))
      #f
      summe))

; Aufgabe 5 - korrekt
(define (ibanRechner block1 block2 block3)
	(- 98 (modulo (+ (* 62 block1) (* 81 block2) block3) 97)))

; Aufgabe 6 - korrekt
(define (bmiRechner groesse gewicht)
  (define normal "Normalgewicht")
  (define unter "Untergewicht")
  (define ueber "Übergewicht")
  (define bmi (/ gewicht (* groesse groesse)))
  (cond ((< bmi 20) unter)
        ((and (<= bmi 25) (>= bmi 20)) normal)
        ((> bmi 25) ueber)))
