#lang racket

;Aufgabe 5
(define (aflaeche a r)
  (define flaeche-quadrat
    (* a a))
  (define flaeche-kreis
    (* pi r r ))
  (- flaeche-quadrat flaeche-kreis))

(define (flaechenberechnung radius laenge)
  (- (* laenge laenge) (* pi radius radius)))

;Aufgabe 6
(define (berechnePruefziffer zahl1 zahl2 zahl3)
  (- 98 (modulo (+ (* zahl1 (expt 10 16))
      (* zahl2 (expt 10 8))
      zahl3) 97)))

(define (berechnePruefziffer2 zahl1 zahl2 zahl3)
  (define gesamtzahl
    (+ (* zahl1 (expt 10 16))
        (* zahl2 (expt 10 8))
        zahl3))
  (- 98 (modulo gesamtzahl 97)))

;Aufgabe 7
(define (bmi gewicht groesse)
  (define wert
    (/ gewicht (* groesse groesse)))
  (string-append 
  (cond ((< wert 18.5) "Untergewicht: ")
        ((< wert 30) "Normalgewicht: ")
        (else "Stark adipos mit: ")) (~a wert))) 


;Aufgabe 9
