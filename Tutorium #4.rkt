#lang racket
; Tutorium #4
(require math/number-theory)

; Aufgabe 12
(define (sdq-rek n)
  (if (= n 1)
      1
      (+ (* n n) (sdq-rek (- n 1)))))

(define (sdq-iter n ergebnis)
  (if (= n 1)
      (+ 1 ergebnis)
      (sdq-iter (- n 1) (+ ergebnis (* n n)))))

; Hier wird die rekursive Variante aufgerufen,
; falls es iterativ sein soll, in der letzten
; Zeile (sdq-rek n) mit (sdq-iter n 0) ersetzen.
(define (summe-der-quadrate n)
  (if (< n 1)
      0
      (sdq-rek n)))

;(summe-der-quadrate 6)
;(summe-der-quadrate 6)

; Aufgabe 13
(define (qds-rek n)
  (define (summe n)
    (if (= n 1)
        1
        (+ n (summe (- n 1)))))
  (expt (summe n) 2))

(define (qds-iter n ergebnis)
  (if (< n 1)
      (expt ergebnis 2)
      (qds-iter (- n 1) (+ ergebnis n))))

; Hier wird die iterative Variante aufgerufen,
; falls es rekursiv sein soll, in der letzten
; Zeile (qds-iter n 0) mit (qds-rek n) ersetzen.
(define (quadrat-der-summe n)
  (if (< n 1)
      0
      (qds-iter n 0)))

;(quadrat-der-summe 3)
;(quadrat-der-summe 5)

; Aufgabe 14
(define (drehe-iter zahl ergebnis)
  (if (< zahl 10)
      (+ (* 10 ergebnis) zahl)
      (drehe-iter (quotient zahl 10) 
                  (+ (* 10 ergebnis)
                     (remainder zahl 10)))))

(define (drehe zahl)
  (drehe-iter zahl 0))

(define (palindrom n)
  (= n (drehe n)))

;(palindrom 9889)
;(palindrom 7610)

; Aufgabe 15
(define (finde-primzahl n)
  (define (iter counter anzahl)
    (if (= n anzahl)
        (- counter 1)
        (iter (+ 1 counter)
              (if (prime? counter)
                  (+ 1 anzahl)
                  anzahl))))                
  (iter 3 1))

;(finde-primzahl 1)
;(finde-primzahl 2)
;(finde-primzahl 4)
;(finde-primzahl 100)


                             
                             
                             