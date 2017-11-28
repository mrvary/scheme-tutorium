#lang racket
; ----------- CONVENIENCE-METHODE (kann ignoriert werden)
(define (>> text)
  (begin
    (newline)
    (display text)
    (newline)))

; ----------- KLAUSURAUFGABEN ----------------------- ;
; Aufgabe T2.1
(define (vergleich zahl op)
  (if (< zahl 10)
      #f
      (vergleich-iter zahl op 0)))

(define (vergleich-iter zahl op ergebnis)
  (let* ((first (remainder zahl 10))
         (rest (quotient zahl 10))
         (second (remainder rest 10)))
    (cond ((< zahl 10) ergebnis)
          ((op second first) (vergleich-iter rest op (+ ergebnis 1)))
          (else (vergleich-iter rest op ergebnis)))))

(>> "Aufgabe T2.1")
(vergleich 112233 <)
(vergleich 112233 >)

; Aufgabe T2.2
(require math/number-theory)
(define (primzahlen f)
  (if (prime? (f 0))
      (primzahl-iter f 1)
      #f))

(define (primzahl-iter f zähler)
  (if (prime? (f zähler))
      (primzahl-iter f (+ 1 zähler))
      (- zähler 1)))

(>> "Aufgabe T2.2")
(define (f n) (+ (* n n) n 41))
(primzahlen f)

(provide primzahlen vergleich)