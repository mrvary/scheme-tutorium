#lang racket
; Tutorium #6

; ----------- CONVENIENCE-METHODE (kann ignoriert werden)
(define (>> text)
  (begin
    (newline)
    (display text)
    (newline)))

; AUFGABEN --------------------------------------- ;

; Klausuraufgaben
; - zahl modulieren Nr. 18
; - palindrom? Nr. 14
; - einstellige quersumme Nr. 16
; - vergleich  T2.1
; - primzahlen T2.2

; Aufgabe 20
(define (r n a)
  (if (< n 0)
      0
      (r-rek n a)))

(define (r-rek n a)
  (if (< n 0)
      0
      (+ (a n) (r-rek (- n 1) a))))

(>> "Aufgabe 18")
(define (a i) i)
(r 10 a)


