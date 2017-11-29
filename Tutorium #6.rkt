#lang racket
; Tutorium #6

; ----------- CONVENIENCE-METHODE (kann ignoriert werden)
(define (>> text)
  (begin
    (newline)
    (display text)
    (newline)))

; AUFGABEN --------------------------------------- ;
(define (modulieren-rek zahl teiler)
  (let [[letzte (remainder zahl 10)]
        [rest (quotient zahl 10)]]
    (if (= zahl 0)
        0
        (+ (modulo letzte teiler)
           (* 10 (modulieren-rek rest teiler))))))

(define (zahl-modulieren zahl teiler)
  (if (or (< zahl 0)
          (<= teiler 0)
          (> teiler 10)) 
       -1
       (modulieren-rek zahl teiler)))

(>> "Aufgabe 18")
(zahl-modulieren 432 3)
(zahl-modulieren 687 6)

; Aufgabe T2.5
; Variante 1
(define (kleinste-iter a b n)
  (if (> (* a n) b)
      n
      (kleinste-iter a b (+ n 1))))

(define (groesste-iter a b n)
  (if (>= (* a n) b)
      (- n 1)
      (groesste-iter a b (+ n 1))))

(define (finde-n a b)
  (cond ((or (<= a 0)
             (<= b 0)) 0)
        ((> a b) 1)
        ((even? a) (kleinste-iter a b 1))
        ((odd? a) (groesste-iter a b 1))))

; Variante 2
(define (finde-iter a b n op)
  (if (op (* a n) b)
      (if (op 1 1) (- n 1) n)
      (finde-iter a b (+ n 1) op)))

(define (finde-n2 a b)
  (cond ((or (<= a 0)
             (<= b 0)) 0)
        ((> a b) 1)
        ((even? a) (finde-iter a b 1 >))
        ((odd? a) (finde-iter a b 1 >=))))

(>> "Aufgabe 19")
(= (+ (finde-n 4 0)
      (finde-n 0 5)
      (finde-n -4 5)
      (finde-n 5 5))
   0)
(finde-n 5 20)
(finde-n 16 100)
(finde-n 3 5)
