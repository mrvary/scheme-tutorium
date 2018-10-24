#lang racket
; Algorithmik Übungsblatt Nr. 4
; Aufgabe Nr. 15

(define (ganzzahlige-wurzel? n)
  (if (integer? (sqrt n))
      #t
      #f))

;(ganzzahlige-wurzel? 25)
;(ganzzahlige-wurzel? 24)

; Aufgabe Nr. 16
(define (fakt n)
  (define (ganzzahlige-wurzel? n)
  (if (integer? (sqrt n))
      #t
      #f))
  (define (fakt-iter a b-quadrat)
    (if (ganzzahlige-wurzel? b-quadrat)
        (- a (sqrt b-quadrat))
        (fakt-iter (+ a 1)
                   (- (* (+ a 1)(+ a 1)) n))))
  (if (even? n)
      2
      (fakt-iter (ceiling (sqrt n))
                 (- (* (ceiling (sqrt n))
                       (ceiling (sqrt n))) n))))

;(fakt 2183)
;(fakt 25)
;(fakt 100)
;(fakt 11)

; Aufgabe Nr. 17
(define (primzahl? n)
  (if (= (fakt n) 1.0)
      #t
      #f))
  

;(primzahl? 11)
;(primzahl? 26737)
;(primzahl? 200)
;(primzahl? 121)

; Aufgabe Nr. 18

(define (kubiksumme n)
  (define (quersumme-iter zahl summe)
    (if (< zahl 10)
        (+ summe zahl)
        (quersumme-iter (quotient zahl 10)
                        (+ summe (remainder zahl 10)))))
  (define (kubik x) (* x x x))
  (kubik (quersumme-iter n 0)))

;(kubiksumme 101042)
;(kubiksumme 34567)

; Aufgabe Nr. 19
(define (caesar_encrypt n k)
  (define (caesar-iter rest counter sum)
    (if (= rest 0)
        sum
        (caesar-iter (quotient rest 10)
                     (+ counter 1)
                     (+ sum (* (modulo (+ (remainder rest 10) k) 10)
                               (expt 10 counter))))))
  (caesar-iter n 0 0))

;(caesar_encrypt 1234 1)
;(caesar_encrypt 7901 2)
;(caesar_encrypt 987 1)







    