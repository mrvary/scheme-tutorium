#lang racket
; Tutorium 5 - Aufgabe 11
(define (ganzzahlige-wurzel? n)
  (if (integer? (sqrt n))
      #t
      #f))

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

(define (primzahl? n)
  (if (= (fakt n) 1.0)
      #t
      #f))

(define (primzahl-tinder zahl)
  (define (p-t-iter counter anzahl letzterTreffer)
    (if (= zahl anzahl)
        letzterTreffer
        (p-t-iter (+ 1 counter)
                  (if (primzahl? counter)
                      (+ 1 anzahl)
                      anzahl)
                  (if (primzahl? counter)
                      counter
                      letzterTreffer))))
  (p-t-iter 2 1 2))

(define (primzahlfinder zahl)
  (define (iter counter anzahl)
    (if (= zahl anzahl)
        (- counter 1)
        (iter (+ 1 counter)
              (if (primzahl? counter)
                  (+ 1 anzahl)
                  anzahl))))
                  
  (iter 3 1))

(primzahlfinder 1)
(primzahlfinder 2)
(primzahlfinder 4)
(primzahlfinder 100)

