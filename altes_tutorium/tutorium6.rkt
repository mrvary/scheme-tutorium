#lang racket
; tutorium 6

; Aufgabe T2.G1.A1
(define (turn zahl)
  (define (iter old new)
    (if (= old 0)
        new
        (iter (quotient old 10) (+ (* 10 new) (remainder old 10)))))
  (iter zahl 0))

(define (bearbeiten zahl teiler)
  (define (iter ergebnis zahl)
    (if (= zahl 0)
        (turn ergebnis)
        (iter (+ (modulo (remainder zahl 10) teiler) (* 10 ergebnis)) (quotient zahl 10) )))
    (if (or (<= 0) (> teiler 10))
        -1 10)
    (iter 0 zahl))

; Aufgabe T2.G1.A1

(define praedikatTest
  (lambda (x) (= x 1)))
           
(define (zaehle start ende praedikat)
  (define (zahl-iter ergebnis rest)
  (cond ((< rest 10) (if (praedikat rest)
                         (+ 1 ergebnis)
                         ergebnis))
        (else (zahl-iter (if (praedikat (modulo rest 10))
                         (+ 1 ergebnis)
                         ergebnis)
                         (floor (/ rest 10))))))
  (define (zaehle-iter start ergebnis)
    (if (> start ende)
        ergebnis
        (zaehle-iter (+ 1 start)
                     (+ ergebnis (zahl-iter 0 (abs start))))))
  (zaehle-iter start 0))
    
        
        
  