#lang racket
;tutorium #4?
; Aufgabe 8
; überarbeiten .. 
(define (mod zahl modul)
  (define (positiv zahl counter)
    (if (< zahl modul)
        (list counter zahl)
        (positiv (- zahl modul)
                 (+ counter 1))))
  (define (negativ zahl counter)
    (if (or (= 0 zahl)
            (< (* -1 zahl) modul))
        (list counter (+ zahl modul))
        (negativ (+ zahl modul)
                 (- counter 1))))
  (if (< zahl 0)
      (negativ zahl 0)
      (positiv zahl 0)))
           

; Aufgabe 9
(define (quersumme zahl)
  (define (quersumme-iter zahl summe)
    (if (< zahl 10)
        (+ summe zahl)
        (quersumme-iter (quotient zahl 10)
                        (+ summe (remainder zahl 10)))))
  (quersumme-iter zahl 0))
        
; Aufgabe 10
(define (nr10 z1 z2 limit)
  (define (nr10-iter summe zahl1 zahl2 counter)
    (if (> counter limit)
        summe
        (nr10-iter (+ summe (cond ((= (remainder counter (* zahl1 zahl2)) 0) (* 2 counter))
                                  ((or (= (remainder counter zahl1) 0)
                                       (= (remainder counter zahl2) 0)) counter)
                                  (else 0)))
                   zahl1
                   zahl2
                   (+ counter 1))))
  (nr10-iter 0 z1 z2 0))
        
        