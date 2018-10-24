#lang racket

;Übung 2 - Aufgabe 7
;--------------------------------------------------
; Variante 1
(define (nat-wurzel x)
  (define (count-odds x)
    (cond ((= x 1) 1)
          ((odd? x) (+ (count-odds (- x 1)) 1))
          (else (count-odds (- x 1)))))
  (define (sum-odds x)
    (cond ((= x 1) 1)
          ((odd? x) (+ (sum-odds (- x 1)) x))
          (else (sum-odds (- x 1)))))
  (define (wurzel-rek zahl x)
    (cond ((= x 0) "keine Wurzel")
          ((= (sum-odds x) zahl) (count-odds x))
          (else (wurzel-rek zahl (- x 1)))))
  (wurzel-rek x x))

; Variante 2
(define (nat-wurzel2 x)
  (define (wurzel-iter summe summanden zahl zaehler)
    (cond ((= summe zahl) summanden)
          ((> summe zahl) "keine Wurzel")
          ((even? zaehler) (wurzel-iter summe summanden zahl (+ 1 zaehler)))
          (else (wurzel-iter (+ summe zaehler)
                             (+ 1 summanden)
                             zahl
                             (+ 1 zaehler)))))
  (wurzel-iter 0 0 x 1))

;Variante 3
(define (nat-wurzel3 x)
  (define (wurzel-iter a b c d)
    (cond ((= a c) b)
          ((> a c) "keine Wurzel")
          ((even? d) (wurzel-iter a b c (+ 1 d)))
          (else (wurzel-iter (+ a d)
                             (+ 1 b)
                             c
                             (+ 1 d)))))
  (wurzel-iter 0 0 x 1))

;Übung 2 - Aufgabe 8
;--------------------------------------------------
(define (zahl-umdrehen eingabe)
  (define (umdrehen-iter zahl ergebnis)
    (if (>= zahl 10) (umdrehen-iter (quotient zahl 10)
                                    (+ (* ergebnis 10)
                                       (modulo zahl 10)))
                     (+ zahl (* ergebnis 10))))
  (umdrehen-iter eingabe 0)) 


;(zahl-umdrehen 123)
 

;Übung 2 - Aufgabe 9
;--------------------------------------------------
(define (aufsteigendes-produkt? a b c d)
  (and (< a b c d)
       (= (* a b c ) d)))

;(aufsteigendes-produkt? 1 2 3 6)
;(aufsteigendes-produkt? 2 1 3 6)
;(aufsteigendes-produkt? 2 3 5 11)


;Übung 2 - Aufgabe 10
;--------------------------------------------------
(define (f1 a b )
  (and (not (or a b))
       (or a b)
       a
       (not b)))

(define (f2 a b c)
  (or a
      (and a b (not c))
      (and (not a) c)     
      (and (not (and a b)) c)))

(define (f3 a b c d)
  (define (ixor zahl1 zahl2)
    (or (and (not zahl1) zahl2)
        (and (not zahl2) zahl1)))
  (and (ixor (not a) b)
       (not(or a (not b) c)
       (or (not d)
           (not c)
           (not b)
           (not a)))))

