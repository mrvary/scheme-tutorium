#lang racket
; Tutorium #9
; ----------- CONVENIENCE-METHODE (kann ignoriert werden)
(define (>> text)
  (begin
    (newline)
    (display text)
    (newline)))

; AUFGABEN ----------------------------------------------
; Aufgabe T3.2
(require math/number-theory)

(define (prim-rek zahl zaehler)
  (let [[keine-primzahl? (not (prime? zaehler))]
        [letzter-faktor? (= (/ zahl zaehler) 1)]
        [zaehler-teilt? (= 0 (remainder zahl zaehler))]]
    (cond [keine-primzahl? (prim-rek zahl (+ zaehler 1))]
          [letzter-faktor? (cons zaehler '())]
          [zaehler-teilt? (cons zaehler (prim-rek (/ zahl zaehler) zaehler))]
          [else (prim-rek zahl (+ zaehler 1))])))

(define (primfaktoren-iter zahlen)
  (if (empty? zahlen)
      '()
      (cons (prim-rek (car zahlen) 2) (primfaktoren-iter (cdr zahlen)))))

(define (zerlegung . zahlen)
  (if (empty? zahlen)
      '()
      (primfaktoren-iter zahlen)))

(>> "Aufgabe T3.2")
(zerlegung 6)
(zerlegung 33 120)

; Aufgabe T3.5
(define (entnehmen liste1 liste2 liste3)
  (define (iter l1 l2 l3 ergebnis)
    (let [[active (modulo (length ergebnis) 3)]]
      (if (empty? l1)
          ergebnis
          (iter (cdr l1)
                (cdr l2)
                (cdr l3)
                (append ergebnis
                        (list (car (cond [(= active 0) l1]
                                         [(= active 1) l2]
                                         [(= active 2) l3]))))))))
  (iter liste1 liste2 liste3 '()))

(>> "Aufgabe T3.5")
(entnehmen '(1 2 3 4 5 6 7 8)
           '(11 12 13 14 15 16 17 18)
           '(21 22 23 24 25 26 27 28))

; Aufgabe T3.3
(define (aufteilen liste praedikat1 praedikat2)
  (define (filter l p)
    (cond [(empty? l) '()]
          [(p (car l)) (cons (car l) (filter (cdr l) p))]
          [else (filter (cdr l) p)]))
  (list (filter liste praedikat1) (filter liste praedikat2)))

(>> "Aufgabe T3.3")
(aufteilen '(0 1 2 3 4 5) (lambda (x) (< x 4)) odd?)
(aufteilen '(0 1 2 3 4 5) even? odd?)

  





                      