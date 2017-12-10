#lang racket
; Tutorium #7

; ----------- CONVENIENCE-METHODE (kann ignoriert werden)
(define (>> text)
  (begin
    (newline)
    (display text)
    (newline)))

; AUFGABEN --------------------------------------- ;

; Aufgabe 20
(define (r n a)
  (if (< n 0)
      0
      (r-rek n a)))

(define (r-rek n a)
  (if (< n 0)
      0
      (+ (a n) (r-rek (- n 1) a))))

(>> "Aufgabe 20")
(define (a i) i)
(r 10 a)

; Aufgabe 21
; Fizzbuzz-Summe
(define (fizzbuzz-summe bis)
  (define (iter von bis summe)
    (if (> von bis)
        summe
        (iter (+ 1 von)
              bis
              (+ summe (cond ((= (remainder von 3) (remainder von 5) 0) (* 3 von))
                             ((= (remainder von 5) 0) (* 2 von))
                             ((= (remainder von 3) 0) von)
                             (else 0))))))
  (iter 1 bis 0))

(>> "Aufgabe 21")
(fizzbuzz-summe 5)
(fizzbuzz-summe 15)


;(cons (cons 3 (cons 3 null)) (cons 3 (cons 3 (cons 3 3))))

; nil muss in Racket erst noch definiert werden,
; alternativ kann auch null verwendet werden
(define nil '())

; Struktur1
(define struktur1
  (cons (cons 24
              (cons 1 2))
        (cons 20
              (cons 1 1))))

; Struktur2
; Variante 1
(define struktur2a
  (cons (cons 7
              (cons 2
                    (cons 3
                          (cons 1 nil))))
        nil))

(define struktur2b
  (cons (list 7 2 3 1)
        nil))

; Struktur 3
; Variante 1
(define struktur3a
  (cons (cons 3
              (cons 3 nil))
        (cons 3
              (cons 3
                    (cons 3 3)))))

; Variante 2
(define struktur3b
  (cons (list 3 3)
        (cons 3
              (cons 3
                    (cons 3 3)))))

; Struktur 4
; Variante 1
(define struktur4a
  (cons (cons (cons nil
                    (cons 6
                          (cons 1
                                (cons 0
                                      (cons 2 4)))))
              (cons 3 3))
        (cons 10
              (cons 7
                    (cons 8
                          (cons 9
                                (cons 3
                                      nil)))))))

; Variante 2
(define struktur4b
  (cons (cons (cons nil
                    (cons 6
                          (cons 1
                                (cons 0
                                      (cons 2 4)))))
              (cons 3 3))
        (list 10 7 8 9 3)))

(>> "Strukturen: ")
(>> "Nr. 1:")
struktur1

(>> "Nr. 2:")
struktur2a
struktur2b

(>> "Nr. 3:")
struktur3a
struktur3b

(>> "Nr. 4:")
struktur4a
struktur4b