#lang racket
;Übung 1 - Aufgabe 1
(define x
  (- (/ (+ 9 6)
        (* (- 3 1) 5))
     (* (- (/ 7 8) 2)
        4)))

;Übung 1 - Aufgabe 2
(define (g u v w)
  (+ (/ (- v (* 7 u))
        (- u w))
     (/ (+ u v)
        (- (* w 6)
           v ))))

;Übung 1 - Aufgabe 3
(define (my-max x y)
  (if (> x y) x y))

;Übung 1 - Aufgabe 4
(define (groesser-zehn? x)
  (if (> x 10) #t #f))

;Übung 1 - Aufgabe 5
(define (groesserp? x y z)
  (if (> (+ x y) z) #t #f))

;Übung 1 - Aufgabe 6
;Version 1
(define (squared-max x y z)
  (cond ((and (> (sqr x)(sqr y))
              (> (sqr x)(sqr z))) (sqr x))
        ((> (sqr y)(sqr z)) (sqr y))
        (else (sqr z))))

;Version 2
(define (squared-max2 x y z)
  (cond ((and (> (* x x)(* y y))
              (> (* x x)(* z z))) (* x x))
        ((>   (* y y)(* z z))     (* y y))
        (else (* z z))))

;Version 3
(define (betrag x)
  (if (< x 0) (- x) x))

(define (squared-max3 x y z)
  (sqr (max (betrag x)
            (betrag y)
            (betrag z))))
             
  