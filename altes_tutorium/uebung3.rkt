#lang racket
; Übungsblatt Nr. 3
; ==================================================

; Aufgabe 11
; --------------------------------------------------
(define (sinus-approx x)
  (if (<= x 0.1)
      x
      (- (* 3.0 (sinus-approx (/ x 3.0)))
         (* 4.0 (* (sinus-approx (/ x 3.0))
               (sinus-approx (/ x 3.0))
               (sinus-approx (/ x 3.0)))))))

;gibt Differenz aus
(define (sin-compare x)
  (display (abs (- (sin x)
              (sinus-approx x))))
  (newline))

;test-Iteration
(define (test limit)
  (sin-compare limit)
  (if (= 0 limit)
      "ende"
      (test (- limit 1))))





; Aufgabe 12
; --------------------------------------------------
(define (count-perm x)
  (define (count-perm-iter produkt zahl)
    (if (= zahl 1)
      produkt
      (count-perm-iter (* produkt zahl) (- zahl 1))))
  (count-perm-iter 1 x))


; Fakultät (z.Vgl.)
(define (fak-iter produkt zahl)
  (if (= zahl 1)
      produkt
      (fak-iter (* produkt zahl) (- zahl 1))))


; Aufgabe 13
; --------------------------------------------------
(define (isbn-test x)
  (define (letzte-ziffer x)
    (remainder x 10))
  (define (rest x)
    (quotient x 10))
  (define (isbn-iter stelle summe zahl)
    (if (< stelle 1)
        (if (= (remainder summe 11) 10)
            "X"
            (remainder summe 11))
        (isbn-iter (- stelle 1)
                   (+ (* stelle (letzte-ziffer zahl)) summe)
                   (rest zahl))))
  (isbn-iter 9 0 x))

;(isbn-test 344615497) ; -> 3
;(isbn-test 026201153) ; -> 0
;(isbn-test 392511825) ; -> X

; Aufgabe 14
; --------------------------------------------------
(define (zylinder-kegel radius-zylinder hoehe-zylinder
                        radius-kegel hoehe-kegel)
  (define (pi*r*r*h radius hoehe)
    (* pi radius hoehe))
  (define volumen-zylinder
    (pi*r*r*h radius-zylinder hoehe-zylinder))
  (define volumen-kegel
    (/ (pi*r*r*h radius-kegel hoehe-kegel) 3))
  (/ volumen-zylinder volumen-kegel))

(define (zylinder-kegel2 radius-zylinder hoehe-zylinder
                         radius-kegel hoehe-kegel)
  (/ (* radius-zylinder hoehe-zylinder)
     (/ (* radius-kegel hoehe-kegel) 3)))