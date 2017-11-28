#lang racket
; ----------- CONVENIENCE-METHODE (kann ignoriert werden)
(define (>> text)
  (begin
    (newline)
    (display text)
    (newline)))


; CODESNIPPETS ----------------------------------- ;
; Beispiel einer iterativen Summenfunktion
(define (sum-iter bis summe)
  (if (= 0 bis)
      summe
      (sum-iter (- bis 1) (+ summe bis))))

; Beispiel einer rekursiven Summenfunktion
(define (sum-rek bis)
  (if (= 0 bis)
      0
      (+ bis (sum-rek (- bis 1)))))


; LÖSUNGEN ZU DEN AUFGABEN ----------------------- ;

; Aufgabe 9
(define (number-magic number)
  (define last-digit
    (abs (remainder number 10)))
  (define first-digits
    (quotient number 10))
  (if (< (abs number) 10)
      #f
      (+ last-digit first-digits)))

(>> "Aufgabe 9:")
(number-magic 1675)
(number-magic -123)
(number-magic -12)
(number-magic 3)

; Aufgabe 10
; Teil1 - Quersummenprozeduren
(define (quersumme-iter zahl summe)
    (if (< zahl 10)
        (+ summe zahl)
        (quersumme-iter (quotient zahl 10)
                        (+ summe (remainder zahl 10)))))

(define (quersumme-rek zahl)
  (if (< zahl 10)
      zahl
      (+ (remainder zahl 10) (quersumme-rek (quotient zahl 10)))))

(>> "Aufgabe 10:")
(>> "Teil 1 - Quersummen iterativ / rekursiv")
(quersumme-iter 1234 0) ; => 10
(quersumme-rek 1234) ; => 10

; Teil 2 - Quersummenprozeduren mit Prädikat
; gelöst mit iterativem Prozess
(define (quersumme-iter-p zahl prädikat summe)
  (define letzte-ziffer (remainder zahl 10))  
  (if (< zahl 10)
      (+ summe (if (prädikat zahl) zahl 0))
      (quersumme-iter-p (quotient zahl 10)
                        prädikat
                        (+ summe (if (prädikat letzte-ziffer) letzte-ziffer 0)))))

; gelöst mit rekursivem Prozess
(define (quersumme-rek-p zahl prädikat)
  (define letzte-ziffer (remainder zahl 10))
  (if (< zahl 10)
      (if (prädikat zahl) zahl 0)
      (+ (if (prädikat letzte-ziffer) letzte-ziffer 0)
         (quersumme-rek-p (quotient zahl 10) prädikat))))

; Methode, die dann jeweils entweder die rekursive
; oder iterative Variante aufruft
; --> um die iterative Prozedur zu nutzen,
;     die zweite Zeile auf (quersumme-iter-p zahl prädikat 0) ändern
(define (quersumme-p zahl prädikat)
  (quersumme-rek-p zahl prädikat))

(>> "Teil 2 - Quersumme mit Prädikaten:")
(quersumme-p 1234 odd?) ; => 4
(quersumme-p 1234 even?) ; => 6
(quersumme-p 5555246246 odd?) ; => 20

; Teil 3 - Prädikat, das für Zahlen kleiner 5 #t wird
(define (kleiner-5? x)
  (< x 5))

(>> "Teil 3 - Quersumme mit Ziffern kleiner 5:")
(quersumme-p 123475758 kleiner-5?) ; => 10


