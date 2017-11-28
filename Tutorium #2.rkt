#lang racket
; ----------- CONVENIENCE-METHODE (kann ignoriert werden)
(define (>> text)
  (begin
    (newline)
    (display text)
    (newline)))


; CODESNIPPETS ----------------------------------- ;

; Aufgabe 2 noch mal aufgegriffen
(define (flaecheninhalt a r)
  ; speicherung der Rechenergebnisse in Variablen
  (define quadrat (* a a))
  (define kreis (* pi r r))
  (define puzzleteil (- quadrat kreis))
  ; prüfen, dass der Flächeninhalt positiv bleibt
  (if (> quadrat kreis)
      puzzleteil
      "Negativer Rückgabewert"))
(>> "Aufgabe 2 mit Test auf neg. Flächeninhalt:")
(flaecheninhalt 2 4)

; LÖSUNGEN ZU DEN AUFGABEN ----------------------- ;

; Aufgabe 5
(define (formatieren zahl)
  (/ (ceiling (* 100 zahl)) 100))
;(formatieren 1.234) --> Teil-Methoden immer testen

(define (durchschnitt a b c d e)
  (formatieren (/ (+ a b c d e) 5)))

(>> "Aufgabe 5:")
(durchschnitt 123.35 5.64 4.62 2.51 2)

; Aufgabe 6
(define (kategorischer-durchschnitt a b c d e)
  (define durchschnitt (/ (+ a b c d e) 5))
  (cond ((< durchschnitt 1.0) 1.0)
        ((< durchschnitt 1.3) 1.3)
        ((< durchschnitt 1.7) 1.7)
        ((< durchschnitt 2.0) 2.0)
        ((< durchschnitt 2.3) 2.3)
        ((< durchschnitt 2.7) 2.7)
        ((< durchschnitt 3.0) 3.0)
        ((< durchschnitt 3.3) 3.3)
        ((< durchschnitt 3.7) 3.7)
        ((< durchschnitt 4.0) 4.0)
        (else 5.0)))

(>> "Aufgabe 6:")
(kategorischer-durchschnitt 2.3 5.0 4.0 3.7 5.0)
(kategorischer-durchschnitt 1.0 2.0 3.7 2.3 1.7)

; aufgabe 7
(define (bmi-rechner größe gewicht)
  (define bmi (/ gewicht (* größe größe)))
  (cond ((< bmi 18.5) "Untergewicht")
        ((<= bmi 25) "Normalgewicht")
        (else "Übergewicht")))

(>> "Aufgabe 7:")
(bmi-rechner 2.00 60) 

; aufgabe 8
(define (f a b c d e)
  (and (and a b (not e))
       (not (or (and a (not b))
                (and c d e)
                (not (xor a e))))))

(>> "Aufgabe 8:")
(f #t #t #t #f #f)
(f #t #t #t #f #t)