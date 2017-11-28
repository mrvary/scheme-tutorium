#lang racket
; --- Anleitung: ----------------------------------------------------------------------- ;
;
;   1. Funktionen implementieren und in Datei Namens "Pruefung2" speichern
;   2. Als unterstes in eurer Pruefung2.rkt die Zeile (provide vergleich primzahlen) hinzufügen
;   3. Diese Datei ins selbe Verzeichnis kopieren und ausführen
; -------------------------------------------------------------------------------------- ;
(require rackunit
         "Pruefung2.rkt"
         rackunit/text-ui)

(define aufgabe1
  (test-suite
   "Tests für die Aufgabe 1"
   (newline)
   (display "Ergebnis für Aufgabe 1:")
   (newline)
   (check-equal?
    (vergleich 112233 <) 2 "Falsches Ergebnis bei Belegung: 112233 <")
   (check-equal?
    (vergleich 112233 >) 0 "Falsches Ergebnis bei Belegung: 112233 >")
   (check-equal?
    (vergleich 3 >) #f "Falsches Ergebnis bei Belegung: 3 >")
   (check-equal?
    (vergleich 112233 =) 3 "Falsches Ergebnis bei Belegung: 112233 =")))

(define aufgabe2
  (test-suite
   "Tests für die Aufgabe 2"
   (newline)
   (display "Ergebnis für Aufgabe 2:")
   (newline)
   (check-equal?
    (primzahlen (lambda (n) (+ (* n n) n 41))) 39 "Falsches Ergebnis bei Belegung: (lambda (n) (+ (* n n) n 41))")
   (check-equal?
    (primzahlen (lambda (n) (* n 2))) #f "Falsches Ergebnis bei Belegung: (lambda (n) (* n 2))")
   (check-equal?
    (primzahlen (lambda (n) (+ (* n n) (* -79 n) 1601))) 79 "Falsches Ergebnis bei Belegung: (lambda (n) (+ (* n n) (* -79 n) 1601))")))

(run-tests aufgabe1)
(run-tests aufgabe2)
