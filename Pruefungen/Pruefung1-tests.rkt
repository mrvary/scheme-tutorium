#lang racket

; --- Anleitung: ----------------------------------------------------------------------- ;
;
;   1. Funktionen implementieren und in Datei Namens "Pruefung1" speichern
;   2. Als unterstes in eurer Pruefung1.rkt die Zeile (provide note anzahl f) hinzufügen
;   3. Diese Datei ins selbe Verzeichnis kopieren und ausführen
; -------------------------------------------------------------------------------------- ;
(require rackunit
         "Pruefung1.rkt"
         rackunit/text-ui)

(define aufgabe1
  (test-suite
   "Tests für die Aufgabe 1"
   (newline)
   (display "Ergbnis für Aufgabe 1:")
   (newline)
   (check-equal?
    (note 2.0 3.0 2.3) 2.3 "Falsches Ergebnis bei Belegung: 2.0 3.0 2.3")
   (check-equal?
    (note 2.0 1.0 2.3) 2.0 "Falsches Ergebnis bei Belegung: 2.0 1.0 2.3")
   (check-equal?
    (note 4.0 3.7 3.3) 3.7 "Falsches Ergebnis bei Belegung: 4.0 3.7 3.3")
   (check-equal?
    (note 5.0 5.0 3.3) 5.0 "Falsches Ergebnis bei Belegung: 5.0 5.0 3.3")))

(define aufgabe2
  (test-suite
   "Tests für die Aufgabe 2"
   (newline)
   (display "Ergbnis für Aufgabe 2:")
   (newline)
   (check-eq?
    (anzahl 0 1 -2) #f "Falsches Ergebnis bei Belegung: 0 1 -2")
   (check-eq?
    (anzahl 1 1 -2) 2 "Falsches Ergebnis bei Belegung: 1 1 -2")
   (check-eq?
    (anzahl 2 1 1) 0 "Falsches Ergebnis bei Belegung: 2 1 1")
   (check-eq?
    (anzahl 1 2 1) 1 "Falsches Ergebnis bei Belegung: 1 2 1")))

(define aufgabe3
  (test-suite
   "Tests für die Aufgabe 3"
   (newline)
   (display "Ergbnis für Aufgabe 3:")
   (newline)
   (check-eq?
    (f #f #f #f #t #t) #f "Falsches Ergebnis bei Belegung: #f #f #f #t #t")
   (check-eq?
    (f #t #t #f #t #t) #t "Falsches Ergebnis bei Belegung: #t #t #f #t #t")
   (check-eq?
    (f #f #f #t #f #t) #t "Falsches Ergebnis bei Belegung: #f #f #t #t #t")))

(run-tests aufgabe1)
(run-tests aufgabe2)
(run-tests aufgabe3) 