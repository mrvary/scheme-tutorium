#lang racket
; Tutorium #5

; ----------- CONVENIENCE-METHODE (kann ignoriert werden)
(define (>> text)
  (begin
    (newline)
    (display text)
    (newline)))

; CODESNIPPETS ----------------------------------- ;

; LET
; Statt wie bisher mit define, lassen sich mit let lokale
; Variablen erzeugen

; am Beispiel eines Programms, das Summe mit Produkt addiert
; .. bisher:
(define (verrechne1 a b)
  (define summe (+ a b))
  (define produkt (* a b))
  (+ summe produkt))

; .. mit let:
(define (verrechne2 a b)
  (let ((summe (+ a b))
        (produkt (* a b)))
    (+ summe produkt)))

; Runde Klammern, die nur gruppieren sollen, lassen sich mit eckigen ersetzen
(define (verrechne-mit-eckigen-Klammern a b)
  (let [(summe (+ a b))
        (produkt (* a b))]
    (+ summe produkt)))

; LAMBDA
; Lambdas sind Funktionen die keinen Namen brauchen/bekommen.
; Ein Define lässt sich also in ein Lambda umwandeln, indem
; man den Namen rausstreicht und statt "define" "lambda" schreibt.

(>> "LAMBDA Codesnippets:")

; Beispiel 1:
; mit define:
(define (mal-2 x)
  (* x 2))
(mal-2 5) ; => 10

; oder mit Lambda:
((lambda (x) (* x 2)) 5) ; => 10

; Beispiel 2 (kein Input):
; mit define:
(define (3-mal-2)
  (* 3 2))
(3-mal-2) ; => 6

; oder mit Lambda:
((lambda () (* 3 2))) ; => 6

; LÖSUNGEN ZU DEN AUFGABEN ----------------------- ;

; Aufgabe 16
(define (quersumme zahl)
  (if (< zahl 10)
      zahl
      (+ (remainder zahl 10) 
         (quersumme (quotient zahl 10)))))

(define (einstellige-quersumme zahl)
  (let ((qs (quersumme zahl)))
    (if (< qs 10)
        qs
        (einstellige-quersumme qs))))

(>> "Aufgabe 16:")
(einstellige-quersumme 123) ; => 6
(einstellige-quersumme 9999999999912) ; => 3

; Aufgabe 17
(define (apply-two n f g)
  (g (f n)))

(>> "Aufgabe 17:")
(apply-two 2
           (lambda (x) (* 2 x))
           (lambda (x) (* 10 x))) ; => 40

; Aufgabe 11 - revisited
; ...gelöst mit iterativem Prozess
(define (quersumme-iter-p zahl prädikat summe)
  (define letzte-ziffer (remainder zahl 10))  
  (if (= zahl 0)
      summe
      (quersumme-iter-p (quotient zahl 10)
                        prädikat
                        (+ summe (if (prädikat letzte-ziffer) letzte-ziffer 0)))))

; ...gelöst mit rekursivem Prozess
(define (quersumme-rek-p zahl prädikat)
  (define letzte-ziffer (remainder zahl 10))
  (if (= zahl 0)
      0
      (+ (if (prädikat letzte-ziffer) letzte-ziffer 0)
         (quersumme-rek-p (quotient zahl 10) prädikat))))

; ...rekursiver Prozess mit let
(define (quersumme-rek-p-let zahl prädikat)
  (let ((letzte-ziffer (remainder zahl 10)))
    (if (= zahl 0)
        0
        (+ (if (prädikat letzte-ziffer) letzte-ziffer 0)
           (quersumme-rek-p (quotient zahl 10) prädikat)))))

; Methode, die dann jeweils entweder die rekursive
; oder iterative Variante aufruft
; --> um die iterative Prozedur zu nutzen,
;     die zweite Zeile auf (quersumme-iter-p zahl prädikat 0) ändern
(define (quersumme-p zahl prädikat)
  (quersumme-rek-p zahl prädikat))

(>> "Aufgabe 11 - revisited:")
(quersumme-p 1234 odd?) ; => 4
(quersumme-p 1234 even?) ; => 6
(quersumme-p 5555246246 odd?) ; => 20

; Prädikat, das für Zahlen kleiner 5 #t wird
(define (kleiner-5? x)
  (< x 5))
(quersumme-p 123475758 kleiner-5?) ; => 10
; ... oder mit Lambda gelöst
(quersumme-p 123475758 (lambda (x) (< x 5))) ; => 10
