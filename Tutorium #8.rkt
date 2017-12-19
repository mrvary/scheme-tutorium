#lang racket
; Tutorium #8
; ----------- CONVENIENCE-METHODE (kann ignoriert werden)
(define (>> text)
  (begin
    (newline)
    (display text)
    (newline)))

(define (inc n) (+ n 1))

; HIGHER ORDER FUNCTIONS ---------------------------------- ;

; FILTER
; rekursiv
(define (filter1 liste p)
  (cond [(empty? liste) '()]
        [(p (car liste)) (cons (car liste) (filter (cdr liste) p))]
        [else (filter (cdr liste) p)]))

; iterativ
(define (filter2 liste p)
  (define (iter liste ergebnis)
    (cond [(empty? liste) ergebnis]
          [(p (car liste)) (iter (cdr liste) (append ergebnis (list (car liste)) ))]
          [else (iter (cdr liste) ergebnis)]))
  (iter liste '()))

; MAP
; rekursiv
(define (map f liste)
  (if (empty? liste)
      '()
      (cons (f (car liste)) (map f (cdr liste)))))

; iterativ
(define (map-iter rest ergebnis)
  (if (empty? rest)
      ergebnis
      (map-iter (cdr rest) (append (list (car rest)) ergebnis))))

; AUFGABEN --------------------------------------- ;

; Aufgabe 22
; iterativ
(define (sträwkcür l)
  (define (iter liste ergebnis)
    (if (empty? liste)
        ergebnis
        (iter (cdr liste) (cons (car liste) ergebnis))))
  (iter l '()))

; rekursiv
(define (sträwkcür2 l)
  (if (empty? l)
      l
      (append (sträwkcür2 (cdr l)) (list (car l)))))

(>> "Aufgabe 22")
(>> "iterativ")
(sträwkcür (list 1 2 3))
(>> "rekursiv")
(sträwkcür2 (list 1 2 3))

; Aufgabe 23
; iterativ
(define (durchschnitt . werte)
  (define (iter rest ergebnis)
    (if (empty? rest)
        (/ ergebnis (length werte))
        (iter (cdr rest) (+ ergebnis (car rest)))))
  (iter werte 0))

; rekursiv
(define (durchschnitt-rek . werte)
  (define (rek liste)
    (if (empty? liste)
        0
        (+ (car liste) (rek (cdr liste)))))
  (/ (rek werte) (length werte)))

(>> "Aufgabe 23")
(>> "iterativ")
(durchschnitt 10.0 1)
(durchschnitt 10.0 2 4 1)
(>> "rekursiv")
(durchschnitt-rek 10.0 1)
(durchschnitt-rek 10.0 2 4 1)

; Aufgabe T3.3
(define (kombiniere op)
  (lambda (liste1 liste2)
    (define (rek rest1 rest2)
      (cond [(and (empty? rest1) (empty? rest1)) '()]
            [(empty? rest1) (cons (op 0 (car rest2)) (rek '() (cdr rest2)))]
            [(empty? rest2) (cons (op (car rest1) 0) (rek (cdr rest1) '()))]
            [else (cons (op (car rest1) (car rest2)) (rek (cdr rest1) (cdr rest2)))]))
    (rek liste1 liste2)))

(>> "Aufgabe T3.3")
(define (plus a b) (+ a b))
(define func (kombiniere plus))
(func '(1 2 3 1 2 3) '(3 4 5))

; Aufgabe T3.4
(require math/number-theory)

(define (prim-rek zahl zähler)
  (cond ((not (prime? zähler)) (prim-rek zahl (inc zähler)))
        ((= (/ zahl zähler) 1) (cons zähler '()))
        ((= 0 (remainder zahl zähler)) (cons zähler (prim-rek (/ zahl zähler) zähler)))
        (else (prim-rek zahl (inc zähler)))))

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






