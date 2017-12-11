#lang racket
; Methode yum Inkrementieren
(define (binär-zähler l)
  (define (binär-iter rest nuller einser)
    (cond ((empty? rest) (cons nuller einser))
          ((= 0 (car rest)) (binär-iter (cdr rest) (add1 nuller) einser))
          (else (binär-iter (cdr rest) nuller (add1 einser)))))
  (binär-iter l 0 0))

;(binär-zähler (list 1 0 1))
;(binär-zähler (list 1 0 1 0 0 1))
;(binär-zähler (list 0 0))

; ---
(define (sträwkcür l)
  (define (iter rest ergebnis)
    (if (empty? rest)
        ergebnis
        (iter (cdr rest) (cons (car rest) ergebnis))))
  (iter l '()))

;(sträwkcür (list 1 2 3))

; ---
(define (durchschnitt . werte)
  (define (iter rest summe zähler)
    (if (empty? rest)
        (/ summe zähler)
        (iter (cdr rest) (+ summe (car rest)) (add1 zähler))))
  (iter werte 0 0))

;(durchschnitt 10.0 1)
;(durchschnitt 10.0)
;(durchschnitt 10.0 1 4 2)

; Klausuraufgaben
; T3.2
(require math/number-theory)

(define (prim-rek zahl zähler)
  (cond ((not (prime? zähler)) (prim-rek zahl (add1 zähler)))
        ((= (/ zahl zähler) 1) (cons zähler '()))
        ((= 0 (remainder zahl zähler)) (cons zähler (prim-rek (/ zahl zähler) zähler)))
        (else (prim-rek zahl (add1 zähler)))))

(define (primfaktoren-iter zahlen)
  (if (empty? zahlen)
      '()
      (cons (prim-rek (car zahlen) 2) (primfaktoren-iter (cdr zahlen)))))

(define (zerlegung . zahlen)
  (if (empty? zahlen)
      '()
      (primfaktoren-iter zahlen)))

;(zerlegung 6)
;(zerlegung 33 120)

; ---
; todo
(define (kombiniere2 op)
  (lambda (liste1 liste2)
    (define (iter rest1 rest2 ergebnis)
      (cond ((and (empty? rest1)
                  (empty? rest2)) ergebnis)
            ((empty? rest1) (iter '() (cdr rest2) (cons (op 0 (car rest2)) ergebnis)))
            ((empty? rest2) (iter (cdr rest1) '() (cons (op (car rest1) 0) ergebnis)))
            (else (iter (cdr rest1) (cdr rest2) (cons (op (car rest1) (car rest2)) ergebnis)))))
    (iter liste1 liste2 '())))

(define (kombiniere op)
  (lambda (liste1 liste2)
    (define (rek rest1 rest2)
       (cond ((and (empty? rest1) (empty? rest2)) '())
             ((empty? rest1) (cons (op 0 (car rest2)) (rek '() (cdr rest2))))
             ((empty? rest2) (cons (op 0 (car rest1)) (rek (cdr rest1) '())))
             (else (cons (op (car rest1) (car rest2)) (rek (cdr rest1) (cdr rest2))))))
    (rek liste1 liste2)))
      

;(define (plus a b) (+ a b))
;(define func (kombiniere2 plus))
;(func '(1 2 3 1 2 3) '(3 4 5))

; ---
(define (entnehmen liste1 liste2 liste3)
  (define (rek l1 l2 l3 zähler)
    (cond ((empty? l1) '())
          (