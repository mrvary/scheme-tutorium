#lang racket
; Methode yum Inkrementieren
(define (inc x)
  (+ 1 x))

(define (binär-zähler l)
  (define (binär-iter rest nuller einser)
    (cond ((empty? rest) (cons nuller einser))
          ((= 0 (car rest)) (binär-iter (cdr rest) (inc nuller) einser))
          (else (binär-iter (cdr rest) nuller (inc einser)))))
  (binär-iter l 0 0))

(binär-zähler (list 1 0 1))
(binär-zähler (list 1 0 1 0 0 1))
(binär-zähler (list 0 0))