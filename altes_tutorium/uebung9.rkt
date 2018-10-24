#lang racket
; Übung Nr. 9

; Aufgabe 41
(define (tuerme-von-hanoi n)
  (define (bewege zahl start temp ziel ergebnis)
    (cond ((> zahl 0) (append (bewege (- zahl 1) start ziel temp ergebnis)
                              (list (cons start ziel))
                              (bewege (- zahl 1) temp start ziel ergebnis)))
          (else '())))
  (bewege n 'a 'b 'c '()))

(tuerme-von-hanoi 15)

; Aufgabe 42
(define (liste-teilen eingabe)
  (define (teilen-iter rest listOdd listEven counter)
    (cond ((empty? rest) (list listOdd listEven))
          ((odd? counter) (teilen-iter (cdr rest)
                                       (append listOdd (list (car rest)))
                                       listEven
                                       (+ counter 1)))
          (else (teilen-iter (cdr rest)
                             listOdd
                             (append listEven (list (car rest)))
                             (+ counter 1)))))
  (teilen-iter eingabe '() '() 1))

;(liste-teilen '(1 2 3 4 5 6 7 8 9))
;(liste-teilen '(1 2 3 4 5 6 7 8 9 10))
;(liste-teilen '())

; Aufgabe 43
(define (listen-verschmelzen eingabe)
  (define (verschmelzen-iter listOdd listEven ergebnis counter)
    (cond ((and (empty? listEven)
                (empty? listOdd)) ergebnis)
          ((odd? counter) (verschmelzen-iter (cdr listOdd)
                                             listEven
                                             (append ergebnis (list (car listOdd)))
                                             (+ counter 1)))
          (else (verschmelzen-iter listOdd
                                   (cdr listEven)
                                   (append ergebnis (list (car listEven)))
                                   (+ counter 1)))))
  (verschmelzen-iter (car eingabe) (cadr eingabe) '() 1))
                     
;(listen-verschmelzen '((1 3 5 7 9) (2 4 6 8)))
;(listen-verschmelzen '((1 3 5 7 9) (2 4 6 8 10)))
;(listen-verschmelzen '(() ()))

; Aufgabe 44
(define (hamming bin1 bin2)
  (if (empty? bin1)
      0
      (if (= (car bin1) (car bin2))
          (hamming (cdr bin1) (cdr bin2))
          (+ 1 (hamming (cdr bin1) (cdr bin2))))))

;(hamming '(1 0 1 1 0 1 0 1) '(0 1 1 1 0 1 0 0))
;(hamming '(1 0 1) '(1 0 1)) 