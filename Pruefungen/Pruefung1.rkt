#lang racket
; Beispiellösung Prüfung 1

; T1.G1.A1
(define (note vortrag hausarbeit kolloquium)
  
  (define schnitt
    (/ (+ vortrag hausarbeit (* 2 kolloquium)) 4))

  (define nachkommastelle
    (- schnitt (floor schnitt)))
  
  (cond ((> schnitt 4.0) 5.0)
        ((<= nachkommastelle 0.15) (floor schnitt))
        ((<= nachkommastelle 0.5) (+ (floor schnitt) 0.3))
        ((<= nachkommastelle 0.85) (+ (floor schnitt) 0.7))
        (else (ceiling schnitt))))

; folgende Version ist benötigt etwas weniger Logik und bietet
; sich für Prüfungssituationen evtl. an
(define (note-alternative vortrag hausarbeit kolloquium)
  (define schnitt
    (/ (+ vortrag hausarbeit (* 2 kolloquium)) 4))
  (cond ((<= schnitt 1.15) 1.0)
        ((<= schnitt 1.5) 1.3)
        ((<= schnitt 1.85) 1.7)
        ((<= schnitt 2.15) 2.0)
        ((<= schnitt 2.5) 2.3)
        ((<= schnitt 2.85) 2.7)
        ((<= schnitt 3.15) 3.0)
        ((<= schnitt 3.5) 3.3)
        ((<= schnitt 3.85) 3.7)
        ((<= schnitt 4.0) 4.0)
        (else 5.0)))
        
;(note 2.0 3.0 2.3) ;2.3

; T1.G1.A2
(define (anzahl a b c)
  (define diskriminante
    (- (* b b) (* 4 a c)))
  (cond ((= a 0 ) #f)
        ((< diskriminante 0) 0)
        ((= diskriminante 0) 1)
        (else 2)))

;(anzahl 1 1 -2) ;2
;(anzahl 2 1 1) ;0
;(anzahl 1 2 1) ;1
;(anzahl 0 2 1) ;#f

; T1.G1.A2
(define (ixor a b)
  (or (and a (not b))
      (and b (not a))))

;(ixor2 #f #t)
;(ixor2 #f #f)
;(ixor2 #t #t)
;(ixor2 #t #f)

(define (f a b c d e)
  (or (and a b d e)
      (not (or (and c a d)
               (not (and (ixor a e)
                         c))))))

;(f #f #f #f #t #t) ;#f
;(f #t #t #f #t #t) ;#t
;(f #f #f #t #f #t) ;#t 

(provide note
         anzahl
         f)




