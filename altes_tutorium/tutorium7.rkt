#lang racket
; tutorium nr. 7
; Aufgabe 16
(define (pyramide-max input)
  (listen-iter input 0 0))

(define (listen-iter liste sum max)
  (cond ((empty? liste) (+ max sum))
        ((list? (car liste)) (listen-iter (car liste)
                                          (+ sum max)
                                          0))
        ((> (car liste) max) (listen-iter (cdr liste)
                                          sum
                                          (car liste)))
        (else (listen-iter (cdr liste)
                           sum
                           max))))

(define test (list 3 (list 7 4 (list 2 4 6 (list 8 5 9 3)))))
; (pyramide-max test)

; Aufgabe 17
(define (drehe liste)
  (if (null? liste) (list)
      (append (drehe (cdr liste))
                     (list (car liste)))))

(define (drehe-string s)
  (list->string (drehe (string->list s))))

(drehe-string "Hallihallo")

; Aufgabe 18
(define (ersetze-a-b s)
  (define (ersetze-iter liste ergebnis)
    (cond ((empty? liste) ergebnis)
          ((or (equal? (car liste) #\a)
               (equal? (car liste) #\b)) (ersetze-iter (cdr liste)
                                                       (append ergebnis (list #\?))))
          (else ersetze-iter (cdr liste) (ersetze-iter (cdr liste)
                                                       (append ergebnis (list (car liste)))))))
  (list->string (ersetze-iter (string->list s) '())))

(ersetze-a-b "ghaslhgueöas")
          