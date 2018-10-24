#lang racket
; Klausur4 - SS 16
; Aufgabe 1
(define (quicksort_alt liste)
  (if (<= (length liste) 1) liste
      (let ((pivot (car liste)))
        (append (quicksort_alt (filter (lambda (x) (<= x pivot) (cdr liste))))
                (list pivot)
                (quicksort_alt (filter (lambda (x) (> x pivot)) (cdr liste)))))))

(define (make-quicksort praedikat)
  (define (quicksort liste)
    (if (<= (length liste) 1) liste
        (let ((pivot (car liste)))
          (append (quicksort (filter (lambda (x) (praedikat x pivot)) (cdr liste)))
                  (list pivot)
                  (quicksort (filter (lambda (x) (not (praedikat x pivot))) (cdr liste)))))))
  (lambda (x) (quicksort x)))
  
;(define qs (make-quicksort <))
;(qs '(4 1 9 7 8 8 5 -7 4 9 0))

; Aufgabe 2
(define (filtern start ende . zahlen)
  (filter (lambda (x) (and (> x start) (< x ende))) zahlen))

;(filtern 0 100 7 -3 22 500)

; Aufgabe 3
(define (passwort satz)
  (define (iter gesehen restsatz ergebnis)
    (cond ((= (length restsatz) 1) (append ergebnis (list (car restsatz))))
          ((= 32 (char->integer (car restsatz))) (iter gesehen (cdr restsatz) (append ergebnis (list (car gesehen)))))
          (else (iter (cons (car restsatz) gesehen) (cdr restsatz) ergebnis))))
  (list->string (iter '() (string->list satz) '())))

;(passwort "Letzte Aufgabe in diesem Semester")

; Klausur 4 - WS 15/16
; Aufgabe 1
(define (trennzeichen? buchstabe liste)
  (if (empty? liste) #f
      (if (eq? buchstabe (car liste)) #t
          (trennzeichen? buchstabe (cdr liste)))))

(define (teilen text trennzeichen)
  (define (iter ergebnis rest wort)
    (cond ((empty? rest) (if (not (empty? wort)) (append ergebnis (list (list->string wort))) ergebnis))
          ((trennzeichen? (car rest) (string->list trennzeichen))
           (iter (if (not (empty? wort)) (append ergebnis (list (list->string wort)))
                     ergebnis)
                 (cdr rest)
                 '()))
          (else (iter ergebnis (cdr rest) (append wort (list (car rest)))))))
  (iter '() (string->list text) '()))

(teilen "nach der Abschluss-Prüfung, kommen die Ferien" " -,")

; Aufgabe 2
(define (delete-v-items daten indizes)
  (if (empty? indizes) #()
      (vector-append (vector-drop daten (car indizes)) (delete-v-items (cdr indizes))
 

(define (delete-l-items daten indizes)0)
  


(define (loeschen daten indizes)
  (let ((vDaten? (vector? daten))
        (vIndizes (vector? indizes)))
    (delete-v-items daten indizes)))








  
