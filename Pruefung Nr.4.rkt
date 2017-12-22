#lang racket
#|
Prüfung 4
Diese Stimme gerhört dem Mann, der Sie die ganze Zeit durch die Gegend schaukelt.
Sollte Ihnen diese Fahrt nicht gefallen haben, schicken Sie doch einfach
mal Ihre Schwiegereltern vorbei.
|#

; Aufgabe 1
(define (teilen text trennzeichen)

  (define (trennt? buchstabe)
    (define (iter b t)
      (cond [(empty? t) #f]
            [(equal? b (car t)) #t]
            [else (iter b (cdr t))]))
    (iter buchstabe (string->list trennzeichen)))
  
  (define (teilen-iter text wort ergebnis)
    (cond
      [(empty? text) (append ergebnis
                             (list (list->string wort)))]
      [(trennt? (car text))
       (teilen-iter (cdr text)
                    '()
                    (if (empty? wort)
                        ergebnis
                        (append ergebnis
                                (list (list->string wort)))))]
      [else
       (teilen-iter (cdr text)
                    (append wort (list (car text)))
                    ergebnis)]))
  
  (teilen-iter (string->list text) '() '()))

(teilen "nach den Abschluss-Prüfungen, kommen die Ferien" " ,-")

; Aufgabe 2
(define (loeschen daten indizes)
  (if (list? daten)
      (list-prozedur daten indizes)
      (list->vector (list-prozedur (vector->list daten) (vector->list indizes)))))

(define (list-prozedur daten indizes)
  (define (iter daten indizes ergebnis zaehler)
    (cond [(empty? daten) ergebnis]
          [(empty? indizes) (append ergebnis daten)]
          [(= (car indizes) zaehler)
           (iter (cdr daten) (cdr indizes) ergebnis (+ zaehler 1))]
          [else (iter (cdr daten) indizes (append ergebnis (list (car daten))) (+ 1 zaehler))]))
  (iter daten indizes '() 1))
  
(loeschen '(1 2 3 4 5 6 7 8 9 10) '(3 6 8))
(loeschen '#(1 2 3 4 5 6 7 8 9 10) '#(3 6 8))

; Aufgabe 3
(define (sicher? passwort p1 p2 p3)
  (define (iter passwort p1count p2count p3count)
    (cond [(empty? passwort) (not (= 0 (* p1count p2count p3count)))]
          [(p1 (car passwort)) (iter (cdr passwort) (+ p1count 1) p2count p3count)]
          [(p2 (car passwort)) (iter (cdr passwort) p1count (+ p2count 1) p3count)]
          [(p3 (car passwort)) (iter (cdr passwort) p1count p2count (+ p3count 1))]
          [else (iter (cdr passwort) p1count p2count p3count)]))
  (iter (string->list passwort) 0 0 0))

(define (ziffer? x) (char-numeric? x))
(define (klein? x) (char-lower-case? x))
(define (gross? x) (char-upper-case? x))
(sicher? "12345678" ziffer? klein? gross?)
(sicher? "abcDEF12" ziffer? klein? gross?)