#lang racket
; Tutorium 18.01.

; Aufgabe 25
(define (praefixe wort)
  (define (iter counter result)
    (cond ((> counter (string-length wort)) result)
          (else (iter (+ 1 counter) (string-append result (substring wort 0 counter))))))
  (iter 0 ""))
    
;(praefixe "Hallo Kai")

; Aufgabe 25.1
(define (changeListsVectors liste)
  (if (empty? liste)
      '()
      (cons (if (vector? (car liste))
                  ''()
                  #())
              (changeListsVectors (cdr liste)))))

;(changeListsVectors (list '() '() #() #() '()))

; Aufgabe 26
(define (integer-sum liste)
  (if (empty? liste)
      0
      (+ (char->integer (car liste)) (integer-sum (cdr liste)))))

(define (asciiDifferenz wort1 wort2)
  (- (integer-sum (string->list wort1))
     (integer-sum (string->list wort2))))

(asciiDifferenz "Bernd das?" "Brot!")

; Aufgabe 27
(define (wiederholung prefix wort)
  (if (string-prefix? wort prefix)
      (wiederholung (string-append prefix (car (string->list wort)))
                    wort)
      prefix))

(define (methode wort)
  (if (empty? (wiederholung "" wort))
      (methode (list->string (cdr (string->list wort))))
      #f))

(methode "Horst mag Wurst")
