#lang racket
#| Tut Nr. 10 |#
; ---- CONVENIENCE-METHODE (kann ignoriert werden) ------------ ;
(define (>> text)
  (begin
    (newline)
    (display text)
    (newline)))

; ---- AUFGABEN ----------------------------------------------- ;
; Aufgabe Nr. 24
(define (binär-zähler l)
  (define (binär-iter rest nuller einser)
    (cond ((empty? rest) (list nuller einser))
          ((= 0 (car rest)) (binär-iter (cdr rest) (+ nuller 1) einser))
          (else (binär-iter (cdr rest) nuller (+ einser 1)))))
  (binär-iter l 0 0))

(>> "Aufgabe 24")
(binär-zähler (list 1 0 1))
(binär-zähler (list 1 0 1 0 0 1))
(binär-zähler (list 0 0))

; Aufgabe Nr. 25
(define (make-1337 s)
  (define (rek rest)
    (if (empty? rest)
        '()
        (let [[x (car rest)]]
          (cons (cond ((equal? x #\a) #\4)
                      ((equal? x #\e) #\3)
                      ((equal? x #\l) #\1)
                      ((equal? x #\s) #\5)
                      ((equal? x #\t) #\7)
                      ((equal? x #\o) #\0)
                      (else x)) (rek (cdr rest))))))
  (list->string (rek (string->list s))))

(>> "Aufgabe 25")
(make-1337 "scheme")
(make-1337 "leetspeak")

; Aufgabe 26
(define (FILTER s)
  (define (rek rest)
    (cond ((empty? rest) '())
          ((or (< (char->integer (car rest)) 65)
               (> (char->integer (car rest)) 90)) (cons (car rest) (rek (cdr rest))))
          (else (rek (cdr rest)))))
  (list->string (rek (string->list s))))

(>> "Aufgabe 26")
(FILTER "Read Evaluate Print Loop!")
(FILTER "TasdfEfdsaST")

; Aufgabe 27
(define (contains? coll el)
  (cond ((empty? coll) #f)
        ((equal? (car coll) el) #t)
        (else (contains? (cdr coll) el))))

(define (entferne s zeichen) 
  (define (rec rest)
    (cond ((empty? rest) '())
          ((contains? (string->list zeichen) (car rest))
           (rec (cdr rest)))
          (else (cons (car rest) (rec (cdr rest))))))
  (list->string (rec (string->list s))))

(>> "Aufgabe 27")
(entferne "scheme" "mc")
(entferne "qwerty" "yq")

; Aufgabe 28

; VOWEL?
; a) mit Hilfsmethode
(define (contains-char? l c)
  (cond ((empty? l) #f)
        ((equal? (car l) c) #t)
        (else (contains? (cdr l) c))))

(define (is-vowel? c) (contains-char? (string->list "aeiouy") c))

; b) direkt
(define (vowel? char)
  (or (equal? char #\a)
      (equal? char #\e)
      (equal? char #\i)
      (equal? char #\o)
      (equal? char #\u)
      (equal? char #\y)))

; BUTLAST
; a) mit reverse-Funktion
(define (butlast-mit-reverse l)
  (reverse (cdr (reverse l))))

; b) handgemacht
(define (butlast l)
  (if (empty? (cdr l))
      '()
      (cons (car l) (butlast (cdr l)))))

; ENDING
; a) mit reverse-Funktion
(define (ending-mit-reverse l)
  (car (reverse l)))

; b) handgemacht
(define (ending l)
  (if (empty? (cdr l))
      (car l)
      (ending (cdr l))))

; HAUPTMETHODE
(define (plural word)
  (let [[chars (string->list word)]]
    (list->string (cond ((or (and (vowel? (ending chars))
                                  (vowel? (ending (butlast chars))))
                             (not (vowel? (ending chars))))
                         (append chars '(#\s)))
                        (else (append (butlast chars) '(#\i #\e #\s)))))))

(>> "Aufgabe 28")
(plural "computer")
(plural "toy")
(plural "spy")


               