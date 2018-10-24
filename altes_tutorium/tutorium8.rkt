#lang racket
; tutorium Nr. 8

; Aufgabe 19
(define (is-lowercase-char? char)
  (if (and (> (char->integer char) 96)
           (< (char->integer char) 128)) #t #f))

(define (make-uppercase char)
  (integer->char (- (char->integer char) 32)))

(define (lower-to-upper s)
  (define (l-t-u-iter rest ergebnis)
    (cond ((empty? rest) ergebnis)
          ((is-lowercase-char? (car rest)) (l-t-u-iter (cdr rest)
                                                  (append ergebnis (list (make-uppercase (car rest))))))
          (else (l-t-u-iter (cdr rest) (append ergebnis (list (car rest)))))))
  (list->string (l-t-u-iter (string->list s) '())))

;(lower-to-upper "dasdasDAS!!!DAS")
;(lower-to-upper "asdfhjklASFDHJLSDGF")

; Aufgabe 20
(define (entferne-a-b s)
  (define (entferne-iter liste ergebnis)
    (cond ((empty? liste) ergebnis)
          ((or (equal? (car liste) #\a)
               (equal? (car liste) #\b)) (entferne-iter (cdr liste)
                                                       ergebnis))
          (else entferne-iter (cdr liste) (entferne-iter (cdr liste)
                                                       (append ergebnis (list (car liste)))))))
  (list->string (entferne-iter (string->list s) '())))

;(entferne-a-b "ghaslhgueöas")
;(entferne-a-b "ghjghsdjfhksdhaaaaabbbaaaajfjfjf")

; Aufgabe 21
(define (paste liste num)
  (define (paste-iter left right)
    (cond ((empty? right) (append left (list num)))
          ((< num (car right)) (append left (list num) right))
          (else (paste-iter (append left (list (car right))) (cdr right)))))
  (paste-iter '() liste))

(define (sort2 liste)
  (define (iter rest result)
    (if (empty? rest)
        result
        (iter (cdr rest) (paste result (car rest)))))
  (iter liste '()))

(define (chars->ints liste)
  (define (iter rest result)
    (if (empty? rest) result
        (iter (cdr rest)
              (append result (list (char->integer (car rest)))))))
  (iter liste '()))

(define (ints->chars liste)
  (define (iter rest result)
    (if (empty? rest) result
        (iter (cdr rest)
              (append result (list (integer->char (car rest)))))))
  (iter liste '()))
  
(define (ascii-sort str)
  (list->string (ints->chars (sort2 (chars->ints (string->list str))))))

(ascii-sort "abv!01abCD")

  