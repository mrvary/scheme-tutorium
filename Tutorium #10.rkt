#lang racket
#| Tut Nr. 10 |#


(define (binär-zähler l)
  (define (binär-iter rest nuller einser)
    (cond ((empty? rest) (list nuller einser))
          ((= 0 (car rest)) (binär-iter (cdr rest) (add1 nuller) einser))
          (else (binär-iter (cdr rest) nuller (add1 einser)))))
  (binär-iter l 0 0))

#|
(binär-zähler (list 1 0 1))
(binär-zähler (list 1 0 1 0 0 1))
(binär-zähler (list 0 0))
|#

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

#|
(make-1337 "scheme")
(make-1337 "leetspeak")
 |#

(define (FILTER s)
  (define (rek rest)
    (cond ((empty? rest) '())
          ((or (< (char->integer (car rest)) 65)
               (> (char->integer (car rest)) 90)) (cons (car rest) (rek (cdr rest))))
          (else (rek (cdr rest)))))
  (list->string (rek (string->list s))))

;(FILTER "Read Evaluate Print Loop!")
;(FILTER "TasdfEfdsaST")

(define (contains2? coll el)
  (cond ((empty? coll) #f)
        ((equal? (car coll) el) #t)
        (else (contains2? (cdr coll) el))))

(define (entferne s zeichen) 
  (define (rec rest)
    (cond ((empty? rest) '())
          ((contains2? (string->list zeichen) (car rest))
           (rec (cdr rest)))
          (else (cons (car rest) (rec (cdr rest))))))
  (list->string (rec (string->list s))))

(entferne "scheme" "mc")

        
; butlast
; ending
; vowel?

(define (contains? l c)
  (cond ((empty? l) #f)
        ((equal? (car l) c) #t)
        (else (contains? (cdr l) c))))

(define (vowel? c) (contains? (string->list "aeiouy") c))

(define (butlast l)
  (if (empty? (cdr l))
      '()
      (cons (car l) (butlast (cdr l)))))

(define (butlast1 l)
  (reverse (cdr (reverse l))))

(define (ending l)
  (if (empty? (cdr l))
      (car l)
      (ending (cdr l))))

(define (plural word)
  (define word-l (string->list word))
  (cond ((and (vowel? (ending word-l))
              (vowel? (ending (butlast word-l)))) (string-append word "s"))
        ((vowel? (ending word-l)) (string-append (butlast word) "ies"))
        (else (string-append word "s"))))


               