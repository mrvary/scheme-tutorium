#lang racket
; Algo-Übung Nr. 8

; Aufgabe 38
;-------------------- OLD ------------------------;
(define (compressOLD liste)  
  (define (compress-iter char rest count result)
    (define (append-to-result char count)
      (if (= count 1)
          (append result (list char))
          (append result (list count char))))
    (cond ((empty? (cdr rest)) (if (equal? char (car rest))
                                   (append-to-result char (+ 1 count))
                                   (append (append-to-result char count)(list (car rest)))))
          ((equal? char (car rest)) (compress-iter char (cdr rest) (+ 1 count) result))
          (else (compress-iter (car rest) (cdr rest) 1 (append-to-result char count)))))
  (compress-iter (car liste) (cdr liste) 1 '()))

(define (append-to-resultOLD char count l)
  (if (= count 1)
      (append l (list char))
      (append l (list count char))))

;---------------------- NEW ------------------------;
(define (append-to-result char count)
  (append '() (if (= count 1) (list char) (list count char))))

(define (compress l)
  (define (compress-iter rest char count)
    (cond ((empty? rest) (append-to-result char count))
          ((equal? char (car rest))
           (compress-iter (cdr rest) char (+ 1 count)))
          (else (append (append-to-result char count )
                        (compress-iter (cdr rest) (car rest) 1)))))
  (if (empty? l) '()
      (compress-iter (cdr l) (car l) 1)))

(compress '(a b c))
(compress '(a b b c c c))
(compress '(a b b c c c a b c))
(compress '(a a a a a a a a a a))


; Aufgabe 39
(define (make-chars count char)
  (if (= count 1)
      (list char)
      (append (list char) (make-chars (- count 1) char))))

(define (expandiere sym-liste)
  (define (expand-iter rest)
    (cond ((empty? rest) '())
          ((integer? (car rest)) (append (make-chars (car rest) (cadr rest))
                                         (expand-iter (cddr rest))))
          (else (append (list (car rest))
                        (expand-iter (cdr rest))))))
  (expand-iter sym-liste))

(define (expand-rec l)
  (cond ((empty? l) '())
        ((integer? (car l)) (append (make-chars (car l) (cadr l))
                                    (expand-rec (cddr l))))
        (else (append (list (car l))
                      (expand-rec (cdr l))))))

;(expand-rec '(a b c))
;(expand-rec '(a 2 b 3 c))
;(expand-rec '(a 2 b 3 c a b c))
;(expand-rec '(10 a))

; Aufgabe 40
(define (loeschen liste n)
  (cond ((> n (length liste)) '())
        ((= n 0) liste)
        (else (loeschen (cdr liste)
                        (- n 1)))))


;(loeschen '(2 3 4 5 6 7) 3)
;(loeschen '() 3)
