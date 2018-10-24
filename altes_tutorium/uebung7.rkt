#lang racket
; Aufgabe 33
(define (loesche liste praedikat)
  (if (not (praedikat (car liste))) liste
      (loesche (cdr liste)
               praedikat)))
      
;(loesche (list 4 6 8 3 2 4 5) even?)

; Aufgabe 34
(define (drehe liste)
  (if (null? liste) (list)
      (append (drehe (cdr liste))
                     (list (car liste)))))

;(drehe (list 1 2 3))
;(drehe (list 1 2 (list 3)))
;(drehe (list 1 (list 5 6) 2 (list 3 4)))
  
; Aufgabe 35
(define (typ-or typ1 typ2)
  (lambda (x)
    (or (typ1 x)
        (typ2 x))))

(define paar-oder-liste? (typ-or pair? list?))
(define integer-oder-boolean? (typ-or integer? boolean?))

;(integer-oder-boolean? (paar-oder-liste? (cons 1 2)))

; Aufgabe 36
(define (operation operatoren n)
  (define (op-iter n liste)
    (if (= n 1)
        (car liste)
        (op-iter (- n 1)
                 (cdr liste))))
  (lambda (l)
    ((op-iter n operatoren) (car l) (cadr l))))

;(define plus (operation (list + - * /) 1))
;(plus (list 1 2))

;(define minus (operation (list + - * /) 2))
;(minus (list 1 2))

; Aufgabe 37
(define (caesar_encrypt_list data key)
  (define (encrypt-iter d k result)
    (cond ((null? d) result)
          ((null? k) (encrypt-iter d key result))
          (else (encrypt-iter (cdr d)
                              (cdr k)
                              (append result (list (modulo (+ (car d)
                                                              (car k))
                                                           10)))))))
  (encrypt-iter data key (list)))

;(caesar_encrypt_list (list 1 2 3 4 5 6) (list 1 3 3 7))


















