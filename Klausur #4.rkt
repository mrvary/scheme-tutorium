#lang racket
;; Aufgabe 1 -----------------------------------------------------------

(define (enthalten buchstabe)
  (define (enthalten-iter chars count)
    (if (empty? chars)
        count
        (enthalten-iter (cdr chars) (if (equal? (car chars) buchstabe)
                                        (+ 1 count)
                                        count))))
  (lambda (str)
    (enthalten-iter (string->list str) 0)))

;(define anzahlL (enthalten #\l))
;(anzahlL "hallo Welt")

;; Aufgabe 2 -----------------------------------------------------------
(define (zaehle-blaetter baum op)
  (define (rek baum)
    (if (not (pair? baum))
        (if (op baum) 1 0)
        (+ (rek (car baum)) (rek (cadr baum)))))
  (rek baum))

;(zaehle-blaetter (cons (list 1 2) (cons 3 4)) odd?)

;; Aufgabe 3 -----------------------------------------------------------
(define (reduce func list)
  (if (null? (cdr list))
      (car list)
      (func (car list) (reduce func (cdr list)))))


;(alle-gleich-lang? (list (vector 1 2 3) (vector 3 4 5) (vector 3 3 3)))
; Variante 1
(define (alle-gleich-lang? vektoren)
  (cond ((empty? (cdr vektoren)) #t)
        ((not (= (vector-length (car vektoren))
                 (vector-length (cadr vektoren)))) #f)
        (else (alle-gleich-lang? (cdr vektoren)))))

(define (add vek1 vek2)
  (define (rek counter)
    (if (= counter (vector-length vek2))
        vek2
        (rek (and (vector-set! vek2
                                counter
                                (+ (vector-ref vek1 counter)
                                   (vector-ref vek2 counter)))
                  (add1 counter)))))
  (rek 0))

(define (addiere vektor1 . vektoren)
  (let [[alle-vektoren (append (list vektor1) vektoren)]]
    (if (alle-gleich-lang? alle-vektoren)
        (reduce add (append (list vektor1) vektoren))
        #f)))
  
(addiere (vector 1 2 3) (vector 10 13 16) (vector 100 120 140))

; Variante 2
(define (alle-gleichlang? v-list)
  (reduce (lambda (x y) (if (= x y) x (/ (* x y) x)))
          (map vector-length v-list)))

(define (ad vektoren)
  (define (rek counter)
    (if (= counter (vector-length (car vektoren)))
        '()
        (cons  (reduce + (map (lambda (x) (vector-ref x counter)) vektoren))
               (rek (+ counter 1)))))
  (rek 0))

(define (addiere2 vektor1 . vektoren)
  (let [[alle-vektoren (append (list vektor1) vektoren)]]
    (if (alle-gleichlang? alle-vektoren)
        (list->vector (ad vektoren))
        #f)))

(addiere2 (vector 1 2 3) (vector 10 13 16) (vector 100 120 140))

; Variante 3
; alles in einer Methode, zwei extra defines, jeweils eins für jeden Loop
; oder doch mit set! alles auf einen vektor addieren?
    