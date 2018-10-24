#lang racket
; Algorithmik Übung Nr. 6

; Aufgabe 27
(define (zaehlen start ende n)
  (define (teilbar? z)
    (if (and (= (remainder z 7) 0)
             (= (remainder z 3) 0))
        #t
        #f))
  (define (zaehlen-iter sum counter)
    (cond ((= sum n) (- counter 1))
          ((> counter ende) 0)
          ((teilbar? counter) (zaehlen-iter (+ sum 1) (+ counter 1)))
          (else (zaehlen-iter sum (+ counter 1)))))
  (cond ((< n 1) #f)
        (else (zaehlen-iter 0 start))))
            
;(zaehlen 10 100 1)
;(zaehlen 10 100 2)
;(zaehlen 10 100 3)
;(zaehlen 20 22 1)
;(zaehlen -21 0 1)
;(zaehlen -21 21 2)
;(zaehlen -21 22 3)

; Aufgabe 28

(define (gleiche-ziffern zahl)
  (define (vorne-hinten-gleich? n)
    (define letzte-ziffer (remainder (abs n) 10))
    (define (erste-ziffer-iter num)
      (if (< num 10)
          (if (= letzte-ziffer num)
              #t
              #f)
          (erste-ziffer-iter (quotient num 10))))
    (erste-ziffer-iter (abs n)))  
  (define (gleiche-ziffern-iter zaehler)
    (if (vorne-hinten-gleich? zaehler)
        zaehler
        (gleiche-ziffern-iter (+ zaehler 1))))
  (gleiche-ziffern-iter zahl))

;(gleiche-ziffern 123)
;(gleiche-ziffern 4567)

; Aufgabe 29
(define (konst-addierer n)
  (lambda (x) (+ x n)))

;(define plus1 (konst-addierer 1))
;(plus1 98)
;(define plus10 (konst-addierer 10))
;(plus10 98)

; Aufgabe 30
(define (konst-ggt b)
  (define (ggt a b)
    (if (= 0 b) a
        (ggt b (remainder a b))))    
  (lambda (a) (ggt a b)))

;(define ggt10 (konst-ggt 10))
;(ggt10 25)
;(ggt10 27)

;(define ggt987 (konst-ggt 987))
;(ggt987 762351)
;(ggt987 98123746)

; Aufgabe 31
(define (paar-operation op)
  (lambda (x) (if (pair? x)
                  (op (car x) (cdr x))
                  #f)))

(define paar=? (paar-operation =))
     (paar=? (cons 2 3))
     (paar=? (cons 3 3))

(define paar<? (paar-operation <))
     (paar<? (cons 2 3))
     (paar<? (cons 3 3))

; Aufgabe 32
(define nil (list))
(define struktur1 (cons (cons 24
                              (cons 1 2))
                        (cons 20
                              (cons 1 1))))

(define struktur2 (cons (cons 7
                              (cons 2
                                    (cons 3
                                          (cons 1
                                                nil))))
                        nil))

(define struktur3 (cons (cons 3
                              (cons 3
                                    nil))
                        (cons 3
                              (cons 3
                                    (cons 3 3)))))

(define struktur4 (cons (cons (cons nil
                                    (cons 6
                                          (cons 1
                                                (cons 0
                                                      (cons 2 4)))))
                              (cons 3 3))
                        (cons 10
                              (cons 7
                                    (cons 8
                                          (cons 9
                                                (cons 3 nil)))))))
                        
