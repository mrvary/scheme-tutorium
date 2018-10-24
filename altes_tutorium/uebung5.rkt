#lang racket
; Algorithmik Übungsblatt Nr. 5

; Aufgabe Nr. 20
(define (euler-n n)
  ;n = Summand, m = Zähler dieses Summanden
  (define (euler-iter sum product n m)
    (cond ((= n 0) (+ sum 1))
          ((= m 0) (euler-iter (+ sum product)
                               1
                               (- n 1)
                               (- n 1)))
          (else (euler-iter sum
                            (* product (/ 1.0 m))
                            n
                            (- m 1)))))
  (euler-iter 0 1 n n))

;(euler-n 0)
;(euler-n 1)
;(euler-n 2)
;(euler-n 27)

; Aufgabe Nr. 21
(define (ackermann n m)
  (cond ((= n 0) (+ m 1))
        ((= m 0) (ackermann (- n 1) 1))
        (else (ackermann (- n 1) (ackermann n (- m 1))))))

;(ackermann 0 0)
;(ackermann 0 1)
;(ackermann 4 0)
;(ackermann 3 1)
;(ackermann 3 9)

; Aufgabe Nr. 22
(define (osterformel j)
  (let* ((a (remainder j 19))
         (b (remainder j 4))
         (c (remainder j 7))
         (k (quotient j 100))
         (p (quotient (+ (* 8 k) 13) 25))
         (q (quotient k 4))
         (M (remainder (- (+ 15 k) p q) 30))
         (N (remainder (+ 4 k (- q)) 7))
         (d (remainder (+ M (* 19 a)) 30))
         (e (remainder (+ b b (* 4 c) (* 6 d) N) 7))
         (o (+ 22 d e)))
    o))
         
(osterformel 2010)
(osterformel 2011)
(osterformel 2013)



; Aufgabe Nr. 23
(define (maxziffer n)
  (define (max-iter rest maximum)
    (if (= rest 0)
        maximum
        (max-iter (quotient rest 10)
                  (max (remainder rest 10) maximum))))
  (max-iter (abs n) 0))

;(maxziffer 3475376)
;(maxziffer 1012)

; Aufgabe Nr. 24
(define (n x)
  (+ x 1))

(define (sum x y)
  (if (= y 0)
      x
      (n (sum x (- y 1)))))

; Aufgabe Nr. 25
(define (mul x y)
  (if (= y 0)
      0
      (sum x (mul x (- y 1)))))

; Aufgabe Nr. 26
(define (q n)
  (if (or (= n 1)(= n 2))
      1
      (+ (q (- n (q (- n 1))))
         (q (- n (q (- n 2)))))))

;(q 15)
;(q 16)
;(q 35)

