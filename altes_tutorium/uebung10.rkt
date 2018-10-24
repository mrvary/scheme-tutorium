#lang racket

; Übung Nr. 10
(define (teilstring s von bis)
  (list->string (teilstring-rek (string->list s) von bis 1)))

(define (teilstring-rek s von bis jetzt)
  (cond ((or (null? s) (> jetzt bis)) '())
        ((< jetzt von) (teilstring-rek (cdr s) von bis (+ jetzt 1)))
        (else (cons (car s) (teilstring-rek (cdr s) von bis (+ jetzt 1))))))

(define (removeFirstLast string)
  (teilstring string 2 (- (length (string->list string)) 1)))

;(removeFirstLast "Hallo Welt")
;(removeFirstLast "Algorithmik")

; Aufgabe 46
(define (sicheresPasswort passwort)
  (define (kleinbuchstabe? x)
    (and (> x 96)
         (< x 123)))
  (define (grossbuchstabe? x)
    (and (> x 65)
         (< x 97)))
  (define liste
    (string->list passwort))
  (define (iter rest grossbuchstaben kleinbuchstaben zeichen)
    (cond ((empty? rest) #f)
          ((and (> grossbuchstaben 0)
                (> kleinbuchstaben 0)
                (> zeichen 1)) #t)
          ((grossbuchstabe? (char->integer (car rest)))
           (iter (cdr rest) (+ 1 grossbuchstaben) kleinbuchstaben zeichen))
          ((kleinbuchstabe? (char->integer (car rest)))
           (iter (cdr rest) grossbuchstaben (+ 1 kleinbuchstaben) zeichen))
          (else
           (iter (cdr rest) grossbuchstaben kleinbuchstaben (+ 1 zeichen)))))
  (if (< (length liste) 8)
      #f
      (iter liste 0 0 0)))

;(sicheresPasswort "aUljsb!f/KasDhf")
;(sicheresPasswort "ABC123")

; Aufgabe 47
; hack ..
(define (asciiSum liste)
  (cond ((empty? liste) 0)
        ((or (< (char->integer (char-downcase (car liste))) 65)
             (> (char->integer (char-downcase (car liste))) 122)) (asciiSum (cdr liste)))
        (else (+ (char->integer (char-downcase (car liste))) (asciiSum (cdr liste))))))
  

(define (isAnagramm2 anagramm string)
  (= (asciiSum (string->list anagramm))
     (asciiSum (string->list string))))

; Anagramm korrekt
(define (clean liste)
  (let ((ints (foreach liste (lambda (x) (char->integer (char-downcase x))))))         
        (filter2 ints (lambda (x) (and (> x 96) (< x 123))))))
    
(define (foreach liste f)
  (if (empty? liste)
      '()
      (cons (f (car liste)) (foreach (cdr liste) f))))

(define (filter2 liste predicate)
  (cond ((empty? liste) '())
        ((predicate (car liste)) (cons (car liste) (filter2 (cdr liste) predicate)))
        (else (filter2 (cdr liste) predicate))))

(define (sortAscending liste)
  (define (paste liste num)
    (define (paste-iter left right)
      (cond ((empty? right) (append left (list num)))
            ((< num (car right)) (append left (list num) right))
            (else (paste-iter (append left (list (car right))) (cdr right)))))
    (paste-iter '() liste))
  (define (iter rest result)
    (if (empty? rest)
        result
        (iter (cdr rest) (paste result (car rest)))))
  (iter liste '()))

;(sortAscending (clean (string->list "helASDFlo     ")))
(define (isAnagramm anagramm string)
  (equal? (sortAscending (clean (string->list anagramm)))
          (sortAscending (clean (string->list string)))))

(isAnagramm "Desperation" "A rope ends it")
(isAnagramm "Eleven plus two" "Twelve plus one")

; Aufgabe 48
(define (vektor-add . vektoren)
  (define (iter restVektoren rest ergebnisAlt ergebnisNeu)
    (cond ((empty? restVektoren) ergebnisNeu)
          ((empty? rest) (iter (cdr restVektoren) (car restVektoren) ergebnisAlt ergebnisNeu))
          (else (iter restVektoren
                      (cdr rest)
                      (cdr ergebnisAlt)
                      (append ergebnisNeu (list (+ (car rest) (car ergebnisAlt))))))))
  
  (iter (cddr vektoren) (cadr vektoren) (car vektoren) '()))

;(vektor-add '(1 2) '(1 2))
;(vektor-add '(123) '(456) '(789))

(define (create-null-vector length)
  (if (= 0 length)
      '()
      (cons 0 (create-null-vector (- length 1)))))

(define (add-teilvektoren v1 v2)
  (if (empty? v1)
      '()
      (cons (+ (car v1)(car v2)) (add-teilvektoren (cdr v1) (cdr v2)))))

(define (vektor-add2 . vektoren)
  (define (iter rest result)
    (if (empty? rest)
        result
        (iter (cdr rest) (add-teilvektoren (car rest) result))))
  (iter vektoren (create-null-vector (length (car vektoren)))))

  
;(vektor-add2 '(1 2) '(1 2))
;(vektor-add2 '(1 2 3) '(4 5 6) '(7 8 9))




















  