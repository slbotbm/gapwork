;loading libraries
(load "simply.scm")
(load "database.scm")
(load "newttt.scm")
(load "ttt.scm")
(load "match.scm")
(load "functions.scm")
(load "spread.scm")

;plural words
(define (plural wd)
    (if (equal? (last wd) 'y)
        (if (member? (last (bl wd)) '(a e i o u)) 
            (word wd 's) (word (bl wd) 'ies)) (word wd 's)))

;pig latin - start
(define (pigl wd)
    (if (pl-done? wd)
        (word wd 'ay)
        (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
    (vowel? (first wd)))

(define (vowel? letter)
    (member? letter '(a e i o u)))
;pig latin - end

;buzz
(define (buzz n) (cond 
    ((equal? (remainder n 7) 0) 'buzz)
    ((member? 7 n) 'buzz)
    (else n)))

;argue: recursion - start

(define (argue s)
    (if (empty? s)
        '()
        (se (opposite (first s)) (argue (bf s)))))

(define (opposite w)
    (cond ((equal? w 'like) 'hate)
        ((equal? w 'hate) 'like)
        ((equal? w 'wonderful) 'terrible)
        ((equal? w 'terrible) 'wonderful)
        ((equal? w 'great) 'awful)
        ((equal? w 'awful) 'great)
        ((equal? w 'terrific) 'yucky)
        ((equal? w 'yucky) 'terrific)
        (else w) ))
;argue: recursion - end

;pig latin sentence

(define (pigl-sent s)
    (if (empty? s)
        '()
        (se (pigl (first s))
            (pigl-sent (bf s)))))

;pascal's triangle
(define (pascal row col)
    (cond ((= col 0) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) (- col 1))
                (pascal (- row 1) col) ))))

;summing individual digits in a number
(define (sent-sum sent)
    (if (empty? sent)
        0
        (+ (first sent) (sent-sum (bf sent)))))

;summing numbers from a to b with a function applied to a
(define (sum-of func a b) 
    (if (> a b) 
        0 
        (+ (func a) (sum-of func (+ a 1) b))))