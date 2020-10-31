;square root through local declaration of procedures
(define (sqrt x)
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
    (define (average y z) (/ (+ y z) 2))
    (define (improve guess) (average guess (/ x guess)))
    (define (sqrt-iter guess) 
        (if (good-enough? guess) 
            guess
            (sqrt-iter (improve guess))))
    (sqrt-iter 1.0))

;cube root - start
(define (improve-cube y x) (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (good-enough-cube? guess x) (< (abs (- (improve-cube guess x) guess)) 0.001))

(define (cube-root-iter guess x) (if (good-enough-cube? guess x) guess (cube-root-iter (improve-cube guess x) x)))

(define (cube-root x) (cube-root-iter 1.0 x))
;cube root - end

;factorial - recursive
(define (factorial-recur n) (if (= n 1) 1 (* n (factorial-recur (- n 1)))))

; factorial - iterative - start
(define (factorial-iter n)
(define (factorial-worker product counter max-num)
    (if (> counter max-num) product (factorial-worker (* product counter) (+ counter 1) max-num)))
(factorial-worker 1 1 n))
