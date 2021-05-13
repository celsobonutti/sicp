#lang racket

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(abs -5)

(+ 200 5)

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

(define (sum-of-two-larger x y z)
  (if (> x y)
      (if (> y z)
          (sum-of-squares x y)
          (sum-of-squares x z))
      (if (> x z)
          (sum-of-squares x y)
          (sum-of-squares y z))))

(define (average x y)
  (/ (+ x y) 2))

(define (cube-root-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-root-iter (improve-cube guess x) x)))

(define (cube x) (* x x x))

(define (cube-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
