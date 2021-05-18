#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (inc n) (+ n 1))

(define (sum-integers a b)
  (define (id x) x)
  (sum id a inc b))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (double f)
  (lambda (x) (f (f x))))

(define (square x) (* x x))

((compose square inc) 6)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)

(define (iterative-improve good-enough? improve)
  (define (iter x)
    (if (good-enough? x)
        x
        (iter (improve x))))
  (lambda (x)
    (iter x)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  ((iterative-improve (lambda (guess)
                        (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess)
                        (average guess (/ x guess))))
   1.0))

(sqrt 2)
