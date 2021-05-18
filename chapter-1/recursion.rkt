#lang racket

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-i n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (fib n)
  (define (iter a b cnt)
    (if (= cnt n)
        b
        (iter (+ a b) a (+ cnt 1))))
  (iter 1 0 0))

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f-i n)
  (define (iter a b c cnt)
    (if (< cnt 3)
        a
        (iter (+ a (* 2 b) (* 3 c)) a b (- cnt 1))))
  (iter 2 1 0 n))

(define (square x) (* x x))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-i b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* b a)))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mul a b)
  (cond ((= b 1) a)
        ((even? b) (mul (double a) (halve b)))
        (else (+ a (mul a (- b 1))))))

(define (mul-i a b)
  (mul-iter a b 0))

(define (mul-iter a b acc)
  (cond ((= b 0) acc)
        ((even? b) (mul-iter (double a) (halve b) acc))
        (else (mul-iter a (- b 1) (+ a acc)))))

