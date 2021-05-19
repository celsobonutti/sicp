#lang racket

(define zero (λ (f) (λ (x) x)))

(define (add-1 n)
  (λ (f) (λ (x) (f ((n f) x)))))

(define one
  (λ (f) (λ (x) (f (((λ (x) x) f) x)))))

(define two
  (λ (f) (λ (x) (f (((λ (x) (f (((λ (x) x) f) x))) f) x)))))
