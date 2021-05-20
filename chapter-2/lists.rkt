#lang racket

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (last-pair items)
  (cond ((null? items) (error "empty list"))
        ((null? (cdr items)) items)
        (else (last-pair (cdr items)))))

(last-pair (list 23 72 149 34))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define (filter fn items)
  (if (null? items)
      '()
      (let ((head (car items))
            (tail (cdr items)))
        (if (fn head)
            (cons head (filter fn tail))
            (filter fn tail)))))

(define (same-parity . items)
  (filter
   (if (even? (car items)) even? odd?)
   items))
