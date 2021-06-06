#lang racket

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (let* ([start (start-segment segment)]
        [end (end-segment segment)]
        [mid-x (average (x-point start) (x-point end))]
        [mid-y (average (y-point start) (y-point end))])
    (make-point mid-x mid-y)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; (define (make-rectangle left-top-corner right-bottom-corner)
;;   (cons left-top-corner right-bottom-corner))

;; (define (rec-top rectangle)
;;   (cdr (car rectangle)))

;; (define (rec-bottom rectangle)
;;   (cdr (cdr rectangle)))

;; (define (rec-left rectangle)
;;   (car (car rectangle)))

;; (define (rec-right rectangle)
;;   (car (cdr rectangle)))

(define (make-rectangle left top right bottom)
  (list left top right bottom))

(define (rec-top rectangle)
  (cadr rectangle))

(define (rec-left rectangle)
  (car rectangle))

(define (rec-right rectangle)
  (caddr rectangle))

(define (rec-bottom rectangle)
  (cadddr rectangle))

(define (rec-height rectangle)
  (abs (- (rec-top rectangle) (rec-bottom rectangle))))

(define (rec-width rectangle)
  (abs (- (rec-left rectangle) (rec-right rectangle))))

(define (rec-perimeter rectangle)
  (+ (* 2 (rec-width rectangle))
     (* 2 (rec-height rectangle))))

(define (rec-area rectangle)
  (* (rec-width rectangle)
     (rec-height rectangle)))

(define (cons x y)
  (λ (m) (m x y)))

(define (car z)
  (z (λ (p q) p)))

(define (cdr z)
  (z (λ (p q) q)))
