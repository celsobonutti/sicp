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

(define (filter proc items)
  (if (null? items)
      '()
      (let ((head (car items))
            (filtered-tail (filter proc (cdr items))))
        (if (proc head)
            (cons head filtered-tail)
            filtered-tail))))

(define (same-parity . items)
  (filter
   (if (even? (car items)) even? odd?)
   items))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))

(define (for-each proc items)
  (if (null? items)
      #t
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

(for-each (λ (x) (newline) (display x)) (list 1 2 3 4))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (reverse items)
  (define (reverse-iter remaining accumulator)
    (if (null? remaining)
        accumulator
        (reverse-iter (cdr remaining) (cons (car remaining) accumulator))))
  (reverse-iter items '()))

(define (deep-reverse items)
  (define (reverse-iter remaining accumulator)
    (if (null? remaining)
        accumulator
        (reverse-iter (cdr remaining)
                      (cons
                       (if (pair? (car remaining))
                           (deep-reverse (car remaining))
                           (car remaining))
                       accumulator))))
  (reverse-iter items '()))

(define x (list (list 1 (list 3 4 5)) (list 3 4)))

(define (fringe tree)
  (if (null? tree)
      '()
      (if (pair? (car tree))
          (append (fringe (car tree)) (fringe (cdr tree)))
          (cons (car tree) (fringe (cdr tree))))))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch)
    (if (list? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balanced? mobile)
  (define (calculate-torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (let* ((left-torque (calculate-torque (left-branch mobile)))
         (right-torque (calculate-torque (right-branch mobile))))
    (= left-torque right-torque)))

(define (tree-map proc tree)
  (if (null? tree)
      '()
      (cons (if (list? (car tree))
                (tree-map proc (car tree))
                (proc (car tree)))
            (tree-map proc (cdr tree)))))


(define tree (list 1 2 3 (list 2 3) (list 1 (list 2 3 (list 4)))))

(define (square x)
  (* x x))

(define (square-tree tree)
  (tree-map square tree))

(square-tree tree)

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (λ (r) (cons (car s) r))
                      rest)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (equal? fst snd)
  (if (list? fst snd)
      (and
       (eq? (car fst) (car snd))
       (equal? (cdr fst) (cdr snd)))
      (eq? fst snd)))
