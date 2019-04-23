#lang eopl
;; Jacob Darabaris
;; HW1
(provide (all-defined-out))

; 1

(define pi (* 4 (atan 1.0)))

(define (degrees->radians d)
  (* pi (/ d 180)))

; 2

(define (distance v t)
  (* v t))

; 3

(define g 9.8)

(define (time-to-ground vy)
  (* 2 (/ vy g)))

; 4

(define (ball-distance v theta)
  (let ([theta1 (degrees->radians theta)])
    (distance (* v (sin theta1)) (time-to-ground(* v (cos theta1))))))

; 1.15

(define (duple n x)
  (if (zero? n)
      '()
      (cons x (duple (- n 1) x))))

; 1.17

(define (down lst)
  (map list lst))

; 1.19

(define (list-set lst n x)
  (cond [(null? lst)
                '()]
        [(zero? n) (cons x (cdr lst))]
  [else (cons (car lst)
              (list-set (cdr lst) (- n 1) x))]))