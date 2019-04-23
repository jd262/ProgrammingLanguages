#lang eopl

;; Jacob Darabaris
;; HW3
;; 3.6 3.7 3.8 3.12 3.15

;; 3.12 Not done ;;

(provide (all-defined-out))

;; BOOK CODE UNLESS NOTED ;;

(define scanner-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)))

(define grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)

    ;; 3.6 ;;
    (expression ("minus" "(" expression ")") minus-exp)

    ;; 3.7 ;;
    (expression ("+" "(" expression "," expression ")") plus-exp)
    (expression ("*" "(" expression "," expression ")") mul-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)

    ;; 3.8 ;;
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)

    ;; 3.12 ;;
    (expression ("print" "(" expression ")") print-exp)
    ))


(sllgen:make-define-datatypes scanner-spec grammar)


(define list-the-datatypes
  (lambda()
    (sllgen:list-define-datatypes scanner-spec grammar)))

(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (emptylist-val)
  (cons-val (first expval?) (rest expval?)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))

(define expval->emptylist?
  (lambda (val)
    (cases expval val
      (emptylist-val () #t)
      (cons-val (first rest) #f)
      (else (report-expval-extractor-error 'cons-or-emptylist val)))))

(define report-expval-extractor-error
  (lambda (expected val)
    (eopl:error 'expval-extractor "Expected a ~s, got ~s" expected val)))


;; 3.15 ;;

(define print
  (lambda (val)
    (display (expval->sexp val))
    (newline)
    (num-val 1)))

(define expval->sexp
  (lambda (val)
    (cases expval val
      (num-val (num)
        num)
      (bool-val (bool)
        bool)
      (emptylist-val ()
        '())
      (cons-val (first rest)
        (cons (expval->sexp first) (expval->sexp rest))))))

(define empty-env
  (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

(define apply-env
  (lambda (initial-env search-var)
    (letrec ((loop (lambda (env)
                     (cond ((null? env)
                            (report-no-binding-found search-var initial-env))
                           ((and (pair? env) (pair? (car env)))
                            (let ((saved-var (caar env))
                                  (saved-val (cdar env))
                                  (saved-env (cdr env)))
                              (if (eqv? search-var saved-var)
                                  saved-val
                                  (loop saved-env))))
                           (else
                            (report-invalid-env initial-env))))))
      (loop initial-env))))

(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s in ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment ~s" env)))

(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
      (extend-env 'v (num-val 5)
        (extend-env 'x (num-val 10)
          (empty-env))))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (diff-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (num-val (- num1 num2)))))
      (zero?-exp (exp1)
        (let ((val1 (value-of exp1 env)))
          (let ((num1 (expval->num val1)))
            (bool-val (zero? num1)))))
      (if-exp (exp1 exp2 exp3)
        (let ((val1 (value-of exp1 env)))
          (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))
      (var-exp (var)
        (apply-env env var))
      (let-exp (var exp1 body)
        (let ((val1 (value-of exp1 env)))
          (value-of body (extend-env var val1 env))))

      ;; 3.6 ;;
      (minus-exp (exp1)
        (let ((val1 (value-of exp1 env)))
          (let ((num1 (expval->num val1)))
            (num-val (- num1)))))

      ;; 3.7 ;;
      (plus-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (num-val (+ num1 num2)))))
      (mul-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (num-val (* num1 num2)))))
      (div-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (num-val (quotient num1 num2)))))

      ;; 3.8 ;;
      (equal?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (bool-val (= num1 num2)))))
      (greater?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (bool-val (> num1 num2)))))
      (less?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (bool-val (< num1 num2)))))

      )))
