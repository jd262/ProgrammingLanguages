#lang eopl
;Jacob Darabaris
;HW2
(provide (all-defined-out))

; 2.5

(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
(lambda (var val env)
(list 'extend-env var val env)))
(define apply-env
  (lambda (env search-var)
    (cond
((eqv? (car env) 'empty-env) (report-no-binding-found search-var))
((eqv? (car env) 'extend-env) (let ((saved-var (cadr env))
             (saved-val (caddr env))
(saved-env (cadddr env))) (if (eqv? search-var saved-var)
saved-val
           (apply-env saved-env search-var))))
      (else
(report-invalid-env env)))))
(define report-no-binding-found
  (lambda (search-var)
(eopl:error apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
(eopl:error apply-env "Bad environment: ~s" env)))

(define a
  (extend-env 'a 4
              (extend-env 'b 8
                          (extend-env 'c 10
                                      (empty-env)))))
;2.8

(define empty-env?
  (lambda (lst) (equal? (car lst) 'empty-env) #t #f ))

(define empty-env?-a-list null?)

;2.9
(define has-binding?
  (lambda (env s)
    (cond ((null? env)
           #f)
          (eqv? (car env)
          #t)
          (else (has-binding? (cdr env)
    )))))

;2.10
(define extend-env*
  (lambda (var val env)
    (cond ((and (null? var) (null? val)) env)
          ((and (pair? var) (pair? val))
           (extend-env* (cdr var) (cdr val)
                        (extend-env (car var) (car val) env)))
          (else
           (report-no-binding-found)))))