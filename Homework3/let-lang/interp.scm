(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define (value-of-program pgm)
      (cases program pgm
        [a-program (exp1)
          (value-of exp1 (init-env))]))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define (value-of exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        [const-exp (num) (num-val num)]

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        [var-exp (var) (apply-env env var)]

        ;\commentbox{\diffspec}
        [diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2))))]

        ;\commentbox{\zerotestspec}
        [zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f))))]
              
        ;\commentbox{\ma{\theifspec}}
        [if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env)))]

        ;\commentbox{\ma{\theletspecsplit}}
        [let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env)))]
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

        ;; 3.15 ;;
         (print-exp (arg)
		      (let ((val (value-of arg env)))
			(print val)
			(num-val 1)))
        ))

  ;; 3.15 ;;

  (define print
    (lambda (val)
      (cases expval val
        (bool-val (val1) (display val1))
        (num-val (val1) (display val1))
        (cons-val (f b) (display f) (display b))
        (emptylist-val() (display ""))
      )
      (num-val 1)
      )
    )
)