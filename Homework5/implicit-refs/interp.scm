(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the IMPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of)
;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-statement-program (statement)
          (result-of statement (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (const-exp (num) (num-val num))

        (var-exp (var) (deref (apply-env env var)))
        
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

       (add-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (+ num1 num2)))))

        (mult-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (* num1 num2)))))

        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        (let-exp (var exp1 body)       
          (let ((v1 (value-of exp1 env)))
            (value-of body
              (extend-env var (newref v1) env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
          (begin
            (setref!
              (apply-env env var)
              (value-of exp1 env))
            (num-val 27)))

        (not-exp (exp)
                 (let ((val (value-of exp env)))
                   (if (expval->bool val)
                       (bool-val #f)
                       (bool-val #t))))
        )))
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  ;; uninstrumented version
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        (procedure (var body saved-env)
                   (value-of body
                             (extend-env var (newref val) saved-env))))))

  (define (ref-list vars)
    (if(null? vars)
       '()
       [cons (newref 0) (ref-list(cdr vars))]
       ))
  
  (define (result-of statement2 env)
  (cases statement statement2
    [assign-statement (var exp)
                      (setref! (apply-env env var) (value-of exp env))
                      (num-val 27)]
    
    [print-statement (exp) (display (expval->num
                                     (value-of exp env)))
                     (newline)
                     (num-val 1)]
;                             [num-val (num)
;                                      (display num)
;                                      (newline)]
;                             [bool-val (bool)
;                                       (display bool)
;                                       (newline)]
;                             [proc-val (proc)
;                                       (display "<procedure>")
;                                       (newline)]
;                             [ref-val  (ref)
;                                       (display "<reference>")
;                                       (newline)]
                             
    [brace-statement (statement statements) (for-each (lambda (s)
                                              (result-of s env))
                                            (cons statement statements))]
    
    [if-statement (exp statement1 statement2) (if (expval->bool (value-of exp env))
                                                  (result-of statement1 env)
                                                  (result-of statement2 env))]
    [while-statement (exp statement) (let loop ()
                                       (if (expval->bool (value-of exp env))
                                           (begin (result-of statement env)
                                                  (loop))
                                           'break))]
    [var-statement (var vars statement) (let* ((new-vars (cons var vars))
                                               (ref-vars (ref-list new-vars))
                                               (body-env (extend-env* new-vars ref-vars env)))
                                   (result-of statement body-env))]
    )
    )
  )

  


  
