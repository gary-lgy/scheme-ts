; export const stdlibMacros = `
(defmacro let (bindings body . bodies)
  \`((lambda (,@(map car bindings)) ,body ,@bodies) ,@(map cadr bindings)))

(defmacro let* (bindings body . bodies)
  (if (null? bindings)
    \`(let ,bindings ,body ,@bodies)
    \`(let (,(car bindings))
       (let* (,@(cdr bindings))
         ,body
         ,@bodies))))

(defmacro letrec (bindings body . bodies)
  \`(let ()
     ,@(map (lambda (binding) (cons 'define binding)) bindings)
     (let () ,body ,@bodies)))

(defmacro begin (expr . exprs)
  \`((lambda () ,expr ,@exprs)))

(defmacro cond (clause . clauses)
  (define sequence->exp
    ((lambda ()
       (define (last-exp? seq) (null? (cdr seq)))
       (define (first-exp seq) (car seq))
       (define (make-begin seq) (cons 'begin seq))

       (lambda (seq) (if (last-exp? seq) (first-exp seq) (make-begin seq))))))

  (define (clause-predicate clause) (car clause))
  (define (clause-actions clause) (cdr clause))

  ; else clause
  (define (else-clause? clause)
    (eq? (clause-predicate clause) 'else))
  (define (expand-else-clause clause rest-clauses)
    (if (null? rest-clauses)
      (if (null? (clause-actions clause))
        (error "else clause must have at least one expression")
        (sequence->exp (clause-actions clause)))
      (error "else clause isn't last" clause)))

  ; predicate only clause, e.g., (cond (< 0 2))
  (define (predicate-only-clause? clause)
    (null? (clause-actions clause)))
  (define (expand-predicate-only-clause clause rest-clauses)
    ; if the result of evaluating the predicate is truthy, return the result
    ; otherwise, move to the next clause
    (let ((temp (gensym)))
      \`(let ((,temp ,(clause-predicate clause)))
         (if ,temp ,temp ,(expand-clauses rest-clauses)))))

  ; procedure clause, e.g., (cond ((+ 1 1) => display))
  (define (procedure-clause? clause)
    (eq? '=> (car (clause-actions clause))))
  (define (procedure-clause-procedure clause)
    (caddr clause))
  (define (expand-procedure-clause clause rest-clauses)
    ; apply the procedure to the result of evaluating the predicate if the result is truthy
    (let ((temp (gensym)))
      \`(let ((,temp ,(clause-predicate clause)))
         (if ,temp
           (,(procedure-clause-procedure clause) ,temp)
           ,(expand-clauses rest-clauses)))))

  ; normal clause: predicate followed by at least one expression, e.g., (cond ((+ 1 1) (display "yes") (do-something)))
  (define (expand-normal-clause clause rest-clauses)
    \`(if ,(clause-predicate clause)
       ,(sequence->exp (clause-actions clause))
       ,(expand-clauses rest-clauses)))

  (define (expand-clauses clauses)
    (if (null? clauses)
      '() ; no else clause
      (let ((first-clause (car clauses))
            (rest-clauses (cdr clauses)))
        (if (else-clause? first-clause)
          (expand-else-clause first-clause rest-clauses)
          (if (predicate-only-clause? first-clause)
            (expand-predicate-only-clause first-clause rest-clauses)
            (if (procedure-clause? first-clause)
              (expand-procedure-clause first-clause rest-clauses)
              (expand-normal-clause first-clause rest-clauses)))))))

  (expand-clauses (cons clause clauses)))

(defmacro and args
  (cond
    ((null? args) #t)
    ((null? (cdr args)) (car args))
    (else
      \`(if ,(car args) (and ,@(cdr args)) #f))))

(defmacro or args
  (cond
    ((null? args) #f)
    ((null? (cdr args)) (car args))
    (else
      (let ((temp (gensym)))
        \`(let ((,temp ,(car args)))
           (if ,temp ,temp (or ,@(cdr args))))))))

(defmacro show-expansion-then-evaluate (code)
    \`(begin (display ',(macroexpand code)) ,code))
`
