// magic file that is both typescript and scheme
export const stdlibProcedures = `
;; pair accessors

(define (caar xs)
  (car (car xs)))

(define (cadr xs)
  (car (cdr xs)))

(define (cdar xs)
  (cdr (car xs)))

(define (cddr xs)
  (cdr (cdr xs)))

(define (caaar xs)
  (car (car (car xs))))

(define (caadr xs)
  (car (car (cdr xs))))

(define (cadar xs)
  (car (cdr (car xs))))

(define (caddr xs)
  (car (cdr (cdr xs))))

(define (cdaar xs)
  (cdr (car (car xs))))

(define (cdadr xs)
  (cdr (car (cdr xs))))

(define (cddar xs)
  (cdr (cdr (car xs))))

(define (cdddr xs)
  (cdr (cdr (cdr xs))))

(define (caaaar xs)
  (car (car (car (car xs)))))

(define (caaadr xs)
  (car (car (car (cdr xs)))))

(define (caadar xs)
  (car (car (cdr (car xs)))))

(define (caaddr xs)
  (car (car (cdr (cdr xs)))))

(define (cadaar xs)
  (car (cdr (car (car xs)))))

(define (cadadr xs)
  (car (cdr (car (cdr xs)))))

(define (caddar xs)
  (car (cdr (cdr (car xs)))))

(define (cadddr xs)
  (car (cdr (cdr (cdr xs)))))

(define (cdaaar xs)
  (cdr (car (car (car xs)))))

(define (cdaadr xs)
  (cdr (car (car (cdr xs)))))

(define (cdadar xs)
  (cdr (car (cdr (car xs)))))

(define (cdaddr xs)
  (cdr (car (cdr (cdr xs)))))

(define (cddaar xs)
  (cdr (cdr (car (car xs)))))

(define (cddadr xs)
  (cdr (cdr (car (cdr xs)))))

(define (cdddar xs)
  (cdr (cdr (cdr (car xs)))))

(define (cddddr xs)
  (cdr (cdr (cdr (cdr xs)))))

;; logical operator

(define (not obj)
  (if obj #f #t))

;; numerical operator

(define (zero? num)
  (and (number? num) (= num 0)))

;; list helpers

(define (list? xs)
  (or (null? xs)
      (and (pair? xs) (list? (cdr xs)))))

(define (length xs)
  (if (null? xs) 0 (+ 1 (length (cdr xs)))))

(define (list-tail xs k)
  (if (zero? k)
    xs
    (list-tail (cdr xs) (- k 1))))

(define (list-ref xs k)
  (car (list-tail xs k)))

(define (append xs ys)
  (if (null? xs)
    ys
    (cons (car xs) (append (cdr xs) ys))))

(define (reverse xs)
  (letrec ((reverse-iter
          (lambda (remaining reversed)
            (if (null? remaining)
              reversed
              (reverse-iter (cdr remaining) (cons (car remaining) reversed))))))
    (reverse-iter xs '())))

(define (map proc xs)
  (if (null? xs)
    xs
    (cons (proc (car xs)) (map proc (cdr xs)))))

(define (for-each proc xs)
  (if (null? xs)
    xs
    (begin
      (proc (car xs))
      (for-each proc (cdr xs)))))
; `
