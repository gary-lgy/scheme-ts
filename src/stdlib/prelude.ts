export const stdlibPrelude = `
(define (not obj)
  (if obj #f #t))

(define (zero? num)
  (and (number? num) (= num 0)))

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

(define (cadr xs)
  (list-ref xs 1))

(define (caddr xs)
  (list-ref xs 2))

(define (cadddr xs)
  (list-ref xs 3))

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
`
