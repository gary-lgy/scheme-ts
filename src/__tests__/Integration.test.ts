import { ExpressibleValue, makeList } from '../interpreter/ExpressibleValue'
import { makeBool, makeSymbol } from '../interpreter/SExpression'
import { prepareContext, runUntilDone } from '../testHelpers'
import { Variant } from '../types'
import { stringify } from '../utils/stringify'

describe.each<Variant>(['base', 'no-tco', 'macro'])('integration tests', variant => {
  function evaluateUntilDone(code: string): ExpressibleValue {
    const context = prepareContext(variant)
    return runUntilDone(code, context).value
  }

  test('1', () => {
    const prelude = `(define add (lambda (x) (+ x 1)))`
    expect(stringify(evaluateUntilDone(prelude + ` (add 2)`))).toEqual(`3`)
    expect(stringify(evaluateUntilDone(prelude + ` (add 10)`))).toEqual(`11`)
  })

  test('2', () => {
    const prelude = `(define my-last(lambda  (x)(if (null? (cdr x)) x (my-last (cdr x)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (my-last '(a b c d))`))).toEqual(`(d)`)
    expect(stringify(evaluateUntilDone(prelude + ` (my-last '(1 2 3))`))).toEqual(`(3)`)
  })

  test('3', () => {
    const prelude = `(define (my-but-last liste)(if (or (null? liste) (null? (cdr liste))(null? (cddr liste))) liste (my-but-last (cdr liste))))`
    expect(stringify(evaluateUntilDone(prelude + ` (my-but-last '(a b c d))`))).toEqual(`(c d)`)
    expect(stringify(evaluateUntilDone(prelude + ` (my-but-last '(1 2 3))`))).toEqual(`(2 3)`)
  })

  test('4', () => {
    const prelude = `(define (element-at liste numero) (if (= 1 numero) (car liste)(element-at (cdr liste) (- numero 1))))`
    expect(stringify(evaluateUntilDone(prelude + ` (element-at '(a b c d e) 3)`))).toEqual(`c`)
    expect(stringify(evaluateUntilDone(prelude + ` (element-at '(1 2 3 4 5) 3)`))).toEqual(`3`)
  })

  test('5', () => {
    const prelude = `(define (number-of-elements liste) (if (null?  liste) 0 (+ 1 (number-of-elements (cdr liste)))))`
    expect(
      stringify(evaluateUntilDone(prelude + ` (number-of-elements '(a b h j (k t) l l j))`))
    ).toEqual(`8`)
    expect(
      stringify(evaluateUntilDone(prelude + ` (number-of-elements '(a b h j k t l l j))`))
    ).toEqual(`9`)
  })

  test('6', () => {
    const prelude = `(define (my-reverse liste)(if (null? liste) '() (append (my-reverse (cdr liste)) (list (car liste)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (my-reverse '(s ff k h (k l) l t))`))).toEqual(
      `(t l (k l) h k ff s)`
    )
    expect(stringify(evaluateUntilDone(prelude + ` (my-reverse '(1 2 3 4 5))`))).toEqual(
      `(5 4 3 2 1)`
    )
  })

  test('7', () => {
    const prelude = `
    (define (reverse liste)
      (if (null? liste)
        '()
        (append (reverse (cdr liste)) (list (car liste)))))
    (define (is-palindrome liste)
      (equal? liste (reverse liste)))`
    expect(
      evaluateUntilDone(prelude + ` (is-palindrome '(x (a b) a m a (a b) x))`)
    ).toHaveMatchingValue(makeBool(true))
    expect(
      evaluateUntilDone(prelude + ` (is-palindrome '(my-reverse '(1 2 3 4 5)))`)
    ).toHaveMatchingValue(makeBool(false))
  })

  test('8', () => {
    const prelude = `(define (my-flatten liste) (if (null? liste) '() (append (if (list? (car liste)) (my-flatten (car liste))(list (car liste))) (my-flatten (cdr liste)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (my-flatten '(a (b (c d) e)))`))).toEqual(
      `(a b c d e)`
    )
  })

  test('9', () => {
    const prelude = `(define (my-reverse liste)(if (null? liste) '() (append (my-reverse (cdr liste)) (list (car liste)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (my-reverse '(s ff k h (k l) l t))`))).toEqual(
      `(t l (k l) h k ff s)`
    )
    expect(stringify(evaluateUntilDone(prelude + ` (my-reverse '(1 2 3 4 5))`))).toEqual(
      `(5 4 3 2 1)`
    )
  })

  test('10', () => {
    const prelude = `(define (compress liste) (if (or (null? liste) (null? (cdr liste))) liste (let ((compressed-cdr (compress (cdr liste)))) (if (equal? (car liste) (car compressed-cdr)) compressed-cdr (cons (car liste) compressed-cdr)))))`
    expect(
      stringify(
        evaluateUntilDone(
          prelude + ` (compress '(a a a a b (c c) (c c) (a b) (a b) a a d e e e e))`
        )
      )
    ).toEqual(`(a b (c c) (a b) a d e)`)
    expect(stringify(evaluateUntilDone(prelude + ` (compress '(1 1 1 (2 2) 3))`))).toEqual(
      `(1 (2 2) 3)`
    )
  })

  test('11', () => {
    const prelude = `(define (pack liste) (if (null? liste) (list liste) (let ((packed-cdr (pack (cdr liste)))) (cond ((null? (car packed-cdr)) (list (list (car liste)))) ((equal? (car liste) (caar packed-cdr)) (cons (cons (car liste) (car packed-cdr)) (cdr packed-cdr))) (else (cons (list (car liste)) packed-cdr))))))`
    expect(
      stringify(evaluateUntilDone(prelude + ` (pack '(a a a a b c c a a d e (e) (e) e))`))
    ).toEqual(`((a a a a) (b) (c c) (a a) (d) (e) ((e) (e)) (e))`)
  })

  test('12', () => {
    const prelude = `
    (define (pack liste)
      (if (null? liste)
        (list liste)
        (let ((packed-cdr (pack (cdr liste))))
          (cond ((null? (car packed-cdr))
                (list (list (car liste))))
                ((equal? (car liste) (caar packed-cdr))
                (cons (cons (car liste)
                            (car packed-cdr))
                      (cdr packed-cdr)))
                (else (cons (list (car liste))
                            packed-cdr))))))
    (define (encode liste)
    (define (encode-helper packed-list)
      (if (null? packed-list)
          '()
          (cons (list (length (car packed-list))
                      (caar packed-list))
                (encode-helper (cdr packed-list)))))
    (encode-helper (pack liste)))`
    expect(
      stringify(evaluateUntilDone(prelude + ` (encode '(a a a a b c c a a d e e e e))`))
    ).toEqual(`((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))`)
  })

  test('13', () => {
    const prelude = `
(define (pack liste)
(if (null? liste)
    (list liste)
    (let ((packed-cdr (pack (cdr liste))))
      (cond ((null? (car packed-cdr))
             (list (list (car liste))))
            ((equal? (car liste) (caar packed-cdr))
             (cons (cons (car liste)
                         (car packed-cdr))
                   (cdr packed-cdr)))
            (else (cons (list (car liste))
                        packed-cdr))))))
(define (encode liste)
(define (encode-helper packed-list)
  (if (null? packed-list)
      '()
      (cons (list (length (car packed-list))
                  (caar packed-list))
            (encode-helper (cdr packed-list)))))
(encode-helper (pack liste)))
(define (encode-modified liste)
(define (simplify encoded-list)
  (if (null? encoded-list)
      '()
      (cons (if (= 1 (caar encoded-list))
                (cadar encoded-list)
                (car encoded-list))
            (simplify (cdr encoded-list)))))
(simplify (encode liste)))

(define (decode-rl encoded-list)
(define (expand element count)
  (if (= count 0)
      '()
      (cons element
            (expand element (- count 1)))))
(cond ((null? encoded-list) '())
      ((not (list? (car encoded-list)))
       (append (list (car encoded-list))
               (decode-rl (cdr encoded-list))))
      (else (append (expand (cadar encoded-list)
                            (caar encoded-list))
                    (decode-rl (cdr encoded-list))))))
`
    expect(
      stringify(
        evaluateUntilDone(prelude + ` (decode-rl (encode-modified '(a a a a b c c a a d e e e e)))`)
      )
    ).toEqual(`(a a a a b c c a a d e e e e)`)
  })

  test('14', () => {
    const prelude = `
    (define (pack liste)
    (if (null? liste)
        (list liste)
        (let ((packed-cdr (pack (cdr liste))))
          (cond ((null? (car packed-cdr))
                (list (list (car liste))))
                ((equal? (car liste) (caar packed-cdr))
                (cons (cons (car liste)
                            (car packed-cdr))
                      (cdr packed-cdr)))
                (else (cons (list (car liste))
                            packed-cdr))))))
    (define (encode liste)
    (define (encode-helper packed-list)
      (if (null? packed-list)
          '()
          (cons (list (length (car packed-list))
                      (caar packed-list))
                (encode-helper (cdr packed-list)))))
    (encode-helper (pack liste)))

    (define (encode-modified liste)
    (define (simplify encoded-list)
      (if (null? encoded-list)
          '()
          (cons (if (= 1 (caar encoded-list))
                    (cadar encoded-list)
                    (car encoded-list))
                (simplify (cdr encoded-list)))))
    (simplify (encode liste)))`
    expect(
      stringify(evaluateUntilDone(prelude + ` (encode-modified '(a a a a b c c a a d e e e e))`))
    ).toEqual(`((4 a) b (2 c) (2 a) d (4 e))`)
  })

  test('15', () => {
    const prelude = `
(define (encode-direct list-to-encode)
(define (simplify liste)
  (if (null? liste)
      '()
      (cons (if (= 1 (caar liste))
                (cadar liste)
                (car liste))
            (simplify (cdr liste)))))
(define (encode-direct-helper liste)
  (if (null? liste)
      '()
      (let ((encoded-cdr (encode-direct-helper (cdr liste))))
        (cond ((null? encoded-cdr) (list (list 1
                                               (car liste))))
              ((equal? (car liste)
                       (cadar encoded-cdr))
               (cons (list (+ 1 (caar encoded-cdr))
                           (car liste))
                     (cdr encoded-cdr)))
              (else (cons (list 1
                                (car liste))
                          encoded-cdr))))))
(simplify (encode-direct-helper list-to-encode)))`
    expect(
      stringify(evaluateUntilDone(prelude + ` (encode-direct '(a a a a b c c a a d e e e e))`))
    ).toEqual(`((4 a) b (2 c) (2 a) d (4 e))`)
  })

  test('16', () => {
    const prelude = `(define (dupli liste) (if (null? liste) '()  (append (list (car liste) (car liste)) (dupli (cdr liste)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (dupli '(a b c c d))`))).toEqual(
      `(a a b b c c c c d d)`
    )
  })

  test('17', () => {
    const prelude = `
    (define (repli liste count)
      (define (replicate-element element how-many)
        (if (= how-many 0)
          '()
          (cons element (replicate-element element (- how-many 1)))))
      (if (null? liste)
        '()
        (append (replicate-element (car liste) count) (repli (cdr liste) count))))`
    expect(stringify(evaluateUntilDone(prelude + ` (repli '(a b c) 9)`))).toEqual(
      `(a a a a a a a a a b b b b b b b b b c c c c c c c c c)`
    )
  })

  test('18', () => {
    const prelude = `(define (dupli liste) (if (null? liste) '()  (append (list (car liste) (car liste)) (dupli (cdr liste)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (dupli '(a b c c d))`))).toEqual(
      `(a a b b c c c c d d)`
    )
  })

  test('19', () => {
    const prelude = `(define (drop liste position) (define (drop-helper temp-list temp-position) (cond ((null? temp-list) '()) ((= 1 temp-position) (drop-helper (cdr temp-list) position)) (else (cons (car temp-list) (drop-helper (cdr temp-list) (- temp-position 1)))))) (drop-helper liste position))`
    expect(stringify(evaluateUntilDone(prelude + ` (drop '(a b c d e f g h i k) 3)`))).toEqual(
      `(a b d e g h k)`
    )
  })

  test('20', () => {
    const prelude = `(define (split liste first-part-length) (if (= 0 first-part-length) (list '() liste) (let ((splited-cdr (split (cdr liste)  (- first-part-length 1)))) (list (cons (car liste)  (car splited-cdr)) (cadr splited-cdr)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (split '(a b c d e f g h i k) 3)`))).toEqual(
      `((a b c) (d e f g h i k))`
    )
  })

  test('21', () => {
    const prelude = `(define (split liste first-part-length) (if (= 0 first-part-length) (list '() liste) (let ((splited-cdr (split (cdr liste)  (- first-part-length 1)))) (list (cons (car liste)  (car splited-cdr)) (cadr splited-cdr))))) (define (slice liste start stop) (car (split (cadr (split liste (- start 1))) (+ 1 (- stop start)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (slice '(a b c d e f g h i k) 3 7)`))).toEqual(
      `(c d e f g)`
    )
  })

  test('22', () => {
    const prelude = `
    (define (split liste first-part-length)
      (if (= 0 first-part-length)
        (list '() liste)
        (let ((splited-cdr (split (cdr liste) (- first-part-length 1))))
          (list (cons (car liste) (car splited-cdr)) (cadr splited-cdr)))))
    (define (slice liste start stop)
      (car (split (cadr (split liste (- start 1))) (+ 1 (- stop start)))))
    (define (rotate liste arg)
      (if (null? liste)
        '()
        (let ((splited-liste (split liste  (modulo arg (length liste)))))
          (append (cadr splited-liste) (car splited-liste)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (rotate '(a b c d e f g h) 3)`))).toEqual(
      `(d e f g h a b c)`
    )
    expect(stringify(evaluateUntilDone(prelude + ` (rotate '(a b c d e f g h) -2)`))).toEqual(
      `(g h a b c d e f)`
    )
  })

  test('23', () => {
    const prelude = `(define (remove-at liste place) (if (= 1 place) (cdr liste) (cons (car liste) (remove-at (cdr liste) (- place 1)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (remove-at '(a b c d) 2)`))).toEqual(`(a c d)`)
  })

  test('24', () => {
    const prelude = `(define (insert-at element liste place) (if (= 1 place) (cons element liste)  (cons (car liste) (insert-at element (cdr liste) (- place 1)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (insert-at 'alfa '(a b c d) 2)`))).toEqual(
      `(a alfa b c d)`
    )
  })

  test('25', () => {
    const prelude = `(define (range start stop) (if (= start stop) (list stop) (cons start (range (+ start 1) stop))))`
    expect(stringify(evaluateUntilDone(prelude + ` (range 4 9)`))).toEqual(`(4 5 6 7 8 9)`)
  })

  test('26', () => {
    const prelude = `
(define (combinaison k liste)
(define (add-element x liste)
  (if (null? liste)
      '()
      (cons (cons x (car liste))
            (add-element x (cdr liste)))))
(cond ((or (= 0 k) (> k (length liste))) '(()))
      ((= k (length liste)) (list liste))
      (else (append (combinaison k (cdr liste))
            (add-element (car liste)
                         (combinaison (- k 1) (cdr liste)))))))`
    expect(evaluateUntilDone(prelude + ` (combinaison 3 '(a b c d e f))`)).toHaveMatchingValue(
      makeList([
        makeList([makeSymbol('d', true), makeSymbol('e', true), makeSymbol('f', true)]),
        makeList([makeSymbol('c', true), makeSymbol('e', true), makeSymbol('f', true)]),
        makeList([makeSymbol('c', true), makeSymbol('d', true), makeSymbol('f', true)]),
        makeList([makeSymbol('c', true), makeSymbol('d', true), makeSymbol('e', true)]),
        makeList([makeSymbol('b', true), makeSymbol('e', true), makeSymbol('f', true)]),
        makeList([makeSymbol('b', true), makeSymbol('d', true), makeSymbol('f', true)]),
        makeList([makeSymbol('b', true), makeSymbol('d', true), makeSymbol('e', true)]),
        makeList([makeSymbol('b', true), makeSymbol('c', true), makeSymbol('f', true)]),
        makeList([makeSymbol('b', true), makeSymbol('c', true), makeSymbol('e', true)]),
        makeList([makeSymbol('b', true), makeSymbol('c', true), makeSymbol('d', true)]),
        makeList([makeSymbol('a', true), makeSymbol('e', true), makeSymbol('f', true)]),
        makeList([makeSymbol('a', true), makeSymbol('d', true), makeSymbol('f', true)]),
        makeList([makeSymbol('a', true), makeSymbol('d', true), makeSymbol('e', true)]),
        makeList([makeSymbol('a', true), makeSymbol('c', true), makeSymbol('f', true)]),
        makeList([makeSymbol('a', true), makeSymbol('c', true), makeSymbol('e', true)]),
        makeList([makeSymbol('a', true), makeSymbol('c', true), makeSymbol('d', true)]),
        makeList([makeSymbol('a', true), makeSymbol('b', true), makeSymbol('f', true)]),
        makeList([makeSymbol('a', true), makeSymbol('b', true), makeSymbol('e', true)]),
        makeList([makeSymbol('a', true), makeSymbol('b', true), makeSymbol('d', true)]),
        makeList([makeSymbol('a', true), makeSymbol('b', true), makeSymbol('c', true)])
      ])
    )
  })

  test('27', () => {
    const prelude = `(define (my-gcd n m)(if (= m 0) n (my-gcd m (modulo n m))))`
    expect(stringify(evaluateUntilDone(prelude + ` (my-gcd 36 63)`))).toEqual(`9`)
  })

  test('28', () => {
    const prelude = `(define (my-gcd n m) (if (= m 0) n (my-gcd m (modulo n m)))) (define (my-coprime n m) (= (my-gcd n m) 1))`
    expect(evaluateUntilDone(prelude + ` (my-coprime 35 64)`)).toHaveMatchingValue(makeBool(true))
  })

  test('29', () => {
    const prelude = `
(define (my-gcd n m)
(if (= m 0)
    n
    (my-gcd m (modulo n m))))
(define (my-coprime n m)
(= (my-gcd n m) 1))
(define (my-totient-phi n)
(define (my-totient-phi-aux n i)
  (cond ((<= n i) 0)
        ((my-coprime n i) (+ 1 (my-totient-phi-aux n (+ 1 i))))
        (else (my-totient-phi-aux n (+ 1 i)))))
(cond ((< n 1) 0)
      ((= n 1) 1)
      (else (my-totient-phi-aux n 1))))`
    expect(stringify(evaluateUntilDone(prelude + ` (my-totient-phi 7)`))).toEqual(`6`)
    expect(stringify(evaluateUntilDone(prelude + ` (my-totient-phi 1)`))).toEqual(`1`)
    expect(stringify(evaluateUntilDone(prelude + ` (my-totient-phi 10)`))).toEqual(`4`)
  })

  test('30', () => {
    const prelude = `
(define (my-length xs)
  (cond ((null? xs) 0)
        (else (+ 1 (my-length (cdr xs))))))
(define (my-is-empty-tree candidate)
  (null? candidate))

(define (my-is-nonempty-tree candidate)
  (and (list? candidate)
       (= (my-length candidate) 3)
       (symbol? (car candidate))))

(define (my-is-tree candidate)
  (define (my-is-tree-aux candidate)
    (if (my-is-empty-tree candidate)
        #t
        (if (not (my-is-nonempty-tree candidate))
            #f
            (and (my-is-tree-aux (cadr candidate))
                 (my-is-tree-aux (caddr candidate))))))
  (my-is-tree-aux candidate))`
    expect(evaluateUntilDone(prelude + ` (my-is-tree '(a () ()))`)).toHaveMatchingValue(
      makeBool(true)
    )
    expect(evaluateUntilDone(prelude + ` (my-is-tree '(a () "123"))`)).toHaveMatchingValue(
      makeBool(false)
    )
    expect(evaluateUntilDone(prelude + ` (my-is-tree '(a (b () f) ()))`)).toHaveMatchingValue(
      makeBool(false)
    )
    expect(
      evaluateUntilDone(prelude + ` (my-is-tree '(my-is-tree '(a (b () ()))))`)
    ).toHaveMatchingValue(makeBool(false))
  })

  test('31', () => {
    const prelude = `(define bank-account
  (let ((balance 10))
    (lambda (n)
    (set! balance (+ balance n))
    balance)))`
    expect(stringify(evaluateUntilDone(prelude + ` (bank-account 20)`))).toEqual(`30`)
    expect(stringify(evaluateUntilDone(prelude + ` (bank-account -25)`))).toEqual(`-15`)
  })

  test('32', () => {
    const prelude = `(define (fact n)
   (if (= n 1)
    1
    (* n (fact (- n 1)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (fact 5)`))).toEqual(`120`)
  })

  test('33', () => {
    const prelude = `
(define (fact-tail n)
  (fact-rec n n))
(define (fact-rec n p)
  (if (= n 1)
      p
      (let ((m (- n 1)))
    (fact-rec m (* p m)))))`
    expect(stringify(evaluateUntilDone(prelude + ` (fact-tail 5)`))).toEqual(`120`)
  })

  test('34', () => {
    const prelude = `(define var 1)  (set! var (* var 10))`
    expect(stringify(evaluateUntilDone(prelude + ` var`))).toEqual(`10`)
  })

  test('35', () => {
    const prelude = `(define (range start stop)
(if (= start stop)
    (list stop)
    (cons start
          (range (+ start 1)
                 stop))))`
    expect(stringify(evaluateUntilDone(prelude + ` (range 4 9)`))).toEqual(`(4 5 6 7 8 9)`)
  })

  test('36', () => {
    const prelude = `(define (range start stop)
(if (= start stop)
    (list stop)
    (cons start
          (range (+ start 1)
                 stop))))`
    expect(stringify(evaluateUntilDone(prelude + ` (range 4 9)`))).toEqual(`(4 5 6 7 8 9)`)
  })

  test('37', () => {
    const prelude = `(define (deriv exp var)
(cond ((number? exp)
        0)
      ((variable? exp)
        (if (same-variable? exp var)
            1
            0))
      ((sum? exp)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var)))
      ((product? exp)
        (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
      ((exponentiation? exp)
        (let ((n (exponent exp))
              (u (base exp)))
            (make-product
                n
                (make-product
                    (make-exponentiation
                        u
                        (- n 1))
                    (deriv u var)))))
      (else
        (error "unknown expression type -- DERIV" exp))))

;; exponentiation

(define (make-exponentiation base exponent)
(cond ((= exponent 0)
        1)
      ((= exponent 1)
        base)
      (else
        (list '** base exponent))))

(define (exponentiation? x)
(and (pair? x)
    (eq? (car x) '**)))

(define (base exp)
(cadr exp))

(define (exponent exp)
(caddr exp))

;; number

(define (=number? exp num)
(and (number? exp)
     (= exp num)))

;; variable

(define (variable? x)
(symbol? x))

(define (same-variable? v1 v2)
(and (variable? v1)
     (variable? v2)
     (eq? v1 v2)))

;; sum

(define (make-sum a1 a2)
(cond ((=number? a1 0)
        a2)
      ((=number? a2 0)
        a1)
      ((and (number? a1) (number? a2))
        (+ a1 a2))
      (else
        (list '+ a1 a2))))

(define (sum? x)
(and (pair? x)
     (eq? (car x) '+)))

(define (addend s)
(cadr s))

(define (augend s)
(caddr s))

;; product

(define (make-product m1 m2)
(cond ((or (=number? m1 0) (=number? m2 0))
        0)
      ((=number? m1 1)
        m2)
      ((=number? m2 1)
        m1)
      ((and (number? m1) (number? m2))
        (* m1 m2))
      (else
        (list '* m1 m2))))

(define (product? x)
(and (pair? x)
     (eq? (car x) '*)))

(define (multiplier p)
(cadr p))

(define (multiplicand p)
(caddr p))`
    expect(stringify(evaluateUntilDone(prelude + ` (deriv '(** x 0) 'x)`))).toEqual(`0`)
    expect(stringify(evaluateUntilDone(prelude + ` (deriv '(** x 2) 'x)`))).toEqual(`(* 2 x)`)
    expect(stringify(evaluateUntilDone(prelude + ` (deriv '(** x 3) 'x)`))).toEqual(
      `(* 3 (** x 2))`
    )
    expect(stringify(evaluateUntilDone(prelude + ` (deriv '(** x 1) 'x)`))).toEqual(`1`)
  })

  test('38', () => {
    const prelude = `
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

;; tree

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
      (if (null? bits)
          '()
          (let ((next-branch
                  (choose-branch (car bits) current-branch)))
              (if (leaf? next-branch)
                  (cons (symbol-leaf next-branch)
                        (decode-1 (cdr bits) tree))
                  (decode-1 (cdr bits) next-branch)))))

  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0)
          (left-branch branch))
        ((= bit 1)
          (right-branch branch))
        (else
          (error "bad bit -- CHOOSE-BRANCH" bit))))

(define tree (make-code-tree (make-leaf 'a 4)
               (make-code-tree (make-leaf 'b 2)
                               (make-code-tree (make-leaf 'd 1)
                                               (make-leaf 'c 1)))))

(define msg '(0 1 1 0 0 1 0 1 0 1 1 1 0))`

    expect(stringify(evaluateUntilDone(prelude + ` (decode msg tree)`))).toEqual(`(a d a b b c a)`)
  })

  test('39', () => {
    const prelude = `(define (f n)
    (if (< n 3)
        n
        (+ (f (- n 1))
           (* 2 (f (- n 2)))
           (* 3 (f (- n 3))))))`
    expect(stringify(evaluateUntilDone(prelude + ` (f 1)`))).toEqual(`1`)
    expect(stringify(evaluateUntilDone(prelude + ` (f 3)`))).toEqual(`4`)
    expect(stringify(evaluateUntilDone(prelude + ` (f 4)`))).toEqual(`11`)
  })

  test('40', () => {
    const prelude = `(define (fringe tree)
    (cond ((null? tree)
            '())
          ((not (pair? tree))
            (list tree))
          (else
            (append (fringe (car tree))
                    (fringe (cadr tree))))))
(define (count-leaves tree)
    (length (fringe tree)))`
    expect(
      stringify(evaluateUntilDone(prelude + ` (count-leaves (list (list 1 2) (list 3 4)))`))
    ).toEqual(`4`)
  })

  test('41', () => {
    const prelude = `
(define (element-of-set? x set)
    (cond ((null? set)
            #f)
          ((equal? x (car set))
            #t)
          (else
            (element-of-set? x (cdr set)))))`
    expect(
      evaluateUntilDone(prelude + ` (element-of-set? 5 (list 1 3 5 7 9 7 5 3 1))`)
    ).toHaveMatchingValue(makeBool(true))
    expect(
      evaluateUntilDone(prelude + ` (element-of-set? 10086 (list 1 3 5 7 9 7 5 3 1))`)
    ).toHaveMatchingValue(makeBool(false))
  })

  test('42', () => {
    const prelude = `
(define (adjoin-set x set)
    (cons x set))`
    expect(stringify(evaluateUntilDone(prelude + ` (adjoin-set 1 (list 2 3 4))`))).toEqual(
      `(1 2 3 4)`
    )
    expect(stringify(evaluateUntilDone(prelude + ` (adjoin-set 1 (list 1 3 5 7 9))`))).toEqual(
      `(1 1 3 5 7 9)`
    )
  })

  test('43', () => {
    const prelude = `
(define (union-set set another)
    (cond ((and (null? set) (null? another))
            '())
          ((null? set)
            another)
          ((null? another)
            set)
          (else
            (let ((x (car set)) (y (car another)))
                (cond ((= x y)
                        (cons x (union-set (cdr set) (cdr another))))
                      ((< x y)
                        (cons x (union-set (cdr set) another)))
                      ((> x y)
                        (cons y (union-set set (cdr another)))))))))`
    expect(stringify(evaluateUntilDone(prelude + ` (union-set '() (list 1 2 3))`))).toEqual(
      `(1 2 3)`
    )
    expect(
      stringify(evaluateUntilDone(prelude + ` (union-set (list 1 2 3) (list 1 3 5))`))
    ).toEqual(`(1 2 3 5)`)
    expect(
      stringify(evaluateUntilDone(prelude + ` (union-set (list 1 2 3) (list 1 3 5 7 9))`))
    ).toEqual(`(1 2 3 5 7 9)`)
  })

  test('44', () => {
    const prelude = `
(define (cons x y)
    (lambda (m)
        (m x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))`
    expect(stringify(evaluateUntilDone(prelude + ` (cdr (cons 1 2))`))).toEqual(`2`)
  })

  test('45', () => {
    const prelude = `
(define (cont-frac n d k)

    (define (cf i)
        (if (= k i)
            (/ (n k) (d k))
            (/ (n i)
               (+ (d i) (cf (+ i 1))))))

    (cf 1))

(define (golden-ratio k)
    (+ 1
       (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k)))
`
    expect(stringify(evaluateUntilDone(prelude + ` (golden-ratio 1)`))).toEqual(`2`)
  })

  test('46', () => {
    const prelude = `
(define (contents datum)
    (cond ((number? datum)
            datum)
          ((pair? datum)
            (cdr datum))
          (else
            (error "Bad tagged datum -- CONTENT" datum))))
`
    expect(stringify(evaluateUntilDone(prelude + ` (contents 10)`))).toEqual(`10`)
    expect(
      stringify(evaluateUntilDone(prelude + ` (contents (cons 'rectangular (cons 3 4)))`))
    ).toEqual(`(3 . 4)`)
  })

  test('47', () => {
    const prelude = `
(define (pascal row col)
    (cond ((> col row)
            (error "unvalid col value"))
          ((or (= col 0) (= row col))
            1)
          (else (+ (pascal (- row 1) (- col 1))
                   (pascal (- row 1) col)))))
`
    expect(stringify(evaluateUntilDone(prelude + ` (pascal 4 0)`))).toEqual(`1`)
    expect(stringify(evaluateUntilDone(prelude + ` (pascal 4 4)`))).toEqual(`1`)
    expect(stringify(evaluateUntilDone(prelude + ` (pascal 4 2)`))).toEqual(`6`)
  })

  test('48', () => {
    const prelude = `
(define (fold-right op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (fold-right op initial (cdr sequence)))))


 (define (fold-left op initial sequence)
   (define (iter result rest)
     (if (null? rest)
         result
         (iter (op result (car rest))
               (cdr rest))))
   (iter initial sequence))

(define (reverse-using-right items)
   (fold-right (lambda (first already-reversed)
                 (append already-reversed (list first)))
               '()
               items))

 (define (reverse-using-left items)
   (fold-left (lambda (result first) (cons first result))
              '()
              items))

(define items (list 1 2 3 4 5))
`
    expect(stringify(evaluateUntilDone(prelude + ' (reverse-using-right items)'))).toEqual(
      '(5 4 3 2 1)'
    )

    expect(stringify(evaluateUntilDone(prelude + '(reverse-using-left items)'))).toEqual(
      '(5 4 3 2 1)'
    )
  })

  test('49', () => {
    const prelude = `
(define (func inner)
  (lambda (x) (begin (set! inner (* inner x)) inner)))

(define f
  (func 1))
`
    expect(stringify(evaluateUntilDone(prelude + ` (+ (f 0) (f  1))`))).toEqual(`0`)
  })

  test('50', () => {
    const prelude = `
(define (insert-quene! quene item)
  (let ((new-pair (cons item '())))
    (cond ((empty-quene? quene)
       (set-front-ptr! quene new-pair)
       (set-rear-ptr! quene new-pair)
       quene)
    (else
     (set-cdr! (rear-ptr quene) new-pair)
     (set-rear-ptr! quene new-pair)
     quene))))

(define (delete-quene! q)
  (cond ((empty-quene? q)
     (error "DELETE! call with an empty quene" q))
     (else
      (set-front-ptr! q (cdr (front-ptr q)))))
  q)

(define make-quene
  (cons '() '()))


(define (front-ptr q)
  (car q))

(define (rear-ptr q)
  (cdr q))

(define (empty-quene? q)
  (and (null? (front-ptr q)) (null? (rear-ptr q))))

(define (set-front-ptr! quene item)
  (set-car! quene item))

(define (set-rear-ptr! quene item)
  (set-cdr! quene item))

(define q1 make-quene)


(define (print-quene q)
  (display (front-ptr q)))

`
    expect(stringify(evaluateUntilDone(prelude + ` (insert-quene! q1 'a)`))).toEqual(`((a) a)`)
  })

  test('51', () => {
    const prelude = `
(define (count-pair-imprv x)
  (cond ((not (pair? x)) 0)
    ((eq? (car x) (cdr x)) (+ 1 (count-pair-imprv (cdr x))))
    (else (+ 1 (count-pair-imprv (car x)) (count-pair-imprv (cdr x))))))

(define x (list 'a 'b 'c))

(define y (cons x x))
`
    expect(stringify(evaluateUntilDone(prelude + ` (count-pair-imprv y)`))).toEqual(`4`)
  })

  test('52', () => {
    const prelude = `
(define (count-pair-imprv x)
  (cond ((not (pair? x)) 0)
    ((eq? (car x) (cdr x)) (+ 1 (count-pair-imprv (cdr x))))
    (else (+ 1 (count-pair-imprv (car x)) (count-pair-imprv (cdr x))))))

(define x (list 'a 'b 'c))

(define y (cons x x))
`
    expect(stringify(evaluateUntilDone(prelude + ` (count-pair-imprv y)`))).toEqual(`4`)
  })

  test('53', () => {
    const prelude = `
(define (make-deque)
  (let ((front-ptr '())
      (ahead-rear '())
      (rear-ptr '()))


    (define (front-insert! item)
      (let ((new-pair (list item)))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else (set! front-ptr (cons item front-ptr))
                front-ptr))))

    (define (front-delete!)
      (cond ((empty?) (error "quene empty -- FRONT-DELETE"))
            (else (set! front-ptr (cdr front-ptr))
              (if (null? front-ptr)
                  (set! rear-ptr '()))
              front-ptr)))


    (define (delete-rear!)
      (define (helper deque)
    (cond ((empty?) (error "empty deque -- AHEAD-REAR"))
          ((null? (cdr deque)) '())
          ((null? (cdr (cdr deque))) (list (car deque)))
          (else (cons (car deque) (helper (cdr deque))))))
      (set! front-ptr (helper front-ptr))
      front-ptr)


    (define (empty?)
      (null? front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'front-insert!) front-insert!)
        ((eq? m 'front-delete!) front-deleted!)
        ((eq? m 'delete-rear!) delete-rear!)
        (else (error "unknown operation -- DISPATCH" m))))
    dispatch))




(define a (make-deque))
`
    expect(stringify(evaluateUntilDone(prelude + ` ((a 'front-insert!) 'k)`))).toEqual(`(k)`)
  })

  test('54', () => {
    const prelude = `(define (count-pair-imprv x)
  (cond ((not (pair? x)) 0)
    ((eq? (car x) (cdr x)) (+ 1 (count-pair-imprv (cdr x))))
    (else (+ 1 (count-pair-imprv (car x)) (count-pair-imprv (cdr x))))))

(define x (list 'a 'b 'c))

(define y (cons x x))
`
    expect(stringify(evaluateUntilDone(prelude + ` (count-pair-imprv y)`))).toEqual(`4`)
  })

  test('55', () => {
    const prelude = `
(define (identity x)
  (cond ((not (pair? x)) x)
    (else (cons (identity (car x)) (identity (cdr x))))))




(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe tree)
  (cond ((not (pair? tree))
     (if (null? tree)
         '()
         (list tree)))
    (else (append (fringe (car tree)) (fringe (cdr tree))))))

`
    expect(
      stringify(evaluateUntilDone(prelude + ` (fringe (list 1 2 3(list 4 5) (list 5 6) 6))`))
    ).toEqual(`(1 2 3 4 5 5 6 6)`)
  })

  test('56', () => {
    const prelude = `
(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b))))`
    expect(
      stringify(evaluateUntilDone(prelude + ` (product (lambda (x) x) 1(lambda (i) (+ i 1))10)`))
    ).toEqual(`3628800`)
  })

  test('57', () => {
    const prelude = `
(define (product term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (+ a 1)
                  (* (term a) result))))
    (iter a 1))`
    expect(
      stringify(evaluateUntilDone(prelude + ` (product (lambda (x) x) 1 (lambda (i) (+ i 1)) 10)`))
    ).toEqual(`3628800`)
  })

  test('58', () => {
    const prelude = `
(define (mystery x)
  (define (loop x y)
    (if (null? x)
    y
    (let ((tmp (cdr x)))
      (set-cdr! x y)
      (loop tmp x))))
  (loop x '()))
(define v (list 'a 'b 'c 'd))

(define w (mystery v))`
    expect(stringify(evaluateUntilDone(prelude + ` w`))).toEqual(`(d c b a)`)
  })

  test('59', () => {
    const prelude = `
(define (cons  x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))


(define (cdr z)
  (z (lambda (p q) q)))
`
    expect(stringify(evaluateUntilDone(prelude + ` (car (cons 1 2))`))).toEqual(`1`)
  })

  test('60', () => {
    const prelude = `
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
`
    expect(stringify(evaluateUntilDone(prelude + ` (length '(a b c) )`))).toEqual(`3`)
  })

  test('61', () => {
    const prelude = `
(define (square x)
  (* x x))
(define (map proc items)
  (cond ((null? items) items)
    (else (cons (proc (car items)) (map proc (cdr items))))))

(define (square-tree tree)
  (cond ((null? tree) '())
    ((not (pair? tree)) (square tree))
    (else (cons (square-tree (car tree))
            (square-tree (cdr tree))))))




(define (square-tree-map tree)
  (map (lambda (subtree)
     (cond ((pair? subtree) (square-tree-map subtree))
           (else (square subtree))))

       tree))
`
    expect(
      stringify(
        evaluateUntilDone(prelude + ` (square-tree-map (list 2 3 5 (list 5 6) 5 (list 22 3)))`)
      )
    ).toEqual(`(4 9 25 (25 36) 25 (484 9))`)
  })

  test('62', () => {
    const prelude = `(if (> 3 2) (- 3 2) (+ 3 2))`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`1`)
  })

  test('63', () => {
    const prelude = `(define x 2) (set! x 4)`
    expect(stringify(evaluateUntilDone(prelude + ` (+ x 1)`))).toEqual(`5`)
  })

  test('64', () => {
    const prelude = `(cond ((> 3 2) 'greater) ((< 3 2) 'less))`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`greater`)
  })

  test('65', () => {
    const prelude = `(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`equal`)
  })

  test('66', () => {
    const prelude = `(and (= 2 2) (> 2 1))`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(true))
  })

  test('67', () => {
    const prelude = `(and (= 2 2) (< 2 1))`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(false))
  })

  test('68', () => {
    const prelude = `(and 1 2 'c '(f g))`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`(f g)`)
  })

  test('69', () => {
    const prelude = `(and 1 2 'c '(f g))`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`(f g)`)
  })

  test('70', () => {
    const prelude = `(and)`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(true))
  })

  test('71', () => {
    const prelude = `(or (= 2 2) (> 2 1))`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(true))
  })

  test('72', () => {
    const prelude = `(or #f #f #f)`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(false))
  })

  test('73', () => {
    const prelude = `(let ((x 2) (y 3)) (* x y))`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`6`)
  })

  test('74', () => {
    const prelude = `(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))`
    expect(stringify(evaluateUntilDone(prelude))).toEqual(`35`)
  })

  test('75', () => {
    const prelude = `(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y)))  (* z x)))`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`70`)
  })

  test('76', () => {
    const prelude = `(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y)))  (* z x)))`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`70`)
  })

  test('77', () => {
    const prelude = `(define x 0)`
    expect(stringify(evaluateUntilDone(prelude + ` (begin (set! x 5) (+ x 1)) `))).toEqual(`6`)
  })

  test('78', () => {
    const prelude = `((lambda x x) 3 4 5 6)`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`(3 4 5 6)`)
  })

  test('79', () => {
    const prelude = `((lambda (x y . z) z) 3 4 5 6) `
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`(5 6)`)
  })

  test('80', () => {
    const prelude = `(define reverse-subtract (lambda (x y) (- y x))) (reverse-subtract 7 10) `
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`3`)
  })

  test('81', () => {
    const prelude = `((if #f + *) 3 4)`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`12`)
  })

  test('82', () => {
    const prelude = `(define first car)`
    expect(stringify(evaluateUntilDone(prelude + ` (first '(1 2))`))).toEqual(`1`)
  })

  test('83', () => {
    const prelude = `(let ((x 5)) (define foo (lambda (y) (bar x y))) (define bar (lambda (a b) (+ (* a b) a))) (foo (+ x 3)))`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`45`)
  })

  test('84', () => {
    const prelude = `(not #t)`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(false))
  })

  test('85', () => {
    const prelude = `(not (list))`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(false))
  })

  test('86', () => {
    const prelude = `(let ((x '(a))) (eq? x x)) `
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(true))
  })

  test('87', () => {
    const prelude = `(let ((p (lambda (x) x))) (eq? p p))`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(true))
  })

  test('88', () => {
    const prelude = `(let ((p (lambda (x) x))) (eq? p p))`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(true))
  })

  test('89', () => {
    const prelude = `(eq? car car)`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(true))
  })

  test('90', () => {
    const prelude = `(eq? '() '())`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(true))
  })

  test('91', () => {
    const prelude = `(eq? '() '())`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(true))
  })

  test('92', () => {
    const prelude = `(eq? 'mISSISSIppi 'mississippi)`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(true))
  })

  // This test fails for the macro sublanguage because the macros assume keywords have not been redefined
  // test('93', () => {
  //   const prelude = `(let ((=> #f)) (cond (#t => 'ok)))`
  //   expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`ok`)
  // })

  test('94', () => {
    const prelude = `(define add4 (let ((x 4)) (lambda (y) (+ x y)))) (add4 6)`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`10`)
  })

  test('95', () => {
    const prelude = `(define add4 (let ((x 4)) (lambda (y) (+ x y)))) (add4 6)`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`10`)
  })

  test('96', () => {
    const prelude = `(eqv? (cons 1 2) (cons 1 2))`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(false))
  })

  test('97', () => {
    const prelude = `(equal? "abc" "abc")`
    expect(evaluateUntilDone(prelude + ` `)).toHaveMatchingValue(makeBool(true))
  })

  test('98', () => {
    const prelude = `(append '(a b) '(c . d))`
    expect(stringify(evaluateUntilDone(prelude + ` `))).toEqual(`(a b c . d)`)
  })
})
