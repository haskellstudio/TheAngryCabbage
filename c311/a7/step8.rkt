#lang racket

(define last-non-zero
  (λ (ls)
    (let/cc k
      (letrec
          ((last-non-zero
            (λ (ls)
              (cond
                [(empty? ls) '()]
                [(zero? (car ls)) (k (last-non-zero (cdr ls)))]
                [else (cons (car ls) (last-non-zero (cdr ls)))]))))
        (last-non-zero ls)))))
(define lex
  (lambda (expr acc)
    (match expr
      [`,y #:when (number? y) `(const ,y)]
      [`,y #:when (symbol? y) `(var ,(look-up y acc))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x acc)))]
      [`(zero? ,nexp) `(zero ,(lex nexp acc))]
      [`(sub1 ,x) `(sub1 ,(lex x acc))]
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
      [`(if ,test ,conseq ,alt) `(if ,(lex test acc)
                                     ,(lex conseq acc)
                                     ,(lex alt acc))]
      [`(let ([,id ,val]) ,body) `(let ,(lex val acc) ,(lex body (cons id acc)))]
      ;;let/cc
      [`(let/cc ,x ,b) `(let/cc ,(lex b (cons x acc)))]
      ;;throw
      [`(throw ,k-exp ,v-exp) `(throw ,(lex k-exp acc) ,(lex v-exp acc))]
      [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))])))
(define look-up          
  (lambda (x acc)
    (cond
      [(null? acc) #f]
      [(eqv? x (car acc)) 0]
      [else (add1 (look-up x (cdr acc)))])))







;;;;;;;;;;;
;; Step 3
;;;;;;;;;;;


(define value-of-cps
  (lambda (expr env-cps k^)
    (match expr
      [`(const ,expr) (apply-k k^ expr)] ;;apply-k
      [`(mult ,x1 ,x2) (value-of-cps x2 env-cps (lambda (v) (value-of-cps x1 env-cps (lambda (w) (apply-k k^ (* v w))))))] ;;apply-k
      [`(sub1 ,x) (value-of-cps x env-cps (lambda (v) (apply-k k^ (sub1 v))))] ;;apply-k
      [`(zero ,x) (value-of-cps x env-cps (lambda (v) (apply-k k^ (zero? v))))] ;;apply-k
      [`(if ,test ,conseq ,alt) (value-of-cps test env-cps (lambda (v) (if v
                                                                           (value-of-cps conseq env-cps k^)
                                                                           (value-of-cps alt env-cps k^))))]
      [`(let/cc ,body) (value-of-cps body (extend-env env-cps body k^) k^)] ;;apply-env
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env-cps (λ (k)(value-of-cps v-exp env-cps k)))] ;;didn't really do much w/ this line
      [`(let ,e ,body) (value-of-cps e env-cps
                                     (lambda (a)
                                       (value-of-cps body (extend-env env-cps e a) k^)))] ;;extend environment
      [`(var ,expr) (apply-env env-cps expr k^)] ;;apply-env
      [`(lambda ,body) (apply-k k^(make-closure env-cps k^ body))] ;;make-closure
      [`(app ,rator ,rand) (value-of-cps rator env-cps (lambda (c-cps)
                                                         (value-of-cps rand env-cps
                                                                       (lambda (arg)
                                                                         (apply-closure c-cps arg k^)))))]))) ;;apply-closure 




(define empty-env
  (lambda ()
    `(empty-env)))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))


(define apply-env
  (lambda (env-cps y k)
    (match env-cps
      [`(empty-env) (error 'value-of "unbound identifier")]
      [`(extend-env ,env-cps ,x ,arg)
       (if (zero? y)
           (apply-k k arg)
           (apply-env env-cps (sub1 y) k))])))


(define apply-closure
  (lambda (c a k)
    (c a k)))

(define apply-k
  (lambda (k v)
    (k v)))

(define extend-env
  (lambda (env-cps x arg)
    `(extend-env ,env-cps ,x ,arg)))


(define make-closure
  (lambda (env-cps x body)
    (lambda (arg k)
      (value-of-cps body (extend-env env-cps x arg) k))))

