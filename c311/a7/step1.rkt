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
;; Step 1
;;;;;;;;;;;

(define value-of-cps
  (lambda (expr env-cps k^)
    (match expr
      [`(const ,expr) (k^ expr)]
      [`(mult ,x1 ,x2) (value-of-cps x2 env-cps (lambda (v) (value-of-cps x1 env-cps (lambda (w) (k^ (* v w))))))]
      [`(sub1 ,x) (value-of-cps x env-cps (lambda (v) (k^ (sub1 v))))]
      [`(zero ,x) (value-of-cps x env-cps (lambda (v) (k^ (zero? v))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env-cps (lambda (v) (if v
                                                                           (value-of-cps conseq env-cps k^)
                                                                           (value-of-cps alt env-cps k^))))]
      [`(let/cc ,body) (value-of-cps body (lambda (y k^^) (if (zero? y)
                                                              (k^^ k^)
                                                              (env-cps y k^^))) k^)]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env-cps (λ (k)(value-of-cps v-exp env-cps k)))]
      [`(let ,e ,body) (value-of-cps e env-cps
                                     (lambda (a)
                                       (value-of-cps body (lambda (y k^^) (if (zero? y)
                                                                              (k^^ a)
                                                                              (env-cps (sub1 y)))) k^)))]
      [`(var ,expr) (env-cps expr k^)]
      [`(lambda ,body) (k^ (lambda (a k^^) (value-of-cps body (lambda (y k^^^) (if (zero? y)
                                                                                   (k^^^ a)
                                                                                   (env-cps (sub1 y) k^^^))) k^^)))]
      [`(app ,rator ,rand) (value-of-cps rator env-cps (lambda (c-cps)
                                                         (value-of-cps rand env-cps
                                                                       (lambda (arg)
                                                                         (c-cps arg k^)))))])))


(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))




