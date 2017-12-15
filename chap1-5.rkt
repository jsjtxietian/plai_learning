#lang plai-typed

(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])

(define ma1 : MisspelledAnimal (caml 2))
(define ma2 : MisspelledAnimal (yacc 1.9))

;;; (define (good? [ma : MisspelledAnimal]) : boolean
;;;   (type-case MisspelledAnimal ma
;;;     [caml (humps) (>= humps 2)]
;;;     [yacc (height) (> height 2.1)]))

(define (good? [ma : MisspelledAnimal]) : boolean
  (type-case MisspelledAnimal ma
             [caml (h) (>= h 2)]
             [yacc (h) (> h 2.1)]))

;;; (define (good? [ma : MisspelledAnimal]) : boolean
;;;   (cond
;;;     [(caml? ma) (>= caml-humps ma) 2]
;;;     [(yacc? ma) (> (yacc-height ma) 2.1)]))


;;; (test (good? ma1) #t)
;;; (test (good? ma2) #f)



(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

;;; (define (parse [s : s-expression])
;;;   (cond
;;;     [(s-exp-number? s) (numC (s-exp->number s))]
;;;     [(s-exp-list? s)
;;;      (let ([sl (s-exp->list s)])
;;;        (case (s-exp->symbol (first sl))
;;;          [(+) (plusC (parse (second sl)) (parse (third sl)))]
;;;          [(*) (multC (parse (second sl)) (parse (third sl)))]
;;;          [else (error 'parse "invalid list input")]))]  ;无效的表输入
;;;     [else (error 'parse "invalid input")]))  ;无效的输入

;;; (parse '(+ (* 1 2) (+ 2 3)))

;;; (define (interp [a : ArithC]) : number
;;;   (type-case ArithC a
;;;              [numC (n) n]
;;;              [plusC (l r) (+ (interp l) (interp r))]
;;;              [multC (l r) (* (interp l) (interp r))]))

(define-type ArithS  ;表层算术
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

;去语法糖
(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]))  ;二元减法子句

(define (parse [s : s-expression])
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]  ;无效的表输入
    [else (error 'parse "invalid input")]))  ;无效的输入

(define (interp [a : ArithC]) : number
    (type-case ArithC a
             [numC (n) n]
             [plusC (l r) (+ (interp l) (interp r))]
             [multC (l r) (* (interp l) (interp r))]))

(define (interpS [a : ArithS]) ;:number
    (interp (desugar a)))