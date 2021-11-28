#lang plai-typed

(define-type Binding
  [binding (name : symbol) (named-expr : CFWAE)])

(define-type CFWAE
  [num (n : number)]
  [binop (op : (number number -> number)) (lhs : CFWAE) (rhs : CFWAE)]
  [with (lob : (listof Binding)) (body : CFWAE)]
  [id (name : symbol)]
  [if0 (c : CFWAE) (t : CFWAE) (e : CFWAE)]
  [fun (args : (listof symbol)) (body : CFWAE)]
  [app (f : CFWAE) (args : (listof CFWAE))])

(define-type Env
  [mtEnv]
  [anEnv (name : symbol) (value : CFWAE-Value) (env : Env)])

(define-type CFWAE-Value
  [numV (n : number)]
  [closureV (params : (listof symbol))
            (body : CFWAE)
            (env : Env)])

;; parse : expression -> CFWAE
;; This procedure parses an expression into a CFWAE
(define (parse (sexp : s-expression)) : CFWAE
  (cond
    [(s-exp-number? sexp) (num (s-exp->number sexp))]
    [(s-exp-symbol? sexp) (id (check-id (s-exp->symbol sexp)))]
    [(s-exp-list? sexp)
     (let ([sl (s-exp->list sexp)])
       (if (empty? sl)
           (error 'parse "invalid input")
           (if (s-exp-symbol? (first sl))
               (case (s-exp->symbol (first sl))
                 [(+ - * /) (binop (binop-symbol->op (s-exp->symbol (first sl)))
                                   (parse (second sl))
                                   (parse (third sl)))]
                 [(if0) (if0 (parse (second sl))
                             (parse (third sl))
                             (parse (fourth sl)))]
                 [(with) (with (parse-bindings (second sl))
                               (parse (third sl)))]
                 [(fun) (fun (parse-params (second sl))
                             (parse (third sl)))]
                 [else (error 'parse "invalid input")])
               (app (parse (first sl))
                    (parse-args (rest sl))))))]))

(define (check-id (id : symbol)) : symbol
  (case id
    [(+ - * / with if0 fun) (error 'parse "invalid id")]
    [else id]))

(define (binop-symbol->op sym)
  (case sym
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]))

(define (parse-bindings (bindings : s-expression)) : (listof Binding)
  (if (s-exp-list? bindings)
    (reverse (parse-bindings-aux (s-exp->list bindings) empty))
    (error 'parse "invalid input")))

(define (parse-bindings-aux (bindings : (listof s-expression))
                            (result : (listof Binding)))
  (if (empty? bindings)
      result
      (let ([b (parse-binding (first bindings))])
        (if (member (binding-name b) (map binding-name result))
            (error 'parse "duplicate binding")
            (parse-bindings-aux (rest bindings)
                                (cons b result))))))

(define (parse-binding (bbinding : s-expression))
  (if (s-exp-list? bbinding)
      (let ([b (s-exp->list bbinding)])
        (if (and (= (length b) 2)
                 (s-exp-symbol? (first b)))
            (binding (check-id (s-exp->symbol (first b)))
                     (parse (second b)))
            (error 'parse "invalid input")))
      (error 'parse "invalid input")))

(test (parse-binding '(x 1)) (binding 'x (num 1)))
(test/exn (parse-binding '(+ 1)) "parse: invalid id")
(test/exn (parse-binding '(x if0)) "parse: invalid id")
(test/exn (parse-binding '(x 1 2)) "parse: invalid input")
(test/exn (parse-binding '(1 2)) "parse: invalid input")
(test (parse-bindings '((x 1) (y 1)))
      (list (binding 'x (num 1)) (binding 'y (num 1))))
(test (parse-bindings '((x 1) (y x)))
      (list (binding 'x (num 1)) (binding 'y (id 'x))))
(test (parse-bindings '()) empty)
(test/exn (parse-bindings '1) "parse: invalid input")
(test/exn (parse-bindings '((x 1) (x x)))
          "parse: duplicate binding")

(define (parse-params (params : s-expression)) : (listof symbol)
  (if (s-exp-list? params)
      (reverse (parse-params-aux (s-exp->list params) empty))
      (error 'parse "invalid input")))

(define (parse-params-aux (params : (listof s-expression))
                          (result : (listof symbol)))
  (if (empty? params)
      result
      (if (s-exp-symbol? (first params))
          (let ([param (s-exp->symbol (first params))])
            (if (member param result)
                (error 'parse "duplicate parameter")
                (parse-params-aux (rest params)
                                  (cons (check-id param) result))))
          (error 'parse "invalid input"))))

(test (parse-params '(a b c)) (list 'a 'b 'c))
(test (parse-params '()) empty)
(test/exn (parse-params '(a a)) "parse: duplicate parameter")
(test/exn (parse-params '(if0)) "parse: invalid id")
(test/exn (parse-params '(())) "parse: invalid input")

(define (parse-args (args : (listof s-expression))) : (listof CFWAE)
  (map parse args))

(test (parse '{with {{x 10} {y 20}} y})
      (with (list (binding 'x (num 10)) (binding 'y (num 20))) (id 'y)))
(test/exn (parse '{with {{x 10} {x 20}} y})
          "parse: duplicate binding")
(test (parse '{fun {x y} 10}) (fun (list 'x 'y) (num 10)))
(test (parse '{fun {} x}) (fun empty (id 'x)))
(test/exn (parse '{fun {x x} 10}) "parse: duplicate parameter")
(test (parse '((fun (x y) (with ((z (+ x y))) (if0 z x y))) 1 2))
      (app (fun (list 'x 'y)
                (with (list (binding 'z (binop + (id 'x) (id 'y))))
                      (if0 (id 'z) (id 'x) (id 'y))))
           (list (num 1) (num 2))))

;; interp : CFWAE -> CFWAE-Value
;; This procedure interprets the given CFWAE and produces a result 
;; in the form of a CFWAE-Value (either a closuerV or a numV)
(define (interp (expr : CFWAE)) : CFWAE-Value
  (itp expr (mtEnv)))

(define (itp (expr : CFWAE) (env : Env)) : CFWAE-Value
  (type-case CFWAE expr
    [num (n) (numV n)]
    [id (name) (lookup name env)]
    [binop
     (op lhs rhs)
     (let ([l (itp lhs env)]
           [r (itp rhs env)])
       (if (and (numV? l) (numV? r))
           (numV (do-binop op (numV-n l) (numV-n r)))
           (error 'interp "binop should be applied to number")))]
    [with
     (bindings body)
     (itp body (extend-env-with bindings env))]
    [if0
     (c t e)
     (let ([c-value (itp c env)])
       (if (numV? c-value)
           (if (= 0 (numV-n c-value))
               (itp t env)
               (itp e env))
           (error 'interp "if0 only accepts number as predicate")))]
    [fun (params body) (closureV params body env)]
    [app
     (f args)
     (let ([closure (itp f env)])
       (if (closureV? closure)
           (itp (closureV-body closure)
                (extend-env-app closure args env))
           (error 'interp "app should apply to a fun")))]))

(define (do-binop (op : (number number -> number)) l r) : number
  (if (and (eq? op /)
           (= r 0))
      (error 'interp "division by zero")
      (op l r)))
    
(define (lookup (name : symbol) (env : Env)) : CFWAE-Value
  (type-case Env env
    [mtEnv () (error 'interp "unbound identifier")]
    [anEnv (n v e)
           (if (symbol=? n name)
               v
               (lookup name e))]))

(define (extend-env-with bindings env)
  (extend-env (map binding-name bindings)
              (map (lambda (b)
                     (itp (binding-named-expr b) env))
                   bindings)
              env))

(define (extend-env-app closure args env)
  (let* ([ids (closureV-params closure)]
         [vals (map (lambda (arg) (itp arg env)) args)]
         [length-i (length ids)]
         [length-v (length vals)])
    (cond
      [(< length-i length-v) (error 'interp "too many arguments")]
      [(> length-i length-v) (error 'interp "too few arguments")]
      [else (extend-env ids vals (closureV-env closure))])))

(define (extend-env ids vals env)
  (if (empty? ids)
      env
      (extend-env (rest ids)
                  (rest vals)
                  (anEnv (first ids) (first vals) env))))

(test (interp (parse '{with {{x 10} {y 20}} y}))
      (numV 20))
(test (interp (parse '{fun {} x}))
      (closureV empty (id 'x) (mtEnv)))
(test (interp (parse '((fun (x y) (with ((z (+ x y))) (if0 z x y))) 1 2)))
      (numV 2))
(test (interp (parse '((fun (x y) (with ((z (+ x y))) (if0 z x y))) 1 -1)))
      (numV 1))
(test/exn (interp (parse '(/ 1 0)))
          "division by zero")
(test/exn (interp (parse '((fun (x y) 0 1))))
          "too few arguments")
(test/exn (interp (parse '((fun (x y) 0) 1 2 3)))
          "too many arguments")