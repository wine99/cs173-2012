#lang plai-typed

(require "typed-lang.rkt")

(define (make-ids (n : number)) : (listof symbol)
  (build-list n (lambda (n) (string->symbol (string-append "var-" (to-string n))))))

;; cascade-lets will build up the nested lets, and use body as the
;; eventual body, preserving order of evaluation of the expressions
(define (cascade-lets (ids : (listof symbol))
                      (exprs : (listof ExprC))
                      (body : ExprC)) : ExprC
  (cond [(empty? ids) body]
        [(cons? ids)
         (LetC (first ids) (first exprs) (cascade-lets (rest ids) (rest exprs) body))]))

;; check-type builds an expression that checks the type of the expression
;; given as an argument
(define (check-type (expr : ExprC) (type : string)) : ExprC
  (Prim2C '== (Prim1C 'tagof expr) (StrC type)))

;; and builds up an and expression from its two pieces
(define (and (expr1 : ExprC) (expr2 : ExprC)) : ExprC
  (IfC expr1 expr2 (FalseC)))

;; all builds up a series of ands over the expression arguments
(define (all (exprs : (listof ExprC))) : ExprC
  (foldl (lambda (exp result) (and exp result)) (TrueC) exprs))

;; map-subtract builds an expression that maps 'num- over a list of expressions
(define (map-subtract (exprs : (listof ExprC))) : ExprC
  (foldl (lambda (expr result) (Prim2C 'num- result expr)) (first exprs) (rest exprs)))

(define (map-add (op : symbol) (exprs : (listof ExprC))) : ExprC
  (foldl (lambda (expr result) (Prim2C op result expr)) (first exprs) (rest exprs)))

(define (desugar-subtract (args : (listof ExprP))) : ExprC
  (local ([define ids (make-ids (length args))]
          [define id-exps (map IdC ids)])
    (cascade-lets ids (map desugar args)
                  (IfC (all (map (lambda (e) (check-type e "number")) id-exps))
                       (map-subtract id-exps)
                       (ErrorC (StrC "Bad arguments to -"))))))

(define (desugar-add args)
  (local ([define ids (make-ids (length args))]
          [define id-exps (map IdC ids)])
    (cascade-lets
     ids (map desugar args)
     (IfC (check-type (first id-exps) "number")
          (IfC (all (map (lambda (e) (check-type e "number")) id-exps))
               (map-add 'num+ id-exps)
               (ErrorC (StrC "Bad arguments to +")))
          (IfC (check-type (first id-exps) "string")
               (IfC (all (map (lambda (e) (check-type e "string")) id-exps))
                    (map-add 'string+ id-exps)
                    (ErrorC (StrC "Bad arguments to +")))
               (ErrorC (StrC "Bad arguments to +")))))))

(define (desugar-SeqP exps)
  (cond
    [(< (length exps) 1) (ErrorC (StrC "no expression in SeqP"))]
    [(= (length exps) 1) (desugar (first exps))]
    [else (SeqC (desugar (first exps))
                (desugar-SeqP (rest exps)))]))

(define (bin-op (op : symbol) (e1 : ExprC) (e2 : ExprC)) : ExprC
  (case op
    ['- 
     (IfC (and (check-type e1 "number") (check-type e2 "number"))
          (Prim2C 'num- e1 e2)
          (ErrorC (StrC "Bad arguments to -")))]
    ['+ 
     (IfC (and (check-type e1 "number") (check-type e2 "number"))
          (Prim2C 'num+ e1 e2)
          (IfC (and (check-type e1 "string") (check-type e2 "string"))
               (Prim2C 'string+ e1 e2)
               (ErrorC (StrC "Bad arguments to +"))))]))

(define (desugar (exprP : ExprP)) : ExprC
  (type-case ExprP exprP

    [NumP (n) (NumC n)]
    [StrP (s) (StrC s)]
    [TrueP () (TrueC)]
    [FalseP () (FalseC)]

    [ObjectP
     (fields)
     (ObjectC
      (map (lambda (field)
             (type-case FieldP field
               [fieldP (name value)
                       (fieldC name (desugar value))]))
           fields))]

    [DotP
     (obj field)
     (GetFieldC (desugar obj) (StrC (symbol->string field)))]
    [BracketP
     (obj field)
     (GetFieldC (desugar obj) (desugar field))]
    [DotMethodP
     (obj field args)
     (LetC 'temp-self (desugar obj)
           (AppC (GetFieldC (IdC 'temp-self)
                            (StrC (symbol->string field)))
                 (cons (IdC 'temp-self) (map desugar args))))]
    [BrackMethodP
     (obj field args)
     (LetC 'temp-self (desugar obj)
           (AppC (GetFieldC (IdC 'temp-self)
                            (desugar field))
                 (cons (IdC 'temp-self) (map desugar args))))]

    [FuncP (args body)
           (FuncC args (desugar body))]
    [AppP (func args)
          (AppC (desugar func) (map desugar args))]
    [DefvarP (id value body)
             (LetC id (desugar value) (desugar body))]
    [DeffunP (name ids funbody body)
             (LetC name (FuncC empty (ErrorC (StrC "Dummy function")))
	           (SeqC (Set!C name
		                (FuncC ids (desugar funbody)))
			 (desugar body)))]
    [IdP (name) (IdC name)]

    [AssignP
     (lhs value)
     (type-case LHS lhs
       [BracketLHS
        (obj field)
        (SetFieldC (desugar obj) (desugar field) (desugar value))]
       [DotLHS
        (obj field)
        (SetFieldC (desugar obj)
                   (StrC (symbol->string field))
                   (desugar value))]
       [IdLHS (id) (Set!C id (desugar value))])]

    [SeqP (es)
          (desugar-SeqP es)]
    [IfP (pred then alternative)
         (IfC (desugar pred) (desugar then) (desugar alternative))]

    [PrimP
     (op args)
     (case op
       ['-
        (cond
          [(= 0 (length args)) (ErrorC (StrC "Empty list for prim op"))]
          [(< 0 (length args)) (desugar-subtract args)])]
       ['+
        (cond
          [(= 0 (length args)) (ErrorC (StrC "Empty list for prim op"))]
          [(< 0 (length args)) (desugar-add args)])]
       [(> < ==)
        (cond
          [(= (length args) 2)
           (Prim2C op (desugar (first args)) (desugar (second args)))]
          [else (ErrorC (StrC "Bad primop"))])]
       [(print tagof)
        (cond
          [(= 1 (length args)) (Prim1C op (desugar (first args)))]
          [else (ErrorC (StrC "Bad primop"))])]
       [else (ErrorC (StrC "Bad primop"))])]

    [PrimAssignP
     (op lhs value)
     (type-case LHS lhs
       [BracketLHS
        (obj field)
        (LetC 'obj-value (desugar obj)
              (LetC 'field-name-value (desugar field)
                    (SetFieldC (IdC 'obj-value)
                               (IdC 'field-name-value)
                               (bin-op op
                                       (GetFieldC (IdC 'obj-value)
                                                  (IdC 'field-name-value))
                                       (desugar value)))))]
       [DotLHS
         (obj field)
         (LetC 'obj-value (desugar obj)
               (SetFieldC (IdC 'obj-value)
                          (StrC (symbol->string field))
                          (bin-op op
                                  (GetFieldC (IdC 'obj-value)
                                             (StrC (symbol->string field)))
                                  (desugar value))))]
       [IdLHS
         (id)
         (Set!C id (bin-op op (IdC id) (desugar value)))])]

    [PreIncP (lhs) (Set!C lhs (Prim2C 'num+ (IdC lhs) (NumC 1)))]
    [PreDecP (lhs) (Set!C lhs (Prim2C 'num- (IdC lhs) (NumC 1)))]
    [PostIncP (lhs) (LetC 'old-value (IdC lhs)
                          (SeqC (Set!C lhs (Prim2C 'num+ (IdC lhs) (NumC 1)))
                                (IdC 'old-value)))]
    [PostDecP (lhs) (LetC 'old-value (IdC lhs)
                          (SeqC (Set!C lhs (Prim2C 'num- (IdC lhs) (NumC 1)))
                                (IdC 'old-value)))]

    [WhileP
     (test body)
     ;; dummy-fun will tell us it was called if we do so accidentally
     (local ([define dummy-fun (FuncC (list) (ErrorC (StrC "Dummy function")))])
       (IfC (desugar test)

            ;; while-var will hold the actual function once we tie
            ;; everything together
            (LetC 'while-var dummy-fun
                  (LetC 'while-func

                        ;; this function does the real work - it runs the body of
                        ;; the while loop, then re-runs it if the test is true, and
                        ;; stops if its false
                        (FuncC (list)
                               (LetC 'temp-var
                                     (desugar body)
                                     (IfC (desugar test)
                                          (AppC (IdC 'while-var) (list))
                                          (IdC 'temp-var))))

                        ;; The Set!C here makes sure that 'while-var will resolve
                        ;; to the right value later, and the AppC kicks things off
                        (SeqC (Set!C 'while-var (IdC 'while-func))
                              (AppC (IdC 'while-var) (list)))))

            (FalseC)))]

    [ForP
     (init test update body)
     (LetC 'init-value (desugar init)
           (IfC (desugar test)
                (LetC 'for-recur-fn
                      (FuncC empty (ErrorC (StrC "Dummy function")))
                      (SeqC (Set!C 'for-recur-fn
                                   (FuncC empty
                                          (LetC 'body-value (desugar body)
                                                (SeqC (desugar update)
                                                      (IfC (desugar test)
                                                           (AppC (IdC 'for-recur-fn)
                                                                 empty)
                                                           (IdC 'body-value))))))
                            (AppC (IdC 'for-recur-fn) empty)))
                (IdC 'init-value)))]))
