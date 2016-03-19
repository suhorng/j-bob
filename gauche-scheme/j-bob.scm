
(define (quote-c value)
  `',value)
(define (quote? x)
  (match x
    [('quote ,val) #t]
    [else #f]))
(define (quote.value e)
  (match e
    [('quote ,val) val]
    [else (error 'quote.value (format "Non-quote value: ~s" e))]))

(define (if-c Q A E) `(if ,Q ,A ,E))
(define (if? x)
  (match x
    [(if ,Q ,A ,E) #t]
    [else #f]))
(define (if.Q e)
  (match e
    [(if ,Q ,A ,E) Q]
    [else (error 'if.Q (format "Non-if expression: ~s" e))]))
(define (if.A e)
  (match e
    [(if ,Q ,A ,E) A]
    [else (error 'if.A (format "Non-if expression: ~s" e))]))
(define (if.E e)
  (match e
    [(if ,Q ,A ,E) E]
    [else (error 'if.E (format "Non-if expression: ~s" e))]))

(define (app-c name args) `(,name . ,args))
(define (app? x)
  (match x
    [('quote . ,dr) #f]
    [(if . ,dr) #f]
    [(,name . ,args) #t]
    [else #f]))
(define (app.name e)
  (match e
    [(,name . ,args) name]
    [else (error 'app.name (format "Non-application: ~s" e))]))
(define (app.args e)
  (match e
    [(,name . ,args) args]
    [else (error 'app.args (format "Non-application: ~s" e))]))

(define (var? x)
  (match x
    ['t #f]
    ['nil #f]
    [,n (guard (equal? (natp n) 't)) #f]
    [(,ar . ,dr) #f]
    [else #t]))

(define (defun-c name formals body)
  `(defun ,name ,formals ,body))
(define (defun? x)
  (match x
    [(defun ,name ,formals ,body) #t]
    [else #f]))
(define (defun.name def)
  (match def
    [(defun ,name ,formals ,body) name]
    [else (error 'defun.name (format "Non-defun: ~s" def))]))
(define (defun.formals def)
  (match def
    [(defun ,name ,formals ,body) formals]
    [else (error 'defun.formals (format "Non-defun: ~s" def))]))
(define (defun.body def)
  (match def
    [(defun ,name ,formals ,body) body]
    [else (error 'defun.body (format "Non-defun: ~s" def))]))

(define (dethm-c name formals body)
  `(dethm ,name ,formals ,body))
(define (dethm? x)
  (match x
    [(dethm ,name ,formals ,body) #t]
    [else #f]))
(define (dethm.name def)
  (match def
    [(dethm ,name ,formals ,body) name]
    [else (error 'dethm.name (format "Non-dethm: ~s" def))]))
(define (dethm.formals def)
  (match def
    [(dethm ,name ,formals ,body) formals]
    [else (error 'dethm.formals (format "Non-dethm: ~s" def))]))
(define (dethm.body def)
  (match def
    [(dethm ,name ,formals ,body) body]
    [else (error 'dethm.body (format "Non-dethm: ~s" def))]))

(define (rator? name)
  (member name
    '(equal atom car cdr cons natp size + <)))

(define (rator.formals rator)
  (cond [(member rator '(atom car cdr natp size))
         '(x)]
        [(member rator '(equal cons + <))
         '(x y)]
        [else 'nil]))

(define (def.name def)
  (match def
    [(defun ,name ,formals ,body) name]
    [(dethm ,name ,formals ,body) name]
    [else def]))

(define (def.formals def)
  (match def
    [(defun ,name ,formals ,body) formals]
    [(dethm ,name ,formals ,body) formals]
    [else '()]))

(define (if-c-when-necessary Q A E)
  (if (equal? A E) A `(if ,Q ,A ,E)))

(define (conjunction es)
  (cond [(null? es) `'t]
        [(pair? (cdr es)) ; x1 . x2 . xs
         `(if ,(car es)
            ,(conjunction (cdr es))
            'nil)]
        [else (car es)])) ; x1 . '()

(define (implication es e)
  (cond [(null? es) e]
        [else
         `(if ,(car es)
            ,(implication (cdr es) e)
            't)]))

(define (lookup name defs)
  (cond [(null? defs) name]
        [(equal? (def.name (car defs)) name)
         (car defs)]
        [else
         (lookup name (cdr defs))]))

(define (undefined? name defs)
  (or (not (var? name)) (equal? (lookup name defs) name)))

(define (arity? vars es)
  (cond [(and (pair? vars) (pair? es))
         (arity? (cdr vars) (cdr es))]
        [else (and (null? vars) (null? es))]))

(define (args-arity? def args)
  (match def
    [(defun ,name ,formals ,body)
     (arity? formals args)]
    [(dethm ,name ,formals ,body) #f]
    [,rator (guard (rator? rator))
     (arity? (rator.formals rator) args)]
    [else #f]))

(define (app-arity? defs app)
  (match app
    [(,name . ,args)
     (args-arity? (lookup name defs) args)]))

(define (exprs? defs vars es)
  (every (lambda (e) (expr? defs vars e)) es))
(define (expr? defs vars e)
  (match e
    [,x (guard (var? x)) (or (equal? vars 'any) (member x vars))]
    [('quote ,val) #t]
    [(if . ,QAE) (exprs? defs vars QAE)]
    [(,name . ,args)
     (if (app-arity? defs `(,name . ,args))
       (exprs? defs vars args)
       #f)]))

(define (get-arg n args)
  (define (get-arg-from args from)
    (cond [(null? args) 'nil]
          [(equal? n from) (car args)]
          [else (get-arg-from (cdr args) (+ from '1))]))
  (get-arg-from args '1))

(define (set-arg n args y)
  (define (set-arg-from args from)
    (cond [(null? args) '()]
          [(equal? n from) (cons y (cdr args))]
          [else (cons (car args)
                  (set-arg-from (cdr args) (+ from '1)))]))
  (set-arg-from args '1))

(define (<=len n args)
  (define (<=len-from args from)
    (cond [(null? args) #f]
          [(equal? n from) #t]
          [else (<=len-from (cdr args) (+ from '1))]))
  (if (s.< '0 n) (<=len-from args '1) #f))

(define (list-extend xs x)
  (cond [(null? xs) (list x)]
        [(equal? (car xs) x) xs]
        [else (cons (car xs)
                (list-extend (cdr xs) x))]))

(define (list-union xs ys)
  (cond [(null? ys) xs]
        [else (list-union (list-extend xs (car ys))
                (cdr ys))]))

(define (formals? vars)
  (cond [(null? vars) #t]
        [else (and (var? (car vars))
                (formals? (cdr vars))
                (not (member (car vars) (cdr vars))))]))

(define (direction? dir)
  (or (equal (natp dir) 't) (member dir '(Q A E))))

(define (step-args? defs def args)
  (match def
    [(defun ,name ,formals ,body)
     (and (arity? formals args)
       (exprs? defs 'any args))]
    [(dethm ,name ,formals ,body)
     (and (arity? formals args)
       (exprs? defs 'any args))]
    [,rator (guard (rator? rator))
     (and (arity? (rator.formals rator) args)
       (every quote? args))]
    [else #f]))

(define (step-app? defs app)
  (step-args? defs
    (lookup (app.name app) defs)
    (app.args app)))

(define (step? defs step)
  (and (every direction? (car step))
    (app? (cadr step))
    (step-app? defs (cadr step))))

(define (steps? defs steps)
  (every (lambda (step) (step? defs step)) steps))

(define (induction-scheme-for? def vars e)
  (match `(,def . ,e)
    [((defun ,name1 ,formals ,body) . (,name2 . ,args))
     (and (arity? formals args)
       (formals? args)
       (every (lambda (x) (member x vars)) args))]
    [else #f]))

(define (induction-scheme? defs vars e)
  (match e
    [(,name . ,args)
     (induction-scheme-for?
       (lookup name defs)
       vars
       e)]
    [else #f]))

(define (seed? defs def seed)
  (if (equal? seed 'nil)
    #t
    (match def
      [(defun ,name ,formals ,body) (expr? defs formals seed)]
      [(dethm ,name ,formals ,body)
       (induction-scheme? defs formals seed)]
      [else #f])))

(define (extend-rec defs def)
  (if (defun? def)
    (list-extend defs
      `(defun ,(defun.name def) ,(defun.formals def)
         (,(defun.name def) . ,(defun.formals def))))
    defs))

(define (def-contents? known-defs formals body)
  (and (formals? formals) (expr? known-defs formals body)))

(define (def? known-defs def)
  (match def
    [(defun ,name ,formals ,body)
     (if/nil (undefined? name known-defs)
       (def-contents?
         (extend-rec known-defs def)
         formals
         body)
       #f)]
    [(dethm ,name ,formals ,body)
     (if/nil (undefined? name known-defs)
       (def-contents? known-defs
         formals
         body)
       #f)]
    [else #f]))

(define (defs? known-defs defs)
  (cond [(null? defs) #t]
        [(def? known-defs (car defs))
         (defs? (list-extend known-defs (car defs))
           (cdr defs))]
        [else #f]))

(define (proof? defs pf)
  (match pf
    [(,def ,seed . ,steps)
     (and (def? defs def)
       (seed? defs def seed)
       (steps? (extend-rec defs def) steps))]
    [else #f]))

(define (proofs? defs pfs)
  (cond [(null? pfs) #t]
        [(proof? defs (car pfs))
         (proofs? (list-extend defs (caar pfs))
           (cdr pfs))]
        [else #f]))

(define (sub-var vars args var)
  (cond [(null? vars) var]
        [(equal? (car vars) var) (car args)]
        [else (sub-var (cdr vars) (cdr args) var)]))

(define (sub-es vars args es)
  (map (lambda (e) (sub-e vars args e)) es))
(define (sub-e vars args e)
  (match e
    [,x (guard (var? x))
     (sub-var vars args x)]
    [('quote ,val) `(quote ,val)]
    [(if . ,QAE) `(if . ,(sub-es vars args QAE))]
    [(,name . ,appargs) `(,name . ,(sub-es vars args appargs))]))

(define (exprs-recs f es)
  (fold-right list-union '()
    (map (lambda (e) (expr-recs f e)) es)))
(define (expr-recs f e)
  (match e
    [,x (guard (var? x)) '()]
    [('quote ,val) '()]
    [(if . ,QAE) (exprs-recs f QAE)]
    [(,name . ,args) (guard (equal? name f))
     (list-union
       (list e)
       (exprs-recs f args))]
    [(,name . ,args)
     (exprs-recs f args)]))

(define (totality/meas meas formals apps)
  (map (lambda (app)
         `(< ,(sub-e formals (app.args app) meas) ,meas))
    apps))

(define (totality/if/nil meas f formals e)
  (match e
    [(if ,Q ,A ,E)
     (conjunction
      (list-extend
        (totality/meas meas formals
          (expr-recs f Q))
        (if-c-when-necessary Q
          (totality/if/nil meas f formals A)
          (totality/if/nil meas f formals E))))]
    [else
     (conjunction
      (totality/meas meas formals
        (expr-recs f e)))]))

(define (totality/claim meas def)
  (match `(,meas . ,def)
    [(nil . (defun ,name ,formals ,body))
     (if (equal? (expr-recs name body) '())
       `'t
       `'nil)]
    [else
     `(if (natp ,meas)
        ,(totality/if/nil meas (defun.name def)
           (defun.formals def)
           (defun.body def))
        'nil)]))

(define (induction/prems vars claim apps)
  (map (lambda (app) (sub-e vars (app.args app) claim))
    apps))

(define (induction/if/nil vars claim f e)
  (match e
    [(if ,Q ,A ,E)
     (implication
       (induction/prems vars claim
         (expr-recs f Q))
       (if-c-when-necessary Q
         (induction/if/nil vars claim f A)
         (induction/if/nil vars claim f E)))]
    [else
     (implication
      (induction/prems vars claim
        (expr-recs f e))
      claim)]))

(define (induction/defun vars claim def)
  (match def
    [(defun ,name ,formals ,body)
     (induction/if/nil vars claim name
       (sub-e formals vars body))]))

(define (induction/claim defs seed def)
  (match `(,seed . ,def)
    [(nil . (dethm ,name ,formals ,body)) body]
    [((,name . ,args) . (dethm ,thmname ,formals ,body))
     (induction/defun args body (lookup name defs))]))

(define (find-focus-at-direction dir e)
  (match e
    [(if ,Q ,A ,E)
     (cond [(equal? dir 'Q) Q]
           [(equal? dir 'A) A]
           [(equal? dir 'E) E])]
    [(,name . ,args) (get-arg dir args)]))

(define (rewrite-focus-at-direction dir e1 e2)
  (match e1
    [(if ,Q ,A ,E)
     (cond [(equal? dir 'Q) `(if ,e2 ,A ,E)]
           [(equal? dir 'A) `(if ,Q ,e2 ,E)]
           [(equal? dir 'E) `(if ,Q ,A ,e2)])]
    [(,name . ,args) `(,name . ,(set-arg dir args e2))]))

(define (focus-is-at-direction? dir e)
  (match e
    [(if ,Q ,A ,E) (member dir '(Q A E))]
    [(,name . ,args) (guard (not (equal? name 'quote)))
     (<=len dir args)]
    [else #f]))

(define (focus-is-at-path? path e)
  (cond [(null? path) #t]
        [(focus-is-at-direction? (car path) e)
         (focus-is-at-path? (cdr path)
           (find-focus-at-direction (car path) e))]
        [else #f]))

(define (find-focus-at-path path e)
  (if (null? path)
    e
    (find-focus-at-path (cdr path)
      (find-focus-at-direction (car path) e))))

(define (rewrite-focus-at-path path e1 e2)
  (if (null? path)
    e2
    (rewrite-focus-at-direction (car path) e1
      (rewrite-focus-at-path (cdr path)
        (find-focus-at-direction (car path) e1)
        e2))))

(define (prem-A? prem path e)
  (match `(,path . ,e)
    [('()        . ,e) #f]
    [(('A . ,ps) . (if ,Q ,A ,E))
     (guard (equal? Q prem)) #t]
    [((,p . ,ps) . ,e)
     (prem-A? prem ps (find-focus-at-direction p e))]))

(define (prem-E? prem path e)
  (match `(,path . ,e)
    [('()        . ,e) #f]
    [(('E . ,ps) . (if ,Q ,A ,E))
     (guard (equal? Q prem)) #t]
    [((,p . ,ps) . ,e)
     (prem-E? prem ps (find-focus-at-direction p e))]))

(define (follow-prems path e thm)
  (match thm
    [(if ,Q ,A ,E) (guard (prem-A? Q path e))
     (follow-prems path e A)]
    [(if ,Q ,A ,E) (guard (prem-E? Q path e))
     (follow-prems path e E)]
    [else thm]))

(define (unary-op rator rand)
  (match rator
    ['atom (atom rand)]
    ['car (car rand)]
    ['cdr (cdr rand)]
    ['natp (natp rand)]
    ['size (size rand)]
    [else 'nil]))

(define (binary-op rator rand1 rand2)
  (match rator
    ['equal (equal rand1 rand2)]
    ['cons (cons rand1 rand2)]
    ['+ (+ rand1 rand2)]
    ['< (< rand1 rand2)]
    [else 'nil]))

(define (apply-op rator rands)
  (cond [(member rator '(atom car cdr natp size))
         (unary-op rator (car rands))]
        [(member rator '(equal cons + <))
         (binary-op rator (car rands) (cadr rands))]
        [else 'nil]))

(define (eval-op app)
  (match app
    [(,name . ,args)
     `',(apply-op name (map quote.value args))]))

(define (equality focus a b)
  (cdr (assoc focus
         `((,a . ,b) (,b . ,a) (,focus . ,focus)))))

(define (equality/equation focus concl-inst)
  (match concl-inst
    [(equal ,a ,b) (equality focus a b)]
    [else focus]))

(define (equality/path e path thm)
  (if (focus-is-at-path? path e)
    (rewrite-focus-at-path path e
      (equality/equation
        (find-focus-at-path path e)
        (follow-prems path e thm)))
    e))

(define (equality/def claim path app def)
  (match `(,def . ,app)
    [(,def . ,app) (guard (rator? def))
     (equality/path claim path
       `(equal ,app ,(eval-op app)))]
    [((defun ,name ,formals ,body) . (,appname . ,args))
     (equality/path claim path
       (sub-e formals args
         `(equal (,name . ,formals) ,body)))]
    [((dethm ,name ,formals ,body) . (,appname . ,args))
     (equality/path claim path
       (sub-e formals args body))]
    [else claim]))

(define (rewrite/step defs claim step)
  (match step
    [(,path (,name . ,args))
     (equality/def claim path `(,name . ,args)
       (lookup name defs))]))

(define (rewrite/continue defs steps old new)
  (if (or (equal? new old) (null? steps))
    new
    (rewrite/continue defs (cdr steps) new
      (rewrite/step defs new (car steps)))))

(define (rewrite/steps defs claim steps)
  (if (null? steps)
    claim
    (rewrite/continue defs (cdr steps) claim
      (rewrite/step defs claim (car steps)))))

(define (rewrite/prove defs def seed steps)
  (cond [(defun? def)
         (rewrite/steps defs
           (totality/claim seed def)
           steps)]
        [(dethm? def)
         (rewrite/steps defs
           (induction/claim defs seed def)
           steps)]
        [else `'nil]))

(define (rewrite/prove+1 defs pf e)
  (if (equal? e `'t)
    (match pf
      [(,def ,seed . ,steps)
       (rewrite/prove defs def seed steps)])
    e))

(define (rewrite/prove+ defs pfs)
  (if (null? pfs)
    `'t
    (match pfs
      [((,def ,seed . ,steps) . ,pfs)
       (rewrite/prove+1 defs `(,def ,seed . ,steps)
         (rewrite/prove+ (list-extend defs def) pfs))])))

(define (rewrite/define defs def seed steps)
  (if (equal? (rewrite/prove defs def seed steps) `'t)
    (list-extend defs def)
    defs))

(define (rewrite/define+1 defs1 defs2 pfs)
  (if (equal? defs1 defs2)
    defs1
    (match pfs
      [((,def ,seed . ,pf) . ,pfs)
       (rewrite/define+1 defs2
         (rewrite/define defs2 def seed pf) pfs)]
      [else defs2])))

(define (rewrite/define+ defs pfs)
  (match pfs
    [((,def ,seed . ,pf) . ,pfs)
     (rewrite/define+1 defs
       (rewrite/define defs def seed pf)
       pfs)]
    [else defs]))

(define (J-Bob/step defs e steps)
  (if (and (defs? '() defs)
       (exprs? defs 'any e)
       (steps? defs steps))
    (rewrite/steps defs e steps)
    e))

(define (J-Bob/prove defs pfs)
  (if (and (defs? '() defs) (proofs? defs pfs))
    (rewrite/prove+ defs pfs)
    `'nil))

(define (J-Bob/define defs pfs)
  (if (and (defs? '() defs) (proofs? defs pfs))
    (rewrite/define+ defs pfs)
    defs))
(define (axioms)
  '((dethm atom/cons (x y)
      (equal (atom (cons x y)) 'nil))
    (dethm car/cons (x y)
      (equal (car (cons x y)) x))
    (dethm cdr/cons (x y)
      (equal (cdr (cons x y)) y))
    (dethm equal-same (x)
      (equal (equal x x) 't))
    (dethm equal-swap (x y)
      (equal (equal x y) (equal y x)))
    (dethm if-same (x y)
      (equal (if x y y) y))
    (dethm if-true (x y)
      (equal (if 't x y) x))
    (dethm if-false (x y)
      (equal (if 'nil x y) y))
    (dethm if-nest-E (x y z)
      (if x 't (equal (if x y z) z)))
    (dethm if-nest-A (x y z)
      (if x (equal (if x y z) y) 't))
    (dethm cons/car+cdr (x)
      (if (atom x)
        't
        (equal (cons (car x) (cdr x)) x)))
    (dethm equal-if (x y)
      (if (equal x y) (equal x y) 't))
    (dethm natp/size (x)
      (equal (natp (size x)) 't))
    (dethm size/car (x)
      (if (atom x)
        't
        (equal (< (size (car x)) (size x)) 't)))
    (dethm size/cdr (x)
      (if (atom x)
        't
        (equal (< (size (cdr x)) (size x)) 't)))
    (dethm associate-+ (a b c)
      (equal (+ (+ a b) c) (+ a (+ b c))))
    (dethm commute-+ (x y)
      (equal (+ x y) (+ y x)))
    (dethm natp/+ (x y)
      (if (natp x)
        (if (natp y)
          (equal (natp (+ x y)) 't)
          't)
        't))
    (dethm positives-+ (x y)
      (if (< '0 x)
        (if (< '0 y)
          (equal (< '0 (+ x y)) 't)
          't)
        't))
    (dethm common-addends-< (x y z)
      (equal (< (+ x z) (+ y z)) (< x y)))
    (dethm identity-+ (x)
      (if (natp x) (equal (+ '0 x) x) 't))))

(define (prelude)
  (J-Bob/define (axioms)
    '(((defun list-induction (x)
         (if (atom x)
           '()
           (cons (car x)
             (list-induction (cdr x)))))
       (size x)
       ((A E) (size/cdr x))
       ((A) (if-same (atom x) 't))
       ((Q) (natp/size x))
       (() (if-true 't 'nil)))
      ((defun star-induction (x)
         (if (atom x)
           x
           (cons (star-induction (car x))
             (star-induction (cdr x)))))
       (size x)
       ((A E A) (size/cdr x))
       ((A E Q) (size/car x))
       ((A E) (if-true 't 'nil))
       ((A) (if-same (atom x) 't))
       ((Q) (natp/size x))
       (() (if-true 't 'nil))))))
