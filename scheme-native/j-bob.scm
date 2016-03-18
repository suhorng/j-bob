
(defun list0 () '())
(defun list0? (x) (equal x '()))

(defun list1 (x) (cons x (list0)))
(defun list1? (x)
  (if/nil (atom x) 'nil (list0? (cdr x))))
(defun elem1 (xs) (car xs))

(defun list2 (x y) (cons x (list1 y)))
(defun list2? (x)
  (if/nil (atom x) 'nil (list1? (cdr x))))
(defun elem2 (xs) (elem1 (cdr xs)))

(defun list3 (x y z) (cons x (list2 y z)))
(defun list3? (x)
  (if/nil (atom x) 'nil (list2? (cdr x))))
(defun elem3 (xs) (elem2 (cdr xs)))

(defun tag (sym x) (cons sym x))
(defun tag? (sym x)
  (if/nil (atom x) 'nil (equal (car x) sym)))
(defun untag (x) (cdr x))

(defun quote-c (value)
  `',value)
(defun quote? (x)
  (match x
    [('quote ,val) #t]
    [else #f]))
(defun quote.value (e)
  (match e
    [('quote ,val) val]
    [else (error 'quote.value (format "Non-quote value: ~s" e))]))

(defun if-c (Q A E) `(if ,Q ,A ,E))
(defun if? (x)
  (match x
    [(if ,Q ,A ,E) #t]
    [else #f]))
(defun if.Q (e)
  (match e
    [(if ,Q ,A ,E) Q]
    [else (error 'if.Q (format "Non-if expression: ~s" e))]))
(defun if.A (e)
  (match e
    [(if ,Q ,A ,E) A]
    [else (error 'if.A (format "Non-if expression: ~s" e))]))
(defun if.E (e)
  (match e
    [(if ,Q ,A ,E) E]
    [else (error 'if.E (format "Non-if expression: ~s" e))]))

(defun app-c (name args) `(,name . ,args))
(defun app? (x)
  (match x
    [('quote . ,dr) #f]
    [(if . ,dr) #f]
    [(,name . ,args) #t]
    [else #f]))
(defun app.name (e)
  (match e
    [(,name . ,args) name]
    [else (error 'app.name (format "Non-application: ~s" e))]))
(defun app.args (e)
  (match e
    [(,name . ,args) args]
    [else (error 'app.args (format "Non-application: ~s" e))]))

(defun var? (x)
  (match x
    ['t #f]
    ['nil #f]
    [,n (guard (equal? (natp n) 't)) #f]
    [(,ar . ,dr) #f]
    [else #t]))

(defun defun-c (name formals body)
  `(defun ,name ,formals ,body))
(defun defun? (x)
  (match x
    [(defun ,name ,formals ,body) #t]
    [else #f]))
(defun defun.name (def)
  (match def
    [(defun ,name ,formals ,body) name]
    [else (error 'defun.name (format "Non-defun: ~s" def))]))
(defun defun.formals (def)
  (match def
    [(defun ,name ,formals ,body) formals]
    [else (error 'defun.formals (format "Non-defun: ~s" def))]))
(defun defun.body (def)
  (match def
    [(defun ,name ,formals ,body) body]
    [else (error 'defun.body (format "Non-defun: ~s" def))]))

(defun dethm-c (name formals body)
  `(dethm ,name ,formals ,body))
(defun dethm? (x)
  (match x
    [(dethm ,name ,formals ,body) #t]
    [else #f]))
(defun dethm.name (def)
  (match def
    [(dethm ,name ,formals ,body) name]
    [else (error 'dethm.name (format "Non-dethm: ~s" def))]))
(defun dethm.formals (def)
  (match def
    [(dethm ,name ,formals ,body) formals]
    [else (error 'dethm.formals (format "Non-dethm: ~s" def))]))
(defun dethm.body (def)
  (match def
    [(dethm ,name ,formals ,body) body]
    [else (error 'dethm.body (format "Non-dethm: ~s" def))]))

(defun if-QAE (e)
  (match e
    [(if . ,QAE) QAE]))
(defun QAE-if (es)
  `(if ,(elem1 es) ,(elem2 es) ,(elem3 es)))

(defun rator? (name)
  (member name
    '(equal atom car cdr cons natp size + <)))

(defun rator.formals (rator)
  (if (member rator '(atom car cdr natp size))
    '(x)
    (if (member rator '(equal cons + <))
      '(x y)
      'nil)))

(defun def.name (def)
  (match def
    [(defun ,name ,formals ,body) name]
    [(dethm ,name ,formals ,body) name]
    [else def]))

(defun def.formals (def)
  (match def
    [(defun ,name ,formals ,body) formals]
    [(dethm ,name ,formals ,body) formals]
    [else '()]))

(defun if-c-when-necessary (Q A E)
  (if (equal? A E) A `(if ,Q ,A ,E)))

(defun conjunction (es)
  (cond [(null? es) `'t]
        [(pair? (cdr es)) ; x1 . x2 . xs
         `(if ,(car es)
            ,(conjunction (cdr es))
            'nil)]
        [else (car es)])) ; x1 . '()

(defun implication (es e)
  (cond [(null? es) e]
        [else
         `(if ,(car es)
            ,(implication (cdr es) e)
            't)]))

(defun lookup (name defs)
  (cond [(null? defs) name]
        [(equal? (def.name (car defs)) name)
         (car defs)]
        [else
         (lookup name (cdr defs))]))

(defun undefined? (name defs)
  (or (not (var? name)) (equal? (lookup name defs) name)))

(defun arity? (vars es)
  (cond [(and (pair? vars) (pair? es))
         (arity? (cdr vars) (cdr es))]
        [else (and (null? vars) (null? es))]))

(defun args-arity? (def args)
  (match def
    [(defun ,name ,formals ,body)
     (arity? formals args)]
    [(dethm ,name ,formals ,body) #f]
    [,rator (guard (rator? rator))
     (arity? (rator.formals rator) args)]
    [else #f]))

(defun app-arity? (defs app)
  (args-arity? (lookup (app.name app) defs)
    (app.args app)))

(defun bound? (var vars)
  (or (equal? vars 'any) (member var vars)))

(defun exprs? (defs vars es)
  (every (lambda (e) (expr? defs vars e)) es))
(defun expr? (defs vars e)
  (match e
    [,x (guard (var? x)) (bound? x vars)]
    [('quote ,val) #t]
    [(if . ,QAE) (exprs? defs vars QAE)]
    [(,name . ,args)
     (if (app-arity? defs `(,name . ,args))
       (exprs? defs vars args)
       #f)]))

(defun get-arg-from (n args from)
  (cond [(null? args) 'nil]
        [(equal? n from) (car args)]
        [else (get-arg-from n (cdr args) (+ from '1))]))
(defun get-arg (n args)
  (get-arg-from n args '1))

(defun set-arg-from (n args y from)
  (cond [(null? args) '()]
        [(equal? n from) (cons y (cdr args))]
        [else (cons (car args)
                (set-arg-from n (cdr args) y
                  (+ from '1)))]))
(defun set-arg (n args y)
  (set-arg-from n args y '1))

(defun <=len-from (n args from)
  (cond [(null? args) #f]
        [(equal? n from) #t]
        [else (<=len-from n (cdr args) (+ from '1))]))
(defun <=len (n args)
  (if (s.< '0 n) (<=len-from n args '1) #f))

(defun subset? (xs ys)
  (every (lambda (x) (member x ys)) ys))

(defun list-extend (xs x)
  (cond [(null? xs) (list x)]
        [(equal? (car xs) x) xs]
        [else (cons (car xs)
                (list-extend (cdr xs) x))]))

(defun list-union (xs ys)
  (cond [(null? ys) xs]
        [else (list-union (list-extend xs (car ys))
                (cdr ys))]))

(defun formals? (vars)
  (cond [(null? vars) #t]
        [else (and (var? (car vars))
                (formals? (cdr vars))
                (not (member (car vars) (cdr vars))))]))

(defun direction? (dir)
  (or (equal (natp dir) 't) (member dir '(Q A E))))

(defun path? (path)
  (every direction? path))

(defun quoted-exprs? (args)
  (every quote? args))

(defun step-args? (defs def args)
  (match def
    [(defun ,name ,formals ,body)
     (and (arity? formals args)
       (exprs? defs 'any args))]
    [(dethm ,name ,formals ,body)
     (and (arity? formals args)
       (exprs? defs 'any args))]
    [,rator (guard (rator? rator))
     (and (arity? (rator.formals rator) args)
       (quoted-exprs? args))]
    [else #f]))

(defun step-app? (defs app)
  (step-args? defs
    (lookup (app.name app) defs)
    (app.args app)))

(defun step? (defs step)
  (and (path? (car step))
    (app? (cadr step))
    (step-app? defs (cadr step))))

(defun steps? (defs steps)
  (every (lambda (step) (step? defs step)) steps))

(defun induction-scheme-for? (def vars e)
  (match `(,def . ,e)
    [((defun ,name1 ,formals ,body) . (,name2 . ,args))
     (and (arity? formals args)
       (formals? args)
       (subset? args vars))]
    [else #f]))

(defun induction-scheme? (defs vars e)
  (match e
    [(,name . ,args)
     (induction-scheme-for?
       (lookup name defs)
       vars
       e)]
    [else #f]))

(defun seed? (defs def seed)
  (if (equal? seed 'nil)
    #t
    (match def
      [(defun ,name ,formals ,body) (expr? defs formals seed)]
      [(dethm ,name ,formals ,body)
       (induction-scheme? defs formals seed)]
      [else #f])))

(defun extend-rec (defs def)
  (if (defun? def)
    (list-extend defs
      `(defun ,(defun.name def) ,(defun.formals def)
         (,(defun.name def) . ,(defun.formals def))))
    defs))

(defun def-contents? (known-defs formals body)
  (and (formals? formals) (expr? known-defs formals body)))

(defun def? (known-defs def)
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

(defun defs? (known-defs defs)
  (cond [(null? defs) #t]
        [(def? known-defs (car defs))
         (defs? (list-extend known-defs (car defs))
           (cdr defs))]
        [else #f]))

(defun list2-or-more? (pf)
  (and (pair? pf) (pair? (cdr pf))))

(defun proof? (defs pf)
  (and (list2-or-more? pf)
    (def? defs (car pf))
    (seed? defs (car pf) (cadr pf))
    (steps? (extend-rec defs (car pf))
      (cddr pf))))

(defun proofs? (defs pfs)
  (cond [(null? pfs) #t]
        [(proof? defs (car pfs))
         (proofs? (list-extend defs (caar pfs))
           (cdr pfs))]
        [else #f]))

(defun sub-var (vars args var)
  (cond [(null? vars) var]
        [(equal? (car vars) var) (car args)]
        [else (sub-var (cdr vars) (cdr args) var)]))

(defun sub-es (vars args es)
  (map (lambda (e) (sub-e vars args e)) es))
(defun sub-e (vars args e)
  (match e
    [,x (guard (var? x))
     (sub-var vars args x)]
    [('quote ,val) `(quote ,val)]
    [(if . ,QAE) `(if . ,(sub-es vars args QAE))]
    [(,name . ,appargs) `(,name . ,(sub-es vars args appargs))]))

(defun exprs-recs (f es)
  (fold-right list-union '()
    (map (lambda (e) (expr-recs f e)) es)))
(defun expr-recs (f e)
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

(defun totality/< (meas formals app)
  `(< ,(sub-e formals (app.args app) meas) ,meas))

(defun totality/meas (meas formals apps)
  (map (lambda (app) (totality/< meas formals app)) apps))

(defun totality/if/nil (meas f formals e)
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

(defun totality/claim (meas def)
  (match `(,meas . ,def)
    [(nil . (defun ,name ,formals ,body))
     (if (equal? (expr-recs name body) '())
       `'t
       `'nil)]
    [else
     `(if (natp . ,(list1 meas))
        ,(totality/if/nil meas (defun.name def)
           (defun.formals def)
           (defun.body def))
        'nil)]))

(defun induction/prems (vars claim apps)
  (map (lambda (app) (sub-e vars (app.args app) claim))
    apps))

(defun induction/if/nil (vars claim f e)
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

(defun induction/defun (vars claim def)
  (match def
    [(defun ,name ,formals ,body)
     (induction/if/nil vars claim name
       (sub-e formals vars body))]))

(defun induction/claim (defs seed def)
  (match `(,seed . ,def)
    [(nil . (dethm ,name ,formals ,body)) body]
    [((,name . ,args) . (dethm ,thmname ,formals ,body))
     (induction/defun args body (lookup name defs))]))

(defun find-focus-at-direction (dir e)
  (match e
    [(if ,Q ,A ,E)
     (cond [(equal? dir 'Q) Q]
           [(equal? dir 'A) A]
           [(equal? dir 'E) E])]
    [(,name . ,args) (get-arg dir args)]))

(defun rewrite-focus-at-direction (dir e1 e2)
  (match e1
    [(if ,Q ,A ,E)
     (cond [(equal? dir 'Q) `(if ,e2 ,A ,E)]
           [(equal? dir 'A) `(if ,Q ,e2 ,E)]
           [(equal? dir 'E) `(if ,Q ,A ,e2)])]
    [(,name . ,args) `(,name . ,(set-arg dir args e2))]))

(defun focus-is-at-direction? (dir e)
  (match e
    [(if ,Q ,A ,E) (member dir '(Q A E))]
    [(,name . ,args) (guard (not (equal? name 'quote)))
     (<=len dir args)]
    [else #f]))

(defun focus-is-at-path? (path e)
  (cond [(null? path) #t]
        [(focus-is-at-direction? (car path) e)
         (focus-is-at-path? (cdr path)
           (find-focus-at-direction (car path) e))]
        [else #f]))

(defun find-focus-at-path (path e)
  (if (null? path)
    e
    (find-focus-at-path (cdr path)
      (find-focus-at-direction (car path) e))))

(defun rewrite-focus-at-path (path e1 e2)
  (if/nil (atom path)
    e2
    (rewrite-focus-at-direction (car path) e1
      (rewrite-focus-at-path (cdr path)
        (find-focus-at-direction (car path) e1)
        e2))))

(defun prem-A? (prem path e)
  (if/nil (atom path)
    'nil
    (if/nil (equal (car path) 'A)
      (if/nil (equal (if.Q e) prem)
        't
        (prem-A? prem (cdr path)
          (find-focus-at-direction (car path)
            e)))
      (prem-A? prem (cdr path)
        (find-focus-at-direction (car path)
          e)))))

(defun prem-E? (prem path e)
  (if/nil (atom path)
    'nil
    (if/nil (equal (car path) 'E)
      (if/nil (equal (if.Q e) prem)
        't
        (prem-E? prem (cdr path)
          (find-focus-at-direction (car path)
            e)))
      (prem-E? prem (cdr path)
        (find-focus-at-direction (car path)
          e)))))

(defun follow-prems (path e thm)
  (if (if? thm)
    (if/nil (prem-A? (if.Q thm) path e)
      (follow-prems path e (if.A thm))
      (if/nil (prem-E? (if.Q thm) path e)
        (follow-prems path e (if.E thm))
        thm))
    thm))

(defun unary-op (rator rand)
  (if/nil (equal rator 'atom)
    (atom rand)
    (if/nil (equal rator 'car)
      (car rand)
      (if/nil (equal rator 'cdr)
        (cdr rand)
        (if/nil (equal rator 'natp)
          (natp rand)
          (if/nil (equal rator 'size)
            (size rand)
            'nil))))))

(defun binary-op (rator rand1 rand2)
  (if/nil (equal rator 'equal)
    (equal rand1 rand2)
    (if/nil (equal rator 'cons)
      (cons rand1 rand2)
      (if/nil (equal rator '+)
        (+ rand1 rand2)
        (if/nil (equal rator '<)
          (< rand1 rand2)
          'nil)))))

(defun apply-op (rator rands)
  (if (member rator '(atom car cdr natp size))
    (unary-op rator (elem1 rands))
    (if (member rator '(equal cons + <))
      (binary-op rator
        (elem1 rands)
        (elem2 rands))
      'nil)))

(defun rands (args)
  (if/nil (atom args)
    '()
    (cons (quote.value (car args))
      (rands (cdr args)))))

(defun eval-op (app)
  `',(apply-op (app.name app)
       (rands (app.args app))))

(defun app-of-equal? (e)
  (if (app? e)
    (equal (app.name e) 'equal)
    'nil))

(defun equality (focus a b)
  (if/nil (equal focus a)
    b
    (if/nil (equal focus b)
      a
      focus)))

(defun equality/equation (focus concl-inst)
  (if/nil (app-of-equal? concl-inst)
    (equality focus
      (elem1 (app.args concl-inst))
      (elem2 (app.args concl-inst)))
    focus))

(defun equality/path (e path thm)
  (if (focus-is-at-path? path e)
    (rewrite-focus-at-path path e
      (equality/equation
        (find-focus-at-path path e)
        (follow-prems path e thm)))
    e))

(defun equality/def (claim path app def)
  (if (rator? def)
    (equality/path claim path
      `(equal ,app ,(eval-op app)))
    (if (defun? def)
      (equality/path claim path
        (sub-e (defun.formals def)
          (app.args app)
          `(equal
              (,(defun.name def) . ,(defun.formals def))
              ,(defun.body def))))
      (if (dethm? def)
        (equality/path claim path
          (sub-e (dethm.formals def)
            (app.args app)
            (dethm.body def)))
        claim))))

(defun rewrite/step (defs claim step)
  (equality/def claim (elem1 step) (elem2 step)
    (lookup (app.name (elem2 step)) defs)))

(defun rewrite/continue (defs steps old new)
  (if/nil (equal new old)
    new
    (if/nil (atom steps)
      new
      (rewrite/continue defs (cdr steps) new
        (rewrite/step defs new (car steps))))))

(defun rewrite/steps (defs claim steps)
  (if/nil (atom steps)
    claim
    (rewrite/continue defs (cdr steps) claim
      (rewrite/step defs claim (car steps)))))

(defun rewrite/prove (defs def seed steps)
  (if (defun? def)
    (rewrite/steps defs
      (totality/claim seed def)
      steps)
    (if (dethm? def)
      (rewrite/steps defs
        (induction/claim defs seed def)
        steps)
      `'nil)))

(defun rewrite/prove+1 (defs pf e)
  (if/nil (equal e `'t)
    (rewrite/prove defs (elem1 pf) (elem2 pf)
      (cdr (cdr pf)))
    e))

(defun rewrite/prove+ (defs pfs)
  (if/nil (atom pfs)
    `'t
    (rewrite/prove+1 defs (car pfs)
      (rewrite/prove+
        (list-extend defs (elem1 (car pfs)))
        (cdr pfs)))))

(defun rewrite/define (defs def seed steps)
  (if/nil (equal (rewrite/prove defs def seed steps)
             `'t)
    (list-extend defs def)
    defs))

(defun rewrite/define+1 (defs1 defs2 pfs)
  (if/nil (equal defs1 defs2)
    defs1
    (if/nil (atom pfs)
      defs2
      (rewrite/define+1 defs2
        (rewrite/define defs2
          (elem1 (car pfs))
          (elem2 (car pfs))
          (cdr (cdr (car pfs))))
        (cdr pfs)))))

(defun rewrite/define+ (defs pfs)
  (if/nil (atom pfs)
    defs
    (rewrite/define+1 defs
      (rewrite/define defs
        (elem1 (car pfs))
        (elem2 (car pfs))
        (cdr (cdr (car pfs))))
      (cdr pfs))))

(defun J-Bob/step (defs e steps)
  (if (defs? '() defs)
    (if (expr? defs 'any e)
      (if/nil (steps? defs steps)
        (rewrite/steps defs e steps)
        e)
      e)
    e))

(defun J-Bob/prove (defs pfs)
  (if (defs? '() defs)
    (if (proofs? defs pfs)
      (rewrite/prove+ defs pfs)
      `'nil)
    `'nil))

(defun J-Bob/define (defs pfs)
  (if (defs? '() defs)
    (if (proofs? defs pfs)
      (rewrite/define+ defs pfs)
      defs)
    defs))

(defun axioms ()
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

(defun prelude ()
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
