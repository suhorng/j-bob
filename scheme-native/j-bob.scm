
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
    [(if ,Q ,A ,E) `(,Q ,A ,E)]))
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
  (if (dethm? def)
    #f
    (if (defun? def)
      (arity? (defun.formals def) args)
      (if (rator? def)
        (arity? (rator.formals def) args)
        #f))))

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
    [(if ,Q ,A ,E) (exprs? defs vars (list Q A E))]
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
  (if/nil (atom xs)
    #t
    (if (member (car xs) ys)
      (subset? (cdr xs) ys)
      #f)))

(defun list-extend (xs x)
  (if/nil (atom xs)
    (list1 x)
    (if/nil (equal (car xs) x)
      xs
      (cons (car xs)
        (list-extend (cdr xs) x)))))

(defun list-union (xs ys)
  (if/nil (atom ys)
    xs
    (list-union (list-extend xs (car ys))
      (cdr ys))))

(defun formals? (vars)
  (if/nil (atom vars)
    't
    (if (var? (car vars))
      (if (member (car vars) (cdr vars))
        'nil
        (formals? (cdr vars)))
      'nil)))

(defun direction? (dir)
  (if/nil (natp dir)
    #t
    (if (member dir '(Q A E))
      #t
      #f)))

(defun path? (path)
  (if/nil (atom path)
    't
    (if (direction? (car path))
      (path? (cdr path))
      'nil)))

(defun quoted-exprs? (args)
  (if/nil (atom args)
    #t
    (if (quote? (car args))
      (quoted-exprs? (cdr args))
      #f)))

(defun step-args? (defs def args)
  (if (dethm? def)
    (if/nil (arity? (dethm.formals def) args)
      (exprs? defs 'any args)
      #f)
    (if (defun? def)
      (if/nil (arity? (defun.formals def) args)
        (exprs? defs 'any args)
        #f)
      (if (rator? def)
        (if/nil (arity? (rator.formals def) args)
          (quoted-exprs? args)
          #f)
        #f))))

(defun step-app? (defs app)
  (step-args? defs
    (lookup (app.name app) defs)
    (app.args app)))

(defun step? (defs step)
  (if/nil (path? (elem1 step))
    (if (app? (elem2 step))
      (step-app? defs (elem2 step))
      #f)
    #f))

(defun steps? (defs steps)
  (if/nil (atom steps)
    't
    (if (step? defs (car steps))
      (steps? defs (cdr steps))
      'nil)))

(defun induction-scheme-for? (def vars e)
  (if (defun? def)
    (if/nil (arity? (defun.formals def) (app.args e))
      (if/nil (formals? (app.args e))
        (subset? (app.args e) vars)
        #f)
      #f)
    #f))

(defun induction-scheme? (defs vars e)
  (if (app? e)
    (induction-scheme-for?
      (lookup (app.name e) defs)
      vars
      e)
    #f))

(defun seed? (defs def seed)
  (if/nil (equal seed 'nil)
    't
    (if (defun? def)
      (expr? defs (defun.formals def) seed)
      (if (dethm? def)
        (induction-scheme? defs
          (dethm.formals def)
          seed)
        #f))))

(defun extend-rec (defs def)
  (if (defun? def)
    (list-extend defs
      `(defun ,(defun.name def) ,(defun.formals def)
         (,(defun.name def) . ,(defun.formals def))))
    defs))

(defun def-contents? (known-defs formals body)
  (if/nil (formals? formals)
    (expr? known-defs formals body)
    #f))

(defun def? (known-defs def)
  (if (dethm? def)
    (if/nil (undefined? (dethm.name def)
          known-defs)
      (def-contents? known-defs
        (dethm.formals def)
        (dethm.body def))
      #f)
    (if (defun? def)
      (if/nil (undefined? (defun.name def)
            known-defs)
        (def-contents?
          (extend-rec known-defs def)
          (defun.formals def)
          (defun.body def))
        #f)
      #f)))

(defun defs? (known-defs defs)
  (if/nil (atom defs)
    't
    (if (def? known-defs (car defs))
      (defs? (list-extend known-defs (car defs))
        (cdr defs))
      'nil)))

(defun list2-or-more? (pf)
  (if/nil (atom pf)
    'nil
    (if/nil (atom (cdr pf))
      'nil
      't)))

(defun proof? (defs pf)
  (if/nil (list2-or-more? pf)
    (if (def? defs (elem1 pf))
      (if (seed? defs (elem1 pf) (elem2 pf))
        (steps? (extend-rec defs (elem1 pf))
          (cdr (cdr pf)))
        'nil)
      'nil)
    'nil))

(defun proofs? (defs pfs)
  (if/nil (atom pfs)
    't
    (if/nil (proof? defs (car pfs))
      (proofs?
        (list-extend defs (elem1 (car pfs)))
        (cdr pfs))
      'nil)))

(defun sub-var (vars args var)
  (if/nil (atom vars)
    var
    (if/nil (equal (car vars) var)
      (car args)
      (sub-var (cdr vars) (cdr args) var))))

(defun sub-es (vars args es)
  (if/nil (atom es)
    '()
    (if (var? (car es))
      (cons (sub-var vars args (car es))
        (sub-es vars args (cdr es)))
      (if (quote? (car es))
        (cons (car es)
          (sub-es vars args (cdr es)))
        (if (if? (car es))
          (cons
            (QAE-if
              (sub-es vars args
                (if-QAE (car es))))
            (sub-es vars args (cdr es)))
          (cons
            `(,(app.name (car es)) .
              ,(sub-es vars args
                 (app.args (car es))))
            (sub-es vars args (cdr es))))))))
(defun sub-e (vars args e)
  (elem1 (sub-es vars args (list1 e))))

(defun exprs-recs (f es)
  (if/nil (atom es)
    '()
    (if (var? (car es))
      (exprs-recs f (cdr es))
      (if (quote? (car es))
        (exprs-recs f (cdr es))
        (if (if? (car es))
          (list-union
            (exprs-recs f (if-QAE (car es)))
            (exprs-recs f (cdr es)))
          (if/nil (equal (app.name (car es)) f)
            (list-union
              (list1 (car es))
              (list-union
                (exprs-recs f
                  (app.args (car es)))
                (exprs-recs f (cdr es))))
            (list-union
              (exprs-recs f (app.args (car es)))
              (exprs-recs f
                (cdr es)))))))))
(defun expr-recs (f e)
  (exprs-recs f (list1 e)))

(defun totality/< (meas formals app)
  `(< . ,(list2 (sub-e formals (app.args app) meas)
           meas)))

(defun totality/meas (meas formals apps)
  (if/nil (atom apps)
    '()
    (cons
      (totality/< meas formals (car apps))
      (totality/meas meas formals (cdr apps)))))

(defun totality/if/nil (meas f formals e)
  (if (if? e)
    (conjunction
      (list-extend
        (totality/meas meas formals
          (expr-recs f (if.Q e)))
        (if-c-when-necessary (if.Q e)
          (totality/if/nil meas f formals
            (if.A e))
          (totality/if/nil meas f formals
            (if.E e)))))
    (conjunction
      (totality/meas meas formals
        (expr-recs f e)))))

(defun totality/claim (meas def)
  (if/nil (equal meas 'nil)
    (if/nil (equal (expr-recs (defun.name def)
                 (defun.body def))
               '())
      `'t
      `'nil)
    `(if (natp . ,(list1 meas))
       ,(totality/if/nil meas (defun.name def)
          (defun.formals def)
          (defun.body def))
       'nil)))

(defun induction/prems (vars claim apps)
  (if/nil (atom apps)
    '()
    (cons
      (sub-e vars (app.args (car apps)) claim)
      (induction/prems vars claim (cdr apps)))))

(defun induction/if/nil (vars claim f e)
  (if (if? e)
    (implication
      (induction/prems vars claim
        (expr-recs f (if.Q e)))
      (if-c-when-necessary (if.Q e)
        (induction/if/nil vars claim f (if.A e))
        (induction/if/nil vars claim f (if.E e))))
    (implication
      (induction/prems vars claim
        (expr-recs f e))
      claim)))

(defun induction/defun (vars claim def)
  (induction/if/nil vars claim (defun.name def)
    (sub-e (defun.formals def) vars
      (defun.body def))))

(defun induction/claim (defs seed def)
  (if/nil (equal seed 'nil)
    (dethm.body def)
    (induction/defun (app.args seed)
      (dethm.body def)
      (lookup (app.name seed) defs))))

(defun find-focus-at-direction (dir e)
  (if/nil (equal dir 'Q)
    (if.Q e)
    (if/nil (equal dir 'A)
      (if.A e)
      (if/nil (equal dir 'E)
        (if.E e)
        (get-arg dir (app.args e))))))

(defun rewrite-focus-at-direction (dir e1 e2)
  (if/nil (equal dir 'Q)
    `(if ,e2 ,(if.A e1) ,(if.E e1))
    (if/nil (equal dir 'A)
      `(if ,(if.Q e1) ,e2 ,(if.E e1))
      (if/nil (equal dir 'E)
        `(if ,(if.Q e1) ,(if.A e1) ,e2)
        `(,(app.name e1) . ,(set-arg dir (app.args e1) e2))))))

(defun focus-is-at-direction? (dir e)
  (if/nil (equal dir 'Q)
    (if? e)
    (if/nil (equal dir 'A)
      (if? e)
      (if/nil (equal dir 'E)
        (if? e)
        (if (app? e)
          (<=len dir (app.args e))
          #f)))))

(defun focus-is-at-path? (path e)
  (if/nil (atom path)
    't
    (if (focus-is-at-direction? (car path) e)
      (focus-is-at-path? (cdr path)
        (find-focus-at-direction (car path) e))
      'nil)))

(defun find-focus-at-path (path e)
  (if/nil (atom path)
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
  (if/nil (focus-is-at-path? path e)
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
  (if/nil (defs? '() defs)
    (if (expr? defs 'any e)
      (if/nil (steps? defs steps)
        (rewrite/steps defs e steps)
        e)
      e)
    e))

(defun J-Bob/prove (defs pfs)
  (if/nil (defs? '() defs)
    (if/nil (proofs? defs pfs)
      (rewrite/prove+ defs pfs)
      `'nil)
    `'nil))

(defun J-Bob/define (defs pfs)
  (if/nil (defs? '() defs)
    (if/nil (proofs? defs pfs)
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
