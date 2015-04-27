;Interpreter Project Part 4

;notes: We have included code for try stuff, but when we did we got an error in lookup that we weren't able
;to debug and get working in time. We will fix it for next week. For now, we have reverted to the code to 
;before we started the try stuff where we had more tests working properly. Some of the try functions are 
;still included and commented out, but we've taken the try parameter out of the other functions

(load "classParser.scm")

(define prefix
  (lambda ()
    (list car cadr caddr)))

;Call to parse the file
(define interpret
  (lambda (fileName class)
    (wrapper (call/cc
     (lambda (return)
         (mState (string->symbol class) (mStateList (parser fileName) '((()())) return (lambda (v) v) (lambda (v) v) (lambda (v) v) (string->symbol class)) return (lambda (v) v) (lambda (v) v) (lambda (v) v) (string->symbol class)))))))


;boolean wrapper, changes #t to true, #f to false, or does nothing
(define wrapper
  (lambda (bool)
    (cond
      ((eq? bool #t) 'true)
      ((eq? bool #f) 'false)
      (else bool))))

;////////////////////State Functions/////////////////////////////
;mStateList top level mState function
(define mStateList
  (lambda (list enviornment return next break continue class)
    (cond
      ((null? list) (next enviornment))
      (else (mStateAll (car list) enviornment return break continue (lambda (v) (mStateList (cdr list) v return next break continue class)) class)))))

(define mState
  (lambda (name env return break continue next class)
    (mStateList (cadr (lookupFunction name 'main env)) (finalEnvironment name env) return next break continue env)))

;mStateAll feeds to the other mstate functions
(define mStateAll
  (lambda (stmt env return break continue next class)
    (cond
      ((or (eq? 'var (car stmt)) (eq? '= (car stmt))) (next (mStateAssign stmt env class)))
      ((null? (car stmt)) (mStateAll stmt env env break continue next class))
      ((eq? (car stmt) 'return) (return (mValueAll (cadr stmt) env class)))
      ((eq? (car stmt) '=) (next (mStateAssign stmt env class)))
      ((eq? 'if (car stmt)) (mStateIf stmt env return next break continue class))
      ((eq? (car stmt) 'break) (break env))
      ((eq? (car stmt) 'continue) (continue env))
      ((eq? (car stmt) 'while) (mStateWhile stmt env return next class))
      ((eq? (car stmt) 'function) (next (mStateFuncDec stmt env return next break continue)))
      ((eq? (car stmt) 'funcall) (begin (mStateFuncCall (getName stmt) (cddr stmt) env class) (next env)))      
      ((eq? (car stmt) 'class) (begin (set! env (add (cadr stmt) (newClass stmt env) env)) (mStateList (cadddr stmt) env return next break continue (cadr stmt))))
      ((eq? (car stmt) 'static-function) (setFunctionEnv class (mStateFuncDec stmt (getFunctionEnv class env) return next break continue) env next))
      ((eq? (car stmt) 'static-var) (setFieldEnv class (mStateAssignStatic stmt (getFieldEnv class env) class) env next))
      ((eq? (car stmt) 'dot) (dotOp (cadr stmt) (caddr stmt) env class))
      ((and (null? (cdr stmt)) (eq? (car stmt) 'begin)) (next env))
      ((eq? (car stmt) 'begin) (mStateList (cdr stmt) (newLayerEnv env) return (lambda (v) (next (cdrEnvLayer v))) (lambda (v) (break (cdrEnvLayer v))) (lambda (v) (continue (cdrEnvLayer v))) class))
      (else (next env)))))

;mState for executing function bodies
(define mStateFuncBody
  (lambda (actualParam formalParam body env return class)
    (cond
      ((not (eq? (length actualParam) (length formalParam))) (error '(Wrong param and arguements)))
      ((null? actualParam) (mStateList body env return (lambda (v) v) (lambda (v) v) (lambda (v) v) class))
      (else (mStateFuncBody (cdr actualParam) (cdr formalParam) body (add (car formalParam) (car actualParam) env) return class)))))

;mState for function calls
(define mStateFuncCall
  (lambda (name paramlist env class)
      (call/cc (lambda (return)
                 (mStateFuncBody (mValueAllParams paramlist env class) (car (lookup name env)) (cadr (lookup name env)) ((caddr (lookup name env)) env) return class)))))

;mState declaration
(define mStateFuncDec
  (lambda (stmt env ret next break continue)
    (addBinding (getName stmt) (makeClosure (getParamList stmt) (getFuncBody stmt) functionEnv) env)))

;mStateBoolean feeds to the value function
; ==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, 
(define mStateBoolean
  (lambda (expression enviornment class)
    (mValueBoolean expression enviornment prefix class)))

;mStateIf handles if statements
(define mStateIf
  (lambda (statement enviornment return next break continue class)
    (cond
      ((eq? #t (mStateBoolean (getCondition statement) enviornment class)) (mStateAll (getBody statement) enviornment return break continue next class))
      ((null? (cdddr statement)) (next enviornment))
      (else (mStateAll (cadddr statement) enviornment return break continue next class)))))

;mStateWhile handles while loops
(define mStateWhile
  (lambda (stmt environment ret next)
	(letrec ((loop (lambda (condition body environment next)
			(if (mStateBoolean condition environment)
			(mStateAll body environment ret (lambda (env) (next env)) (lambda (env) (loop condition body env next)) (lambda (env) (loop condition body env next)))
                        (next environment))))) 
	(loop (getCondition stmt) (getBody stmt) environment next))))

;mState for assignment
(define mStateAssign
  (lambda (stmt env class)
    (cond
      ((null? stmt) env)
      ((and (eq? (car stmt) 'var) (null? (cddr stmt))) (add (cadr stmt) 'null env))
      ((eq? (car stmt) 'var) (add (cadr stmt) (mValueAll (caddr stmt) env class) env))
      ((eq? (car stmt) '=) (myRemove (cadr stmt) (mValueAll (caddr stmt) env class) env (lambda (v) v)))
      (else (error '(Assignment Error))))))

;mState for static assignment
(define mStateAssignStatic
  (lambda (stmt env class)
    (cond
      ((null? stmt) env)
      ((and (eq? (car stmt) 'static-var) (null? (cddr stmt))) (add (cadr stmt) 'null env))
      ((eq? (car stmt) 'static-var) (add (cadr stmt) (mValueAll (caddr stmt) env class) env))
      ((eq? (car stmt) '=) (myRemove (cadr stmt) (mValueAll (caddr stmt) env class) env (lambda (v) v)))
      (else (error '(Assignment Error))))))

;mState functions for try stuff
;(define mStateTry
;  (lambda (tryBody catch finallyBody return break continue next class environment)
;    (cond
;      ((null? finallyBody) (mStateDoTry tryBody catch return break continue next class environment))
;      (else (begin (mStateDoTry tryBody catch return break continue next class environment) (mStateList finallyBody (newLayerEnv environment) return (lambda (v) (next (cdrEnvlayer v))) (lambda (v) (break (cdrEnvlayer v))) (lambda (v) (continue (cdrEnvlayer v))) class class environment))))))

;(define mStateDoTry
;  (lambda (tryBody catch return break continue next class environment)
;    (mStateList tryBody (newLayerEnv environment) return (lambda (v) (next (cdrEnvlayer v))) (lambda (v) (break (cdrEnvlayer v))) (lambda (v) (continue (cdrEnvlayer environment))) (lambda (v) (mStateList (caddr catch) (add (cadr catch) v (newLayerEnv env)) return (lambda (v) (next (cdrEnvlayer v))) (lambda (v) (break (cdrEnvlayer v))) (continue (cdrEnvlayer v))) class class environment) class class environment)))







;///////////////Value Functions//////////////////////////////////////////////
;top level mValue function
(define mValueAllParams
  (lambda (param env class)
    (cond
      ((null? param) '())
      (else (cons (mValueAll (car param) env class) (mValueAllParams (cdr param) env class))))))

;mValue handles boolean values and feeds to function state or feeds other expressions to evaluate
(define mValueAll
  (lambda (stmt env class)
    (cond
      ((number? stmt) stmt)
      ((eq? 'true stmt) true)
      ((eq? 'false stmt) false)
      ((not (list? stmt)) (lookup stmt env))
      ((and (eq? (car stmt) 'funcall) (list? (cadr stmt))) (mStateFuncCall (caddr (getName stmt)) (cddr stmt) (finalEnvironment (cadr (getName stmt)) class) class))
      ((eq? (car stmt) 'funcall) (mStateFuncCall (mValueAll (getName stmt) env class) (cddr stmt) env class))
      ((eq? (car stmt) 'dot) (dotOp (cadr stmt) (caddr stmt) env class))
      (else (evaluate stmt env prefix class)))))



;gets the values for expressions
(define evaluate
  (lambda (expression environment form class)
    ((lambda (operator leftOperand rightOperand)
       (cond
         ((null? expression) 0)
         ((number? expression) expression)
         ((not (list? expression)) (lookup expression environment))
         ((eq? '+ (operator expression)) (+ (mValueAll (leftOperand expression) environment class) (mValueAll (rightOperand expression) environment class)))
         ((and (eq? '- (operator expression)) (null? (cdr (cdr expression)))) (- (mValueAll (cadr expression) environment class)))
         ((eq? '- (operator expression)) (- (mValueAll (leftOperand expression) environment class) (mValueAll (rightOperand expression) environment class)))
         ((eq? '* (operator expression)) (* (mValueAll (leftOperand expression) environment class) (mValueAll (rightOperand expression) environment class)))
         ((eq? '/ (operator expression)) (quotient (mValueAll (leftOperand expression) environment class) (mValueAll (rightOperand expression) environment class)))
         ((eq? '% (operator expression)) (modulo (mValueAll (leftOperand expression class) environment) (mValueAll (rightOperand expression) environment class)))
         ((and (not (list? expression)) (number? (lookup expression environment))) (lookup expression environment))
         (else (mValueBoolean expression environment form class))))
     (getOperator form) (getLeftOperand form) (getRightOperand form))))

;gets value for boolean expressions
;==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, !
(define mValueBoolean
  (lambda (expression environment form class)
    ((lambda (operator leftOperand rightOperand)
       (cond
         ((not (list? expression)) (lookup expression environment))
         ((eq? 'true (car expression)) true)
         ((eq? 'false (car expression)) false)
         ((eq? '&& (operator expression)) (and (mValueAll  (leftOperand expression) environment class) (mValueAll  (rightOperand expression) environment class)))
         ((eq? '! (operator expression))  (not (mValueAll (cadr expression) environment class)))
         ((eq? '|| (operator expression)) (or (mValueAll  (leftOperand expression) environment class) (mValueAll  (rightOperand expression) environment class)))
         ((eq? '== (operator expression)) (equal? (mValueAll  (mValueAll (leftOperand expression) environment class) environment class) (mValueAll  (mValueAll (rightOperand expression) environment class) environment class)))
         ((eq? '!= (operator expression)) (not (equal? (mValueAll  (mValueAll (leftOperand expression) environment class) environment class) (mValueAll  (mValueAll (rightOperand expression) environment class) environment class))))
         ((eq? '>= (operator expression)) (>= (mValueAll  (mValueAll (leftOperand expression) environment class) environment class) (mValueAll  (mValueAll (rightOperand expression) environment class) environment class)))
         ((eq? '> (operator expression)) (> (mValueAll  (mValueAll (leftOperand expression) environment class) environment class) (mValueAll  (mValueAll (rightOperand expression) environment class) environment class)))
         ((eq? '<= (operator expression)) (<= (mValueAll  (mValueAll (leftOperand expression) environment class) environment class) (mValueAll  (mValueAll (rightOperand expression) environment class) environment class)))
         ((eq? '< (operator expression)) (< (mValueAll  (mValueAll (leftOperand expression) environment class) environment class) (mValueAll  (mValueAll (rightOperand expression) environment class) environment class)))
         (else (error '(Operator is undefined)))))
     (getOperator form) (getLeftOperand form) (getRightOperand form))))




;///////////////////////////Class stuff /////////////////////////

(define newClass
  (lambda (stmt env)                 
    (cond                
      ((null? (caddr stmt)) (list '((()())) '((()()))  '((()())) '(()()))) ;basic format
      (else (list (getFunctionEnv (getExtends stmt) env)
                  (getFieldEnv (getExtends stmt) env)
                  (getInstanceEnv (getExtends stmt) env)
                  (getParent (getExtends stmt) env))))))

;handles extending
(define getExtends
  (lambda (stmt)
    (car (cdaddr stmt))))

;parent
(define getParent
  (lambda (class env)
    (cadddr (lookup class env))))

;getters and setters for the parts of the environment containing the different parts of class
(define getFunctionEnv
  (lambda (class env)
    (car (lookup class env))))

(define setFunctionEnv
  (lambda (class new env next)
    (myRemove class (list new (getFieldEnv class env) (getInstanceEnv class env) (getParent class env)) env next)))
    
(define getFieldEnv
  (lambda (class env)
    (cadr (lookup class env))))

(define setFieldEnv
  (lambda (class new env next )
    (myRemove class (list (getFunctionEnv class env) new (getInstanceEnv class env) (getParent class env)) env next)))

(define getInstanceEnv
  (lambda (class env)
    (caddr (lookup class env))))

(define setInstanceEnv
  (lambda (class new env next)
    (myRemove class (list (getFunctionEnv class env) (getFieldEnv class env) new (getParent class env)) env next)))
    

(define dotOp
  (lambda (name called env class)
    (cond
      ((eq? 'super name) (lookupSuper called env #f))
      (else (lookup called (finalEnvironment name env))))))



;///////////////////////////Layers, Closures, and enviorments////////////////////////
(define finalEnvironment
  (lambda (class env)
    (append
     (list (car (getFunctionEnv class env))
          (car (getFieldEnv class env))
          (car (getInstanceEnv class env))
          (getParent class env))
     env)))

(define mainEnv
  (lambda (class env)
    (lookup class env)))

(define functionEnv
  (lambda (s)
    (cond
      ((null? s) (newLayerEnv s))
      ((null? (cdr s)) (newLayerEnv s))
      (else (functionEnv (cdr s))))))

(define changeClosure
  (lambda (actual closure env class)
    (cond
      ((and (null? actual) (null? (closedParam closure))) env)
      ((or (null? actual) (null? (closedParam closure))) (error '(Undefined inside closure)))
      (else (changeClosure (cdr actual)
                           (cons (cdr (closedParam closure)) (cdr closure))
                           (addBinding (car (closedParam closure)) (mValueAll (car actual) env class) env))))))
      
(define closedParam
  (lambda (closure)
    (car closure)))
(define closedBody
  (lambda (closure)
    (cadr closure)))
(define closedF
  (lambda (closure)
    (caddr closure)))


(define addBinding
  (lambda (name val env)
    (add name val env)))

(define makeClosure
  (lambda (args body Fstate)
    (list args body Fstate)))

(define newLayerEnv
  (lambda (env)
    (cons '(()()) env)))

(define cdrEnvLayer
  (lambda (env)
    (cdr env)))    

;lookup variable
(define lookup
  (lambda (var enviornment)
    (cond
      ((or (eq? var 'true) (eq? var 'false) (number? var)) var)
      ((null? enviornment) (error 'undefined))
      ((null? (caar enviornment)) (lookup var (cdr enviornment)))
      ((eq? var (caaar enviornment)) (unbox (caadar enviornment)))
      ((null? (caaar enviornment)) (lookup var (cdr enviornment)))
      (else (lookup var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)))))))

(define lookupFunction
  (lambda (class var env)
    (lookup var (getFunctionEnv class env))))

;super functionality
(define lookupSuper
  (lambda (var enviornment bool)
    (cond
      ((or (eq? var #t) (eq? var #f) (number? var)) var)
      ((null? enviornment) (error 'undefined))
      ((null? (caar enviornment)) (lookupSuper var (cdr enviornment) bool))
      ((and (eq? #t bool) (eq? var (caaar enviornment))) (unbox (caadar enviornment)))
      ((and (eq? #f bool) (eq? var (caaar enviornment))) (lookupSuper var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)) #t))
      ((and (eq? 'true bool) (eq? var (caaar enviornment))) (unbox (caadar enviornment)))
      ((and (eq? 'false bool) (eq? var (caaar enviornment))) (lookupSuper var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)) #t))
      ((null? (caaar enviornment)) (lookupSuper var (cdr enviornment)))
      (else (lookupSuper var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)) bool)))))



(define add
  (lambda (var val enviornment)
    (cons (list (cons var (caar enviornment)) (cons (box val) (cadar enviornment))) (cdr enviornment))))

(define myRemove
  (lambda (var val s next)
    (cond
      ((null? s) (error '(Undefined variable)))
      ((null? (caar s)) (myRemove var val (cdr s) (lambda (v) (next (cons '(()()) v)))))
      ((eq? var (caaar s)) (next (begin (set-box! (caadar s) val) s)))
      (else (myRemove var val (cons (list (cdaar s) (cdadar s)) (cdr s)) (lambda (v) (next (cons (list (cons (caaar s) (caar v)) (cons (caadar s) (cadar v))) (cdr v)))))))))

;/////////////////////General Helpers//////////////////////////////
(define getName
  (lambda (stmt)
    (cadr stmt)))
(define getParamList
  (lambda (stmt)
    (caddr stmt)))
(define getFuncBody
  (lambda (stmt)
    (cadddr stmt)))

(define getCondition
  (lambda (stmt)
    (cond
      ((eq? 'while (car stmt)) (cadr stmt))
      ((eq? 'if (car stmt)) (cadr stmt)))))

(define getBody
  (lambda (stmt)
    (cond
      ((eq? 'while (car stmt)) (caddr stmt))
      ((eq? 'if (car stmt)) (caddr stmt)))))

; retrieves the operator function from the format
(define getOperator
  (lambda (form)
    (car (form))))

; retrieves the leftOperand function from the format
(define getLeftOperand
  (lambda (form)
    (cadr (form))))

; retrieves the rightOperand function from the format
(define getRightOperand
  (lambda (form)
    (caddr (form))))
