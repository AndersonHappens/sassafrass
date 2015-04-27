;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EECS 345
; Interpreter Project Part 4
;                                                                                             
;   ;;;;;          ;                                         ;                            ;;  
;     ;            ;                                         ;                           ;;;  
;     ;    ;;;;  ;;;;;;   ;;;;  ; ;;;  ;;;;   ; ;;;   ;;;; ;;;;;;   ;;;;  ; ;;;          ; ;  
;     ;    ;;  ;   ;     ;    ; ;;  ;  ;;  ;  ;;  ;  ;    ;  ;     ;    ; ;;  ;         ;  ;  
;     ;    ;   ;   ;     ;;;;;; ;      ;   ;  ;      ;;;;;;  ;     ;;;;;; ;            ;   ;  
;     ;    ;   ;   ;     ;      ;      ;   ;  ;      ;       ;     ;      ;            ;;;;;;;
;     ;    ;   ;   ;     ;;     ;      ;   ;  ;      ;;      ;     ;;     ;                ;  
;   ;;;;;  ;   ;   ;;;;   ;;;;; ;      ;;;;   ;       ;;;;;  ;;;;   ;;;;; ;                ;  
;                                      ;                                                      
;                                      ;                                                      
;                                      ;  


(load "classParser.scm")

; Interpret interprets a file and returns the result
; file has class definitions, class is a string naming which class's main method to use
; calls main method on the environment created by parsing file
(define interpret
  (lambda (file class)
       ((lambda (class-environment)
          (call-func '(funcall main '()) (make-inst (string->symbol class) class-environment) class-environment defaultReturn (lambda (throw state) (error 'exc-generic))))
     ; lookup class, lookup parent, assign parent body to super in environment
     (build-class-environment (parser file) newState (lambda(v) v) (lambda (throw) (error 'exc-generic))))
    ))

; (name (extends) ((var) (val)) ((func) (def)) (statements?))
(define build-class-environment
  (lambda (parse env return exc)
    (if (null? parse) env
        (build-class-environment (cdr parse) 
                                 (state-classdef (car parse) env env return exc) return exc))))

(define state-classdef
  (lambda (stmt state env return exc)
    (cond
     ((Declared (funcdef-fn-name stmt) state) (error (funcdef-fn-name stmt) "Function name has already been defined as something else."))
     (else (return (Add state (cadr stmt) (create-class-closure stmt state env exc)))))))

;I don't really know what this closure thing is all about. I don't think I quite need it yet, not having instances and all.
; And by that, I mean I don't need to understand how bind-all-in-state works right now
(define create-class-closure
  (lambda (stmt state env exc)
    ((lambda (parent superenv)
       ((lambda (parent-body)
       (list parent (if (null? parent)(car (define-all-variables (class-stmts stmt) newState env (lambda (a) a) exc))
                       ; (car (define-all-variables (class-stmts stmt) newState env (lambda (a) a))))
                        (list (append (caar (define-all-variables (class-stmts stmt) superenv env (lambda (a) a) exc)) (car (cadr parent-body)))
                              (append (cadr(cadr parent-body)) (cadar (define-all-variables (class-stmts stmt) superenv env (lambda (a) a) exc)))))
                        ;Includes parent's values, but issues with repeated var/func names
             (if (null? parent)(car (define-all-functions (class-stmts stmt) newState env (lambda (a) a) exc))
                ; (car (define-all-functions (class-stmts stmt) newState env (lambda (a) a))))
                        (list (append (caar (define-all-functions (class-stmts stmt) superenv env (lambda (a) a) exc)) (car (caddr parent-body)))
                              (append (cadr (caddr parent-body)) (cadar (define-all-functions (class-stmts stmt) superenv env (lambda (a) a) exc)))))
             ;Includes parent's values, but issues with repeated var/func names
             (lambda (actualParams currentExecutionState)
               ; rebind variables in parameter list here when executed with the current execution state
               (bind-all-in-state (funcdef-fn-params stmt) actualParams (PushLevel (PushSeparator (TrimStates currentExecutionState state))) currentExecutionState (lambda (returnState) returnState exc))
            )))
        (if (null? parent) '() (unbox (caadar parent)))))
     (find-parent (class-extends stmt) state (class-extends stmt)) (find-parent (class-extends stmt) state '(() super)))))

(define find-parent
  (lambda (parent env rdac-name)
    (cond 
      ((null? parent) '())
      (else (Add newState (cadr rdac-name) (Lookup (cadr parent) env))))))

; probably another misnomer. I'm good at those
; called between setting up classes and calling main
; formulates a level of fields and funcs from the given class
(define make-inst      
  (lambda (classname env)
    ((lambda (vars funcs)
       (cons (list (append (car vars) (car funcs)) (append (cadr funcs) (cadr vars))) env))
     (cadr (Lookup classname env)) (caddr (Lookup classname env)))))

;---------------------------------------------------------------------------------------------------------------------
; Defines the default return lambda for two-value continuations
(define defaultReturn (lambda (returnVal returnState) returnVal))

(define class-name cadr)
(define class-extends caddr)
(define class-stmts cadddr)

; Like parse-class, except only for variables
(define define-all-variables
  (lambda (stmts state env return exc)
    (cond
     ((null? stmts) (return state))
     ((isvardec (car stmts)) (state-declare (car stmts) state env (lambda (newState)
                                                                (define-all-variables (cdr stmts)
                                                                                  newState
                                                                                  return exc)) exc))
     
     ((is-static-var-dec (car stmts)) (state-declare (car stmts) state env (lambda (newState)
                                                                (define-all-variables (cdr stmts)
                                                                  newState
                                                                  env 
                                                                  return
                                                                  exc)) exc))
     ((isassign (car stmts)) (state-assign (car stmts) state env (lambda (newState)
                                                               (define-all-variables (cdr stmts)
                                                                 newState
                                                                 env 
                                                                 return
                                                                 exc)) exc))
     (else (define-all-variables (cdr stmts) state env return exc)))));

; Value gets the value of the list of statements given the current state
(define value
  (lambda (stmts state env return exc)
    (values-control stmts
                    state
                    env
                    return
                    (lambda (returnValue breakState)
                      (error "Break statement outside of control structure"))
                    (lambda (returnValue continueState)
                      (error "Continue statement outside of control structure"))
                    exc)))

; values-control
; sets up passing through continuations for break and continue, handling the break and continue definitions
(define values-control
  (lambda (stmts state env return break continue exc)
    (cond
     ((or (null? stmts) (null? (car stmts))) (return '() state))
     ((isblock (car stmts)) (values-control (cdar stmts) (PushLevel state) env (lambda (returnVal newState)
                                                                                 (cond
                                                                                  ((eq? returnVal #t) (return 'true newState))
                                                                                  ((eq? returnVal #f) (return 'false newState))
                                                                                  ((not (null? returnVal)) (return returnVal newState))
                                                                                  (else (values-control (cdr stmts) (PopLevel newState) env return break continue exc))))
                                            break
                                            continue
                                            exc))
     ((isbreak (car stmts)) (break '() (PopLevel state) env))
     ((iscontinue (car stmts)) (continue '() (PopLevel state) env))
     (else (value-stmt (car stmts) state env (lambda (returnVal newState)
                                           (cond
                                            ((eq? returnVal #t) (return 'true newState))
                                            ((eq? returnVal #f) (return 'false newState))
                                            ((not (null? returnVal)) (return returnVal newState))
                                            (else (values-control (cdr stmts) newState env return break continue exc)))) break continue exc)))))

; value-stmt computes the value that is created by a single statement
(define value-stmt
  (lambda (stmt state env return break continue exc)
    (cond
     ((null? stmt) (return '() state))
     ((iswhilestmt stmt) (value-while-stmt stmt state env return break continue exc))
     ((isreturn stmt) (value-return stmt state env return break continue exc))
     ((isifstmt stmt) (value-if-stmt stmt state env return break continue exc))
     ((isfncall stmt) (call-func stmt state env (lambda (returnVal returnState)
                                              (return '() returnState)) exc))
     ((is-try stmt) (value-exceptions (exc-try-block stmt) state env return break continue 
                                      (lambda (throw newState)(value-exceptions throw newState env return break continue exc (exc-catch-block stmt) (exc-finally-block stmt)))
                                      (exc-catch-block stmt) (exc-finally-block stmt)))
     ((is-throw stmt) (exc (list stmt) state))
     (else (state-stmt stmt state env (lambda (v)
                                    (return '() v)) exc)))))

; Computes the return value and state changes associated with a while loop
(define value-while-stmt
  (lambda (stmt state env return break continue exc)
    (value-bool-expr (while-stmt-cond stmt) state env (lambda (boolVal ifstate)
                                                        (if boolVal (values-control (list (while-stmts stmt))
                                                                                state
                                                                                env
                                                                                (lambda (returnVal returnState)
                                                                                  (if (not (null? returnVal))
                                                                                      (return returnVal returnState)
                                                                                      (value-while-stmt stmt returnState env return break continue exc)))
                                                                                (lambda (breakReturnVal breakState)
                                                                                  (return breakReturnVal breakState))
                                                                                (lambda (continueReturnVal continueState)
                                                                                  (value-while-stmt stmt continueState return break continue exc)) exc)
                                                        (return '() state))) exc)))

; Gets the condition from a while statement
(define while-stmt-cond cadr)
; Gets the block executed for a while statement
(define while-stmts caddr)

; value-if-stmt returns the value generated by the inner statements of the if statement
(define value-if-stmt
  (lambda (stmt state env return break continue exc)
    (value-bool-expr (if-stmt-cond stmt) state env (lambda (v1 v2)
                                                     (if v1 (values-control (list (if-stmt-true stmt)) state env return break continue exc) 
                                                         (values-control (list (if-stmt-false stmt)) state env return break continue exc))) exc)))

; value-return returns the value returned by a return statement
(define value-return
  (lambda (stmt state env return break continue exc)
    (value-expr (cadr stmt) state env return exc)))

; Value-expr gets the value from an expression
(define value-expr
  (lambda (expr state env return exc)
    (cond
     ((null? expr) (return '() state))
     ((atom? expr) (value-atom expr state env return))
     ((member (expr-operator expr) '(+ - * / %)) (value-arith-expr expr state env return exc))
     ((member (expr-operator expr) '(== != < > <= >= && || !)) (value-bool-expr expr state env return exc))
     ((isfncall expr) (call-func expr state env return exc))
     ((is-super expr) (return (Lookup (caddr expr) (list (cadr (Lookup 'super state)))) state))
     ((is-dot expr) (return (Lookup (caddr expr) (list (cadr (Lookup (cadr expr) env)))) state))
     (else (error "Undefined Operator" (expr-operator expr)))
     )))

; value-arith-expr returns the value of an arithmetic expression
(define value-arith-expr
  (lambda (expr state env return exc)
    (cond
     ((eq? '+ (expr-operator expr)) (apply-operator + (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     ((eq? '- (expr-operator expr)) (value-expr (expr-left-operand expr) state env (lambda (leftVal leftState)
                                                                                 (value-expr (expr-right-operand expr) state env (lambda (rightVal rightState)
                                                                                                                               (if (null? rightVal) (return (- 0 leftVal) state) (return (- leftVal rightVal) state))) exc)) exc))
     ((eq? '* (expr-operator expr)) (apply-operator * (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     ((eq? '/ (expr-operator expr)) (apply-operator quotient (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     ((eq? '% (expr-operator expr)) (apply-operator modulo (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     (else (error "Not a mathematical operator: " (expr-operator expr)))
     )))

; value-bool-expr returns the value of a boolean expression
(define value-bool-expr
  (lambda (expr state env return exc)
    (cond
     ((eq? expr 'true) (return #t state))
     ((eq? expr 'false) (return #f state))
     ((eq? '== (expr-operator expr)) (apply-operator eq? (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     ((eq? '!= (expr-operator expr)) (apply-operator (lambda (v1 v2) (not (eq? v1 v2))) (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     ((eq? '< (expr-operator expr)) (apply-operator < (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     ((eq? '> (expr-operator expr)) (apply-operator > (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     ((eq? '<= (expr-operator expr)) (apply-operator <= (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     ((eq? '>= (expr-operator expr)) (apply-operator >= (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     ((eq? '&& (expr-operator expr)) (apply-operator (lambda (v1 v2) (and v1 v2)) (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     ((eq? '|| (expr-operator expr)) (apply-operator (lambda (v1 v2) (or v1 v2)) (expr-left-operand expr) (expr-right-operand expr) state env return exc))
     ((eq? '! (expr-operator expr)) (value-expr (expr-left-operand expr) state env (lambda (leftVal stateLeft)
                                                                                 (return (not leftVal) state)) exc))
     (else (error "Not a boolean operator: " (expr-operator expr)))
     )))

; Apply-operator takes the operator and applies it to the given operands.
; The operands are automatically passed into value-expr and then evaluated
(define apply-operator
  (lambda (operator leftOperand rightOperand state env return exc)
    (value-expr leftOperand state env (lambda (leftVal stateLeft)
                                    (value-expr rightOperand state env (lambda (rightVal stateRight)
                                                                     (return (operator leftVal rightVal) state)) exc)) exc)))
; value-atom returns the value of an atom
(define value-atom
  (lambda (atom state env return)
    (cond
     ((number? atom) (return atom state))
     ((eq? atom 'true) (return #t state))
     ((eq? atom 'false) (return #f state))
     (else (return (Lookup atom state) state)))))

; expr-operator gets the operator from an expression
(define expr-operator car)

; expr-left-operand gets the left operand from an expression
(define expr-left-operand cadr)

; expr-right-operand gets the right operand from an expression, returning null '() if there is not one.
(define expr-right-operand
  (lambda (expr)
    (cond
     ((null? (cddr expr)) '())
     (else (caddr expr)))))

; Try - Catch - Finally stuff
(define value-exceptions
  (lambda (stmts state env return break continue exc catch finally)
    (cond 
      ((null? stmts) (if (null? finally) (return '() state)
                         (value-exceptions (cadr finally) state env return break continue exc catch '())))
      ((is-throw (car stmts)) (if (null? catch) (throw (cdar stmts))
                                  (value-exceptions (caddr catch) (Add state (caadr catch) (value-atom (cadar stmts) state env return)) env return break continue exc '() finally)))   
      ((isreturn (car stmts)) (if (null? finally) (value-return (car stmts) state env return break continue)
                                  (value-exceptions (append (cadr finally) stmts) state env return break continue exc catch '())))
      (else (value-exceptions (cdr stmts) (values-control (list (car stmts)) state env (lambda (v1 v2) v2) break continue exc) env return break continue exc catch finally)))
    ))

(define throw
  (lambda (e)
    (error "Error:" e)))

(define exc-try-block cadr)
(define exc-catch-block
  (lambda (stmt)
    (if (null? (cddr stmt)) '() (caddr stmt))))
(define exc-finally-block 
  (lambda (stmt)
    (if (null? (cdddr stmt)) '() (cadddr stmt))))


(define state-stmts
  (lambda (stmts state env return)
    (cond
     ((null? stmts) (return state))
     (else (state-stmt (car stmts) state env (lambda (v)
                                           (state-stmts (cdr stmts) v env return)))))))

; State-stmt computes the state bindings given an initial state and a statement
(define state-stmt
  (lambda (stmt state env return exc)
    (cond
     ((null? stmt) (return state))
     ((isassign stmt) (state-assign stmt state env return exc))
     ((isvardec stmt) (state-declare stmt state env return exc))
     ((isifstmt stmt) (state-if-stmt stmt state env return exc))
     ((iswhilestmt stmt) (state-while-stmt stmt state env return exc))
     (else (return state))
     )))

; Computes the state changes done in a while loop
(define state-while-stmt
  (lambda (stmt state env return exc)
    (if (value-bool-expr (while-stmt-cond stmt) state env exc) (state-while-stmt stmt (state-stmt (while-stmts stmt) state env return exc) env return exc) (return state))))
;shouldn't value-bool-expr have a fourth parameter?

; state-if-stmt computes the state changes associated with evaluating an if statement
(define state-if-stmt
  (lambda (stmt state env return exc)
    (cond
     ((value-bool-expr (if-stmt-cond stmt) state env exc) (return (state-stmt (if-stmt-true stmt) state env)))
     (else (state-stmt (if-stmt-false stmt) state env return)))))

;if-stmt-cond retrieves the condition from an if statement
(define if-stmt-cond cadr)

; if-stmt-true retrieves the statement that would be evaluated if the condition is true
(define if-stmt-true caddr)

; if-stmt-false retrieves the satement that would be evaluated if the condition is false
(define if-stmt-false
  (lambda (stmt)
    (if (null? (cdddr stmt)) '() (cadddr stmt))))

; state-assign computes the updated state after evaluating an assignment statement
(define state-assign
  (lambda (stmt state env return exc)
    (cond
     ((not (Declared (assign-var-name stmt) state)) (error (assign-var-name stmt) "Variable assignment before declaration!"))
     (else (return (Replace state (assign-var-name stmt) (assign-value stmt state env exc))))
     )))

; assign-var-name gets the name of the variable being assigned in an assignment statement
(define assign-var-name cadr)

; assign-value retrieves the value that is being assigned to a variable in an assignment statement
(define assign-value
  (lambda (stmt state env exc)
    (value-expr (caddr stmt) state env (lambda (v1 v2) v1) exc)))

; state-declare computes the updated state after evaluating a variable declaration statement
(define state-declare
  (lambda (stmt state env return exc)
    (cond
     ((DeclaredInScope (declare-var-name stmt) state) (error (declare-var-name stmt) "Variable is being re-defined"))
     (else (return (Addf state (declare-var-name stmt) (declare-value stmt state env exc)))))))

; declare-value returns the value that a variable is being declared to 
; in a declare statement, or the empty list if there is no specified value
(define declare-value
  (lambda (stmt state env exc)
    (cond
     ((null? (cddr stmt)) '())
     (else (value-expr (caddr stmt) state env (lambda (v1 v2) v1) exc)))))

; delare-var-name retrieves the variable name from a given declare statement
(define declare-var-name cadr)

; Determines the return value and state changes done by a function call.
(define call-func
  (lambda (stmt state env return exc)
    ((lambda (fn-closure)
       (value (fn-closure-body fn-closure) (begin
                                             (define-all-functions (fn-closure-body fn-closure) ((fn-closure-state-fn fn-closure) 
                                                                                                 (fn-call-actual-params stmt) state env) env (lambda (v) v) exc)) env (lambda (returnVal returnState)
                                                                                                                                                            (return returnVal state)) exc)) 
     (fn-dot-manager (fn-call-name stmt) state env))))
;((fn-closure-state-fn fn-closure) (fn-call-actual-params stmt) state env) env (lambda (v) v))) returns #t 
; rather than returning a state with #t bound to b

; checks if function call is normal or a dot operator
(define fn-dot-manager
  (lambda (name state env)
    (cond
      ((atom? name) (Lookup name state))
      ((is-super name) (Lookup (caddr name) (list (caddr (Lookup 'super state)))))
      ((is-dot name) (Lookup (caddr name) (list (caddr (Lookup (cadr name) env)))))
      (else (error name "is not a valid fncall name")))))

; Gets the actual parameters from a function call statement
(define fn-call-actual-params cddr)
; Gets the name of the function called froma function call statement
(define fn-call-name cadr)

; Gets the body of a function from a function's closure
(define fn-closure-body cadr)
; Gets the execution state creation function from a function's closure
(define fn-closure-state-fn caddr)
; Gets the list of formal parameters from the function's closure
(define fn-closure-formal-params car)

; Adds any function definitions in the list of statement to the highest layer of the given state
(define define-all-functions
  (lambda (stmts state env return exc)
    (cond
     ((null? stmts) (return state))
     ((isfuncdef (car stmts)) (state-funcdef (car stmts) state env (lambda (newState)
                                                                     (define-all-functions (cdr stmts)
                                                                       newState
                                                                       env 
                                                                       return
                                                                       exc)) exc))
     ((is-static-func-def (car stmts)) (state-static-funcdef (car stmts) state env (lambda (newState)
                                                                                          (define-all-functions (cdr stmts)
                                                                                            newState
                                                                                            env 
                                                                                            return
                                                                                            exc)) exc))
     (else (define-all-functions (cdr stmts) state env return exc)))));

; Computes the function closure and adds it to the state given
(define state-funcdef
  (lambda (stmt state env return exc)
    (cond
     ((Declared (funcdef-fn-name stmt) state) (error (funcdef-fn-name stmt) "Function name has already been defined as something else."))
     (else (return (Add state (funcdef-fn-name stmt) (create-fn-closure stmt state env exc)))))))

(define state-static-funcdef
  (lambda (stmt state env return exc)
    (cond
     ((Declared (funcdef-fn-name stmt) state) (error (funcdef-fn-name stmt) "Function name has already been defined as something else."))
     (else (return (Add state (funcdef-fn-name stmt) (create-fn-closure stmt (PushLevel state) env exc)))))))

; creates the function closure object
(define create-fn-closure
  (lambda (stmt state env exc)
    (list (funcdef-fn-params stmt) (funcdef-fn-body stmt) (lambda (actualParams currentExecutionState currentEnvironment)
                                        ; rebind variables in parameter list here when executed with the current execution state
                                                            (bind-all-in-state (funcdef-fn-params stmt) actualParams (PushLevel (PushSeparator (TrimStates currentExecutionState state))) currentExecutionState currentEnvironment (lambda (returnState) returnState) exc)
                                                            ))))
; Trims the curent execution state to be the length of the declaration state for static scoping
(define TrimStates
  (lambda (state1 state2)
    (cond
     ((eq? (length state1) (length state2)) state1)
     (else (TrimStates (cdr state1) state2)))))

; binds all the actual parameters (exprs) to the formal parameters (names) using the current state to compute the 
; value of the expressions and storing the results in the declarestate
(define bind-all-in-state
  (lambda (names exprs declarestate currentstate env return exc)
    (cond
     ((null? names) (return declarestate))
     ((not (eq? (length names) (length exprs))) (error "Mismatched function formal and actual parameters"))
     (else (value-expr (car exprs) currentstate env (lambda (exprVal exprState)
                                                  (bind-all-in-state (cdr names) (cdr exprs) declarestate currentstate env (lambda (newState)
                                                                                                                         (return (Addf newState (car names) exprVal))) exc)) exc)))))
; Gets the name of a function from its declaration statement
(define funcdef-fn-name cadr)
; Gets the list of formal parameters of a function from its declaration statement
(define funcdef-fn-params caddr);
; Gets the body of a function from its declaration statement
(define funcdef-fn-body cadddr);


; isifstmt determins if the given statement is an if statement
(define isifstmt
  (lambda (stmt)
    (eq? (car stmt) 'if)))

; isvardec determines if the given statement is a variable declaration statement
(define isvardec
  (lambda (stmt)
    (eq? (car stmt) 'var)))

; isreturn determines if the given statement is a return statement
(define isreturn
  (lambda (stmt)
    (eq? (car stmt) 'return)))

; isassign determines if the given statement is an assignment statement
(define isassign
  (lambda(stmt)
    (eq? (car stmt) '=)))

; iswhilestmt determines if the given statement is a while statement
(define iswhilestmt
  (lambda (stmt)
    (eq? (car stmt) 'while)))

; isblock determines if the given statement is a block of statements
(define isblock
  (lambda (stmt)
    (eq? (car stmt) 'begin)))

; isbreak determines if the given statement is break statement
(define isbreak
  (lambda (stmt)
    (eq? (car stmt) 'break)))

; iscontinue determines if the given statement is a continue statement
(define iscontinue
  (lambda (stmt)
    (eq? (car stmt) 'continue)))

; isfuncdef determines if the given statement is a function declaration statement
(define isfuncdef
  (lambda (stmt)
    (eq? (car stmt) 'function)))

; isfncall determines if the given statement is a function call statement
(define isfncall
  (lambda (stmt)
    (eq? (car stmt) 'funcall)))

; isstaticfuncdef determinees if the given statement is a static function definition statement
(define is-static-func-def
  (lambda (stmt)
    (eq? (car stmt) 'static-function)))

; isstaticvar determines if the given statement is a static variable declaration
(define is-static-var-dec
  (lambda (stmt)
    (eq? (car stmt) 'static-var)))

; is-dot determines if the given statement is a dot call
(define is-dot
  (lambda (stmt)
    (eq? (car stmt) 'dot)))

; is-super determines if the given statement is a super call
(define is-super
  (lambda (stmt)
    (eq? (cadr stmt) 'super)))      

; is-try determines if the given statement is a try block
(define is-try
  (lambda (stmt)
    (eq? (car stmt) 'try)))

; is-throw determines if the given statement is a throw command
(define is-throw
  (lambda (stmt)
    (eq? (car stmt) 'throw)))


; BEGIN STATE TABLE MANAGEMENT CODE
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; PushLevel adds a new level to the current state
(define PushLevel
  (lambda (state)
    (cons newStateLevel state)))

(define PushSeparator
  (lambda (state)
    (cons separator state)))

; PopLevel removes the highest level from the current state
(define PopLevel
  (lambda (state)
    (cdr state)))

; Lookup will retrieve the value of the given variable from any of the levels in the state table
(define Lookup
  (lambda (variable state)
    (cond
     ((null? state) (error variable "Variable undefined in state"))
     ((eq? (car state) separator) (Lookup variable (cdr state)))
     ((DeclaredInLevel variable (car state)) (LookupInLevel variable (car state)))
     (else (Lookup variable (cdr state))))))

; LookupInLevel allows you to lookup the value of a variable in the given state level. Will error if there is no binding in place, and WILL error if the variable has not been
; assigned a value. Use Assigned to check and see if the variable has a value before calling this method if you wish to avoid errors.
(define LookupInLevel
  (lambda (variable level)
    (unbox (GetVal (- (length (car level)) (GetIndex variable (car level) (lambda (v) v))) (cadr level)))))

;For use in ReplaceInLevel
(define BoxedLookupInLevel
  (lambda (variable level)
    (GetVal (- (length (car level)) (GetIndex variable (car level) (lambda (v) v))) (cadr level))))

; Queries ???????????????????????????????????????????????????????????????????????????????????????????????????????????
; DeclaredInLevel determines if the variable has been declared in the given level
(define DeclaredInLevel
  (lambda (var level)
    (if (or (eq? level separator) (not (member var (car level)))) #f #t)))

; Declared determines if a variable has been defined in the given state, and will look accross all levels
(define Declared
  (lambda (var state)
    (cond
     ((null? state) #f)
     ((DeclaredInLevel var (car state)) #t)
     (else (Declared var (cdr state))))))

; DeclaredInScope will look and see whether or not the variable is declared somewhere before it encounters a function separator
(define DeclaredInScope
  (lambda (var state)
    (cond
     ((null? state) #f)
     ((eq? (car state) separator) #f)
     ((DeclaredInLevel var (car state)) #t)
     (else (DeclaredInScope var (cdr state))))))

;NOT USED
; Assigned determines if a variable has been assigned a value across all current state levels
(define Assigned
  (lambda (variable state)
    (cond
     ((null? state) #f)
     ((AssignedInLevel variable (car state)) #t)
     (else (Assigned variable (cdr state))))))
;NOT USED
; AssignedInLevel determines if the variable has an assigned (non-null) value in the given level
(define AssignedInLevel
  (lambda (variable level)
    (cond
     ((null? (car level)) #f)
     ((eq? (unbox (caar level)) variable) (not (null? (caadr level))))
     (else (AssignedInLevel variable (list (cdar level) (cdadr level)))))))

; End Queries ?????????????????????????????????????????????????????????????????

; Add will add the variable binding to the highest (first) level of the state only if it hasn't previously been defined in the state before at any level
(define Add
  (lambda (state variable value)
    (cond
     ((not (Declared variable state)) (cons (AddToLevel (car state) variable value) (cdr state)))
     (else state)
     )))

; Addf checks to see if the variable is defined only in the highest level of the state and if no adds it in.
(define Addf
  (lambda (state variable value)
    (cond
     ((not (DeclaredInLevel variable (car state))) (cons (AddToLevel (car state) variable value) (cdr state)))
     (else (error variable "Variable already defined in state."))
     )))

; Adds the binding to the mapping if it does not already exist.
(define AddToLevel
  (lambda (level variable value)
    (cond
     ((null? (car level)) (list (list variable) (list (box value))))
     (else (list (append (car level) (list variable)) (cons (box value) (cadr level)))))))

; ReplaceInState will replace the instance of the variable in the state if it exists, regardless of the level it is defined in
(define Replace
  (lambda (state variable newValue)
    (cond
     ((null? state) state)
     ((DeclaredInLevel variable (car state)) (cons (ReplaceInLevel (car state) variable newValue) (cdr state)))
     (else (cons (car state) (Replace (cdr state) variable newValue))))))

; Remove removes a variable binding from the state if it exists. Returns the given state if there is no variable bound in this state
(define ReplaceInLevel
  (lambda (level variable newValue)
    (begin (set-box! (BoxedLookupInLevel variable level) newValue) level)))

(define GetIndex
  (lambda (variable varlist return)
    (cond
      ((null? varlist) (error "Variable not delared"))
      ((eq? (car varlist) variable) (return 1))                                   ;yes, it's 1-indexed. I'd like it to be zero, but at the same time, this makes life easier
      (else (GetIndex variable (cdr varlist) (lambda (v) (return (+ 1 v))))))))

; gets the value of a variable given the index of the value and the value list
; to call using GetIndex: (GetVal (- (length list) (GetIndex . . . list . . .)) . . . )
(define GetVal
  (lambda (index vallist)
    (cond
      ((null? vallist) (error "GetVal failed"))
      ((zero? index) (car vallist))
      (else (GetVal (- index 1) (cdr vallist))))))


(define newStateLevel '(()()))
(define newState (list newStateLevel))
(define separator "|")

;END STATE TABLE MANAGEMENT CODE
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;atom? determines if the given s-expression is an atom (i.e. not a list or pair of things)
(define (atom? x) (not (or (pair? x) (null? x))))
