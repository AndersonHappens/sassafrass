; ***********************************************************************************************************************
; ***** EECS 345 INTERPRETER: PART 4 ************************************************************************************
; ***********************************************************************************************************************
; ***** APRIL 20, 2015 **************************************************************************************************
; ***********************************************************************************************************************
(load "classParser.scm")

; ***********************************************************************************************************************
; ***** STATE FUNCTIONS FOR DECLARATION AND ASSIGNMENT STATEMENTS *******************************************************
; ***********************************************************************************************************************

; state function for variable declaration
(define State_Declare
  (lambda (expression state classname throw)
    (cond
      ((null? (val expression)) (addbinding (var expression) 'novalue state)) ; 'novalue is placeholder element
      ((validtype? (M_Value (val expression) state classname throw)) (addbinding (var expression) (M_Value (val expression) state classname throw) state))
      (else (error 'invalid-type)))))

; state function for variable assignment
(define State_Assign
  (lambda (expression state classname throw)
    (begin (classremove (var expression) (M_Value (val expression) state classname throw) state classname) state)))

; helper functions for variable assignment and declaration
(define var cadr)

; a value won't necessarily be part of a declaration, so this handles the case where it isn't
(define val
  (lambda (expression)
    (if (null? (cddr expression))
        '()
        (caddr expression))))

; checks if something is an integer or boolean, which are the two types variables are allowed to be
(define validtype?
  (lambda (n)
    (or (integer? n) (boolean? n))))

; ***********************************************************************************************************************
; ***** STATE FUNCTIONS FOR FUNCTION DEFINITIONS AND CALLS **************************************************************
; ***********************************************************************************************************************

; state function for the body of a function
(define State_Func_Body
  (lambda (actualparam formalparam body state return classname throw)
    (cond
      ((not (eq? (length actualparam) (length formalparam))) (error 'mismatched-parameters-and-arguments))
      ((null? actualparam) (M_State_List body state return (lambda (v) v) (lambda (v) v) (lambda (v) v) classname throw))
      (else (State_Func_Body (cdr actualparam) (cdr formalparam) body (addbinding (car formalparam) (car actualparam) state) return classname throw)))))

; state function for function definitions
(define State_Func_Def
  (lambda (expression state)
    (addbinding (functionname expression) (closure (paramlist expression) (functionbody expression) functionlayer) state)))

; state function for function calls
(define State_Func_Call
  (lambda (name parameters state classname throw)
    (call/cc (lambda (return)
               (State_Func_Body (Value_Func_Parameters parameters state classname throw) ; actual parameters
                                (car (classlookup classname name state)) ; formal parameters
                                (cadr (classlookup classname name state)) ; body
                                ((caddr (classlookup classname name state)) name state) ; state, returned by function passed to closure
                                return classname throw))))) ; return, class, and throw

;returns a list containing the values of a function's parameter list
(define Value_Func_Parameters
  (lambda (parameters state classname throw)
    (cond
      ((null? parameters) '())
      (else (cons (M_Value (car parameters) state classname throw)
                  (Value_Func_Parameters (cdr parameters) state classname throw))))))

; helper functions for function definitions and calls

; returns the name of the function
(define functionname cadr)

; returns the function's parameter list
(define paramlist caddr)

; returns the body of the function
(define functionbody cadddr)

; creates a closure for the function
(define closure
  (lambda (args body functionstate)
    (list args body functionstate)))

; ***********************************************************************************************************************
; ***** STATE FUNCTION FOR IF STATEMENTS ********************************************************************************
; ***********************************************************************************************************************

; state function for 'if' statements
(define State_If
  (lambda (statement state return break continue next classname throw)
    (cond
      ((eq? #t (M_Boolean (condition statement) state classname throw))
       (Evaluate (body statement) state return break continue next classname throw))
      ((null? (cdddr statement)) (next state))
      (else (Evaluate (cadddr statement) state return break continue next classname throw)))))

; returns condition for 'if' and 'while' structures
(define condition
  (lambda (expression)
    (cond
      ((eq? 'while (car expression)) (cadr expression))
      ((eq? 'if (car expression)) (cadr expression)))))

; returns body for 'if' and 'while' structures
(define body
  (lambda (expression)
    (cond
      ((eq? 'while (car expression)) (caddr expression))
      ((eq? 'if (car expression)) (caddr expression)))))

; ***********************************************************************************************************************
; ***** STATE FUNCTION FOR WHILE LOOPS **********************************************************************************
; ***********************************************************************************************************************

; While loop state function using continuation passing style
(define State_While
  (lambda (expression state return next classname throw)
    (letrec ((loop (lambda (condition body state next)
                     (if (M_Boolean condition state classname throw)
                         (Evaluate body state return
                                   (lambda (state) (next state)) ; break
                                   (lambda (state) (loop condition body state next)) ; continue
                                   (lambda (state) (loop condition body state next)) ; next
                                   classname throw)
                         (next state)))))
      (loop (condition expression) (body expression) state next))))

; ***********************************************************************************************************************
; ***** STATE FUNCTIONS FOR TRY/CATCH/FINALLY ***************************************************************************
; ***********************************************************************************************************************

; state function for try/catch/finally blocks
(define State_Exception_Handling
  (lambda (expression state return break continue next classname throw)
    (if (null? (finallyblock expression))
        (State_Try expression state return break continue next classname throw)
        (State_Try expression state
                   (lambda (v) (M_State_List (finallyblock expression) v return break continue next classname throw)); return
                   (lambda (v) (M_State_List (finallyblock expression) v return break continue next classname throw)); break
                   (lambda (v) (M_State_List (finallyblock expression) v return break continue next classname throw)); continue
                   (lambda (v) (M_State_List (finallyblock expression) v return break continue next classname throw)); next
                   classname
                   (lambda (v) (M_State_List (finallyblock expression) v return break continue next classname throw)))))) ; throw

; state function for executing code in the try block
(define State_Try
  (lambda (expression state return break continue next classname throw)
    (M_State_List (tryblock expression) (addlayer state) return
                  (lambda (v) (break (removelayer v)))
                  (lambda (v) (continue (removelayer v)))
                  (lambda (v) (next (removelayer v)))
                  classname
                  (lambda (v) ; throw continuation
                    (call/cc (lambda (throw) ; for nested throws
                               (M_State_List (catchbody expression)
                                             (addbinding (catchvar expression) v (addlayer state))
                                             return
                                             (lambda (v) (break (removelayer v)))
                                             (lambda (v) (continue (removelayer v)))
                                             (lambda (v) (next (removelayer v)))
                                             classname
                                             throw)))))))

; helper definitions for parsing try/catch/finally blocks
(define tryblock cadr)
(define catchblock caddr)

(define catchbody
  (lambda (expression)
    (caddr (catchblock expression))))

(define catchvar
  (lambda (expression)
    (caadr (catchblock expression))))

; defines finallyblock if it is present
(define finallyblock
  (lambda (expression)
    (if (null? (cadddr expression))
        '()
        (cadr (cadddr expression)))))

; ***********************************************************************************************************************
; ***** VALUE FUNCTIONS FOR EXPRESSION EVALUATION ***********************************************************************
; ***********************************************************************************************************************

; passes an expression to Value_Expr function for evaluation
(define M_Value
  (lambda (expression state classname throw)
    (cond
      ((number? expression) expression)
      ((eq? 'true expression) true)
      ((eq? 'false expression) false)
      (else (Value_Expr expression state classname throw)))))

; Function mostly for evaluating mathematical expressions
(define Value_Expr
  (lambda (expression state classname throw)
    (cond
      ((null? expression) 0)
      ((number? expression) expression)
      ((not (list? expression)) (classlookup classname expression state))
      ((and (eq? '- (operator expression)) (null? (cddr expression)))
       (- (Value_Expr (cadr expression) state classname throw))) ; handles unary (-) case
      ((eq? '+ (operator expression)) (+ (Value_Expr (leftterm expression) state classname throw)
                                         (Value_Expr (rightterm expression) state classname throw)))
      ((eq? '- (operator expression)) (- (Value_Expr (leftterm expression) state classname throw)
                                         (Value_Expr (rightterm expression) state classname throw)))
      ((eq? '* (operator expression)) (* (Value_Expr (leftterm expression) state classname throw)
                                         (Value_Expr (rightterm expression) state classname throw)))
      ((eq? '/ (operator expression)) (quotient (Value_Expr (leftterm expression) state classname throw)
                                                (Value_Expr (rightterm expression) state classname throw)))
      ((eq? '% (operator expression)) (modulo (Value_Expr (leftterm expression) state classname throw)
                                              (Value_Expr (rightterm expression) state classname throw)))
      
      ((and (eq? 'funcall (operator expression)) (list? (leftterm expression))) ; case of function being called through dot operator
       (State_Func_Call (car (cddadr expression)) (cddr expression) state (convertclass classname (cadadr expression) state) throw))
      ((eq? 'funcall (operator expression)) (State_Func_Call (functionname expression) (cddr expression) state classname throw))
      ((and (not (list? expression)) (number? (classlookup classname expression state))) (classlookup classname expression state)) ; if expression is a variable
      ((and (eq? 'dot (operator expression)) (eq? 'super (leftterm expression))) ; case of dot operator being used with 'super call
       (Value_Expr (classlookup (classlookup classname 'super state) (rightterm expression) state) state (classlookup classname 'super state) throw))
      ((eq? 'dot (operator expression)) (Value_Expr (classlookup (leftterm expression) (rightterm expression) state) state (leftterm expression) throw))
      (else (Boolean_Expr expression state classname throw)))))

; passes a boolean expression into Boolean_Expr for evaluation
(define M_Boolean
  (lambda (expression state classname throw)
    (Boolean_Expr expression state classname throw)))

; Evaluates expressions containing boolean and comparison operators
(define Boolean_Expr
  (lambda (expression state classname throw)
    (cond
      ((not (list? expression)) (classlookup classname expression state))
      ((or (eq? 'true expression) (eq? 'true (car expression))) true)
      ((or (eq? 'false expression) (eq? 'false (car expression))) false)
      
      ; boolean operators
      ((eq? '&& (operator expression)) (and (Value_Expr (leftterm expression) state classname throw)
                                            (Value_Expr (rightterm expression) state classname throw)))
      ((eq? '|| (operator expression)) (or (Value_Expr (leftterm expression) state classname throw)
                                           (Value_Expr (rightterm expression) state classname throw)))
      ((eq? '! (operator expression))  (not (Value_Expr (cadr expression) state classname throw)))
      
      ; comparison operators
      ((eq? '== (operator expression)) (equal? (Value_Expr (Value_Expr (leftterm expression) state classname throw) state classname throw)
                                               (Value_Expr (Value_Expr (rightterm expression) state classname throw) state classname throw)))
      ((eq? '!= (operator expression)) (not (equal? (Value_Expr (Value_Expr (leftterm expression) state classname throw) state classname throw)
                                                    (Value_Expr (Value_Expr (rightterm expression) state classname throw) state classname throw))))
      ((eq? '< (operator expression)) (< (Value_Expr (Value_Expr (leftterm expression) state classname throw) state classname throw)
                                         (Value_Expr (Value_Expr (rightterm expression) state classname throw) state classname throw)))
      ((eq? '> (operator expression)) (> (Value_Expr (Value_Expr (leftterm expression) state classname throw) state classname throw)
                                         (Value_Expr (Value_Expr (rightterm expression) state classname throw) state classname throw)))
      ((eq? '<= (operator expression)) (<= (Value_Expr (Value_Expr (leftterm expression) state classname throw) state classname throw)
                                           (Value_Expr (Value_Expr (rightterm expression) state classname throw) state classname throw)))
      ((eq? '>= (operator expression)) (>= (Value_Expr (Value_Expr (leftterm expression) state classname throw) state classname throw)
                                           (Value_Expr (Value_Expr (rightterm expression) state classname throw) state classname throw)))
      (else (error 'undefined-operator)))))

; abstraction definitions for value functions
(define operator car)

(define leftterm cadr)

(define rightterm caddr)

; ***********************************************************************************************************************
; ***** FUNCTIONS FOR MANIPULATION OF STATE/ENVIRONMENT *****************************************************************
; ***********************************************************************************************************************

; adds a binding in the state
(define addbinding
  (lambda (name value state)
    (cons (list (cons name (caar state)) (cons (box value) (cadar state))) (cdr state))))

; removes a binding from the state
(define removebinding
  (lambda (name value state next)
    (cond
      ((null? state) 'variable-out-of-scope)
      ((null? (caar state)) (removebinding name value (cdr state) (lambda (v) (next (cons '(()()) v)))))
      ((eq? name (caaar state)) (next (begin (set-box! (caadar state) value) state)))
      (else (removebinding name value
             (cons (list (cdaar state) (cdadar state)) (cdr state)) ; state
             (lambda (v) (next (cons (list (cons (caaar state) (caar v)) (cons (caadar state) (cadar v))) (cdr v))))))))) ; next

; remove function relating to a specific class
(define classremove
  (lambda (name value state classname)
    (if (eq? 'variable-out-of-scope (removebinding name value state (lambda (v) v)))
        (removebinding name value (lookup classname state) (lambda (v) v))
        (removebinding name value state (lambda (v) v))))) 

; adds a binding to "super" in the state
(define addsuper
  (lambda (variables state)
    (if (null? (caar variables))
        state
        (addsuper (list (list (cdaar variables) (cdadar variables)))
                  (list (list (cons (caaar variables) (caar state)) (cons (caadar variables) (cadar state))))))))

; looks up a variable in the state
(define lookup
  (lambda (name state)
    (cond
      ((or (eq? name 'true) (eq? name 'false) (eq? name #t) (eq? name #f) (number? name)) name)
      ((null? state) (error 'undeclared-variable))
      ((null? (caar state)) (lookup name (cdr state)))
      ((eq? name (caaar state)) (unbox (caadar state)))
      (else (lookup name (cons (list (cdaar state) (cdadar state)) (cdr state)))))))

; looks something up in a specific class
(define classlookup
  (lambda (classname name state)
    (if (not (instate? name state))
        (lookup name (lookup classname state))
        (lookup name state))))

; if newclass is a call to super, gets state of class it is referencing
(define convertclass
  (lambda (oldclass newclass state)
    (if (eq? 'super newclass)
        (classlookup oldclass newclass state)
        newclass)))

; checks whether or not a variable is in the state
(define instate?
  (lambda (name state)
    (cond
      ((or (eq? name 'true) (eq? name 'false) (number? name)) name)
      ((null? state) #f)
      ((null? (caar state)) (instate? name (cdr state)))
      ((eq? name (caaar state)) #t)
      (else (instate? name (cons (list (cdaar state) (cdadar state)) (cdr state)))))))

; the initial version of the state
(define newstate '((()())))

; adds a new layer to the state
(define addlayer
  (lambda (state)
    (cons '(()()) state)))

; removes a layer from the state
(define removelayer
  (lambda (state)
    (cdr state)))

;function that creates environment for a function call from the global environment
(define functionlayer
  (lambda (name state)
    (cond
      ((null? state) (addlayer state))
      ((null? (cdr state)) (addlayer state))
      ((not (instate? name (outerlayer state))) (addlayer state)) ; if function is not in the outer layer of the state
      (else (functionlayer name (cdr state))))))

; returns the layer of the state containing functions and global variables
(define outerlayer
  (lambda (state)
    (cond
      ((null? state) '())
      ((null? (cdr state)) state)
      (else (outerlayer (cdr state))))))

; ***********************************************************************************************************************
; ***** FUNCTIONS FOR INTERPRETING PARSE TREE ***************************************************************************
; ***********************************************************************************************************************

; function for passing in the parse tree
(define interpret
  (lambda (fileName classname)
    (call/cc
     (lambda (return)
       (M_State (M_State_Class (parser fileName) newstate return (lambda (v) v) (lambda (v) v)) ; state
                return                                                           ; return
                (lambda (v) v)                                                   ; break
                (lambda (v) v)                                                   ; continue
                (lambda (v) v)                                                   ; next
                (string->symbol classname)                                       ; classname
                (lambda (v) v))))))                                              ; throw

; initializes states at beginning of parse tree evaluation
(define M_State
  (lambda (state return break continue next classname throw)
    (M_State_List (cadr (classlookup classname 'main state)) (addlayer state)
                  (lambda (v)
                    (cond
                      ((eq? #t v) (return 'true)) ; interprets #t and #f tokens as 'true' and 'false'
                      ((eq? #f v) (return 'false))
                      (else (return v)))) ; return continuation
                  break continue next classname throw)))

; for setting up class definitions
(define M_State_Class
  (lambda (classes state return next throw)
    (cond
      ((null? classes) state)
      ((not (eq? (caar classes) 'class)) (error 'invalid-class-declaration))
      ((null? (caddar classes))
       (M_State_Class
        (cdr classes)
        (addbinding (cadar classes)
                    (M_State_List (car (cdddar classes)) newstate return (lambda (v) v) (lambda (v) v) next 'null throw)
                    state)
        return next throw))
      (else
       (M_State_Class
        (cdr classes)
        (addbinding (cadar classes)
                    (list (car (M_State_List
                                (car (cdddar classes)) ; list
                                (addsuper              ; state
                                 (lookup (cadr (caddar classes)) state)                        ; super variables
                                 (addbinding 'super (cadr (caddar classes)) (addlayer state))) ; super state
                                return                 ; return
                                (lambda (v) v)         ; break
                                (lambda (v) v)         ; continue
                                next 'null throw)))    ; next, classname, throw
                    state)
        return next throw)))))

; used to initialize states
(define M_State_List
  (lambda (list state return break continue next classname throw)
    (cond
      ((null? list) (next state))
      (else (Evaluate (car list) state return break continue
                      (lambda (v) (M_State_List (cdr list) v return break continue next classname throw)) ; next
                      classname throw)))))

; function that passes sublists of the parse tree to their appropriate state functions
(define Evaluate
  (lambda (expression state return break continue next classname throw)
    (cond
      ((null? (car expression)) (next state))
      ((or (eq? 'var (car expression)) (eq? 'static-var (car expression))) (next (State_Declare expression state classname throw)))
      ((eq? '= (car expression)) (next (State_Assign expression state classname throw)))
      ((eq? 'if (car expression)) (State_If expression state return break continue next classname throw))
      ((eq? 'while (car expression)) (State_While expression state return next classname throw))
      ((and (eq? 'begin (car expression)) (null? (cdr expression))) (next state))
      ((eq? 'begin (car expression)) (M_State_List (cdr expression) (addlayer state) return ; blocks of code
                                                   (lambda (v) (break (removelayer v)))
                                                   (lambda (v) (continue (removelayer v)))
                                                   (lambda (v) (next (removelayer v)))
                                                   classname
                                                   (lambda (v) (throw (removelayer v)))))
      ((or (eq? 'function (car expression)) (eq? 'static-function (car expression)))
       (next (State_Func_Def expression state)))
      ((and (eq? 'funcall (car expression)) (list? (cadr expression)))
       (begin (State_Func_Call (car (cddadr expression)) ; name
                               (cddr expression)         ; parameters
                               state                     ; state
                               (convertclass classname (cadadr expression) state) ; classname
                               throw)                    ; try
              (next state)))
      ((eq? 'funcall (car expression)) (begin (State_Func_Call (functionname expression) (cddr expression) state classname throw) (next state)))
      ((eq? 'return (car expression)) (return (M_Value (cadr expression) state classname throw)))
      ((eq? 'break (car expression)) (break state))
      ((eq? 'continue (car expression)) (continue state))
      
      ((eq? 'try (car expression)) (begin (State_Exception_Handling expression state return break continue next classname throw) (next state)))
      ((eq? 'throw (car expression)) (throw (M_Value (cadr expression) state classname throw)))
      
      (else (next state)))))
