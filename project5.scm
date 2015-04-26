; Jake Anderson, jta40
; Joseph Tate, jgt17
; Michael Volkovitsch, mtv25
; EECS 345, Project 5

(load "classParser.scm")

;***************************************************************************************************************************************
;PARSE, INTERPRET, AND EVALUATE
;***************************************************************************************************************************************
; state ( [function/if/while blocks [classname='()] (className (var names)(var values [may be a state itself]) (functionNames) (funcDefs)) [super classes...] ) runtime
; classes ((className superClassName (var names) (initial var values) (function names) (func defs) (static var names) (static var values) (static function names) (static function defs) [more classes])

;parses and interprets the code in the given file
(define interpret
  (lambda (filename className)
    (evaluate (append (parser filename) (mainCall (string->symbol className))) (newEnvironment) (string->symbol className) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v))))
;lambda (v) v as placeholders for continue, break, and return, acts as do nothing until in a loop or function.

;evaluate the parse tree
(define evaluate
  (lambda (stmts state class continue break return exception)
    (cond
      ((not (list? state)) state)
      ((null? stmts) state)
      (else (evaluate (cdr stmts) (M_state (firststmt stmts) state class continue break return exception) class continue break return exception)))))

;returns the command to trigger a call to the appropriate main
;"((return (funcall (dot <className> main))))"
(define mainCall
  (lambda (className)
    (list (cons 'return (list (cons 'funcall (list (cons 'dot (cons className '(main))))))))))

;***************************************************************************************************************************************
;STATE AND ENVIRONMENT MANIPULATION
;***************************************************************************************************************************************

;defines the newEnvironment consisting of 1 layer in a list
(define newEnvironment
  (lambda ()
    (cons (newLayer) '())
    ))

;defines a new layer
(define newLayer
  (lambda ()
    '(()())
    ))

;adds a layer to the state
(define addLayer
  (lambda (state)
    (cons (newLayer) state)))

;removes a layer from the state
(define removeLayer
  (lambda (state)
    (cdr state)))

;returns the top layer of a state
(define topLayer
  (lambda (state)
    (car state)))

;trims the first var entry from the layer
(define trimlayer
  (lambda (layer)
     (cons (cdr (car layer)) (cons (cdr (cadr layer)) '()))))

; removes all variables in a layer before the one with the given name
(define pruneLayer
  (lambda (name layer)
    (cond
      ((null? (vars layer)) '())
      ((eq? name (firstvarname layer)) layer)
      (else   (pruneLayer name (trimlayer layer))))))

;***************************************************************************************************************************************
; FUNCTION ENVIRONMENT
;***************************************************************************************************************************************
; create_func_envi
; creates the environment for functions to run in
(define create_func_envi
  (lambda (name values state class exception)
    (cond
      ((list? name) (create_func_envi (caddr name) values (addvar (caddr name) (M_value_dot name state class exception) state) class exception))
      ((isdeclaredinlayer? name (car state)) (addParams (func_param_names (M_value_var name state class exception)) values (addLayer (cons (pruneLayer name (car state)) (removeLayer state))) class exception))
      (else (create_func_envi name values (removeLayer state) class exception)))))

; create_func_envi helpers

;adds the function parameters to the function state
(define addParams
  (lambda (names values state class exception)
    (cond
      ((null? names) state)
      (else (addParams (cdr names) (cdr values) (addvar (car names) (M_value (car values) state class exception) state) class exception)))))

;helper functions for functions
;both def and call
(define func_name
  (lambda (func)
    (cadr func)))

;def only
(define func_params
  (lambda (funcDef)
    (caddr funcDef)))

;def only
(define func_code
  (lambda (funcDef)
    (cadddr funcDef)))

;call only
(define func_param_values
  (lambda (funcCall)
    (cddr funcCall)))

;closure only
(define func_param_names
  (lambda (funcClosure)
    (car funcClosure)))

;closure only
(define func_code_list
  (lambda (funcClosure)
    (cadr funcClosure)))

;***************************************************************************************************************************************
; M_STATE
;***************************************************************************************************************************************
;Main M_state
;checks type of statement, passes it down to the correct Mstate handler
(define M_state
  (lambda (stmt state class continue break return exception)
    ((lambda (stmt state class stmttype)
      (cond
        ((eq? stmttype 'var) (M_state_var stmt state class exception))
        ((eq? stmttype '=) (M_state_assign stmt state class exception))
        ((eq? stmttype 'return) (M_state_return stmt state class return exception))
        ((eq? stmttype 'if) (M_state_if stmt state class continue break return exception))
        ((eq? stmttype 'while) (M_state_while stmt state class return exception))
        ((eq? stmttype 'begin) (M_state_block stmt state class continue break return exception))
        ((eq? stmttype 'break) (M_state_break state break))
        ((eq? stmttype 'continue) (M_state_continue state continue))
        ((eq? stmttype 'function) (M_state_function_declaration stmt state))
        ((eq? stmttype 'funcall) (M_state_function_call stmt state class exception))
        ((eq? stmttype 'class) (M_state_class stmt state))
        ((eq? stmttype 'static-var) (M_state_static_var stmt state class exception))
        ((eq? stmttype 'static-function) (M_state_static_function_declaration stmt state))
        ((eq? stmttype 'try) (M_state_try stmt state class continue break return exception))
        ((eq? stmttype 'throw) (M_state_throw stmt state class exception))
        (else (error 'Invalid_stmt_type stmt))))
      stmt state class (stmttype stmt))))

;M_state_return
;checks if it's a boolean statement or number statement and returns the correct evaluation of the statement
(define M_state_return
  (lambda (exp s class return exception)
    (return (cond
              ((null? exp) '())
              (else ((lambda (v)
                       (if (boolean? v)
                           (boolReturnHelper v)
                           v))
                     (M_value (cadr exp) s class exception)))))))


;returns the current statement (car of the statement list)
(define firststmt
  (lambda (stmts)
    (car stmts)))

;returns the type of a stmt (car of the stmt) ("=" is assignment)
(define stmttype
  (lambda (stmt)
    (car stmt)))

; handles returning true and false instead of #t and #f in M_state_return
(define boolReturnHelper
  (lambda (bool)
    (if bool
      'true
      'false)))


;***************************************************************************************************************************************
; M_STATE_BLOCK, CONTINUE, BREAK
;***************************************************************************************************************************************
;M_state_block, handles block statements
(define M_state_block
  (lambda (stmt state class continue break return exception)
    (removeLayer (evaluate (cdr stmt) (addLayer state) class (lambda (v) (continue (removeLayer v))) (lambda (v) (break (removeLayer v))) return exception))))

;M_state_break, handles break
; the break that is passed in is a continuation function from the call/cc on line 287 int M_state_while,
; modified by M_state_block calls so that it removes the layer(s) before triggering the continuation.
(define M_state_break
  (lambda (state break)
    (break state)))

;M_state_continue, handles continue
; the continue that is passed in is a continuation function from the call/cc on line 290 in M_state_while,
; modified by M_state_block calls so that it removes the layer(s) before triggering the continuation.
(define M_state_continue
  (lambda (state continue)
    (continue state))) 

;***************************************************************************************************************************************
; M_STATE_IF
;***************************************************************************************************************************************
(define M_state_if
  (lambda (ifBlock state class continue break return exception)
    (cond
      ((M_bool (condition ifBlock) state class exception) (M_state (ifStmt ifBlock) state class continue break return exception))
      ((noElseStmt ifBlock) state)
      (else (M_state (elseStmt ifBlock) state class continue break return exception)))))

; misc definitions for M_state_if
(define condition
  (lambda (l)
    (cadr l)))

(define ifStmt
  (lambda (l)
    (caddr l)))

(define elseStmt
  (lambda (l)
    (cadddr l)))

(define noElseStmt
  (lambda (l)
    (null? (cdddr l))))
;***************************************************************************************************************************************
; M_STATE_ASSIGN
;***************************************************************************************************************************************
(define M_state_assign
  (lambda (assignment state class exception)
    (if (isdeclared? (varName assignment) state)
      (updatevar (varName assignment) (M_value (expr assignment) state class exception) state)
      (error 'Variable/Function_not_declared))))

; misc definitions for M_state_assign
(define varName
  (lambda (l)
    (cadr l)))
(define expr
  (lambda (l)
    (caddr l)))

;***************************************************************************************************************************************
; M_STATE_VAR (for checking declaration and declaring)
;***************************************************************************************************************************************

;M_state_var
(define M_state_var
  (lambda (stmt state class exception)
    (cond
      ((and (isdeclaredinlayer? (cadr stmt) (topLayer state)) (null? (cddr stmt))) state)
      ((isdeclaredinlayer? (cadr stmt) (topLayer state)) (error 'Redefining_Variable))
      ((null? (cddr stmt)) (addvar (cadr stmt) '() state))
      (else (addvar (cadr stmt) (M_value (caddr stmt) state class exception) state)))))

;addvar adds a var and it's initial value ('() if undefined) to state at the top level
(define addvar
  (lambda (var val state)
    (cons (cons (cons var (vars (topLayer state))) (cons (cons (box val) (vals (topLayer state))) '())) (cdr state))))

;add var to specific layer for when someone is reassigning a value
(define addvarlayer
  (lambda (var val layer)
    (cons (cons var (vars layer)) (cons (cons (box val) (vals layer)) '()))))

;updates the value of a var
(define updatevar
  (lambda (var val state)
    (updatevar2 var val state state)))

(define updatevar2
  (lambda (var val state originalState)
    (if (isdeclaredinlayer? var (topLayer state))
        (begin (set-box! (car (vals (pruneLayer var (topLayer state)))) val) originalState)
        (updatevar2 var val (removeLayer state) originalState))))

; helper method to update a variable in a specific layer
(define updatevarlayer
  (lambda (var val layer)
    (addvarlayer var val (removevarlayer var layer))))

(define vars
  (lambda (layer)
    (car layer)))
(define vals
  (lambda (layer)
    (cadr layer)))

;removevar from a layer, if present
(define removevarlayer
  (lambda (var layer)
    (cond
      ((null? (car layer)) '())
      ((eq? var (firstvarname layer)) (trimlayer layer))
      (else (addvarlayer (firstvarname layer) (firstvarvalue layer) (removevarlayer var (trimlayer layer)))))))

;checks if a var is declared in the state
(define isdeclared?
  (lambda (varname state)
    (if (null? state)
        #f
        ((lambda (varval)
          (if varval
              varval
              (isdeclared? varname (removeLayer state))))
         (isdeclaredinlayer? varname (topLayer state))))))
        
;checks if a var is declared in a layer
(define isdeclaredinlayer?
  (lambda (varname layer)
    (cond
      ((null? (car layer)) #f)
      ((eq? varname (firstvarname layer)) #t)
      (else (isdeclaredinlayer? varname (trimlayer layer))))))

;***************************************************************************************************************************************
; M_STATE_WHILE
;***************************************************************************************************************************************
;M_State_while, handles the while loop with continues and breaks.
(define M_state_while
  (lambda (while state class return exception)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (condition body state)
                                (if (M_bool condition state class exception)
                                    (loop condition body (call/cc (lambda (continue) (M_state body state class continue break return exception))))
                                    state))))
                 (loop (condition while) (body while) state))))))

;misc while helper functions
(define condition
  (lambda (while)
    (cadr while)))

(define body
  (lambda (while)
    (caddr while)))

;***************************************************************************************************************************************
; M_STATE_FUNCTION
;***************************************************************************************************************************************
; M_state_function_declaration
; creates the function closure and adds it to the state
(define M_state_function_declaration
  (lambda (funcDef state)
    (addvar (func_name funcDef) (append (list (func_params funcDef)) (list (func_code funcDef))) state)))

; M_state_function_call
; Calls a function to change the state
(define M_state_function_call
  (lambda (funcCall state class exception)
    (append (evaluate (func_code_list (M_value_var (func_name funcCall) state class exception)) (create_func_envi (func_name funcCall) (param_values (func_param_values funcCall) state class exception) state class exception) (lambda (v) v) (lambda (v) v) (lambda (v) state) class exception) (cdr state))))

;***************************************************************************************************************************************
; M_STATE_CLASS
;***************************************************************************************************************************************
;M_state_class
;adds a class definition to the state
(define M_state_class
  (lambda (class state)
    (addvar (className class) (classEniv class) state)))

;helper functions for parts of the class
(define className
  (lambda (class)
    (cadr class)))

;gets the superClass of a class from the class definition
(define superClass
  (lambda (class)
    ((lambda (super)
       (if (null? super)
           'none
           (cadr super)))
    (caddr class))))

;gets the body of a class from the class definition
(define classBody
  (lambda (class)
    (cadddr class)))

;makes the enivronment of the class
(define classEniv
  (lambda (class)
    (evaluate (classBody class) (addvar 'super (superClass class) (addLayer (newEnvironment))) class (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v))))

;M_state_dot
;evaluates the dot expression
(define M_state_dot
  (lambda (dot state class)
    (M_state (caddr dot) (append (M_value_var (cadr dot) state class) state) class)))

;M_state_static_function
(define M_state_static_function_declaration
  (lambda (funcDef state)
    (M_state_function_declaration funcDef state)))

;M_state_static_var
(define M_state_static_var
  (lambda (stmt state class exception)
    (M_state_var stmt state class exception)))


;***************************************************************************************************************************************
; M_STATE_TRY, CATCH, FINALLY, EXCEPTION
;***************************************************************************************************************************************
;M_state_try
;M_state function for try constructs
(define M_state_try
  (lambda (try state class continue break return oldException)
    ((lambda (try catch finally)
      (cond
        ((and (null? finally) (null? catch)) (call/cc (lambda (exception) (M_state try state class continue break return exception))))
        ((and (null? finally) (not (null? catch))) (call/cc (lambda (exception) (M_state try state class continue break return (lambda (e)
                                                                                                                                 (exception
                                                                                                                                  (M_state_catch e catch state class continue break return oldException)))))))
        ((and (not (null? finally)) (null? catch)) (M_state finally (call/cc (lambda (exception) (M_state try state class continue break return exception))) class continue break return oldExCeption))
        ((and (not (null? finally)) (not (null? catch))) (call/cc (lambda (exception) (M_state finally (M_state try state class continue break return (lambda (e)
                                                                                                                                                        (exception
                                                                                                                                                         (M_state finally (M_state_catch e catch state class continue break return oldException) class continue break return oldException))))
                                                                                               class continue break return oldException))))))
     (tryBlock try) (catchBlock try) (finallyBlock try))))

;helpers for try
(define tryBlock
  (lambda (try)
    (cons 'begin (cadr try))))

(define catchBlock
  (lambda try
    (if (null? (caddar try))
        '()
        (cons 'begin (caddr(caddar try))))))
    
(define finallyBlock
  (lambda (try)
    (if (null? (cadr(cddr try)))
        '()
         (cons 'begin (cadar(cdddr try))))))

;M_state_catch
;the M_state function for evaluating catch blocks
;ex is the value of the exception
(define M_state_catch
  (lambda (ex catch state class continue break return exception)
    (removeLayer (M_state catch (addvar 'e ex (addLayer state)) class continue break return exception))))

;M_state_throw
;handles the throw statement by calling the exception continuation
(define M_state_throw
  (lambda (e state class exception)
    (exception (M_value (cadr e) state class (lambda (v) v)))))

;***************************************************************************************************************************************
; M_VALUE, M_BOOL
;***************************************************************************************************************************************
; M_value, handles +,-,*,/,% and calls M_value_var if it finds a variable
(define M_value
  (lambda (expression state class exception)
    (cond
      ((number? expression) expression)
      ((or (eq? expression '#t) (eq? expression '#f)) (M_bool expression state class exception))
      ((or (eq? expression 'true) (eq? expression 'false)) (M_bool expression state class exception))
      ((not (list? expression)) (M_value_var expression state class exception))
      ((eq? '+ (operator expression)) (+ (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception)))
      ((eq? '/ (operator expression)) (quotient (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception)))
      ((eq? '% (operator expression)) (remainder (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception)))
      ((and (eq? '- (operator expression)) (null? (cddr expression))) (- 0 (M_value (lOperand expression) state class exception)))
      ((eq? '- (operator expression)) (- (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception)))
      ((eq? '* (operator expression)) (* (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception)))
      ((eq? 'funcall (operator expression)) (get_function expression state class exception))
      ((eq? 'dot (operator expression)) (M_value_dot expression state class exception))
      (else (M_bool expression state class exception)))))

; M_boolean, handles conditionals and equality ==, !=, <, >, <=, >=
(define M_bool
  (lambda (expression state class exception)
    (cond
      ((eq? expression '#t) #t)
      ((eq? expression '#f) #f)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((boolean? expression) expression)
      ((number? expression) '(not_a_bool)) 
      ((not (list? expression)) (M_value_var expression state class exception))
      ((eq? '== (operator expression)) (eq? (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception)))
      ((eq? '!= (operator expression)) (not (eq? (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception))))
      ((eq? '< (operator expression)) (< (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception)))
      ((eq? '> (operator expression)) (> (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception)))
      ((eq? '<= (operator expression)) (or (eq? (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception)) (< (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception))))
      ((eq? '>= (operator expression)) (or (eq? (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception)) (> (M_value (lOperand expression) state class exception) (M_value (rOperand expression) state class exception))))
      ((eq? '&& (operator expression)) (and (M_bool (lOperand expression) state class exception) (M_bool (rOperand expression) state class exception)))
      ((eq? '|| (operator expression)) (or (M_bool (lOperand expression) state class exception) (M_bool (rOperand expression) state class exception)))
      ((eq? '! (operator expression)) (not (M_bool (lOperand expression) state class exception)))
      ((eq? 'funcall (operator expression)) (M_value_function_call expression state class exception))
      ((eq? 'dot (operator expression)) (M_value_dot expression state exception))
      (else '(not_a_bool)))))
          
; misc definitions for M_value, M_bool
(define operator
  (lambda (l)
  (car l)))
(define lOperand
  (lambda (l)
  (cadr l)))
(define rOperand
  (lambda (l)
  (caddr l)))

;***************************************************************************************************************************************
; M_VALUE_VAR
;***************************************************************************************************************************************
;returns the value assigned to varname in the state
(define M_value_var
  (lambda (varname state class exception)
    (cond 
      ((or (null? state) (null? class)) (error 'Variable/function_not_declared_in_scope))
      ((and (list? varname) (eq? 'dot (car varname))) (M_value_dot varname state class exception))
      (else
       ((lambda (varval2)
          (if(null? varval2)
             (error 'Variable/function_not_declared_in_scope)
             varval2))
        ((lambda (varval)
           (if (null? varval)
              (if (and (null? (cdr state)) (not (eq? varname class)))
                  (M_value_var_class varname state class exception)
                  (M_value_var varname (removeLayer state) class exception))
              varval))
         (M_value_var_layer varname (topLayer state))))))))

;helper method for M_value_var, returns the value assigned to varname in a layer, or null if the variable does not exist in that layer
(define M_value_var_layer
  (lambda (varname layer)
    (cond
      ((null? (vars layer)) '())
      ((and (eq? varname (firstvarname layer)) (null? (firstvarvalue layer))) (error 'Variable_not_initialized))
      ((eq? varname (firstvarname layer)) (firstvarvalue layer))
      (else (M_value_var_layer varname (trimlayer layer))))))

(define firstvarname
  (lambda (layer)
    (car (car layer))))

(define firstvarvalue
  (lambda (layer)
    (unbox (car (cadr layer)))))

;***************************************************************************************************************************************
; M_VALUE_FUNCTION
;***************************************************************************************************************************************
; M_value_function_call
; Returns the value of a function call
(define M_value_function_call
  (lambda (funcCall state class exception)
    (cond
      ((not (= (length (car (M_value_var (func_name funcCall) state class exception))) (length (func_param_values funcCall)))) (error 'Function_argument_mismatch))
      ((list? (func_name funcCall)) (call/cc (lambda (return) (evaluate (func_code_list (M_value_dot (func_name funcCall) state class exception)) (create_func_envi (func_name funcCall) (param_values (func_param_values funcCall) state class exception) state class exception) class (lambda (v) v) (lambda (v) v) return exception))))
      (else (call/cc (lambda (return) (evaluate (func_code_list (M_value_var (func_name funcCall) state class exception)) (create_func_envi (func_name funcCall) (param_values (func_param_values funcCall) state class exception) state class exception) (lambda (v) v) (lambda (v) v) return exception)))))))

;find the function definition and makes a function call
(define get_function
  (lambda (funcCall state class exception)
    (if (list? (func_name funcCall));if it's a dot call, make the call, else make a dot call after finding the definition
        (M_value_function_call funcCall state class exception)
        (M_value_function_call (append (list 'funcall (list 'dot (find_defining_class (func_name funcCall) state class exception) (func_name funcCall))) (func_param_values funcCall)) state class exception))))

;finds the class that the function is defined in.  May be the current class or a superclass of the current class
(define find_defining_class
  (lambda (name state class exception)
    (cond
      ((null? class) (error 'could_not_find_function))
      ((isdeclared? name (M_value_var class state class exception)) class)
      (else (find_defining_class name state (M_value_var 'super (M_value_var class state class exception) class exception) exception)))))
        

; param_values
; calculates the parameter values for function calls
(define param_values
  (lambda (params state class exception)
    (cond
      ((null? params) '())
      (else (cons (M_value (car params) state class exception) (param_values (cdr params) state class exception))))))

;***************************************************************************************************************************************
; M_VALUE_CLASS
;***************************************************************************************************************************************
;M_value_dot
(define M_value_dot
  (lambda (dot state class exception)
    (if (eq? 'super (cadr dot))
         (M_value (caddr dot) (M_value_var (M_value_var 'super (M_value_var class state class exception) class exception) state class exception) class exception)
         (M_value (caddr dot) (M_value_var (cadr dot) state class exception) class exception))))

;M_value_var_class
(define M_value_var_class
  (lambda (varname state class exception)
    ((lambda (classEnvi)
      (if (isdeclared? varname classEnvi)
        (M_value_var varname classEnvi class exception)
        ((lambda (super)
          (if (eq? super 'none)
            '()
            (M_value_var_class varname state super exception)))
         (M_value_var 'super classEnvi class exception))))
      (M_value_var class state class exception))))
