; Jake Anderson, jta40
; Joseph Tate, jgt17
; Michael Volkovitsch, mtv25
; EECS 345, Project 4

(load "classParser.scm")

;state implemented as a list whose first element is the list of varnames, the second is the list of values

;parses and interprets the code in the given file
(define interpret
  (lambda (filename className)
    (evaluate (append (parser filename) (mainCall className)) (newEnvironment) className (lambda (v) v) (lambda (v) v) (lambda (v) v))))
;lambda (v) v as placeholders for continue, break, and return, acts as do nothing until in a loop or function.

;returns the command to trigger a call to the appropriate main
;"((return (funcall (dot <className> main))))"
(define mainCall
  (lambda (className)
    (list (cons 'return (list (cons 'funcall (list (cons 'dot (cons className '(main))))))))))

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

;evaluate the parse tree
(define evaluate
  (lambda (stmts state class continue break return)
    (cond
      ((not (list? state)) state)
      ((null? stmts) state)
      (else (evaluate (cdr stmts) (M_state (firststmt stmts) state class continue break return) state continue break return)))))


;returns the current statement (car of the statement list)
(define firststmt
  (lambda (stmts)
    (car stmts)))

;returns the type of a stmt (car of the stmt) ("=" is assignment)
(define stmttype
  (lambda (stmt)
    (car stmt)))

;Main M_state
;checks type of statement, passes it down to the correct Mstate handler
(define M_state
  (lambda (stmt state class continue break return)
    ((lambda (stmt state class stmttype)
       (display '_state:)
       (display stmt)
       (newline)
      (cond
        ((eq? stmttype 'var) (M_state_var stmt state class))
        ((eq? stmttype '=) (M_state_assign stmt state))
        ((eq? stmttype 'return) (M_state_return stmt state class return))
        ((eq? stmttype 'if) (M_state_if stmt state continue break return))
        ((eq? stmttype 'while) (M_state_while stmt state return))
        ((eq? stmttype 'begin) (M_state_block stmt state continue break return))
        ((eq? stmttype 'break) (M_state_break state break))
        ((eq? stmttype 'continue) (M_state_continue state continue))
        ((eq? stmttype 'function) (M_state_function_declaration stmt state))
        ((eq? stmttype 'funcall) (M_state_function_call stmt state))
        ((eq? stmttype 'class) (M_state_class stmt state))
        ((eq? stmttype 'static-var) (M_state_static_var stmt state class))
        ((eq? stmttype 'static-function) (M_state_static_function_declaration stmt state))
        ;((eq? stmttype 'static-function) (M_state_static_function_call stmt state))
        (else (error 'Invalid_stmt_type stmt))))
      stmt state class (stmttype stmt))))

;M_state_var
(define M_state_var
  (lambda (stmt state class)
    (cond
      ((and (isdeclaredinlayer? (cadr stmt) (topLayer state)) (null? (cddr stmt))) state)
      ((isdeclaredinlayer? (cadr stmt) (topLayer state)) (error 'Redefining_Variable))
      ((null? (cddr stmt)) (addvar (cadr stmt) '() state))
      (else (addvar (cadr stmt) (M_value (caddr stmt) state class) state)))))

;M_state_return
;checks if it's a boolean statement or number statement and returns the correct evaluation of the statement
(define M_state_return
  (lambda (exp s class return)
    (return (cond
              ((null? exp) '())
              (else ((lambda (v)
                       (if (boolean? v)
                           (boolReturnHelper v)
                           v))
                     (M_value (cadr exp) s class)))))))

; handles returning true and false instead of #t and #f
(define boolReturnHelper
  (lambda (bool)
    (if bool
      'true
      'false)))

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
    
;returns the value assigned to varname in the state
(define M_value_var
  (lambda (varname state class)
    (display '_val_var)
    (newline)
    (display varname)
    (newline)
    (display state)
    (newline)
    (cond 
      ((and (null? state) (null? class)) (error 'Variable/function_not_declared))
      ((and (null? (cdr state)) (not (null? class))) (M_value_var varname (M_state_dot (cons 'dot (cons class (list varname))) state '())))
      ((list? varname) (M_value_dot varname state class))
      (else
        ((lambda (varval)
          (if (null? varval)
              (M_value_var varname (removeLayer state) class)
              varval))
         (M_value_var_layer varname (topLayer state)))))))

;helper method for M_value_var, returns the value assigned to varname in a layer, or null if the variable does not exist in that layer
(define M_value_var_layer
  (lambda (varname layer)
    (cond
      ((null? (vars layer)) '())
      ((and (eq? varname (firstvarname layer)) (null? (firstvarvalue layer))) (error 'Variable_not_initialized))
      ((eq? varname (firstvarname layer)) (firstvarvalue layer))
      (else (M_value_var_layer varname (trimlayer layer))))))

;trims the first var entry from the layer
(define trimlayer
  (lambda (layer)
     (cons (cdr (car layer)) (cons (cdr (cadr layer)) '()))))

(define firstvarname
  (lambda (layer)
    (car (car layer))))

(define firstvarvalue
  (lambda (layer)
    (unbox (car (cadr layer)))))

; M_value, handles +,-,*,/,% and calls M_value_var if it finds a variable
(define M_value
  (lambda (expression state class)
    (display expression)
    (cond
      ((number? expression) expression)
      ((or (eq? expression '#t) (eq? expression '#f)) (M_bool expression state))
      ((or (eq? expression 'true) (eq? expression 'false)) (M_bool expression state))
      ((not (list? expression)) (M_value_var expression state class))
      ((eq? '+ (operator expression)) (+ (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((eq? '% (operator expression)) (remainder (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((and (eq? '- (operator expression)) (null? (cddr expression))) (- 0 (M_value (lOperand expression) state)))
      ((eq? '- (operator expression)) (- (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((eq? '* (operator expression)) (* (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((eq? 'funcall (operator expression)) (M_value_function_call expression state class))
      ((eq? 'dot (operator expression)) (M_value_dot expression state))
      (else (M_bool expression state)))))

; M_boolean, handles conditionals and equality ==, !=, <, >, <=, >=
(define M_bool
  (lambda (expression state)
    (cond
      ((eq? expression '#t) #t)
      ((eq? expression '#f) #f)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((boolean? expression) expression)
      ((number? expression) '(not_a_bool)) 
      ((not (list? expression)) (M_value_var expression state))
      ((eq? '== (operator expression)) (eq? (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (M_value (lOperand expression) state) (M_value (rOperand expression) state))))
      ((eq? '< (operator expression)) (< (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((eq? '> (operator expression)) (> (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((eq? '<= (operator expression)) (or (eq? (M_value (lOperand expression) state) (M_value (rOperand expression) state)) (< (M_value (lOperand expression) state) (M_value (rOperand expression) state))))
      ((eq? '>= (operator expression)) (or (eq? (M_value (lOperand expression) state) (M_value (rOperand expression) state)) (> (M_value (lOperand expression) state) (M_value (rOperand expression) state))))
      ((eq? '&& (operator expression)) (and (M_bool (lOperand expression) state) (M_bool (rOperand expression) state)))
      ((eq? '|| (operator expression)) (or (M_bool (lOperand expression) state) (M_bool (rOperand expression) state)))
      ((eq? '! (operator expression)) (not (M_bool (lOperand expression) state)))
      ((eq? 'funcall (operator expression)) (M_value_function_call expression state))
      ((eq? 'dot (operator expression)) (M_value_dot expression state))
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

;M_state_if
(define M_state_if
  (lambda (ifBlock state continue break return)
    (cond
      ((M_bool (condition ifBlock) state) (M_state (ifStmt ifBlock) state continue break return))
      ((noElseStmt ifBlock) state)
      (else (M_state (elseStmt ifBlock) state continue break return)))))

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

;M_state_assign
(define M_state_assign
  (lambda (assignment state)
    (if (isdeclared? (varName assignment) state)
      (updatevar (varName assignment) (M_value (expr assignment) state) state)
      (error 'Variable/Function_not_declared))))

; misc definitions for M_state_assign
(define varName
  (lambda (l)
    (cadr l)))
(define expr
  (lambda (l)
    (caddr l)))

;M_State_while, handles the while loop with continues and breaks.
(define M_state_while
  (lambda (while state return)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (condition body state)
                                (if (M_bool condition state)
                                    (loop condition body (call/cc (lambda (continue) (M_state body state continue break return))))
                                    state))))
                 (loop (condition while) (body while) state))))))

;misc while helper functions
(define condition
  (lambda (while)
    (cadr while)))

(define body
  (lambda (while)
    (caddr while)))

;M_state_block, handles block statements
(define M_state_block
  (lambda (stmt state continue break return)
    (removeLayer (evaluate (cdr stmt) (addLayer state) (lambda (v) (continue (removeLayer v))) (lambda (v) (break (removeLayer v))) return))))

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

; create_func_envi
; creates the environment for functions to run in
(define create_func_envi
  (lambda (name values state class)
    (display name)
    (newline)
    (display values)
    (newline)
    (display state)
    (newline)
    (cond
      ((list? name) (create_func_envi (caddr name) values (addvar (caddr name) (M_value_dot name state class) state) class))
      ((isdeclaredinlayer? name (car state)) (addParams (func_param_names (M_value_var name state class)) values (addLayer (cons (pruneLayer name (car state)) (removeLayer state)))))
      (else (create_func_envi name values (removeLayer state))))))

; create_func_envi helpers
; removes all variables in a layer before the one with the given name
(define pruneLayer
  (lambda (name layer)
    (cond
      ((null? (vars layer)) '())
      ((eq? name (firstvarname layer)) layer)
      (else   (pruneLayer name (trimlayer layer))))))
;adds the function parameters to the function state
(define addParams
  (lambda (names values state)
    (cond
      ((null? names) state)
      (else (addParams (cdr names) (cdr values) (addvar (car names) (M_value (car values) state) state))))))
        
; M_state_function_declaration
; creates the function closure and adds it to the state
(define M_state_function_declaration
  (lambda (funcDef state)
    (addvar (func_name funcDef) (append (list (func_params funcDef)) (list (func_code funcDef))) state)))

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

; M_value_function_call
; Returns the value of a function call
(define M_value_function_call
  (lambda (funcCall state class)
    (display '_func_call)
             (newline)
    (display funcCall)
    (newline)
    (cond
      ((not (= (length (car (M_value_var (func_name funcCall) state class))) (length (func_param_values funcCall)))) (error 'Function_argument_mismatch))
      ((list? (func_name funcCall)) (call/cc (lambda (return) (evaluate (func_code_list (M_value_dot (func_name funcCall) state class)) (create_func_envi (func_name funcCall) (param_values (func_param_values funcCall) state) state class) class (lambda (v) v) (lambda (v) v) return))))
      (else (call/cc (lambda (return) (evaluate (func_code_list (M_value_var (func_name funcCall) state)) (create_func_envi (func_name funcCall) (param_values (func_param_values funcCall) state) state) (lambda (v) v) (lambda (v) v) return)))))))

; param_values
; calculates the parameter values for function calls
(define param_values
  (lambda (params state)
    (cond
      ((null? params) '())
      (else (cons (M_value (car params) state) (param_values (cdr params) state))))))

; M_state_function_call
; Calls a function to change the state
(define M_state_function_call
  (lambda (funcCall state)
    (append (evaluate (func_code_list (M_value_var (func_name funcCall) state)) (create_func_envi (func_name funcCall) (param_values (func_param_values funcCall) state) state) (lambda (v) v) (lambda (v) v) (lambda (v) state)) (cdr state))))

;M_state_class
;adds a class definition to the state
(define M_state_class
  (lambda (class state)
    (addvar (className class) (classEniv class) state)))

;helper functions for parts of the class
(define className
  (lambda (class)
    (cadr class)))

(define superClass
  (lambda (class)
    (caddr class)))

(define classBody
  (lambda (class)
    (cadddr class)))

;makes the enivronment of the class
(define classEniv
  (lambda (class)
    (evaluate (classBody class) (addvar 'super (superClass class) (newEnvironment)) class (lambda (v) v) (lambda (v) v) (lambda (v) v))))

;M_state_dot
;evaluates the dot expression
(define M_state_dot
  (lambda (dot state class)
    (display '_dot)
    (newline)
    (display (cadr dot))
    (newline)
    (display (M_value_var (cadr dot) state class))
    (newline)
    (M_state (caddr dot) (append (M_value_var (cadr dot) state) state))))
     
;M_value_dot
(define M_value_dot
  (lambda (dot state class)
    (display '_dot_val)
    (newline)
    (display dot)
    (newline)
    (display (M_value_var (cadr dot) state class))
    (newline)
    (display (caddr dot))
    (newline)
    (display (append (M_value_var (cadr dot) state class) state))
    (newline)
    (M_value (caddr dot) (append (M_value_var (cadr dot) state class) state) class)))

;M_state_static_function
(define M_state_static_function_declaration
  (lambda (funcDef state)
    (M_state_function_declaration funcDef state)))

;M_state_static_var
(define M_state_static_var
  (lambda (stmt state class)
    (M_state_var stmt state class)))