(load "classParser.scm")

;Interpreter part Four!


;Main interpret function
(define interpret
  (lambda (file class)
    (execute_function main_function_name '() (get_class_state (parseInputClassName class) (buildglobalstatelayer (parser file) initialstate)) null)))

(define main_function_name 'main)

(define parseInputClassName
  (lambda (name)
    (cond
      ((string? name) (string->symbol name))
      (else name))))

;first pass function for the entire program
(define buildglobalstatelayer
  (lambda (statements state)
    (cond
      ((null? statements) state)
      (else (buildglobalstatelayer (cdr statements) (mstate_class_decl (car statements) state))))))
     
;first pass function for a class
(define buildclassglobalstatelayer
  (lambda (statements state)
    (cond
      ((null? statements) state)
      (else (buildclassglobalstatelayer (cdr statements) (mstate_declarestatement (car statements) state null))))))
           
    

;the initial value of the state
(define initialstate '(()))

(define execute_function
  (lambda (functionname params state throw)
    (cond
      ((not (list? functionname))(execute_function_with_closure (getclosureforfunction functionname state) params state throw))
      ((eq? 'dot (operator functionname)) (execute_function (parsedot functionname state throw) params (get_state_for_funcall functionname state) throw))
      (else (execute_function_with_closure functionname params state throw)))))
    
(define execute_function_with_closure
  (lambda (closure params state throw)
    (call/cc
     (lambda (return)
       (processlist (closure_body closure) ((closure_statefunc closure) params state) return throw)))))

(define parsedot
  (lambda (expression state throw)
    (cond
      ((not (list? expression)) expression)
      ((not (eq? 'dot (operator expression))) expression)
      (else (mvalue_expression (dot_varname expression) (get_class_state (dot_classname expression) state) throw)))))

(define get_state_for_funcall
  (lambda (functionexpr state)
    (cond
      ((not (list? functionexpr)) state);this function should execute in the current state
      ((eq? 'dot (operator functionexpr)) (append (cons (dot_classname functionexpr) (get_state_for_funcall (dot_classname functionexpr) (get_class_state (dot_classname functionexpr) state))) state))
      (else state))))

     
(define dot_classname cadr)
(define dot_varname caddr)

;this function is the main 'loop' that processes the code
(define processlist
  (lambda (statements state return throw)
    (cond
      ((null? statements) (return));... return was never called?
      (else (processlist (cdr statements) (mstate_statement (car statements) state return null null throw) return throw)))))

;mvalue for return statements; cleans up values for returning
(define mvalue_return
  (lambda (expr state return throw)
    (cond
      ((boolean? expr) (return (convertbool expr)))
      ((number? expr) (return expr))
      ((or (eq? expr 'true) (eq? expr 'false)) (return expr))
      ((and (list? expr) (eq? 'dot (operator expr))) (mvalue_return (parsedot expr state)))
      (else (mvalue_return (mvalue_expression (cadr expr) state throw) state return throw))))); pass it back into mvalue_return so that booleans will be properly converted to "true" or "false"

;mvalue of mathematical expressions (can be bool or number)
;I made the decision to treat bools and numbers the same, as they are both values that can be stored and manipulated
(define mvalue_expression
  (lambda (expr state throw)
    (cond
      ((null? expr) 0)
      ((number? expr) expr)
      ((boolean? expr) expr)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #f)
      ((not (pair? expr)) (mvalue_variable expr state));single atom (variable)
      ((and (eq? '- (operator expr)) (not (hassecondop expr))) (- 0 (mvalue_expression (firstop expr) state throw))); to handle the unary -
      ((and (eq? '! (operator expr)) (not (hassecondop expr))) (not (mvalue_expression (firstop expr) state throw))); to handle the unary !
      ((eq? '+ (operator expr)) (+ (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? '- (operator expr)) (- (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? '* (operator expr)) (* (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? '/ (operator expr)) (quotient (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? '% (operator expr)) (remainder (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? '== (operator expr)) (= (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? '!= (operator expr)) (not (= (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw))))
      ((eq? '> (operator expr)) (> (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? '>= (operator expr)) (>= (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? '< (operator expr)) (< (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? '<= (operator expr)) (<= (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? '&& (operator expr)) (and (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? '|| (operator expr)) (or (mvalue_expression (firstop expr) state throw) (mvalue_expression (secondop expr) state throw)))
      ((eq? 'dot (operator expr)) (parsedot expr state throw))
      ((eq? 'funcall (operator expr)) (mvalue_fcall expr state throw))
      (else (error "Invalid operator" expr)))))

(define mvalue_fcall
  (lambda (expr state throw)
    (execute_function (parsedot (firstop expr) state throw) (functionparamsaslist expr) (get_state_for_funcall (firstop expr) state) throw)))

;returns the first operand of the given statement
(define firstop cadr)
;returns the second operand of the given statement
(define secondop caddr)
;returns true if the given statement has a second operand
(define hassecondop (lambda (l) (pair? (cddr l))))
;returns the parameters (as a list) given a funcall statement
(define functionparamsaslist cddr)

;;; *** mstate functions *** ;;;

;determines the state resulting from a given statement
(define mstate_statement
  (lambda (statement state return continue break throw)
    (cond
      ((null? statement) statement);this allows for a shortcut when evaluating if statements without else statements
      ((eq? (operator statement) 'if) (mstate_condition statement state return continue break throw))
      ((eq? (operator statement) 'return) (return (convertbool (mvalue_expression (firstop statement) state throw))))
      ((eq? (operator statement) 'begin) (mstate_begin statement state return continue break throw))
      ((eq? (operator statement) 'while) (mstate_while statement state return throw))
      ((eq? (operator statement) 'funcall) (begin (execute_function (firstop statement) (functionparamsaslist statement) state throw) state));execute the function, but ignore its return value and return the state
      ((eq? (operator statement) 'continue) (continue state))
      ((eq? (operator statement) 'break) (break state))
      ((eq? (operator statement) 'try) (mstate_try statement state return continue break throw))
      ((eq? (operator statement) 'throw) (throw (mvalue_expression (firstop statement) state throw)))
      (else (mstate_declarestatement statement state throw)))))

; this function is a continuation of mstate_statement; it handles only declaration and assignment statements
; It is seperated so that the first pass parser can use it without needing the extra parameters of mstate_statement
(define mstate_declarestatement
  (lambda (statement state throw)
    (cond
      ((eq? (operator statement) 'var) (mstate_declare statement state throw))
      ((eq? (operator statement) 'static-var) (mstate_declare statement state null))
      ((eq? (operator statement) '=) (mstate_assign statement state throw))
      ((eq? (operator statement) 'function) (mstate_function_decl statement state))
      ((eq? (operator statement) 'static-function) (mstate_function_decl statement state))
      ((eq? (operator statement) 'class) (mstate_class_decl statement state))
      (else (error "invalid statement" statement)))))
  
(define try_finally_body cadr)
(define try_finally cadddr)
(define try_catch caddr)
(define try_catch_variable (lambda (l) (car (cadr l))))
(define try_catch_body caddr)
(define try_body cadr)

(define mstate_try
  (lambda (statement state return continue break throw)
    (mstate_finally 
     (try_finally statement) 
     (mstate_catchblock 
      (call/cc 
       (lambda (throw) 
         (mstate_compound (try_body statement) state return continue break (lambda (value) (throw (list 'EXCEPTION value)))))) 
      (try_catch statement) state return break continue throw)
     return continue break throw)))
     
(define mstate_catchblock
  (lambda (value catchbody state return break continue throw)
    (cond
      ((null? catchbody) value)
      ((not (list? value)) value);this shouldn't happen
      ((eq? 'EXCEPTION (car value)) (endblockstate (mstate_compound (try_catch_body catchbody) (assignvalue (try_catch_variable catchbody) (cadr value) (adduninitializedvarible (try_catch_variable catchbody) (newblockstate state))) return continue break throw)))
      (else value))))

(define mstate_finally
  (lambda (body state return continue break throw)
    (cond
      ((null? body) state)
      (else (mstate_compound (try_finally_body body) state return continue break throw)))))


(define mstate_class_decl
  (lambda (statement state)
    (addClassToState (classdecl_classname statement) (classdecl_extensions statement) (classdecl_body statement) state)))

(define classdecl_classname cadr)
(define classdecl_extensions caddr)
(define classdecl_body cadddr)

;returns the operator of the given statement
(define operator car)

;adds the given function declaration statement to the state
(define mstate_function_decl
  (lambda (statement state)
    (assignvalue (function_decl_name statement) (build_function_closure statement state) (adduninitializedvarible (function_decl_name statement) state))))

;these defines determine where to look in the source code for the various parts of a function declaration
(define function_decl_name cadr)
(define function_decl_params caddr)
(define function_decl_body cadddr)

(define build_function_closure
  (lambda (statement state)
    (list (function_decl_params statement) (function_decl_body statement) (lambda (args curstate) (bind_variables_for_call args (function_decl_params statement) (addfcalllayer function_layer_seperator curstate) curstate)))))

(define function_layer_seperator 0)

;these defines determine where each 'field' is in the closure
(define closure_params car)
(define closure_body cadr)
(define closure_statefunc caddr)

; helper function for build_function_closure
; transfers the parameters into the state for a function call
(define bind_variables_for_call
  (lambda (inputargs declargnames declstate callstate)
    (cond
      ((null? inputargs) declstate)
      ((not (eq? (length inputargs) (length declargnames))) (error "Parameter mismatch; expected" (length declargnames) 'parameter/s 'recieved (length inputargs)))
      (else (bind_variables_for_call (cdr inputargs) (cdr declargnames) (assignvalue (car declargnames) (mvalue_expression (car inputargs) callstate null) (adduninitializedvarible (car declargnames) declstate)) callstate)))))
      

;determines the state change (if any) resulting from an if statement
(define mstate_condition
  (lambda (statement state return continue break throw)
    (cond
       ((mvalue_expression (cond_condition statement) state throw) (mstate_statement (cond_then statement) state return continue break throw))
       ((cond_haselse statement) (mstate_statement (cadddr statement) state return continue break throw));this is if we have an else statement as well
       (else state))))

;returns true if the given if statement has an else statement
(define cond_haselse (lambda (l) (pair? (cdddr l))))
;returns the else statement of the given if statement
(define cond_getelse cadddr)
;returns the condition of an if statement
(define cond_condition cadr)
;returns the then statement of an if-then statement
(define cond_then caddr)

;mstate function for compound lists of statements
(define mstate_compound
  (lambda (statement state return continue break throw)
    (cond
      ((null? statement) state) ;no statements means no state change!
      (else (mstate_compound (cdr statement) (mstate_statement (car statement) state return continue (lambda (breakstate) (break (endblockstate breakstate))) throw) return continue break throw)))))

;mstate function for blocks
(define mstate_begin
  (lambda (statement state return continue break throw)
    (endblockstate (mstate_compound (cdr statement) (newblockstate state) return continue break throw))))

;mstate function for while loops
(define mstate_while
  (lambda (statement state return throw)
   (call/cc (lambda (break) 
    (cond
      ((mvalue_expression (while_cond statement) state throw) (mstate_while statement 
        (call/cc (lambda (continue) (mstate_statement (while_loop statement) state return continue break throw))) return throw))
      (else state))))))

;returns the condition of a while loop
(define while_cond cadr)
;returns the looping statement of a while loop
(define while_loop caddr)

;mstate function for variable declarations
(define mstate_declare
  (lambda (statement state throw)
    (cond
      ((declared? (firstop statement) state) (error "Variable has already been declared" statement))
      ((not (hassecondop statement)) (adduninitializedvarible (firstop statement) state));if the initial value is omitted
      (else (assignvalue (firstop statement) (mvalue_expression (secondop statement) state throw) (adduninitializedvarible (firstop statement) state))))))

; determines the state after variable assignment
(define mstate_assign
  (lambda (statement state throw)
    (assignvalue (firstop statement) (mvalue_expression (secondop statement) state throw) state)))
      
;Adds an uninitalized variable to the given state
(define adduninitializedvarible
  (lambda (variable state)
    (cons (cons (list variable (box '())) (state_toplayer state)) (state_otherlayers state))))

; converts from #t/#f to true/false
(define convertbool
  (lambda (bool)
    (cond
      ((not (boolean? bool)) bool)
      (else (if bool 'true 'false)))))

;returns true if the given variable is declared within the given state
(define declared?
  (lambda (variable state)
    (cond
      ((null? state) #f)
      ((not (list? (state_toplayer state))) #f) ; we hit one of the spacers added when a function is called
      ((declaredinlayer? variable (state_toplayer state)) #t)
      (else (declared? variable (state_otherlayers state))))))

;returns the top layer of the given state
(define state_toplayer car)
;returns all layers except for the top of the given state
(define state_otherlayers cdr)
;returns the name of the first variable defined in the given state layer
(define state_topvariable caar)
;returns the value paired with the first variable in the given state layer
(define state_valueoffirstvar (lambda (q) (unbox (state_firstvarbox q))))
(define state_firstvarbox (lambda (q) (cadr (car q))))
       
;utility function, returns true if the given variable has been declared in the given state layer
(define declaredinlayer?
  (lambda (variable layer)
    (cond
      ((null? layer) #f)
      ((not (list? layer)) #f) ; this checks if we've hit a spacer due to a function call
      ((eq? (state_topvariable layer) variable) #t)
      (else (declaredinlayer? variable (cdr layer))))))

(define getclosureforfunction
  (lambda (functionname state)
    (mvalue_variable functionname state)))

(define getclosureforclass
  (lambda (classname state)
    (mvalue_variable classname state)))

;mvalue of a variable; searches through the state list(s) for the value of the given variable
(define mvalue_variable
  (lambda (var state)
    (cond
      ((null? state) (error "undefined or uninitalized variable" var))
      ;((not (list? (state_toplayer state))) (mvalue_variable var (state_otherlayers state))); if we encounter a function call spacer, ignore it
      ((declaredinlayer? var (state_toplayer state)) (variablevalueinlayer var (state_toplayer state)))
      (else (mvalue_variable var (state_otherlayers state)))))) ; check the next layer

(define null_value (box '()))

;helper function: returns the value of the given variable in the given state layer
(define variablevalueinlayer
  (lambda (var layer)
    (cond
      ((null? layer) (error "undefined or uninitalized variable" var))
      ((not (firstvariableinlayerinitalized? layer)) (variablevalueinlayer var (cdr layer)));skip any variables that haven't been initalized yet
      ((eq? (state_topvariable layer) var) (state_valueoffirstvar layer));we found it!
      (else (variablevalueinlayer var (cdr layer))))))

(define firstvariableinlayerinitalized? (lambda (layer) (not (null? (unbox (state_firstvarbox layer))))))

(define addClassToState
  (lambda (classname extensions body state)
    (assignvalue classname (build_class_closure classname extensions body state) (adduninitializedvarible classname state))))

(define build_class_closure
  (lambda (classname extensions body state)
    (buildclassglobalstatelayer body (getstatefromextensions classname extensions state))))
                                      
(define getstatefromextensions
  (lambda (classname extensions state)
    (cond
      ((null? extensions) initialstate)
      ;(else (addfcalllayer classname (get_class_state (extensions_parentclass extensions) state)))
      (else 
       ((lambda (classname parentstate parentname)
          (assignvalue 'super parentstate (adduninitializedvarible 'super (addfcalllayer parentname parentstate))))
        classname (get_class_state (extensions_parentclass extensions) state) (extensions_parentclass extensions))))))

(define buildclassstateext
  (lambda (classname parentstate)
    (assignvalue 'super parentstate (adduninitializedvarible 'super (addfcalllayer classname parentstate)))))

(define extensions_parentclass cadr)

(define get_class_state
  (lambda (classname state)
    (append (getclosureforclass classname state) state)))

;(define class_closure_state car)
    


;assigns the given value to the given variable; searches all layers of the given state
(define assignvalue
  (lambda (variable value state)
    (cond
      ((null? state) (error "Failed to assign value" value 'to 'unknown 'variable variable))
      ((isclassmarker? (state_toplayer state)) (begin (assignvalue variable value (get_class_state (state_toplayer state) state)) state))
      ((declaredinlayer? variable (state_toplayer state)) (cons (assignvalueinlayer variable value (state_toplayer state)) (state_otherlayers state)))
      (else (cons (state_toplayer state) (assignvalue variable value (state_otherlayers state))))))); if we haven't found the variable in this layer, keep looking in the lower layers

  
(define isclassmarker?
  (lambda (var)
    (and (not (list? var)) (not (number? var)))))
  
;assigns the given value to the given variable in the given state layer
(define assignvalueinlayer
  (lambda (variable value state)
    (cond
      ((null? state) (error "Failed to assign value" value 'to 'unknown 'variable variable))
      ((eq? (state_topvariable state) variable) (assignvaluetotopvariableinlayer value state))
      (else (cons (car state) (assignvalueinlayer variable value (cdr state))))))); if we haven't found the variable yet, keep looking

;(define assignvaluetotopvariableinlayer
;  (lambda (value layer)
;    (cond
;      ((firstvariableinlayerinitalized? layer) (begin (set-box! (state_firstvarbox layer) value) layer)); this variable is already initalized, so its box just needs to be updated
;      (else (cons (list (state_topvariable layer) (box value)) (cdr layer)))))); the variable was not initalized, so it needs to be updated

(define assignvaluetotopvariableinlayer
  (lambda (value layer)
    (firstvariableinlayerinitalized? layer) (begin (set-box! (state_firstvarbox layer) value) layer)))
      

;adds a layer to the state and returns it
(define newblockstate
  (lambda (state)
    (cons '() state)))

;adds another layer and a spacer in preparation for calling a function
(define addfcalllayer
  (lambda (functionname state)
    (newblockstate (cons functionname state))))
;removes the topmost layer from the state and returns it
(define endblockstate
  (lambda (state)
    (cdr state)))

