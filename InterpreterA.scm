; EECS 345 - PLC

(load "classParser.scm")

(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

; stack operations
(define pop (lambda (stack) (cdr stack)))
(define push (lambda (state stack) (cons state stack)))
(define peek (lambda (stack) (car stack)))

; Environment Abstraction
; The environment is represented as a "stack" (list of states)

; some definitions
(define initEnvironment '())
(define newStackFrame '(()()))
(define blankstate '((()())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; recursively examines each part of the list, determing the state at each step
;(define evalParseTree
;  (lambda (tree state class)
;    (if (null? tree)
;         state
 ;        (evalParseTree (cdr tree) (Mstate (car tree) state (lambda (v) (error 'incorrect-return)) (lambda (v) v) (lambda (v) v) class) class))))

;creates the initial environment by populating a base state with globally available functions and variables
;(define interpret
;  (lambda (filename)
;    (interpret-h (prepInterpreter filename))))

;calls the main function and turns the output to something human readable if it needs to be (ex #t --> true)
;(define interpret-h
;  (lambda (environ)
;    (discern_return (Mstate_funcall '(funcall main) environ))))

; outermost call - calls Mstate on global vars & function declarations
;(define prepInterpreter
;  (lambda (filename classname)
;    (evalParseTree (parser filename) blankstate classname)))

(define interpret
  (lambda (filename classname)
              (discern_return (Mstate_funcall '(funcall main) (add_classes_from_parselist (parser filename) blankstate) (string->symbol classname)))))

(define add_classes_from_parselist
  (lambda (parsedcode environ)
    (cond
      ((null? parsedcode) environ)
      (else (add_classes_from_parselist (cdr parsedcode) (add_class (car parsedcode) environ))))))

;Some class-interpreting abstractions
(define classname cadr)
(define class_extends caddr)
(define class_body cadddr)

(define add_class
  (lambda (stmt environ)
    (cond
      ((eq? 'class (operator stmt)) (Mstate_classdecl (classname stmt) (class_extends stmt) (class_body stmt) environ))
      (else (error "Invalid Parse tree can Only Have Classes at outermost level")))))

;initially a class will be 
(define Mstate_classdecl
  (lambda (name extends body environ)
    (Mstate_classbody_list body (Mstate_Add name (cons '((()())) (cons '() (clarify_extends extends))) environ) name)
    ))

;if the class doesnt extend anything extends = '() else just include the classname as the extends statement
;ex: '(extends classB) --> '(classB), but '() remains '()
(define clarify_extends
  (lambda (extends_stmt)
    (if (null? extends_stmt) '(())
        (list (cadr extends_stmt)))))

;recursively parse the classbody (parses static variables and functions)
(define Mstate_classbody_list
  (lambda (classbody environ class)
    (cond
      ((null? classbody) environ)
      (else (Mstate_classbody_list (cdr classbody) (Mstate_classbody (car classbody) environ class) class)))))

(define Mstate_classbody
  (lambda (stmt environ class)
    (cond
      ((eq? 'static-var (operator stmt)) (Mstate_staticvar_decl  stmt environ class))
      ((eq? 'static-function (operator stmt)) (Mstate_funcDecl stmt environ class))
      (else environ))))
      ;(else (error "we really should never get here")))))

(define function_name cadr)

(define exists_in_class?
  (lambda (stmt class environ)
    (exists? stmt (get_class_environment class environ))))

(define get_class_environment
  (lambda (class environ)
    (car (scopedLookup class environ class))))

;handles static variable declarations within classes
;updates the class definition by adding the static variable to the class environment
;cons that environment to the rest of the class environment, and then replaces the 
;value associated with the classname with the new class construct
(define Mstate_staticvar_decl
  (lambda (stmt environ class)
    (update_environ class 
                    (include_rest_of_class 
                     (Mstate_Add (cadr stmt) 
                                 (Mval (caddr stmt) (retrieve_class_environ class environ)  class)
                                 (car (scopedLookup class environ class))) ;car (scopedLookup class environ) returns local class environmentg 
                     class environ) 
                    environ)))

; adds function binding to the state
(define Mstate_funcDecl
  (lambda (expression environ class)
    (update_environ class
                    (include_rest_of_class
                     (Mstate_Add (cadr expression)
                                 (cddr expression)
                                 (car (scopedLookup class environ class)))
                     class environ)
                    environ)))
                                

;(car (scopedLookup class environ) returns the class closure, car of that is the class environment. 
;We want access to the class environment and the current environment so we cons them
(define retrieve_class_environ
  (lambda (class environ)
    (cons (car (car (scopedLookup class environ class))) environ)))
    ;(car (car (scopedLookup class environ))) environ))

(define lookup_in_class
  (lambda (name environ class)
    (scopedLookup name (retrieve_class_environ class environ) class)))

;includes
(define include_rest_of_class
  (lambda (body class environ)
    (cons body (cdr (scopedLookup class environ class)))))

;updates a binding in the environment with the new value
(define update_environ
  (lambda (name value environ)
    (cond
      ((null? environ) environ)
      ((not (exists_inscope? name environ))  (cons (car environ)(update_environ name value (cdr environ))))
      (else (Mstate_Add name value (stateRemove name environ))))))
      ;((null? (cdr environ)) (list (cons name (caar environ)) (cons (box value) (car (cdr (car (environ)))))))
      ;(else (list (car environ) (update_environ name value (cdr environ))))
    
; analyzes an arithmatic function
; calls mval on each operand to fully evaluate each operand before running the comparison statement
(define Mvalue_Arith
  (lambda (expression state class)      
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (Mvalue_Arith (Mval (leftoperand expression)state  class) state class) 
                                         (Mvalue_Arith (Mval (rightoperand expression)state class) state class)))
      ((eq? '* (operator expression)) (* (Mvalue_Arith (Mval (leftoperand expression)state  class) state class) 
                                         (Mvalue_Arith (Mval (rightoperand expression)state class) state class)))
      ((eq? '- (operator expression)) (if (null? (cddr expression))
                                          (- 0 (Mvalue_Arith (Mval(leftoperand expression)state class) state class))
                                          (- (Mvalue_Arith (Mval (leftoperand expression)state class) state class) (Mvalue_Arith (Mval (rightoperand expression)state class) state class))))
      ((eq? '/ (operator expression)) (quotient (Mvalue_Arith (Mval (leftoperand expression)state  class) state class) 
                                         (Mvalue_Arith (Mval (rightoperand expression)state class) state class)))
      ((eq? '% (operator expression)) (remainder (Mvalue_Arith (Mval (leftoperand expression)state  class) state class) 
                                         (Mvalue_Arith (Mval (rightoperand expression)state class) state class)))     
      (else (error 'unknown-operator)))))

; recursively analyzes a comparison statement by analyzing the left and right operands with the passed in operator, 
; calls Mval on each operand in case the operand is an arithmetic or boolean statement, or needs a lookup for a var
(define Mvalue_Compare
  (lambda (expression state class)
    (cond
      ((number? expression) expression)
      ((eq? '== (operator expression)) (eq? (Mvalue_Compare (Mval(leftoperand expression) state class) state class) (Mvalue_Compare (Mval (rightoperand expression)state class) state class)))
      ((eq? '!= (operator expression)) (not (eq? (Mvalue_Compare (Mval(leftoperand expression) state class) state class) (Mvalue_Compare (Mval (rightoperand expression)state class) state class))))
      ((eq? '< (operator expression)) (< (Mvalue_Compare (Mval(leftoperand expression) state class) state class) (Mvalue_Compare (Mval (rightoperand expression)state class) state class)))
      ((eq? '> (operator expression)) (> (Mvalue_Compare (Mval(leftoperand expression) state class) state class) (Mvalue_Compare (Mval (rightoperand expression)state class) state class)))
      ((eq? '<= (operator expression)) (<= (Mvalue_Compare (Mval(leftoperand expression) state class) state class) (Mvalue_Compare (Mval (rightoperand expression)state class) state class)))
      ((eq? '>= (operator expression)) (>= (Mvalue_Compare (Mval(leftoperand expression) state class) state class) (Mvalue_Compare (Mval (rightoperand expression)state class) state class)))
      (else (error 'unknown-comparison)))))

; recursively analyzes a boolean statement by analyzing the left and right operands with the passed in operator, calls Mval on each operand in case comparisons or lookups are needed
(define Mvalue_Bool
  (lambda (expression state  class)
    (cond
      ((eq? #t expression) #t)
      ((eq? #f expression) #f)
      ((eq? '&& (operator expression))(and (Mvalue_Bool (Mval(leftoperand expression)state class) state class) (Mvalue_Bool (Mval(rightoperand expression)state class) state class)))
      ((eq? '|| (operator expression)) (or (Mvalue_Bool (Mval(leftoperand expression)state class) state class) (Mvalue_Bool (Mval(rightoperand expression)state class) state class)))
      ((eq? '! (operator expression)) (not (Mvalue_Bool (Mval(leftoperand expression)state class) state class)))
      (else (error 'unknown-boolean)))))

; checks if a var exists in a state
(define exists?
  (lambda (var environ)
    (cond
      ((null? environ) #f)
      ((exists_inscope? var environ) #t)
      (else (exists? var (cdr environ))))))
 

; used for removing from the state
(define stateRemove
  (lambda (var environ)
    (cond
      ((null? environ) environ)
      ((not (exists_inscope? var environ)) (cons (car environ) (stateRemove var (cdr environ))))
      (else (cons (stateRemove_helper var (car environ)) (cdr environ))))))

; used for removing from the state
(define stateRemove_helper
  (lambda (var state)
    (cond
      ((null? (car state)) state)
      ((eq? var (car (car state))) (list (cdr (car state)) (cdr (cadr state))))
      (else (list 
             (cons  (car (car state)) (car (stateRemove_helper var (list (cdr (car state)) (cdr (cadr state))))))
             (cons (car (cadr state)) (cadr (stateRemove_helper var (list (cdr (car state)) (cdr (cadr state)))))))))))

; checks if a var exists in a state
(define exists_inscope?
  (lambda (var state)
    (cond
      ((null? (car (car state))) #f)
      ((eq? (car (car (car state))) var) #t)
      (else (exists_inscope? var (cons (list (cdr (car (car state))) (cdr (car (cdr (car state))))) (cdr state)))))))


; used for adding to the state
(define Mstate_Add
  (lambda (name val environ)
    (cons (list (cons name (car (peek environ))) (cons (box val) (cadr (peek environ)))) (pop environ))))

; adds the var binding to the state with the passed in value or '() if no value provided.  ror if var already exists in state
(define Mstate_Decl
  (lambda (expression environ class)
    (cond 
      ((exists_inscope? (leftoperand expression) environ) (error 'multiple-instantiation)) ;;should be exists in scope?
      ((null? (cddr expression)) (Mstate_Add (leftoperand expression) '() environ))
      (else (Mstate_Add (leftoperand expression) (Mval (rightoperand expression) environ class)  environ)))))


; handles function calls
;(define Mstate_func
;  (lambda (expression environ ret class)
;    (pop (Mstate_list (cadr (scopedLookup (cadr expression) environ)) (static_scoping (add_list_params (cadr expression) (cddr expression) environ)) ret (lambda (v) v) (lambda (v) v) class))))

(define Mstate_func
  (lambda (expression environ ret base_class class) ;maybe keep track of original class and current class (parent of original) if we run into probs
    (cond
      ((exists_in_class? (cadr expression) class environ)
       (if (eq? base_class class)
           (pop (Mstate_list (cadr (lookup_in_class (cadr expression) environ class)) (add_list_params (cadr expression) (cddr expression) (retrieve_class_environ class environ) class) ret (lambda (v) v) (lambda (v) v) class  ))
           (pop (Mstate_list (cadr (lookup_in_class (cadr expression) environ class)) (add_list_params_2 (lookup_in_class (cadr expression) environ class) (cddr expression) (retrieve_class_environ base_class environ) base_class) ret (lambda (v) v) (lambda (v) v) class  ))
       ))
      ((null? (get_class_parent class environ)) (error 'function_doesnt_exist))
      (else (pop (Mstate_func expression environ ret class (get_class_parent class environ)  )))))) ;if function isnt found in class try and execute the function in the parent

(define Mstate_funcall
  (lambda (expression environ class  )
    (call/cc (lambda (ret)
             (Mstate_func expression (push newStackFrame environ) ret class class  )))))

;this will call a helper function that will create a new state that pairs function parameter names with the passed in values
(define add_list_params
  (lambda (name param-vals environ class)
    (parse_list_params (car (lookup_in_class name environ class)) param-vals environ class)))

;this will call a helper function that will create a new state that pairs function parameter names with the passed in values
(define add_list_params_2
  (lambda (looked_up param-vals environ class)
    (parse_list_params (car looked_up) param-vals environ class)))

;creates an environment that limits the view of the function only to parameters that are visible in static scoping
(define static_scoping
  (lambda (environ)
    (cond
      ((null? (cdr environ)) environ)
      (else (cons (car environ) (get_functions environ))))))

;helps static_scoping method return visible fucntions
(define get_functions
  (lambda (environ)
    (cond
      ((null? environ) environ)
      ((contains_function (car (cdr (car environ)))) (cons (car environ) (get_functions (cdr environ))))
      (else (get_functions (cdr environ ))))))
 
;helps get_fucntions find functions
(define contains_function
  (lambda (stack)
    (cond
      ((null? stack) #f)
      ((pair? (unbox (car stack))) #t)
      (else (contains_function (cdr stack))))))
     
;adds each parameter value to the state (paired with the parameter name)
(define parse_list_params
  (lambda (param-names param-vals environ class)
    (cond
      ((and (null? param-names)(null? param-vals)) environ)
      ((null? param-names) (error 'too-many-params))
      ((null? param-vals) (error 'too-few-params))                                                  ;pop environ to prevent using a local variable
      (else (parse_list_params (cdr param-names) (cdr param-vals) (Mstate_Add (car param-names) (Mval (car param-vals) (pop environ) class) environ) class)))))

; to return true and false instead of #t and #f
(define mapBool
  (lambda (bool)
    (if bool 'true 'false)))

; if variable has already been declared: add the state to the corresponding variable in state (removing the previous binding)  s error if var not declared
(define Mstate_Assign
  (lambda (expression currstate environ class   )
    (cond
      ((is_static_var? (leftoperand expression) currstate class) ;maybe using environ would be a better state? both work for test 6
        (update_environ class 
                    (include_rest_of_class 
                     (update_environ (leftoperand expression) 
                                 (Mval (rightoperand expression) (retrieve_class_environ class environ)  class)
                                 (car (scopedLookup class environ class))) ;car (scopedLookup class environ) returns local class environmentg 
                     class environ) 
                    environ))
      ((not (exists? (leftoperand expression) currstate)) (error 'using-before-declaring))
      ((exists_inscope? (leftoperand expression) currstate)
       (Mstate_Add (leftoperand expression) (Mval (rightoperand expression) environ class) (stateRemove (leftoperand expression) currstate)))
      (else (cons (car currstate) (Mstate_Assign expression (cdr currstate) environ class))))))

; (if (conditional) then-statement optional-else-statement)
; (if (> (* x x) y)
; 4 components to an if: (if (conditional) then-statement optional-else-statement)
; evaluats the conditional with Mval, returns the corresponding statement, returning the else if the optional 4th component is passed in (and condition == false)
(define Mstate_if ;editted to have break and cont
  (lambda (expression environ ret break cont class  )
    (cond
     ((Mval (leftoperand expression) environ class) (Mstate (rightoperand expression) environ ret break cont class))
     ((null? (cdddr expression)) environ)
     (else (Mstate (cadddr expression) environ ret break cont class)))))

; unified Mstate method, determins which mstate method to call (from the operator) and passes the statement to that function
(define Mstate
  (lambda (stmt state ret break cont class) ;editted to have break and cont
    (cond
      ((eq? '= (operator stmt)) (Mstate_Assign stmt state state class  ))
      ((eq? 'var (operator stmt)) (Mstate_Decl stmt state class))       
      ((eq? 'if (operator stmt)) (Mstate_if stmt state ret break cont class  ))
      ((eq? 'return (operator stmt)) (ret (Mval stmt state class) ))
      ((eq? 'dot (operator stmt)) (Mstate_dot stmt state class))
      ;((eq? 'return (operator stmt)) (ret state))
      ((eq? 'while (operator stmt)) (Mstate_while stmt state ret class  ))
      ((eq? 'begin (operator stmt)) (Mstate_begin stmt state ret break cont class  ))
      ((eq? 'break (operator stmt)) (break state))
      ((eq? 'continue (operator stmt)) (cont state))
      ;((and (eq? 'function (operator stmt)) (pair? (cdr environment
      ((eq? 'function (operator stmt)) (Mstate_funcDecl stmt state class))
      ((eq? 'funcall (operator stmt)) (Mstate_call_classfunc stmt state class  ))
      ;((eq? 'funcall (operator stmt)) (begin (Mstate_call_classfunc stmt state class) state))
      ((eq? 'try (operator stmt)) (Mstate_try stmt state ret break cont class  ))
      (else (Mval stmt state class)))))

; unified MValue function, determines the values of the statement by determing the operator and passsing it to the corresponding mval function
(define Mval
  (lambda (stmt state class)
    (cond
      ((null? stmt) '())
      ((number? stmt) stmt)
      ((eq? 'true stmt) #t)
      ((eq? 'false stmt) #f)
      ((not (list? stmt)) (lookup_in_class stmt state class))
      ((null? (cdr stmt)) (Mval (car stmt) state class))
      ((eq? 'dot (operator stmt)) (Mstate_dot stmt state class))
      ((eq? 'return (operator stmt)) (Mval (leftoperand stmt) state class))
      ((or (eq? '*  (operator stmt)) (eq? '/  (operator stmt)) (eq? '+  (operator stmt)) (eq? '-  (operator stmt)) (eq? '% (operator stmt))) (Mvalue_Arith stmt state class))
      ((or (eq? '&& (operator stmt)) (eq? '|| (operator stmt)) (eq? '! (operator stmt))) (Mvalue_Bool stmt state class))
      ((or (eq? '< (operator stmt)) (eq? '> (operator stmt))
           (eq? '<= (operator stmt))(eq? '>= (operator stmt))
           (eq? '== (operator stmt))(eq? '!= (operator stmt))) (Mvalue_Compare stmt state class))
     ; ((and (eq? 'funcall (operator stmt)) (null? (pop state))) (Mstate_funcall stmt (pop state)))
      ((eq? 'funcall (operator stmt)) (Mstate_call_classfunc stmt state class))
      ;((eq? 'funcall (operator stmt)) (begin (Mstate_call_classfunc stmt state class) state))
      (else stmt))))
      

;to turn #t and #f into true and false if they are returned
(define discern_return
  (lambda (value)
    (if (boolean? value)
        (mapBool value)
        value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; finds a variable within the environment but abides by scope rules
; 1) you can access definitions outside your block
; 2) you cannot access any definitions outside contained in another block outside your block
; 3) if you dont find it within your block scope, then you search outside your block scope
(define scopedLookupHelper
  (lambda (var environ return)
    (cond
      ((or (null?  environ)(null? (car environ))) (return '()))
      ((and (list? (car environ)) (list? (car (car environ)))) (scopedLookupHelper var (peek environ) (lambda (v1) (scopedLookupHelper var (pop environ) 
                                                                                                                           (lambda (v2)
                                                                                                                             (if (null? v1)
                                                                                                                                 (return v2)
                                                                                                                                 (return (cons v1 v2))))))))
      ((eq? var (car (car environ))) (if (list? (car (cadr environ)))
                                       (return 'unassigned-variable) ;unassigned variable: awful fix to get around
                                       (return (unbox (car (cadr environ))))))
      (else (scopedLookupHelper var (list (cdr (car environ)) (cdr (cadr environ))) (lambda (v)  (return v)))))))

; new lookup function to work with our stack. If it doesn't find it in uppermost state, it looks in every other state until it is found
(define scopedLookup
  (lambda (var environ class)
    (scopedLookupHelper var environ (lambda (v)
                                      (cond
                                        ((null? v) (if (null? (get_class_parent class environ)) 
                                                       (error "using-before-declaring -->" var)
                                                       (scopedLookup var (retrieve_class_environ (get_class_parent class environ) environ) class)))
                                        ((eq?  (car v) 'unassigned-variable) (error 'unassigned-variable))
                                        (else (car v)))))))
  
;  (define Mstate_func ;Duplicate code?
;  (lambda (expression environ ret base_class class  ) ;maybe keep track of original class and current class (parent of original) if we run into probs
;    (cond
;      ((exists_in_class? (cadr expression) class environ)
;       (if (eq? base_class class)
;           (pop (Mstate_list (cadr (lookup_in_class (cadr expression) environ class)) (add_list_params (cadr expression) (cddr expression) (retrieve_class_environ class environ) class) ret (lambda (v) v) (lambda (v) v) class  ))
;           (pop (Mstate_list (cadr (lookup_in_class (cadr expression) environ class)) (add_list_params_2 (lookup_in_class (cadr expression) environ class) (cddr expression) (retrieve_class_environ base_class environ) base_class) ret (lambda (v) v) (lambda (v) v) class  ))
;       ))
;      ((null? (get_class_parent class environ)) (error 'function_doesnt_exist))
;      (else (pop (Mstate_func expression environ ret class (get_class_parent class environ))))))) ;if function isnt found in class try and execute the function in the parent

; mstate function to determine the state of a program after a while loop. Loops as long as condition == true
(define Mstate_while
  (lambda (expression environ ret class  )
    (call/cc (lambda (break)
               (letrec ((loop (lambda (condition body environ)
                                (if (Mval condition environ class)
                                    (loop condition body 
                                          (call/cc
                                           (lambda (continue) (Mstate body environ ret break continue class)))) environ))))
                 (loop (leftoperand expression) (rightoperand expression) environ))))))

; pushes a new state to the stack and handles the list of statements within the begin block, after the block is over it pops the now invalid state off the stack
(define Mstate_begin 
  (lambda (expression environ ret break cont class  )
    (pop (Mstate_list (cdr expression) (cons newStackFrame environ) ret (lambda (v) (break (pop v))) (lambda (v) (cont (pop v))) class))))

; helper to handle blocks of statements. Calls mstate on each statement in the list
(define Mstate_list
  (lambda (list environ ret break cont class  )
    (if (null? list) 
        environ
        (Mstate_list (cdr list) (Mstate (car list) environ ret break cont class  ) ret break cont class  ))))

;some class-based function call abstractions
(define dot_portion cadr)
(define function_portion caddr)
(define param_portion cddr)

;(class-name (if (eq? (cadr dot-expr)'super) (get-class-parent class env) (cadr dot-expr)))
(define determine_classname_dot
  (lambda (stmt environ class)
    ;(if (eq? 'super (cadr (dot_portion stmt)))
        (if (eq? 'super (cadr stmt))
        (get_class_parent class environ)
        class)))

;(class-name (if (eq? (cadr dot-expr)'super) (get-class-parent class env) (cadr dot-expr)))
(define determine_classname
  (lambda (stmt environ class)
    (if (eq? 'super (cadr (dot_portion stmt)))
        (get_class_parent class environ)
        (cadr (dot_portion stmt)))))

;should return parent portion of the class closure
(define get_class_parent
  (lambda (class environ)
    ;(display (scopedLookup class environ))
    (caddr (scopedLookup class environ class))))

(define Mstate_dot
 (lambda (expression environ class)
   (cond
   ;((eq? 'super (leftoperand expression)) (Mstate_dot expression environ (get_class_parent (leftoperand expression) environ)))  
   ((exists_in_class? (rightoperand expression) class environ) (lookup_in_class (rightoperand expression) environ (determine_classname_dot expression environ class)))
   ((and (not (eq? 'super (leftoperand expression))) (exists_in_class? (rightoperand expression) (leftoperand expression) environ))
    (lookup_in_class (rightoperand expression) environ (leftoperand expression)))
   ((null? (get_class_parent (leftoperand expression) environ)) (error 'no_function_decl))
   (else (Mstate_dot expression environ (get_class_parent (leftoperand expression) environ))))))

(define Mstate_call_classfunc
  (lambda (stmt environ class  )
    ;(display (construct_functioncall (function_portion (dot_portion stmt)) (cddr stmt)))
    (if (list? (cadr stmt))
      ; if a (dot A f) part is provided
        (Mstate_funcall (construct_functioncall (function_portion (dot_portion stmt)) (cddr stmt)) environ (determine_classname stmt environ class) )
      ; if the function is called in the current class
      (Mstate_funcall stmt environ class  ))))

(define is_static_var?
  (lambda (var environ class)
    (cond
      ((exists? var (get_class_environment class environ)) #t)
      ((null? (get_class_parent class environ)) #f)
      (is_static_var? var environ (get_class_parent class environ)))))
   
;funcall takes expression environment class
(define construct_functioncall
  (lambda (func_name func_params)
    (append (list 'funcall func_name) func_params))) 
