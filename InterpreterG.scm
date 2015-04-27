(load "classParser.scm")

; Interpret: Returns the "return value" of a series of expressions.
(define interpret
  (lambda (file classname)
    (let* ((stmts (parser file))
           (classenv (addclasses stmts (makeclassshells stmts emptystate)))
           (runstate (copypastarino (string->symbol classname) classenv (string->symbol classname))))
      (return-erator (Mv_function (cadr (unbox (findaspair 'main runstate))) '() runstate classenv (string->symbol classname))))))

; Ms_stmts: Breaks a list of multiple statements into singular statements to be interpreted by Ms_stmt.
(define Ms_stmts
  (lambda (stmts state classenv instance return break continue try)
    (cond
      ((null? stmts) state) ; No return value here. This is because the program ended without a return call.
      (else (Ms_stmts (cdr stmts) 
                      (Ms_stmt (car stmts) state classenv instance return break continue try) classenv instance return break continue try)))))

; Ms_stmt: Determines what kind of statement is being considered, and calls the appropriate functions to handle them.
(define Ms_stmt
  (lambda (stmt state classenv instance return break continue try)
    (cond
      ((null? stmt) (error "Null statement"))
      ((declare? stmt) (Ms_declare stmt state classenv instance))
      ((assign? stmt) (Ms_assign stmt state classenv instance))
      ((elseif? stmt) (Ms_elseif stmt state return break continue try))
      ((if? stmt) (Ms_if stmt state classenv instance return break continue try))
      ((while? stmt) (Ms_while stmt state classenv instance return break continue try))
      ((block? stmt) (Ms_block stmt state classenv instance return break continue try)) ; this could potentially not be what you want, but im not sure where the cps stuff comes in
      ((return? stmt) (Ms_return stmt state classenv instance return))
      ((break? stmt) (break state))
      ((continue? stmt) (continue state))
      ((funcdefine? stmt) (Ms_function stmt state))
      ((try? stmt) (Ms_finally stmt state classenv instance return break continue try))
      ((throw? stmt) (try (exception (cadr stmt))))
      (else (begin (Mv_stmt stmt state classenv instance) state)))))

; Ms_declare: Adds a variable to the state, with or without a value; also checks if the variable is already defined.
(define Ms_declare
  (lambda (stmt state classenv instance)
    (cond
      ;((instate? (cadr stmt) state) (error "Cannot initialize the same variable twice"))
      ((null? (cddr stmt)) (addtostate (cadr stmt) state))
      (else (addpairtostate (cadr stmt) (Mv_stmt (caddr stmt) state classenv instance) state)))))

; Ms_assign: Adds a value to a variable already existing in the state; checks if the value is the correct type, and that the variable is already defined.
(define Ms_assign
  (lambda (stmt state classenv instance)
    (cond
      ((not (instate? (cadr stmt) state)) (error "Cannot do assignment on undefined var"))
      ((not (eqtype? (caddr stmt) (car (unbox (findaspair (cadr stmt) state))))) (error "Assignment with invalid type"))
      (else (replace (cadr stmt) (Mv_stmt (caddr stmt) state classenv instance) state)))))

; Ms_elseif: If the initial statement is true, using Mv_stmt, then the next statement is evaluated, otherwise the third statement is evaluated.
(define Ms_elseif
  (lambda (stmt state return break continue try)
    (cond
      ((Mv_stmt (cadr stmt) state) (Ms_stmt (caddr stmt) state return break continue try))
      (else (Ms_stmt (cadddr stmt) state return break continue try)))))

; Ms_if: If the initial statement evaluates to true, using Mv_stmt, then the next statement is evaluated, otherwise the state is not changed.
(define Ms_if
  (lambda (stmt state classenv instance return break continue try)
    (cond
      ((Mv_stmt (cadr stmt) state classenv instance) (Ms_stmt (caddr stmt) state return break continue try))
      (else state))))

; Ms_return: Adds a return "variable" with the program's return value binded to it, so that later it can be literally returned by the interpeter.
(define Ms_return
  (lambda (stmt state classenv instance return)
    (return (Mv_stmt (cadr stmt) state classenv instance))))


; Mv_stmt: Functions very similarly to Ms_stmt, except for the functions that are called to handle the expressions determine the expression's value
;          also returns value of basic numbers, booleans, and variables
(define Mv_stmt
  (lambda (stmt state classenv instance )
    (cond
      ((null? stmt) (error "Null statement"))
      ((number? stmt) stmt)
      ((bool? stmt) (eq? stmt 'true))
      ((name? stmt state) (cadr (unbox (findaspair stmt state))))
      ((expression? stmt) (Mv_expression stmt state classenv instance))
      ((boolexpr? stmt) (Mv_boolexpr stmt state classenv instance))
      ((function? stmt) (Mv_function 
                         (Mv_stmt (functionname stmt) state classenv instance) (map (lambda (v) (Mv_stmt v state classenv instance)) (cddr stmt)) state classenv instance))
      ((dotexpr? stmt) (cadr (unbox (findaspair (dotname stmt) (copypastarino (dotclass stmt) classenv instance)))))
      ((name? stmt (copypastarino (supper instance classenv) classenv instance)) (cadr (unbox (findaspair stmt (copypastarino (supper instance classenv) classenv instance)))))
      (else (error "Statememnt undefined" stmt)))))

; Mv_expression: Determines what type of mathematical expression an input is, then breaks it apart to be used by Mv_stmt and applies mathematical operators
(define Mv_expression
  (lambda (stmt state classenv instance)
    (cond
      ((plus? (operator stmt)) (+ (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      ((unaryminus? (operator stmt) (cddr stmt)) (- (Mv_stmt (loperand stmt) state classenv instance)))
      ((minus? (operator stmt)) (- (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      ((times? (operator stmt)) (* (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      ((divide? (operator stmt)) (quotient (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      ((modulo? (operator stmt)) (remainder (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      (else (error "Invalid operator")))))

; Mv_boolexpr: Determines what type of boolean expression an input is, utilizes Mv_stmt to determine its values and applies the logical operators
(define Mv_boolexpr
  (lambda (stmt state classenv instance)
    (cond
      ((equals? (operator stmt)) (eq? (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      ((noteq? (operator stmt)) (not (eq? (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance))))
      ((greateroreq? (operator stmt)) (>= (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      ((lessoreq? (operator stmt)) (<= (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      ((greaterthan? (operator stmt)) (> (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      ((lessthan? (operator stmt)) (< (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      ((not? (operator stmt)) (not (Mv_stmt (loperand stmt) state classenv instance)))
      ((or? (operator stmt)) (or (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      ((and? (operator stmt)) (and (Mv_stmt (loperand stmt) state classenv instance) (Mv_stmt (roperand stmt) state classenv instance)))
      (else (error "Not a valid boolean expression")))))

; plus?: Function that defines if an atom is the + symbol.
(define plus?
  (lambda (v)
    (eq? v '+)))
; unaryminus?: Function that defines if a pair of atoms constitutes a unary minus.
(define unaryminus?
  (lambda (v1 v2)
    (and (eq? v1 '-) (null? v2))))
; minus?: Function that defines if an atom is the - symbol.
(define minus?
  (lambda (v)
    (eq? v '-)))
; times?: Function that defines if an atom is the * symbol.
(define times?
  (lambda (v)
    (eq? v '*)))
; divide?: Function that defines if an atom is the / symbol.
(define divide?
  (lambda (v)
    (eq? v '/)))
; modulo?: Function that defines if an atom is the % symbol.
(define modulo?
  (lambda (v)
    (eq? v '%)))
; equals?: Function that defines if an atom is the == symbol.
(define equals?
  (lambda (v)
    (eq? v '==)))
; lessoreq?: Function that defines if an atom is the <= symbol.
(define lessoreq?
  (lambda (v)
    (eq? v '<=)))
; greateroreq?: Function that defines if an atom is the >= symbol.
(define greateroreq?
  (lambda (v)
    (eq? v '>=)))
; lessthan?: Function that defines if an atom is the < symbol.
(define lessthan?
  (lambda (v)
    (eq? v '<)))
; greaterthan?: Function that defines if an atom is the > symbol.
(define greaterthan?
  (lambda (v)
    (eq? v '>)))
; not?: Function that defines if an atom is the ! symbol.
(define not?
  (lambda (v)
    (eq? v '!)))
; noteq?: Funciton that defines if an atom is the != symbol.
(define noteq?
  (lambda (v)
    (eq? v '!=)))
; or?: Function that defines if an atom is the || symbol.
(define or?
  (lambda (v)
    (eq? v '||)))
; and?: Function that defines if an atom is the && symbol.
(define and?
  (lambda (v)
    (eq? v '&&)))

; if?: Function that defines if an atom is the word if.
(define if?
  (lambda (stmt)
    (eq? (car stmt) 'if)))

; elseif?: Function that returns true if a statement has an if as well as a secondary else statement.
(define elseif?
  (lambda (stmt)
    (and (if? stmt) (not (null? (cdddr stmt))))))

; bool?: Function that determines if a statement is a boolean value.
(define bool?
  (lambda (stmt)
    (or (eq? stmt 'true) (eq? stmt 'false))))

; boolexpr?: Function that determines if an expression has a boolean operator in it.
(define boolexpr?
  (lambda (stmt)
    (and (list? stmt) (inlist? (car stmt) boolhead))))

; boolhead: Defines all the valid symbols for boolean expressions.
(define boolhead '(== > < >= <= ! != || &&))

; return?: Determines if a statement is a return statement.
(define return?
  (lambda (stmt)
    (eq? (car stmt) 'return)))

; name?: Determines if a statement is the name of a preexisting variable.
(define name?
  (lambda (stmt state)
    (instate? stmt state)))

; expression?: Determines if a statement is a mathematical expression.
(define expression?
  (lambda (stmt)
    (cond
      ((not (list? stmt)) #f)
      (else (inlist? (car stmt) expressionhead)))))

; expressionhead: Defines the list of all valid mathematical operators.
(define expressionhead '(+ - / * %))

; declare?: Determines if a statement is a declaration statement.
(define declare?
  (lambda (stmt)
    (cond
      ((not (list? stmt)) #f)
      (else (eq? (car stmt) var)))))

; var: Defines the atom 'var;
(define var 'var)

; assign?: Determines if a statement is an assignment.
(define assign?
  (lambda (stmt)
    (cond
      ((not (list? stmt)) #f)
      (else (eq? (car stmt) '=)))))

; addpairtostate: Function that adds a variable name and value pair to the list that is the state.
(define addpairtostate
  (lambda (var value state)
    (cons (box (list var value)) state)))

; addtostate: Function that adds just a variable name to the state, therefore it is defined, but not assigned a value.
(define addtostate
  (lambda (var state)
    (cons (box (list var)) state)))

; removefromstate: Searches for and removes the first instance of a variable in the state.
(define removefromstate
  (lambda (var state)
    (cond
      ((null? state) state)
      ((inlist? var (car state)) (cdr state))
      (else (cons (car state) (removefromstate var (cdr state)))))))

; instate?: Determines if a variable name is in the state.
(define instate?
  (lambda (var state)
    (not (eq? (findaspair var state) #f))))

; eqtype?: This is where we would do some type checking, but we are assuming no malformed inputs are made, and the parser should do this anyways.     
(define eqtype?
  (lambda (t1 t2)
    #t))

; findaspair: Searches for a variable within the state; if it exists the variable is returned along with its value (as a pair), otherwise false is returned
(define findaspair
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((list? (car l)) (findaspair a (car l)))
      ((inlist? a (unbox (car l))) (car l))
      (else (findaspair a (cdr l))))))

; inlist?: Determines whether or not an atom is within a list.
(define inlist? 
  (lambda (a l)
    (not (eq? (member a l) #f))))

; return-erator: Function that maps #t to true and #f to false; otherwise it returns the input.
(define return-erator
  (lambda (a)
    (cond
      ((eq? a #t) 'true)
      ((eq? a #f) 'false)
      (else a))))

;*****************************************************************************************************************************************;
;----------------------------------------------------------Part 2-------------------------------------------------------------------------;
;*****************************************************************************************************************************************;

; operator: Function that abstracts the function call to return the operator of a statement
(define operator
  (lambda (l)
    (car l)))

; loperand: Function that abstracts the function call to return the left operand of a statement
(define loperand
  (lambda (l)
    (cadr l)))

; roperand: Function that abstracts the function call to return the right operand of a statement
(define roperand
  (lambda (l)
    (caddr l)))

; emptystate: Function that abstracts what the empty state actually is
(define emptystate
  '())

; addlayer: Function that abstracts the addition of a new layer onto the current state. The outermost state-space, or most general,
;           will be the innermost list of the state, and the outermost list of the state will be the current block.
(define addlayer
  (lambda (currentstate)
    (cons currentstate emptystate)))

; removelayer: Function that abstracts leaving, or subtracting, a layer from the current state. When a block is exited in the program,
;            the next inner state-space will be returned as the current state; thus deleting all the variables declared in the previous block
(define removelayer
  (lambda (currentstate)
    (cond
      ((null? currentstate) (error "No layer to remove, already at outermost layer"))
      ((null? (car currentstate)) '())
      ((list? (car currentstate)) (car currentstate))
      (else (removelayer (cdr currentstate))))))

; while? : Determines if a statement is a while loop
(define while?
  (lambda (stmt)
    (eq? (car stmt) 'while)))

; block? : Determines if a statement is entering a new block
(define block?
  (lambda (stmt)
    (eq? (car stmt) 'begin)))

; Ms_while: Much like if, using Mv_stmt if the condition is true the body is evaluated, otherwise the state is unchanged
(define Ms_while
  (lambda (stmt state classenv instance return break continue try)
    (cond
      ((block? (caddr stmt))
       (call/cc (lambda (newBreak)
                  (cond
                    ((Mv_stmt (cadr stmt) state classenv instance)
                     (Ms_while stmt (call/cc (lambda (newContinue) (Ms_stmt (caddr stmt) state classenv instance return newBreak newContinue))) classenv instance return break continue try))
                    (else state)))))
      (else
       (call/cc (lambda (newBreak)
                  (cond
                    ((Mv_stmt (cadr stmt) state)
                     (Ms_stmt stmt (call/cc (lambda (newContinue) (Ms_stmt (caddr stmt) state return newBreak newContinue))) return break continue try))
                    (else state))))))))

; Ms_block: Adds a new layer to the state, because a block has been entered, calls Ms_stmts on the innards, then pops
;           the layer off after the insides of the block statement have been evaluated; this returning to the outside state
(define Ms_block
  (lambda (stmts state classenv instance return break continue try)
    (removelayer 
     (letrec ((inner_part (lambda (stmts state return break continue try)
                            (cond
                              ((null? stmts) state)
                              (else (inner_part (cdr stmts) (Ms_stmt (car stmts) state classenv instance return break continue try) return break continue try))))))
       (inner_part (cdr stmts) (addlayer state) return break continue try)))))

;********************************************************************************************************;
;****************************************Part 2 Continued************************************************;
;********************************************************************************************************;

; nullContinuation: Error function that can be used to throw meaningful errors
(define nullContinuation
  (lambda (v)
    (error "Break/Continue used out of context. No loop to break/continue to.")))

; break?: Determines if a statement is a break statement
(define break?
  (lambda (stmt)
    (cond
      ((eq? (car stmt) 'break) #t)
      (else #f))))

; continue?: Determines if a statement is a continue statement
(define continue?
  (lambda (stmt)
    (cond
      ((eq? (car stmt) 'continue) #t)
      (else #f))))

; replace: Finds a preexisting variable in the state and updates its value binding to a new one
(define replace
  (lambda (x v state)
    (cond
      ((null? state) state)
      ((list? (car state)) (cons (replace x v (car state)) '()))
      ((inlist? x (unbox (car state))) (begin (set-box! (car state) (list x v)) state))
      (else (cons (car state) (replace x v (cdr state)))))))

;********************************************************************************************************;
;***********************************************PART 3***************************************************;
;********************************************************************************************************;




; closure?: function that determines if a name's value pair is a closure
(define closure?
  (lambda (v)
    (pair? (cdr v))))

; makeclosure: function that creates a closure
(define makeclosure
  (lambda (paramlist body state static?)
    (list static? paramlist body (lambda (v) (getlayer v (currentdepth state))))))

; functionclosurefunction: given a function closure, this function returns the function that generates the correct environment
;                          to execute the function whos closure it is? :D
(define functionclosurefunction
  (lambda (v)
    (cadddr v)))

; Ms_params: adds parameters to the state
(define Ms_params
  (lambda (state params values cleanstate classenv instance)
    (cond
      ((and (null? params) (null? values)) state)
      ((null? params) (error "Too many parameters provided"))
      ((null? values) (error "Not enough parameters provided"))
      (else (Ms_params (addpairtostate (car params) (Mv_stmt (car values) cleanstate classenv instance) state) (cdr params) (cdr values) cleanstate classenv instance))))) 

; Mv_function: determines the value of a function
(define Mv_function
  (lambda (closure values state classenv instance)
    (call/cc (lambda (return) (Ms_stmts (cbody closure) 
                                        (Ms_params (addlayer ((cfunc closure) state)) (cparamlist closure) values state classenv instance) classenv instance
                                        return nullContinuation nullContinuation nullContinuation)))))

; currentdepth: returns the numerical "depth" of a state, or the number of layers put on it
(define currentdepth
  (lambda (state)
    (cond
      ((null? state) 0)
      ((list? (car state)) (+ 1 (currentdepth (car state))))
      (else (currentdepth (cdr state))))))

; getlayer: given the state and a depth value, this function returns the layer at the specified depth
(define getlayer
  (lambda (state layer)
    (letrec ((siftdown
              (lambda (state depth)
                (cond
                  ((zero? depth) state)
                  ((list? (car state)) (siftdown (car state) (- depth 1)))
                  (else (siftdown (cdr state) depth))))))
      (siftdown state (- (currentdepth state) layer)))))

; cparamlist: Function that returns the parameter list out of a function closure
(define cparamlist
  (lambda (c)
    (cadr c)))
; cbody: Function that returns the body out of a function closure
(define cbody
  (lambda (c)
    (caddr c)))
; cfunc: Function that returns 
(define cfunc
  (lambda (c)
    (cadddr c)))

; function?: Determines if a statement is a function call
(define function?
  (lambda (stmt)
    (and (list? stmt) (eq? (car stmt) functioncall))))

; funcdefine?: Determines if a statement is a function definition
(define funcdefine?
  (lambda (stmt)
    (eq? (car stmt) functionstart)))

; Ms_funciton: Adds a function to the state, for when it is declared.
(define Ms_function
  (lambda (stmt state)
    (addpairtostate (functionname stmt) 
                    (makeclosure (paramlist stmt) (functionbody stmt) state) state)))



;****************************************** Part 3 continued ***************************************;

; functionstart: abstracts the atom that denotes the start of a function declaration
(define functionstart
  'function)

; functioncall: abstracts the atom that denotes the start of a function call
(define functioncall
  'funcall)

; functionname: given the list containing a function declaration, this will retrieve the name
(define functionname
  (lambda (l)
    (cadr l)))

; paramlist: given the list containing a function declaration, this will return the parameter list
(define paramlist
  (lambda (l)
    (caddr l)))

; functionbody: given the list containing a function declaration, this will return the body of the function
(define functionbody
  (lambda (l)
    (cadddr l)))

; addouterfunctions: Adds the outermost functions to the state, to be in a global scope
(define addouterfunctions
  (lambda (stmts state classenv instance) 
    (cond
      ((null? stmts) state)
      ((and (list? (car stmts)) (eq? (caar stmts) functionstart)) 
       (addouterfunctions (cdr stmts) (addpairtostate (functionname (car stmts)) 
                                                     (makeclosure (paramlist (car stmts)) (functionbody (car stmts)) state #f) state) classenv instance))
      ((and (list? (car stmts)) (staticfuncdefine? (car stmts))) 
       (addouterfunctions (cdr stmts) (addpairtostate (functionname (car stmts)) 
                                                     (makeclosure (paramlist (car stmts)) (functionbody (car stmts)) state #t) state) classenv instance))
      ((staticdeclare? (car stmts)) (addouterfunctions (cdr stmts) (Ms_declare (car stmts) state classenv instance) classenv instance))
      (else (addouterfunctions (cdr stmts) state))))) 

;****************************************************************************************************;
;----------------------------------Part 4 Incoming---------------------------------------------------;
;****************************************************************************************************;

; classdef?: Determines if a statement is a class definition
(define classdef?
  (lambda (stmt)
    (eq? (car stmt) 'class)))

; classname: Given the list containing a class definition, this returns the name of the class
(define classname
  (lambda (stmt)
    (cadr stmt)))

; classparent: Given the list containing a class definition, this returns the parent of the class
(define classparent
  (lambda (stmt)
    (cond
      ((null? (caddr stmt)) '())
      (else (cdaddr stmt)))))

; classbody: Given the list containing a class definition, this returns the body of the class
(define classbody
  (lambda (stmt)
    (cadddr stmt)))

; staticdeclare?: Determines if a statement is a static variable declaration 
(define staticdeclare?
  (lambda (stmt)
    (eq? (car stmt) 'static-var)))

; staticfuncdefine?: Determines if a statement is a static function definition
(define staticfuncdefine?
  (lambda (stmt)
    (eq? (car stmt) 'static-function)))

; staticfunction?: Determines if a statement is a static function call
(define staticfunction?
  (lambda (stmt)
    (and (eq? (car stmt) functionstart) (list? (functioname stmt)))))

; staticname?: Determines if a statement is a static variable reference
;              This is supposed to look up class name from the call, and 
;              call instate on the body of the class to determine if the 
;              variable exists
(define staticname?
  (lambda (stmt state)
    (instate (cddar stmt) (findaspair (cdar stmt) state))))

; throw?: Determines if a statement is a throw statement
(define throw?
  (lambda (stmt)
    (eq? (car stmt) 'throw)))

; try?: Determines if a statement is the start of a try block
(define try?
  (lambda (stmt)
    (eq? (car stmt) 'try)))

; trybody: Returns the body of the try statement
(define trybody
  (lambda (stmt)
    (cadr stmt)))

; catch?: Determines if there is a catch statement
(define catch?
  (lambda (stmt)
    (pair? (cddar stmt))))

; catcherror: Returns the error to be caught by this catch
;             I dont think this is necessary because there are no types, so only one catch block
(define catcherror
  (lambda (stmt)
    (caadr (caddr stmt))))

; catchbody: Returns the body of the catch statement
(define catchbody
  (lambda (stmt)
    (caddr (caddr stmt))))

; finally?: Determines if there is a finally statement
(define finally?
  (lambda (stmt)
    (pair? (cdddar stmt))))

; finallybody: Returns the body of the finally statement
(define finallybody
  (lambda (stmt)
    (cond
      ((null? (cadddr stmt)) '())
      (else (cadr (cadddr stmt))))))

; getparamnames: Returns a list of bindings from static fields to their values
(define getstaticfieldbindings
  (lambda (classbody)
    (cond
      ((null? classbody) '())
      ((not (staticdeclare? (car classbody))) (getstaticfieldbindings (cdr classbody)))
      ((null? (cddar classbody)) (cons (list (cadar classbody) 'void) (getstaticfieldbindings (cdr classbody))))
      (else (cons (list (cadar classbody) (caddar classbody)) (getstaticfieldbindings (cdr classbody)))))))

; getparamnames: Returns a list of bindings from static fields to their values
(define getfieldbindings
  (lambda (classbody)
    (cond
      ((null? classbody) '())
      ((not (declare? (car classbody))) (getfieldbindings (cdr classbody)))
      ((null? (cddar classbody)) (cons (list (cadar classbody) 'void) (getfieldbindings (cdr classbody))))
      (else (cons (list (cadar classbody) (caddar classbody)) (getfieldbindings (cdr classbody)))))))

(define makeclassshells
  (lambda (stmts state)
    (cond
      ((null? stmts) state)
      (else (addpairtostate (classname (car stmts)) (list (classparent (car stmts)) '()) (makeclassshells (cdr stmts) state))))))

(define addliteraldeclares
  (lambda (classbody)
    (cond
      ((null? classbody) classbody)
      ((and (staticdeclare? (car classbody))))))) 

; addclasses: Adds all the class definitions to the environment
(define addclasses
  (lambda (stmts state)
    (cond
      ((null? stmts) state)
      (else (replace (classname (car stmts)) (makeclass (car stmts) state) (addclasses (cdr stmts) state))))))

; makeclass: Constructs the 4-tuple for the class, instances reference these
(define makeclass
  (lambda (stmt classenv)
    (list (classparent stmt)
          (addouterfunctions (classbody stmt) emptystate classenv (classname stmt)))))

; creates a new instance of an object. Is just a list containing (run-time-type compile-time-type field-values)
(define makeinstance
  (lambda (classname args state)
    (list classname classname (cadddr (cadr (unbox (findaspair classname state)))))))

; runtimetype: returns the run time type of an instance
(define runtimetype
  (lambda (instance)
    (car instance)))

; compiletimetype: returns the compile time type of an instance
(define compiletimetype
  (lambda (instance)
    (cadr instance)))

; classstate: takes a class and returns a state contianing the static fields and all the functions
(define classstate
  (lambda (c)
    (cadr c)))

; copypastarino: takes a classname, the class environment, and an instance and returns a new state that contains
; all the informaiton in the class state
(define copypastarino
  (lambda (classname classenv instance)
    (cond
      ((eq? classname 'super) (classstate (cadr (unbox (findaspair (supper instance classenv) classenv)))))
      (else (classstate (cadr (unbox (findaspair classname classenv))))))))

; checks if an expression is a dot expression
(define dotexpr?
  (lambda (stmt)
    (and (list? stmt) (eq? (car stmt) 'dot))))

; returns the class referenced by a dot expression
(define dotclass
  (lambda (stmt)
    (cadr stmt)))

; returns the field or funciton in a class referenced by a dot expression
(define dotname
  (lambda (stmt)
    (caddr stmt)))

; supper: (aka super) takes a class name and a class environment and returns the super class of the given class or the class
; if no super class exists
(define supper
  (lambda (classname classenv)
    (cond
      ((not (null? (car (cadr (unbox (findaspair classname classenv)))))) (caar (cadr (unbox (findaspair classname classenv)))))
    (else classname))))

; exception: packages an exception so it can be identified later
(define exception
  (lambda (arg)
    (list 'exception arg)))

; checks if e is an exception
(define exception?
  (lambda (e)
    (cond
      ((not (list? e)) #f)
      (else (eq? (car e) 'exception)))))

; Ms_finally: returns the state after a finally block has been exicuted. Since the finally block always runs this is called first.
; *I realize that this function should also do something with the return break and continue continuations to stop them from
; bypassing the finally block*
(define Ms_finally
  (lambda (stmt state classenv instance return break continue try)
    (Ms_stmts (finallybody stmt) (Ms_try stmt state classenv instance return break continue try) classenv instance return break continue try)))

; Ms_try: returns the state after a try block has been exicuted. Applies the catch block if an exception was thrown
(define Ms_try
  (lambda (stmt state classenv instance return break continue try)
    (let ((e (call/cc (lambda (newtry) (addlayer (Ms_stmts (trybody stmt) state classenv instance return break continue newtry))))))
      (cond
        ((exception? e) (Ms_catch stmt state classenv instance return break continue try (cadr e)))
        (else (removelayer e))))))

; Ms_catch: returns the state after a catch block has been exicuted.
(define Ms_catch
  (lambda (stmt state classenv instance return break continue try exception)
    (removelayer (Ms_stmts (catchbody stmt) (addpairtostate (catcherror stmt) exception (addlayer state)) classenv instance return break continue try))))
