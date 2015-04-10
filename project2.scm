; Jake Anderson, jta40
; Joseph Tate, jgt17
; Michael Volkovitsch, mtv25
; EECS 345, Project 2

(load "simpleParser.scm")

;state implemented as a list whose first element is the list of varnames, the second is the list of values

;parses and interprets the code in the given file
(define interpret
  (lambda (filename)
    (evaluate (parser filename) (newEnvironment) (lambda (v) v) (lambda (v) v))))
;lambda (v) v as placeholders for continue and break, acts as do nothing until in a loop.

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
  (lambda (stmts state continue break)
    (cond
      ((not (list? state)) state)
      ((null? stmts) state)
      (else (evaluate (cdr stmts) (M_state (firststmt stmts) state continue break) continue break)))))


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
  (lambda (stmt state continue break)
    ((lambda (stmt state stmttype)
      (cond
        ((eq? stmttype 'var) (M_state_var stmt state))
        ((eq? stmttype '=) (M_state_assign stmt state))
        ((eq? stmttype 'return) (M_state_return stmt state))
        ((eq? stmttype 'if) (M_state_if stmt state continue break))
        ((eq? stmttype 'while) (M_state_while stmt state))
        ((eq? stmttype 'begin) (M_state_block stmt state continue break))
        ((eq? stmttype 'break) (M_state_break state break))
        ((eq? stmttype 'continue) (M_state_continue state continue))
        (else (error 'Invalid_stmt_type))))
      stmt state (stmttype stmt))))

;M_state_var
(define M_state_var
  (lambda (stmt state)
    (cond
      ((and (isdeclared? (cadr stmt) state) (null? (cddr stmt))) state)
      ((isdeclared? (cadr stmt) state) (error 'Redefining_Variable))
      ((null? (cddr stmt)) (addvar (cadr stmt) '() state))
      (else (addvar (cadr stmt) (M_value (caddr stmt) state) state)))))

;M_state_return
;checks if it's a boolean statement or number statement and returns the correct evaluation of the statement
(define M_state_return
  (lambda (exp s)
    (cond
      ((boolean? (M_bool (cadr exp) s)) (boolReturnHelper (M_bool (car (cdr exp)) s)))
      ((number? (M_value (cadr exp) s)) (M_value (car (cdr exp)) s))
      (else (M_value (cadr exp) s)))))

; handles returning true and false instead of #t and #f
(define boolReturnHelper
  (lambda (bool)
    (if bool
      'true
      'false)))

;addvar adds a var and it's initial value ('() if undefined) to state at the top level
(define addvar
  (lambda (var val state)
    (cons (cons (cons var (vars (topLayer state))) (cons (cons val (vals (topLayer state))) '())) (cdr state))))

;add var to specific layer for when someone is reassigning a value
(define addvarlayer
  (lambda (var val layer)
    (cons (cons var (vars layer)) (cons (cons val (vals layer)) '()))))

;updates the value of a var
(define updatevar
  (lambda (var val state)
    (updatevar2 var val state (lambda (v) v))))

; helper method for updating a variable
(define updatevar2
  (lambda (var val state return)
    (if (isdeclaredinlayer? var (topLayer state))
        (return (cons (updatevarlayer var val (topLayer state)) (removeLayer state)))
        (updatevar2 var val (removeLayer state) (lambda (v) (return (cons (topLayer state) v)))))))

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
  (lambda (varname state)
    (if (null? state)
        (error 'Variable_not_declared))
        ((lambda (varval)
          (if (null? varval)
              (M_value_var varname (removeLayer state))
              varval))
         (M_value_var_layer varname (topLayer state)))))

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
    (car (cadr layer))))

; M_value, handles +,-,*,/,% and calls M_value_var if it finds a variable
(define M_value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((not (list? expression)) (M_value_var expression state))
      ((eq? '+ (operator expression)) (+ (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((eq? '% (operator expression)) (remainder (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((and (eq? '- (operator expression)) (null? (cddr expression))) (- 0 (M_value (lOperand expression) state)))
      ((eq? '- (operator expression)) (- (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      ((eq? '* (operator expression)) (* (M_value (lOperand expression) state) (M_value (rOperand expression) state)))
      (else (M_bool expression state)))))

; M_boolean, handles conditionals and equality ==, !=, <, >, <=, >=
(define M_bool
  (lambda (expression state)
    (cond
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
  (lambda (ifBlock state continue break)
    (cond
      ((M_bool (condition ifBlock) state) (M_state (ifStmt ifBlock) state continue break))
      ((noElseStmt ifBlock) state)
      (else (M_state (elseStmt ifBlock) state continue break)))))

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
      (error 'Variable_not_declared))))

; misc definitions for M_state_assign
(define varName
  (lambda (l)
    (cadr l)))
(define expr
  (lambda (l)
    (caddr l)))

;M_State_while, handles the while loop with continues and breaks.
(define M_state_while
  (lambda (while state)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (condition body state)
                                (if (M_bool condition state)
                                    (loop condition body (call/cc (lambda (continue) (M_state body state continue break))))
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
  (lambda (stmt state continue break)
    (removeLayer (evaluate (cdr stmt) (addLayer state) (lambda (v) (continue (removeLayer v))) (lambda (v) (break (removeLayer v)))))))

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

