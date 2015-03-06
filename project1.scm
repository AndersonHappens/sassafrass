; Jake Anderson, jta40
; Joseph Tate, jgt17
; Michael Volkovitsch, mtv25
; EECS 345, Project 1

(load "simpleParser.scm")

;state implemented as a list whose first element is the list of varnames, the second is the list of values

;parses and interprets the code in the given file
(define interpret
  (lambda (filename)
    (evaluate (parser filename) (newEnvironment))))

;defines the newEnvironment
(define newEnvironment
  (lambda ()
    '(()())
    ))

;evaluate the parse tree
(define evaluate
  (lambda (stmts state)
    (cond
      ((not (list? state)) state)
      ((null? stmts) state)
      (else (evaluate (cdr stmts) (M_state (firststmt stmts) state))))))


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
  (lambda (stmt state)
    ((lambda (stmt state stmttype)
       
      (cond
        ((eq? stmttype 'var) (M_state_var stmt state))
        ((eq? stmttype '=) (M_state_assign stmt state))
        ((eq? stmttype 'return) (M_state_return stmt state))
        ((eq? stmttype 'if) (M_state_if stmt state))
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
    (cons (cons var (vars (topLayer state))) (cons (cons val (vals (topLayer state))) '()))))

(define vars
  (lambda (layer)
    (car layer)))
(define vals
  (lambda (layer)
    (cadr layer)))

;removevar from the top layer, if present
(define removevar
  (lambda (var state)
    (cond
      ((null? (car (topLayer state))) '())
      ((eq? var (firstvarname (topLayer state))) (trimlayer (topLayer state)))
      (else (addvar (firstvarname (topLayer state)) (firstvarvalue (topLayer state)) (removevar var (trimlayer (topLayer state))))))))

;checks if a var is declared in the state
(define isdeclared?
  (lambda (varname state)
    (if (null? state)
        #f
        ((lambda (varval)
          (if varval)
              varval
              (isDeclared? varname (removeLayer state))))
         (isdeclaredinlayer? varname (topLayer state)))))
        

;checks if a var is declared in a layer
(define isdeclaredinlayer?
  (lambda (varname layer)
    (cond
      ((null? (car layer)) #f)
      ((eq? varname (firstvarname layer)) #t)
      (else (isdeclared? varname (trimlayer layer))))))
    
;returns the value assigned to varname in the state
(define M_value_var
  (lambda (varname state)
    (if (null? state)
        (error 'Variable_not_declared))
        ((lambda (varval)
          (if (null? varval)
              (M_value_var varname (removeLayer state))
              (varval)))
         (M_value_var_layer varname (topLayer state)))))

;returns the value assigned to varname in a layer
(define M_value_var_layer
  (lambda (varname layer)
    (cond
      ((null? (vars layer)) '()
      ((and (eq? varname (firstvarname layer)) (null? (firstvarvalue layer))) (error 'Variable_not_initialized))
      ((eq? varname (firstvarname layer)) (firstvarvalue layer))
      (else (M_value_var varname (trimlayer layer))))))

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
  (lambda (ifBlock state)
    (cond
      ((M_bool (condition ifBlock) state) (M_state (ifStmt ifBlock) state))
      ((noElseStmt ifBlock) state)
      (else (M_state (elseStmt ifBlock) state)))))

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
      (addvar (varName assignment) (M_value (expr assignment) state) (removevar (varName assignment) state))
      (error 'Variable_not_declared))))

; misc definitions for M_state_assign
(define varName
  (lambda (l)
    (cadr l)))
(define expr
  (lambda (l)
    (caddr l)))

;M_state_block
;the M_state function for blocks;
(define M_state_block
  (lambda (stmtlist state)
    (Remove-Layer((M_state_block stmtlist) (Add-layer state)))))

;while loop should not know anything about your layers
(define M_state_while
  (lambda (while state)
    (letrec ((loop (lambda (condition body state) (if (M_bool condition state)
                                                      (loop condition body (M_state body state))
                                                      state))))
      (loop (condition while) (body while) state))))

;misc while helper functions
(define condition
  (lambda while
    (cadr while)))

(define body
  (lambda while
    (caddr while)))
