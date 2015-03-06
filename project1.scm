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

;defines the newEnvironment consisting of 1 layer in a list
(define newEnvironment
  (lambda ()
    '((()()))
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
        ((eq? stmttype 'while) (M_state_while stmt state))
        ((eq? stmttype 'begin) (M_state_block stmt state))
        ((eq? stmttype 'break) (M_state_break state))
        ((eq? stmttype 'continue) (M_state_continue state))
        (else (error 'Invalid_stmt_type))))
      stmt state (stmttype stmt))))

;M_state_block
(define M_state_block
  (lambda (stmt state)
    (removeLayer ((M_state (cdr smtmt) (addLayer state))))))

;M_state_break
(define M_state_break
  (lambda (state)
    (break state)))

;M_state_continue
(define M_state_continue
  (lambda (state)
    (continue state)))

;topLayer
(define topLayer
  (lambda (state)
    (car state)))

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

;addvar adds a var and it's initial value ('() if undefined) to state
(define addvar
  (lambda (var val state)
    (cons (cons var (vars state)) (cons (cons val (vals state)) '()))))

(define vars
  (lambda (state)
    (car state)))
(define vals
  (lambda (state)
    (cadr state)))

;removevar
(define removevar
  (lambda (var state)
    (cond
      ((null? (car state)) '())
      ((eq? var (firstvarname state)) (trimstate state))
      (else (addvar (firstvarname state) (firstvarvalue state) (removevar var (trimstate state)))))))
       
;checks if a var is declared in state
(define isdeclared?
  (lambda (varname state)
    (cond
      ((null? (car state)) #f)
      ((eq? varname (firstvarname state)) #t)
      (else (isdeclared? varname (trimstate state))))))
    

;returns the value assigned to varname in state
(define M_value_var
  (lambda (varname state)
    (cond
      ((null? (vars state)) (error 'Variable_not_declared))
      ((and (eq? varname (firstvarname state)) (null? (firstvarvalue state))) (error 'Variable_not_initialized))
      ((eq? varname (firstvarname state)) (firstvarvalue state))
      (else (M_value_var varname (trimstate state))))))

;trims the first var entry from the state
(define trimstate
  (lambda (state)
     (cons (cdr (car state)) (cons (cdr (cadr state)) '()))))

(define firstvarname
  (lambda (state)
    (car (car state))))

(define firstvarvalue
  (lambda (state)
    (car (cadr state))))

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

;braces can go anywhere! not just in loops and conditions... :P
;only thing that should know about layers
(define Mstate
  (lambda (stmtlist state)
    (Remove-Layer((Mstate stmtlist) (Add-layer state)))))
;while loop should not know anything about your layers
(define M_state_while
  (lambda (condition body state return)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (condition body state)
                                (if (M_bool condition state)
                                    (loop condition body (Mstate body state return))
                                    state))))
                 (call/cc (lambda (continue)
                            (loop condition body state))))))))
