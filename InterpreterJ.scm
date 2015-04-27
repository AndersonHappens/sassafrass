; EECS 345 Interpreter Project Part 4
; 13 February 2015

; This program uses continuation passing style whenever it needs to worry about running into a
; return, break, continue or throw statement. For example, it needs to watch out for return inside
; an if statement, but not an addition expression. And it does not use boxes. Other than that, it
; should be pretty standard. Passes all the given tests but not the extras.

(load "classParser.scm")

; Main program: interprets and executes the given file

(define interpret
  (lambda (filename classname)
    (Mvalue '(funcall main) (Mstate (parser filename) (state_init) 'noclass 'noinstance) (ensure_atom classname) 'noinstance)))

; --------------------------------
; ===== Mvalue functionality =====
; --------------------------------

; Get the value of an arbritrary syntax construct
(define Mvalue
  (lambda (syntax state classname instance)
    (Mvalue-cps syntax state classname instance noreturn nobreak nocontinue 'nothrow)))

(define Mvalue-cps
  (lambda (syntax state classname instance return break continue throw)
    (cond
      ((null? syntax) (error "Bad parse tree"))
      ((number? syntax) syntax)
      ((boolean? syntax) (Mbool syntax state classname instance))
      ((bool_literal? syntax) syntax)
      ((not (pair? syntax)) (state_var_value syntax state classname instance))
      ((null? (car syntax)) (error "Bad parse tree"))
      ((and (list? (car syntax)) (null? (cdr syntax))) (Mvalue-cps (car syntax) state classname instance return break continue throw))
      ((list? (car syntax)) (Mvalue-cps (cdr syntax) (Mstate-cps (car syntax) state classname instance return break continue throw) classname instance return break continue throw))
      ((pair? (car syntax)) (error "Bad parse tree"))
      (else (Mvalue_stmt-cps syntax state classname instance return break continue throw)))))

; Get the value of a single statement
; Precondition: (car syntax) is a valid operator
(define Mvalue_stmt-cps
  (lambda (syntax state classname instance return break continue throw)
    (cond
      ((eq? (operator syntax) '+) (Mvalue_+-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) '-) (Mvalue_--cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) '*) (Mvalue_*-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) '/) (Mvalue_/-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) '%) (Mvalue_%-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'var) (Mvalue_var-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'static-var) (Mvalue_var-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) '=) (Mvalue_=-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'return) (Mvalue_return-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'if) (Mvalue_if-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'begin) (Mvalue_begin-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'while) (Mvalue_while-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'funcall) (Mvalue_funcall-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'dot) (Mvalue_dot-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'throw) (throw (list 'throwvalue (Mvalue (onlyoperand syntax) state classname instance) (Mstate-cps (onlyoperand syntax) state classname instance return break continue throw))))
      (else (if (Mbool_stmt syntax state classname instance) 'true 'false)))))

; Get the value of a single statement with the named operator
(define Mvalue_+-cps
  (lambda (syntax state classname instance return break continue throw)
    (+ (Mvalue-cps (leftoperand syntax) state classname instance return break continue throw)
       (Mvalue-cps (rightoperand syntax) (Mstate-cps (leftoperand syntax) state classname instance return break continue throw) classname instance return break continue throw))))

(define Mvalue_--cps
  (lambda (syntax state classname instance return break continue throw)
    (if (unary? syntax)
        (- (Mvalue-cps (onlyoperand syntax) state classname instance return break continue throw))
        (- (Mvalue-cps (leftoperand syntax) state classname instance return break continue throw)
           (Mvalue-cps (rightoperand syntax) (Mstate-cps (leftoperand syntax) state classname instance return break continue throw) classname instance return break continue throw)))))

(define Mvalue_*-cps
  (lambda (syntax state classname instance return break continue throw)
    (* (Mvalue-cps (leftoperand syntax) state classname instance return break continue throw)
       (Mvalue-cps (rightoperand syntax) (Mstate-cps (leftoperand syntax) state classname instance return break continue throw) classname instance return break continue throw))))

(define Mvalue_/-cps
  (lambda (syntax state classname instance return break continue throw)
    (quotient (Mvalue-cps (leftoperand syntax) state classname instance return break continue throw)
              (Mvalue-cps (rightoperand syntax) (Mstate-cps (leftoperand syntax) state classname instance return break continue throw) classname instance return break continue throw))))

(define Mvalue_%-cps
  (lambda (syntax state classname instance return break continue throw)
    (remainder (Mvalue-cps (leftoperand syntax) state classname instance return break continue throw)
               (Mvalue-cps (rightoperand syntax) (Mstate-cps (leftoperand syntax) state classname instance return break continue throw) classname instance return break continue throw))))

(define Mvalue_var-cps 
  (lambda (syntax stat return break continue throw)
    (if (unary? syntax)
        'varspecialreturnvalue
        (Mvalue-cps (rightoperand syntax) state classname instance return break continue throw))))

(define Mvalue_=-cps
  (lambda (syntax state classname instance return break continue throw)
    (Mvalue-cps (rightoperand syntax) state classname instance return break continue throw)))

(define Mvalue_return-cps
  (lambda (syntax state classname instance return break continue throw)
    (return (cons (Mvalue-cps (onlyoperand syntax) state classname instance return break continue throw)
                  (Mstate-cps (onlyoperand syntax) state classname instance return break continue throw)))))

(define Mvalue_if-cps
  (lambda (syntax state classname instance return break continue throw)
    (cond
      ((Mbool-cps (ifcond syntax) state classname instance return break continue throw)
       (Mvalue-cps (ifthen syntax) (Mstate-cps (ifcond syntax) state classname instance return break continue throw) classname instance return break continue throw))
      ((trinary? syntax)
       (Mvalue-cps (ifelse syntax) (Mstate-cps (ifcond syntax) state classname instance return break continue throw) classname instance return break continue throw))
      (else 'ifspecialreturnvalue))))

(define Mvalue_begin-cps
  (lambda (syntax state classname instance return break continue throw)
    (Mvalue-cps (cdr syntax) state classname instance return break continue throw)))

(define Mvalue_while-cps
  (lambda (syntax state classname instance return break continue throw)
    (if (Mbool-cps (whilecond syntax) state classname instance return break continue throw)
        (Mvalue-cps syntax
                    (Mstate-cps (whilebody syntax)
                                (Mstate-cps (whilecond syntax)
                                            state
                                            classname
                                            instance
                                            return
                                            break
                                            continue
                                            throw)
                                classname
                                instance
                                return
                                break
                                continue
                                throw)
                    classname
                    instance
                    return
                    break
                    continue
                    throw)
        'whilefailvalue)))

(define Mvalue_funcall-cps
  (lambda (syntax state classname instance return break continue throw)
    (call/cc (lambda (return)
               (Mvalue-cps (func_closure_body (state_var_value (funcall_name syntax) state classname instance))
                           (state_add_params (func_closure_params (state_var_value (funcall_name syntax) state classname instance))
                                             (funcall_params syntax)
                                             ((func_closure_state_func (state_var_value (funcall_name syntax) state classname instance)) state)
                                             state
                                             (update_classname (funcall_name syntax) state classname instance)
                                             instance)
                           (update_classname (funcall_name syntax) state classname instance)
                           instance
                           (lambda (v) (return (car v)))
                           break
                           continue
                           (lambda (v) (throw (list (car v)
                                                    (cadr v)
                                                    (state_recombine (state_rollback (funcall_name syntax)
                                                                                     (caddr v)
                                                                                     classname
                                                                                     instance)
                                                                     state
                                                                     classname
                                                                     instance)))))))))

(define Mvalue_dot-cps
  (lambda (syntax state classname instance return break continue throw)
    (state_var_value syntax state classname instance)))

; --------------------------------
; ===== Mbool functionality ======
; --------------------------------

; TODO HACK
(define Mbool-cps
  (lambda (syntax state classname instance return break continue throw)
    (Mbool syntax state classname instance)))

; Get the boolean interpretation of an arbritrary syntax construct
(define Mbool
  (lambda (syntax state classname instance)
    (cond
      ((null? syntax) (error "Bad parse tree"))
      ((number? syntax) (error "Numbers cannot be used as boolean values"))
      ((boolean? syntax) syntax)
      ((eq? syntax 'true) #t)
      ((eq? syntax 'false) #f)
      ((not (pair? syntax)) (state_var_bool syntax state classname instance))
      ((null? (car syntax)) (error "Bad parse tree"))
      ((and (list? (car syntax)) (null? (cdr syntax))) (Mbool (car syntax) state classname instance))
      ((list? (car syntax)) (Mbool (cdr syntax) (Mstate (car syntax) state classname instance) classname instance))
      ((pair? (car syntax)) (error "Bad parse tree"))
      (else (Mbool_stmt syntax state classname instance)))))

; Get the boolean interpretation of a single statement
; Precondition: (car syntax) is a valid operator returning a value representable as a boolean
(define Mbool_stmt
  (lambda (syntax state classname instance)
    (cond
      ((eq? (operator syntax) '==) (Mbool_== syntax state classname instance))
      ((eq? (operator syntax) '!=) (Mbool_!= syntax state classname instance))
      ((eq? (operator syntax) '>) (Mbool_> syntax state classname instance))
      ((eq? (operator syntax) '<) (Mbool_< syntax state classname instance))
      ((eq? (operator syntax) '>=) (Mbool_>= syntax state classname instance))
      ((eq? (operator syntax) '<=) (Mbool_<= syntax state classname instance))
      ((eq? (operator syntax) '&&) (Mbool_&& syntax state classname instance))
      ((eq? (operator syntax) '||) (Mbool_|| syntax state classname instance))
      ((eq? (operator syntax) '!) (Mbool_! syntax state classname instance))
      ((eq? (operator syntax) 'if) (Mbool_if syntax state classname instance))
      ((eq? (operator syntax) 'begin) (Mbool_begin syntax state classname instance))
      ((eq? (operator syntax) 'while) (Mbool_begin syntax state classname instance))
      ((eq? (operator syntax) 'funcall) (Mbool_funcall syntax state classname instance))
      (else (error "Invalid operator")))))

; Get the boolean interpretation of a single statement with the named operator
(define Mbool_== (lambda (syntax state classname instance) (= (Mvalue (leftoperand syntax) state classname instance) (Mvalue (rightoperand syntax) (Mstate (leftoperand syntax) state classname instance) classname instance))))
(define Mbool_!= (lambda (syntax state classname instance) (not (= (Mvalue (leftoperand syntax) state classname instance) (Mvalue (rightoperand syntax) (Mstate (leftoperand syntax) state classname instance) classname instance)))))
(define Mbool_> (lambda (syntax state classname instance) (> (Mvalue (leftoperand syntax) state classname instance) (Mvalue (rightoperand syntax) (Mstate (leftoperand syntax) state classname instance) classname instance))))
(define Mbool_< (lambda (syntax state classname instance) (< (Mvalue (leftoperand syntax) state classname instance) (Mvalue (rightoperand syntax) (Mstate (leftoperand syntax) state classname instance) classname instance))))
(define Mbool_>= (lambda (syntax state classname instance) (>= (Mvalue (leftoperand syntax) state classname instance) (Mvalue (rightoperand syntax) (Mstate (leftoperand syntax) state classname instance) classname instance))))
(define Mbool_<= (lambda (syntax state classname instance) (<= (Mvalue (leftoperand syntax) state classname instance) (Mvalue (rightoperand syntax) (Mstate (leftoperand syntax) state classname instance) classname instance))))
(define Mbool_&& (lambda (syntax state classname instance) (and (Mbool (leftoperand syntax) state classname instance) (Mbool (rightoperand syntax) (Mstate (leftoperand syntax) state classname instance) classname instance))))
(define Mbool_|| (lambda (syntax state classname instance) (or (Mbool (leftoperand syntax) state classname instance) (Mbool (rightoperand syntax) (Mstate (leftoperand syntax) state classname instance) classname instance))))
(define Mbool_! (lambda (syntax state classname instance) (not (Mbool (onlyoperand syntax) state classname instance))))
(define Mbool_if (lambda (syntax state classname instance) (cond
                                                             ((Mbool (ifcond syntax) state classname instance) (Mbool (ifthen syntax) (Mstate (ifcond syntax) state classname instance) classname instance))
                                                             ((trinary? syntax) (Mbool (ifelse syntax) (Mstate (ifcond syntax) state classname instance) classname instance))
                                                             (else 'ifspecialreturnvalue))))
(define Mbool_begin (lambda (syntax state classname instance) (Mbool (cdr syntax) state classname instance)))
(define Mbool_while (lambda (syntax state classname instance) (if (Mbool (whilecond syntax) state classname instance)
                                                                  (Mbool syntax (Mstate (whilebody syntax) (Mstate (whilecond syntax) state classname instance) classname instance) classname instance)
                                                                  'whilefailvalue)))
(define Mbool_funcall
  (lambda (syntax state classname instance)
    (Mbool (Mvalue syntax state classname instance) (Mstate syntax state classname instance) classname instance)))

; --------------------------------
; ===== Mstate functionality =====
; --------------------------------

; Get the new state object following an arbritrary syntax construct
(define Mstate
  (lambda (syntax state classname instance)
    (Mstate-cps syntax state classname instance noreturn nobreak nocontinue 'nothrow)))

(define Mstate-cps
  (lambda (syntax state classname instance return break continue throw)
    (cond
      ((null? syntax) state)
      ((boolean? syntax) state)
      ((number? syntax) state)
      ((not (pair? syntax)) state)
      ((null? (car syntax)) (error "Bad parse tree"))
      ((and (list? (car syntax)) (null? (cdr syntax))) (Mstate-cps (car syntax) state classname instance return break continue throw))
      ((list? (car syntax)) (Mstate-cps (cdr syntax) (Mstate-cps (car syntax) state classname instance return break continue throw) classname instance return break continue throw))
      ((pair? (car syntax)) (error "Bad parse tree"))
      (else (Mstate_stmt-cps syntax state classname instance return break continue throw)))))

; Get the new state object following a single statement
; Precondition: (car syntax) is a valid operator returning a value

(define Mstate_stmt-cps
  (lambda (syntax state classname instance return break continue throw)
    (cond
      ((eq? (operator syntax) 'var) (Mstate_var-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'static-var) (Mstate_var-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) '=) (Mstate_=-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'if) (Mstate_if-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'begin) (Mstate_begin-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'while) (Mstate_while-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'return) (Mstate_return-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'break) (break state))
      ((eq? (operator syntax) 'continue) (continue state))
      ((eq? (operator syntax) 'function) (Mstate_function-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'static-function) (Mstate_function-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'funcall) (Mstate_funcall-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'class) (Mstate_class-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'throw) (Mstate_throw-cps syntax state classname instance return break continue throw))
      ((eq? (operator syntax) 'try) (Mstate_try-cps syntax state classname instance return break continue throw))
      ; now deal with every other operator
      ((unary? syntax) (Mstate-cps (onlyoperand syntax) state classname instance return break continue throw))
      ((binary? syntax) (Mstate-cps (rightoperand syntax) (Mstate-cps (leftoperand syntax) state classname instance return break continue throw) classname instance return break continue throw))
      (else state))))

; Get the new state object following a single statement with the named operator
(define Mstate_var-cps
  (lambda (syntax state classname instance return break continue throw)
    (if (unary? syntax)
        (state_add (onlyoperand syntax) state classname instance)
        (state_assign (leftoperand syntax)
                      (Mvalue-cps (rightoperand syntax)
                                  state
                                  classname
                                  instance
                                  return
                                  break
                                  continue
                                  throw)
                      (state_add (leftoperand syntax)
                                 (Mstate-cps (rightoperand syntax)
                                             state
                                             classname
                                             instance
                                             return
                                             break
                                             continue
                                             throw)
                                 classname
                                 instance)
                      classname
                      instance))))

(define Mstate_=-cps
  (lambda (syntax state classname instance return break continue throw)
    (state_assign (leftoperand syntax)
                  (Mvalue-cps (rightoperand syntax)
                              state
                              classname
                              instance
                              return
                              break
                              continue
                              throw)
                  (Mstate-cps (rightoperand syntax)
                              state
                              classname
                              instance
                              return
                              break
                              continue
                              throw)
                  classname
                  instance)))

(define Mstate_if-cps
  (lambda (syntax state classname instance return break continue throw)
    (cond
      ((Mbool-cps (ifcond syntax) state classname instance return break continue throw)
       (Mstate-cps (ifthen syntax) (Mstate-cps (ifcond syntax) state classname instance return break continue throw) classname instance return break continue throw))
      ((trinary? syntax)
       (Mstate-cps (ifelse syntax) (Mstate-cps (ifcond syntax) state classname instance return break continue throw) classname instance return break continue throw))
      (else (Mstate-cps (ifcond syntax) state classname instance return break continue throw)))))

(define Mstate_begin-cps
  (lambda (syntax state classname instance return break continue throw)
    (state_pop (Mstate-cps (cdr syntax)
                           (state_push state)
                           classname
                           instance
                           return
                           (lambda (v) (break (state_pop v)))
                           (lambda (v) (continue (state_pop v)))
                           (lambda (v) (throw v))))))

(define Mstate_while-cps
  (lambda (syntax state classname instance return break continue throw)
    (if (Mbool-cps (whilecond syntax) state classname instance return break continue throw)
        (call/cc (lambda (break) (Mstate-cps syntax
                                             (call/cc (lambda (continue) (Mstate-cps (whilebody syntax)
                                                                                     (Mstate-cps (whilecond syntax) state classname instance return break continue throw)
                                                                                     classname
                                                                                     instance
                                                                                     return
                                                                                     break
                                                                                     continue
                                                                                     throw)))
                                             classname
                                             instance
                                             return
                                             break
                                             nocontinue
                                             throw)))
        (Mstate-cps (whilecond syntax) state classname instance return break continue throw))))

(define Mstate_return-cps
  (lambda (syntax state classname instance return break continue throw)
    (return (cons (Mvalue-cps (onlyoperand syntax) state classname instance return break continue throw)
                  (Mstate-cps (onlyoperand syntax) state classname instance return break continue throw)))))

(define Mstate_function-cps
  (lambda (syntax state classname instance return break continue throw)
    (state_assign (function_name syntax)
                  (make_func_closure (function_name syntax)
                                     (function_params syntax)
                                     (function_body syntax)
                                     classname
                                     instance)
                  (state_add (function_name syntax) state classname instance)
                  classname
                  instance)))

(define Mstate_funcall-cps
  (lambda (syntax state classname instance return break continue throw)
    (state_recombine (state_rollback (funcall_name syntax)
                                     (call/cc (lambda (return)
                                                     (Mstate-cps (func_closure_body (state_var_value (funcall_name syntax) state classname instance))
                                                                 (state_add_params (func_closure_params (state_var_value (funcall_name syntax) state classname instance))
                                                                                   (funcall_params syntax)
                                                                                   ((func_closure_state_func (state_var_value (funcall_name syntax) state classname instance)) state)
                                                                                   state
                                                                                   (update_classname (funcall_name syntax) state classname instance)
                                                                                   instance)
                                                                 (update_classname (funcall_name syntax) state classname instance)
                                                                 instance
                                                                 (lambda (v) (return (cdr v)))
                                                                 break
                                                                 continue
                                                                 (lambda (v) (throw (list (car v)
                                                                                          (cadr v)
                                                                                          (state_recombine (state_rollback (funcall_name syntax)
                                                                                                                           (caddr v)
                                                                                                                           classname
                                                                                                                           instance)
                                                                                                           state
                                                                                                           classname
                                                                                                           instance)))))))
                                     classname
                                     instance)
                     state
                     classname
                     instance)))

(define Mstate_class-cps
  (lambda (syntax state classname instance return break continue throw)
    (state_assign (class_name syntax)
                  (make_class_closure (class_name syntax)
                                      (class_parent syntax)
                                      (class_body syntax)
                                      state
                                      classname
                                      instance)
                  (state_add (class_name syntax) state classname instance)
                  classname
                  instance)))

(define Mstate_throw-cps
  (lambda (syntax state classname instance return break continue throw)
    (throw (list 'throwvalue (Mvalue-cps (onlyoperand syntax) state classname instance return break continue throw) (Mstate-cps (onlyoperand syntax) state classname instance return break continue throw)))))

(define Mstate_try-cps
  (lambda (syntax state classname instance return break continue throw)
    (let ((result (call/cc (lambda (throw)
                             (Mstate-cps (try_body syntax)
                                         state
                                         classname
                                         instance
                                         return
                                         break
                                         continue
                                         throw)))))
      (if (throwvalue? result)
          (let ((newresult (state_pop (Mstate-cps (try_catch_body syntax)
                                                  (state_assign (try_catch_varname syntax)
                                                                (throwvalue_value result)
                                                                (state_add (try_catch_varname syntax)
                                                                           (state_push (state_strip (throwvalue_state result) state))
                                                                           classname
                                                                           instance)
                                                                classname
                                                                instance)
                                                  classname
                                                  instance
                                                  return
                                                  break
                                                  continue
                                                  throw))))
            (if (try_has_finally? syntax)
                (Mstate-cps (try_finally_body syntax)
                            newresult
                            classname
                            instance
                            return
                            break
                            continue
                            throw)
                newresult))
          (if (try_has_finally? syntax)
              (Mstate-cps (try_finally_body syntax)
                          result
                          classname
                          instance
                          return
                          break
                          continue
                          throw)
              result)))))

; ----------------------------------------------------
; ================ Function closures =================
; ----------------------------------------------------

(define make_func_closure
  (lambda (name params body classname instance)
    (list params
          body
          (lambda (state)
            (state_rollback name state classname instance)))))

(define func_closure_params car)
(define func_closure_body cadr)
(define func_closure_state_func caddr)


; ----------------------------------------------------
; ================== Class closures ==================
; ----------------------------------------------------

(define make_class_closure
  (lambda (name parent body state classname instance)
    (add_to_class (reverse body)
                  (list name
                        parent
                        (fields_init)
                        (methods_init)
                        (constructors_init))
                  state
                  classname
                  instance)))

(define fields_init (lambda () '()))
(define methods_init (lambda () '()))
(define constructors_init (lambda () '()))

(define class_closure_name car)
(define class_closure_parent cadr)
(define class_closure_fields caddr)
(define class_closure_methods cadddr)
(define class_closure_constructors
  (lambda (v)
    (car (cddddr v))))

(define add_to_class
  (lambda (syntax class state classname instance)
    (cond
      ((null? syntax) class)
      ((pair? (car syntax)) (add_to_class (car syntax) (add_to_class (cdr syntax) class state classname instance) state classname instance))
      ((eq? (operator syntax) 'var) (add_var_to_class syntax class state classname instance))
      ((eq? (operator syntax) 'static-var) (add_var_to_class syntax class state classname instance))
      ((eq? (operator syntax) 'function) (add_function_to_class syntax class state classname instance))
      ((eq? (operator syntax) 'static-function) (add_function_to_class syntax class state classname instance))
      (else (error "Only definitions allowed in class body")))))

(define add_var_to_class
  (lambda (syntax class state classname instance)
    (list (class_closure_name class)
          (class_closure_parent class)
          (if (unary? syntax)
              (add_field (onlyoperand syntax) (class_closure_fields class))
              (add_field (leftoperand syntax) (Mvalue (rightoperand syntax) (state_assign (class_closure_name class) class (state_add (class_closure_name class) state classname instance) classname instance) (class_closure_name class) 'noinstance) (class_closure_fields class)))
          (class_closure_methods class)
          (class_closure_constructors class))))

(define add_function_to_class
  (lambda (syntax class state classname instance)
    (list (class_closure_name class)
          (class_closure_parent class)
          (class_closure_fields class)
          (add_method (function_name syntax)
                      (make_func_closure (function_name syntax)
                                         (function_params syntax)
                                         (function_body syntax)
                                         (class_closure_name class)
                                         'noinstance)
                      (class_closure_methods class))
          (class_closure_constructors class))))

(define add_field
  (lambda (var fields)
    (cons (cons var 'nil) fields)))

(define add_field
  (lambda (var value fields)
    (cons (cons var value) fields)))

(define add_method
  (lambda (name closure methods)
    (cons (cons name closure) methods)))

(define assign_field
  (lambda (var value fields)
    (cond
      ((null? fields) (error "Field assigned before declaration"))
      ((eq? (caar fields) var) (cons (cons var value) (cdr fields)))
      (else (cons (car fields) (assign_field var value (cdr fields)))))))

(define field_value
  (lambda (var fields)
    (cond
      ((null? fields) (error "Field used before declaration"))
      ((eq? (caar fields) var)
       (if (eq? (cdar fields) 'nil)
           (error "Field used before assignment")
           (cdar fields)))
      (else (field_value var (cdr fields))))))

(define field_exists
  (lambda (var fields)
    (cond
      ((null? fields) #f)
      ((eq? (caar fields) var) #t)
      (else (field_exists var (cdr fields))))))

(define method_closure
  (lambda (var methods)
    (cond
      ((null? methods) (error "Method used before declaration"))
      ((eq? (caar methods) var)
       (if (eq? (cdar methods) 'nil)
           (error "Method used before assignment")
           (cdar methods)))
      (else (method_closure var (cdr methods))))))

(define method_exists
  (lambda (var methods)
    (cond
      ((null? methods) #f)
      ((eq? (caar methods) var) #t)
      (else (method_exists var (cdr methods))))))

(define class_fake_state
  (lambda (class)
    (state_assign (class_closure_name class) class (state_add (class_closure_name class) (state_init) 'noclass 'noinstance) 'noclass 'noinstance)))

; ----------------------------------------------------
; =================== Class access ===================
; ----------------------------------------------------

(define class_assign
  (lambda (field value class state classname instance)
    (list (class_closure_name class)
          (class_closure_parent class)
          (assign_field field value (class_closure_fields class))
          (class_closure_methods class)
          (class_closure_constructors class))))

(define class_field_value
  (lambda (field class state classname instance)
    (cond
      ((class_field_exists_noparent field class) (field_value field (class_closure_fields class)))
      ((class_has_parent class state classname instance) (class_field_value field (class_parent_closure class state 'noclass 'noinstance) state classname instance))
      (else (error "Field used before definition")))))


(define class_method_closure
  (lambda (method class state classname instance)
    (cond
      ((class_method_exists_noparent method class) (method_closure method (class_closure_methods class)))
      ((class_has_parent class state classname instance) (class_method_closure method (class_parent_closure class state 'noclass 'noinstance) state classname instance))
      (else (error "Method used before definition")))))

(define class_field_exists_noparent
  (lambda (field class)
    (field_exists field (class_closure_fields class))))

(define class_method_exists_noparent
  (lambda (method class)
    (method_exists method (class_closure_methods class))))

(define class_field_exists
  (lambda (field class state classname instance)
    (cond
      ((class_field_exists_noparent field class) #t)
      ((class_has_parent class state classname instance) (class_field_exists field (class_parent_closure class state 'noclass 'noinstance) state classname instance))
      (else #f))))

(define class_method_exists
  (lambda (method class state classname instance)
    (cond
      ((class_method_exists_noparent method class) #t)
      ((class_has_parent class state classname instance) (class_method_exists method (class_parent_closure class state 'noclass 'noinstance) state classname instance))
      (else #f))))

(define class_field/method_value
  (lambda (var class state classname instance)
    (cond
      ((class_field_exists var class state classname instance) (class_field_value var class state classname instance))
      ((class_method_exists var class state classname instance) (class_method_closure var class state classname instance)))))

(define class_field/method_exists
  (lambda (var class state classname instance)
    (or (class_field_exists var class state classname instance)
        (class_method_exists var class state classname instance))))

(define class_has_parent
  (lambda (class state classname instance)
    (not (null? (class_closure_parent class)))))

(define class_parent_closure
  (lambda (class state classname instance)
    (state_var_value (class_closure_parent class) state classname instance)))

; ----------------------------------------------------
; ===== Environment data structure functionality =====
; ----------------------------------------------------

; Get an empty state object
(define state_init (lambda () '(())))

; Declare a variable in the given state
; Dot expresions NOT allowed
(define state_add
  (lambda (var state classname instance)
    (cond
      ((dot_expr? var) (error "Dot expressions not allowed in variable definition"))
      ((pair? var) (error "Malformed variable in definition"))
      ((layer_var_exists? var (car state) classname instance) (error "Variable declared twice"))
      (else (cons (layer_add var (car state) classname instance) (cdr state))))))

; Assign a variable in the given state
; Dot expressions allowed
(define state_assign
  (lambda (var value state classname instance)
    (cond
      ((null? state) (error "Variable assigned before declaration"))
      ((dot_expr? var)
       (state_assign (dot_left var)
                     (class_assign (dot_right var)
                                   value
                                   (state_var_value (dot_left var) state classname instance)
                                   state
                                   classname
                                   instance)
                     state
                     classname
                     instance))
      ((pair? var) (error "Malformed variable in assignment"))
      ((layer_var_exists? var (car state) classname instance) (cons (layer_assign var value (car state) classname instance) (cdr state)))
      (else (cons (car state) (state_assign var value (cdr state) classname instance))))))

; Get the assigned value of a variable in the given state
; Dot expressions allowed
(define state_var_value
  (lambda (var state classname instance)
    (cond
      ((null? state) (error "Variable used before declaration"))
      ((super_expr? var)
       (class_field/method_value (dot_right var)
                                 (class_parent_closure (state_var_value classname state 'noclass 'noinstance) state 'noclass 'noinstance)
                                 state
                                 classname
                                 instance))
      ((dot_expr? var)
       (class_field/method_value (dot_right var)
                                 (state_var_value (dot_left var) state classname instance)
                                 state
                                 classname
                                 instance))
      ((pair? var) (error "Malformed variable referenced"))
      ((layer_var_exists? var (car state) classname instance) (layer_var_value var (car state) classname instance))
      (else (state_var_value var (cdr state) classname instance)))))

; Get the boolean interpretation of the assigned value of a variable in the given state
; Dot expressions allowed
(define state_var_bool
  (lambda (var state classname instance)
    (cond
      ((null? state) (error "Variable used before declaration"))
      ((dot_expr? var)
       (Mbool (class_field/method_value (dot_right var)
                                        (state_var_value (dot_left var) state classname instance)
                                        state
                                        classname
                                        instance)
              state
              classname
              instance))
      ((pair? var) (error "Malformed variable referenced"))
      ((layer_var_exists? var (car state) classname instance) (layer_var_bool var (car state) classname instance))
      (else (state_var_bool var (cdr state) classname instance)))))

; Push a new layer onto the given state
(define state_push
  (lambda (state)
    (cons (layer_init) state)))

; Pop the highest layer off the given state
(define state_pop
  (lambda (state)
    (cdr state)))

; Get the state in which the given variable was declared
; Dot expressions allowed
(define state_rollback
  (lambda (var state classname instance)
    (cond
      ((null? state) (error "Variable used before declaration"))
      ((dot_expr? var)
       (if (layer_var_exists? classname (car state) classname instance)
           state
           (state_rollback var (cdr state) classname instance)))
      ((layer_var_exists? var (car state) classname instance) state)
      (else (state_rollback var (cdr state) classname instance)))))

;----------------------

; Add function parameters to the state in a new layer, matching formal to actual
(define state_add_params
  (lambda (formal actual state_to_add state classname instance)
    (state_add_params_nolayer formal actual (state_push state_to_add) state classname instance)))

; Add function parameters to the state, matching formal to actual
(define state_add_params_nolayer
  (lambda (formal actual state_to_add state classname instance)
    (cond
      ((and (null? formal) (null? actual)) state_to_add)
      ((or (null? formal) (null? actual)) (error "Incorrect number of function parameters"))
      (else (state_assign (car formal)
                          (Mvalue (car actual) state classname instance)
                          (state_add (car formal)
                                     (state_add_params_nolayer (cdr formal)
                                                               (cdr actual)
                                                               state_to_add
                                                               state
                                                               classname
                                                               instance)
                                     classname
                                     instance)
                          classname
                          instance)))))

; Apply a changed state after a function call to the main state
(define state_recombine
  (lambda (func_state state classname instance)
    (cond
      ((> (length state) (length func_state)) (cons (car state) (state_recombine func_state (cdr state) classname instance)))
      ((= (length state) (length func_state)) func_state)
      (else (error "Internal: function state is too big")))))

(define state_strip
  (lambda (state_to_strip state)
    (cond
      ((> (length state_to_strip) (length state)) (state_strip (cdr state_to_strip) state))
      ((= (length state_to_strip) (length state)) state_to_strip)
      (else (error "Internal: state to strip is too small")))))

;------------------
; NOTE: for all layer functions, var CANNOT be a dot expression; that is handled by the state-level functions
; However, the layer functions are where the classname is handled

; Get an empty layer object
(define layer_init (lambda () '()))

; Declare a variable in the given layer
; Ignores classname
(define layer_add
  (lambda (var layer classname instance)
    (if (layer_var_exists? var layer classname instance)
        (error "Variable declared twice")
        (cons (cons var 'nil) layer))))

; Assign a variable in the given layer
(define layer_assign
  (lambda (var value layer classname instance)
    (cond
      ((null? layer) (error "Variable assigned before declaration"))
      ((eq? (caar layer) var) (cons (cons var value) (cdr layer)))
      ((and (eq? (caar layer) classname) (class_field_exists var (layer_var_value classname layer classname instance) (layer_fake_state layer) classname instance))
       (cons (cons classname (class_assign var value (layer_var_value classname layer classname instance) (layer_fake_state layer) classname instance)) (cdr layer)))
      (else (cons (car layer) (layer_assign var value (cdr layer) classname instance))))))

; Test if a variable is declared in the given layer
(define layer_var_exists?
  (lambda (var layer classname instance)
    (cond
      ((null? layer) #f)
      ((eq? (caar layer) var) #t)
      ((and (eq? (caar layer) classname) (class_field/method_exists var (layer_var_value classname layer classname instance) (layer_fake_state layer) classname instance)) #t)
      (else (layer_var_exists? var (cdr layer) classname instance)))))

; Get the assigned value of a variable in the given layer
(define layer_var_value
  (lambda (var layer classname instance)
    (cond
      ((null? layer) (error "Variable used before declaration"))
      ((eq? (caar layer) var)
       (if (eq? (cdar layer) 'nil)
           (error "Variable used before assignment")
           (cdar layer)))
      ((and (eq? (caar layer) classname) (class_field/method_exists var (layer_var_value classname layer classname instance) (layer_fake_state layer) classname instance))
       (class_field/method_value var (layer_var_value classname layer classname instance) (layer_fake_state layer) classname instance))
      (else (layer_var_value var (cdr layer) classname instance)))))

; Get the boolean interpretation of the assigned value of a variable in the given layer
(define layer_var_bool
  (lambda (var layer classname instance)
    (cond
      ((null? layer) (error "Variable used before declaration"))
      ((eq? (caar layer) var)
       (if (eq? (cdar layer) 'nil)
           (error "Variable used before assignment")
           (Mbool (cdar layer) (list layer) classname instance)))
      ((and (eq? (caar layer) classname) (class_field/method_exists var (layer_var_value classname layer classname instance) (layer_fake_state layer) classname instance))
       (Mbool (class_field/method_value var (layer_var_value classname layer classname instance) (layer_fake_state layer) classname instance) (list layer) classname instance))
      (else (layer_var_bool var (cdr layer) classname instance)))))

(define layer_fake_state
  (lambda (layer)
    (list layer)))

; -----------------------------
; ===== Utility functions =====
; -----------------------------

; Aliases for operator and operands
(define operator car)
; unary
(define onlyoperand cadr)
; binary
(define leftoperand cadr)
(define rightoperand caddr)
; trinary (if then else)
(define ifcond cadr)
(define ifthen caddr)
(define ifelse cadddr)
; while loop
(define whilecond cadr)
(define whilebody caddr)
; function definition
(define function_name cadr)
(define function_params caddr)
(define function_body cadddr)
; function call
(define funcall_name cadr)
(define funcall_params cddr)
; class definition
(define class_name cadr)
(define class_parent
  (lambda (syntax)
    (if (null? (caddr syntax))
        '()
        (cadr (caddr syntax)))))
(define class_body cadddr)
; dot expression
(define dot_left
  (lambda (syntax)
    (ensure_atom (leftoperand syntax))))
(define dot_right rightoperand)
(define ensure_atom
  (lambda (s)
    (if (string? s)
        (string->symbol s)
        s)))
; try/catch/finally statement
(define try_body cadr)
(define try_catch_varname
  (lambda (syntax)
    (caar (cdaddr syntax))))
(define try_catch_body
  (lambda (syntax)
    (cadr (cdaddr syntax))))
(define try_finally_body
  (lambda (syntax)
    (cadr (cadddr syntax))))
(define try_has_finally?
  (lambda (syntax)
    (not (null? (cadddr syntax)))))
(define throwvalue?
  (lambda (syntax)
    (eq? (car syntax) 'throwvalue)))
(define throwvalue_value cadr)
(define throwvalue_state caddr)

; Operator arity tests
(define arity (lambda (syntax) (- (length syntax) 1)))
(define unary? (lambda (syntax) (= (arity syntax) 1)))
(define binary? (lambda (syntax) (= (arity syntax) 2)))
(define trinary? (lambda (syntax) (= (arity syntax) 3)))

; Literals
(define bool_literal?
  (lambda (atom)
    (or (eq? atom 'true) (eq? atom 'false))))

;--------------------

(define dot_expr?
  (lambda (var)
    (and (pair? var) (eq? (operator var) 'dot))))

(define super_expr?
  (lambda (var)
    (and (dot_expr? var) (eq? (dot_left var) 'super))))

(define update_classname
  (lambda (syntax state classname instance)
    (cond
      ((super_expr? syntax) (update_classname (dot_right syntax)
                                              state
                                              (class_closure_parent (state_var_value classname state 'noclass 'noinstance))
                                              instance))
      ((dot_expr? syntax) (update_classname (dot_right syntax) state (dot_left syntax) instance))
      (else classname))))

;--------------------

;(define noreturn (lambda (v) 'noreturn))
;(define nobreak (lambda (v) 'nobreak))
;(define nocontinue (lambda (v) 'nocontinue))
;(define nothrow (lambda (v) 'nothrow))

(define noreturn (lambda (v) v))
(define nobreak (lambda (v) v))
(define nocontinue (lambda (v) v))
(define nothrow (lambda (v) v))

;(interpret "test4-14.txt" "A")
