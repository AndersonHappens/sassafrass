; EECS345 - Interpreter Part 4 rev 1
(load "classParser.scm")

#| FIX ALL function calls to pass in exception. |#

#|
    TODO FIFO QUEUE
    get try return working right
    check state for functions too
|#

; Main entry point into the code.
; Calls the parser, and processes the input from there.
; Note: implemented in cps style
(define interpret
  (lambda (filename classname)
    (M_state2 (reverse-cps (parser filename) (lambda (v) v)) 
             (lambda (return_state) 
               (polish_out (M_eval_main (string->symbol classname) return_state))) ; return
             )))

; ========== Utilities ==========

(define reverse-cps
  (lambda (l return)
    (cond
      ((null? l) (return '()))
      (else (reverse-cps (cdr l) (lambda (l2) (return (append-cps l2 (cons (car l) '()) (lambda (v) v)))))))))

(define append-cps
  (lambda (l1 l2 return)
    (cond
      ((null? l1) (return l2))
      (else (append-cps (cdr l1) l2 (lambda (l) (return (cons (car l1) l))))))))

(define polish_out
  (lambda (val)
    (cond
      ((eq? #t val) 'true)
      ((eq? #f val) 'false)
      (else val))))

(define M_eval_main
  (lambda (class_name state)
    (cond
      ((type_func 'int (list 'funcall (list 'dot class_name 'main)) (get_value class_name state) 'global state) 
       (M_value (list 'funcall (list 'dot class_name 'main)) 
                (get_value class_name state) ; class 
                'global ; instance 
                state
                (lambda (excep) (error "M_eval_main: uncaught exception"))))
      ((type_func 'bool (list 'funcall (list 'dot class_name 'main)) (get_value class_name state) 'global state) 
       (M_bool (list 'funcall (list 'dot class_name 'main)) 
               (get_value class_name state) ; class 
               'global ; instance 
               state
               (lambda (excep) (error "M_eval_main: uncaught exception"))))
      (else (error "M_eval_main: main doesn't fit type")))))

; ========== M_state ==========

(define M_state2 ; outer wrapper, reads in classes
  (lambda (arg_list return)
    (cond
    ((null? arg_list) (return (new_state)))
    ((is_class_def? (car arg_list)) (M_s2_class_def (car arg_list) (cdr arg_list) return))
    (else (M_state2 (cdr arg_list) return)))))
     
(define M_s2_class_def
  (lambda (arg rest_of_args return)
    (M_state2 rest_of_args (lambda (mod_state)
                             (cond
                               ((null? (get_class_parent arg))
                                (return (set_binding (get_class_name arg)
                                                     (create_class2
                                                      (get_class_name arg)
                                                      '()
                                                      (get_class_body arg))
                                                     mod_state)))
                               (else
                                (return (set_binding (get_class_name arg) 
                                                     (create_class2
                                                      (get_class_name arg)
                                                      (get_parent_state (cadr (get_class_parent arg)) mod_state)
                                                      (get_class_body arg))
                                                     mod_state))))))))

(define get_parent_state
  (lambda (class_name state)
    (get_value class_name state)))

; ========= M_class ==========
; originally named for M_state, but now it modifies the class instead of the state
; The main M_state function. It "handles" arguments, and passes them to the correct sub-function.
; Please note: All functions with M_s are class modifying functions designed to handle specific argument cases.

(define M_state 
  (lambda (arg_list class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg_list) (return state)) ; if there are no args, then return the state (no changes)
      ((not (is_valid_state? state)) (error "M_state: invalid state"))
      ((is_return? (car arg_list)) 
       (M_s_return (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_static_function_def? (car arg_list)) 
       (M_s_function_define (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_function_def? (car arg_list)) 
       (M_s_function_define (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_function_call? (car arg_list)) 
       (M_s_function_call (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_static_declare? (car arg_list)) 
       (M_s_declare (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_declare? (car arg_list)) 
       (M_s_declare (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_static_declare_assign? (car arg_list)) 
       (M_s_declare_assign (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_declare_assign? (car arg_list)) 
       (M_s_declare_assign (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_if_then? (car arg_list)) 
       (M_s_if_then (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_if_then_else? (car arg_list)) 
       (M_s_if_then_else (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_assign? (car arg_list)) 
       (M_s_assign (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_while? (car arg_list)) 
       (M_s_while (car arg_list) (cdr arg_list) class instance state return exception top_return))
      ((is_block? (car arg_list)) 
       (M_s_block (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_break? (car arg_list)) 
       (loop_break (M_state (cdr arg_list) class instance state (lambda (v) v) loop_break loop_continue exception top_return)))
      ((is_continue? (car arg_list)) 
       (loop_continue (M_state (cdr arg_list) class instance state (lambda (v) v) loop_break loop_continue exception top_return)))
      ((is_try_catch? (car arg_list)) 
       (M_s_try_catch2 (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_throw? (car arg_list))
       (M_s_throw (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      ((is_class_def? (car arg_list)) 
       (M_s_class_define (car arg_list) (cdr arg_list) class instance state return loop_break loop_continue exception top_return))
      (else (return state)))))

; M_state for a return element in the parse tree
(define M_s_return
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_return: null arg"))
      (else (M_state rest_of_args class instance state (lambda (mod_state)
                                          (cond
                                            ((number? mod_state) (top_return mod_state))
                                            ((not (is_valid_state? mod_state)) (error "M_s_return: invalid mod_state"))
                                            ((type_check 'bool (cadr arg) class instance mod_state) (top_return (M_bool (cadr arg) class instance mod_state exception)))
                                            ((type_check 'int (cadr arg) class instance mod_state) (top_return (M_value (cadr arg) class instance mod_state exception)))
                                            ((type_check 'func (cadr arg) class instance mod_state)
                                             (cond
                                               ((type_func 'int (get_func_name (cadr arg)) class instance mod_state) (top_return (M_value (cadr arg) class instance mod_state exception)))
                                               ((type_func 'bool (get_func_name (cadr arg)) class instance mod_state) (top_return (M_bool (cadr arg) class instance mod_state exception)))
                                               (else (error "M_s_return: function return did not match type"))))
                                            ((type_check 'init (cadr arg) class instance mod_state) (error "M_s_return: argument (var?) not assigned before return."))
                                            ((type_check 'error (cadr arg) class instance mod_state) (error "M_s_return: argument (var?) not declared before return."))
                                            (else (error "M_s_return: did not match pattern"))))
                     loop_break 
                     loop_continue
                     exception
                     top_return)))))

; M_state for a function declaration

(define M_s_function_define
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_function_define: null arg"))
      (else (M_state rest_of_args class instance state (lambda (mod_state)
                                          (cond
                                            ((not (is_valid_state? mod_state)) (error "M_s_function_define: invalid state"))
                                            ((is_not_reserved? (cadr arg)) (return (set_binding (get_func_name arg) 
                                                                                                (create_function (get_func_name arg) 
                                                                                                                 (get_func_params arg) 
                                                                                                                 (get_func_body arg) 
                                                                                                                 mod_state) 
                                                                                                mod_state)))
                                            (else (error "M_s_function_define: did not match pattern. May be reserved name"))))
                     loop_break 
                     loop_continue 
                     exception
                     top_return)))))

(define M_s_function_call
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_function_call: null arg"))
      ((not (is_function_call? arg)) (error "M_s_function_call: arg not a function call"))
      (else (M_state rest_of_args class instance state (lambda (mod_state)
                                          (cond
                                            ((not (is_valid_state? mod_state)) (error "M_s_function_call: invalid state"))
                                            ((is_dot_func? arg) 
                                             (cond
                                               ((is_this_dot? (cadr arg)) ; eval in current class/instance
                                                ;M_v_function_call (caddr (cadr arg)) class instance state exception))
                                                (evaluate_function (get_func (caddr (cadr arg)) class)
                                                                   (get_func_args arg)
                                                                   class instance
                                                                   state 
                                                                   exception
                                                                   (lambda (function_return)
                                                                     (return mod_state))))
                                               ((is_super_dot? (cadr arg))
                                                ;M_v_function_call (caddr (cadr arg)) (get_class_parent class) instance state exception))
                                                (evaluate_function (get_func (caddr (cadr arg)) (get_class_parent class))
                                                                   (get_func_args arg)
                                                                   (get_class_parent class) instance
                                                                   state 
                                                                   exception
                                                                   (lambda (function_return)
                                                                     (return mod_state))))
                                               (else
                                                ;M_v_function_call (append-cps (list 'funcall (caddr (cadr arg))) (cddr arg) (lambda (v) v)) (get_value (cadr (cadr arg)) state) instance state exception)
                                                (evaluate_function (get_func (caddr (cadr arg)) (get_value (cadr (cadr arg)) state))
                                                                   (get_func_args arg)
                                                                   (get_value (cadr (cadr arg)) state) instance
                                                                   state 
                                                                   exception
                                                                   (lambda (function_return)
                                                                     (return mod_state))))
                                                ))
                                            ((eq? (get_func (get_func_name arg) class) 'error)
                                                  (evaluate_function (get_value (get_func_name arg) mod_state)
                                                                     (get_func_args arg)
                                                                     class instance
                                                                     state 
                                                                     exception
                                                                     (lambda (function_return)
                                                                       (return mod_state))))
                                            (else (evaluate_function (get_func (get_func_name arg) class)
                                                                     (get_func_args arg)
                                                                     class instance
                                                                     state 
                                                                     exception
                                                                     (lambda (function_return)
                                                                       (return mod_state)))) ; with modify, etc. this will return the updated state
                                            ))
                     loop_break
                     loop_continue
                     exception
                     top_return)))))

; M_state for a declaration element in the parse tree
(define M_s_declare
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_declare: null arg"))
      (else (M_state rest_of_args class instance state (lambda (mod_state)
                                          (cond
                                            ((not (is_valid_state? mod_state)) (error "M_s_declare: invalid state"))
                                            ((is_not_reserved? (cadr arg)) (return (set_binding (cadr arg) 'init mod_state)))
                                            (else (error "M_s_declare: did not match pattern. May be reserved name"))))
                     loop_break 
                     loop_continue 
                     exception
                     top_return)))))

; M_state for an assignment statement in the parse tree
(define M_s_assign
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_assign: null arg"))
      (else (M_state rest_of_args class instance state (lambda (mod_state)
                                          (cond
                                            ((not (is_valid_state? mod_state)) (error "M_s_assign: invalid state"))
                                            ((eq? 'error (get_var (cadr arg) class)) 
                                             (cond
                                               ((eq? 'error (get_value (cadr arg) mod_state)) (display arg) (display mod_state) (error "M_s_assign: variable not yet declared (using before declaring)"))
                                               ((type_check 'bool (caddr arg) class instance mod_state) 
                                                (return (modify_binding3 (cadr arg) (M_bool (caddr arg) class instance mod_state exception) class instance mod_state)))
                                               ((type_check 'int (caddr arg) class instance mod_state) 
                                                (return (modify_binding3 (cadr arg) (M_value (caddr arg) class instance mod_state exception) class instance mod_state)))
                                               ((type_check 'func (caddr arg) class instance mod_state)
                                                (cond
                                                  ((type_func 'bool (get_func_name (caddr arg)) class instance mod_state)
                                                   (return (modify_binding3 (cadr arg) (M_bool (caddr arg) class instance mod_state exception) class instance mod_state)))
                                                  ((type_func 'int (get_func_name (caddr arg)) class instance mod_state)
                                                   (return (modify_binding3 (cadr arg) (M_value (caddr arg) class instance mod_state exception) class instance mod_state)))))
                                               (else (error "M_s_assign: did not match pattern"))))
                                            ((type_check 'bool (caddr arg) class instance mod_state) 
                                             (cond
                                               ((eq? 'done (modify_member (cadr arg) (M_bool (caddr arg) class instance mod_state exception) 'bool class))
                                                (return mod_state))
                                               (else (error "M_s_assign: modify_member didn't like it"))))
                                            ((type_check 'int (caddr arg) class instance mod_state) 
                                             (cond
                                               ((eq? 'done (modify_member (cadr arg) (M_value (caddr arg) class instance mod_state exception) 'int class))
                                                (return mod_state))
                                               (else (display cadr arg) (display class) (error "M_s_assign: modify_member didn't like it"))))
                                            ((type_check 'func (caddr arg) class instance mod_state)
                                             (cond ; TODO might need modding
                                               ((type_func 'bool (get_func_name (caddr arg)) class instance mod_state)
                                                (return (modify_binding3 (cadr arg) (M_bool (caddr arg) class instance mod_state exception) class instance mod_state)))
                                               ((type_func 'int (get_func_name (caddr arg)) class instance mod_state)
                                                (return (modify_binding3 (cadr arg) (M_value (caddr arg) class instance mod_state exception) class instance mod_state)))))
                                            (else (error "M_s_assign: did not match pattern"))))
                     loop_break 
                     loop_continue 
                     exception
                     top_return)))))

; M_state function for a declaration/assignment element in the parse tree.
; It handles this be restructuring the argument into two elements and putting them back into the parse tree.
;  This ensures that all declaration and assignment implementations/uses are consistent.
(define M_s_declare_assign
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_declare_assign: null arg"))
      ; handle declare+assign by putting the declare and assign into the list of args and calling M_state on it.
      (else (M_state (cons (cons '= (cdr arg)) (cons (list (car arg) (cadr arg)) rest_of_args)) 
                     class instance
                     state 
                     (lambda (mod_state) (return mod_state)) 
                     loop_break 
                     loop_continue 
                     exception
                     top_return)))))

; M_state function for an if ? then : else ternary operator.
(define M_s_if_then_else
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_if_then_else: null arg"))
      (else (M_state rest_of_args class instance
                     state (lambda (mod_state)
                             (cond
                               ((not (is_valid_state? mod_state)) (error "M_s_if_then_else: invalid state"))
                               ((not (type_check 'bool (get_conditional arg) class instance mod_state)) (error "M_s_if_then_else: condition not a bool"))
                               ((M_bool (get_conditional arg) class instance mod_state exception) (M_state (list (get_stmt1 arg)) class instance
                                                                                                           mod_state (lambda (mod_state2)
                                                                                                                       (return mod_state2))
                                                                                                           loop_break
                                                                                                           loop_continue
                                                                                                           exception
                                                                                                           top_return))
                               (else (M_state (list (get_stmt2 arg)) class instance 
                                              mod_state (lambda (mod_state2) 
                                                          (return mod_state2)) 
                                              loop_break 
                                              loop_continue 
                                              exception
                                              top_return))))
                     loop_break 
                     loop_continue 
                     exception
                     top_return)))))

; M_state function an if ? then ternary operator with no else. Does not change state if condition is false.
(define M_s_if_then 
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_if_then: null arg"))
      (else (M_state rest_of_args class instance 
                     state (lambda (mod_state)
                             (cond
                               ((not (is_valid_state? mod_state)) (error "M_s_if_then: invalid state"))
                               ((not (type_check 'bool (get_conditional arg) class instance mod_state)) (error "M_s_if_then: condition not a bool"))
                               ((M_bool (get_conditional arg) class instance mod_state exception) (M_state (list (get_stmt1 arg)) class instance
                                                                                                           mod_state (lambda (mod_state2) 
                                                                                                                       (return mod_state2)) 
                                                                                                           loop_break
                                                                                                           loop_continue
                                                                                                           exception
                                                                                                           top_return))
                               (else (return mod_state))))
                     loop_break 
                     loop_continue 
                     exception
                     top_return)))))

; M_state function for a while loop.
; The function operates as follows:
;  - define break and continue continuations with a let statement
;  - evaluate the condition:
;     #t - put the body back onto the parse tree to be evaluated by M_state (in cps)
;     #f - return up the (evaluated + functions) tree to the end of the while block
(define M_s_while
  (lambda (arg rest_of_args class instance state return exception top_return)
    (cond
      ((null? arg) (error "M_s_while: null arg"))
      (else 
       ; use let to define the loop break and continue for this loop
       (let ((loop_break (lambda (break_state) (return break_state))) ; break returns the state passed into the break
             (loop_continue (lambda (continue_state) ; continue applies the loop to the state passed into the continue
                              (cond
                                ((M_bool (get_conditional arg) class instance continue_state exception) (M_s_while arg 
                                                                                                                   (list (get_while_body arg))
                                                                                                                   class instance
                                                                                                                   continue_state 
                                                                                                                   (lambda (mod_state2)                                 
                                                                                                                     (return mod_state2))
                                                                                                                   exception
                                                                                                                   top_return))
                                (else (return continue_state))))))
         ; let body
         (M_state rest_of_args class instance state (lambda (mod_state)
                                       (cond
                                         ((not (is_valid_state? mod_state)) (error "M_s_while: invalid state"))
                                         ((not (type_check 'bool (get_conditional arg) class instance mod_state)) (error "M_s_while: condition not a bool"))
                                         ((M_bool (get_conditional arg) class instance mod_state exception) (M_s_while arg 
                                                                                                                       (list (get_while_body arg)) 
                                                                                                                       class instance
                                                                                                                       mod_state 
                                                                                                                       (lambda (mod_state2) 
                                                                                                                                          (return mod_state2))
                                                                                                                       exception
                                                                                                                       top_return))
                                         (else (return mod_state))))
                  loop_break
                  loop_continue
                  exception
                  top_return))))))

; M_state function for a block of code.
; This function is responsible for adding and removing layers.
; Layers are added on the function's first cps pass through.
; They are either removed at the completion of the block or during a break/continue step.
(define M_s_block
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_block: null arg"))
      (else (M_state rest_of_args class instance state (lambda (mod_state)
                                          (cond
                                            ((not (is_valid_state? mod_state)) (error "M_s_block: invalid state"))
                                            (else (M_state (reverse-cps (get_block_body arg) (lambda (v) v)) 
                                                           class instance
                                                           (add_layer (new_layer) mod_state) 
                                                           (lambda (mod_state2) 
                                                             (return (remove_layer mod_state2)))
                                                           (lambda (break_state) (loop_break (remove_layer break_state)))
                                                           (lambda (continue_state) (loop_continue (remove_layer continue_state)))
                                                           exception
                                                           top_return))))
                     loop_break 
                     loop_continue 
                     exception
                     top_return)))))

(define M_s_try_catch2
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_try_catch2: null arg"))
      ((not (is_valid_state? state)) (error "M_s_try_catch2: invalid state"))
      (else
       (let* 
           ((process_tf (lambda (mod_state) (M_state (reverse-cps (get_finally_args arg) (lambda (v) v))
                                                     class instance
                                                     mod_state
                                                     (lambda (v) v) ; mod_state
                                                     (lambda (v) v) ; break_state
                                                     (lambda (v) v) ; continue_state
                                                     exception
                                                     top_return)))
            (process_tcf (lambda (state_with_excep) (process_tf 
                                                     (M_state (reverse-cps (get_catch_args arg) (lambda (v) v))
                                                              class instance
                                                              state_with_excep
                                                              (lambda (v) v) ; mod_state
                                                              (lambda (v) v) ; break_state
                                                              (lambda (v) v) ; continue_state
                                                              exception
                                                              top_return)))))
       (M_state rest_of_args class instance state (lambda (mod_state) 
                                                    (cond
                                                      ((not (is_valid_state? mod_state)) (error "M_s_try_catch2: invalid state"))
                                                      (else (M_state (reverse-cps (get_try_args arg) (lambda (v) v))
                                                                     class instance
                                                                     mod_state
                                                                     (lambda (mod_state2)
                                                                       (return (process_tf mod_state2)))
                                                                     (lambda (break_state)
                                                                       (loop_break (process_tf break_state)))
                                                                     (lambda (continue_state)
                                                                       (loop_continue (process_tf break_state)))
                                                                     (lambda (exception_val)
                                                                       (return (process_tcf (set_binding (car (cadr (caddr arg))) exception_val mod_state))))
                                                                     (lambda (return_val) (begin (process_tf mod_state) (top_return return_val)))))))
                loop_break
                loop_continue
                exception
                top_return))))))
    
(define M_s_try_catch
  (lambda (arg rest_of_args class instance state try_return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_try_catch: null arg"))
      ((not (is_valid_state? state)) (error "M_s_try_catch: invalid state"))
      (else 
       (let*
           ((finally (lambda (f_state return loop_break loop_continue exception top_return)
                       (cond
                         ((not (is_valid_state? f_state)) (error "finally: invalid state"))
                         ((null? (get_finally_args arg)) f_state) ; if there are no args in finally return
                         (else (M_state (reverse-cps (get_finally_args arg) (lambda (v) v))
                                        class instance
                                        f_state
                                        (lambda (mod_state) (return mod_state))
                                        (lambda (break_state) (loop_break break_state))
                                        (lambda (continue_state) (loop_continue continue_state))
                                        (lambda (exception_val) (exception exception_val))
                                        top_return)))))
            (catch (lambda (mod_state return loop_break loop_continue exception top_return)
                     (cond
                         ((null? (get_catch_args arg)) mod_state) ; if there are no args in catch finally
                         (else (M_state (reverse-cps (get_catch_args arg) (lambda (v) v))
                                        class instance
                                        mod_state
                                        (lambda (mod_state) (return mod_state))
                                        (lambda (break_state) (loop_break break_state))
                                        (lambda (continue_state) (loop_continue continue_state))
                                        exception
                                        (lambda (val_return) (top_return val_return))))))))
         (M_state rest_of_args class instance state (lambda (mod_state)
                                       (cond
                                         ((not (is_valid_state? mod_state)) (error "M_s_block: invalid state"))
                                         (else (M_state (reverse-cps (get_try_args arg) (lambda (v) v))
                                                        class instance
                                                        mod_state
                                                        (lambda (mod_state2) (try_return (finally mod_state2 try_return loop_break loop_continue exception top_return)))
                                                        (lambda (break_state) (loop_break (finally break_state try_return loop_break loop_continue exception top_return)))
                                                        (lambda (continue_state) (loop_continue (finally continue_state try_return loop_break loop_continue exception top_return)))
                                                        (lambda (exception_val) 
                                                          ;
                                                          ;
                                                          (catch (set_binding (car (cadr (caddr arg))) exception_val mod_state)
                                                                        (lambda (caught_state) (try_return (finally caught_state
                                                                                                                (lambda (final_state) (try_return final_state))
                                                                                                                (lambda (break_state) (loop_break break_state))
                                                                                                                (lambda (continue_state) (loop_continue continue_state))
                                                                                                                (lambda (exception_val) (exception exception_val))
                                                                                                                (lambda (val_return) (top_return val_return)))))
                                                                        (lambda (break_state) (loop_break (finally break_state
                                                                                                                (lambda (final_state) (return final_state))
                                                                                                                (lambda (break_state) (loop_break break_state))
                                                                                                                (lambda (continue_state) (loop_continue continue_state))
                                                                                                                (lambda (exception_val) (exception exception_val))
                                                                                                                (lambda (val_return) (top_return val_return)))))
                                                                        (lambda (continue_state) (loop_continue (finally continue_state
                                                                                                                (lambda (final_state) (return final_state))
                                                                                                                (lambda (break_state) (loop_break break_state))
                                                                                                                (lambda (continue_state) (loop_continue continue_state))
                                                                                                                (lambda (exception_val) (exception exception_val))
                                                                                                                (lambda (val_return) (top_return val_return)))))
                                                                        (lambda (exception_val) (begin (finally mod_state
                                                                                                                (lambda (final_state) (return final_state))
                                                                                                                (lambda (break_state) (loop_break break_state))
                                                                                                                (lambda (continue_state) (loop_continue continue_state))
                                                                                                                (lambda (exception_val) (exception exception_val))
                                                                                                                (lambda (val_return) (top_return val_return)))
                                                                                                       (exception exception_val)))
                                                                        (lambda (val_return) (begin (finally mod_state
                                                                                                                (lambda (final_state) (return final_state))
                                                                                                                (lambda (break_state) (loop_break break_state))
                                                                                                                (lambda (continue_state) (loop_continue continue_state))
                                                                                                                (lambda (exception_val) (exception exception_val))
                                                                                                                (lambda (val_return) (top_return val_return))) 
                                                                                                    (top_return val_return)))
                                                                        ))
                                                        
                                                        ;
                                                        ; todo
                                                        (lambda (val_return) (begin (finally mod_state) (top_return val_return)))))))
                  loop_break 
                  loop_continue 
                  exception 
                  top_return))))))

(define M_s_throw
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_try_catch: null arg"))
      ((not (is_valid_state? state)) (error "M_s_try_catch: invalid state"))
      (else (M_state rest_of_args class instance state (lambda (mod_state)
                                                         (cond
                                                           ((not (is_valid_state? mod_state)) (error "M_s_throw: invalid mod_state"))
                                                           ((type_check 'bool (cadr arg) class instance mod_state) (exception (M_bool (cadr arg) class instance mod_state exception)))
                                                           ((type_check 'int (cadr arg) class instance mod_state) (exception (M_value (cadr arg) class instance mod_state exception)))
                                                           ((type_check 'func (cadr arg) class instance mod_state)
                                                            (cond
                                                              ((type_func 'int (get_func_name (cadr arg)) class instance mod_state) (exception (M_value (cadr arg) class instance mod_state exception)))
                                                              ((type_func 'bool (get_func_name (cadr arg)) class instance mod_state) (exception (M_bool (cadr arg) class instance mod_state exception)))
                                                              (else (error "M_s_throw: function return did not match type"))))
                                                           ((type_check 'init (cadr arg) class instance mod_state) (error "M_s_throw: argument (var?) not assigned before return."))
                                                           ((type_check 'error (cadr arg) class instance mod_state) (error "M_s_throw: argument (var?) not declared before return."))
                                                           (else (error "M_s_throw: did not match pattern"))))
                     loop_break 
                     loop_continue
                     exception
                     top_return)))))
      
;      ((type_check 'int (cadr arg) class instance state) (exception (M_value (cadr arg) class instance state exception)))
;      ((type_check 'bool (cadr arg) class instance state) (exception (M_bool (cadr arg) class instance state exception)))
;      (else ("M_s_throw: exception type not bool, int, not implemented")))))

(define M_s_class_define
  (lambda (arg rest_of_args class instance state return loop_break loop_continue exception top_return)
    (cond
      ((null? arg) (error "M_s_class_define: null arg"))
      (else (M_state rest_of_args class instance state (lambda (mod_state)
                                          (cond
                                            ((not (is_valid_state? mod_state)) (error "M_s_function_define: invalid state"))
                                            ((is_not_reserved? (cadr arg)) 
                                             (cond
                                               ((null? (get_class_parent arg)) (return (set_binding (get_class_name arg) 
                                                                                                ; (def create_class
                                                                                                ; (lambda (name parent_name static_fields methods instance_fields current_state)
                                                                                                (create_class (get_class_name arg) 
                                                                                                              '() 
                                                                                                              (get_class_body arg)
                                                                                                              class instance
                                                                                                              mod_state) 
                                                                                                mod_state)))
                                               (else (return (set_binding (get_class_name arg) 
                                                                          ; (def create_class
                                                                          ; (lambda (name parent_name static_fields methods instance_fields current_state)
                                                                          (create_class (get_class_name arg) 
                                                                                        (get_value (cadr (get_class_parent arg)) mod_state)
                                                                                        (get_class_body arg)
                                                                                        class instance
                                                                                        mod_state) 
                                                                          mod_state)))))
                                            (else (error "M_s_class_define: did not match pattern. May be reserved name"))))
                     loop_break 
                     loop_continue 
                     exception
                     top_return)))))

; extended M_state functions
; These are helper functions to the above M_state functions

(define new_state
  (lambda ()
    (list (new_layer) (new_layer)))) ; ( (<layer> (vars) (vals) (types)) )

(define new_layer
  (lambda ()
    '(() () ()) ))

; extended M_s_function helpers

(define create_function
  (lambda (name params body current_state)
    (cond
      ((not (is_valid_state? current_state)) (error "create_function: invalid state"))
      ((not (is_body? body)) (error "create_function: body not valid"))
      ((not (is_params? params)) (error "create_function: params not valid"))
      (else (list 'function name params body (create_func_closure name current_state))))))

(define create_func_closure
  (lambda (f_name func_state)
    (cond
      ((not (is_valid_state? func_state)) (error "create_func_closure: invalid state"))
      ; return a function that takes the current state, 
      ;   gets the function definition for the given name, 
      ;   then adds it to the state where the function was created
      (else (lambda (current_state)
              (set_binding f_name (get_value f_name current_state) func_state))))))

; M_s_class helpers
(define create_class
  (lambda (name parent body class instance current_state)
    (cond
      ((not (is_valid_state? current_state)) (error "create_function: invalid state"))
      (else (list 'class name parent (create_class_closure name parent body (list 'class name parent (lambda (c_state) (error "fake lambda called"))) instance current_state))))))

(define create_class_closure
  (lambda (name parent body class instance class_state)
    (cond
      ((not (is_valid_state? class_state)) (error "create_class_closure: invalid state"))
      ((null? parent) (lambda (current_state)
              ;(set_binding name (get_value name current_state)
                        (M_state (reverse-cps body (lambda (v) v))
                                 class instance
                                 (add_layer (new_layer) current_state)
                                 (lambda (return_state) return_state)
                                 (lambda (break_state) (error "create_class_closure: break called in closure creation"))
                                 (lambda (continue_state) (error "create_class_closure: continue called in closure creation"))
                                 (lambda (exception_state) (error "create_class_closure: exception thrown in closure creation"))
                                 (lambda (return_val) (error "create_class_closure: top_return thrown in closure creation")))));)
      (else (lambda (current_state)
              ;(set_binding name (get_value name current_state)
                           (M_state (reverse-cps body (lambda (v) v))
                                    class instance
                                    (add_layer (new_layer) ((get_class_closure parent) current_state))
                                    (lambda (return_state) return_state)
                                    (lambda (break_state) (error "create_class_closure: break called in closure creation"))
                                    (lambda (continue_state) (error "create_class_closure: continue called in closure creation"))
                                    (lambda (exception_state) (error "create_class_closure: exception thrown in closure creation"))
                                    (lambda (return_val) (error "create_class_closure: top_return thrown in closure creation"))))))));)

; === Create class v2 ===
(define create_class2 
  (lambda (name parent_class body)
    (cond
      ((null? parent_class)
       (process_static (new_class name parent_class) body))
      (else
       (process_static 
        (strip_add (strip_add parent_class parent_class 2)
                   name
                   1)
        body)))))
     
(define process_static
  (lambda (class body)
    (cond
      ((null? body) class)
      ((is_static_declare? (car body)) (process_static (set_static_var class (cadr (car body)) 'init) (cdr body)))
      ((is_static_declare_assign? (car body)) 
       (cond
         ((type_check 'int (caddr (car body)) class 'static (new_state))
          (process_static (set_static_var class (cadr (car body)) (M_value (caddr (car body)) class 'static (new_state) (lambda (e) (error "M_val exception")))) (cdr body)))
         ((type_check 'bool (caddr (car body)) class 'static (new_state))
          (process_static (set_static_var class (cadr (car body)) (M_bool (caddr (car body)) class 'static (new_state) (lambda (e) (error "M_bool exception")))) (cdr body)))
         (else (error "process_static: currently int and bool only"))))
      ((is_static_function_def? (car body)) (process_static (set_static_func class (cadr (car body)) (cons 'function (cdr (car body)))) (cdr body)))
      (else (process_static class (cdr body))))))

(define new_class
  (lambda (name parent_class)
    ;     (class name parent       static_vars static methods instance_vars instance_methods)
    (list 'class name parent_class (new_layer) (new_layer) (new_layer) (new_layer))))

(define strip_add
  (lambda (class insert depth)
    (cond
      ((null? class) '())
      ((not (list? class)) (display class) (error "strip_add: malformed class"))
      ((zero? depth) (cons insert (cdr class)))
      (else (cons (car class) (strip_add (cdr class) insert (- depth 1)))))))

(define add_to_layer
  (lambda (var val type layer)
    (list (cons var (car layer)) (cons (box val) (cadr layer)) (cons (box type) (caddr layer)))))

#|(define set_in_layer
  (lambda (var val type layer)
    (cond
      ((eq? (get_box_in_layer var (car layer) (cadr layer) (caddr layer)) 'error) (error "set_in_layer: variable not yet declared"))
      (else (begin
      (set-box! (car (get_box_in_layer var (car layer) (cadr layer) (caddr layer))) val)
      (set-box! (cadr (get_box_in_layer var (car layer) (cadr layer) (caddr layer))) type)
      layer)))))|#
    
(define get_box_in_layer
  (lambda (var vars vals types)
    (cond
      ((or (null? vars) (or (null? vals) (null? types))) 'error)
      ((eq? var (car vars)) (list (car vals) (car types)))
      (else (get_box_in_layer var (cdr vars vals types))))))

(define get_static_vars
  (lambda (class)
    (cadddr class)))

(define get_static_funcs
  (lambda (class)
    (caddddr class)))

(define cadddddr
  (lambda (l)
    (cadddr (cddr l))))

(define get_instance_vars
  (lambda (class)
    (cadddddr class)))

(define caddddddr
  (lambda (l)
    (cadddr (cdddr l))))

(define get_instance_funcs
  (lambda (class)
    (caddddddr class)))

(define set_static_var
  (lambda (class var val)
    (cond
      ((number? val)
       (strip_add class (add_to_layer var val 'int (get_static_vars class)) 3))
      ((eq? 'init val)
       (strip_add class (add_to_layer var val 'init (get_static_vars class)) 3))
      ((or (eq? 'false val) (eq? #f val))
       (strip_add class (add_to_layer var #f 'bool (get_static_vars class)) 3))
      ((or (eq? 'true val) (eq? #t val))
       (strip_add class (add_to_layer var #t 'bool (get_static_vars class)) 3))
      (else
       (display val) (error "set_static_var: did not match pattern")))))

(define set_static_func
  (lambda (class var val)
    (strip_add class (add_to_layer var val 'func (get_static_funcs class)) 4)))

(define modify_member
  (lambda (mem val type class)
    (cond
      ((eq? (modify_helper mem val type
                           (append-cps (car (class_funcs class)) (car (class_vars class)) (lambda (v) v))
                           (append-cps (cadr (class_funcs class)) (cadr (class_vars class)) (lambda (v) v))
                           (append-cps (caddr (class_funcs class)) (caddr (class_vars class)) (lambda (v) v))) 'done) 'done)
      (else 'error))))

(define modify_helper
  (lambda (mem val type vars vals types)
    (cond
      ((or (null? vars) (or (null? vals)) (null? types)) 'error)
      ((eq? mem (car vars))
       (begin
         (set-box! (car vals) val)
         (set-box! (car types) type)
         'done))
      (else (modify_helper mem val type (cdr vars) (cdr vals) (cdr types))))))

(define class_vars
  (lambda (class)
    (merge_layer (get_static_vars class) (get_instance_vars class) (lambda (l1 l2 l3) (list l1 l2 l3)))))

(define merge_layer
  (lambda (l1 l2 return)
    (cond
      ((null? (car l1)) (return (car l2) (cadr l2) (caddr l2)))
      (else (merge_layer (list (cdr (car l1)) (cdr (cadr l1)) (cdr (caddr l1))) 
                         l2
                         (lambda (vars vals types)
                           (return (cons (car (car l1)) vars) (cons (car (cadr l1)) vals) (cons (car (caddr l1)) types))))))))


(define class_funcs
  (lambda (class)
    (merge_layer (get_static_funcs class) (get_instance_funcs class) (lambda (l1 l2 l3) (list l1 l2 l3)))))
    
(define get_dot_var
  (lambda (dot class state)
    (cond
      ((is_this_dot? dot) (unbox (get_dot_box (caddr dot) (car (class_vars class)) (cadr (class_vars class)))))
      ((is_super_dot? dot) (unbox (get_dot_box (caddr dot) (car (class_vars (get_class_parent class))) (cadr (class_vars (get_class_parent class))))))
      (else (unbox (get_dot_box (caddr dot) (car (class_vars (get_value (cadr dot) state))) (cadr (class_vars (get_value (cadr dot) state)))))))))

(define get_var
  (lambda (var class)
    (cond
      ((not (box? (get_dot_box var (car (class_vars class)) (cadr (class_vars class))))) 'error)
      (else (unbox (get_dot_box var (car (class_vars class)) (cadr (class_vars class))))))))

(define get_dot_func
  (lambda (dot class state)
    (cond
      ((is_function_call? dot) (get_dot_func (cadr dot) class state))
      ((is_this_dot? dot) (unbox (get_dot_box (caddr dot) (car (class_funcs class)) (cadr (class_funcs class)))))
      ((is_super_dot? dot) (unbox (get_dot_box (caddr dot) (car (class_funcs (get_class_parent class) (cadr (class_funcs (get_class_parent class))))))))
      (else (unbox (get_dot_box (caddr dot) (car (class_funcs (get_value (cadr dot) state))) (cadr (class_funcs (get_value (cadr dot) state)))))))))

(define get_func
  (lambda (f_name class)
    (cond
      ((not (box? (get_dot_box f_name (car (class_funcs class)) (cadr (class_funcs class))))) 'error)
      (else (unbox (get_dot_box f_name (car (class_funcs class)) (cadr (class_funcs class))))))))

(define get_dot_box
  (lambda (var vars vals)
    (cond
      ((or (null? vars) (null? vals)) 'error)
      ((eq? var (car vars)) (car vals))
      (else (get_dot_box var (cdr vars) (cdr vals))))))

(define is_this_dot?
  (lambda (dot)
    (cond
      ((not (list? dot)) #f)
      ((not (eq? (length dot) 3)) #f)
      (else (and (eq? (car dot) 'dot) (eq? (cadr dot) 'this))))))

(define is_super_dot?
  (lambda (dot)
    (cond
      ((not (list? dot)) #f)
      ((not (eq? (length dot) 3)) #f)
      (else (and (eq? (car dot) 'dot) (eq? (cadr dot) 'super))))))

; Returns an unformatted version of the variables pulled from the state for reference. 
; Layering/ordering is retained.
(define get_vars
  (lambda (state)
    (cond
      ((not (is_valid_state? state)) (error "get_vars: invalid state"))
      (else (map_compose_state car state)))))

; Returns an unformatted version of the values pulled from the state for reference. 
; Layering/ordering is retained.
(define get_vals
  (lambda (state)
    (cond
      ((not (is_valid_state? state)) (error "get_vals: invalid state"))
      (else (map_compose_state cadr state)))))

; Returns an unformatted version of the types pulled from the state for reference. 
; Layering/ordering is retained.
(define get_types
  (lambda (state)
    (cond
      ((not (is_valid_state? state)) (error "get_types: invalid state"))
      (else (map_compose_state caddr state)))))

; an abstraction used to help pull out info from state for vars/vals/types
(define map_compose_state
  (lambda (f substate)
    (cond
      ((not (is_valid_state? substate)) (error "map_compose_state: invalid state"))
      (else 
       (letrec 
           ((loop (lambda (f substate return)
                    (cond
                      ((null? substate) (return '()))
                      (else (loop f (cdr substate) (lambda (l) (return (cons (f (car substate)) l)))))))))
         (loop f substate (lambda (v) v)))))))

(define is_valid_state?
  (lambda (state)
    (cond
      ((null? state) (error "Malformed State Error: null state"))
      ((not (list? state)) (display state) (error "Malformed State Error: state not a list"))
      (else (valid_state_check? state (lambda (v) v))))))

(define valid_state_check?
  (lambda (substate break)
    (cond
      ((null? substate) (break #t))
      ((not (is_valid_layer? (car substate))) (break #f))
      (else (valid_state_check? (cdr substate) break)))))

(define is_valid_layer?
  (lambda (frame)
    (cond
      ((null? frame) (error "Malformed Frame Error: null state"))
      ((not (list? frame)) (display frame) (error "Malformed Frame Error: state not a list"))
      ((not (eq? (length frame) 3)) (error "Malformed Frame Error: state length is not 3"))
      ((not (eq? (length (car frame)) (length (cadr frame)))) (error "Malformed Frame Error: vars, vals different lengths"))
      ((not (eq? (length (car frame)) (length (caddr frame)))) (error "Malformed Frame Error: vars, types different lengths"))
      (else #t))))

(define set_binding ; returns the state with the new variable bound to the new value
  ; throws error if binding exists in the first layer
  (lambda (var val state)
    (cond
      ((not (is_valid_state? state)) (error "set_binding: invalid_state"))
      ((not (is_not_reserved? var)) (error "set_binding: var name is reserved value."))
      ((not (eq? (get_value var (list (skim_layer state) (new_layer))) 'error)) (error "set_binding: Variable already declared (redefining)"))
      (else
       (cond
         ((eq? val 'init) (put_in_state var val 'init (car (get_vars state)) (car (get_vals state)) (car (get_types state))
                                        (lambda (vars vals types) 
                                          (add_layer (list vars vals types) (remove_layer state)))))
         ((number? val) (put_in_state var val 'int (car (get_vars state)) (car (get_vals state)) (car (get_types state))
                                      (lambda (vars vals types) 
                                        (add_layer (list vars vals types) (remove_layer state)))))
         ((or (eq? val #t) (eq? val #f)) (put_in_state var val 'bool (car (get_vars state)) (car (get_vals state)) (car (get_types state))
                                                       (lambda (vars vals types) 
                                                         (add_layer (list vars vals types) (remove_layer state)))))
         ((is_function? val) (put_in_state var val 'func (car (get_vars state)) (car (get_vals state)) (car (get_types state))
                                           (lambda (vars vals types) 
                                             (add_layer (list vars vals types) (remove_layer state)))))
         ((is_class? val) (put_in_state var val 'class (car (get_vars state)) (car (get_vals state)) (car (get_types state))
                                            (lambda (vars vals types)
                                              (add_layer (list vars vals types) (remove_layer state)))))
         (else (display var) (display val) (error "set_binding: type of value not valid")))))))

; helper to set_binding
(define put_in_state
  ; returns a var_list, val_list, type_list for a layer
  (lambda (var_name val type var_list val_list type_list return)
    (return (cons var_name var_list) (cons (box val) val_list) (cons (box type) type_list))))

(define modify_binding3
  (lambda (var val class instance state)
    (cond
      ((not (is_valid_state? state)) (error "modify_binding3: invalid_state"))
      ((eq? (get_value var state) 'error) (error "modify_binding3: Variable not in scope or not declared (assign before declaring)"))
      (else
       (begin
         (set-box! (get_val_box var state) val)
         (set-box! (get_type_box var state) (type val class instance state))
         state)))))

(define add_layer
  (lambda (layer state)
    (cond
      ((not (is_valid_state? state)) (error "add_layer: invalid state"))
      (else (cons layer state)))))

(define remove_layer
  (lambda (state)
    (cond
      ((not (is_valid_state? state)) (error "remove_layer: invalid state"))
      (else (cdr state)))))

(define skim_layer
  (lambda (state)
    (cond
      ((null? state) '())
      ((not (is_valid_state? state)) (error "skim_layer: invalid state"))
      (else (car state)))))

#| (define get_value
  (lambda (var state)
    (cond
      ((null? var) (error "get_value: null variable"))
      ((not (is_valid_state? state)) (error "get_value: invalid state"))
      (else (search_value var (get_vars state) (get_vals state) (lambda (v) v))))))
|#

(define get_value
  (lambda (var state)
    (cond
      ((eq? (get_val_box var state) 'error) 'error)
      ((not (box? (get_val_box var state))) (display var) (error "get_value: var does not return a box"))
      (else (unbox (get_val_box var state))))))

(define get_val_box
  (lambda (var state)
    (cond
      ((null? var) (error "get_value: null variable"))
      ((not (is_valid_state? state)) (error "get_value: invalid state"))
      (else (search_box var (get_vars state) (get_vals state) (lambda (v) v))))))

(define get_type_box
  (lambda (var state)
    (cond
      ((null? var) (error "get_value: null variable"))
      ((not (is_valid_state? state)) (error "get_value: invalid state"))
      (else (search_box var (get_vars state) (get_types state) (lambda (v) v))))))

; helper to get_box
(define search_box
  (lambda (var_name var_list val_list return)
    (cond
      ((or (null? var_list) (null? val_list)) (return 'error))
      ((list? (car var_list)) (search_box var_name (car var_list) (car val_list) 
                                          (lambda (val)
                                            (cond
                                              ((eq? val 'error) (search_box var_name (cdr var_list) (cdr val_list) return))
                                              (else (return val))))))
      ((eq? var_name (car var_list)) (return (car val_list)))
      (else (search_box var_name (cdr var_list) (cdr val_list) return)))))

; helper to get_value
(define search_value
  (lambda (var_name var_list val_list return)
    (cond
      ((or (null? var_list) (null? val_list)) (return 'error))
      ((list? (car var_list)) (search_value var_name (car var_list) (car val_list) 
                                            (lambda (val)
                                              (cond
                                                ((eq? val 'error) (search_value var_name (cdr var_list) (cdr val_list) return))
                                                (else (return val))))))
      ((eq? var_name (car var_list)) (return (unbox (car val_list))))
      (else (search_value var_name (cdr var_list) (cdr val_list) return)))))


; ========== M_value ==========

; Returns the (int) value of a given parse element
; Please note: All functions with M_v are value functions designed to handle specific argument cases.
(define M_value
  (lambda (arg class instance state exception) 
    (cond
      ((null? arg) (error "M_value: null arg"))
      ((not (is_valid_state? state)) (error "M_value: invalid state"))
      ((is_dot_var? arg) (get_dot_var arg class state))
      ((is_dot_func? arg) 
       (cond
         ((type_func 'int arg class instance state) (M_v_function_call arg class instance state exception (lambda (v) v)))
         (else (error "M_value rev2: type of dot func did not eval to int"))))
      ((and (is_function_call? arg) (type_func 'int (get_func_name arg) class instance state)) (M_v_function_call arg class instance state exception (lambda (v) v)))
      ((type_check 'func arg class instance state) (error "M_value: function type does not evaluate to an int"))
      ((type_check 'init arg class instance state) (display arg) (error "M_value: arg not assigned yet. (using before assigning)"))
      ((not (type_check 'int arg class instance state)) (error "M_value: arg type does not eval to int"))
      ((math_operator? arg) (M_v_math_operator arg class instance state exception))
      ((number? arg) arg)
      ((eq? 'error (get_var arg class)) 
       (cond
         ((eq? 'error (get_value arg state)) (error "M_value: variable value does not exist. Probably not declared yet."))
         (else (get_value arg state))))
      ((eq? 'init (get_var arg class)) (error "M_value: variable value is init. Probably not assigned yet."))
      (else (get_var arg class)))))

; Returns the value of any one of the given math operators (+ - * / % -)
(define M_v_math_operator
  (lambda (arg class instance state exception)
    (cond
      ((null? arg) (error "M_v_math_operator: null arg"))
      ((not (list? arg)) (error "M_v_math_operator: arg not a list"))
      ((not (is_valid_state? state)) (error "M_v_math_operator: invalid state"))
      ((operator_match? '+ arg) (+ (M_value (get_first_arg arg) class instance state exception) (M_value (get_second_arg arg) class instance state exception)))
      ((operator_match? '* arg) (* (M_value (get_first_arg arg) class instance state exception) (M_value (get_second_arg arg) class instance state exception)))
      ((operator_match? '/ arg) (quotient (M_value (get_first_arg arg) class instance state exception) (M_value (get_second_arg arg) class instance state exception)))
      ((operator_match? '% arg) (remainder (M_value (get_first_arg arg) class instance state exception) (M_value (get_second_arg arg) class instance state exception)))
      ((and (eq? 2 (length arg)) (operator_match? '- arg)) (- 0 (M_value (get_first_arg arg)class instance state exception)))
      ((operator_match? '- arg) (- (M_value (get_first_arg arg) class instance state exception) (M_value (get_second_arg arg) class instance state exception)))
      (else (error "M_v_math_operator: arg does not match a pattern")))))

(define M_v_function_call
  (lambda (arg class instance state exception return)
    (cond
      ((null? arg) (error "M_v_function_call: null arg"))
      ((is_dot_func? arg) 
       (cond
         ((is_this_dot? (cadr arg)) ; eval in current class/instance
          (M_v_function_call (caddr (cadr arg)) class instance state exception (lambda (v) v)))
         ((is_super_dot? (cadr arg))
          (M_v_function_call (caddr (cadr arg)) (get_class_parent class) instance state exception (lambda (v) v)))
         (else
          (M_v_function_call (append-cps (list 'funcall (caddr (cadr arg))) (cddr arg) (lambda (v) v)) (get_value (cadr (cadr arg)) state) instance state exception (lambda (v) v))))) ; TODO check next time around
      ((not (is_function_call? arg)) (error "M_v_function_call: arg not a function call"))
      ((not (is_valid_state? state)) (error "M_v_function_call: invalid state"))
      (else (evaluate_function (get_func (get_func_name arg) class)
                               (get_func_args arg)
                               class instance
                               state 
                               exception
                               (lambda (function_return)
                                 (cond
                                   ((type_check 'int function_return class instance state) (return function_return))
                                   (else (error "M_v_function_call: function did not return type int")))))))))

; ========== M_bool ==========

; Returns the boolean value for the given parse element.
(define M_bool
  (lambda (arg class instance state exception)
    (cond
      ((null? arg) (error "M_bool: null arg"))
      ((not (is_valid_state? state)) (error "M_bool: invalid state"))
      ((is_dot_var? arg) (get_dot_var arg class state))
      ((is_dot_func? arg) 
       (cond
         ((type_func 'bool arg class instance state) (M_b_function_call arg class instance state exception)) ; TODO resume editing M_bool here
         (else (error "M_bool rev2: type of dot func did not eval to bool"))))
      ((and (is_function_call? arg) (type_func 'bool (get_func_name arg) class instance state)) (M_b_function_call arg class instance state exception))
      ((type_check 'func arg class instance state) (error "M_bool: function type does not evaluate to a bool"))
      ((not (type_check 'bool arg class instance state)) (error "M_bool: arg type does not eval to bool"))
      ((comparison_operator? arg) (M_b_comparison_operator arg class instance state exception))
      ((boolean_operator? arg) (M_b_boolean_operator arg class instance state exception))
      ((or (eq? arg #t) (eq? arg 'true)) #t)
      ((or (eq? arg #f) (eq? arg 'false)) #f)
      ((eq? 'error (get_var arg class)) 
       (cond
         ((eq? 'error (get_value arg state)) (error "M_bool: variable value does not exist. Probably not declared yet."))
         (else (get_value arg state))))
      ((eq? 'init (get_var arg class)) (error "M_bool: variable value is init. Probably not assigned yet."))
      (else (get_var arg class)))))

; Returns the boolean value for a comparision operator ( > >= == <= < != )
(define M_b_comparison_operator
  (lambda (arg class instance state exception)
    (cond
      ((null? arg) (error "M_b_comparison_operator: null arg"))
      ((not (list? arg)) (error "M_b_comparison_operator: arg not a list"))
      ((not (is_valid_state? state)) (error "M_b_comparison_operator: invalid state"))
      ((operator_match? '== arg) (eq? (M_value (get_first_arg arg) class instance state exception) (M_value (get_second_arg arg) class instance state exception)))
      ((operator_match? '!= arg) (not (eq? (M_value (get_first_arg arg) class instance state exception) (M_value (get_second_arg arg) class instance state exception))))
      ((operator_match? '< arg) (< (M_value (get_first_arg arg) class instance state exception) (M_value (get_second_arg arg) class instance state exception)))
      ((operator_match? '> arg) (> (M_value (get_first_arg arg) class instance state exception) (M_value (get_second_arg arg) class instance state exception)))
      ((operator_match? '<= arg) (<= (M_value (get_first_arg arg) class instance state exception) (M_value (get_second_arg arg) class instance state exception)))
      ((operator_match? '>= arg) (>= (M_value (get_first_arg arg) class instance state exception) (M_value (get_second_arg arg) class instance state exception)))
      (else (display arg) (error "M_b_comparison_operator: arg does not match a pattern")))))

; Returns the boolean value for a boolean operator ( && || ! )
(define M_b_boolean_operator
  (lambda (arg class instance state exception)
    (cond
      ((null? arg) (error "M_b_comparison_operator: null arg"))
      ((not (list? arg)) (error "M_b_comparison_operator: arg not a list"))
      ((not (is_valid_state? state)) (error "M_b_comparison_operator: invalid state"))
      ((operator_match? '&& arg) (and (M_bool (get_first_arg arg) class instance state exception) (M_bool (get_second_arg arg) class instance state exception)))
      ((operator_match? '|| arg) (or (M_bool (get_first_arg arg) class instance state exception) (M_bool (get_second_arg arg) class instance state exception)))
      ((and (eq? 2 (length arg)) (operator_match? '! arg)) (not (M_bool (get_first_arg arg) class instance state exception)))
      (else (error "M_b_comparison_operator: arg does not match a pattern")))))

(define M_b_function_call
  (lambda (arg class instance state exception)
    (cond
      ((null? arg) (error "M_b_function_call: null arg"))
      ((is_dot_func? arg) 
       (cond
         ((is_this_dot? (cadr arg)) ; eval in current class/instance
          (M_b_function_call (caddr (cadr arg)) class instance state exception))
         ((is_super_dot? (cadr arg))
          (M_b_function_call (caddr (cadr arg)) (get_class_parent class) instance state exception))
         (else
          (M_b_function_call (append-cps (list 'funcall (caddr (cadr arg))) (cddr arg) (lambda (v) v)) (get_value (cadr (cadr arg)) state) instance state exception))))
      ((not (is_function_call? arg)) (error "M_b_function_call: arg not a function call"))
      ((not (is_valid_state? state)) (error "M_b_function_call: invalid state"))
      (else (evaluate_function (get_func (get_func_name arg) class) 
                               (get_func_args arg) 
                               class instance
                               state 
                               exception
                               (lambda (function_return)
                                 (cond
                                   ((type_check 'bool function_return class instance state) function_return)
                                   (else (error "M_b_function_call: function did not return type bool")))))))))

; ========== DOT ==========

(define dot_eval ; returns a pair (class instance), ex. (A a)
  (lambda (dot class instance state)
    (cond
      ((null? dot) (error "dot_eval: Null dot"))
      ((not (eq? (car dot) 'dot)) (display dot) (error "dot_eval: malformed dot"))
      ((type_check 'class (cadr dot) class instance state) ; is a class
       (cond
         ((and (is_class? class) (eq? (cadr dot) (get_class_name class))) ; if it matches existing
          (list class instance))
         (else (list (get_value (cadr dot) state) 'static))))
      ((type_check 'obj (cadr dot) class instance state)
       (error "type_check: type check should not be valid for object")
       (list (get_obj_class (get_value (cadr dot) state)) (get_value (cadr dot) state)))
      (else (error "dot_eval: left hand side not a obj or class")))))

(define dot_env
  (lambda (dot_pair state) ; TODO update for state
    (cond
      ((null? dot_pair) (error "dot_env: malformed (null) pair"))
      ((not (eq? (length dot_pair) '2)) (error "dot_env: malformed (invalid length) pair"))
      ((eq? (cadr dot_pair) 'static) ((get_class_closure (car dot_pair)) state))
      ((is_class? (cadr dot_pair)) ((get_obj_closure (cadr dot_pair)))) ; TODO update to is_obj next time around
      (else ((get_class_closure (car dot_pair)) state)))))


; ========== M_name ==========

(define M_name
  (lambda (arg state) arg))

; ========== Function Evaluation ==========

(define evaluate_function
  (lambda (function args class instance current_state exception top_return)
    (cond
      ((not (is_valid_state? current_state)) (error "evaluate_function: invalid state"))
      ((not (is_function? function)) (display function) (error "evaluate_function: not a function"))
      (else ; get function, get function environment w/ closure, evaluate parameters in current_state, bind params to func_environ, interpret body
       (let
           ((eval_f (lambda (rvs_body function_state) ;interprets the body
                      (M_state rvs_body
                               class instance
                               function_state
                               (lambda (return_state) 
                                 (top_return #f))
                               (lambda (break_state) (error "evaluate_function: break called outside of a while loop"))
                               (lambda (continue_state) (error "evalutate_function: continue called outside of a while loop"))
                               exception
                               top_return))))
         (eval_f 
          (reverse-cps (get_func_body function) (lambda (v) v)) ; body in reverse order
          (evaluate_args (get_func_params function)
                         args 
                         class instance
                         current_state 
                         current_state
                         exception) ;function_state
          ))))))

(define evaluate_args ; evaluates args in current state, then attaches arg as param in function state, returns function_state with args
  (lambda (params args class instance current_state function_state exception)
    (bind_param_list params 
                     (evaluate_arg_cps args
                                       class instance
                                       current_state
                                       exception
                                       (lambda (arg_list) arg_list))
                     function_state
                     (lambda (mod_f_state) mod_f_state))))

(define evaluate_arg_cps
  (lambda (args class instance current_state exception return)
    (cond
      ((not (is_valid_state? current_state)) (error "evalutate_arg_cps: invalid state"))
      ((null? args) (return '()))
      (else (evaluate_arg_cps (cdr args) class instance current_state exception (lambda (arg_list)
                                                         (cond
                                                           ((type_check 'int (car args) class instance current_state)
                                                            (return (cons (M_value (car args) class instance current_state exception) arg_list)))
                                                           ((type_check 'bool (car args) class instance current_state)
                                                            (return (cons (M_bool (car args) class instance current_state exception) arg_list)))
                                                           (else (error "evaluate_arg_cps: arg did not eval to int or bool")))))))))

(define bind_param_list
  (lambda (params evald_args function_state return)
    (cond
      ((not (is_valid_state? function_state)) (error "bind_param_list: invalid state"))
      ((not (eq? (length params) (length evald_args))) (error "bind_param_list: In function call, the length of the parameters and arguments is not equal."))
      ((or (null? params) (null? evald_args)) (return (add_layer (new_layer) function_state)))
      (else (bind_param_list (cdr params) (cdr evald_args) function_state (lambda (mod_f_state)
                                                                            (return (set_binding (car params)
                                                                                                 (car evald_args)
                                                                                                 mod_f_state))))))))


; ========== Helper Functions by output ==========

; === structure ===
; These functions abstract away portions of the parse information, including the prefix notation, etc.
(define get_operator
  (lambda (arg)
    (car arg)))

(define get_first_arg
  (lambda (arg)
    (cadr arg)))

(define get_second_arg
  (lambda (arg)
    (caddr arg)))

; portions of if statements
(define get_conditional
  (lambda (arg)
    (cadr arg)))

(define get_stmt1
  (lambda (arg)
    (caddr arg)))

(define get_stmt2
  (lambda (arg)
    (cadddr arg)))

; portions of while statements
(define get_while_body
  (lambda (arg)
    (caddr arg)))

(define get_block_body
  (lambda (arg)
    (cdr arg)))

; portions of a function (def, value or call)
(define get_func_body
  (lambda (func)
    (cond
      ((null? func) (error "get_func_body: null function"))
      ((is_function_def? func) (cadddr func))
      ((is_static_function_def? func) (cadddr func))
      ((is_function? func) (cadddr func))
      (else (error "get_func_body: malformed function")))))

(define is_body?
  (lambda (body)
    (list? body)))

(define get_func_name
  (lambda (func)
    (cond
      ((null? func) (error "get_func_name: null function"))
      ((is_function_def? func) (cadr func))
      ((is_static_function_def? func) (cadr func))
      ((is_function? func) (cadr func))
      ((is_function_call? func) (cadr func))
      (else (display func) (error "get_func_name: malformed function")))))

(define get_func_params
  (lambda (func)
    (cond
      ((null? func) (error "get_func_params: null function"))
      ((is_function_def? func) (caddr func))
      ((is_static_function_def? func) (caddr func))
      ((is_function? func) (caddr func))
      (else (error "get_func_params: malformed function")))))

(define get_func_args
  (lambda (func)
    (cond
      ((null? func) (error "get_func_args: null function"))
      ((is_function_call? func) (cddr func))
      (else (error "get_func_args: malformed function")))))

(define is_params?
  (lambda (params)
    (list? params)))

(define get_func_closure
  (lambda (func)
    (cond
      ((null? func) (error "get_func_closure: null function"))
      ((is_function? func) (caddddr func))
      (else (error "get_func_closure: malformed function")))))

(define caddddr
  (lambda (list)
    (cadddr (cdr list))))

; portions of try catch
(define is_try?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      (else (and (>= (length arg) 2) (eq? (car arg) 'try))))))

(define is_catch?
  (lambda (arg)
    (cond
      ((not (list? arg)) #f)
      (else (or (null? arg) (and (eq? (length arg) 3) (and (eq? (car arg) 'catch) (is_list_e? (cadr arg)))))))))

(define is_throw?
  (lambda (arg)
    (cond
      ((not (list? arg)) #f)
      (else (and (eq? (length arg) 2) (eq? (car arg) 'throw))))))

(define is_list_e?
  (lambda (arg)
    (cond
      ((not (list? arg)) #f)
      (else (eq? (length arg) 1)))))

(define is_finally?
  (lambda (arg)
    (cond
      ((not (list? arg)) #f)
      (else (or (null? arg) (and (eq? (car arg) 'finally) (eq? (length arg) 2)))))))

(define get_try_args
  (lambda (try)
    (cond
      ((not (is_try? try)) (error "get_try_args: can't get args from invalid try catch"))
      (else (cadr try)))))

(define get_catch_args
  (lambda (try)
    (cond
      ((not (is_try_catch? try)) (error "get_catch_args: can't get args from invalid try catch"))
      ((null? (caddr try)) '())
      (else (caddr (caddr try))))))

(define get_finally_args
  (lambda (try)
    (cond
      ((not (is_try_catch? try)) (error "get_finally_args: can't get args from invalid try catch"))
      ((null? (cadddr try)) '())
      (else (cadr (cadddr try))))))

; portions of class

(define get_class_name
  (lambda (class)
    (cond
      ((null? class) (error "get_class_name: null class"))
      ((is_class_def? class) (cadr class))
      ((is_class? class) (cadr class))
      (else (display class) (error "get_class_name: did not match pattern")))))

(define get_class_parent 
  (lambda (class) 
    (cond
      ((null? class) (error "get_class_parent: null class"))
      ((is_class_def? class) (caddr class))
      ((is_class? class) (caddr class))
      (else (display class) (error "get_class_parent: did not match pattern")))))

(define get_class_body
  (lambda (class) 
    (cond
      ((null? class) (error "get_class_body: null class"))
      ((is_class_def? class) (cadddr class))
      (else (display class) (error "get_class_body: did not match pattern")))))

(define get_class_closure
  (lambda (class) 
    (cond
      ((null? class) (error "get_class_body: null class"))
      ((is_class_def? class) (cadddr class))
      (else (display class) (error "get_class_closure: did not match pattern")))))

; === boolean (question) ===

; These functions take an argument/parser element, and return if it 
;  matches certain common forms for different portions of the above functions.

(define operator_match?
  (lambda (symbol arg)
    (eq? symbol (get_operator arg))))

(define comparison_operator?
  (lambda (arg)
    (cond
      ((null? arg) (error "comparison_operator?: null arg"))
      ((not (list? arg)) #f)
      ((eq? (get_operator arg) '==) #t)
      ((eq? (get_operator arg) '!=) #t)
      ((eq? (get_operator arg) '<) #t)
      ((eq? (get_operator arg) '>) #t)
      ((eq? (get_operator arg) '<=) #t)
      ((eq? (get_operator arg) '>=) #t)
      (else #f))))

(define boolean_operator?
  (lambda (arg)
    (cond
      ((null? arg) (error "boolean_operator?: null arg"))
      ((not (list? arg)) #f)
      ((eq? (get_operator arg) '&&) #t)
      ((eq? (get_operator arg) '!) #t)
      ((eq? (get_operator arg) '||) #t)
      (else #f))))

(define math_operator?
  (lambda (arg)
    (cond
      ((null? arg) (error "math_operator?: null arg"))
      ((not (list? arg)) #f)
      ((eq? (get_operator arg) '+) #t)
      ((eq? (get_operator arg) '-) #t)
      ((eq? (get_operator arg) '*) #t)
      ((eq? (get_operator arg) '/) #t)
      ((eq? (get_operator arg) '%) #t)
      (else #f))))

(define is_not_reserved?
  (lambda (var_name)
    (cond
      ((null? var_name) #f) ; null is special
      ((number? var_name) #f) ; is not a number
      ((or (eq? #t var_name) (eq? #f var_name)) #f) ; is not a special boolean
      ((or (eq? 'true var_name) (eq? 'false var_name)) #f) ; is not a reserved name boolean
      (else #t))))

; === M_state filters ===

; These methods take in an argument/parse tree element and return a boolean value if they match
;  common M_state evaluation patterns.

(define is_return?
  (lambda (arg)
    (eq? 'return (car arg))))

(define is_declare?
  (lambda (arg)
    (and (eq? 'var (car arg)) (eq? 2 (length arg)))))

(define is_static_declare?
  (lambda (arg)
    (and (eq? 'static-var (car arg)) (eq? 2 (length arg)))))

(define is_declare_assign?
  (lambda (arg)
    (and (eq? 'var (car arg)) (eq? 3 (length arg)))))

(define is_static_declare_assign?
  (lambda (arg)
    (and (eq? 'static-var (car arg)) (eq? 3 (length arg)))))

(define is_assign?
  (lambda (arg)
    (eq? '= (car arg))))

(define is_if_then?
  (lambda (arg)
    (and (eq? 'if (car arg)) (eq? 3 (length arg)))))

(define is_if_then_else?
  (lambda (arg)
    (and (eq? 'if (car arg)) (eq? 4 (length arg)))))

(define is_while?
  (lambda (arg)
    (eq? 'while (car arg))))

(define is_block?
  (lambda (arg)
    (eq? 'begin (car arg))))

(define is_break?
  (lambda (arg)
    (eq? 'break (car arg))))

(define is_continue?
  (lambda (arg)
    (eq? 'continue (car arg))))

; TODO make more structural

(define is_function? ; is function definition
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      (else (eq? (car arg) 'function)))))

(define is_function_call?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      (else (eq? (car arg) 'funcall)))))

(define is_function_def?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      (else (eq? (car arg) 'function)))))

(define is_static_function_def?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      (else (eq? (car arg) 'static-function)))))

(define is_try_catch?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      (else (and (eq? (length arg) 4) 
                 (and (is_try? arg) 
                      (and (is_catch? (caddr arg)) 
                           (is_finally? (cadddr arg)))))))))

(define is_class_def?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      (else (and (eq? (length arg) 4)
                 (eq? (car arg) 'class))))))

(define is_class? ; local define
  (lambda (arg)
    (cond
      ((null? arg) #t)
      ((not (list? arg)) #f)
      (else (and (eq? (length arg) 7)
                 (eq? (car arg) 'class))))))

(define is_dot_var?
  (lambda (arg)
    (cond
      ((null? arg) #t)
      ((not (list? arg)) #f)
      (else (and (eq? (length arg) 3)
                 (eq? (car arg) 'dot))))))

(define is_dot_func?
  (lambda (arg)
    (cond
      ((null? arg) #t)
      ((not (list? arg)) #f)
      (else (and (>= (length arg) 2)
                 (and (is_function_call? arg)
                      (is_dot_var? (cadr arg))))))))

; ========== TYPE ==========

; This method returns the type of a given parse element.
; This refers to both the maintained type-list for all variables, as well as
;  the type of output for any given statement (int or boolean).
(define type
  (lambda (arg class instance state)
    (cond
      ((null? arg) (error "type: null arg"))
      ((not (is_valid_state? state)) (error "type: invalid state"))
      ((comparison_operator? arg) 'bool)
      ((boolean_operator? arg) 'bool)
      ((math_operator? arg) 'int)
      ((number? arg) 'int)
      ((is_function_call? arg) 'func)
      ((or (eq? arg #t) (eq? arg 'true)) 'bool)
      ((or (eq? arg #f) (eq? arg 'false)) 'bool)
      ((is_dot_var? arg) (type (caddr arg) (get_value (cadr arg) state) instance state))
      ((and (is_class? class) (eq? (get_class_name class) arg)) 'class)
      ((and (is_class? instance) (eq? (get_class_name instance) arg)) 'obj) ; TODO fix for next part (class should be obj)
      ((eq? 'error (get_type arg class)) 
       (cond 
         ((eq? 'error (get_type2 arg state)) (display arg) (display "\n") (display class) (display "\n") (display state) (error "type: type of var returned error. Not declared in scope/before use (using before declaring)"))
         (else (get_type2 arg state))))
      (else (get_type arg class)))))

(define type_loose
  (lambda (arg class instance state)
    (cond
      ((null? arg) (error "type: null arg"))
      ((not (is_valid_state? state)) (error "type: invalid state"))
      ((comparison_operator? arg) 'bool)
      ((boolean_operator? arg) 'bool)
      ((math_operator? arg) 'int)
      ((number? arg) 'int)
      ((is_function_call? arg) 'func)
      ((or (eq? arg #t) (eq? arg 'true)) 'bool)
      ((or (eq? arg #f) (eq? arg 'false)) 'bool)
      ((is_dot_var? arg) (type (caddr arg) (get_value (cadr arg) state) instance state))
      ((and (is_class? class) (eq? (get_class_name class) arg)) 'class)
      ((and (is_class? instance) (eq? (get_class_name instance) arg)) 'obj) ; TODO fix for next part (class should be obj)
      ((eq? 'error (get_type arg class)) 'error)
      (else (get_type arg class)))))

; returns the type of a given variable
(define get_type
  (lambda (var class)
    (cond
      ((null? var) (error "get_type: null variable"))
      ((not (is_class? class)) (display class) (error "get_type: invalid class"))
      (else (check_var var (car (class_vars class)) (caddr (class_vars class)) (lambda (v) v))))))

(define get_type2
  (lambda (var state)
    (cond
      ((null? var) (error "get_type2: null variable"))
      ((not (is_valid_state? state)) (display state) (error "get_type2: invalid state"))
      (else (check_var var (get_vars state) (get_types state) (lambda (v) v))))))

; helper to get_type
(define check_var
  (lambda (var_name var_list type_list break)
    (cond
      ((or (null? var_list) (null? type_list)) (break 'error))
      ((list? (car var_list)) (check_var var_name (car var_list) (car type_list) 
                                         (lambda (type)
                                           (cond
                                             ((eq? type 'error) (check_var var_name (cdr var_list) (cdr type_list) break))
                                             (else (break type))))))
      ((eq? var_name (car var_list)) (break (unbox (car type_list))))
      (else (check_var var_name (cdr var_list) (cdr type_list) break)))))

; a wrapper/helper to (type) that will compare a statement's evaluated type to a given type.
(define type_check
  (lambda (type_to_compare arg class instance state)
    (eq? type_to_compare (type arg class instance state))))

(define type_check_loose
  (lambda (type_to_compare arg class instance state)
    (cond
      ((void? state) (error "type_check_loose: void state"))
      (else (eq? type_to_compare (type_loose arg class instance state))))))

; === type evaluation for functions ===

(define type_func
  (lambda (type_to_compare f_name class instance state)
    (cond
      ((is_dot_func? f_name) 
       (cond
         ((is_this_dot? f_name)
          (cond
            ((eq? (type_process_func (get_dot_func f_name class state) class instance state) type_to_compare) #t)
            (else (eq? (type_process_func (get_dot_func f_name class state) class instance state) 'allow))))
         ((is_super_dot? f_name)
          (cond
            ((eq? (type_process_func (get_dot_func f_name class state) (get_class_parent class) instance state) type_to_compare) #t)
            (else (eq? (type_process_func (get_dot_func f_name class state) (get_class_parent clasS) instance state) 'allow))))
         (else
          (cond
            ((eq? (type_process_func (get_dot_func f_name class state) (get_value (cadr (cadr f_name)) state) instance state) type_to_compare) #t)
            (else (eq? (type_process_func (get_dot_func f_name class state) (get_value (cadr (cadr f_name)) state) instance state) 'allow))))))
          ;(type_func type_to_compare (caddr (cadr f_name)) (get_value (cadr (cadr f_name)) state) instance state)) ; TODO need to update for 5
      ((is_dot_var? f_name) 
       (cond
         ((eq? (type_process_func (get_dot_func f_name class state) (get_value (cadr f_name) state) instance state) type_to_compare) #t)
         (else (eq? (type_process_func (get_dot_func f_name class state) (get_value (cadr f_name) state) instance state) 'allow))))
      ((eq? (get_func f_name class) 'error) (display f_name) (display "\n") (display class) (error "type_func: f_name evaled to error"))
      ((eq? (type_process_func (get_func f_name class) class instance state) type_to_compare) #t)
      (else (eq? (type_process_func (get_func f_name class) class instance state) 'allow)))))

(define type_process_func ; returns the type of return for the function
  (lambda (function class instance state)
    (cond
      ((not (is_function? function)) (display function) (error "type_process_func: can't process non-function"))
      ((not (is_valid_state? state)) (error "type_process_func: invalid state"))
      (else
       ; letrec removed
       (find_return2 (reverse-cps (get_func_body function) (lambda (v) v)) class instance state)))))

(define find_return2 
  (lambda (rvs_body class instance state)
    (cond
      ((null? rvs_body) 'error)
      ((is_while? (car rvs_body)) (find_return2 (reverse-cps (append-cps (get_while_body (car rvs_body)) (cdr rvs_body) (lambda (v) v)) (lambda (v) v)) class instance state))
      ;(is_if...
      ((is_if_then? (car rvs_body)) (find_return2 (cons (get_stmt1 (car rvs_body)) (cdr rvs_body)) class instance state))
      ;(is_if_else...
      ((is_if_then_else? (car rvs_body)) (find_return2 (list (get_stmt2 (car rvs_body)) (get_stmt1 (car rvs_body)) (cdr rvs_body)) class instance state))
      ;(is_block...
      ((is_block? (car rvs_body)) (find_return2 (append-cps (reverse-cps (get_block_body (car rvs_body)) (lambda (v) v)) (cdr rvs_body) (lambda (v) v)) class instance state))
      ((is_return? (car rvs_body))
       (cond
         ((and (type_check_loose 'func (cadr (car rvs_body)) class instance state) (is_function_call? (cadr (car rvs_body))))
          (cond
            ((not (eq? (find_return2 (cdr rvs_body) class instance state) 'error)) (find_return2 (cdr rvs_body) class instance state)) ; check for other returns)
            (else (type_process_func (call_to_function (cadr (car rvs_body)) class instance state) class instance state))))
         ((eq? (type_loose (cadr (car rvs_body)) class instance state) 'error) ; case: local variable, type not already in state
          ; letrec removed
          (type_forward2 (cadr (car rvs_body)) (cdr rvs_body) class instance state))
         (else (type (cadr (car rvs_body)) class instance state)))) ; else, it is an easily checked type
      (else (find_return2 (cdr rvs_body) class instance state)))))

(define type_forward2 
  (lambda (var rvs_body class instance state)
    (cond
      ((null? rvs_body) 'allow)
      ((and (is_assign? (car rvs_body)) (eq? var (cadr (car rvs_body)))) (cond
                                                                           ((eq? (type_loose (caddr (car rvs_body)) class instance state) 'error) 
                                                                            (type_forward2 
                                                                             (caddr (car rvs_body)) (cdr rvs_body) class instance state)) ; if it is an error, the var was set to previous var, check forward with new var
                                                                           (else (type (caddr (car rvs_body)) class instance state))))
      ((and (is_declare_assign? (car rvs_body)) (eq? var (cadr (car rvs_body)))) (cond
                                                                                   ((eq? (type_loose (caddr (car rvs_body)) class instance state) 'error) 
                                                                                    (type_forward2 
                                                                                     (caddr (car rvs_body)) (cdr rvs_body) class instance state)) ; if it is an error, the var was set to previous var, check forward with new var
                                                                                   (else (type (caddr (car rvs_body)) class instance state))))
      ((is_while? (car rvs_body)) (type_forward2 var 
                                                 (append-cps (reverse-cps (cdr (get_while_body (car rvs_body))) (lambda (v) v)) (cdr rvs_body) (lambda (v) v)) 
                                                 class instance 
                                                 state))
      ;(is_if...
      ((is_if_then? (car rvs_body)) (type_forward2 var 
                                                   (cons (get_stmt1 (car rvs_body)) (cdr rvs_body)) 
                                                   class instance 
                                                   state))
      ;(is_if_else...
      ((is_if_then_else? (car rvs_body)) (type_forward2 var 
                                                        (append-cps (list (get_stmt2 (car rvs_body)) (get_stmt1 (car rvs_body))) (cdr rvs_body) (lambda (v) v)) 
                                                        class instance 
                                                        state))
      ;(is_block...
      ((is_block? (car rvs_body)) (type_forward2 var 
                                                 (append-cps (reverse-cps (get_block_body (car rvs_body)) (lambda (v) v)) (cdr rvs_body) (lambda (v) v)) 
                                                 class instance 
                                                 state))
      (else (type_forward2 var 
                           (cdr rvs_body) 
                           class instance 
                           state)))))

(define call_to_function
  (lambda (funcall class instance state)
    (cond
      ((not (is_valid_state? state)) (error "call_to_function: invalid state"))
      ; if it is a dot funcall, rewrite+restate
      ((is_dot_func? funcall) (get_dot_func funcall class state))
      (else (get_func (cadr funcall) class)))))
