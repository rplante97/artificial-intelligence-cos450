;Goal is listed in CNF as well
;IF more than one clause in the goal they must therefore be OR'd together
;This means we just have to prove 1 and disprove the other

(defun run ()
  (load "main.lisp")
  (load "resolver.lisp")
  ;(defvar *count* 0)
  (let
    ((axiom_set (get_axiom_set 'garden))
     (goal (get_goal 'true))
     (negated_goal))
     (setq negated_goal (negate goal))
     (resolve axiom_set negated_goal)
    )
    ;(print "END")
    )

(defun z ()
  (let ((var1 '?a)
        (var2 'trucks)
        (L '((not (LIKE JOHN ?a)) (NOT (VEGETABLE ?a)))))
   (mapcar (lambda (lst) (subst var2 var1 lst)) L)
   ))
