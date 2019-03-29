;Goal is listed in CNF as well
;IF more than one clause in the goal they must therefore be OR'd together
;This means we just have to prove 1 and disprove the other

(defun run ()
  (load "main.lisp")
  ;(defvar *count* 0)
  (let
    ((axiom_set (get_axiom_set 'garden))
     (goal (get_goal 'true))
     (negated_goal))
     (negate goal)
    )
    ;(print "END")
    )
