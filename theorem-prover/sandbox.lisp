(defun run ()
  (load "main.lisp")
  (let
    ((domain (get_axioms 'garden))
     (theory (get_theory 'true)))

    ;test domain search
    (search_domain domain theory)
    );end let
;(print "END")
  )



(defun test_unify ()
  (load "unify.lisp")
  (add-binding ('?a 'cat (list bindings)))
  )
