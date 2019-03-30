(defparameter *count* 0)


(defun resolve (axiom_set goal)
 (print "---------------------------------------------------------------------")
  (setq *count* (+ *count* 1))
  (let ((search_clause goal)
        (negated_search_clause '())
        (search_item '())
        (potential_resolutions '())
        (stored_potential_resolutions '())
        (stored_search_clauses '())
        (bindings '())
        temp
        temp2)
    ;Store unused search clauses for backtracking
    (print "Initial goal set to: ") (princ search_clause)
    (setq negated_search_clause (negate search_clause))
    (print "NEGATED SEARCH CLAUSE: ")(princ negated_search_clause)
    ;If negated search clause is a list of clauses select first clause to search
    (if (some #'listp negated_search_clause)
      ;If first element is a list just grab that elements first member
      (cond ((listp (first negated_search_clause))
            (cond ((some #'listp (first negated_search_clause))
                   (print "nested not")
                   (setq search_item (list 'not (rest (first negated_search_clause)))))
              (t (print "cond 1:")(princ (first negated_search_clause))
              (setq search_item (first (first negated_search_clause)))
               ))
             )
        ;If first element is not a list we have a (not (some thing))
        (t
         (print "cond 2")(princ (first (remove_nest (rest negated_search_clause))))
         (setq search_item (list 'not (first (remove_nest (rest negated_search_clause)))))))
      (setq search_item (first negated_search_clause)))

    ;  (cond ((> (list-length goal) 2)
    ;   (setq stored_search_clauses (rest goal))
    ;   (setq search_clause (negate (first search_clause)))
    ;   (print "MULTI: ")(princ search_clause))
    ;    (t (setq search_clause (search_clause)))
    ;     (print "SINGLE: ")(princ search_clause))
    ;(print "Search function: ")(princ search_clause)
    (print "search item: ")(princ search_item)
    (setq potential_resolutions (get_potential_resolutions axiom_set search_item))
    ;CHECKPOINT 1:
    (print " ")(print " ")
    (print "Checkpoint 1, resolve count: ") (princ *count*)
    (print "Potential Resolutions: ")(princ potential_resolutions)
    (cond
      ((not potential_resolutions) ;check for nil return
                                   (cond
                                     (stored_search_clauses ;If other search clauses exist
                                                            (setq search_clause (first stored_search_clauses))
                                                            (setq stored_search_clauses (rest stored_search_clauses))
                                                            (resolve axiom_set search_clause);???????????
                                                            )
                                     ;No stored search clauses + no resolutions means we cant prove this
                                     (t (print "The goal statement is proved false."))))
      ;Some amount of potential resolutions were found!

      ((> (list-length potential_resolutions) 1)
       (setq potential_resolutions (first potential_resolutions))
       (setq stored_potential_resolutions (rest potential_resolutions))))
    ;At this point we have a single search clause and single resolution axiom
    ;Check axioms to ensure our potential_resolution is compatable
    (print " ")(print " ")
    (print "Checkpoint 2: Resolutions found!")
    (print "Using resolution: ")(princ potential_resolutions)
    (setq temp potential_resolutions)
    (setq temp2 (insert_bindings potential_resolutions negated_search_clause))
    (print "RETURNED BINDINGS")(princ temp2)

    (cond ((not temp2)
           (setq potential_resolutions (remove_nest (rest (rest potential_resolutions)))))
      (t
        (setq potential_resolutions
         (apply_bindings (first temp2) (rest temp2) (remove_nest (rest (rest potential_resolutions)))))
        (setq negated_search_clause (apply_bindings (first temp2) (rest temp2) negated_search_clause))
       ))


  ;  (setq negated_search_clause (remove_nest (rest temp2)))
    ;(setq potential_resolutions (remove_nest (first temp2)))
    (print "Bindings set: ")
    (print potential_resolutions)
   (print negated_search_clause)
    (cond
      ;If bindings fail resolution failed, backtrack
      ((not potential_resolutions)
      (resolve stored_potential_resolutions search_clause))

      ;Bindings succeeded, do the resolution
      (t
       (print "RESOLVE OCCURING INPUTS: ")
       (print "Negated search clause: ")(princ negated_search_clause)
       (print "Potential Resolutions: ")(princ potential_resolutions)
       (cond ((equal negated_search_clause potential_resolutions)
         (print "Theorem successfully proven!!! :D")
         (return)))
       (let ((res_len 0)
             (clause_len 0)
             (tempvar 0))
         (if (some #'listp potential_resolutions)
           (setq res_len (list-length potential_resolutions))
           (setq res_len 1))
         (if (some #'listp negated_search_clause)
           (setq clause_len (list-length negated_search_clause))
           (setq clause_len 1))
         (print "Res_len: ")(princ res_len)
         (print "Clause_len: ")(princ clause_len)
         (if (> res_len clause_len)
           (setq tempvar (remove negated_search_clause potential_resolutions :test #'equal))
           (setq tempvar (negate (remove potential_resolutions negated_search_clause :test #'equal))));else
         (print "tempvar")(princ tempvar)
         (setq goal (remove_nest tempvar)))


       ;Remove resolved clause from the resolution

       ;(setq goal (nth (+ (nth 1 potential_resolutions) 1) potential_resolutions))
       ;(print potential_resolutions)
      ; (print "Goal 2: ")(princ goal)
       ;Remove index data at indexs 0 and 2 for clean resolve call
      ; (setq goal (rest (rest goal)))
       ;(print "Goal 3: ")(princ goal)
       (setq axiom_set (remove-nth-element (first temp) axiom_set))
       (print "Resolving with:")
       (print "Axiom set: ")(princ axiom_set)
       (print "Goal: ") (princ goal)
       (resolve axiom_set goal)
       );)
      )

    ;-----------------------
    ;Chose first goal clause and preform search, store the rest of the goal clause
    ;If no branch choose next clause etc.
    ;When branch is returned set search clause to the proper sub clause
    ;Chose first sub branch to preform resoluton on, store rest of branch
    ;If BRANCH SUCCEED:
    ;new branch will be returned, remove used branch from axiom set
    ;take first clause from branch, negate and make new goal clause, search
    ;for new branch...

    ;If BRANCH FAIL backtrack to stored branch and attempt resolution again
    ;If all branches fail backtrack to goal clause, chose next option, and
    ;search for branches




    ;------------------------
    ;returns list of possible resolution paths, we always resolve with the first
    ;path (not a great heuristic, but easier coding wise)
    ;(setq current_branch (first (get_potential_resolutions axiom_set goal)))
    ;(get_bindings current_branch search_clause) ;Nothing to do for these atm
    ;get_bindings successful return means we can preform our resolve

    ;(setq next_branch (remove-nth-element (+ (nth 1 current_branch) 1) current_branch))
    ;remove used axiom from the domain
    ;(setq axiom_set (remove-nth-element (first next_branch) axiom_set))
    ;(print (rest (rest next_branch)))
    ;(resolve axiom_set (rest (rest next_branch)))
    )
  (setq *count* 0))
