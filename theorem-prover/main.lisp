(defparameter *count* 0)


(defun resolve (axiom_set goal) ;We search for negation of negated goal
  (setq *count* (+ *count* 1))
  (let ((search_clause (negate (list goal)))
      (potential_resolutions '())
      (stored_potential_resolutions '())
      (stored_search_clauses '())
      (bindings '()))
  ;Store unused search clauses for backtracking
  (if (> (list-length search_clause) 1)
    (setq stored_search_clauses (rest goal)))
  (setq search_clause  (first search_clause))
  (print "Search function: ")(princ search_clause)

  (setq potential_resolutions (get_potential_resolutions axiom_set (first search_clause)))
  ;CHECKPOINT 1:
  (print "Checkpoint 1, resolve count: ") (princ *count*)
  (print "Search clause: ") (princ search_clause)
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
 (print "Checkpoint 2: Resolutions found!")
 (print "Search clause: ") (princ search_clause)
 (print "Using resolution: ")(princ potential_resolutions)

 (setq bindings (get_bindings potential_resolutions search_clause))
 (print "Bindings set: ") (princ bindings)
 (cond
   ((not bindings) ;If bindings fail resolution failed, backtrack
    (resolve stored_potential_resolutions search_clause))
   ;Bindings succeeded, do the resolution
   (t
     ;Remove resolved clause from the resolution
     (setq goal (remove-nth-element (+ (nth 1 potential_resolutions) 1) potential_resolutions))
     ;Remove index data at indexs 0 and 2 for clean resolve call
     (setq goal (rest (rest goal)))

     (setq axiom_set (remove-nth-element (first potential_resolutions) axiom_set))
     (print "Resolve successful, recursing!")
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









;Returns a predefined axiom set based on the provided domain
(defun get_axiom_set (axiom_set)
  (cond
    ((equal 'garden axiom_set)
     '(
       (like john carrots)
       (like mary carrots)
       (or (not (like john ?a)) (not (vegetable ?a)) (grow john ?a))
       (vegetable carrots)
       (or (not (like ?b ?c)) (vegetable ?c) (grow ?b ?c))
       (or (not (eat ?d ?e)) (own ?d ?e))
       (or (not (grow ?f ?g)) (own ?f ?g))
       (or (not (grow ?h ?i)) (own ?h ?sk_hi))
       (or (not (grow ?j ?k)) (garden ?sk_jk))
      ;fake axioms for testing
      (grow gord pail)
       ))
    (t (error "Error: Invalid axiom set!"))))

;Returns a predefined axiom to prove for a certain axiom set
(defun get_goal (goal)
  (cond
    ((equal 'true goal)
      '(grow john carrots))
    (t (error "Error: Invalid goal!"))))

;Helper function to flatten lists
(defun flatten (L)
  (unless (endp L) ;Check for base case so we know when to stop
    (if (atom (first L)) ;Check if first item is another list
      (cons (first L) (flatten (rest L))) ;If it is we recusively call flatten
      (append (flatten (first L)) (flatten (rest L)))))) ;Add the lists together

;Provides negation for an axiom
;Works on single and series of clauses
(defun negate (goal)
  (cond
    ((some #'listp goal) ;Provided axiom contains multiple clauses
    ;strip leading OR if it exists
    (if (equal (first goal) 'or)
     (setq goal (rest goal)))
    (let ((negated_goal '()))
    (loop for item in goal do
      (cond ((equal (first item) 'not) ;Remove the not
           (setq negated_goal (append negated_goal (list (rest item)))))
        (t ;Add in a not
           (setq item (append (list 'not) item))
           (setq negated_goal (append negated_goal (list item))))))
      negated_goal))
   ;No sublist means the passed axiom is a single clause and did not contain a
   ;OR or NOT
   (t (append (list 'not) goal))))


;strips any leading OR's and NOT's from an axiom
(defun strip_logic (axiom)
(cond
  ((equal 'not (first axiom))
    (strip_logic (rest axiom)))
  ((equal 'or (first axiom))
    (strip_logic (rest axiom)))
  (t axiom)))

(defun get_potential_resolutions (axiom_set goal)
  (let ((resolve_list '())
        (sublist '())
        (tmp '())
        (search_function goal)) ;TODO: allow this to properly search 'NOT function'
    (loop for i from 0 to (- (list-length axiom_set) 1) do ;Loop through all axioms
      (cond
        ((some #'listp (nth i axiom_set)) ;Axiom has sublist (OR'd)
         ;we loop from 1 because if there are sublists we know first member of
         ;list will be OR based on how our domains are defined in CNF
         (loop for j from 1 to (- (list-length (nth i axiom_set)) 1) do
           (setq tmp (flatten (nth j (nth i axiom_set)))) ;Item within axiom
           (cond
             ;checks to see if any of sub-axioms can match with search_clause
             ((equal search_function (first tmp)) ;see search_function comment
               (setq sublist (append sublist (list i))) ;append index
               (setq sublist (append sublist (list j)))
               (setq sublist (append sublist (rest (nth i axiom_set))))))
             (setq resolve_list (append resolve_list (list sublist)))
             (setq sublist '())))
       ((equal search_function (first (nth i axiom_set)))
        (setq sublist (append sublist (list i)))
        (setq sublist (append sublist (list 1))) ;dummy sublist index
        (setq sublist (append sublist (list(nth i axiom_set))))
        (setq resolve_list (append resolve_list (list sublist)))
        (setq sublist '())))) ;end loop
    ;returns list of list of axioms and their index that have search function
    ;match
    (remove nil resolve_list)))

(defun get_bindings (current_branch goal)
  (let ((arguments (rest(flatten goal)))
        bindings)
    (setq current_branch (rest current_branch));strip out master index
    (setq current_branch (rest (nth (first current_branch) current_branch))) ; Grab correct sublist
    (loop for i from 0 to (- (list-length current_branch) 1) do
      (cond
        ((variable? (nth i current_branch)) ;SEARCH BRANCH ARG IS VARIABLE
         (setq bindings (append (list (nth i arguments) (nth i current_branch))))
              )
        (t ;SEARCH BRANCH ARG IS NOT VARIABLE
         (cond
           ((variable? (nth i arguments)) ;If base arg is a variable just bind as normal
            (setq bindings (append (list (nth i arguments) (nth i current_branch))))
            )
          ;If branch arg is ALSO not a varible ensure that branch arg and base
          ;arg match
          (t
           (if (equal (nth i arguments) (nth i current_branch))
            () ;all good, dont need to do anything
            (error "Argument mismatch while binding!")
             )
           )
           )
         )
        ) ;end cond
      );end loop
    bindings);end let
      )


    ;TODO: ignore beginning or's and not's, then get "function"

;----------------Sandbox function for testing new code----------------------
(defun remove-nth-element (nth alist)
  (cond
   ((equal nil alist) alist)
   ((zerop nth) (cdr alist))
   (t (cons (car alist)
            (remove-nth-element (- nth 1)
                                (cdr alist))))))


(defun variable? (thing)
    (or (and (listp thing)
             (equal (car thing) '*var*))
        (and (symbolp thing)
             (equal (char (symbol-name thing) 0)
                    #\?))))
