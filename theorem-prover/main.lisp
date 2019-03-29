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

;Removes the outer list from a list if outer list has only 1 member
(defun remove_nest (L)
  (if (atom (rest L))
     (first L)
    L))


;Provides negation for an axiom
;Works on single and series of clauses
(defun negate (goal)
  ;Axiom consists of some amount of nested lists
  (cond ((some #'listp goal)
         ;strip leading OR if it exists
         (if (equal (first goal) 'or)
           (setq goal (rest goal)))
         ;Check and see if first element of goal is a list, if not we have the
         ;(not (some list)) case and just want to take (some list)
         (cond ((not (listp (first goal)))
                (remove_nest(rest goal)))
           ;Goal contains 2 or more lists
           (t
            (let ((negated_goal '()))
              (loop for item in goal do
                ;Remove a not for the negation
                (cond ((equal (first item) 'not)
                       (setq negated_goal (append negated_goal (rest item))))
                  ;Add in a not for the negation
                  (t
                   (setq item (append (list 'not) (list item)))
                   (setq negated_goal (append negated_goal (list item))))))
              ;Somewhere im double nesting my list, this removes the double nest
              (remove_nest negated_goal)))))
    ;Passed just one list, this is the (some case) case and just need to add in
    ;a not to get (not (some case))
    (t (cond ((equal (first goal) 'not)
              (rest goal))
         (t (list 'not goal))))))



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

    (defun insert_bindings (resolution search)
      (let ((arguments (rest(flatten search)))
            bindings
            temp)
        (setq resolution (rest resolution));strip out master index
        (setq temp resolution)
        (setq resolution (rest (nth (first resolution) resolution))) ; Grab correct sublist
      ;  (print "args: ")(princ arguments)
      ;  (print "resolution" )(princ resolution)
        (loop for i from 0 to (- (list-length resolution) 1) do
          (cond
            ((variable? (nth i resolution)) ;SEARCH BRANCH ARG IS VARIABLE
             (setq bindings (append (list (nth i arguments) (nth i resolution))))
             (setf (nth (first temp) temp) search)
        ;     (print "new res")(princ temp)
                  )
            (t ;SEARCH BRANCH ARG IS NOT VARIABLE
             (cond
               ((variable? (nth i arguments)) ;If base arg is a variable just bind as normal
                (setq bindings (append (list (nth i arguments) (nth i resolution))))
                )
              ;If branch arg is ALSO not a varible ensure that branch arg and base
              ;arg match
              (t
               (if (equal (nth i arguments) (nth i resolution))
                () ;all good, dont need to do anything
                (error "Argument mismatch while binding!")
                 )
               )
               )
             )
            ) ;end cond
          );end loop
        bindings
        );end let
          )

(defun help (bindings L)
  (subst (first bindings) (rest bindings) L) )
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



(defun apply_bindings (new old L)
(cond ((equal old L)
       (list new))
      ((atom L)
       L)
      (t
       (cons (apply_bindings new old (car L))
             (apply_bindings new old (cdr L))))))
