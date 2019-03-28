;Returns a predefined axiom set based on the provided domain
(defun get_axioms (domain)
  (cond
    ((equal 'garden domain)
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
    (t (error "Error: Invalid Domain!"))))

;Returns a predefined axiom to prove for a certain axiom set
(defun get_theory (axiom)
  (cond
    ((equal 'true axiom)
      '(grow john carrots))
    (t (error "Error: Invalid clause!"))))

;Negate the axiom specefied in get_theory so that we may prove it using
;resolution
;TODO: make more robust, handle different possible theorums to prove
(defun negate (axiom)
  (list 'not axiom)
  )

(defun flatten (L)
  (unless (endp L) ;Check for base case so we know when to stop
    (if (atom (first L)) ;Check if first item is another list
      (cons (first L) (flatten (rest L))) ;If it is we recusively call flatten
      (append (flatten (first L)) (flatten (rest L)))))) ;Add the lists together

;strips any leading OR's and NOT's from a list
(defun strip_logic (axiom)
(cond
  ((equal 'not (first axiom))
    (strip_logic (rest axiom)))
  ((equal 'or (first axiom))
    (strip_logic (rest axiom)))
  (t axiom)))

(defun get_potential_resolutions (domain search_clause)
  (let ((resolve_list '())
        (sublist '())
        (tmp '())
        (search_function (first search_clause))) ;TODO: allow this to properly search 'NOT function'
    (loop for i from 0 to (- (list-length domain) 1) do ;Loop through all axioms
      (cond
        ((some #'listp (nth i domain)) ;Axiom has sublist (OR'd)
         ;we loop from 1 because if there are sublists we know first member of
         ;list will be OR based on how our domains are defined in CNF
         (loop for j from 1 to (- (list-length (nth i domain)) 1) do
           (setq tmp (flatten (nth j (nth i domain)))) ;Item within axiom
           (cond
             ;checks to see if any of sub-axioms can match with search_clause
             ((equal search_function (first tmp)) ;see search_function comment
               (setq sublist (append sublist (list i))) ;append index
               (setq sublist (append sublist (list j)))
               (setq sublist (append sublist (rest (nth i domain))))))
             (setq resolve_list (append resolve_list (list sublist)))
             (setq sublist '())))
       ((equal search_function (first (nth i domain)))
        (setq sublist (append sublist (list i)))
        (setq sublist (append sublist (list 1))) ;dummy sublist index
        (setq sublist (append sublist (list(nth i domain))))
        (setq resolve_list (append resolve_list (list sublist)))
        (setq sublist '())))) ;end loop
    ;returns list of list of axioms and their index that have search function
    ;match
    (remove nil resolve_list)))

(defun get_bindings (current_branch theory)
  (let ((arguments (rest(flatten theory)))
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
            (print "All good!") ;all good, dont need to do anything
            (error "Argument mismatch while binding!")
             )
           )
           )
         )
        ) ;end cond
      );end loop
    bindings);end let
      )

(defun resolve (domain theory)
  (let ((search_clause (flatten theory))
        (current_branch)
        (next_branch))
    ;returns list of possible resolution paths, we always resolve with the first
    ;path (not a great heuristic, but easier coding wise)
    (setq current_branch (first (get_potential_resolutions domain theory)))
    (get_bindings current_branch search_clause) ;Nothing to do for these atm
    ;get_bindings successful return means we can preform our resolve

    (setq next_branch (remove-nth-element (+ (nth 1 current_branch) 1) current_branch))
     ;remove used axiom from the domain
    (setq domain (remove-nth-element (first next_branch) domain))
    ;(print (rest (rest next_branch)))
    (resolve domain (rest (rest next_branch)))
    ) ;end let

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
