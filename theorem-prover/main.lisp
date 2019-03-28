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

(defun search_domain (domain search_clause)
  (let ((count 0)
        (resolve_list '())
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
               (setq sublist (append sublist (rest (nth i domain)))))
            );END COND
             (setq resolve_list (append resolve_list (list sublist)))
             (setq sublist '())
          );END LOOP

        );END COND TEST1
       ((equal search_function (first (nth i domain)))
        (setq sublist (append sublist (list i)))
        (setq sublist (append sublist (nth i domain)))
        (setq resolve_list (append resolve_list (list sublist)))
        (setq sublist '())
       );END COND TEST2
        );END COND

      ;(print (search_domain (first (strip_logic (nth i domain))) search_clause))
      ) ;end loop
   ;(format t "~{( ~{~S ~})~%~}" resolve_list)
  ; (print resolve_list)
    resolve_list);end let
  )

(unless (fboundp 'string-append)
  (defmacro string-append (&rest strings)
    `(concatenate 'string ,@strings)))

;Using the axiom we are trying to prove find any axioms in the domain that
;contain a negation of the axiom and return a list
(defun get_potential_resolutions (axiom)
  (let ((search_clause (strip_logic (flatten axiom))))

  ) ;end let

)
    ;TODO: ignore beginning or's and not's, then get "function"

;----------------Sandbox function for testing new code----------------------

(defun run ()

  )
