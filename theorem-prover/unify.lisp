; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*File%20info/modification%20history][File info/modification history:1]]
;;; Author: Roy Turner <rturner@maine.edu>, UMaine
;;; Date: Created: 1/25/90
;;; Modifications:
;;;   o 3/14/90: prepared for class
;;;   o 13:04 - Thu Nov 5, 1998 -rmt- Modifications added for COS 470, F98:
;;;      o newSymbol symbol generator added.  Call is:
;;;             (newsymbol <foo>)
;;;        where <foo> is a symbol or a string.  A new, unique symbol based
;;;        on that is returned.  Any trailing numerals are stripped from the 
;;;        symbol, and then a new number is appended to make a unique name.
; File info/modification history:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*Symbol%20generator%20code][Symbol generator code:1]]
(defclass SymbolGenerator ()
  ((counterTable :initform (make-hash-table :test #'equal))))
; Symbol generator code:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*Symbol%20generator%20code][Symbol generator code:2]]
(defun newSymbol (&optional prefix &key (package) (intern t))
  (with-slots (counterTable) *symbolGenerator*
   (let (num sym) 
     (cond
       ((symbolp prefix)
	;; convert to string, call again:
	(newSymbol (symbol-name prefix) :package package :intern intern))
       ((stringp prefix)
	;; get rid of trailing numerals:
	(setq prefix (string-right-trim "0123456789" prefix))
	;; try new symbol names until we find one that is not in use:
	(loop do
	      (cond
	       ((setq num (gethash prefix counterTable))
		;; number exists for this prefix -- new number is just incremented
		;; one: 
		(setq num (1+ num))
		(setf (gethash prefix counterTable) num))
	       (t
		;; no number yet:
		(setf (gethash prefix counterTable) 1)
		(setq num 1)))
	    until (not (find-symbol
			(setq sym (string-append prefix 
						 (princ-to-string num))))))
	;; found one, create the symbol...
	(setq sym (make-symbol sym))
	(when intern			     ;then intern the symbol:
	  (setq sym (if package
		      (intern (format nil "~a~s" prefix num) package)
		      (intern (format nil "~a~s" prefix num)))))
	sym)
       (t
	;; then can't do any better than regular old gensym:
	(gensym))))))
; Symbol generator code:2 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*Symbol%20generator%20code][Symbol generator code:3]]
(defvar  *SymbolGenerator* (make-instance 'SymbolGenerator))
; Symbol generator code:3 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*Package-related%20bookkeeping][Package-related bookkeeping:1]]
(defvar *intern-package* (or (find-package "CL-USER") *package*)
  "Package in which to intern symbols created by unify, etc., functions.")
; Package-related bookkeeping:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*Macros][Macros:1]]
(unless (fboundp 'string-append)
  (defmacro string-append (&rest strings)
    `(concatenate 'string ,@strings)))
; Macros:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*Macros][Macros:2]]
(set-macro-character #\?
   #'(lambda (stream char)
	(let ((next-char (peek-char nil stream))
	      next foo)
	  (cond
	   ((equal next-char #\))
	    ;;it's a paren, so it's invalid as a variable...just
	    ;; return symbol ?
	    (setq foo (intern "?" *intern-package*))
	    foo)
	   ((equal next-char #\space)
	    (setq foo (intern "?" *intern-package*))
	    foo)
	   (t
	    (setq next (read stream t nil t))
	    (cond
	     ((atom next)
	      ;;return ?atom
	      (multiple-value-bind (thing dummy)
				   (intern  (string-append (string #\?)
							   (symbol-name next)))
				   thing))
	     (t
	      `(*var* ,next)))))))
   t)
; Macros:2 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*unify][unify:2]]
(defun unify (p1 p2 &optional bindings)
  (cond
   ((variable? p1)
    (unify-variable p1 p2 bindings))
   ((variable? p2)
    (unify-variable p2 p1 bindings))
   ((and (atom p1) (atom p2)) 
    (unify-atoms p1 p2 bindings))
   ((and (listp p1) (listp p2))
    (unify-elements p1 p2 bindings))
   (t (values nil bindings))))
; unify:2 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*unify-atoms][unify-atoms:1]]
(defun unify-atoms (p1 p2 bindings)
  (values (eql p1 p2) bindings))
; unify-atoms:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*unify-elements][unify-elements:1]]
(defun unify-elements (p1 p2 bindings)
  (let (blist matched?)
    (multiple-value-setq (matched? blist)
      (unify (first p1) (first p2) bindings))
    (cond
     ((null matched?)
      (values nil bindings))
     ((multiple-value-setq (matched? blist)
	(unify (rest p1) (rest p2) blist))
      (values matched? blist))
     (t
      (values nil bindings)))))
; unify-elements:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*unify-variable][unify-variable:1]]
(defun unify-variable (p1 p2 bindings)
  (cond
   ((eql p1 p2)
    (values t bindings))
   (t
    (let ((binding (find-binding p1 bindings)))
      (if binding
	  (unify (extract-value binding) p2 bindings)
	(if (inside? p1 p2 bindings)
	    (values nil bindings)
	    (values t (add-binding p1 p2 bindings))))))))
; unify-variable:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*find-binding][find-binding:1]]
(defun find-binding (var bindings &optional not-one-of)
  (let ((binding
	 (car (member var bindings
		      :test #'(lambda (a b)
				(let ((poss (cond
					     ((eql a (car b))
					      (cadr b))
					     ((eql a (cadr b))
					      (car b)))))
				  (when (and poss
					     (not (member poss not-one-of)))
				    t)))))))
    (cond
     ((null binding) (values nil nil))
     ((eql var (car binding))
      (values binding t))
     (t (list var
	      (values (car binding) t))))))
; find-binding:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*extract-value][extract-value:1]]
(defun extract-value (binding)
  (cadr binding))
; extract-value:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*inside?%20and%20inside-or-equal?][inside? and  inside-or-equal?:1]]
(defun inside? (var expr bindings)
  (if (equal var expr)
      nil
      (inside-or-equal? var expr bindings)))

(defun inside-or-equal? (var expr bindings)
  (cond
   ((equal var expr) t)
   ((and (not (variable? expr)) (atom expr)) nil)
   ((variable? expr)
    (let ((binding (find-binding expr bindings)))
      (when binding
	(inside-or-equal? var (extract-value binding) bindings))))
   (t (or (inside-or-equal? var (first expr) bindings)
	  (inside-or-equal? var (rest expr) bindings)))))
; inside? and  inside-or-equal?:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*add-binding][add-binding:1]]
(defun add-binding (var val bindings)
  (if (eq '_ var)
      bindings
      (cons (list var val) bindings)))
; add-binding:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*variable?][variable?:1]]
(defun variable? (thing)
  (or (and (listp thing)
           (equal (car thing) '*var*))
      (and (symbolp thing)
           (equal (char (symbol-name thing) 0)
                  #\?))))
; variable?:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*varname][varname:1]]
(defun varname (var)
  (cond
    ((and (consp var)
          (consp (cdr var)))
     (cadr var))
    ((equal (char (string var) 0) #\?)
     (intern  (string-left-trim '(#\?) (string var))
	      (find-package *intern-package*)))))
; varname:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*make-var][make-var:1]]
(defun make-var (var)
  (intern (concatenate 'string "?" 
		       (cond
			((stringp var) var)
			(t (symbol-name var))))))
; make-var:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*instantiate][instantiate:1]]
(defun instantiate (thing bindings &key (if-unbound :first))
  (cond
   ((variable? thing)
    (instantiate-variable thing bindings :if-unbound if-unbound))
   ((atom thing)
    thing)
   (t
    (cons (instantiate (car thing) bindings :if-unbound if-unbound)
	  (instantiate (cdr thing) bindings :if-unbound if-unbound)))))
; instantiate:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*instantiate-variable][instantiate-variable:1]]
(defun instantiate-variable (var bindings &key (if-unbound :first))
  (multiple-value-bind (found val)
      (inst-var var bindings)
    (cond
     (found val)
     ((eql if-unbound :first)
      var)
     ((eql if-unbound :last)
      (cadr val))
     (t
      if-unbound))))
; instantiate-variable:1 ends here

; [[file:~/WebSites/COS470/assignments/rtp/unify.org::*instantiate-variable][instantiate-variable:2]]
(defun inst-var (var bindings &optional (depth 0))
  (loop with deeper-var = nil 
      for binding in bindings
      do
	(when (member var binding)
	  (let (found
		(val (if (eql var (car binding) )
			 (cadr binding)
		       (car binding))))
	    (cond
	     ((not (variable? val))
	      (return (values t val)))
	     ((multiple-value-setq (found val)
		(inst-var val
			  (remove binding bindings :test #'equal)
			  (1+ depth)))
	      (return (values t val)))
	     ((variable? (cadr val))
	      (when (or (null deeper-var)
		      (> (car val) (car deeper-var)))
		(setq deeper-var val))))))
      finally
	;; if we get here, we haven't returned from the things above --
	;; meaning we haven't found var in bindings at all!  In this case, we
	;; need to return the variable itself as the value, though noting that
	;; we haven't found a real binding.
	(return (values nil 
			  (if (or (null deeper-var)
				(<= (car deeper-var) depth))
			    (list depth var)
			  deeper-var)))))
; instantiate-variable:2 ends here
