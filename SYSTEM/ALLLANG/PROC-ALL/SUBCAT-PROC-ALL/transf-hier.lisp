(in-package "USER")

; **** debug function; to be cancelled **************
(defun xsa (x) 
   (format t "SUBC--> ~a~%" (get x 'subcat-patterns))
   (format t "PAR --> ~a~%" (get x 'grclass-parents)))

;**************************************************************
;**************************************************************
;**************************************************************
; *** the next functions apply the transformations.
;     They are called from the function that travels across
;     the tree of subcategorization classes (transform-hierarchy)

; $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
; 		ACTUAL TRANSFORMATION
; $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

; *** this establishes if a pattern is preferred over another;
;     three criteria are applied:
;     1- Patterns obtained via less transformations are always preferred
;	 the number of transformations is stored in the fourth element of the
;        pattern
;     2- With equal number of transformations, patterns obtained from
;        preferred classes are preferred. Preferred classes
;        are declared in SUBCATS/grvocab/*CLASS-PREFERENCES*
;     3- With equal number of transformations, patterns obtained via
;        preferred transformations are preferred. Preferred transformations
;        are declared in SUBCATS/grvocab/*TRANSF-PREFERENCES*
; *** if the two patterns are not comparable, returns nil
; VERSIONE CON PREFERENZA AL NUMERO DI TRASF *****************
(defun pattern-precedes (patt1 patt2)
  (declare (special *CLASS-PREFERENCES* *TRANSF-PREFERENCES*))
  (let ((basic1 (list (first (get-subc-transfs patt1))))
  	(basic2 (list (first (get-subc-transfs patt2))))
  	(tr1 (rest (get-subc-transfs patt1)))
  	(tr2 (rest (get-subc-transfs patt2))))
   (cond 
         ((< (length tr1) (length tr2)) t)
         ((> (length tr1) (length tr2)) nil)
         ((= (length tr1) (length tr2))
	   (cond ((patt-pref *CLASS-PREFERENCES* basic1 basic2) t)
    	 	 ((patt-pref *CLASS-PREFERENCES* basic2 basic1) nil)
	         ((patt-pref *TRANSF-PREFERENCES* tr1 tr2) t)
		 (t nil)))
	 (t nil))))

; *** actually, this evaluates if a pattern strictly precedes 
;     another. So, when they are not comparable, it returns nil
(defun patt-pref (preferences tr1 tr2)
; *** tr1 and tr2 are lists of applied transformations
  (cond ((null preferences) nil)
	((memq (first (first preferences)) tr1)
	  (cond ((intersection tr2 (second (first preferences))) t)
		(t nil)))
	(t (patt-pref (rest preferences) tr1 tr2))))

; *************************************************************************
; *** this changes the applicability conditions of the input pattern
(defun change-appl-cond (old-pattern tr-name new-cond)
  (cond ((check-tr-compatibility tr-name (get-subc-name old-pattern))
          (let ((new-cond
                    (put-in-and (normalize (get-subc-applc old-pattern))
			        (normalize (put-not new-cond)))))
     	       (cond ((null new-cond) nil)
	   	     (t (list (get-subc-name old-pattern)	; name
	 	     	      new-cond
	 	     	      (get-subc-prefs old-pattern)
	 	     	      (get-subc-transfs old-pattern)
		     	      (get-subc-cases old-pattern))))))
	(t old-pattern)))

; *************************************************************************
; *** checks if the transformation about to be applied is compatible with
;     all transformations applied to get the old pattern
(defun check-tr-compatibility (tr-name old-name)
  (declare (special *INCOMPATIBLE-TRANSF*))
  (null (intersection
 		(first (leggi *INCOMPATIBLE-TRANSF* tr-name))
		(get-subc-transfs (get-inthn-patterns old-name)))))

; *********************************************************************************
(defun apply-transform (old-pattern transf-rule)
; *** this applies a transformation rule
; *** it gets in input:
;	old-pattern: the subcat-pattern to transform. It is a list including:
;	   - pattern name (i.e. the class name, as 'basic-trans')
;	   - applicability conditions
;	   - internal preferences (ex. after impersonalization the head verb
;	     should usually precede the object)
;	   - list of applied transformations
;	   - actual pattern, which in turn is a triple 
;		<surface-pattern, surface-labels, labels>
;	transf-rule: a transformation rule in its standard format (see the
;	   comments above to gr-readtransf)
; *** it returns a new pattern, which has the same structure as the old
;     pattern.
 (let ((old-name (get-subc-name old-pattern))
       (old-cond (get-subc-applc old-pattern))
       (old-order (get-subc-prefs old-pattern))
       (old-pref (get-subc-transfs old-pattern))
       (superf-pattern (get-surf-def (get-subc-cases old-pattern)))
       (s-labels (get-surf-lab (get-subc-cases old-pattern)))
       (labels (get-deep-lab (get-subc-cases old-pattern)))
       (transf-name (get-transf-name transf-rule))
       (transf-cond (get-transf-cond transf-rule))
       (transf-order (get-transf-order transf-rule)))
  (let ((new-cond (put-in-and (normalize old-cond) (normalize transf-cond)))
	(new-order (put-in-and (normalize old-order) (normalize transf-order))))
; *** if the resulting condition (new-cond) is contradictory (nil) or the
;     new transformation is not compatible with the ones already applied, the
;     transformation cannot be applied: return nil
    (cond ((or (null new-cond)
	       (eq transf-name old-name) ; CAN old-name (e.g. TRANS) be equal to
                                         ; a transf-name (e.g. PASSIVIZATION) ???
	       (not (check-tr-compatibility transf-name old-name))) nil)
; *** otherwise, loop on the different pieces of transformation, and at the
;     end return the new transformed pattern
	  (t (dolist (nexttr (get-transf-modif transf-rule)
		(cond ;((eq superf-pattern 'fail) nil)
	   	      (t (list (concatl (put-separator '+ 
						(list old-name transf-name)))
		 		new-cond
				new-order
				(append1 old-pref transf-name)
		 		(list superf-pattern s-labels labels)))))
    ; (cond ((eq (first transf-rule) 'object-and-ind-object-raising) (break "here2")))
   (let ((oper (first nexttr)))
	(case oper
; *** add-case just adds a new superficial case description and a new label:
; *** (add-case "new-pattern" "new-label")
	   (add-case 
	      (cond ((neq superf-pattern 'fail)
		  	(setq superf-pattern 
				(cons (second nexttr) superf-pattern))
			(setq s-labels (cons (third nexttr) s-labels))
			(setq labels (cons (fourth nexttr) labels)))))
; *** cancel-case deletes one of the original cases; it gets in input the
;     label of the case to be deleted: (cancel-case "label")
	   (cancel-case
	      (cond ((neq superf-pattern 'fail)
			(setq superf-pattern 
     ; *** the second of 'nexttr' is the case to cancel (e.g. s-subj)
				(remov-case-sp (inlist (second nexttr))
				      		s-labels superf-pattern old-name))
			(setq s-labels 
				(remov-case-lab (inlist (second nexttr)) s-labels)))))
; *** modif-case changes the superficial description of one of the original
;     cases; it gets in input the label of the case to be modified, and the
;     description of the modification. 
;     A particularly complex form for the modification is 
;		(insert-prep prep condit)
;     It specifies that the surface definition of the case must be given in
;       term of "prep". This is used for passivization. I'm not certain 
;       about how to use the condit, which specifies that passivized subjects
; 	cannot be verbs (but consider "Sono stato stressato dall'andare a
;       fare il concorso"). This would involve a "down" condition on a
;       surface description; actually, this could be supported by the
;       standard non-verbal rules. I'm not sure about what happens with
;	verbal rules. But I try!
	   (modif-case
; *** complab is the pattern label which is compatible with the transformation
;     label (if any)
		(let ((complab 
			(second
			    (member-compatib-lab 
     ; *** the second of 'nexttr' is the name change (e.g. (s-subj s-agtcompl))
     ; *** rem-surf-marker returns the deep name (e.g. subj from s-subj)
				(rem-surf-marker (first (second nexttr)))
				 s-labels))))
     ; *** now, complab is the surface label (among the ones present in the
     ;     original pattern) compatible with the one to modify (i.e. s-subj,
     ;     if complab is 'subj' and s-labels is '(s-obj s-subj)')
		       (cond ((not (null complab))
 				(setq superf-pattern
     ; *** update-case does the actual work; 'third nexttr' is the description
     ;     of the transformation of the case
	 			   (update-case 
					complab
					(third nexttr)
			         	s-labels
					superf-pattern))
				(setq s-labels
				   (my-subst
     ; *** 'second of second of nexttr' is the new surface name (e.g. s-agtcompl)
					(second (second nexttr)) 
					complab
					s-labels))))))
; *** exactly as modif-case, but if the case to be modified does not occur
;     among the existing cases, nothing is done
	   (conditional-modif-case
	      (cond ((and (neq superf-pattern 'fail)
			  (member (first (second nexttr)) s-labels))
	 		(setq superf-pattern
	   	    		(update-case (first (second nexttr))
					 (third nexttr)
			         		s-labels superf-pattern))
			(setq s-labels (my-subst (second (second nexttr))
				      		(first (second nexttr))
				      		s-labels)))))))))))))

; *** as a standard subst, but oldvals could be a list, in which case any
;     element is oldvals must be substituted by newval
(defun my-subst (newval oldvals lis)
  (cond ((null lis) nil)
	((memq (first lis) (inlist oldvals)) (cons newval (rest lis)))
	(t (cons (first lis) (my-subst newval oldvals (rest lis))))))

; *************************************************************************
; *** This extracts from a formula all predicates marked as !overcome
;     returns two values: the original formula, where the !overcome
;     markers are removed, and the list of overcome predicates:
;     (and (P) (!overcomes (Q)) (or (R) (!overcomes (not (S)))))
;     --->
;     (and (P) (Q) (or (R) (not (S)))) +
;     ((Q) (not (S)))
(defun find-overcome-cond (condit)
 (let (resexpr resbuff tempexpr tempbuff)
  (cond ((null condit) (values nil nil))
        ((eq condit t) t)
  ; *** found: return the two values (N.B. !overcome cannot be nested)
        ((eq (first condit) '!overcomes)
           (values (second condit) (list (second condit))))
  ; *** binary operator iterate the extraction of !overcome on all arguments
        ((memq (first condit) '(and or))
           (setq resexpr (list (first condit)))
           (setq resbuff nil)
           (dolist (argument (rest condit))
             (multiple-value-setq (tempexpr tempbuff)
                    (find-overcome-cond argument))
             (setq resexpr (append1 resexpr tempexpr))
             (setq resbuff (append resbuff tempbuff)))
           (values resexpr resbuff))
  ; *** not: look for !overcome in its operand
        ((eq (first condit) 'not)
           (multiple-value-setq (resexpr resbuff)
                    (find-overcome-cond (second condit)))
           (values (list 'not resexpr) resbuff))
  ; *** simple predicate: return unchanged
        (t (values condit nil)))))
 
; *************************************************************************
(defun remove-overcome-cond (old-cond overcome-cond)
 (let (resform temp)
  (cond ((null old-cond) nil)
        ((eq old-cond t) t)
    ; *** if overcome-cond is (... (not (is-passive)) ...) and 
    ;     old-cond is (is-passive), this is the first case where the old
    ;     condition must be removed (i.e. ignored)
        ((member (list 'not old-cond) overcome-cond :test #'equal) t)
        ((memq (first old-cond) '(and or))
           (setq resform (list (first old-cond)))
           (dolist (argument (rest old-cond))
              (setq temp (remove-overcome-cond argument overcome-cond))
              (cond ((neq temp t) (setq resform (append1 resform temp)))))
           (cond ((= (length resform) 2) (second resform))
                 ((= (length resform) 1) t)
    ; *** the branch above is slightly inappropriate for "or", but I cannot
    ;     imagine any situation where this can happen for "or"
                 (t resform)))
        ((eq (first old-cond) 'not)
    ; *** This is the inverse as the above: if overcome-cond is  (... (is-passive) ...)
    ;     and old-cond is (not (is-passive)), this is the second case where the
    ;     old condition must be removed (i.e. ignored)
           (cond ((member (second old-cond) overcome-cond :test #'equal) t)
                 (t (list 'not
                        (remove-overcome-cond (second old-cond) overcome-cond)))))
    ; *** simple predicate
        (t old-cond))))

; *************************************************************************
; *** puts two conditions in and; if one of them is t, the other one is left
;     if one or both of them is an and-ed condition, the top-level 'and' is
;     stripped off, before starting the simplification, made by 'simplify-and'
; *** Ex: cond1 = (and c1 c2 c3)
;	  cond2 = c4
;	    ---> (simplify-and (c1 c2 c3 c4))
(defun put-in-and (cond1 cond2)
; *** this checks on 'temp' just for debugging
   (let ((temp (my-put-in-and cond1 cond2)))
       (cond ((nilmember temp) (break "DUE")))
       temp))

(defun my-put-in-and (cond1 cond2)
; *** both cond1 and cond2 should be in disjunctive normal form
;     cond2 could include !overcomes conditions
 (let (overcome-cond)
  (multiple-value-setq (cond2 overcome-cond)
       (find-overcome-cond cond2))
; *** the next removes the overcome subformulae from the original condition
  (setq cond1 (remove-overcome-cond cond1 overcome-cond))
  (normalize (list 'and cond1 cond2))))

;(defun my-put-in-and-old (cond1 cond2)
; (let (overcome-cond tempres)
;  (multiple-value-setq (cond2 overcome-cond)
;       (find-overcome-cond cond2))
;; *** the next removes the overcome subformulae from the original condition
;  (setq cond1 (remove-overcome-cond cond1 overcome-cond))
;  (cond ((null cond1) nil) ; (break "in put-in-and")
;	((eq cond1 t) cond2)
;	((eq cond2 t) cond1)
;	((eq (first cond1) 'and)
;	  (cond ((eq (first cond2) 'and)
;		  (simplify-and (append (rest cond1) (rest cond2)) nil))
;	        ((eq (first cond2) 'or)
;                  (do ((nextdisj (second cond2) (first remcond))
;                       (remcond (rest (rest cond2)) (rest remcond)))
;                     ((null nextdisj)
;                         (cond ((null tempres) nil)
;                               ((eq 1 (length tempres)) (first tempres))
;                               (t (cons 'or tempres))))
;                     (cond ((eq (first nextdisj) 'and)
;                              (setq simpl (simplify-and (append (rest nextdisj) (rest cond1)) nil)))
;                           (t (setq simpl (simplify-and (append (list nextdisj) (rest cond1)) nil))))
;                     (cond ((not (null simpl)) (setq tempres (append1 tempres simpl))))))
;                (t (simplify-and (append1 (rest cond1) cond2) nil))))
;	((eq (first cond2) 'and)
;          (cond ((eq (first cond1) 'or)
;                  (do ((nextdisj (second cond1) (first remcond))
;                       (remcond (rest (rest cond1)) (rest remcond)))
;                     ((null nextdisj)
;                         (cond ((null tempres) nil)
;                               ((eq 1 (length tempres)) (first tempres))
;                               (t (cons 'or tempres))))
;                     (cond ((eq (first nextdisj) 'and)
;                              (setq simpl (simplify-and (append (rest nextdisj) (rest cond2)) nil)))
;                           (t (setq simpl (simplify-and (append (list nextdisj) (rest cond2)) nil))))
;                     (cond ((not (null simpl)) (setq tempres (append1 tempres simpl))))))
;	        (t (simplify-and (cons cond1 (rest cond2)) nil))))
;	(t (simplify-and (list cond1 cond2) nil)))))
;
;(defun my-put-in-and-old-2 (cond1 cond2)
; (let (overcome-cond)
;  (multiple-value-setq (cond2 overcome-cond)
;       (find-overcome-cond cond2))
;; *** the next removes the overcome subformulae from the original condition
;  (setq cond1 (remove-overcome-cond cond1 overcome-cond))
;  (cond ((null cond1) nil) ; (break "in put-in-and")
;	((eq cond1 t) cond2)
;	((eq cond2 t) cond1)
;	((eq (first cond1) 'and)
;	  (cond ((eq (first cond2) 'and)
;		  (simplify-and (append (rest cond1) (rest cond2)) nil))
;		(t (simplify-and (append1 (rest cond1) cond2) nil))))
;	((eq (first cond2) 'and)
;	  (simplify-and (cons cond1 (rest cond2)) nil))
;	(t (simplify-and (list cond1 cond2) nil)))))
 
;; *************************************************************************
(defun put-not (condit)
   (cond ((eq (first condit) 'and)
            (cons 'or (mapcar #'put-not (rest condit))))
         ((eq (first condit) 'or)
            (cons 'and (mapcar #'put-not (rest condit))))
         ((eq (first condit) 'not)
            (second condit))
         (t (list 'not condit))))

; *************************************************************************
; *** this eliminates redundant conditions and checks for contradictions
;     This is done in a very simple way: a contradiction is revealed just
;     in case the conditions c1 and (not c1) appear together in condlist
(defun simplify-and (condlist acc)
; *** recursion completed: returns the list of conditions with 'and' in front
  (cond ((null condlist) (cons 'and acc))
; *** a condition is repeated in the rest of the list; it can be omitted, since
;     its next occurrence will be retained
	((member (first condlist) (rest condlist) :test #'equal)
	   (simplify-and (rest condlist) acc))
; *** a negated condition (not c1) is the first element of the list, but the
;     condition c1 occurs in the rest: a contradiction
	((and (eq (first (first condlist)) 'not)
	      (member (second (first condlist)) (rest condlist) :test #'equal))
	   nil)
; *** a condition (c1) is the first element of the list, but its negation
;     occurs in the rest: a contradiction
	((member (first condlist) (rest condlist) 
		 :test #'(lambda (x y) 
			   (and (eq (first y) 'not) (equal x (second y)))))
	   nil)
; *** a negated and-ed condition (not (and c1 c2 ...)) is the first element of
;     the list, but the condition (not (and cx1 cx2 ...)), or (not cx1) occurs
;     either in acc or in the rest, with cx1, cx2, ... a subset of c1, c2, ...;
;     the current negated condition (being more specific) is omitted
	((eq (first (first condlist)) 'not)
	   (cond ((or (and (eq (first (second (first condlist))) 'and)
	      	           (listx-member (rest (second (first condlist)))
					 (append acc (rest condlist))))
	              (and (neq (first (second (first condlist))) 'and)
	      	           (listx-member (list (second (first condlist)))
					 (append acc (rest condlist)))))
	   	   (simplify-and (rest condlist) acc))
	         (t (simplify-and (rest condlist) 
				(append1 acc (first condlist))))))
; *** the standard case: accumulate the first condition and go on
	(t (simplify-and (rest condlist) (append1 acc (first condlist))))))

; *************************************************************************
; *** list1 is a list of and-ed conditions (c1 c2 ...) 
; *** list2 is a list of full conditions (op1 (op2 cx1 cx2 ...))
; *** if op1 is 'not' and 'op2' is 'and', then list-member returns true if the
;     list (cx1 cx2 ...) is a subset of (c1 c2 ...)
(defun listx-member (list1 list2)
   (cond ((null list2) nil)
	 ((and (eq (first (first list2)) 'not)
	       (or (and (eq (first (second (first list2))) 'and) 
	                (subset (rest (second (first list2))) list1))
	           (and (neq (first (second (first list2))) 'and)
	                (member (second (first list2)) list1 :test #'equal))))
	   t)
	 (t (listx-member list1 (rest list2)))))

; ****************************************************************************
; *** remov-case-sp removes some cases having given labels ("canc-keys"). It advances
;     in parallel on the two lists (keys and elems) but returns just 
;     the elements from which the element corresponding to the position of "key"
;     has been substituted with "nil"
(defun remov-case-sp (canc-keys keys elems class-name)
  (cond ((null canc-keys) elems)
        (t (remov-case-sp (rest canc-keys) keys 
                  (int-rem-case-sp (first canc-keys) keys elems class-name)
                  class-name))))

; ****************************************************************************
(defun int-rem-case-sp (sing-canc-key keys elems class-name)
  (cond ((null keys) 
          (cond ((not (is-inthn-dummy? class-name))
	          (format t 
                     "***** WARNING: no case to cancel in transf-hier:remov-case-sp: ~a ~%"
                     sing-canc-key)
                ;  (break "")
                 ))
	   nil)
; *** member-compatib-lab (defined in ALLLANG/PROC_ALL/SUBCAT-PROC-ALL/subc-hier) 
;     checks if one of the labels subsumes the other in the hierarchy. This is
;     used to obtain that, for instance, cancelling the subject, automatically 
;     produces cancelling the finverbcompl-subj
; *** the next applies in case the corresponding case has been already
;     cancelled by another transformation
        ((is-a-cancelled-case (first keys))
           (cons (first elems)
                 (int-rem-case-sp sing-canc-key (rest keys) (rest elems) class-name)))
; *** rem-surf-marker cancels the 's-' prefix, since the hierarchy is defined
;     over the deep labels
  	((member-compatib-lab (rem-surf-marker (first keys)) (inlist sing-canc-key))
	   (cons nil (rest elems)))
	(t (cons (first elems) 
		 (int-rem-case-sp sing-canc-key (rest keys) (rest elems) class-name)))))

; ****************************************************************************
; *** remov-case-lab just removes some elements from a list; there should be a 
;     LISP system function for this
; *** canc-labs: the labels to remove
; *** labels: the original labels
(defun remov-case-lab (canc-labs labels)
  (cond ((null canc-labs) labels)
        (t (remov-case-lab (rest canc-labs)
                           (int-rem-case-labs (first canc-labs) labels)))))

; ****************************************************************************
(defun int-rem-case-labs (sing-lab labels)
  (cond ((null labels) nil)
        ((eq (ult (expl+cats (explode (first labels)) #\-)) nil)
  ; *** this case was already been removed
           (cons (first labels)
                 (int-rem-case-labs sing-lab (rest labels))))
; *** for member-compatib-lab, see the comments in the function above
  	((member-compatib-lab 
		(rem-surf-marker (first labels)) (inlist sing-lab))
  ; *** the case label is removed by adding -nil to its name;
  ;     ex: s-subj --> s-subj-nil
           (cons (concatl (list (first labels) '-nil))
  	         (rest labels)))
	(t (cons (first labels) 
                 (int-rem-case-labs sing-lab (rest labels))))))

; ****************************************************************************
; *** see the comments in "apply-transform" for the effects and parameters of
;     this function, and "remov-case" for the implementation details
;     oldlab is used as a key for searching the pattern to change
(defun update-case (oldlab modif s-labels patterns)
  (cond ((null s-labels)
	   (format t "WARNING: no case to update in transf-hier: update-case ~%")
	   nil)
  	((eq oldlab (first s-labels))
	   (cons (act-upd-case modif (first patterns)) (rest patterns)))
	(t (cons (first patterns) 
		 (update-case oldlab modif (rest s-labels) (rest patterns))))))

(defun act-upd-case (modif superf-descr)
; *** the modification to carry out is as follows:
;     old description (superf-descr): (cond1 cond2 ... condn)
;     new description: ((prep (word-typ val) (down cond1'))
;			(prep (word-typ val) (down cond2'))
;				.......
;			(prep (word-typ val) (down condn')))
;     where "val" is the actual preposition
;	and condi' is "(cat condi)" if condi is an atom
;		"(cat (first condi)) (rest condi)" otherwise
;   EX: old description: (noun (pron (case lsubj)))
;	new description: ((prep (word-typ &agent-compl-prep)
;                               (down (cat noun) (agree)))
;			  (prep (word-typ &agent-compl-prep) 
;				(down (cat pron) (case lsubj) (agree))))
; *** of course, the things cannot be left this way, since the resulting
;     case of the pron cannot be "lsubj", and no agreement check must be
;     made. So, some update must be allowed also on the internal
;     specification of the pattern.
; *** modif could be:
;     ((cancel verb) (cancel noun)
;      (change pron (include prep &agent-compl-prep)
;                   (upd case obl) (erase agree)))
  (dolist (modspec modif superf-descr)
	  (case (first modspec)
		(cancel 
		  (setq superf-descr
				(cancel-spec superf-descr (second modspec))))
		(change 
		  (setq superf-descr
			  	(mapcar #'(lambda (x) (change-spec x 
							(second modspec)
							(rest (rest modspec))))
				  	superf-descr)))
                (add 
                  (setq superf-descr
                        (append (second modspec) superf-descr))))))

(defun cancel-spec (descr categ)
; *** descr is the superficial description of a single case
; *** categ is a category that cannot appear in the transformed description
  (cond ((null descr) nil)
        ((eq categ 'all-spec) nil)
	((test-spec-corresp (first descr) categ) (rest descr))
	(t (cons (first descr) (cancel-spec (rest descr) categ)))))

; *** categ is the condition imposed by the rule on the element to be 
;     transformed. If it is an atom, it must be a category name. So, all
;     elements of that category will be affected.
(defun test-spec-corresp (spec categ)
  (cond ((atom categ)
	  (cond ((atom spec) (eq spec categ))
		(t (eq categ (first spec)))))
; *** if categ is not an atom, it is something of the form 
; *** (realcateg constr1 ... constrn), and all the constri must be matched
;     in spec
	((atom spec) nil)
; *** both spec and categ are lists, the first elements are the categories
;     they have to be the same, before starting further checks
	((eq (first spec) (first categ))
; *** if all constraints are examined, then return t
	  (dolist (featconstr (rest categ) t)
		(cond ((not (member featconstr (rest spec) :test #'equal))
; *** if one of them fails, then return nil
			(return nil)))))
	(t nil)))
	
(defun change-spec (spec categ modtype)
; *** spec is a single line in the superficial description of a single case
;	(see the mapcar in act-upd-case)
; *** categ is a category whose constraints must be modified
; *** modtype is the modification to be made to the old description
  (cond ((test-spec-corresp spec categ)
	  (let ((include? (leggi modtype 'include))
		(spec (inlist spec)))
	       (cond (include?
			(list (first include?)
			    (list 'word-typ (second include?))
			    (cons 'down 
				 (cons (list 'cat (first spec))
				    (apply-gr-changes (rest spec) modtype)))))
	  	     (t (cons (first spec)
			      (apply-gr-changes (rest spec) modtype))))))
	(t spec)))

(defun apply-gr-changes (spec modifs)
  (cond ((null modifs) spec)
	(t (apply-gr-changes
		 (apply-sing-chang spec (first modifs)) (rest modifs)))))

(defun apply-sing-chang (spec mod)
   (case (first mod)
	(upd (upd-s-spec spec (second mod) (third mod)))
	(add (cons (second mod) spec))
	(erase (rem-s-spec spec (second mod)))
	(include spec)))

(defun upd-s-spec (spec feature newval)
  (cond ((null spec) nil)
	((eq (first (first spec)) feature)
	  (cons (list feature newval) (rest spec)))
	(t (cons (first spec) (upd-s-spec (rest spec) feature newval)))))

(defun rem-s-spec (spec feature)
  (cond ((null spec) nil)
	((eq (first (first spec)) feature)
	  (rest spec))
	(t (cons (first spec) (rem-s-spec (rest spec) feature)))))
  
; $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
; 		EXTENSION OF THE HIERARCHY VIA TRANSFORMATIONS
; $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

;**************************************************************
; *** In this file, functions for the construction of the hierarchy
;     including the transformed patterns.
; *** They take as input the definition of the transformations and produce a
;     new hierarchy, which describes all possible surface patterns; the basic
;     hierarchy is accessed through the pointers from the transformations
(defun transform-hierarchy ()
  (declare (special *VERB-CF-TRANSFORM*))
; *** apply all transformations in sequence
   (dolist (transf *VERB-CF-TRANSFORM*)
; *** apply them to all classes to which they must be applied
      (let ((transf-name (get-transf-name transf))
	    (classes (get-transf-classes transf))
	    ok-transform transformed-nodes)
	 (dolist (nxt-class classes)
; *** the transformation is applied to the class. Inside apply-tr-to-node,
;     its application is propagated to all derived classes
	       (setq ok-transform (apply-tr-to-node nxt-class 
                                                   nil transf transf-name nil))
; *** the subtree of the original class is examined. It is traversed
;     breadth-first (by levels). The 'do' goes down, a level per cycle.
;     It stops when it finds an empty level
; *** nxt-level is a list of pairs <node, parent-node>
               (setq transformed-nodes (cons nxt-class transformed-nodes))
               (do* ((nxt-level 
		      (select-basic-classes
		        (not-same-or-incompat-transf
		          (mapcar #'(lambda (x) (list x nxt-class))
			          (get-inthn-deps nxt-class))
		          transf-name))
; *** select-basic-classes excludes the classes obtained via transformations
;     they have to be reached in a different way (i.e. via a kind of
;     'horizontal' movement)
; *** subtr-by-key handles nodes already reached before (stored in 
;     transformed-nodes)
; *** elimdup-by-key handles nodes reached twice at the same level
; *** not-same-or-incompat-transf avoids applying twice the same transformation
;     and to apply incompatible transformations
		      (select-basic-classes
		        (not-same-or-incompat-transf
		          (elimdup-by-key
		  	    (subtr-by-key
		     	      (flatten 
			        (mapcar
				   #'(lambda (x) 
				         (mapcar #'(lambda (y) 
						    (list y (first x)))
					    (get-inthn-deps (first x))))
			   	   nxt-level))
			      transformed-nodes))
		          transf-name))))
	   	   ((null nxt-level) nil)
; *** the dolist loops on all the classes at the same level
	     	   (setq transformed-nodes 
		 	(append transformed-nodes 
					(mapcar #'first nxt-level)))
	   	   (format t "$$$$$$$$$$$ next level $$$$$$$$$$$$~%")
	       	   (dolist (nxt-pair nxt-level)
	      		(let* ((nxt-dep (first nxt-pair))
		     	       (nxt-par (second nxt-pair))
; *** transf-par is the name of the transformed parent
			       (transf-par 
				  (second 
				     (assoc transf-name
					  (get-inthn-tr-sons nxt-par)))))
; *** if the top class underwent the transformation, the same is assumed to
;     happen for its dependents
	       		  (cond (ok-transform
; *** the fourth argument of apply-tr-to-node is a list including as its
;     only pair the name of the original (non trasformed) parent and the name
;     of the transformed parent
; *** if transf-par is NIL, then the parent could not be transformed
;     presumably, the same should happen to the daughter, but I let the
;     the transformation to be applied; in case of incompatibility, 
;     'apply-tr-to-node' should do nothing
				  (apply-tr-to-node 
					nxt-dep nxt-par transf transf-name
					(list (list nxt-par transf-par))))
; *** if the basic class is not transformable, try
;     to apply the transformation to the classes derived from nxt-dep
;     (the same is done on the transformable classes within apply-tr-to-node)
	       		        (t (apply-tr-to-derived-classes 
						nxt-dep nxt-par transf)))
)))))))

;**************************************************************
; *** it inserts a new (transformed) class in the hierarchy
; *** INPUT:
;   --->  nxt-class is the original class
;   --->  transf is the definition of the transformation
;   --->  transf-class-def is the result of the transformation
(defun insert-tr-class-in-hier (nxt-class transf transf-class-def)
  (let ((transf-name (get-transf-name transf)))
       (cond ((eq (get-transf-type transf) 'replacing)
; *** if the transformation is 'replacing' then the applicability condition of
;     associated with the original class must be updated.
   	       (put-inthn-patterns nxt-class
 	    		(change-appl-cond (get-inthn-patterns nxt-class)
		        transf-name 
		        (get-transf-cond transf)))))
; *** update the hierarchy by inserting (by means of a suitable classification
;     algorithm) the new (transformed) class into the previous hierarchy
;     'verbs is the root of the hierarchy
   ;  (cond ((and (eq transf-name 'object-and-ind-object-raising)
   ;              (eq nxt-class 'btrans-indobj+null-subj)) (break "here")))
       (ins-classify 'verbs transf-class-def transf-name nxt-class)
; *** link the original class to the transformed class
       (put-inthn-tr-source (first transf-class-def)
				    (list transf-name nxt-class))
       (put-inthn-tr-sons nxt-class 
				  (list transf-name (first transf-class-def)))
       (format t "Classification of ~a after ~a done!!!~%" nxt-class (first transf))))

; ****************************************************************************
; *** this applies transformations to derived classes. It simply moves across
;     the (horizontal) tree of derived classes and calls 'apply-tr-to-node'
;     to do the work on the particular node
(defun apply-tr-to-derived-classes (nxt-dep nxt-par transf)
   (let ((transf-name (get-transf-name transf)) derived)
; *** derived is a set of transformed sons obtained via transformations other
;     than the one we are currently working upon. It is represented as a list
;     of pairs <transf-name derived-node>
;	 (do ((derived 
;		(subtr-by-key (get-inthn-tr-sons nxt-dep) (list transf-name))
;		(subtr-by-key 
;		   (flatten (mapcar #'get-inthn-tr-sons
;				    (mapcar #'second derived)))
;		   (list transf-name))))
;	     ((null derived) nil)
 	     (setq derived
		(subtr-by-key (get-inthn-tr-sons nxt-dep) (list transf-name)))
	     (dolist (deriv-cl derived)
; *** deriv-par is the class derived from nxt-par via the transformation
;     under consideration (first deriv-cl)
; *** if the derived class was obtained via a transformation incompatible
;     with the current one, nothing is done!
		(cond ((check-tr-compatibility transf-name (second deriv-cl))
		  (cond ((null nxt-par)
; *** nxt-par is NIL if the transformation is being applied to the root of
;     the subtree (explicit class appearing in the definition of the transf)
			  (apply-tr-to-node (second deriv-cl) 
                                               nil transf transf-name nil))
	          	(t (let* ((deriv-par
	   	      	           (concatl 
				      (put-separator '+ 
					 (list nxt-par (first deriv-cl)))))
			 	  (tr-deriv-par 
			    	   (second 
				      (assoc transf-name
				         (get-inthn-tr-sons deriv-par)))))
; *** if a parent does exist, but its transformed counterpart does not exist,
;     we are in a situation where the current transformation is applied to a
;     node higher in the hierarchy than the node to which the transformation
;     obtaining the derived node was applied. For example, when we apply
;     INFINITIVIZATION to basic-trans, basic-trans was reached from subj-verbs;
;     but basic-trans+passivization does exist, while subj-verbs+passivization
;     does not (because basic-trans is the root for the application of
;     PASSIVIZATION). In these cases, the application to the derived class
;     must be carried about as if it is the root (parent=NIL)
				 (cond ((and (not (is-inthn-dummy?
							 (second deriv-cl)))
			            	     (not (null deriv-par)))
				 	 (cond ((null tr-deriv-par)
				  		  (apply-tr-to-node 
				    	   	     (second deriv-cl)
						     nil transf transf-name nil))
				       	       (t (apply-tr-to-node 
				    	   	     (second deriv-cl)
				    	   	     deriv-par
				    	   	     transf transf-name
				    	   	     (list 
					     	       (list
					        	 deriv-par
							 tr-deriv-par))))
				      )))))))))
;)
))

; *** INPUT
;  ---> node: the name of a class (a node in the hierarchy)
;             for the representation of a subcat class as a set of
;             properties, see ALLLAN/PROC-ALL/PARSER-PROC-ALL/hier-funct
;  ---> parent: the parent of the node from which the node was reached
;               (travelling the tree downward)
;  ---> transf: the transformation which is currently being applied
;  ---> newp: a list of pairs; each pair is <orig-parent transf-parent>,
;	      where orig-parent is one of the parents of 'node', and 
;             transf-parent is the corresponding parent of the transformed
;             dependent. In input, it arrives initialized with the pair
;             <parent transform-of-parent>
(defun apply-tr-to-node (node parent transf transf-name newp)
  (let ((newparents newp)
	trparpatt new-class-name newdef affected)
; *** the cond used to avoid duplicating transformation for nodes already
;     transformed (the hierarchy is a graph)
 ; (cond ((eq transf-name 'null-indobj)
 ;          (format t "Apply-tr-to-node: ~a~%" node)
 ;          (break "")))
   (cond ((not (member transf-name 
			(mapcar #'first (get-inthn-tr-sons node))))
; *** if parent is NIL, then we are working on the root of a transformation
;     subtree (i.e. one of the classes explicitly associated with the
;     transformation)
; *** the transformation is applied to the full definition (obtained via
;     inheritance) of the class; 'apply-transform' defined in gr-interp
   ;  (cond ((eq transf-name 'object-and-ind-object-raising) (break "here1")))
     (cond ((null parent)
	    (let* ((full-class-def (get-inthn-patterns node))
		   (transf-class-def (apply-transform full-class-def transf)))
; *** the derived class is inserted in the hierarchy just in case its
;     transformed definition is not NIL
	       (cond ((not (null transf-class-def))
			(insert-tr-class-in-hier 
				node transf transf-class-def)))
; *** then apply the transformation to all classes derived from the current one
;     Notice that this must be done also when transf-class-def is NIL. In
;     fact, it can be nil because there is no compatibility on the
;     conditions of application. For example, BASIC-TRANS includes the
;     condition 'not passive' after application of 'passivization', so that
;     'da-infinitivization' returns for it NIL. But it has to be applied to
;     'basic-trans+infinitivization'.
; *** in this call, the second argument of 'apply-tr-to-derived-classes' is
;     NIL, because this is the root of the subtree to which the transformation
;     applies. This contrast with the case when the derived classes are
;     inspected during the navigation in the subtree, where a specific parent
;     produced the access to the class. In the current case (NIL), the
;     transformed class must be simply classified under 'verbs', while in
;     the others, the parent has a privileged status, since to it the
;     transformation at hand has already been applied.
	       (apply-tr-to-derived-classes node nil transf)
; *** the return value is t if the transformation could be applied, nil
;     otherwise
	       (cond ((not (null transf-class-def)) t)
		     (t nil))))
; *** parent is not NIL; so, we are descending in a subtree
       (t (let ((new-cond 
		  (put-in-and (normalize (get-subc-applc (get-inthn-patterns node)))
			      (normalize (get-transf-cond transf)))))
; *** if the resulting condition (new-cond) is not contradictory, do the work
;     on the node
    (cond ((not (null new-cond))
	  (setq new-class-name 
		(concatl (put-separator '+ (list node transf-name))))
	  (put-inthn-tr-sons node (list transf-name new-class-name))
	  (put-inthn-tr-source new-class-name (list transf-name node))
; *** affected is a pair whose first element are the affected cases defined
;     locally, and whose second element are the parents which include some
;     affected cases. What must be done now is to build a new subcat-pattern
;     including all those cases, transform it, detach the parent, and insert
;     the result into the hierarchy
	  (setq affected (affected-by-transf node transf parent))
  (cond ((equal affected '(NIL NIL))
	  (format t " ~a. Transf: ~a (not affected)~%" node transf-name))
	(t (format t " ~a. Transf: ~a (affected)~%" node transf-name)))
; *** newdef is initialized 
	  (setq newdef (get-inthn-loc-cf-def node))
; *** and, if it is affected, is transformed
	  (cond ((not (null (first affected)))
		  (let ((pattern (get-inthn-patterns node)))
		       (setq newdef
			    (get-subc-cases
			     	       (apply-transform 
					  (list (get-subc-name pattern)
						(get-subc-applc pattern)
						(get-subc-prefs pattern)
						(get-subc-transfs pattern)
						newdef)
			 	 	  transf))))))
; *** the loc-cf-def of the new class is inserted in the node
	  (cond ((equal (first newdef) '(NIL))
		   (put-inthn-loc-cf-def new-class-name '(NIL NIL NIL)))
; *** COSI' NON VA! PRIMA BISOGNA FARE MERGE-CF e POI CF-DIFFERENCE!!!
;     IL CASO È QUELLO DI IMPERSONALIZATION, IN CUI È AFFECTED 'OBJECT',
;     A CUI BISOGNA AGGIUNGERE 'AGREE' ('si sono mangiate tante portate',
;     e non 'si è mangiato tante portate', 'li si sono visti correre' e non
;     'li si è visto correre'; ma perchè 'li si vede correre'? Nota anche
;     'si è mangiato bene')
;		(t (put-inthn-loc-cf-def new-class-name newdef))
		(t (let ((up-cf (get-subc-cases 
				    (get-inthn-patterns 
					(second (first newparents))))))
		      (put-inthn-loc-cf-def 
			new-class-name 
			(cf-difference 
			    up-cf (merge-cf newdef up-cf) transf-name)))))
; *** then, newdef is extended with the cases inherited from the affected
;     parents (if any). merge-cf defined in subc-hier.lisp
	  (dolist (nxt-parent 
		      (subtrl (mapcar #'get-syn-repr (get-inthn-parents node))
			      (list parent)))
		 (cond ((member nxt-parent 
				(mapcar #'get-syn-repr (second affected)))
; *** the affected parent is inspected to see if it has already a transformed
;     offspring, which could be used as the new parent
			 (let ((transf-node 
				  (second 
				      (assoc transf-name
					 (get-inthn-tr-sons nxt-parent)))))
			    (cond ((null transf-node)
; *** if the offspring does not exist, then it is created; 
; *** first, the transformed pattern is obtained
			 	    (setq trparpatt
				      (apply-transform 
			 	        (get-inthn-patterns nxt-parent) transf))
; *** if the parent can be transformed via transf-name, then do it,
;     otherwise create a dummy transformed node. For example, since subj-verbs
;     cannot be passivized, instead of producing 'subj-verbs+passivization',
;     it is created 'dummy-subj-verbs+passivization'
				    (cond ((not (null trparpatt))
				     (cond ((transformable 
					     (get-syn-repr nxt-parent) transf)
					    (setq transf-node
				              (concatl 
			   	                (put-separator '+ 
						  (list 
						    (get-syn-repr nxt-parent)
						    transf-name)))))
					  (t (setq transf-node
				              (concatl 
			   	                (put-separator '+ 
						  (list 
						    'dummy
						    (get-syn-repr nxt-parent)
						    transf-name))))
					     (put-inthn-dummy transf-node)
					     (setq trparpatt
						(cons transf-node
						     (rest trparpatt)))))
; *** then, the new node is created (without parents)
				    (create-inthn 
					transf-node		; *** node name
				        nil			; *** synonyms
				        nil			; *** parents
				        (get-surf-def 
						(get-subc-cases trparpatt))
				        (get-surf-lab 
						(get-subc-cases trparpatt))
				        (get-deep-lab 
						(get-subc-cases trparpatt)))
; *** it is doubly linked to the source of tranfomation
		     		    (put-inthn-tr-sons nxt-parent
					(list transf-name transf-node))
		     		    (put-inthn-tr-source transf-node
					(list transf-name nxt-parent))
; *** the applicability conditions of the source parent are changed
  		     		    (cond ((eq (get-transf-type transf)
						 'replacing)
   			      		    (put-inthn-patterns nxt-parent
 	    		  	  	       (change-appl-cond 
				          	 (get-inthn-patterns nxt-parent)
					  	 transf-name
					  	 (get-transf-cond transf)))))
; *** and finally it is inserted into the hierarchy (classified)
	       			    (ins-classify 'verbs trparpatt 
					transf-name transf-node)))))
; *** now, transf-node (already existing or created anew) can be used as
;     the parent of the node we were working upon
			   (cond ((not (null transf-node))
				   (setq newparents 
				      (append1 newparents 
					(list nxt-parent transf-node)))
			   	   (setq newdef 
			            (merge-cf newdef 
				      (get-subc-cases 
					(get-inthn-patterns transf-node)))))
				 (t (break "Parent affected, but not transformable")))))
; *** if the next parent was not affected by the transformation, simply
;     include its def in newdef
		           (t (setq newparents 
				    (append1 newparents 
					(list nxt-parent nxt-parent)))
			     (setq newdef 
			        (merge-cf newdef 
				     (get-subc-cases 
					  (get-inthn-patterns nxt-parent)))))))
; *** now we have in newdef the full definition of the new node and in
;     newparents the list of all its parents. nxt-dep must be inserted
;     under all parents, and its local definition must be evaluated
; *** if the transformation is 'replacing', then the original class' condition
;     must be changed
		     (let ((npatt (get-inthn-patterns node)))
  		     (cond ((eq (get-transf-type transf) 'replacing)
   			      (put-inthn-patterns node
 	    		  	  (change-appl-cond 
				          npatt
					  transf-name
					  (get-transf-cond transf)))))
; *** connect all the parents (original or transformed)
		     (dolist (newpar newparents)
			     (add-parent 
				  node 
				  new-class-name 
				  (first newpar)
				  (second newpar)
				  transf-name
				  nil))
       		     (put-inthn-cf-def new-class-name 
				       (inherit-cf new-class-name))
  		     (cond ((eq (get-transf-type transf) 'replacing)
       		      	      (put-inthn-patterns new-class-name
             		      	   (list new-class-name
		   		      (put-in-and (normalize (get-subc-applc npatt))
					  	  (normalize (get-transf-cond transf)))
		   		      (get-subc-prefs npatt)
 		   		      (append1 (get-subc-transfs npatt)
						 transf-name)
                   		      (get-inthn-cf-def new-class-name))))
       		      	   (t (put-inthn-patterns new-class-name
             		      	   (list new-class-name
		   		      (get-subc-applc npatt)
		   		      (get-subc-prefs npatt)
 		   		      (append1 (get-subc-transfs npatt)
 		   		      		transf-name)
                   		      (get-inthn-cf-def new-class-name)))))))))
	  (apply-tr-to-derived-classes node parent transf)
; *** for nodes inside a subtree, the function always returns t
	  t)))
; *** if the transformation has already been applied to the class, it could
;     still need be applied to a derived class
      (t (apply-tr-to-derived-classes node parent transf))
		)))

; *** from a list of classes (hierarchy nodes) extracts the ones which are
;     basic (i.e. not obtained via a transformation)
(defun select-basic-classes (class-list)
   (cond ((null class-list) nil)
	 ((= 1 (length (get-subc-transfs 
			   (get-inthn-patterns (first (first class-list))))))
	   (cons (first class-list) (select-basic-classes (rest class-list))))
	 (t (select-basic-classes (rest class-list)))))

; *** checks if class is transformable via the transformation tr.
;     it simply verifies if any of the classes defined for the transformation
;     subsumes the class to be checked
(defun transformable (class tr)
  (let ((basic-class (first (get-subc-transfs (get-inthn-patterns class))))
	(roots (get-transf-classes tr))
	(ok nil))
	(do ((nxtroot (first roots) (first roots))
	     (roots (rest roots) (rest roots)))
	    ((or ok (null nxtroot)) ok)
	    (cond ((trh-subsumes nxtroot basic-class)
		     (setq ok t))))))

; *** true if 'up' is an ancestor of 'down' in the hierarchy.
;     it goes depth-first without any attempt to avoid repetitions.
;     I'm not sure about its efficiency
(defun trh-subsumes (up down)
   (let ((parents (get-inthn-parents down))
	 (found nil))
	  (cond ((null parents) nil)
		((member up parents) t)
		(t (do ((nxtpar (first parents) (first parents))
			(parents (rest parents) (rest parents)))
		       ((or found (null nxtpar)) found)
		       (cond ((trh-subsumes up nxtpar)
				(setq found t))))))))

; *** removes from the list of pairs class-parent-pairs all the pairs whose
;     first component (the class to be transformed) already underwent the
;     transformation transf-name, as well as all pairs whose first component
;     underwent a tranformation incompatible with transf-name
(defun not-same-or-incompat-transf (class-parent-pairs transf-name)
   (declare (special *INCOMPATIBLE-TRANSF*))
   (cond ((null class-parent-pairs) nil)
	 ((intersection
	      (cons transf-name
		      (first (leggi *INCOMPATIBLE-TRANSF* transf-name)))
	      (get-subc-transfs 
		 (get-inthn-patterns (get-hn-name (first class-parent-pairs)))))
	    (not-same-or-incompat-transf (rest class-parent-pairs) transf-name))
	 (t (cons (first class-parent-pairs)
	    	  (not-same-or-incompat-transf 
				(rest class-parent-pairs) transf-name)))))

(defun extract-from-l (keys keylist elemlist)
   (mapcar #'(lambda (x) (intextrfl x keylist elemlist)) keys))

; *** given a list of search keys and a list of elements, it finds the
;     element in the same position as 'key' in the list of keys
(defun intextrfl (key keyli elemli)
   (cond ((null keyli) (break "extract-from-l: transf-hier"))
	 ((eq key (first keyli)) (first elemli))
	 (t (intextrfl key (rest keyli) (rest elemli)))))

; **************************************************************
;    This and the following functions assume that some information has already
;    been computed in the initial loading of the classes (in particular,
;    the full cf, with all inherited infos, are already available in the
;    property 'cf-def of the class-name).
; *** see subc-hier.lisp for further details

; **************************************************************
(defun ins-classify (node transf-class-def transf-name orig-class)
; *** INPUT
;   ---> node is a node existing in the hierarchy
;   ---> transf-class-def is a *SUBCAT-PATTERN*
;   ---> transf-name is a transformation name
;   ---> orig-class is the name of the class to which the transformation was
;        applied. It is NIL, if the transformed class is inserted as a
;        daughter of the class actually mentioned in the transformation
; *** the classification is centered on the 'cases'. Starting from the root,
;     the procedure checks if any of the cases of the root's daughters
;     correspond to one of the cases of the transformed pattern. If this
;     happens, then the process is iterated on the found daughter; otherwise,
;     the transformed pattern is simply inserted as a new daughter below the
;     current node.
;	For instance a passivized cf ought to be inserted below 'subj-verbs',
;     since there is a match between the subject of subj-verbs and the former
;     object (now subject) of the transformed cf.
; *** Note that the match on the case labels has to be checked on the surface
;     case labels and not on the deep ones.
; *** The match is carried out by inspecting if, for each case of the node in
;     the hierarchy, there is a corresponding case in the transformed cf. As a
;     special case, the match always succeeds if the cf in the hierarchy is
;     empty.
  (let ((daughters (get-inthn-deps node)) bestmatch match-res)
      (do ((nextdaught (first daughters) (first daughters))
	   (daughters (rest daughters) (rest daughters)))
; *** if no more daughters end of loop; if no match found, insert under the
;     current node, otherwise proceed downward starting from the best match
	  ((null nextdaught) 
	    (cond ((null bestmatch)
		    (insert-in-hier 
				node transf-class-def transf-name orig-class))
		  (t (ins-classify 
			(second bestmatch)
			transf-class-def
			transf-name
			orig-class))))
; *** is there a match between the current node and the new pattern?
;     match-res is either nil (no match) or a pair <numcas, numsurf>, where
;     numcas is the number of matched cases and numsurf is the number of
;     surface definitions matched. Recall that the new definition must include
;     the full node definition (i.e. must be subsumed by it), so that the result
;     just says how complex the node definition is; in other words, the new
;     definition is always inserted below the most complex matched node.
	  (setq match-res 
		(match-class-def
		     (get-inthn-patterns nextdaught)
		      transf-class-def))
;	(format t "INS-CLASSIFY. Transf: ~s~%     Nextdaught: ~s~%     class: ~s~%     match-res: [~s.~s]~%"
;		  transf-name nextdaught (first transf-class-def)
;                 (car match-res) (cdr match-res))
;	(break "")
; *** if there is a match, and it is better than the previous one, then use it
	  (cond ((and (not (equal match-res '(0 . 0)))
   ; *** if match-res is 0.0, then no case and no surface pattern has been
   ;     matched. This is ok, but it is the worst case, so bestmatch cannot
   ;     be updated. This may happen when matching with 'verbs', which have
   ;     an empty cf (everything is subsumed by 'verbs')
   ; *** not cosidering this kind of matches produces a more flat hierarchy: e.g.
   ;     all classes having a null cf are sisters instead of daughters
		      (is-best-tr match-res bestmatch))
		   (setq bestmatch (list match-res nextdaught)))))))

(defun match-class-def (hier-pattern new-pattern)
; *** for each case in the pattern of the hierarchy, see if it is 'covered' by
;     the new pattern. 'covered' means that the surface label is the same and
;     that each surface realization occurs also in the new pattern. If all
;     cases are covered, then the new class is more specific than the existing
;     (hier) one. As a special case, an empty hier-pattern satisfies the match.
  (let* ((hier-cf-defin (get-subc-cases hier-pattern))
	 (hier-surf-def (get-surf-def hier-cf-defin))
	 (hier-surf-lab (get-surf-lab hier-cf-defin))
	 (hier-deep-lab (get-deep-lab hier-cf-defin))
         (new-cf-defin (get-subc-cases new-pattern))
	 (new-surf-def (get-surf-def new-cf-defin))
	 (new-surf-lab (get-surf-lab new-cf-defin))
	 (new-deep-lab (get-deep-lab new-cf-defin))
	 (ok-match t)
	 (numcas 0)
	 (numsurf 0))
; *** loop on the cases of the hierarchy node.
;     N.B. The work is carried out on the full cf-def, not on the local cf-def
      (do ((next-h-s-def (first hier-surf-def) (first hier-surf-def))
           (hier-surf-def (rest hier-surf-def) (rest hier-surf-def))
	   (next-h-s-lab (first hier-surf-lab) (first hier-surf-lab))
	   (hier-surf-lab (rest hier-surf-lab) (rest hier-surf-lab))
	   (next-h-d-lab (first hier-deep-lab) (first hier-deep-lab))
	   (hier-deep-lab (rest hier-deep-lab) (rest hier-deep-lab)))
; *** exit the loop when all hierarchy cases have been matched, or when a 
;     mismatch has been found
	  ((or (null next-h-s-lab) (not ok-match))
	    (cond (ok-match (cons numcas numsurf))
		  (t nil)))
; *** find a match on the next case ******
; *** first on the case labels
;     member-corresp verifies if next-h-s-lab is a member of new-surf-lab and, in
;     the same position there is in new-deep-lab the element next-h-d-lab
;     This is required to avoid, for instance, that passivized TRANS is inserted
;     below INTRANS; in fact, although at the surface level this is acceptable, 
;     the deep cases of the two S-SUBJ do not correspond (one is SUBJ, the other
;     is OBJ) and inheritance cannot be applied
	  (cond ((member-corresp next-h-s-lab new-surf-lab next-h-d-lab new-deep-lab )
; *** then on the surface case definitions
		  (let ((nxtnumsurf (match-surf-def
				    next-h-s-lab new-surf-lab
				    next-h-s-def new-surf-def)))
		      (cond ((null nxtnumsurf)
			      (setq ok-match nil))
			    (t (setq numsurf (+ numsurf nxtnumsurf))
		  	       (setq numcas (1+ numcas))))))
		(t (setq ok-match nil))))))

; *** given two elements (el1 and el2), and two parallel lists (list1 and list2),
;     the function checks if el1 is member of list1, and in the same position in 
;     list2 there is the element el2.
;     Ex.: (member-corresp 's-subj '(s-obj s-subj) 'subj '(obj subj)) --> T
;          (member-corresp 's-subj '(s-agtcompl s-subj) 'subj '(subj obj)) --> NIL
;          (member-corresp 's-subj '(s-obj s-indobj) 'subj '(obj indobj)) --> NIL
(defun member-corresp (el1 list1 el2 list2)
   (cond ((null list1) nil)
         ((eq el1 (first list1))
            (cond ((eq el2 (first list2)) t)
                  (t (member-corresp el1 (rest list1) el2 (rest list2)))))
         (t (member-corresp el1 (rest list1) el2 (rest list2)))))

; *** this is just to find the right 'new surface definition', by using the
;     hierarchy surface case label as a search key
(defun match-surf-def (h-s-lab n-s-lab h-s-def n-s-def)
  (cond ((eq h-s-lab (first n-s-lab))
	   (act-match-surf-def (first n-s-def) h-s-def 0))
	(t (match-surf-def h-s-lab (rest n-s-lab) h-s-def (rest n-s-def)))))

; *** the match succeeds if each specification in h-def subsumes one of the
;     specifications in new-def
(defun act-match-surf-def (new-def h-def nsurf)
; *** INPUT
;   ---> new-def: a *SURFACE-REALIZATION* (the one produced by the transform)
;   ---> h-def: a *SURFACE-REALIZATION* (the one associated with the node
;		 existing in the hierarchy)
; *** OUTPUT
;   ---> nil (if no match) or nsurf (total number of matched realizations)
   (cond ((null h-def) nsurf)
	 ((satisf-surf-def new-def (first h-def))
	   (act-match-surf-def new-def (rest h-def) (1+ nsurf)))
	 (t nil)))

; *** checks specificity on a single surface definition
;     single-h-def must subsume one of the elements in new-def
(defun satisf-surf-def (new-def single-h-def)
; *** INPUT
;   ---> new-def: a *SURFACE-REALIZATION* (the one associated with the result
;		 of the transformation)
;   ---> single-h-def: a syntactic category 'syncat' or a list
;		 (syncat restr1 ... restrn)
; *** OUTPUT
;   ---> nil (if no match) or nsurf (number or previously matched realizations)
  (cond ((null new-def) nil)
; *** if single-h-def is an atom it is of the form 'adj'. If the first in
;     new-def is also an atom, then succeed if they are the same atom. 
	((atom single-h-def)
	   (cond ((atom (first new-def))
	   	   (cond ((eq single-h-def (first new-def)) t)
		 	 (t (satisf-surf-def (rest new-def) single-h-def))))
; *** if 'first new-def' is not an atom, succeed if single-h-def is eq to the
;     first of the first of new-def; new-def: ((adj (agree)) ...)
	   	 (t (cond ((eq single-h-def (first (first new-def))) t)
		 	  (t (satisf-surf-def (rest new-def) single-h-def))))))
; *** if single-h-def is not an atom, but the first of new-def is an atom, then
;     single-h-def cannot subsume it: continue.
	((atom (first new-def))
	   (satisf-surf-def (rest new-def) single-h-def))
; *** if neither of them is an atom, complex check
	(t (cond ((and (eq (first single-h-def) (first (first new-def)))
		       (subset (rest single-h-def) (rest (first new-def))))
		    t)
		 (t (satisf-surf-def (rest new-def) single-h-def))))))
; -----------------
; forse si può anche fare tutto semplicemente con
;  (subset (inlist single-h-def) (inlist (first new-def)))
; per ora lascio così !!!

(defun is-best-tr (new-res prev-best)
; *** INPUT
;   ---> new-res is simply a pair <numcases, numsurf> or NIL
;   ---> prev-best is a list whose first element is such a pair, and whose rest
;          is the previous best hierarchy node or NIL
  (cond ((null new-res) nil)
	((null prev-best) t)
	((> (car new-res) (car (first prev-best))) t)
        ((= (car new-res) (car (first prev-best)))
	  (> (cdr new-res) (cdr (first prev-best))))
	(t nil)))

; ******* sets the properties of a new node in the hierarchy and includes the
;   links to the parent; new-def is obtained via a transformation
(defun insert-in-hier (parent new-def transf-name tr-orig)
; *** INPUT
;   ---> parent is the parent node: it has the properties associated with any
;	    node in the hierarchy
;   ---> new-def is of type *SUBCAT-PATTERN* 
;	    (node structure and types defined in
;            ALLLANG/PROC-ALL/SUBCATS-PROC-ALL/subc-hier)
;   ---> transf-name is a transformation name
;   ---> tr-orig is the class to which the transformation was applied to get
;           new-def. It is NIL, if the transformed class is inserted as a
;        daughter of the class actually mentioned in the transformation
; *** the next two establish the links
  (put-inthn-deps parent (first new-def))
  (put-inthn-parents (first new-def) (list parent))
; *** the definitions are included
  (put-inthn-patterns (first new-def) new-def)
  (put-inthn-cf-def (first new-def) (fifth new-def))
  (put-inthn-loc-cf-def (first new-def) 
	   		(cf-difference 
				(get-subc-cases (get-inthn-patterns parent))
				(get-subc-cases new-def)
				transf-name))
; *** links the node from which the transformed one originates by means of
;     transformation, to the transformed node (e.g. BASIC-TRANS is linked to
;     BASIC-TRANS+PASSIVIZATION)
;  (cond ((not (null tr-orig))
;	  (put-inthn-tr-source (first new-def) (list transf-name tr-orig))
;  	  (put-inthn-tr-sons tr-orig (list transf-name (first new-def)))))
)

; *** this computes the difference between two case-frame definitions. It
;     performs the task opposite to inheritance, and it's used to obtain
;     the local cf of a daughter, given a parent
(defun cf-difference (up-cf down-cf transf-name)
   (let ((up-defs (get-surf-def up-cf))
	 (down-defs (get-surf-def down-cf))
	 (up-surfcases (get-surf-lab up-cf))
	 (down-surfcases (get-surf-lab down-cf))
	 (up-deepcases (get-deep-lab up-cf))
	 (down-deepcases (get-deep-lab down-cf))
	 difference found def-diff deepc-diff)
     (do ((downsc (first down-surfcases) (first down-surfcases))
	  (down-surfcases (rest down-surfcases) (rest down-surfcases))
          (downdc (first down-deepcases) (first down-deepcases))
	  (down-deepcases (rest down-deepcases) (rest down-deepcases))
	  (downdef (first down-defs) (first down-defs))
	  (down-defs (rest down-defs) (rest down-defs)))
	 ((null downsc) difference)
; *** for each case of the daughter, check if it appears in the parent. If not,
;     then its full definition must be included in the difference. Otherwise,
;     inspect the definition. Notice that the match is made on the surface
;     label of the case
	(setq found nil)
	(do ((upsc (first up-surfcases) (first up-surfcases))
	     (up-surfcases (rest up-surfcases) (rest up-surfcases))
	     (updc (first up-deepcases) (first up-deepcases))
	     (up-deepcases (rest up-deepcases) (rest up-deepcases))
	     (updef (first up-defs) (first up-defs))
	     (up-defs (rest up-defs) (rest up-defs)))
	    ((or found (null upsc))
	      (cond ((not found)
		      (setq difference 
			 (list (append1 (first difference) downdef)
			       (append1 (second difference) downsc)
			       (append1 (third difference) downdc))))))
   ; *** the second conjunct added to account for multiple s-nil labels,
   ;     occurring for different traces; if the surface label is s-nil, 
   ;     then the check is made on the deep labels
	    (cond ((eq upsc downsc)
                  ; (and (eq upsc downsc)
                  ;      (or (neq upsc 's-nil)
                  ;          (eq updc downdc)))
; *** same label: inspect the definition
		    (setq found t)
; *** elements of updef put in list to simplify the search inside find-def-diff
		    (setq def-diff
			(find-def-diff updef downdef))
		    (cond ((neq updc downdc)
			    (setq deepc-diff downdc)))
		    (cond ((or (not (null def-diff)) (not (null deepc-diff)))
		     	    (setq difference
			      (list 
				(append1 (first difference) def-diff)
			        (append1 (second difference) downsc)
			      	(append1 (third difference) downdc)))))))))))

; *** checks if any of the specifications appearing in the surface definition
;     of a case of the daughter is not present in the corresponding
;     specification of the parent. Returns all not present.
(defun find-def-diff (updef downdef)
   (cond ((null downdef) nil)
	 ((member (first downdef) updef :test #'equal)
	    (find-def-diff updef (rest downdef)))
	 (t (cons (first downdef) (find-def-diff updef (rest downdef))))))

; ******************************************************************************
; *** checks if a (dependent) class is affected by a transformation
; *** INPUT
;  ---> dep-class: the name of the dependent class
;  ---> transf: the full definition of the transformation
;  ---> up-class: the name of the parent class
; *** OUTPUT
;  ---> a pair <loc-cases, other-parent-affected>
;	if loc-cases is NIL, then the local definition is not affected.
;       if other-parent-affected is not-NIL, then there are cases inherited
;       by parents other than up-class, which are affected. In this 
;       situation, dep-class must be 'detached' from such other parents,
;       whose (transformed) definition must be copied in dep-class
(defun affected-by-transf (dep-class transf up-class)
; *** dep-surf-cases are the cases mentioned (locally) in the class. The class
;     is affected if any of its local cases is operated upon by the
;     transformation. The cases in the transformation are surface cases, so
;     that the (deep) cases appearing in the local class definition are
;     suitably modified
; *** aff-surf-cases are the cases on which the transformation works. They need
;     be extracted from the operations done by the transformations (nthcdr)
;     They are surface cases.
  (let ((dep-surf-cases (second (get-inthn-loc-cf-def dep-class)))
	(aff-surf-cases (get-aff-cases (nthcdr 5 transf)))
	affparents)
       (dolist (nextpar (get-inthn-parents dep-class))
; *** only the parents other than the one the class is being attached are
;     considered
	   (cond ((and (neq nextpar up-class)
; *** if the next parent alread underwent the transformation at hand, then
;     it is not considered
		       (not (member (get-transf-name transf)
				    (get-subc-transfs 
					(get-inthn-patterns nextpar))))
; *** and the next parent must include some affected cases
		       (mult-member-compatib-lab
			    (get-surf-lab (get-inthn-cf-def nextpar))
			    aff-surf-cases))
		    (setq affparents (cons nextpar affparents)))))
       (list (mult-member-compatib-lab dep-surf-cases aff-surf-cases) 
	     affparents)))

; **********************************************************************************
; *** finds out which (surface) cases are affected by a list of transformational
;     operations
(defun get-aff-cases (transf-opers)
  (cond ((null transf-opers) nil)
        ((memq (first (first transf-opers)) 
	       '(modif-case conditional-modif-case))
; *** (modif-case (old-surf-lab new-surf-lab) def-modifications)
; *** (conditional-modif-case (old-surf-lab new-surf-lab) def-modifications)
          (let ((mod-case (first (second (first transf-opers)))))
	  	 (cond ((atom mod-case)
			 (cons mod-case (get-aff-cases (rest transf-opers))))
		       (t (append mod-case 
				  (get-aff-cases (rest transf-opers)))))))
        ((eq (first (first transf-opers)) 'cancel-case)
; *** (cancel-case surf-lab)
	  (cons-or-append (second (first transf-opers))
			  (get-aff-cases (rest transf-opers))))
        ((eq (first (first transf-opers)) 'add-case)
; *** (add-case new-surf-def new-surf-lab new-deep-lab)
	  (get-aff-cases (rest transf-opers)))))

; *** returns the list of the labels in 'surfcases1' which are compatible
;     with any of the labels in 'surfcases2'
(defun mult-member-compatib-lab (surfcases1 surfcases2)
  (cond ((null surfcases1) nil)
; *** member-compatib-lab defined in ALLLANG/PROC-ALL/SUBCAT-PROC-ALL/subc-hier
;     its first parameter must be a surface label without the s- prefix
        ((is-a-cancelled-case (first surfcases1))
	;   [cond [[member-compatib-lab 
        ;            [remove-cancel-marker [rem-surf-marker [first surfcases1]]]
        ;           surfcases2]
	;        [cons [first surfcases1] 
	;	         [mult-member-compatib-lab [rest surfcases1] surfcases2]]]
	;         [t
            (mult-member-compatib-lab (rest surfcases1) surfcases2))
        ;  ))
	((member-compatib-lab (rem-surf-marker (first surfcases1)) surfcases2)
	  (cons (first surfcases1) 
		(mult-member-compatib-lab (rest surfcases1) surfcases2)))
	(t (mult-member-compatib-lab (rest surfcases1) surfcases2))))
	    
; **********************************************************************************
; *** this function is used to insert a dependent whose local definition is
;     not affected by a transformation. In this case, the definition of the
;     dependent has to be duplicated without changes (so far as the local
;     definition is concerned), and the transformed class name has to be 
;     substituted to the original class name among the parents
; *** INPUT
;  ---> orig-dep: the original dependent to which the transformation was applied
;		(e.g. TRANS-TOPIC)
;  ---> transf-dep: the transformed dependent (e.g. TRANS-TOPIC+PASSIVIZATION)
;  ---> orig-parent: the original parent (e.g. TRANS)
;  ---> transf-parent: the transformed parent (e.g. TRANS+PASSIVIZATION)
;  ---> transf-name: the transformation name (e.g. PASSIVIZATION)
;  ---> transf-loc-cf-def: the transformed local cf (possibly nil)
(defun add-parent (orig-dep transf-dep orig-parent transf-parent transf-name
			transf-loc-cf-def)
;   (let* ((transf-parents (get-repr-inthn-parents transf-dep))
;	  (new-parents 
;	     (subst transf-parent (get-syn-repr orig-parent)
;		   (cond ((null transf-parents)
;			    (get-repr-inthn-parents orig-dep))
;			 (t transf-parents)))))
       (put-inthn-deps transf-parent transf-dep)
       (put-inthn-parents transf-dep (list transf-parent))		 ;new-parents
; *** inserts the double link from the original to the transformed dependent
       (put-inthn-tr-source transf-dep (list transf-name orig-dep))
       (put-inthn-tr-sons orig-dep (list transf-name transf-dep)))
;)

;************ This actually executes the transformations ************

; *** this prints a hierarchy onto a file named 'newhier.dat', for manual
;     check, and onto a file named 'newhier.out', for subsequent reading
;     by other LISP programs
;     the printing is made by levels, as always; there is the no-repetition
;     check, for obvious reasons
(defun print-hierarchy ()
  (with-open-file (outport (build-file-name "ALLLANG/KB-ALL/SUBCAT-KB-ALL/newhier.out")
				:direction :output
                                :if-exists :overwrite
                                :if-does-not-exist :create)
; *** examine the subtree of the original class. It is traversed breadth-
;     first (by levels). The 'do' goes down, a level per cycle. It stops
;     when it finds an empty level
; *** nxt-level is a list of pairs <node, parent-node>
   (let ((nodecount 0))
	       (do* ((levelnumb 0 (1+ levelnumb))
		     (nxt-level '((verbs nil))
; *** subtr-by-key handles nodes already reached before (stored in 
;     transformed-nodes)
; *** elimdup-by-key handles nodes reached twice at the same level
			     (elimdup-by-key
			  	(subtr-by-key
			     	   (flatten 
				      (mapcar
					 #'(lambda (x) 
					      (mapcar #'(lambda (y) 
							    (list y (first x)))
						    (get-inthn-deps (first x))))
				   	 nxt-level))
				   printed-nodes)))
		     (printed-nodes 
			 '(verbs)
			 (append printed-nodes (mapcar #'car nxt-level))))
		   ((null nxt-level) nil)
; *** the dolist loops on all the classes at the same level
		   (format t "$$$$$$$$$$$ next level: ~s $$$$$$$$$$$$~%"
				levelnumb)
		   (dolist (nxt-node nxt-level)
			 (print-out-class (first nxt-node) outport)
                         (setq nodecount (1+ nodecount))
	))
     (format t ">>>>>>>> Total Number of Transformed Nodes: ~a~%" nodecount))))

(defun print-out-class (node port)
   (format port "(~s ~%" node)
   (cond ((null (get node 'syn-repr))
     (let ((subcp (get node 'subcat-patterns)))
	(format port "  (loc-cf-def ~s)~%" (get node 'loc-cf-def))
	(format port "  (cf-def ~s)~%" (get node 'cf-def))
	(cond ((not (null (get node 'synonyms)))
	  	 (format port "  (synonyms ~s)~%" (get node 'synonyms))))
	(cond ((get node 'dummy)
	  	 (format port "  (dummy t)~%")))
	(format port "  (grclass-parents ~s)~%" (get node 'grclass-parents))
	(format port "  (grclass-deps ~s)~%" (get node 'grclass-deps))
	(format port "  (transf-source ~s)~%" (get node 'transf-source))
	(format port "  (transf-sons ~s)~%" 
		(mapcar #'second (get node 'transf-sons)))
	(format port "  (appl-cond ~s)~%" (second subcp))
	(format port "  (order-cond ~s))~%" (third subcp))))
	  (t (format port "   (SYNONYM ~s))~%" (get node 'syn-repr)))))

(defun print-dat-class (node port2)
   (format port2 "------>  ~a ~%" node)
   (cond ((null (get node 'syn-repr))
     (let ((subcp (get node 'subcat-patterns)))
	(format port2 "        loc-cf-def: ~s ~%" (get node 'loc-cf-def))
	(format port2 "            cf-def: ~s ~%" (get node 'cf-def))
	(cond ((not (null (get node 'synonyms)))
	  (format port2 "          synonyms: ~s ~%" (get node 'synonyms))))
	(format port2 "   grclass-parents: ~s ~%" (get node 'grclass-parents))
	(format port2 "      grclass-deps: ~s ~%" (get node 'grclass-deps))
	(format port2 "     transf-source: ~s ~%" (get node 'transf-source))
	(format port2 "       transf-sons: ~s ~%" 
		(mapcar #'second (get node 'transf-sons)))
	(cond ((get node 'dummy)
	  	 (format port2 "  dummy~%")))
	(format port2 "    subcat-pattern: ~%")
	(format port2 "             - appl-cond: ~s~%" (second subcp))
	(format port2 "             - cases:     ~s~%" (fifth subcp))))
	  (t (format port2 "     SYNONYM of  ~s ~%" (get node 'syn-repr)))))

(defun overcmember (clause)
  (cond ((null clause) nil)
        ((atom clause)
             (eq clause '!overcomes))
        (t (let ((recurs (mapcar #'overcmember clause)))
               (cond ((all-null recurs) nil)
                     (t t))))))
         
(defun nilmember (clause)
  (cond ((atom clause) nil)
        ((member nil clause) t)
        (t (let ((recurs (mapcar #'nilmember clause)))
               (cond ((all-null recurs) nil)
                     (t t))))))
         
; ***************************************************************************
; *** This should produce the disjunctive normal form of a formula.
;     It also makes some simple checks about tautologies and contradictions
(defun normalize (formula)
  (let ((temp-formula (normalize-int formula)) result)
     (cond ((atom temp-formula) temp-formula)
           ((eq (first temp-formula) 'or)
              (setq temp-formula 
                   (cons 'or (mapcar #'simplify-and-n (rest temp-formula))))
              (do ((or-arg (first (rest temp-formula)) (first temp-formula))
                   (temp-formula (rest (rest temp-formula)) (rest temp-formula)))
                  ((or (eq result t) (and (null or-arg) (null temp-formula)))
                     (cond ((eq result t) t)
                           ((null result) nil)
                           ((eq 1 (length result)) result)
                           (t (cons 'or result))))
                  (cond ((null or-arg) nil)
                        ((eq or-arg t) (setq result t))
                        ((member or-arg temp-formula :test #'equal) nil)
              ; *** the subformula occurs twice; the first occurrence is ignored
                        ((and (eq (first or-arg) 'not)
                              (member (second or-arg) temp-formula :test #'equal))
                           (setq result t)) 
                        ((member (list 'not or-arg) temp-formula :test #'equal)
                           (setq result t)) 
                        (t (setq result (append1 result or-arg))))))
           (t (simplify-and-n temp-formula)))))

; ***************************************************************************
; *** This naively checks for contradictions in and-ed formulae
(defun simplify-and-n (formula)
  (let ((result t))
    (cond ((eq (first formula) 'and)
            (do ((and-arg (first (rest formula)) (first formula))
                 (formula (rest (rest formula)) (rest formula)))
                ((or (null result) (and (null and-arg) (null formula)))
                    (cond ((null result) nil)
                          (t (cons 'and result))))
                (cond ((null and-arg) nil)
                      ((eq and-arg t) nil)
                      ((member and-arg formula :test #'equal) nil)
                      ((and (eq (first and-arg) 'not)
                             (member (second and-arg) formula :test #'equal))
                         (setq result nil)) 
                      ((member (list 'not and-arg) formula :test #'equal)
                         (setq result nil)) 
                      ((eq result t) (setq result (list and-arg)))
                      (t (setq result (append1 result and-arg))))))
          (t formula))))

; ***************************************************************************
; *** This should produce the disjunctive normal form of a formula.
(defun normalize-int (formula)
 (let (or-args rest-args temp-result normalized-args result inner-temp-res
       in-in-temp-res)
   (cond ((atom formula) formula)
   ; *** if the main operator is "or", then normalize the subformulae
         ((eq (first formula) 'or)
            (setq normalized-args (mapcar #'normalize-int (rest formula)))
            (cond ((member t normalized-args) t)
                  (t (setq temp-result nil)
      ; *** the loop to bring the arguments of a "or-ed" subformula to the
      ;     top level
                     (dolist (arg normalized-args)
                          (cond ((eq (first arg) 'or) 
                                   (setq temp-result (append temp-result (rest arg))))
                                (t (setq temp-result (append1 temp-result arg)))))
                     (cons 'or temp-result))))
   ; *** if the main operator is "and", then normalize the subformulae
        ((eq (first formula) 'and)
            (setq normalized-args (mapcar #'normalize-int (rest formula)))
            (setq normalized-args (remove t normalized-args))
            (cond ((= (length normalized-args) 0) t)
                  ((= (length normalized-args) 1)
                     (normalize-int (first normalized-args)))
                  (t (multiple-value-setq (or-args rest-args)
                        (find-or-args normalized-args))
                   (cond ((null or-args) 
   ; *** if no subformula is headed by "or", then append the subformulae, but
   ;     bring the arguments of "and-ed" subformulae to the top level
                            (setq temp-result nil)
                            (dolist (arg normalized-args)
                                (cond ((eq 'and (first arg))
                                         (setq temp-result (append temp-result (rest arg))))
                                      (t (setq temp-result (append1 temp-result arg)))))
                            (cons 'and temp-result))
                         (t (setq temp-result (list rest-args))
    ; *** otherwise, for each "or-ed" argument, carry out a sort of cartesian
    ;     product (distributive property)
    ; *** if formula is '(and (a) (d) (or (b) (c)) (or (e) (f)))
    ;     or-args are '((or (b) (c)) (or (e) (f)))
    ;     temp-result is (((a) (d)))
                            (dolist (arg or-args)
    ; *** arg is first (or (b) (c)) and then (or (e) (f))
                              (setq inner-temp-res nil)
                              (setq result nil)
                              (dolist (sing-arg (rest arg))
    ; *** at the first iteration, sing-arg is first (b), then (c)
    ; *** the inner do must produce (((a) (d) (b)) ((a) (d) (c)))
                                (setq in-in-temp-res nil)
                                (dolist (singtemp temp-result)
                                   (setq in-in-temp-res 
                                      (append1 in-in-temp-res 
                                              (append1 singtemp sing-arg))))
                                (setq inner-temp-res
                                   (append inner-temp-res in-in-temp-res)))
                              (setq result (append result inner-temp-res))
                              (setq temp-result result))
                            (normalize 
                               (cons 'or (mapcar #'(lambda (x)
                                                     (simplify-and-n 
                                                          (cons 'and (flatten-ands x))))
                                            temp-result))))))))
        ((eq (first formula) 'not)
            (cond ((eq (first (second formula)) 'not)
                      (normalize-int (second (second formula))))
                  ((eq (first (second formula)) 'and)
                      (normalize-int (cons 'or (mapcar #'put-not (rest (second formula))))))
                  ((eq (first (second formula)) 'or)
                      (normalize-int (cons 'and (mapcar #'put-not (rest (second formula))))))
                  (t formula)))
        (t formula))))

(defun flatten-ands (arglist)
   (cond ((null arglist) nil)
         ((eq (first (first arglist)) 'and)
              (append (rest (first arglist)) (flatten-ands (rest arglist))))
         (t (cons (first arglist) (flatten-ands (rest arglist))))))

(defun find-or-args (arglist)
 (let (ors notors)
  (dolist (arg arglist (values ors notors))
     (cond ((eq (first arg) 'or) (setq ors (append1 ors arg)))
           (t (setq notors (append1 notors arg)))))))
   
(break "step")
