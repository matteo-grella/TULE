; ££££££££££££££££££££ 
;  The main sections in this file are:
;	LOGICAL BASIS OF THE HIERARCHY
;	TYPES
;	INITIALIZATION PROCEDURES FOR THE HIERARCHY
; ££££££££££££££££££££ 

; *** these initialization procedures build the internal representation of
;     the hierarchy, before the application of the transformation. The
;     resulting hierarchy includes only base (i.e. non transformed) nodes
; *** In order to obtain the full hierarchy, one must evaluate the function
;     'transform-hierarchy', which is defined in PROC/ALLANG/transf-hier

(in-package "USER")

;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;			LOGICAL BASIS OF THE HIERARCHY
;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

; *** The hierarchy actually merges two different hierarchies, having
;     different meanings:
;     - the hierarchy of dependent cases
;     - the hierarchy of surface realizations
; *** The first one has to be read as follows:
;     "Each node includes a specification of all 'obligatory' cases. If a
;      case is missing, this doesn't mean that it isn't obligatory, but just
;      that is is undefined with respect to obligatoriness"
;     So, BASIC-TRANS is below SUBJ-VERBS: SUBJ-VERBS defines all frames having,
;     possibly among other cases, an obligatory subject. BASIC-TRANS is more
;     restricted (i.e. specific, and so below it) because the direct-object is
;     not free with respect to obligatoriness.
;     In other words, the root VERBS must be seen as a specification as:
;     VERBS: subject (present Y or N default N)
;	     object (present Y or N default N)
;	     indirect-object (present Y or N default N)
;	     to-loc (present Y or N default N)
;	     ... (present Y or N default N)
;     So that SUBJ-VERBS is simply:
;     	SUBJ-VERBS is VERBS with subject restricted to present Y
;     And TRANS is:
;     	TRANS is SUBJ-VERBS with object restricted to present Y
;     If I say that a verb is TRANS, then I say that it has a direct-object
;	(present Y), but that it does not have a TO-LOC (default: present N)
; *** The hierarchy of surface realizations applies to verbs having a given
;     case. The interpretation is analogous to the previous one:
;     "If it is specified that case X has realizations cat1 ... catn, this
;      means that these realizations are possible, but nothing is said about
;      the other cat's"
;     For instance, if I say that BASIC-TRANS admits an N subject, and that
;     TRANS (below BASIC-TRANS) also admits a V subject, then I state that any
;     BASIC-TRANS surface case-frame admits a N, but it is not said if it also
;     admits a V, whereas BASIC-TRANS certainly admits both.
;     BASIC-TRANS subject: N (admissible Y)
;	    		   V (admissible Y or N default N)
;	     		   P (admissible Y or N default N)
;	     		   CONJ (admissible Y or N default N)
;	     		   ... (admissible Y or N default N)
;     So that TRANS is:
;     TRANS is BASIC-TRANS with subject:V restricted to admissible Y
; *** Finally, it must be observed that each realization can have different
;     degrees of specificity (e.g. noun[agree] is more specific than noun).
;     This requires some care; in fact, it comes out that "subject:N,V" is
;     neither more generic nor more specific than "subject:N[agree]". In fact
;     the first one is more restrictive wrt the admissibility of a V subject,
;     while the second one is more restrictive wrt the characteristics of N.

;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;		LIST OF ABSTRACT CLASSES
;	(there are no verbs belonging directly to them)
;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

; *** This is now given in subc-classes.dat
;(defvar *ABSTRACT-SUBCAT-CLASSES*
;      '(topic-verbs theme-verbs in-loc-verbs to-loc-verbs from-loc-verbs
;	indobj-verbs result-verbs scompl-a-verbs predsubj-verbs predobj-verbs 
;	sentobj-verbs scompl-che-verbs scompl-di-verbs scompl-inf-verbs 
;	scompl-yn-verbs scompl-q-verbs sentsubj-verbs ssubj-che-verbs 
;	ssubj-di-verbs ssubj-inf-verbs))

;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;			INITIALIZATION PROCEDURES
;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

; *** set-verb-class-def initializes the properties of the verbal-class
;     according to the definition given above; the possible subcat patterns
;     will be augmented on the basis of the transformation rules. This
;     augmentation is carried out by the functions in PROC/ALLANG/transf-hier
; *** the property 'subcat-patterns' is initialized as follows
;     - transf-cl-name: the original class-name; it seems redundant, but it is
;       used later for obtaining the names of the transformed CF's
;     - appl-cond: t; the basic patterns being stored are applicable to all
;       verbs of the class (but the same is not true for transformed patterns)
;     - internal-prefs: t; no preferences applicable to the basic pattern
;     - applied-transfs: (class-name); no transformations applied
;     - cases: the complete case frame definition, obtained by means of
;       inheritance from the local input definition and from the parents
; *** INPUT:
;   -->  class-defs (of type *HIERARCHY-NODE-DEF*)
(defun set-verb-class-def (class-defs)
; *** the first dolist simply stores in the property "loc-cf-def" of each
;     class (node of the graph) the local definition, as given in the input
;     and in the property "grclass-parent" the name of the parents in the
;     graph, and sets up the links to the parents and to the synonyms
  (dolist (cdef class-defs)
	(create-inthn 
		(get-hn-name cdef)
           	(get-hn-synonyms cdef)
          	(get-hn-parents cdef)
          	(get-hn-surf-def cdef)
          	(mapcar #'make-surface-label (get-hn-deep-lab cdef))
          	(get-hn-deep-lab cdef)))
; *** the second dolist determines, via inheritance, the full cf-definition,
;     and, consequently, also the form of the subcat pattern of the class
;     This must be done via a second dolist, since all local cf definitions 
;     must be stored before starting the inheritance
  (dolist (cdef class-defs)
	(set-full-cf-and-patt
		(get-hn-name cdef))))

(defun make-surface-label (deep-label)
      (concatl `(s - ,deep-label)))

; *** the next applies inheritance and fills up the subcat pattern.
(defun set-full-cf-and-patt (cl-name)
; *** Inherit-cf travels the graph upward, according to the parent definition
;     in the class. As a side effect, it sets the property 'cf-def' of the
;     class (if it is already present, it does nothing)
    (inherit-cf cl-name)
; *** Then the full subcat-pattern is stored
    (put-inthn-patterns cl-name 
	     (list cl-name t t (list cl-name) (get-inthn-cf-def cl-name))))

; *** inherit-cf travels the graph upward, in order to inherit the
;     specifications appearing in the ancestors of the class under analysis
(defun inherit-cf (class-name)
    (let ((complete-def (get-inthn-cf-def class-name)))
; *** if the full definition of the class is not already available, compute it
;     from the parents.
; *** it could be already present, since, if a daughters's full CF has already
;     been computed, it has produced the evaluation of the full CF for all
;     of its ancestors
       (cond ((null complete-def)
    	       (let ((parents (get-inthn-parents class-name))
	  	     (currdef (get-inthn-loc-cf-def class-name)))
; *** in currdef the local definition of the case frame; 
; *** in the next dolist, the full definition of the case frame is obtained
;     via inheritance; it has the same form as a local cf definition. 
        	   (dolist (nextpar parents)
; *** if any parent has the full definition already available, then use it,
;     otherwise propagate upward the request to evaluate the full cf.
		 	 (let ((parcf-def (get-inthn-cf-def nextpar)))
	  		     (cond ((null parcf-def)
		  	             (setq parcf-def (inherit-cf nextpar))
		     	     	     (put-inthn-cf-def nextpar parcf-def)))
		     	     (setq currdef (merge-cf currdef parcf-def))))
		   (put-inthn-cf-def class-name currdef)))
	     (t complete-def))))

(defun merge-cf (tempdef inhdef)
;  INPUT --> tempdef: the current local definition, possibly (partially)
;     		     updated via some parent information
;	     inhdef: e new piece of inherited information to be integrated
; *** both tempdef and inhdef have the form of a case definition:
;	(surface-definitions surface-cases deep-cases)
;	-- for each surface case name appearing in the parent's definition:
;	   * if the case name does not appear among the cases of the 
;	     daughter, then it is simply added (together with its surface
;	     definition; the three lists are parallel)
;	   * if the case name already exist, then the daughter's surface
;	     definition is extended according to the new possibilities
;	     specified in the parent
; *** this is a kind of 'union', implemented in the following way:
;     - The final result is collected in 'newdef'
;     - Each newly inherited case (inhdef) is looked for in 'tempdef'
;     - If no correspondence is found, the new case is simply added to 'newdef'
;     - If there is a corresponding case in the previous definition, then the
;       two definitions are merged, and the old definition (in 'tempdef') is
;       marked (written in 'mergedcases')
;     - at the end, newdef is extended with all unmarked cases remained in
;       tempdef
  (cond ((or (null inhdef)
	     (equal inhdef '(nil nil nil)))
	    tempdef)
        ((or (null tempdef)
	     (equal tempdef '(nil nil nil)))
	    inhdef)
	(t (let (mergedcases newcasedefs newsurfcases newdeepcases newcaselab)
	    (do ((nextinhcase (first (second inhdef)) (first reminhcases))
		 (reminhcases (rest (second inhdef)) (rest reminhcases))
		 (nextinhdef (first (first inhdef)) (first reminhdef))
		 (reminhdef (rest (first inhdef)) (rest reminhdef))
		 (nextinhdeep (first (third inhdef)) (first reminhdeep))
		 (reminhdeep (rest (third inhdef)) (rest reminhdeep))
		 (found nil nil))
		((null nextinhcase) 
		   (let ((notmodif (not-used-cases tempdef mergedcases)))
			   (list
			     (append newcasedefs (first notmodif))
			     (append newsurfcases (second notmodif))
			     (append newdeepcases (third notmodif)))))
                (do ((nexttempcase (first (second tempdef)) 
				   (first remtempcases))
		     (remtempcases (rest (second tempdef)) (rest remtempcases))
		     (nexttempdef (first (first tempdef)) (first remtempdef))
		     (remtempdef (rest (first tempdef)) (rest remtempdef))
		     (nextdtempcase (first (third tempdef))
				    (first remdtempcases))
		     (remdtempcases (rest (third tempdef)) 
				    (rest remdtempcases)))
; *** if there are no more 'temp' cases, then the inherited case has not been
;     matched: it is added as such to the new definition; otherwise (found
;     true) nothing need be done (the merged definition has already been added)
		    ((or found (null nexttempcase))
			(cond ((not found)
				(setq newcasedefs
			    		(append1 newcasedefs nextinhdef))
				(setq newsurfcases
			    		(append1 newsurfcases nextinhcase))
				(setq newdeepcases
			    		(append1 newdeepcases nextinhdeep)))))
; *** now we have a single 'tempdef' case and a single 'inherited' case; we
;     can proceed to the match
                   (let ((compatiblab 
   ; *** if the last element of the exploded label is 'nil', then the label
   ;     refers to a case cancelled by a transformation (e.g. s-subj-nil or
   ;     s-infverbcompl-obj-nil). So the test has to be made after having
   ;     removed the final nil. But, in order to maintain the compatibility,
   ;     also the local case must have been cancelled (???)
                         (cond ((is-a-cancelled-case nextinhcase)
                                 (cond ((not (is-a-cancelled-case nexttempcase)) nil)
                                       (t (member-compatib-lab 
		                            (rem-surf-marker
                                              (remove-cancel-marker nextinhcase))
                                            (list 
                                              (remove-cancel-marker nexttempcase))))))
                               (t (member-compatib-lab 
			   	       (rem-surf-marker nextinhcase)
				       (list nexttempcase))))))
; *** if the two labels are compatible, then a match is supposed to have been
;     found. Then, the corresponding case definition has to be extended;
;     compatibility is defined according to the label hierarchy (ex. OBJ and
;     VERBCOMPL-OBJ); the first element of the result is 'up' if the next inherited
;     label is more generic than the one found in 'tempdef', 'down' in the
;     opposite case, 'msg' if neither is subsumed by the other, but a common
;     ancestor was found. The second element is the compatible label found in
;     'tempdef' (if the first is 'up' or 'down') or the most specific common
;     ancestor (if the first is 'msg'); the label in 'newdef' must be the more
;     general: nextinhcase (if 'up'), nexttempcase (if 'down'), 'second
;     compatiblab (if 'msg'). If there is no compatibility, compatiblab is NIL
		 (cond (compatiblab
			(setq found t)
; *** the actual merge of the new case definition is accomplished by
;     'add-case-info'
			(setq mergedcases (cons nexttempcase mergedcases))
			(setq newcasedefs
			    (append1 newcasedefs
			   	(add-case-info nexttempdef nextinhdef)))
; *** the new surface case label depends on the result of the match (see the
;     comments above)
			(cond ((eq (first compatiblab) 'up)
				 (setq newcaselab nextinhcase))
			      ((eq (first compatiblab) 'down)
				 (setq newcaselab nexttempcase))
			      ((eq (first compatiblab) 'msg)
				 (setq newcaselab
					(make-surface-label 
						(second compatiblab)))))
			(setq newsurfcases
			    (append1 newsurfcases newcaselab))
; *** the new deep case label is harder to determine. I assume the following:
;     If the old surface case label (i.e. s-infverbcompl-subj) 'corresponds'
;     to the old deep label (i.e. infverbcompl-subj), then the new label is the
;     one found as 'msg' (e.g. 'subj'). But if there is no correspondence, 
;     the new deep label remains unchanged. This is supposed to cover the case
;     where the deep label comes from a transformation (e.g. s-infverbcompl-obj
;     vs. agt-compl)
			(setq newdeepcases
			  (append1 newdeepcases
			     (cond ((and (eq (first compatiblab) 'msg)
					    (equal nexttempcase
						 (make-surface-label 
							nextdtempcase)))
					 (second compatiblab))
				   (t (let ((compdlab 
						(member-compatib-lab
						   nextdtempcase
						   (list (make-surface-label
								 nextinhdeep)))))
					(cond ((eq (first compdlab) 'up)
						 nextdtempcase)
				   	      (t nextinhdeep))))))))))))))))

(defun not-used-cases (tempdef mergedcases)
   (cond ((equal tempdef '(nil nil nil)) nil)
	 ((member (first (second tempdef)) mergedcases)
	    (not-used-cases 
		(list (rest (first tempdef))
		      (rest (second tempdef))
		      (rest (third tempdef)))
 		mergedcases))
	 (t (let ((tempres (not-used-cases
				(list (rest (first tempdef))
		      	      	      (rest (second tempdef))
		              	      (rest (third tempdef)))
			 	mergedcases)))
		(list (cons (first (first tempdef)) (first tempres))
		      (cons (first (second tempdef)) (second tempres))
		      (cons (first (third tempdef)) (third tempres)))))))

; *** checks if two labels are compatible, according to the label hierarchy.
;     Unfortunately, the hierarchy includes deep labels, but the test must be
;     made on surface labels. The initial s- has already been removed from the
;     input case. This work must be done on the hierarchy cases
; *** Compatible means that they have a common ancestor more specific than
;     Complement or Adjunct
(defun member-compatib-lab (nosurf-inp hiercases)
   (cond ((null hiercases) nil)
         ((null nosurf-inp)
            (dropnil (mapcar #'is-a-cancelled-case hiercases)))
         ((is-a-cancelled-case (first hiercases))
	   (member-compatib-lab nosurf-inp (rest hiercases)))
	 (t (let* ((nosurf-hier (rem-surf-marker (first hiercases)))
; *** get-lab-ancestors finds all ancestors more specific than Complement or
;     Adjunct
		   (inp-ancest (get-lab-ancestors nosurf-inp))
		   (hier-ancest (get-lab-ancestors nosurf-hier))
		   (common-ancest 
			(reverse (intersection inp-ancest hier-ancest))))
		(cond ((member nosurf-inp hier-ancest)
; *** the input case subsumes the case appearing in the hierarchy
			(list 'up (first hiercases)))
		      ((member nosurf-hier inp-ancest)
; *** the case appearing in the hierarchy subsumes the input case 
			(list 'down (first hiercases)))
		      ((not (null common-ancest))
; *** most specific generalization
			(list 'msg (first common-ancest)))
	 	      (t (member-compatib-lab nosurf-inp (rest hiercases))))))))

; *** get-lab-ancestors finds all ancestors more specific than RMOD or
;     VERB-ARG
; *** the 'all' parameter, when t, specifies that the search must not stop
;     at RMOD, VERB-ARG, etc, but it must proceed until the top
;     (DEPENDENT)
(defun get-lab-ancestors (label &optional all)
   (cond ((memq label '(NOFUNCTION RMOD ARG APPOSITION VERB-ARG))
           (cond (all 
                   (cond ((eq label 'RMOD)
                            '(RMOD FUNCTION))
                         ((eq label 'VERB-ARG)
                            '(VERB-ARG ARG FUNCTION))
                         ((eq label 'ARG)
                            '(ARG FUNCTION))
                         ((eq label 'APPOSITION)
                            '(APPOSITION FUNCTION))
                         (t '(NOFUNCTION))))
                 (t nil)))
	 ((null label)
	   (exception 'parse-error 
                      "PROC/subc-hier: get-lab-ancestors 1"))
	 ((eq label 'DEPENDENT)
           (cond (all '(DEPENDENT))
	         (t (exception 'parse-error
                             "PROC/subc-hier: get-lab-ancestors 2"))))
	 (t (cons label 
                (merge-flatten (mapcar #'(lambda (x) (get-lab-ancestors x all))
					 (get label 'more-gen-labels)))))))

; *** lab-subsumes is t, if the first label subsumes the second
(defun lab-subsumes (gen spec)
   (memq gen (get-lab-ancestors spec t)))

; *** it removes the s- prefix (if any) from a surface label
(defun rem-surf-marker (surflabel)
  (let ((expl (explode surflabel)))
	 (cond ((and (or (char= (first expl) #\S) (char= (first expl) #\s))
		     (char= (second expl) #\-))
		  (implode (rest (rest expl))))
	       (t surflabel))))

(defun add-case-info (newsurfdef prevsurfdef)
   (cond ((equal '(nil) prevsurfdef) newsurfdef)
         ((equal '(nil) newsurfdef) prevsurfdef)
	 (t (union prevsurfdef newsurfdef :test #'equal))))

; ************************************************************************
; *** this checks if the last element of the label is NIL, in which case,
;     the label refers to a case cancelled by a transformation
(defun is-a-cancelled-case (label)
  (null (ult (expl+cats (explode label) #\-))))

; ************************************************************************
; *** this eliminates the final NIL from the label of a case cancelled
;     by a transformation
(defun remove-cancel-marker (label)
  (concatl
    (put-separator '- 
      (butlast (expl+cats (explode label) #\-)))))

;$$$$$$$$$$$$$$$$$$$$$$$$$ CLASSES OF THE VERBS $$$$$$$$$$$$$$$$$$$$$$$$$$
; *** this attaches to each verb the list of its subcat classes
(defun set-verbal-classes (v&class)
  (dolist (vc v&class)
    (putprop (first vc) (second vc) 'verbal-class)))

; *** this is for EVALITA2009: since the dictionary verbal class does not
;     appear in the DATA entry, it must be taken from the dictionary. In
;     order to avoid complex modifications to the procedures, I simply
;     add the values of the 'verbal-class feature for all the verbs
;     appearing in the dictionary.
(defun extend-verbal-classes (dictport)
  (do ((entry (read dictport nil #\Escape) (read dictport nil #\Escape)))
      ((equal entry #\Escape))
      (dolist (nxtinterp (second entry))
           (cond ((and (eq 'verb (prendi nxtinterp 'cat))
                       (null (prendi nxtinterp 'type))
                       (null (get (first nxtinterp) 'verbal-class)))
                   (let ((transit (prendi nxtinterp 'transitive)))
                       (putprop (first nxtinterp) 
                                (cond ((eq transit 'yes) '(trans))
                                      ((eq transit 'no) '(intrans))
                                      ((eq transit 'rifl) '(refl)))
                                'verbal-class)))))))

;$$$$$$$$$$$$$$$$$$$$$$$$$ GOVERNED PREPOSITIONS $$$$$$$$$$$$$$$$$$$$$$$$$
; *** this saves in the ALLLANG/KB-ALL/SUBCAT-KB-ALL/verbclass-marked-cases.dat
;     file the set of prepositions which mark complements of the given class
; *** this stores in the same file the list of abstract classes
(defun set-governed-preps (all-classes abstract-classes)
   (with-open-file (oport (build-file-name "ALLLANG/KB-ALL/SUBCAT-KB-ALL/verbclass-marked-cases.dat")
                          :direction :output :if-does-not-exist :create :if-exists :overwrite)
    (let (vcf-def casedefs caselabs found clfound)
      (format oport "~%;$$$$$$$$$$$$$$$$$$$$$$$$$ LIST OF ABSTRACT CLASSES $$$$$$$$$$$$$$$$$$$$$$$$$~%")
      (format oport "(setq *ABSTRACT-SUBCAT-CLASSES*~%   '(~%")         ;  ))
            (dolist (nxtabs abstract-classes)
                (format oport "     ~a~%" nxtabs))
      (format oport "   ))~%")                                      ;  ((
      (format oport "~%;$$$$$$$$$$$$$$$$$$$$$$$$$ GOVERNED PREPOSITIONS $$$$$$$$$$$$$$$$$$$$$$$$$$$$~%")
      (format oport "(setq *VERBCLASS-MARKED-CASES*~%   '(~%")         ;  ))
      (dolist (nxtclass all-classes)
           (cond ((not (member nxtclass abstract-classes))
                    (setq vcf-def (get nxtclass 'cf-def))
                    (setq casedefs (first vcf-def))
                    (setq caselabs (third vcf-def))
         ; *** for each class we get the case definitions
         ;     for instance (the example is on an abstract class just because it is simpler,
         ;       but abstract classes do not undergo this process):
         ;     nxtclass = loc-to-verbs
         ;     casedefs = (((prep (word-typ &loc-to-prep-2)
         ;                        (not (down (cat verb))))
         ;                  (adv (type loc))))
         ;     caselabs = (verb-indcompl-loc+to)
                    (setq clfound nil)
                    (do ((nxtcd (first casedefs) (first casedefs))
                         (casedefs (rest casedefs) (rest casedefs))
                         (nxtcl (first caselabs) (first caselabs))
                         (caselabs (rest caselabs) (rest caselabs)))
                       ((null nxtcd)
                          ;(cond (clfound 
                                   (format oport "     (~a ~a)~%" nxtclass clfound))
                          ; ))
             ; *** Now, we have:
             ;     nxtcd =  ((prep (word-typ &loc-to-prep-2)
             ;                     (not (down (cat verb))))
             ;               (adv (type loc)))
             ;     nxtcl =  verb-indcompl-loc+to 
                      (cond ((or (lab-subsumes 'verb-indcompl nxtcl)
                                 (eq nxtcl 'verb-obj))
                ; *** ver-obj is included for "decided that", where what is decided is the verb-obj
                              (setq found nil)
                              (do ((nxtreal (first nxtcd) (first nxtcd))
                                   (nxtcd (rest nxtcd) (rest nxtcd)))
                                  ((or (null nxtreal) found)
                                     (cond (found 
                                             (setq clfound (cons found clfound)))))
                 ; *** When nxtreal is
                 ;        (prep (word-typ &loc-to-prep-2) (not (down (cat verb))))
                 ;     found is set to (verb-indcompl-loc+to &loc-to-prep-2)
                                  (cond ((and (listp nxtreal)
                                              (eq (first nxtreal) 'prep))
                                           (setq found 
                                              (list nxtcl 
                                                   (first (leggi (rest nxtreal) 'word-typ)))))))))))))
      (format oport "   ))")                                      ;  ((
   )))



