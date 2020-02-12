(in-package "USER")

;********************************************************************
; *** INPUT:
;  >>> condition: condition to verify
;  >>> head: the line of the parent (the governing verb)
;  >>> dependent: the line of the daughter (the possible complement)
;  >>> allines: the lines of the words of the sentence
;  >>> allabs: the labels (the ones already assigned or NIL) of the sentence.
;         allines and allabs are parallel lists
; *** OUTPUT: t if the "condition" is satisfied
(defun gramrul-cf-condit (condition head headlab dependent allines allabs)
   (cond ((null condition) t)
	 ((eq condition t) t)
	 ((eq (first condition) 'and)
	    (gramrul-cf-condit (rest condition)
			 head headlab dependent allines allabs))
	 ((eq (first condition) 'not)
	    (not (gramrul-cf-condit (rest condition)
			 head headlab dependent allines allabs)))
; *** subconditions assumed in "and"; when the list is empty ---> success
	 (t (let ((firstcond (gramrul-cf-singleval 
		       (first condition) head headlab dependent allines allabs)))
              ;(format t " Condition = ~a~%" (first condition))
              ;(break "")
              (cond (firstcond
	               (gramrul-cf-condit 
		           (rest condition) head headlab dependent allines allabs))
                    (t nil))))))

;********************************************************************
(defun gramrul-cf-singleval (subcond head headlab dependent allines allabs)
; *** head and dependent are word lines
; !!!!!!! currently this function is used just to test the conditions on the
; !!!!!!! possible arguments of verbs, and for the enabling conditions on the
; !!!!!!! verb itself. In this context, the only possible predicates are:
;     For the arguments:
;	- not (standard negation)
; 	- word (the dependent is a particular word)
; 	- word-typ (the dependent is of a particular garmmatical type)
;	- type (the dependent is of a particular syntactic subtype)
;	- agree (the dependent and the verb agree in person, gender, and number)
;	- refl-agr (the dependent is a possible emptycompl of a reflexive verb)
;	- down (move down on the dependents of the dependent; this was fully
;	        operating in the application where the links were already
;	   	established, and only the label had to be determined; I try to
;		use them here by exploiting already assigned labels)
;	- cat (a dependent of the dependent is of a particular syntactic category)
;	- mood (the dependent is a verb in this mood)
;	- case (the dependent is a pronoun in this case)
;	- gender (the dependent has this gender)
;	- number (the dependent has this number)
;	- notmyaux
;	- semtype (the dependent has this semantic type - obtained from the
;		   file *HOME-DIR*/KB/GRAMM/semtypes)
;	- lexsemtype (the dependent has this semantic type - obtained from the
;		   lexicon)
;	- clitic (the dependent is a pronominal clitic)
;	- precedes-adjacent (the dependent immediately precedes the verb)
;	- has-interr
; 	- is-a-date (the dependent is a date; to exclude them from being subjects)
;     For the verb:
;	- is-passive (the verb is in passive form)
;	- is-inf-form (the verb is in infinite tense)
;	- has-a-lobj-clitic (a lobj clitic has already been found as a dependent
;		of the verb)
;	- gov-by-modal-with-lobj-visitor (the verb is governed by a modal which has a single
;		visitor which has the lobj case; see the comments on the 'down' operator above)
;	- gov-by-modal-with-liobj-visitor (the verb is governed by a modal which has a single
;		visitor which has the lobj case; see the comments on the 'down' operator above)
;	- gov-by-modal-with-lobj-and-liobj-visitor (the verb is governed by a modal which has a 
;               single visitor which has the lobj case; see the comments on the 'down' operator
;               above)
;	- up-da (the verb is included in a structure of the type 'bello da vedere',
;		 or similar; this involves a complex navigation of the tree, so
;		 that the comments on the 'down' operator apply here)
; *** The remaining predicates were used for determining the labels of non-verbal
;	links. I have left them here or possible extensions
(declare (special *LANGUAGE*))
 (let ((oper (first subcond))
       (arg (second subcond)))
   (case oper
; *** operators for depending complements
        (not (not (gramrul-cf-singleval 
			(second subcond) head headlab dependent allines allabs)))
    ; *** the next used for verbal locutions
        (word (memq (get-synt-word dependent) (inlist arg)))
        (word-typ (has-gramm-type (get-synt-word dependent) arg))
        (type (memq (get-synt-type dependent) (append1 (inlist arg) 'allval)))
	(agree (check-cf-agree head dependent allines allabs))
	(refl-agr (check-cf-refl-agr head dependent allines allabs))
        (down (let ((daughters 
			(find-cf-deps (get-synt-numb dependent) allines allabs)))
                  (cond ((null daughters) nil)
                        (t (setq daughters (remove-daughter daughters 'conj 'coord))
    ; *** remove-daughter removes conjunctions, so that now the length of the list
    ;     should be 1
			  (cond ((null daughters) nil)
                                ((not (= 1 (length daughters)))
				  (setq daughters 	
					(get-main-daughter dependent daughters))
                             	  (gramrul-cf-condit
                                	     (rest subcond)
					     dependent
					     nil
                                	     (first daughters)
                                	     allines allabs))
                                (t (gramrul-cf-condit
                                	     (rest subcond)
					     dependent
					     nil
                                	     (first daughters)
                                	     allines allabs)))))))
        (cat (memq (get-synt-categ dependent) (inlist arg)))
        (mood (check-mood dependent arg allines allabs))
        (case (or (null (get-synt-cases dependent))
                        (member arg 
		           (expl+cats (explode (get-synt-cases dependent)) #\+))))
    ; *** expl+cats (defined in GULL/LEXIC/menudisamb)
        (gender (eq (get-synt-gender dependent) arg))
        (number (eq (get-synt-number dependent) arg))
	(notmyaux (not (eq (first (last dependent)) 'myaux)))
    ; *** if semtype is tested on an art, it is assumed that the test refers to
    ;     the governed noun
        (semtype (cond ((eq (get-synt-categ dependent) 'art)
			  (gramrul-cf-singleval (list 'down subcond)
					       head nil dependent allines allabs))
    ; *** the possible values for arg are in 'GULL/SUBCATS/semtypes'
             	       (t (or (inh-member (get-synt-word dependent) arg)
                              (has-ont-type dependent (sem-to-ont arg))
                                                ; has-ont-type in chunk-parser
               		      (superc-member arg 
		    		(expl+cats 
				   (explode (get-synt-semtype dependent)) #\+))))))
    ; *** nc-head used in SUBCATS/TRHIER/subc-hier to test if an adjective is
    ;     the head of a noun complex
	(nc-head (check-nc-head dependent allines allabs))
        (lexsemtype
          	(superc-member arg 
		    (expl+cats (explode (get-synt-semtype dependent)) #\+)))
        (clitic (is-a-synt-clitic? dependent))
        (precedes-adjacent (tb-cf-adjacent dependent head allines))
        (has-interr (is-question-case dependent allines allabs))
	(is-a-date (check-date dependent allines allabs))
; *** operators for verbs (transformation applicability conditions)
	(is-passive (is-passive head allines allabs))
	(is-inf-form (is-inf-form head allines allabs))
	(has-a-lobj-clitic 
		(has-a-cf-lobj-clitic (get-synt-numb head) allines allabs))
	(gov-by-modal-with-lobj-visitor 
		(gov-by-modal-with-visitor head headlab allines allabs 'lobj))
	(gov-by-modal-with-liobj-visitor 
		(gov-by-modal-with-visitor head headlab allines allabs 'liobj))
	(gov-by-modal-with-lobj-and-liobj-visitor 
		(gov-by-modal-with-visitor head headlab allines allabs 'lobj+liobj))
	(language (member *LANGUAGE* arg))
	(up-da (check-up-da head allines allabs))
; *** other (currently unused) operators
	(raised-obj (check-synt-raised-obj head allines allabs))
        (tense (eq (get-synt-tense dependent) arg))
	(passive (let ((pass (is-passive dependent allines allabs)))
		    (cond ((eq arg 'yes) pass)
			  (t (not pass)))))
	(vderiv (get-synt-vderiv dependent))
	(vtrans (eq (get-synt-vtrans dependent) arg))
        (proper (cond ((eq arg 'yes) (eq (get-synt-type dependent) 'proper))
                       (t (neq (get-synt-type dependent) 'proper))))
        (transitive		; ??? and the reflexives ???
                (cond ((eq arg 'yes)
                        (eq (get-synt-subcat dependent) 'trans))
                      (t (not (eq (get-synt-subcat dependent) 'trans)))))
	(up (let ((upgov (get-parser-governor nil allines allabs))) 
		   (cond ((null upgov) nil)
			 (t (gramrul-cf-singleval
                	 	  (rest subcond) upgov nil head allines allabs)))))
        (int-range (cond ((eq arg 'month-day)
			    (and (integerp (get-synt-value dependent))
			         (< (get-synt-value dependent) 32)))
			  (t (exception 'parse-error "PROC/checkcond: Unknown 'range' argument"))))
        (value (eq (get-synt-value dependent) arg))
	(no-dependents 
		(null (find-cf-deps (get-synt-numb dependent) allines allabs)))
        (has-mod (check-modifiers dependent (rest subcond) allines allabs))
; *** unknown operator
	(otherwise (exception 'parse-error "PROC/checkcond: Unknown condition code" oper)))))

; **************************************************************************
; *** performs the agreement check between a dependent (possibly a subject)
;     and the verb
; *** head is the (main) verb
; *** dependent is the dependent word on which the agreement check is required
; *** allines are all the data lines of the sentence
; *** allabs are all the labels already found of the sentence
(defun check-cf-agree (head dependent allines allabs)
(declare (special *LANGUAGE*))
(let (dep-gend dep-numb dep-pers)
 (cond ((eq 'verb (get-synt-categ head))
	 (cond ((memq (get-synt-categ dependent) '(verb conj))
; *** it is a sort of default that sentential subjects are masculine singular:
;     'andare al mare è considerato un piacere'
	         (setq dep-gend 'm)
		 (setq dep-numb 'sing)
		 (setq dep-pers 3))
	       ((eq (get-synt-categ dependent) 'prep)
	         (setq dep-gend 'm)
		 (setq dep-pers 3)
		 (cond ((has-gramm-type (get-synt-word dependent) '&subj-in)
; *** this condition for 'in molti', which must be plural. I don't know if
;     a more accurate control is required for the dependent of 'in'. In the
;     process of case-frame check, this function should be used just in case
;     the case in the TB is marked as subject, and this should happen just for
;     'in molti', 'in pochi', which are necessarily plural
		 	  (setq dep-numb 'pl))
		       (t (setq dep-numb 'sing))))
	       (t (setq dep-gend (get-synt-gender dependent))
		 (setq dep-numb 
			(get-cf-numb-with-conj dependent allines allabs))
		 (setq dep-pers (get-synt-person dependent))
	         (cond ((null dep-pers) (setq dep-pers 3)))))
; *** this is a request to check agreement between a verb and a possible
;     subject; the check is very complex. Relevant examples are:
;	- I ragazzi hanno cors'o' a lungo
;	- I ragazzi sono cors'i' a casa
;	- I ragazzi sono stat'i' salutat'i'
;	- I ragazzi hanno incontrat'o' Maria
;	- I ragazzi si erano occupati (pseudo-riflessivo)
;	- I ragazzi si erano lavati (vero riflessivo)
;	- Si era visto che (impersonale)
;	- Si era (?veniva) visti come dei rivoluzionari
;	- I ragazzi la hanno incontrat'a'
; *** I will leave out the last example, since it involves an agreement between
;     the verb and the direct object, which, in case, will be requested via a
;     different predicate
; *** However, the procedure is entered with the 'head' argument bound to the
;     main verb (participle); but we must check:
;     1 - Number agreement with the top auxiliary
;     2 - Gender and number agreement with any verb governed by 'essere' (to be)
;         or 'venire' (to come, which can also be used to form the passive)
;     3 - Nothing with any verb governed by 'avere' (to have)
; *** So, the first thing to do is to check if the verb is a participle, and
;     in this case to retrieve its auxiliary in the tree.
	 (cond ((and (eq 'participle (get-synt-mood head))
		     (eq 'past (get-synt-tense head)))
; *** the auxiliaries are extracted from the case frame 
;     'I ragazzi sono stati visti in giardino' --> (stati)
		 (let* ((nearestaux 
			  (select-aux
			    (find-cf-deps (get-synt-numb head) allines allabs))))
		    (cond ((not (null nearestaux))
; *** there is at least one auxiliary
			    (cond ((has-gramm-type (get-synt-word nearestaux) '&to-be)
       ; *** it is "to be" check gender and number agreement of the head
				    (cond ((not (int-ch-agree
						     head
						     (list dep-gend dep-numb)
					 	     '(gender number)))
					      nil)
       ; *** the head agrees. Proceed with the auxiliaries
			  	          ((and (eq 'participle 
						    (get-synt-mood nearestaux))
			     	      		(eq 'past 
						    (get-synt-tense nearestaux)))
       ; *** the nearest auxiliary is 'to be' in the past participle; if it does
       ;     not agree in gender and number, failure;
		  		    	     (cond
				      	        ((not (int-ch-agree
							   head
							   (list dep-gend dep-numb)
					 		   '(gender number)))
					 	   nil)
       ; *** if ok, there must be another auxiliary (for 'sono stati visti')
       ;     The second auxiliary is required: *Stati visti i problemi, ...
       ;     N.B. I allow going up just two levels
				  	        (t (let* ((secondaux 
					   	       (select-aux 
						         (find-cf-deps
						          (get-synt-numb nearestaux)
							   allines allabs))))
					   	     (cond ((null secondaux)
							     nil)
							   (t (int-ch-agree
							      secondaux
							      (list dep-numb
								    dep-pers)
							      '(number person))))))))
       ; *** This is 'essere', but not in the past participle: it must be the main
       ;     auxiliary; check for number and person
					      (t (int-ch-agree
						      nearestaux
						      (list dep-numb dep-pers)
						      '(number person)))))
			          ((has-gramm-type (get-synt-word nearestaux) '&other-aux)
; *** The auxiliary is 'venire', 'vedere', or 'andare' (va visto come ...): it
;     must be the main auxiliary: check for number and person
				    (int-ch-agree
					      nearestaux
					      (list dep-numb dep-pers)
					      '(number person)))
; *** The auxiliary is 'avere'
			          ((has-gramm-type (get-synt-word nearestaux) '&to-have)
; *** if there is a clitic, then there need not be agreement in gender or
;     number ('il tuo amico le ha viste'). The agreement in person must be
;     checked in any case.
; *** Note that, as stated in a previous comment, there must be agreement
;     between the clitic and the participle. This is not checked here. 
;     I'm not sure if it should, or if a different predicate is needed for
;     this special 'object agreement'
				    (cond ((eq *LANGUAGE* 'italian)
                                            (cond ((has-a-cf-lobj-clitic
						       head allines allabs)
					            (and (int-ch-agree head
						             (list dep-pers)
						             '(person))
					                 (int-ch-agree nearestaux
						             (list dep-pers)
						             '(person))))
; *** if there is no clitic, then the main past participle must be masculine
;     singular (le ragazze hanno visto le loro amiche)
					         ((and (eq 'm (get-synt-gender head))
				      	               (eq 'sing 
						              (get-synt-number head)))
					            (int-ch-agree nearestaux
						             (list dep-numb dep-pers)
						             '(number person)))
				                 (t nil)))
           ; *** if the language is not Italian, standard check
                                          (t (int-ch-agree nearestaux
					             (list dep-numb dep-pers)
					             '(number person)))))
			          ((has-gramm-type 
                                      (get-synt-word nearestaux) '&progressive-aux) nil)
      ; *** the previous one is an error
				  (t (exception 'parse-error
                                               "PROC/checkcond: Problems with auxiliaries 3-b" 
                                               head nearestaux))))
; *** if there are no auxiliaries, this is a reduced relative
; *** In case of reduced relatives, the agreement is not relevant; return t
			  (t t))))
; *** If the verb is not a past participle, standard agreement
	       (t (int-ch-agree head (list dep-numb dep-pers)
				     '(number person)))))
; *** the head is not a verb: agreement on gender and number; I assume that
;     agreement is never tried on conjunctions, adverbs, etc. In any case,
;     it would return true, since nil unifies with anything (as well as allval)
       (t (int-ch-agree head 
		 	(list (get-synt-gender dependent)
			      (get-synt-number dependent))
			'(gender number))))))

; **************************************************************************
; *** performs the agreement check between a dependent and the verb to check
;     if the dependent is a possible emptycompl of a reflexive
; *** head is the (main) verb
; *** dependent is the dependent word on which the agreement check is required
; *** allines are all the data lines of the sentence
; *** allabs are all the labels already found of the sentence
; *** The conditions are that the dependent is a pronoun which is
;     - of type 'refl-impers' if the verb is in the third person (si)
;     - of type 'pers', of person 1 or 2, which must be the same person of
;       the verb (mi ti ci vi)
;     - of the same number of the verb (*vi lavi)
(defun check-cf-refl-agr (head dependent allines allabs)
  (let ((dep-numb (get-synt-number dependent))
        (dep-pers (get-synt-person dependent))
        (dep-type (get-synt-type dependent))
        (verb-person 'fail)
        (verb-number 0))
     (cond ((and (eq 'participle (get-synt-mood head))
		 (eq 'past (get-synt-tense head)))
; *** the auxiliaries are extracted from the case frame 
;     'I ragazzi si sono rifiutati' --> (sono)
	     (let* ((nearestaux 
		      (select-aux (find-cf-deps (get-synt-numb head)
                                         allines allabs))))
		 (cond ((not (null nearestaux))
; *** there is at least one auxiliary
			 (cond ((has-gramm-type (get-synt-word nearestaux) '&to-be)
; *** the nearest auxiliary is 'essere', as it must be
			  	  (cond ((and (eq 'participle 
						  (get-synt-mood nearestaux))
			     	      	      (eq 'past 
						  (get-synt-tense nearestaux)))
; *** the nearest auxiliary is in the past participle; ther cannot be two
;     auxiliaries before with a reflexive
                                          nil)
                                        (t (setq verb-person
						(get-synt-person nearestaux))
                                           (setq verb-number
						(get-synt-number nearestaux)))))
; *** the nearest auxiliary is not 'essere': failure
			       (t nil)))
; *** the main verb is in the past participle, but without auxiliaries
;     this could happen for 'le piante, risvegliatesi in primavera'
		       (t (setq verb-number (get-synt-number head))
                          (setq verb-person 3)))))
; *** the main verb is not in the past participle
   ; *** if it is in the infinite or gerund, the refle
           ((memq (get-synt-mood head) '(infinite gerund))
	      (setq verb-person 4))
           (t (setq verb-person (get-synt-person head))
              (setq verb-number (get-synt-number head))))
; *** now we have the number and person both of the verb and of the pronoun
    (or (and (eq dep-type 'refl-impers) (memq verb-person '(3 4)))
        ; *** if refl-impers, the verb must be in the third person, or in 
        ;     infinite or gerund
        (and (eq dep-type 'pers)
        ; *** otherwise, the pronoun must be of first or second person,
        ;     and the verb must be infinite or gerund, or agree in person and
        ;     number
             (memq dep-pers '(1 2))
             (or (eq verb-person 4)
                 (and (eq dep-numb verb-number)
                      (eq dep-pers verb-person)))))))

;********************************************************************
; *** gets the number of a dependent; if it is singular, it checks if it
;     governs a coordinating conjunction (and: The boy and the girl); in
;     such a case, it returns 'pl
(defun get-cf-numb-with-conj (dep allines allabs)
  (declare (special *LANGUAGE*))
  (let ((numb (get-synt-number dep)))
	(cond ((eq numb 'pl) 'pl)
	      (t (let ((depdeps (find-cf-deps (get-synt-numb dep) allines allabs)))
		     (cond ((is-there-a-coord depdeps) 'pl)
; *** in the next one, numb could be either 'sing or NIL 
                           ((and (eq *LANGUAGE* 'english)
                                 (is-there-a-collect depdeps))
                              'allval)
			   (t numb)))))))

;********************************************************************
; *** this is for "a number of them are ..."
(defun is-there-a-collect (deps)
   (cond ((null deps) nil)
	 ((and (eq 'noun (get-synt-categ (first deps)))
               (has-gramm-type (get-synt-word (first deps)) '&group-noun))
	   t)
	 (t (is-there-a-collect (rest deps)))))

;********************************************************************
(defun is-there-a-coord (deps)
   (cond ((null deps) nil)
	 ((and (eq 'conj (get-synt-categ (first deps)))
               (or (eq (get-synt-word (first deps)) #\,)
	           (has-gramm-type (get-synt-word (first deps)) '&and)))
	   t)
	 (t (is-there-a-coord (rest deps)))))

;********************************************************************
; *** checks the agreement between word1 and word2 on the specified features
; *** used in this file, in "check-cf-agree, which is invoked"
;	above in correspondence with the 'agree' operator
; !!!!!!!!!! agr-unif defined in ALLLANG/PROC-ALL/TAGGER-PROC-ALL/postagger
(defun int-ch-agree (word1 word2-vals features)
  (cond ((null features) t)
  	((eq 'gender (first features))
	  (and (agr-unif (list (get-synt-gender word1))
	  		 (list (first word2-vals)))
	       (int-ch-agree word1 (rest word2-vals) (rest features))))
  	((eq 'number (first features))
	  (and (agr-unif (list (get-synt-number word1))
	  		 (list (first word2-vals)))
	       (int-ch-agree word1 (rest word2-vals) (rest features))))
  	((eq 'person (first features))
	  (and (agr-unif (list (get-synt-person word1))
	  		 (list (first word2-vals)))
	       (int-ch-agree word1 (rest word2-vals) (rest features))))
	(t (exception 'parse-error "PROC/checkcond: Unknown agreement feature" features))))

;********************************************************************
(defun check-synt-raised-obj (head allines allabs)
; *** 'head' is a modal or any type enabling object raising; the function
;     checks if the controlled verb lacks the object
; *** it is assumed that the first verb in the infinite following 'head' is the
;     controlled verb
; !!!!! Currently, it does nothing, since the parse tree is not supposed to be
; !!!!! entirely available
;  (dolist (nextdep head-cf)
;     (cond ((and (eq (get-synt-categ nextdep) 'verb)
;		 (eq (get-synt-mood nextdep) 'infinite))
; *** 'nextdep' is the controlled verb
; *** extract-case-frame returns nil if no match was found, the cf otherwise
;	     (return (extract-case-frame nextdep dtree sent-id '(raised-obj))))))
	nil)

;********************************************************************
; *** find all dependents already established for the word whose line number is
;     lnumb
(defun find-cf-deps (lnumb allines allabs)
  (let (deps nxtlab)
     (dolist (nxtline allines deps)
         (setq nxtlab (first allabs))
         (setq allabs (rest allabs))
         (cond ((and (not (null nxtlab))
		     (equal (first nxtlab) lnumb))
	         (setq deps (append1 deps nxtline)))))))

;********************************************************************
; *** find all dependents already established for the word whose line number is
;     lnumb, but returns also their link upward
(defun find-cf-deps+labs (lnumb allines allabs)
  (let (deps nxtlab)
     (dolist (nxtline allines deps)
         (setq nxtlab (first allabs))
         (setq allabs (rest allabs))
         (cond ((and (not (null nxtlab))
		     (equal (first nxtlab) lnumb))
	         (setq deps (append1 deps (list nxtline nxtlab))))))))

;********************************************************************
; *** removes from a list of daughters the ones which are of category 'cat'
;     and of type 'type'
(defun remove-daughter (daughters cat type)
  (cond ((null daughters) nil)
	((and (eq (get-synt-categ (first daughters)) cat)
	      (or (eq type 'any)
	  	  (eq (get-synt-type (first daughters)) type)))
	  (remove-daughter (rest daughters) cat type))
	(t (cons (first daughters)
	  	 (remove-daughter (rest daughters) cat type)))))

;****************************************************************
; *** in presence of multiple daughters, it chooses, if any, the main one
(defun get-main-daughter (line daughters)
  (cond ((eq (get-synt-categ line) 'prep)
  	   (remove-daughter daughters 'adv 'any))
        ((eq (get-synt-categ line) 'num)
	   (remove-daughter daughters 'adv 'any))
        ((eq (get-synt-categ line) 'art)
	   (remove-daughter (remove-daughter daughters 'adv 'any) 'predet 'any))
	(t daughters)))

;*****************************************************************************
; *** does the verb have an interrogative dependent?
;     This need be implemented as a trick, since the verb in question usually
;     has not yet been analyzed.
;     It looks for the last unlinked element in the lines preceding the 
;     dependent (assumed to be a verb). Then, it looks if it is of interr
;     type. 
;     This should work both for adverbials (where) and for adjectives (which
;     concerts), since the latter are the head of the phrase.
;     In case the found element is a prep, then the line that follows it is
;     inspected ("from where")
(defun is-question-case (dep allines allabs)
   (let ((prevlines (reverse (find-cf-prevlines allines dep)))
         (prevlabs (reverse (find-cf-prevlabs allines allabs dep)))
         (foundanswer nil))
       (do* ((lineafter nil line)
             (line (first prevlines) (first prevlines))
             (lab (first prevlabs) (first prevlabs))
             (prevlines (rest prevlines) (rest prevlines))
             (prevlabs (rest prevlabs) (rest prevlabs)))
           ((or foundanswer (null line))
             (cond ((eq foundanswer 'yes) t)
                   (t nil)))
           (let ((categ (get-synt-categ line)))
               (cond ((and (eq categ 'verb) (neq (get-synt-type line) 'aux))
                        (setq foundanswer 'no))
                     ((null lab)
                        (cond ((and (member categ '(adj adv pron))
                                    (eq 'interr (get-synt-type line)))
                                 (setq foundanswer 'yes))
                              ((and (eq categ 'prep)
                                    (eq 'interr (get-synt-type lineafter)))
                                 (setq foundanswer 'yes)))))))))

;*****************************************************************************
; *** this collects all the lines preceding "dep" 
(defun find-cf-prevlines (lines dep)
   (cond ((null lines) 
            (exception 'parse-error "PROC/checkcond: find-cf-prevline"))
         ((equal dep (first lines)) nil)
         (t (cons (first lines) (find-cf-prevlines (rest lines) dep)))))
 
;*****************************************************************************
; *** this collects all the lines preceding "dep" and returns them in inverse
;     order (the first of the result is the immediately preceding, and so on)
(defun find-cf-prevlabs (lines labs dep)
   (cond ((null lines) 
            (exception 'parse-error "PROC/checkcond: find-cf-prevlab"))
         ((equal dep (first lines)) nil)
         (t (cons (first labs) (find-cf-prevlabs (rest lines) (rest labs) dep)))))
 
;*****************************************************************************
; *** does the word (a determiner) governs a date?
(defun check-date (dep allines allabs)
  (declare (special *SYSTEM-CONTEXT*))
  (let ((modifs (find-cf-deps (get-synt-numb dep) allines allabs)))
   ; *** the format of a date governed by an article is "il 28 ottobre 2004"
   ;     or "il 28/10/2004"; in both cases, the article has just one dependent
   ;     either a number (28) or a date (28/10/2004)
    (cond ((not (= 1 (length modifs))) nil)
          (t (let ((depcat (get-synt-categ (first modifs))))
               (cond ((eq depcat 'DATE) t)
   ; *** it actually is of category date
                     ((eq depcat 'NUM)
   ; *** it is of category number
                       (let ((modifs2 (find-cf-deps (get-synt-numb (first modifs))
							 allines allabs)))
   ; *** and governs a month
                         (cond ((and (= (length modifs2) 1)
                                     (eq 'noun (get-synt-categ (first modifs2)))
                                     (inh-member (get-synt-word (first modifs2))
                                                 '£month))
                                  t)
           ; *** it is a number, but does not govern a month
			       (t nil))))
                     ((eq depcat 'NOUN)
                        (and (neq *SYSTEM-CONTEXT* 'atlas)
                             (inh-member (get-synt-word (first modifs)) '£time-loc)))
   ; *** its category is neither date nor number nor a noun
		     (t nil)))))))

;*****************************************************************************
; **** checks if among all the dependents of 'dependent' there is one that
;      satisfies the conditions
(defun check-modifiers (dependent conditions allines allabs)
  (let ((found nil)
        (modifs (find-cf-deps (get-synt-numb dependent) allines allabs)))
   (do ((nextmod (first modifs) (first modifs))
        (modifs (rest modifs) (rest modifs)))
       ((or (null modifs) found) found)
       (setq found 
	     (gramrul-cf-singleval 
			conditions dependent nil nextmod allines allabs)))))
   
; ***************************************************************
;    returns true if verb-cf includes a pronominal clitic in the LOBJ case.
;    The condition on LOBJ set because this applies to object cliticization
(defun has-a-cf-lobj-clitic (verblnumb allines allabs)
  (let ((found nil))
    (do ((nxtline (first allines) (first allines))
         (allines (rest allines) (rest allines))
         (nxtlab (first allabs) (first allabs))
         (allabs (rest allabs) (rest allabs)))
        ((or found (null nxtline)) found)
        (cond ((and (not (null nxtlab))
		    (equal (first nxtlab) verblnumb)
		    (is-a-cf-lobj-clitic? nxtline))
		 (setq found t))))))

;*****************************************************************************
; *** the 'line' contains the main verb, but the check has to be made on the
;     top-most auxiliary
(defun check-mood (verbline arg allines allabs)
   (memq (get-synt-mood (first-aux verbline allines allabs)) (inlist arg)))

;********************************************************************
; *** the function returns t, nil, or "obl". The latter is used for "andare"
;     (va visto) and venire (viene visto), that are necessarily passive
;     forms, differently from "to be", which is a tense auxiliary in case
;     of intransitives
(defun is-passive (verbline allines allabs)
  (let* ((verb-cf (find-cf-deps (get-synt-numb verbline) allines allabs))
         (auxiliaries 
            (dropnil 
               (mapcar 
                  #'(lambda (x) (cond ((eq (get-synt-type x) 'aux) x) (t nil)))
                  verb-cf)))
         (aux-verbs (mapcar #'get-synt-word auxiliaries)))
   (cond ((and (eq (get-synt-mood verbline) 'participle)
  	       (eq (get-synt-tense verbline) 'past)
  	       (is-passivizable verbline))
; *** There is no possibility to check if 'this use' of the verb is passivizable
;     or not. What 'is-passivizable' does is to check if 'at least one' of the
;     subcat classes of the verb admit passivization. This produces that in
;     'La nave è affondata' the verb is taken as a passive
; *** the first branch of the or checks that the verb has no auxiliaries
;     (for reduced relative clauses)
	    (cond ((or (null auxiliaries)
; *** The next branch checks if there is a 'to be' auxiliary, but without
;     a refl-impers particle (si: refl-impers) to exclude 'si e' visto' 
;     -impersonal- and 'si e' lavato' -reflexive, which are not passives.
                       (and (has-gramm-type (first aux-verbs) '&to-be)
 	               (neq 'refl-impers 
                           (get-synt-type 
                              (pick-prevword 
                                 (get-synt-numb (first auxiliaries)) allines)))))
                     t)
; *** the next for 'venire' and 'andare' (viene considerato, va considerato)
;     note that this must be the first auxiliary (*è venuto considerato)
	          ((has-gramm-type (first aux-verbs) '&strong-pass-aux)
                     'obl)
                  (t nil)))
         (t nil))))

; ***************************************************************
(defun is-passivizable (verbline)
; *** checks if at least one of the subcat classes of the verb is a subclass of
;     either 'trans' or 'indobj-verbs', which are the most general classes
;     undergoing passivization (according to the definition of that transformation
;     appearing in KB/subc-hier")
  (cf-hier-dep (get-cf-verbclass verbline) '(basic-trans sentobj-verbs) nil))

; ***************************************************************
(defun cf-hier-dep (downnodes upnodes closed)
  (cond ((null downnodes) nil)
	((intersection downnodes upnodes) t)
	(t (let ((levelup (flatten (mapcar #'get-inthn-parents downnodes))))
	       (cf-hier-dep 
		     (subtrl levelup closed) upnodes (union closed downnodes))))))

; ***************************************************************
; *** it filters out from a list of subcat class labels the ones beginning
;     with 'dummy+'. In fact these classes are just abstract classes, not
;     corresponding with any actual realization
(defun exclude-dummy (cllist)
  (cond ((null cllist) nil)
	((get (first cllist) 'dummy) (exclude-dummy (rest cllist)))
	(t (cons (first cllist) (exclude-dummy (rest cllist))))))

; ***************************************************************
;    returns true if the verb is in infinite form (i.e. requires cancellation
;    of the subject)
(defun is-inf-form (verbline allines allabs)
  (let ((aux (first-aux verbline allines allabs)))
; *** the actual verb is in the infinite
       (or (eq (get-synt-mood verbline) 'infinite)
; *** it is a gerund without auxiliaries
           (and (or (null aux)
                    (equal aux verbline))
		(eq (get-synt-mood verbline) 'gerund))
; *** it is a present participle without auxiliaries
           (and (or (null aux)
                    (equal aux verbline))
		(eq (get-synt-mood verbline) 'participle)
                (eq (get-synt-tense verbline) 'pres))
; *** its first auxiliary is in the infinite or in the gerund
      	   (and aux
	        (memq (get-synt-mood aux) '(infinite gerund))
		(eq (get-synt-mood verbline) 'participle)
; *** the next condition to avoid that a subordinate of a modal having a
;     governing aux is taken as the auxiliary of the modal
;     (as in "voleva essere amato")
		(not (and (eq (get-synt-type verbline) 'mod)
			  (index-precedes (get-synt-numb verbline) 
					  (get-synt-numb aux))))))))

; ***************************************************************
(defun first-aux (verbline allines allabs)
  (let ((aux (select-aux (find-cf-deps (get-synt-numb verbline) allines allabs))))
      (cond ((null aux) verbline)
	    (t (first-aux aux allines allabs)))))

; ***************************************************************
(defun select-aux (daughters)
   (cond ((null daughters) nil)
	 ((eq 'aux (get-synt-type (first daughters))) (first daughters))
	 (t (select-aux (rest daughters)))))

; ***************************************************************
(defun get-parser-governor (verblink allines allinks)
   (cond ((null verblink) nil)
         (t (find-a-line `(position ,(first verblink))
                                  allines allinks))))

; ***************************************************************
; *** checks if "verbline" is governed by a modal that has an
;     unmarked RMOD
(defun gov-by-modal-with-visitor (verbline verblab allines allabs case)
  (let ((parent (first (get-parser-governor verblab allines allabs)))
        nextunm unmarked)
     (cond ((and (not (null parent))
		 (eq (get-synt-categ parent) 'verb)
		 (eq (get-synt-type parent) 'mod))
   ; *** if the verb in question has a parent and that parent is a modal
	     (let* ((par-cf (find-cf-deps (get-synt-numb parent) allines allabs))
                    (cf-lines (get-cf-deps 'RMOD par-cf allines allabs)))
       ; *** retrieve all its RMOD dependents
                 (dolist (nxtline cf-lines)
           ; *** and retrieve the unmarked ones
                     (setq nextunm nil)
                     (cond ((and (not (eq (get-synt-categ nxtline) 'prep))
                                 (or (not (eq (get-synt-categ nxtline) 'pron))
                                     (member 'lobj 
                                          (expl+cats 
                                                (explode (get-synt-cases nxtline)) #\+))))
                              (setq nextunm (cons 'lobj nextunm))))
                     (cond ((or (and (eq (get-synt-categ nxtline) 'prep)
                                     (has-gramm-type (get-synt-word nxtline) '&indobj-prep))
                                (and (eq (get-synt-categ nxtline) 'pron)
                                     (member 'liobj 
                                         (expl+cats (explode (get-synt-cases nxtline)) #\+))))
                              (setq nextunm (cons 'liobj nextunm))))
                     (setq unmarked (cons nextunm unmarked)))
                 (cond ((eq 1 (length unmarked))
         ; *** if the length of unmarked is one, then it is either ((lobj)), or ((liobj)) or
         ;     ((lobj liobj))
                         (cond ((member case (first unmarked)) t)
                               (t nil)))
         ; *** if the length of unmarked is two, then it is either ((lobj) (liobj)), or 
         ;     ((liobj) (lobj)) or ((lobj liobj) (lobj)) ....
                       ((eq 2 (length unmarked))
                         (cond ((and (equal case 'lobj+liobj)
                                     (or (and (member 'lobj (first unmarked))
                                              (member 'liobj (second unmarked)))
                                         (and (member 'liobj (first unmarked))
                                              (member 'lobj (second unmarked)))))
                                  t)
                               (t nil)))
                           (t ;(exception-nothrow "More than two unmarked visitors (checkcond.lisp)")
                              nil))))
           (t nil))))

; ***************************************************************
; *** extracts from lines-to-check all lines that are linked to their
;     governor via the "label" label
(defun get-cf-deps (label lines-to-check allines allabs)
   (cond ((null lines-to-check) nil)
	 ((null allines) 
            (exception 'parse-error "PROC/checkcond: get-cf-deps"))
	 ((equal (first lines-to-check) (first allines))
	    (cond ((eq (second (first allabs)) label) 
                     (cons (first allines) 
                           (get-cf-deps label (rest lines-to-check) (rest allines) (rest allabs))))
		  (t (get-cf-deps label (rest lines-to-check) (rest allines) (rest allabs)))))
	 (t (get-cf-deps label lines-to-check (rest allines) (rest allabs)))))

; ***************************************************************
(defun class-of-controlled-verb (verb-comp verb-cf classes)
; *** verb-comp is a modal or any type enabling object raising; the function
;     checks if the controlled verb (in verb-cf) is of a transitivity class,
;     i.e. if it has an object that can be raised
; *** it is assumed that the first verb in the infinite in verb-cf is the
;     controlled verb
   (dolist (nextdep verb-cf)
      (cond ((and (eq (get-synt-categ nextdep) 'verb)
 		 (eq (get-synt-mood nextdep) 'infinite))
 	     (return (intersection (get-verb-class nextdep) classes))))))

; ***************************************************************
(defun sem-to-ont (semconc)
  (cond ((eq semconc '£gen-loc) '££location)
        (t semconc)))

; ***************************************************************
(defun check-nc-head (dependent allines allabs)
   (let (found)
     (dolist (nxtdep 
		(find-cf-deps (get-synt-numb dependent) allines allabs)
                found)
           (let ((depcat (get-synt-categ nxtdep)))
                   (cond ((or (member depcat '(noun pron))
                              (and (eq depcat 'verb)
                                   (member (get-synt-mood nxtdep)
                                           '(participle gerund infinite))))
                           (setq found t)))))))
