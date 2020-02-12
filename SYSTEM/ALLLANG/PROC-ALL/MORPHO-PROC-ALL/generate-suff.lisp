 (in-package "USER")

; *** The procedures in this file generate an index on suffixes, in order
;     to make faster the search during morphological analysis.
;
; *** The input data are in the global *SUFFIX-TABLE*, which is defined in
;     KB/MORPHO/suffixes.dat, and have the following form:
; >>>> for ADJ and NOUN:
;	  (CATEG (CODE-1 ((SUFF-111 SUFF-112 ... SUFF-11K)
;                          ...
;	                  (SUFF-1N1 SUFF-1N2 ... SUFF-1NK))
;		  CODE-2 ((SUFF-211 SUFF-212 ... SUFF-21K)
;                          ...
;	                  (SUFF-2N1 SUFF-2N2 ... SUFF-2NK))
;            ... ) ...)
;     where CATEG is NOUN or ADJ, CODE-i are integers, and SUFF-ijk are the
;     actual suffixes (m: masculine, f: feminine, n: neuter):
;         The three indices are associated with:
;         i: the morphological class code
;         j: the case (in case the language has no case, just one row exists
;            for each code)
;         k: the <gender number> identifier
;     the actual meaning of the j and k possible values is defined in the file
;     "suff-tab-descr.dat"
;	  !!! SUFF-ij may be NIL, in case either the word the entry is not
;             defined (e.g. feminine for masculine nouns or some tenses for
;             different languages) or @empt in case of null suffix (the
;             surface form is identical to the lemma)
; >>>> for ADVERBS:
;        (CODE-1 SUFF-1 CODE-2 SUFF-2 ... CODE-N SUFF-N)
;     it is assumed that adverbs are of two types:
;     1. Invariable forms occurring as such in the dictionary
;     2. Productively generated from adjectives. In this case, the CODE-i
;        appearing in the table refers to adjectival codes, while the
;        corresponding SUFF-i specifies the suffix to be attached to the
;        adjectival root in order to generate the adverb
;        Ex. "admirable" --> (admir ((cat adj classe (3))))
;            ["admir" is the root, the third class for the "able" suffix]
;            In the adverb entry of the table there is the pair (3 ably),
;            so that "admirably" is generated
; >>>> for VERBS:
;     (VERB (CODE-1 ((SUFF-1a1 SUFF-1a2 ... SUFF1a6) 
;				 ...
;	             (SUFF-1g1 SUFF-1g2 ... SUFF1g6) 
;	             (SUFF-1h1 SUFF-1h2 SUFF-1h3 SUFF-1h4)
;	             (SUFF-1i1 SUFF-1i2 SUFF-1i3 SUFF-1i4)
;	             (SUFF-1l)
;	             (SUFF-1m)
;	             (SUFF-1n1 SUFF-1n2 ... SUFF1n6))
;	     CODE-2 ( ...) ...))
;     where SUFF-ijk are the suffixes for:
;		j=a indicative present
;		j=b indicative imperfect
;		j=c past (passato remoto)
;		j=d future
;		j=e conjunctive present
;		j=f conjunctive past
;		j=g conditional
;		j=h present participle
;		j=i past participle
;		j=l gerund
;		j=m infinite
;		j=n imperative
;        for j=a,g there are six entries for each sublist: 1st, 2nd and 3rd
;            person singular and 1st, 2nd and 3rd person plural;
;	 for j=h,i there are four entries for sublist (m sing, m pl, f sing, f pl)
;        for j=l,m there is just a single entry
;        for j=n (imperative) there are six entries, but the first is always nil)
; ****************************************************************************
; *** the output is written on the file KB/MORPHO/suff-tab.dat and has the
;     form:
;     (suffix-1 (entry-11 entry-12 ... entry-1n1)
;      suffix-2 (entry-21 entry-22 ... entry-2n2)
;      ...
;      suffix-m (entry-m1 entry-m2 ... entry-mnm))
;     where each entry-ij is the specification of a form:
;     (CATEG CODE features)
;     For example, the entry for the Italian suffix "IO" (passagg-io):
;      IO				+++ suffix-i
;        (ADJ				+++ CATEG
;          (3 (GENDER M NUMBER SING)	+++ CODE-i-1 and features-i-1 for ADJ
;           6 (GENDER M NUMBER SING))	+++ CODE-i-2 and features-i-2 for ADJ
;         NOUN
;          (6 (GENDER M NUMBER SING)	+++ CODE-i-1 and features-i-1 for NOUN
;           37 (GENDER M NUMBER SING))	+++ CODE-i-2 and features-i-2 for NOUN
;         VERB
;          (5 (A (1))			+++ CODE-i-1 and features-i-1 for VERB
;           11 (A (1)))			+++ CODE-i-2 and features-i-2 for VERB
;        )
;
; *** The structure is of the precedure is:
;	  compile-suff
;      |--> int-comp-suff
;      |	|--> adj-noun-expand-suff
;      |	|	 |--> expand-one-code
;      |	|	    	 |--> expand-one-suff
;      |	|	    	  	   |--> int-exp-one-suff
;      |	|	    	  	    	 |--> exp-suff-in-table
;      |	|	    	  	    	  	   |--> exp-categ-in-table
;      |	|	    	  	    	  	    	 |--> exp-feat-in-table
;      |	|	    	  	    	  	    	  	   |--> merge-features
;      |	|	    	  	    	  	    	  	   |--> exp-feat-in-tense
;      |	|--> verb-expand-suff
;      |	 	 |--> verb-expand-one-code
;      |	 	         |--> expand-one-code**
;      |	 	         |--> expand-standard-tense
;      |	 	    	 	|--> expand-persnumb
;      |	 	    	 	 	 |--> int-exp-persnumb
;      |	 	    	 	   	 	 |--> exp-persnumb-in-table
;      |	 	    	 	    	   		 |--> exp-persnumb-in-verb
;      |	 	    	 	   	 	 	 	 |--> exp-persnumb-in-code
;      |--> write-suff-table
;
; ****************************************************************************
; *** this simply initializes for recursion the two arguments
(defun compile-suff ()
  (declare (special *SUFFIX-TABLE* *SUFF-TAB-DESCR*))
  (let (all-suff pn-codes)
  ; *** pn-codes is the list of person-number codes (e.g. 1 2 3 4 5 6)
  ;     they are initialized in int-comp-suff and used in write-suff-table
     (declare (special pn-codes))
      (setq all-suff (int-comp-suff *SUFFIX-TABLE* *SUFF-TAB-DESCR* nil))
      (write-suff-table all-suff)
      (build-network all-suff)))

; ****************************************************************************
; *** according to the category to which the first remaining entry of the table
;     refers, the result is expanded via 'adj-noun-expand-suff' (for adjectives and
;     nouns) or via 'verb-expand-suff' (for verbs)
(defun int-comp-suff (inp-table tab-scheme output-table)
   (cond ((null inp-table) output-table)
         (t (let* ((key (first inp-table)) 
                   (infos (second inp-table)) 
                   (descr (rest (assoc key tab-scheme))))
		(cond ((or (eq key 'adj) (eq key 'noun))
			 (int-comp-suff (rest (rest inp-table)) tab-scheme
				   (adj-noun-expand-suff key descr output-table infos)))
		      ((eq key 'adv)
		         (int-comp-suff (rest (rest inp-table)) tab-scheme
				   (adv-expand-suff output-table infos)))
		      ((eq key 'verb)
                         (int-comp-suff (rest (rest inp-table)) tab-scheme
				   (verb-expand-suff descr output-table infos)))
                      (t (break "Unknown category in int-comp-suff")))))))

; ****************************************************************************
; *** handles a single suffix code and recurs
;     INPUT:
;   >>> categ; either "adj" or "noun"
;   >>> descr: the "schema" description of the data
;              It includes two entries:
;              (('case' case-list)
;               ('gender-number' gender-number-list))
;              - case-list is either a list of case names (e.g. nominative, oblique, ...)
;                          or NIL, in case no case is defined in the language
;                In case case-list is nil, then a single line must occur in the suffix
;                table, otherwise, the first line defines the suffixes for the first
;                case, and so on.
;              - gender-number-list is a list of pairs. Each pair is associated with
;                a column of the suffix table, and specifies the gender and number
;                of the word, when it has the given suffix.
;                Example: 
;                        ((m sing)   (m pl)  ... (f pl))  
;                            |          |           |
;                            V          V           V
;	         (CODE-1 ((SUFF-111 SUFF-121 ... SUFF-1K1)  --> case1
;                          ...
;	                  (SUFF-11N SUFF-12N ... SUFF-1KN)) --> caseN
;		  CODE-2 ((SUFF-211 SUFF-221 ... SUFF-2K1)  --> case1
;                          ...
;	                  (SUFF-21N SUFF-22N ... SUFF-2KN)) --> caseN
;                 ... ) ...)
;              where CODE-i are integers, and SUFF-ijk are the actual suffixes
;   >>> suffixes: the accumulator of results used in the recursion
;   >>> infos: the piece of suffix table associated with that category, i.e.
(defun adj-noun-expand-suff (categ descr suffixes info)
   (cond ((null info) suffixes)
	 (t (let ((all-cases (second (assoc 'cases descr)))	; *** the list of cases
	          (gender-number (second (assoc 'gender-number descr)))
                                    ; *** the list of gender-number pairs
                  (all-genders (second (assoc 'genders descr)))
                  (all-numbers (second (assoc 'numbers descr)))
                  (next-code (first info))		; *** the next CODE-i
                  (next-suffdef (second info))	       
                                    ; *** the set of suffixes associated with CODE-i
                  (rem-table (rest (rest info))))
                                    ; *** the definition of the codes from CODE-i+1 on
              (declare (special all-cases all-genders all-numbers))
                (adj-noun-expand-suff 
                   categ descr
		   (expand-one-code categ suffixes all-cases gender-number next-code next-suffdef)
		   rem-table)))))

; ****************************************************************************
;; *** handles a single suffix code for adverbs and recurs
(defun adv-expand-suff (suffixes info)
  (cond ((null info) suffixes)
	(t (adv-expand-suff 
	 		(int-exp-one-suff 
			   'adv (first info) (first (second info)) suffixes 'no-case nil nil nil)
	 		(rest (rest info))))))

; ****************************************************************************
; *** handles a single suffix list
;     INPUT:
;   >>> categ: "adj", "noun", "adv" or "verb"
;   >>> prev-table: the accumulator of results
;   >>> all-cases: the list of syntactic cases (i.e. the number of rows - sublists -
;              of the code-suff table). Possibly nil; in this case, the code-suff
;              table must include a single row
;   >>> gender-number: the list of gender-number pairs (i.e. the number of
;              columns - elements of each sublist - of the code-suff table)
;   >>> code: an integer, i.e. the morphological class code
;   >>> code-suff: the actual set of suffixes associated with "code"
;   >>> tense: the tense (in case of verbs). This option depends on the fact that
;              this function is also used for participles
;     OUTPUT:
;   >>> the updated "prev-table"
(defun expand-one-code (categ prev-table cases gender-number morph-class code-suff &optional tense)
; *** the next cond to enable the execution of the next dolist once, in correspondence
;     with the single definition of the gender-number pairs
 (let (nxt-suff-tab-line)
  (cond ((null cases)
          (setq cases (list 'no-case))))
  (dolist (nxtcase cases prev-table)
; *** this loop, on all defined cases
     (setq nxt-suff-tab-line (first code-suff))
     (setq code-suff (rest code-suff))
     (setq prev-table
        (dolist (nxt-g-n gender-number prev-table)
; *** this loop, on all defined gender-number pairs
           (setq prev-table 
    ; *** expand-one-suff does the actual work on the single entry of the suff-table
	      (expand-one-suff 
                 categ morph-class nxtcase (first nxt-suff-tab-line) prev-table 
                  (first nxt-g-n) (second nxt-g-n) tense))
           (setq nxt-suff-tab-line (rest nxt-suff-tab-line)))))))

; ****************************************************************************
; *** handles a single suffix 
;     it must look for it in the output suffix table; if it already appears,
;     then the corresponding infos must be extended; otherwise a new entry
;     must be created
;     INPUT:
;   >>> categ: "adj", "noun", "adv" or "verb"
;   >>> code: an integer, i.e. the morphological class code
;   >>> case: the case name, or 'no-case if the language has no case system
;   >>> suff: a single suffix
;        actually, it can be a list in case multiple forms are allowed for the same
;        combination of features
;   >>> output-table: the accumulator of results
;   >>> gender: an atom, identifying the gender (usually, "m", "f", or "n")
;   >>> number: an atom, identifying the syntactic number (usually, "sing", or "pl")
;   >>> tense: the tense (in case of verbs). This option depends on the fact that
;              this function is also used for participles (usually "pres" or "past")
;     OUTPUT:
;   >>> the updated "output-table"
(defun expand-one-suff (categ code case suff prev-table gender number tense)
   (cond ((null suff) prev-table)
     ; *** null suffixes exist for invariables or for empty entries (e.g. the
     ;     feminine of a noun which is masculine)
         ((atom suff)
	    (int-exp-one-suff categ code suff prev-table case gender number tense))
         (t (let ((temp-table prev-table))
               (dolist (nxtsuff suff temp-table)
	          (setq temp-table
                     (int-exp-one-suff categ code nxtsuff temp-table case gender number tense)))))))

; ****************************************************************************
; *** as above, but null entries have been filtered
(defun int-exp-one-suff (categ code suff prev-table case gender number tense)
   (cond ((null prev-table)
    ; *** prev-table is empty: add new entry
    ;     For ex.:  (IO (ADJ (3 (GENDER M NUMBER SING))))
    ;     This happens if this is the first occurrence of te 'IO' suffix
	   (cond ((or (eq categ 'adj) (eq categ 'noun))
		; *** for adjectives and nouns
                   (cond ((is-a-no-case-language case)
		     ; *** if there is no case, the entry includes only the data about
                     ;     gender and number
		            (list suff (list categ 
			           (list code (list 'gender gender 'number number)))))
                         (t
		     ; *** otherwise, add all data
		            (list suff (list categ 
			           (list code (list 'case case 'gender gender 'number number)))))))
		 ((eq categ 'adv)
	   ; *** for adverbs
		   (list suff (list categ (list code))))
		 ((eq categ 'verb)
	           (cond ((null gender)
	      ; *** for the gerunds and infinites of verbs
	                    (list suff (list categ (list code (list tense)))))
	   ; *** for the participles of verbs
                         ((is-a-no-case-language case)
	                    (list suff (list categ (list code (list tense
					       (list 'gender gender 'number number))))))
                         (t (list suff (list categ (list code (list tense
				       (list 'case case 'gender gender 'number number))))))))
                 (t (break "unknown category in int-exp-one-suff"))))
	 ((eq (first prev-table) suff)
    ; *** the first element is the suffix looked for: expand the second element
    ;     For ex.:  The entry for 'IO' already exists. The second of prev-table could be:
    ;                 (ADJ (3 (GENDER M NUMBER SING))
    ;                  NOUN (6 (GENDER M NUMBER SING)))
	   (cons suff 
	     (cons (exp-suff-in-table 
			categ code (second prev-table) case gender number tense)
		   (rest (rest prev-table)))))
    ; *** otherwise, recursion: go on with the search in the previous table
	 (t (cons (first prev-table)
	    	  (cons (second prev-table)
		            (int-exp-one-suff 
				categ code suff
				(rest (rest prev-table))
				case gender number tense))))))

; ****************************************************************************
; *** that particular suffix does exist; expand its entry
(defun exp-suff-in-table (categ code suff-prev-table case gender number tense)
   (cond ((null suff-prev-table)
	; *** the category does not exist: add the new one
	   (cond ((or (eq categ 'adj) (eq categ 'noun))
	   ; *** for adjectives and nouns
                   (cond ((eq case 'no-case)
		     ; *** if there is no case, the entry includes only the data about
                     ;     gender and number
		            (list categ (list code (list 'gender gender 'number number))))
                         (t
		     ; *** otherwise, add all data
		            (list categ 
			       (list code (list 'case case 'gender gender 'number number))))))
		 ((eq categ 'adv)
	   ; *** for adverbs
		   (list categ (list code)))
		 ((eq categ 'verb)
		    (cond ((null gender)
	   ; *** for the gerunds and infinites of verbs
		             (list categ (list code (list tense))))
	   ; *** for the participles of verbs
                          ((is-a-no-case-language case)
                            (list categ
                                (list code
                                   (list tense `(gender ,gender number ,number)))))
                          (t (list categ
                                (list code
                                   (list tense `(case ,case gender ,gender number ,number)))))))
                 (t (break "unknown category in exp-suff-in-table"))))
         ((eq categ (first suff-prev-table))
    ; *** the first element is the categ looked for: expand the second element
    ;     For ex.:  In the entry for 'IO', the ADJ category already exists. The
    ;               second of suff-prev-table could be:
    ;                 (3 (GENDER M NUMBER SING))
	    (cons categ 
		 (cons (exp-categ-in-table 
			    categ code (second suff-prev-table) case gender number tense)
		       (rest (rest suff-prev-table)))))
    ; *** otherwise, go on with the search in the previous table of that categ
	 (t (cons (first suff-prev-table)
	    	  (cons (second suff-prev-table)
		        (exp-suff-in-table
				categ code
			        (rest (rest suff-prev-table))
				case gender number tense))))))

; ****************************************************************************
; *** that particular category exists for that suffix; expand its entry
(defun exp-categ-in-table (categ morph-class categ-prev-table case gender number tense)
  (cond ((null categ-prev-table)
	; *** the morph-class does not exist: add a new entry
	   (cond ((or (eq categ 'adj) (eq categ 'noun))
	   ; *** for adjectives and nouns
                   (cond ((is-a-no-case-language case)
                            (list morph-class `(gender ,gender number ,number)))
                         (t (list morph-class `(case ,case gender ,gender number ,number)))))
		 ((eq categ 'adv)
	   ; *** for adverbs
		   (list morph-class))
		 ((eq categ 'verb)
		   (cond ((null gender)
	       ; *** for the gerunds and infinites of verbs
		            (list morph-class (list tense)))
	       ; *** for the participles of verbs
                         ((is-a-no-case-language case)
                            (list morph-class 
                                 (list tense `(gender ,gender number ,number))))
                         (t (list morph-class 
                                 (list tense `(case ,case gender ,gender number ,number))))))
                 (t (break "unknown category in exp-categ-in-table"))))
        ((eq morph-class (first categ-prev-table))
    ; *** the first element is the morph-class looked for: expand the second element
	   (cons morph-class 
	     (cons (exp-feat-in-table (second categ-prev-table) case gender number tense)
		   (rest (rest categ-prev-table)))))
    ; *** otherwise, go on with the search in the previous table of that morph-class
	 (t (cons (first categ-prev-table)
	    	  (cons (second categ-prev-table)
		        (exp-categ-in-table
				categ morph-class (rest (rest categ-prev-table))
				case gender number tense))))))

; ****************************************************************************
; *** that particular code exists for that suffix; merge the features
(defun exp-feat-in-table (feat-prev-table case gender number tense)
  (cond ((null tense)
	  (merge-features feat-prev-table case gender number))
	(t (exp-feat-in-tense feat-prev-table case gender number tense))))

; ****************************************************************************
; *** this is a merge, because the previous entry is (genderX numberY), and
;     the new entry is (genderZ numberW); if either genderX=genderZ or
;     numberY=numberW, the equal entries must be substituted by ALLVAL
(defun merge-features (feat-prev-table case gender number)
  (declare (special all-cases all-genders all-numbers))
   (let ((expanded-table (expand-feat-table feat-prev-table))
         same-val)
      (setq expanded-table 
         (cond ((is-a-no-case-language case)
                  (cons `(gender ,gender number ,number) expanded-table))
               (t (cons `(case ,case gender ,gender number ,number) expanded-table))))
; *** first check number
      (cond ((is-a-no-case-language case)
               (dolist (nxtgend all-genders)
                    (setq same-val 
                          (search-tab-val 
                                expanded-table 'number '(gender) (list nxtgend)))
                    (cond ((subset all-numbers same-val)
                             (setq expanded-table 
                                (put-allval expanded-table 
                                            'number '(gender) (list nxtgend))))))
    ; now, expanded-table has the form:
    ;    ((gender m number x1) (gender f number x2) (gender n number x3))
    ; where x1, x2 and x3 are either sing or pl or allval, and any of the four
    ;    n-tuples can be missing
               (dolist (nxtnumb (cons 'allval all-numbers))
                    (setq same-val 
                          (search-tab-val 
                                expanded-table 'gender '(number) (list nxtnumb)))
                    (cond ((subset all-genders same-val)
                             (setq expanded-table 
                                (put-allval expanded-table 
                                            'gender '(number) (list nxtnumb)))))))
            (t (dolist (nxtcase all-cases)
                 (dolist (nxtgend all-genders)
                    (setq same-val 
                          (search-tab-val 
                                expanded-table 'number '(case gender) (list nxtcase nxtgend)))
                    (cond ((subset all-numbers same-val)
                             (setq expanded-table 
                                (put-allval expanded-table 
                                            'number '(case gender)  (list nxtcase nxtgend)))))))
               (dolist (nxtcase all-cases)
                 (dolist (nxtnumb (cons 'allval all-numbers))
                    (setq same-val 
                          (search-tab-val 
                                expanded-table 'gender '(case number) (list nxtcase nxtnumb)))
                    (cond ((subset all-genders same-val)
                             (setq expanded-table 
                                (put-allval expanded-table 
                                            'gender '(case number)  (list nxtcase nxtnumb)))))))
               (dolist (nxtgend (cons 'allval all-genders))
                 (dolist (nxtnumb (cons 'allval all-numbers))
                    (setq same-val 
                          (search-tab-val 
                                expanded-table 'case '(gender number) (list nxtgend nxtnumb)))
                    (cond ((subset all-cases same-val)
                             (setq expanded-table 
                                (put-allval expanded-table 
                                            'case '(gender number)  (list nxtgend nxtnumb)))))))))
    ; *** now, to each <case gender> pair, one or more of the possible gender values, or
    ;     allval is associated
        (cond ((= 1 (length expanded-table))
                 (first expanded-table))
              (t expanded-table))))

; ****************************************************************************
(defun is-a-no-case-language (case)
  (or (null case)
      (eq case 'no-case)
      (equal case '(no-case))))

; ****************************************************************************
; *** this expands a feature table, by substituting the possible "allval" values
(defun expand-feat-table (prev-table)
  (cond ((null prev-table) nil)
        ((atom (first prev-table))
         ;  (break "expand-feat-table")
           ; *** this should only happen in the old version without case infos
           (single-e-feat-tab prev-table))
        (t (append (single-e-feat-tab (first prev-table))
                   (expand-feat-table (rest prev-table))))))

; ****************************************************************************
; *** extracts from "table" the values of "feat-to-find", taking them from the
;     rows such that "search-feat" = "search-val" (for all features in the list
;     search-feat (see next function)
; *** table = ((case direct gender f number sing)
;              (case direct gender m number sing)
;              (case direct gender m number pl)
;              (case oblique gender f number sing))
; *** search-feat (case number)
; *** search-val (direct sing)
; *** result:  (f m)
(defun search-tab-val (table feat-to-find search-feat search-val)
  (cond ((null table) nil)
        ((ok-search-tab-val (first table) search-feat search-val)
           (cons (prendi (first table) feat-to-find)
                 (search-tab-val (rest table) feat-to-find search-feat search-val)))
        (t (search-tab-val (rest table) feat-to-find search-feat search-val))))

; ****************************************************************************
; *** search-feat is a list of features (e.g. case number)
; *** search-val is a listof feature values (e.g. direct sing)
; *** It returns true if in this entry of the suffix table all features in
;     search-feat have the corresponding value in search-val
;     A table-entry is something as (case direct gender f number sing)
(defun ok-search-tab-val (table-entry search-feat search-val)
   (let ((result t))
      (do ((nxtfeat (first search-feat) (first search-feat))
           (search-feat (rest search-feat) (rest search-feat))
           (nxtval (first search-val) (first search-val))
           (search-val (rest search-val) (rest search-val)))
          ((or (null nxtfeat)
               (not result))
              result)
          (cond ((neq (prendi table-entry nxtfeat) nxtval) 
                   (setq result nil))))))

; ****************************************************************************
; *** puts in all rows of "table" where "search-feat" = "search-val" the value
;     of "feat-to-find" to "allval"
(defun put-allval (table feat-to-find search-feat search-val)
  (cons (cons feat-to-find (cons 'allval (make-pair-list search-feat search-val)))
        (rem-feat-vals table search-feat search-val)))

; ****************************************************************************
(defun make-pair-list (list1 list2)
  (cond ((null list1) nil)
        (t (append (list (first list1) (first list2))
                   (make-pair-list (rest list1) (rest list2))))))

; ****************************************************************************
(defun rem-feat-vals (table search-feat search-val)
  (cond ((null table) nil)
        ((ok-search-tab-val (first table) search-feat search-val)
          (rem-feat-vals (rest table) search-feat search-val))
        (t (cons (first table) (rem-feat-vals (rest table) search-feat search-val)))))

; ****************************************************************************
; *** this explodes an entry, in case it contains some 'allval' values, into
;     all possible values for the associated feature
; *** note that prev-pair could in principle be a triple, in case the syntactic
;     case is present
(defun single-e-feat-tab (prev-pair)
 (declare (special all-cases all-genders all-numbers))
 (let ((case (cond ((and (not (null all-cases))
                         (not (equal all-cases '(no-case))))
                      (prendi prev-pair 'case))
                  (t nil)))
       (number (prendi prev-pair 'number))
       (gender (prendi prev-pair 'gender))
       result)
  (cond ((null all-cases)
           (cond ((and (eq gender 'allval)
                       (neq number 'allval))
               ; *** explode the gender, keeping the number fixed
               ;     Ex:
               ;         prev-pair = (gender allval number sing)
               ;         all-genders = (m f other)
               ;         result = ((gender m number sing)
               ;                   (gender f number sing)
               ;                   (gender other number sing))
                   (do ((nxtgend (first all-genders) (first all-genders))
                        (all-genders (rest all-genders) (rest all-genders)))
                       ((null nxtgend) result)
                       (setq result (append1 result `(gender ,nxtgend number ,number)))))
                 ((and (neq gender 'allval)
                       (eq number 'allval))
               ; *** similarly for number, with gender fixed
                   (do ((nxtnumb (first all-numbers) (first all-numbers))
                        (all-numbers (rest all-numbers) (rest all-numbers)))
                       ((null nxtnumb) result)
                       (setq result (append1 result `(gender ,gender number ,nxtnumb)))))
                 ((and (neq gender 'allval)
                       (neq number 'allval))
               ; *** no allval, nothing to expand
                    (list prev-pair))
               ; *** the table should not contain already "allval" for both gender and number
               ;     since this means that there is duplicate information in the input
                 (t (break "merge-features 2"))))
        (t
    ; *** this is a language with case marking
           (cond ((and (eq case 'allval)
                       (neq gender 'allval)
                       (neq number 'allval))
               ; *** explode the case, keeping the gender and number fixed
               ;     Ex:
               ;         prev-pair = (case allval gender m number sing)
               ;         all-cases = (nom acc dat)
               ;         result = ((case nom gender m number sing) 
               ;                   (case acc gender f number sing) 
               ;                   (case dat gender other number sing)) 
                   (do ((nxtcase (first all-cases) (first all-cases))
                        (all-cases (rest all-cases) (rest all-cases)))
                       ((null nxtcase) result)
                       (setq result (append1 result `(case ,nxtcase gender ,gender number ,number)))))
                 ((and (neq case 'allval)
                       (eq gender 'allval)
                       (neq number 'allval))
               ; *** explode the gender, keeping the case and number fixed
                   (do ((nxtgend (first all-genders) (first all-genders))
                        (all-genders (rest all-genders) (rest all-genders)))
                       ((null nxtgend) result)
                       (setq result (append1 result `(case ,case gender ,nxtgend number ,number)))))
                 ((and (neq case 'allval)
                       (neq gender 'allval)
                       (eq number 'allval))
               ; *** similarly for number, with case and gender fixed
                   (do ((nxtnumb (first all-numbers) (first all-numbers))
                        (all-numbers (rest all-numbers) (rest all-numbers)))
                       ((null nxtnumb) result)
                       (setq result (append1 result `(case ,case gender ,gender number ,nxtnumb)))))
                 ((and (eq case 'allval)
                       (eq gender 'allval)
                       (neq number 'allval))
               ; *** if two items are allval, all combinations must be generated;
               ;     here, all pairs of case and gender, with number fixed
                   (do ((nxtcase (first all-cases) (first all-cases))
                        (all-cases (rest all-cases) (rest all-cases)))
                       ((null nxtcase) result)
                       (do ((nxtgend (first all-genders) (first sav-genders))
                            (sav-genders (rest all-genders) (rest sav-genders)))
                           ((null nxtgend))
                           (setq result 
                              (append1 result `(case ,nxtcase gender ,nxtgend number ,number))))))
                 ((and (eq case 'allval)
                       (eq gender 'allval)
                       (neq number 'allval))
               ; *** as above, for generating all pairs of case and number, with gender fixed
                   (do ((nxtcase (first all-cases) (first all-cases))
                        (all-cases (rest all-cases) (rest all-cases)))
                       ((null nxtcase) result)
                       (do ((nxtnumb (first all-numbers) (first sav-numbers))
                            (sav-numbers (rest all-numbers) (rest sav-numbers)))
                           ((null nxtnumb))
                           (setq result 
                              (append1 result `(case ,nxtcase gender ,gender number ,nxtnumb))))))
                 ((and (neq case 'allval)
                       (eq gender 'allval)
                       (eq number 'allval))
               ; *** as above, for generating all pairs of gender and number, with case fixed
                   (do ((nxtgend (first all-genders) (first all-genders))
                        (all-genders (rest all-genders) (rest all-genders)))
                       ((null nxtgend) result)
                       (do ((nxtnumb (first all-numbers) (first sav-numbers))
                            (sav-numbers (rest all-numbers) (rest sav-numbers)))
                           ((null nxtnumb))
                           (setq result 
                              (append1 result `(case ,case gender ,nxtgend number ,nxtnumb))))))
                 ((and (neq case 'allval)
                       (neq gender 'allval)
                       (neq number 'allval))
               ; *** no allval, nothing to expand
                    (list prev-pair))
               ; *** the table should not contain already "allval" for case, gender and number
               ;     since this means that there is duplicate information in the input
                 (t (break "merge-features 3")))))))

; ****************************************************************************
; *** this is used for participles, gerunds and infinites
(defun exp-feat-in-tense (prev-table case gender number tense)
  (cond ((null prev-table)
    ; *** the tense is not already present for that verb: add it
	; *** for gerunds and infinites
	   (cond ((null gender) (list tense))
	; *** for participles
                 ((is-a-no-case-language case)
                     (list tense (list 'gender gender 'number number)))
		 (t (list tense (list 'case case 'gender gender 'number number)))))
	((eq (first prev-table) tense)
    ; *** the first element is the entry for that tense: expand it
	  (cons tense 
	     (cons (merge-features (second prev-table) case gender number)
		   (rest (rest prev-table)))))
    ; *** otherwise, go on with the search in the table
	(t (cons (first prev-table)
	         (cons (second prev-table)
		       (exp-feat-in-tense
				(rest (rest prev-table)) case gender number tense))))))

; ****************************************************************************
; *** extracts the general info about the verbal tables and calls "int-verb-exp-suff"
;     INPUT:
;   >>> descr: the whole sub-table description associated with verbal suffixes
;   >>> output-table: the final result, that must be updated with verbal infos
;   >>> info: the actual data, i.e. the suffixes for the various verbal classes
(defun verb-expand-suff (descr output-table info)
	   (let ((all-cases (second (assoc 'cases descr)))
         (all-genders (second (assoc 'genders descr)))
         (all-numbers (second (assoc 'numbers descr)))
         (all-persons (second (assoc 'persons descr)))
         (all-m-t-defs (rest (assoc 'moods-tenses-def descr))))
  ; *** see below for the description of the variables above
     (declare (special all-cases all-genders all-numbers all-persons))
     (int-verb-exp-suff output-table all-cases all-genders all-numbers all-persons all-m-t-defs info)))

; ****************************************************************************
; *** handles a single suffix code for a verb and recurs
;     INPUT:
;   >>> output-table: the final result, that must be updated with verbal infos
;   >>> all-cases: the list of allowed case names, or nil if the language has no case system
;   >>> all-genders: the list of allowed genders (usually '(m f) or '(m f n))
;   >>> all-numbers: the list of allowed numbers (usually '(sing pl))
;   >>> all-persons: the list of allowed persons (usually '(1 2 3))
;   >>> all-m-t-defs: the list of infos associated with the various tense-mood codes
;       an example is:
;           ((a (ind pres) (person number)
;               ((1 sing) (2 sing) (3 sing) (1 pl) (2 pl) (3 pl)))
;            (b (ind imperf) (person number)
;               ((1 sing) (2 sing) (3 sing) (1 pl) (2 pl) (3 pl)))
;              ...
;           )
;       This says that (for Italian) the infos associated with the mood-tense code "a"
;       refers to the mood "ind" (indicative) and to the tense "pres" (present)
;       It also says that the single entries refer to the suffix associated with the
;       various combinations of "person" and "mood". Since, in Italian, there are
;       three persons (1, 2, 3) and two numbers ("sing" and "pl"), then there should
;       be (in the suffix definitions) six entries, which are associated, in this order,
;       to the entries (1 sing), (2 sing), ...
;       Similarly for the other mood-tense codes
;   >>> info: the actual data, i.e. the suffixes for the various verbal classes
;       For instance:
;           (1 ((o     i     a     iamo    ate    ano)
;               (avo   avi   ava   avamo   avate  avano)
;                ...)
;            2 ((o     i     e     iamo    ete    ono) 
;               (evo   evi   eva   evamo   evate  evano)
;                ...)
;           )
(defun int-verb-exp-suff (output-table all-cases all-genders all-numbers all-persons all-m-t-defs info)
   (cond ((null info) output-table)
	 (t (let ((next-class (first info))      ; *** the next CODE-i (ex. 1, 2, ...)
                  (next-suffdef (second info))   ; *** the set of suffixes associated with CODE-i, e.g.
                                                 ;     ((o     i     a     iamo    ate    ano)
                                                 ;      (avo   avi   ava   avamo   avate  avano)
                                                 ;        ...)
                  (rem-table (rest (rest info))) ; *** the definition of the codes from CODE-i+1 on
    ; *** It is assumed that the structure of the data in next-suffdef matches all-m-t-defs
                  new-out-table)
               (setq new-out-table
		   (verb-expand-one-class 
                        output-table all-cases all-genders all-numbers all-persons all-m-t-defs 
                        next-class next-suffdef))
    ; *** continue the work on the remaining verbal classes (rem-table)
               (int-verb-exp-suff new-out-table
                        all-cases all-genders all-numbers all-persons all-m-t-defs rem-table)))))

; ****************************************************************************
; *** handles a single morphological class for verbs
;     The description of that class (contained in "next-suff-defs") must match the
;     structure contained in "all-m-t-defs". The morphological class code
;     is given in "curr-class")
;     For instance, next-suff-defs can be:
;      ((o     hi    a     hiamo   ate    ano)            ; ind.pres.
;       (avo   avi   ava   avamo   avate  avano)          ; ind.imperf.
;        ...
;       (are)                                             ; (m) infinite
;       (nil   a     hi    hiamo   ate    hino))          ; (n) imperative
; *** See the function above, for the definition of the input parameters
(defun verb-expand-one-class (output-table all-cases all-genders all-numbers all-persons
                             all-m-t-defs curr-class next-suff-defs)
 (let (next-suff-data)
  (dolist (next-mt all-m-t-defs output-table)
   ; *** next-mt is the definition of a single mood-tense code, e.g.
   ;           (a (ind pres) (person number)
   ;              ((1 sing) (2 sing) (3 sing) (1 pl) (2 pl) (3 pl)))
     (setq next-suff-data (first next-suff-defs))
     (setq next-suff-defs (rest next-suff-defs))
   ; *** next-suff-data are the actual data about that mood-tense code, e.g.
   ;           (o     i     a     iamo    ate    ano)
     (setq output-table
          (expand-mood-tense output-table curr-class next-mt next-suff-data 
                             all-cases all-genders all-numbers all-persons)))))
 
; ****************************************************************************
(defun expand-mood-tense (output-table curr-class next-mt next-suff-data 
                             all-cases all-genders all-numbers all-persons)
; *** The input is in next-suff-data, which is one line of the verb table, as:
;      (o     hi    a     hiamo   ate    ano)
; *** The expected result is as shown in the following example
;     (HI
;       (VERB
;          ( 4 (A (2) E (1 2 3) N (3))
;     ) )  )
;     Here, it is said that the suffix "HI" is the one used in the fourth morphological
;     class of verbs, in the indicative present (A), second person singular (2); it is
;     also used for subjunctive present (E), for first, second and third person singular
;     (1 2 3), and finally for imperative (N), third person singular (3)
;     A verb of the fourth class is "caricare" - load - (you "caric-hi", ...)
; *** for participles, the output format is different:
;     (ATO
;       (VERB
;          ( 4 (I (GENDER M NUMBER SING))
;     ) )  )
;     Here, I've shown again an example of fourth class ("caric-ato" - loaded m sing)
;     where the mood-tense code is "I" (participle past). In this case the info is
;     explicit, in order to keep homogeneity with nouns and adjectives
  (declare (special pn-codes))	; ********* pn-codes introduced in compile-suff
                                ;     they are special since they must be used also in
                                ;     write-suff-table
  (let ((next-mt-code (first next-mt))		; *** ex. "a"
       ; (next-mt-vals (second next-mt))		; *** ex. (ind pres)
        (next-mt-feat (third next-mt))		; *** ex. (person number)
        (next-mt-defs (fourth next-mt))		; *** ex. ((1 sing) (2 sing) ... (3 pl))
        next-suff-case
        )
    (cond ((null next-mt-feat)
   ; *** this happens for gerund and infinites (e.g. in Italian and English)
             (expand-one-suff 'verb curr-class all-cases (first next-suff-data)
                                  output-table nil nil next-mt-code))
          ((equal next-mt-feat '(gender number))
             (expand-one-code 'verb output-table all-cases next-mt-defs 
                          curr-class next-suff-data next-mt-code))
   ; *** in case we are working on gender and number, the values must be stated explicitly
   ;     See the comments above
          (t 
   ; *** on the contrary, for person and number, the pair is specified by a numerical code
   ;     This was done to limit the dictionary size
            ; *** check all cases; if no case, then introduce a one-element list (no-case) to
            ;     execute the loop once
              (setq pn-codes (generate-int-list next-mt-defs 1))
              (cond ((null all-cases)
                      (setq all-cases (list 'no-case))))
              (dolist (nxtcase all-cases output-table)
            ; *** this loop, on all defined cases
                 (setq next-suff-case (first next-suff-data))
                 (setq next-suff-data (rest next-suff-data))
                 (dolist (persnumb-code pn-codes output-table)
    	             (cond ((not (null (first next-suff-case)))
     ; *** null suffixes exist for invariables or for empty entries (e.g. the
     ;     feminine of a noun which is masculine)
                              (setq output-table 
	                         (expand-persnumb output-table curr-class next-mt-code persnumb-code 
                                                  (first next-suff-case)))))
	             (setq next-suff-case (rest next-suff-case))))))))

; ****************************************************************************
; *** this generates a list of integers of length equal to the one of "list",
;     starting from "start"
; *** (generate-int-list '(a b c) 2) --> (2 3 4)
(defun generate-int-list (list start)
  (cond ((null list) nil)
        (t (cons start (generate-int-list (rest list) (1+ start))))))
  
; ****************************************************************************
; *** Here, it starts the update of the output table, which is inspected to find the proper
;     placement of the new infos
; *** this loops on all suffixes already present in the output final result
(defun expand-persnumb (output-table morph-class mt-code persnumb-code suff)
  (cond ((atom suff)
          (cond ((null output-table)
    ; *** output-table is empty: add new entry
    ;     This always happens if, at the end of the recursion, the suffix has not
    ;     been found, i.e. this is a new suffix
		   (list suff 
                         (list 'verb 
                               (list morph-class (list mt-code (list persnumb-code))))))
		((eq (first output-table) suff)
    ; *** the first element is the suffix looked for: expand the second element
    ;     i.e. the definition associated with that suffix
		   (cons suff 
		     (cons (exp-persnumb-in-table 
					morph-class (second output-table) mt-code persnumb-code)
			   (rest (rest output-table)))))
    ; *** otherwise, go on with the search in the previous table
		 (t (cons (first output-table)
		    	  (cons (second output-table)
		                (expand-persnumb 
				        (rest (rest output-table))
					morph-class mt-code persnumb-code suff))))))
; *** the suffix is not an atom; this happens in case the same mood-tense-person-number
;     combination admits different endings (for instance, in Italian, this happens for
;     two entries in verbal class 2)
	((= (length suff) 2)
          (setq output-table 
               (expand-persnumb output-table morph-class mt-code persnumb-code (first suff)))
	  (expand-persnumb output-table morph-class mt-code persnumb-code (second suff)))
        (t (break "In expand-persnumb"))))

; ****************************************************************************
; *** this loops on all categories already present for that suffix
(defun exp-persnumb-in-table (morph-class prev-table mt-code persnumb-code)
  (cond ((null prev-table)
    ; *** there is no verbal entry for this suffix: add it
	   (list 'verb (list morph-class (list mt-code (list persnumb-code)))))
	((eq (first prev-table) 'verb)
    ; *** the first element is the verbal entry for that suffix: expand it
	   (cons 'verb
		 (cons (exp-persnumb-in-verb
				morph-class (second prev-table) mt-code persnumb-code)
		       (rest (rest prev-table)))))
    ; *** otherwise, go on with the search in the table
	(t (cons (first prev-table)
	  	 (cons (second prev-table)
		       (exp-persnumb-in-table 
				morph-class (rest (rest prev-table)) mt-code persnumb-code))))))

; ****************************************************************************
; *** this loops on all entries for the verbal definition associated with that suffix
(defun exp-persnumb-in-verb (morph-class prev-table mt-code persnumb-code)
  (cond ((null prev-table)
    ; *** the code is not already present for that verb: add it
	  (list morph-class (list mt-code (list persnumb-code))))
	((eq (first prev-table) morph-class)
    ; *** the first element is the verbal entry for that code: expand it
	  (cons morph-class
	     (cons (exp-persnumb-in-morph-class (second prev-table) mt-code persnumb-code)
	           (rest (rest prev-table)))))
    ; *** otherwise, go on with the search in the table
	(t (cons (first prev-table)
	         (cons (second prev-table)
	               (exp-persnumb-in-verb
				morph-class (rest (rest prev-table)) mt-code persnumb-code))))))

; ****************************************************************************
; *** this loops on all data about that morphological class for verbs
(defun exp-persnumb-in-morph-class (prev-table mt-code persnumb-code)
  (cond ((null prev-table)
    ; *** the mood-tense code is not already present for that class: add it
	   (list mt-code (list persnumb-code)))
	((eq (first prev-table) mt-code)
    ; *** the first element is the entry for that mood-tense code: expand it
	   (cons mt-code
	         (cons (append1 (second prev-table) persnumb-code)
		       (rest (rest prev-table)))))
    ; *** otherwise, go on with the search in the table
	(t (cons (first prev-table)
                 (cond ((atom (second prev-table))
         ; *** this should happen when the sequence of codes is ( .... M)
         ;     or ( .... M N ( ... ))
		          (exp-persnumb-in-morph-class
			       (rest prev-table) mt-code persnumb-code))
                       (t (cons (second prev-table)
		             (exp-persnumb-in-morph-class
				(rest (rest prev-table)) mt-code persnumb-code))))))))

; ****************************************************************************
; *** this simply initializes for recursion the two arguments
(defun write-suff-table (inverse-suff-table)
 (declare (special *OUTPUT-TAB-FILE* pn-codes))
   ; *** *OUTPUT-TABLE* defined in the load file: TULE/SYSTEM/loadsuff
 (let ((outfile (build-file-name *OUTPUT-TAB-FILE*)))
  (with-open-file (outf outfile :direction :output :if-exists :overwrite
				:if-does-not-exist :create)
    (format outf "; *** This table associates with each suffix the categories, class codes, and features that it identifies~%")
    (format outf "( ~%")
	(do ((nextsuff (first inverse-suff-table) (first inverse-suff-table))
	     (nextinfo (second inverse-suff-table) (second inverse-suff-table))
	     (inverse-suff-table (rest (rest inverse-suff-table)) 
				  (rest (rest inverse-suff-table))))
        ((null nextsuff)
    		(format outf ") ~%"))
	   (format outf "~s ~%  ( ~%" nextsuff)
       (do ((nextcateg (first nextinfo) (first nextinfo))
			(nextcatinfo (second nextinfo) (second nextinfo))
		    (nextinfo (rest (rest nextinfo)) (rest (rest nextinfo))))
           ((null nextcateg)
    			(format outf "  ) ~%"))
	    (cond ((memq nextcateg '(adj noun verb))
		     (format outf "  ~a ~%    ( ~%" nextcateg)
       	  	     (do ((nextcode (first nextcatinfo) (first nextcatinfo))
			  (nextcodeinfo (second nextcatinfo) (second nextcatinfo))
		    	  (nextcatinfo (rest (rest nextcatinfo))
				       (rest (rest nextcatinfo))))
        		((null nextcode)
    			    (format outf "    ) ~%"))
               ; *** the next outputs the class code
			(format outf "     ~a " nextcode)
               ; *** in order to output the class infos, special treament is deserved
               ;     for verbs, in order to avoid the output lists of all-person-number
               ;     codes (i.e. (1 2 3 4 5 6))
                        (cond ((eq nextcateg 'verb)
                                (format outf "(")
                                (do ((nxtel (first nextcodeinfo) (first nextcodeinfo))
                                     (nextcodeinfo (rest nextcodeinfo) (rest nextcodeinfo)))
                                   ((and (null nxtel)
                                         (null nextcodeinfo))
                                      (format outf ")~%"))
			           (cond ((and (not (null nxtel))
                                               (not (equal nxtel pn-codes)))
                                          (format outf "~a " nxtel)))))
			      (t (format outf "~a ~%" nextcodeinfo)))))
		  ((eq nextcateg 'adv)
		     (format outf "  ~a ~%    (~a)~%" 
							nextcateg (first nextcatinfo)))
		  (t (break "PROC/MORPHO/generate-suff:write-suff-table"))))))))

; ****************************************************************************
; *** this generates the basic suffix network
(defun build-network (inverse-suff-table)
 (declare (special *NETWORK-FILE* last-state-numb))
   ; *** *NETWORK-FILE* defined in the load file: TULE/SYSTEM/loadsuff
 (let ((outfile (build-file-name *NETWORK-FILE*))
       curritem currstate)
  (setq last-state-numb 1)
  (with-open-file (outf outfile :direction :output :if-exists :overwrite
				:if-does-not-exist :create)
 ; *** curritem identifies the previous character. At the beginning, it is
 ;     set to |£ |
 ; *** currstate identifies the state of the automaton. At the beginning,
 ;     it is set to n1
	(do ((nextsuff (first inverse-suff-table) (first inverse-suff-table))
             (suffcount 1 (1+ suffcount))
	     (inverse-suff-table (rest (rest inverse-suff-table)) 
				  (rest (rest inverse-suff-table))))
        ((null nextsuff)
    	   (write-network outf))
 ; *** during the construction, we expand the property lists of the states
 ;     A state (as n1) must have the following structure:
 ;     n1 -- char --> (nX charlist final?)
 ;        where char is the name of the property (i.e. the name of the
 ;                   character enabling the transition)
 ;              nX is the name of the state to reach after the transition
 ;              charlist is the sequenc of characters associated that enabled
 ;                   to reach this state
 ;              final? is true if this is a final state
     (cond ((neq nextsuff '@empt)
       (let ((expl-suff (reverse (explode nextsuff)))
             nxtstate nxtinfo nxtitem prevstate previnfo
             currinfo newinfo)
       (setq curritem (implode (list #\£ #\space)))
       (setq prevstate nil)
       (setq previnfo nil)
       (setq currstate 'n1)
       (setq currinfo nil)
           (do ((nxtchar (first expl-suff) (first expl-suff))
                (expl-suff (rest expl-suff) (rest expl-suff)))
              ((null nxtchar)
      ; *** the previous state is a final state; update the property
                  (setq previnfo (get prevstate curritem))
      ; *** currstate is the state that had to be reached if the
      ;     suffix continued. If it has no following characters, it
      ;     was just created. But in this case, it must be replaced
      ;     with the "stop" next state
                  (cond ((null (get currstate 'nxtch))
                          ; (format t " Nextsuff: ~a; Currstate; ~a; Previnfo ~a~%"
                          ;           nextsuff currstate previnfo)
                          ; (cond ((eq 0 (mod suffcount 10))
                          ;           (break "STOP")
                          ;        ))
                           (setq newinfo '(stop t))
                           (setq last-state-numb (1- last-state-numb))
                           (putprop currstate nil 'nxtch)
                           (putprop currstate nil 'suff))
                        (t (setq newinfo (list (first previnfo) t))))
                  (putprop prevstate newinfo curritem))
              (setq nxtitem (implode (list #\£ nxtchar)))
      ; *** if nxtchar is #\s, nxtitem is |£s|
	      (setq nxtinfo (get currstate nxtitem))
      ; *** if the transition from nX via |£s| has already been defined,
      ;     then nxtinfo is <nY, chars-to-nY, final?>
              (cond ((null nxtinfo)
      ; *** otherwise, it is defined as <nNEW, chars-to-nX+newChar, nil>
      ;     assuming, for the time being, that this is not a final state
                       (setq nxtstate (generate-new-state))
                       (putprop nxtstate 
                                (cons nxtchar (get currstate 'suff))
                                'suff)
                       (setq nxtinfo (list nxtstate nil))
                       (putprop currstate nxtinfo nxtitem)
                       ;    (format t " Nextsuff: ~a; Nxtstate; ~a; Currstate ~a~%"
                       ;              nextsuff nxtstate currstate)
                       ;              (break "STOP 2")
                       (putprop currstate 
                                (append1 (get currstate 'nxtch) nxtchar)
                                'nxtch))
                    (t (setq nxtstate (first nxtinfo))
                       (cond ((eq nxtstate 'stop)
                                (setq nxtstate (generate-new-state))))
                       (putprop nxtstate 
                                (cons nxtchar (get currstate 'suff))
                                'suff)
                       (putprop currstate (list nxtstate (second nxtinfo))
                                           nxtitem)))
              (setq curritem nxtitem)
              (setq prevstate currstate)
              (setq currstate nxtstate)))))))))

; ****************************************************************************
(defun generate-new-state ()
   (declare (special last-state-numb))
   (setq last-state-numb (1+ last-state-numb))
   (concatl (list 'n last-state-numb)))

; ****************************************************************************
(defun write-network (outf)
  (declare (special last-state-numb))
  (let (nxtstate follchars nxtinfo final nxtitem prtstate currsuff)
    (format outf "(in-package \"USER\") ~% ~%")
    (format outf ";**********************************************************~%")
    (format outf "; *** The global variable *SUFF-NET* holds the suffix ATN network~%")
    (format outf "(setq *SUFF-NET* ~%")
    (format outf "  '(~%")
    (do ((nxtindex 1 (1+ nxtindex)))
        ((= nxtindex (1+ last-state-numb))
           (format outf "  )) ~%"))
        (setq prtstate (concatl (list #\n nxtindex)))
        (setq nxtstate (concatl (list 'n nxtindex)))
        (setq follchars (get nxtstate 'nxtch))
        (setq currsuff (get nxtstate 'suff))
        (format outf "    (~a                             ; *** state ~a: after ~a~%"
                prtstate prtstate (implode currsuff))
        (dolist (follc follchars)
           (setq nxtitem (concatl (list '#\£ follc)))
           (setq nxtinfo (get nxtstate nxtitem))
           (cond ((null nxtinfo)
                      (break "Problem with a char in write-network"))
                 (t (setq final (second nxtinfo))
                    (format outf "      (|~a| " follc)
                    (cond (final
                             (format outf "final ~%")
                             (format outf "        (setoutp '|~a|)~%"
                                          (implode (cons follc currsuff))))
                          (t (format outf "nil ~%")))
                    (format outf "        (nxt-state-lex '~a))~%"
                                 (first nxtinfo)))))
        (format outf "     )~%"))))

;(setq mytable
;  '(MENTE (ADV (1))
;     IAMENTE (ADV (3))
;     AMENTE (ADV (2))
;     EMENTE (ADV (4)) 
;     IO (ADJ
;           (3 (GENDER M NUMBER SING)
;            6 (GENDER M NUMBER SING)))
;     O (ADJ
;           (1 (GENDER M NUMBER SING)
;            4 (GENDER M NUMBER SING)
;            5 (GENDER M NUMBER SING)))
;  ))

;(setq *NETWORK-FILE* "network.pro")
;(break "build-network")

;(build-network mytable)

