(in-package "USER")

; ****************************************************************
;     DICTIONARIES
;****************************************************************
; *** This function loads the dictionaries.
;     ff: standard dictionary
;     fnpg: Proper names common to all languages (e.g. Beethoven)
;     fnph: Proper names related to HOPS (e.g. event titles or player names)
;     fsigla: Coded expressions (as "Prof.")
;     fsiggiur: Legal coded expressions (just for Italian)
;     ffunct: dictionary of function words (not for Italian)
(defun load_all (ff fsigla fsiggiur &optional ffunct fforeign)
 (declare (special *MAIN-DICT* *PROPER-DICT* *SIGLE-DICT* *LANGUAGE* *CHAR-SET-ID*))
  ;  *** *MAIN-DICT* *PROPER-DICT* *SIGLE-DICT* defined at the top level 
  ;      (nlpp, lan, tule, tocai, ..., according to *SYSTEM-CONTEXT*)
 (let (root sav-charsetid def)
  ; *** The internal format of dictionaries is ISO-8859-1 ***
  (setq sav-charsetid *CHAR-SET-ID*)
  (setq *CHAR-SET-ID* 'ISO-8859-1)
     ; *** standard dictionary ***
  (do ((entry (read ff nil #\Escape) (read ff nil #\Escape)))
      ((equal entry #\Escape))
     ; (setq root (dictionary-normalize (first entry)))
     ; (setq def (mapcar 
     ;                #'(lambda (x) (cons (dictionary-normalize (first x)) 
     ;                                    (test-dict-norm (rest x))))
     ;                (second entry)))
       (setq root (first entry))
       (setq def (second entry))
      (putprop root def *MAIN-DICT*))
  (cond ((not (null ffunct))
   ; *** dictionary of function words (if separate from general words)
          (do ((entry (read ffunct nil #\Escape) (read ffunct nil #\Escape)))
              ((equal entry #\Escape))
          ;    (setq root (dictionary-normalize (first entry)))
              (setq root (first entry))
              (putprop root (append (get root *MAIN-DICT*) (second entry)) *MAIN-DICT*))))
  (cond ((not (null fforeign))
   ; *** dictionary of words with alphabets non-ISO-8859-1 
          (do ((entry (read fforeign nil #\Escape) (read fforeign nil #\Escape)))
              ((equal entry #\Escape))
              (setq root (first entry))
              (putprop root (second entry) *MAIN-DICT*)
             ; (break "foreign word")
             )))
   ; *** coded expressions ***
  (do ((entry (read fsigla nil #\Escape) (read fsigla nil #\Escape)))
      ((equal entry #\Escape))
      (setq root (first entry))
  ; (format t "SIGLE: ~a; ~a~%" root entry)
  ;  (break "")
      (putprop root (second entry) *SIGLE-DICT*))
   ; *** legal coded expressions ***
  (cond ((not (null fsiggiur))
          (do ((entry (read fsiggiur nil #\Escape) (read fsiggiur nil #\Escape)))
              ((equal entry #\Escape))
              (setq root (first entry))
              (add-prop-val root *SIGLE-DICT* (second entry)))))
  (setq *CHAR-SET-ID* sav-charsetid)))

; *****************************************************************
(defun dictionary-normalize (item) 
  (base-uppercase
     (convert-currlisp-atom-or-string-to-base item)))

; *****************************************************************
(defun test-dict-norm (syndata) 
  (cond ((and (eq 'ordin (prendi syndata 'type))
              (not (null (prendi syndata 'value)))
              (not (numberp (prendi syndata 'value))))
           (subst-dict-feat syndata 'value 
                  (dictionary-normalize (prendi syndata 'value))))
        (t syndata)))

; *****************************************************************
(defun subst-dict-feat (syndata feat newval) 
  (cond ((null syndata)
           (exception-nothrow "Loading dictionary: normalization"))
        ((eq (first syndata) feat)
           (cons feat (cons newval (rest (rest syndata)))))
        (t (cons (first syndata) (subst-dict-feat (rest syndata) feat newval)))))

;(defun dictionary-normalize (item)
;  (declare (special *LANGUAGE* *NON-DICTIONARY-CHARS*))
;  (cond ((eq *LANGUAGE* 'hindi)
;           (first item))
;  ; *** what follows is a patch to handle utf-8 characters not included in ISO-8859-1
;  ; *** The character code is taken. There are three possible cases:
;  ;     1. It is in the ASCII standard set (code < 128) --> it is left unchanged
;  ;     2. It is not in the standard set, but it is in a set of accepted (printable)
;  ;        characters (that include only the stressed vowels, the c with cedilla, the n
;  ;        with tilde - this is the complement of *NON-DICTIONARY-CHARS* list in 
;  ;        MORPHO/char-funct) --> it is converted according to the chose encoding. For example, 
;  ;        À is read with code 192 (ISO-8859-1); if the chosen encoding (charset) is
;  ;         ISO-8859-1, then it is left unchanged, otherwise it is suitably converted
;  ;         (e.g. in UTF-8 as (195 129))
;  ;     3. It is in none of the sets above. Then it is left as it is, together with the
;  ;        following char. For instance Î is 206. It is assumed to be the first byte of
;  ;        the UTF-8 encoding of some greek letters (e.g. (206 177) is alpha). In this
;  ;        way, if the chosen encoding scheme is UTF-8, a greek word will be recognized;
;  ;        If the scheme is different, no word will correspond to it. Of course, this
;  ;        a patch, in the sense that Î cannot occur as such in a dictionary entry.
;        (t (let (result double tule-name
;                 (itemcodes (mapcar #'char-code (explode item))))
;               (do ((nxtcode (first itemcodes)
;                             (cond (double (second itemcodes))
;                                   (t (first itemcodes))))
;                    (itemcodes (rest itemcodes)
;                             (cond (double (rest (rest itemcodes)))
;                                   (t (rest itemcodes)))))
;                   ((null nxtcode) (implode (reverse result)))
;                 (cond ((< nxtcode 128)
;          ; *** in this case, the encoding is left unchanged and we pass to the next char
;                         (setq result (cons nxtcode result))
;                         (setq double nil))
;                       ((member nxtcode *NON-DICTIONARY-CHARS*)
;          ; *** in this case, the encoding is left unchanged and we use two chars
;                         (setq result (cons (first itemcodes) (cons nxtcode result)))
;                         (setq double t))
;          ; *** in this case, if the encoding is not the same of the dictionary
;          ;     (i.e. not ISO-8859-1), then obtain the actual code
;                       (t (setq tule-name (get-numcode-from-base-code nxtcode))
;                          (setq result (append (reverse tule-name) result))
;                          (setq double nil))))))))

; *****************************************************************
; *** loading invariable data entries (invariabili, prep_art, locutions, 
;     multi-word proper names)
;     "property" can be:
;     1. invardef
;     2. prepartdef
;     3. locutdef
;     4. mproperdef
;     "invlist" is a list of the form:
;     (entry1 data-for-entry-1 entry2 data-for-entry-2 ....)
;       each data-for-entry-i is a list of definitions. The add-more-property
;       below is used to account for the possibility that "entry-i" occurs more
;       than once (as could happen in case we have many domain-specific lexicons)
(defun loadinvar (property invlist)
  (do* ((inv invlist (rest (rest inv)))
        (nextinv (first inv) (first inv))
        (nextdata (second inv) (second inv)))
       ((null inv) 'ok)
     (add-more-property nextinv property nextdata)))

; ****************************************************************
; *** as above, but the second argument is not a list, but a file from
;     which the data are read
(defun f-loadinvar (property invfile)
  (with-open-file (iport invfile
                        :direction :input :if-does-not-exist :error)
     (do ((nextinv (read iport nil 'eof) (read iport nil 'eof))
          (nextdata (read iport nil 'eof) (read iport nil 'eof)))
         ((eq nextdata 'eof) 'ok)
        (add-more-property nextinv property nextdata))))

; ****************************************************************
; *** as above, but the data about the name are included in a list together
;     with the data, so a single read is required
(defun f-loadnames (property invfile)
  (with-open-file (iport invfile
                        :direction :input :if-does-not-exist :error)
     (do ((nextname (read iport nil 'eof) (read iport nil 'eof)))
         ((eq nextname 'eof) 'ok)
        (add-more-property (first nextname) property (second nextname)))))

; ****************************************************************
; *** add-more-property is similar to add-prop-val.
;     the difference is that newvalues is a list, which is appended,
;     not added, to the actual velue of "property"
(defun add-more-property (elem property newvalues)
  (putprop elem (append (get elem property) newvalues) property))

; ****************************************************************
;     VERBAL CLASSES AND LABEL HIERARCHY
; ****************************************************************

(defvar *LABELLIST* nil)
(setq *LABELLIST* '(top dependent))
; **************************************************************************
(defun load-hierarchy ()
; *** reads the data of the transformed hierarchy
;     For each node, loads its property list, which includes:
;     - loc-cf-def: the local case frame
;     - cf-def: the complete case frame (after inheritence)
;     - synonyms: possibly empty
;     - grclass-parents: the parent node(s) in the hierarchy
;     - grclass-deps: the dependent node(s) in the hierarchy
;     - transf-source: if a transformed node, from what transformation it
;       derives, and from what node
;     - transf-sons: what other nodes derive from this after transformation
;     - appl-cond: applicability conditions of the pattern
;     - order-cond: conditions on the order of the dependents
;     - dummy: t, if this is a dummy node (abstract)
; *** for the subcat classes which do not appear as such in the hierarchy,
;     since they are synonyms of actual treenodes (as INTRANS, which
;     correspond to SUBJ-VERBS), the only property is:
;     - syn-repr: its representative (e.g. SUBJ-VERBS) in the hierarchy
  (declare (special *VERB-HIER-FILE*))
   (with-open-file (iport *VERB-HIER-FILE*
                        :direction :input :if-does-not-exist :error)
    (let (nodename)
     (do ((nextnode (read iport nil 'eof) (read iport nil 'eof)))
	 ((eq nextnode 'eof) 'ok)
	 (setq nodename (first nextnode))
	 (setplist nodename nil)
	 (dolist (nextprop (rest nextnode))
; *** for synonyms, store the inverse pointers: from the class - e.g. INTRANS -
;     to the node where the data are stored - e.g. SUBJ-VERBS
	     (cond ((eq (first nextprop) 'synonyms) 
	     	      (dolist (syn (second nextprop))
; *** the next to cancel a property loaded in the non-hierarchy version
			   (putprop syn nil 'subcat-patterns)
		  	   (putprop syn nodename 'syn-repr))))
	     (putprop nodename (second nextprop) (first nextprop)))))))

; **************************************************************************
(defun load-labhier ()
  (declare (special *LAB-HIER-FILE* *LABELLIST*))
   (with-open-file (lport *LAB-HIER-FILE*
                        :direction :input :if-does-not-exist :error)
     (do ((nxtnode (read lport nil 'end) (read lport nil 'end)))
	 ((eq nxtnode 'end) 'ok)
	 (putprop (first nxtnode) (second nxtnode) 'more-spec-labels)
	 (setq *LABELLIST* (append (second nxtnode) *LABELLIST*))
	 (dolist (nxtdaughter (second nxtnode))
	   (add-prop-val nxtdaughter 'more-gen-labels (first nxtnode))))))

;**********************************************************************
; ****** GRAMMATICAL TYPES
;**********************************************************************
; ***  It loads the grammtypes from the various language-dependent files
;      grammtypes-lang.dat
(defun load-grammtypes (grtypes)
  (dolist (nxttype grtypes)
    (putprop (first nxttype)
            ; (mapcar #'dictionary-normalize (second nxttype))
             (second nxttype)
             'grtype)))

;**********************************************************************
; ****** NON-VERBAL PARSE RULES
;**********************************************************************
; ***  It loads the rules from the file 'parserules.dat"
;      The input format is described in that file. The output is the
;	attachment to the 'head category' of a list representing a tree,
;	which is stored in the property 'chunk-rules'.
;	The root of each subtree is a syntactic subtype (e.g. 'demons',
;	'qualif'). At the next level, we have the position information,
;	possibly augmented with extra infos, and at the next level a set of
;	pairs <conditions, arc-label>
; *** Ex.
;	ADJ
;        |
;	chunk-rules
;	 	|
;	 	|--demons-------|
;		|		|---after-------|
;		|				|-- <NOUN(agree), NBAR-DEF>
;		|
;		|--indef--------|
;		|		|--follows------|
;		|		  (adj qualif*) |-- <NOUN(agree), NBAR-QUANTIF>
;		|
;	 	|---qualif------|
;		 		|---before------|
;		 		|		|-- <ADV(compar), ADVBMOD-COMPAR>
;		 		|		|-- <ADV(quant), ADVBMOD-QUANT>
;		 		|
;		 		|---after-------|
;		 				|-- <CONJ(compar), COORD-COMPAR>
;		
(defun load-parserules ()
  (declare (special *PARSERULES-FILE*))
  (with-open-file (rport *PARSERULES-FILE*
			:direction :input :if-does-not-exist :error)
     (dolist (categ '(adj adv art conj noun predet prep pron verb))
	  (putprop categ nil 'chunk-rules))
     (do ((nextrule (read rport nil nil) (read rport nil nil)))
	 ((null nextrule))
; *** the function is very simple, because the rule is already written in a
;     tree format
	 (add-prop-val (first nextrule) 
		       'chunk-rules 
		       (rest nextrule)))))

; ****************************************************************
;     ONTOLOGY
; ****************************************************************
(defvar *ALL-LINKS*
  '(subclass-of has-subclass
    instance has-instance
    relinstance has-relinstance
    restricts restricted-by
    range range-of
    domain domain-of
    argument arg-of
    value value-of))

(defvar *ALL-IDS* nil)     ; *** all identifiers (classes, ...) in the KB

(defvar *ALL-ONT-CLASSES* nil)
(defvar *ALL-RELATIONS* nil)
(defvar *ALL-INSTANCES* nil)
(defvar *ALL-RELINSTANCES* nil)
(defvar *ALL-REL-RESTRS* nil)

(setq *ALL-IDS* nil)     ; *** all identifiers (classes, ...) in the KB

(setq *ALL-ONT-CLASSES* nil)
(setq *ALL-RELATIONS* nil)
(setq *ALL-INSTANCES* nil)
(setq *ALL-RELINSTANCES* nil)
(setq *ALL-REL-RESTRS* nil)

(defvar *INVERSES*
  '((subclass-of has-subclass)
    (part-of has-part)
    (instance has-instance)
    (relinstance has-relinstance)
    (restricts restricted-by)
    (range range-of)
    (domain domain-of)
    (argument arg-of)
    (value value-of)))

(defvar *LISTS*
  '((defconcept *ALL-ONT-CLASSES*)
    (relation *ALL-RELATIONS*)
    (instance *ALL-INSTANCES*)
    (relinstance *ALL-RELINSTANCES*)
    (rel-restr *ALL-REL-RESTRS*)))

(defun get-inverse (link)
  (declare (special *INVERSES*))
     (do ((pair (first *INVERSES*) (first rempairs))
          (rempairs (rest *INVERSES*) (rest rempairs)))
         ((or (null pair)
              (memq link pair))
            (cond ((eq (first pair) link) (second pair))
                  (t (first pair))))))

(defun get-list (link)
  (declare (special *LISTS*))
  (second (assoc link *LISTS*)))

(defun add-to-list (linktype newlink)
  (set (get-list linktype)
       (append1 (eval (get-list linktype)) newlink)))

; ********************************************************************
; *** main function for loading the ontology
;     It assumes that types (classes, relations) are defined before the
;     associated instances
; ********************************************************************
(defun load-ontology ()
  (let (loadres)
   (declare (special *SYSTEM-CONTEXT*
              *ONTOLOGY-FILE-BASE* *ONTOLOGY-FILE-PARTIC*
              *ONTOLOGY-FILE-PLACES* *ONTOLOGY-FILE-TITLES*))
   (with-open-file (oba-port *ONTOLOGY-FILE-BASE*
                          :direction :input :if-does-not-exist :error)
     (setq loadres (int-load-onto oba-port)))
   (cond ((not (memq *SYSTEM-CONTEXT* '(tule legal tocai tocai-test atlas)))
           (with-open-file (opa-port *ONTOLOGY-FILE-PARTIC*
                                  :direction :input :if-does-not-exist :error)
             (int-load-onto opa-port))
           (with-open-file (opl-port *ONTOLOGY-FILE-PLACES*
                                  :direction :input :if-does-not-exist :error)
             (int-load-onto opl-port))
           (with-open-file (ot-port *ONTOLOGY-FILE-TITLES*
                                  :direction :input :if-does-not-exist :error)
             (int-load-onto ot-port)))
         (t loadres))))

; ********************************************************************
(defun int-load-onto (lport)
  (let (assert-type assert-firstarg)
     (do ((nxtassertion (read lport nil 'end) (read lport nil 'end)))
         ((eq nxtassertion 'end) 'ok)
; *** the "assertion id" is the second element (e.g. the class name)
         (setq assert-type (first nxtassertion))
         (setq assert-firstarg (second nxtassertion))
     ;    (cond ((eq 'UTF-8 *LISP-CHAR-SET-ID*)
     ;             (setq assert-firstarg (convert-base-atom-or-string-to-currlisp assert-firstarg))))
         (cond ((memq assert-type '(defconcept relation))
                 (cond ((member assert-firstarg *ALL-IDS*)
                          (exception 'loadonto-error 
                                     "PROC/loadfunctions: Duplicate identifier in ontology"
                                      assert-firstarg))
                       (t (setq *ALL-IDS*
                                 (cons assert-firstarg *ALL-IDS*))))))
; *** for defconcept, add it to the list of defined concepts
         (cond ((eq assert-type 'defconcept)
                 (add-to-list assert-type assert-firstarg))
; *** for subclass-of, add the link and its inverse
               ((memq assert-type '(subclass-of part-of))
                 (let ((assert-secondarg (third nxtassertion)))
     ;               (cond ((eq 'UTF-8 *LISP-CHAR-SET-ID*)
     ;                        (setq assert-secondarg 
     ;                             (convert-base-atom-or-string-to-currlisp assert-secondarg))))
                    (cond ((and (member assert-firstarg *ALL-ONT-CLASSES*)
                                (member assert-secondarg *ALL-ONT-CLASSES*))
                             (add-prop-val assert-firstarg assert-type assert-secondarg)
                             (add-prop-val assert-secondarg (get-inverse assert-type) assert-firstarg))
                          (t (exception 'loadonto-error
                                  "PROC/loadfunctions: Subclass spec for a non existing concept (Upper concept, Lower concept):"
                            assert-firstarg assert-secondarg)))))
  ; *** the first two branches (subclass and instance) are identical;
  ;     they have been taken apart to enable for specific checks in the future
               ((eq assert-type 'instance)
                 (let ((assert-secondarg (third nxtassertion)))
     ;               (cond ((eq 'UTF-8 *LISP-CHAR-SET-ID*)
     ;                        (setq assert-secondarg 
     ;                             (convert-base-atom-or-string-to-currlisp assert-secondarg))))
                    (add-to-list assert-type assert-firstarg)
                    (cond ((member assert-secondarg *ALL-ONT-CLASSES*)
                             (add-prop-val assert-firstarg assert-type assert-secondarg)
                             (add-prop-val assert-secondarg (get-inverse assert-type) assert-firstarg))
                          (t (exception 'loadonto-error
                                  "PROC/loadfunctions: Instance spec for a non existing concept; Concept:"
                                  assert-secondarg)))))
  ; *** for relations, add links for restricts, domain, range, funct
               ((memq assert-type '(relation rel-restr))
                 (add-to-list assert-type assert-firstarg)
                 (dolist (nxtspec (rest (rest nxtassertion)))
                    (let ((oper (first nxtspec))
                          (argument (second nxtspec)))
         ;           (cond ((eq 'UTF-8 *LISP-CHAR-SET-ID*)
         ;                    (cond ((atom argument)
         ;                             (setq argument (convert-base-atom-or-string-to-currlisp argument)))
         ;                          (t (setq argument 
         ;                                  (mapcar #'convert-base-atom-or-string-to-currlisp argument))))))
                    (case oper
                      (restricts
                         (add-prop-val assert-firstarg oper argument)
                         (add-prop-val argument (get-inverse oper) assert-firstarg))
                      (range
                         (add-prop-val assert-firstarg oper argument)
            ; *** the range could be the union or two or more classes
                         (cond ((atom argument)
                                  (add-prop-val argument (get-inverse oper) assert-firstarg))
                               ((eq (first argument) 'union)
                                  (dolist (nxtcl (rest argument))
                                        (add-prop-val nxtcl (get-inverse oper) assert-firstarg)))
                               (t (exception 'loadonto-error 
					"PROC/loadfunctions: Unknown range op:" oper))))
                      (domain
                         (add-prop-val assert-firstarg oper argument)
            ; *** the domain could be basic datatype or the union or two or more classes
                         (cond ((atom argument)
                                  (cond ((not (is-basic-datatype argument))
                                           (add-prop-val argument (get-inverse oper) assert-firstarg))))
                               ((eq (first argument) 'union)
                                  (dolist (nxtcl (rest argument))
                                        (add-prop-val nxtcl (get-inverse oper) assert-firstarg)))
                               (t (exception 'loadonto-error
                                        "PROC/loadfunctions: Unknown domain op:" oper))))
                      (funct
                         (add-prop-val assert-firstarg oper (rest nxtspec)))
                      (expands-as
                         (format t "-- WARNING -- 'Expands as' in loading ontology has no effect~%"))
                      (otherwise 
                         (exception 'loadonto-error 
                                "PROC/loadfunctions: Unknown specification in relation def:" oper))))))
  ; *** for relation instances
  ;     ex. (relinstance 		: the assert type
  ;             &has-office1 		: second, the assert (instance) name
  ;             &has-office 		: third, the relation type
  ;             £Lingotto_Musica  	: fourth, the instance argument
  ;             £Lingotto_Musica_TC)	: third, the instance value
               ((eq assert-type 'relinstance)
                 (let ((reltype (third nxtassertion))
                       (instance1 (fourth nxtassertion))
                       (instance2 (fifth nxtassertion)))
                 (add-to-list assert-type assert-firstarg)
                 (add-prop-val assert-firstarg 		; &has-office1 is a 
                               assert-type 		; relinstance
                               reltype) 		; of &has-office 
                 (add-prop-val reltype			; &has-office has, as a new
                               (get-inverse assert-type) ; relinstance (has-relinstance),
                               assert-firstarg) 	; &has-office1
                 (add-prop-val assert-firstarg 		; &has-office1 has, as
                               'argument 		; argument, 
                               instance1)  		; £Lingotto_Musica 
                 (add-prop-val instance1		; £Lingotto_Musica is the
                               (get-inverse 'argument)	; argument of
                               assert-firstarg) 	; &has-office1
                 (add-prop-val assert-firstarg  	; &has-office1 has, as
                               'value			; value,
                               instance2) 		; £Lingotto_Musica_TC 
                 (cond ((not (is-subclass-of 
                                (first
                                  (get-link-val 'domain (third nxtassertion)))
                                '££datatype))
                         (add-prop-val instance2 ; £Lingotto_Musica_TC is
                                       (get-inverse 'value) ; the value of
                                       assert-firstarg))))) 	; &has-office1
               (t (exception 'loadonto-error 
                        "PROC/loadfunctions: Unknown assertion type" assert-type))))))

; ********************************************************************
(defun load-wmean ()
  (let (wm-port)
   (declare (special *WORD-MEAN-FILE* *WORD-MEANING*))
   (with-open-file (wm-port *WORD-MEAN-FILE*
                          :direction :input :if-does-not-exist :error)
      (do ((entry (read wm-port nil #\Escape) (read wm-port nil #\Escape)))
          ((equal entry #\Escape)
             (setq *WORD-MEANING* (reverse *WORD-MEANING*)))
          (setq *WORD-MEANING* 
             (cons entry *WORD-MEANING*)))
      (setq *WORD-MEANING* (reverse *WORD-MEANING*)))))
                     ;  (currscheme-uppercase (first entry))

; ********************************************************************
;(defun load-loc-concepts (conc-names)
;  (declare (special *LOCAL-ONTOLOGY-CONCEPTS*))
;  (let (tempres)
;      (setq *LOCAL-ONTOLOGY-CONCEPTS*
;          (dolist (entry conc-names (reverse tempres))
;               (setq tempres
;                   (cons (list (first entry) 
;                               ;(convert-base-to-currscheme-uppercase 
;                               (second entry))
;                                ;)
;                         tempres))))))

; ********************************************************************
(defun load-concept-mapping (conc-defs)
  (declare (special *OA-MAPPING*))
  (let (tempres)
      (setq *OA-MAPPING*
          (dolist (entry conc-defs (reverse tempres))
               (setq tempres
                   (cons (cons ;(convert-base-to-currscheme-uppercase-no-accent (first entry))
                               (first entry)
                               (rest entry))
                         tempres))))))

