(in-package "USER")

; ********************************************************
; *** this simply moves across the RESTR path of the semantic 
;     query in order to match it to the predicate definition
; *** just the restriction is used; it includes the topic, while the
;     query goal is not useful, since it involves what is looked for,
;     not what is given
(defun sem-to-pred-translation (inputquery)
 (declare (special *ANAPHORIC-CONTEXT*))
 (let (goal-part restr-part par-result
  ; *** the next because inputquery has a double level of parentheses
  ;     (ambiguity? multiple sentences?)
       (semquery (first inputquery)))
; *** the next cond extracts and interprets the goal part ***************
  (cond ((eq (first semquery) 'select)
  ; ** it is a standard select ... from ... where ... query
  ;     translate the select ... part
  ; ** in handling the select part, int-stp-transl takes as input also
  ;    the "from" concept"
          (setq goal-part 
                    (int-stpr-transl (second semquery) (fourth semquery)))
  ; ** if nothing is obtained from the translation of the "select" part,
  ;    assume that the "from" part is a subclass of a basic data type,
  ;    and use it as goal
          (cond ((all-null goal-part)
                   (setq goal-part (list (list (list (fourth semquery))))))))
  ; ** the next should not occur in TOCAI, at least in this first version
        ((eq (first semquery) 'about)
          (setq goal-part 
                    (int-stpr-transl (fourth semquery))))
        (t (exception 'parameter-error "PROC/buildpred: Unknown expression" (first semquery))))
; *** the next cond extracts and interprets the goal part ***************
  (cond ((eq (first semquery) 'about)
           (setq restr-part nil))
        ((null (sixth semquery))
  ; *** but if the restriction is nil, then use the 'from' concept
          (setq restr-part
               (int-stpr-transl (list (fourth semquery)))))
        (t (setq restr-part (int-stpr-transl (sixth semquery)))))
  (setq par-result (list (elimdup goal-part) (elimdup restr-part)))
  ; *** the first element of *ANAPHORIC-CONTEXT* is the syntactic and
  ;     ontological part of the user's input; now, also its parameter
  ;     representation is added
  ;(setq *ANAPHORIC-CONTEXT* 
  ;      (cons (append1 (first *ANAPHORIC-CONTEXT*) par-result)
  ;            (rest *ANAPHORIC-CONTEXT*)))
; *** finally, the mastro query is built ***********************************
  (setq par-result (generate-mastro-query par-result))
  par-result))

; ********************************************************
; *** takes as input a list of two elements; the first of them is
;     the result of the translation of the "select" clause, the second
;     is the result of the translation of the "where" clause.
; *** each sublist includes data about the predicates involved in the
;     final query
; *** the items of the first sublist can be 
;     1. triples representing binary predicates 
;        (e.g. <loc ££component ££locationDescr>)
;        The target variable is last one of the triple, while the first one acts as
;        a link to the RHS of the query. In the case above, the RHS could be 
;        <installed ££component>, so that the resulting query would be
;        Q(X1) :- loc(Y1,X1), installed(Y1)
;     2. single items (e.g. ££generalConfigurationState)
;        In this case, the variable acts both as target variable and as link
;        variable
; *** the items of the second sublist can be either triples as above, pairs
;     representing unary predicates or special triples representing constants
;     (e.g. <%value ££serialNumber £x328A>)
;     In this case, it is assumed that there is another triple of the formof ££serial
;     <PRED conc ££serialNumber>; the final result involves a replacement, in this
;     last triple of ££serialNumber with £x328A, and the deletion of the %value triple
(defun generate-mastro-query (tempquery)
   (let ((targets (flatten (first tempquery)))
         (constraints (flatten (second tempquery)))
             ; *** flatten removes a level of parentheses
         (varindex 0)
         targetconcs targetvars preds-ok preds-no newvars targetpreds nxtconc
         constants all-preds)
      (declare (special varindex))
   ; *** the next carries out the replacement mentioned in the last comment above
      (multiple-value-setq (constraints constants) (replace-constants constraints))
      (cond ((all-null targets) (setq targets nil)))
      (dolist (nxttarget targets)
             (cond ((> (length nxttarget) 1)
                      (setq targetpreds (cons nxttarget targetpreds))))
             (cond ((not (member (ult nxttarget) constants))
                      (setq nxtconc (ult nxttarget))
                      (setq targetconcs (cons nxtconc targetconcs))
                      (setq targetvars (cons (list nxtconc (generate-var 'x))
                                             targetvars)))))
      (setq all-preds (append constraints targetpreds))
      (setq preds-ok all-preds)
      (setq preds-no nil)
      (setq newvars targetvars)
      (do ()
          ((or (null preds-ok) (equal preds-ok '(nil)))
             (generate-prolog-form (reverse targetvars) (reverse all-preds) newvars))
   ; *** in preds-ok all predicates where "targetconc" occurs
   ;     in preds-no the remaining predicates
   ; *** newvars is extended by generating the variable names for all new
   ;     concepts appearing in the predicates where already bound 
   ;     variables occur
          (multiple-value-setq (newvars preds-no)
                 (find-involved-preds preds-ok newvars constants))
          (cond ((equal preds-ok preds-no)
   ; *** if the new preds-no is equal to the previus pred-ok, then no
   ;     connecting predicate has been found
                  (exception 'parameter-error
                         "PROC/buildpredicate Generate mastro query"))
   ; *** otherwise, the remaining predicates are set as the targets for the next step
                (t (setq preds-ok preds-no))))))
 
; ********************************************************
(defun replace-constants (constraints)
   (let (replacements trueconstrs newconstrs constants)
 ; *** take apart triples concerning constants from other triples
      (dolist (nxtconstr constraints)
         (cond ((eq (first nxtconstr) '%value)
                 (setq replacements (cons nxtconstr replacements))
                 (setq constants (cons (third nxtconstr) constants)))
               (t (setq trueconstrs (cons nxtconstr trueconstrs)))))
 ; *** for all triple concerning constants, carry out the replacement
      (dolist (nxtrepl replacements)
         (dolist (nxtconstr trueconstrs)
            (setq newconstrs (cons (substitute (third nxtrepl) (second nxtrepl) nxtconstr)
                                   newconstrs)))
         (setq trueconstrs newconstrs)
         (setq newconstrs nil))
      (values trueconstrs constants)))
      
; ********************************************************
; *** looks for all preds where a given set of concepts appears
(defun find-involved-preds (preds conc-var-pairs constants)
   (let ((concepts (mapcar #'first conc-var-pairs))
         foundpreds rempreds prednewconcs)
    ; *** all predicates are inspected
    ;     at the end, conc-var-pairs has been extended with the
    ;     new pairs concerning the predicates where the pivot
    ;     (previous step elements) appears
       (dolist (nxtpred preds (values conc-var-pairs rempreds))
    ; *** concepts are the conceptes to which a variable has already
    ;     been assigned
             (cond ((intersection concepts (rest nxtpred))
                      (setq foundpreds (cons nxtpred foundpreds))
                      (setq prednewconcs
                          (elimdup (set-difference (rest nxtpred) concepts)))
    ; *** a new variable is generated, for each new concept occurring in this
    ;     predicate
                      (dolist (nxtconc prednewconcs)
                          (cond ((not (member nxtconc constants))
                                   (setq concepts (cons nxtconc concepts))
                                   (setq conc-var-pairs 
                                        (cons (list nxtconc (generate-var 'y))
                                              conc-var-pairs))))))
                   (t (setq rempreds (cons nxtpred rempreds)))))))
        
; ********************************************************
; ***
(defun generate-var (root)
  (declare (special varindex))
   (setq varindex (1+ varindex))
   (concat root varindex))
      
; ********************************************************
(defun generate-prolog-form (targetvars constraints newvars)
  (let (form (targetprolvars (mapcar #'second targetvars)))
  ; *** build-up the LHS
    (setq form (concat 'q #\( (first targetprolvars)))
    (dolist (nxtv (rest targetprolvars))
        (setq form (concat form #\, nxtv)))
    (setq form (concat form #\)))
  ; *** include in the antecedent all target variables
    (setq form (concat form #\Space #\: #\- #\Space))
    (setq form (concat form (gen-prolog-pred (first constraints) newvars)))
    (dolist (pred (rest constraints))
       (setq form (concat form #\, #\Space (gen-prolog-pred pred newvars))))
    (list 'ok form)))

; ********************************************************
(defun gen-prolog-pred (pred newvars)
  (let (form var)
    (setq form (concat (first pred) #\( (first (leggi newvars (second pred)))))
    (dolist (conc (rest (rest pred)))
         (setq var (first (leggi newvars conc)))
   ; *** if the concept name does not appear in newvars, then it is a constant
   ;     and is left unchanged
         (setq form (concat form #\, (cond ((null var) conc) (t var)))))
    (concat form #\))))

; ********************************************************
; *** the input ("path" and the optional "from") may come either from the 
;     goal part or from the where part of the query [prevpath is just
;     an accumulator of the part that has already been seen; it is not
;     currently used, but it must be checked]
; *** in case the input is from the goal part, "from" is non-null (it is the
;     pivot concept, a single atomic concept) and "path" has the form:
;     (el1 el2 ... elN) 
;       or
;     ((el11 el12 ... el1N1) and (el21 el22 ... el22N2) and ...)
;     The various el may correspond to concepts, links (e.g. range, subclass-of),
;     or relations. 
;     The object to translate is normalized into:
;     ((from el1 el2 ... elN)) or into:
;     ((from el11 el12 ... el1N1) (from el21 el22 ... el22N2) ...)
;     In the goal part, in fact, the "from" concept is not included in the "select"
;     list
; *** in case the input is from the where part,
;     "from" is null and "path" has the form:
;     (el1 el2 ... elN) 
;       or
;     (el1 (and ((el11 el12 ... el1N1) (el21 el22 ... el22N2) ...)))
;     Here, the object to translate is normalized into:
;     ((el1 el2 ... elN)) or into:
;     ((el1 el11 el12 ... el1N1) (el1 el21 el22 ... el22N2) ...)
; *** The choice of having infix operators in the goal part and prefix operators
;     in the restriction part is questionable, but it helps keeping apart two
;     different sorts of "and": in the goal part, it is the union of data that
;     must be retrieved; in the restriction part, it is a real logical "and" of
;     conditions
(defun int-stpr-transl (path &optional from)
; *** the semantic query is just a sequence of atoms or sublists
; *** The relevant concepts (associated with a translation into a predicate)
;     act as an index onto a table including subpaths. For instance, in
;     order to specify that the pn (part-number) predicate, is associated
;     with a path connecting ££serialNumber to ££component, we have the
;     following entry:
;      (££COMPONENT ((&HAS-SERIAL-NUMBER ££SERIALNUMBER) (PN ££COMPONENT ££SERIALNUMBER)))
;      (££SERIALNUMBER ((&HAS-SERIAL-NUMBER ££COMPONENT) (PN ££COMPONENT ££SERIALNUMBER)))
;     This has to be read as:
;     - upon encountering the ££COMPONENT concept, if it is followed by a
;       path passing through &HAS-SERIAL-NUMBER and leading to ££SERIALNUMBER,
;       then this path refers to the predicate PN, where the component is
;       the first argument, and the serial number is the second argument.
;       Similarly for the second entry, that is generated automatically
;       from the first.
; *** Another option concerns instances, where the constant is the argument
;     of a comparison operator, as in:
;      (££COMPONENT ((££SPECIFICATION (EQ £OBSOLETE)) (OBSOLETE ££COMPONENT)))
;      (££OBSOLETE ((@@eq-value-of ££SPECIFICATION ££COMPONENT) (OBSOLETE ££COMPONENT)))
  (let ((norm-path (normalize-path path from)) resquery tempquery)
  ; *** after the normalization, the representation is a set of lists, each of
  ;     which connects the pivot concept to either a goal element or a restriction
  ; (cond ((null from)
    ; *** we are handling a restriction
            (setq resquery
               (dolist (nxtpath norm-path tempquery)
                  (setq tempquery (cons (sing-extract-pred nxtpath nil nil) tempquery))))
            (mapcar #'remove-internal-predicates resquery)
    ;  )
    ;   (t
    ; *** we are handling a select clause element
    ;     The operations are the same, since this is under construction
    ;      (dolist (nxtpath norm-path resquery)
    ;         (setq resquery (cons (sing-extract-pred nxtpath nil nil) resquery)))))
    ;  resquery
      ))

; ********************************************************
(defun remove-internal-predicates (query)
  (declare (special *INTERNAL-PREDS*))
  (let ((newquery query))
    (do ((currpred (first *INTERNAL-PREDS*) (first preds))
          (preds  (rest *INTERNAL-PREDS*) (rest preds)))
         ((null currpred) newquery)
         (setq newquery (match-int-pred currpred newquery)))))

; ********************************************************
; *** This replaces parts of a query, according to the preddef definition:
;     if p1, p2, ... pn, q are predicates:
;     - ((p1 p2) q) + (p3 p2 p4 p1 p5) --> (q p3 p4 p5)
;     - ((p1 p2) q) + (p3 p4 p1 p5) --> (p3 p4 p1 p5)
(defun match-int-pred (preddef query)
   (let ((oldpred (first preddef)) (newpred (second preddef)) (newquery query) fail found)
   ; *** the external do loops on all predicates that must be replaced; if any of
   ;     them is not found in query, then the match does not succeed, and the query
   ;     is returned unchanged
      (do ((currpd (first oldpred) (first oldpred))
           (oldpred (rest oldpred) (rest oldpred)))
          ((or (null currpd) fail)
             (cond (fail query)
                   (t (cons newpred newquery))))
          (setq found nil)
   ; *** the next do loops on the predicates of the original query, in order to
   ;     see if one of them is equal to the part of preddef currently under inspection
   ;     If such a predicate is found, it is removed from resquery
    ;(format t "Match-int-pred: Entering inner loop: newquery = ~a~%" newquery)
    ;(break "")
          (do* ((resquery nil (append1 resquery qpred))
                (qpred (first newquery) (first remquery))
                (remquery (rest newquery) (rest remquery)))
              ((or (null qpred) found)
                 (cond ((not found) (setq fail t))
       ; *** if px is found (in (p1 p2 px p3 p4)), the current situation is
       ;     resquery: (p1 p2 px)
       ;     qpred: p3
       ;     newquery: (p4)
       ; *** but if it is found in (p1 p2 px), the situation is
       ;     resquery: (p1 p2 px)
       ;     qpred: nil
       ;     newquery: nil
       ;     The cond is needed to avoid the result (p1 p2 nil)
                       (t (setq newquery (append (butlast resquery)
                                                 (cond ((null qpred) nil)
                                                       (t (cons qpred remquery))))))))
              (cond ((equal currpd qpred)
                       (setq found t)))))))

; ********************************************************
; *** this takes a path (which should not include any logical operator) and
;     produces a list of predicates.
; *** the form of the input should be 
;     (el1 el2 ... elN)
;     where all elK can be concept, relations or links (as domain and range)
;     The unique exception is elN, which could also be a constant specification
;     in the form "(eq val)"
; *** binding-table is a table that specifies, for each concepts (that will be
;     associated with a Mastro variable), if it has already appeared in the path
;     it is extended as soon as a new piece of path is analyzed; its form is
;     ((conc1 lastassignedindex1) (conc2 lastassignedindex2) ....)
;     It is initially empty
(defun sing-extract-pred (path prevpath binding-table)
  (declare (special *OPRED-MAPPING*))
   (cond ((null path) nil)
         (t (let* ((param-value (leggi *OPRED-MAPPING* (first path)))
      ; *** param-value is the definition found in *OPRED-MAPPING* for the initial
      ;     element of the query path. e.g., for ££component:
      ;        (((&hasSn ££serialNumber)
      ;                 (pn ££item ££serialNumber))
      ;         ((&hasItemCode ££itemCode)
      ;                 (pn ££item ££itemCode)) ...)
                 (predic-eval (match-ont-path param-value path binding-table))
                 (predicate (first predic-eval))
                 (usedpath (second predic-eval))
                 (restpath (third predic-eval))
                 newbinding actvarname)
      ; *** restpath is the remaining part of the input query, to be translated (e.g.
      ;   (££SERIALNUMBER DOMAIN-OF &HAS-SERIAL-NUMBER RANGE ££SERIALIZABLEPHYSICALCOMPONENT
      ;    SUBCLASS-OF ££PHYSICALCOMPONENT RANGE-OF &IMPLEMENTS DOMAIN ££COMPONENT))
      ; *** in this example, this has to be flushed until &HAS-SERIAL-NUMBER, and then
      ;     until ££COMPONENT
      ; *** Here, we must handle the variable bindings; in fact,
      ;     in case the where part of the ontological query is:
      ;     (££ITEM RANGE-OF &REPLACINGITEM DOMAIN ££REPLACE DOMAIN-OF &REPLACEDITEM 
      ;      RANGE ££ITEM DOMAIN-OF &HASITEMCODE RANGE ££ITEMCODE (EQ X76576))
      ;     The first ££ITEM is different from the second
      ;     After the first step of the outer loop, we have that
      ;     restpath is (££REPLACE DOMAIN-OF &REPLACEDITEM 
      ;      RANGE ££ITEM DOMAIN-OF &HASITEMCODE RANGE ££ITEMCODE (EQ X76576))
   ;(format t " Predicate: ~a~%; restpath: ~a~%" predicate restpath)
   ;(break "buildpredicate: sing-extract-pred")
               (cond ((null predic-eval)
     ; *** in this case, there is the possibility that the form is "concept (eq val)"
                       (cond ((and (eq 2 (length path))
                                   (listp (second path))
                                   (eq 'eq (first (second path))))
                                (setq actvarname 
                                   (get-sing-var-name (first path) binding-table))
                                (list (list '%value actvarname (second (second path)))))
                             (t (sing-extract-pred (rest path) 
                                             (cons (first path) prevpath)
                                             binding-table))))
                     (t (setq newbinding (expand-binding binding-table predicate restpath))
                        (cons predicate
                            (sing-extract-pred restpath (append usedpath prevpath) newbinding))))))))

; ********************************************************
(defun expand-binding (oldbinding predicate path)
   (let ((newbinding oldbinding))
      (dolist (conc (rest predicate) newbinding)
          (cond ((neq conc (first path))
   ; *** this cond, since the first item of path is the one that must act as link
   ;     to the previous part of the query, so that it actually is the same variable
   ;     already existing in the previous part
   ;     e.g. predicate = (OGGRAD ££REPLACE ££ITEM)
   ;          path = (££REPLACE DOMAIN-OF ...)
                   (setq newbinding (add-binding conc newbinding)))))))

; ********************************************************
; *** this increments the second item of the pair associated with conc
;     if there is no such pair a new one is created, with second item = 1
(defun add-binding (conc binding)
   (cond ((null binding)
            (list (list conc 1)))
         ((eq conc (first (first binding)))
            (cons (list conc (1+ (second (first binding))))
                  (rest binding)))
         (t (cons (first binding)
                  (add-binding conc (rest binding))))))

; ********************************************************
; *** see the comments to int-stpr-transl above
(defun normalize-path (path from)
  (let ()
     (cond ((null path) '(nil))
           ((not (null from))
   ; *** we are in the "select" clause; boolean operators are infix
              (cond ((atom (first path))
         ; *** no boolean operators: (el1 el2 ...)
                       (list (cons from path))) 
              ; *** a level of parentheses is added: ((from el1 el2 ...))
                    ((null (second path))
         ; *** last operand: ((el1 el2 ...))
                       (list (cons from (first path))))
              ; *** insertion of the "from" info: ((from el1 el2 ...))
                    ((eq 'and (second path))
         ; *** first operand of an and-ed expression: ((el11 el12 ...) and (el21 el22 ...) and ...)
                       (cons (cons from (first path))
                             (normalize-path (rest (rest path)) from)))
              ; *** insertion of the "from" info in the first and recursive call
                    (t (exception 'parameter-error
                              "PROC/buildpredicate: unknown select structure" path))))
           (t
   ; *** we are in the "where" clause; boolean operators are prefix
              (cond ((atom (first path))
                       (mult-cons (first path)
                                  (normalize-path (rest path) nil)))
                    ((eq 'and (first (first path)))
   ; *** (and ((el11 el12 ...) ... (elk1 elk2 ...)))
                       (flatten (mapcar #'(lambda (x) (normalize-path x nil)) (second (first path)))))
                    ((eq 'eq (first (first path)))
                       (list (list (first path))))
                    (t (exception 'parameter-error
                              "PROC/buildpredicate: unknown restriction structure" path)))))))

; ********************************************************
; *** this tries to see if the elements in restpath match the
;     description provided in pred-value (obtained from *OPRED-MAPPING*)
(defun match-ont-path (pred-value restpath binding-table)
  (let (nxtdefconc nxtdefpred found lastmatchedconc boundpred)
   (do ((nxtdef (first pred-value) (first pred-value))
        (pred-value (rest pred-value) (rest pred-value))
        (restquery restpath restpath))
; *** the external loop exits when all parts of the ont-to-pred definition
;     associated with a given concept have been examined
;     It returns the best match, i.e. a triple such that
;     1. It includes the possibly found match, the pathe covered to get the match
;        and the remaining path
;     2. The path covered is the shortest one, i.e. the smallest portion of the
;        input has been consumed
       ((null nxtdef) 
             (find-best-pred-def found))
   ; *** nxtdefconc is the expected path (e.g. (&HAS-SERIAL-NUMBER ££COMPONENT))
   ; *** nxtdefpred is the associated predicate (e.g. (PN ££COMPONENT ££SERIALNUMBER))
       (setq nxtdefconc (first nxtdef))
       (setq nxtdefpred (second nxtdef))
       (do* ((nxtqconc (first restquery) (first restquery))
             (restquery (rest restquery) (rest restquery))
             (usedquery nil (cons nxtqconc usedquery)))
           ((or (null nxtqconc) (null nxtdefconc))
   ; *** if nxtdefconc is null then all concepts in the definition have been found, so the
   ;     match has succeeded
              (cond ((null nxtdefconc)
                      (setq boundpred (get-var-names nxtdefpred binding-table))
                      (setq found 
                         (append1 found
                            (list boundpred (rest (rest usedquery))
                              (cons lastmatchedconc (cons-nonil nxtqconc restquery))))))))
           (cond ((null nxtqconc) nil)
   ; *** if nxtqconc is nil, then the input query has been inspected until its end
   ;     without finding a match
                 ((equal nxtqconc (first nxtdefconc))
   ; *** if there is a match, go ahead in the definition
                     (setq lastmatchedconc (first nxtdefconc))
                     (setq nxtdefconc (rest nxtdefconc)))
                 (t nil))))))

; ********************************************************
(defun get-var-names (pred var-table)
    (cons (first pred) (mapcar #'(lambda (x) (get-sing-var-name x var-table)) (rest pred))))

; ********************************************************
(defun get-sing-var-name (basevar var-table)
    (let ((val (first (leggi var-table basevar))))
       (cond ((null val) basevar)
             (t (concat basevar '- val)))))

; ********************************************************
; *** found is alist of triples:
;     - found predicate
;     - path covered to find the match
;     - remaining path
; *** the function returns the triple with the shortest path covered
(defun find-best-pred-def (found)
 (let ((bestlength (length (second (first found)))) (bestsol (first found)))
  (dolist (triple (rest found) bestsol)
     (cond ((< (length (second triple)) bestlength)
              (setq bestsol triple)
              (setq bestlength (length (second triple))))))))

; ********************************************************
; *** This takes the definition associated with a given concept
;     and evaluates it
(defun pred-q-eval (conc-name pred-def restquery prevpath)
  (declare (special *SYSTEM-CONTEXT*))
  (let (firstarg secondarg thirdarg)
   (cond ((null pred-def) nil)
      ; *** no definition is associated with the concept
         ((eq (first (first pred-def)) '*NO*DATA*)
      ; *** the concept correspond to something the system cannot do
            (cons (first pred-def) 
                  (pred-q-eval conc-name (rest pred-def) restquery prevpath)))
         ((eq (first (first pred-def)) 'XCASE)
      ; *** the translation of the concept depends on some other concept
            (append (cond-param-eval conc-name (rest (first pred-def)) restquery prevpath)
                  (pred-q-eval conc-name (rest pred-def) restquery prevpath)))
         ((atom (second (first pred-def)))
      ; *** standard definition: the first of pred-def is the parameter name
            (cons (first pred-def) 
                  (pred-q-eval conc-name (rest pred-def) restquery prevpath)))
         ((eq 'fun (first (second (first pred-def))))
      ; *** the value is obtained by evaluating a function
            (cons (list (first (first pred-def))
                        (eval (second (second (first pred-def)))))
                   (pred-q-eval conc-name (rest pred-def) restquery prevpath)))
         ((eq 'fun2 (first (second (first pred-def))))
      ; *** the value is obtained by evaluating a function which must be given
      ;     as input the previous and the next part of the ontological query
            (cond ((eq *SYSTEM-CONTEXT* 'flat-analysis)
                     (setq firstarg 
                        (list 'quote (mapcar #'first (mapcar #'first prevpath))))
                     (setq secondarg 
                        (list 'quote (mapcar #'first (mapcar #'first restquery))))
                     (setq thirdarg 
                        (cond ((atom conc-name)
                                 (list 'quote conc-name))
          ; *** conc-name could include the number info (e.g. (££week sing))
                              (t (list 'quote (first conc-name))))))
                  ((memq *SYSTEM-CONTEXT* '(hops hops-dial tocai))
                     (setq firstarg (list 'quote prevpath))
                     (setq secondarg (list 'quote restquery))
                     (setq thirdarg (list 'quote conc-name))))
            (cons (list (first (first pred-def))
                        (eval (append (second (second (first pred-def)))
                                      (list firstarg secondarg thirdarg))))
                  (pred-q-eval conc-name (rest pred-def) restquery prevpath)))
        (t (exception 'parameter-error
                  "PROC/buildparam: Unknown complex parameter definition" pred-def)))))

; ********************************************************
; *** This takes a conditional definition associated with a given concept
;     and evaluates it
; *** a mult-pred-def is a set of tuples such that the first of them is
;     a predicate to evaluate, and the rest is a set of parameter values
;     - the predicates are evaluated in sequence; as soon as one of them
;       is true, the associated parameter values are added to the query
(defun cond-param-eval (conc-name mult-param-def restquery prevpath)
 (declare (special *SYSTEM-CONTEXT*))
  (cond ((null mult-param-def)
           (exception 'parameter-error
		 "PROC/buildparam: Wrong definition of a conditional parameter"))
        (t (let* ((firstclause (first mult-param-def))
                  (condition (first firstclause))
                  (operator (first condition))
                  condeval)
             (setq condeval
                (case operator
                    (in-query
                        (cond ((neq *SYSTEM-CONTEXT* 'flat-analysis)
                                 (q-intersection (rest condition)
                                               (append restquery prevpath)))
           ; *** if we are carrying on a flat analysis, assume that ££dialogue
           ;     is in focus
                              ((eq (second condition) '££dialogue-topic) t)
                              (t (q-intersection (rest condition)
                                         (flatten (mapcar #'first 
                                               (append restquery prevpath)))))))
                    (is-singular 
                        (cond ((neq *SYSTEM-CONTEXT* 'flat-analysis)
                                 (eq (get-ont-query-cardinality restquery prevpath)
                                     'sing))
                              (t (eq (second conc-name) 'sing))))
                    (is-plural
                        (cond ((neq *SYSTEM-CONTEXT* 'flat-analysis)
                                 (eq (get-ont-query-cardinality restquery prevpath)
                                     'pl))
                              (t (eq (second conc-name) 'pl))))
                    (is-number-allval t)
                    (else t)
                    (otherwise
                        (exception 'parameter-error
			    "PROC/buildparam: Unknown operator in conditional parameter"
		            operator))))
             (cond (condeval
                      (param-eval conc-name
                               (rest (first mult-param-def)) restquery prevpath))
                   (t (cond-param-eval conc-name
                               (rest mult-param-def) restquery prevpath)))))))

; ********************************************************
(defun q-intersection (conc-list query)
  (cond ((null conc-list) nil)
        ((int-q-inters (first conc-list) query) t)
        (t (q-intersection (rest conc-list) query))))

(defun int-q-inters (conc query)
  (cond ((null query) nil)
        ((atom (first query))
           (eq conc (first query)))
        ((eq 'eq (first (first query)))
           (eq conc (second (first query))))
        (t (int-q-inters conc (rest query)))))
        
; ********************************************************
; *** this gets a cardinality from a query; it proceeds on the query until
;     1. a link is found which is not among "subclass-of has-subclass instance
;        has-instance". In this case, we are moving to another concept, so the
;        information about the cardinality is missing
;     2. A "cardinality" or "ident" clause is found
(defun get-ont-query-cardinality (query prevquery)
  (let ((cardin (get-ont-card query))
        (prevcardin (get-ont-card prevquery)))
      (cond ((null prevcardin)
   ; *** no cardinality info from the first part: use the second part (if any)
               (first cardin))
            ((eq (second prevcardin) 'actual)
   ; *** from the first part cardinality which is not a default: use it
               (first prevcardin))
            ((eq (second cardin) 'actual)
   ; *** from the first part cardinality which is a default: if from the second part
   ;     not a default, use the second
               (first cardin))
   ; *** otherwise, both defaults: use the first
            (t (first prevcardin)))))

(defun get-ont-card (query)
  (declare (special *ALL-LINKS*))
  (cond ((null query) nil)
        ((atom (first query))
          (cond ((member (first query) *ALL-LINKS*)
   ; *** *ALL-LINKS* defined in HOPS/loadontol
   ; !!!! all branches are identical: this is just a way to make it work
                   (cond ((member (first query) 
                              '(subclass-of has-subclass instance has-instance))
                            (get-ont-card (rest query)))
                         (t (get-ont-card (rest query)))))
                (t (get-ont-card (rest query)))))
        ((eq (first (first query)) 'cardinality)
   ; *** explicit cardinality infos: not a default
           (list (second (first query)) 'actual))
        ((eq (first (first query)) 'eq)
          (cond ((numberp (second (first query)))
                   (get-ont-card (rest query)))
                ((is-subclass-of (get-instance-class (second (first query))) 
                              '££simple-ev-descr)
  ; *** proper names (having an ident) are assumed to correspond to single entities
                   '(sing default))
                (t (get-ont-card (rest query)))))
        ((eq (first (first query)) 'ident)
  ; *** proper names (having an ident) are assumed to correspond to single entities
           (exception 'parameter-error 
              "PROC/buildparam: IDENT present in a semantic query" (first query)))
        ((eq (first (first query)) 'and)
  ; *** for conjunctive conditions, I assume that the cardinality info should
  ;     not occur in one of the conjuncts
            nil)
        (t (exception 'parameter-error 
              "PROC/buildparam: Unknown non-atomic element in query" (first query)))))
          
; ********************************************************
; *** if a parameter has two values, keeps the first
;     unless one of the two is a default
(defun remove-dupl-par (paramlist)
  (int-rem-d-p paramlist nil))

(defun int-rem-d-p (paramlist prevparams)
  (cond ((null paramlist)
           (reverse prevparams))
        (t (let ((otherval (assoc (first (first paramlist)) prevparams)))
               (cond (otherval
          ; *** if there are two occurrences of the |requestFocus| parameter and
          ;     at least one of them is "single-event", then assume it as the
          ;     correct value
                       (cond ((and (eq (first (first paramlist)) '|requestFocus|)
                                   (or (equal (second otherval) "single_event")
                                       (equal (second (first paramlist)) "single_event")))
                                (cons '(|requestFocus| "single_event")
                                      (int-rem-d-p (rest paramlist)
                                           (remove-param '|requestFocus| prevparams t))))
                             (t (int-rem-d-p (rest paramlist) prevparams))))
                     (t (int-rem-d-p (rest paramlist) 
                                 (cons (first paramlist) prevparams))))))))

; ********************************************************
; *** if a set of parameters refers to a date in the old notation,
;     it is translated into the new notation
(defun convert-date-params (paramlist)
  (let ((day (first (leggi paramlist 'EV-DATE-DAY)))
        (month (first (leggi paramlist 'EV-DATE-MONTH)))
        (year (first (leggi paramlist 'EV-DATE-YEAR))))
    (cond ((and (null day) (null month) (null year))
              paramlist)
          ((and (null month) (null year))
             (setq day (add-zero (explode day)))
             (setq month (add-zero (explode (get-today-month))))
             (setq year (add-zero (explode (get-today-year))))
             (cons (list '|date| 
                       (implode (append day '(#\/) month '(#\/) year)))
                   (remove-param 'ev-date-day paramlist t)))
          ((and (null day) (null year))
             (setq year (add-zero (explode (get-today-year))))
             (cons (list '|date| 
                       (implode (append '(#\0 #\0 #\/) month '(#\/) year)))
                   (remove-param 'ev-date-month paramlist t)))
          ((and (null day) (null month))
             (setq year (add-zero (explode (get-today-year))))
             (cons (list '|date| 
                       (implode (append '(#\0 #\0 #\/ #\0 #\0 #\/) year)))
                   (remove-param 'ev-date-year paramlist t)))
          ((null year)
             (setq day (add-zero (explode day)))
             (setq month (add-zero (explode month)))
             (setq year (add-zero (explode (get-today-year))))
             (cons (list '|date| 
                       (implode (append day '(#\/) month '(#\/) year)))
                   (remove-param 'ev-date-day
                         (remove-param 'ev-date-month paramlist t) t)))
          ((null day)
             (setq month (add-zero (explode month)))
             (setq year (add-zero (explode year)))
             (cons (list '|date| 
                       (implode (append '(#\0 #\0 #\/) month '(#\/) year)))
                   (remove-param 'ev-date-year
                         (remove-param 'ev-date-month paramlist t) t)))
          ((null month)
             (exception 'parameter-error "buildparam: Specification of day and year, but not month"))
          (t (setq day (add-zero (explode day)))
             (setq month (add-zero (explode month)))
             (setq year (add-zero (explode year)))
             (cons (list '|date| 
                       (implode (append day '(#\/) month '(#\/) year)))
                   (remove-param 'ev-date-year
                      (remove-param 'ev-date-month
                         (remove-param 'ev-date-day paramlist t) t) t))))))

(defun remove-param (paramname paramlist mustBeFound)
   (cond ((null paramlist)
           (cond (mustBeFound
                   (exception 'parameter-error "buildparam: Removal of a non-existent parameter"))
                 (t nil)))
         ((eq paramname (first (first paramlist)))
            (rest paramlist))
         (t (cons (first paramlist)
                  (remove-param paramname (rest paramlist) mustBeFound)))))
            



