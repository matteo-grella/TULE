(in-package "USER")

; *****************************************************************
;                   TREE REPRESENTATIONS
; -----------------------------------------------------------------
; In TUT, there are various ways for representing trees:
; 1. The temporary representation used by the parser; here, we have
;    two parallel lists, the first one is a list of morphological data
;    (each line is associated with an input item; sentences are
;    sublists of the whole list). The second list is a list of pairs
;    representing the tree pointers. They are kept separate because
;    the first of them is the output of the POS tagger, while the
;    second is created by the parser. 
;    Ex: "The boy runs. He runs fast.":
;            First list of lists (morphological infos)
;        (((1 The (THE ART DEF ALLVAL ALLVAL) nil nil)
;          (2 boy (BOY NOUN COMMON M SING) nil nil)
;          (3 runs (RUN VERB MAIN IND PRES 3 SING) nil nil)
;          (4 . (#\. PUNCT) nil nil))
;         ((1 He (HE PRON PERS M SING) nil nil)
;          (2 runs (RUN VERB MAIN IND PRES 3 SING) nil nil)
;          (3 fast (FAST ADV MANNER) nil nil)
;          (4 . (#\. PUNCT) nil nil)))
;        Where the two nil's in each line refer to trace co-indexing and
;        possible "comments"
;            Second list of lists (tree infos)
;        (((3 VERB-SUBJ)
;          (1 DET+DEF-ARG)
;          (0 TOP-VERB)
;          (3 END))
;         ((2 VERB-SUBJ)
;          (0 TOP-VERB)
;          (2 ADVB-RMOD-MEANSMANNER)
;          (2 END)))
; 2. The actual TUT format, which is the one that can be found in the
;    TUT files. This is produced mainly for readability.
;        ********* FRASE EXAMPLE-1 *********************
;        1 The (THE ART DEF ALLVAL ALLVAL) [3;VERB-SUBJ]
;        2 boy (BOY NOUN COMMON M SING) [1;DET+DEF-ARG]
;        3 runs (RUN VERB MAIN IND PRES 3 SING) [0;TOP-VERB]
;        4 . (#\. PUNCT) [3;END]
;        ********* FRASE EXAMPLE-2 *********************
;        1 He (HE PRON PERS M SING) [2;VERB-SUBJ]
;        2 runs (RUN VERB MAIN IND PRES 3 SING) [0;TOP-VERB]
;        3 fast (FAST ADV MANNER) [2;ADVB-RMOD-MEANSMANNER]
;        4 . (#\. PUNCT) [2;END]
; 3. A "flat" AVM format, where the various infos are represented
;    as <feature value> pairs, but the tree infos are still represented
;    as pointers to parents. I report here only the first sentence above
;    It has to be noted that this format exists only here. Inside the parser,
;    the tree information is still kept on a separate list
;        ( ( (posit 1) 
;            (form The) 
;            (coref NIL)
;            (sem NIL)
;            (syn ((lemma THE) (cat ART) (type DEF) (gender ALLVAL) (number ALLVAL)))
;            (tree ((parent 3) (label VERB-SUBJ))))
;          ( (posit 2) 
;            (form boy) 
;            (coref NIL)
;            (sem NIL)
;            (syn ((lemma BOY) (cat NOUN) (type COMMON) (gender M) (number SING)))
;            (tree ((parent 1) (label DET+DEF-ARG))))
;          ( (posit 3) 
;            (form runs) 
;            (coref NIL)
;            (sem NIL)
;            (syn ((lemma RUN) (cat VERB) (type MAIN) (mood IND) (tense PRES) (person 3) (number SING)))
;            (tree ((parent 0) (label TOP-VERB))))
;          ( (posit 4) 
;            (form .) 
;            (coref NIL)
;            (sem NIL)
;            (syn ((lemma #\.) (cat PUNCT)))
;            (tree ((parent 3) (label END)))) )
;    The "coref" value is empty in this example, The "sem" value is always empty before the semantic
;    procedures are applied. The "syn" features change according to the category and type.
; 4. The full AVM format, in which a tree is a list of nested items.
;        ((head 
;            ((form runs) 
;             (coref NIL)
;             (syn ((lemma RUN) (cat VERB) (type MAIN) (mood IND) (tense PRES) (person 3) (number SING)))
;             (sem NIL)
;             (link TOP-VERB)
;             (position 3)))
;         (dependents 
;            ((head 				<--------------- FIRST SUBTREE
;               ((form The) 
;                (coref NIL)
;                (syn ((lemma THE) (cat ART) (type DEF) (gender ALLVAL) (number ALLVAL)))
;                (sem NIL)
;                (link VERB-SUBJ)
;                (position 1)))
;             (dependents 
;                ((#)
;                 (head 
;                   ((form boy) 
;                    (coref NIL)
;                    (syn ((lemma BOY) (cat NOUN) (type COMMON) (gender M) (number SING)))
;                    (sem NIL)
;                    (link DET+DEF-ARG)
;                    (position 2)))
;                 (dependents 
;                    ((#))))))
;            (#)				<--------------- POSITION OF THE ROOT
;            ((head 				<--------------- SECOND SUBTREE
;                ((form .) 
;                 (coref NIL)
;                 (syn ((lemma #\.) (cat PUNCT)))
;                 (sem NIL)
;                 (link END)
;                 (position 4)))
;              (dependents 
;                    ((#))))))
; *** The function "reshuffle-tree" makes the conversion from 3 to 4 for a set of trees
;
;******************************************************************
; *** takes as input the name of a file containing trees represented in flat 
;     AVM format (3 above) and produces a file of full AVM trees (4 above)
(defun file-reshuffle-tree (flatfname actfname simpactfname)
  (declare (special *print-level* *print-length*))
   (setq *print-level* nil)
   (setq *print-length* nil)
   (with-open-file (flatavmport flatfname :direction :input
                                       :if-does-not-exist :error
                                       :external-format :utf8)
     (with-open-file (actavmport actfname :direction :output
                                    :if-exists :overwrite
                                    :if-does-not-exist :create
                                    :external-format :utf8)
       (with-open-file (simpleactavmport simpactfname :direction :output
                                    :if-exists :overwrite
                                    :if-does-not-exist :create
                                    :external-format :utf8)
        (let (avmtree linebuff (start t) iline emptybuff)
         (do ((line (read-line flatavmport nil #\Escape)
                    (read-line flatavmport nil #\Escape)))
             ((equal line #\Escape)
                (setq avmtree (singsent-reshuffle-tree (reverse linebuff)))
                (format actavmport "~s~%" avmtree)
                (print-actavm-readable avmtree simpleactavmport))
       ; *** process lines ***********
            (cond ((string= (string-trim '(#\Space #\Tab #\Return) line) "")
                     (setq emptybuff (cons line emptybuff)))
                  (t (setq iline (read-from-string line))
                     (cond ((listp iline)
                              (setq linebuff (cons iline linebuff)))
                           ((same-chars? iline #\?)
                              (setq emptybuff (cons line emptybuff)))
                           ((is-sentence-heading line nil)
                             (cond ((not start)
                                     (setq avmtree (singsent-reshuffle-tree (reverse linebuff)))
                                     (format actavmport "~s~%" avmtree)
                                     (print-actavm-readable avmtree simpleactavmport)
                                     (dolist (nxtline (reverse emptybuff))
                                         (format actavmport "~a~%" nxtline)
                                         (format simpleactavmport "~a~%" nxtline))
                                     (format actavmport "~a~%" line)
                                     (format simpleactavmport "~a~%" line)
                                     (setq emptybuff nil)
                                     (setq linebuff nil))
                                   (t (setq start nil)
                                     (dolist (nxtline (reverse emptybuff))
                                         (format actavmport "~a~%" nxtline)
                                         (format simpleactavmport "~a~%" nxtline))
                                     (setq emptybuff nil)
                                     (format actavmport "~a~%" line)
                                     (format simpleactavmport "~a~%" line)))))))))))))

;******************************************************************
; *** takes as input a list of trees represented in flat AVM format (3 above) and
;     produces a list of full AVM trees (4 above)
(defun reshuffle-tree (synt-trees)
  (mapcar #'singsent-reshuffle-tree synt-trees))

;******************************************************************
(defun from-tut-to-flatavm (prsfile favfile)
  (declare (special *print-pretty*))
  (setq *print-pretty* nil)
   (with-open-file (parsedport prsfile :direction :input
                                       :if-does-not-exist :error
                                       :external-format :utf8)
     (with-open-file (flatavmport favfile :direction :output
                                    :if-exists :overwrite
                                    :if-does-not-exist :create
                                    :external-format :utf8)
       (let (iline)
        (do ((line (read-line parsedport nil #\Escape)
                   (read-line parsedport nil #\Escape)))
            ((equal line #\Escape)
               (setq *print-pretty* t))
      ; *** process lines ***********
           (cond ((or (string= (string-trim '(#\Space #\Tab #\Return) line) "")
                      (same-chars? (read-from-string line) #\?)
                      (is-sentence-heading line nil))
                   (format flatavmport "~a~%" line))
                 (t (setq iline (interp-newtb-line line))
                    (let ((position (get-newtb-numb iline))
                          (inpword (get-newtb-inpword iline))
                          (lemma (get-newtb-word iline))
                          (categ (get-newtb-categ iline))
                          (treeinfo (fourth iline))
                          avmtree avmsynt avmsem vderiv mood case semtype
                          value corefinfo avmline hour minute second)
 ; *** collection of the syntactic infos
                        (setq avmsynt `((lemma ,lemma) (cat ,categ)))
                        (cond ((memq categ '(NOUN ADJ ART PRON))
                                (setq avmsynt
                                     (append avmsynt 
                                         `((type ,(get-newtb-type iline))
                                           (gender ,(get-newtb-gender iline))
                                           (number ,(get-newtb-number iline)))))))
                        (cond ((eq categ 'NOUN)
                                (setq vderiv (get-newtb-vderiv iline))
                                (cond ((not (null vderiv))
                                        (setq avmsynt
                                             (append avmsynt 
                                                 `((v-deriv ,vderiv)
                                                   (v-trans ,(get-newtb-vtrans iline)))))))))
                        (cond ((eq categ 'VERB)
                                (setq mood (get-newtb-mood iline))
                                (cond ((memq mood '(infinite gerund))
                                         (setq avmsynt
                                            (append avmsynt 
                                                `((mood ,mood) 
                                                  (tense ,(get-newtb-tense iline))
                                                  (trans ,(get-newtb-subcat iline))))))
                                      ((eq mood 'participle)
                                         (setq avmsynt
                                            (append avmsynt 
                                                `((mood ,mood)
                                                  (tense ,(get-newtb-tense iline))
                                                  (trans ,(get-newtb-subcat iline))
                                                  (number ,(get-newtb-number iline))
                                                  (gender ,(get-newtb-gender iline))))))
                                      (t (setq avmsynt
                                            (append avmsynt 
                                                `((mood ,mood)
                                                  (tense ,(get-newtb-tense iline))
                                                  (trans ,(get-newtb-subcat iline))
                                                  (person ,(get-newtb-person iline))
                                                  (number ,(get-newtb-number iline)))))))))
                        (cond ((eq categ 'PRON)
                                (setq case (get-newtb-cases iline))
                                (cond ((not (null case))
                                         (setq avmsynt (append1 avmsynt `(case ,case)))))))
                        (cond ((is-a-synt-locution iline)
                                (setq avmsynt (append1 avmsynt `(locution t)))))
 ; *** collection of the semantic infos
                        (setq semtype (get-newtb-semtype iline))
                        (cond ((not (null semtype))
                                 (setq avmsem (append1 avmsem `(semtype ,semtype)))))
                        (cond ((eq categ 'NUM)
                                 (setq value (get-newtb-value iline))
                                 (cond ((not (null value))
                                          (setq avmsem (append1 avmsem `(value ,value)))))))
                        (cond ((eq categ 'DATE)
                                 (setq value (get-newtb-year iline))
                                 (cond ((not (null value))
                                          (setq avmsem (append1 avmsem `(year ,value)))))))
                        (cond ((eq categ 'HOUR)
                                 (setq hour (get-newtb-dayhour iline))
                                 (setq minute (get-newtb-dayminute iline))
                                 (setq avmsem 
                                      (append avmsem 
                                            `((dayhour ,hour) (dayminute ,minute))))
                                 (setq second (get-newtb-daysecond iline))
                                 (cond ((not (null second))
                                          (setq avmsem 
                                             (append1 avmsem `(daysecond ,second)))))))
 ; *** information about the tree structure
                        (setq avmtree
                            `((parent ,(first treeinfo)) (label ,(second treeinfo))))
 ; *** construction of the line
                        (setq avmline
                             `((posit ,position)
                               (form ,inpword)
                               (syn ,avmsynt)
                               (sem ,avmsem)
                               (tree ,avmtree)))
 ; *** possible insertion of trace information
                        (cond ((is-a-newtb-trace? iline)
                                 (setq corefinfo (fifth iline))
                                 (cond ((and (not (null corefinfo))
                                             (neq corefinfo 'empty))
                                          (setq avmline 
                                              (append1 avmline
                                                  `(coref ((cline ,(first corefinfo))
                                                           (ctype ,(second corefinfo)))))))
                                       (t (setq avmline 
                                              (append1 avmline (list 'coref nil)))))))
 ; *** final output
                        (format flatavmport "~s~%" avmline)))))))))

;******************************************************************
;     This does the word on each single sentence
; *** starting from the root, it proceeds recursively downward
;     in order to collect all the daughters of each node
; *** the third argument serves to keep the whole tree
(defun singsent-reshuffle-tree (synt-tree)
   (build-actual-tree (find-tempavm-root synt-tree) synt-tree synt-tree))

(defun build-actual-tree (root synt-tree whole-tree)
  (let* ((oldcoref (get-flatavm-corefinfo root))
         newnode
         (headcat (get-flatavm-categ root))
         ;(headgender (get-flatavm-gender root))
         (headnumber (get-flatavm-number root))
         (syntinfo (get-flatavm-syntinfo root))
         (dependents (find-tempavm-alldeps root synt-tree)))
      (cond ((and (memq headcat '(noun pron adj art num verb))
                  (or (null headnumber) (eq headnumber 'allval)))
          ;    (setq headnumber (determine-tempavm-number root dependents whole-tree))
              (cond ((not (null headnumber))
                       (setq syntinfo 
                           (replace-flatavm-feat-val 
                               (cons (list (first (first syntinfo))
                                           (second (first syntinfo)))
                                     (rest syntinfo))
                               'number headnumber))))))
      (setq newnode
            (cond ((null oldcoref)
                    `((head ((form ,(get-flatavm-inpword root))
                      (position ,(get-flatavm-numb root))
                      (syn ,syntinfo)
                      (link ,(get-tempavm-label root))
                      (sem ,(get-flatavm-seminfo root))))))
                  (t `((head ((form ,(get-flatavm-inpword root))
                      (position ,(get-flatavm-numb root))
                      (syn ,syntinfo)
                      (coref ,(get-flatavm-corefinfo root))
                      (link ,(get-tempavm-label root))
                      (sem ,(get-flatavm-seminfo root))))))))
   ;(break "avm-transf")
      (cond ((null dependents)
               newnode)
            (t (append1 newnode 
                   `(dependents ,(int-build-act-tree 
                                    dependents synt-tree whole-tree)))))))

; *** this repeats on a list of nodes (the dependents of another node)
;     the construction of the tree
; *** it takes into account the (#\#) marker of the position of the head
(defun int-build-act-tree (nodelist synt-tree whole-tree)
  (cond ((null nodelist) nil)
        ((equal (first nodelist) '(#\#))
           (cons '(#\#) (int-build-act-tree (rest nodelist) synt-tree whole-tree)))
        (t 
           (cons (build-actual-tree (first nodelist) synt-tree whole-tree)
                 (int-build-act-tree (rest nodelist) synt-tree whole-tree)))))
       
; ***************************************************************************
;      FUNCTIONS FOR GETTING INFOS FROM A FLAT AVM TREE
; *** These functions concern movements on the tree, while the basic functions
;     for getting feature values from a flat AVM tree are in MORPHO/tb-functions
; ***************************************************************************

; *** the next three functions are anomalous, since they are used just in the
;     transition from flatavm to full avm. In fact, "mergeresult" (defined in
;     PROC-ALL/top.level-fun) puts together the line and the links (which are
;     kept separate in the parser also for the flat avm representation),
;     by including the "tree" feature in the representation, which has, as
;     subfeatures, "parent" and "label"
(defun get-tempavm-parent (line)
  (get-flatavm-feat-val (get-flatavm-feat-val line 'tree) 'parent))
      ; *** get-flatavm-feat-val in tb-functions
 
(defun get-tempavm-label (line)
  (get-flatavm-feat-val (get-flatavm-feat-val line 'tree) 'label))
      ; *** get-flatavm-feat-val in tb-functions
 
(defun find-tempavm-root (tree)
; *** the root is defined as a word having a 0 parent pointer
 (let (found)
  (do ((nxtword (first tree) (first tree))
       (tree (rest tree) (rest tree)))
      ((or found (null nxtword))
        (cond (found found)
              (t (exception 'semantic-error "PROC/avm-transf; tree without root in seminterp"))))
      (cond ((eq 0 (get-tempavm-parent nxtword)) (setq found nxtword))))))

; *******************************************************************
; *** finds all lines (within the lines 'allines', which need not be
;     all the lines of a sentence, but could be a piece of it) that
;     have a link pointing to 'headline'
; *** among the dependents there is the (#\#) element, marking the
;     position of the head
(defun find-tempavm-alldeps (headline alllines)
 (let ((headnumb (get-flatavm-numb headline))
       deps)
  (do ((nxtline (first alllines) (first alllines))
       (alllines (rest alllines) (rest alllines)))
     ((null nxtline) deps)
  ; *** if a line points to the head or is the head, add infos,
  ;     otherwise do nothing
     (cond ((equal (get-tempavm-parent nxtline) headnumb)
             (setq deps (append1 deps nxtline)))
           ((equal (get-flatavm-numb nxtline) (get-flatavm-numb headline))
             (setq deps (append1 deps '(#\#))))))))

; ***************************************************************************
; *** it looks around a node, whose number is undefined (nil or allval)
;     to see if the number can be determined looking at the parent or at
;     the dependents. This is made just one level up or down.
(defun determine-tempavm-number (node dependents whole-tree)
  (let* ((parent (find-tempavm-line (get-tempavm-parent node) whole-tree))
         (categ (get-flatavm-categ node))
         (parent-categ (get-flatavm-categ parent))
         (parent-number (get-flatavm-number parent))
         (truedep (remove-head-marker dependents))
         found nxtdep-numb)
   (cond ((eq parent-categ 'VERB)
  ; *** if this is the subject of a verb, its number is the same of the verb
  ;     if any
            (cond ((and (memq (get-tempavm-label node) '(VERB-SUBJ AUX))
                        (memq parent-number '(SING PL)))
                    (setq found parent-number))))
  ; *** for any other parent category, enforce agreement, if the number is available
  ;     for the parent
         ((memq parent-number '(SING PL))
            (setq found parent-number)))
  ; *** if not found try to get it from dependents (if found, the loop is exited
  ;     immediately)
   (do* ((nxtdep (first truedep) (first truedep))
         (truedep (rest truedep) (rest truedep)))
     ((or found (null nxtdep))
         found)
    (setq nxtdep-numb (get-flatavm-number nxtdep))
    (cond ((eq categ 'VERB)
  ; *** if the node whose number is unknown is a verb, and the next dependent is
  ;     its subject, the number of the verb is the same as the number of the subject,
  ;     if any
             (cond ((and (memq (get-tempavm-label nxtdep) '(VERB-SUBJ AUX))
                         (memq nxtdep-numb '(SING PL)))
                     (setq found nxtdep-numb))))
  ; *** if the node whose number is unknown is not a verb, then any dependent for
  ;     which there is an available number is ok
          ((memq nxtdep-numb '(SING PL))
                     (setq found nxtdep-numb))))))

; ***************************************************************************
; *** it looks around a node, whose number is undefined (nil or allval)
;     to see if the number can be determined looking at the parent or at
;     the dependents. This is made just one level up or down.
(defun find-tempavm-line (position whole-tree)
 (let (found)
  (cond ((eq position 0) nil)
        (t (do ((nxtline (first whole-tree) (first whole-tree))
                (whole-tree (rest whole-tree) (rest whole-tree)))
               ((or (null nxtline) found)
                 (cond (found found)
                       (t (exception 'semantic-error 
                             "PROC/avm-transf: search for nonexistent line: main" position))))
               (cond ((equal (get-flatavm-numb nxtline) position)
                        (setq found nxtline))))))))

; *******************************************************************
; *** removes from a list of dependents the head marker (#\#)
;     type, i.e. that are continuations of locutions
(defun remove-head-marker (dependents)
  (cond ((null dependents) nil)
        ((equal (first dependents) '(#\#)) (rest dependents))
        (t (cons (first dependents) (remove-head-marker (rest dependents))))))

; ***************************************************************************
;      FUNCTIONS FOR GETTING INFOS FROM A TRUE AVM TREE
;      [these are marked by the "actavm" component in the function name]
; ***************************************************************************

; *******************************************************************
; *** general function for reading feature values
(defun get-actavm-featval (feature tree)
  (case feature
      (link (get-actavm-headlink tree))
      (cat (get-actavm-headcateg tree))
      (type (get-actavm-headtype tree))
      (lemma (get-actavm-headlemma tree))
      (lexmean (get-actavm-headlexmean tree))
      (otherwise (exception 'semantic-error
                           "PROC/seminterp: Search condition on unknown feature: seminterp"
                           feature))))

; *** access to the main components: head and dependents
(defun get-actavm-head (avmtree)
   (second (assoc 'head avmtree)))

(defun get-actavm-dependents (avmtree)
   (second (assoc 'dependents avmtree)))

; *******************************************************************
; *** the next is as get-actavm-dependents, but in case the head of the tree is
;     a full trace, it returns the dependents of the referent (if any) of the trace
(defun get-actavm-deps-with-traces (tree &optional traces)
  (declare (special *full-tree*))
  (let ((result (leggi tree 'dependents)))
      (cond ((null result)
              (cond ((and traces
                          (is-a-actavm-trace? tree)
                          (not (null (get-actavm-headsyn tree)))) ; *** it is null for traces inserted
                                                               ;     during the annotation
                       (let ((referent (find-coreferent (get-actavm-headcorefline tree)
                                                        (list *full-tree*))))
                          (first (leggi referent 'dependents))))))
            (t (first result)))))

; *** access to the internal features of the head (root of the tree): first level
(defun get-actavm-headform (avmtree)
   (first (leggi (get-actavm-head avmtree) 'form)))

(defun get-actavm-headnumb (avmtree)
   (first (leggi (get-actavm-head avmtree) 'position)))

(defun get-actavm-headlinumb (tree)
  (let ((pos (get-actavm-headnumb tree)))
     (cond ((numberp pos) pos)
           (t (first pos)))))

(defun get-actavm-headcoref (avmtree)
  (first (leggi (get-actavm-head avmtree) 'coref)))

(defun get-actavm-headsyn (avmtree)
   (first (leggi (get-actavm-head avmtree) 'syn)))

(defun get-actavm-headsem (avmtree)
   (first (leggi (get-actavm-head avmtree) 'sem)))

(defun get-actavm-headlink (avmtree)
   (first (leggi (get-actavm-head avmtree) 'link)))

; *** access to the internal features of the head (root of the tree): second level
(defun get-actavm-headcorefline (avmtree)
  (let ((corefinfo (get-actavm-headcoref avmtree)))
      (cond ((eq 'empty corefinfo) nil)
            (t (first (leggi corefinfo 'line))))))

(defun get-actavm-headcoreftype (avmtree)
  (let ((corefinfo (get-actavm-headcoref avmtree)))
      (cond ((eq 'empty corefinfo) nil)
            (t (first (leggi corefinfo 'ctype))))))

(defun get-actavm-headlemma (avmtree)
   (first (leggi (get-actavm-headsyn avmtree) 'lemma)))

(defun get-actavm-headcateg (avmtree)
   (first (leggi (get-actavm-headsyn avmtree) 'cat)))

(defun get-actavm-headtype (avmtree)
   (first (leggi (get-actavm-headsyn avmtree) 'type)))

(defun get-actavm-headgender (avmtree)
   (first (leggi (get-actavm-headsyn avmtree) 'gender)))

(defun get-actavm-headnumber (avmtree)
   (first (leggi (get-actavm-headsyn avmtree) 'number)))

(defun get-actavm-headperson (avmtree)
   (first (leggi (get-actavm-headsyn avmtree) 'person)))

(defun get-actavm-headmood (avmtree)
   (first (leggi (get-actavm-headsyn avmtree) 'mood)))

(defun get-actavm-headcase (tree)
  (first (leggi (get-actavm-headsyn tree) 'case)))

(defun get-actavm-headlexmean (avmtree)
   (first (leggi (get-actavm-headsem avmtree) 'lexmean)))

(defun get-actavm-ext-headlexmean (tree)
  (get-actavm-headlexmean (skip-question-tense-marker tree)))

(defun get-actavm-headvalue (avmtree)
   (first (leggi (get-actavm-headsem avmtree) 'value)))

(defun get-actavm-headlexident (avmtree)
   (first (leggi (get-actavm-headsem avmtree) 'ident)))

(defun get-actavm-headyear (avmtree)
   (first (leggi (get-actavm-headsem avmtree) 'year)))

(defun get-actavm-headdayhour (avmtree)
   (first (leggi (get-actavm-headsem avmtree) 'dayhour)))

(defun get-actavm-headdayminute (avmtree)
   (first (leggi (get-actavm-headsem avmtree) 'dayminute)))

(defun get-actavm-headdaysecond (avmtree)
   (first (leggi (get-actavm-headsem avmtree) 'daysecond)))

; *******************************************************************
; *** returns true if the root of tree is a trace
;     is-a-synt-trace? in tb-functions
(defun is-a-actavm-trace? (tree)
  (and (not (equal tree '(#\#)))
       (is-a-synt-trace? (get-actavm-head tree))))

; ***************************************************************************
; *** is this a trace whose referent is inside the sentence?
;     they are identified by having a coref index not empty
(defun is-a-coref-trace? (avmtree)
  (let ((coref (get-actavm-headcorefline avmtree)))
    (and (not (null coref)) (neq coref 'empty))))

; ***************************************************************************
; *** is this a trace whose referent is outside the sentence?
;     they are identified by having an empty coref index
(defun is-an-anaph-trace? (avmtree)
  (let ((coref (get-actavm-headcorefline avmtree)))
    (or (null coref) (eq coref 'empty))))

 ; ***************************************************************************
; *** returns the subtree of one of the trees in treeset, which has, as an
;     immediate dependent localtree
(defun find-actavm-parent (localtree treeset)
   (cond ((null treeset) nil)
         ((equal (first treeset) '(#\#))
            (find-actavm-parent localtree (rest treeset)))
         ((member localtree (get-actavm-dependents (first treeset)) :test #'equal)
            (first treeset))
         (t (let ((firstsubtree
                     (find-actavm-parent localtree
                          (get-actavm-dependents (first treeset)))))
               (cond ((null firstsubtree)
                         (find-actavm-parent localtree (rest treeset)))
                     (t firstsubtree))))))

; *******************************************************************
; *** finds a descendant such that the "path" of features feat-path
;     assumes the values given in feat-vals
; *** deps is a list of nodes, which are the roots of the subtrees
;     within which the search has to be made
; *** feat-path --> (constr1 constr2 ... constrN)
(defun find-actavm-descendant (feat-path feat-vals deps)
  (cond ((null deps) nil)
   ; *** end of horizontal movement: nothing found
        ((equal (first deps) '(#\#))
   ; *** the next dependent is the mark of the position of the head:
   ;     go ahead
           (find-actavm-descendant feat-path feat-vals (rest deps)))
        ((equal (get-actavm-headlink (first deps)) 'PRON-RMOD-LOC+METAPH)
   ; *** pron-rmod-loc+metaph is skipped unless it is explicitly asked for
           (cond ((and (equal feat-path '(LINK))
                       (member 'PRON-RMOD-LOC+METAPH feat-vals))
                    (first deps))
                 (t (find-actavm-descendant feat-path feat-vals (rest deps)))))
        ((check-treefeat-condit
           (inlist (first feat-path))
           (inlist (first feat-vals))
           (first deps))
   ; *** first element of search condition satisfied
          (cond ((null (rest feat-path))
       ; *** end of search conditions: element found
                   (first deps))
       ; *** otherwise: continue the search on remaining conditions
                (t (find-actavm-descendant
                        (rest feat-path)
                        (rest feat-vals)
                        (get-actavm-dependents (first deps))))))
   ; *** first element of search condition not satisfied: try
   ;     next dependent
        (t (find-actavm-descendant feat-path feat-vals (rest deps)))))

; *******************************************************************
; *** checks if, for "node", all the features in featl have the values
;     in valsl. featl and valsl are assumed to be parallel lists
(defun check-treefeat-condit (featl valsl node)
  (cond ((null featl) t)
        ((label-eq (get-actavm-featval (first featl) node)
             (first valsl) (first featl))
          (check-treefeat-condit (rest featl) (rest valsl) node) )
        (t nil)))

; *******************************************************************
; *** finds a dependent which is linked upward via the label "lab"
(defun label-eq (act-val test-val feat)
  (cond ((eq feat 'link)
           (lab-subsumes test-val act-val))
        (t (eq act-val test-val))))

; *******************************************************************
; *** finds a dependent which is linked upward via the label "lab"
;     It must be a dependent of the root of the tree
; *** The optional allconjuncts can be t, in which case the function
;     returns not only the found tree, but a including it plus all
;     its subtree acting as conjuncts of it
; *** If lab=VERB-OBJ
;     tree=
;      mangia
;      |<--VERB-SUBJ-- Luigi
;      |<--VERB-OBJ-- pere
;                     |<--COORD-- ,
;                                 |<--COORD2ND--mele
;                                               |<--COORD-- e
;                                                           |<--COORD2ND--banane
;     If allconjuncts is nil, the result is:
;      pere
;       |<--COORD-- ,
;                   |<--COORD2ND--mele
;                                 |<--COORD-- e
;                                             |<--COORD2ND--banane
;     If allconjuncts is t, the result is:
;      (pere
;       |<--COORD-- ,
;                   |<--COORD2ND--mele
;                                 |<--COORD-- e
;                                             |<--COORD2ND--banane
;       mele
;       |<--COORD-- e
;                   |<--COORD2ND--banane
;       banane)
(defun find-actavm-dep (label tree &optional allconjuncts)
   (mult-find-actavm-dep (inlist label) tree allconjuncts))

(defun mult-find-actavm-dep (labels tree allconjuncts)
 (let (found)
   (do ((nxtlab (first labels) (first labels))
        (labels (rest labels) (rest labels)))
       ((or (null nxtlab) found)
          (cond ((null allconjuncts) found)
                (t (find-all-conjuncts found))))
      (setq found (find-actavm-descendant '(link) (list nxtlab)
                                   (get-actavm-deps-with-traces tree t))))))

; *******************************************************************
; *** the optionals enables this function to be used both for building
;     "restrictions", that include a restriction type and an upper tree
;     and simple conjuncts, which are just trees
; *** the restrictions are used in buildquery
(defun find-all-conjuncts (tree &optional up-subtree restrtype)
 (let (restrs)
  (do ((nxtconj tree (find-bq-conjunct nxtconj)))
      ((null nxtconj) (reverse restrs))
      (setq restrs
          (cond ((null restrtype)
                   (cons nxtconj restrs))
                (t (cons (list restrtype nxtconj (get-actavm-head up-subtree)) restrs)))))))

; *******************************************************************
; *** this is the same as above, but in case we are positioned on the top
;     empty "about" node (i.e. the --about- empty prep associated automatically
;     with "give-info"), we first move down one level.
;     This is because in case of "Tomorrow it will rain and after tomorrow there will be
;     a sunny day", the conjunction (and) is linked to "will rain", and not to the empty
;     prep
(defun find-obj-all-conjuncts (subtree &optional up-subtree restrtype)
   (cond ((eq (get-actavm-headlexmean subtree) '--about-relation)
            (find-all-conjuncts (find-actavm-dep 'PREP-ARG subtree) subtree restrtype))
         (t (find-all-conjuncts subtree up-subtree restrtype))))

; *******************************************************************
; *** this checks if, among the dependents of the head of tree, there is a
;     coordinating conjunction, and returns the subtree which is the
;     second conjunct
(defun find-bq-conjunct (tree)
  (let ((deps (get-actavm-deps-with-traces tree)) found)
    (do ((nxtdep (first deps) (first deps))
         (deps (rest deps) (rest deps)))
        ((or found (null nxtdep)) found)
        (cond ((equal nxtdep '(#\#)) nil)
              ((and (eq (get-actavm-headlink nxtdep) 'COORD)
                    (or (and (eq (get-actavm-headcateg nxtdep) 'CONJ)
                             (eq (get-actavm-headtype nxtdep) 'COORD))
                        (eq (get-actavm-headcateg nxtdep) 'PUNCT)))
                 (setq found (find-actavm-dep 'COORD2ND nxtdep)))))))

; ****************************************************************************
;     END OF FUNCTIONS FOR ACCESSING THE "TRUE AVM" REPRESENTATION
; ****************************************************************************
; *** This reads the tree, and produces a printable representation in the form:
;     ( (indent1 (form1 position1 lemma1 cat1 type1))
;       (indent2 (form2 position2 lemma2 cat2 type2))
;               ...
;     )
(defun build-actavm-readable (avmtree indent)
  (let (deps res fres)
    (cond ((null avmtree) nil)
          (t (setq deps (get-actavm-dependents avmtree))
             (setq res (mapcar #'(lambda (x) (list-actavm-dep x avmtree indent)) deps))
             (setq fres (flatten res))
            ; (format t "~a~%    ~a~%" res fres)
            ; (break "")
             fres
            ))))

(defun list-actavm-dep (dep avmtree indent)
  (let (meaning ident semcomp res)
  (cond ((or (eq dep #\#) (equal dep '(#\#)))
           (setq meaning (get-actavm-headlexmean avmtree))
           (setq ident (get-actavm-headlexident avmtree))
           (cond ((not (null ident))
                    (setq semcomp (list meaning ident)))
                 ((not (null meaning))
                    (setq semcomp meaning)))
           (setq res (list (get-actavm-headform avmtree)
                           (get-actavm-headnumb avmtree)
                           (get-actavm-headlemma avmtree)
                           (get-actavm-headcateg avmtree)
                           (get-actavm-headtype avmtree)))
       ;    (cond ((null semcomp) 
       ;             (list indent (append1 res (get-actavm-headlink avmtree))))
       ;          (t 
             (list indent (append res (list semcomp (get-actavm-headlink avmtree))))
       ;     ))
       )
        (t (build-actavm-readable dep (1+ indent))))))

(defun print-actavm-readable (avmtree outport)
  (declare (special *print-pretty*))
  (setq *print-pretty* nil)
   (let ((avmreadable (build-actavm-readable avmtree 0)))
       (do ((nxtindent (first avmreadable) (first avmreadable))
            (nxtwrd (second avmreadable) (second avmreadable))
            (avmreadable (rest (rest avmreadable)) (rest (rest avmreadable))))
          ((null nxtindent))
          (do ((indc nxtindent (1- indc)))
              ((<= indc 0))
              (format outport "  "))
          (format outport "~a~%" nxtwrd)))
  (setq *print-pretty* t))

