(in-package "USER")

(defvar *PREV-SENTENCES* nil)
(defvar *TURN-TO-TALK* '§myself)

(defun go-main (&optional data-string expected-attrs)
 (declare (special *DIALOGUE-CONTEXT* *ONTOLOGY-CACHE* *PRINT-LEV*
                   *TREE-FORMAT* *SYSTEM-CONTEXT*))
;**************************************************************
; *** MAIN
;***********************************************************************
 (let (tempres result)
 (cond ((eq *SYSTEM-CONTEXT* 'hops-dial)
          (let (ris ontology-cache)
           (declare (special ontology-cache))
            (setq *TREE-FORMAT* 'avm)
            (format t "Do you want to start a dialogue (d) or to give a single sentence (s)?")
            (setq ris (check_risp '(d s) 0))
            (format t "Print level? ~%0: no print;~%1: parameter values;~%2: semantic query and param values;~%3: also syntactic trees~%")
            (setq *PRINT-LEV* (check_risp '(0 1 2 3 4) 0))
            (cond ((eq ris 'd)
    ; *** expected-attrs shouldnot be used in the dialogue manager, since it handles
    ;     the attributes by itself, but I need it for the tests
                    (format t "Expected attributes? (enclosed in parentheses or nil)~%")
                    (setq expected-attrs (read))
                    (catch 'generic-error (dialogue-manager expected-attrs)))
                  (t (format t "Do you want to build a query (q) or to extract parameters (p)?")
                    (setq ris (check_risp '(p q) 0))
                    (setq *DIALOGUE-CONTEXT* '((+REQUEST+)))
                    (format t "Sentence? ")
                  (catch 'generic-error
                    (start-hops (read-line) ris))))))
       ((eq *SYSTEM-CONTEXT* 'hops)
         (setq *TREE-FORMAT* 'avm)
         (setq *PRINT-LEV* 0)
         (setq tempres
            (catch 'generic-error (start-hops data-string)))
         (setq result (adjust-attributes 
                            (append (first tempres) (second tempres))
                            expected-attrs))
   ; *** in case of error, return NIL
   ;     a LOG file should be introduced !!!
         (cond ((stringp (first result)) nil)
               (t result)))
; *** GENERIC-ERRORS
		; "PROC/utilities: putprop to nil"
		; "PROC/utilities: add-prop-val to nil"
		; "PROC/top-level-fun: ana-text used in the HOPS context"
		; "PROC/tb-functions: extract-linenumb"
		; "PROC/tb-functions: extract-coref"
		; "PROC/tb-functions: extract-coref 1"
		; "PROC/tb-functions: extract-coref 2"
		; "PROC/tb-functions: extract-newtb-treeinfo 1"
		; "PROC/tb-functions: extract-newtb-treeinfo 2"
		; "PROC/tb-functions: Something after treeinfo, but no question marks"
		; "PROC/tb-functions: tb-cf-adjacent"
		; "PROC/tb-functions: Mood taken from a non-verbal component"
		; "PROC/tb-functions: Tense taken from a non-verbal component"
		; "PROC/tb-functions: Transitivity taken from a non-verbal component"
		; "PROC/tb-functions: Value taken from a non-number component"
		; "PROC/tb-functions: V-deriv taken from a non-nominal component"
		; "PROC/tb-functions: V-trans taken from a non-nominal component"
		; "PROC/tb-functions: Mood taken from a non-verbal component (avm)"
		; "PROC/tb-functions: Tense taken from a non-verbal component (avm)"
		; "PROC/tb-functions: Transitivity taken from a non-verbal component (avm)"
		; "PROC/tb-functions: Value taken from a non-number component (avm)"
		; "PROC/tb-functions: V-deriv taken from a non-nominal component (avm)"
		; "PROC/tb-functions: V-trans taken from a non-nominal component (avm)"
		; "PROC/tb-functions: get-synt-posit used for TUT"
		; "PROC/tb-functions: get-synt-corefinfo used for TUT"
		; "PROC/tb-functions: get-synt-seminfo used for TUT"
       ((eq *SYSTEM-CONTEXT* 'tocai)
         (catch 'generic-error (start-tocai)))
       ((memq *SYSTEM-CONTEXT* '(tule legal tocai-test atlas))
         (catch 'generic-error (tulemain))))))

;***********************************************************************
;**** this is the main function for the TULE environment (used to carry out 
;     text analysis on corpora). It decides which task to carry over
(defun tulemain ()
  (declare (special *SYSTEM-CONTEXT*))
  (let (ris)
   (do ()
    ((eq ris '$) 'bye)
     (format t " ~% Which of the options do you want to follow? ~%~%")
     (format t " 1. PARSING OF A TEXT FILE~%")
     (format t " 2. OTHER FUNCTIONS~%")
     (format t " 3. INTERPRETATION OF A SENTENCE~%")
     (format t " 4. HELP~%")
     (format t " $. Exit to Lisp~%")
     (setq ris (check_risp '(1 2 3 4 $) 0))
     (cond ((eq ris 1) (ana-and-tag-and-parse))
           ((eq ris 2)
             (format t " ~% Which special function? ~%~%")
             (format t " a. MORPHOLOGICAL ANALYSIS~%")
             (format t " b. PARSING EVALUATION ON AUTOMATICALLY TAGGED DATA~%")
             (format t " c. PARSING EVALUATION ON MANUALLY TAGGED DATA~%")
             (format t " d. PARSING EVALUATION ON MANUALLY TAGGED DATA WITH EXTENDED LABELS~%")
             (format t " e. PARSING FROM AN ALREADY TAGGED FILE~%")
             (format t " f. CONVERSION FROM TANLL FORMAT TO TUT FORMAT~%")
             (format t " g. COLLECT TAGGING RULE STATISTICS~%")
             (format t " h. HELP~%")
             (format t " %. Return to the previous question~%")
             (setq ris (check_risp '(a b c d e f %) 0))
             (cond ((eq ris 'a) (ana-text))  ; ana-text in PROC-ALL/top-level-functions
                   ((eq ris 'b) (ana-and-tag-and-parse-and-eval))
                   ((eq ris 'c) (parse-and-eval))
                   ((eq ris 'd) (parse-extended-and-eval))
                   ((eq ris 'e) (parse-from-tbfile))
                   ((eq ris 'f) (convert-conll-tut))		; in ../PARLI/conll-to-tut
                   ((eq ris 'g) (collect-tag-stat))
                   ((eq ris 'h) (tulehelp2))
                   ((eq ris '%) nil)))
           ((eq ris 3) 
              (cond ((eq *SYSTEM-CONTEXT* 'atlas)
                       (start-atlas))
                    (t (ana-and-tag-and-parse-and-seminterp 'i))))
           ((eq ris 4) (tulehelp))))))

(defun tulehelp ()
   (format t "~%  *********************************************************** ~% ~%")
   (format t " - With option 1 (PARSING) you are enabled to specify a plain text file~%")
   (format t "   or group of files which are then parsed in the Turin University~%")
   (format t "   Treebank format. The result is stored in a new file having the~%")
   (format t "   same name of the input file, but extension '.prs'~%")
   (format t " - With option 2 (INTERPRETATION) you are enabled to give a sentence~%")
   (format t "   via keyboard. This sentence is parsed in the AVM format, and then~%")
   (format t "   interpreted on the basis of the knowledge stored in the ontology~%")
   (format t " ******* MORE INFORMATION IN THE TULE/DESCR/MANUALs ***************~%"))

(defun tulehelp2 ()
   (format t "~%  *********************************************************** ~% ~%")
   (format t " - With option 'a' (MORPHOLOGY) you can type a list of words and get~%")
   (format t "   for them the list of all lemmas corresponding to those words~%")
   (format t "   This is useful for testing the morphological analyser and for~%")
   (format t "   checking the contents of the dictionary~%")
   (format t " - With option 'b' (TAGGING) you are enabled to give a sentence~%")
   (format t "   via keyboard and have it POS tagged. This is useful just for~%")
   (format t "   having the POS tagged file (same name of the input file (say XXX)), with~%")
   (format t "   extension '.tb' (i.e. XXX.tb), that can be manually corrected in order to~%")
   (format t "   subsequently run option d (see below) ~%")
   (format t " - With option 'c' (PARSING EVALUATION) you can test the performances~%")
   (format t "   of the parser (and of the POS tagger). This is achieved by comparing~%")
   (format t "   the 'XXX.prs' file that is obtained by parsing the input file with a~%")
   (format t "   'XXX.man' file that must contain the correct parse tree. Statistics~%")
   (format t "   of the errors (including also POS errors) appear in a 'XXX.err' file~%")
   (format t " - With option 'd' (PARSING EVALUATION ON CORRECTLY TAGGED FILE) you can~%")
   (format t "   test how well would work the parser if there were no POS errors~%")
   (format t "   This may be useful to verify if the POS errors 'propagate' around~%")
   (format t "   and produce many parser errors~%")
   (format t "~%  ********* SHORT DESCRIPTION OF THE PARSING PROCESS ******** ~% ~%")
   (format t " The following steps are applied to get the final result:~%")
   (format t " 1. The input file (a raw text) is submitted to the morphological~%")
   (format t "    analyser and to the search in the lexicon. The home directory~%")
   (format t "    of the input file is assumed to be *HOME-DIR*/../DATI ~%")
   (format t "    You will be asked the name of the file (ex. inpfile, alfa.dat)~%")
   (format t "    The result of the previous step is disambiguated by the tagger ~%")
   (format t "    Three new files are created: the first of them (with extension .tb)~%")
   (format t "    is the actual result of the tagger. The second one (with extension~%")
   (format t "    .tball) includes also the non-preferred readings. The third (with~%")
   (format t "    extension .sim) includes just the citation forms of the input words~%")
   (format t " 2. The result of the automatic tagger can be compared with the manual~%")
   (format t "    check of previous tagging analysis on the same file. This can be~%")
   (format t "    accomplished by storing the manual file (with same name and ~%")
   (format t "    extension .man) under the same directory. The output consists ~%")
   (format t "    in two more new files: the first of them (extension .ext - i.e. ~%")
   (format t "    extended) contains the merging of the manual and automatic files,~%")
   (format t "    in the following sense: If a word is tagged in the same way in ~%")
   (format t "    both inputs, then it is rewritten with possible new information~%")
   (format t "    present in the automatic file. Otherwise, if they are tagged~%")
   (format t "    differently, the manual tag is kept. Any comments, as well as~%")
   (format t "    the information on dependency, present in the .man file are ~%")
   (format t "    copied as such in the .ext file. The second file resulting ~%")
   (format t "    from this step has extension .dif, and stores the lines where~%")
   (format t "    some parsing discrepancy has been found between the automatic~%")
   (format t "    and the manual files.~%")
   (format t " 3. Different files obtained in the previous step 3 can be merged and~%")
   (format t "    sorted in order to collect all the patterns for each verb in a~%")
   (format t "    readable form.~%")
   (format t " 4. A further option will be to take an .ext file~%")
   (format t "    (see step 2) and to find out the label of the connecting arcs in~%")
   (format t "    Dependency Tree (grammatical relations).~%")
   (format t "~%  ********* ITERATIONS ******** ~% ~%")
   (format t " The options described above can be applied to any group of files, provided~%")
   (format t " that their names are stored in the file~%")
   (format t "      *HOME-DIR*/../DATI/tagcorpus.dat~%")
   (format t " ******* MORE INFORMATION IN THE TULE/DESCR/MANUALs ***************~%"))

;***********************************************************************
(defun start-hops (&optional data-string query-or-par)
 (declare (special *PRINT-LEV* *TREE-FORMAT* *SYSTEM-CONTEXT*))
 (let (tagres parseres new-tree semrepr result tempparse)
  ; *** the first COND aims at setting the input data:
  ;     if the context is "hops-dial", then the query must be provided via keyboard
  ;     in case a single query has to be analyzed and translated in a DB query
  ;     On the contrary, in a "p" (parameter) interaction, it is the dialogue manager
  ;     that gets the inputs, so that the "data-string" argument must be non-null
  ; *** If the context is "hops", then the main must put itself in a listening loop
  ;     in order to wait for input from the external stream
   (cond ((eq *SYSTEM-CONTEXT* 'hops-dial)
            (cond ((eq query-or-par 'q)
                     (cond ((null data-string)
			      (exception 'generic-error
                                 "PROC/main: call to start-hops in single sentence without data"))))
                  ((eq query-or-par 'p)
                     (cond ((null data-string)
                              (exception 'generic-error
                                 "PROC/main: sentence not provided by the dialogue manager"))))
                  (t (exception 'generic-error
                                 "PROC/main: Unknown interaction type parameter in HOPS-DIAL"))))
         ((eq *SYSTEM-CONTEXT* 'hops) nil))
   (setq tagres
         (catch 'morpho-error (hops-ana-text+tag data-string)))
		; "PROC/endings: Unknown language" *LANGUAGE*
		; "PROC/postagger: Unknown verbal type ambiguity" values
		; "PROC/postagger: Unknown person ambiguity in dis-same-cat" values
		; "PROC/postagger: Attempt to take an 'all' value in case the feature is not CAT" feature
		; "PROC/postagger: Use of the MULTIPLE feature with non null value" value
		; "PROC/postagger: single-prendi-word" sing-val
		; "PROC/postagger: sing-lex-dis-prendi" pos
		; "PROC/parsenumbers" fin
		; "PROC/parsenumbers; number-split 1" oper
		; "PROC/parsenumbers; number-split 2" oper
		; "PROC/tokenizer: Empty line in string input"
		; "PROC/tokenizer: in tok-outelem" extra
		; "PROC/top-level-fun; single-component ambiguity" interp
		; "PROC/top-level-fun: unknown word" word
		; "PROC/top-level-fun: Wrong system context" *SYSTEM-CONTEXT*
		; "PROC/tb-functions: Component not found (Sentence number, word):" sent-id comp
		; "PROC/tb-functions: Compound mismatch  (Sentence number, word):" sent-id nxtw
		; "PROC/tb-functions: In list-to-number"
        ; *** tagres is an avm without tree information
   (cond ((stringp (first tagres)) 
            (flat-analysis data-string))
         (t (setq tempparse
                  (catch 'parse-error (start-parse-sentences tagres)))
            (cond ((stringp (first tempparse))
                     (flat-analysis data-string))
                  (t (setq parseres (mergeresult tempparse))
		; "PROC/checkcond: Unknown 'range' argument"
		; "PROC/checkcond: Unknown condition code" oper
		; "PROC/checkcond: Problems with auxiliaries 3" head nearestaux
		; "PROC/checkcond: Unknown agreement feature" features
		; "PROC/checkcond: find-cf-prevline"
		; "PROC/checkcond: find-cf-prevlab"
		; "PROC/checkcond: check-link-label"
		; "PROC/chunk-parser: Unknown condition for 'precedes'"
		; "PROC/chunk-parser: subst-chunk-link"
		; "PROC/chunk-parser: find-chunk-word-1"
		; "PROC/chunk-parser: eval-parsecond" condit
		; "PROC/chunk-parser: test-singl-nw" (second condition)
		; "PROC/chunk-parser: my-find-aux 1" firstc
		; "PROC/chunk-parser: my-find-aux 2"
		; "PROC/chunk-parser: take-noun-dep"
		; "PROC/chunk-parser: Argument of even length in lab list"
		; "PROC/chunk-parser: move-kb-first"
		; "PROC/chunk-parser: get-unlinked-pos"
		; "PROC/chunk-parser: merge-sing-cf-assign"
		; "PROC/chunk-parser: Non-verbal conjunction"
		; "PROC/chunk-parser: Governed verb not found"
		; "PROC/chunk-parser: find-first-finite"
		; "PROC/chunk-parser: default-attach-verbs"
		; "PROC/chunk-parser: put-root-arc"
		; "PROC/chunk-parser: find-prec-lines"
		; "PROC/chunk-parser: Unknown condition in find-a-line" condition
		; "PROC/chunk-parser: change-lab"
		; "PROC/chunk-parser: mult-change-lab"
		; "PROC/chunk-parser: find-locution-beg"
		; "PROC/chunk-parser: find-name-beg"
		; "PROC/chunk-parser: Misalignment in buff-data" line nxtsent
		; "PROC/hier-funct: null condition" patt
		; "PROC/hier-funct: Non basic transf origin" patt
		; "PROC/subc-hier: get-lab-ancestors 1"
		; "PROC/subc-hier: get-lab-ancestors 2"
        ; *** parseres is an avm with tree information;
        ;     mergeres is needed, since the result of parse-sentences is
        ;     given in form of two separate list: the word list ant the link list
                     (cond ((and (eq *SYSTEM-CONTEXT* 'hops-dial)
                                 (= *PRINT-LEV* 4))
                             (format t "PARSERES: ~s ~%" parseres)
                             (break "")))
                     (setq new-tree 
                           (catch 'semantic-error (reshuffle-tree parseres)))
                     (cond ((stringp (first new-tree))
                              (flat-analysis data-string))
                           (t (cond ((and (eq *SYSTEM-CONTEXT* 'hops-dial)
                                          (>= *PRINT-LEV* 3))
                                      (format t "PARSERES (NEW FORMAT): ~s ~%" new-tree)
                                      (break "")))
                              (setq semrepr 
                                (catch 'semantic-error (semantic-interpretation new-tree)))
		; "PROC/conc-funct: Incorrect date: " day month year
		; "PROC/conc-funct: get-next-spec" (first eq-spec)
		; "PROC/avm-transf; tree without root in seminterp"
		; "PROC/avm-transf: search for nonexistent line: main" position
		; "PROC/buildquery: unknown argument for ££want" (get-tree-headlexmean obj-subtree)
		; "PROC/buildquery: unknown argument for ££tell"
		; "PROC/buildquery: unknown argument for ££can"
		; "PROC/buildquery: Unknown argument for ££see"
		; "PROC/buildquery: Undefined top-level operator"
		; "PROC/buildquery: No topic subtree found"
		; "PROC/buildquery: No topic found" (get-tree-headlemma topic-subtree)
		; "PROC/buildquery: Unknown preposition in build-sem-query" (get-tree-headlemma topic-subtree)
		; "PROC/buildquery: No theme subtree"
		; "PROC/buildquery: Unknown number use" meaning
		; "PROC/buildquery: Unknown top category" categ
		; "PROC/buildquery: Problems in final-build-query"
		; "PROC/buildquery: Non-matching segments"
		; "PROC/buildquery: Phrasal category, but not yes/no"
		; "PROC/buildquery: Problems in end of dialogue"
		; "PROC/buildquery: Undefined dialogue context in build-sem-query"
		; "PROC/onto-reasoning: Attempt to use a NIL ontology class" class up-class
		; "PROC/onto-reasoning: Attempt to use a NIL ontology relation" relation up-relation
		; "PROC/onto-reasoning: Looking for a path to a nil node" node1 node2
		; "PROC/onto-reasoning: No property for an ontology concept" lnode1 lnode2
		; "PROC/onto-reasoning: No path found in find-shortest-path"
		; "PROC/onto-reasoning: no path found in find-shortest-path-2"
		; "PROC/onto-reasoning: checking complement compatibility for a non-subject"
		; "PROC/seminterp: the root of the tree is of a wrong category" headcateg
		; "PROC/seminterp: Dependent not found in replace-dependent"
		; "PROC/seminterp: Adjoining under a trace or subcomponent" pos
		; "PROC/seminterp: Null path in attach-subtree"
		; "PROC/seminterp: Dependent not found in attach-dependent"
		; "PROC/seminterp: Attachment under a trace or subcomponent" pos
		; "PROC/seminterp: A pronoun with person allval, but not a trace"
		; "PROC/seminterp: No anaphoric reference available"
		; "PROC/seminterp: Word trace in seminterp"
		; "PROC/seminterp: Search condition on unknown feature: seminterp" feature
		; "PROC/seminterp: Auxiliary without person" actperson
		; "PROC/seminterp: Verb without person, but not participle"
		; "PROC/seminterp: Identifier without class" (first wmean)
                              (cond ((and (eq *SYSTEM-CONTEXT* 'hops-dial)
                                          (>= *PRINT-LEV* 2))
                                      (setq *print-level* nil)
                                      (setq *print-length* nil)
                                      (format t "SEMANTIC QUERY: ~s ~%" semrepr)
                                      (break "")))
                              (cond ((stringp (first semrepr))
                                       (flat-analysis data-string))
                                    (t (setq result 
                                            (catch 'parameter-error
                                                   (sem-to-par-translation semrepr)))
		; "PROC/buildparam: Unknown expression" (first semquery)
		; "PROC/buildparam: Unknown complex parameter definition" param-def
		; "PROC/buildparam: Wrong definition of a conditional parameter"
		; "PROC/buildparam: Unknown operator in conditional parameter" operator
		; "PROC/buildparam: Unknown non-atomic element in query" (first query)
   			              (cond ((stringp (first result))
                                               (flat-analysis data-string))
                                            (t (list (flatten (mapcar #'first result))
                                                     (flatten (mapcar #'second result)))
                                                 ))))))))))))

;***********************************************************************
(defun start-atlas ()
 (declare (special *PREV-SENTENCES* *TURN-TO-TALK*))
 (let (semres result mode filename)
       ; (format t "start-atlas: *TURN-TO-TALK*: ~a~%" *TURN-TO-TALK*)
   (format t "Do you want to work in interactive mode (one sentence at a time)~%")
   (format t "    or in file mode (on a whole file)?~%")
   (format t "  Interactive=i, File=f: ")
   (setq mode (check_risp '(i f) 0))
   (cond ((eq mode 'i)
           (setq semres (ana-and-tag-and-parse-and-seminterp 'i))
       ; *** ana-and-tag-and-parse-and-seminterp returns a pair <result-code result>
       ;     in case there is an error, result is the input sentence
       ; *** otherwise (result-code=ok), result is the semantic query
           (case (first semres)
              (ok
                 (setq *PREV-SENTENCES* (cons result *PREV-SENTENCES*))
                 (setq result 
                    (catch 'sem-conversion-error
                          (sem-to-atlas-translation (second semres) (third semres))))
           ; *** third semres is a flag specifying if a trace is required
                 (cond ((stringp (first result))
                          (flat-analysis (second semres)))
                       (t (format t "Semantic frame: ~a~%" result)
                          (break "")
                          (cond ((not (null (fourth semres)))
                                  (with-open-file (outframport (change-extens (fourth semres) ".atl")
                                       :direction :output :if-exists :overwrite :if-does-not-exist :create)
                                    (format outframport "~a~%" result)))))))
              (morpho 
                 (format t " ***** ERROR in MORPHO/TAGGING; THE ANALYSIS PROCEEDS ANYWAY *****~%")
                 (flat-analysis (second semres)))
              (parser 
                 (format t " ***** ERROR in PARSING; THE ANALYSIS PROCEEDS ANYWAY *****~%")
                 (flat-analysis (second semres)))
              (seminterp 
                 (format t " ***** ERROR in SEMANTIC INTERPRETATION; THE ANALYSIS PROCEEDS ANYWAY *****~%")
                 (flat-analysis (second semres)))))
       ((eq mode 'f)
         (format t "Name of the input file (without double quotes)?~%")
         (format t "  (The base directory is *HOME-DIR*/DATI/ATLAS/)~%")
         (setq filename (build-subdir-file-name (read-line) "../DATI/ATLAS/"))
         (setq semres (ana-and-tag-and-parse-and-seminterp 'f filename))))))
        
;***********************************************************************
(defun start-tocai ()
 (declare (special *PREV-SENTENCES* *TURN-TO-TALK*))
 (let (semres result flag)
   (setq semres (ana-and-tag-and-parse-and-seminterp 'i))
   ; *** ana-and-tag-and-parse-and-seminterp returns a pair <result-code result>
   ;     in case there is an error, result is the input sentence
   ; *** otherwise (result-code=ok), result is the semantic query
   (case (first semres)
      (ok
         (setq result 
            (catch 'predicate-error
                  (sem-to-pred-translation (second semres))))
         (cond ((stringp (first result))
                  (setq flag 'sem-to-pred-translation-error)
                  (setq result (flat-analysis (second semres))))
               (t (setq flag 'ok)
                  (setq result (second result)))))
      (morpho 
         (format t " ***** ERROR in MORPHO/TAGGING; THE ANALYSIS PROCEEDS ANYWAY *****~%")
         (setq flag 'morphological-error)
         (setq result (flat-analysis (second semres))))
      (parser 
         (format t " ***** ERROR in PARSING; THE ANALYSIS PROCEEDS ANYWAY *****~%")
         (setq flag 'parser-error)
         (setq result (flat-analysis (second semres))))
      (seminterp 
         (format t " ***** ERROR in SEMANTIC INTERPRETATION; THE ANALYSIS PROCEEDS ANYWAY *****~%")
         (setq flag 'semantic-interpretation-error)
         (setq result (flat-analysis (second semres)))))
   (with-open-file (outframport (change-extens (fourth semres) ".pro")
                  :direction :output :if-exists :overwrite :if-does-not-exist :create)
       (format outframport "  ~a ~%~a~%" flag result))
   'ok))

;***********************************************************************
; *** This post-processes the resulting attributes:
;     1. it cancels possible duplicates
;     2. If YES/NO-ANSWER is among the extracted attributes, it is deleted
;        from the result and, if any attribute in the "yes-no" set is
;        present in the expected list, it is included in the result
;     3. if initDate and finalDate are present among the extracted
;        attributes, they are replaced by a sequence of "date" values
;     4. If - requestedData is not among the requested attributes
;           - queryFocus is among the requested attributes
;           - a requestedData has been extracted by the NLPP
;        Then that requestedData is taken as queryFocus and it is 
;           cancelled, as a requestedData, from the result
;     5. The queryFocus is extended with all values having got a value
;     6. If itamNumber is in the queryFocus, it is substituted by "title"
(defun adjust-attributes (extracted-attrs expected-attrs)
 (let (result-attrs focus-attrs queryf initdate finaldate
       savfocus day-attr month-attr actdate object no-object
       (yes-no (assoc '|yesNoAnswer| extracted-attrs))
  ;     (*YES/NO-ATTRIBUTES* 
  ;        '(|returnNextPageForCA_EventListResult| |returnMenu|
  ;          |anotherSearch| |fullDescription| |confirm|))
  ; *** the next are all attributes appearing either in the NLPP or in
  ;     in the Dialogue Flow or in both
  ;     (*ALL-NLPP-ATTRIBUTES* 
  ;        '(
  ;         |askRepeat| |requestFocus| YES-NO-ANSWER
  ;         |returnNextPageForCA_EventListResult| |returnMenu|
  ;         |anotherSearch| |fullDescription| |confirm|
  ;          |yesNoAnswer| |queryFocus| |requestedData| |serviceId|
  ;          |location| |date| |title| |initDate| |finalDate|))
  ; *** the next are the attributes that do not contribute to building the
  ;    focus of the query
       (*NOT-FOCUS-ATTRIBUTES* 
          '(
            |requestedData| |serviceId|
  ;         |requestFocus| |returnNextPageForCA_EventListResult| |returnMenu|
  ;         |anotherSearch| |fullDescription| |confirm|
             )))
  (setq extracted-attrs (elimdup extracted-attrs))
  ; *** if, among the resulting attributes, there is "YES/NO-ANSWER", then
  ;     this attribute is replaced by one in the list *YES/NO-ATTRIBUTES*
  (setq extracted-attrs (delete nil extracted-attrs))
  (setq day-attr (assoc 'EV-DATE-DAY extracted-attrs))
  (setq month-attr (assoc 'EV-DATE-MONTH extracted-attrs))
  (cond ((not (null day-attr))
          (cond ((not (null month-attr))
                   (setq actdate
                         (make-out-date 
                              (second day-attr) (second month-attr) 2007))
                   (setq extracted-attrs
                       (cons `(|date| ,actdate)
                           (delete day-attr
                               (delete month-attr extracted-attrs :test #'equal)
                               :test #'equal)))))))
  (cond (yes-no 
          (cond 
                ((member '|itemNumber| expected-attrs)
                    (setq result-attrs
                          (cond ((equal "no" (second yes-no)) 
                                   '((|itemNumber| "quit")))
                                (t '((|itemNumber| "next")))))
                   (setq extracted-attrs
                        (delete yes-no extracted-attrs :test #'equal))))))
  ; *** expand a date interval into a sequence of dates
  (setq initdate (assoc '|initDate| extracted-attrs))
  (cond ((not (null initdate))
          (setq finaldate (assoc '|finalDate| extracted-attrs))
          (cond ((not (null finaldate))
                  (setq extracted-attrs
                        (delete initdate extracted-attrs :test #'equal))
                  (setq extracted-attrs
                        (delete finaldate extracted-attrs :test #'equal))
                  (setq result-attrs 
                       (append result-attrs
                              (expand-date-interval
                                  (second initdate) (second finaldate))))))))
  ; *** if, among the expected attributes, there is "queryFocus", then a
  ;     remaining "requestedData" is taken as the queryFocus
  ;(cond ((member '|queryFocus| expected-attrs)
  ;        (let ((requested (assoc '|requestedData| extracted-attrs)))
  ;               (cond ((not (null requested))
  ;                       (setq focus-attrs (list (list (second requested)))))))))
  ; *** requestedData is returned just in case it is explicitly asked for
  (cond ((not (member '|requestedData| expected-attrs))
          (cond ((member '|queryFocus| expected-attrs)
             ; *** if queryFocus appears among the expected attrs a
             ;     returned requestedData is used as queryFocus
                  (let* ((found-requested
                           (leggi extracted-attrs '|requestedData|)))
                      (cond ((not (null (leggi extracted-attrs found-requested)))
                               (setq focus-attrs 
                                  (cons found-requested focus-attrs)))
                            ((not (eq (first found-requested) '|title|))
                               (setq savfocus found-requested))))))
             ; *** This does not work if queryFocus is asked for at the very
             ;     beginning: it must be used just in case the specific query
             ;     the user has to answer refers to what he wants to focus on
          (setq extracted-attrs
               (delete
                    (assoc '|requestedData| extracted-attrs)
                    extracted-attrs :test #'equal))))
  ; *** the next removes from the resulting attributes any "requested datum" that
  ;     appears among the ones given by the user
  (setq extracted-attrs (remove-requested-given extracted-attrs))
  (setq result-attrs (append result-attrs extracted-attrs))
  (setq result-attrs (manage-addresses result-attrs))
  ; *** the next: unknown cities are added in flat-analysis in order to
  ;     produce the right behavior in the preceding manage-addresses  
  (setq result-attrs (delete '(|city| "unknown") result-attrs :test #'equal))
  ; *** the next is repeated in order to remove a possible address from the 
  ;     requestedData
  (setq result-attrs (remove-requested-given result-attrs))
  ; *** delete from the list of extracted-attributes the unexpected ones
  ;(dolist (nextattr extracted-attrs)
  ;   (cond ((member (first nextattr) expected-attrs)
  ;           (setq result-attrs (cons nextattr result-attrs)))))
  ; *** treatment of object names to avoid duplicates
  ;     This is useful to avoid that "object" has both the value
  ;     "chair" and the value "rocking chair". All values are
  ;     selected and the "longest" name is chosen (in the
  ;     hypothesis that it correspondds to the lower one in the hierarchy,
  ;     i.e. the most specific)
  (multiple-value-setq (object no-object)
        (get-attr-values '|object| result-attrs))
  (cond ((not (null object))
           (setq result-attrs (cons (get-longest-value object) no-object))))
  ; *** build the queryFocus attribute
  (dolist (nextattr result-attrs)
     (cond ((not (member (first nextattr) *NOT-FOCUS-ATTRIBUTES*))
             (setq focus-attrs (cons nextattr focus-attrs)))))
  ; *** return the result
  (setq queryf 
   ;   (substitute '|title| '|itemNumber|
          (elimdup (mapcar #'first focus-attrs))
   ;   )
     )
  (cond ((or (equal (second (assoc '|serviceId| result-attrs)) "afc")
             (not (null (assoc '|object| result-attrs))))
           (multiple-value-setq (queryf result-attrs)
                    (get-attr-values '|queryFocus| result-attrs))
           (multiple-value-setq (queryf result-attrs)
                    (get-attr-values '|yesNoAnswer| result-attrs))
           result-attrs)
        ((null queryf)
  ; *** if no query focus has been found, but it is expected, while a
  ;     requestedData is not expected, use as queryFocus, the one that
  ;     was saved above.
          (cond ((not (null savfocus))
                   (cons (list '|queryFocus| savfocus) result-attrs))
                (t result-attrs)))
        (t (cons (list '|queryFocus| queryf) result-attrs)))))

;***********************************************************************
; *** given a list of attributes and an attribute name, it returns two
;     lists; one with all attributes with that name (i.e. duplicates)
;     the other with the remaining attributes
(defun get-attr-values (attrname attrlist)
   (let (result remaining)
   (dolist (attr attrlist (values result remaining))
        (cond ((eq (first attr) attrname)
                 (setq result (append1 result attr)))
              (t (setq remaining (append1 remaining attr)))))))

;***********************************************************************
(defun get-longest-value (attrlist)
   (cond ((null attrlist) nil)
         (t (let ((longest (first attrlist)))
                (dolist (attr (rest attrlist) longest)
                    (cond ((> (length (explode (second attr)))
                              (length (explode (second longest))))
                            (setq longest attr))))))))

;***********************************************************************
(defun expand-date-interval (initdate finaldate)
  (let (inityear initmonth initday finyear finmonth finday
        curryear currmonth currday result)
     (multiple-value-setq (inityear initmonth initday) (decode-xdate initdate))
     (multiple-value-setq (finyear finmonth finday) (decode-xdate finaldate))
     (setq curryear inityear)
     (setq currmonth initmonth)
     (setq currday initday)
   ; *** make-out-date in conc-funct.lisp
   ;      (format t "FIN ~a ~a ~a ~%" finday finmonth finyear)
     (do ()
         ((and (= curryear finyear)
               (= currmonth finmonth)
               (= currday finday))
            (append1 result (list '|date| 
                              (make-out-date currday currmonth curryear))))
   ;      (format t "CURR: ~a ~a ~a ~%" currday currmonth curryear)
         (setq result (append1 result (list '|date| 
                              (make-out-date currday currmonth curryear))))
         (multiple-value-setq (curryear currmonth currday)
                 (next-interv-date curryear currmonth currday)))))

; ***************************************************************************
(defun decode-xdate (outdate)
 ; *** outdate is in the format YYYYMMDD
 ;     the result is in the format yyyy mm dd
 ;       where yyyy, mm and dd are integers
  (let ((explin (explode outdate)))
      (values (i-int-p (reverse (mapcar #'char-code (first-n 4 explin))))
              (i-int-p (reverse (mapcar #'char-code (first-n 2 (nthcdr 4 explin)))))
              (i-int-p (reverse (mapcar #'char-code (nthcdr 6 explin)))))))

; ***************************************************************************
; *** given a day specification, it returns the next day
(defun next-interv-date (year month day)
   (cond ((or (and (= day 31)
                   (member month '(1 3 5 7 8 10)))
              (and (= day 30)
                   (member month '(4 6 9 11)))
              (and (= day 28) (= month 2) (not (= 0 (rem year 4))))
              (and (= day 29) (= month 2) (= 0 (rem year 4))))
   ; **** end of month, but not end of year
           (values year (1+ month) 1))
         ((and (= day 31) (= year 12))
           (values (1+ year) 1 1))
         (t (values year month (1+ day)))))

; ***************************************************************************
(defun remove-requested-given (attrs)
  (let (result (given (mapcar #'first attrs)))
   ; *** in "given" all attributes whose values have been extracted from the query
     (dolist (item attrs result)
          (cond ((or (neq (first item) '|requestedData|)
                     (not (or (member (second item) given)
                              (and (eq (second item) '|date|)
                                    (intersection '(ev-date-month ev-date-year) given)))))
                   (setq result (cons item result)))))))
  
; ***************************************************************************
(defun manage-addresses (attrs)
  (let (street str-numb newval)
    (cond ((or (member '(|requestedData| |address|) attrs :test #'equal)
               (and (not (null (assoc '|location| attrs)))
                    (not (null (assoc '|city| attrs)))))
            (setq street (first (leggi attrs '|location|)))
            (setq str-numb (first (leggi attrs '|street-number|)))
            (cond ((null str-numb)
                     (setq newval 
                         (coerce 
                             (symbol-name  
                                 (implode 
                                      (append (explode street) '(#\space #\,))))
                             'string)))
                  (t (setq newval 
                         (coerce 
                             (symbol-name  
                                 (implode 
                                      (append (explode street) '(#\space #\, #\space)
                                              (explode str-numb))))
                             'string))))
            (cons `(|address| ,newval) 
                  (delete '(|requestedData| |address|) attrs :test #'equal)))
          (t attrs))))
          

