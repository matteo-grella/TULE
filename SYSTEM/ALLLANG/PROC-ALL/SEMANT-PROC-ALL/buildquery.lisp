
(in-package "USER")

; ***************************************************************************
(defun build-sem-query (annotated-tree &optional retry default-param-vals)
; *** in the comment below, it is assumed that the interpretation is carried
;     out inside a dialogue. In this case, there are two possible situations:
;     1. We are at the beginning of the dialogue, so that the first user
;        input aims to clarify what the dialogue is about
;     2. We are inside a dialogue, so that it is now concerned with the
;        details fo the user goals.
;     Of course this assumption holds for the only dialogue-based system,
;     i.e. the one associated with the context HOPS-DIAL
; *** the optional "retry" says if the "standard" interpretation must be
;     carried out. This happens when, at start-dialogue, the user's
;     input is a full query. In this case, the "dialogue topic"
;     interpretation fails, and build-sem-query is restarted disregarding
;     the context. A second case where "retry" is used is when the final
;     question about dialogue continuation was answered by the user directly
;     asking something. In this case, a "yes" value is assumed for the
;     for the YES-NO-ANSWER parameter. This value appears in the variable
;     "default-param-vals"
; ***
; &&&&&& The overall organization is as follows:
;       *DIALOGUE-CONTEXT*?
;       ---> +REQUEST+ (the user is issuing a request of some infos)
;            HEADSEM?
;            --> ££get-info +++ verb-indcompl-theme
;            --> ££want +++ verb+modal-indcompl +++ ££know
;                  +++ rmod(verb) +++ adv(interr)
;            --> ££can +++ verb+modal-indcompl +++ ££tell
;                  +++ verb-obj(verb) +++ adv(interr)
;       ---> meta-level (the user is talking about what the dialogue is about)
;            --> ££get-info +++ verb-indcompl-theme(--about-relation)
; *** the basic idea is to move downwards, starting from the root, to
;     identify the relevant concepts and their restrictions
 (declare (special *DIALOGUE-CONTEXT* *ALL-DIAL-CONTEXTS* restr-dial-parameters
           *SYSTEM-CONTEXT* annotated-tree))
 (let (theme-subtree obj-subtree topic-subtree full-topic-subtree
       act-topic-subtree (+FULL-TREE+ annotated-tree) temp-subtree
       topics mult-default-infos restrictions semrestr head-ident head-number
       curr-context topic-changes newrestrs)
  (declare (special +FULL-TREE+ topic-changes))
  (cond ((eq *SYSTEM-CONTEXT* 'hops-dial)
           (setq curr-context (get-top-block-name *DIALOGUE-CONTEXT*))))
      ; *** question and tense markers are the English modals. The goal is to reach the
      ;     top-level content word
  (setq annotated-tree (skip-question-tense-marker annotated-tree))
  (let ((headsem (get-actavm-headlexmean annotated-tree))
        result-query obj-subtree-mean to-be-search)
      ; *** if the context is "hops", "tule", or "tocai", no check on the dialogue context:
      ;     this is always the branch to follow
      ; *** The only case where this is not the right branch is when we are in the 
      ;     beginning of a real dialogue (context "hops-dial" and not dialogue
      ;     context = +START-DIALOGUE+ or +END-OF-DIALOGUE+ and not "retry"; for
      ;     "retry" see the comments at the beginning of this function)
    (cond ((or (memq *SYSTEM-CONTEXT* '(hops tule tocai atlas))
               retry
               (and (memq curr-context (cons '+REQUEST+ *ALL-DIAL-CONTEXTS*))
            ; *** the next condition is removed in order to allow for an easier test
            ;     of the HOPS NLPP. In this way, the behavior of the hops-dial version
            ;     and the hops version should be the same
                 ;   (not (memq curr-context 
                 ;             '(+START-DIALOGUE+ +ASK-END-OF-DIALOGUE+)))
               ))
            ; *** see the comment above
            ; (cond ((and (eq *SYSTEM-CONTEXT* 'hops-dial)
            ;             (not (null default-param-vals)))
                      ; *** see the comment above wrt. "default-param-vals"
            ;          (setq restr-dial-parameters
            ;                (append restr-dial-parameters default-param-vals))))
      ; *** ££get-info *******************************************************
      ;    "biglietteria settembre musica" 
      ;    The ££get-info node has been added when a head noun was encountered
      ;    The 'verb-indcompl-theme' link connects it with the noun (ex. biglietteria)
      ;    Interpreted as "give me the 'default-info' about 'ticket-counters' where
      ;    the 'ticket-counter' is 'of' the modifiers of 'biglietteria'.
             (cond ((eq '££get-info headsem)
                      (setq theme-subtree
                           (find-actavm-dep 'VERB-INDCOMPL-THEME annotated-tree))
                      (cond ((null theme-subtree)
            ; *** this could happen in case ££get-info comes from "looking for"
                               (setq theme-subtree
                                 (skip-determiner
                                   (find-actavm-dep 'VERB-OBJ annotated-tree)))))
                      (cond ((null theme-subtree)
            ; *** this is for "speak to someone about"
                               (setq theme-subtree
                                     (find-actavm-descendant 
                                        '((link lexmean))
                                        '((rmod --about-relation))
                                        (get-actavm-deps-with-traces annotated-tree)))))
                      (cond ((null theme-subtree)
                               (exception 'semantic-error
                                        "PROC/buildquery: no obj subtree for ££get-info *** 2 ***"))
                            ((eq '££information (get-actavm-headlexmean theme-subtree))
                 ; *** if this is the case, go down, assuming that the input is "information about"
                 ;     this covers also "information related to", and returns the subtree
                 ;     governed by "about" or "to"
                               (setq theme-subtree
                                   (find-information-topic theme-subtree)))))
      ; *** ££want ££know ****************************************************
      ;    "vorrei sapere dove è la biglietteria di settembre musica" 
      ;    It assumes that the 'verb-object' of "to tell" is a verb-headed group
      ;    and that the verb governs a question adverb
      ;    The question adverb identifies what is asked, while the other complements
      ;    identify the search conditions
                   ((eq '££want headsem)	; I want to know ...
                     (setq obj-subtree
      ; *** "Vorrei andare ...", "I want to ...", "pretendo di sapere" ...
      ;     get-sentential-object defined in seminterp
                           (get-sentential-object annotated-tree))
                     (cond ((null obj-subtree)
                             (setq theme-subtree
      ; *** "Vorrei un'informazione  ...", "I want an information ...",
                                 (skip-determiner
                                     (get-standard-object annotated-tree)))
                             (cond ((and (eq 'PRON (get-actavm-headcateg theme-subtree))
                                         (eq 1 (get-actavm-headperson theme-subtree)))
      ; *** this is the case of "mi servono informazioni", where "mi" is
      ;     (wrongly?) labelled as verb-obj, so we must take the verb-subj
      ; *** Now, theme-subtree could be headed by "information"
                                      (setq theme-subtree 
                                            (find-actavm-dep 'VERB-SUBJ annotated-tree))))
                             (cond ((eq '££information
                                       (get-actavm-headlexmean theme-subtree))
      ; *** If this is the case, go down two levels, assuming that the input is
      ;     "information about"
                                     (setq theme-subtree
                                           (find-information-topic theme-subtree)))))
                           ((eq '££know (get-actavm-headlexmean obj-subtree))
                             (setq theme-subtree (find-know-theme obj-subtree)))
      ; *** I would like to get information ...
                           ((eq '££obtain (get-actavm-headlexmean obj-subtree))
                             (setq theme-subtree 
                                 (skip-determiner (find-actavm-dep 'verb-obj obj-subtree)))
                             (cond ((eq '££information (get-actavm-headlexmean theme-subtree))
                                     (setq theme-subtree
                                           (find-information-topic theme-subtree)))))
      ; *** the next for "print it", translated as "the user wants that the system prints it"
                           ((or (one-is-subclass-of (get-actavm-headlexmean obj-subtree)
                                             '££system-operation)
                                (one-is-subclass-of (get-actavm-headlexmean obj-subtree)
                                             '££external-service))
                             (setq theme-subtree obj-subtree))
                           (t (exception 'semantic-error
                                        "PROC/buildquery: Unknown argument for ££want"
                                        (get-actavm-headlexmean obj-subtree))))
                     (cond ((null theme-subtree)
                              (exception 'semantic-error "PROC/buildquery: No theme subtree"))
                           ((eq '££possess (get-actavm-headlexmean theme-subtree))
              ; *** if "I want to know if you have information ..."
              ;     theme-subtree is "have information ..."
                              (setq temp-subtree
                                     (find-actavm-dep 'verb-obj theme-subtree))
                              (cond ((eq '££information
                                         (get-actavm-headlexmean temp-subtree))
                                       (setq theme-subtree
                                           (find-information-topic temp-subtree)))))
                           ((eq '££can (get-actavm-headlexmean theme-subtree))
              ; *** if "I want to know where I can buy ....", then the
              ;     theme-subtree is "I buy ..."
              ; *** Note that the "where" adv should have been moved below "buy"
                               (setq theme-subtree
                                     (find-actavm-dep 'verb+modal-indcompl theme-subtree)))))
      ; *** ££can ££tell *****************************************************
      ;    "puoi dirmi il titolo?"
      ;    "puoi dirmi dove è la biglietteria di settembre musica" 
      ;    It takes the 'verb-object' of "to tell"
      ;    - if is a verb-headed group, it assumes that the verb governs a question adverb
      ;      The question adverb identifies what is asked, while the other complements
      ;      identify the search conditions
      ;    - If it is a nominal group, it assumes it is the topic
                   ((eq '££can headsem)		; can you tell me
                     (setq obj-subtree
                           (find-actavm-dep 'verb+modal-indcompl annotated-tree))
                     (setq obj-subtree-mean (get-actavm-headlexmean obj-subtree))
                     (cond ((eq obj-subtree-mean '££tell)
              ; *** look for the object of tell
                             (setq theme-subtree (find-actavm-dep 'VERB-OBJ obj-subtree))
                             (cond ((eq (get-actavm-headlexmean theme-subtree) '££can)
                     ; *** can you tell me how I can ...
                                       (setq theme-subtree 
                                            (find-actavm-dep 'VERB+MODAL-INDCOMPL theme-subtree)))))
                           ((eq obj-subtree-mean '££occur)
              ; *** THIS is a big patch: In italian, I interpret "dare" as "occur"
              ;     for "dove danno lo spettacolo?"
                              (setq temp-subtree
                                     (find-actavm-dep 'VERB-OBJ obj-subtree))
                              (cond ((eq '££information (get-actavm-headlexmean temp-subtree))
                                       (setq theme-subtree
                                           (find-information-topic temp-subtree)))))
                           ((eq obj-subtree-mean '££get-info)
                ; *** In this case, instead of having the standard processing,
                ;     the result is found via a recursive call
                             (setq result-query 
                                 (build-sem-query obj-subtree retry default-param-vals)))
                           ((or (one-is-subclass-of obj-subtree-mean '££system-operation)
                                (one-is-subclass-of obj-subtree-mean '££external-service))   ; ex. ££collect
                             (setq theme-subtree obj-subtree))
                           (t (exception 'semantic-error
                                         "PROC/buildquery: Unknown argument for ££can")))
                     (cond ((and (null theme-subtree)
                                 (null result-query))
                              (exception 'semantic-error
                                    "PROC/buildquery: No theme subtree for ££can"))))
      ; *** ££to-be *****************************************************
      ;    "Are there cabaret events?"
      ;    "dove è la biglietteria di settembre musica" 
      ;    It takes the full annotated tree
                   ((eq '££to-be headsem)
                     (let ((prep-case 			; I'm after rock concerts
                               (find-actavm-descendant 
                                    '((link lexmean))
                                    '((rmod --after-relation))
                                    (get-actavm-deps-with-traces annotated-tree))))
                         (cond ((null prep-case)
                                  (setq theme-subtree annotated-tree))
                               (t (setq to-be-search t)
                                 ; *** the variable to-be-search is used below to determine
                                 ;     if the verb "to be" is used for "look for" (in
                                 ;     "to be after")
                                  (setq theme-subtree 
                                     (skip-determiner
                                        (find-actavm-dep 'prep-arg prep-case)))))))
                   ((eq headsem '££put)		; ho trovato (buttato) una poltrona
                      (setq theme-subtree annotated-tree)))
; ********************************************************************************************
; *** Now, the theme has been found for ££get-info, ££can, ££tell, ££to-be *******************
; *** and ££want+££know
             (cond ((not (null result-query)) result-query)
    ; *** the branch above concerns the recursive call occurred with ££can+££get-info
                   ((eq headsem '££give-info)		; "al Regio" (answer to system)
    ; *** ££give-info is the only case where we get no topic, but just an obj-subtree
                     (setq obj-subtree (find-actavm-dep 'VERB-INDCOMPL-THEME annotated-tree))
              ; *** look for the provided information
                     (cond ((null obj-subtree)
                              (setq obj-subtree
              ; *** look for the provided information
                                 (find-actavm-dep 'VERB-OBJ annotated-tree)))))
                   ((eq headsem '££see)				; "vediamo qual è la situazione"
                     (setq obj-subtree (find-actavm-dep 'VERB-OBJ annotated-tree))
                     (cond ((null obj-subtree)
                              (exception 'semantic-error "PROC/buildquery: No obj subtree for ££see"))
                           (t (setq result-query (build-sem-query obj-subtree)))))
   ; *** otherwise, use the theme for identifying topic and restrictions
                   ((memq headsem '(££get-info ££want ££can ££to-be ££put))
                      (multiple-value-setq 
                            (topic-subtree full-topic-subtree topics restrictions)
                        (find-topic-subtree theme-subtree))
                      (cond ((eq (get-actavm-headlexmean theme-subtree)
                                 '££to-be)   ; Per quali item ci sono sostituzioni?
                              (setq restrictions 
                                   (append restrictions
                                         (get-verb-restrictions theme-subtree full-topic-subtree)))))
                      (cond ((or (eq headsem '££get-info)
                                 (and (eq headsem '££to-be)
                                      to-be-search)
                                 (and (eq headsem '££want)
                                      (eq '££information
                                          (get-actavm-headlexmean
                                               (find-actavm-dep 'VERB-OBJ annotated-tree)))))
                               ; *** the first disjunct of this branch aims at coping with
                               ;     a syntax error. In case of "I am looking for events
                               ;     tomorrow", "tomorrow" is attached to "looking", instead 
                               ;     of to "events". The grammar cannot be changed, since an
                               ;     adverb must, in general, be attached to a verb rather than
                               ;     to a noun. The solution is to consider as restrictions also
                               ;     the modifiers of the main verb.
                               ; *** the second disjunct aims at coping with "to be" used for
                               ;     "looking for", as in "I'm after ..."
                               ; *** the third disjunct for "I want information about ... for ..."
                               ;     where "for ..." is attached to "want"
                              (setq restrictions 
                                (append restrictions
                                 (get-verb-restrictions
                                         annotated-tree full-topic-subtree))))))
                   ((eq headsem '££to-have)
                      (multiple-value-setq 
                            (topic-subtree full-topic-subtree restrictions)
                        (find-topic-subtree annotated-tree))
                      (setq restrictions 
                           (append restrictions
                                 (get-verb-restrictions annotated-tree full-topic-subtree))))
                   ((one-is-subclass-of headsem '££system-operation)
                      (multiple-value-setq 
                            (topic-subtree full-topic-subtree topics restrictions)
                        (find-topic-subtree annotated-tree)))
                   (t (exception 'semantic-error "PROC/buildquery: Undefined top-level operator")))
 ; ********************************************************************************************
 ; *** end of the analysis of the tree and of the collection of information from its structure
 ;     In general, we now have in topic-subtree the goal of the query and in restrictions the
 ;     constraints ("where" part) for the selection of the relevant items
 ; *** now, actual start of query construction
   ; *** case 1: the branch below concerns the recursive call occurred with ££can+££get-info
             (cond ((not (null result-query)) result-query)
   ; *** case 2: ££give-info: we don't have the topic, but an obj-subtree (see above)
                   ((eq '££give-info headsem)
                      (setq semrestr (build-restr-sem '££DIALOGUE
                                           (find-obj-all-conjuncts 
                                                 obj-subtree annotated-tree 'verb) nil nil t))
                      (final-build-givinfo '££DIALOGUE semrestr))
   ; *** case 3: requests to the system (includes also "list something", interpreted as "print")
                   ((and (not (null theme-subtree))
                         (or (one-is-subclass-of (get-actavm-headlexmean theme-subtree)
                                          '££system-operation)
                             (one-is-subclass-of (get-actavm-headlexmean theme-subtree)
                                          '££external-service)))
                      (setq semrestr (build-restr-sem '££DIALOGUE 
                                               (list (list 'verb theme-subtree))))
                      (final-build-givinfo '££DIALOGUE semrestr))
   ; *** case 4: error: the topic subtree is missing; since not ££give-info, a topic-subtree
   ;             had to be found before
                   ((null topic-subtree)    
                      (exception 'semantic-error "PROC/buildquery: No topic subtree found"))
   ; *** case 5: standard processing (e.g. for queries: get-info)********************
                   (t 
      ; *** the next cond sets the variable head-number **********************************
                     (cond ((and (eq 'noun (get-actavm-headcateg topic-subtree))
                                 (neq 'proper (get-actavm-headtype topic-subtree)))
                              (setq head-number (get-actavm-headnumber topic-subtree)))
                           ((eq 'pron (get-actavm-headcateg topic-subtree))
         ; *** for interrogative pronouns, even if they are singular, it is assumed that
         ;     the answer can be a list, so that the number is forced to 'pl
                              (cond ((eq 'interr (get-actavm-headtype topic-subtree))
                                       (setq head-number 'pl))
                                    (t (setq head-number 
                                             (get-actavm-headnumber topic-subtree)))))
                           (t (setq head-number nil)))
      ; *** the next fragment refers to topics that are proper names *********************
      ; *** currently this happens just in case the sentence is a single identifier,
      ;     as "Settembre Musica", so the topic also includes the identifier restriction
                     (setq head-ident (get-actavm-headlexident topic-subtree))
                     (cond ((not (null head-ident))
                              (setq restrictions (cons topic-subtree restrictions))))
      ; *** here starts the actual construction of the query; this is done after *********
      ; *** the topic subtree has been split in actual topic and internal restrictions ***
      ; *** the latter are added to the restrictions coming from outside the topic subtree
                  ;   (multiple-value-setq (topics newrestrs)
                  ;            (get-noun-restrictions topic-subtree t nil)) 
                  ;   (cond ((null topics)
                  ;           (exception 'semantic-error
                  ;                     "PROC/buildquery: No topic found"
                  ;                     (get-actavm-headlemma topic-subtree))))
                  ;   (setq restrictions (append restrictions newrestrs))
   ; *** "restrictions" is a list of tree nodes (the dependent of the element
   ;     being interpreted). The last parameter (head-number) is not nil,
   ;     in case the topic is associated to a common noun or a pronoun
              ; (format t " Topics: ~a~% " topics)
              ; (format t " Topics: ~a~% Topic subtree: ~a~% Restrictions: ~a~%"
              ;            topics topic-subtree restrictions)
              ; (break "")
                     (setq semrestr (build-restr-sem topics restrictions head-number))
                     (setq mult-default-infos (mapcar #'get-default-infos topics))
   ; *** "semrestr" is a list of paths on the ontology; one for each
   ;     syntactic restriction 
                     (final-build-query mult-default-infos semrestr))))
; .... FROM NOW TO THE END: HOPS ............................................
        ((and (eq *SYSTEM-CONTEXT* 'hops-dial)
              (eq curr-context '+START-DIALOGUE+))
   ; *** at +START-DIALOGUE+ the topic of the dialogue is the goal of the
   ;     dialogue itself, and not a domain concept; in fact, when the system
   ;     starts the dialogue, it tries to determine if the user wants some
   ;     specific or generic info
      ; *** ££get-info ......................................................
      ;    "su un concerto"
      ;    "vorrei informazioni sugli spettacoli di domani"
      ;    The ££get-info node has been added when a head noun or prep was
      ;    encountered
      ;    The 'verb-indcompl-theme' link connects it with the "su" preposition
      ;    In this dialogue context, it is interpreted as "I want to talk about X"
             (cond ((eq '££get-info headsem)
                      (setq topic-subtree
                           (find-actavm-dep 'VERB-INDCOMPL-THEME annotated-tree))
                      (cond ((null topic-subtree)
            ; *** this could happen in case ££get-info comes from "looking for"
                               (setq act-topic-subtree
                                   (find-actavm-dep 'VERB-OBJ annotated-tree)))
                            ((memq (get-actavm-headlexmean topic-subtree)
                               '(--dummy-prep --about-relation))
      ; *** the sentence is "informazioni su xxx" or "su xxx": the useful topic
      ;     is not "su", but "xxx" or "interested in"
         ; *** in act-topic-subtree the noun (or pronoun) governed by the
         ;     preposition (skipping possible determiners)
                             (setq act-topic-subtree 
                                  (skip-determiner 
                                       (find-actavm-dep 'PREP-ARG topic-subtree))))
                            (t (exception 'semantic-error
                                   "PROC/buildquery: Unknown preposition in build-sem-query"
                                   (get-actavm-headlemma topic-subtree))))
                      (setq head-ident 
                           (get-actavm-headlexident act-topic-subtree))
         ; *** if the topic is a common noun, get its syntactic number
                      (cond ((and (eq 'noun (get-actavm-headcateg
                                                    act-topic-subtree))
                                  (neq 'proper (get-actavm-headtype
                                                    act-topic-subtree)))
                              (setq head-number
                                   (get-actavm-headnumber act-topic-subtree))))
                      (setq restrictions 
                           (cons act-topic-subtree 
                                 (get-verb-restrictions 
                                       annotated-tree act-topic-subtree)))
                      (setq semrestr (build-restr-sem '£DIALOGUE
                                                restrictions head-number))
                      (final-build-givinfo (list '££DIALOGUE) semrestr))
                   ((eq '££see headsem)
     ; *** "I've seen the poster of ..."
                      (setq obj-subtree
                         (skip-determiner
                            (find-actavm-dep 'VERB-OBJ annotated-tree)))
                      (cond ((is-subclass-of (get-actavm-headlexmean obj-subtree)
                                             '££advertisement)
                              (setq theme-subtree
                                   (find-actavm-dep 'PREP-ARG 
                                          (find-actavm-dep 'PREP-RMOD obj-subtree))))
                            (t (setq theme-subtree obj-subtree)))
                      (cond ((null theme-subtree)
                               (exception 'semantic-error
                                    "PROC/buildquery: Unknown argument for ££see")))
                      (setq restrictions (list theme-subtree))
                      (setq semrestr (build-restr-sem '££DIALOGUE restrictions))
                      (final-build-givinfo '££DIALOGUE semrestr))
     ; *** in case the form of the form of the input is not as expected
     ;     at start of dialogue, perhaps the user has directly posited
     ;     the question (user initiative), so try a full interpretation
                   (t (build-sem-query annotated-tree t))))
        ((and (eq *SYSTEM-CONTEXT* 'hops-dial)
              (eq curr-context '+ASK-END-OF-DIALOGUE+))
          (cond ((eq '££give-info headsem)	; "yes" or "no"
                  (setq obj-subtree
              ; *** look for the "yes" or "no" subtree
                        (find-actavm-descendant
                                '((link cat))
                                '((VERB-OBJ phras))
                                     (get-actavm-deps-with-traces annotated-tree)))
                  (cond ((null obj-subtree)
                          (setq obj-subtree
                 ; *** look for an adverbial argument (es. "tomorrow")
                            (find-actavm-descendant
                                '((link cat))
                                '((VERB-OBJ adv))
                                     (get-actavm-deps-with-traces annotated-tree)))))
                  (setq head-ident (get-actavm-headlexident obj-subtree))
                  (cond ((not (null head-ident))
                           (setq restrictions (list obj-subtree)))
                        (t (exception 'semantic-error
                                "PROC/buildquery: Problems in end of dialogue")))
                  (setq semrestr (build-restr-sem '££DIALOGUE restrictions))
                  (cond ((and (listp semrestr) (eq 1 (length semrestr)))
                           (setq semrestr (first semrestr))))
                  (final-build-givinfo '(££DIALOGUE) semrestr))
                (t (build-sem-query annotated-tree t '((YES-NO-ANSWER default-yes))))))
    ; *** in this case, I assume that the final yes/no anwer about the
    ;     continuation of the dialogue has been skipped by the user, who
    ;     has directly asked something new; so a "yes" answer to this
    ;     question was understood
    ; *** the desired behavior is obtained by forcing an (YES-NO-ANSWER yes)
    ;     as a parameter value, and then going on with the analysis
        (t (exception 'semantic-error
                 "PROC/buildquery: Undefined dialogue context in build-sem-query"))))))

; ***************************************************************************
(defun apply-topic-changes (topics topic-changes)
   (dolist (nxtchange topic-changes topics)
       (setq topics (substitute (second nxtchange) (first nxtchange) topics))))

; ***************************************************************************
; *** This looks for the subtree describing the second argument of "££know"
;     (what one wants to know)
(defun find-know-theme (obj-subtree)
   (let ((theme
              ; *** look for the verb object
              ;     I want to know "where IS ..."
           (skip-question-tense-marker
                (find-actavm-descendant
                             '((link))
                             '((VERB-OBJ))
                             (get-actavm-deps-with-traces obj-subtree)))))
        (cond ((null theme)
                (setq theme
              ; *** look for a sentence governed by "if": "I want to know if"
                    (find-actavm-descendant
                           '((link cat))
                           '((VERB-OBJ conj))
                           (get-actavm-deps-with-traces obj-subtree)))
                (cond ((not (null theme))
                        (setq theme
                          ; *** go down another level
                               (skip-question-tense-marker
                                     (find-actavm-descendant
                                            '((link cat))
                                            '((CONJ-ARG verb))
                                            (get-actavm-deps-with-traces theme))))))))
        theme))

; ***************************************************************************
; *** This looks for the subtree describing the topic about which a piece of
;     information is looked for;
; *** it inspects all dependents of the node whose meaning is ££information
;     and looks for a (prep-rmod --about-relation) dependent (i.e. "about",
;     "on") or for a (adjc-rmod &related-to) dependent (i.e. "relative a")
(defun find-information-topic (info-subtree)
   (let ((info-dependents (get-actavm-deps-with-traces info-subtree))
         info-topic)
       (do ((nxtdep (first info-dependents) (first info-dependents))
            (info-dependents (rest info-dependents) (rest info-dependents)))
           ((or (null nxtdep) (not (null info-topic)))
              info-topic)
           (cond ((equal nxtdep '(#\#)) nil)
                 ((and (eq (get-actavm-headlink nxtdep) 'PREP-RMOD)
		       (eq (get-actavm-headlexmean nxtdep) '--about-relation))
                    (setq info-topic (find-actavm-dep 'PREP-ARG nxtdep)))
                 ((and (eq (get-actavm-headlink nxtdep) 'ADJC+QUALIF-RMOD)
		       (eq (get-actavm-headlexmean nxtdep) '&related-to))
                    (setq info-topic (find-actavm-dep 'ADJC-ARG nxtdep)))))))

; ***************************************************************************
; *** Given a tree which is the argument of query, it returns four values:
;     - the subtree (complement or adjunct) which acts as topic of the
;       input tree, 
;     - the full subtree of which this topic is a part (they are different in
;       case the head of the actual topic is an article or a question adjective),
;     - the topics, i.e. the items that describe what is under consideration
;       without their restrictions (i.e. in "the name and address of the owner",
;       the topics are the ontology concepts of "name" and "address")
;     - a possible set of restrictions extracted from inside that
;       subtree. For instance, it is assumed that in NP's, the actual topic is
;       just the NP head, while all dependents are restrictions.
; *** e.g. for "I want to know where I can buy ..."
;          1. the "where" subtree
;          2. the "where" subtree
;          3. (££location)
;          4. nil
;     while for "Which concerts directed by Abbado are there tomorrow?"
;          1. the "concerts directed by Abbado" subtree
;          2. the "which concerts directed by Abbado" subtree
;          3. (££concerts)
;          4. the "directed by Abbado" subtree
;     Note that the restriction carried by "tomorrow" is handled outside this function
;     for "can you tell me the place and time of the concerts directed by Abbado?"
;          1. the "place and time of the concerts directed by Abbado" subtree
;          2. the "the place and time of the concerts directed by Abbado" subtree
;          3. (££location ££startTime)
;          4. the "concerts directed by Abbado" subtree
(defun find-topic-subtree (tree)
  (declare (special *LANGUAGE*))
  (let (topic-subtree restrictions act-topic-subtree temp-subtree temp-deps topics
        (treeheadsem (get-actavm-headlexmean tree)) 
        (treeheadcat (get-actavm-headcateg tree)) 
        (treedeps (get-actavm-deps-with-traces tree)) 
        prep-subtree prep-mean other-topic-subtree topic-categ)
   (cond ((and (eq 'verb treeheadcat) 
               (eq '££can treeheadsem))			; ----------------- ££can
         ; *** can you tell me how I can arrange
         ;     here, we are working now on "can arrange ...", but the topic must 
         ;     be looked for in "arrange ..."
            (find-topic-subtree (find-actavm-dep 'VERB+MODAL-INDCOMPL tree)))
         ((and (eq 'verb treeheadcat) 
               (eq '££to-be treeheadsem)
               (eq '££entity (get-actavm-headlexmean (find-actavm-dep 'VERB-PREDCOMPL+SUBJ tree))))
         ; *** "which are the items ...?"
         ;     the meaning of "which" is "££entity"; the actual topic are "items ..."
            (find-topic-subtree (find-actavm-dep 'VERB-SUBJ tree)))
         ((and (eq 'verb treeheadcat) 
               (eq '££to-have treeheadsem))
         ; *** today, we have some rain (ATLAS)
            (find-topic-subtree (find-actavm-dep 'VERB-OBJ tree)))
         (t 
; ...........................................................................................
; ... in this last case, the work is made in two steps: first look for the full topic subtree
; ... then split it into topics and restrictions
         ; *** the head category of the input tree is a preposition *************************
            (cond ((and (eq 'prep treeheadcat)			; --- PREP: about or dummy
                        (memq treeheadsem '(--about-relation --dummy-prep)))
               ; *** --about-relation for "information on"
               ; *** --dummy-prep for "interested in"
               ; ***** the tree refers to "about something". "something is the topic, while
               ;       the restrictions are empty
               ; ***** get-preposition-arg in "seminterp"
                    (setq topic-subtree (skip-determiner (get-preposition-arg tree))))
         ; *** the head category of the input tree is a conjunction *************************
                  ((and (eq 'conj treeheadcat) (eq '££manner treeheadsem))	; - CONJ: manner
               ; *** in "can you tell me how ..", "how" can be interpreted as a conjunction
                    (setq topic-subtree tree))
                  (t (setq topic-subtree (find-interr-adv treedeps))
                     (cond ((and (not (null topic-subtree))
                                ; *** ADVERBIAL QUESTION ELEMENT ********************
                                 (eq '£manner (get-actavm-headlexmean topic-subtree)))
                             (setq topic-subtree tree)))
                     (cond ((null topic-subtree)              ; ... not adv QE ........
                                ; *** try PRONOMINAL question elements
                             (setq topic-subtree (find-interr-pron treedeps))))
                     (cond ((and (not (null topic-subtree))
                                ; *** Which are the ...?
                                ;     In this case, the topic is not "which", but the subject
                                ;     of "to be"
                                 (eq '--q-pron (get-actavm-headlexmean topic-subtree))
                                 (eq '££to-be treeheadsem))
                             (setq other-topic-subtree (find-actavm-dep 'VERB-SUBJ tree))
                             (cond ((or (null other-topic-subtree)
                                        (equal topic-subtree other-topic-subtree))     
                                      (setq topic-subtree
                                                   (find-actavm-dep 'VERB-PREDCOMPL+SUBJ tree)))
                                   (t (setq topic-subtree other-topic-subtree)))))
                     (cond ((null topic-subtree)              ; ... not adv or pron QE.......
                                ; *** try ADJ+ADV (how long?) question elements
                              (setq topic-subtree (find-interr-adj+adv treedeps))))
                     (cond ((null topic-subtree)            ; ... not adv or pron or adj+adv QE
                                ; *** try ADJECTIVAL question elements
                                ; *** Which concerts are there tomorrow?
                                ; **** N.B. This works for "which", but not for "for which"
                              (setq topic-subtree (find-interr-adjec treedeps))))
                     (cond ((null topic-subtree)
                              ; *** the topic subtree is still empty for "Vorrei un'informazione"
                              ;     (I want an information) or "Ci sono concerti di Abbado?"
                              ;     (are there concerts directed by Abbado?)
                             (cond ((memq treeheadcat '(art noun))	   
                                       ; --- nominal root of the tree
                                       ;     if "tree" is "an information" (first example above),
                                       ;     it is already the topic
                                     (setq topic-subtree tree))
                               ; *** but if the head of the tree is ££to-be, then the sentence
                               ;     is a yes/no question. However, in some cases, it is an
                               ;     implicit request for infos
                               ;     It is not cooperative to answer to "Are there concerts
                               ;     conducted by Abbado" with "yes". In these cases, some extra
                               ;     info must be provided, as if the question were "Which
                               ;     concerts ..."
                               ; *** Currently, only "Are there ..." is handled
                               ;     ££to-have for Catalan "Hi ha ..."
                               ;     ££to-have without loc-metaph for Spanish "Hay algun events ..."
                                   ((or (and (memq *LANGUAGE* '(italian english)) ; --- yes/no question
                                             (eq treeheadsem '££to-be)
                                             (not (null (find-actavm-dep 'PRON-RMOD-LOC+METAPH tree))))
                                        (and (eq *LANGUAGE* 'catalan)
                                             (memq treeheadsem '(££to-be '££to-have))
                                             (not (null (find-actavm-dep 'PRON-RMOD-LOC+METAPH tree))))
                                        (and (eq *LANGUAGE* 'spanish)
                                             (memq treeheadsem '(££to-be '££to-have))))
                                     (setq topic-subtree (find-actavm-dep 'VERB-SUBJ tree)))
                                   ((and (eq '££tell treeheadsem)	     ; --- the System "tells"
                                         (member 
                                            (get-actavm-headlexmean (find-actavm-dep 'VERB-SUBJ tree))
                                            '(§myself nil)))
                              ; *** NIL because in the polite third person ("Puo' dirmi ..."),
                              ;     the system is currently not able to recover itself as the
                              ;     addressee
                                     (setq topic-subtree (find-actavm-dep 'VERB-OBJ tree)))
                                   ((and (eq '££know treeheadsem)  	     ; --- the System "knows"
                                         (member (get-actavm-headlexmean (find-actavm-dep 'VERB-SUBJ tree))
                                            '(§myself nil)))
                                     (setq topic-subtree (find-actavm-dep 'VERB-OBJ tree)))
                              ; *** the next for "can you give me some information ..."
                                   ((and (eq '££give treeheadsem)	     ; --- the System "gives"
                                         (member (get-actavm-headlexmean (find-actavm-dep 'VERB-SUBJ tree))
                                             '(§myself nil)))
                                      (setq temp-subtree 
                                              (skip-determiner (find-actavm-dep 'VERB-OBJ tree)))
                                      (cond ((eq (get-actavm-headlexmean temp-subtree) '££information)
                                              (setq temp-deps 
                                                 (remove '(#\#) 
                                                   (get-actavm-deps-with-traces temp-subtree)))
                                              (cond ((null temp-deps)
                                     ; *** the word "information" without modifiers
                                     ;     possibly, the sentence is "give information for ..."
                                     ;     where "for" is attached to the main verb
                                                      (setq temp-subtree (find-actavm-dep 'RMOD tree)))
                                            (t (setq prep-subtree 
                                                   (find-information-topic temp-subtree))
                                     ; *** otherwise there must be a preposititonal modifier
                                     ;     "about" or "on" (but this does not cover "information
                                     ;     concerning" or "related to")
                                               (cond ((not (null prep-subtree))
                                                       (setq prep-mean (get-actavm-headlexmean prep-subtree))
                                                       (cond ((memq prep-mean 
                                                                 '(--about-relation --on-relation))
                                                               (setq topic-subtree prep-subtree))
                                                             (t (exception 'semantic-error
                                                                   "PROC/buildquery: 'information' with a wrong preposition"))))
                                                     (t (exception 'semantic-error
                                                           "PROC/buildquery: 'information' with modifiers but no preposition"))))))
                                     ; *** currently, the only thing that the system can give is 
                                     ;     information
                                    (t (exception 'semantic-error
                                               "PROC/buildquery: asking the system to give something which is not information")))))))
                 ; *** now, we must have in topic-subtree the full topic; *********************
                 ;     but possible prepositions and/or determiners must be skipped
                 ; *** except in case the head of act-topic-subtree is a noun, the
                 ;     (local) restrictions are empty (nil)
                     (setq topic-categ (get-actavm-headcateg topic-subtree))
                     (cond ((eq topic-categ 'prep)
                              (setq act-topic-subtree 
                                  (skip-determiner
                                         (get-preposition-arg topic-subtree)))
                              (multiple-value-setq (topics restrictions)
                                       (get-noun-restrictions act-topic-subtree t)))
                           ((eq topic-categ 'verb)
                              (setq act-topic-subtree (skip-determiner topic-subtree))
                              (setq restrictions (get-verb-restrictions act-topic-subtree nil)))
                           ((and (eq topic-categ 'conj)
                                 (eq (get-actavm-headtype topic-subtree) 'subord))
                              (setq act-topic-subtree topic-subtree)
                              (setq topics (list (get-actavm-headlexmean tree)) )
                              (setq restrictions (list (find-actavm-dep 'CONJ-ARG topic-subtree))))
                           (t (setq act-topic-subtree (skip-determiner topic-subtree))
                              (multiple-value-setq (topics restrictions)
                                       (get-noun-restrictions act-topic-subtree t))))
                    (values act-topic-subtree topic-subtree topics restrictions)))))))

; ***************************************************************************
; *** looks for a question adverb, among the dependents of a verb
(defun find-interr-adv (deps)
   (let ((qadv (find-actavm-descendant
     ; *** there is no constraint on the link, since this works both
     ;     for adverbial complements (I want to know "where" is ...)
     ;     and for adverbial adjuncts (I want to know where I can buy ...)
     ; **** N.B. This works for "where", but not for "from where"
                        '((cat type))
                        '((adv interr))
                        deps)))
      (cond ((null qadv)
     ; *** if not found, try an explicit link; this is inserted for traces
     ;     in seminterp; for instance, in "I want to buy", we get "I want
     ;     I KNOW WHERE to buy ...". Here, WHERE is a trace, so it cannot
     ;     have syntactic infos (cat, type), and it is identified by the
     ;     link
               (find-actavm-dep 'ADVB+INTERR-RMOD deps))
            (t qadv))))

; ***************************************************************************
; *** looks for a question pronoun, among the dependents of a verb
(defun find-interr-pron (deps)
   (find-actavm-descendant
         '((cat type)) '((pron interr)) deps))

; ***************************************************************************
; *** looks for a dependent (of a verb) that is headed by (or includes)
;     an interrogative adjective
(defun find-interr-adjec (deps)
  (declare (special +FULL-TREE+))
   (let ((result (find-actavm-descendant '((cat type)) '((adj interr)) deps)))
      (cond ((null result)
    ; *** the next for "On which ..."
              (setq result 
                  (find-actavm-descendant 
                     '((link cat))
                     '((rmod prep))
                     deps))
              (cond ((is-a-actavm-trace? result)
                      (setq result
                            (find-coreferent (get-actavm-headcorefline result)
                                             (list +FULL-TREE+)))))
              (setq result 
                  (find-actavm-descendant 
                        '((link cat type)) 
                        '((prep-arg adj interr))
                        (get-actavm-deps-with-traces result)))))
      (cond ((null result) nil)
            ((is-a-actavm-trace? result)
         ; *** if the found interrogative adjective is a trace, look in the
         ;     full tree for its referent and returns it as the result
              (find-coreferent (get-actavm-headcorefline result)
                               (list +FULL-TREE+)))
            (t result))))

; ***************************************************************************
; *** looks for a dependent (of a verb) that is headed by (or includes)
;     an interrogative adjective
(defun find-interr-adj+adv (deps)
  (declare (special +FULL-TREE+))
   (let ((adj-subtree (find-actavm-descendant '((cat)) '((adj)) deps))
         adv-subtree)
      (cond ((not (null adj-subtree))
               (setq adv-subtree 
                  (find-actavm-dep 'ADVB+INTERR-RMOD adj-subtree))))
      (cond ((not (null adv-subtree)) adj-subtree)
            (t nil))))

; ***************************************************************************
; *** main function for building the interpretation of a subtree
; *** INPUT:
;  >>> semtopics: a list of possible meanings of the head node of the tree
;  >>> restrictions: a list of syntactic restrictions of the head node;
;      A restriction is a triple <applic-range subtree head-node>
;      where applic-range can take the values:
;      "single": this means that it applies just to "head-node"
;      "distrib": this means that it applies to all topics (it is for conjunctions)
;      "verb": this means that it comes from a verbal argument; it also applies
;              to all topics
;  >>> the optional "not-simplify" serves to block and-simplification in case the
;      restrictions come from an "and" of give-info topics (e.g. "tomorrow it will rain
;      and the day after tomorrow there will be sun")
; *** OUTPUT:
;  >>> a list of lists, one for each possible interpretation of the input.
;      So, the result of build-restr-sem is "ambiguous"
;      The result represents the possible interpretations of the structure 
;      semtopics + restrictions: it does not only build the semantic of the restrictions but
;      also links them to the governing node
(defun build-restr-sem (semtopics restrictions &optional noun-number up-ident not-simplify)
                                                ;(format t " Semtopics: ~a~% " semtopics)
                                                ;(break "Entering build-restr-sem")
 (let ((multresult 
          (mapcar 
             #'(lambda (x) 
                 (catch 'semantic-error 
                        (build-sing-restr-sem x restrictions noun-number up-ident not-simplify)))
             (inlist semtopics)))
       actresult)
     (setq actresult (remove-failure multresult))
                                                ;(format t 
                                                ;    "Build-restr-sem. up-ident: ~a~% actresult: ~a~%"
                                                ;     up-ident actresult)
                                                ;(break "xbx")
     (cond ((null actresult)
               (exception 'semantic-error
                     "PROC/buildquery: failure in building the meaning of a restriction"))
         ; *** the next tries to further simplify the query (see comments in move-up-subclass-and)
           (t (mapcar #'move-up-subclass-and actresult)))))

; ***************************************************************************
; *** this removes from a list of result all failures;
;     Since each result should be a path in the ontology, an atom (e.g. 'fail)
;     is a failure, as well as a list starting with a string (i.e. information
;     received from an exception)
(defun remove-failure (resultlist)
   (cond ((null resultlist) nil)
         ((or (atom (first resultlist))
              (stringp (first (first resultlist))))
            (remove-failure (rest resultlist)))
         (t (cons (first resultlist) 
                  (remove-failure (rest resultlist))))))

; ***************************************************************************
; *** this builds the interpretation concerning a single "semtopic" (i.e.
;     just one of the possible meanings of the upper node).
;     the result is the interpretation of the structure semtopic + restrictions
;     So, this does not only build the semantic of the restrictions but also
;     links them to the governing node
; *** INPUT:
;  >>> semtopic: a single concept (a node in the ontology), which is one of the possible
;      meanings of the head
;  >>> restrictions: the syntactic restrictions (subtrees) of the head node
;  >>> noun-number (optional): the syntactic number of the head node
;  >>> up-ident (optional): the possible identifier (instance) of the head node
;  >>> not-simplify (optional): blocks and-simplification in case the
;      restrictions come from an "and" of give-info topics (e.g. "tomorrow it will rain
; *** OUTPUT:
;  >>> a list representing the chosen interpretation for the tree headed in the node whose
;      associated meaning is semtopic, including all the restrictions
(defun build-sing-restr-sem (semtopic restrictions &optional noun-number up-ident not-simplify)
 (declare (special topic-changes))
                                                        ;(break "Entering build-sing-restr-sem")
 (cond ((listp semtopic)		; *** the concept is ambiguous
          (exception 'semantic-error "Multiple semtopics in buildquery: build-sing-restr-sem")))
 (cond ((null restrictions)
         (cond ((is-deictic-concept semtopic)    ; e.g. "the city"
                  (let ((referent (get-deictic-referent semtopic)))
						 ; e.g. referent = £Torino
                  (append
                     (add-cardinality-infos semtopic noun-number)
      ; *** the rest, since the result includes the semtopic, which already
      ;     appears in the list obtained by "add-cardinality-infos"
                     (rest (append1 
                                (choose-best-ontpath 
                                    (find-shortest-path semtopic 
                                                (get-instance-class referent)))
                                (list 'eq referent))))))
               (t (add-cardinality-infos semtopic noun-number))))
       (t (let (paths newpaths
                (restrictions (sort-restr restrictions nil nil)))
    ; *** the next do collects all the paths going from a concept (topic) to
    ;     all of its restrictions
              (do ((nxtrestr (first restrictions) (first restrictions))
                   (restrictions (rest restrictions) (rest restrictions)))
                  ((null nxtrestr)
    ; *** exit from the do on the restrictions: now, we have in paths the interpretations of
    ;     all restrictions: put them together in a single representation
          ;(format t "Exiting Build-sing-restr-sem. up-ident: ~a~% paths: ~a~%" up-ident paths)
          ;(break "xbx")
                    (cond ((null paths)
              ; *** this branch in case no dependent produces a real constraint
                             (add-cardinality-infos semtopic noun-number))
                          ((eq 1 (length paths)) 
              ; *** this branch in case of just one restriction: no composition is needed
                               (cond ((equal (first (first paths)) semtopic)
                                        (append (add-cardinality-infos 
                                                     (first (first paths)) noun-number)
                                                (rest (first paths))))
                                     (t (exception 'semantic-error
                                           "PROC/buildquery: mismatch in build-restr-sem 1"))))
                          ((has-ordinal-desc paths) 
              ; *** this branch in case of an ordinal description (il primo giorno del mese)
                             (setq newpaths (dropnil (build-ordinal-description semtopic paths)))
                             (cond ((eq 1 (length newpaths)) 
                                 ; *** the only actual restriction is the ordinal
                                      (cond ((equal (first (first newpaths)) semtopic)
                                               (append (add-cardinality-infos 
                                                            (first (first newpaths)) noun-number)
                                                       (rest (first newpaths))))
                                            (t (exception 'semantic-error
                                                  "PROC/buildquery: mismatch in build-restr-sem 2"))))
                                   (t (standard-compose-restrs 
                                             newpaths semtopic noun-number not-simplify))))
              ; *** last branch: standard case
                          (t (standard-compose-restrs paths semtopic noun-number not-simplify))))
    ; *** body of the do on the restrictions ************************************
                (let ((rtype (first nxtrestr))		; *** this is an atom
                      (rdef (second nxtrestr))		; *** this is a tree
                      (rhead (third nxtrestr))		; *** this is a tree node
                      nodemean)
                                              ; (format t "Rdef: ~a~%" rdef)
                                              ; (break "xbx")
                    (cond ((not (null rhead)) 
                            (setq nodemean (first (leggi (first (leggi rhead 'sem)) 'lexmean)))))
                    (case rtype
     ; *** the first case for the situation where this is just an application to simple items
     ;     (as in the case of coordination)
     ; *** the second one for the (standard) case, where the third argument is the subtree root
     ;     such that the involved restrictions are its subtrees
     ; *** the third case to check if the topic actually is the same,
     ;     but it was restricted in a previous step
                         (single (cond ((or (null nodemean)
     ; *** the second disjunct aims to apply the restriction just to the right conjunct:
     ;     in case of "the whisky and the water with gas", the "with gas" restriction is
     ;     "single", and it must apply only to water, i.e. when the current semtopic
     ;     is the same as the top-meaning of the restriction (nodemean)
     ; *** This is what keeps apart "single" restrictions (this branch of "case") from
     ;     "distrib" restrictions
                                            (is-subtype-of-one-of semtopic (inlist nodemean)))
                                          (setq paths 
                                             (append1 paths
                                                  (build-sing-r-sem semtopic rdef up-ident))))
                                       (t nil)))
                         (distrib (setq paths (append1 paths
                                                    (build-sing-r-sem semtopic rdef up-ident))))
                         (verb (setq paths (append1 paths
                                                    (build-sing-r-sem semtopic rdef up-ident))))
                         (otherwise (exception 'semantic-error
                                        "PROC/buildquery: unknown restriction type")))
     ; *** if the just found path (now stored in first paths) has length 1, then it
     ;     refers to a subtype restriction; so the goal topic must become the new
     ;     concept (in "stato di obsolescenza", "££status"  is changed into
     ;     "££generalConfigurationState")
                    (setq paths (remove-failure paths))
                    (cond ((eq 1 (length (first paths)))
                             (setq topic-changes 
                                 (cons (list semtopic (first (first paths))) topic-changes))
                             (setq semtopic (first (first paths)))))))))))

; *******************************************************************
; *** it is assumed that all paths starts with the same ontology concept
;     so ((c1 x1 x2) (c1 y1 y2) (c1 z1 z2)) is converted into
;        (c1 (and (x1 x2) (y1 y2) (z1 z2)))
; *** semtopic is used just to check consistency
;     returns a single path (non-ambiguous)
(defun standard-compose-restrs (paths semtopic noun-number not-simplify)
   (let ((simpl-f (simplify-bq-and (list 'and paths) not-simplify)))
       (cond ((and (atom (first simpl-f))
                   (member (first simpl-f) (inlist semtopic)))
                (append (add-cardinality-infos (first simpl-f) noun-number)
                        (rest simpl-f)))
             (t (exception 'semantic-error
                       "PROC/buildquery: simplification of formula in build-restr-sem")))))

; *******************************************************************
(defun has-ordinal-desc (paths) 
   (cond ((null paths) nil)
         ((member '££ordinal-descriptor (first paths)) t)
         (t (has-ordinal-desc (rest paths)))))

; *******************************************************************
; *** ordinals are managed in the following way:
;     1. One of the restrictions refers to the ordinal. If, for instance, the 
;        expression is "last day", the form of this restriction is:
;        (££day ... ££ordinal-description domain-of &ordinal-desc-selector range
;         ££ordinal-descriptor has-instance £last)
;        This restriction is identified by the presence of ££ordinal-descriptor
;     2. Among the other restrictions there can be one of the form
;        (££day range-of &day-in-daymonth domain ££day-month-part-of domain-of
;         &month-in-daymonth range ££month)
;        This is characterized by the presence of ££day-month-part-of, which is
;        a sub-concept of ££part-of
;     3. The resulting representation is built assuming that the larger entity
;        (here ££month, since &month-in-daymonth restricts &part-bigger) is 
;        the reference sequence for the ordering, so that the direct ££part-of
;        connection is replaced by a connection through ££ordinal-description,
;        where ££day is the &ord-described-item and ££month is the &reference-sequence
;        The result is:
;        (££day subclass-of ££time-interval subclass-of ££sequenceable-entity
;            range-of &ord-described-item domain 
;         ££ordinal-description
;            (and (domain-of &ordinal-desc-selector range ££ordinal-descriptor 
;                  has-instance £last)
;                 (domain-of &reference-sequence range ££entity-sequence has-subclass
;                  ££day-sequence has-subclass ££month)))
; *** Comments:
;     a. Actually, the second restriction includes the full representation of the
;        the specified sequence (e.g. the month name)
;     b. In case no sequence description is found, the second conjunct of the
;        specification of ££ordinal-description is missing (left unspecified)
;     c. all other restrictions of the top noun (e.g. ££day) are treated in the
;        standard way
; *** the function returns a list of paths such that the paths concerned with the ordinal
;     (one of two depending on the fact that the reference sequence appears explicitly)
;     are replaced with the representation of the ordinal
(defun build-ordinal-description (semtopic paths)
  (let (found other-restrs found2 rem-restrs res first-part selector seq-descr sequence)
; *** the external do looks for the ordinal modifier. It must exist since the function
;     is evaluated just in case such a modifier has been found
   (do ((nxtpath (first paths) (first paths))
        (paths (rest paths) (rest paths)))
       ((or (null nxtpath) found)
          (cond ((not found) 
                  (exception 'semantic-error "Ordinal not found in build-ordinal-description")))
          (cond ((not (null nxtpath))
                   (setq other-restrs (append other-restrs (list nxtpath) paths))))
         ; *** now, we have in "found" the ordinal (e.g. £last) and in other-restrs
         ;     the other (if any) restrictions of the "ordinalized" noun
         ; *** look, among them, for one that is a sub-relation of ££part-of
          (do ((nxtrestr (first other-restrs) (first other-restrs))
               (other-restrs (rest other-restrs) (rest other-restrs)))
              ((or (null nxtrestr) found2)
                (cond ((not (null nxtpath))
                        (setq rem-restrs (append rem-restrs (list nxtrestr) other-restrs))))
                (cond (found2
                        (setq first-part 
                           (reverse (member '££ordinal-description (reverse found))))
                  ; *** first-part is the connection between the described entity
                  ;     (e.g. ££day) and the concept ££ordinal-description
                        (setq selector (rest (member '££ordinal-description found)))
                  ; *** selector is the ordinal representation (&ordinal-descriptor ...)
                        (setq seq-descr (find-sequence-descr found2))
                  ; *** seq-descr is the portion of found2 that comes after the "larger"
                  ;     concept (e.g. the description of the month)
                        (setq sequence 
                            (rest (member '££ordinal-description 
                                     (choose-best-ontpath 
                                         (find-shortest-path semtopic (first seq-descr) 
                                                '&reference-sequence)))))
                  ; *** sequence is the new part. that should connect ££ordinal-description
                  ;     to the larger part (the sequence)
                        (setq res
                           (append1 first-part
                               (list 'and (list selector 
                                            (append sequence (rest seq-descr))))))
         ;    (format t "Seq-descr: ~a~%sequence: ~a~%first-part: ~a~%" seq-descr sequence first-part)
         ;    (break "")
                        (cons res rem-restrs))
                     (t (cons found rem-restrs))))
       ; *** body of the internal loop: look for a sub-concept of ££part-of
              (cond ((includes-subclass-of nxtrestr '££part-of)
                       (setq found2 nxtrestr)))))
 ; *** body of the external loop: look for the concept ££ordinal-descriptor
   (cond ((member '££ordinal-descriptor nxtpath)
           (setq found nxtpath))
         (t (setq other-restrs (append1 other-restrs nxtpath)))))))

; *******************************************************************
; *** this looks for the "larger" argument of the "part-of" relation between
;     the item and the sequence
(defun find-sequence-descr (path)
  (cond ((null path)
           (exception 'semantic-error "Ordinal not found in build-ordinal-description"))
        ((or (is-a-structural-item (first path))
             (not (is-relation (first path)))
             (not (is-restriction-of (first path) '&part-bigger)))
           (find-sequence-descr (rest path)))
        ((equal (second path) 'range)
           (rest (rest path)))
        (t (exception 'semantic-error "Found bigger part in sequence-descr, but missing range"))))

; *******************************************************************
; *** checks if any of the concepts in path is subsumed by class
(defun includes-subclass-of (path target-class)
  (cond ((null path) nil)
        ((is-a-structural-item (first path))
          (includes-subclass-of (rest path) target-class))
        ((is-subclass-of (first path) target-class) t)
        (t (includes-subclass-of (rest path) target-class))))

; *******************************************************************
(defun sort-restr (restrictions single non-single)
   (cond ((null restrictions) (append single non-single))
         ((eq (first (first restrictions)) 'single)
            (sort-restr (rest restrictions) (append1 single (first restrictions)) non-single))
         (t (sort-restr (rest restrictions) single (append1 non-single (first restrictions))))))

; *******************************************************************
; *** it gets an ontology concept; if it is countable and the syntactic
;     number is not null, it returns a list containing the concept and 
;     the syntactic number, otherwise it returns a list containing just
;     the concept
(defun add-cardinality-infos (concept number)
  (cond ((null number) (list concept))
        ((is-countable concept)
           (list concept `(cardinality, number)))
        (t (list concept))))

; *******************************************************************
(defun is-countable (concept)
   (is-subclass-of concept '££countable))

; ***************************************************************************
; ***************************************************************************
; *** The situation is:
;     (... C1 has-subclass C2 (and COND1 ... (subclass-of C1 CONDi) ... CONDn))
;     what happens is that from C1 we go down to C2, and then (among the
;        constraints on C2), there is one that refers to its C1 upper class
; *** The result should be:
;     (... C1 (and CONDi (has-subclass C2 (and COND1 ... CONDn))))
;     so, the constraint related to C1 has been attached directly to C1
(defun move-up-subclass-and (semrestr)
    (int-move-up-s-a semrestr nil))

(defun int-move-up-s-a (semrestr prevpath)
  (let (subcl nosubcl remconds newand)
    (cond ((null semrestr) (reverse prevpath))
          ((or (atom (first semrestr))
               (neq (first (first semrestr)) 'and)
               (neq (second prevpath) 'has-subclass))
            (int-move-up-s-a 
                (rest semrestr) (cons (first semrestr) prevpath)))
  ; *** the dolist looks for all arguments of and that start with "subclass-of C",
  ;     where the previous path was "C has-subclass X" (reversed in prevpath)
          (t (dolist (nxtandarg (rest (first semrestr)))
                   (cond ((and (eq (first nxtandarg) 'subclass-of)
                               (eq (second nxtandarg) (third prevpath)))
                           (setq subcl (cons nxtandarg subcl)))
                         (t (setq nosubcl (cons nxtandarg nosubcl)))))
             (cond ((null subcl)
                     (int-move-up-s-a 
                         (rest semrestr) (cons (first semrestr) prevpath)))
                   (t (setq remconds
                         (cond ((null nosubcl) nil)
                               ((eq 1 (length nosubcl))
                                  (append (list 'has-subclass (first prevpath))
                                          (first nosubcl)))
                               (t (append (list 'has-subclass (first prevpath))
                                          (cons 'and nosubcl)))))
                      (setq newand
                         (cons 'and 
                               (append2 (mapcar #'(lambda (x) (rest (rest x))) subcl)
                                        remconds)))
                      (int-move-up-s-a 
                         (rest semrestr) (cons newand (rest (rest prevpath))))))))))

; ***************************************************************************
; *** it converts an and-ed formula in a simplified form, such that
;     all prefixes appear just once
;     INPUT: (and ((alfa beta gamma) (alfa (and ((beta) (delta)))) (delta omega)))
;     FLAT: ((alfa beta gamma) (alfa beta) (alfa delta) (delta omega))
;     GROUPED: ((alfa (beta (nil gamma)) delta) (delta omega))
;     RESULT: (and ((alfa (and ((beta gamma) (delta)))) (delta omega)))
;     paths is a list of ontology paths
(defun simplify-bq-and (anded-f &optional not-simplify)
   (cond ((not not-simplify)
            (let* ((flat (flatten-bq-and anded-f))
                   (grouped (group-bq-paths flat))
                   (result (add-bq-and grouped)))
                (cond ((listp (first result))
                         (first result))
                      (t result))))
         (t (basic-bq-simpl anded-f))))

; ***************************************************************************
; *** when a formula must not be simplified, it is assumed that it refers to the
;     topics of the interaction, so the prefixes (all referring to the ££dialogue
;     topics) must be unified anyway
(defun basic-bq-simpl (formula)
   (cond ((eq (first formula) 'and)
            (let (common-pref remainders)
                (multiple-value-setq (common-pref remainders) 
                     (get-common-prefixes (second formula) nil))
                (append1 common-pref (list 'and remainders))))
         (t formula)))

; ***************************************************************************
(defun get-common-prefixes (formulae prefix)
   (cond ((all-bq-equal (first (first formulae)) (mapcar #'first formulae))
            (get-common-prefixes (mapcar #'rest formulae) 
                                 (append1 prefix (first (first formulae)))))
         (t (values prefix formulae))))

; ***************************************************************************
(defun all-bq-equal (firstitem items)
  (cond ((null items) t)
        ((equal firstitem (first items))
           (all-bq-equal firstitem (rest items)))
        (t nil)))

; ***************************************************************************
; *** flatten-bq-and works on (and ((a b c))) or (and ((x y) (z)))
;     ARG: (and (conj1 conj2 ... conjN))
;     RES: (flatconj1 flatconj2 ... flatconjN)
(defun flatten-bq-and (anded-f)
   (act-bq-flat (mapcar #'int-flat-bq-and (second anded-f))))

; *** int-flat-bq-and works on (a b c) or (x (and ((y t) (w))))
;     ARG: (simplelem1 simplelem2 ... simplorcomplemlemN)
;     RES: (flatconj1 flatconj2 ... flatconjN)
(defun int-flat-bq-and (conjunct)
   (cond ((null conjunct) nil)
         ((null (rest conjunct))
            (cond ((listp (first conjunct))
                     (cond ((eq 'and (first (first conjunct)))   ; it must be an "and"
                              (flatten-bq-and (first conjunct)))
                           ((member (first (first conjunct)) '(eq cardinality))
                              conjunct)
                           (t (exception 'semantic-error 
                                        "PROC/buildquery: in flattening a formula 1"))))
                  (t conjunct)))
  ;       ((and (listp (first conjunct))  ; it cannot be complex if it is not the last
  ;             (not (member (first (first conjunct)) '(eq cardinality))))
  ;          (exception 'semantic-error "PROC/buildquery: in flattening a formula 2"))
         (t (let ((restres (int-flat-bq-and (rest conjunct))))
                (cond ((and (listp (first restres))
                            (not (member (first (first restres)) '(eq cardinality))))
                          (mult-bq-cons (first conjunct) restres))
                      (t (cons (first conjunct) restres)))))))

(defun mult-bq-cons (firstelem list-of-lists)
   (cond ((null list-of-lists) nil)
         (t (cons (cons firstelem (first list-of-lists))
                  (mult-bq-cons firstelem (rest list-of-lists))))))

(defun act-bq-flat (lists)
  (cond ((null lists) nil)
        ((atom (first (first lists)))
          (cons (first lists) (act-bq-flat (rest lists))))
        (t (append (first lists) (act-bq-flat (rest lists))))))

; ***************************************************************************
; *** group-bq-paths works on ((a b c) (a e f) (a b d g) (h e))
;     in order to produce: ((a ((b ((c)
;                                   (d ((g)))))
;                               (e ((f))))
;                           (h ((e)))))
;     ARG: (list1 list2 ... listN)
;     RES: (conj1 conj2 ... conjM)
;          where each conjI has the form: (firstel rests)
;          where firstel is a concept, and rests has the same form of RES
(defun group-bq-paths (paths)
  (cond ((null paths) nil)
        ((equal paths '(nil)) nil)
        (t (let ((firsts (elimdup (mapcar #'first paths))))
               (collect-bq-rests firsts paths)))))
   
(defun collect-bq-rests (firsts paths)
  (cond ((null firsts) nil)
        (t (let* ((tails (group-bq-paths (find-all-rests (first firsts) paths)))
                  (starts (cond ((null tails) (list (first firsts)))
                                (t (list (first firsts) tails)))))
               (cons starts (collect-bq-rests (rest firsts) paths))))))

(defun find-all-rests (elem paths)
  (cond ((null paths) nil)
        ((eq elem (first (first paths)))
           (cond ((eq 1 (length (first paths)))
                     (find-all-rests elem (rest paths)))
                 (t (cons (rest (first paths)) (find-all-rests elem (rest paths))))))
        (t (find-all-rests elem (rest paths)))))

; ***************************************************************************
; *** add-bq-and works on: ((a ((b ((c)
;                                   (d ((g)))))
;                               (e ((f))))
;                           (h ((e)))))
;     and produces:
;                  (and ((a (and ((b (and ((c) (d g))))
;                                 (e f))))
;                        (h e)))
(defun add-bq-and (grouped-f)
   (cond ((null grouped-f) nil)
         ((= 1 (length grouped-f))     ; no "and" is needed
            (cons (first (first grouped-f))
                  (add-bq-and (second (first grouped-f)))))
         (t (list (list 'and (mapcar #'int-a-bq-and grouped-f))))))

(defun int-a-bq-and (single-g-f)
; *** single-g-f has the form (prefix restform)
   (cons (first single-g-f) (add-bq-and (second single-g-f))))

; ***************************************************************************
; *** this function builds the semantic representation for a governing
;     concept and a single restriction of it. The restriction is given as
;     a full annotated subtree (but not as a triple, as it happens
;     in build-sing-restr-sem)
; *** up-ident, if non-nil, is an identifier associated with semtopic
;     (i.e. the meaning of the upper node)
; *** currently, the result must be a single list, since given one semtopic and
;     one restriction, ambiguities should be solved inside the function. This
;     could be changed afterwards
; *** INPUT:
;  >>> semtopic: a single possible meaning of the head node
;  >>> singrestr: a single syntactic restriction (subtree)
;  >>> up-ident (optional): a possible identifier (individual) associated with the head
; *** OUTPUT:
;  >>> a list representing a single path in the ontology (non-ambiguous)
(defun build-sing-r-sem (semtopic singrestr &optional up-ident)
  (declare (special +FULL-TREE+))
; *** if the restrictions refers to a trace, then use in its place the referent
  (cond ((is-a-actavm-trace? singrestr)
          (let ((corefline (get-actavm-headcorefline singrestr)))
             (cond ((and (not (null corefline))
                         (memq (get-actavm-headcoreftype singrestr) '(#\f f)))
                     (setq singrestr (find-coreferent corefline (list +FULL-TREE+))))))))
  (let ((categ (get-actavm-headcateg singrestr))
        (meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr)))
                                           ;(break "Entering build-sing-r-sem")
     (cond ((eq meaning 'verbal-locut) nil)
   ; *** the restriction is a component of a verbal locution; its meaning is
   ;     already included in the verbal head
           ((stringp ident)
              (append1          ; *** it is a descriptor enclosed in quotes
                  (choose-best-ontpath (find-shortest-path semtopic meaning))
                  (list 'eq ident)))
           ((eq 'art categ)	
   ; *** a determiner is the head: determiners are currently ignored, so the
   ;     tree is travelled downward
              (build-sing-r-sem semtopic (skip-determiner singrestr) up-ident))
           ((eq 'adj categ)
              (build-adj-sem semtopic singrestr))
           ((eq 'noun categ)
              (build-noun-sem semtopic singrestr up-ident))
           ((eq 'prep categ)
              (build-prep-sem semtopic singrestr up-ident))
           ((eq 'adv categ)         		; *** this happens for "domani"
              (build-adv-sem semtopic singrestr up-ident))
                                                ;     up-ident is required for intensifiers
           ((eq 'verb categ)
              (build-verb-sem semtopic singrestr))
           ((eq 'pron categ)
              (build-pron-sem semtopic singrestr))
           ((or (eq 'phras categ)         ; *** this happens for "yes/no"
                (eq 'interj categ))       ; *** this happens for "buongiorno", ...
              (build-phras-sem semtopic singrestr))
           ((eq 'num categ)         ; *** this happens for italian descriptions of
                                    ;     dates (Il 10 luglio)
              (build-num-sem semtopic singrestr))
           ((eq categ 'punct) nil) 
           ((eq categ 'conj)
              (build-conj-sem semtopic singrestr))
           ((and (null categ) 
                 (eq meaning '--about-relation))
             ; *** this is a trace for the "about" pronoun, inserted when the user
             ;     answers a request with an NP (e.g. "Abbado")
             (build-sing-restr-sem semtopic 
                 (mapcar #'(lambda (x) (list 'single x nil))
                        (skip-determiner (find-actavm-dep 'PREP-ARG singrestr t) t))))
           (t (exception 'semantic-error "PROC/buildquery: Unknown top category" categ)))))

; ***************************************************************************
(defun build-adj-sem (semtopic singrestr)
  (let ((adjtype (get-actavm-headtype singrestr))
        (meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr))
        tempsem tempsem2 restrs newtopics)
      (cond ((eq meaning '-dummy-adj) nil)    ; adjectives (provisionally) not interpreted
            ((eq adjtype 'interr)
             ; *** interrogative adjectives (quale, che, ...)
               (build-sing-r-sem semtopic (skip-determiner singrestr)))
            ((memq adjtype '(deitt demons))
             ; *** deictic and demonstrative adjectives (this, next ...)
               (find-path-to-deictic semtopic singrestr))
            ((eq adjtype 'indef)
             ; *** indefinite adjectives (some ...)
               (build-sing-r-sem semtopic (skip-determiner singrestr)))
            ((eq adjtype 'ordin)
             ; *** ordinal adjectives
               (cond ((not (null (find-actavm-dep 'noun-rmod-month singrestr)))
                       ; *** august 3rd
                        (build-date-repr semtopic singrestr))
                       ; *** the next is the standard case: e.g. "first course"
                     (t (choose-best-ontpath (find-shortest-path semtopic ident)))))
            ((null ident)
             ; *** for standard adjectives, the restriction is interpreted by finding
             ;     the shortest path between the governing noun (semtopic) and the
             ;     adjective (meaning). The recursion on build-restr-sem is currently
             ;     useless, since adjectives do not have restrictions
               (multiple-value-setq (newtopics restrs) (get-noun-restrictions singrestr))
               (compose-restr-path
                   (choose-best-ontpath (find-shortest-path semtopic meaning))
                   (build-restr-sem meaning restrs)))
             ; *** the last are adjectives specifying a type (e.g. classical music). In this case,
             ;     the meaning of the adjective has <lexmean, property-name>, <ident, property-value>
             ;     For instance: classical music --> <lexmean ££musical-type> <ident £classic>
            (t (multiple-value-setq (newtopics restrs) (get-noun-restrictions singrestr))
                   ;     (break "adj 1")
               (cond ((null restrs)			; *** no extra adverbials
             ; *** the next checks if there is a specific adj interpretation (see the comments in
             ;     db-adj-lookup)
                        (setq tempsem (db-adj-lookup semtopic meaning ident))
                        (cond ((null tempsem)
                                 (append1 
                                    (choose-best-ontpath (find-shortest-path semtopic meaning))
                                    (list 'eq ident)))
                              (t tempsem)))
                     (t (setq tempsem (build-restr-sem meaning restrs nil ident))
                        (cond ((null (second tempsem))
                                 (setq tempsem (first tempsem)))
                              (t (exception 'semantic-error 
                                     "PROC/buildquery: multiple paths in build-adj-sem")))
                        (setq tempsem2
                           (choose-best-ontpath (find-shortest-path semtopic meaning)))
                        ;(break "adj; xbx")
                        (cond ((intensification-found tempsem)
          ; *** in this case, the restriction has produced an "intensification" of the original
          ;     adjective (e.g. "very strong" wrt. "strong"), so that the intensified value
          ;     is used in place of the original one
                                 (compose-restr-path tempsem2 tempsem))
                              (t (put-up-and tempsem2 tempsem (list 'eq ident))))))))))
                                      
; ***************************************************************************
; *** an intensification relation has been found if the found path includes a
;     relation instance of ££intensification-rel
(defun intensification-found (path)
   (cond ((null path) nil)
         ((atom (first path))
            (cond ((is-instance-of (first path) '££intensification-rel) t)
                  (t (intensification-found (rest path)))))
         ((eq 'and (first (first path)))
           (let (found)
            (do* ((nxtarg (first (second (first path))) (first andargs))
                  (andargs (rest (second (first path))) (rest andargs)))
                 ((or (null nxtarg) found) found)
                 (setq found (intensification-found nxtarg)))))
         (t (intensification-found (rest path)))))

; ***************************************************************************
; *** this aims at interpreting structures as "musica classica" or "settore orientale"
;     In the first example we have upconc=££music, downconc=££music-type, downident=£classical
;     In the second we have upconc=££it-geogr-area, downconc=££it-area-spec, downident=£eastern
; *** A simple "find-shortest-path" would return (££music &has-music-type ££music-type (eq £classical))
;     or (££it-geogr-area &has-it-area-spec ££it-area-spec (eq £eastern))
;     So, in both cases, we fail to retrieve the "interesting" concept (£classical-music and
;     £it-eastern-area, respectively)
; *** what I do is to force a "lookup" of £classical (or £eastern) in the "DB table" corresponding
;     to the relation &has-music-type (or &has-it-area-spec), and, if it exists, to retrieve the
;     other item of the relation (provided it is an instance or subclass of ££music, or ££it-geogr-area)
; *** So the structure is:
;      UPCONC<-----domain-------xxxrel-----range-->DOWNCONC
;    (££music)             (&has-music-type)    (££music-type)
;         ^                        ^                   ^
;         |                        |                   |
;      firstarg<---argument-- nxtrelinst---value-->downident
;  (£classic-music)         (&has-m-type7)        (£classical)
;
; *** the returned path will be:
;     (££music HAS-INSTANCE £classic-music ARG-OF &has-m-type7 RELINSTANCE &has-music-type)
;     (RANGE ££music-type HAS-INSTANCE £classical)
;
(defun db-adj-lookup (upconc downconc downident)
   (let ((relinstance (get downident 'VALUE-OF)) firstarg actrel relrange found)
       (do ((nxtrelinst (first relinstance) (first relinstance))
            (relinstance (rest relinstance) (rest relinstance)))
           ((or (null nxtrelinst) found) found)
           (setq firstarg (first (get nxtrelinst 'ARGUMENT)))
           (setq actrel (first (get nxtrelinst 'RELINSTANCE)))
           (setq relrange (first (get actrel 'RANGE)))
   ; *** the next loop, because when a lexmean is determined on the basis of an identifier
   ;     (i.e. it is its direct class) it is a list of possible concepts (for instance,
   ;     £northern is an instance both of ££cardinal-direction and of ££it-area-spec)
   ;     This happens also in case there is no ambiguity (downconc is a list of length 1)
           (do ((nxtdownconc (first downconc) (first downconc))
                (downconc (rest downconc) (rest downconc)))
               ((or (null nxtdownconc) found) found)
               (cond ((and (is-instance-or-subclass-of firstarg upconc)	; *** £classic-music - ££music
                           (is-instance-or-subclass-of nxtdownconc relrange))
                        (setq found
                           (append (first (find-shortest-path upconc firstarg))
                                 (list 'ARG-OF nxtrelinst 'RELINSTANCE actrel 'RANGE)
                                 (first (find-shortest-path relrange nxtdownconc))
                                 (list (list 'EQ downident))))))))))

; ***************************************************************************
; *** this performs the following operation:
;     1. path1 = [a, b, c, d, e]
;     2. path2 = [e, d, c, f, g, h]
;     3. eq-expr = [eq k]
;       ---->
;      (a, b, c (and ((d, e (eq k)) (f, g, h)))) 
(defun put-up-and (path1 path2 eq-expr)
   (let ((revpath1 (reverse path1)))
     (do* ((prev1 nil nxt1)		                ; *** at the end, prev1 is c
           (common nil (cons nxt1 common))
           (nxt1 (first revpath1) (first revpath1))	; *** at the end, nxt1 is b
           (revpath1 (rest revpath1) (rest revpath1))	; *** at the end, revpath1 is [a] 
           (nxt2 (first path2) (first path2))
           (path2 (rest path2) (rest path2)))
        ((or (null nxt1) (null nxt2) (not (eq nxt1 (inv-r-d nxt2))))
           (cond ((not (null nxt2))
                   (append1 (reverse (cons prev1 (cons nxt1 revpath1)))
                            (list 'and
                                  (list (append1 (rest common) eq-expr)
                                        (cons nxt2 path2)))))
                 (t (append (reverse (cons prev1 (cons nxt1 revpath1)))
                            (append1 (rest common) eq-expr))))))))
                         
; ***************************************************************************
; *** in case of governing adj (ex. mare molto mosso),
;     -- semtopic is the meaning of the adj (e.g. "mosso": ££sea-status-description)
;     -- singrestr is the adevrbial modifier (e.g. molto)
;     -- up-ident is the identifier of the adj (e.g. "mosso": £mare-agitato)
(defun build-adv-sem (semtopic singrestr up-ident)
  (let ((meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr))
        temp-path temp2-path)
     (cond ((eq meaning '--max-) nil)
	     ; *** the above is for "il piu' vicino". Currently, "più" is ignored
           ((memb-or-eq '££intensifier meaning)
	     ; *** this is for, e.g., "molto".
             ;     I assume that in this case ident and up-ident have a value
              (setq temp-path (find-subtype-path up-ident ident 'intens))
              ;(format t "build-adv-sem; temp-path: ~a~%" temp-path)
              ;(break "xbx")
              (setq temp2-path (choose-best-ontpath (find-shortest-path semtopic meaning)))
              (cond ((null temp-path) 
                       (append1 temp2-path (list 'eq up-ident)))
                    (t  (cons semtopic
                            (cons (list 'temp-eq (first temp-path))
                                  (rest temp-path))))))
           ((memb-or-eq '££empty-conc meaning) nil)
             ; *** the above for adverbs which do not carry information (e.g. "in programma",
             ;     where "in programma" is taken as an adverbial locution)
           ((memb-or-eq '£manner meaning) nil)
             ; *** the above for how. It is ignored, since in "how can I buy"
             ;     the actual topic is "buy"
           ((null ident)	; *** standard adverb
             (choose-best-ontpath (find-shortest-path semtopic meaning)))
           (t (setq temp-path
                  (choose-best-ontpath (find-shortest-path semtopic meaning)))
              (cond ((null temp-path)
                       (exception 'semantic-error
                                "PROC/buildquery: Empty path in build-adv-sem"))
                    (t (append1 temp-path        ; *** something as "certamente" (certainly)
                               (list 'eq ident))))))))

; ***************************************************************************
(defun build-conj-sem (semtopic singrestr)
  (declare (special annotated-tree))
  (let ((meaning (get-actavm-headlexmean singrestr)))
     (cond ((eq meaning '--and-operator)
              (exception 'semantic-error "PROC/buildquery: conj (under development)"))
           (t (exception 'semantic-error "PROC/buildquery: Unknown top conj")))))
                                       
; ***************************************************************************
; *** it builds the semantic representation of "singrestr" and links it to 
;     semtopic. singrestr has a nominal head
(defun build-noun-sem (semtopic singrestr &optional semtopic-ident quantif)
  (let ((is-proper (eq (get-actavm-headtype singrestr) 'proper))
        (meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr))
        (deps (remove-continuations (get-actavm-deps-with-traces singrestr)))
        street-numb best-ontpath prep-dep restr-meaning qmean temppath 
        topics restrs actmeaning)
                                              ;(break "Enter build noun sem")
   ; *** the next is to "modify" the meaning or the identifier in case the "quantif" is
   ;     non-null. This applies to "un po' di pioggia" or "un po' di instabilità". The
   ;     assumption (to be verified) is that in case the noun is associated with an
   ;     identifier (as, in the current implementation "£instabilità"), then the
   ;     identifier has to be replaced by a related modifier (e.g. "£moderata-instabilità"),
   ;     while in case it is associated just to a generic concept (e.g. "££rain"), a path
   ;     must be found (subtype path) to another concept (e.g. "££moderate-rain"). The two
   ;     situations seem to be the same, and probably this is true, and depends on some
   ;     wrong decisions about the presence of an identifier for £instabilità. However,
   ;     at the present stage, I prefer to keep both roads open.
     (cond ((null ident)
             (cond ((not is-proper)	; *** it is not a proper name
           ; *** interpret the restriction, and link it to the parent
                      (multiple-value-setq (topics restrs)
                                     (get-noun-restrictions singrestr))
                      (setq restr-meaning
                           (build-restr-sem topics restrs
                                 (get-actavm-headnumber singrestr)))
           ; *** but if, from the upper level, a quantifier has been sent below, include
           ;     it in the resulting structure
                      (cond ((not (null quantif))
                              (setq temppath 
                                  (choose-best-ontpath 
                                      (find-shortest-path meaning (first quantif))))
            ; *** restr-meaning is a set of paths
                              (setq restr-meaning
                                  (list
                                      (list (first (first restr-meaning))
                                            (list 'and 
                                                 (list (append (rest temppath) 
                                                          (replace-has-instance (rest quantif)))
                                                       (rest (first restr-meaning)))))))))
                      (mult-compose-restr-path
                            (find-shortest-path semtopic meaning)
                            restr-meaning))
                   (t 		; *** it has a null ident, but it is a proper name
                     (let (downtopics downrestrs)
                         (multiple-value-setq (downtopics downrestrs)
                                         (get-noun-restrictions singrestr))
                         (cond ((not (null downrestrs))
                                 (exception 'semantic-error
                                        "PROC/buildquery: Proper name with restrictions"))
                               (t (setq meaning (get-default-ident semtopic))
                                  (cond ((null meaning)
                                          (exception 'semantic-error
                                                 "PROC/buildquery: Unknown identifier"))
                                        (t (append1
                                                (mult-compose-restr-path
                                                  (find-shortest-path semtopic meaning)
                                                  (build-restr-sem meaning nil 'sing))
                                                (list 'eq (get-actavm-headlemma singrestr)))))))))))
           ((equal '((#\#)) deps)   ; *** it has a non-null ident and no dependents
              (setq best-ontpath
                  (choose-best-ontpath (find-shortest-path semtopic meaning)))
              (cond ((null best-ontpath)
                       (exception 'semantic-error
                                  "PROC/buildquery: No path from topic to meaning"))
                    (t ;(list 
                       (append1 best-ontpath (list 'eq ident))
                       ;)
                       )))
           (t 	; *** ident is non-null, but there are dependents
                ;     This is, for instance, the case of "South of ..."
                ; *** Here, for "weather in the South of Italy", semtopic=weather
                ;     and meaning=cardinal-direction
          ; *** First, we interpret the restrictions.
          ;     We pass downward "ident", which is necessary for the correct
          ;     interpretation of the restriction "of Italy", in the phrase
          ;     "South of Italy", where a subpart path is involved,
          ;     since "South of Italy" is not a special type of South, but an
          ;     identifier of the Italian Southern Regions.
          ; *** the next cond for quantifier nouns (as "un poco di X" - a little of X)
          ;     it checks the structure depending on "little", and then interprets the X structure
               (cond ((member ident '(--q-little))
                        (setq deps (remove-head-marker deps))
                        (cond ((null deps)
                                 (exception 'semantic-error
                                    "PROC/buildquery: Quantifier noun without dependents")))
                        (cond ((> (length deps) 1)
                                 (exception 'semantic-error
                                    "PROC/buildquery: Quantifier noun with many dependents")))
                        (setq prep-dep (first deps))
                        (cond ((neq (get-actavm-headcateg prep-dep) 'prep) 
                                 (exception 'semantic-error
                                    "PROC/buildquery: Quantifier noun with a non-prep dependent")))
                        (setq singrestr 
                               (skip-determiner (find-actavm-dep 'PREP-ARG prep-dep)))
                        (setq qmean (choose-best-ontpath 
                                           (find-shortest-path meaning ident)))
           ; *** now, we have moved down in the tree, skipping the nominal quantifier structure:
           ;     apply again "build-noun-sem", keeping the upper node (semtopic) unchanged
           ; *** the extra argument (qmean) enables the interpreter to add the
           ;     information that the depending noun has a "modifying" quantifier
           ;     (e.g. "un po' di pioggia" "a little [of] rain")
                        (build-noun-sem semtopic singrestr semtopic-ident qmean))
                     (t		; *** it is not a quantifier noun
                        (multiple-value-setq (topics restrs)
                                         (get-noun-restrictions singrestr))
                        (setq restr-meaning 
                              (build-restr-sem meaning restrs
                                           (get-actavm-headnumber singrestr) ident))
 ;  (format t "Semtopic: ~a; meaning: ~a~% restr-meaning: ~a~%" semtopic meaning restr-meaning)
 ;  (break "build noun sem 2")
                        (cond ((null restr-meaning)
    ; *** if have not found a subtype or subpart path, then try standard interpretation
    ; *** this is the case of "instabilità al Sud", where "instabilità" is a weather
    ;     description with identifier £instabilità
                                (setq best-ontpath 
                                     (choose-best-ontpath (find-shortest-path semtopic meaning)))
                                (cond ((not (null best-ontpath))
                                         (multiple-value-setq (topics restrs)
                                                             (get-noun-restrictions singrestr))
                                         (compose-restr-path
                                             (append1 
                                                  best-ontpath
                                                  (list 'eq semtopic-ident))
                                             (build-restr-sem meaning restrs
                                                       (get-actavm-headnumber singrestr)
                                                       ident)))
                                      (t nil)))
    ; *** a restr-meaning is the full path from the down concept, including all of its
    ;     restrictions. It is assumed that in case of restrictions, just one solution
    ;     exists (which is "first restr-meaning"). Since a solution is a path, we use
    ;     its first, to find the connections with the node above
                              (t (setq best-ontpath
                                    (choose-best-ontpath
                                      (find-shortest-path semtopic (first (first restr-meaning)))))
                                 (cond ((null semtopic-ident) 
                                          (compose-restr-path best-ontpath restr-meaning))
                                       (t (cons (first best-ontpath)
                                               (cons (list 'eq semtopic-ident)
                                                    (compose-restr-path 
                                                         (rest best-ontpath) restr-meaning))))))))))
                 )))

; ***************************************************************************
; *** replaces all pairs "... has-instance instanceid ..." with "... (eq instanceid) ..."
(defun replace-has-instance (path)
   (cond ((null path) nil)
         ((eq (first path) 'has-instance)
            (cons (list 'eq (second path))
                  (replace-has-instance (rest (rest path)))))
         (t (cons (first path) (replace-has-instance (rest path))))))

; ***************************************************************************
(defun get-default-ident (semtopic)
  (declare (special *DEFAULT-CONCEPT-INFOS*))
  ; *** *DEFAULT-CONCEPT-INFOS* has the form:
  ;     ( (conc1 (c11 c12 .... ident-conc1 subclass-of datatype))
  ;       (conc2 (c21 c22 .... ident-conc2 subclass-of datatype)) ...)
  ;     Here, we must extract ident-concK, if semtopic=concK
  (third (reverse (first (leggi *DEFAULT-CONCEPT-INFOS* semtopic)))))

; ***************************************************************************
(defun build-num-sem (semtopic singrestr)
  (let ((meaning (get-actavm-headlexmean singrestr)))
     (cond ((eq meaning '££day-numb-descr)
             (build-date-repr semtopic singrestr))
           ((eq semtopic '££number)
             ; *** This is for "number 3"
              `(,semtopic (eq ,(get-actavm-headvalue singrestr))))
           ((eq semtopic '££dialogue)
             ; *** This is for "3"
              (append1 
                  (choose-best-ontpath (find-shortest-path semtopic '££number))
                  `(eq ,(get-actavm-headvalue singrestr))))
           ((not (null (find-actavm-dep 'contin singrestr)))
             ; *** this for "the 3rd", where "rd" is linked to 3 via "contin". We
             ;     should also check it is of category "ordinsuff"
              (append1 
                  (choose-best-ontpath (find-shortest-path semtopic '££number))
                  `(eq ,(get-actavm-headvalue singrestr))))
           ((is-subclass-of semtopic '££location)   ; assume that the number is a street number
                                                    ; so it has already been analyzed in the
                                                    ; governing street name
              `(,semtopic
                range-of
                &has-street-number    ; *** Via Roma, 33
                domain
                ££street-number
                (eq ,(get-actavm-headvalue singrestr))))
           (t nil 
   ; *** this case refers to "meteo 2" in the ATLAS context. Now, the number 2 is simply ignored
            ; (exception 'semantic-error
            ;             "PROC/buildquery: Unknown number use" meaning)
              ))))

; ***************************************************************************
(defun build-phras-sem (semtopic singrestr)
  (let ((meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr)))
     (cond ((null ident)	
              (exception 'semantic-error
                   "PROC/buildquery: Phrasal category or interjection, but without a specific meaning"))
           (t (append1          ; *** it is "yes" or "no"
                  (choose-best-ontpath (find-shortest-path semtopic meaning))
                  (list 'eq ident))))))

; ***************************************************************************
(defun build-prep-sem (semtopic singrestr &optional up-ident)
  (declare (special *PREP-TEMPLATES* *LANGUAGE*))
  (let ((meaning (get-actavm-headlexmean singrestr)) downtree tempres up-path
        topics restrs inv-dep other-deps newtree)
    ; *** a query beginning with "on"
 ;(break "Entering build-prep-sem")
     (cond ((and (memq meaning '(--about-relation --on-relation))
                 (eq semtopic '££dialogue))
              (setq downtree (skip-determiner (find-actavm-dep 'PREP-ARG singrestr)))
              (cond ((eq 'prep (get-actavm-headcateg downtree))
       ; *** THis is for "per buttare dei mobili"
                       (build-sing-r-sem semtopic 
                            (skip-determiner (find-actavm-dep 'PREP-ARG downtree))))
                    (t (build-sing-r-sem semtopic downtree up-ident))))
           ((and (eq meaning '--on-relation)
                 (eq *LANGUAGE* 'english)		; events on October 7
                 (neq semtopic '££dialogue))
              (build-sing-r-sem semtopic 
                            (skip-determiner (find-actavm-dep 'PREP-ARG singrestr) up-ident)))
           (t (let ((prep-mean (first (leggi *PREP-TEMPLATES* meaning)))
                    (downtree (skip-determiner (get-preposition-arg singrestr)))
                    down-mean down-ident act-prep-mean down-type downdowntree)
    ;(format t "Prep sem - downtree: ~a~%" downtree)
    ;(break "Uno")
                 (cond ((null prep-mean)
                        ; *** in case the preposition has no associated preplate entry,
                          (setq down-ident (get-actavm-headlexident downtree))
                          (cond ((and (not (null up-ident))
                                      (not (null down-ident)))
                            ; *** if it links two items having an identifier, try to
                            ;     link the identifiers, via a subpart relation
                                   (build-subpart-repr up-ident down-ident semtopic downtree))
                                (t (build-sing-r-sem semtopic downtree))))
                       ((and (is-relational-conc semtopic)
                             (has-gramm-type (get-actavm-headlemma singrestr) '&neutral-prep))
                        ; *** if the upper concept is "relational", as "mother", the
                        ;     connecting preposition (mother of) simply marks an argument
                        ;     of the "mother" relation, without carrying any specific meaning
                        ; *** has-gramm-type in chunk-parser
                          (build-sing-r-sem semtopic downtree))
                       ((is-a-actavm-trace? singrestr) nil)
                       (t (setq down-mean (get-actavm-headlexmean downtree))
                          (setq down-ident (get-actavm-headlexident downtree))
                          (setq down-type (get-actavm-headtype downtree))
                          (cond ((and (eq 'adj (get-actavm-headcateg downtree))
                                      (memq down-type '(deitt demons)))
                                  ; *** in case the head of downtree is a demonstrative or
                                  ;     deictic adjective, move down one level (events of THIS
                                  ;     saturday; events of NEXT weekend)
                                   (cond ((eq down-type 'deitt)
                                            (setq downdowntree
                                                   (find-actavm-dep 'DET+INDEF-ARG downtree)))
                                         ((eq down-type 'demons)
                                            (setq downdowntree
                                                   (find-actavm-dep 'DET+DEF-ARG downtree))))
                                   (cond ((not (null downdowntree))
                                            (setq downtree downdowntree)
                                            (setq down-mean (get-actavm-headlexmean downtree))
                                            (setq down-ident 
                                                      (get-actavm-headlexident downtree))))))
                           ; *** now we have found the actual argument of the preposition, so
                           ;     we look for the actual meaning in the preplate table
                          (cond ((eq 'unambiguous prep-mean)
                                   (setq act-prep-mean meaning))
                                (t (setq act-prep-mean 
                                        (select-prep-mean prep-mean semtopic down-mean))))
                          (cond ((eq act-prep-mean 'fail)
                           ; *** a first case of failure could be the occurrence of a subtype or
                           ;     subpart relation. For instance in "Sud d'Italia", we do not
                           ;     have in the preplate table that "di" could mean that "Sud d'Italia"
                           ;     is a part-of Italia. So special processing
                                  (cond ((and (not (null down-ident))
                                              (not (null up-ident)))
                            ; *** if it links two items having an identifier, try to
                            ;     link the identifiers, via a subpart relation
                                           (let ((subpart-spec
                                                   (build-subpart-repr 
                                                       up-ident down-ident semtopic downtree)))
                                               (cond ((null subpart-spec)
                                                       (exception-internal 'semantic-error
                                                          "PROC/buildquery: no meaning selected for a preposition"
                                                          meaning prep-mean))
                                                     (t subpart-spec))))
                                        ((is-subclass-of-one-of down-mean 
                                                   '(££prop-value ££measureunit))
          ; *** check if there is an inversion in property val (giorno con valori di temperatura
          ;     elevati);
                                           (multiple-value-setq 
                                                (inv-dep down-mean act-prep-mean other-deps)
                                                (find-inverted-property 
                                                    downtree semtopic meaning))
                                           (cond ((null inv-dep)
                                                    (exception-internal 'semantic-error
                                                        "PROC/buildquery: no meaning selected for a preposition 1"))
            ; *** the inverted property has been found:
            ;     in inv-dep there is the subtree of the depending noun related with the property
            ;     (di temperatura), while in other-deps we have all other dependents of it 
            ;     (elevati)
                                                 (t (setq newtree
                                                       (build-inverted-tree 
                                                                  downtree inv-dep other-deps))
                                                    (multiple-value-setq (topics restrs)
                                                                (get-noun-restrictions newtree))
                                                    (compose-restr-path
                                                       (choose-best-ontpath
                                                           (find-shortest-path 
                                                               semtopic down-mean act-prep-mean))
                                                       (build-restr-sem
                                                            topics restrs
                                                            (get-actavm-headnumber newtree))))))
                                        (t (exception-internal 'semantic-error
                                             "PROC/buildquery: no meaning selected for a preposition 2"))))
                           ; *** the next for restrictions which do not carry information
                           ;     (e.g. "in programmazione"). These must be encoded in the
                           ;     preplate table
                           ;     ((equal act-prep-mean '(++NEUTRAL++))
                           ; *** in this case, it is assumed that the meaning of the upper
                           ;     concept is already included in the one of the lower concept
                           ;     (e.g. "month of june")
                           ;        (build-noun-sem down-mean downtree up-ident))
                                ((eq down-mean '££empty-conc) nil)
                                ((and (eq 'adj (get-actavm-headcateg downtree))
                                      (eq 'demons (get-actavm-headtype downtree)))
                                    (find-path-to-deictic semtopic downtree act-prep-mean))
                                ((null down-ident)
                                  ; *** the argument of the preposition is not a proper name
                                  (multiple-value-setq (topics restrs)
                                                         (get-noun-restrictions downtree))
                                  (compose-restr-path
                                       (choose-best-ontpath
                                            (find-shortest-path semtopic down-mean act-prep-mean))
                                       (build-restr-sem
                                           down-mean
                                           restrs
                                           (get-actavm-headnumber downtree))))
                                  ; *** the argument of the preposition is a proper name
                                (t 
                   ; (format t "Noun sem in build-prep-sem:~% downtree: ~a~% downtree sem:~a~%"
                   ;          downtree (build-noun-sem down-mean downtree up-ident))
                   ; (break "")
                                    (compose-restr-path
                                       (choose-best-ontpath
                                            (find-shortest-path semtopic down-mean act-prep-mean))
                                       (list (build-noun-sem
                                                down-mean downtree up-ident))))))))))))
                                ; *** the third argument of find-shortest path is intended to be a
                                ;     constraint, provided by the preposition, on the path to find
                                ;     in the ontology

; ***************************************************************************
; *** checks the presence of an inverted property, as in case
;     "a pool with three meters of depth"
;     In this case, we have:
;     1. deptree = "three meters of depth" (with "meters" as root)
;     2. up-noun-sem = ££pool
;     3. up-prep-sem = --with-relation
(defun find-inverted-property (deptree up-noun-sem up-prep-sem)
  (declare (special *PREP-TEMPLATES*))
  (let ((down-deps (get-actavm-deps-with-traces deptree)) 
         prep-mean act-prep-mean savdownsem savprepmean
         actdep down-sem path found-deps other-deps otherpath)
; *** the do loops on all dependents of the intermediate noun (meters)
      (do ((nxtdep (first down-deps) (first down-deps))
           (down-deps (rest down-deps) (rest down-deps)))
; *** exit from the loop
         ((null nxtdep)
     ; *** at the end, we have in found-deps the dependents involved in the inversion
     ;     (of depth) and in other-deps the other dependents (three)
           (cond ((null found-deps) nil)
                 ((= (length found-deps) 1)
     ; *** this is the case at hand; in found-deps we have a single dependent that
     ;     connects as a property the down-down item (depth) with the upper item
     ;     (pool). Now check if the intermediate item (meter) can be a value of the
     ;     down-down one (depth)
                   (setq otherpath 
                      (choose-best-ontpath
                           (find-shortest-path down-sem (get-actavm-headlexmean deptree))))
                   (cond ((includes-subclass-of otherpath '££has-value)
     ; *** now everything is ok: return the found dependent (of depth) and the other ones
                            (values (first found-deps) savdownsem savprepmean other-deps))))
                 (t (exception 'semantic-error "more than one path in inverted properties"))))
; *** body of the loop
         (cond ((equal nxtdep '(#\#)) nil)
               ((eq (get-actavm-headcateg nxtdep) 'PREP)
                  (setq actdep (find-actavm-dep 'PREP-ARG nxtdep))
                  (setq down-sem (get-actavm-headlexmean (skip-determiner actdep)))
      ; *** check if "pool with depth" has any meaning
                  (setq prep-mean (first (leggi *PREP-TEMPLATES* up-prep-sem))) 
                  (setq act-prep-mean (select-prep-mean prep-mean up-noun-sem down-sem))
                  (cond ((or (null act-prep-mean)
                             (eq act-prep-mean 'fail))
                           (setq other-deps (append1 other-deps nxtdep)))
      ; *** check if the path found for "pool with depth" is related with properties
      ;     i.e. if "depth" is a property of "pool"
                        (t (setq path 
                             (choose-best-ontpath
                                (find-shortest-path up-noun-sem down-sem act-prep-mean)))
                           (cond ((includes-subclass-of path '££has-property)
                                    (setq found-deps (append1 found-deps nxtdep))
                                    (setq savdownsem down-sem)
                                    (setq savprepmean act-prep-mean))
                                 (t (setq other-deps (append1 other-deps nxtdep)))))))
               (t (setq other-deps (append1 other-deps nxtdep)))))))

; ***************************************************************************
; *** this function reshuffles a tree, by extracting the portion related to the
;     measured property. The original structure is as follows (downtree)
;     (for "valori di temperatura elevati")
;
;                       head-noun (valori)
;                            |        \
;                         prep (di)   depX (elevati)
;                            |
;                 property-noun (temperatura)
;                            |
;                          depY (---)
;
;    - inv-dep is the subtree rooted in "prep". 
;    - other-deps are all other dependents (depX)
;    - In the example, there is just one other dependent, but in principle there could be
;      more than one.
;    - In the example, property-noun has no dependents, but one or more of them could be
;      present.
; *** The reshuffled tree should be:
;
;                 property-noun (temperatura)
;                            |            \
;                   head-noun (valori)   depY (---)
;                            |
;                       depX (elevati)
;
(defun build-inverted-tree (downtree inv-dep other-deps)
   (let ((newtree (find-actavm-dep 'PREP-ARG inv-dep)) newsubtree)
; *** newtree is the subtree rooted in "property-noun" (temperatura)
;     with depY, but without "valori"
       (setq newsubtree (list (assoc 'head downtree) 
                              (list 'dependents other-deps)))
; *** newsubtree is the subtree rooted in head-noun (valori) without the "di temperatura"
;     dependent
       (list (assoc 'head newtree)
             (list 'dependents (cons newsubtree (get-actavm-deps-with-traces newtree))))))

; ***************************************************************************
(defun build-subpart-repr (up-ident down-ident semtopic downtree)
  (let ((tempres (find-subtype-path up-ident down-ident 'subpart)))
      (cond ((null tempres)
               (build-sing-r-sem semtopic downtree))
   ; *** if tempres is not null, then the restriction has been
   ;     interpreted as a subtype (e.g. ££It-southern-region)
   ;     So, the result involves the composition of the path
   ;     from semtopic to the found subtype
            (t (setq up-path
                    (choose-best-ontpath (find-shortest-path semtopic tempres)))
   ; *** up-ident must be included in the result
               (cons (first up-path) (replace-has-instance (rest up-path)))))))

; ***************************************************************************
; *** this is for "stato di obsolescenza" (obsolescence status)
;     In this example, upmean is "££status" and downmean is "£obsolescence"
; *** it is also used for finding the involved subpart in the case of the
;     connection between £North (upmean) and £Italy (downmean). In this
;     case, we first find (via find-shortest-path) a sequence as 
;     <£North range-of &has-it-north-selector domain £North-Italy-part-of
;      domain-of &has-it-north-bigger range £Italy>
; *** then we see if in this sequence there is an instance of ££geogr-part-of
;     (in this example £North-Italy-part-of), then we look for its
;     'smaller-part' (in this example ££it-northern-region)
; *** interestingly, this also applies to adverbial modifications, where we 
;     have that "molto" (--intensifier-adv) is is an "Intensifier", so we have:
;     <--intensifier-adv- is-a ££intensifier range-of &intensification-selector
;      domain ££intensification-rel domain-of &intensification-arg range
;      £mare-agitato>
;     In this case, what need be found is the &intensification-value, which in
;     this case is £mare-molto-agitato
;     In order to apply te same procedure, we must have:
;     upmean=--intensifier-adv;   downmean=£mare-agitato
(defun find-subtype-path (upmean downmean searchtype)
   (let ((path (choose-best-ontpath
                    (find-shortest-path upmean downmean)))
          found)
       (cond ((null path) nil)
             (t (do* ((prevpath nil (cons nxtelem prevpath))
                      (nxtelem (first path) (first path))
                      (path (rest path) (rest path)))
                   ((or found (null nxtelem))
                     (cond ((or (null found)
                                (eq found 'fail)
                                (neq found searchtype)) nil)
                           ((eq found 'subtype)
    ; *** what has been found is something as
    ;           <££status domain-of &has-status-subtype range ££statusType ...>:
    ;     where: path = <££statusType>
    ;            nxtelem = range
    ;            prevpath = <&has-status-subtype domain-of ££status>
                              (cond ((eq nxtelem 'domain) (list (first path)))
                                    ((eq (second prevpath) 'domain-of)
                                       (list (third  prevpath)))
                                    (t (exception 'semantic-error
                                              "PROC/buildquery: ontology relation without domain"))))
                           ((eq found 'subpart)
                             (find-result-role (first prevpath) found))
                           ((eq found 'intens)
                             ;(format t "find-subtype-path: prevpath = ~a~%" prevpath)
                             ;(break "xbx")
        ; *** for "molto mosso", we have now:
        ;     prevpath = (&intens-rel-1 argument &moved-sea-intens-1 value-of £mare-agitato)
        ;     path = (arg-of &moved-sea-intens-3 value --intensifier-adv)
        ;     find-result-role -> (arg-of &moved-sea-intens-2 value £mare-molto-agitato)
        ; *** so, the final result is
        ;     (£mare-agitato value-of &moved-sea-intens-1
        ;                 argument £intens-rel-1
        ;                     (and ((arg-of &moved-sea-intens-3 value --intensifier-adv)
        ;                           (arg-of &moved-sea-intens-2 value £mare-molto-agitato))))
                             (append1 (reverse prevpath)
                                   (list 'and
                                        (list path
                                            (find-result-role 
                                                  (first prevpath) found)))))))
    ; **** body
                  (cond ((is-a-structural-item nxtelem) nil)
                        ((and (is-restriction-of nxtelem '&has-subtype)
                              (eq searchtype 'subtype))
                           (setq found 'subtype))
                        ((and (is-instance-of nxtelem '££intensification-rel)
                              (eq searchtype 'intens))
                           (setq found 'intens))
                        ((and (is-instance-of nxtelem '££geogr-part-of)
                              (eq searchtype 'subpart))
                           (setq found 'subpart))
        ; *** if in the path there is a relation which is not a description, 
        ; *** and it is not a role of a part-of, then is not a subtype path
                        ((and (is-relation nxtelem)
                              (not (is-restriction-of nxtelem '&has-description))
                              (not (is-restriction-of nxtelem '&part-bigger))
                              (not (is-restriction-of nxtelem '&part-smaller))
                              (not (is-restriction-of nxtelem '&part-selector)))
                           (setq found 'fail))
                         ))))))

;****************************************************************************
; *** a concept is a subtype of another concept if they are linked via a
;     relation that is a restriction of &has-subtype. In the involved path,
;     it may occur also a restriction of &has-description. This is partially
;     incorrect, but it enables me to take an ££obsolescenceStatusDescription
;     as a subtype of a ££status
(defun is-subtype (conc1 conc2)
 (let ((path (choose-best-ontpath (find-shortest-path conc1 conc2))) fail)
   (do ((nxtitem (first path) (first path))
        (path (rest path) (rest path)))
       ((or (null path) fail) (not fail))
       (cond ((and (is-relation nxtitem)
                   (not (is-restriction-of nxtitem '&has-subtype))
                   (not (is-restriction-of nxtitem '&has-description)))
                (setq fail t))))))

; ***************************************************************************
; *** as above, but on a list of concepts as second argument
(defun is-subtype-of-one-of (conc1 conclist)
   (cond ((null conclist) nil)
         ((is-subtype conc1 (first conclist)) t)
         (t (is-subtype-of-one-of conc1 (rest conclist)))))

; ***************************************************************************
; *** given a concept that is a reification of an instance of the 
;     &geogr-part-of relation, returns 
;     1. If selecttype = subpart:
;        the item that is its "smaller part" (i.e. from £south-Italy-part-of, it
;        returns ££it-southern-region)
;     2. If selecttype = intens:
;        the item that is its "intensified value" (i.e. from £intens-rel-1, it
;        returns £mare-molto-agitato)
;        relation = £intens-rel-1
(defun find-result-role (relation selecttype)
  (let ((roles
     ; *** the next cond, because subtype works on relations, while intens works
     ;     on relation instances
           (cond ((member selecttype '(subpart subtype)) (get relation 'domain-of))
                 ((eq selecttype 'intens) (get relation 'arg-of))))
         found)
     (do ((nxtrole (first roles) (first roles))
          (roles (rest roles) (rest roles)))
        ((or (null nxtrole) found)
   ; *** in found, we have the name of the "smaller part" relation
          (cond (found 
                  (cond ((member selecttype '(subtype subpart))
                           (first (get found 'range)))
                        ((eq selecttype 'intens)
     ; *** now, in found we have &moved-sea-intens-2
     ;     so, what is returned is 
     ;     (arg-of &moved-sea-intens-2 value £mare-molto-agitato)
                           (list 'arg-of found 'value
                               (first (get found 'value))))))
                (t nil)))
        (cond ((or (and (member selecttype '(subtype subpart))
                        (is-restriction-of nxtrole '&geogr-part-smaller))
                   (and (eq selecttype 'intens)
                        (is-relinstance-of nxtrole '&intensification-value)))
                 (setq found nxtrole))))))

; ***************************************************************************
(defun build-pron-sem (semtopic singrestr)
  (let ((meaning (get-actavm-headlexmean singrestr))
        (prontype (get-actavm-headtype singrestr)))
     (cond ((eq prontype 'ordin)
            ; *** ordinals standing alone, as "third", are pronouns
             (let ((down-restr (find-actavm-dep 'PREP-RMOD-MONTH singrestr)))
                (cond ((null down-restr)
                       ; *** not a date expression
                        (append1 
                            (choose-best-ontpath (find-shortest-path semtopic '££number))
                            `(eq ,(get-actavm-headvalue singrestr))))
                      (t (let ((down-mean (get-actavm-headlexmean down-restr)))
                            (cond ((eq down-mean '--di-relation)
                                     ; *** third of january
                                    (let* ((down-down-restr (find-actavm-dep 'PREP-ARG down-restr))
                                           (down-down-mean 
                                                 (get-actavm-headlexmean down-down-restr))
                                           (down-down-ident 
                                                 (get-actavm-headlexident down-down-restr))
                                           (res1 
                                              (append1
                                                 (choose-best-ontpath 
                                                    (find-shortest-path semtopic '££day-numb-descr))
                                                 (list 'eq (get-actavm-headvalue singrestr))))
                                           (res2 
                                              (append1
                                                 (choose-best-ontpath 
                                                    (find-shortest-path semtopic down-down-mean))
                                                 (list 'eq down-down-ident))))
                                        ; *** both res1 and res2 begin with "semtopic",
                                        ;     which is merged
                                        (list (first res1) 
                                              (list 'and 
                                                    (list (rest res1) (rest res2))))))))))))
           ((eq prontype 'interr)
             ; *** for interogative pronouns, even if they are singular, it is assumed that
             ;     the answer can be a list, so that the number is forced to 'pl
             (compose-restr-path
                 (choose-best-ontpath (find-shortest-path semtopic meaning))
                 (build-restr-sem meaning (get-verb-restrictions singrestr nil) 'pl)))
           ((memq (get-actavm-headlexmean singrestr)
                  '(§speaker §generic-ag §indef-ref NIL))
             ; *** "I" and "you" are not interpreted
             ;     NIL can occur in 3rd person polite questions ("Puo' dirmi ...")
              nil)
           ((and (eq (get-actavm-headperson singrestr) 3)
                 (eq (get-actavm-headnumber singrestr) 'pl))
              nil)
           ((eq (get-actavm-headlexmean singrestr) '--neutral-pron)
             ; *** this is "one" in "next one"
              (let* ((ordin-node (find-actavm-dep 'ADJC-RMOD singrestr))
                     (ordin-cat (get-actavm-headcateg ordin-node))
                     (ordin-ident (get-actavm-headlexident ordin-node)))
                  (cond ((and (eq ordin-cat 'adj)
                              (not (null ordin-ident)))
                          ; *** "the fourth one"
                          (choose-best-ontpath (find-shortest-path semtopic ordin-ident)))
                        ((or (eq ordin-cat 'num)
                             (null ordin-ident))
                          (append1 
                              (choose-best-ontpath (find-shortest-path semtopic '££number))
                              `(eq ,(get-actavm-headvalue ordin-node)))))))
             ; *** other pronouns?
           (t (compose-restr-path
                   (choose-best-ontpath (find-shortest-path semtopic meaning))
                   (build-restr-sem meaning 
                          (get-verb-restrictions singrestr nil)
                          (get-actavm-headnumber singrestr)))))))

; ***************************************************************************
(defun build-verb-sem (semtopic singrestr)
  (declare (special *PREP-TEMPLATES* *LANGUAGE* *SYSTEM-CONTEXT*))
  (let ((meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr)) downtree preparg)
      (cond ((is-a-relclause singrestr)
    ; *** this applies to non-reduced relative clauses
               (let* ((relpron-data (find-relpron-dep singrestr))
                      (deptype (first relpron-data))
                      (relpron-path (second relpron-data)))
                   (cond ((eq 'direct deptype)
            ; *** in this case, the pronoun is an unmarked case (subj or obj)
                            (compose-restr-path
                                (choose-best-ontpath (find-shortest-path semtopic meaning))
                                (build-restr-sem meaning (get-verb-restrictions singrestr nil t))))
                         ((eq 'simple deptype)
                ; *** the table on which you put the lamp
                ; *** relpron-path includes the single node "on"
                ;     what must be done is to interpret "put on table", and then reverse
                ;     the result
                            (let* ((prep-mean 
                                     (first 
                                        (leggi *PREP-TEMPLATES* 
                                           (get-actavm-headlexmean relpron-path))))
                                   (act-prep-mean 
                                     (select-prep-mean prep-mean meaning semtopic)))
                  ; *** meaning is the meaning of the verb; semtopic of the governing noun
                  ;     "the table on which you put the book" -->
                  ;     meaning=££put semtopic=££table
                                (cond ((eq act-prep-mean 'fail)
                                         (exception-internal 'semantic-error
                                             "PROC/buildquery: no meaning 2 selected for a preposition"))
                                      (t (compose-restr-path
                                             (choose-best-ontpath 
                                                    (find-shortest-path 
                                                             semtopic meaning act-prep-mean))
                                             (build-restr-sem meaning 
                                                    (get-verb-restrictions singrestr nil t)))))))
                         (t (exception-internal 'semantic-error
                                 "PROC/buildquery: complex path to a relpron")))))
            ((eq meaning '££modal-can)
       ; *** The modal "can" is skipped ("local rains can fall" --> "local rains fall")
               (build-verb-sem semtopic (find-actavm-dep 'VERB+MODAL-INDCOMPL singrestr)))
            ((eq 'participle (get-actavm-headmood singrestr))
              (compose-restr-path
                  (choose-best-ontpath (find-shortest-path semtopic meaning))
                  (build-restr-sem meaning (get-verb-restrictions singrestr nil t))))
            ((or (one-is-subclass-of meaning '££system-operation)
                 (one-is-subclass-of meaning '££external-service))
               (cond ((not (null ident))
                       ; *** "stop" and similar include their ident
                       (append1 
                            (choose-best-ontpath (find-shortest-path semtopic meaning))
                            (list 'eq ident)))
                       ; *** other system op (as "reserve") require a full treatment
                     (t (compose-restr-path
                            (choose-best-ontpath (find-shortest-path semtopic meaning))
                            (build-restr-sem meaning 
                                            (get-verb-restrictions singrestr nil t))))))
            ((eq meaning '££to-be)
              (let ((rmodpreparg (find-actavm-dep 'RMOD singrestr)) 
                    (locpreparg (find-actavm-dep 'VERB-INDCOMPL-LOC+IN singrestr)) 
                    vsubj)
                 ; the first condition for "I'm in Shaftsbury Avenue"
                 ; the second for "Sono in Shaftsbury Avenue"
             ; *** this is a special HOPS construction
                 (cond ((or (and (not (null rmodpreparg))
                                 (memq (get-actavm-headlexmean rmodpreparg)
                                       '(--in-relation --on-relation)))
                            (not (null locpreparg)))
                         (let* ((actpreparg
                                  (cond ((not (null rmodpreparg)) rmodpreparg)
                                        (t locpreparg)))
                                (vsubj (find-actavm-dep 'VERB-SUBJ singrestr))
                                (subjmean (get-actavm-headlexmean (skip-determiner vsubj))))
                             (cond ((eq subjmean '§speaker)
                                      (build-sing-r-sem semtopic actpreparg))
                                   (t (build-restr-sem semtopic
                                          (list (list 'single vsubj actpreparg)))))))
             ; *** the next should be the standard treatment of the copula
                       (t (setq vsubj (find-actavm-dep 'VERB-SUBJ singrestr))
                          (setq preparg (find-actavm-dep 'VERB-PREDCOMPL+SUBJ singrestr))
                          (cond ((and (not (null vsubj))
                                      (not (null preparg)))
                                   (build-to-be-sem semtopic vsubj preparg nil))
		                (t (compose-restr-path
			                  (choose-best-ontpath
                                              (find-shortest-path semtopic meaning))
			                  (build-restr-sem 
			                     meaning 
                                             (get-verb-restrictions singrestr nil t)))))))))
      ; *** main verb: "Where I can BUY the ticket ..."
      ;     In this case, "Where" is the topic, "I" (subject of "can") and 
              ;     "BUY the ticket ..." are the restrictions
	    ((eq meaning '££to-have)
      ; *** in case of Catalan "Hi Ha .."
               (cond ((and (eq *LANGUAGE* 'catalan)
                           (not (null (find-actavm-dep 'pron-rmod-loc+metaph singrestr))))
                       (setq downtree (find-actavm-dep 'VERB-SUBJ singrestr))
                       (cond ((is-a-actavm-trace? downtree)
         ; *** here, we should look for the actual subject of "there is", which is sometimes
         ;     attached as RMOD
                               (exception 'semantic-error 
                                   "PROC/buildquery: No match between default infos and semrestrs in final-build-query"))))
                     ((eq *SYSTEM-CONTEXT* 'atlas)
         ; *** in the ATLAS context, "to have" is normally used in expressions as "we have nice weather"
         ;     this is encoded in the ontology simply by specifying that ££to-have may be a useful
         ;     ££dialogue-topic
                        (compose-restr-path
                             (choose-best-ontpath (find-shortest-path semtopic meaning))
                             (build-restr-sem meaning (get-verb-restrictions singrestr nil t))))))
            (t (compose-restr-path
                    (choose-best-ontpath (find-shortest-path semtopic meaning))
                    (build-restr-sem meaning (get-verb-restrictions singrestr nil t)))))))

; ***************************************************************************
; *** a verbal dependent is a (non reduced) relative clause if its link
;     to the govermor is "verb-rmod+relcl"
(defun is-a-relclause (avmtree)
   (eq (get-actavm-headlink avmtree) 'VERB-RMOD+RELCL))

; ***************************************************************************
; *** given a subtree associated with a relative clause, returns a pair, where
;     the second element is its subtree (dependent) that includes a relative pronoun
;     while the first element can be:
;     direct: the relpron is a direct dependent of the verb
;     simple: the relpron is a dependent of a preposition linked to the verb
;     multiple: the relpron is included in a complex PP (the table on the top of which
;               you put the lamp) 
(defun find-relpron-dep (avmtree)
   (let ((deps (get-actavm-deps-with-traces avmtree)) checkres found)
      (do ((nxtdep (first deps) (first deps))
           (deps (rest deps) (rest deps))
           (deppos 1 (1+ deppos)))
         ((or found (null nxtdep))
            (cond (found found)
                  (t (exception 'semantic-error
                         "PROC/buildquery: relative clause without relative pronoun"))))
         (cond ((equal nxtdep '(#\#)) nil)
               (t (setq checkres (check-relpron-dep nxtdep 1))
                  (cond ((= checkres 0) nil)
                        ((= checkres 1) (setq found (list 'direct nxtdep)))
                        ((= checkres 2) (setq found (list 'simple nxtdep)))
                        (t (setq found (list 'multiple nxtdep)))))))))

; ***************************************************************************
; *** returns 0 if the subtree does not includes a relative pronoun
;     otherwise returns the depth of the relpron (starting from 1)
;     "with the man" --> 0
;     "which" --> 1
;     "whose" --> 1 !!! ERROR: A CASE TO HANDLE !!!
;     "on which" --> 2
;     "the top of which" --> 3
;     "on the top of which" --> 4
;     ....
(defun check-relpron-dep (subtree depth)
   (cond ((null subtree) 0)
         ((and (eq 'PRON (get-actavm-headcateg subtree))
               (eq 'RELAT (get-actavm-headtype subtree)))
             (cond ((and (memb-or-eq 'poss (get-actavm-headcase subtree))
                         (eq depth 1))
     ; *** this should apply to "whose" and "cui". It actually does not work, since
     ;     the lexicon does not associate the "poss" case with these forms
                      2)
                   (t depth)))
         ((eq 'PREP (get-actavm-headcateg subtree))
             (let ((preparg (find-actavm-dep 'prep-arg subtree)))
                 (cond ((not (null preparg)) 
                          (check-relpron-dep (skip-determiner preparg) (1+ depth)))
                       (t (exception 'semantic-error
                              "PROC/buildquery: preposition without argument")))))
         ((eq 'NOUN (get-actavm-headcateg subtree))
             (check-relpron-dep (find-actavm-dep 'PREP-RMOD subtree) (1+ depth)))
         (t 0)))

; ***************************************************************************
; *** it builds the semantic representation of a copula
; *** semtopic is the item governing the verb "to be"
;     vsubj is the subject of "to be"
;     predcompl is the verb-predcompl-subj
;     other-deps are other dependents (if any) of "to be"
(defun build-to-be-sem (semtopic vsubj predcompl other-deps)
  (let* ((vsubj-nodet (skip-determiner vsubj))
         (subjmean (get-actavm-headlexmean vsubj-nodet))
         (subj-interp (build-noun-sem semtopic vsubj-nodet))
         (predcat (get-actavm-headcateg predcompl))
         (predmean (get-actavm-headlexmean predcompl))
         downpred predsem
        )
     (cond ((eq 'art predcat)	
   ; *** a determiner is the head: determiners are currently ignored, so the
   ;     tree is travelled downward
              (setq downpred (skip-determiner predcompl))
              (cond ((eq 'noun (get-actavm-headcateg downpred))
                       (setq predsem (build-noun-sem semtopic downpred nil)))
                    (t (exception 'semantic-error
                          "Strange predcompl in build-to-be-sem"))))
           ((eq 'adj predcat)
              (setq predsem (build-adj-sem subjmean predcompl)))
           ((eq 'noun predcat)
              (setq predsem (build-noun-sem subjmean predcompl nil)))
           ((eq 'prep predcat)
              (setq predsem (build-prep-sem subjmean predcompl nil)))
           ((eq 'pron predcat)
              (setq predsem (build-pron-sem subjmean predcompl)))
           (t (exception 'semantic-error "Unknown predcompl category in build-to-be-sem")))
   ; *** now, we have in subj-interp the semantic interpretation of the subject (wrt to the
   ;     item governing the copula) and in predsem the interpretation of the verb predcompl
   ; *** the just have to be put together
     ;(mult-compose-restr-path
     ;      (find-shortest-path semtopic meaning) restr-meaning)
   ; *** subj-interp should be something as (semtopic ... subjmean path1)
   ; *** predsem something as (subjmean path2)
   ;     So, we obtain (semtopic ... subjmean (and path1 path2))
    (merge-paths-with-pivot subj-interp predsem) 
      ))

; ***************************************************************************
; *** subj-interp should be something as (semtopic ... subjmean path1)
; *** predsem something as (subjmean path2)
;     So, we obtain (semtopic ... subjmean (and path1 path2))
(defun merge-paths-with-pivot (subj-interp predsem) 
   (cond ((null subj-interp)
            (exception 'semantic-error "in merge-paths-with-pivot"))
         ((eq (first subj-interp)
              (first predsem))
            (cons (first predsem)
                  (list (list 'and (list (rest subj-interp) (rest predsem))))))
         (t (cons (first subj-interp)
                  (merge-paths-with-pivot (rest subj-interp) predsem)))))

; ***************************************************************************
; *** builds the representation of a date, assuming that singrestr has, as
;     top element, a number (expressed either as a number or as an ordinal
;     adjective) and a dependent expressing a month
(defun build-date-repr (semtopic singrestr)
   (let ((down-restr (find-actavm-dep 'NOUN-RMOD-MONTH singrestr)))
       (cond ((null down-restr)
                (setq down-restr
                   (find-actavm-dep 'PREP-ARG (find-actavm-dep 'PREP-RMOD singrestr)))))
       (let* ((down-mean (get-actavm-headlexmean down-restr))
              (down-ident (get-actavm-headlexident down-restr))
              (res1 (append1
                       (choose-best-ontpath (find-shortest-path semtopic 
                                                 '££day-numb-descr))
                       (list 'eq (get-actavm-headvalue singrestr))))
              (res2 (append1
                       (choose-best-ontpath (find-shortest-path semtopic down-mean))
                       (list 'eq down-ident))))
                  ; *** both res1 and res2 begin with "semtopic", which is merged
           (list (first res1) 
                 (list 'and (list (rest res1) (rest res2)))))))

; ***************************************************************************
(defun final-build-query (default-infos semrestrs)
   (cond ((= 1 (length default-infos))
            (final-build-simple-query (first default-infos) (first semrestrs)))
         (t (let ((compl-query (mapcar #'final-build-simple-query default-infos semrestrs)) temp)
   ; *** compl-query actually is a list of queries, one for each search target
   ;     now, the attempt is to merge them in a single query
                (setq temp (final-build-complex-query compl-query))))))
   ; *** temp used just to debugging purposes
 
; ***************************************************************************
(defun final-build-simple-query (default-infos semrestrs)
; *** default-infos is a set of paths
;     (e.g. ( (££cinema-loc subclass-of ££location ...)
;             (££cinema subclass-of ££genre ...)))
;     The final element of default-infos must be a subclass of ££datatype
;     The first element of default-infos must be equal to the first element of semrestrs
; *** semrestrs is another path (starting from concept)
; *** the final query must be:
;     <select <concept last-four-elements-of-default-infos>
;      from concept
;      where concept <inverted-rem-path> semrestrs>
;     where inverted-rem-path is the inversion of the initial part of the
;      default-infos, apart from the last four elements
; *** so that the last elements must be:
;     <concept range-of relation domain descr-type subclass-of $some-datatype>
;     unless its length is 6  
; *** another possibility is that default-infos is just
;     <subclass-of some-datatype>
 (let ((taillength 6) topic act-default-infos found act-semr)
  (setq act-default-infos
       (do ((first-default (first default-infos) (first default-infos))
            (default-infos (rest default-infos) (rest default-infos)))
           ((or found (null first-default))
               (cond (found found)
                     (t (exception 'semantic-error 
                              "PROC/buildquery: No match between default infos and semrestrs in final-build-query"))))
           (cond ((atom (first semrestrs))
                   (cond ((eq (first semrestrs) (first first-default))
                            (setq found first-default)
                            (setq act-semr semrestrs))))
   ; *** semrestrs could be a list e.g. "Voldria informació sobre teatre",
   ;     where semrestrs is ((££theater) (££theater-loc))
                 (t (do ((first-semr (first semrestrs) (first semrestrs))
                         (semrestrs (rest semrestrs) (rest semrestrs)))
                        ((or found (null first-semr)))
                       (cond ((eq (first first-semr) (first first-default))
                               (setq found first-default)
                               (setq act-semr first-semr))))))))
  (setq topic (first act-default-infos))
  (setq act-default-infos (rest act-default-infos))
  (cond ((is-subclass-of (ult act-default-infos) '££datatype)
          (cond ((or (= 2 (length act-default-infos))
                     (> (length act-default-infos) (1- taillength)))
                  (let (sel-clause from-clause wh-clause)
                     (cond ((<= (length act-default-infos) taillength)
                             (setq sel-clause act-default-infos)
                             (setq from-clause topic)
                             (setq wh-clause act-semr))
                           (t (let ((split-pos (- (length act-default-infos)
                                                  (1+ taillength))))
                                  (setq sel-clause 
                                        (nthcdr (1+ split-pos) act-default-infos))
                                  (setq from-clause 
                                        (nth split-pos act-default-infos))
                                  (setq wh-clause 
                                     (rem-subclass-of-itself
                                        (append 
                                           (inv-range-dom
                                               (reverse (first-n (1+ split-pos)
                                                                 act-default-infos)))
                                           act-semr)
                                        nil)))))
                     `(select ,sel-clause from ,from-clause where ,wh-clause)))
                ((<= (length act-semr) 1)
        ; *** The next added for "Puo' dirmi il titolo?"
                     `(select ,default-infos from ,topic))))
       (t (exception 'semantic-error "PROC/buildquery: Problems in final-build-query")))))

; ***************************************************************************
(defun final-build-complex-query (queries)
   (let* ((common-restr-path (ult (find-common-path queries)))
          (prefixes (remove-tail queries common-restr-path))
          newprefixes)
       (cond ((null prefixes) (break "in final-build-complex-query"))
    ; *** prefixes and queries are assumed to be parallel lists
             (t (do ((nxtprefix (first prefixes) (first prefixes))
                     (prefixes (rest prefixes) (rest prefixes))
                     (nxtquery (first queries) (first savqueries))
                     (savqueries (rest queries) (rest savqueries)))
                   ((null nxtquery))
                  (setq newprefixes 
                      (cons (rem-subclass-of-itself
                               (append (reverse (inv-range-dom nxtprefix)) (second nxtquery))
                               nil)
                            newprefixes)))
                `(select ,(put-in-and newprefixes)
                  from ,(first common-restr-path)
                  where ,common-restr-path)))))

; ***************************************************************************
(defun remove-tail (queries tail)
   (int-rem-tail (mapcar 'sixth queries) (reverse tail) (length tail)))

(defun int-rem-tail (restrs revtail taillength)
   (cond ((null restrs) nil)
         (t (let ((remresult (int-rem-tail (rest restrs) revtail taillength)))
                 (cond ((eq remresult 'fail) 'fail)
                       ((is-a-prefix revtail (reverse (first restrs)))
                          (cons (first-n (- (length (first restrs)) taillength) 
                                         (first restrs))
                                remresult))
                       (t 'fail))))))

; ***************************************************************************
(defun put-in-and (args)
   (cond ((eq 1 (length args)) args)
         (t (let ((result (list (first args))))
               (dolist (nxtarg (rest args) result)
                   (setq result (append result (list 'and nxtarg))))))))
                   
; ***************************************************************************
; *** this looks for common sublists
;     e.g. queries = < <x1, x2, x3, x4, x5, x6> <y1, x3, x4, x5, y2, y3> <z1, x2, x3, x4, z2> >
;     result: <x3, x4>
(defun find-common-path (queries)
; *** the operation is made on the "where" part of the queries, i.e. the sixth element
   ;(format t "find-common-path: queries = ~a ~%" queries)
   ;(break "")
   (let ((restrictions (mapcar #'sixth queries))
         found-common diff all-found-common tempmatch 
         (useless-starts '(domain range has-subclass domain-of range-of subclass-of)))
       (setq tempmatch (list (first restrictions)))
   ; *** tempmatch is the list of matches found until the n-th list
       (dolist (nxtrestr (rest restrictions) tempmatch)
     ; *** for all further lists in queries, update tempmatch
   ;(format t "find-common-path: dolist1; tempmatch = ~a ~%" tempmatch)
   ;(format t "                           nxtrestr = ~a ~%" nxtrestr)
   ;(break "")
        (setq tempmatch
         (dolist (nxtmatch tempmatch all-found-common)
   ;(format t "find-common-path: dolist2; nxtmatch = ~a ~%" nxtmatch)
   ;(break "")
             (setq all-found-common nil)
     ; *** in nxtmatch one previous match
            (do ((nxtconc (first nxtrestr) (first nxtrestr))
                 (nxtrestr (rest nxtrestr) (rest nxtrestr)))
        ; *** each element of a new query list is inspected
                ((null nxtconc) all-found-common)
   ;(format t "find-common-path: do3; nxtconc = ~a ~%" nxtconc)
   ;(break "")
                (setq found-common nil)
          ; *** and matched against a previous result
                (do ((nxtmatchel (first nxtmatch) (first nxtmatch))
                     (nxtmatch (rest nxtmatch) (rest nxtmatch)))
                    ((null nxtmatchel))
   ;(format t "find-common-path: do4; nxtmatchel = ~a ~%" nxtmatchel)
   ;(break "")
                    (cond ((and (eq nxtconc nxtmatchel)
                                (not (member nxtconc useless-starts)))
          ; *** we are at the beginning of a match
                             (setq found-common (list nxtconc))
          ; *** if the element is part of the list (e.g. x3 in the example above), then
          ;     in nxtmatch there is the part of the previous match following (and
          ;     including) the element in question (e.g. <x3, x4, x5, x6>)
          ; *** the two sublists are scanned in parallel until one ends or a different
          ;     element is found
          ; *** in found-common, we add common elements
                             (setq diff nil)
                             (do ((nxtpart (first nxtrestr) (first temprestr))
                                  (temprestr (rest nxtrestr) (rest temprestr))
                                  (nxtpartold (first nxtmatch) (first tempmatch))
                                  (tempmatch (rest nxtmatch) (rest tempmatch)))
                                 ((or (null nxtpart) (null nxtpartold) diff)
                                    (setq all-found-common 
                                      (cons (reverse found-common) all-found-common)))
   ;(format t "find-common-path: do5; nxtpart = ~a ~%" nxtpart)
   ;(break "")
                                 (cond ((eq nxtpart nxtpartold)
                                          (setq found-common
                                                (cons nxtpart found-common)))
                                       (t (setq diff t))))))))))
        (setq tempmatch 
           (remove-sublists 
                (sort tempmatch #'(lambda (x y) (< (length x) (length y)))))))))

; ***************************************************************************
(defun remove-sublists (lists)
   (cond ((null lists) nil)
         ((is-a-sublist (first lists) (rest lists))
             (remove-sublists (rest lists)))
         (t (cons (first lists)
                  (remove-sublists (rest lists))))))

; ***************************************************************************
; *** returns t if l is a sublist of one of the lists
(defun is-a-sublist (l lists)
   (cond ((null lists) nil)
         ((sublist? l (first lists)) t)
         (t (is-a-sublist l (rest lists)))))

; ***************************************************************************
; *** returns t if l1 is a sublist of l2
(defun sublist? (l1 l2)
  (let (found)
      (do ((nl2 t (first xl2))
           (xl2 l2 (rest xl2)))
          ((or (null nl2) found) found)
         (setq found (is-a-prefix l1 xl2)))))

; ***************************************************************************
; *** returns t if l1 is equal to the first part of l2
(defun is-a-prefix (l1 l2)
   (cond ((null l1) t)
         ((eq (first l1) (first l2))
            (is-a-prefix (rest l1) (rest l2)))
         (t nil)))

; ***************************************************************************
(defun final-build-givinfo (topic semrestrs)
; *** this is a simplified version of final-build-query above:
;     no query target!
; *** it is applied when the semantic head is ££get-info or ££see
;     The idea is that the user is not asking anything, but s/he is
;     just providing the system with new information. An example is
;     at end of dialogue, when the user must simply tell the system
;     if s/he wants to continue or not
; *** topic is a concept (currently, ££dialogue)
; *** semrestrs is a path (starting from concept)
; *** the final query must be:
;      about concept
;      where concept semrestrs
  (cond ((and (= 1 (length semrestrs))
              (listp (first semrestrs)))
          (setq semrestrs (first semrestrs)))
        (t (exception 'semantic-error "PROC/buildquery: in final-build-givinfo")))
  (cond ((atom topic)
           `(about ,topic where ,semrestrs))
        ((= 1 (length topic))
           `(about ,(first topic) where ,semrestrs))
        (t `(about ,(build-with-topic-list) where ,semrestrs))))

; ***************************************************************************
;     this removes "class subclass-of class" and "class has-subclass class"
(defun rem-subclass-of-itself (path prevpath)
  (cond ((null path) (reverse prevpath))
        ((and (memq (first path) '(has-subclass subclass-of))
                    (eq (second path) (first prevpath)))
           (rem-subclass-of-itself (rest (rest path)) prevpath))
        (t (rem-subclass-of-itself (rest path) (cons (first path) prevpath)))))

; ***************************************************************************
; *** this changes "domain" into "domain-of", "domain-of" into "domain" and so on
;     it also removes "class subclass-of class" and "class has-subclass class"
(defun inv-range-dom (path)
  (mapcar #'inv-r-d path))

; ***************************************************************************
(defun inv-r-d (singitem)
  (case singitem
      (range 'range-of)
      (domain 'domain-of)
      (range-of 'range)
      (domain-of 'domain)
      (has-subclass 'subclass-of)
      (subclass-of 'has-subclass)
      (otherwise singitem)))

; ***************************************************************************
; *** this should choose the best of the paths connecting two nodes in
;     the ontology
; *** the preferred path is the shorter one, after removing all subclass links
;     This gives preference to 
;     (££MOVIE HAS-SUBCLASS ££COMEDY-MOVIE RANGE-OF &HAS-COMEDY-M-T DOMAIN £COMEDY)
;     With respect to
;     (££MOVIE RANGE-OF &HAS-MOVIE-TYPE DOMAIN ££MOVIE-TYPE HAS-INSTANCE £COMEDY)
; *** the variable "targetbest" records the fact that a path corresponds to something
;     that has been stored in "query-target". For instance, if the competing paths 
;     are 
;       <$STRING HAS-SUBCLASS ££AUTHORIZATIONREQUESTREASON RANGE-OF &HAS-AUTHORIZ-REASON
;        DOMAIN ££AUTHORIZATIONREQUEST SUBCLASS-OF ££REQUEST>
;     and 
;       <$STRING HAS-SUBCLASS ££AUTHORIZATIONREQUESTCODE RANGE-OF &HAS-AUTHORIZ-CODE
;        DOMAIN ££AUTHORIZATIONREQUEST SUBCLASS-OF ££REQUEST>
;     and the path through the CODE is the standard path to report the infos about
;     a ££REQUEST, then the second path must be preferred
(defun choose-best-ontpath (ont-semrestrs)
  (let ((best (first ont-semrestrs)) bestshort bestlength no-subcl-nxt 
        targetbest targetnxt instancebest instancenxt initgreetbest initgreetnxt)
     (cond ((eq best 'fail) 
              (setq bestshort 'fail)
              (setq bestlength 1000)
              (setq targetbest nil)
              (setq instancebest nil)
              (setq initgreetbest nil))
           (t (setq bestshort (remove-subcl-links best))
              (setq bestlength (length bestshort))
              (setq targetbest (is-target-path best))
              (setq instancebest (is-relinstance-path best))
              (setq initgreetbest (is-initgreet-path best))))
     (do ((next (second ont-semrestrs) (first ont-semrestrs))
          (ont-semrestrs (rest (rest ont-semrestrs)) (rest ont-semrestrs)))
         ((null next) best)
         (cond ((neq 'fail next)
                  (cond ((eq 'fail best) (setq best next))
                        (t (setq no-subcl-nxt (remove-subcl-links next))
                           (cond ((< (length no-subcl-nxt) bestlength)
                                    (setq best next)
                                    (setq bestlength (length no-subcl-nxt)))
                                 ((= (length no-subcl-nxt) bestlength)
                                    (setq targetnxt (is-target-path next))
                                    (setq instancenxt (is-relinstance-path next))
                                    (setq initgreetnxt (is-initgreet-path next))
                                    (cond ((and targetnxt (not targetbest))
                                             (setq best next)
                                             (setq targetbest t))
                                          ((and instancenxt (not instancebest))
                                             (setq best next)
                                             (setq instancebest t))
                                          ((and initgreetnxt (not initgreetbest))
                                             (setq best next)
                                             (setq initgreetbest t))))))))))))

; *******************************************************************
; *** returns true if the path involves a relation instance. It is recognized
;     by the fact that the path ends as:
;     (... xxx ARG-OF rrr VALUE yyy)
;     An example could be "£classical-music ARG-OF &has-music-type33 VALUE £classic"
; *** These paths could be preferred if the goal is to retrieve the node xxx on
;     the basis of yyy.
(defun is-relinstance-path (path)
   (let ((revpath (reverse path)))
      (and (eq 'value (second revpath))
           (eq 'arg-of (fourth revpath)))))

; *******************************************************************
; *** this checks if a path has, as a sublist, one of the default query targets
(defun is-target-path (path)
   (declare (special *DEFAULT-CONCEPT-INFOS*))
   (let* ((string-path (reverse (member '$string path)))
          (inv-string-path (inv-range-dom string-path))
          (string-rev-path (reverse (member '$string (reverse path))))
          (inv-string-rev-path (inv-range-dom string-rev-path))
          found nxtcheck)
       (do ((nxttarget (first *DEFAULT-CONCEPT-INFOS*) (first defaults))
            (defaults (rest *DEFAULT-CONCEPT-INFOS*) (rest defaults)))
           ((or found (null nxttarget)) found)
           (setq nxtcheck (cons (first nxttarget) (second nxttarget)))
           (setq found (or (part-of-list nxtcheck string-path)
                           (part-of-list nxtcheck inv-string-path)
                           (part-of-list nxtcheck string-rev-path)
                           (part-of-list nxtcheck inv-string-rev-path))))))

; *******************************************************************
; *** this checks (in ATLAS) if there are no previos sentence (beginning of a 
;     report) and the path includes the ££greetings concept
(defun is-initgreet-path (path)
  (declare (special *PREV-SENTENCES*))
  (and (null *PREV-SENTENCES*) (member '££greetings path)))

; *******************************************************************
; *** removes any subclass link together with the following concept
(defun remove-subcl-links (path)
  (cond ((null path) nil)
        ((memq (first path) '(has-subclass subclass-of))
            (remove-subcl-links (rest (rest path))))
        (t (cons (first path) (remove-subcl-links (rest path))))))

; *******************************************************************
; *** this repeats the next functions on different first segments
;     This may happen when the first segment is ambiguous
; *** This returns a single (unambiguous) path
(defun mult-compose-restr-path (first-segment other-segments)
  (let (all-paths)
     (cond ((listp (first first-segment))		; *** ambiguous concept
             (dolist (nxtfirst first-segment)
                   (setq all-paths 
                       (cons (compose-restr-path nxtfirst other-segments) all-paths)))
             (choose-best-ontpath all-paths))
           (t (compose-restr-path first-segment other-segments)))))

; *******************************************************************
; *** this takes a path on the ontology, representing a search restriction,
;     and composes it with a set of other paths representing sub-restrictions.
; *** this only takes the different other-segments and looks for the first one
;     that matches first-segment
; *** Input: first-segment (e.g. (££ticket-counter subclass-of ££office))
;            other-segments (e.g. ((££office xxx) (££task yyy)) or 
;                            ((£office (and xxx zzz))) )
; ***        Note that other-segments can include some relevant path, but
;            also some wrong interpretation (in this context) of the
;            restrictions; this "join" should cut off these "wrong" paths
; ***        In case the optional noun-number is not nil, it contains the
;            syntactic number of a noun, an info that has to be kept in
;            the semantic query
; *** Output: the composed path
;            e.g. (££ticket-counter subclass-of ££office (and xxx zzz))
(defun compose-restr-path (first-segment other-segments)
  (cond ((null other-segments) 'fail)
        (t (let ((first-res (sing-comp-restr-path first-segment (first other-segments))))
               (cond ((null first-res)
                        (compose-restr-path first-segment (rest other-segments)))
                     (t first-res))))))

;*****************************************************************
(defun sing-comp-restr-path (first-segment second-segment)
  (cond ((and (listp (ult first-segment))
              (eq 'eq (first (ult first-segment)))
              (eq (ult (butlast first-segment)) (first second-segment)))
    ; *** first-segment is obtained from a proper name, so that its
    ;     last element is (eq name)
           (cond ((null (rest second-segment)) first-segment)
                 (t (append (butlast first-segment)
                           `((and (,(ult first-segment)
                                   ,(rest second-segment))))))))
        ((listp (first second-segment))		; *** ambiguous concept
           (let ((chosen-second 
                      (select-segment (ult first-segment) second-segment)))
              (cond ((null chosen-second) 
                       'fail)
                    (t (compose-restr-path first-segment chosen-second)))))
        (t (let ((match? (tail-head-match first-segment second-segment)))
               (cond ((eq match? 'fail)
    ; *** in case there is no match, another possibility is that second-segment starts with
    ;     a subtype specification of the last item of first-segment
                        (setq match? (subtype-match first-segment second-segment))
                        (cond ((eq match? 'fail) nil)
        ; *** in the next case, match? includes the full path, since first-segment had to be
        ;     re-computed inside subtype-match
                              (t match?)))
                     (t (append first-segment match?)))))))
   
; *******************************************************************
; *** the tail of the first list must overlap with the beginning of the
;     second list (ex. (a b c d e f) (d e f g h))
; *** it returns the not common part of the second list (i.e. (g h))
(defun tail-head-match (firstl secondl)
  (let ((revfirst (reverse firstl)) found rempart count)
   ; *** moves ahead on the reversed first list (f e d c b a)
    (setq rempart
       (do* ((tailfirst nil (cons nxtfirst tailfirst))
             (nxtfirst (first revfirst) (first revfirst))
             (revfirst (rest revfirst) (rest revfirst)))
     ; *** when the reversed first list is empty no match has been found
     ; *** if a match was found ("d" in the example above), then the
     ;     tail collected in tailfirst is matched against the beginning of secondl
           ((or (null nxtfirst) found)
              (cond (found 
                      (cond ((equal tailfirst (first-n (length tailfirst) secondl))
                               (nthcdr (length tailfirst) secondl))
                            (t 'fail)))
                    (t 'fail)))
           (setq found (equal nxtfirst (first secondl)))))
   ; *** now, it checks if the remaining part is the reverse of the end of the
   ;     first list (ex. (a b c d) (d c))
   ;     This case may happen if the second list comes from a subclass restriction
   ;     (as in "richieste di acquisto in deroga" [lit. requests of purchase in
   ;     waiver"]). Here, just the extra-purchases (purchase in waiver) require a
   ;     request, so that "richieste di acquisto" is interpreted as
   ;     "requests of an extrapurchase, which is a purchase" (actually <££REQUEST
   ;     HAS-SUBCLASS ££AUTHORIZATIONREQUEST RANGE-OF &HAS-PURCHASE-REQUEST DOMAIN
   ;     ££EXTRAPURCHASE SUBCLASS-OF ££PURCHASE>) where "which is a purchase" is 
   ;     included since the lexical meaning of the word "acquisto" is ££PURCHASE.
   ; *** But "acquisto in deroga" is interpreted as <££PURCHASE HAS-SUBCLASS 
   ;     ££EXTRAPURCHASE> so that the info is repeated twice.
   ; *** N.B. The arc labels are reversed (e.g. HAS-SUBCLASS vs. SUBCLASS-OF)
   ; !!!!!!!!!!!!!!!!!!!!!!!!!!!! N.B. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ; *** The part below has been removed since, though it is in general ok, it may
   ;     happen that the result of this function is just a piece of a more complex
   ;     structure. For instance, in "Stato e posizione delle componenti installate"
   ;     what happens is that "posizione delle componenti installate" could undergo
   ;     a simplification of the type foreseen here, since it actually means
   ;     "the position where have been installed the items that have been installed"
   ;     But the first conjunct is interpreted as "the status of the item that have
   ;     been installed", so that the final "that have been installed" must be kept
   ;     also in the second conjunct in order to put together the two pieces.
    ;(setq count (1- (- (length firstl) (length rempart))))
    ;(cond ((< count 0) rempart)
    ;      ((equal (inv-range-dom (reverse rempart))
    ;              (nthcdr count (butlast firstl)))
    ;         nil)
    ;      (t rempart))
    rempart))

; *******************************************************************
; *** this tries to ascertain if, second-segment
;     starts with a subtype of the last item of first-segment 
(defun subtype-match (first-segment second-segment)
   (cond ((is-subtype-of-one-of (first second-segment) (inlist (ult first-segment)))
           (let ((newfirst (choose-best-ontpath 
                               (find-shortest-path
                                  (first first-segment)
                                  (first second-segment)))))
   ; *** newfirst replaces the old "first-segment"; it was a path going from some
   ;     concept to the "unrestricted" start of the second segment; since it has
   ;     been restricted by the lower dependents, it must replace the unrestricted
   ;     one, i.e. a new "best path" has to be looked up
               (append newfirst (rest second-segment))))
         (t 'fail)))

;(defun subtype-match (first-segment other-segments)
;   (cond ((null other-segments) nil)
;         ((is-subtype-of-one-of (first (first other-segments)) (ult first-segment))
;            (let ((newfirst (choose-best-ontpath 
;                               (find-shortest-path
;                                  (first first-segment)
;                                  (first (first other-segments))))))
;                (cons (append newfirst (rest (first other-segments)))
;                      (subtype-match first-segment (rest other-segments)))))
;         (t (subtype-match first-segment (rest other-segments)))))

; *******************************************************************
; *** chooses, among "segments", the one that begins with "concept"
(defun select-segment (concept segments)
 (declare (special topic-changes))
 (let ((start-segment (first (first segments))))
  (cond ((null segments) nil)
        ((eq concept start-segment)
            (first segments))
        ((member (list concept start-segment) topic-changes :test #'equal)
            (append `(,concept has-subclass) (first segments)))
        ((member (list start-segment concept) topic-changes) :test #'equal
            (append `(,concept subclass-of) (first segments)))
        (t (select-segment concept (rest segments))))))

; *******************************************************************
(defun get-default-infos (topic)
  (declare (special *DEFAULT-CONCEPT-INFOS*))
  (let ((alltopics (inlist topic)) found)
      (do ((nxttopic (first alltopics) (first alltopics))
           (alltopics (rest alltopics) (rest alltopics)))
          ((null nxttopic)
             (cond ((null found)
                      (exception 'semantic-error
                           "PROC/buildquery: missing default infos 1 for" topic))
                   (t found)))
          (setq found 
              (cons (cons nxttopic (int-get-def-inf nxttopic))
                    found)))))

(defun int-get-def-inf (topic)
  (declare (special *DEFAULT-CONCEPT-INFOS*))
  (let ((path (find-default-path (list (list nil topic)))) lastnode)
      (cond ((null path)
                (exception 'semantic-error
                         "PROC/buildquery: missing default infos 2 for" topic))
            (t (setq lastnode (ult path))
               (cond ((is-subclass-of lastnode '££datatype) path)
                     (t (append path (int-get-def-inf lastnode))))))))

; **************************************************************************
; *** looks for a default path for any concept in the given list
;     Actually, topics is a list of pairs <subclass-path concept>
;     where subclass-path is the path of subclass-of links that have been
;     traversed to reach the concept under analysis
;     The functions goes up in a breadth-first fashion, in order to find,
;     for ££trn-location, also a possible definition of ££location
;     Unfortunately a node can have multiple parents, so the search
;     becomes a little more complex
(defun find-default-path (topics)
  (declare (special *DEFAULT-CONCEPT-INFOS*))
  (let (found upconcs result)
    (cond ((null topics) nil)
          (t (setq result
                (do ((nxttopic (first topics) (first topics))
                     (topics (rest topics) (rest topics)))
                    ((or (second found) (null nxttopic))
                        found)
                    (cond ((eq (second nxttopic) '££datatype)
                            (setq found (list (first nxttopic) (list '££datatype))))
                          (t (setq found 
                               (list (first nxttopic)
                                     (first (leggi *DEFAULT-CONCEPT-INFOS* (second nxttopic)))))))
                  ; *** if, for a given concept, no def is found, try with its superconcepts
                    (cond ((null (second found))
                              (setq upconcs 
                                   (append upconcs 
                                       (mapcar #'(lambda (x) 
                                                    (list (append1 (first nxttopic)
                                                                   (second nxttopic))
                                                           x))
                                               (get (second nxttopic) 'subclass-of))))))))
            (cond ((null (second result))
                    (find-default-path upconcs))
                  (t (build-subclass-path result)))))))

; *******************************************************************
(defun build-subclass-path (foundpath)
   (let ((subclass-path (first foundpath))
         (target-path (second foundpath))
         resultpath)
     (do ((nxtconc (first subclass-path) (first subclass-path))
          (subclass-path (rest subclass-path) (rest subclass-path)))
         ((null nxtconc) (append resultpath target-path))
         (setq resultpath
             (append resultpath `(subclass-of ,nxtconc))))))

; *******************************************************************
; *** returns all restrictions of theme-subtree, which is assumed to be a verbal
;     subtree. All dependents are included, except the already considered
;     topic-subtree, punctuation marks, and auxiliaries.
; *** in "looking for information about the opera", theme-subtree is "about ..." and 
;     topic-subtree is "opera ..."
; ** INPUT:
;  >>> theme-subtree: the subtree (usually headed by the verb) inside which the
;      restrctions must be found
;  >>> topic-subtree: the subtree (usually a dependent of the verb) that has
;      already been recognized as the topic of the question
;  >>> skip-head: used for getting the actual restrictions of the verb. When it
;      is NIL, then the full verb subtree could act as the resulting restriction
; ** OUTPUT: a list of restrictions.
;     A restriction is a triple <applic-range actual-restr head-node>
;     where:
;     - applic-range can take the values:
;        "single": this means that it applies just to "head-node"
;        "distrib": this means that it applies to all topics (it is for conjunctions)
;        "verb": this means that it comes from a verbal argument
;     - actual-restr is the subtree which is the restriction
;     - head-node is the tree node that governs actual-restr
;    This result is due to the need to treat cases as "type and location of X"
;     Here, the restriction is "of X", it is of type "distrib", since it applies
;     both to "type" and to "location", and the head-node is the one of "location".
;     This enables the interpreter to move up from "location" looking for previous
;     conjuncts to which the restriction must be applied
; *** in the case of verbs, this is not particularly useful, so that the result is
;     <verb actual-restr nil>
;     N.B. This could be relevant for "where and how can I buy a ticket?", but here
;          the syntactic structure is different
(defun get-verb-restrictions (theme-subtree topic-subtree &optional skip-head)
  (declare (special +FULL-TREE+))
  (cond ((equal theme-subtree topic-subtree)
           nil)
   ; *** the two inputs may be equal, for instance, in case of "vorrei
   ;     un'informazione". In this case, because of the search process
   ;     both "theme-subtree" and "topic-subtree" refer to "un'informazione"
        ((and (not skip-head) (member (get-actavm-headlexmean theme-subtree) '(££put)))
   ; *** In case the sentence is "Where can I put ..", the entire subtree headed
   ;     by "put" is the restrictions of "where"
           (list (list 'verb theme-subtree nil)))
        (t (let ((deps (get-actavm-deps-with-traces theme-subtree)) 
                  restrs finrestrs temp-prep)
             (setq finrestrs
               (dolist (nxtdep deps restrs)
                   (cond ((and (not (equal nxtdep '(#\#)))
                               (eq (get-actavm-headlink nxtdep) 'CONTIN+PREP))
                            (setq nxtdep (find-actavm-dep 'PREP-ARG nxtdep))))
                   (cond ((equal nxtdep '(#\#)) nil)
                         ((equal nxtdep topic-subtree) nil)
                         ((memq (get-actavm-headlexmean nxtdep)
                                     '(--dummy-prep --about-relation --after-relation))
                           ; *** --after-relation has been included to cope with
                           ;     "I'm after ..." in the sense of "looking for"
                           ;     It must be handled more properly in case more standard
                           ;     senses are involved
                            (let ((down-tree
                                    (skip-determiner (find-actavm-dep 'prep-arg nxtdep))))
                                (cond ((equal topic-subtree down-tree)
                                         nil)
                                      ((is-a-date-descr? down-tree)
                                         (setq restrs 
                                             (cons (list 'verb down-tree nil) restrs)))
                                      (t (setq restrs 
                                             (cons (list 'verb nxtdep nil) restrs))))))
                         ((and (is-a-actavm-trace? nxtdep)
                               (or (eq 'empty (get-actavm-headcoref nxtdep))
                                   (equal (get-actavm-headcorefline nxtdep)
                                          (get-actavm-headnumb 
                                             (find-actavm-parent theme-subtree (list +FULL-TREE+))))))
       ; *** the next dependent is a trace of the topic
                            nil)
       ; *** the next for subtrees including relative pronouns (see build-verb-sem)
                         ((neq 0 (check-relpron-dep nxtdep 1)) nil)
                         ((eq (get-actavm-headcateg nxtdep) 'prep)
    ; *** in case the next dependent is headed by a preposition,
                            (cond ((equal (find-actavm-dep 'PREP-ARG nxtdep)
                                          topic-subtree)
            ; it could be the involved topic ("A che ora comincia ...")
            ; in this case, the preposition saved in temp-prep
                                    (setq temp-prep (get-actavm-head nxtdep)))
            ; or any other dependent
                                  (t (setq restrs (cons (list 'verb nxtdep nil) restrs)))))
                         ((memq (get-actavm-headlink nxtdep) 
       ; *** the last one for "ci" in "ci sono" ("there are")
                                '(END SEPARATOR PARTICLE PRON-RMOD-LOC+METAPH))
                            nil)
                         ((and (eq (get-actavm-headlink nxtdep) 'VERB-PREDCOMPL+SUBJ)
                               (eq (get-actavm-headlexmean nxtdep) '££entity))
       ; *** this for "Which are ...?", where "which" should not count as a restriction
                            nil)
                         ((eq (get-actavm-headtype nxtdep) 'AUX) nil)
       ; *** the next for the first person subject of the verb
                         ((and (eq (get-actavm-headcateg nxtdep) 'pron)
			       (memq (get-actavm-headlexmean nxtdep) 
                                     '(§speaker §myself §indef-ref)))
                            nil)
       ; *** the next for "Quale è" ("what is ...")
                         ((and (eq (get-actavm-headcateg nxtdep) 'pron)
			       (eq (get-actavm-headlexmean nxtdep) '--q-pron)) nil)
       ; *** the next for "Quale è" ("what is ...")
                         ((subtree-member topic-subtree nxtdep) nil)
       ; *** the next for locutions
                         ((eq (get-actavm-headlemma nxtdep) 
                              (get-actavm-headlemma theme-subtree)) nil)
       ; *** the next for standard cases, where the dependent acts as a restriction
                         (t (setq restrs (cons (list 'verb nxtdep nil) restrs))))))
               (cond ((and (eq 'verb (get-actavm-headcateg theme-subtree))
                           (is-content-verb (get-actavm-headlexmean theme-subtree))
                           (member-or-prep topic-subtree (get-actavm-deps-with-traces theme-subtree)))
   ; *** in case the theme-subtree refers to a content verb (e.g. "conduct")
   ;     the restriction is given by the same tree, apart of the removed
   ;     dependents. For instance, in "Which concerts were conducted by Abbado
   ;     last week", "concerts" is the topic, and "conducted by Abbado last week"
   ;     is the (only) restriction.
   ; *** The last condition is to avoid that when handling "conducted by Abbado 
   ;     next week", this branch is followed, leading to infinite recursion
                       (cond ((null temp-prep)
                                `(verb 
                                  ((head ,(get-actavm-head theme-subtree))
                                   (dependents ,finrestrs))
                                  nil))
   ; *** In case the topic was headed by a preposition, it is inserted as
   ;     head of the restriction: "In which theater they play La Locandiera"
   ;     topic --> "theater"
   ;     restriction --> "in play La Locandiera"
                             (t `(verb
                                  ((head ,temp-prep)
                                   (dependents
                                       ((#\#)
                                        ((head ,(subst-head-val 
                                                   (get-actavm-head theme-subtree)
                                                    'link 'prep-arg))
                                         (dependents ,finrestrs)))))
                                  nil))))
                     (t finrestrs))))))

; *******************************************************************
; *** returns true if any of the elements in deps is equal to topic-subtree
;     or is a PP whose argument is topic-subtree
(defun member-or-prep (topic-subtree deps)
   (cond ((null deps) nil)
         ((equal (first deps) '(#\#))
            (member-or-prep topic-subtree (rest deps)))
         ((equal topic-subtree (first deps)) t)
         ((and (eq (get-actavm-headcateg (first deps)) 'prep)
               (equal topic-subtree
                      (find-actavm-dep 'prep-arg (first deps))))
            t)
         (t (member-or-prep topic-subtree (rest deps)))))

; *******************************************************************
(defun is-content-verb (meaning)
   (memq meaning 
        '(££conduct ££buy ££begin ££to-cost ££collect ££end ££possess)))

; *******************************************************************
; *** returns a pair whose first element are the topic concepts and the
;     second one are all restrictions
;     A restriction is a triple <applic-range actual-restr head-node>
;     where:
;     - applic-range can take the values:
;        "single": this means that it applies just to "head-node"
;        "distrib": this means that it applies to all topics (it is for conjunctions)
;        "verb": this means that it comes from a verbal argument
;     - actual-restr is the subtree which is the restriction
;     - head-node is the tree node that governs actual-restr
; *** this result is due to the need to treat cases as "type and location of X"
;     Here, the restriction is "of X", it is of type "distrib", since it applies
;     both to "type" and to "location", and the head-node is the one of "location".
;     This enables the interpreter to move up from "location" looking for previous
;     conjuncts to which the restriction must be applied
; *** All dependents are included except the marker of the head position
;     and locution continuations.
(defun get-noun-restrictions (noun-subtree &optional for-goal conjunc)
  (let ((deps (get-actavm-deps-with-traces noun-subtree)) 
        restrs newtopic newrestrs distributivity
        (topics (list (get-actavm-headlexmean noun-subtree))))
      (dolist (nxtdep deps (values topics restrs))
          (cond ((equal nxtdep '(#\#)) nil)
   ; *** the next for locutions
                ((eq (get-actavm-headlemma nxtdep) 
                     (get-actavm-headlemma noun-subtree)) nil)
   ; *** punctuation marks are ignored
                ((eq (get-actavm-headcateg nxtdep) 'punct) nil)
   ; *** the next for coordinations which are intended to mean a list of
   ;     goals, and not a restriction
                ((and for-goal
                      (eq (get-actavm-headcateg nxtdep) 'conj)
                      (eq (get-actavm-headtype nxtdep) 'coord))
                   (let ((downnode (skip-determiner (find-actavm-dep 'COORD2ND nxtdep))))
                      (cond ((memq (get-actavm-headcateg downnode) '(noun prep adj))
                               (multiple-value-setq (newtopic newrestrs)
                                       (get-noun-restrictions downnode for-goal t))
                               (setq topics (append topics newtopic))
                               (setq restrs (append restrs newrestrs)))
                            (t (exception 'semantic-error
                                      "PROC/buildquery: unknown categ for a 2nd conjunct")))))
                (t (cond (for-goal (setq distributivity 'distrib))
                         (t (setq distributivity 'single)))
                   (cond ((eq (get-actavm-headcateg nxtdep) 'prep)
                    ; *** the next for subtype descriptions (Pseudo-locutions); they refer to 
                    ;     expressions as "stato di obsolescenza", where "obsolescence" does not seem
                    ;     to be a "restriction" in the sense of search condition, but rather a
                    ;     specification of a status type
                            (let* ((downnode (skip-determiner (find-actavm-dep 'PREP-ARG nxtdep)))
                                   (down-ident (get-actavm-headlexident downnode)))
                                (cond ((not (null down-ident))
                                        (let ((subtype-spec
                                                (find-subtype-path (ult topics) down-ident 'subtype)))
                                            (cond ((not (null subtype-spec))
                                                    (setq topics 
                                                       (append (butlast topics) subtype-spec)))
                    ; *** if it is a preposition, but not a subtype spec, it is a true restriction
                                                  (t (setq restrs 
                                                       (append restrs 
                                                          (find-all-conjuncts
                                                             nxtdep noun-subtree distributivity)))))))
                    ; *** if it is a preposition, but without a down ident, it is a true restriction
                                      (t (setq restrs 
                                             (append restrs 
                                                  (find-all-conjuncts
                                                          nxtdep noun-subtree distributivity)))))))
                    ; *** if it is not a preposition, this is a true restriction; special handling
                    ;     of conjunctions; see the comments above for the keyword "distrib"
                         (conjunc
                            (setq restrs 
                                (append1 restrs (list 'distrib nxtdep (get-actavm-head noun-subtree)))))
   ; *** in the standard case, the dependent is added to the restrictions. The loop aims
   ;     at finding all conjuncts of the dependent, in order to use all of them as
   ;     restrictions
                         (t (setq restrs 
                               (append restrs 
                                   (find-all-conjuncts nxtdep noun-subtree distributivity))))))))))

; *******************************************************************
; *** this checks if a "downnode" modification of "upnode" has to be
;     interpreted as a subtype specification of "upnode". 
; *** This should be checked on the ontology, but it anticipates
;     ontological interpretation; so, either the result is cached somewhere
;     or the search is made twice
(defun is-subtype-restr (downnode upnode)
   (declare (special *SUBTYPES-SPEC*))
   (cond ((eq (get-actavm-headcateg downnode) 'prep)
            (is-subtype-restr (find-actavm-dep 'PREP-ARG downnode) upnode))
         ((memq (get-actavm-headcateg downnode) '(noun adj))
            (member (list (get-actavm-headlemma upnode) 
                          (get-actavm-headlemma downnode))
                  *SUBTYPES-SPEC* :test #'equal))
         (t nil)))
   
; *******************************************************************
(defun is-deictic-concept (concept)    ; e.g. "the city"
  (declare (special *DEICTIC-REFERENCE*))
   (leggi *DEICTIC-REFERENCE* concept))

(defun get-deictic-referent (concept)
  (declare (special *DEICTIC-REFERENCE*))
   (first (leggi *DEICTIC-REFERENCE* concept)))

; *******************************************************************
; *** this function aims at handling expressions including deictics,
;     as "this", "next". They can occur in two syntactic positions:
;     1. As governors of the noun, in case no article appears ("for
;        next week" "in this month" "in this city")
;     2. As modifiers of the noun ("for the next week" "la prossima
;        settimana")
; *** in the first case, semtopic is something above the whole expression
;     (often "££dialogue"), and singrestr is the expression. 
;     - ident is £next
;     - down-node is the one of "week"
;       it is reached via the "det+def-arg" arc for "this" and via the
;       det+indef-arg for "next". The reason is that the first is taken
;       as a demonstrative (type=demons), while the second is a deictic
;       (type=deitt). Perhaps, this could be changed at the lexical level.
;     - down-mean is ££week
; *** in the second case, semtopic is "££week" and singrestr is the
;     subtree including only "next". Note that "this" cannot occur in 
;     this case, neither in Italian, nor in English ("*the this week"
;     "*la questo mese"). This means that, since true deictics, in the
;     present context are the demonstratives, there is no possibility of
;     having a deictic referent (as ££city) here. "the next city" is
;     syntactically correct, but does not make sense in the present
;     context. Consequently:
;     - ident is £next
;     - down-node is nil
;     - down-mean is nil
(defun find-path-to-deictic (semtopic singrestr &optional prep-mean)
; *** this branch refers to structures as "next week", "this month", where
;     "singrestr" is the whole expression (and semtopic is, usually,
;     ££dialogue). In these cases, the resulting expression comes from
;     finding the path from "££dialogue" to "££week" (or "££month")
;     and adding the condition (eq £next), or (eq £this).
; *** Here, ££week is down-mean, and ident is £this
  (let ((meaning (get-actavm-headlexmean singrestr))
        (ident (get-actavm-headlexident singrestr))
        (categ (get-actavm-headcateg singrestr))
        (type (get-actavm-headtype singrestr))
        down-node down-mean)
     (cond ((or (memq '££deictic-descr meaning)
                (memq '££deictic-specification meaning))	; "other" items
             (cond ((eq 'demons type)
                      (setq down-node (find-actavm-dep 'DET+DEF-ARG singrestr)))
                   ((eq 'deitt type)
                      (setq down-node (find-actavm-dep 'DET+INDEF-ARG singrestr)))
                   (t (exception 'semantic-error
                           "PROC/buildquery: unknown deictic adj type " 
                                type)))
             (cond ((null down-node)
    ; *** we are in case 1 (the next week) 
                      (cond ((is-subclass-of semtopic '££time-interval)
                               `(,semtopic (eq ,ident)))
                            ((memq '££deictic-specification meaning)
                               `(,semtopic (eq ,ident)))
                            (t (exception 'semantic-error
                                "PROC/buildquery: unknown deictic concept 1: " 
                                semtopic))))
                    (t (setq down-mean (get-actavm-headlexmean down-node))
    ; *** we are in case 1 (next week, this week, this city) 
                      (cond ((is-deictic-concept down-mean)   
                                     ; e.g. "this city", "this moment"
                              (let ((deict-ref 
                                        (get-deictic-referent down-mean)))
                                 (cond ((eq deict-ref '%neutral)
                                  ; *** the next are for, e.g., "this monday"
                                         (let* ((down-ident 
                                                    (get-actavm-headlexident
                                                               down-node)))
                                            (cond ((null down-ident)
                                                    (choose-best-ontpath
                                                      (find-shortest-path
                                                           semtopic
                                                           down-mean
                                                           prep-mean)))
                                                  (t (choose-best-ontpath
                                                       (find-shortest-path
                                                           semtopic
                                                           down-ident
                                                           prep-mean))))))
                                  ; *** the next are for, e.g., "this city"
                                       (t (choose-best-ontpath
                                             (find-shortest-path
                                                 semtopic deict-ref
                                                 prep-mean))))))
                            ((is-subclass-of down-mean '££time-interval)
            ; *** this branch is for "next week", "this month", where
            ;     "singrestr" is the whole expression (and semtopic is, usually,
            ;     ££dialogue). In these cases, the resulting expression comes from
            ;     finding the path from "££dialogue" to "££week" (or "££month")
            ;     and adding the condition (eq £next), or (eq £this).
            ; *** Here, ££week is down-mean, and ident is £this
                               (append1
                                  (choose-best-ontpath 
                                     (find-shortest-path semtopic down-mean))
                                     `(eq ,ident)))
                            (t (exception 'semantic-error
                                  "PROC/buildquery: unknown deictic concept 2: " 
                                   down-mean))))))
           (t (exception 'semantic-error
                    "PROC/buildquery: unknown deictic adjective " meaning)))))

; *******************************************************************
; *** this sould return true if "concept" is a relational concept (e.g. type,
;     friend, ...). Currently, the only such concepts are "types"
(defun is-relational-conc (concept)
 ; *** "concept" may be a list in case of semantically ambiguous words
 ;     (e.g. "direct": direct-movie, conduct-concert)
   (cond ((atom concept)
            (is-subclass-of concept '££type))
         (t (let (found)
               (do ((nextc (first concept) (first concept))
                    (concept (rest concept) (rest concept)))
                   ((or (null nextc) found)
                      found)
                   (setq found (is-subclass-of nextc '££type)))))))

; *******************************************************************
; *** given the meaning of preposition, chooses the actual ontology concept
;     on the basis of the upper and lower concepts
(defun select-prep-mean (prep-mean up-conc down-conc)
  (let (found (up-conc (inlist up-conc)) (down-conc (inlist down-conc)))
      (do ((next-p-mean (first prep-mean) (first prep-mean))
           (prep-mean (rest prep-mean) (rest prep-mean)))
          ((or found (null next-p-mean))
             (cond (found
                     (cond ((eq found 'no-rel) nil)
                           (t found)))
                   (t 'fail)))
          (cond ((and (match-prep-subclasses up-conc (first (first next-p-mean)))
                      (match-prep-subclasses down-conc (second (first next-p-mean))))
                  (setq found (second next-p-mean)))))))

; *******************************************************************
; *** checks if any of the concepts in conclist is a subconcept of conc
(defun match-prep-subclasses (conclist conc)
  (cond ((null conclist) nil)
        ((is-subclass-of (first conclist) conc) t)
        (t (match-prep-subclasses (rest conclist) conc)))) 

; *******************************************************************
(defun is-a-date-descr? (avmtree)
   (let ((headcat (get-actavm-headcateg avmtree))
         (headtyp (get-actavm-headtype avmtree)))
      (or (and (or (eq headcat 'num)
      ; *** 26 June, June 26, 26th June, June 26th
                   (and (eq headcat 'ADJ)
                        (eq headtyp 'ORDIN)))
      ; *** seventh June, June seventh
               (not (null (find-actavm-dep 'NOUN-RMOD-MONTH avmtree))))
          (and (eq headcat 'NUM)
      ; *** 1 of June
               (eq '££month-descr
                  (get-actavm-headlexmean
                     (find-actavm-dep 'PREP-ARG
                            (find-actavm-dep 'PREP-RMOD avmtree)))))
          (and (and (eq headcat 'PRON)
                    (eq headtyp 'ORDIN)))
      ; *** seventh of June
               (not (null (find-actavm-dep 'PREP-RMOD-MONTH avmtree))))))

; *******************************************************************
(defun subtree-member (subtree tree)
   (cond ((null tree) nil)
         ((equal '(#\#) tree) nil)
         ((equal subtree tree) t)
         (t (int-subtree-member subtree (get-actavm-deps-with-traces tree)))))

; *******************************************************************
(defun int-subtree-member (subtree tree-list)
  (let (found)
      (do ((nxttree (first tree-list) (first tree-list))
           (tree-list (rest tree-list) (rest tree-list)))
          ((or (null nxttree) found) found)
          (setq found (subtree-member subtree nxttree)))))

; *******************************************************************
; *** this inspects a tree, checking if the root is the first element of a 
;     "and" sequence of conjuncts (John and Mary and Bill).
; *** in any case, it returns a list including all meanings of the various
;     conjuncts (a singleton list, if no such conjunct exists)
(defun get-topic-concepts (topic-tree)
   (let ((conjunct (find-actavm-dep 'COORD topic-tree)))
      (cond ((null conjunct)
               (list (get-actavm-headlexmean topic-tree)))
            ((eq '--and-operator (get-actavm-headlexmean conjunct))
               (let ((secondarg (find-actavm-dep 'COORD2ND conjunct)))
                   (cond ((null secondarg)
                             (exception 'semantic-error
                                  "PROC/buildquery: no second argument for an --and-operator"))
                         (t (cons (get-actavm-headlexmean topic-tree)
                                  (get-topic-concepts secondarg)))))))))
      
; *******************************************************************
; *** removes from a list of nodes all that have an upward link of the "CONTIN"
;     type, i.e. that are continuations of locutions
(defun remove-continuations (dependents)
  (cond ((null dependents) nil)
        ((or (equal (first dependents) '(#\#))
             (not (member 'contin (get-lab-ancestors (get-actavm-headlink (first dependents))))))
           (cons (first dependents) (remove-continuations (rest dependents))))
        (t (remove-continuations (rest dependents)))))
        

