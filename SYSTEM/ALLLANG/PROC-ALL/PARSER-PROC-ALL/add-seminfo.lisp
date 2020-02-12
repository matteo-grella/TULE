(in-package "USER")

; +++++++++++++++++++++++++ GENERAL +++++++++++++++++++++++++++++++
; ****************************************************************************
; *** puts the lines in "newlines" in place of the corresponding lines in "sent"
; *** returns two values: the new version of the sentence, and the line numbers
;     of the lines that have been replaced.
(defun subst-lines (sent newlines)
  (let (res substlinumbs newline (restlines newlines))
; *** loops on all lines in sent
    (do ((curline (first sent) (first sent))
         (sent (rest sent) (rest sent)))
        ((null curline) (values (reverse res) substlinumbs))
        (multiple-value-setq (newline restlines) (change-line? curline restlines))
        (setq res (cons newline res))
        (cond ((not (equal newline curline))
                 (setq substlinumbs (append1 substlinumbs (get-newtb-numb curline))))))))

; ****************************************************************************
; *** this checks if "curline" is among the lines in "newlines" and then
;     returns a pair with the line (changed or not, depending on its presence
;     in newlines) and newlines (where the possible "new" line matching
;     curline has been removed; otherwise unchanged)
;     (change-line? l3 (l1' l4' l7')) --> [l3 (l1' l4' l7')]
;     (change-line? l4 (l1' l4' l7')) --> [l4' (l1' l7')]
(defun change-line? (curline newlines)
  (let ((savnewlines newlines) found)
; *** loops on all lines in newlines
   (do* ((prevnew nil (cons firstnew prevnew))
         (firstnew (first newlines) (first newlines))
         (newlines (rest newlines) (rest newlines)))
       ((or (null firstnew) found)
; *** if curline has been found in newlines, then
         (cond (found (values found
                              (append (reverse (rest prevnew)) (list firstnew) newlines)))
               (t (values curline savnewlines))))
       (cond ((equal (get-newtb-numb curline) (get-newtb-numb firstnew))
                (setq found firstnew))))))

; +++++++++++++++++++++++++ CONJUNCTIONS ++++++++++++++++++++++++++
; *****************************************************************
; *** this loops on all lines of "sent" and extends the COORD and
;     COORD2ND labels
(defun adjust-tut-coords (sent)
  (let (resbuff coordstack coordtype prev1st prev2nd save2nds label
        (savesent sent))
     (do ((nxtline (first sent) (first sent))
          (sent (rest sent) (rest sent)))
        ((null nxtline) (reverse resbuff))
        (setq label (get-newtb-label nxtline))
        (cond ((null coordstack)
      ; *** we have not found a preceding coordination
                (cond ((or (eq label 'COORD)
                           (and (eq label 'TOP)
                                (eq (get-newtb-type nxtline) 'COORD)))
                         (setq coordtype (find-coord-type nxtline savesent))
         ; *** in coordtype the type of coordination
                         (setq coordstack
                             (cons (list (get-newtb-numb nxtline) 'COORD coordtype)
                                   coordstack))
         ; *** which is saved on the stack
                         (cond ((eq label 'COORD)
                                  (setq resbuff
                                      (cons (change-link nxtline (get-newtb-parent nxtline)
                                                 (make-coord-label 'COORD coordtype))
                                             resbuff)))
                               (t (setq resbuff (cons nxtline resbuff)))))
                      ((eq label 'COORD2ND)
                         (setq resbuff (cons nxtline resbuff)))
                      (t (setq resbuff (cons nxtline resbuff)))))
      ; *** we have found a preceding coordination
              (t (cond ((eq label 'COORD)
                         (setq prev2nd (is-in-save (get-newtb-parent nxtline) save2nds))
                         (cond ((null prev2nd)   ; **** first coord of a sequence
                                  (setq coordtype (find-coord-type nxtline savesent)))
                               (t (setq coordtype (third prev2nd))))
                         (setq coordstack
                             (cons (list (get-newtb-numb nxtline) 'coord coordtype)
                                   coordstack))
                         (setq resbuff (cons (change-link nxtline (get-newtb-parent nxtline)
                                                 (make-coord-label 'coord coordtype))
                                             resbuff)))
                      ((eq label 'COORD2ND)
                         (multiple-value-setq (prev1st coordstack)
                                (is-in-stack (get-newtb-parent nxtline) coordstack))
                         (cond ((null prev1st)
                                  (setq resbuff (cons nxtline resbuff)))
                               (t (setq resbuff
                                       (cons (change-link nxtline (get-newtb-parent nxtline)
                                                 (make-coord-label 'COORD2ND (third prev1st)))
                                             resbuff))
                                  (setq save2nds (cons (list (get-newtb-numb nxtline) 'COORD2ND
                                                            (third prev1st))
                                                       save2nds)))))
         ; *** not a coordination line
                      (t (setq resbuff (cons nxtline resbuff)))))))))

; *************************************************************************
; *** this specifies the type of coordination
(defun find-coord-type (nxtline sent)
  (let ((type (get-newtb-semtype nxtline)) gov-gov)
      (cond ((eq type 'coord)
               (setq gov-gov (find-parent (find-parent nxtline sent) sent))
               (cond ((memq (get-newtb-word gov-gov) '(tra fra))
                       'SYMMETRIC)
                     ((eq (get-synt-word nxtline) 'sia) 'CORRELAT)
                     (t 'BASE)))
            ((eq (get-newtb-word nxtline) #\,) 'base)
            ((or (eq type 'compar)
                 (and (eq (get-newtb-categ nxtline) 'prep)
                      (eq (get-newtb-word nxtline) 'di)))
               'compar)
            ((eq type 'disj) 'base)
            ((eq type 'advers) 'advers)
            ((eq type 'explic) 'esplic)
            (t nil))))

; *************************************************************************
(defun find-parent (line sent)
  (let ((parnumb (get-newtb-parent line)) found)
    (do ((curl (first sent) (first sent))
         (sent (rest sent) (rest sent)))
        ((or (null curl) found) found)
        (cond ((equal parnumb (get-newtb-numb curl)) (setq found curl))))))

; *************************************************************************
(defun is-in-stack (linumb coordstack)
 (let (found)
  (do* ((prev nil (cons nxtentry prev))
        (nxtentry (first coordstack) (first coordstack))
        (coordstack (rest coordstack) (rest coordstack)))
       ((or (null nxtentry) found)
          (cond (found
                   (values found (reverse (cons nxtentry prev))))
                (t (values nil (reverse prev)))))
       (cond ((equal linumb (first nxtentry))
                (setq found nxtentry))))))

; ************************************************************************
(defun is-in-save (linumb save2nds)
   (cond ((null save2nds) nil)
         ((equal linumb (first (first save2nds))) (first save2nds))
         (t (is-in-save linumb (rest save2nds)))))

; *************************************************************************
(defun make-coord-label (firstpart type)
   (cond ((null type) firstpart)
         (t (implode (append (explode firstpart) '(#\+) (explode type))))))

; +++++++++++++++++++++++++ AUXILIARIES +++++++++++++++++++++++++++
; ****************************************************************************
(defun adjust-aux (sent)
  (let ((savesent sent) newlines nextverb)
    (do ((line (first sent) (first sent))
         (sent (rest sent) (rest sent)))
       ((null line))
       (cond ((and (eq (get-newtb-categ line) 'verb)
                   (eq (get-newtb-type line) 'aux))
               (setq nextverb (find-first-verb sent))
               (cond ((eq (get-newtb-word line) 'essere)
                       (cond ((and (not (null nextverb))
                                   (memq (get-newtb-subcat nextverb) '(intrans refl)))
                                (setq newlabel 'AUX-TENSE))
                             (t (setq newlabel 'AUX-PASSIVE)))
                       (setq newlines
                              (cons (change-link line (get-newtb-parent line) 'AUX+TENSE)
                                    newlines)))
                     ((eq (get-newtb-word line) 'stare)
                       (cond ((and (not (null nextverb))
                                   (eq (get-newtb-mood nextverb) 'gerund))
                                (setq newlines
                                   (cons (change-link line (get-newtb-parent line)
                                                           'AUX+PROGRESSIVE)
                                         newlines)))))
                     ((eq (get-newtb-word line) 'venire)
                        (setq newlines
                             (cons (change-link line (get-newtb-parent line) 'AUX+PASSIVE)
                                         newlines)))))))
    (subst-lines savesent newlines)))

; ****************************************************************************
(defun find-first-verb (sent)
  (cond ((null sent) nil)
        ((eq (get-newtb-categ (first sent)) 'verb) (first sent))
        (t (find-first-verb (rest sent)))))

; +++++++++++++++++++++++++ MODIFIERS +++++++++++++++++++++++++++++
; ****************************************************************************
(defun adjust-rmod (line sentbuff)
; *** line is the line with a RMOD label
;     sentbuff is the whole sentence
; *** this is implemented as a loop on the whole sentence; it is useless, but
;     it is made in view of a deeper analysis of the subtree headed by the RMOD
  (let ((rmodlemma (get-newtb-word line))
        (rmodlabel (base-uppercase (get-newtb-label line)))
        (rmodcateg (get-newtb-categ line))
        (rmodtype (get-newtb-type line))
        (rmodsemtype (get-newtb-semtype line))
        (savesent sentbuff)
        actlabel)
; *** loops on all lines of "sentbuff"
    (do ((curline (first sentbuff) (first sentbuff))
         (sentbuff (rest sentbuff) (rest sentbuff)))
        ((or (equal curline line) (null curline))
           (cond ((null curline) (exception "Rmod line not found"))
                 ((eq rmodcateg 'prep)
                    (cond ((eq rmodsemtype 'time)
                             (setq actlabel 'PREP-RMOD-TIME))
                          ((eq rmodsemtype 'loc)
                             (setq actlabel 'PREP-RMOD-LOC))
                          (t (setq actlabel
                                (check-prep-rmod curline savesent)))))
                 ((eq rmodcateg 'adv)
                    (cond ((eq rmodsemtype 'time)
                             (setq actlabel 'ADVB-RMOD-TIME))
                          ((eq rmodsemtype 'loc)
                             (setq actlabel 'ADVB-RMOD-LOC))
                          ((eq rmodtype 'neg)
                             (setq actlabel 'ADVB-RMOD-NEG))
                          (t (setq actlabel 'ADVB-RMOD))))
                 ((eq rmodcateg 'noun)
       ; *** this works on nouns, but the semantic characterization is only checked
       ;     on the head now, not on the whole substructure
                    (cond ((inh-member rmodlemma '£time)
                             (setq actlabel 'NOUN-RMOD-TIME))
                          ((inh-member rmodlemma '£gen-loc)
                             (setq actlabel 'NOUN-RMOD-LOC))
                          (t (setq actlabel 'NOUN-RMOD))))
                 ((eq rmodcateg 'pron)
                    (cond ((memq (get-newtb-word line) '(ci ne))
                             (setq actlabel 'PRON-RMOD))
                          (t (setq actlabel (get-newtb-label line)))))
                 (t (setq actlabel (get-newtb-label line))))
           (change-link curline (get-newtb-parent curline) actlabel)))))

; ****************************************************************************
(defun change-link (intline parentlinumb label)
  (list (first intline) (second intline) (third intline)
        (list parentlinumb (base-lowercase label))))

; ****************************************************************************
(defun check-prep-rmod (line sent)
 ; *** line is the line of the preposition
  (let ((savesent sent) (preplinumb (get-newtb-numb line)) found)
     (do ((curline (first sent) (first sent))
          (sent (rest sent) (rest sent)))
         ((or (null curline) found)
           (cond (found (prep-comp-type line found))
                 (t ;(break "Prep without dependent in check-prep-rmod: continue")
                    'comp)))
         (cond ((equal (get-newtb-parent curline) preplinumb)
                  (setq found curline))))))

; ****************************************************************************
; *** prepline is the line of the preposition
;     depline is the line of the governed word (notice that determiners have
;             already been inverted)
(defun prep-comp-type (prepline depline)
   (let ((prep (get-newtb-word prepline))
         (rmodlemma (get-newtb-word depline)))
       (cond ((memq prep '(da in))
                (cond ((eq 'num (get-newtb-categ depline)) 'PREP-RMOD-TIME)
                      ((inh-member rmodlemma '£time) 'PREP-RMOD-TIME)
                      ((or (eq 'proper (get-newtb-type depline))
                           (inh-member rmodlemma '£gen-loc))
                         'PREP-RMOD-LOC)
                      (t 'PREP-RMOD)))
             ((eq prep 'su) 'PREP-RMOD-LOC)
             ((eq prep 'come) 'PREP-RMOD)
             ((eq prep 'a)
               (cond ((or (inh-member rmodlemma '£gen-loc)
                          (memq (get-newtb-semtype depline)
                                '(££city ££state ££continent)))
                      'PREP-RMOD-LOC)
                     (t 'PREP-RMOD)))
             ((memq prep '(tra fra))
                (cond ((inh-member rmodlemma '£time) 'PREP-RMOD-TIME)
                      (t 'PREP-RMOD)))
             ((eq prep 'di)
                (cond ((inh-member rmodlemma '£time) 'PREP-RMOD-TIME)
                      (t 'PREP-RMOD)))
             ((eq prep 'per)
                (cond ((and (inh-member rmodlemma '£time)
                            (not (inh-member rmodlemma '£volta)))
                         'PREP-RMOD-TIME)
                      (t 'PREP-RMOD)))
             (t 'PREP-RMOD))))

