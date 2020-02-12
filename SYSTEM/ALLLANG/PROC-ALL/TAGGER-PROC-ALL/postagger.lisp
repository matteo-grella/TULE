(in-package "USER")

; ***************************************************************
; *** any data file must include two initial lines of the form:
;	FILE-LABEL P
;	INITIAL-SENT-NUMBER xxx
;     (not case-sensitive)
;     this function returns P and xxx
(defun read-header (inport)
  (let (head1 head2 file-lab init-numb)
     (setq head1 (read inport))
     (cond ((neq head1 'file-label)
	     (format t "WARNING: missing 'file-label' line in input file. Default file label is 'PROVA'~%")
             (setq file-lab 'prova))
	   (t (setq file-lab (read inport))))
     (setq head2 (read inport))
     (cond ((neq head2 'initial-sent-number)
	     (format t "WARNING: missing 'initial-sent-number' line in input file. Default initial number is 1~%~%")
             (setq init-numb 1))
	   (t (setq init-numb (read inport))))
     (values file-lab init-numb)))

; ************************************************************************
; *** it performs the actual disambiguation
; *** INPUT:
;  >>> phr-scanner+analyser: a pair whose first element is the list of the
;      words in the output format of the tokenizer, and the second is the
;      result of the morphological analysis on those words. It is assumed that
;      the two lists are parallel
; ========== the remaining (optional) parameters appear just in case the function =====
; ========== is working on data in text files, in which case all of them must appear: =
;  >>> file-label: identifier of the file (as specified in the header of the input file)
;  >>> sent-numb: number of the sentence
;  >>> outpt: port of the .dis file
;  >>> outpt2: port of the .tb file
;  >>> outpt3: port of the .tball file
;  >>> outpt4: port of the .sim file
; *** SIDE EFFECT:
;  >>> the result (in TUT format) is printed on the files .tb, .tball, .dis, .sim
; *** OUTPUT:
;  >>> the result of the extraction of the useful data from the result of the disambiguation
;      (extractfraseinfos below). The format of this output is a "flat AVM format", where
;      the lines are aligned exactly as in TUT format, but the data of a single line are
;      represented as pairs <attributes,values> 
(defun postagger
	(phr-scanner+analyser &optional file-label sent-numb outpt outpt2 outpt3 outpt4)
  (declare (special glob-time tag-time wri-time))
  ; *** glob-time, tag-time e wri-time globals in file-ana-text+tag 
  ;     (ALLLANG/PROC-ALL/top-level-fun)
  (let ((frase-in (mapcar #'first phr-scanner+analyser))
        (frase-out (mapcar #'second phr-scanner+analyser))
        disamb-res)
     (cond ((not (null file-label))
             (setq glob-time (get-internal-run-time))))
     (setq disamb-res (lexdisamb frase-in frase-out))
     (cond ((not (null file-label))
             (setq tag-time (+ tag-time (- (get-internal-run-time) glob-time)))
             (setq glob-time (get-internal-run-time))
             (printfrasenewtb outpt outpt2 outpt3 outpt4
	              frase-in (first disamb-res) (second disamb-res) file-label sent-numb)
             (setq wri-time (+ wri-time (- (get-internal-run-time) glob-time)))))
     (extractfraseinfos frase-in (first disamb-res))))

;************************************************************************
; *** it reads a sentence from the file. It goes on until it believes that
;     a sentence is finished (period, semicolon, question mark, etc.)
;     The separation is only partially correct
(defun leggifr (inputport)
 (let (frase-in frase-out)
  (do* ((inpword (double-read-line inputport nil 'end)
                 (double-read-line inputport nil 'end))
        (nextwd (read inputport nil 'end) (read inputport nil 'end)))
; *** inpword is the input word; nextwd is the analysed word
; *** lex-dis-prendi extracts an info from compound words; the last parameter
;     specifies from which element of the compound the info must be taken
       ((and (not (stringp nextwd))
	     (or (eq 'end nextwd)
	         (and (memq 'segnointer (lex-dis-prendi nextwd 'cat 'last))
		      (intersection '(#\! #\. #\: #\; #\?)
			      	    (lex-dis-prendi nextwd 'word 'last)))))
	 (cond ((and (null frase-out) (eq 'end nextwd)) "FINE")
	       ((eq 'end nextwd)
		  (list (nreverse frase-in) (nreverse frase-out)))
	       (t (list (nreverse (cons inpword frase-in))
			(nreverse (cons nextwd frase-out))))))
; *** double-read-line returns strings; frase-in is a list of strings
      (setq frase-in (cons inpword frase-in))
      (setq frase-out (cons nextwd frase-out)))))

;************************************************************************
; *** it reads a line, but if it is the empty string, it reads the next
(defun double-read-line (inpport par1 par2)
 (let ((dato (read-line inpport par1 par2)))
      (cond ((equal dato "") (read-line inpport par1 par2))
            (t dato))))

;************************************************************************
; *** it actually disambiguates, by applying the rules
; *** INPUT:
;  >>> frasin: the list of the input words (output of 'tokenizer');
;  >>> frase: result of the morpholexical analysis
(defun lexdisamb (frasin frase)
 (declare (special *MARKERS* *LANGUAGE*))
 (let (wrdchoice reason categs diff-cat xwords savelocut tempreason
	chosencat word-that-cat is-a-proper? continue
    ;    tag-glob-time (loc-mark-time 0) (act-disamb-time 0) 
         )
; *** the form of locut-buffer is: (component1 ... componentn end-locut)
; *** locut-buffer and locut-buff-ahead are initialized outside the loop
    ; (setq tag-glob-time (get-internal-run-time))
   (do* ((disambword nil (compose-interps wrdchoice disword))
         (all-beg-frase nil (cons disambword all-beg-frase))
         (beg-frase nil (cons (first disambword) beg-frase))
         (disword (first frase) (first rest-frase))
         (rest-frase (rest frase) (rest rest-frase))
         (firstcateg nil nil)
         (inpword (first frasin) (first rest-frasin))
         (rest-frasin (rest frasin) (rest rest-frasin))
         (locut-buffer (check-locut frase) locut-buff-ahead)
         (locut-buff-ahead (check-locut (rest frase))
			               (check-locut rest-frase))
         (marker-buffer (check-marker-or frasin frase *MARKERS* nil)
                        marker-buff-ahead)
         (marker-buff-ahead
	                (check-marker-or (rest frasin) (rest frase) *MARKERS* nil)
	                (check-marker-or rest-frasin rest-frase *MARKERS* nil))
         (criteria nil (cons reason criteria)))
      ((null disword) 
    ;     (format t " Locution and markers time: ~a~% Actual disambiguation time: ~a~%"
    ;               loc-mark-time act-disamb-time)
         (list (nreverse all-beg-frase) (nreverse criteria)))
      (declare (special disword locut-buff-ahead))
    ;  (setq loc-mark-time (+ loc-mark-time (- (get-internal-run-time) tag-glob-time)))
    ;  (setq tag-glob-time (get-internal-run-time))
      (setq continue t)
      (setq reason nil)
; *** if locut-buffer is not 'fail, we are inside or immediately after a locution
;     locut-buffer can be 'fail or have the form
;     (comp1 comp2 ... compN)
;     All compI are (lemma ...) except the last, which can have this form or the
;     form ((subcompN-1-a subcompN-2-a ...) (subcompN-1-b subcompN-2-b) ...) 
;     To my knowledge, the only case for the second situation is when a locution
;     ending with a preposition is joined to form a prep+art, and the article is
;     "l'", which is ambiguous between feminine and masculine
      ;(format t "Locut buffer: ~a~%Locut-buff-ahead: ~a~%" locut-buffer locut-buff-ahead)
      ;(break "")
      (cond ((neq locut-buffer 'fail)
    ; *** 'wrdchoice' is included in 'beg-frase' (the result) at the beginning
    ;     of the loop
              (let ((nxtlocword (first locut-buffer)))
                  (cond ((or (atom (first nxtlocword))
                             (atom (first (second nxtlocword))))
    ; *** this second disjunct to go ahead regularly in case the locution is
    ;     compound, but the second component is not amobiguous
                           (multiple-value-setq (wrdchoice reason)
                              (disamb-locut locut-buffer rest-frase))
                           (cond ((neq 'yes (prendi (first wrdchoice) 'locut))
                                    (setq locut-buffer nil)))
                           (setq continue nil)) ; the disambiguation is done
                        (t ;(break "locution management")
    ; *** in this branch, the input word corresponds to a locution attached to
    ;     an ambiguous element (as in "in cima all'" - on the top of the -
    ;     where the last word (all') has been split in the end of the locution
    ;     ("a") and the ambiguous article "l'", which could stand for "la"
    ;     feminine or "lo" masculine). The structure of nxtlocword is now:
    ;     ((in_cima_a cat prep ..) 
    ;      ((il cat art gender m ..) (il cat art gender f ..)))
                           (setq disword
                                 (mapcar #'(lambda (x) (list (first nxtlocword) x))
                                         (second nxtlocword)))
                           (setq continue t))) ; the disambiguation has to be done in the
                                               ;  standard way 
    ; *** if at the end of a locution, initialize 'locut-buff-ahead' for the next cycle
                  (cond ((null (rest locut-buffer))
                          (setq locut-buff-ahead (check-locut rest-frase)))
                        (t (setq locut-buff-ahead (rest locut-buffer))))))
; *** the handling of markers is exactly the same as the one for locutions above
            ((neq marker-buffer 'fail)
              (setq wrdchoice (list (car marker-buffer)))
              (setq reason '("text-marker"))
              (setq continue nil)
              (cond ((null (rest marker-buffer))
                      (setq marker-buff-ahead 
	                 (check-marker-or rest-frasin rest-frase *MARKERS* nil)))
	            (t (setq marker-buff-ahead (rest marker-buffer))))))
; *** if both 'locut-buffer' and 'marker-buffer' are 'fail, we are in the 
;     standard situation: apply the rules
; *** 'disword' is a string if the morpholexical analysis failed on that word
      (cond (continue
          (cond
            ((stringp disword)
    ; *** If the morpholexical analysis specified that it was a possible proper
    ;     name, then assume that in fact it is so
              (cond ((string-equal disword "Does not exist; maybe a proper name")
                      (setq wrdchoice '(($SAME CAT NOUN PROPER YES)))
                      (setq reason '("unknown-proper")))
                    (t (setq wrdchoice disword)
                      (setq reason '("morphological failure")))))
;*** 'disword' is not a string: standard analysis; first remove locutions ******************
      	    (t (setq savelocut disword)
               (setq disword (remove-locut disword locut-buffer))
    ; *** since locut-buffer is 'fail, then possible interpretations as part of
    ;     a locution must be removed; and if no other interpretation remains
    ;     the disambiguation fails (error flag in the output)
              (cond ((null disword)
                      (cond ((eq 'yes (prendi (first savelocut) 'proper))
                               (setq wrdchoice '(($SAME CAT NOUN PROPER YES)))
                               (setq reason '("unknown-proper")))
                            (t ;(setq wrdchoice "Exists only as part of a locution") ;; - Matteo, 18giungo2011
							   (setq wrdchoice '(($SAME CAT NOUN PROPER YES))) ;; + Matteo, 18giungo2011
                               (setq reason '("morphological failure")))))
;*** 'disword' has just one interpretation *************************************************
    ; *** if, after removing the locutions, just one possibility remains, then
    ;     the word is non-ambiguous: return the word
     	            ((equal 1 (length disword))
     	               (cond ((and (eq *LANGUAGE* 'english)
                                   (has-ambiguous-components disword))
        ; *** in this case different treatment. Ongoing work (Nov 5 06)
        ;     so the two branches are equal [Case "I'm"]
                               ; (setq disword (distr-components disword))
            ; !!!!!! This is a TRICK !!!!! The standard disambiguation is substituted
            ;        by a simple check on the following items
                                (setq categs (first (lex-dis-prendi disword 'cat 'all)))
                                (setq xwords (first (lex-dis-prendi disword 'word 'all)))
                                (cond ((and (equal '(pron verb) categs)
                                            (memq (second xwords) '(be have)))
                                        (cond ((and
					         (memq 'verb 
                                                      (lex-dis-prendi 
                                                            (first rest-frase) 'cat 'all))
					         (memq 'gerund 
                                                      (lex-dis-prendi 
                                                            (first rest-frase) 'mood 'all)))
                                                (setq wrdchoice 
                                                  (get-from-ambig-compon disword 'type 'aux)))
                                              (t (setq wrdchoice 
                                                  (get-from-ambig-compon disword 'type 'main))))
                                        (setq reason '("patch: see postagger")))
                                      (t (setq wrdchoice disword)
                                         (setq reason '("unambiguous word")))))
                             (t (setq wrdchoice disword)
                                (setq reason '("unambiguous word")))))
;*** 'disword' has more than one interpretation ********************************************
    ; *** otherwise, extract all categories (taking into account compounds)
                    (t (setq categs (mapcar #'buildcatnames
                                            (lex-dis-prendi disword 'cat 'all)))
    ; *** is-a-proper? is true if the only nominal interpretation is Proper.
    ;     	       is nil if there is no nominal interpretation
                      (setq is-a-proper? (check-proper disword nil))
    ; *** remove duplicate categories
                      (setq diff-cat (elimdup categs))
;*** 'disword' has more than one interpretation but just one category **********************
    ; *** if just one category, inside-category disambiguation
                      (cond ((= 1 (length diff-cat))
                              (multiple-value-setq (wrdchoice reason)
                                   (try-dis-same-cat disword (first diff-cat) beg-frase
                                                     rest-frase inpword (first rest-frasin))))
;*** 'disword' has more than one interpretation and more than one category *****************
    ; *** disambiguation among more than one categories
    ; *** chosencat has the form <category, used rule, certainty factor>
                            (t (setq chosencat
                                   (disamb-categ
                                        diff-cat beg-frase disword
                                        rest-frase inpword (first rest-frasin)
                                        is-a-proper?))
    ; *** the next for compound categories (ex. prep+art), where the choice is
    ;     made only on the first of them (es. prep)
                             ; (format t "Tagger: chosencat = ~a~%" chosencat)
                             ; (break "")
                              (setq firstcateg
                                   (expl+cats (explode (first chosencat))
                                   #\+))
    ; *** extract all the interpretations of the chosen category
                              (setq word-that-cat
                                  (extract-interp disword 'cat firstcateg t 'all))
                             ; (format t "Tagger: Selected word = ~a~%" word-that-cat)
                             ; (break "")
                              (setq reason (list (second chosencat)))
    ; *** if more than one remains, ...
                              (cond ((= 1 (length word-that-cat))
                                      (setq wrdchoice word-that-cat))
;*** 'disword' after the category disambiguation still has more than one interpretation ****
                                    (t (multiple-value-setq (wrdchoice tempreason)
                                           (try-dis-same-cat word-that-cat (first chosencat) beg-frase
                                                  rest-frase inpword (first rest-frasin)))
                                       (setq reason (append reason tempreason))))))
    ;  (setq act-disamb-time (+ act-disamb-time (- (get-internal-run-time) tag-glob-time)))
    ;  (setq tag-glob-time (get-internal-run-time))
)))))))))

; **************************************************************************
(defun disamb-locut (locut-buffer fine-frase)
  (declare (special fine-frase))
  (let ((locution-length (length locut-buffer)))
  (declare (special disword locution-length *SYSTEM-CONTEXT*))
    (cond ((neq *SYSTEM-CONTEXT* 'atlas)
            (values (list (first locut-buffer)) '("locution")))
          (t (apply-within-cat-rules 
                       'atlas-locut disword nil 'locution-disambiguation locut-buffer)))))

; **************************************************************************
; *** applies various strategies for inside-category disambiguations
; *** returns two values:
;     - the chosen interpretation
;     - the reasons for the choice
(defun try-dis-same-cat (disword category beg-frase rest-frase inpword nextword)
  (let (singchoice wrdchoice xwords tempreason)
      (cond ((all-proper disword)
              (setq wrdchoice (list (get-proper-not-default disword)))
              (cond ((null wrdchoice)
                       (setq wrdchoice (extract-interp disword 'multiple nil t))))
              (values wrdchoice '((proper-not-multiple))))
       ; *** the result of 'dis-same-cat' has the form:
       ;     (chosen interpretation, list of applied rules)
       ;   <feature usata per disambiguare, valore feature scelto, regola usata, CF>
            (t (setq singchoice 
                   (dis-same-cat category beg-frase disword rest-frase inpword nextword))
              (setq wrdchoice (first singchoice))
              (cond ((= (length wrdchoice) 1)
                       (values wrdchoice (list (second singchoice))))
    ; *** the intra-category rules have left more than one interpretation
                    ((all-proper wrdchoice)
                       (setq wrdchoice (list (get-proper-not-default wrdchoice)))
                       (cond ((null wrdchoice)
                                (setq wrdchoice (extract-interp wrdchoice 'multiple nil t))))
                       (values wrdchoice '((proper-not-multiple))))
    ; *** the last possibility is that there is a word-based lexical preference
		    (t (setq tempreason (second singchoice))
                       (setq xwords (elimdup (lex-dis-prendi wrdchoice 'word 'all)))
		       (cond ((not (= (length xwords) 1))
                                (setq singchoice (apply-lex-pref xwords))
                                (cond ((null singchoice)
        ; *** no lexical preference: random choice
                                         (values (list (first wrdchoice)) 
                                                 (list (append1 tempreason 'random))))
                                      (t (setq wrdchoice 
                                              (extract-interp wrdchoice 'word singchoice 'all))
                                         (values wrdchoice 
                                                 (list (append1 tempreason 'lexical-preference))))))
        ; *** if they are the same lemma, random choice
                             (t (values (list (first wrdchoice)) 
                                              (list (append1 tempreason 'random)))))))))))

; **************************************************************************
; *** this looks in the list of pairs *LEXICAL-PREFERENCES*
;     if any item in xwords occur there as first element, it is returned as result,
;     otherwise, the result is nil
(defun apply-lex-pref (xwords)
  (declare (special *LEXICAL-PREFERENCES*))
  (let (found)
     (do ((word (first xwords) (first remwords))
          (remwords (rest xwords) (rest remwords)))
         ((or (null word) found)
           (cond (found (first found))
                 (t nil)))
         (setq found (assoc word *LEXICAL-PREFERENCES*)))))

; **************************************************************************
; *** among a set of proper noun interpretations, chooses the one which is not
;     a default
(defun get-proper-not-default (wword)
   (cond ((null wword) nil)
         ((eq (prendi (without-lemma (first wword)) 'default) 'yes)
             (get-proper-not-default (rest wword)))
         (t (first wword))))
   
; **************************************************************************
; *** among all interpretations of a word, it puts in front of the list the
;     one chosen by the tagger. After it, it puts all the remaining ones
;     (unordered) except the locutionary (part of multi-words) ones.
;     If the input is a string (not found word) it is put in a list
(defun compose-interps (choice all-interp)
   (cond ((or (stringp choice) (stringp all-interp)) (list choice))
	 (t (cons choice (other-interp (first choice) all-interp)))))

; **************************************************************************
; *** the next takes all interpretations of a word, except the one chosen
;     by the tagger and the locutionary ones
(defun other-interp (choice all-interp)
   (cond ((null all-interp) nil)
; *** if the next interp is the one chose, it is not in the result
	 ((equal choice (first all-interp))
	    (other-interp choice (rest all-interp)))
; *** the next is compound, so it cannot be a locution and is put in the result
	 ((not (atom (first (first all-interp))))
	    (cons (list (first all-interp))
		  (other-interp choice (rest all-interp))))
; *** the next is part of a locution: not in the result
	 ((eq (prendi (without-lemma (first all-interp)) 'locut) 'yes)
	    (other-interp choice (rest all-interp)))
; *** otherwise, put it in the result
	 (t (cons (list (first all-interp))
		  (other-interp choice (rest all-interp))))))
 
; **************************************************************************
; *** it checks if the only nominal interpretation of a word (if any) is as a
;     a proper name
(defun check-proper (wrd trov)
  (cond ((null wrd) trov)
; *** a "noun" interpretation has been found; f it is a proper name, the flag
;     "trov" is set to true; the search continues to look for common nouns,
;     that should anyway produce a nil result
	((eq (sing-lex-dis-prendi (first wrd) 'cat 'first) 'noun)
	   (cond ((eq (sing-lex-dis-prendi (first wrd) 'proper 'first) 'yes)
		    (check-proper (rest wrd) t))
		 (t nil)))
	(t (check-proper (rest wrd) trov))))
   
;************************************************************************
; *** applies the rules for disambiguating among different categories
(defun disamb-categ (diff-cat rev-iniz-frase disword fine-frase inpword
		    nextword proper?)
; *** INPUT:
;  >>> diff-cat: the categories
;  >>> rev-iniz-frase: the syntactic infos of the preceding words (already
;		disambiguated). The list is reversed, so that "first
;		rev-iniz-frase" is the word which precedes immediately the
;		one which is being disambiguated 
;  >>> disword: the syntactic infos on the word to be disambiguated
;  >>> fine-frase: the syntactic infos on the words that follows the one that is
;		being disambiguated; these are still ambiguous, since the
; 		POS tagger proceeds from left to right
;  >>> inpword: the surface (input) form of the word to be disambiguated
;  >>> nextword: the surface (input) form of the following word
;  >>> proper?: true if the only nominal interpretation is as a proper name
  (declare (special rev-iniz-frase disword fine-frase inpword nextword))
; *** The above variables used in the rules
  (let* ((sortinput (copy diff-cat))
; *** the previous line is needed because sort destroys the input data
         (ambiguity (concatl (put-separator '- (sort sortinput #'alphalessp))))
         (rules (get ambiguity 'lexdisrules))
         (default (get ambiguity 'defaultc))
         choices no-proper not-amb)
; *** if:
;       1. There are no tagging rules
;       2. There is no default
;       3. There is just one nominal interpretation, and it is for proper noun
;     then
;       it is assumed that the nominal interpretation is just a default and is
;       removed (This happens for all sentence-initial words, for set of tags
;       which are not covered by the rules; for instance, in "Che tu venga ..."
;       produces a set of tags which is the one of the word "che", plus the
;       default nominal; of course, the set including 'noun' does not exist)
     (cond ((and (null rules) (null default) proper?)
             (setq no-proper (remove 'noun diff-cat))
             (cond ((> (length no-proper) 1)
                     (setq sortinput (copy no-proper))
                     (setq ambiguity 
                         (concatl
                             (put-separator '- (sort sortinput #'alphalessp))))
                     (setq rules (get ambiguity 'lexdisrules))
                     (setq default (get ambiguity 'defaultc)))
                   (t (setq not-amb t)))))
; *** If just one interpretation is left, then the word is considered as
;     non-ambiguous (not-amb=true)
     (cond (not-amb (list (first no-proper) 'not-proper 'A))
           (t 
; *** main loop of rule application;
;     in 'choices' the rules that succeeded; 'choices' is a list of triples
;     <chosen tag, tule name, CF>
;     The possible CFs are C (Certain), A (Almost certain), U (Uncertain),
;     to which, here, Z (Zero-certain) is added for completely random choices
;     (in absence of a default)
            (dolist (nextrule rules)
               (setq choices (append3 choices (eval-tag-rule nextrule))))
; *** If no rule succeeded, then 'choices' is empty
            (cond ((null choices)
                    (cond ((null default)
    ; *** If no default is defined, then take the first category (random choice)
    ;     and specify with Z the certainty factor
                            (list (first diff-cat) "Random choice" 'Z))
    ; *** Otherwise, use the default
                          (t (list default (concat 'default- ambiguity) 'U))))
; *** If at least one rule succeeded, pick the best choice, according to the
;     certainty factor
	       (t (choosebest (first choices) (rest choices))))))))
     
;********************************************************************
; *** this evaluates a single tagging rule
(defun eval-tag-rule (tagrule &optional feature)
  (declare (special *LANGUAGE*))
  (let ((antec (lexdisr-if (eval tagrule)))
        (chosenval (lexdisr-then (eval tagrule)))
        (CF (lexdisr-CF (eval tagrule)))
        (language (lexdisr-lang (eval tagrule))))
      (cond ((and (or (null language)
                      (memb-or-eq *LANGUAGE* language))
                  (eval antec) )
               (cond ((null feature) (list chosenval tagrule CF))
                     (t  (list feature chosenval tagrule CF))))
            (t nil))))

;********************************************************************
; *** chooses the best rule on the basis of the certainty factor
(defun choosebest (bestchoice otherchoices)
  (cond ((null otherchoices) bestchoice)
        ((CF-better bestchoice (first otherchoices))
          (choosebest bestchoice (rest otherchoices)))
        (t (choosebest (first otherchoices) (rest otherchoices)))))

;********************************************************************
; *** compare the CFs; in case of equal values, return true
;     numerical values come from rules learned automatically
(defun CF-better (choice1 choice2)
  (let ((CF1 (first (last choice1)))
        (CF2 (first (last choice2))))
       (cond ((and (numberp CF1)(numberp CF2))
               (> CF1 CF2))
       	     (t (or (eq CF1 'C)
                    (and (eq CF1 'A) (neq CF2 'C))
                    (and (eq CF1 'U) (eq CF2 'U)))))))

;********************************************************************
; *** builds complex atom names, using the members of a list and "separator":
;     (build-separator '- '(alfa beta gamma)) ---> alfa-beta-gamma
; *** Used here in disamb-categs, and in ALLLANG/PROC-ALL/SUBCAT-PROC-ALL: gr-interp
(defun put-separator (separator categs)
   (cond ((null (rest categs)) categs)
	 (t (cons (first categs) 
		  (cons separator (put-separator separator (rest categs)))))))

;********************************************************************
; *** application of within-category disambiguation rules
; *** INPUT:
;   >>> categ: the (common) category
;   >>> rev-iniz-frase: the previous (already disambiguated) words; the list
;       is inverted, so that 'first rev-iniz-frase' is the words that precedes
;       the one to disambiguate now
;   >>> disword: all interpretations of the word to disambiguate
;   >>> fine-frase: the syntactic infos on the following words (not yet
;       disambiguated)
;   >>> inpword: the surface form of the word to disambiguate
;   >>> nextword: the surface form of the word that follows
(defun dis-same-cat (categ rev-iniz-frase disword fine-frase inpword nextword)
  (declare (special disword rev-iniz-frase fine-frase inpword nextword))
  (let (appl-rules values rrules (tempresult disword) propnotdef)
     (cond ((member categ '(VERB VERB+PRON VERB+ADV))              ; verb+adv: cannot
; ***  rules for the verbs: 
;      verb-aux-main, verb-main-mod, verb-fin-inf, general-pl-sing,
;      verb-1-2-3 (for person)
   ; *** first, try with the rules on type (main, aux, mod)
	  (setq values (elimdup (lex-dis-prendi tempresult 'type 'first)))
      (cond ((> (length values) 1)
              (cond ((and (memq 'AUX values) (intersection values '(MAIN NIL)))
                       (multiple-value-setq (tempresult appl-rules)
                           (apply-within-cat-rules 
                                 'VERB-AUX-MAIN tempresult appl-rules 'type)))
                    ((and (memq 'MOD values) (intersection values '(MAIN NIL)))
                       (multiple-value-setq (tempresult appl-rules)
                           (apply-within-cat-rules 
                                 'VERB-MAIN-MOD tempresult appl-rules 'type)))
                    (t (exception 'morpho-error 
                               "PROC/postagger: Unknown verbal type ambiguity" values)))))
      (cond ((> (length tempresult) 1)
   ; *** if "tempresult" includes more entries, then there is another ambiguity,
   ;     within the type; try with finite-infinite rules
              (setq values (elimdup (lex-dis-prendi tempresult 'mood 'first)))
              (cond ((and (intersection values '(IND CONG CONDIZ IMPER))
                          (intersection values '(PARTICIPLE GERUND INFINITE)))
                       (multiple-value-setq (tempresult appl-rules)
                           (apply-within-cat-rules 
                               'VERB-FIN-INF tempresult appl-rules 'mood))))))
      (cond ((> (length tempresult) 1)
   ; *** if "tempresult" includes more entries, there is another ambiguity,
   ;     try with cong-imper, imper-ind or cong-imper-ind rules;
   ;     or with verb-gerund-participle rules for English
              (setq values (elimdup (lex-dis-prendi tempresult 'mood 'first)))
              (cond ((member 'IMPER values)
                      (cond ((member 'CONG values)
                              (cond ((member 'IND values)
                                      (setq rrules 'VERB-CONG-IMPER-IND))
                                    (t (setq rrules 'VERB-CONG-IMPER))))
                            ((member 'IND values)
                              (setq rrules 'VERB-IMPER-IND))
                            (t (setq rrules nil))))
                    ((and (member 'GERUND values)
                          (member 'PARTICIPLE values)) ; in English gerund=participle
                      (setq rrules 'VERB-GERUND-PARTICIPLE))
                    ((and (member 'INFINITE values)
                          (member 'PARTICIPLE values)) ; for English "found"
                      (setq rrules 'VERB-INFINITE-PARTICIPLE))
                    ((and (member 'CONG values)
                          (member 'IND values)) ; English modals: no imperative
                      (setq rrules 'VERB-CONG-IND))
                    (t (setq rrules nil)))
              (cond ((not (null rrules))
                       (multiple-value-setq (tempresult appl-rules)
                            (apply-within-cat-rules 
                                 rrules tempresult appl-rules 'mood))))))
      (cond ((> (length tempresult) 1)
   ; *** if "tempresult" includes more entries, then there is still ambiguity,
   ;     try with number rules
              (setq values (elimdup (lex-dis-prendi tempresult 'number 'first)))
              (cond ((and (member 'SING values)
                          (member 'PL values))
                       (multiple-value-setq (tempresult appl-rules)
                          (apply-within-cat-rules 
                               'GENERAL-PL-SING tempresult appl-rules 'number))))))
      (cond ((> (length tempresult) 1)
   ; *** if "tempresult" includes more entries, then there is another ambiguity,
   ;     try with person rules
              (setq disword tempresult)
              (setq values (elimdup (lex-dis-prendi tempresult 'person 'first)))
              (cond ((= (length values) 3)
                       (multiple-value-setq (tempresult appl-rules)
                           (apply-within-cat-rules 
                                 'VERB-1-2-3 tempresult appl-rules 'person)))
                    ((and (member 1 values) (member 2 values))
                       (multiple-value-setq (tempresult appl-rules)
                           (apply-within-cat-rules 
                                 'VERB-1-2 tempresult appl-rules 'person)))
                    ((and (member 1 values) (member 3 values))
                       (multiple-value-setq (tempresult appl-rules)
                           (apply-within-cat-rules 
                                 'VERB-1-3 tempresult appl-rules 'person)))
                    ((and (member 2 values) (member 3 values))
       ; *** for imperatives
                       (multiple-value-setq (tempresult appl-rules)
                           (apply-within-cat-rules 
                                 'VERB-2-3 tempresult appl-rules 'person)))
                    ((member 'allval values)
                       tempresult)
                    ((not (= (length values) 1))
                       (format t " Word: ~s ~% Values: ~s ~%" tempresult values)
                       (exception 'morpho-error 
                                 "PROC/postagger: Unknown person ambiguity in dis-same-cat" 
                                 values))))))
; ***  rules for the nouns: 
;      noun-common-proper, general-pl-sing, general-f-m
    ((eq categ 'NOUN)
      (setq values (lex-dis-prendi tempresult 'proper 'first))
      (cond ((and (memq 'yes values) (memq nil values))
               (multiple-value-setq (tempresult appl-rules)
                     (apply-within-cat-rules 
                           'noun-common-proper tempresult appl-rules 'proper))))
      (cond ((> (length tempresult) 1)
   ; *** if "tempresult" includes more entries, then there is still ambiguity
              (cond ((all-proper tempresult)
     ; *** if all items are proper, choose the one not default
                        (setq propnotdef (list (get-proper-not-default tempresult)))
                        (cond ((null propnotdef)
                                 (setq tempresult
                                       (extract-interp tempresult 'multiple nil t)))
                              (t (setq tempresult propnotdef)))))))
      (cond ((> (length tempresult) 1)
   ; *** if "tempresult" includes more entries, then there is still ambiguity,
   ;     try with number rules
              (setq values (lex-dis-prendi tempresult 'number 'first))
              (cond ((and (member 'sing values)
                          (member 'pl values))
                       (multiple-value-setq (tempresult appl-rules)
                          (apply-within-cat-rules 
                               'general-pl-sing tempresult appl-rules 'number))))))
      (cond ((> (length tempresult) 1)
   ; *** if "tempresult" includes more entries, then there is still ambiguity,
   ;     try with gender rules
              (setq values (lex-dis-prendi tempresult 'gender 'first))
              (cond ((and (member 'f values)
                          (member 'm values))
                       (multiple-value-setq (tempresult appl-rules)
                          (apply-within-cat-rules 
                                'general-f-m tempresult appl-rules 'gender)))))))
; *** for articles, the only possibility is with elisions (l', dell'), so the
;     art-f-m rules are used
    ((or (eq categ 'art) (eq categ 'prep+art))
      (setq values (lex-dis-prendi tempresult 'gender 'last))
      (cond ((and (member 'f values)
                  (member 'm values))
               (multiple-value-setq (tempresult appl-rules)
                    (apply-within-cat-rules 
                          'art-f-m tempresult appl-rules 'gender)))))
; *** adjectives: qualif or indef (certe, certi)
;                 poss or qualif (proprio, propria)
;                 exclam or interr (catalan: quins)
;                 masc or fem (quest', quell')
;                 sing o pl (fine)
     ; *** first packet
    ((eq categ 'ADJ)
      (setq values (lex-dis-prendi tempresult 'type 'first))
      (cond ((and (memq 'indef values)
                  (memq 'qualif values))
               (multiple-value-setq (tempresult appl-rules)
                    (apply-within-cat-rules 
                          'adj-indef-qualif tempresult appl-rules 'type))))
     ; *** second packet
      (cond ((> (length tempresult) 1)
              (cond ((and (memq 'exclam values)
                          (memq 'interr values))
                       (multiple-value-setq (tempresult appl-rules)
                            (apply-within-cat-rules 
                                  'adj-exclam-interr tempresult appl-rules 'type))))))
     ; *** third packet
      (cond ((> (length tempresult) 1)
             (cond ((and (memq 'poss values)
                         (memq 'qualif values))
              (multiple-value-setq (tempresult appl-rules)
                  (apply-within-cat-rules 
                        'adj-poss-qualif tempresult appl-rules 'type))))))
     ; *** fourth packet, but applied to gender
      (cond ((> (length tempresult) 1)
             (setq values (lex-dis-prendi tempresult 'gender 'first))
             (cond ((and (memq 'f values)
                         (memq 'm values))
              (multiple-value-setq (tempresult appl-rules)
                  (apply-within-cat-rules 
                        'general-f-m tempresult appl-rules 'gender))))))
     ; *** fifth packet, applied to number
      (cond ((> (length tempresult) 1)
             (setq values (lex-dis-prendi tempresult 'number 'first))
             (cond ((and (memq 'sing values)
                         (memq 'pl values))
              (multiple-value-setq (tempresult appl-rules)
                  (apply-within-cat-rules 
                        'general-pl-sing tempresult appl-rules 'number)))))))
; *** adverbs: three packets:
;     1. comparatives or temporal (piu')
;     2. locative or comparatives (oltre)
;     3. quantif or temporal (appena)
       ((eq categ 'ADV)
	  (setq values (elimdup (lex-dis-prendi tempresult 'type 'first)))
	  (cond ((or (equal values '(time compar))
		     (equal values '(compar time)))
                   (multiple-value-setq (tempresult appl-rules)
                    (apply-within-cat-rules 
                          'adv-compar-time tempresult appl-rules 'type)))
	        ((or (equal values '(loc compar))
		     (equal values '(compar loc)))
                   (multiple-value-setq (tempresult appl-rules)
                    (apply-within-cat-rules 
                          'adv-loc-compar tempresult appl-rules 'type)))
	        ((or (equal values '(manner time))
		     (equal values '(time manner)))
                   (multiple-value-setq (tempresult appl-rules)
                    (apply-within-cat-rules 
                          'adv-manner-time tempresult appl-rules 'type)))
	        ((or (equal values '(quant time))
		     (equal values '(time quant)))
                   (multiple-value-setq (tempresult appl-rules)
                    (apply-within-cat-rules 
                          'adv-quant-time tempresult appl-rules 'type)))))
; *** conjunctions: two packets:
;     coord or subord (mentre)
;     compar or subord (che) [there is just the default]
       ((eq categ 'CONJ)
	  (setq values (lex-dis-prendi tempresult 'type 'first))
	  (cond ((and (memq 'compar values)	; *** "che"
                      (memq 'subord values)
                      (memq 'coord values))
                  (multiple-value-setq (tempresult appl-rules)
                        (apply-within-cat-rules 
                            'conj-compar-coord-subord tempresult appl-rules 'type)))
	        ((and (memq 'coord values)
                      (memq 'subord values))
                  (multiple-value-setq (tempresult appl-rules)
                        (apply-within-cat-rules 
                            'conj-coord-subord tempresult appl-rules 'type)))))
; *** pronouns: five packets:
;     pron-exclam-interr-relat (the word CHI),
;     pron-interr-relat (the word QUALE, QUANTO),
;     pron-demons-relat (the english word THAT)
;     pron-indef-relat (the word CHIUNQUE)
;     pron-pers-loc (the words CE, CI, VE, VI)
;     pron-clitic (the words ME, TE)
    ((eq categ 'PRON)
      (setq values (lex-dis-prendi tempresult 'type 'first))
      (cond ((and (memq 'interr values)
                  (memq 'relat values))
               (cond ((memq 'exclam values)
     ; *** first packet
                        (multiple-value-setq (tempresult appl-rules)
                          (apply-within-cat-rules 
                            'pron-exclam-interr-relat tempresult appl-rules 'type)))
     ; *** second packet
                     (t (multiple-value-setq (tempresult appl-rules)
                          (apply-within-cat-rules 
                            'pron-interr-relat tempresult appl-rules 'type)))))
     ; *** third packet
            ((and (memq 'demons values)
                  (memq 'relat values))
              (multiple-value-setq (tempresult appl-rules)
                  (apply-within-cat-rules 
                        'pron-demons-relat tempresult appl-rules 'type)))
     ; *** fourth packet
            ((and (memq 'indef values)
                  (memq 'relat values))
              (multiple-value-setq (tempresult appl-rules)
                  (apply-within-cat-rules 
                        'pron-indef-relat tempresult appl-rules 'type)))
     ; *** fifth packet
            ((and (memq 'pers values)
                  (memq 'loc values))
              (multiple-value-setq (tempresult appl-rules)
                   (apply-within-cat-rules 
                        'pron-pers-loc tempresult appl-rules 'type))))
     ; *** sixth packet, but applied to form (clitic or NIL)
     (cond ((> (length tempresult) 1)
             (setq values (lex-dis-prendi tempresult 'form 'first))
             (cond ((and (memq 'clitic values)
                         (memq NIL values))
              (multiple-value-setq (tempresult appl-rules)
                  (apply-within-cat-rules 
                        'pron-clitic tempresult appl-rules 'form))))))))
; *** if, now, an ambiguity remains, the only possibility is to choose at random
  ;(cond ((> (length tempresult) 1)
  ;        (list (list (first tempresult)) (cons 'random appl-rules)))
  ;      (t (list tempresult appl-rules)))
       (list tempresult appl-rules)
    ))

;********************************************************************
; *** this actually applies the intra-category disambiguation rules
; *** INPUT:
;  >>> packet-name: the name of the rule packet
;  >>> amb-word: the word to disambiguate
;  >>> ok-rules: the rules that have been applied before for disambiguation
;      (but which did not lead to a single choice)
;  >>> feature: the feature which is the basis for the disambiguation
; *** GLOBALS:
;  >>> rev-iniz-frase: the previous (already disambiguated) words; the list
;      is inverted, so that 'first rev-iniz-frase' is the words that precedes
;      the one to disambiguate now
;  >>> fine-frase: the syntactic infos on the following words (not yet
;       disambiguated)
; *** OUTPUT:
;  >>> A two-values result:
;      - the result of the disambiguation (the list of remaining interpretations) 
;      - the updated list of applied rules (if one of this packet was applied)
(defun apply-within-cat-rules (packet-name amb-word ok-rules feature &optional locut-buffer)
  (declare (special locut-buffer))
  (let ((rules (get packet-name 'lexdisrules))
        (default (get packet-name 'defaultc))
        choices chosenrule)
     (dolist (nextrule rules)
        (setq choices (append3 choices (eval-tag-rule nextrule feature))))
  ; *** if choices is empty, no rule applied: use the default
     (cond ((null choices)
	     (setq chosenrule (list feature default 'default 'U)))
  ; *** otherwise, choose the best; in principle the rules are already sorted
  ;     in decreasing CF order, but 'choosebest' compares the CF's in order to
  ;     avoid manual errors. Anyway, among rules with even CF, the manual order
  ;     does matter
	   (t (setq chosenrule (choosebest (first choices) (rest choices)))))
  ; *** now, apply the rule to extract the interpretation(s)
     (values (extract-interp amb-word (first chosenrule) (second chosenrule) t)
             (append1 ok-rules 
                 (cond ((neq (third chosenrule) 'default)
                          (third chosenrule))
                       (t (concat 'default- packet-name)))))))
                          
;********************************************************************
; *** BASE FUNCTIONS FOR EXTRACTING INTERPRETATIONS
;********************************************************************
; *** selects the words that have a given value for a given property
;     usually, the consequent of the rule is the chosen value of the feature
;     but there are five exceptions:
;     TYPE: if the value is 'main', it does not appear in the word, since
;           it is the default for verbs
;     MOOD: FIN: ind, cong, condiz, imper; INF: participle gerund infinite
;     PROPER: common, proper
;     PERSON: p1, p2, p3
;     MULTIPLE: to extract non-multiple interpretations
; *** Arguments:
;  ### wword: the input word (possibly ambiguous); in general, this need not be 
;             the word being disambiguated (which is in disword), but the word
;             to which the condition applies
;  ### feature: the feature on which the selection is based (e.g. "proper")
;  ### value: the value that feature must have (e.g. "noun")
;  ### break?: true if the function must signal that no interpretation
;      satisfying the conditions has been found
;  ### pos: it may be:
;        first: in case of compound words, pick the first component 
;        last: in case of compound words, pick the last component 
;        all: this is used when feature is "cat" to extract all interpretations
;             of that category. This seems dishomogeneous wrt. the previous values
; *** OUTPUT:
;  ### a list of interpretations (same structure of a word) satisfying the
;      conditions
; *** it is used to extract locutions in check-locut: 
;            (extract-interp (first fras) 'locut 'yes nil)
;     In ok-condiz? (where "first condiz" is the feature, and "second condiz" is
;                    the value, but see the comments in ok-condiz?
;            (extract-interp word (first condiz) (second condiz) nil pos)
;     In prep-govern-next
;            (extract-interp disword 'cat 'prep t)
;     In lexdisamb: in the standard case, to extract all interpretations of a given cat
;            (extract-interp disword 'cat firstcateg t 'all)
;                   but also to extract something in case he word is "all-proper" (?)
;            (extract-interp disword 'multiple nil t))
;     In apply-within-cat-rules:
;            (extract-interp amb-word (first chosenrule) (second chosenrule) t)
; *** There are some special cases. One of this concerns the feature
;              'locution-disambiguation, where it is assumed that:
;              1. The call was made from inside 'apply-within-cat-rules'
;              2. The special variable 'locut-buffer' contains the various
;                 parts of the locution
(defun extract-interp (wword feature value break? &optional pos)
 (declare (special locut-buffer))
  ; *** wword is a string if it was not found in the lexicon
 (cond ((stringp wword) nil)
       ((and (null wword) 
             (or (null value)
                 (member nil (inlist value))))
           t)
       ((and (eq feature 'locution-disambiguation)
             (eq value 'locution-yes))
          (list (first locut-buffer)))
       (t (let ((found (int-extr-interp wword feature value pos)))
  ; *** if no interpretation is found, the search is not for a locution, and a
  ;     break is required in this situation, then break
    	    (cond (found found)
                  ((neq feature 'locut)
              	    (cond (break? 
                       	    ;(format T "PROC/TAGGER/postagger: extract-interp 1 ~a ~a~%~%" 
                            ;       	    wword value)
	               	    ;(break "PROC/TAGGER/postagger: extract-interp 2")
	               	    (setq found (list (first wword)))) 
                                           ; *** this just to enable "continue"
  ; *** otherwise, simply return nil
                    	  (t nil)))
                  (t nil))))))

;********************************************************************
; *** this loops on all interpretations of a word
(defun int-extr-interp (wword feature value pos)
 (let (res)
    (dolist (nwrd wword res)
      (cond ((satisfy-tag-cond nwrd feature value pos)
              (setq res (cons nwrd res)))))))

;********************************************************************
; *** checks if a word interpretation satisfies a tagger condition
;   INPUT:
;  ## winterp: a word interpretation, which may be simple (asino cat noun ...)
;              or compound ((in cat prep ...) (il cat art ...))
;              Note that this is the word on which the check is made (perhaps
;              the next or next2 word); the word being disambiguated, if needed,
;              is in disword
;  ## feature: the feature on which the check must be made
;  ## value: the value that feature must have
;  ## pos: first: the extraction has to be made on the basis of the first
;                  (or unique) component
;          last: the extraction has to be made on the basis of the last
;                  (or unique) component
;          all: the extraction has to be made on the basis of all components
;                 so that '(verb)' does not match a verb+pron
;          nil: any component is ok for the match
(defun satisfy-tag-cond (winterp feature value pos)
  (cond ((eq pos 'first)
          (satisfy-tag-on-comp (prendi-first-comp winterp) feature value))
  ; *** check on the first component
        ((eq pos 'last)
          (satisfy-tag-on-comp (prendi-last-comp winterp) feature value))
  ; *** check on the last component
        ((eq pos 'all)
  ; *** the 'all case seems reserved for compound categories, i.e. tagging
  ;     rules referring to NOUN+ART or VERB+CLIT. Here, I assume that in 
  ;     this case the 'value' is a list of the form (noun art) or (verb clit).
  ;     The conversion of the name in the list has been made in "lexdisamb"
          (cond ((eq feature 'cat)
                  (cond ((not (is-compound winterp))
                          (setq winterp (list winterp))))
                  (equal (mapcar #'(lambda (x) (prendi (without-lemma x) 'cat)) winterp)
                         value))
                (t (exception 'morpho-error 
                       "PROC/postagger: Attempt to take an 'all' value in case the feature is not CAT" 
                       feature))))
  ; *** pos=nil means that if the value is ok for any component, the test is true
        ((null pos)
          (cond ((atom (first winterp))
                  (satisfy-tag-on-comp winterp feature value))
                (t (check-tag-all-compon winterp feature value))))))

(defun is-compound (winterp)
  (and (listp winterp)
       (not (null winterp))
       (not (atom (first winterp)))))

(defun prendi-first-comp (winterp)
  (cond ((is-compound winterp) (first winterp))
        (t winterp)))

(defun prendi-last-comp (winterp)
  (cond ((is-compound winterp)
  ; *** this is more complex, to account for "prender-vi", where
  ;     the second component (vi) is in its turn ambiguous between
  ;     the personal and locative readings; the choice of one of
  ;     them is random here (ult)
           (let ((lastc (ult winterp)))
               (cond ((is-compound lastc) (ult lastc))
                     (t lastc))))
        (t winterp)))

(defun prendi-butlast-comp (winterp)
  (cond ((is-compound winterp)
  ; *** this is more complex, to account for "prender-vi", where
  ;     the second component (vi) is in its turn ambiguous between
  ;     the personal and locative readings; the choice of one of
  ;     them is random here (ult)
           (let ((lastc (second (reverse winterp))))
               (cond ((is-compound lastc) (ult lastc))
                     (t lastc))))
        (t nil)))

;********************************************************************
; *** This is called (see above) when 'pos' is 'nil'. 
(defun check-tag-all-compon (winterp feature value)
  (cond ((null winterp) nil)
        ((satisfy-tag-on-comp (first winterp) feature value) t)
        (t (check-tag-all-compon (rest winterp) feature value))))

;********************************************************************
;   INPUT:
;  ## winterp is a single word component: (word feat1 val1 feat2 val2 ...)
;  ## feature is one of the feati, with the following possible exceptions:
;     TYPE: if the value is 'main', it does not appear in the word, since
;           it is the default for verbs
;     MOOD: FIN: ind, cong, condiz, imper; INF: participle gerund infinite
;     PROPER: common, proper
;     PERSON: p1, p2, p3
;     MULTIPLE: to extract non-multiple interpretations
;  ## value: 'art (cat)
(defun satisfy-tag-on-comp (inp-wcompon feature value)
  (declare (special *GRAMMTYPES* *LANGUAGE* disword))
  ; *** the next conditional is for ambiguous components (currently, the only
  ;     case is the italian clitic CI (or VI)
  (let ((wcompon (cond ((atom (first inp-wcompon)) (without-lemma inp-wcompon))
                       (t (without-lemma (first inp-wcompon))))))
; *** first exception: TYPE MAIN
   (cond ((and (eq feature 'TYPE) (eq value 'main))
           (memq (prendi wcompon 'TYPE) '(main nil)))
; *** second exception: MOOD
         ((eq feature 'MOOD)
             (cond ((eq value 'fin)
                     (memq (prendi wcompon 'mood) '(ind cong condiz imper)))
	           ((eq value 'inf) 
                     (memq (prendi wcompon 'mood) '(participle gerund infinite)))
    ; *** the MOOD may also be a real value, as CONG in the default for
    ;     verb-cong-imper
                   (t (memq (prendi wcompon 'mood) (inlist value)))))
; *** third exception: PROPER
          ((eq feature 'PROPER)
             (cond ((memq value '(proper yes))
 ;(cond ((eq (prendi wcompon 'cat) 'noun)
 ;         (format t "proper: value: ~a~%   inp-wcompon: ~a~%"  value inp-wcompon)
 ;         (break "")))
                     (eq (prendi wcompon 'proper) 'yes))
                   (t (neq (prendi wcompon 'proper) 'yes))))
; *** fourth exception: WORD-TYP
          ((eq feature 'word-typ)
             (member (sld-prendi inp-wcompon 'word)
                     (get value 'grtype)
                     :test #'eq-word))
		; *** sld-prendi just to treat 'word' as a feature
; *** fifth exception: PERSON
          ((eq feature 'person)
             (cond ((and (eq *LANGUAGE* 'english)
                         (eq (prendi wcompon 'cat) 'adv)) t)	; this for can+not
                   (t (let ((wpers (prendi wcompon 'person)))
                          (cond ((not (numberp wpers)) (eq wpers 'allval))
                                ((eq value 'p1) (= 1 wpers))
                                ((eq value 'p2) (= 2 wpers))
                                ((eq value 'p3) (= 3 wpers)))))))
; *** sixth exception: FORM
          ((eq feature 'form)
      ; *** used to extract a given pronoun after the application of the
      ;     within-pronoun rules
             (cond ((eq value 'clitic)
                     (eq 'clitic (prendi wcompon 'form)))
                   ((eq value 'notclit)
                     (neq 'clitic (prendi wcompon 'form)))))
; *** seventh exception: MULTIPLE
   ; *** the feature name 'multiple' is used (with value NIL) to extract
   ;     non-multiple elements (for the use of multiple elements, see
   ;     PROC/MORPHO/analizzatore:split-elements)
          ((eq feature 'multiple)
             (cond ((null value)
                     (null (prendi wcompon 'multiple)))
		   (t (exception 'morpho-error 
                               "PROC/postagger: Use of the MULTIPLE feature with non null value"
                               value))))
; *** standard case
;     Here, value may be a complex element having one of the following possible 
;     forms
;     -- an atom (a single value to check)
;     -- a list of atoms (an 'or' of single values)
;     -- a list including sublists (x1 x2 subl2 x3 subl3 x4 x5)
;        each sublist subli is a set of further conditions on xi. This form is used
;        just in case the feture is "cat", so that xi is the name of a category, and
;        subli is a set of conditions on other features
;	 These subli may refer to:
;          -- mood (e.g. participle, infinite)
;          -- proper (yes or nil)
;          -- semtype (advers, for a conj)
;          -- type (in this case the subvalue may in turn be atomic - e.g. interr, qualif -
;                   or a sub-sublist - e.g. (qualif interr))
;          -- word-typ (e.g. &base-relat, values in *GRAMMTYPES*)
;          -- not (the argument of not is one of these subconditions, e.g. (type aux))
;          -- or (the argument of or are two or more of these subconditions, e.g. 
;                 (type aux) (type mod))
;          -- and (the argument of or are two or more of these subconditions, e.g. 
;                 (gender m) (number pl))
          (t (check-tag-condit feature value inp-wcompon)))))

;********************************************************************
(defun check-tag-condit (feature value component)
  (let ((thisval (sld-prendi component feature)))
  (cond ((null value) nil)
  ; *** simple condition: e.g. 'art
        ((atom value)
          (cond ((atom thisval)		; it is not an atom for "case"
                   (or (eq value thisval)
                       (eq thisval 'allval)))
                (t (member value thisval))))
  ; *** complex condition, but the second element is an alternative in 'or'
  ;     e.g. '(adj noun ...)
        ((atom (second value))
           (cond ((or (eq (first value) thisval)
                      (eq thisval 'allval)) t)
                 (t (check-tag-condit feature (rest value) component))))
  ; *** complex condition, but the second element is a further subcondition
  ;     e.g. '(adj (type qualif) ...)
        (t (cond ((and (or (eq (first value) thisval)
                           (eq thisval 'allval))
                       (ok-subcondit? component (second value)))
                     t)
                 (t (check-tag-condit feature (rest (rest value)) component)))))))

;********************************************************************
; *** checks the truth of a subcondition. See the description of subli above
(defun ok-subcondit? (component subcond)
  (declare (special *GRAMMTYPES*))
  (cond ((eq (first subcond) 'not)
          (not (ok-subcondit? component (second subcond))))
        ((eq (first subcond) 'or)
          (check-or-subcondit component (rest subcond)))
        ((eq (first subcond) 'and)
          (check-and-subcondit component (rest subcond)))
        ((eq (first subcond) 'word-typ)
	  (member (sld-prendi component 'word)
                  (get (second subcond) 'grtype)
                  ;(first (leggi *GRAMMTYPES* ;'&that
                  ;                    (second subcond)))
                  :test #'eq-word))
        ((eq (first subcond) 'proper)
          (cond ((memq (second subcond) '(proper yes))
                   (eq (prendi (without-lemma component) 'proper) 'yes))
                (t (neq (prendi (without-lemma component) 'proper) 'yes))))
        ((or (eq (first subcond) 'gender)
             (eq (first subcond) 'number))
          (agr-unif (list (prendi (without-lemma component) (first subcond)))
                    (inlist (second subcond))))
        ((and (eq (first subcond) 'type)
              (eq (second subcond) 'main))
          ; *** the "main" type is the default for verbs
          (null (sld-prendi component 'type)))
	(t (member (sld-prendi component (first subcond))
                   (inlist (second subcond))))))

(defun check-or-subcondit (comp or-list)
  (cond ((null or-list) nil)
        ((ok-subcondit? comp (first or-list)) t)
        (t (check-or-subcondit comp (rest or-list)))))

(defun check-and-subcondit (comp and-list)
  (cond ((null and-list) t)
        ((ok-subcondit? comp (first and-list))
           (check-and-subcondit comp (rest and-list)))
        (t nil)))

;********************************************************************
; *** this extract from 'wrd' the interpretations having the value 'val' for
;     the feature 'feature'
; *** recursion on the interpretations in wrd
; *** INPUT:
;  >>> wrd: a list interpretations, any of which can be simple or compound
;  >>> feature: the feature that must be used to select the interpretation
;       (and that, possibly, was used for disambiguating)
;  >>> sing-val: the value of that feature. It is a list, which usually
;       includes a single value, but that could also include more than one
;       element for compounds, e.g. (verb pron)
;  >>> pos: first: the extraction has to be made on the basis of the first
;                  (or unique) component
;           last: the extraction has to be made on the basis of the last
;                  (or unique) component
;           all: the extraction has to be made on the basis of all components
;                 so that '(verb)' does not match a verb+pron
;           nil: any component is ok for the match
(defun single-prendi-wrd (wrd feature sing-val &optional pos)
  (cond ((> (length (inlist sing-val)) 1) 
           (exception 'morpho-error "PROC/postagger: single-prendi-word" sing-val)))
  (cond ((null wrd) nil)
        ((atom (first (first wrd)))
  ; *** the next interpretation is simple (not a compound)
          (cond ((or (and (atom sing-val) 
                          (eq sing-val (prendi (without-lemma (first wrd)) feature)))
                     (and (listp sing-val) 
                          (= 1 (length sing-val))
                          (eq (first sing-val) (prendi (without-lemma (first wrd)) feature))))
                  (cons (first wrd) 
                        (single-prendi-wrd (rest wrd) feature sing-val pos)))
                (t (single-prendi-wrd (rest wrd) feature sing-val pos))))
        ((null pos)
  ; *** the next interpretation is complex, and the check has to be made on any
  ;     component
          (cond ((member sing-val 
                        (mapcar #'(lambda (x) (prendi (without-lemma x) feature)) (first wrd)))
                  (cons (first wrd) (single-prendi-wrd (rest wrd) feature sing-val pos)))
                (t (single-prendi-wrd (rest wrd) feature sing-val pos))))
        ((eq pos 'all)
  ; *** the next interpretation is complex, and the check has to be made on all
  ;     components. This is used to keep apart, for instance, 'verb' and 'verb+pron'
  ;     interpretations
          (cond ((equal sing-val 
			(rem-lex-dup (lex-dis-prendi (first wrd) feature pos))
)
                  (cons (first wrd) (single-prendi-wrd (rest wrd) feature sing-val pos)))
                (t (single-prendi-wrd (rest wrd) feature sing-val pos))))
  ; *** the next interpretation is complex, and the check has to be made on the first
  ;     component
        ((eq pos 'first)
          (cond ((eq sing-val (prendi (without-lemma (first (first wrd))) feature))
                  (cons (first wrd) (single-prendi-wrd (rest wrd) feature sing-val pos)))
                (t (single-prendi-wrd (rest wrd) feature sing-val pos))))
  ; *** the next interpretation is complex, and the check has to be made on the last
  ;     component
        ((eq pos 'last)
          (cond ((eq sing-val (prendi (without-lemma (ult wrd)) feature))
                  (cons (first wrd) (single-prendi-wrd (rest wrd) feature sing-val pos)))
                (t (single-prendi-wrd (rest wrd) feature sing-val pos))))))

;********************************************************************
; *** converts something as (x y (z z)) into (x y z)
;     actually, in case of sublists, it takes the first without checking if
;     all elements are equal
(defun rem-lex-dup (li)
  (cond ((null li) nil)
	((atom (first li)) (cons (first li) (rem-lex-dup (rest li))))
	(t (cons (first (first li)) (rem-lex-dup (rest li))))))

;********************************************************************
; *** returns true if a line has only 'proper noun' interpretations
(defun all-proper (disw)
   (cond ((null disw) t)
	 ((eq (sing-lex-dis-prendi (first disw) 'proper 'first) 'yes)
	   (all-proper (rest disw)))
	 (t nil)))

;********************************************************************
(defun lex-dis-prendi-noloc (elemen featur pos)
   (lex-dis-prendi (remove-locut elemen 'fail) featur pos))

;********************************************************************
; *** this handles, at least partially, multiple words (e.g. prendilo)
;     if the word is simple, it works as 'prendi'; if the word is a
;     compound, takes the desired value form the first, the last, or all
;     the components, according to the value of the 'pos' argument (which
;     may take the values 'first', 'last', or 'all')
; *** The feature 'all' is used to get the categories, used to choose
;     the rule for tagging. So, in "capitolo", we get "NOUN" and "VERB+PRON"
; N.B. As usual, the form of the morphological output is:
;	(interp1 interp2 ... interpn)
;	swhere each 'interpi' may be (root cat ...) if the word is simple,
;	or ((root1 cat1 ...) (root2 cat2 ..) .....) if it is compound
(defun lex-dis-prendi (elemen featur pos)
  (declare (special featur pos))
  (cond ((listp elemen)
  	   (mapcar #'(lambda (x) (sing-lex-dis-prendi x featur pos))
	  	   elemen))
	(t nil)))

;********************************************************************
(defun sing-lex-dis-prendi (elemen featur pos)
  (cond ((stringp elemen) nil)	; *** morphological failure
	((atom (first elemen))	; *** simple element
          (sld-prendi elemen featur))
        ((eq pos 'first)	; *** compound element; use first component
          (sld-prendi (first elemen) featur))
        ((eq pos 'last)		; *** compound element; use last component
          (sld-prendi (ult elemen) featur))
; *** if 'pos' is 'all', it returns a list with all the values
; *** the cond inside the 'lambda' for forms with enclosed ambiguities, as 'vi'
;     in "tentarvi": ((tentare ....) ((vi .. pers) (vi .. dimos)))
        ((eq pos 'all)		; *** compound element; use all components
          (mapcar #'(lambda (x)
                        (cond ((atom (car x)) (sld-prendi x featur))
                              (t (sld-prendi (first x) featur))))
                  elemen))
        (t (exception 'morpho-error "PROC/postagger: sing-lex-dis-prendi" pos))))

;********************************************************************
; *** just to handle "word" as a feature;
(defun sld-prendi (el feat)
  (cond ((eq feat 'word) (first el))
	(t (prendi (without-lemma el) feat))))

;********************************************************************
; *** 		LOCUTIONS AND MARKERS
;********************************************************************
; *** checks if the words at the beginning of the list 'fras' can be a
;     locution. In this case, returns the corresponding list
(defun check-locut (fras)
   (let ((locut (int-ch-loc fras nil nil)))
       (cond ((eq locut 'fail) 'fail)
; *** locutions can be ambiguous, since a locutions can be the prefix
;     of another locution ((l11 l12 l13) (l21 l22 l23 l24 l25))
;     The longest one is chosen
; *** add-locut-feat is used for flexible locutions. In this case, only the
;     first item carries the feature infos (i.e. gender and number). It must
;     be copied in other items
             (t (let ((extlocut (mapcar #'add-locut-feat locut)))
                       (choose-longest (rest extlocut) (first extlocut)))))))

;********************************************************************
(defun add-locut-feat (singloc)
  (let (features resultloc)
     (cond ((eq 'flex (prendi (first singloc) 'loctype))
     ; *** this applies only to flexible locutions
        ; *** find the features: they have to be found in the component that
        ;     includes the feature "morphcat"
             (do ((nword (first singloc) (first temploc))
                  (temploc (rest singloc) (rest temploc)))
                 ((or (null nword) features))
                (cond ((not (null (prendi nword 'morphcat)))
                        (setq features `(gender ,(prendi nword 'gender)
                                         number ,(prendi nword 'number))))))
             (cond ((null features)
                      (exception 'tagger "features not found in a flexible locution")))
          ;(format t "singloc: ~a~%" singloc)
          ;(break "tagger: add-locut-feat")
             (dolist (nword singloc resultloc)
                  (cond ((not (null (prendi nword 'morphcat)))
                           (setq resultloc (append1 resultloc nword)))
                        (t (setq resultloc (append1 resultloc 
                                               (append nword features)))))))
           (t singloc))))

;********************************************************************
; *** INPUT:
;  >>> fras: the list of possibly ambiguous words
;  >>> locname: the identifier of the locution (e.g. "da_solo")
;  >>> contin: the next component of the locution
; **** this is recursive on "fras", i.e. it moves ahead on the possible
;      words composing the locution
(defun int-ch-loc (fras locname contin)
 (let (result tempres elem1 elem2)
  ; *** if at the end of the locution, ok
    (cond ((and locname (null contin)) '(nil))
  ; *** if at end of sentence, fail
          ((null fras) 'fail)
  ; *** if the next word is not known, fail
          ((not (listp (first fras))) 'fail)
          (t (let ((locut (extract-interp (first fras) 'locut 'yes nil)))
  ; *** if the next word has no locution interpretation, fail
	             (cond ((null locut) 'fail)
  ; *** otherwise, find a possible continuation
                           (t (dolist (currint locut
          ; *** the dolist loops on all possible locution interpretations of
          ;     the current word
                                       (cond ((null result) 'fail)
                                             (t ;(break "int-ch-loc-3")
                                                result)))
  ; *** we are within the locution: check the correspondence between what
  ;     expected ('locname' and 'contin'), and what found ('currint')
                               (cond ((listp (first currint))
    ; *** the sentence entry is a compound (case of prepart), so the first
    ;     word is its first and the continuation of the sentence is the rest
    ; *** here, I suppose that this situation arises only for prepart, so the
    ;     maximum lenght of compound elements is 2. 
                                       (setq elem1 (first currint))
                                       (setq elem2 (second currint))
    ; *** test on the first element
                                       (cond ((and
                                               (or 
                                                 (and (null locname)
                                                      (null 
                                                         (prendi 
                                                            (without-lemma 
                                                                       elem1)
                                                            'prev)))
                                     ; *** first word of the locution
                                                 (and (eq locname 
                                                            (first elem1))
                                                      (eq contin 
                                                         (prendi 
                                                            (without-lemma
                                                                       elem1)
                                                            'root))))
   ; *** test on the second element
                                               (null 
                                                  (prendi 
                                                     (without-lemma elem1)
                                                     'next))
                                               (null 
                                                  (prendi 
                                                     (without-lemma elem2) 
                                                     'locut)))
                                       ;(break "int-ch-loc-1")
                ; *** no recursion here, assuming that compound elements are at the end
                ;     If result is nil, standard processing
                ;     If it is non-nil, this means that there is an ambiguity. It can
                ;     arise (now, i.e. april 28, 2007) only from the attachment of an
                ;     article at the end of the locution ("in cima a"+"l'" --> in cima all';
                ;     i.e. "on the top of" + "the")
                ;     In this case, result has the form (((comp11 comp12)))
                ;        where the first level of parentheses concerns all results obtained so
                ;        far (just one here), the second level concerns the words composing the
                ;        result (just one here, since we are at the end of the locution), and the
                ;        third one refers to the compound word
                ;     and currint has the form          (comp11 comp22)
                ;     What is needed is (((comp11 (comp12 comp22))))
                                              (cond ((null result)
                                                       (setq result (list (list currint))))
                                                    ((equal (first currint)
                                                            (first (first (first result))))
                                                      (setq result 
                                                         (list 
                                                            (list 
                                                               (list (first currint)
                                                                     (list 
                                                                        (second currint) 
                                                                        (second 
                                                                           (first (first result)))))))))
                                                   (t (exception 'morpho-error 
                                                          "PROC/postagger: compound locutions"))))))
   ; *** the sentence entry is not a compound (standard case)
                                     ((or (and (null locname)
                                               (null 
                                                  (prendi 
                                                     (without-lemma currint)
                                                     'prev)))
                                          (and (eq locname (first currint))
                                               (eq contin 
                                                  (prendi 
                                                     (without-lemma currint)
                                                     'root))))
; *** the current word has satisfied the test, go on to the next word
                                       (setq tempres
                                           (int-ch-loc
                                              (rest fras)
                                              (first currint)
                                              (prendi (without-lemma currint)
                                                      'next)))
                                       (cond ((neq tempres 'fail)
                                       ;(break "int-ch-loc-2")
                                                (setq result
                                                    (append 
                                                        (mult-cons currint tempres)
                                                         result)))))
                                     (t nil))))))))))
			  
;*******************************************************************
; *** removes the locutions from a word interpretation
(defun remove-locut (word-interp locut-buffer)
  (cond ((null word-interp) nil)
        ((stringp word-interp) word-interp)
        ((atom (first word-interp)) word-interp)
        ((listp (first (first word-interp)))
   ; *** compound words (prepart)
           (cond ((and (eq 'yes (prendi (without-lemma (first (first word-interp))) 'locut))
                       (or ;(eq 1 (length word-interp))
                           (eq 'fail locut-buffer)))
                    (remove-locut (rest word-interp) locut-buffer))
                 (t (cons (first word-interp) 
                          (remove-locut (rest word-interp) locut-buffer))))
                                 	; if the locutionary prep+art is ambiguous
                                        ; (in cima all') keep it
          )
        ((eq 'yes (prendi (without-lemma (first word-interp)) 'locut))
          (remove-locut (rest word-interp) locut-buffer))
        (t (cons (first word-interp) (remove-locut (rest word-interp) locut-buffer)))))

;*******************************************************************
; *** controlla se gli elementi in testa a fras formano un marker
(defun check-marker-or (frasin fras marks buff)
     (cond ((null marks) 'fail)
	   ((null fras) 'fail)
	   ((equal (car marks) '|**|)
	     (let ((chosen1 (check-marker-cond (car fras) (cdr marks))))
		    (cond ((eq chosen1 'fail) 'fail)
			  (t (append1 buff chosen1)))))
	   ((atom (car marks))
	     (cond ((marker-equal (car frasin) (car marks))
		      (append1 buff (list (car frasin) 'cat 'marker)))
		   (t (check-marker-or frasin fras (rest marks) buff))))
	   (t (let ((first-and (check-marker-and frasin fras (car marks) buff)))
		 (cond ((eq first-and 'fail)
			 (check-marker-or frasin fras (cdr marks) buff))
		       (t first-and))))))
	     
;*******************************************************************
; *** analoga alla precedente, ma per il livello and
(defun check-marker-and (frasin fras marks buff)
     (cond ((null marks) buff)
	   ((null fras) 'fail)
	   ((atom (car marks))
	     (cond ((marker-equal (car frasin) (car marks))
		     (check-marker-and (cdr frasin) (cdr fras) (cdr marks)
				(append1 buff (list (car frasin) 'cat 'marker))))
		   (t 'fail)))
	   (t (let ((first-or (check-marker-or frasin fras (car marks) buff)))
		 (cond ((eq first-or 'fail) 'fail)
		       (t (check-marker-and (cdr frasin) (cdr fras)
				(cdr marks) first-or)))))))
	     
;*******************************************************************
; *** analoga alla precedente, ma per le condizioni (**)
(defun check-marker-cond (word condit)
     (cond ((not (listp word)) 'fail)
	   (t (int-ch-mark-cond word condit condit))))

;*******************************************************************
(defun int-ch-mark-cond (word condit origcond)
    (cond ((null word) 'fail)
	  ((null condit)
	     (first word))
	  ((and (equal (second condit) 
		  (sing-lex-dis-prendi (car word) (first condit) 'first))
		(neq 'yes (sing-lex-dis-prendi (car word) 'locut 'first)))
	      (int-ch-mark-cond word (rest (rest condit)) origcond))
	  (t (int-ch-mark-cond (rest word) origcond origcond))))

;*******************************************************************
; *** confronta un elemento di marker che arriva dal file GULL/LEXIC/
;     text-marker con una parole in input. Nel file, gli elementi sono
;     memorizzati come stringhe, ma dall'input possono arrivare anche
;     come atomi. In questo caso il confronto va fatto sull'explode.
(defun marker-equal (word marker)
     (cond ((stringp marker)
	     (equal (string-trim '(#\space) word)
		    (string-trim '(#\space) marker)))
	   (t (equal (explode word)
		     (explode (string-trim '(#\space) marker))))))
	
;********************************************************************
; *** 		END OF LOCUTIONS AND MARKERS
;********************************************************************

;********************************************************************
; *** applies 'mapcar' only if the second argument is not a string
(defun tag-mapcar (funz arg)
  (cond ((listp arg) (mapcar funz arg))))

;********************************************************************
; *** tratta i nomi di categorie (incluse quelle composte)
(defun buildcatnames (categ)
   (cond ((atom categ) categ)
	 (t (buildmultcatname (cdr categ) (explode (car categ))))))

;********************************************************************
; ***  costruisce i nomi delle categorie di parole composte
(defun buildmultcatname (catlist nameacc)
  (cond ((null catlist) (implode nameacc))
; *** i segni di interpunzione non sono considerati
	((eq (car catlist) 'segnointer)
	  (buildmultcatname (cdr catlist) nameacc))
	(t (buildmultcatname (cdr catlist)
			(append nameacc '(#\+) (explode (car catlist)))))))

;********************************************************************
; *** split names, according to a 'separator'
;     ex prep+art ---> (prep art)
; *** N.B. The input has already been exploded ('p 'r 'e 'p '+ 'a 'r 't)
(defun expl+cats (explcateg separator)
     (int-expl+c explcateg separator nil))

(defun int-expl+c (chars separator acc)
  (cond ((null chars) (list (implode acc)))
        ((eq (car chars) separator)
          (cons (implode acc) (int-expl+c (cdr chars) separator nil)))
        (t (int-expl+c (cdr chars) separator (append1 acc (car chars))))))

;#######################################################################
;
;	FUNCTIONS APPEARING IN THE ANTECEDENTS OF THE RULES
;
;#######################################################################

; *** This comment describes the predicates used in the tagging rules
;     (lexdisambr.lisp), which are defined as Lisp functions below
;     The predicates accept arguments of the following types:
;     ARG-TYPE-1: an atomic value. The involved feature is identified by
;                 the predicate's name
;     ARG-TYPE-2: an atomic value or a list of values. The involved 
;                 feature is identified by the predicate's name
;     ARG-TYPE-3: either an atom or a list. In case it is a list, then
;                 each atomic item is a value, each pair is a <feature
;                 value> item, referring to the previous value;
;                 'noun --> the PoS Tag value 'noun'
;                 '(adj noun) --> either the value adj or the value noun
;                 '(adj (gender m) noun) --> aither an item with PoS Tag
;                      adj and gender m, or an item with PoS Tag noun
;
; 1. Predicates testing the categories of the surrounding words
;    All predicates below admit an argument of type ARG-TYPE-3,
;    except the unes with suffix -agr, which take a first argument
;    of ARG-TYPE-3, and a second argument of type ARG-TYPE-1
;     ---> subconditions:
;          -- mood participle, infinite, 
;          -- proper yes, 
;          -- proper nil, 
;          -- type interr, qualif, coord, refl-impers, subord, aux,
;                  indef, (qualif interr), relat, deitt, time, mod,
;          -- word-typ &base-relat,
;          -- not (type aux), or (type aux) (type mod),
;    - beforecat:             11
;    - prev2cat:              93
;    - prevcat+:              15
;    - prevcat-agr:           70
;    - prevcat:              388
;    - nextcat:              422
;    - nextcat-agr:           72
;    - next2cat:              85
;    - next2cat-agr:           4
; 2. Predicates testing the type feature (subcategory) of the surrounding words
;    They take a single argument of type ARG-TYPE-2; the value(s) must be values
;    of the feature 'type'
;    - prev2type:              7
;    - prevtype:              37
;    - currtype:              54
;    - nexttype:              28
;    - next2type:              1
; 3. Predicates testing the special class
;    (as stored in language/KB-lang/DICTION-KB-lang/grammtypes-lang.dat)
;    of the surrounding words
;    They take a single argument of type ARG-TYPE-2; the value(s) must be special
;    class names
;    - beforeword-typ:         4
;    - prev2word-typ:         15
;    - prevword-typ:          83
;    - currword-typ:         149
;    - nextword-typ:         110
;    - next2word-typ:         11
;    - afterword-typ:          1
; 4. Predicates testing the semantic category
;    (as stored in language/KB-lang/SEMANT-KB-lang/semtypes-lang.lisp)
;    of the surrounding words
;    They take a single argument of type ARG-TYPE-1; the value must be semantic
;    category names
;    - prevsemtype:            4
;    - currsemtype:            4
;    - nextsemtype:            2
;    - next2semtype:           1
; 5. A predicate testing the ontological category
;    (as stored in ALLANG/KB-ALL/SEMANT-KB-ALL/ontoology-base.dat)
;    of the surrounding words
;    It takes a single argument of type ARG-TYPE-1; the value must be an
;    ontological category name. In principle, if an ontology were always
;    available, all predicates of the previous group should be substituted
;    by predicates of this group
;    - nextonttype:            1
; 6. Predicates testing single characters
;    They take a single argument of type ARG-TYPE-2; the value(s) must be 
;    lisp character names
;    - prevchar:              53
;    - nextchar:              33
;    - next2char:              3
; 7. Predicates testing the verbal mood
;    They take a single argument of type ARG-TYPE-2; the value(s) must be 
;    verbal moods
;    - prev2mood:              5
;    - prevmood:               4
;    - currmood:              94
;    - nextmood:              50
;    - nextmood-simple:        4
;    - next2mood:             22
; 8. Predicates testing the verbal tense
;    They take a single argument of type ARG-TYPE-2; the value(s) must be 
;    verbal tenses
;    - prevtense:              4
;    - currtense:             33
;    - nexttense:              6
;    - next2tense:             2
; 9. A predicate testing the verbal mood and tense (for the same item)
;    It takes two arguments of ARG-TYPE-1, the first a mood, the second
;    a tense
;    - currmood+tense 	       2 
; 10. Predicates testing the syntactic number
;    They take a single argument of type ARG-TYPE-1; the value must be 
;    a syntactic number (sing, pl, allval)
;    - prev2number:            1
;    - prevnumber:             5
;    - currnumber:             1
;    - nextnumber:             4
; 11. Predicates testing the syntactic gender
;    They take a single argument of type ARG-TYPE-1; the value must be 
;    a syntactic gender (m, f, allval)
;    - prev2gender:            1
;    - prevgender:             2
;    - nextgender:             4
;    - next2gender:            1
; 12. Predicates testing the syntactic person
;    They take a single argument of type ARG-TYPE-1; the value must be 
;    a syntactic person (p1, p2, p3)
;    - currperson:            13
;    - nextperson:             7
; 13. Predicates testing the syntactic case
;    They take a single argument of type ARG-TYPE-1; the value must be 
;    a syntactic case (lsubj, lobj, liobj, obl)
;    - nextcase:	       1
; 14. Miscellanea
;    - currproper	      18
;      It takes no argument and checks if there is, for the current word,
;      a proper name interpretation
;    - default-proper	       8
;      It takes no argument and returns true if the only proper name 
;      intepretation of the current word is a default
;    - all-capital	       2
;      It takes no argument and returns true if the current input word
;      is formed only with capitalized letters
;    - capitalized-w	       3
;      It takes no argument and returns true if the first letter of the
;      current input word is a capital letter
;    - interr-sent	       5
;      It takes no argument and returns true if the sentence includes
;      an interrogative marker (e.g. '?' for Italian and English)
;    - single-verb-sent	       5
;      It takes no argument and returns true if the sentence includes
;      just one verb (excluding auxiliaries)
;    - prep-govern-next	       5
;      It checks if the current word (its required prepositional interpretation)
;      can govern the preposition that follows. It takes an optional argument
;      which is 't', in case the preposition can directly govern a noun group
;    - verb-govern-adv	       2
;      It takes no argument and checks if the immediately preceding verb
;      can govern the current item, which must be an adverb. Used for English
;      compound verb (as 'move up')
;    - is-third-word	       2
;      It takes no arguments and checks if there are no more than two words
;      before the current one (used for English imperatives)
;    - after-loc-type	       1
;      It is the first attempt at locution disambiguation. It takes one
;      argument and checks if after the end of the current locution there
;      is a word of that type (subcategory). It also uses a global variable
;      (locution-length) that must contain the length of the locution under 
;      analysis
;    - has-prevverb-subcat-class   1
;      It takes a single argument of type ARG-TYPE-2, and checks if the previous
;      word (which must be a verb) belongs (possibly among others) to that
;      verbal subcategorization class (it applies verb class inheritance)
;      (as stored in language/KB-lang/SUBCAT-KB-lang/verbclass-lang.dat)

;********************************************************************
; *** Usually, the structure of the elements of rem-frase is:
;     (wordn wordn+1 wordn+2 ....)
;     where each word-n+i is a list of ambiguities 
;       [es. ((democratico cat adj ...) (democratico cat noun ...))]
;    If a wordn+i is ambiguous, it has one more level of parentheses
;       [in the case of "nei" (It for "in+the" and for "moles"), we have:
;        (((in cat prep...) (il cat art ...)) (neo cat noun ...))]
; *** In these cases, the "next word" should be ambiguous between
;     "in" and "neo"      
(defun take-next-word (rem-frase)
   (let ((nextw (first (skip-sic rem-frase))) result)
      (cond ((stringp nextw) nil)
            (t (dolist (nextamb nextw result)
                  (cond ((is-compound nextamb)
                           (setq result (append1 result (first nextamb))))
                        (t (setq result (append1 result nextamb)))))))))

(defun take-2next-word (rem-frase)
   (rest2-locut (skip-sic (rest-locut (skip-sic rem-frase)))))

; *** In the following functions, prev-frase has the following structure:
;     (prevelem prev2elem prev3elem ...)
;     Each prev-i-elem is a list of ambiguities:
;     <interp-i-1 interp-i-2 interp-i-3 ...>. Actually, the list 
;     is always of length 1, since the previous words have already been
;     disambiguated.
;     In case the word is not a compound, interp-i-1 has the form 
;     [lemma cat ... type ..]
;     Otherwise, it has the form 
;     [[sublemma-a cat ... type ..] [sublemma-b cat ... type ..]]
;     
(defun take-prev-word (prev-frase)
   (first (skip-sic-back prev-frase)))

(defun take-2prev-word (prev-frase)
; *** prev is the actual interpretation (the only one, i.e. the first)
;     of the previous word. Actually, the rest of "take-prev-word ..."
;     should always be nil
   (let ((prev (take-prev-word prev-frase)) prev2)
       (cond ((stringp prev)
                (setq prev2 (second prev-frase))
                (cond ((is-compound prev2)
                         (list (prendi-first-comp prev2)))
                      (t prev2)))
             ((is-compound (first prev))
                (list (prendi-butlast-comp (first prev))))
             (t (first (skip-sic-back (rest (skip-sic-back prev-frase))))))))

(defun take-last-word (rem-frase)
   (ult rem-frase))

(defun null-3prev (prev-frase)
   (let ((prev (take-prev-word prev-frase)) prev2)
       (cond ((stringp prev)
                (setq prev2 (second prev-frase))
                (cond ((is-compound prev2) nil)
                      (t (null (rest (rest prev-frase))))))
             ((is-compound (first prev))
                (null (rest prev-frase)))
             (t (null (rest (rest prev-frase)))))))

;********************************************************************
;  this function just to skip "((sic))" comments
; *** on july 27, 2008, I try to skip all parentheses
(defun skip-sic (rem-frase)
  ; *** the first branch for true "((sic))"
   (cond ((and (equal (first (lex-dis-prendi (first rem-frase) 'word 'first))
			  #\()
   	       (equal (first (lex-dis-prendi (second rem-frase) 'word 'first))
			  #\()
   	       (equal (first (lex-dis-prendi (third rem-frase) 'word 'first))
			  'sic)
   	       (equal (first (lex-dis-prendi (fourth rem-frase) 'word 'first))
			  #\))
   	       (equal (first (lex-dis-prendi (fifth rem-frase) 'word 'first))
			  #\)))
	   (nthcdr 5 rem-frase))
  ; *** the second branch to skip parentheses
         ((equal (first (lex-dis-prendi (first rem-frase) 'word 'first))
		  #\()
            (rem-after-close-par (rest rem-frase)))
	 (t rem-frase)))

;********************************************************************
; *** this takes the rest of a sentence after a closed par
(defun rem-after-close-par (rem-frase)
   (cond ((null rem-frase) nil)
   	 ((equal (first (lex-dis-prendi (first rem-frase) 'word 'first))
		  #\))
            (rest rem-frase))
         (t (rem-after-close-par (rest rem-frase)))))

;********************************************************************
;  the same as above, but moving backward
(defun skip-sic-back (rem-frase)
   (cond ((and (equal (first (lex-dis-prendi (first rem-frase) 'word 'first))
			  #\))
   	       (equal (first (lex-dis-prendi (second rem-frase) 'word 'first))
			  #\))
   	       (equal (first (lex-dis-prendi (third rem-frase) 'word 'first))
			  'sic)
   	       (equal (first (lex-dis-prendi (fourth rem-frase) 'word 'first))
			  #\()
   	       (equal (first (lex-dis-prendi (fifth rem-frase) 'word 'first))
			  #\())
	   (nthcdr 5 rem-frase))
  ; *** the second branch to skip parentheses
         ((equal (first (lex-dis-prendi (first rem-frase) 'word 'first))
		  #\))
            (rem-before-open-par (rest rem-frase)))
	 (t rem-frase)))

;********************************************************************
; *** this takes the beginning of a sentence before an open par
(defun rem-before-open-par (rem-frase)
   (cond ((null rem-frase) nil)
   	 ((equal (first (lex-dis-prendi (first rem-frase) 'word 'first))
		  #\()					; )
            (rest rem-frase))
         (t (rem-before-open-par (rest rem-frase)))))

;********************************************************************
;  this is a lookahead! It checks if the next word is the beginning of a
;  locution. In such a case, it advances to the end of the locution, instead
;  of advancing of a single word
(defun rest-locut (rem-frase)
   (let ((locut (check-locut rem-frase)))
        (cond ((eq locut 'fail) (rest rem-frase))
              (t (nthcdr (length locut) rem-frase)))))

;********************************************************************
;  This is used inside take-2next-word. After advancing to the end of
;  the next word, it checks for a possible locution interpretation of
;  what follows. If it is a locution, it keeps only the correct
;  locution interpretation, otherwise it cancels all locutions 
(defun rest2-locut (rem-frase)
   (let ((locut (check-locut rem-frase)))
        (cond ((eq locut 'fail) 
                 (remove-locut (first rem-frase) 'fail))
              (t (list (first locut))))))

;********************************************************************
;********************************************************************
; **** ACTUAL FUNCTIONS
;********************************************************************
;********************************************************************

; %%%%%%%%%%%% GENERAL FUNCTIONS %%%%%%%%%%%%%
;********************************************************************
; *** This merges two subconditions
;     It is assumed that the subconditions are in and. If both of them
;     are already and-ed, a single 'and' is produced. If just one of them
;     already is an and, then the other is added as further conjunct
;     If one of them is an or, it is simply taken as a conjunct (the and
;     dominates)
(defun expand-condit (cond subcond)
  (cond ((null cond) nil)
   ; *** end of expansion
        ((atom (second cond))
   ; *** cond = (art adj ...)
   ;     result: (art subcond adj ...)
           (cons (first cond)
                 (cons subcond
                       (expand-condit (rest cond) subcond))))
        ((eq 'and (first (second cond)))
   ; *** cond = (pron (and (type indef) (case C)) adj ...)
           (cond ((eq 'and (first subcond))
       ; *** subcond = (and (gender m) (number pl))
       ;     result: (pron (and (type indef) (case C) (gender m) (number pl)) adj ...)
                    (cons (first cond)
                          (cons (cons 'and (append (rest (second cond)) (rest subcond)))
                                (expand-condit (rest (rest cond)) subcond))))
       ; *** subcond = (gender m)
       ;     result: (pron (and (type indef) (case C) (gender m)) adj ...)
                 (t (cons (first cond)
                          (cons (append1 (second cond) subcond)
                                (expand-condit (rest (rest cond)) subcond))))))
   ; *** cond = (pron (type indef) adj ...)
        (t (cond ((eq 'and (first subcond))
       ; *** subcond = (and (gender m) (number pl))
       ;     result: (pron (and (type indef) (gender m) (number pl)) adj ...)
                    (cons (first cond)
                          (cons (append1 subcond (second cond))
                                (expand-condit (rest (rest cond)) subcond))))
       ; *** subcond = (gender m)
       ;     result: (pron (and (type indef) (gender m)) adj ...)
                 (t (cons (first cond)
                          (cons (list 'and (second cond) subcond)
                                (expand-condit (rest (rest cond)) subcond))))))))
       
;********************************************************************
; *** checks if the previous word and the current word may form an
;     italian noun-noun sequence
(defun noun-noun ()
 (declare (special disword rev-iniz-frase *NOUN-NOUN*))
    (let ((prevlemma (first (prendi-last-comp (take-prev-word rev-iniz-frase))))
          (currlemmas (mapcar #'first (extract-interp disword 'cat 'noun nil 'first)))
          found)
  ; *** there may be more than one currlemmas if the current word has more than one
  ;     noun interpretation (e.g. morte - death - vs. morto - dead)
      (do ((lemma (first currlemmas) (first currlemmas))
           (currlemmas (rest currlemmas) (rest currlemmas)))
         ((or found (null lemma)) found)
         (setq found (member (list prevlemma lemma) *NOUN-NOUN* :test #'equal)))))

;*********************************************************************
; ****** this is USED in nextonttype ****
; *** This builds a single list of semantic ambiguities, starting from the
;     meanings of different words. Each word meaning is represented as:
;     (££person)      simple unambiguous meaning 
;     ((cat1 ££conc1) (cat2 ££conc2) ... (catN concN))  
;                     ambiguity that can be disambiguated on the basis of the category
;     (((££conc1 ££conc2 ££conc3 ...)))   
;                     standard ambiguity; the multiple level of parentheses has been
;                     added just to keep apart this case form the previous one
; *** the result of sem-flatten is single-level parenthesis, where an item can be
;     a pair in case a meaning is associated with a given category
;     (sem-flatten '((conc1) (conc2) ((cat1 conc3) (cat2 conc4)) (((conc5 conc6))) (conc7)))
;     ---> (conc1 conc2 (cat1 conc3) (cat2 conc4) conc5 conc6 conc7)
(defun sem-flatten (meanlist)
  (let ((firstmean (first meanlist)))
      (cond ((null firstmean) nil)
            ((atom (first firstmean))
               (cons (first firstmean) (sem-flatten (rest meanlist))))
            ((atom (first (first firstmean)))
               (append firstmean (sem-flatten (rest meanlist))))
            (t (append (first (first firstmean)) 
                       (sem-flatten (rest meanlist)))))))

;********************************************************************
(defun formagree (nxwrd thiswrd)
; *** this carries out a check on the compatibility of the "quel vs. quello" adj
;     This applies to Italian:
;     - quello scaffale: ok; quel scaffale: no
;     - quel cane: ok; quello cane: no
;     Useful for "quello politico", where the ADJ interpretation for "quello" is
;     not admissible, so that "quello" must be a pronoun
;     In this example, nxwrd is "politico" and this wrd is "quello"
; *** nxwrd and thiswrd are the morphsynt data
;     (e.g. quello --> ((QUELLO CAT ADJ TYPE DEMONS ...)))
;     inpword and nextword are di input item (e.g. quello --> |quello|)
  ; *** inpword and nextword global from disamb-categ (input form)
  (declare (special inpword nextword *LANGUAGE*))
  ; *** this check is currently made just for Italian
  (cond ((and (eq *LANGUAGE* 'italian)
              (eq (prendi (without-lemma (first thiswrd)) 'cat) 'adj) 
	      (has-gramm-type (sld-prendi (first thiswrd) 'word) '&that))
	   (form-compatible inpword nextword))
	(t t)))

;********************************************************************
(defun form-compatible (this next)
  (let ((explthis (reverse (explode this)))
	(explnext (explode next)))
       (or 
; *** femminine (singular or plural), or elided: ok
	   (char= #\a (first explthis))			; quella
           (char= #\e (first explthis))			; quelle
           (char= #\' (first explthis))			; quell'
; *** masculine (singular): extended form (quello)
;     before a word starting with s+consonante
           (and (char= #\o (first explthis))	        ; quello
		(char= #\l (second explthis))
		(char= #\s (first explnext))		; s+consonante
		(not (member (second explnext) '(#\a #\e #\i #\o #\u))))
; *** masculine (plural): extended form (quegli)
;     before a word starting with s+consonante
           (and (char= #\i (first explthis))	        ; quegli
		(char= #\l (second explthis))
		(or (and (member (first explnext) '(#\s #\S))	; s+consonante
		         (not (member (second explnext) '(#\a #\e #\i #\o #\u))))
		(member (first explnext) '(#\a #\e #\i #\o #\u))))  ; vowel
; *** masculine (singular or plural): reduced form (quel o quei)
           (and (char= #\l (first explthis))		; quel or quei
		(char= #\e (second explthis))
		(or (not (char= #\s (first explnext)))	; not s+consonante
		    (member (second explnext) '(#\a #\e #\i #\o #\u)))))))

;*********************************************************************
; *** checks if the sentence includes just one verb
(defun single-verb-sent ()
  (declare (special fine-frase))
  ; *** "first-first" takes the first component of the first interpretation
  (let ((words fine-frase) (numverb 0) verbinterp)
     (do ((nextw (first words) (first words))
          (words (rest words) (rest words))
          (is-aux nil nil))
         ((or (null nextw) (> numverb 1))
            (cond ((> numverb 1) nil)
                  (t t)))
         (setq verbinterp (extract-interp nextw 'cat 'verb nil 'last))
  ; *** the next to exclude that an auxiliary is taken as a separate verb
         (do ((nxtverb (first verbinterp) (first verbinterp))
              (verbinterp (rest verbinterp) (rest verbinterp)))
             ((or (null nxtverb) is-aux)
     ; *** on exit, if no auxiliary interpretation has been found, it is a main verb
               (cond ((not is-aux)
                       (setq numverb (1+ numverb)))))
             (cond ((atom (first nxtverb))    ; *** not compound
                     (setq is-aux (eq (prendi nxtverb 'type) 'aux)))
                   (t (setq is-aux (eq (prendi (first nxtverb) 'type) 'aux))))))))

;*********************************************************************
; *** checks if in the two lists thare are at least two values unifiable for agreement
(defun agr-unif (vallist1 vallist2)
  (or (null vallist1)
      (null vallist2)
      (member nil vallist1)
      (member nil vallist2)
; *** for instance, if the word is the adjective "ex", vallist is (nil)
      (member 'allval vallist1)
      (member 'allval vallist2)
      (intersection vallist1 vallist2)))

;*********************************************************************
; *** returns true if the the first char of the input word is capital letter
(defun capitalized-w ()
   (declare (special inpword))
   (member (get-tule-char-name-from-base-code (mapcar #'char-code (explode inpword)))
           (get-charset-value 'caplet)))

;*********************************************************************
; *** returns true if the input word is composed of only capitalized
;     letters
(defun all-capital ()
   (declare (special inpword))
   (let ((explinp (explode inpword)))
       (a-c-int (convert-base-codes-to-tule-char-names (mapcar #'char-code explinp))
                (get-charset-value 'caplet))))

(defun a-c-int (tulenames capitals)
  (cond ((null tulenames) t)
        ((member (first tulenames) capitals)
           (a-c-int (rest tulenames) capitals))
        (t nil)))

;*********************************************************************
; *** returns true if the only proper name interpretation is a default
(defun default-proper ()
  (declare (special disword))
  (int-d-p disword))

(defun int-d-p (word)
  (cond ((null word) t)
        ((eq 'yes (prendi (without-lemma (first word)) 'proper))
          (cond ((eq 'yes (prendi (without-lemma (first word)) 'default))
                   (int-d-p (rest word)))
                (t nil)))
        (t (int-d-p (rest word)))))

;*********************************************************************
; *** checks if the last item of the sentence is a question mark
(defun interr-sent ()
  (declare (special fine-frase))
  (let ((lastw (take-last-word fine-frase)) found)
      (cond ((listp lastw)
           ; *** it is not an unknown word
              (do ((nxtint (first lastw) (first lastw))
                   (lastw (rest lastw) (rest lastw)))
             ; *** nxtint is the next interpretation
                  ((or (null nxtint) found) found)
                 (cond ((atom (first nxtint))
                ; *** nxtint is not compound
                         (setq found
                             (equal (sld-prendi nxtint 'word) #\?)))
                ; *** nxtint is compound
                       (t (setq found
                             (equal (sld-prendi (ult nxtint) 'word) #\?))))))
            (t nil))))

;********************************************************************
; *** true if the there are no more than two words before
(defun is-third-word ()
 (declare (special rev-iniz-frase))
 (null-3prev rev-iniz-frase))

; %%%%%%%%%%%% FUNCTIONS FOR CHECKING THE CURRENT WORD %%%%%%%%%%%%%

;*********************************************************************
; *** this is useful in disambiguation rules for general categories
;     (as general-pl-sing)
(defun currcat (categs)
  (declare (special disword))
  (extract-interp disword 'cat categs nil 'first))

;*********************************************************************
(defun currmood (moods)
  (declare (special disword))
  (extract-interp disword 'mood moods nil 'first))

;*********************************************************************
(defun currmood+tense (mood tense)
  (declare (special disword))
  (let ((tempwrd (extract-interp disword 'mood mood nil 'first)))
       (extract-interp tempwrd 'tense tense nil 'first)))

;*********************************************************************
(defun currmood+pers (mood pers)
  (declare (special disword))
  (let ((tempwrd (extract-interp disword 'mood mood nil 'first)))
       (extract-interp tempwrd 'person pers nil 'first)))

;*********************************************************************
(defun currnumber (number)
  (declare (special disword))
  (extract-interp disword 'number number nil 'first))

;*********************************************************************
(defun currtense (tense)
  (declare (special disword))
  (extract-interp disword 'tense tense nil 'first))

;*********************************************************************
(defun currperson (pers)
  (declare (special disword))
  (extract-interp disword 'person pers nil 'first))

;*********************************************************************
(defun currproper ()
  (declare (special disword))
  (extract-interp disword 'proper 'yes nil 'first))

;*********************************************************************
(defun currtype (type)
  (declare (special disword))
  (extract-interp disword 'type type nil 'first))

;*********************************************************************
(defun currword-typ (word-typ)
  (declare (special disword))
  (extract-interp disword 'word-typ word-typ nil 'first))

;*********************************************************************
(defun currsemtype (cat semtype)
  (declare (special disword *LISP-CHAR-SET-ID*))
  (let* ((ninterp (extract-interp disword 'cat cat nil 'first))
            ; *** all interpretations having as first (or single) element
            ;     the one of the given category
         (firstints (mapcar #'prendi-first-comp ninterp))
            ; *** the first (or single) elements of those interpretations
         (nwords (mapcar #'first firstints))
            ; *** the lemma of those elements
         found)
     (do ((nxtword (first nwords) (first nwords))
          (nwords (rest nwords) (rest nwords)))
         ((or (null nxtword) found) found)
         (setq found (inh-member nxtword semtype)))))
  ; *** inh-member in KB/language/semtypes-lang.lisp
  ;     where "language" is ITALIAN, ENGLISH, SPANISH or FRENCH

; %%%%%%%%%%%% FUNCTIONS FOR CHECKING THE PREVIOUS WORD %%%%%%%%%%%%%

;********************************************************************
; *** checks if the previous word (already disambiguated) satisfies the
;     conditions in categs
(defun prevcat (categs)
 (declare (special rev-iniz-frase))
 (extract-interp (take-prev-word rev-iniz-frase) 'cat categs nil 'last))

;********************************************************************
(defun prevchar (chars)
; *** it checks if the previous word is a character among chars (chars
;     could also be a single char)
 (declare (special rev-iniz-frase))
 (extract-interp (take-prev-word rev-iniz-frase) 'word chars nil 'last))

;*********************************************************************
(defun prevsemtype (semtype)
 (declare (special rev-iniz-frase *LISP-CHAR-SET-ID*))
   ;(cond ((eq 'UTF-8 *LISP-CHAR-SET-ID*)
   ;        (setq semtype (convert-base-atom-or-string-to-currlisp semtype))))
   (inh-member (first (prendi-last-comp (take-prev-word rev-iniz-frase))) semtype))
  ; *** inh-member in KB/language/semtypes-lang.lisp
  ;     where "language" is ITALIAN, ENGLISH, SPANISH or FRENCH

;********************************************************************
; *** as prevcat, but it carries out also agreement checks
(defun prevcat-agr (categs thiscat)
  (declare (special disword rev-iniz-frase))
  (let* ((thisinterp (extract-interp disword 'cat thiscat t 'last))
         (thisnumber 
            (mapcar #' (lambda (x) (prendi (without-lemma x) 'number)) thisinterp))
         (thisgender 
            (mapcar #' (lambda (x) (prendi (without-lemma x) 'gender)) thisinterp))
         newcondit)
  ; *** categs may be 'art or '(art adj ...)
  ;     In the first case, we must produce '(art (and (number X) (gender Y))); 
  ;     In the second, 
  ;       '(art (and (number X) (gender Y)) adj (and (number X) (gender Y)))
      (setq newcondit
           (expand-condit (inlist categs) 
                          `(and (number ,thisnumber) (gender ,thisgender))))
      (extract-interp (take-prev-word rev-iniz-frase) 'cat newcondit nil 'last)))


;********************************************************************
; *** as prevcat, but skipping a possible comma
(defun prevcat+ (categs)
 (declare (special rev-iniz-frase))
 (let ((prec (take-prev-word rev-iniz-frase)))
     (cond ((and (listp prec)
	         (equal (sld-prendi (first prec) 'word) #\,))
	     (setq prec (second rev-iniz-frase))))
     (extract-interp prec 'cat categs nil 'last)))

;*********************************************************************
; *** returns the interpretations of the previous word of gender 'gend'
(defun prevgender (gend)
 (declare (special rev-iniz-frase))
 (extract-interp (take-prev-word rev-iniz-frase) 'gender gend nil 'last))

;*********************************************************************
(defun prevmood (mood)
; *** returns the interpretations of the previous word of mood 'mood'
 (declare (special rev-iniz-frase))
 (extract-interp (take-prev-word rev-iniz-frase) 'mood mood nil 'last))

;*********************************************************************
; *** returns the interpretations of the previous word of number 'numb'
(defun prevnumber (numb)
 (declare (special rev-iniz-frase))
 (extract-interp (take-prev-word rev-iniz-frase) 'number numb nil 'last))

;*********************************************************************
(defun prevperson (pers)
 (declare (special rev-iniz-frase))
 (extract-interp (take-prev-word rev-iniz-frase) 'person pers nil 'last))

;*********************************************************************
; *** returns the interpretations of the previous word of tense 'tense'
(defun prevtense (tense)
 (declare (special rev-iniz-frase))
 (extract-interp (take-prev-word rev-iniz-frase) 'tense tense nil 'last))

;*********************************************************************
(defun prevtype (type)
; *** returns the interpretations of the previous word of type 'type'
 (declare (special rev-iniz-frase))
 (extract-interp (take-prev-word rev-iniz-frase) 'type type nil 'last))

;********************************************************************
(defun prevword-typ (word-typ)
; *** it checks if the previous word is of the grammatical type "word-typ"
 (declare (special rev-iniz-frase))
 (extract-interp (take-prev-word rev-iniz-frase) 'word-typ word-typ nil 'last))

;********************************************************************
; *** returns true if the previous word (a preposition) can govern the current word
(defun prev-prep-govern ()
 (declare (special rev-iniz-frase disword))
 (let ((prep-gov (first (lex-dis-prendi (take-prev-word rev-iniz-frase) 'domin 'last)))
       (currprep 
          (first 
             (lex-dis-prendi 
                 (extract-interp disword 'cat 'prep nil 'first) 'word 'first))))
     (member currprep prep-gov)))

;*********************************************************************
; *** returns true if the previous word (which must be a verb) belongs
;     possibly among others, to the specified subcategorization class
(defun has-prevverb-subcat-class (subcat-class)
 (declare (special rev-iniz-frase))
  ; *** first takes the first interpretation; the other first, the lemma
 (let ((lemma (first (first (take-prev-word rev-iniz-frase)))))
     (is-inthn-dependent (get lemma 'verbal-class) subcat-class)))

; %%%%%%%%%%%% FUNCTIONS FOR CHECKING THE SECOND PREVIOUS WORD %%%%%%%%%%%%%

;********************************************************************
(defun prev2cat (categs)
; *** as prevcat but going back of 2 words
 (declare (special rev-iniz-frase))
 (extract-interp (take-2prev-word rev-iniz-frase) 'cat categs nil 'last))

;********************************************************************
(defun prev2cat+ (categs)
; *** as prevcat but going back of 2 words, and skipping a possible comma
 (declare (special rev-iniz-frase))
 (let ((prec (take-2prev-word rev-iniz-frase)))
     (cond ((and (listp prec)
	         (equal (sld-prendi (first prec) 'word) #\,))
	     (setq prec (third rev-iniz-frase))))
     (extract-interp prec 'cat categs nil 'last)))

;********************************************************************
(defun prev2gender (genders)
; *** as prevgender but going back of 2 words
 (declare (special rev-iniz-frase))
 (extract-interp (take-2prev-word rev-iniz-frase) 'gender genders nil 'last))

;********************************************************************
(defun prev2number (numbers)
; *** as prevgender but going back of 2 words
 (declare (special rev-iniz-frase))
 (extract-interp (take-2prev-word rev-iniz-frase) 'number numbers nil 'last))

;********************************************************************
(defun prev2mood (moods)
; *** as prevmood but going back of 2 words
 (declare (special rev-iniz-frase))
 (extract-interp (take-2prev-word rev-iniz-frase) 'mood moods nil 'last))

;********************************************************************
; *** as prevtype, but back of 2 words
(defun prev2type (types)
 (declare (special rev-iniz-frase))
; *** come la prevtype, ma indietro di 2 parole
 (extract-interp (take-2prev-word rev-iniz-frase) 'type types nil 'last))

;********************************************************************
(defun prev2word-typ (word-typ)
; *** as prevword-typ, but back of 2 words
 (declare (special rev-iniz-frase))
 (extract-interp (take-2prev-word rev-iniz-frase) 'word-typ word-typ nil 'last))

; %%%%%%%%%%%% FUNCTIONS FOR CHECKING THE NEXT WORD %%%%%%%%%%%%%

;*********************************************************************
(defun nextcat (rule-cats)
  (extract-interp (manage-next-locut) 'cat rule-cats nil 'first))

;********************************************************************
; *** as nextcat and next2cat, but they also carry out agreement checks;
; >>> thiscat is the category of the current word that enables the 
;     selection of a particular interpretation of it, i.e. the
;     interpretation to be used in the agreement checks
;     "thiscat" is always an atom; rule-cats may be a complex
;     condition (see nextcat)
(defun nextcat-agr (rule-cats thiscat)
  (int-next-agr rule-cats thiscat 'next))

(defun next2cat-agr (rule-cats thiscat)
  (int-next-agr rule-cats thiscat '2next))

(defun int-next-agr (rule-cats thiscat key)
  (declare (special disword fine-frase locut-buff-ahead))
  (let* ((thisinterp (extract-interp disword 'cat thiscat t 'last))
         (thisnumber 
            (mapcar #' (lambda (x) (prendi (without-lemma x) 'number)) thisinterp))
         (thisgender 
            (mapcar #' (lambda (x) (prendi (without-lemma x) 'gender)) thisinterp))
         newcondit nextinterp)
  ; *** categs may be 'art or '(art adj ...) or '(art (type def) adj ...)
  ;     In the first case, we must produce '(art (and (number X) (gender Y))); 
  ;     In the second, 
  ;       '(art (and (number X) (gender Y)) adj (and (number X) (gender Y)))
  ;     In the third, 
  ;       '(art (and (type def) (number X) (gender Y)) adj (and (number X) (gender Y)))
      (setq newcondit
           (expand-condit (inlist rule-cats) 
                          `(and (number ,thisnumber) (gender ,thisgender))))
      (cond ((eq key 'next)
               (setq nextinterp
                    (extract-interp (manage-next-locut) 'cat newcondit nil 'first)))
            ((eq key '2next)
               (setq nextinterp
                    (extract-interp (take-2next-word fine-frase) 'cat newcondit nil 'first)))
            (t (exception 'morpho-error 
                    "PROC/postagger: Wrong key in checking agreement ahead" key)))
      (cond ((null nextinterp) nil)
            (t (formagree nextinterp thisinterp)))))

;*********************************************************************
(defun nextchar (chars)
  (extract-interp (manage-next-locut) 'word chars nil 'first))

;*********************************************************************
(defun nextgender (gend)
  (extract-interp (manage-next-locut) 'gender gend nil 'first))

;*********************************************************************
(defun nextperson (pers)
  (extract-interp (manage-next-locut) 'person pers nil 'first))

;*********************************************************************
(defun nextmood (moods)
  (extract-interp (manage-next-locut) 'mood moods nil 'first))

;*********************************************************************
(defun nextcase (cases)
  (extract-interp (manage-next-locut) 'case cases nil 'first))

;*********************************************************************
; *** as nextmood, but also requiring that the word is not a compound
(defun nextmood-simple (moods)
  (declare (special fine-frase))
  (let ((nw (manage-next-locut)))
     (cond ((or (null nw) (not (listp nw))) nil)
           (t (int-nxtmood-s moods nw (first fine-frase))))))

; *** loop on all interpretations of the word
; *** "word" includes only the first component of compound next words
;     "tot-word" includes all components
(defun int-nxtmood-s (moods word tot-word)
  (cond ((null word) nil)
        (t (let ((nxtinterp (first word))
                 (nxttotinterp (first tot-word)))
              (cond ((memb-or-eq (prendi nxtinterp 'mood) moods)
                       (cond ((atom (first nxttotinterp)) t)
                             (t nil)))
                    (t (int-nxtmood-s moods (rest word) (rest tot-word))))))))

;*********************************************************************
(defun nextnumber (numb)
  (extract-interp (manage-next-locut) 'number numb nil 'first))

;*********************************************************************
(defun nexttense (tenses)
  (extract-interp (manage-next-locut) 'tense tenses nil 'first))

;*********************************************************************
(defun nexttype (type)
  (extract-interp (manage-next-locut) 'type type nil 'first))

;*********************************************************************
; *** checks if the next word is of the grammatical class word-typ
(defun nextword-typ (word-typ)
  (extract-interp (manage-next-locut) 'word-typ word-typ nil 'first))

;*********************************************************************
(defun nextsemtype (cat semtype)
  (declare (special *LISP-CHAR-SET-ID*))
  (let* ((ninterp (extract-interp (manage-next-locut) 'cat cat nil 'first))
         (nword (first (prendi-first-comp ninterp))))
  ;   (cond ((eq 'UTF-8 *LISP-CHAR-SET-ID*)
  ;            (setq semtype (convert-base-atom-or-string-to-currlisp semtype))))
     (inh-member nword semtype)))
  ; *** inh-member in KB/language/semtypes-lang.lisp
  ;     where "language" is ITALIAN, ENGLISH, SPANISH or FRENCH

;*********************************************************************
; **** this is a first attempt to use ontological knowledge to carry out
;      pos tagging
(defun nextonttype (cat semtype)
  (declare (special fine-frase *WORD-MEANING* *LISP-CHAR-SET-ID* *CHAR-SET-ID*))
  (let* ((ninterp (extract-interp (take-next-word fine-frase) 'cat cat nil 'first))
   ; *** now, extract-interp should be a list of single components;
         (nwords (elimdup (mapcar #'first ninterp)))
   ; *** now, nwords is a list of lemmata
         (meanings 
            (sem-flatten 
                (dropnil 
                     (mapcar #'(lambda (x) (all-accent-leggi *WORD-MEANING* x)) nwords))))
         (found nil))
   ; *** in meanings all known meanings for these lemmata (see below the output of
   ;     sem-flatten)
     (cond ((neq *CHAR-SET-ID* *LISP-CHAR-SET-ID*)
              (setq semtype (convert-base-atom-or-string-to-currlisp semtype))))
      (do ((nxtmean (first meanings) (first meanings))
           (meanings (rest meanings) (rest meanings)))
          ((or (null nxtmean) found) found)
          (cond ((atom nxtmean)		; simple meaning, e.g. ££person
                   (setq found (is-subclass-of nxtmean semtype)))
                ((eq (first nxtmean) cat) 
                   ; ambiguity that can be disambiguated
                   ; on the basis of the category, e.g. (noun ££conc1)
                   (setq found (is-subclass-of (second nxtmean) semtype)))))))

;*********************************************************************
(defun manage-next-locut ()
; *** returns a first selection of the next interpretation, according
;     or not to a possible locutionary interpretation
  (declare (special fine-frase locut-buff-ahead))
  (let ((nxwrd (take-next-word fine-frase)))
     (cond ((eq locut-buff-ahead 'fail) (remove-locut nxwrd 'fail))
           (t (list (first locut-buff-ahead))))))

; %%%%%%%%%%%% FUNCTIONS FOR CHECKING THE SECOND NEXT WORD %%%%%%%%%%%%%

;********************************************************************
; *** as nextcat, but ahead of 2 words
(defun next2cat (categs)
 (declare (special fine-frase))
 (extract-interp (take-2next-word fine-frase) 'cat categs nil 'first))

;*********************************************************************
; *** as nextword-typ, but ahead of 2 words
(defun next2word-typ (word-typ)
 (declare (special fine-frase))
 (extract-interp (take-2next-word fine-frase) 'word-typ word-typ nil 'first))

;*********************************************************************
; *** as nextword-typ, but ahead after a locution
;     locution-length is the number of words composing the locution
(defun after-loc-typ (word-typ)
 (declare (special fine-frase locution-length))
 (extract-interp (first (nthcdr (1- locution-length) fine-frase))
                'word-typ word-typ nil 'first))

;*********************************************************************
(defun next2semtype (cat semtype)
  (declare (special fine-frase *LISP-CHAR-SET-ID*))
  (let* ((ninterp (extract-interp (take-2next-word fine-frase)
                                  'cat cat nil 'first))
         (nword (first (prendi-first-comp ninterp))))
   ;  (cond ((eq 'UTF-8 *LISP-CHAR-SET-ID*)
   ;           (setq semtype (convert-base-atom-or-string-to-currlisp semtype))))
     (inh-member nword semtype)))
  ; *** inh-member in KB/language/semtypes-lang.lisp
  ;     where "language" is ITALIAN, ENGLISH, SPANISH or FRENCH

;********************************************************************
; *** as nextmood, but ahead of 2 words
(defun next2mood (moods)
 (declare (special fine-frase))
 (extract-interp (take-2next-word fine-frase) 'mood moods nil 'first))

;********************************************************************
; *** as nexttense, but ahead of 2 words
(defun next2tense (tenses)
 (declare (special fine-frase))
 (extract-interp (take-2next-word fine-frase) 'tense tenses nil 'first))

;********************************************************************
; *** as nexttype, but ahead of 2 words
(defun next2type (types)
 (declare (special fine-frase))
 (extract-interp (take-2next-word fine-frase) 'type types nil 'first))

;********************************************************************
(defun next2char (chars)
; *** as nextchar, but ahead of two words
 (declare (special fine-frase))
 (extract-interp (take-2next-word fine-frase) 'word chars nil 'first))

;********************************************************************
(defun next2gender (gender)
; *** as nextgender, but ahead of two words
 (declare (special fine-frase))
 (extract-interp (take-2next-word fine-frase) 'gender gender nil 'first))

; %%%%%%%%%%%% FUNCTIONS FOR LOOKING OUTSIDE THE 2 + 2 WINDOW %%%%%%%%%%%%%

;********************************************************************
; *** looks ahead in the sentence if the first found verb is of verb-typ
;     Currently, verb-typ is always &predverb
; *** Not used in the rules (21/11/2010)
(defun nextverb (verb-typ)
  (declare (special fine-frase))
  (let (found (verbs (get verb-typ 'grtype)))
    (do ((nextword (first fine-frase) (first restfrase))
         (restfrase (rest fine-frase) (rest restfrase)))
; *** in 'found' a possible verbal interpretation of the next word
        ((or found (null nextword))
           (member (sld-prendi (first found) 'word) verbs))
	(setq found (extract-interp nextword 'cat 'verb nil 'first)))))

;********************************************************************
; *** looks ahead in the sentence if there is (not farther than maxdistance)
;     a word of the specified category and type; used for sia ... sia ...
;     sia ... che ... (to disambiguate the first "sia")
; *** the default for maxdistance is 5 
(defun aheadcat (categ type &optional maxdistance)
  (declare (special fine-frase))
  (let (found temp)
    (cond ((null maxdistance) (setq maxdistance 5)))
    (do ((nextword (first fine-frase) (first restfrase))
         (restfrase (rest fine-frase) (rest restfrase))
         (maxdistance (1- maxdistance) (1- maxdistance)))
; *** in 'found' a possible verbal interpretation of the next word
        ((or found (null nextword) (< maxdistance 0))
           found)
        (setq temp (extract-interp (remove-locut nextword nil) 'cat categ nil 'first))
        (cond ((not (null type)) 
                  (setq temp (extract-interp temp 'type type nil 'first))))
        (setq found (extract-interp temp 'word-typ '&both-2 nil 'first)))))

;********************************************************************
; *** This checks if the previous word (which must be a verb) may
;     govern the current word (usually adv-prep ambiguous)
(defun verb-govern-adv ()
    (declare (special disword rev-iniz-frase))
    (let ((prev (extract-interp 
                   (take-prev-word rev-iniz-frase) 'cat 'verb nil 'last))
          prevword currword)
       (cond ((null prev) nil)
             (t (setq prevword (sld-prendi (first prev) 'word))
                (setq currword (sld-prendi (first disword) 'word))
                (verb-gan-int (list prevword) currword)))))

;********************************************************************
; *** This checks if the next word can be governed by the current word 
;     (which should have a verbal interpretation)
(defun verb-govern-adv-next ()
    (declare (special disword fine-frase))
    (let ((nextw (extract-interp (manage-next-locut) 'cat 'adv nil 'first))
          nextword currwords)
       (cond ((null nextw) nil)
             (t (setq nextword (sld-prendi (first nextw) 'word))
                (setq currwords (mapcar #'(lambda (x) (sld-prendi x 'word)) disword))
                (verb-gan-int currwords nextword)))))

;********************************************************************
; *** this checks if any of the lemmata in currwords can govern the adv in nxtword
(defun verb-gan-int (currwords nxtword)
 (declare (special *VERB-PARTICLE*))
  (cond ((null currwords) nil)
        ((member nxtword (first (leggi *VERB-PARTICLE* (first currwords)))))
        (t (verb-gan-int (rest currwords) nxtword))))
        
;********************************************************************
; *** This checks if the current word (which must be a preposition) may
;     govern the next word
; *** checknil is t, in case the function must return true in case the
;     proposition can directly govern a NP, and in fact it is followed
;     by a possible NP beginner
(defun prep-govern-next (&optional checknil)
  (declare (special disword fine-frase))
  (let ((prep-gov (first (lex-dis-prendi (extract-interp disword 'cat 'prep t)
                                  'domin 'first)))
        (found nil))
  ; *** if checknil is t, enter the loop (on the single element "nil") in
  ;     order to check that what follows is governable by a prep
    (cond ((and (null prep-gov) checknil)
             (setq prep-gov '(nil))))
  ; *** count is required to determine the end of the loop, since prep-gov
  ;     may contain NIL as one of its elements
    (do ((count (length prep-gov) (1- count))
         (nextelem (first prep-gov) (first prep-gov))
         (prep-gov (rest prep-gov) (rest prep-gov)))
        ((or (= count 0) found)
          (cond (found t)
                (t nil)))
   ; *** if domin specifies NIL, this means that the preposition can
   ;     govern a noun, a pronoun, etc., without an intervening preposition
   ;     This situation is handled by other rules
        (cond ((null nextelem) 
                 (cond ((null checknil) nil)
                       (t (setq found 
                              (not (null (extract-interp 
                                         (manage-next-locut) 
                                         'cat '(predet art noun adj num pron)
                                          nil 'first)))))))
   ; *** 'empty' means that the preposition can stand alone (adverbial function)
   ;     Again, this case is handled elsewhere (adverbial entry in the
   ;     lexicon)
              ((eq nextelem 'empty) nil)
              ((member nextelem
		      (lex-dis-prendi-noloc 
                              (take-next-word fine-frase) 'word 'first))
                (setq found t))))))

;********************************************************************
; *** This checks if the current word (which must be a verb) may
;     govern the next word (which must be a prep)
(defun verb-govern-next-prep ()
  (declare (special disword fine-frase))
  (let* ((verb-interp (extract-interp disword 'cat 'verb t))
         (prep-interp (extract-interp (take-next-word fine-frase) 'cat 'prep nil 'first))
	 (prep-word (lex-dis-prendi-noloc prep-interp 'word 'first)))
     (cond ((listp prep-word)
  ; *** I assume there is a single non-locutionary preposition interpretation
             (cond ((all-equal prep-word)
                      (check-verb-govern-prep verb-interp (first prep-word)))
                   (t (break "verb-govern-next-prep"))))
           (t (check-verb-govern-prep verb-interp prep-word)))))

;********************************************************************
; *** this carries out the actual check, using the infos in *VERBCLASS-MARKED-CASES*
;     and the verbal subcategorization class of the verb
;     I assume that in case such a class is nil, the the verb is TRANS, INTRANS or 
;     REFL (as specified in the dictionary), so that it does not take marked cases
(defun check-verb-govern-prep (verb-interp prep)
 (declare (special *VERBCLASS-MARKED-CASES*))
   (let ((verb-lemma (first (first verb-interp)))
         verbclass (found nil))
    ; *** I think the next could happen for compound verbal forms (e.g. with clitics)
       (cond ((listp verb-lemma) (setq verb-lemma (first verb-lemma))))
       (setq verbclass (get verb-lemma 'verbal-class))
    ; *** Since at this stage, the actual class of the verb occurrence has not yet
    ;     established, the function returns true in case at least one of the classes
    ;     of the verb admits that preposition as the marked of a case
       (do ((nxtclass (first verbclass) (first verbclass))
            (verbclass (rest verbclass) (rest verbclass)))
           ((or (null nxtclass) found) found)
           (setq cldeps (first (leggi *VERBCLASS-MARKED-CASES* nxtclass)))
           (do ((nxtcdep (first cldeps) (first cldeps))
                (cldeps (rest cldeps) (rest cldeps)))
               ((or (null nxtcdep) found) found)
               (cond ((member prep (get (second nxtcdep) 'grtype)) (setq found t)))))))

;********************************************************************
; *** This checks if the current word (which must be a verb) may
;     govern the next word (which must be a prep)
(defun verb-govern-2next-prep ()
  (declare (special disword fine-frase))
  (let* ((verb-interp (extract-interp disword 'cat 'verb t))
         (prep-interp (extract-interp (take-2next-word fine-frase) 'cat 'prep nil 'first))
	 (prep-word (lex-dis-prendi-noloc prep-interp 'word 'first)))
     (cond ((listp prep-word)
             (cond ((all-equal prep-word)
                      (check-verb-govern-prep verb-interp (first prep-word)))
                   (t (break "verb-govern-next-prep"))))
           (t (check-verb-govern-prep verb-interp prep-word)))))

;********************************************************************
; *** looks backward in the sentence if the last preceding verb satisfies
;     "constraint". It is a pair <feature value>, where feature can be
;     "grammtype" (in which case "value" is an atom) or mood or tense
;     (in which case "value" can be an atom or a list)
(defun prevverb (constraint)
  (declare (special rev-iniz-frase))
  (let (found )
    (do ((prevword (first rev-iniz-frase) (first restfrase))
         (restfrase (rest rev-iniz-frase) (rest restfrase)))
; *** in 'found' a possible verbal interpretation of the next word
        ((or found (null prevword))
          (cond (found
                  (cond ((eq (first constraint) 'grammtype)
                           (member (sld-prendi (first found) 'word) 
                                   (get (second constraint) 'grtype)))
                        ((eq (first constraint) 'mood)
                           (memb-or-eq (sld-prendi (first found) 'mood) 
                                       (second constraint)))
                        ((eq (first constraint) 'tense)
                           (memb-or-eq (sld-prendi (first found) 'tense) 
                                       (second constraint)))))
                (t nil)))
	(setq found (extract-interp prevword 'cat 'verb nil 'first)))))

;*********************************************************************
(defun beforecat (categs &optional not-categs maxdistance)
; *** is there in the preceding part of the sentence (at a distance not
;     greater than maxdistance words) a word of the required category without
;     finding before a word satisfying not-categs?
; *** the default for maxdistance is 10
  (declare (special rev-iniz-frase))
  (cond ((null maxdistance) (setq maxdistance 10)))
  (do* ((i 0 (1+ i))
        (word (take-prev-word rev-iniz-frase) 
              (take-prev-word temp-fr))
        (temp-fr (rest rev-iniz-frase) (rest temp-fr))
        (ok-cats (extract-interp word 'cat categs nil 'last)
                 (extract-interp word 'cat categs nil 'last))
        (not-cats (cond ((null not-categs) nil)
                        (t (extract-interp word 'cat not-categs nil 'last)))
                  (cond ((null not-categs) nil)
                        (t (extract-interp word 'cat not-categs nil 'last)))))
 ; *** goes backward on the sentence until i=10 or a word of the given 
 ;     type is found
      ((or (= i maxdistance) ok-cats not-cats)
	(not (or (= i maxdistance) not-cats)))))

;*********************************************************************
(defun beforeword-typ (word-typ &optional maxdistance)
  (declare (special rev-iniz-frase))
; *** is there in the preceding part of the sentence (not farther than maxdistance
;     words) a word of the given grammatical type?
; *** the default for maxdistance is 10
  (let ((grammty (get word-typ 'grtype)))
   (cond ((null maxdistance) (setq maxdistance 10)))
   (do ((i 0 (1+ i))
	(prevword (take-prev-word rev-iniz-frase)
                  (take-prev-word temp-fr))
	(temp-fr (rest rev-iniz-frase) (rest temp-fr)))
 ; *** goes backward on the sentence until i=maxdistance or a word of the given 
 ;     type is found
       ((or (= i maxdistance)
            (and (listp prevword)
	         (member (sing-lex-dis-prendi (first prevword) 'word 'first)
                         grammty
                         :test #'eq-word)))
	 (not (= i maxdistance))))))

;*********************************************************************
(defun afterword-typ (word-typ &optional maxdistance)
  (declare (special fine-frase *GRAMMTYPES*))
; *** is there in the next part of the sentence (not farther than maxdistance
;     words) a word of the given grammatical type?
; *** the default for maxdistance is 10
  (let ((grammty (get word-typ 'grtype)))
   (cond ((null maxdistance) (setq maxdistance 10)))
   (do ((i 0 (1+ i))
	(nextword (first fine-frase) (first temp-fr))
	(temp-fr (rest fine-frase) (rest temp-fr)))
 ; *** goes forward on the sentence until i=maxdistance or a word of the given 
 ;     type is found
       ((or (= i maxdistance)
            (and (listp nextword)
	         (member (sing-lex-dis-prendi 
                              (first (remove-locut nextword 'fail)) 
                              'word 'first)
                         grammty
                         :test #'eq-word)))
	 (not (= i maxdistance))))))

;*********************************************************************
; *** management of the rule set; especially for the learning experimenta
;*********************************************************************
(defvar *all-classes* nil)
(defvar *all-rules* nil)
(defvar *all-cats* nil)
(defvar *all-defs* nil)

; *** creates the internal representation of a rule adding backward pointers
(defun putrule (class rules label &optional cats)
  (cond ((listp class)
          (dolist (single class)
                (putrule single rules label)))
        ((equal 'defaultc label)
           (putprop class rules label)
           (when (listp rules)
                  (setq *all-defs* (append rules *all-defs*))))
        (t (putprop class (append (get class 'lexdisrules) rules) label)
           (setq *all-classes* (cons class *all-classes*))
           (dolist (rule rules)
                (setq *all-rules* (cons rule *all-rules*))
                (add-prop-val rule 'class class))
                (setq *all-cats* (append cats *all-cats*))
                (dolist (cat cats)
                     (putprop cat (append (get cat 'rules) rules) 'rules)))))

; *** deletes all rules
(defun delete-rules ()
 (dolist (x *all-rules*) (remprop x 'class))
 (dolist (x *all-cats*) (remprop x 'rules))
 (dolist (x *all-classes*)
    (remprop x 'lexdisrules)
    (remprop x 'defaultc))
 (setq *all-classes* nil)
 (setq *all-defs* nil)
 (setq *all-rules* nil))

;************************************************************************
; *** OUTPUT FUNCTIONS 
;************************************************************************

;************************************************************************
; *** prints on file the result of the analysis; compound words are written
;     on separate lines
; *** INPUT:
;  >>> outport: the file .dis
;  >>> outport2: the file .tb
;  >>> outport3: the file .tball
;  >>> outport4: the file .sim
(defun printfrasenewtb 
	(outport outport2 outport3 outport4 frasein result rescriter file-lab sent-numb)
  (setq *print-pretty* nil)
  (setq *print-level* nil)
  (setq *print-length* nil)
  (let (all-infos)
      ; *** sentence header of the .tb file
    (format outport2 "************** Frase ~a-~a ************** ~%" 
          file-lab sent-numb)
  (do* ((inpword (car frasein) (car frasein))
        (frasein (cdr frasein) (cdr frasein))
        (count 1 (1+ count))
        (nxtword (car result) (car result))
        (resword (car nxtword) (car nxtword))
        (result (cdr result) (cdr result))
        (usedcrit (car rescriter) (car rescriter))
        (rescriter (cdr rescriter) (cdr rescriter)))
      ((null inpword)
          (format outport2 "~%")
          (setq *print-pretty* t)
          (setq *print-level* 5)
          (setq *print-length* 10)
          'ok)
; *** The next for proper names forced by lexdisamb
      (cond ((and (listp resword) (eq (first (first resword)) '$same))
              (setq nxtword 
                  (cons 
                     (cons (cons inpword (rest (first resword))) (cdr resword))
                     (rest nxtword)))))
; *** writing on XXX.sim
      (dolist (wprint (get-lemma resword inpword))
	    (format outport4 " ~a" wprint))
      (cond ((= (rem count 15) 0)
	      (format outport4 "~%")))
; *** writing on XXX.dis
       (print-on-dis outport inpword resword usedcrit)
; *** extract the information to print (from all interpretations, not just
;     from the one chosen by the tagger)
       (setq all-infos 
             (mapcar #'(lambda (x) (loc-atom-dropnil (get-info2 'file x))) nxtword))
; *** writing on XXX.tb
       (print-newtb-infos count inpword (first all-infos) outport2)
; *** writing on XXX.tball
       (format outport3 "~a ~a " count inpword)
       (print-infos (first all-infos) outport3)
       (format outport3 "      ~a" "$$$")
       (mapcar #'(lambda (x) (print-infos x outport3 )) (rest all-infos))
       (format outport3 " $$$~%"))))

;************************************************************************
(defun loc-atom-dropnil (l)
  (cond ((atom l) l)
        ((atom (first l))
          (dropnil l))
        (t (mapcar #'dropnil l))))

;************************************************************************
; *** it writes on file a word interpretation (used for output on tball)
;     it differs from the next (print-newtb-infos) since this keeps on the
;     same line all components of a compound word
; *** writing infos on separate lines is difficult for .tball, because of
;     possible misalignments between different interpretations (compound vs.
;     non-compound words)
(defun print-infos (infos outport)
  (cond ((listp infos)
; *** la cond usata per forzare la scrittura in formato carattere per i
;     segni di interpunzione (questo per facilitare la lettura successiva
;     del file)
; *** questa funzione opera con un trucco: se la parola è semplice, dentro la 
;     "cond" si atampa la parentesi aperta e la forma normalizzata e la
;     dolist successiva stampa le varie informazioni; se la parola è composta,
;     dentro la cond si stampa la prima componente e con la dolist le 
;     rimanenti. Ovviamente, il primo ramo della cond fallisce sempre per
;     parole composte. Tutto ciò non è bello!
           (cond ((or (eq 'punct (second infos))
		      (eq 'special (second infos))
		      (and (eq 'marker (second infos))
			   (= 1 (length (explode (first infos))))))
		   (format outport "~a~a~a~a" #\( #\# #\\ (first infos)))	; )
		 (t (format outport "~a~s" #\( (first infos))))			; )
     	   (dolist (el (rest infos))
	 	   (format outport " ~s" el))
	   (format outport "~a " #\)))						; (
; *** se non e' una lista e' una stringa (parola non trovata)
        (t (format outport "~s " infos))))

;************************************************************************
; *** it writes on file a word interpretation (used for output on *.tb)
;     it differs from the previous (print-infos) since this prints on
;     separate lines the various components of compound words
(defun print-newtb-infos (count inpword infos outpt2)
  (cond ((listp infos)
          (format outpt2 "~a ~a " count inpword)
          (cond ((atom (first infos))
   ; *** non-compound word
                  (cond ((or (eq 'punct (second infos))
                             (eq 'special (second infos))
                             (and (eq 'marker (second infos))
                                  (= 1 (length (explode (first infos))))))
                          (format outpt2 "~a~a~a~a" #\( #\# #\\ (first infos)))	; )
                        (t (format outpt2 "~a~s" #\( (first infos))))		; )
                          (dolist (el (rest infos))
                                (format outpt2 " ~s" el))
                          (format outpt2 "~a " #\))				; (
                          (format outpt2 "~%"))
   ; *** compound word
                (t (format outpt2 "~s" (first infos))
       	           (format outpt2 "~%")
                   (let ((compcount 1))
                       (dolist (comp (rest infos))
                             (format outpt2 "~a.~a ~a ~s"
                                     count compcount inpword comp)
                             (format outpt2 "~%")
                             (setq compcount (1+ compcount)))))))
   ; *** unknown word
        (t (format outpt2 "~a ~a ~s ~%" count inpword infos))))

;************************************************************************
; *** used to print the file including the normalized form of the word
(defun get-lemma (word inpword)
  (cond ((not (listp word)) (list inpword))
        ((eq (caar word) '$SAME) (list inpword))
        ((check-proper word nil) (list inpword))
        ((atom (caar word)) (list (put-downcase (caar word))))
        (t (mapcar #'put-downcase (mapcar #'get-word-comp (car word))))))

;************************************************************************
; *** 'word' is a component of a compound word. If it is non-ambiguous,
;     then its first is the lemma, otherwise the lemma is the first of
;     its first element (since this is a case very particular - enclitics -
;     the lemma of the ambiguous component is assumed to be the same)
(defun get-word-comp (word)
  (let ((posslemma (first word)))
       (cond ((atom posslemma) posslemma)
             (t (first posslemma)))))

;************************************************************************
(defun put-downcase (atom)
   (implode (mapcar #'char-downcase (explode atom))))

;************************************************************************
; *** extraction of the lexical information to print on the .dis file
(defun get-info (word)
  (cond ((not (listp word)) word)
        ((atom (first (first word))) (single-get-info (first word)))
        (t (mapcar #'single-get-info (rest word)))))

;************************************************************************
(defun single-get-info (inp-s-word)
  (let* ((s-word (without-lemma inp-s-word))
         (categ (prendi s-word 'cat))
         (root (first inp-s-word)))
; *** verbs
       (cond ((eq categ 'verb)
               (list root categ (prendi s-word 'mood) (prendi s-word 'tense)))
; *** nouns
             ((eq categ 'noun)
               (cond ((eq (prendi s-word 'proper) 'yes)
                       (list root categ 'proper
                            (prendi s-word 'gender) (prendi s-word 'number)))
                     (t (list root categ 'common
                            (prendi s-word 'gender) (prendi s-word 'number)))))
; *** adjectives, articles, and non-clitic pronouns
             ((or (eq categ 'adj)
                  (eq categ 'art)
                  (and (eq categ 'pron)
                       (neq (prendi s-word 'form) 'clitic)))
               (list root categ (prendi s-word 'gender) (prendi s-word 'number)))
; *** clitic pronouns
             ((and (eq categ 'pron)
                   (eq (prendi s-word 'form) 'clitic))
               (list root 'pron (prendi s-word 'type) 'clitic))
; *** other categories
             (t (list root categ)))))

;************************************************************************
; *** this function extracts information from the words, in order to:
;     1. write it on the .tb file if the "type" parameter is 'file
;     2. return it to the parser if the "type" parameter is 'sent
; **** output format:
;      VERBS: 
;        (forma-norm VERB type mood tense transitivity ?person ?number ?gender)
;        Where "person", "number" and "gender" are present just in some cases.
;        For instance, for infinites they are absent; for past participles
;        there are just "number" and "gender"; for finite tenses, there
;        are only "person" and "number"; when needed, the feature value
;        can be obtained based on the value itself. Person --> [1 2 3], 
;        Number --> [sing pl], Gender --> [m, f].
;      PRONOUNS:
;        (forma-norm PRON type ?gender ?number ?person ?possnumb ?posspers ?case)
;        Where "person", "number", "gender", "possnumb", "posspers" and "case"
;        are present just in some cases.
;	 Case, e' il caso superficiale (es lobj); in caso di ambiguita', si
;	    ha una lista di valori, reappresentati nell'output con un
;	    "+" come separatore; es (lobj liobj) --> lobj+liobj
;	 Valgono le considerazioni fatte sui verbi per prelevare i valori
;      NOMI:
;	 Nomi comuni:
;        (forma-norm category type gender number ?v-deriv ?v-trans)
;	 v-deriv e v-trans, se presenti, riportano informazioni sulla
;	 derivazione di nomi da verbi. 
;	 Es: caricamento: v-deriv=caricare, v-trans=trans
;	 Nomi propri:
;        (forma-norm category type ?gender ?semantic-type)
;	 gender e semantic-type sono ricavati dal lessico e sono assenti per
;	 nomi propri 'presunti' (cioe' non presenti nel lessico)
;      AGGETTIVI, ARTICOLI, PREDETERMINERS:
;        (forma-norm category type gender number)
;      CONGIUNZIONI SUBORDINATIVE:
;        (forma-norm category type semantic-type)
;	 dove semantic-type e' il valore della feature type di conjdip
;        presente nel lessico
;      ALTRI:
;        (forma-norm category type)
(defun get-info2 (type word)
  (cond ((not (listp word)) word)
        ((atom (first (first word))) (single-get-info2 type (first word)))
        (t (mapcar #'(lambda (x) (single-get-info2 type x)) (first word)))))

;************************************************************************
(defun single-get-info2 (out-type inp-s-word)
; *** The next cond just for the case 
;     "tentarvi: ((tentare ....) ((vi .. pers) (vi .. dimos))).
  (declare (special *DEFAULT-TYPES* *CHAR-SET-ID* *LISP-CHAR-SET-ID*))
  (cond ((not (atom (first inp-s-word)))
	      (setq inp-s-word (first inp-s-word))))
  (let* ((s-word (without-lemma inp-s-word))
         (categ (prendi s-word 'cat))
         (type (cond ((eq categ 'noun)
                       (cond ((eq (prendi s-word 'proper) 'yes) 'proper)))
                     (t (prendi s-word 'type))))
         (semtype 
            (let ((semv (prendi s-word 'semtype)))
                (cond ((null semv) nil)
                      ((or (listp semv)
                           (eq *CHAR-SET-ID* *LISP-CHAR-SET-ID*))
                          semv)
                      (t (implode
                            (convert-tule-char-names-to-numcodes
                               (convert-base-codes-to-tule-char-names
                                  (mapcar #'char-code
                                      (explode semv)))))))))
         (gender (prendi s-word 'gender))
         (number (prendi s-word 'number))
         (person (prendi s-word 'person))
         (grade (prendi s-word 'grade))
         (root (first inp-s-word))
         tempout syntempout semtempout)
; *** if no type in input, get the default type
     (cond ((null type)
              (setq type (first (leggi *DEFAULT-TYPES* categ)))))
; *** in tempout all standard info for the different categories
     (cond ((eq out-type 'file)
             (setq tempout
	         (cond ((eq categ 'verb)
                         (list root categ type
                              (prendi s-word 'mood)
                              (prendi s-word 'tense)
                              (getnewtrans (prendi s-word 'transitive))))
                       ((memq categ '(adj noun art predet))
                         (cond ((eq grade 'pos) (setq grade nil)))
              ; *** the previous one, because grade=pos is the default and must not be
              ;     included in the result
                         (dropnil 
                           (list root categ type gender number grade)))
                       ((eq categ 'pron)
                         (dropnil
                           (list root 'pron type gender number person
                                 (concatl (put-separator '+ (prendi s-word 'case)))
                                 grade)))
                       ((eq categ 'num)
		           (list root 'num (prendi s-word 'value)))
                       ((eq categ 'paragraph-n)
		           (list root 'paragraph-n (prendi s-word 'value)))
                       ((and (eq categ 'conj) (eq type 'subord))
                          (list root 'conj 'subord 
                                (cond ((null semtype) 'neutral)
                                      ((atom semtype) semtype)
                                      (t (concatl (put-separator '+ semtype))))))
                       ((and (eq categ 'conj) (eq type 'coord))
                          (list root 'conj 'coord 
                                (cond ((atom semtype) semtype)
                                      (t (concatl (put-separator '+ semtype))))))
	               ((and (eq categ 'prep) (eq type 'poli))
		                  (list root 'prep 'poli semtype))
	               ((eq categ 'date)
                          (list root 'date
                               (prendi s-word 'day)
                               (prendi s-word 'month)
                               (prendi s-word 'year)))
	               ((eq categ 'hour)
                          (let ((sec (prendi s-word 'seconds)))
                              (cond ((null sec)
                                       (list root 'hour
                                            (prendi s-word 'hours)
                                            (prendi s-word 'minutes)))
                                    (t (list root 'hour
                                            (prendi s-word 'hours)
                                            (prendi s-word 'minutes)
                                            sec)))))
                       (t (list root categ type))))
; *** optionals *****************
             (cond ((eq categ 'verb)
		 	 (setq tempout (append2 tempout person))
		 	 (setq tempout (append2 tempout number))
		 	 (setq tempout (append2 tempout gender)))
                   ((eq categ 'pron)
                     (cond ((eq (prendi s-word 'form) 'clitic)
                              (setq tempout (append1 tempout 'clitic)))
                           ((eq type 'poss)
                              (let ((ppers (prendi s-word 'posspers))
                                    (pnumb (prendi s-word 'possnumb))
                                    (pgend (prendi s-word 'possgend)))
                                 (cond ((not (null ppers))
                                          (setq tempout (append2 tempout (concatl (list 'p- ppers))))))
                                 (cond ((not (null pgend))
                                          (setq tempout (append2 tempout (concatl (list 'p- pgend))))))
                                 (cond ((not (null pnumb))
                                          (setq tempout (append2 tempout (concatl (list 'p- pnumb))))))))
                           ((eq type 'ordin)
                              (let ((pval (prendi s-word 'value)))
                                  (cond ((and (atom pval) 
                                              (not (numberp pval))
                                              (neq *LISP-CHAR-SET-ID* *CHAR-SET-ID*))
                                          (setq pval
                                            (implode
                                             (convert-tule-char-names-to-numcodes
                                                (convert-base-codes-to-tule-char-names
                                                   (mapcar #'char-code
                                                       (explode pval))))))))
                                  (setq tempout (append2 tempout pval))))))
                   ((eq categ 'adj)
                     (cond ((eq type 'poss)
                   ; *** possessive pronouns and adjectives have two referents, the owned thing
                   ;     and the owner. So, in "your book" the owned thing is the book, while
                   ;     the owner is you. For adjectives, it seems worth marking the infos
                   ;     concerning both of them, i.e.
                   ;     - gender of the owned thing (in Italian: "mi+o libro" - my book - is
                   ;       masculine, "mi+a casa" - my house - is feminine)
                   ;     - number of the owned thing (in Italian: "mi+o libro" - my book - is
                   ;       singular, "mi+ei libri" - my books - is plural)
                   ;     - gender of the owner (in Italian, it is not marked, but in English
                   ;       it is: "her book" if the owner is feminine, "his book" if it is
                   ;       masculine, "its book" if it is neuter)
                   ;     - person of the owner (in Italian: "mio libro" - my book - is first
                   ;       person, "tuo libro" - your book - is second person, "suo libro"
                   ;       her/his/its book - is third person)
                   ;     - number of the owner (in Italian: "mio libro" - my book - is
                   ;       singular, "nostro libro" - our book - is plural)
                   ;     Since gender, number and person are identified partly positionally
                   ;     and partly on the basis of the value, it seems useful to keep the
                   ;     values distinct, in order to evoid extra-ambiguity
                   ; *** consequently, we have:
                   ;     For the owned thing:
                   ;     - gender (m, f)
                   ;     - number (sing, pl)
                   ;     - person (1, 2 ,3)
                   ;     For the possessor:
                   ;	 - gender (p-m, p-f)
                   ;     - number (p-sing, p-pl)
                   ;     - person (p-1, p-2, p-3)
                   ;     For the possessor, in case the value is not marked, the feature is absent
                   ; *** Actually, the person of the owned thing is not included in the
                   ;     feature list, since it is fixed to 3. (As in "mine is better")
                              (let ((ppers (prendi s-word 'posspers))
                                    (pnumb (prendi s-word 'possnumb))
                                    (pgend (prendi s-word 'possgend)))
                                 (cond ((not (null ppers))
                                          (setq tempout (append2 tempout (concatl (list 'p- ppers))))))
                                 (cond ((not (null pgend))
                                          (setq tempout (append2 tempout (concatl (list 'p- pgend))))))
                                 (cond ((not (null pnumb))
                                          (setq tempout (append2 tempout (concatl (list 'p- pnumb))))))))
                           ((eq type 'ordin)
                              (let ((pval (prendi s-word 'value)))
                                  (cond ((and (atom pval) 
                                              (not (numberp pval))
                                              (neq *LISP-CHAR-SET-ID* *CHAR-SET-ID*))
                                          (setq pval
                                            (implode
                                             (convert-tule-char-names-to-numcodes
                                                (convert-base-codes-to-tule-char-names
                                                   (mapcar #'char-code
                                                       (explode pval))))))))
                                  (setq tempout (append2 tempout pval))))))
                   ((and (eq categ 'adv) (eq type 'interr))
                     (setq tempout (append2 tempout semtype)))
                   ((eq categ 'noun)
		 	         (setq tempout (append2 tempout semtype))
		 	         (setq tempout (append2 tempout (prendi s-word 'v-deriv)))
		 	         (setq tempout (append2 tempout
				               (getnewtrans (prendi s-word 'v-trans))))))
             (cond ((eq (prendi s-word 'locut) 'yes)
	                 (append1 tempout 'locution))
                  (t tempout)))
 ; *** the next concerns the parameters to be returned to the parser (instead of being
 ;     written on the .tb file)
           ((eq out-type 'sent)
              (setq syntempout
                  (cond ((eq categ 'verb)
                           `((lemma ,root) (cat verb) (type ,type)
                             (mood ,(prendi s-word 'mood))
                             (tense ,(prendi s-word 'tense))
                             (trans ,(getnewtrans (prendi s-word 'transitive)))))
                         ((memq categ '(adj noun art predet))
                           `((lemma ,root) (cat ,categ) (type ,type)
                             (gender ,gender) (number ,number)))
                         ((eq categ 'pron)
                           `((lemma ,root) (cat pron) (type ,type)
                             (gender ,gender) (number ,number) (person ,person)
                             (case ,(concatl
                                      (put-separator '+ (prendi s-word 'case))))))
                         ((memq categ '(date num hour))
                           `((lemma ,root) (cat ,categ)))
                         (t `((lemma ,root) (cat ,categ) (type ,type)))))
               (setq semtempout
                   (cond ((eq categ 'num)
                           `((value ,(prendi s-word 'value))))
                         ((eq categ 'paragraph-n)
                           `((value ,(prendi s-word 'value))))
                         ((and (eq categ 'conj) (eq type 'subord))
                           `((semtype ,(cond ((null semtype) 'neutral)
                                             ((atom semtype) semtype)
                                             (t (concatl
                                                     (put-separator '+ semtype)))))))
                         ((and (eq categ 'conj) (eq type 'coord))
                           `((semtype ,semtype)))
                         ((and (eq categ 'prep) (eq type 'poli))
                           `((semtype ,semtype)))
                         ((eq categ 'date)
                           `((day ,(prendi s-word 'day))
                             (month ,(prendi s-word 'month))
                             (year ,(prendi s-word 'year))))
                         ((eq categ 'hour)
                           (let ((hours (prendi s-word 'hours))
                                 (min (prendi s-word 'minutes))
                                 (sec (prendi s-word 'seconds)))
                              (cond ((null sec)
                                       `((hour ,hours) (minutes ,min)))
                                    (t `((hour ,hours) (minutes ,min) (seconds ,sec))))))))
; *** optionals *****************
               (cond ((eq categ 'verb)
                       (cond (person (setq syntempout
                                       (append1 syntempout `(person ,person)))))
                       (cond (number (setq syntempout
                                       (append1 syntempout `(number ,number)))))
                       (cond (gender (setq syntempout
                                       (append1 syntempout `(gender ,gender))))))
                     ((eq categ 'pron)
                        (cond ((eq (prendi s-word 'form) 'clitic)
                                 (setq syntempout (append1 syntempout '(clitic t))))
                              ((eq type 'ordin)
                                 (setq semtempout (append1 semtempout
                                            `(value ,(prendi s-word 'value)))))))
                               ((and (eq categ 'adj) (eq type 'ordin))
                                 (setq semtempout (append1 semtempout 
                                            `(value ,(prendi s-word 'value)))))
                     ((and (eq categ 'adv) (eq type 'interr))
                       (cond (semtype
                               (setq semtempout
                                     (append1 semtempout `(semtype ,semtype))))))
                     ((eq categ 'noun)
                       (cond (semtype
                               (setq semtempout
                                     (append1 semtempout `(semtype ,semtype)))))
                       (cond ((prendi s-word 'v-deriv)
                               (setq syntempout
                                    (append syntempout
                                        `((v-deriv ,(prendi s-word 'v-deriv))
                                          (v-trans ,(getnewtrans
                                                        (prendi s-word 'v-trans))))))))))
               (cond ((eq (prendi s-word 'locut) 'yes)
                        (list (append1 syntempout '(locution t)) semtempout))
                     (t (list syntempout semtempout)))))))
 
;************************************************************************
; *** changes the dictionary code for transitivity (yes, no, rifl) into the
;     output code (trans, intrans, refl)
(defun getnewtrans (oldtrans)
   (cond ((eq oldtrans 'yes) 'trans)
         ((eq oldtrans 'no) 'intrans)
         ((eq oldtrans 'rifl) 'refl)))

;************************************************************************
(defun extractfraseinfos (frasein result)
  (let (all-infos)
  (do* ((inpword (first frasein) (first frasein))
        (frasein (rest frasein) (rest frasein))
        (count 1 (1+ count))
        (nxtword (first result) (first result))
        (resword (first nxtword) (first nxtword))
        (result (rest result) (rest result)))
      ((null inpword) all-infos)
; *** The next for proper names forced by lexdisamb
      (cond ((and (listp resword) (eq (first (first resword)) '$same))
              (setq resword
                   (cons (cons inpword (rest (first resword))) (cdr resword)))))
; *** extract the information to put in the output
      (setq all-infos
          (append all-infos (build-word-infos count inpword resword))))))

;************************************************************************
(defun build-word-infos (count inpword resword)
   ; *** get-info2 extracts from a word the syntactic infos to be
   ;     included in the output
  (let* ((infos (get-info2 'sent resword))
         result tempres)
    (cond ((listp infos)
            (setq tempres `((posit ,count) (form ,inpword)))
            (cond ((atom (first (first (first infos))))
   ; *** non-compound word
                    (setq result
                       (list (append tempres
                               `((syn ,(first infos)) (sem ,(second infos)))))))
   ; *** compound word
                  (t (setq result
                       (list (append tempres
                               `((syn ,(first (first infos)))
                                 (sem ,(second (first infos)))))))
                    (let ((compcount 1))
                       (do ((syncomp (first (first (rest infos)))
                                     (first (first infos)))
                            (semcomp (second (first (rest infos)))
                                     (second (first infos)))
                            (infos (rest (rest infos)) (rest infos)))
                           ((null syncomp) result)
                           (setq result
                               (append1 result
                                  `((posit (,count ,compcount))
                                    (form ,inpword)
                                    (syn ,syncomp)
                                    (sem ,semcomp))))
                           (setq compcount (1+ compcount)))))))
   ; *** unknown word
        (t (setq result (list `((posit ,count)
                                (form ,inpword)
                                (syn ,infos))))))))

; ***************************************************************************
; *** the next is just to handle traces, where the word can be with or without
;     pipes
; *** in order to compare two words, they are put in a "standard" form,
;     with accent separated from chars, and uppercase
(defun eq-word (w1 w2)
  (equal (up-extern-accent w1) (up-extern-accent w2)))

; ***************************************************************************
(defun print-on-dis (port inpword resword usedcrit)
  (cond ((stringp resword)
 ; *** non-existent word
           (format port " ~a ~%        ~s ~%         ~a ~% "
               inpword resword usedcrit))
        ((atom (first (first resword)))
 ; *** non compound
           (format port " ~a ~%        ~s ~%         ~a ~% "
               inpword (atom-dropnil (get-info resword)) usedcrit))
 ; *** compound
        (t (do ((component (list (first (first resword))) 
                           (list (first (first resword))))
                (resword (list (rest (first resword)))
                         (list (rest (first resword)))))
               ((or (null component)
                    (equal component '(nil))))
                (format port " ~a ~%        ~s ~%         ~a ~% "
                    inpword (atom-dropnil (get-info component)) usedcrit)))))

; ***************************************************************************
; *** this returns true if the word includes an ambiguous subcomponent,
;     as in "I'm", which is resolved in I+am, and "am" is ambiguous
;     between a main and an aux
(defun has-ambiguous-components (word)
  (cond ((is-compound word)
      ; *** assumes it is not possible to have more than three components
           (let ((firstc (first word))
                 (secondc (second word))
                 (thirdc (third word)))
              (or (not (atom (first firstc)))
                  (not (atom (first secondc)))
                  (not (atom (first thirdc))))))
        (t nil)))

; ***************************************************************************
(defun get-from-ambig-compon (word feature val)
      ; *** works on the first interpretation of the word (assuming that it is
      ;     not globally ambiguous) and assumes it has no more than three components
   (let ((firstc (first (first word)))
         (secondc (second (first word)))
         (thirdc (third (first word))))
      (cond ((atom (first firstc))
       ; *** the first component is not ambiguous
              (cond ((atom (first secondc))
         ; *** the second component is not ambiguous
                       (cond ((atom (first thirdc))
           ; *** the third component is not ambiguous
           ;     this should not happen, but since we are in a patch, let it go
                                word)
           ; *** the third component is ambiguous
                             (t (list 
                                   (dropnil
                                     (list firstc secondc 
                                      (find-right-subcomponent thirdc feature val)))))))
         ; *** the second component is ambiguous
                    (t (list
                          (dropnil
                            (list firstc
                             (find-right-subcomponent secondc feature val)
                             thirdc))))))
       ; *** the first component is ambiguous
            (t (list
                  (dropnil 
                    (list 
                      (find-right-subcomponent firstc feature val) secondc thirdc)))))))

; ***************************************************************************
(defun find-right-subcomponent (component feature val)
 (let (result)
   (do ((firstsubc (first component) (first component))
        (component (rest component) (rest component)))
       ((or (null firstsubc) result)
          result)
       (cond ((or (eq (prendi firstsubc feature) val)
                  (and (eq val 'main)
                       (null (prendi firstsubc feature))))
                (setq result firstsubc))))))
                                         

