(in-package "USER")

;#################################################################
; ### IN THIS FILE. ALL BASIC FUNCTIONS FOR LEXICAL ACCESS.
;     THE TOP-LEVEL ONE IS analyser_elem
;    Sections:
;	THE TOP FUNCTION (analyser_elem)	[1 function, 117 lines]
;	CHOICE OF THE BEST INTERPRETATION	[18 functions, 360 lines]
;	ANALYSIS OF A SINGLE TOKENIZER ELEMENT  [10 functions, 122 lines]
;	GENERAL WORDS				[8 functions, 300 lines]
;	SPECIFIC CATEGORIES
;#################################################################

;*****************************************************************

(defvar *MONTHS* '(jan feb mar apr may jun jul aug sep oct nov dec))

;*****************************************************************
;	THE TOP FUNCTION (analyser)
;*****************************************************************

;******************************************************************
; >>> input:
;     scan_frase ---> (scan-el1 scan-el2 ... scan-elN)
;        a sentence is composed of various elements (items roughly
;        corresponding to words)
;     scan-el ---> (scan-interp1 scan-interp2 ... scan-interpM)
;        an item can have different tokenizer interpretations (it may be
;        both a proper name and a common noun)
;     scan-interp ---> (scan-comp1 scan-comp2 ... scan-compL)
;        an interpretation can include more than one component (e.g. 12.
;        which could be either a real number or an integer followed by a
;        period)
;     scan-comp ---> (list-of-chars . tokenizer-type)
(defun analyser (scan_frase)
  (declare (special *LANGUAGE*))
  (let (input newfrase subst-word (begphrase t) element lexic-result)
       (declare (special input begphrase scan-element))
; *** begphrase used in "ana_np" for the proper names
  (do ()
      ((or (null scan_frase) 
           (equal scan_frase '(nil)))
         (values (reverse lexic-result) (reverse newfrase)))
; *** analyser_elem carries out the actual work of lexical access on a single
;     tokenizer item
      (multiple-value-setq (element scan-element)
                           (analyser_elem (first scan_frase)))
; *** The split-elements function eliminates (if possible) the "scanner
;     ambiguity", by merging it with the standard lexical one.
; *** for instance, for "Barbara.", we get:
;     ((((((barbara cat adjc ...) (barbara cat noun ...))) 
;	 (((#\. cat punct)))))
;      (((((Barbara cat noun ...))) (((#\. cat punct))))))
;		--->
;     ((((((barbara cat adjc ...))) 
;	 (((barbara cat noun common ...))) 
;	 (((Barbara cat noun proper ...))))
;       ((((#\. cat punct))))))
; *** remember that, in the example above:
;     - the first level of parentheses identifies the whole output data ("element")
;     - the second level identifies the data associated with a single interpretation
;       of "tokenize" (so that, in the example, we have two elements, one associated
;       with "barbara GW" and one associated with "Barbara NOMEP"); this is the
;       level that should disappear after the application of "split-elements"
;     - the third level identifies the separation between single components of the
;       output of "tokenize" (e.g. the separation between "barbara" and ".")
;     - the fourth level identifies the single lexical intepretation (e.g.
;	"barbara adjc" vs. "barbara noun")
;     - the fifth level identifies possible components for compound words
;       (not present in the example; e.g. "prendi-le")
;     - the sixth level identifies possible ambiguous sub-components (e.g. -ci, in
;	por-ci)
      (setq element (split-elements element))
; *** after split-elements, the lexical output has one more level of parentheses
;     I think that the extra level has the goal of converting a single element
;     into a sentence (usually consisting in a single word)
      (setq scan-element (split-scan scan-element))
; *** after split-scan, the tokenizer output has one more level of parentheses
; *** the next checks the completeness of "element" (i.e. that the analysis
;     had a complete success). If not, it inserts the suitable information
;     or asks the user for explanations
; *** if "element" was already correct, no changes
      (setq subst-word (test-incompl element scan-element))
      (setq element (first subst-word))
      (setq scan-element (second subst-word))
   ;(format t "analyser_elem result: element: ~a~%; scan-element: ~a~%" 
   ;              element scan-element)
   ;(break "")
; *** the next cond takes care of plural genitives in English. 
;     In case the anlysis returned a pair of elements, the second of which is an
;     apostrophe, while the first is a plural noun, and noo othe apostrophe
;     occurs in the previous part of the sentence, then the PUNCT category of the
;     apostrophe is replaced by XGENITIVE
      (cond ((and (eq *LANGUAGE* 'english)
                  (eq 2 (length element))
                  (listp (first element))
                  (listp (second element)))
              (let ((first-comp (first (first (first (first (first (first element)))))))
                    (second-comp (first (first (first (first (first (second element))))))))
                  (cond ((and (eq #\' (first second-comp))
                              (eq 'noun (prendi (without-lemma first-comp) 'cat))
                              (eq 'pl (prendi (without-lemma first-comp) 'number))
                              (not (lemma-member #\' lexic-result)))
                          (setq element 
                             (list (first element) 
                                   '((((((XGENITIVE CAT ART TYPE GENITIVE)))))))))))))
      (setq lexic-result (append (reverse element) lexic-result))
      (setq newfrase (append (reverse scan-element) newfrase))
      (setq begphrase (is-sentence-init lexic-result))
;      (cond ((and (member (get-tule-char-name-from-numcode
;                             (car (caaaar (last scan-element))))
;                          (get-charset-value 's-termin))
;		  (not (and (null lexic-result)
;			    (is-numb (first element)))))
; *** The analyser function takes care of determining the split of sentences
;     Usually, a sentence is ended by a "terminator" (! . : ; ?), but in some
;     cases this must not happen.
;     A first case is related with Sentence numberings: in
;        "3. Chapter 3", this is just one sentence, so the period is not to
;        be taken as a terminator
;	          (setq begphrase t))
;      	    (t (setq begphrase nil)))
   ; *** the next few lines to handle cases as 'buco'. Here, we have that
   ;     the first apostroph is interpreted as a quote, and then the
   ;     sequence "buco'" is analyzed. However, this is the past form of the
   ;     verb 'bucare', so it would be interpreted in that way, and the 
   ;     close quotes would be lost. To avoid that, I set here an in-quotes
   ;     context variable, that makes the closed quote interpretation 
   ;     preferred (inside analyse_elem). The context is kept just for three
   ;     items!
   ;   (cond ((equal (caaaar scan-element) '(get-base-code-from-tule-char-name 'apostrophe)))
   ;           (cond ((equal in-quotes nil)
   ;                    (setq in-quotes t)
   ;                    (setq in-quotes-count 3))
   ;                 (t (setq in-quotes nil))))
   ;         ((equal (first (ult (first (caaaar element)))) #\')
   ;           (setq in-quotes nil))
   ;         (t (setq in-quotes-count (1- in-quotes-count))))
   ;   (cond ((equal in-quotes-count 0)
   ;            (setq in-quotes nil)))
   ; *** end of apostrophes treatment
      (setq scan_frase (rest scan_frase)))))

; **********************************************************************
(defun lemma-member (lemma lex-phrase)
   (cond ((null lex-phrase) nil)
         ((eq lemma (first (first (first (first (first (first (first lex-phrase)))))))) t)
         (t (lemma-member lemma (rest lex-phrase)))))
        
; **********************************************************************
; *** this tries to establish if we are at the beginning of a sentence
;     This happens if anal-prev is empty, or if it includes some kind of numbering
(defun is-sentence-init (anal-prev)
  (cond ((null anal-prev) t)
        ((eq (length anal-prev) 2)
           (cond ((or (not (listp (first anal-prev)))
                      (not (listp (second anal-prev))))
                   nil)
                 (t (let* ((first-lemma 
                              (first (first (first 
                                  (first (first (first (first anal-prev))))))))
                           (second-item 
                              (first (first (first (first (first (second anal-prev)))))))
                           (second-lemma (first second-item)))
                       (or
   ; *** the first case is [;(] "a) This is the first point" 
                        (and (eq first-lemma ;(
                                                #\))
                             (characterp second-lemma)
                             (memq (get-tule-char-name-from-lisp-char second-lemma)
                                   (append (get-charset-value 'minlet)
                                           (get-charset-value 'caplet))))
   ; *** the second case is "a continuation: unsplit sentence"
                        (and (characterp first-lemma)
                             (memq (get-tule-char-name-from-lisp-char first-lemma)
                                   (get-charset-value 's-termin)))
   ; *** the third case is "3. This is the third point"
                        (and (eq first-lemma #\.)
		             (eq 'num (prendi second-item 'cat))))))))
        (t nil)))

; --- The Input --------------------------------------------------

; *** 'analyser_elem' analyses a single element received from the Tokenizer.
;     In principle, each tokenizer element should have the following structure:
;	input ---> {tokenizer-interp1 ... tokenizer-interpn}
;	tokenizer-interp ---> [tokenizer-elem1 ... tokenizer-elemk]
;	tokenizer-elem ---> (<ascii-code list> tokenizer-category)
;     For instance, for "Barbara." we have:
;	   {[(<98 97 114 98 97 114 97> GW)
;	     (<46> SEGNOINTER)]
;	    [(<66 97 114 98 97 114 97> NOMEP)
;	     (<46> SEGNOINTER)]
;	    [(<66 97 114 98 97 114 97 46> SIGLA)]}
;     which includes three possible "tokenizer interpretations":
;     1 - A general word (GW) "barbara" followed by a period. Note that the first
;	  letter has been converted to lowercase (as it appears in the dictionary)
;     2 - A proper name (NOMEP) followed by a period. In this case the uppercase
;	  has been kept
;     3 - A code (SIGLA), which includes the period in a single element.
;     Of course in LISP all parentheses are the same, so the actual format is much
;     less readable:
;	   ((((98 97 114 98 97 114 97) GW)
;	     ((46) SEGNOINTER))
;	    (((66 97 114 98 97 114 97) NOMEP)
;	     ((46) SEGNOINTER))
;	    (((66 97 114 98 97 114 97 46) SIGLA)))
;     Unfortunately, the actual tokenizer output does not respect this structure:
;     if there is no scanner ambiguity, the curled bracket is missing:
;	    cane (dog) --> [(<99 97 110 101> GW)]
;     So this case is immediately checked, and the missing parenthesis is added
;     at the beginning of the function "analyser_elem".

; --- The Output -------------------------------------------------

;     The result of the analysis is much more complex than the input. It must
;     account both for the original scan ambiguities (which are not necessarily
;     solved by the access to the lexicon) and for newly introduced lexical
;     ambiguities. For instance "Barbara." is tokenizer-ambiguous, as noted above.
;     One of the possibilities (the SIGLA) is discarded, but the other two are kept
;     (it can be both a NOMEP and a GW). As a GW, however, it can be either a
;     common noun or an adjective. So, we have three lexical interpretations
;     coming from two different scan analyses. Of course, words can be compound
;     both from a scan ("Barbara.") and from a lexical point of view ("sulla" -->
;     "su+la" - on the). In order to have a unique correspondence between level
;     of parenthesis and information to carry, we need 6 (six) levels, according
;     to the following structure:
;	   element ---> {scan-res1 ... scan-resz1}
;	   scan-res ---> [scan-el1 ... scan-elz2]
;	   scan-el ---> <word-interp1 ... word-interpz3>
;	   word-interp ---> (lex-el1 ... lex-elz4)
;	   lex-el ---> /amb-el1 ... amb-elz5\
;	   amb-el ---> `dati-lessicali'
;     For instance, for "Barbara." we have:
;	   {[<(/`barbara noun ...'\) (/`barbara adjc ...'\)>
;	     <(/`. punto ...'\)>]
;	    [<(/`Barbara proper ...'\)>
;	     <(/`. punto ...'\)>]}
;     Of course the 6 levels do not seem useful for simple examples, but they have
;     to be kept:
;	   {[<(/`bambino noun ...'\)>]}
;     An example of the need for the penultimate level of parentheses (the
;     "slashed" ones) is given by some clitics (ex. "porci", which could be the
;     plural of "porco", i.e. "pigs", or "porre + ci", i.e. "put" + the clitic
;     "ci", which, in turn, can be ambiguous between a locative "put therein" and
;     a personal "put us" meaning).
;	   {[<(/`porre verb ...'\ /`ci1 ...' `ci2 ...'\)
;	      (/`porco noun ...'\)>]}
;     N.B. It could seem easier to repeat the verb "porre", so that the clitic
;          ambiguity is converted into a standard lexical ambiguity:
;          (porre+ci1 porre+ci2 porci)
;          However, the morphological analyser returns the two parts separately;
;          To change this would require to touch functions at a much lower level.

(defun analyser_elem (el)
; *** the next inserts a level of parenthesis in the input, in case it is not
;     ambiguous:
;     Asino: ((((DOWN-A DOWN-S DOWN-I DOWN-N DOWN-O) GW)) 
;             (((UP-A DOWN-S DOWN-I DOWN-N DOWN-O) NOMEP)))
;		---> Ambiguous, no change
;     asino: (((DOWN-A DOWN-S DOWN-I DOWN-N DOWN-O) GW)) 
;		---> Not ambiguous, a level of parentheses is missing
  (declare (special *LANGUAGE*))
  (let (result choice longest ok-el default-sigla code-el)
    (declare (special default-sigla))
; *** - the first 'first' gets a single tokenizer interpretation 
;       (e.g. GW+SEGNOINTER, or NOMEP)
;     - the second 'first' gets the first component (e.g. GW, or NOMEP)
;     - the third 'first' should get the character code list; if it returns an
;	atom, then a level of parenthesis is missing
    (cond ((atom (first (first (first el))))
            (setq el (list el))))
; *** the next substitutes all Tule char names with the base (ISO-8859-1) codes
;     this is needed for dictionary access
    (setq code-el (substitute-tule-char-names el))
    (setq result (mapcar #'anal-scan-el code-el))
 ;(format t "analyser-elem; result: ~a~%" result)
 ;(break "")
; *** 'anal-scan-el' is applied to all interpretations (mapcar)
;     An example of 'code-el' could be (input: "12.")
;          ((((49 50) NUMBER) ((46) SEGNOINTER))
;	    (((49 50 46) NUMBER)))
; 	   In this case, 'result' will be:
;	   ((((((|12| CAT NUM VALUE 12)))) 
;	     ((((#\. CAT PUNCT TYPE PUNTO)))))
;	    (((((|12.| CAT NUM VALUE 12))))))
; *** among the different results, 'choose-best-x' selects the best one, according
;     to the criteria described below. 'choice' has the form:
;	 (best-score best-analysis best-scan-alternative)
;     The second and the third components are lists
    (let ((siglint (get-sigla-interp code-el result))
          (ordseqint (get-ordseq-interp code-el result)))	; for "A-bis"
      ;  (format t "ordseq: ~a~% ordseqint" ordseqint)
      ;  (break "")
        (cond ((or (and (null siglint) (null ordseqint))
                   (and (eq *LANGUAGE* 'english)
                        (equal (list (get-base-code-from-tule-char-name 'up-i)) 
                               (first (first (first code-el))))))   ; I
                 (setq choice 
                    (choose-best-x (find-situat result '(0 0) code-el) result code-el nil nil)))
              ((not (null siglint))
                 (setq choice (cons 1 siglint)))
              ((not (null ordseqint))
                 (setq choice (cons 1 ordseqint)))))
; *** depending on the result, the globals "element" e "scan-element" are set
;     (they are the true result of the function)
; *** if the best score (first choice) is 0, the analysis failed
      ;  (format t "ordseq: ~a~% choice" choice)
      ;  (break "")
    (cond ((= 0 (first choice))
            (cond ((null default-sigla)
                    (values '((nil)) code-el))
                  (t (values (list (list (list (list default-sigla)))) code-el))))
; *** otherwise keep the best analysis, but associate them with the longest
;     scan interpretation (and with uppercase preferred)
          (t (setq longest
   		(choose-longest (rest (third choice)) (first (third choice))))
             (setq ok-el (select-same-length (length longest) code-el))
             (values (second choice)
                     (list (replace-chars
                                longest
	       	   		(select-chars (rest ok-el) 
					      (pick-chars (first ok-el))))))))))

;*****************************************************************
;     Asino: ((((DOWN-A DOWN-S DOWN-I DOWN-N DOWN-O) GW))
;             (((UP-A DOWN-S DOWN-I DOWN-N DOWN-O) NOMEP))) --->
;            ((((97 115 105 110 111) GW)) (((65 115 105 110 111) NOMEP)))
(defun substitute-tule-char-names (el)
  (cond ((null el) nil)
        (t (cons (sing-token-subst-tule-char-names (first el))
                 (substitute-tule-char-names (rest el))))))

;*****************************************************************
;     (((DOWN-A DOWN-S DOWN-I DOWN-N DOWN-O) GW)) -->
;     (((97 115 105 110 111) GW))
(defun sing-token-subst-tule-char-names (sing-tok-el)
  (cond ((null sing-tok-el) nil)
        (t (cons (sing-tok-comp-subst-tule-char-names (first sing-tok-el))
                 (sing-token-subst-tule-char-names (rest sing-tok-el))))))

;*****************************************************************
;     ((DOWN-A DOWN-S DOWN-I DOWN-N DOWN-O) GW) -->
;     ((97 115 105 110 111) GW)
(defun sing-tok-comp-subst-tule-char-names (sing-tok-comp)
   (cons (convert-tule-char-names-to-base-codes (first sing-tok-comp))
         (rest sing-tok-comp)))

;*****************************************************************
(defun select-same-length (leng el)
; *** el is a list of elements; among them, we must extract only those
;     having length leng
 (cond ((null el) nil)
       ((= (length (first el)) leng)
          (cons (first el) (select-same-length leng (rest el))))
       (t (select-same-length leng (rest el)))))

;*****************************************************************
;	CHOICE OF THE BEST INTERPRETATION
;*****************************************************************
; *** looks for a full 'sigla' interpretation
;     in case there is also a sequence char-period, it returns it as a 
;     possible proper name (initial, as R.)
(defun get-sigla-interp (el result)
 (let ((firstel (first el))
       (firstres (first result))
        tempres)
   (declare (special begphrase))
   (cond ((null el) nil)
         ((and (= 1 (length firstel))             ; it is not a compound
               (not (null firstres))              ; and it got an interpretation
               (not (equal firstres '(nil)))
               ; *** the next condition to avoid taking a piece of locution as a sigla
               ;     (they have to be returned as siglas anyway, since in case the
               ;     input has a strange form (as AlfaBeta), this is the only way to
               ;     get it)
               (not (equal (prendi 
                              (without-lemma (first (first (first (first firstres)))))
                              'locut)
                           'yes))
               ; *** the next condition for not taking 'I' at sentence beginning as
               ;     the roman number standing for 'first'. The same for an initial "Vi"
               (or (not begphrase)
                   (and (not (equal (first (first firstel)) 
                                    (list (get-base-code-from-tule-char-name 'up-i))))
                        (not (equal (first (first firstel)) 
                                    (list (get-base-code-from-tule-char-name 'up-v)
                                          (get-base-code-from-tule-char-name 'down-i))))))
               (or (eq 'sigla (ult (first firstel)))          ; it is a sigla
                   (and (eq 'nomep (ult (first firstel)))     ; it is the initial of a name
                        (eq 2 (length (first (first firstel))))
                        (eq (get-base-code-from-tule-char-name 'period) 
                            (second (first (first firstel)))))))
           (setq tempres (get-sigla-interp (rest el) (rest result)))
           (list (cons firstres (first tempres)) 
                 (cons firstel (second tempres))))
         (t (get-sigla-interp (rest el) (rest result))))))

;*****************************************************************
; *** this look for sequences as "a-bis", "C-ter"
;     el and result are (first-interpretation second-interpretation ...)
;     an interpretation is (first-item second-item ...), where more items mean
;        that the interpretation is composed of various parts (e.g. <A - bis>)
;     an item is (first-altern second-altern ...), where more altern mean
;        that the item is lexically ambiguous
;     an altern is (first-part second-part ...), where each part is the
;        component of the single lexical entry (ex. da+lle)
(defun get-ordseq-interp (el result)
 (let* ((firstel (first el))
        (firstres (first result))
        (firstitem (first firstres))
        (seconditem (second firstres))
        (thirditem (third firstres)))
  (cond ((null el) nil)
        ((and (eq 3 (length firstel))
              (eq 1 (length (first (first firstel))))
              (null firstitem)
              (eq #\- (first (first (first (first seconditem)))))
              (eq 'ADV (prendi (rest (first (first (first thirditem)))) 'cat))
              (eq 'ORDSEQ (prendi (rest (first (first (first thirditem)))) 'type)))
           (list 
             (list
              (list (list (list (list (list (code-char (first (first (first firstel)))) 'cat 'special))))
                    (second firstres)
                    (third firstres)))
              (list firstel)))
        (t (get-ordseq-interp (rest el) (rest result))))))

;*****************************************************************
; *** selects the longest tokenizer element
(defun choose-longest (scan-els curbest)
   (cond ((null scan-els) curbest)
	 ((< (length curbest) (length (first scan-els)))
	    (choose-longest (rest scan-els) (first scan-els)))
	 (t (choose-longest (rest scan-els) curbest))))

;*****************************************************************
; *** it builds a sequence of characters where uppercase is preferred to
;     lowercase (it takes them out from the scan elements)
(defun select-chars (scan-inp best)
   (cond ((null scan-inp) best)
         (t (select-chars (rest scan-inp)
                          (cmp-chars best (pick-chars (first scan-inp)))))))

;*****************************************************************
; *** this carries out the actual replacement, keeping in bestch the currently
;     best solution
(defun replace-chars (scan-el bestch)
  (let ((firstel (first scan-el)))
     (cond ((null bestch) nil)
           ((null scan-el) 
              (exception-nothrow "PROC/analizzatore:replace-chars"))
           (t (cons (cons 
			(first-n (length (first firstel)) bestch) ; charlist
		    	(second firstel))		    ; type (ex. NOMEP)
                    (replace-chars (rest scan-el)           ; other components
                                (nthcdr (length (first firstel)) bestch)))))))

;*****************************************************************
; *** compares two lists of chars (which are assumed to be of equal length)
;     choosing the uppercase
(defun cmp-chars (best new)
  (cond ((null best)
   	   (cond ((null new)  nil)
 		 (t (exception-nothrow "PROC/analizzatore: Num of chars in scan output: cmp-chars"))))
    ; *** the two first characters are the same or
    ;     the first character in best is a capital letter, use the first of best
	((or (= (first new) (first best))
	     (member (first best) (get-charset-value 'caplet)))
	   (cons (first best) (cmp-chars (rest best) (rest new))))
    ; *** they are not the same, and the first of new is capital, use it
 	((member (first new) (get-charset-value 'caplet))
	   (cons (first new) (cmp-chars (rest best) (rest new))))
    ; *** they are not the same, but none is a capital letter: this should not happen
	(t (exception-nothrow "PROC/analizzatore: Different chars; but not letters: cmp-chars")
	   (cons (first best) (cmp-chars (rest best) (rest new))))))
 
;*****************************************************************
; *** takes the characters from a tokenizer element (joining the components)
(defun pick-chars (scan-el)
   (cond ((null scan-el) nil)
 	 (t (append (first (first scan-el)) (pick-chars (rest scan-el))))))

;*****************************************************************
; *** this is given a list of possible interpretations (in 'wrd'), each of
;     which obtained by a different tokenizer result) and chooses the best.
; *** It also receives in input 'situat', previously computed by 'find-situat',
;     and encoding the overall status of 'wrd'; in other words, 'find-situat'
;     assesses the situation, while 'choose-best-x' implements the preference
;     policy
;     a - if there is a proper name (real or default) and a common noun and there
;         are no longer tokenizer interpretations: (proper common)
;	  (nome-proprio nome-comune)
;     b - if there are interpretations of different length:
;         b-1 - se ce ne sono di note al lessico (nome proprio non default o
;	        nome comune) usa tutte quelle note, indipendentemente
;	        dalla lunghezza
;	  b-2 - se la piu' breve e' un nome proprio default, e la piu' lunga
; 	        ha dei buchi, ma anche delle parti note dal lessico, scegli
;	        quella piu' lunga con delle parti note.
;	  b-3 - se, insieme ad una nota al lessico, c'e' anche un nome
;	        proprio default, usa anche quest'ultimo
; *** L'obiettivo e` quello di produrre, per cose come "Skropp."
;     l'analisi unknown+SEGNOINTER, con unknown nome proprio (classe 2)
;     piuttosto che quella completamente fallita di sigla o quella con
;     unknown GW. La preferenza per nome proprio ha la duplice funzione di:
;     a - Mantenere nell'output l'iniziale maiuscola
;     b - Permettere la stampa di "Forse nome proprio"
(defun choose-best-x (situat wrd scan best-w best-s)
  (let ((code (first situat)))
      (cond ((= code 0) '(0 nil nil))
   ; ** if no valid analysis, return (0 nil nil)
            ((null wrd)
   ; ** if 'wrd' is null, end of recursion
              (list (first situat) best-w best-s))
	    ((all-null (first wrd))
   ; ** if the current interpretation is a failure, go ahead
              (choose-best-x situat (rest wrd) (rest scan) best-w best-s))
   ; ** if one of the components was not analysed (nil), but the status code
   ;    1 or 5 (so that nothing better exists) take this interpretation if it
   ;    is of maximal length
            ((member nil (first wrd))
              (cond ((and (member code '(1 5))
                          (= (length (first wrd)) (second situat)))
                      (choose-best-x situat (rest wrd) (rest scan)
                                            (append1 best-w (first wrd))
                                            (append1 best-s (first scan))))
                    (t (choose-best-x situat (rest wrd) (rest scan)
                                            best-w best-s))))
   ; ** if this is an analysis of default proper name, then it is adopted if
   ;    the status is 3 or 7 or 8, and if the length is the maximal
            ((is-default-nomep? (first wrd))
              (cond ((and (member code '(3 7 8))
                          (= (length (first wrd)) (second situat)))
                      (choose-best-x situat (cdr wrd) (rest scan)
                                            (append1 best-w (first wrd))
                                            (append1 best-s (first scan))))
                    (t (choose-best-x situat (rest wrd) (rest scan)
					    best-w best-s))))
   ; ** a fully valid analysis is used anyway (but the code should be 2, 4,
   ;    6, 7, or 8, i.e. it should have been found before)
	    (t (cond ((member code '(2 4 6 7 8))
	                (cond ((and (or (has-a-apostr-corresp (first scan) best-w)
                                        (not (= (length (first wrd)) (second situat))))
    ; *** the second condition to handle "un'": it must be included in the result,
    ;     and in fact it is the best choice, even if its length is shorter than
    ;     the one determined in find-situat.
                                    (not (= 39 (ult (first (first (first scan)))))))
				   (choose-best-x situat (rest wrd) (rest scan)
						         best-w best-s))
			      	 (t (choose-best-x situat (rest wrd) (rest scan)
					      (append1 best-w (first wrd))
					      (append1 best-s (first scan))))))
                     (t (exception-nothrow "PROC/analizzatore: choose-best-x")
			(choose-best-x situat (rest wrd) (rest scan)
						         best-w best-s)
                             ))))))

; ******************************************************************************
; *** checks if the current scan interp has an apostroph as its last component
;     and if there is in the list of previous choices an interpretation of length
;     shorter of just one
(defun has-a-apostr-corresp (scan-interp prev-choices)
  (cond ((null prev-choices) nil)
	((not (last-apostr scan-interp)) nil)
	((= (length scan-interp) (1+ (length (first prev-choices)))) t)
	(t (has-a-apostr-corresp scan-interp (rest prev-choices)))))

; ******************************************************************************
; *** checks if the last component of the current interpretation consists in a
;     single apostrophe
(defun last-apostr (scan-interp)
   (equal (list (get-base-code-from-tule-char-name 'apostrophe))
	  (first    	; characters of the component
	     (ult scan-interp))))		; last component

;*****************************************************************
; *** it inspects the result of the morphological analyses of the different
;     tokenizer elements, and returns a code of the overall situation, (cursit)
;     and, in some cases, the length of one analysis (curl), according to:
;     cursit=0 --> no tokenizer element could be the input of a valid analysis
;     cursit=1 --> there is a partially valid analysis (i.e. some components of
;		   the tokenizer output were analyzed, others not) of length
;		   returned in 'curl'
;     cursit=2 --> there is at least one fully valid analysis
;     cursit=3 --> there is a fully valid analysis, but as a default proper
;				   name
;     cursit=4 --> a partially valid analysis of length 'curl' and a shorter
;                  fully valid analysis
;     cursit=5 --> a partially valid analysis of length 'curl' and a fully
;				   valid analysis but as a default proper name
;     cursit=6 --> 1. a partially valid analysis of length 'curl'
;                  2. a shorter fully valid analysis
;		   3. a fully valid analysis but as a default proper name
;     cursit=7 --> none of the longest analyses is even partially valid, but
;                  there is a shorter fully valid analysis and a fully valid
;				   analysis as a default proper name
;     cursit=8 --> there is a fully valid analysis as a non-default proper name
;                  of length 1; This is certainly the best solution
(defun find-situat (wrd cursit scanwrd)
  (cond ((null wrd) cursit)
; *** analysis failed: go ahead *****************************************
        ((all-null (first wrd))
          (find-situat (rest wrd) cursit (rest scanwrd)))
; *** partially valid analysis (nil says that one of the components could
;     not be analysed) **************************************************
        ((member nil (first wrd))
  ; ** if the previous status was 0, update it to 1
          (cond ((= (first cursit) 0)
                  (find-situat (rest wrd) (list 1 (length (first wrd))) (rest scanwrd)))
  ; ** on the contrary, if a better solution was found before, go ahead
                ((member (first cursit) '(2 4 6 7))
                  (find-situat (rest wrd) cursit (rest scanwrd)))
  ; ** if a partial solution was already found, leave the status unchanged, 
  ;    but possibly update the maximal length
                ((member (first cursit) '(1 5))
                  (find-situat (rest wrd)
                           (list (first cursit)
                                 (max (length (first wrd)) (second cursit))) (rest scanwrd)))
  ; ** if a default proper name was found before, pass to status 5
                ((= (first cursit) 3)
                  (find-situat (rest wrd) (list 5 (length (first wrd))) (rest scanwrd)))))
; *** valid analysis, but of a default proper name **********************
        ((is-default-nomep? (first wrd))
          (cond ((= (first cursit) 0)
  ; ** if no previous analysis found, go to status 3
                  (find-situat (rest wrd) (list 3 (length (first wrd))) (rest scanwrd)))
  ; ** a partial analysis was found before: go to status 5
                ((= (first cursit) 1)
                  (find-situat (rest wrd)
                            (list 5 (max (length (first wrd)) (second cursit))) (rest scanwrd)))
  ; ** if the previous status was better, no change
                ((member (first cursit) '(2 4 6 7))
                  (find-situat (rest wrd) cursit (rest scanwrd)))
  ; ** if the previous status was equivalent, possibly change the length
                ((member (first cursit) '(3 5))
                  (find-situat (rest wrd)
                            (list (first cursit)
                                  (max (length (first wrd)) (second cursit))) (rest scanwrd)))))
; *** standard fully valid analysis *************************************
        (t (cond ((and (= 1 (length (first wrd)))
                       (eq 'yes (prendi (first (caaaar wrd)) 'proper))
                       (null (prendi (first (caaaar wrd)) 'default)))
  ; ** a non default proper name of length 1 blocks the search
                   '(8 1))
                 ((and (= 1 (length (first wrd)))
                       (eq 'gw (second (first (first scanwrd)))))
  ; ** a general word reading of length 1 stops the search (it is a full 
  ;    interpretation found in the dictionary, as "pro-drop")
                   '(8 1))
                 ((= (first cursit) 0)
  ; ** if no previous analysis found, go to status 2
                   (find-situat (rest wrd) (list 2 (length (first wrd))) (rest scanwrd)))
  ; ** a partial analysis was already found: go to status 4
                 ((= (first cursit) 1)
                   (find-situat (rest wrd) (list 4 (length (first wrd))) (rest scanwrd)))
  ; ** a full solution was already found, possibly change the length
                 ((member (first cursit) '(2 4 6 7))
                   (cond ((include-locut (first wrd) nil 1)
  ; ** in case the interpretation includes dishomogeneous pieces o locutions,
  ;    leave the situation as before
                            (find-situat (rest wrd) cursit (rest scanwrd)))
                         (t (find-situat (rest wrd)
                               (list (first cursit)
                                     (max (length (first wrd)) (second cursit))) (rest scanwrd)))))
  ; ** a default proper name was found before: go to status 7
                 ((= (first cursit) 3)
                   (find-situat (rest wrd) (list 7 (length (first wrd))) (rest scanwrd)))
  ; ** there already were both a proper name and a partial analysis: go to
  ;    status 6
                 ((= (first cursit) 5)
                   (find-situat (rest wrd) (list 6 (length (first wrd))) (rest scanwrd)))))))

;*****************************************************************
; *** returns true in case the interpretation includes pieces of different
;     idioms, or pieces separated by non-idioms
; *** state is:
;     1: beginning of interpretation
;     2: no idioms found
;     3: idiom found (locution name in prevloc)
;     4: idiom followed by non-idioms
; *** N.B. A sequence <idiom non-idiom> is acceptable (include-locut=nil)
;          since it is used for prep+art, if the prep is part of a idiom (locution)
(defun include-locut (wrdint prevloc state)
  (cond ((null wrdint) nil)
        (t (let ((locint (get-locut-interp (first wrdint) 0 nil)))
               (cond ((= 1 (first locint))
   ; *** only non-locution interpretation
                       (cond ((= 1 state)
                               (include-locut (rest wrdint) nil 2))
                             ((= 2 state)
                               (include-locut (rest wrdint) nil 2))
                             ((= 3 state)
                               (include-locut (rest wrdint) nil 4))
                             ((= 4 state)
                               (include-locut (rest wrdint) nil 4))))
                     ((= 2 (first locint))
   ; *** only locution interpretation
                       (cond ((= 1 state)
                               (include-locut (rest wrdint) (second locint) 3))
                             ((= 2 state) t)
       ; *** a non-locution cannot be followed by an only locution
                             ((= 3 state)
                                (let ((remloc 
                                       (intersection prevloc (second locint))))
                                    (include-locut (rest wrdint) remloc
                                                   (cond (remloc 3) (t 4)))))
                             ((= 4 state) t)))
       ; *** after a locution and a non-locution there cannot be a new locution
                     ((= 3 (first locint))
   ; *** both standard and locution interpretation
                       (cond ((= 1 state)
                               (include-locut (rest wrdint) (second locint) 3))
                             ((= 2 state) 
                               (include-locut (rest wrdint) nil 2))
                             ((= 3 state) 
                                (let ((remloc 
                                       (intersection prevloc (second locint))))
                                    (include-locut (rest wrdint) remloc
                                                   (cond (remloc 3) (t 4)))))
                             ((= 4 state) t))))))))

; *** this examines a component, and returns
;     <1 nil> if it is just a non-locutionary component
;     <2 'list of locution names'> if it is only locutionary
;     <3 'list of locution names'> if it is both standard and locutionary
(defun get-locut-interp (wrdel singstat res)
  ; *** wrdel is a four-level list:
  ;     wrdel --> [comp-interp1 comp-interp2 ...]
  ;     comp-interp --> <scan-el1 scan-el2 ...>
   (cond ((null wrdel) (list singstat res))
         (t (let ((wrdint (first wrdel)))
             (cond ((eq 'yes (prendi (without-lemma (first (first wrdint))) 'locut))
  ; *** it is a locution interpretation
                     (cond ((= singstat 0)
   ; *** singstat 0 = beginning
                             (get-locut-interp (rest wrdel) 2 
                                          (list (first (first (first wrdint))))))
                           ((= singstat 1)
                             (get-locut-interp (rest wrdel) 3 
                                          (list (first (first (first wrdint))))))
                           ((= singstat 2)
                             (get-locut-interp (rest wrdel) 2 
                                         (cons (first (first (first wrdint))) res)))
                           ((= singstat 3)
                             (get-locut-interp (rest wrdel) 3 
                                      (cons (first (first (first wrdint))) res)))))
  ; *** it is not  a locution interpretation
                   (t (cond ((= singstat 0)
   ; *** singstat 0 = beginning
                              (get-locut-interp (rest wrdel) 1 nil))
                            ((= singstat 1)
                              (get-locut-interp (rest wrdel) 1 nil))
                            ((= singstat 2)
                               (get-locut-interp (rest wrdel) 3 res))
                            ((= singstat 3)
                               (get-locut-interp (rest wrdel) 3 res)))))))))

;*****************************************************************
; *** returns true if the interpretation concerns a default proper name (it
;     is a default, because the gender and number features are unassigned)
(defun is-default-nomep? (wrd-i)
  (let ((elem (without-lemma (first (first (first (first wrd-i)))))))
    ; *** "rest" in order to avoid considering the input word as feature
    ;     name, when it is "proper", "gender", or "number"
      (and (= 1 (length (first wrd-i)))
	   (eq (prendi elem 'proper) 'yes)
	   (null (prendi elem 'gender))
	   (null (prendi elem 'number)))))

;*****************************************************************
; *** for each component of a tokenizer output, ex. [(Leonardo ..) (#\. ..)]
;     carry out the lexical analysis
;   N.B. The output could include failures:
;	 	 [(skrupp ..) (#\. ..)] --> [nil (. segnointer)]
;	 	 [(skrupp ..)] --> [nil]
; *** scan-el includes the characters of the input form, expressed in base (ISO-8859-1)
;     numeric codes. For instance, the number "60" is (((54 48) NUMBER))
(defun anal-scan-el (scan-el)
    (mapcar #'adjust-parent (mapcar #'analyser-single-elem scan-el)))
 
;*****************************************************************
; *** adjusts the parentheses according to the schema described above (at the
;     beginning of this file)
(defun adjust-parent (lex-outp)  
  (cond ((stringp lex-outp)
	   (list (list (list lex-outp))))
        (t (mapcar #'single-adj-p lex-outp))))
   
;*****************************************************************
; *** adjusts the parentheses for a single component of the output
(defun single-adj-p (data) 
  (cond ((null (first data)) nil)
	((atom (first data))  
  ; ** if the first element is an atom, two levels of parentheses are missing
          (list (list data))) 
        (t (mapcar #'low-adj-p data))))           
         
;*****************************************************************
; ** if the first element is an atom, a single level of parentheses is missing
(defun low-adj-p (data)         
  (cond ((atom (first data)) (list data))
        (t data)))

;*****************************************************************
; *** split-elements handles cases where the tokenizer analysis presents
;     results of different length. This may happen, even after the choice
;     in 'choose-best-x' if all analyses are fully correct. In particular,
;     it may happen in case of:
;   1. Numbers with final dot (ex. 12.)
;      (
;	     (  ((((|12| CAT NUMB VALUE 12))))
;	        ((((#\. CAT SEGNOINTER TYPE PUNTO))))  )
;	     (  ((((|12.| CAT NUMB VALUE 12))))	  )
;	   )
;   2. Dashes (ex. Bat-film)
;      (
;		 (  ((((|Bat-film| CAT NOUN PROPER YES))))   )
;        (  ((((|Bat| CAT NOUN PROPER YES))))
;	        ((((#\- CAT SEGNOINTER TYPE TRATTINO))))
;	        ((((|film| CAT NOUN PROPER YES))))       )
;        (  ((((|Bat| CAT NOUN PROPER YES))))
;	        ((((#\- CAT SEGNOINTER TYPE TRATTINO))))
;	        ((((FILM CAT NOUN CLASSE (0) GENDER M NUMBER ALLVAL))))  )
;      )
; *** The problem is not trivial, since in the following procedures, it is
;     assumed that there is alignment between input words (i.e. tokenizer
;     components) and lexical interpretations
; *** The only solution I found was to replicate the shorter entries (marking
;     them as multiple elements). It is not clear to me how to use them in
;     the tagger and the parser
;     In the second example above, the first entry becomes:
;      (  (((((|Bat-film| CAT NOUN PROPER YES multiple yes)))))
;         (((((|Bat-film| CAT NOUN PROPER YES multiple yes)))))
;         (((((|Bat-film| CAT NOUN PROPER YES multiple yes)))))  )
; *** The following 4 cases are handled:
;     1 - It is not a tokenizer-ambiguous element, neither it is compound
;         ex. "asino", "pesca", but also "Leonardo" (since the possible
;             tokenizer interpretation "leonardo GW" is not present in the
;	          lexicon)
;	      In this case, we must just add a further level of parenthesis
;		  (compound consisting in a single component)
;	      N.B. Here, as in the other cases 7 (seven) levels of parentheses
;              are required
;     2 - It is not a tokenizer-ambiguous element, but it is compound
;	      ex. "asino." (which includes a GW and a SEGNOINTER)
;		  In this case, we must add to all components of the first (first and
;		  unique interpretation) two levels of parentheses, in order to convert
;		  the two components into two separate words
;		  ex. (((x) (y))) --> ((((x))) (((y))))
;     3 - It is tokenizer-ambiguous, but not compound
;	      ex. "Barbara" (which is ambiguous - even in the lexicon - between
;             GW e NOMEP)
;	      N.B. If the first is not compound, also the others are not, since we
;    	       have checked before that they be of equal length
;	      In this case, we must 'put together' all the first (and unique)
;         elements of all interpretations:
;		  (((x)) ((y))) --> ((((x y))))
;     4 - it is tokenizer-ambiguous, and it is also a compound
;	      ex. "Barbara." (which is ambiguous between GW+SEGNOINTER and
;         NOMEP+SEGNOINTER)
;	      In this case, we must 'put together' all the i-th elements of the
;		  various interpretations:
;		  (((x) (a)) ((y) (b))) --> ((((x y))) (((a b))))
;         N.B. Both here and in the previous case, identical elements are not
;  	           repeated; ex:
;		    (((barbara1 barbara2) (period)) ((Barbara3) (period))) -->
;					((((barbara1 barbara2 Barbara3))) (((period))))
(defun split-elements (input-w)
  (let ((wrd-input input-w))
; *** 'all-same-length' in *HOME-DIR*/PROC/utilities
  	 (cond ((not (all-same-length wrd-input))
; *** 'max-length' in *HOME-DIR*/PROC/utilities
  		 (setq wrd-input (extend-subl wrd-input (max-length wrd-input)))
	  	 ;(format t "Elem of uneven length in analizzatore: split-elements")
                 ))
         (cond ((= 1 (length wrd-input))
	         (cond ((= 1 (length (first wrd-input)))
	                  (list wrd-input))		            ; CASE 1
		       (t (mapcar #'list (mapcar #'list (first wrd-input))))))
								    ; CASE 2
; *** mult-union and mult-mult-union in *HOME-DIR*/PROC/utilities
	       (t (cond ((= 1 (length (first wrd-input)))
	                  (list (list (list (mult-union 
						(mapcar #'first wrd-input))))))
								    ; CASE 3
		 	(t (mapcar #'list 
			     (mapcar #'list (mult-mult-union wrd-input)))))))))
				                                    ; CASE 4

;*****************************************************************
(defun split-scan (scan-el)
; *** as above, but on the tokenizer output
; *** a level of parentheses is added, because the output must be a sequence
;     of unambiguous tokenizer elements
  (cond ((= 1 (length scan-el))
	  (cond ((= 1 (length (first scan-el)))
                    (list scan-el))		                      ; CASE 1
		(t (mapcar #'list (mapcar #'list (first scan-el)))))) ; CASE 2
	(t (mapcar #'list scan-el))))			 	      ; CASES 3 e 4

;*****************************************************************
; *** checks if all sublists of a list have the same length
(defun all-same-length (ll)
   (a-s-l ll (length (car ll))))

(defun a-s-l (ll n)
   (cond ((null ll) t)
	 ((= n (length (first ll))) (a-s-l (rest ll) n))
	 (t nil)))

;*****************************************************************
; *** determines the maximum length of the sublists of a list
(defun max-length (ll)
   (m-le ll (length (car ll))))

(defun m-le (ll n)
   (cond ((null ll) n)
	 ((< n (length (first ll))) (m-le (rest ll) (length (first ll))))
	 (t (m-le (rest ll) n))))

;******************************************************************
; *** Makes all sublists of 'wordl' of the same length (which is the
;     maximum among the existing sublists (input 'leng')). In order to
;     do so, replicates the final element of the shorter sublists
(defun extend-subl (wordl leng)
  (cond ((null wordl) nil)
	 	((= leng (length (first wordl)))
	      (cons (first wordl) (extend-subl (rest wordl) leng)))
	    (t (cons (extend-elem (first wordl) leng (first (first wordl)))
		         (extend-subl (rest wordl) leng)))))

;******************************************************************
; *** extends a given list until it reaches length 'leng', by adding
;     at the end copies of the element 'replic'
(defun extend-elem (singw leng replic)
  (cond ((= 0 leng) nil)
        ((null singw)
  ; ** at the end of 'singw', add copies of 'replic'
          (cons (list (list (list (append (first (first (first replic)))
					  '(multiple yes)))))
	 	(extend-elem nil (1- leng) replic)))
  ; ** otherwise, copy the existing element, but add to it 'multiple yes'
        (t (cons (list (list (list (append 
				     (first (first (first (first singw))))
			            '(multiple yes)))))
		 (extend-elem (rest singw) (1- leng) (first singw))))))

;*****************************************************************
;	ANALYSIS OF A SINGLE TOKENIZER ELEMENT
;*****************************************************************
; *** It takes as input a single element coming from the output of the 
;     tokenizer and makes suitable checks; possibly, it accesses the
;     dictionary
;       Ex input: for "asino": ((97 115 105 110 111) GW)
;          and then "imploded"
;    	N.B. Possible tokenizer ambiguities or compound elements are 
;            handled at an higher level
(defun analyser-single-elem (elem)
  (declare (special *SIGLE-DICT* *SYSTEM-CONTEXT* default-sigla))
  (let ((input (implode (first elem)))
         alloptions alloptions2 up-extern prepart knownsigla tempgw)
     (case (second elem)
	 (gw 
  ; *** GENERAL WORDS ************************
  ;     result formed from:
  ;     1. invariables, 
  ;     2. fixed forms prepositions+articles
  ;     3. locutions, 
  ;     4. locutionary proper names,
  ;        proper names must be handled to account for compounds not
  ;        capitalized (taken as general words by the tokenizer)
  ;     5. standard access to the dictionary
  ;     6. access to the dictionary of abbreviations
  ;        *SIGLE-DICT*, because in some cases, siglas cannot be recognized by the
  ;        tokenizer, i.e. in case no special char (as a dot) is present
  ;        (ex. "ddl", "cfr", etc.)
        ; *** a special case is handled via the cond (not (null prepart))
        ;     It is for locutions ending in a preposition: in case the word is a 
        ;     prepart, then its first element (the preposition) is looked for in
        ;     the property "locutdef"; if it can be part of a locution, then all
        ;     possible 'locution' intepretations are  associated with the article.
        ;     N.B. It is assumed that in preparts, if there is an ambiguity, it is
        ;	   just in the article (case of "dell'"), but the preposition is 
        ;          not ambiguous
        ;     Example: prepart could be:
        ;          (((DA CAT PREP TYPE MONO)
        ;	     (IL CAT ART ARTYPE DEF GENDER M NUMBER SING))
        ;           ((DA CAT PREP TYPE MONO)
        ;	     (IL CAT ART ARTYPE DEF GENDER F NUMBER SING)))
        ;         so, the first is 
        ;           ((DA CAT PREP TYPE MONO)
        ;	     (IL CAT ART ARTYPE DEF GENDER M NUMBER SING))
        ;         so, the first of the first is 
        ;           (DA CAT PREP TYPE MONO)
        ;         and the first of the first of the first is DA
            (multiple-value-setq
                 (alloptions up-extern) (final-accent-and-uppercase input))
            (setq prepart (all-accent-get alloptions 'prepartdef)) ; *** prepositions+articles
            (setq tempgw
	        (append prepart				  
                    (all-accent-get alloptions 'invardef)    ; *** invariables
	            (cond ((not (null prepart))         ; *** see comments above
                             (multiple-value-setq
                                 (alloptions2 up-extern)
                                 (final-accent-and-uppercase (first (first (first prepart)))))
		             (let ((locutprep
		                      (get-fixed-locut
		                        (all-accent-get alloptions2 'locutdef))))
				  (cond ((not (null locutprep))
				           (cartes locutprep
				               (mapcar #'second prepart)))))))
                    (get-fixed-locut (all-accent-get alloptions 'locutdef))  ; *** multi-words
                    (get-proper-interp elem alloptions)
              ; *** in some contexts, lower case proper names are not accepted. But also in these contexts
              ;     portions of proper name locutions (e.g. Ludwig van Beethoven) must be accepted
                    (all-accent-get alloptions *SIGLE-DICT*)))   ; *** abbreviations
            (setq tempgw (append tempgw (elimdup (ana_gw input (null tempgw)))))
              ;   (format t "Analizzatore 1: tempgw= ~a~%" tempgw)
              ;   (break "")
            tempgw)
	 (espressione (ana_expression input))
	 (date (ana_date (first elem)))
	 (number (ana_num (first elem)))
	 (paragraph-numb (ana_parnum (first elem)))
	 (segnointer (ana_segni (first elem) 'punct))
	 (sigla 
              (multiple-value-setq (alloptions up-extern) (final-accent-and-uppercase input))
	      (setq knownsigla (all-accent-get alloptions *SIGLE-DICT*))
            ; *** in case the tokenizer hypothesized a sigla (e.g. "a.32.B"), which is
            ;     not present in the lexicon, the global var default-sigla is set
	      (cond ((null knownsigla)
                       (setq default-sigla
                         (list (list input 'cat 'noun 'proper 'yes 'default 'yes)))
                         nil)
                    (t knownsigla)))
	 (nomep 
              (multiple-value-setq (alloptions up-extern) (final-accent-and-uppercase input))
              (append (all-accent-get alloptions 'mproperdef)
                      (ana_np input)))
	 (year (ana_year (first elem)))
	 (hour (ana_hour (first elem)))
	 (special (ana_segni (first elem) 'special))
	 (non_noto "sequenza-non-nota-allo-scanner"))))

;*****************************************************************
(defun get-proper-interp (elem alloptions)
  (declare (special *SYSTEM-CONTEXT*))
  (cond ((or (not (memq *SYSTEM-CONTEXT* '(tule legal tocai-test)))
             (member (get-tule-char-name-from-base-code (first elem))
                     (get-charset-value 'caplet))
             (member (convert-base-codes-to-tule-char-names (first elem))
                     (list (line-to-tule-char "d'")
                           (line-to-tule-char "de")
                           (line-to-tule-char "di")
                           (line-to-tule-char "von")
                           (line-to-tule-char "van"))
                         :test #'equal))
         ; in the TULE context (corpus analysis), only capitalized words are accepted as proper names
           (all-accent-get alloptions 'mproperdef))			; *** proper names
        (t (filter-just-locutions (all-accent-get alloptions 'mproperdef)))))

;*****************************************************************
; *** it extracts from a list of word interpretation, the ones that are locutions
(defun filter-just-locutions (interplist)
 (let (result)
  (do ((nxtint (first interplist) (first interplist))
       (interplist (rest interplist) (rest interplist)))
      ((null nxtint) result)
      (cond ((eq 'yes (prendi (without-lemma nxtint) 'locut))
               (setq result (cons nxtint result)))))))

;*****************************************************************
; *** it extracts from a list of 'locution' interpretations of a word the
;     'fixed' ones. i.e. the ones morphologically invariable
(defun get-fixed-locut (locut-list)
  (cond ((null locut-list) nil)
        ((null (prendi (without-lemma (first locut-list)) 'morphcat))
          (cons (first locut-list) (get-fixed-locut (rest locut-list))))
        (t (get-fixed-locut (rest locut-list)))))

;*****************************************************************
; *** Analyzes the description of hours given in the form 18:45 or 12:33:24
;     The output includes features for hour, minute, an possibly second
; *** 'tulenames' is the first of the output from tokenizer, so it has the form
;     (char1 ... charn), where the char-i are Tule char names
; *** the foreseen formats (coming from the tokenizer) are:
;     h:mm, hh:mm, h:mm:s, hh:mm:s, h:mm:ss, hh:mm:ss
(defun ana_hour (charcodes)
; *** if the length is greater than 4, the input is of the form '1975
;     It is analysed as 1975
  (let (hours minutes seconds minsec tempres
        (tulenames (convert-base-codes-to-tule-char-names charcodes)))
   (cond ((eq (second tulenames) 'colon)
    ; *** if the second item is a colon, the hour is represented by the first char
	    (setq hours (from-tule-char-names-to-integers (list (first tulenames))))
	    (setq minsec (rest (rest tulenames))))
         ((eq (third tulenames) 'colon)
    ; *** if the third item is a colon, the hour is represented by the first two chars
	    (setq hours 
                (from-tule-char-names-to-integers 
                                 (list (first tulenames) (second tulenames))))
	    (setq minsec (rest (rest (rest tulenames)))))
         (t (exception-nothrow "PROC/analizzatore: hours 1")))
    ; *** the minutes are always given as two digits
   (setq minutes (from-tule-char-names-to-integers (list (first minsec) (second minsec))))
   (cond ((null (third minsec)) nil) ; *** no seconds
         ((null (fifth minsec))
       ; *** the seconds are expressed as a single digit
            (setq seconds (from-tule-char-names-to-integers (list (fourth minsec)))))
       ; *** the seconds are expressed as two digits
         (t (setq seconds (from-tule-char-names-to-integers 
                                (list (fourth minsec) (fifth minsec))))))
   (setq tempres (list (list (implode charcodes) 'cat 'hour 'hours hours 'minutes minutes)))
   (cond ((null seconds) tempres)
         (t (list (append (first tempres) (list 'seconds seconds)))))))

;*****************************************************************
; *** Analyzes the description of years given in the form '98 o `98
;     The output is as for the dates, but with only the year specified
; *** 'val' is the first of the output from tokenizer, so it has the form
;     (char1 ... charn)
(defun ana_year (numcodes)
; *** if the length is greater than 4, the input is of the form '1975
;     It is analysed as 1975
  (let ((tulenames (convert-numcodes-to-tule-char-names numcodes)))
   (cond ((> (length tulenames) 4)
	       (make-year (rest tulenames)))
	 ((= (length tulenames) 4)
	   (cond ((eq 'zero (second tulenames))
	            (make-year (cons 'two (rest tulenames))))             ; '034 --> 2034
		 (t (make-year (cons 'one (rest tulenames))))))          ; '857 --> 1857
	 ((= (length tulenames) 3)
	   (cond ((eq 'zero (second tulenames))
		    (make-year (cons 'two (cons 'zero (rest tulenames)))))    ; '02 --> 2002
	         ((and (eq 'one (second tulenames)) (eq 'one (third tulenames)))
		    (append1 
                        (make-year (cons 'two (cons 'zero (rest tulenames)))) ; '11 --> 2011
                        (list (implode (convert-tule-char-names-to-numcodes (rest tulenames)))
                                   'cat 'num 'type 'cardin 'value 11)))    
                        ; *** in this case it could be "l'11 marzo"
		 (t (make-year (cons 'one (cons 'nine (rest tulenames))))))) ; '97 --> 1997
  ; *** this last condition applies in case it actually wasn't an year, as in
  ;     l'8 febbraio. The tokenizer should be modified, but it's easier to make the
  ;     correction here
         (t (list (list (implode (convert-tule-char-names-to-numcodes (rest tulenames)))
                        'cat 'num 'type 'cardin 'value (digit-tulename-val (second tulenames))))))))

;*****************************************************************
(defun make-year (tulenames)
  (list (list (implode (convert-tule-char-names-to-numcodes tulenames))
              'cat 'date 'year (from-tule-char-names-to-integers tulenames))))
; *** convert-tule-char-names-to-numcodes in MORPHO/char-funct
; *** from-tule-char-names-to-integers in MORPHO/numbers

;*****************************************************************
; *** this functions converts a date expressed as a sequence of ascii codes
;     it returns an output element of the form
;      ((12/5/01 cat date day 12 month may year 2001))
(defun ana_date (data)
 (declare (special *MONTHS*))
 (let (result temp (tulenames (convert-numcodes-to-tule-char-names data)))
    (dolist (nxtchar tulenames 
                (setq result (append1 result (from-tule-char-names-to-integers temp))))
         (cond ((eq nxtchar 'slash)
   	         (setq result (append1 result (from-tule-char-names-to-integers temp)))
    ; *** from-tule-char-names-to-integers in MORPHO/numbers
                 (setq temp nil))
	       (t (setq temp (append1 temp nxtchar)))))
    (list (list (implode data)
	        'cat 'date
	        'day (first result)
	        'month (nth (1- (second result)) *MONTHS*) 
	        'year (cond ((< (third result) 10) (+ 2000 (third result)))
	                    ((> (third result) 99) (third result))
	                    (t (+ 1900 (third result))))))))

;*****************************************************************
; *** analysis of an output of the tokenizer of type 'paragraph-numb'
;     This is actually very simple, since no specific cprocessing is 
;     required
; *** The function returns a double list including:
;     - the input word
;     - the category (paragraph-n)
;     - the numeric value
(defun ana_parnum (numero)
  (let* ((implnum (implode numero))
         (numval (get-subsect-list numero)))
     (list (list implnum
             'cat 'index
	     'value numval))))

;*****************************************************************
; *** given an ascii sequence representing numbers separated by dots, 
;     returns the list of number values
;     "1.3.24.1" --> (1 3 24 1)
(defun get-subsect-list (numero)
  (let (buff result
        (tulenum (convert-base-codes-to-tule-char-names numero)))
     (dolist (nxtchar tulenum 
                  (append1 result (from-tule-char-names-to-integers buff)))
        (cond ((eq nxtchar 'period)
                 (cond ((null buff)
                          (exception-nothrow "PROC/analizzatore: paragraph numbers"))
                       (t (setq result
                             (append1 result (from-tule-char-names-to-integers buff)))
                          (setq buff nil))))
              ((member nxtchar (get-charset-value 'digit))     ; *** see loadfunctions
                 (setq buff (append1 buff nxtchar)))
              (t (exception-nothrow "PROC/analizzatore: paragraph numbers 2"))))))

;*****************************************************************
; *** analysis of an output of the tokenizer of type 'number'
;     This can have three forms: 
;     1. a list of digits, commas and/or periods (a standard number)
;     2. such a list closed by a % (the autput must be 'percentuale')
;     3. such a list closed by a ° (the output must be 'adj ordin')
; *** The function returns a double list including:
;     - the input word
;     - the category (numb, percentuale or adj)
;     - in case of adj, 'type ordin gender allval number allval'
;     - the numeric value
; *** numero includes the characters of the input form, expressed in base (ISO-8859-1)
;     numeric codes. For instance, the number "60" is (54 48)
(defun ana_num (numero)
  (let* ((implnum (implode numero))
         (tulenum (convert-base-codes-to-tule-char-names numero))
   ; *** in tulenum, the input expressed as tule char names "60" --> (SIX ZERO)
         alloptions up-extern locut mproper)
      (multiple-value-setq (alloptions up-extern) (final-accent-and-uppercase implnum))
      (setq locut (all-accent-get alloptions 'locutdef))
               ; *** all-accent-get in char-funct.lisp
      (setq mproper (all-accent-get alloptions 'mproperdef))
      (cond ((eq (ult tulenum) 'percent)
               (break "Percents in MORPHO/analizzatore")
               (append
                  (append
	             (list (list implnum
		                 'cat 'percentuale
		                 'value (from-tule-char-names-to-integers (butlast tulenum))))
                     locut)
                  mproper))
  ; ** the next for ordinals expressed as numbers, as 77° or 77º
            ((memq (ult tulenum) '(nth nth-underscore))
               (append
                  (append
	             (list (list implnum
		                 'cat 'adj 'type 'ordin
		                 'gender 'allval 'number 'allval
		                 'value (from-tule-char-names-to-integers (butlast tulenum))))
                     locut)
                  mproper))
            (t (append
                  (append 
                      (list (list implnum
		                  'cat 'num 'value (from-tule-char-names-to-integers tulenum)))
                      locut)
                  mproper)))))

;*****************************************************************
; *** expressions are not analysed; the output is a (double) list
;     including
;     - the input expression
;     - cat expression
(defun ana_expression (espressione)
  (list (list espressione 'cat 'expression)))

;*****************************************************************
; *** No analysis for punctuation marks and special symbols
; *** 'tok-type' can be 'punct' or 'special'
; *** tok-elem is a list of base (ISO-8859-1) numeric codes
(defun ana_segni (tok-elem tok-type)
  (let* ((implelem (list (implode tok-elem)))
         (locut (all-accent-get implelem 'locutdef))
               ; *** all-accent-get in char-funct.lisp
         (mproper (all-accent-get implelem 'mproperdef))
        )
       (append (append
               (list (list (get-lisp-char-from-base-code (first tok-elem)) 'cat tok-type))
               locut) mproper)
    ))

;*****************************************************************
; *** If 'nome_proprio' exists in the dictionary, then it returns the
;     retrieved data, otherwise, it builds a 'possible' proper name
;     interpretation
(defun ana_np (nome_proprio)
  (declare (special begphrase *PROPER-DICT*))
  (let (proper up-extern alloptions)
      (multiple-value-setq
           (alloptions up-extern) (final-accent-and-uppercase nome_proprio))
      (setq proper (all-accent-get alloptions *PROPER-DICT*))
     (cond ((not (null proper)) proper)
           ((or (and (not begphrase) (> (length (explode nome_proprio)) 1))
   ; *** the next condition for initials, also occurring at the beginning
   ;     of a sentence: W. A. Mozart was ...
                (and (= (length (explode nome_proprio)) 2)
                     (char= (ult (explode nome_proprio)) #\.)))
	      (list (list nome_proprio 'cat 'noun 'proper 'yes 'default 'yes))))))

;*****************************************************************
;	GENERAL WORDS
;*****************************************************************
; *** Analysis of the tokenizer interpretations of type GW (General Words)
;     The function 'endings' carries out the morphological analysis. The result
;     is in output_ana. It is used to match the bottom-up
;     hypotheses about the structure of the word, with the knowledge stored in
;     the dictionary. So, the output must include all information about
;     root-suffix separations.
; *** Numbers written in letters are recognized by the function 'parsenum'
; *** Enclitics: they are represented as further components of a verbal
;	  entry, and have the form:
;	   (CLITIC enclitic) or (DOUBLENCL enclitic)
;     The function ANA_ENCLITICA takes care of translating the information
;     about the enclitics coming from the morphological analyser into the
;     standard output format (to be printed in the output file)
; *** If the root is non-nil, it is looked for in the dictionary, and a match
;     between the morphological info and the dictionary info (for that root)
;     is checked. This is done by ANA_NOUN, ANA_ADJ, ANA_VERB_REG, ANA_IRREG.
; *** output_ana is the result of the morphological analysis
;     Its format is as follows:
;         output_ana --> (interp-root1 ... interp-rootn)
;         interp-root --> (suffix-pos root suffix-interp)
;         suffix-pos --> (start end)
;         suffix-interp --> (categ1 categ1-info ... categN categN-info)
;         categ --> ADJ | ADV | NOUN | VERB
;         categ-info --> (class1 suffix-data1 ... classM suffix-dataM)
;         class --> "the numeric morphological class"
;         suffix-data --> features (gender, number, tense, etc.)
;    suffix-pos specifies the poistin of the sufix inside the word (starting
;    from the end, and is no more relevant at this stage)
;	Ex. capitano
;      (  (((1 3) |capit|
;           (VERB
;              (1 (A (6))
;		        2 (E (6) N (6))
;			    3 (E (6) N (6))
;				4 (A (6))
;               7 (E (6) N (6))
;				8 (E (6) N (6))
;				10 (B (6))))))
;         (((1 1) |capitan|
;           (ADJ
;              (1 (GENDER M NUMBER SING)
;               4 (GENDER M NUMBER SING) 
;               5 (GENDER M NUMBER SING))
;            NOUN
;              (2 (GENDER M NUMBER SING)
;               3 (GENDER M NUMBER SING)
;               5 (GENDER M NUMBER SING)
;               14 (GENDER M NUMBER SING)
;               19 (GENDER M NUMBER SING)
;               20 (GENDER M NUMBER SING)
;               36 (GENDER M NUMBER SING)
;               38 (GENDER M NUMBER SING))
;            VERB
;              (1 (A (1))
;               2 (A (1))
;               3 (A (1))
;               4 (A (1))
;               6 (A (1))
;               7 (A (1))
;               8 (A (1) I (GENDER M NUMBER SING))
;               10 (B (1))))))
;      ) 
;	  dallo
;      (  (((1 1) |dall|
;           (ADJ
;              (1 (GENDER M NUMBER SING)
;               4 (GENDER M NUMBER SING)
;               5 (GENDER M NUMBER SING))
;            NOUN
;              (2 (GENDER M NUMBER SING)
;               3 (GENDER M NUMBER SING)
;               5 (GENDER M NUMBER SING)
;               14 (GENDER M NUMBER SING)
;               19 (GENDER M NUMBER SING)
;               20 (GENDER M NUMBER SING)
;               36 (GENDER M NUMBER SING)
;               38 (GENDER M NUMBER SING))
;            VERB
;              (1 (A (1))
;               2 (A (1))
;               3 (A (1))
;               4 (A (1))
;               6 (A (1))
;               7 (A (1))
;               8 (A (1) I (GENDER M NUMBER SING))
;               10 (B (1))))))
;         (((4 4) |d|
;			(VERB 
;            (1 (N (2))
;             2 (N (3))
;             3 (N (3))
;             4 (N (2))
;             7 (N (3))
;             8 (I (GENDER F NUMBER SING) N (3)))))
;          ((1 3) DOUBLENCL LO))
;      )
;	  regalargliene
;      (  (((1 1) |regalarglien|
;           (ADJ
;            (1 (GENDER F NUMBER PL)
;             2 (GENDER ALLVAL NUMBER SING)
;             3 (GENDER F NUMBER PL)
;             7 (GENDER F NUMBER PL))
;            NOUN
;            (1 (GENDER F NUMBER PL)
;             3 (GENDER F NUMBER PL)
;             7 (GENDER F NUMBER PL)
;             8 (GENDER M NUMBER SING)
;             9 (GENDER F NUMBER PL)
;             10 (GENDER M NUMBER SING)
;             11 (GENDER F NUMBER SING)
;             17 (GENDER ALLVAL NUMBER SING)
;             18 ((GENDER M NUMBER SING) (GENDER F NUMBER PL)))
;            VERB
;            (2 (A (3))
;             3 (A (3))
;             6 (C (3))
;             7 (A (3) C (3))
;             8 (A (3) C (3) I (GENDER F NUMBER PL))))))
;         (((3 3) |regalargli|
;           (VERB
;            (8 (I (GENDER F NUMBER PL))))
;          ((1 2) CLITIC NE)))
;         (((7 8) |regal|
;           (VERB
;            (1 (M)
;             4 (M)
;             6 (M)))
;           ((3 6) CLITIC GLIE)
;           ((1 2) CLITIC NE)))
;      )
(defun ana_gw (parola &optional nofixedinterp)
  (let* (output_ana radice catdiz classeaut classediz suffix-info
         encl-data diz-output out_temp invar_diz_outp numb-out suffix
         (numb-gend 'allval) (numb-numb 'allval) suff-cut suff-add up-extern 
         alloptions longest-suffix (length-longest-suffix 0) final-output
         (explparola (mapcar #'get-tule-char-name-from-lisp-char (explode parola)))
         (firstlowerc (member (first explparola) (get-charset-value 'minlet)))
         (noapostrophe (neq (ult explparola) 'apostrophe))
         (includehyphen (member 'hyphen explparola)))
; *** nofixedinterp is true in case no fixed (invariable, sigla, prepart, ...)
;     interpretation does exist. Just in this case, the method for handling
;     unknown words is applied
; *** LOCAL VARIABLES:
;  >>> ouput_ana: described above
;  >>> out_temp: collects the final output
;  >>> radice: the hypothesized root for 'parola'
;  >>> cataut: possible syntactic categories associated with the suffix 
;  >>> catdiz: possible syntactic categories got from the dictionary
;      for that root
;  >>> classeaut: morphological (variational) classes associated with 
;      the suffix
;  >>> classediz: morphological (variational) classes associated got
;       from the dictionary for that root
;  >>> encl-data: result of the analysis of enclitics (see 'ana_enclitica')
  (declare (special classediz classeaut out_temp catdiz radice locut
                    *CATEG-INVAR* *LANGUAGE* *MAIN-DICT*))
  (cond ((eq *LANGUAGE* 'hindi)
           (setq output_ana (endings parola)))
        (t (setq output_ana (endings (base-lowercase parola)))))
     ; *** lowercase, since it is assumed that general words are stored in the 
     ;     dictionary in lowercase; otherwise, we should generate both lowercase
     ;     and uppercase, and look up twice
  (dolist (interp-root output_ana)
      (setq radice (second (first interp-root)))
      (cond ((null radice)
              (setq radice '@empty-root)))
      (setq suffix-info (third (first interp-root)))
      (setq suffix (fourth (first interp-root)))
      (setq encl-data (ana_enclitica (rest interp-root)))
; *** 'suffix-info' contains the data about the morphological classes
;     associated with the recognized suffix
; *** N.B. "glielo" appears among the "invariabili" as proclit 
      (multiple-value-setq (alloptions up-extern) (final-accent-and-uppercase radice))
      (setq diz-output (all-accent-get alloptions *MAIN-DICT*))
; *** extraction of the 'locution' intepretations
      (setq locut (all-accent-get alloptions 'locutdef))
  ; *** for all locutions, possible morphological information is extracted, 
  ;     and 'cat' is changed to 'loc-cat', and 'morphcat' to 'cat', in order
  ;     to enable for the application of standard morphological procedures
      ;(format t " parola: ~a; interpretazione: ~a~% locution: ~a~%" parola interp-root locut)
      ;(break "analizzatore: change locution cats")
      (cond ((not (null locut))
	      (dolist (singloc locut)
		      (cond ((not (null (prendi (without-lemma singloc) 'morphcat)))
		              (setq diz-output
			             (append1 diz-output 
				           (ch-loc-cats singloc))))))))
      (dolist (nextdiz diz-output)
; *** loop on 'diz-output': the body is repeated for each dictionary
;     interpretation of that root
          (setq catdiz (prendi (without-lemma nextdiz) 'cat))
          (setq classediz (prendi (without-lemma nextdiz) 'classe))
          (do ((cataut (first suffix-info) (first suffix-info))
               (classiaut (second suffix-info) (second suffix-info))
               (suffix-info (rest (rest suffix-info))
                            (rest (rest suffix-info))))
             ((null cataut))
; *** loop on 'suffix-info': the body is repeated for each possible category
;     associated with that root
; *** 'cataut' is one of ADJ, ADV, NOUN, VERB
; *** 'classiaut' has the form 
;       (class-1 features-1 .... class-n features-n)
; *** Analysis of NOUNS
               (cond ((and (eq cataut 'NOUN)
                           (member catdiz '(noun verb adj)))
   ; *** the analysis must be done also for verbs and adjectives, to account
   ;	 for derivations (verbs: "-zione", "-mento", "-o"; adjectives: "-ita")
		       (setq out_temp
		           (append out_temp 
		                 (ana_noun nextdiz classediz classiaut catdiz))))
; *** Analysis of ADJECTIVES
		     ((and (eq cataut 'ADJ)
		           (member catdiz '(adj pron num predet art)))
		       (setq out_temp
		           (append out_temp 
		                  (ana_adj nextdiz 
                                        classediz classiaut catdiz encl-data suffix))))
; *** Analysis of VERBS
	             ((and (eq cataut 'VERB) (eq catdiz 'VERB))
	               (setq out_temp
		           (append out_temp 
		                  (ana_verb nextdiz classediz classiaut encl-data))))
; *** Analysis of ADVERBS (derived from adjectives)
	             ((and (eq cataut 'ADV) (eq catdiz 'ADJ))
		       (setq out_temp
		           (append out_temp (ana_adv radice classiaut classediz)))))
		  ) ; end do on suffix-info (same suffix, but possibly different categs) *****
      ) ; end dolist su diz-output *************
      ; *** the categories of possible locutions are reset to the original value
      ;     (the inverse operation of 'ch-loc-cats'): the feature 'loc-cat' is
      ;     reset to 'cat', and 'cat' to 'morphcat'
      ;(format t " parola: ~a; locution: ~a~%" parola locut)
      ;(break "analizzatore: reset-loc-cats")
      (cond ((and nofixedinterp			; no fixed interpretations (e.g. invariabili)
                  firstlowerc			; the word is not capitalized
                  noapostrophe			; the last character is not an apostrophe
                  (not includehyphen)		; the word does not include an hyphen
                  (null out_temp)		; no interpretations from the dictionary
                  (not (null suffix-info))	; there is a suffix involved
                  (>= (length (explode suffix)) length-longest-suffix))
              (setq longest-suffix
                  (append1 longest-suffix
                      (list :suffix-info suffix-info :suffix suffix)))))
      (cond ((not (null locut)) (setq out_temp (reset-loc-cats out_temp))))
  ) ;end external do on output_ana *************
   ; *** to check the word with null suffix, look it in the *MAIN-DICT*
   ;     and put it in out_temp
   ; *** N.B. Questo va fatto separatamente, perche' i controlli da fare
   ;          sul dizionario (che sia categoria invariabile o che sia
   ;          indicata "classe (0)" sono diversi)
  (multiple-value-setq (alloptions up-extern) (final-accent-and-uppercase parola))
  (setq invar_diz_outp (all-accent-get alloptions *MAIN-DICT*))
       ; *** for all invariable definitions (if any) check if their class is 0,
       ;     or they are of one of the invariable categories (as ADV, CONJ, etc.)
  (dolist (nextinvar invar_diz_outp)
	   (cond ((or (member 0 (prendi (without-lemma nextinvar) 'classe))
		      (member (prendi (without-lemma nextinvar) 'cat) *CATEG-INVAR*))
              ;     (format t "Analizzatore 3: nextinvar= ~a~%" nextinvar)
              ;     (break "")
                   (setq out_temp (append1 out_temp nextinvar)))))
  ;    (format t "Analizzatore 4: out_temp= ~a~%" out_temp)
  ;    (break "")
  (cond ((and (null out_temp)
              (= 1 (length (explode parola))))
           (setq out_temp `((,parola cat special)))))
       ; *** possible interpretation of the input as a number (ex. 'centoventi')
  (setq numb-out (parsenum (mapcar #'char-downcase (explode parola))))
  (cond ((and (not (null numb-out))
              (listp numb-out)
              (listp (second numb-out))
              (listp (third numb-out)))
       ; *** in case the number is an ordinal the second item of the list is a pair (gender number)
       ;     and the third is info about the suffixes
           ;(format t "NUMB-OUT = ~a~%" numb-out)
           (setq numb-gend (first (second numb-out)))
           (setq numb-numb (second (second numb-out)))
           (setq suff-cut (first (third numb-out)))
          ; *** the next to cover versions where suff-cut and suff-add are not
          ;     needed (as in English, where adjectives are invariable)
           (setq suff-add (second (third numb-out)))
           (setq numb-out (first numb-out))))
  (cond ((null suff-cut) (setq suff-cut 0)))
  (setq final-output
     (cond ((null numb-out) out_temp)
           ((listp numb-out)
          ; *** if numb-out is a list, but its second item is not a list,
          ;     the number was truncated (vent', quarant')
          ;     in this case, the first of numb-out is the value, the second is the
          ;     truncated char, which must replace the apostrophe (which should be
          ;     present in the input)
              (let ((expw (mapcar #'char-upcase (explode parola))))
                  (cond ((not (char= (ult expw) #\'))
                           (exception-nothrow "PROC/analizzatore: numbers")
                           out_temp)
                        (t (setq expw (append1 (butlast expw)
                                        (char-upcase (second numb-out))))))
                   (append1 out_temp `(,(implode expw) cat num type cardin
                                       value ,(first numb-out) form elision))))
	   ((= numb-out 1)
             (let ((default-gender (cond ((eq *LANGUAGE* 'english) 'allval)
                                         (t 'm))))
                          ; *** English: one; Italian: uno
	      (append1 out_temp 
		   `(,(implode (mapcar #'char-upcase (explode parola)))
			   cat num type cardin value ,numb-out
						   gender ,default-gender number sing))))
	   ((> 0 numb-out)
         ; *** negative numbers are the result of having parsed an ordinal 
              (let ((base-form (change-ord-suff parola suff-cut suff-add)))
                (append out_temp 
		   `((,base-form cat adj type ordin gender ,numb-gend 
                                 number ,numb-numb value ,(- 0 numb-out))
		     (,base-form cat pron type ordin gender ,numb-gend 
                                 number ,numb-numb value ,(- 0 numb-out))))))
	   (t (append1 out_temp 
		   `(,(implode (mapcar #'char-upcase (explode parola)))
			   cat num type cardin value ,numb-out)))))
; *** the next concerns the analysis of unknown words
;     If there is no fixed interpretation (sigla, proper, etc.), the first character is
;     lower case, there is no internal hyphen, and the word does not terminate with an
;     apostrophe, then assign a default interpretation.
;     This does not work for "corpillo'", but otherwise, something as "albero'" is
;     taken as an unknown perfect of the verb "alberare", since outside this function
;     the most complete interpretation (i.e. this default one) is preferred over the
;     noun+apostrophe one
       ;(format t "Analizzatore 4: final-output= ~a~%" final-output)
       ;(break "")
   (cond ((and nofixedinterp
               firstlowerc
               noapostrophe			; the last character is not an apostrophe
               (not includehyphen)
               (null final-output))
            (let (suffix-info suffix)
               (dolist (buf longest-suffix)
                   (setq suffix-info (getf buf :suffix-info))
                   (setq suffix (getf buf :suffix))
                   (do ((cataut (first suffix-info) (first suffix-info))
                        (classiaut (second suffix-info) (second suffix-info))
                        (suffix-info (rest (rest suffix-info)) (rest (rest suffix-info))))
                       ((null cataut))
                       (cond ((eq cataut 'VERB)
                                (setq final-output 
                                   (append final-output 
                                       (ana_verb (list parola 'cat 'verb) '(1) classiaut nil))))
                             ((eq cataut 'NOUN)
                                (setq final-output 
                                   (append final-output 
                                       (ana_noun (list parola 'cat 'noun) classiaut classiaut cataut))))
                             ((eq cataut 'ADJ)
                                (setq final-output 
                                   (append final-output 
                                       (ana_adj (list parola 'cat 'adj) '(1) classiaut cataut nil suffix))))))))))
  ; (format t "Analizzatore: non noto 1: ~a~%" parola)
  ; (break "")
   (cond ((and nofixedinterp
               firstlowerc
               noapostrophe
               (not includehyphen)
               (null final-output))
            `((,parola cat noun gender allval number allval)))
         (t final-output))
))

;*********************************************************************
; *** modifies the suffix of an ordinal, in order to get the base form
(defun change-ord-suff (woord oldsufflength newsuff)
  (let ((explw (explode woord))
        (explsuf (cond ((null newsuff) nil)
                       (t (explode newsuff))))
        root)
      (setq root (first-n (- (length explw) oldsufflength) explw))
      (base-uppercase (implode (append root explsuf)))))

;*********************************************************************
; *** fa diventare "loc-cat" la property "cat" e "cat" la property "morphcat"
;     (serve per il trattamento delle locuzioni)
;     Esempio: (conferenza_stampa cat noun morphcat noun classe (1)
;			locut yes loctype flex prev nil next stampa) --->
;     	       (conferenza_stampa loc-cat noun cat noun classe (1)
;			locut yes loctype flex prev nil next stampa)
(defun ch-loc-cats (woord)
   (cons (first woord)
      (subst 'cat 'morphcat (subst 'loc-cat 'cat (without-lemma woord)))))

;*********************************************************************
; *** fa l'operazione inversa della precedente, ma su una lista di entries
(defun reset-loc-cats (woord-list)
      (cond ((null woord-list) nil)
	    ((or (null (prendi (without-lemma (first woord-list)) 'locut))
                 (member 'morphcat (first woord-list)))
     ; *** the second coonjunct to avoid double substitutions
	       (cons (first woord-list) (reset-loc-cats (rest woord-list))))
	    (t (cons (cons (first (first woord-list))
                           (subst 'cat 'loc-cat 
			          (subst 'morphcat 'cat (without-lemma (first woord-list)))))
		     (reset-loc-cats (rest woord-list))))))

;*****************************************************************
; *** It handles enclitics hypothesized by the suffix automaton.
;     They appear as second element of the infos stored in the
;     single morphological hypothesis (which is given here as input, 'interp')
; *** For instance, 'enclitica' coud be:
;     Ex.1 'prender-lo' --> (((1 2) CLITIC LO))
;     Ex.2 'prender-ce-lo' --> (((3 4) CLITIC CE) ((1 2) CLITIC LO))
; *** The lexical information about the enclitics are taken from '*ENCLITINFO*'
(defun ana_enclitica (enclitica)
  (declare (special *ENCLITINFO*))
  (cond ((null enclitica) nil)
; *** simple enclitic: return the info obtained for the (single) enclitic
        (t (cons (mapcar #'(lambda (x) (append x '(form clitic)))
                         (prendi *ENCLITINFO* (third (first enclitica))))
		 (ana_enclitica (rest enclitica))))))

;*****************************************************************
;		SPECIFIC CATEGORIES
;*****************************************************************
; *** Analysis of elements having a nominal suffix class
;     They can match true nouns, verbs with suffixes -zione, -mento, or
;     -zion/ment, and adjectives with suffixes -ità
; *** INPUT
;   >>> dizword: dictionary entry (e.g. (BAMBINO CAT NOUN CLASSE (3)))
;   >>> aut-interp: result of suffix analysis 
;        e.g. (((1 1) |bambin| 
;               (ADJ (1 (GENDER M ...) 3 (GENDER ...) ...)
;                NOUN (2 (GENDER M ...) 3 (GENDER ...) ...)
;                VERB ...)))
;   >>> catdiz-n: dictionary category (e.g. NOUN)
;   >>> classeaut-n: suffix morphological classes for NOUN
;                  e.g. (2 (GENDER M ...) 3 (GENDER ...) ...)
;   >>> classediz-n: dictionary morphological class (e.g. (3))
(defun ana_noun (dizword classediz-n classeaut-n catdiz-n)
  (declare (special *LANGUAGE* radice))
  (let* (char nomsuff match transit act-class)
; *** if the dictionary category is not VERB or ADJ, standard analysis
      (cond ((and (neq catdiz-n 'verb)
	          (neq catdiz-n 'adj))
              (let ((autinfo (member (first classediz-n) classeaut-n)))
		 (cond ((not (null autinfo))
                         (cond ((atom (first (second autinfo)))
			         (list (append dizword (second autinfo))))
   ; *** this second case refers to suffixes which are ambiguous for the same
   ;     class. This happens for class 18, where the suffix -e refers both to
   ;     masculine singular and to feminine plural (note that 'allval' cannot
   ;     be used in this case). An example is 'cialtrone'. Here, 'autinfo'
   ;     contains:
   ;     (18 ((gender m number sing) (gender f number pl)))
   ;     so the entry is duplicated for the two possibilities
                               (t (list (append dizword 
                                                (first (second autinfo)))
                                        (append dizword 
                                                (second (second autinfo))))))))))
; *** if a VERB, the input could be a noun obtained from the verb
            (t (cond ((eq catdiz-n 'verb)
   ; *** 'classeaut-n' specifies the found suffix:
   ;     Possible classes for -zione
   ;		30 (-azione), compatible with verbs of classes 1, 4;
   ;		31 (-izione), compatible with verbs of classes 3 e 9; 
   ;		32 (-iazione), compatible with verbs of classes 5 e 11
   ;     Possible classes for -mento
   ;		33 (-amento), compatible with verbs of classes 1, 4;
   ;		34 (-imento), compatible with verbs of classes 3 e 9;
   ;		35 (-iamento), compatible with verbs of classes 5 e 11
   ;     Possible classes for -o
   ;		36 (-o), compatible with verbs of classes 1, 9;
   ;		37 (-io), compatible with verbs of classes 5 e 11;
   ;       	38 (-o -hi; blocco) , compatible with verbs of class 4;
   ; *** The third part of the test checks that the conjugation is regular
   ;     (null cdr) or that it includes the infinite (member 'm ...)
                   (cond ((eq *LANGUAGE* 'italian)
	               (setq nomsuff (prendi (without-lemma dizword) 'nom))
	               (setq transit (prendi (without-lemma dizword) 'transitive))
	               (cond ((not (null nomsuff))
                               (cond ((and (member nomsuff '(zione zion-ment))
		                           (or (and (= (first classeaut-n) 30)
			                            (memq (first classediz-n)
                                                          '(1 4)))
			      	               (and (= (first classeaut-n) 31)
				  	            (memq (first classediz-n) 
                                                          '(3 9)))
		                               (and (= (first classeaut-n) 32)
			                            (memq (car classediz-n)
                                                          '(5 11))))
	 	                           (or (null (rest classediz-n))
		     	                      (member 'm (second classediz-n))))
		      	               (setq char 
                                         (cond ((= (first classeaut-n) 30) '|a|)
		       	                       ((= (first classeaut-n) 31) '|i|)
		       	                       ((= (first classeaut-n) 32) '|ia|)))
                                         (setq nomsuff '|zione|)
                                       (setq act-class (first classeaut-n))
                                       (setq match t))
	      	                     ((and (member nomsuff '(mento zion-ment))
                                           (or (and (= (first classeaut-n) 33)
		                                    (memq (first classediz-n)
                                                          '(1 4)))
                                               (and (= (first classeaut-n) 34)
                                                    (memq (first classediz-n)
                                                          '(2 3 9)))
                                               (and (= (first classeaut-n) 35)
                                                    (memq (first classediz-n)
                                                          '(5 11))))
                                           (or (null (rest classediz-n))
		     	                       (member 'm (first (rest classediz-n)))))
		      	               (setq char
                                         (cond ((= (first classeaut-n) 33) '|a|)
		       	                       ((= (first classeaut-n) 34) '|i|)
		       	                       ((= (first classeaut-n) 35) '|ia|)))
		                       (setq nomsuff '|mento|)
                                       (setq act-class (first classeaut-n))
                                       (setq match t))
	      	                     ((member nomsuff '(o))
                                       (cond ((and (member 36 classeaut-n)
                                                   (memq (first classediz-n)
                                                         '(1 9)))
				                (setq char '||)
                                                (setq act-class 36)
		      	    	                (setq nomsuff '|o|)
			    	                (setq match t))
			     	              ((and (member 37 classeaut-n)
				  	            (memq (first classediz-n)
                                                               '(5 11)))
				                (setq char '|i|)
                                                (setq act-class 37)
		      	    	                (setq nomsuff '|o|)
			    	                (setq match t))
                                              ((and (member 38 classeaut-n)
                                                    (memq (first classediz-n)
                                                          '(4)))
				                (setq char '||)
                                                (setq act-class 38)
		      	    	                (setq nomsuff '|o|)
			    	                (setq match t)))))
                               (cond (match (list (append (list (base-uppercase
                                                  (concat radice char nomsuff))
		                        		'cat 'noun
	  	                        		'classe act-class
		                        		'v-deriv (first dizword)
	  	                        		'v-trans transit)
		                      		(second (member act-class 
                                                           classeaut-n)))))))))
                         ((eq *LANGUAGE* 'english)
    ; *** there are two patterns of nominalization currently available for English
    ;     both are marked as "nom ion". In the first case, the "nom" specification
    ;     can be absent, since for verbs of class 5 (-ize) the corresponding
    ;     nominal suffix (-ization) is assemed by default
    ;     1. The first case is associated with verbal class 5 and to the
    ;        nominal class 30 (nominal-ize --> nominal-ization)
    ;     2. The second case corresponds to verbal class 7, and to the nominal class
    ;        31 (abbreviat-e --> abbreviat-ion)
	               (setq nomsuff (prendi (without-lemma dizword) 'nom))
	               (setq transit (prendi (without-lemma dizword) 'transitive))
	               (cond ((and (or (null nomsuff)
                                       (eq nomsuff 'ion))
                                   (= (first classeaut-n) 30)
			           (= (first classediz-n) 5))
          ; *** in case of -ization, no "nom" specification appears
          ;     in the dictionary, since it is assumed that all
          ;     vers of class 5 accept the -ization suffix
                                (setq nomsuff '|ization|)
                                (setq act-class (first classeaut-n))
                                (setq match t))
                             ((and (eq nomsuff 'ion)
		                   (= (first classeaut-n) 31)
			           (= (first classediz-n) 7))
                                (setq nomsuff '|ion|)
                                (setq act-class (first classeaut-n))
                                (setq match t)))
                       (cond (match (list (append 
                                            (list (base-uppercase
                                                     (concat radice nomsuff))
		                                  'cat 'noun
	  	                                  'classe act-class
		                                  'v-deriv (first dizword)
	  	                                  'v-trans transit)
		                      	    (second (member act-class 
                                                           classeaut-n)))))))))
; *** if an ADJ , the input could be a noun obtained from the adjective (-ità)
   ; *** The possible classes are
   ;	40 (-ita`), compatible with adjectives of classes 1, 2, 4, 5;
   ;	41 (-ieta`), compatible with adjectives of class 6
                     (t (cond ((eq *LANGUAGE* 'italian)
                                (setq nomsuff (prendi (without-lemma dizword) 'nom))
	                        (cond ((not (null nomsuff))
	      	                        (cond ((and (member nomsuff '(ita))
		                                    (or (and (= (first classeaut-n) 40)
		                                             (memq (first classediz-n)
                                                                   '(1 2 4 5)))
                                                        (and (= (first classeaut-n) 41)
                                                             (memq (first classediz-n)
                                                                   '(6))))
                                                    (or (null (cdr classediz-n))
                                                        (member 'm (rest classediz-n))))
		      	                        (setq char
                                                  (cond ((= (first classeaut-n) 40) '|i|)
			                                ((= (first classeaut-n) 41) '|ie|)))
		      	                        (setq nomsuff '|tà|)
			                        (setq match t)))))))
             	       (cond (match (list (append 
                                         (list (concat radice char nomsuff)
                                               'cat 'noun
                                               'classe (first classeaut-n))
					    (second classeaut-n)))))))))))

;*****************************************************************
; *** Analysis of adjectives. 
; *** INPUT: as above for 'ana_noun'
(defun ana_adj (dizword classediz-a classeaut-a catdiz-a enclit suffix)
  (declare (special *LANGUAGE* *LISP-CHAR-SET-ID* *CHAR-SET-ID*))
  (let ((class-codes (member (first classediz-a) classeaut-a)) temp 
        (semgeogr '£geogr))
  (cond ((neq *LISP-CHAR-SET-ID* *CHAR-SET-ID*)
           (setq semgeogr (convert-base-atom-or-string-to-currlisp semgeogr))))
  ; *** the next for appending the default type and grade to standard adj
      (cond ((and (memq catdiz-a '(adj num pron predet art))
                  (or (null (prendi (without-lemma dizword) 'locut))
                      (eq (prendi (without-lemma dizword) 'loctype) 'flex))
                  (or (not (null class-codes))
                      (and (eq 0 (first classediz-a)) (eq suffix '@empt))))
  ; *** there is a match
	      (cond ((not (null enclit))
          ; *** currently, this case refers only to I'd in English, where I, being
          ;     a pronoun is treated as having an adjectival morphological class
                       (list (cons dizword enclit)))
	            ((eq catdiz-a 'adj)
                      (let ((clcod (second class-codes)))
                          (cond ((atom (first clcod))
                                   (setq temp (list (append dizword '(type qualif grade pos) clcod))))
                                (t (setq temp
                                      (mapcar #'(lambda (x)
                                            (append dizword '(type qualif grade pos) x))
                                            clcod)))))
  ; *** the next cond automatically attaches to the adjectival interpretation
  ;     a nominal interpretation, in case the adj is of geographical semtype
                      (cond ((inh-member (first dizword) semgeogr)
                               (append1 temp 
                                  (append
                                     `(,(first dizword) 
                                       cat noun type common
                                       classe ,(map-adj-to-n-cl 
                                                     (prendi (without-lemma dizword) 'classe)))
                                     (second class-codes))))
                            (t temp)))
                    (t (list (append dizword (second class-codes))))))
; *** if there is not a match, but the morphological class is 20, then it is
;     a 'superlativo' (-issim...)
            ((and (eq *LANGUAGE* 'italian)
                  (or (and (= (first classeaut-a) 20)
                           (member (first classediz-a) '(1 2 3 5 6 7 8)))
	              (and (= (first classeaut-a) 21)
                           (= (first classediz-a) 4))))
	      (list (append dizword
                    '(type qualif grade superl)
		    (second classeaut-a)))))))

;*****************************************************************
(defun map-adj-to-n-cl (adjcl)
     (list (case (first adjcl)
         (1 3)
         (2 17)
         (3 6) 		; !!!! only masculine !!!
         (4 20)
         (5 19)
         (6 6)
         (7 9)
         (8 16)         ; !!!! wrong masculine plural !!!!
         (otherwise 
              (exception-nothrow "PROC/analizzatore: map-adj-to-n-cl")
              0))))

;*****************************************************************
; *** Analysis of verbs
; *** INPUT: as above for 'ana_noun'. However, the class definitions, for verbs,
;     are somewhat more complex, since they include also the 'tense' and
;     'person-number' codes.
;     [ex. classediz-v: (3 (A (4) N (2)) 6 (E N (3)));
;          classeaut-v: (6 (D F (1 2 3) N (3)) 7 (D (1 2 3)))].
;     Moreover, we do not have here 'catdiz' (it is necessarily 'verb'), but we
;     have 'enclit', which is the result of the analysis of possible enclitics
(defun ana_verb (dizword classediz-v classeaut-v enclit)
 (declare (special *LANGUAGE*))
 (let (personaut tmaut tmdiz general-info verbresult persnumblist mood+tenseaut)
; *** external do on all dictionary classes (classediz-v) for the root
   (cond ((and (not (null enclit))
               (equal classediz-v '(0)))
            (list (cons dizword enclit)))
         (t (do ()
    ((null classediz-v) verbresult)
    (cond ((and (not (null (rest classediz-v)))
                (listp (second classediz-v)))
; *** in this case, the verb is not regular ! In fact, the class code (e.g. 8)
;     is followed by a parenthesis, encoding the tense to which the root
;     applies
;     tmdiz: tense and mood licensed by the dictionary for that root
;     tmaut: tense and mood licensed by the morphology. 
;       when classediz-v becomes "(6 (E N (3)))", tmdiz is "(E N (3))"
; *** otherwise, tmdiz remains NIL
    	    (setq tmdiz (second classediz-v))))
    (setq tmaut (prendi classeaut-v (first classediz-v)))
; *** and now, tmaut is "(D F (1 2 3) N (3))"
; *** internal do (just for irregular verbs: tmdiz non-null) for each code
;     mood-tense licensed by the dictionary for the common class
;     (6 in the example)
    (cond ((not (null tmdiz))
            (do ()
               ((null tmdiz))
               (let* ((mood+tensediz (first tmdiz))
                      (persondiz (copy
                                  (cond ((memq mood+tensediz '(h i l m)) nil)
                                      ((listp (second tmdiz))
                                        (setq tmdiz (rest tmdiz))
                                        (first tmdiz))
                                      ((eq mood+tensediz 'n)
                                         '(2 3 4 5 6))
                                      (t '(1 2 3 4 5 6))))))
   ; *** tensediz is the code for the tense (e.g. A for the present indicative)
   ; *** if all forms of that tense are accepted, then 'tmdiz' would be , e.g.,
   ;     (D E F); so, 'persondiz' would be nil; otherwise, 'tmdiz' would be
   ;     (D (1 2 3) E ...), so 'persondiz' becomes (1 2 3). An exception is for
   ;     participles, where the list after the tense code specifies number and 
   ;     person.
                  (cond ((member mood+tensediz tmaut)
     ; *** there is a tense correspondence: check the person-number codes
                          (cond ((memq mood+tensediz '(h i l m))
                                  (setq general-info 
                                    (mood_tense mood+tensediz 
                                             (prendi tmaut mood+tensediz)))
                                  (setq personaut nil))
                                (t (setq general-info 
                                         (mood_tense mood+tensediz nil))
                                   (setq personaut (prendi tmaut mood+tensediz))
           ; *** "prendi" takes the element of the list after the one sought (tensediz)
           ;     so, if the element is an atome, it is another tense code, otherwise
           ;     it is a list of person-number codes
                                   (cond ((atom personaut)
                                           (cond ((eq mood+tensediz 'n)
                                                    (setq personaut '(2 3 4 5 6)))
                                                 (t (setq personaut '(1 2 3 4 5 6))))))))
     ; *** if 'persondiz' actually is a person code (as "(3)" in the example),
     ;     verify the correspondence with 'personaut'
                          (cond ((and (listp persondiz)
                                      (not (null persondiz)))
       ; *** the next cond tries to implement a kind of underspecification, including,
       ;     when possible, the "allval" value for the person and/or number code, in
       ;     place of all the values. This should reduce the tagging errors without
       ;     affecting the parser's operations
                                  (cond ((and (all-p-n-val persondiz)
                                              (all-p-n-val personaut))
           ; *** if all six values (1 2 3 4 5 6) are admitted both by the dictionary
           ;     and by the suffix of the word, then include the "allval allval" pair
           ;     for person and number. Reset the persondiz and personaut variables,
           ;     so the subsequent dolist does not add the different values
                                           (setq verbresult
                                              (cons (cons (append dizword general-info
                                                             '(person allval number allval))
                                                            enclit)
                                                      verbresult))
                                           (setq persondiz nil)
                                           (setq personaut nil))
                                        ((and (sing-p-n-val persondiz)
                                              (sing-p-n-val personaut))
           ; *** if only the values for singular (1 2 3) are admitted both by the dictionary
           ;     and by the suffix of the word, then include the "sing allval" pair
           ;     for person and number. Remove from the persondiz and personaut variables
           ;     the values accounted for, so the subsequent dolist will work only on the
           ;     plural codes. Note that they cannot be all, since otherwise the first
           ;     branch of this "cond" would have succeeded
                                           (setq verbresult
                                              (cons (cons (append dizword general-info
                                                             '(person allval number sing))
                                                            enclit)
                                                      verbresult))
                                           (setq persondiz 
                                              (delete 1
                                                 (delete 2
                                                    (delete 3 (copy persondiz)))))
                                           (setq personaut 
                                              (delete 1
                                                 (delete 2
                                                    (delete 3 (copy personaut))))))
                                        ((and (pl-p-n-val persondiz)
                                              (pl-p-n-val personaut))
           ; *** same as above for plural (4 5 6)
                                           (setq verbresult
                                              (cons (cons (append dizword general-info
                                                             '(person allval number pl))
                                                            enclit)
                                                      verbresult))
                                           (setq persondiz 
                                              (delete 4
                                                 (delete 5
                                                    (delete 6 (copy persondiz)))))
                                           (setq personaut 
                                              (delete 4
                                                 (delete 5
                                                    (delete 6 (copy personaut)))))))
			          (dolist (firstpers persondiz)
				     (cond ((member firstpers personaut)
                                             (setq verbresult
                                                (cons (cons (append dizword general-info
                                                               (number_person firstpers
                                                                    mood+tensediz))
                                                            enclit)
                                                      verbresult))))))
     ; *** if the dictionary does not pose constraints on persons, use the ones
     ;     obtained from the morphology 
                                ((and (listp personaut)
                                      (not (null personaut)))
                                  (cond ((all-p-n-val personaut)
           ; *** if all six values (1 2 3 4 5 6) are admitted by the suffix of the word,
           ;     then include the "allval allval" pair
                                           (setq verbresult
                                              (cons (cons (append dizword general-info
                                                             '(person allval number allval))
                                                            enclit)
                                                      verbresult))
                                           (setq personaut nil))
                                        ((sing-p-n-val personaut)
           ; *** if anly the values for singular (1 2 3) are admitted by the suffix of
           ;     the word, then include the "sing allval" pair
                                           (setq verbresult
                                              (cons (cons (append dizword general-info
                                                             '(person allval number sing))
                                                            enclit)
                                                      verbresult))
                                           (setq personaut 
                                              (delete 1
                                                 (delete 2
                                                    (delete 3 (copy personaut))))))
                                        ((pl-p-n-val personaut)
           ; *** same as above for plural (4 5 6)
                                           (setq verbresult
                                              (cons (cons (append dizword general-info
                                                             '(person allval number pl))
                                                            enclit)
                                                      verbresult))
                                           (setq personaut 
                                              (delete 4
                                                 (delete 5
                                                    (delete 6 (copy personaut)))))))
                                  (dolist (firstpers personaut)
                                     (setq verbresult
                                       (cons (cons (append dizword general-info 
                                                      (number_person firstpers
                                                                 mood+tensediz))
                                                    enclit)
                                             verbresult))))
     ; *** also the morphology does not say anything about the person
     ;     (infinitives and participles)
                                (t (setq verbresult 
                                      (cons (cons (append dizword general-info)
                                                  enclit)
                                            verbresult))))))
   ; *** the mood-tense code from the dictionary does not correspond to the one
   ;     licensed by the morphology; go on with the next
                  (setq tmdiz (rest tmdiz))))
   ; *** pass to the next dictionary class
            (setq classediz-v (rest (rest classediz-v))))
          (t (do ()
               ((null tmaut))
; *** the verb is regular: 'tmaut' can have three forms:
;     1. NIL: it is an infinite or a gerund [es. (M)]
;     2. tense, person and number: the standard case [es. (N (3))]
;     3. tense, gender and number: participles [es. (I (GENDER M NUMBER PL))]
; *** note, however, that in all cases 'tmaut' can include further elements
               (setq mood+tenseaut (first tmaut))
               (setq general-info (mood_tense mood+tenseaut nil))
               (cond ((is-fixed-tense-form mood+tenseaut)
      ; *** a non inflected tense (ex. english infinite)
                       (setq verbresult 
                           (cons (cons (append dizword general-info) enclit)
                                 verbresult))
                       (setq tmaut (rest tmaut)))
                     ((is-noperson-tense-form mood+tenseaut)
      ; *** a tense inflected just for gender and number (e.g. italian participles)
                       (setq verbresult 
                          (cons 
                            (cons (append dizword general-info (second tmaut))
                                  enclit)
                            verbresult))
                       (setq tmaut (rest (rest tmaut))))
    ; *** other tense codes: the second of tmaut is a list of person-number
    ;     codes; attach all possible person-number interpretations to the result
    ; *** the next cond in case a standard tense code includes all person-number
    ;     pairs. In this case, they are not specified, so that the list is
    ;     (... E M ...); in this case, for E, the 6 values must be forced
                     (t (cond ((atom (second tmaut))
                                 (setq verbresult 
                                     (cons (cons (append dizword general-info
                                                     (get-tense-allvalues mood+tenseaut))
                                                 enclit)
                                           verbresult))
                                 (setq tmaut (rest tmaut)))
                              (t (setq persnumblist (second tmaut))
                                 (setq tmaut (rest (rest tmaut)))
                                 (cond ((and (neq *LANGUAGE* 'hindi)
                                             (sing-p-n-val persnumblist))
           ; *** if only the values for singular (1 2 3) are admitted by the suffix of
           ;     the word, then include the "sing allval" pair
                                           (setq verbresult
                                              (cons (cons (append dizword general-info
                                                             '(person allval number sing))
                                                            enclit)
                                                      verbresult))
                                           (setq persnumblist 
                                              (delete 1
                                                 (delete 2
                                                    (delete 3 (copy persnumblist))))))
                                       ((and (neq *LANGUAGE* 'hindi)
                                             (pl-p-n-val persnumblist))
           ; *** same as above for plural (4 5 6)
                                           (setq verbresult
                                              (cons (cons (append dizword general-info
                                                             '(person allval number pl))
                                                            enclit)
                                                      verbresult))
                                           (setq persnumblist 
                                              (delete 4
                                                 (delete 5
                                                    (delete 6 (copy persnumblist)))))))))
                        (dolist (firstpers persnumblist)
                            (setq verbresult 
                                (cons (cons (append dizword general-info
                                                (number_person firstpers mood+tenseaut))
                                            enclit)
                                      verbresult))))))
   ; *** pass to the next dictionary class
             (setq classediz-v (rest classediz-v)))))))))

;*****************************************************************
; *** returns true if the mood+tense pair is not inflected
(defun is-fixed-tense-form (mood+tense-code)
   (declare (special *LANGUAGE*))
   (cond ((eq *LANGUAGE* 'hindi)
            (eq mood+tense-code 'h))		; *** h: participle
         ((memq *LANGUAGE* '(italian english french catalan spanish))
            (memq mood+tense-code '(l m)))      ; *** l: gerund; m: infinite
         (t (exception-nothrow 
                 "PROC/analizzatore: unknown language in is-fixed-tense-form"))))

;*****************************************************************
; *** returns true if the mood+tense pair is not inflected for person
(defun is-noperson-tense-form (mood+tense-code)
   (declare (special *LANGUAGE*))
   (cond ((eq *LANGUAGE* 'hindi)
            (memq mood+tense-code '(a b e)))	; *** a: base pres; b: base past
                                                ;     e: obligation
         ((memq *LANGUAGE* '(italian english french catalan spanish))
            (memq mood+tense-code '(h i)))      ; *** h: partic pres; i: partic past
         (t (exception-nothrow 
                 "PROC/analizzatore: unknown language in is-noperson-tense-form"))))

;*****************************************************************
; *** returns "allval" for all possible features
(defun get-tense-allvalues (mood+tense-code)
   (declare (special *LANGUAGE*))
   (cond ((eq *LANGUAGE* 'hindi)
            (cond ((memq mood+tense-code '(d f n)) ; *** d: subjunctive; f: infinitive
                                                   ;     n: imperative
                      '(person allval number allval))
                  ((eq mood+tense-code 'c)         ; *** c: future
                      '(person allval gender allval number allval))))
         ((memq *LANGUAGE* '(italian english french catalan spanish))
            '(person allval number allval))
         (t (exception-nothrow 
                 "PROC/analizzatore: unknown language in get-tense-allvalues"))))
            
;*****************************************************************
; *** decodes the person-number codes appearing in the dictionary
(defun number_person (person mood+tense-code)
  (declare (special *LANGUAGE*))
  (cond ((eq *LANGUAGE* 'hindi)
           (cond ((eq mood+tense-code 'c)		; future
                    (case person
                      (1 `(person 1 gender m number sing))
                      (2 `(person 4 gender m number sing))
                      (3 `(person 2 gender m number sing))
                      (4 `(person 3 gender m number sing))
                      (5 `(person 1 gender m number pl))
                      (6 `(person 4 gender m number pl))
                      (7 `(person 2 gender m number pl))
                      (8 `(person 3 gender m number pl))
                      (9 `(person 1 gender f number sing))
                      (10 `(person 4 gender f number sing))
                      (11 `(person 2 gender f number sing))
                      (12 `(person 3 gender f number sing))
                      (13 `(person 1 gender f number pl))
                      (14 `(person 4 gender f number pl))
                      (15 `(person 2 gender f number pl))
                      (16 `(person 3 gender f number pl))))
                 ((memq mood+tense-code '(d n))		; d: subjunctive; n: imperative
                                                ; for imperatives, 1 should not occur!
                    (case person
                      (1 `(person 1 number sing))
                      (2 `(person 4 number sing))
                      (3 `(person 2 number sing))
                      (4 `(person 3 number sing))
                      (5 `(person 1 number pl))
                      (6 `(person 4 number pl))
                      (7 `(person 2 number pl))
                      (8 `(person 3 number pl))))))
        (t (cond ((member person '(1 2 3)) `(person ,person number sing))
	         ((member person '(4 5 6)) `(person ,(- person 3) number pl))))))

;*****************************************************************
; *** decodes the mood-tense codes appearing in the dictionary
;     for the past participles, it also appends gender and number and/or person
(defun mood_tense (m-t-code gender-numb)
  (declare (special *LANGUAGE*))
  (cond ((eq *LANGUAGE* 'english)
           (case m-t-code
               (a '(mood ind tense pres))
               (b '(mood ind tense past))
               (e '(mood cong tense pres))
               (f '(mood cong tense past))
               (h (append '(mood participle tense pres) gender-numb))
               (i (append '(mood participle tense past) gender-numb))
               (l '(mood gerund tense pres))
               (m '(mood infinite tense pres))
               (n '(mood imper tense pres))
               (otherwise 
                  (exception-nothrow "PROC/analizzatore: mood-tense code"))))
        ((eq *LANGUAGE* 'hindi)
           (case m-t-code
               (a '(mood base tense pres))
               (b '(mood base tense past))
               (c '(mood base tense fut))
               (d '(mood subjunctive))
               (e '(mood oblig))
               (f '(mood infinitive))
               (h '(mood participle))
               (n '(mood imper tense pres))
               (otherwise 
                  (exception-nothrow "PROC/analizzatore: mood-tense code"))))
        (t (case m-t-code
               (a '(mood ind tense pres))
               (b '(mood ind tense imperf))
               (c '(mood ind tense rempast))
               (d '(mood ind tense fut))
               (e '(mood cong tense pres))
               (f '(mood cong tense imperf))
               (g '(mood condiz tense pres))
               (h (append '(mood participle tense pres) gender-numb))
               (i (append '(mood participle tense past) gender-numb))
               (l '(mood gerund tense pres))
               (m '(mood infinite tense pres))
               (n '(mood imper tense pres))
               (p '(mood cong tense fut))))))	; Spanish

;************************************************************************
; *** for adverbs derived from adjectives (suffix -mente)
;     >>> classeaut is the class of the adverbial affix found by the
;         morphological analyser (from suffixes)
;     >>> classediz is the class of the adjective found in the dictionary
(defun ana_adv (radice classeaut classediz)
 (declare (special *LANGUAGE*))
 (let (nomsuff accept advsuff 
       (explrad (reverse (explode radice))))
  (cond ((eq *LANGUAGE* 'italian)
          (setq advsuff '(m e n t e))
; *** abil+e ---> abil+mente ***********************
          (cond ((and (eq (first classeaut) 1)
	              (eq (first classediz) 2)
	              (member (first explrad) '(#\l #\r)))
	           (setq accept t)
	           (setq nomsuff nil))
; *** barbar+o ---> barbar+amente ******************
	        ((and (eq (first classeaut) 2)
	              (memq (first classediz) '(1 4 5 7)))
	           (setq accept t)
	           (setq nomsuff '(a)))
; *** temerar+io ---> temerar+iamente **************
	        ((and (eq (first classeaut) 3)
	              (memq (first classediz) '(3 6)))
	           (setq accept t)
	           (setq nomsuff '(i a)))
; *** fort+e ---> fort+emente *************************
	        ((and (eq (first classeaut) 4)
	              (eq (first classediz) 2)
	              (not (member (first explrad) '(#\l #\r))))
	           (setq accept t)
	           (setq nomsuff '(e)))))
        ((eq *LANGUAGE* 'french)
          (setq advsuff '(m e n t))
; *** contraire+/ ---> contraire+ment ***********************
          (cond ((and (eq (first classeaut) 1)
	              (eq (first classediz) 1))
	           (setq accept t)
	           (setq nomsuff nil))
; *** ancien ---> ancien+nement ******************
	        ((and (eq (first classeaut) 2)
	              (eq (first classediz) 2))
	           (setq accept t)
	           (setq nomsuff '(n e)))
; *** mauvais ---> mauvais+ement **************
	        ((and (eq (first classeaut) 3)
	              (eq (first classediz) 3))
	           (setq accept t)
	           (setq nomsuff '(e)))
; *** bas ---> bas+sement *************************
	        ((and (eq (first classeaut) 4)
	              (memq (first classediz) '(4 8)))
	           (setq accept t)
	           (setq nomsuff '(s e)))
; *** blanc ---> blanc+hement *************************
	        ((and (eq (first classeaut) 5)
	              (eq (first classediz) 7))
	           (setq accept t)
	           (setq nomsuff '(h e)))
; *** fau+x ---> fau+ssement *************************
	        ((and (eq (first classeaut) 6)
	              (eq (first classediz) 9))
	           (setq accept t)
	           (setq nomsuff '(s s e)))
; *** dou+x ---> dou+cement *************************
	        ((and (eq (first classeaut) 7)
	              (eq (first classediz) 10))
	           (setq accept t)
	           (setq nomsuff '(c e)))
; *** inqui+et ---> inqui+ètement *************************
	        ((and (eq (first classeaut) 8)
	              (eq (first classediz) 11))
	           (setq accept t)
	           (setq nomsuff '(è t e)))
; *** lég+er ---> lég+èrement *************************
	        ((and (eq (first classeaut) 9)
	              (eq (first classediz) 12))
	           (setq accept t)
	           (setq nomsuff '(è r e)))
; *** publi+c ---> publi+quement *************************
	        ((and (eq (first classeaut) 10)
	              (eq (first classediz) 15))
	           (setq accept t)
	           (setq nomsuff '(q u e)))
; *** tel ---> tel+lement *************************
	        ((and (eq (first classeaut) 11)
	              (eq (first classediz) 16))
	           (setq accept t)
	           (setq nomsuff '(l e)))
; *** tradition+nel ---> tradition+nellement *************************
	        ((and (eq (first classeaut) 12)
	              (eq (first classediz) 17))
	           (setq accept t)
	           (setq nomsuff '(n e l l e)))
; *** amic+al ---> amic+alement *************************
	        ((and (eq (first classeaut) 13)
	              (memq (first classediz) '(18 19)))
	           (setq accept t)
	           (setq nomsuff '(a l e)))
; *** nouve+au ---> nouve+llement *************************
	        ((and (eq (first classeaut) 14)
	              (eq (first classediz) 20))
	           (setq accept t)
	           (setq nomsuff '(l l e)))
; *** exig+ible ---> exig+iblement *************************
	        ((and (eq (first classeaut) 15)
	              (eq (first classediz) 21))
	           (setq accept t)
	           (setq nomsuff '(i b l e)))
; *** favor+able ---> favor+ablement *************************
	        ((and (eq (first classeaut) 16)
	              (eq (first classediz) 22))
	           (setq accept t)
	           (setq nomsuff '(a b l e)))
; *** long ---> long+uement *************************
	        ((and (eq (first classeaut) 17)
	              (eq (first classediz) 24))
	           (setq accept t)
	           (setq nomsuff '(u e)))
; *** object+if ---> object+ivement *************************
	        ((and (eq (first classeaut) 18)
	              (eq (first classediz) 25))
	           (setq accept t)
	           (setq nomsuff '(i v e)))))
        ((eq *LANGUAGE* 'english)
          (setq advsuff '(l y))
; *** full ---> full+y ***********************
          (cond ((and (eq (first classeaut) 7)
                      (eq (first classediz) 1)
                      (eq (first explrad) #\l)
                      (eq (second explrad) #\l))
	          (setq accept t)
                  (setq advsuff '(y))
	          (setq nomsuff nil))
; *** active ---> active+ly ******************
                ((and (eq (first classeaut) 1)
                      (eq (first classediz) 1)
                      (or (neq (first explrad) #\l)
                          (neq (second explrad) #\l)))
	          (setq accept t)
	          (setq nomsuff nil))
; *** air+y ---> air+ily *********************
                ((and (eq (first classeaut) 2)
	              (eq (first classediz) 2))
	          (setq accept t)
	          (setq nomsuff '(i)))
; *** abomin+able ---> abomin+ably ******************
                ((and (eq (first classeaut) 3)
	              (eq (first classediz) 3))
	          (setq accept t)
	          (setq nomsuff '(a b)))
; *** extens+ible ---> extens+ibly ******************
                ((and (eq (first classeaut) 4)
	              (eq (first classediz) 4))
	          (setq accept t)
	          (setq nomsuff '(i b)))
; *** vol+uble ---> vol+ubly ******************
                ((and (eq (first classeaut) 5)
	              (eq (first classediz) 5))
	          (setq accept t)
	          (setq nomsuff '(u b)))
; *** academ+ic ---> academ+ically **************
                ((and (eq (first classeaut) 6)
	              (memq (first classediz) '(6 7)))
	          (setq accept t)
	          (setq nomsuff '(i c a l))))))
  (cond (accept
          (list (list (implode (append
		                (mapcar #'char-upcase (explode radice))
		                nomsuff
		                advsuff))
		        'cat 'adv 'type 'manner))))))

; **************************************************************************
;     The next to skip the lemma in the search for feature values
;     In fact, the lemma could be equal to a feature name (e.g. number)
(defun without-lemma (wrd-info)
  (rest wrd-info))

; **************************************************************************
; *** this checks if all possible values of person-number codes are admitted
(defun all-p-n-val (pers-codes)
  (and (member 1 pers-codes)
       (member 2 pers-codes)
       (member 3 pers-codes)
       (member 4 pers-codes)
       (member 5 pers-codes)
       (member 6 pers-codes)))

; **************************************************************************
; *** this checks if all singular values of person-number codes are admitted
(defun sing-p-n-val (pers-codes)
  (and (member 1 pers-codes)
       (member 2 pers-codes)
       (member 3 pers-codes)))

; **************************************************************************
; *** this checks if all plural values of person-number codes are admitted
(defun pl-p-n-val (pers-codes)
  (and (member 4 pers-codes)
       (member 5 pers-codes)
       (member 6 pers-codes)
       (member 4 pers-codes)
       (member 5 pers-codes)
       (member 6 pers-codes)))

