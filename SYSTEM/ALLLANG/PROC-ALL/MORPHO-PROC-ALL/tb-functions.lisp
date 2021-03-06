(in-package "USER")


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%%%%%%%% PREMISE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%% These functions provide access both to data in treebank %%%%%%%
; %%% format and to data in AVM format. This implies that most %%%%%%
; %%% functions work differently on the basis of the format. %%%%%%%%
; %%% In order to take apart the formatis, the global %%%%%%%%%%%%%%%
; %%% *TREE-FORMAT*, loaded in the main, is used (it can take the %%%
; %%% values "tut" or "avm") %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; *******************************************************************
;			TYPES (this section is for TUT)
;                       until the TUT-END marker
; *******************************************************************
; *** The functions in this file are based on a set of (implicitly)
;     defined types:
; %FILE-NAME% (a string)
; %STRING-W-DATA% (a string including data on a word, according to the
;	format of the file in the treebank), e.g.:
;	"23  educata  (EDUCATO ADJ QUALIF F SING) [21]   ??? VERB MAIN ??"
;	Each data line includes:
;	- a number (position of word in sentence)
;	- the input word or character (case-sensitive). This cannot be read
;	  by standard Lisp read macro, since it can be a special char
;	- a list of syntactic infos. This is in Lisp-readable format, so
;	  characters are preceded by \# and words including non-readable
;         chars (e.g. accents) are included in pipes. 
;         The list includes:
;	  -> The normalized form (e.g. infinite for verbs) of the word
;	  -> The syntactic category
;	  -> The syntactic subtype (sub-category)
;	  -> Other infos depending on category and subtype (e.g. tense for
;	     verbs)
;	- A specification of the parent(s) included in brackets. The possible
;	  formats are:
;     	  [n] : The head of the current word is word n
;         [n1, n2, ..., nk]: the current word includes k components, for each of
;               which the head is specified. 
;     	  [0+0]: The current word is the head of an "inciso" or "apposition"
;         Each of the ni above can be:
;         - h: a number (position of parent in sentence)
;     	  - [h.k]: The parent is the k-th subword of word h
; 	  In turn, the number h, in both formats, can possibly be
;         - [^u], which means that the word depends on an elliptical word,
;	    which, in turn, depends on word number u. (Ex. [^5, 3], [^7.1])
;	  - [@u], which says that the parent is a word of a preceding
;           sentence. More then one ampersand can appear:
;     	    Ex. [@5] --> (fifth word of the previous sentence)
;               [@@@14] --> (fourteenth word of 3 sentences before)
;	  - [@+u], which says that the parent is a word of a following
;           sentence. More then one ampersand can appear:
;           Ex. [@+6]  --> (sixth word of next sentence)
;               [@@+8]  --> (eighth word of the second next sentence)
;	  The caret may precede the ampersand(s):
;	  Ex. [^@5]: the word depends on the fifth word on the preceding
;	    	     sentence via an elliptical element
;       - A comment, which must be preceded by one or more question marks;
;	  Ex. ??? VERB
; %READABLE-W-DATA% (the data above expressed as a list of list, each
;     element of which, at any level, is a readable Lisp object). So the
;     list contains the following elements:
;	- The line number: an integer
;	- The input word: a lisp atom
;	- The syntactic infos: a lisp list
;	- A %TREEINFO%, as described below
;	- A possibly empty string of comments
; %TREEINFO% (a description of the parent(s) of the current word)
;     It is a list whose elements are integers or pairs of integers,
;     according to the following coding schema:
;	- Simple line number: the corresponding integer
;	- h.k: a list including the integers h and k
;	- ^h: the integer 1000+h
;	- @h: the integer 2000+h (where each @ adds 2000)
;	- @+h: the integer 2500+h (where each @ adds 2000)
;	 Examples:
;		[5]	 ---> (5)
;		[5, 4.1] ---> (5 (4 1))
;		[5, 3]   ---> (5 3)
;		[5.1]    ---> ((5 1))
;		[0+0]	 ----> ((0 0))
;		[^5, 3]   ---> (1005 3)
;		[^7.1]    ---> ((1007 1))
;		[@5]   ---> (2005)
;		[@@@14] ---> (6014)
;		[@+6]  ---> (2506)
;		[^@5]   ---> (3005)
; --- The same coding scheme is used for treeinfos including arclabels 
;     but with one more level of parentheses:
;		[{5;SUBJECT}]	 ---> ((5 SUBJECT))
;		[{5;SUBJECT}, {4.1;NBAR}] ---> ((5 SUBJECT) ((4 1) NBAR))
;		[{5.1;SUBJECT}]   ---> (((5 1) SUBJECT))
; %WORD-INDEX% (the identifier of a word in a sentence). It is as one
;     element of the list of parent infos described above (in readable
;     format (e.g. 5, (5 1), (0 0), (1007 1)))
; %COMPONENT-SPEC% (the description of a component of a word). It is a
;     pair whose first element (integer) is the position of the component 
;     inside a compound (0 if the word is not compound), and the second
;     element are the full word data (%READABLE-W-DATA%)
; %READABLE-SENT-DATA% (a list of %READABLE-W-DATA%, corresponding to the
;     words of a single sentence). Actually, it contains also a %SENT-ID%
;     at the beginning, and comment lines in their original position.
; %SENT-ID% (a string identifying a sentence)
;     it is composed of one or more asterisks, followed by a blank,
;     followed by "FRASE", followed by a blank, followed by the actual
;     sentence identifier, followed by another sequence of asterisks
;     The sentence identifier can be an integer or a form 'XXX-n', where
;     XXX is any sequence of alphabetic characters, and n is an integer
; %HEADING-LINE% (any string, but not beginning with the word "Totale")
;     Any string is accepted, except that a string starting with "Totale"
;     closes the heading section
; %END-OF-HEADING% (any string beginning with "Totale")
; %BASIC-TB-FILE% (a file containg TreeBank Data, in the basic format)
;     The records in the file are organized as follows:
;     1: 0 or more %HEADING-LINE%
;     2: 1 %END-OF-HEADING% line
;     3: 1 %SENT-ID% line
;     4: 1 or more %STRING-W-DATA% lines. These lines must be numbered
;        starting from 1 and with consecutive integers until the end of
;        the sentence.
;     5: any number of repetitions of the groups 3 and 4 above, one for
;        each sentence stored in the file
;     NB: blank lines and comment lines can appear everywhere in the file
; %BASIC-TB-FILE-IMAGE% (a list containing all the records - strings - of
;     a %BASIC-TB-FILE%)
; %BASIC-TB-SENT-IMAGE% (as above, but without the heading lines)
;				NEWTB
; %BASIC-NEWTB-FILE% (a file containg TreeBank Data, in the new format)
;     The records in the file are organized as follows:
;     1: 1 %SENT-ID% line
;     2: 1 or more %STRING-NEWTB-DATA% lines. These lines must be numbered
;        starting from 1 and with consecutive integers until the end of
;        the sentence.
;     3: any number of repetitions of the groups 1 and 2 above, one for
;        each sentence stored in the file
;     NB: blank lines and comment lines can appear everywhere in the file
; %STRING-NEWTB-DATA% (a string including data on a word, according to the
;     new format used in the final Treebank representation)
;	Each data line includes:
;	- a number (position of word in sentence) or a dotted pair of numbers
;    	  The dotted pairs appear for coding:
;	  * Components after the first one of compound words; ex:
;	    8 ai (A PREP MONO) [7;THEMECOMPL]
;	    8.1 ai (I ART DEF M PL) [8;PREPARG]
;	  * Traces; in this case, the integer after the dot is >= 10; ex
;	    7.10 t [] (t EMPTY) [7;SUBJ]   ??? pro-drop ???
;	- the input word or character. As in %STRING-W-DATA%
;	- An optional co-reference pointer of the form [Xp], where X is a
;	  line number; ex.
;	    11 che [10p] (CHE PRON RELAT LSUBJ+LOBJ) [13;SUBJ]
;	  where line 10 refers to the governor of the relative clause
;	- a list of syntactic infos. As in %STRING-W-DATA%
;	- A specification of the parent(s) included in brackets. The only
;	  possible format is:
;     	  [n;arclab]: The governor of the current word is word n, and the label
;	      of the arc linking the governor with the current word is 'arclab'
;	  n can assume one of the forms described in %STRING-W-DATA%
;       - A comment, as in %STRING-W-DATA%
; %READABLE-NEWTB-DATA% (the data above expressed as a list of list, each
;     element of which, at any level, is a readable Lisp object). So the
;     list contains the following elements:
;	- The line number: a %WORD-INDEX%
;	- The input word: a lisp atom
;	- The syntactic infos: a lisp list
;	- A %TREEINFO%; it is a pair whose first element is a %WORD-INDEX%
;	  and whose second element is an arclabel
;	- The coreference index: a %WORD-INDEX%
;	- A possibly empty string of comments

;*************************************************************************
;		TOP-LEVEL FUNCTIONS (ONLY FOR TUT)
;*************************************************************************
; read-tb-file (xinput)
    ; *** xinput is the %FILE-NAME% of a %BASIC-TB-FILE%
    ; *** returns the corresponding %BASIC-TB-FILE-IMAGE%
; init-flush (buffer)
    ; *** buffer is a %BASIC-TB-FILE-IMAGE%
    ; *** it returns a triple:
    ;	 <sent-id-of-first-sentence, list-of-heading-lines, list-of-other-lines>
    ;    list-of-other-lines is a %BASIC-TB-SENT-IMAGE%
; split-sentences (buff)
    ; *** buff is a %BASIC-TB-SENT-IMAGE%
    ; *** it returns the corresponding list, each element of which is a
    ;     %READABLE-SENT-DATA%
; interp-newtb-line (line)
    ; *** this converts a single input line concerning a word in the text
    ;     (a %STRING-NEWTB-DATA%) into the corresponding line in readable LISP
    ;     format (a %READABLE-NEWTB-DATA%)
; ******************* TUT-END *******************************************

;*************************************************************************
; *** What follow are the general comments and functions for the AVM format
;     (until the AVM-END marker)
;*************************************************************************
; *******************************************************************
;                       TYPES
; *******************************************************************
; *** The functions in this file provide access an AVM (attribute-value
;     matrix). It includes the following attributes:
;     - POSIT: the linear position in the input sentence
;          - WLINE: the word index. An integer associated with input forms
;          - WCOMP: the component index. It is nil in case of non-compound
;                   words or of first components of compound words. It is
;                   an integer N in other cases: if 1 <= N <= 9, it is the
;                   the identifier of the (N-1)-th component of a compound
;                   word; if N >= 10, it identifies a trace
;     - FORM: the input form of the word. A LISP atom that maintains
;             upper/lowercase distinctions
;     - SYN: the syntactic information, excluding the parsing data, i.e.
;            the links between words in the Dependency Parse Tree
;          - LEMMA: the citation form
;          - CAT: the syntactic category
;          - TYPE: the syntactic type (sub-category)
;          - GENDER: the syntactic gender (currently, just M or F)
;          - NUMBER: the syntactic number (SING or PL)
;          - PERSON: the syntactic person (1, 2 or 3)
;          - MOOD: the syntactic mood for verbs (ind, conj, participle,
;                  infinite, gerund)
;          - TENSE: the syntactic tense for verbs
;          - TRANS: the basic transitivity for verbs
;          - CASE: the case for pronouns
;          - V-DERIV: the verb from which a noun is derived (if any)
;          - V-TRANS: the transitivity of that verb
;          - CLITIC: t, if the word is a clitic
;          - LOCUTION: t, if the word is part of a locution
;     - TREE: the parsing data, i.e. the links between words in the
;             Dependency Parse Tree
;     - SEM: the semantic information
;          - SEMTYPE: the reference class for proper names
;                     the semantic relation for subordinative conjunctions
;                     the semantic reference for interrogative adverbials
;          - VALUE: the numerical value for numbers
;          - DAYHOUR: the hour of the day (for items of category HOUR)
;          - DAYMINUTE: the minute of a given hour of the day (for items of category HOUR)
;          - DAYSECOND: the second of a given minute of a given hour of the day 
;                       (for items of category HOUR)
;          - DAY: the weekday of dates
;          - MONTH: the month of dates
;          - YEAR: the year of dates
;     - COREF: the coreference information. Currently used just for traces
;          - CLINE: the line number of the coreferent
;          - CTYPE: the type of coreference



; ******************* AVM-END *******************************************


;*************************************************************************
;		INTERNAL FUNCTIONS
;*************************************************************************
; same-chars? (el test-ch &optional all?)
    ; checks whether the leading characters of an atom's name are equal to
    ;     "char". If the optional parameter "all?" is t, then all characters
    ;     must be equal to "char". If the test is satisfied, then the function
    ;     returns the remaining characters of the name (possibly t).
; no-dependents (dep-list)
    ; *** true if the only element in dep-list is #\#
; merge-dependents (up-deps down-deps)
    ; *** up-deps and down-deps are lists of depedendents (for instance
    ;     of an auxiliary and of the main verb).
    ; *** It returns a new list of dependents obtained by merging the two
    ;     lists: all of these are assumed to be cases of the main verb
; %%%%%%%%%%%%%%%%% THE NEXT FUNCTIONS ARE ONLY FOR TUT %%%%%%%%%%%%%%%%
; check-frase-i (wrd)
    ; *** true if the first five characters of wrd are "frase"
; extract-word (string-line)
    ; *** used in interp-newtb-line
    ; *** string-line should be a %STRING-W-DATA% without the line-number
; extract-treeinfo (string-line)
    ; *** used in interp-newtb-line
    ; *** string-line should be a piece of %STRING-W-DATA% starting after
    ;     syntinfo
; extract-comments (string-line)
    ; *** used in interp-line
    ; *** string-line should be a piece of %STRING-W-DATA% starting after
    ;     treeinfo
; extract-linenumb (string-line)
    ; *** used in interp-newtb-line
    ; *** string-line should be %STRING-NEWTB-DATA% 
; extract-coref (string-line)
    ; *** used in interp-newtb-line
    ; *** string-line should be piece of %STRING-NEWTB-DATA% , starting
    ;     after the input word
; is-newtb-gender (val)
    ; *** true if val is 'm or 'f
; is-newtb-number (val)
    ; *** true if val is 'sing or 'pl
; is-newtb-person (val)
    ; *** true if val is 1, 2 or 3
; %%%%%%%%%%%%%%%%% END OF FUNCTIONS ONLY FOR TUT %%%%%%%%%%%%%%%%

; %%%%%%%%%%%%%%%%% DEFINITION OF FUNCTIONS ONLY FOR TUT %%%%%%%%%%%%%%%%
;**********************************************************************
;**********************************************************************
; *** it reads a tb file as a set of strings (one for each record) and
;     puts it in a list (buffer)
; *** it receives in input the name of the input file
(defun read-tb-file (xinput)
 (let (buffer)
     (with-open-file (inputport (merge-pathnames xinput) :direction :input
                                     :if-does-not-exist :error)
      (do* ((line (read-line inputport nil #\Escape)
		  (read-line inputport nil #\Escape)))
	   ((equal line #\Escape) nil)
	   (setq buffer (cons (string-trim '(#\Return) line) buffer)))
      (reverse buffer))))

;**********************************************************************
; *** separates the heading lines of the buffer and the data (sentences)
;     lines. It returns a pair
;	 <list-of-heading-lines, list-of-other-lines>
;     - se trova una linea che inizia con Totale, seguita da una linea
;       con tre asterischi e "frase-i" esce regolarmente
;     - se trova la linea con tre asterischi e frase-i senza aver trovato
;       nella riga precedente la parola totale, segnala un warning ed esce.
;     - se trova una riga con tre asterischi non seguiti da frase-i fa
;       break e, al rientro, continua.
(defun init-flush (buffer)
; *** in buffer, all the lines of the file (a %BASIC-TB-FILE-IMAGE%)
  (let (possible-end firstel pos start-found prevline headings)
; *** loop on the lines of buffer. Exit at end of file or when the
;     beginning of the first line is found (start-found)
  (do ((line (car buffer) (car buffer))
       (buffer (cdr buffer) (cdr buffer)))
      ((or (null line) start-found)
	 (list (reverse headings)
	       (cons prevline (cons line buffer))))
; *** null lines are copied in the headings buffer
      (cond ((string= line "")
	      (setq headings (cons line headings)))
; *** otherwise, in firstel the first element of the line.
;     in pos the position of the first character following what has been read
            (t (multiple-value-setq (firstel pos) (read-from-string line))
; *** if the first element is "totale", then the operations could be
;     completed
              (cond ((equal firstel 'totale)
	     	      (setq headings (cons line headings))
		      (setq possible-end t))
	    	    ((is-sentence-heading line nil)
; *** if the line is a sentence heading, and the preceding line had "Totale"
;     in front, then we're done
		      (cond (possible-end
			      (setq prevline line)
		 	      (setq start-found t))
; *** if "frase-i" is found but it is not preceded by a %END-OF-HEADING%
;     then warning
		            (t (setq prevline line)
			      (setq start-found t))))
		    (t (setq headings (cons line headings)))))))))

;**********************************************************************
(defun check-frase-i (wrd)
   (let ((expl (first-n 5 (explode wrd))))
      (or (equal '(#\F #\R #\A #\S #\E) expl)
          (equal '(#\f #\r #\a #\s #\e) expl))))

;**********************************************************************
; *** takes from a sentence heading string the sentence id; 
;     if the lines does not begin with a sequence of asterisks, or
;     what follows them is not "frase" or "FRASE", then
;     it is assumed not to be a sentence heading, and the function
;     returns nil
; !!!!! P.S. THIS VERSION WORKS ONLY IS THERE ARE SPACES BETWEEN THE
;            ASTERISKS, "FRASE", THE SENT-ID, AND ANYTHING THAT FOLLOWS
; *** firstline? added to cope with markers of UTF8 encoding
(defun is-sentence-heading (line firstline?)
 (cond ((or (not (stringp line))
	    (string= (string-trim '(#\Space #\Tab #\Return) line) ""))
   ; *** if the line is not a string or it is the empty line, nil
	  nil)
       (t (let (firstel pos frase-i)
              (multiple-value-setq (firstel pos) (read-from-string line))
   ; *** otherwise, take the first element of the line
              (cond ((and firstline?  (not (init-same-chars? firstel #\*)))
                        nil)
      ; *** if this is the first line of the file and we do not have an
      ;     initial sequence of asterisks, possibly preceded by the leading
      ;     chars of the character encoding scheme, then it is not a heading line
     	            ((and (not firstline?) (not (same-chars? firstel #\*)))
      ; *** if this is not the first line of the file and we do not have an
      ;     initial sequence of asterisks, then it is not a heading line
                        nil)
      ; *** the first element is a sequence of asterisks
                    (t
	     	     (setq line (subseq line pos))
	     	     (multiple-value-setq (frase-i pos)
			     (read-from-string line))
	     	     (cond ((check-frase-i frase-i)
      ; *** and the second element is "frase" or "FRASE"
		      	     (read-from-string (subseq line pos)))
		   	   (t nil))))))))

;**********************************************************************
; *** it gets a %STRING-NEWTB-DATA% and produces a %READABLE-NEWTB-DATA%
; *** ok-no-notreeinfo is t in case treeinfo data are not required (output of 
;     the tagger, without parse). If ok-no-treeifo=nil, then extract-treeinfo
;     breaks if they are missing
(defun interp-newtb-line (line &optional ok-no-treeinfo)
    (let (linenumb pos word syntinfo comments treeinfo coref saveline)
       (setq saveline line)
       (multiple-value-setq (linenumb pos) (extract-linenumb line 'actnumb))
       (setq line (my-subseq line pos))
       (multiple-value-setq (word pos) (extract-word line))
       (setq line (my-subseq line pos))
       (multiple-value-setq (coref pos) (extract-coref line))
       (setq line (my-subseq line pos))
       (multiple-value-setq (syntinfo pos) (read-from-string line))
       (setq line (my-subseq line pos))
       (multiple-value-setq (treeinfo pos) 
                        (extract-treeinfo line ok-no-treeinfo))
       (setq comments (extract-comments (my-subseq line pos)))
; *** the coref is inserted at the end of the list (before the comments)
;     to maintain the position of the other elements (and the access
;     functions) the same as in %READABLE-W-DATA%
       (list linenumb word syntinfo treeinfo coref comments)))

; *********************************************************************
(defun my-subseq (string pos)
   (cond ((>= pos (length string)) "")
         (t (subseq string pos))))

; *********************************************************************
; *** the same as above, but this is used to read the output of the tagger
;     so it does not care of COREF and structural links (TREEINFO)
;     of the tagger
(defun interp-tb-line (line)
   (let (linenumb pos word syntinfo comments)
   ; *** the line number can be N or N.M (where N and M are integers). In the 
   ;     output, the second case results in a pair (N M). The standard LISP 
   ;     reader cannot be used, since in the second case it would return a
   ;     real number
	(multiple-value-setq (linenumb pos) (extract-linenumb line 'actnumb))
   ; *** the word must be read as a character sequence, since it can include
   ;     any special character (except spaces)
	(setq line (subseq line pos))
	(multiple-value-setq (word pos) (extract-word line))
   ; *** the syntactic infos are already a readable LISP list
	(setq line (subseq line pos))
	(multiple-value-setq (syntinfo pos) (read-from-string line))
   ; *** the comments must be preceded by ?; a string is returned
	(setq line (subseq line pos))
	(setq comments (extract-comments line))
   ; *** the fourth element is nil, because it corresponds to trace coreference
   ;     infos, which are not present in the tagger
	(list linenumb word syntinfo nil comments)))

;**********************************************************************
; *** this function extracts the first characters of a line assuming they are
;     a word.
; *** Actually, not only words must be recognized, but also any special symbol,
;     as a comma, or a parenthesis. So, the function searches for any sequence
;     of characters, and then returns the sequence and its length
(defun extract-word (line)
   (let ((inpu (make-string-input-stream line))
	(end nil) (res nil) (begin t))
     (do ((char (read-char inpu) (read-char inpu))
	  (count 0 (1+ count)))
	 (end (values (implode res) count))
	 (cond (begin
; *** at the beginning skip any leading blanks; if not blank start accumulating
;     the word
		  (cond ((not (char= char #\Space))
			  (setq begin nil)
			  (setq res (append1 res char)))))
; *** if not at the beginning continue accumulating the word, unless a space
;     or an open parenthesis is found
		((and (not (char= char #\Space))
		      (not (char= char #\()))			; )
		  (setq res (append1 res char)))
; *** when a space or an open parenthesis is found (not at the beginning)
;     close the loop
		(t (setq end t))))))

;**********************************************************************
; *** this function extracts the line number from a line
; *** The function returns N if the line number is the integer N,
;     (N1 N2), if the line number has the form N1.N2
; *** 'count' points to the first character after the line number
(defun extract-linenumb (line callt)
   (let ((inpu (make-string-input-stream line))
	 (status 'begin) res first)
     (do ((char (read-char inpu) (read-char inpu))
	  (count 0 (1+ count)))
	 ((eq status 'end)
	    (cond ((null first)
		    (values (list-to-number (mapcar #'char-code res)) 
			    (1- count)))
		  (t (values (list first 
			    (list-to-number (mapcar #'char-code res))) 
			    (1- count)))))
	 (cond ((eq status 'begin)
; *** at the beginning skip any leading blanks; if not blank start accumulating
;     the word
		  (cond ((not (char= char #\Space))
			  (setq status 'first)
			  (setq res (append1 res char)))))
; *** if not at the beginning, a blank means end of the line number
		((or (and (char= char #\Space) (eq callt 'actnumb))
		     (and (char= char #\;) (eq callt 'treeinf))
		     (and (member char '(#\p #\f #\w)) (eq callt 'coref)))
		    (setq status 'end))
; *** a dot means that the line number includes two parts. Save the first part
;     in 'first', and go on
		((char= char #\.)
		  (cond ((eq status 'first)
		    	  (setq first (list-to-number (mapcar #'char-code res)))
		    	  (setq res nil)
		    	  (setq status 'second))
			(t (exception 'generic-error
                                   "PROC/tb-functions: extract-linenumb"))))
		(t (setq res (append1 res char)))))))

;**********************************************************************
; *** this function extracts the coreference index (if any)
; OUTPUT: 
;   nil, if there is no coreference index
;   'empty if the form of the index is [] (usually used for traces)
;   (linenumber code) if there is a coreference index, where
;       linenumb is a line number [n oppure (m n)]
;	code is #\p, #\f, or #\w
(defun extract-coref (line)
   (let ((inpu (make-string-input-stream line))
	 (status 'begin) res (size 0) code)
; *** status can be 'begin, 'start, or 'end
     (do ((char (read-char inpu) (read-char inpu))
	  (count 0 (1+ count)))
	 ((eq status 'end) (values res count))
; *** at the beginning, there can be one or more spaces, which are skipped,
;     and then an open square bracket (there is a coreference index) or an
;     open parenthesis (no coref index); otherwise error.
	 (cond ((eq status 'begin)
	 	(cond ((not (char= char #\Space))
	    		(cond ((char= char #\[) 
				(setq status 'start))
		  	      ((char= char #\() 	;)
				(setq status 'end)
				(setq count (1- count)))
   ; *** the char can be ", in case the word was not recognized: "Does not exist"
		  	      ((char= char #\") 	;"
				(setq status 'end)
				(setq count (1- count)))
                              (t (exception 'generic-error "PROC/tb-functions: extract-coref"))))))
; *** the linenumber is read using the function extract-linenumb
	       ((eq status 'start)
       		 (setq line (subseq line count))
		 (cond ((char= char #\])
		 	  (setq count (+ 1 count))
			  (setq res 'empty)
		 	  (setq status 'end))
		       (t (multiple-value-setq (res size)
					       (extract-linenumb line 'coref))
; *** it returns the linenumber and its length (in number of characters)
		 	  (setq count (+ count size))
       		 	  (setq line (subseq line size))
; *** it advances on the line
   		 	  (setq inpu (make-string-input-stream line))
; *** and reads the code
		 	  (setq code (read-char inpu))
		 	  (cond ((member code '(#\p #\f #\w))
		 	  	  (setq res (list res code)))
		       		(t (exception 'generic-error "PROC/tb-functions: extract-coref 1")))
		 	  (setq char (read-char inpu))
		 	  (cond ((not (char= char #\]))
		          	  (exception 'generic-error "PROC/tb-functions: extract-coref 2")))
; *** count is increased by 2 (the code, and the closed bracket)
		 	  (setq count (+ 2 count))
		 	  (setq status 'end))))))))

; *******************************************************************
; *** it extracts the pointer to the parent from a line; 
; *** it returns NIL if no treeinfo is present (treeinfo= [])
(defun extract-treeinfo (line &optional ok-no-treeinfo)
   (let ((inpu (make-string-input-stream line))
	 (status 'beg)
	 semicol label explab res size)
   (do ((char (read-char inpu nil #\Escape) (read-char inpu nil #\Escape))
	(count 0 (1+ count)))
       ((or (eq status 'end)
	    (char= char #\Escape))
	  (cond ((null res) (values nil count))
	  	(t (values (list res label) count))))
       (cond ((eq status 'beg)
; *** at the beginning, look for the open bracket
		(cond ((char= char #\Space)
			 (setq line (my-subseq line 1)))
		      ((char= char #\[)
			 (setq status 'start)
			 (setq line (my-subseq line 1)))
		      (ok-no-treeinfo (values nil (1- count)))
		      (t (exception 'generic-error
                                       "PROC/tb-functions: extract-newtb-treeinfo 1"))))
	     ((eq status 'start)
	       (cond ((char= char #\])
			(setq res nil)
			(setq status 'end))
		     (t (multiple-value-setq (res size) 
				     (extract-linenumb line 'treeinf))
; *** it returns the linenumber and its length (in number of characters)
			(setq count (+ count size))
       			(setq line (subseq line size))
; *** it advances on the line
   			(setq inpu (make-string-input-stream line))
; *** and reads the semicolon
			(setq semicol (read-char inpu))
			(cond ((char= semicol #\;)
				(setq label (read inpu))
; *** the 'read includes also the closed bracket in the label; it must be
;     taken out and checked
				(setq explab (reverse (explode label)))
				(setq count (+ count (length explab)))
				(setq status 'end)
				(cond ((char= (first explab) #\])
					(setq label (implode 
						(reverse (rest explab)))))
			      	      (t (exception 'generic-error 
                                            "PROC/tb-functions: extract-newtb-treeinfo 2"))))))))))))

; *****************************************************************
; *** this function extracts the comments from a line
;     They can be absent, and, when present, start with a sequence of
;     question marks
(defun extract-comments (line)
  (let (qmark pos)
    (cond ((string= (string-trim '(#\Space #\Tab #\Return) line) "") nil)
	  (t (multiple-value-setq (qmark pos) 
				  (read-from-string line nil #\Escape))
; *** if qmark is null, there are no elements in "line": no comments
    	    (cond ((null qmark) nil)
		  ((char= qmark #\Escape) nil)
	  	  ((same-chars? qmark #\?)
	     	     (subseq line pos))
	  	  (t (exception 'generic-error 
                         "PROC/tb-functions: Something after treeinfo, but no question marks")))))))

; %%%% END OF DEFINITION OF FUNCTIONS ONLY FOR TUT %%%%%%%%%%%%%%%%%%%%%%%%%%

;**********************************************************************
; *** checks whether the leading characters of an atom's name are equal to
;     "char". If the optional parameter "all?" is t, then all characters
;     must be equal to "char". If the test is satisfied, then the function
;     returns the remaining characters of the name (possibly t).
(defun init-same-chars? (el test-ch &optional all?)
   (declare (special *CHAR-SET-ID*))
   (let ((explel (explode el))
         (lead-chars (get *CHAR-SET-ID* 'leading-id-chars))
         init-match restchar)
   ; *** lead-chars is a list of tule char names of the characters that identify the
   ;     encoding scheme (e.g. for UTF-8, it is (down-i-diaer closed-double-angle 
   ;     inv-question-mark), i.e. "﻿")
        (cond ((eq (char-code (first explel)) #xFEFF)
                 (setq init-match 'ok)
                 (setq restchar (rest explel)))
              (t (setq lead-chars 
                     (mapcar #'code-char 
                            (convert-tule-char-names-to-base-codes 
                                   (mapcar #'get-char-upperc lead-chars))))
               (cond ((char= (first explel) (first lead-chars))
          ; *** if the leading chars of the encoding scheme are present, flush them
	                (do ((char (second explel) (first restexpl))
	     	             (restexpl (rest (rest explel)) (rest restexpl))
	                     (l-char (second lead-chars) (first rest-lchars))
	     	             (rest-lchars (rest (rest lead-chars)) (rest rest-lchars)))
	    	     ((or (null char) (null l-char) (not (char= l-char char)))
		 	       (cond ((null l-char) 
                                (setq restchar restexpl)
                                       (setq init-match 'ok))
			             (t (setq init-match 'fail))))))
                     (t (setq init-match 'ok)
                        (setq restchar explel)))))
	(cond ((eq init-match 'ok)
   ; *** at least one occurrence of the required char must be present
	        (cond ((not (char= (first restchar) test-ch)) nil)
	              (t (do ((char (second restchar) (first restchar))
	     	              (restchar (rest (rest restchar)) (rest restchar)))
	    	             ((or (null char) (not (char= test-ch char)))
		 	        (cond ((null char) t)
			              (t (cond (all? nil)
			                       (t (implode (cons char restchar)))))))))))
              (t nil))))

;**********************************************************************
; *** checks whether the leading characters of an atom's name are equal to
;     "char". If the optional parameter "all?" is t, then all characters
;     must be equal to "char". If the test is satisfied, then the function
;     returns the remaining characters of the name (possibly t).
(defun same-chars? (el test-ch &optional all?)
   (let ((explel (explode el)))
	(cond ((not (char= (car explel) test-ch)) nil)
	      (t (do ((char (cadr explel) (car restexpl))
	     	      (restexpl (cddr explel) (rest restexpl)))
	    	     ((or (null char) (not (char= test-ch char)))
		 	(cond ((null char) t)
			      (t (cond (all? nil)
			               (t (implode (cons char explel))))))))))))

; *****************************************************************
;			UTILITIES
; *****************************************************************

; *******************************************************************
; *** returns true if the line line1 precedes immediately (in the linear
;     order in the sentence) the line line2
;     or the two lines are two components of the same word
(defun tb-cf-adjacent (line1 line2 allines)
  (or (same-linumb (get-synt-numb line1) (get-synt-numb line1))
      (int-tb-cf-adjacent line1 line2 allines)))

(defun int-tb-cf-adjacent (line1 line2 allines)
  (cond ((null allines) (exception 'generic-error "PROC/tb-functions: tb-cf-adjacent"))
	((same-synt-line line1 (first allines))
	   (or (same-synt-line line2 (second allines))
    ; *** the next condition is necessary to handle cases as "lo si vede" where
    ;     the object clitic is separated from the verb by another clitic
	       (and (same-synt-line line2 (third allines))
		    (eq (get-synt-type (second allines)) 'refl-impers))))
	(t (int-tb-cf-adjacent line1 line2 (rest allines)))))

; *******************************************************************
; *** finds the word which precedes the head
(defun find-preceding (head dependent prev succ)
   (let ((dep-line (get-synt-numb dependent))
	 (head-line (get-synt-numb head)))
; *** if the dependent precedes the head, then the word before the head must occur
;     in 'succ' (i.e. after the dependent), otherwise, in 'prev'
; *** the dependent is 'cons-ed' to cover the case where they are adjacent
	(cond ((index-precedes dep-line head-line)
		 (pick-prevword head-line (cons dependent succ)))
	      (t (pick-prevword head-line (reverse (cons dependent prev)))))))

; *******************************************************************
; *** finds the words which follow the argument
; *** this is simpler than the previous one, since it assumes that "line" is
;     in "succ"
; *** it is used in "skip-lines", to skip noun sequences
(defun find-successors (line succ)
  (cond ((null succ) (exception 'morpho-error "PROC/tb-functions: find-successors"))
        ((equal line (first succ)) (rest succ))
        (t (find-successors line (rest succ)))))

; *******************************************************************
; *** finds the first auxiliary of a main verb, or, if none, returns the verb
;     itself; the lines in prev must be reversed
(defun find-first-aux (word prev)
   (cond ((null prev) word)
	 ((eq (get-synt-type (first prev)) 'AUX)
	    (find-first-aux (first prev) (rest prev)))
	 ((or (eq (get-synt-categ (first prev)) 'ADV)
	      (eq (get-synt-word (first prev)) #\")        ; "
	      (eq (get-synt-word (first prev)) #\'))
	    (find-first-aux word (rest prev)))
	 (t word)))
 
; *******************************************************************
; *** establishes whether ind1 comes before ind2 in the sentence
(defun index-precedes (ind1 ind2)
     (cond ((and (numberp ind1) (numberp ind2))
	      (< ind1 ind2))
           ((and (numberp ind1) (not (numberp ind2)))
	      (<= ind1 (first ind2)))
           ((and (not (numberp ind1)) (numberp ind2))
	      (< (first ind1) ind2))
           (t (or (< (first ind1) (first ind2))
		  (and (= (first ind1) (first ind2))
                       (< (second ind1) (second ind2)))))))

; *******************************************************************
; *** establishes whether ind1 comes immediately before ind2 in the sentence
;     actually, there could be other lines between in case of compounds
(defun index-prevline (ind1 ind2)
     (cond ((and (numberp ind1) (numberp ind2))	; not compounds
	      (= ind1 (1- ind2)))		; some continuation of ind1 could
                                                ; be present: 10 10.1 11
           ((and (numberp ind1) (not (numberp ind2)))	; the second is compound
	      (or (= ind1 (first ind2))		; there could be: 10 10.1 10.2
	          (= ind1 (1- (first ind2)))))  ; certainly not adjacent
           ((and (not (numberp ind1)) (numberp ind2))
	      (= (first ind1) (1- ind2)))
           (t (or (= (first ind1) (1- (first ind2)))
		  (and (= (first ind1) (first ind2))
                       (< (second ind1) (second ind2)))))))

; *****************************************************************************
; *** returns t if the two line numbers refer to same word or to components
;     of the same word:
;     3 3 --> t
;     3 (3 1) --> t
;     (3 1) (3 2) --> t
(defun same-linumb (ind1 ind2)
  (cond ((numberp ind1)
  	   (cond ((numberp ind2) (= ind1 ind2))
  	         (t (= ind1 (first ind2)))))
  	(t (cond ((numberp ind2) (= (first ind1) ind2))
  	         (t (= (first ind1) (first ind2)))))))

; *******************************************************************
; *** returns the line immediately before 'index' in 'lines'
;     "lines" are some lines in standard order
(defun pick-prevword (index lines)
   (do* ((line (first lines) (first lines))
	 (lines (rest lines) (rest lines))
	 (succline (first lines) (first lines)))
        ((or (equal (get-synt-numb succline) index)
	     (null succline))
	  (cond ((null succline) nil)
		(t line)))))

; ***************************************************************************
; *** this takes the item that linearly precedes a given word
;     prevlines is a list of lines in reverse order
(defun find-line-before (line prevlines)
  (cond ((null prevlines) nil)
        ((equal line (first prevlines)) (second prevlines))
        (t (find-line-before line (rest prevlines)))))

;**********************************************************************
(defun no-dependents (dep-list)
  (null (remove-daughter dep-list 'CONJ 'COORD)))

;**********************************************************************
; *** loop on the lines of the sentence until the line number is found or
;     the sentence ends
; ## INPUT:
;  >>> line: the line number (an integer, independent of the component)
;  >>> comp: the component number (0, if a non-compound word)
;  >>> fras: the whole sentence
;  >>> sent-id: the sentence identifier
; ## OUTPUT:
;  >>> a pair, whose first element is the component number, and the second
;      is the line
(defun find-component (line comp fras sent-id)
   (do ((nxtw (first fras) (first fras))
        (fras (rest fras) (rest fras)))
       ((and (listp nxtw)
	     (or (null nxtw)
		 (equal (get-synt-numb nxtw) line)))
	 (cond ((null nxtw)
	 	 (exception 'morpho-error 
                     "PROC/tb-functions: Component not found (Sentence number, word):"
                     sent-id comp))
; *** if the index refers to a simple component, but the line is compound
;     or vice-versa, there is a compoundness mismatch
	       ((or (and (= 0 comp)
			 (listp (get-synt-numb nxtw)))
		    (and (> 0 comp)
			 (atom (get-synt-numb nxtw))))
	 	 (exception 'morpho-error 
                     "PROC/tb-functions: Compound mismatch  (Sentence number, word):"
		     sent-id nxtw))
; *** otherwise, build and return the %COMPONENT-SPEC%
	       (t (list comp nxtw))))))

; *******************************************************************
; *** finds all lines (within the lines 'allines', which need not be
;     all the lines of a sentence, but could be a piece of it) that
;     have a link pointing to 'headline'
; *** among the dependents there is the (#\#) element, marking the
;     position of the head
;(defun find-dependents-old (headline alllines alllinks)
; (let ((headnumb (get-synt-numb headline))
;       deps deplinks)
;  (do ((nxtline (first alllines) (first alllines))
;       (alllines (rest alllines) (rest alllines))
;       (nxtlink (first alllinks) (first alllinks))
;       (alllinks (rest alllinks) (rest alllinks)))
;     ((null nxtline) (list deps deplinks))
;  ; *** if a line points to the head or is the head, add infos,
;  ;     otherwise do nothing
;     (cond ((equal (first nxtlink) headnumb)
;             (setq deps (append1 deps nxtline))
;             (setq deplinks (append1 deplinks nxtlink)))
;           ;((equal nxtline headline)
;           ;  (setq deps (append1 deps '(#\#)))
;           ;  (setq deplinks (append1 deplinks '(-1))))
;           ))))

; *******************************************************************
; *** differs from the previous one just because it does not include
;     the #\# marker of the position of the head
; *** finds all lines (within the lines 'allines', which need not be
;     all the lines of a sentence, but could be a piece of it) that
;     have a link pointing to 'headline'
(defun find-dependents (headline alllines alllinks)
 (let ((headnumb (get-synt-numb headline))
       deps deplinks)
  (do ((nxtline (first alllines) (first alllines))
       (alllines (rest alllines) (rest alllines))
       (nxtlink (first alllinks) (first alllinks))
       (alllinks (rest alllinks) (rest alllinks)))
     ((null nxtline) (list deps deplinks))
  ; *** if a line points to the head or is the head, add infos,
  ;     otherwise do nothing
     (cond ((equal (first nxtlink) headnumb)
             (setq deps (append1 deps nxtline))
             (setq deplinks (append1 deplinks nxtlink)))))))

; *********************************************************************
; *** given a list of dependents and a line, returns the dependent that is
;     immediately before "line". If line is not among the dependents,
;     returns NIL. If the preceding dep is not a noun or a prepositional
;     group also returns NIL
(defun find-previous-dep (alldeps line)
   (let ((prevdep (first (rest (member line (reverse alldeps))))))
       (cond ((null prevdep) nil)
             ((equal prevdep '(#\#)) nil)
             ((member (get-synt-categ prevdep) '(ART NOUN PREP)) prevdep)
             ((and (eq (get-synt-categ prevdep) 'adj)
                   (memq (get-synt-type prevdep) '(INDEF POSS DEITT DEMONS))) prevdep)
             (t nil))))

; *******************************************************************
(defun same-locution (line1 line2)
   (and (is-a-synt-locution line1)
        (is-a-synt-locution line2)
        (eq (get-synt-word line1)
            (get-synt-word line2))))

; *******************************************************************
; *** a trace is identified by t in the word position. The remaining
;     conditions serve to avoid that a standard T lexical entry (e.g.
;     a name) or a marker is taken as a trace
; *** traces having the "form" #\^ are inserted during semantic interpretation
(defun is-a-newtb-trace? (wdata)
   (and (or (eq t (second wdata))
            (eq '|t| (second wdata))
            (eq #\t (second wdata)))
  ; *** the next to prevent a single 't' (perhaps a typing error), which
  ;     has as its third "Does not exist" from producing an error
	(listp (third wdata))
        (neq t (first (third wdata)))
        (neq '|t| (first (third wdata)))
        (neq #\t (first (third wdata)))
        (neq 'marker (second (third wdata)))
        (neq 'special (second (third wdata)))))

; *******************************************************************
(defun is-a-newtb-clitic? (wdata)
   (member 'clitic (get-newtb-syntinfo wdata)))

; *******************************************************************
; *** a trace is identified by t in the word position. The remaining
;     conditions serve to avoid that a standard T lexical entry (e.g.
;     a name) or a marker is taken as a trace
; *** traces having the "form" #\^ are inserted during semantic interpretation
(defun is-a-flatavm-trace? (wdata)
   (and (member (get-flatavm-inpword wdata) '(t |t| #\t #\^))
  ; *** the next to prevent a single 't' (perhaps a typing error), which
  ;     has as its third "Does not exist" from producing an error
        (listp (get-flatavm-syntinfo wdata))
        (not (member (get-flatavm-word wdata) '(t |t| #\t)))
        (neq 'marker (get-flatavm-categ wdata))))

; *******************************************************************
(defun is-a-flatavm-clitic? (wdata)
    (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'clitic))

; *******************************************************************
(defun is-a-cf-lobj-clitic? (wdata)
; *** returns true if wdata is a clitic in the LOBJ case, but not a 
;     reflexive-impersonal 'si'
   (and (is-a-synt-clitic? wdata)
	(eq (get-synt-cases wdata) 'LOBJ)
	(neq (get-synt-type wdata) 'REFL-IMPERS)))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%% FUNCTIONS FOR WORKING ON THE TUT FORMAT %%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun is-newtb-gender (val)
   (memq val '(m f n)))

; *****************************************************************
(defun is-newtb-number (val)
   (memq val '(sing pl)))

; *****************************************************************
(defun is-newtb-person (val)
   (and (numberp val)
        (or (= val 1) (= val 2) (= val 3))))

; *****************************************************************
(defun same-newtb-line (wdata1 wdata2)
    (equal wdata1 wdata2))

; *******************************************************************
; *** returns the line number of a word
(defun get-newtb-numb (wdata) (first wdata))

; *******************************************************************
; *** this is as the previous, but if the line number is non-atomic (e.g.
;     coming from 15.1, i.e. (15 1)), it returns just the line and not the
;     component number (15)
(defun get-newtb-linumb (wdata) 
  (cond ((numberp (first wdata)) (first wdata))
        (t (first (first wdata)))))

; *******************************************************************
(defun get-newtb-syntinfo (wdata)
   ; *** the third of wdata could not be a list in case of morphological
   ;     failure in which case it is something as "Does not exist"
  (cond ((listp (third wdata)) (third wdata))
        (t nil)))

; *******************************************************************
(defun get-newtb-inpword (wdata)
  (second wdata))

; *******************************************************************
(defun get-newtb-word (wdata)
  (first (get-newtb-syntinfo wdata)))

; *******************************************************************
(defun get-newtb-categ (wdata)
  (second (get-newtb-syntinfo wdata)))

; *******************************************************************
(defun get-newtb-type (wdata)
  (third (get-newtb-syntinfo wdata)))

; *******************************************************************
(defun get-newtb-number (wdata)
 (let ((word (get-newtb-syntinfo wdata)))
  (cond ((eq (second word) 'verb)
          (cond ((is-newtb-number (sixth word)) (sixth word))
                    ; *** the previous one for participles lacking the transitivity
                ((is-newtb-number (seventh word)) (seventh word))
                ((is-newtb-number (eighth word)) (eighth word))
                ((is-newtb-number (ninth word)) (ninth word))
                (t nil)))
        ((memq (second word) '(adj art))
          (fifth word))
        ((eq (second word) 'predet)
          (fourth word))
        ((eq (second word) 'noun)
	  (cond ((eq 'proper (third word)) 'allval)
          	(t (fifth word))))
        ((eq (second word) 'pron)
          (cond ((is-newtb-number (fifth word)) (fifth word))
                ((is-newtb-number (sixth word)) (sixth word))
                ((is-newtb-number (seventh word)) (seventh word))
                (t nil)))
        ((eq (second word) 'num)
	  (cond ((eq (third word) 1) 'sing)
; *** 'third word' is the numerical value
		(t 'pl)))
        (t nil))))

; *******************************************************************
(defun get-newtb-gender (wdata)
 (let ((word (get-newtb-syntinfo wdata)))
; *** it returns the syntactic gender of the component
; *** the position of the "gender" information is not uniquely
;     determined, so that it has to be found according to the
;     category of the entry and its possible other data
  (cond ((eq (second word) 'verb)
          (cond ((is-newtb-gender (sixth word)) (sixth word))
                    ; *** the previous one for participles lacking the transitivity
                ((is-newtb-gender (seventh word)) (seventh word))
                ((is-newtb-gender (eighth word)) (eighth word))
                ((is-newtb-gender (ninth word)) (ninth word))
                (t nil)))
        ((memq (second word) '(adj noun art))
          (fourth word))
        ((eq (second word) 'predet)
          (third word))
        ((eq (second word) 'pron)
          (cond ((is-newtb-gender (fourth word)) (fourth word))
                ((is-newtb-gender (sixth word)) (sixth word))
                ((is-newtb-gender (seventh word)) (seventh word))
                (t nil)))
        (t nil))))

; *******************************************************************
(defun get-newtb-person (wdata)
 (let ((word (get-newtb-syntinfo wdata)))
  (cond ((eq (second word) 'verb)
          (cond ((is-newtb-person (fifth word)) (fifth word))
                    ; *** the previous one for participles lacking the transitivity
                ((is-newtb-person (sixth word)) (sixth word))
                ((is-newtb-person (seventh word)) (seventh word))
                ((is-newtb-person (eighth word)) (eighth word))
                ((is-newtb-person (ninth word)) (ninth word))
                (t nil)))
        ((eq (second word) 'noun) 3)
        ((eq (second word) 'pron)
          (cond ((is-newtb-person (fourth word)) (fourth word))
                ((is-newtb-person (sixth word)) (sixth word))
                ((is-newtb-person (seventh word)) (seventh word))
                (t nil)))
        (t nil))))

; *******************************************************************
(defun get-newtb-mood (wdata)
; *** it returns the syntactic mood of wdata (which must be a verb)
 (let ((word (get-newtb-syntinfo wdata)))
  (cond ((eq (second word) 'verb) (fourth word))
	(t (exception 'generic-error
                      "PROC/tb-functions: Mood taken from a non-verbal component")))))

; *******************************************************************
(defun get-newtb-tense (wdata)
; *** it returns the syntactic mood of wdata (which must be a verb)
 (let ((word (get-newtb-syntinfo wdata)))
  (cond ((eq (second word) 'verb) (fifth word))
	(t (exception 'generic-error 
                      "PROC/tb-functions: Tense taken from a non-verbal component")))))

; *******************************************************************
(defun get-newtb-cases (wdata)
; *** if the component refers to a pronoun, it returns the "cases" of the
;     component (e.g. lsubj+lobj)
 (let ((word (get-newtb-syntinfo wdata)))
  (cond ((eq (second word) 'pron)
	  (do ((nextinfo (first word) (first word))
	       (next2info (second word) (second word))
	       (word (rest word) (rest word)))
	      ((or (null next2info)
                   (eq next2info 'locution)
                   (eq next2info 'clitic))
                  nextinfo)))
	(t nil))))

; *******************************************************************
(defun get-newtb-corefinfo (wdata) (fifth wdata))

; *******************************************************************
(defun get-newtb-semtype (wdata)
; *** it returns the semantic subtype of a conj subord, a prep poli, a pron
;     relat (es. dove), a proper noun or an adv component; it returns nil for proper
;     nouns not appearing in the lexicon
 (cond ((or (and (eq 'conj (get-newtb-categ wdata))
		 (memq (get-newtb-type wdata) '(coord subord)))
            (and (eq 'prep (get-newtb-categ wdata))
	     	 (eq 'poli (get-newtb-type wdata)))
            (and (eq 'pron (get-newtb-categ wdata))
	     	 (eq 'relat (get-newtb-type wdata)))
            (and (eq 'adv (get-newtb-categ wdata))))
	  (fourth (get-newtb-syntinfo wdata)))
       ((and (eq 'noun (get-newtb-categ wdata))
	     (eq 'proper (get-newtb-type wdata)))
	  (sixth (get-newtb-syntinfo wdata)))
       (t nil)))

; *******************************************************************
(defun get-newtb-subcat (wdata)
; *** it returns the verbal subcategorization classes of the component
 (let ((word (get-newtb-syntinfo wdata)))
  (cond ((eq (second word) 'VERB)
          (cond ((memq (sixth word) '(trans intrans refl))
                  (sixth word))
                (t nil)))
	(t (exception 'generic-error
                      "PROC/tb-functions: Transitivity taken from a non-verbal component")))))

; *******************************************************************
(defun get-newtb-value (wdata)
; *** it returns the value of the component (a number)
 (let ((word (get-newtb-syntinfo wdata)))
  (cond ((eq (second word) 'NUM) (third word))
	(t (exception 'generic-error
                      "PROC/tb-functions: Value taken from a non-number component")))))

; ***************************************************************
(defun get-condit-newtb-value (line)
  (cond ((eq 'num (get-newtb-categ line))
           (get-newtb-value line))
        (t -1)))

; *******************************************************************
(defun get-newtb-year (wdata)
; *** it returns the year of the component (a date)
 (let ((word (get-newtb-syntinfo wdata)))
  (cond ((eq (second word) 'date) (third word))
	(t (exception 'generic-error
                      "PROC/tb-functions: Year taken from a non-date component")))))

; *******************************************************************
(defun get-newtb-vderiv (wdata)
; *** it returns the verb from which the noun is derived; nil otherwise
  (cond ((eq (get-newtb-categ wdata) 'NOUN) 
           (cond ((not (null (get-newtb-vtrans wdata))) 
                    (sixth (get-newtb-syntinfo wdata)))
                 (t nil)))
	(t (exception 'generic-error
                      "PROC/tb-functions: V-deriv taken from a non-nominal component"))))

; *******************************************************************
(defun get-newtb-vtrans (wdata)
; *** it returns the transitivity of the verb from which the noun is derived; nil otherwise
 (let (vtrans)
  (cond ((eq (get-newtb-categ wdata) 'NOUN) 
           (setq vtrans (seventh (get-newtb-syntinfo wdata)))
           (cond ((memq vtrans '(trans intrans refl)) vtrans)
                 (t nil)))
	(t (exception 'generic-error
                      "PROC/tb-functions: V-trans taken from a non-nominal component")))))

; *******************************************************************
(defun get-newtb-dayhour (wdata)
; *** it returns the hour of day from a line of category HOUR
 (let ((word (get-newtb-syntinfo wdata)))
  (cond ((eq (second word) 'HOUR) (third word))
	(t (exception 'generic-error
                      "PROC/tb-functions: Dayhour taken from a non-hour component")))))

; *******************************************************************
(defun get-newtb-dayminute (wdata)
; *** it returns the minute of a given hour of day from a line of category HOUR
 (let ((word (get-newtb-syntinfo wdata)))
  (cond ((eq (second word) 'HOUR) (fourth word))
	(t (exception 'generic-error
                      "PROC/tb-functions: Dayminute taken from a non-hour component")))))

; *******************************************************************
(defun get-newtb-daysecond (wdata)
; *** it returns the second of a given minute of a given hour of day from a line of category HOUR
 (let ((word (get-newtb-syntinfo wdata)))
  (cond ((eq (second word) 'HOUR) (fifth word))
	(t (exception 'generic-error
                      "PROC/tb-functions: Daysecond taken from a non-hour component")))))

; *******************************************************************
(defun change-newtb-type (wdata newtype)
  (let ((synt (get-newtb-syntinfo wdata)))
       (cond ((null synt) wdata)
             (t (cons (first wdata)
                     (cons (second wdata)
                           (cons (cons (first synt)
                                       (cons (second synt)
                                             (cons newtype (nthcdr 3 synt))))
                                 (nthcdr 3 wdata))))))))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%% FUNCTIONS FOR WORKING ON THE AVM FORMAT %%%%%%%%%%%%%%%%%%%%%%
; %%%% These are basic functions for the "FLAT" AVM format %%%%%%%%%%%%%%%%%
; %%%%%%% see PARSER-PROC-ALL/avm-transf.lisp for details on the formats %%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; *******************************************************************
; *** checks if two lines are the same line. This is done by comparing
;     the line numbers, the input form and the lemma. I assume that further
;     checks on the full avm are not needed
(defun same-flatavm-line (wdata1 wdata2)
  (and (equal (get-flatavm-numb wdata1) (get-flatavm-numb wdata2))
       (equal (get-flatavm-inpword wdata1) (get-flatavm-inpword wdata2))
       (equal (get-flatavm-word wdata1) (get-flatavm-word wdata2))))

; ***************** general function for extracting a value **********
(defun get-flatavm-feat-val (wdata attr)
   (first (leggi wdata attr)))

; ***************** general function for replacing a value **********
(defun replace-flatavm-feat-val (wdata attr newval)
   (cond ((null wdata) (list (list attr newval)))
         ((eq (first (first wdata)) attr)
           (cons (list attr newval) (rest wdata)))
         (t (cons (first wdata) (replace-flatavm-feat-val (rest wdata) attr newval)))))

; ******************** first level **********************************
; *** returns the line number of a word
(defun get-flatavm-numb (wdata) 
   (get-flatavm-feat-val wdata 'posit))

; *******************************************************************
; *** this is as the previous, but if the line number is non-atomic (e.g.
;     coming from 15.1, i.e. (15 1)), it returns just the line and not the
;     component number (15)
(defun get-flatavm-linumb (wdata) 
  (let ((pos (get-flatavm-numb wdata)))
    (cond ((numberp pos) pos)
          (t (first pos)))))

; *******************************************************************
(defun get-flatavm-inpword (wdata)
  (get-flatavm-feat-val wdata 'form))

; *******************************************************************
(defun get-flatavm-syntinfo (wdata)
   ; *** the value of SYN could not be a list in case of morphological
   ;     failure in which case it is something as "Does not exist"
  (let ((synt (get-flatavm-feat-val wdata 'syn)))
     (cond ((listp synt) synt)
           (t nil))))

; *******************************************************************
(defun get-flatavm-seminfo (wdata)
  (get-flatavm-feat-val wdata 'sem))

; *******************************************************************
(defun get-flatavm-corefinfo (wdata)
  (get-flatavm-feat-val wdata 'coref))

; ******************** second level (syntax) *************************
; *******************************************************************
(defun get-flatavm-word (wdata)
  (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'lemma))

; *******************************************************************
(defun get-flatavm-categ (wdata)
  (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'cat))

; *******************************************************************
(defun get-flatavm-type (wdata)
  (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'type))

; *******************************************************************
(defun change-flatavm-type (wdata newtype)
   (let ((syntdata (get-flatavm-feat-val wdata 'syn)))
       (cond ((null syntdata) wdata)
             (t (replace-flatavm-feat-val wdata 'syn 
                      (replace-flatavm-feat syntdata 'type newtype))))))

; *******************************************************************
(defun get-flatavm-number (wdata)
  (cond ((eq (get-flatavm-categ wdata) 'num)
	  (cond ((eq (get-flatavm-value wdata) 1) 'sing)
		(t 'pl)))
        (t (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'number))))

; *******************************************************************
(defun get-flatavm-gender (wdata)
  (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'gender))

; *******************************************************************
(defun get-flatavm-person (wdata)
  (cond ((eq (get-flatavm-categ wdata) 'noun) 3)
        (t (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'person))))

; *******************************************************************
(defun get-flatavm-mood (wdata)
; *** it returns the syntactic mood of wdata (which must be a verb)
  (cond ((eq (get-flatavm-categ wdata) 'verb)
           (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'mood))
	(t (exception 'generic-error
                      "PROC/tb-functions: Mood taken from a non-verbal component (avm)"))))

; *******************************************************************
(defun get-flatavm-tense (wdata)
; *** it returns the syntactic tense of wdata (which must be a verb)
  (cond ((eq (get-flatavm-categ wdata) 'verb)
           (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'tense))
	(t (exception 'generic-error
                      "PROC/tb-functions: Tense taken from a non-verbal component (avm)"))))

; *******************************************************************
(defun get-flatavm-subcat (wdata)
; *** it returns the verbal subcategorization classes of the component
  (cond ((eq (get-flatavm-categ wdata) 'verb)
           (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'trans))
	(t (exception 'generic-error
                      "PROC/tb-functions: Transitivity taken from a non-verbal component (avm)"))))

; *******************************************************************
(defun get-flatavm-cases (wdata)
; *** if the component refers to a pronoun, it returns the "cases" of the
;     component (e.g. lsubj+lobj)
  (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'case))

; *******************************************************************
(defun get-flatavm-vderiv (wdata)
; *** it returns the verb from which the noun is derived; nil otherwise
  (cond ((eq (get-flatavm-categ wdata) 'noun)
           (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'v-deriv))
	(t (exception 'generic-error
                      "PROC/tb-functions: V-deriv taken from a non-nominal component (avm)"))))

; *******************************************************************
(defun get-flatavm-vtrans (wdata)
; *** it returns the transitivity of the verb from which the noun is derived;
;     nil otherwise
  (cond ((eq (get-flatavm-categ wdata) 'noun)
           (get-flatavm-feat-val (get-flatavm-syntinfo wdata) 'v-trans))
	(t (exception 'generic-error
                      "PROC/tb-functions: V-trans taken from a non-nominal component (avm)"))))

; ******************** second level (semantics) **********************
; *******************************************************************

(defun get-flatavm-lexmean (wdata)
; *** it returns the lexical meaning of the component
  (get-flatavm-feat-val (get-flatavm-seminfo wdata) 'lexmean))

; *******************************************************************
(defun get-flatavm-semtype (wdata)
; *** it returns the semantic subtype of a word; the latter is defined
;     for: conj subord, prep poli, pron relat (es. dove) and proper noun
;     s appearing in the lexicone 
  (get-flatavm-feat-val (get-flatavm-seminfo wdata) 'semtype))

; *******************************************************************
(defun get-flatavm-value (wdata)
; *** it returns the value of the component (a number)
  (cond ((eq (get-flatavm-categ wdata) 'num)
           (get-flatavm-feat-val (get-flatavm-seminfo wdata) 'value))
	(t (exception 'generic-error
                      "PROC/tb-functions: Value taken from a non-numerical component (avm)"))))

; ***************************************************************
(defun get-condit-flatavm-value (line)
  (cond ((eq 'num (get-flatavm-categ line))
           (get-flatavm-value line))
        (t -1)))

; *******************************************************************
(defun get-flatavm-year (wdata)
; *** it returns the value of the component (a number)
  (cond ((eq (get-flatavm-categ wdata) 'date)
           (get-flatavm-feat-val (get-flatavm-seminfo wdata) 'year))
	(t (exception 'generic-error
                      "PROC/tb-functions: Year taken from a non-date component (avm)"))))

; *******************************************************************
(defun get-flatavm-dayhour (line)
; *** it returns the hour of day from a line of category HOUR
  (cond ((eq (get-flatavm-categ line) 'HOUR)
           (get-flatavm-feat-val (get-flatavm-seminfo line) 'dayhour))
	(t (exception 'generic-error
                      "PROC/tb-functions: dayhour taken from a non-hour component"))))

; *******************************************************************
(defun get-flatavm-dayminute (line)
; *** it returns the minute of a given hour of day from a line of category HOUR
  (cond ((eq (get-flatavm-categ line) 'HOUR)
           (get-flatavm-feat-val (get-flatavm-seminfo line) 'dayminute))
	(t (exception 'generic-error
                      "PROC/tb-functions: dayminute taken from a non-hour component"))))

; *******************************************************************
(defun get-flatavm-daysecond (line)
; *** it returns the second of a given minute of a given hour of day from a line of category HOUR
  (cond ((eq (get-flatavm-categ line) 'HOUR)
           (get-flatavm-feat-val (get-flatavm-seminfo line) 'daysecond))
	(t (exception 'generic-error
                      "PROC/tb-functions: daysecond taken from a non-hour component"))))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %%%%%%%%%%% FUNCTIONS FOR TAKING APART TUT FORMAT AND AVM FORMAT %%%%%%%%%
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun same-synt-line (line1 line2)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (same-newtb-line line1 line2))
	(t (same-flatavm-line line1 line2))))

; *** added on Oct 28, 2008 to build a line
;     this is currently used just in "link-conjunctions" to substitute a comma
;     with a dummy conjunction. So, only the semtype field is needed beyond
;     category and type. If more attributes are needed it, must be suitably extended
(defun make-synt-line (pos form lemma categ type semtype)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
          (list pos form (list lemma categ type semtype)))
        (t `((posit ,pos) 
             (form ,form) 
             (syn ((lemma ,lemma) (cat ,categ) (type ,type) (semtype ,semtype))))))) 
  
; *** added on Nov 28, 2009 to replace the lemma. Used for character encoding
(defun set-synt-lemma (line lemma)
  (declare (special *TREE-FORMAT*))
  (let (templine)
    (cond ((eq *TREE-FORMAT* 'tut)
            (list (first line)				; position
                  (second line)				; input word
                  (cons lemma (rest (third line)))	; syntinfo
                  (fourth line)				; treeinfo
                  (fifth line)				; corefinfo
                  (sixth line)))			; comments
          (t (setq templine 
                 (list (list 'posit (get-flatavm-numb line)) 
                       (list 'form (get-flatavm-inpword line))
                       (list 'syn (cons (list 'lemma lemma)
                                    (remove-flatavm-lemma (get-flatavm-syntinfo line))))
                       (list 'sem (get-flatavm-seminfo line))))
             (cond ((not (null (get-flatavm-corefinfo line)))
                      (setq templine 
                         (append1 templine (list 'coref (get-flatavm-corefinfo line))))))
             templine))))
 
(defun remove-flatavm-lemma (syntinfo)
  (cond ((null syntinfo) (exception "tb-functions: remove-flatavm-lemma"))
        ((eq (first (first syntinfo)) 'lemma)
           (rest syntinfo))
        (t (cons (first syntinfo) (remove-flatavm-lemma (rest syntinfo))))))

(defun get-synt-type (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-type line))
	(t (get-flatavm-type line))))

(defun change-synt-type (line newtype)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (change-newtb-type line newtype))
	(t (change-flatavm-type line newtype))))

(defun get-synt-numb (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-numb line))
	(t (get-flatavm-numb line))))

(defun get-synt-linumb (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-linumb line))
	(t (get-flatavm-linumb line))))

(defun get-synt-categ (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-categ line))
	(t (get-flatavm-categ line))))

(defun get-synt-word (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-word line))
	(t (get-flatavm-word line))))

(defun get-synt-inpword (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-inpword line))
	(t (get-flatavm-inpword line))))

(defun get-synt-syntinfo (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-syntinfo line))
	(t (get-flatavm-syntinfo line))))

(defun get-synt-number (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-number line))
	(t (get-flatavm-number line))))

(defun get-synt-gender (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-gender line))
	(t (get-flatavm-gender line))))

(defun get-synt-person (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-person line))
	(t (get-flatavm-person line))))

(defun get-synt-tense (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-tense line))
	(t (get-flatavm-tense line))))

(defun get-synt-mood (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-mood line))
	(t (get-flatavm-mood line))))

(defun get-synt-subcat (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-subcat line))
	(t (get-flatavm-subcat line))))

(defun get-synt-vderiv (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-vderiv line))
	(t (get-flatavm-vderiv line))))

(defun get-synt-vtrans (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-vtrans line))
	(t (get-flatavm-vtrans line))))

(defun get-synt-value (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-value line))
	(t (get-flatavm-value line))))

(defun get-condit-synt-value (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-condit-newtb-value line))
	(t (get-condit-flatavm-value line))))

(defun get-synt-year (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-year line))
	(t (get-flatavm-year line))))

(defun get-all-cases (line)
  (expl+cats (explode (get-synt-cases line)) #\+))

(defun get-synt-cases (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-cases line))
	(t (get-flatavm-cases line))))

(defun get-synt-semtype (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-semtype line))
	(t (get-flatavm-semtype line))))

(defun get-synt-posit (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-numb line))
	(t (get-flatavm-numb line))))

(defun get-synt-corefinfo (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-corefinfo line))
	(t (get-flatavm-corefinfo line))))

(defun get-synt-corefpos (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (exception 'generic-error "PROC/tb-functions: get-synt-corefinfo used for TUT"))
	(t (get-flatavm-feat-val (get-flatavm-corefinfo line) 'line))))

(defun get-synt-coreftype (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (exception 'generic-error "PROC/tb-functions: get-synt-corefinfo used for TUT"))
	(t (get-flatavm-feat-val (get-flatavm-corefinfo line) 'line))))

(defun get-synt-seminfo (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (exception 'generic-error "PROC/tb-functions: get-synt-seminfo used for TUT"))
	(t (get-flatavm-seminfo line))))

(defun is-a-synt-locution (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (member 'locution (get-newtb-syntinfo line)))
	(t (get-flatavm-feat-val (get-flatavm-syntinfo line) 'locution))))

(defun is-a-synt-trace? (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
          (is-a-newtb-trace? line))
	(t (is-a-flatavm-trace? line))))

(defun is-a-synt-clitic? (line)
; *** returns true if wdata is a clitic
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
          (is-a-newtb-clitic? line))
        (t (is-a-flatavm-clitic? line))))

(defun get-synt-dayhour (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-dayhour line))
	(t (get-flatavm-dayhour line))))

(defun get-synt-dayminute (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-dayminute line))
	(t (get-flatavm-dayminute line))))

(defun get-synt-daysecond (line)
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (get-newtb-daysecond line))
	(t (get-flatavm-daysecond line))))

