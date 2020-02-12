(in-package "USER")

(defun tokenize (input)
 (let (word1 next-state tule-char arcs curr-arc result next-pause
       next-double-pause out-data prev-char temp-line errorflag)
  ; *** the data to tokenize are contained in the variable rem-line.
  ;     The file lines are read one at a time and put in temp-line.
  ;     Then, temp-line is converted into tule-chars and a "newline" is put at the end:
  ;           the result is put in rem-line
  ;     The characters in rem-line are inspected by the automaton; it may happen that:
  ;     1. an end of sentence is detected in the mid of the line.
  ;        In this case, the remaining chars are assumed to be in rem-line, and the
  ;        tokenizer proceeds on them after the POS tagging of the first part
  ;     2. the end of the line is reached before an end of sentence is found.
  ;        In this case, rem-line is empty, and is re-filled by reading a new line
  ;        The tokenizer proceeds from the beginning of the new line
  ;     3. An end of file is found. The analysis stops.
  ;     In case 1 and 2 occure at the same time, no special processing is needed,
  ;     since rem-line is empty and, after POS tagging, a new line is read.
   (declare (special word1 rem-line))
   (cond ((null input) (list 's-eof nil nil))
         (t (cond ((and (stringp input)
                        (null rem-line))
       ; *** if the input is a string and not a port, then we are working on a single
       ;     sentence, which is the string "input"
       ; *** but if rem-line is non-null, then in theinput string there were more
       ;     than one sentence, so the data (in tule-char format) are already present
       ;     in rem-line
                     (setq rem-line (append1 (line-to-tule-char input) 'escape))))
       ; *** Escape forces the final transition in the token automaton
           (do ((current-state 's-0 next-state)
	        (pause nil next-pause)
                (double-pause nil next-double-pause))
	       ((or errorflag
                    (eq current-state 's-eos)
	            (eq current-state 's-eof))
	          (progn
; *** if the current char has to be reused, but the analysis of the current
;     sentence is finished, then put it back in front of rem-line. It will
;     be found when the next sentence is analyzed
		   (cond (errorflag
                           (exception 'morpho-error
                                          "PROC/tokenizer: Empty line in string input"))
                         (pause (setq rem-line (cons tule-char rem-line)))
                         (double-pause (setq rem-line 
                                          (cons prev-char (cons tule-char rem-line)))))
		   (cons current-state (list (nreverse result) 
                                             (cond ((null (butlast rem-line)) nil)
                                                   (t (coerce (symbol-name 
                                                        (implode (butlast rem-line)))
                                                      'string)))))))
; *** the "rem-line" is returned for continuations (e.g. after siglas)
	       (cond ((null rem-line)
   ; *** rem-line could remain empty just in case we are working on a file input, so that
   ;     we must now move to the next line of the input file
   ; *** but if we are in a pause, do nothing, waiting for the next step (where the escape
   ;     should be analysed from state s-0)
                       (cond ((stringp input)
                               (cond (pause nil)
                                     (t (setq erroflag t))))
  ; *** the result of "read-line" is a string
  ;     the result of "line-to-tule-char" is a sequence of tule character names
  ; *** if the encoding is one-byte, each character in the string corresponds to one
  ;     tule char name, otherwise the number of tule char can be less than the
  ;     characters in the string
                             (t (setq temp-line (read-line input nil #\Escape))
         ;(break "tokenize: read-line")
                                (setq rem-line 
                                  (append1 (line-to-tule-char temp-line) 'newline))))))
  ; *** the sequence ﻿ specifies (I believe) that the file is encoded in utf-8
  ;     actually, it should appear only at the beginning of the file, not at the
  ;     beginning of each sentence, but repeating the check should not be
  ;     troublesome
               (cond ((eq (first rem-line) 'start-utf-8)
                        (setq rem-line (rest rem-line)))
                     ((and (eq (first rem-line) 'down-i-diaer)
                           (eq (second rem-line) 'closed-double-angle)
                           (eq (third rem-line) 'inv-question-mark))
	                (exception 'morpho-error
                                "Character encoding: this appears to be an utf-8 file")))
; *** if the previous transition has set "pause" to true, then "tule-char" and
;     "word1" remain unchanged
               (cond (double-pause
                       (setq tule-char prev-char)
                       (setq double-pause nil))
                     ((not pause)
                       (setq tule-char (first rem-line))
                       (setq rem-line (rest rem-line))))
	       (setq arcs (get current-state 'tok-arcs))
	       (setq curr-arc (tok-get-arc arcs tule-char))
               (cond ((eq curr-arc 'no-char) (setq errorflag t))
	             (t (setq next-state (second curr-arc))
	                (setq word1 (cond (pause word1)
			                  (t (cons tule-char word1))))
	                (cond ((eq (third curr-arc) '!)
		                 (setq next-pause t)
		                 (setq out-data (rest (rest (rest curr-arc)))))
                              ((eq (third curr-arc) '!!)
                                 (setq next-double-pause t)
                                 (setq out-data (rest (rest (rest curr-arc)))))
		              (t (setq next-pause nil)
		                 (setq out-data (rest (rest curr-arc)))))
                        (cond ((not (null out-data))
		                (setq result 
		                    (cons-nonil (tok-output out-data) result)))))))))))
   
; ****************************************************************************
; *** gets the automaton arc corresponding to "tule-char" out of "arclist"
(defun tok-get-arc (arclist tule-char)
   (cond ((null tule-char) 'no-char)
         ((null arclist)
; *** no arc applies: a default "failure" arc is returned. The next state
;       will be s-0. The character buffer is emptied, except for the offending
;       character [of course, the error could have occurred well before!!]
	    '(fail s-0 ((error 1))))
         (t (let ((key (first (first arclist))))
	        (cond ((or (eq key 'else)
                           (and (is-a-tule-char key)
                                (eq key tule-char))
	                   (member tule-char (get-charset-value key)))
	                 (first arclist))
	              (t (tok-get-arc (rest arclist) tule-char)))))))

;************************************************************
; *** a key of the tokenizer can be a char name or a char set name
;     it is a char-name if it has no associated charset-value
(defun is-a-tule-char (name)
    (null (get-charset-value name)))

; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;     JAVA VERSION
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
; The whole output of a text is called ^TokenizerFullAnalysis^
; It is a sequence (an array) of ^TokenizerItemAnalysis^
; Each of them is an ambiguity set (an array) of ^TokenizerInterpretation^
; Each of them is a sequence (an array) of ^TokenizerToken^
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;************************************************************
; *** out-data is a list where each element is a list of output elements
;     each top-level list represents an alternative interpretation of the
;     current portion of the input string (which is stored in the buffer
;     "word1") 
; *** The content of word1 is modified for each output; so, it has to be
;        saved and restored before each alternative
(defun tok-output (out-data)
  (declare (special word1))
  (let (save-word first-out)
      (setq save-word (copy-list word1))
      (setq first-out (single-tok-out (first out-data)))
; *** the next cond must decide if there is any other output to produce
;     however, the decision is taken on the basis of the existence of
;     something in the rest of "out-data". This does not work with the
;     "language" condition, since it may happen that nothing is done in
;     that case, so that "word1" continues to contain its characters.
;     The only way out seem to be that instead of checking if the rest
;     of out-data is null, it is checked if there is some clause which,
;     either does not contain the "language" condition or applies to the
;     current language. This duplicates a check that will anyway be made
;     afterwards
      (cond ((no-tok-out-applic (rest out-data))
               (cons-nonil first-out nil))
            (t (setq word1 (copy-list save-word))
               (cons-nonil first-out (tok-output (rest out-data)))))))

;************************************************************
(defun no-tok-out-applic (out-data)
  (declare (special *LANGUAGE*))
  (cond ((null out-data) t)
        ((eq 'language (first (first out-data)))
          (cond ((member *LANGUAGE* (inlist (second (first out-data))))
                  nil)
                (t (no-tok-out-applic (rest out-data)))))
        (t nil)))

;************************************************************
; *** single-out-data is made of one (or more) output elements. An example
;        where more than one output element is required is given by an input 
;        sequence as "boy."
; *** In this case, we must output the hypothesis where the sequence is 
;        formed by a GW followed by a punctuation mark (SEGNOINTER).
; *** This is encoded as ((GW 2) (SEGNOINTER 1)) in case the current
;        character is the one following the period (A character which must be
;        in the buffer word1).
; *** Note, however, that also a SIGLA interpretation is valid, as in "mr."
(defun single-tok-out (single-out-data)
  (declare (special *LANGUAGE*))
   (cond ((null single-out-data) nil)
         ((eq 'language (first single-out-data))
; *** this is output conditioned on language; for instance, we can have:
;     (language english (gw 2) (segnointer 1) (gw 0))
;     which is applied to handle genitives (John's).
           (cond ((member *LANGUAGE* (inlist (second single-out-data)))
                   (single-tok-out (rest (rest single-out-data))))
                 (t nil)))
; *** cons-nonil leaves the second argument unchanged if the first one is nil
;     defined in "utilities"
         (t (cons-nonil (tok-outelem (first single-out-data))
                        (single-tok-out (rest single-out-data))))))

(defun tok-outelem (outp-elem)
; *** The form of output elem is as follows:
;     1- (tok-categ rem)
;	 rem says how many characters must remain in the word1 buffer; all
;        characters taken out from word1 are output
;     2- (FLUSH rem)
;        as above (word1 is reduced), but without any output
;     3- (tok-categ @ char-code)
;        the output elements are all characters of word1 up to (but not
;        including) the first char equal to char-code
;     4- (tok-categ $ char-code)
;        the output elements are all characters of word1 up to (and
;        including) the first char equal to char-code
;     5- (tok-categ - char-code)
;        the output elements are all characters of word1 up to (but not
;        including) the first char different from char-code
;     6- (tok-categ rem = default)
;        as in 1, but with the chars in "default" sent in output in place
;        of the ones taken from word1
;     7- (tok-categ rem + append)
;	 as above, but with the chars in "append" sent in output together
;        with (appended at the end of) the ones taken from word1
;
;     it must be remembered that word1 contains the characters not yet used
;     for output in reversed order. An "outp-elem" of the form (gw 2) means that
;     all characters (except for the last two, i.e. the first two of word1)
;     must be included in the output. So, what I do is to transfer the first n
;     (2 in the example) characters from word1 to "savebuf", to output what has
;     remained in word1 (reversed), and finally to restore word1 with the
;     remaining n (2) characters for continuation
   (declare (special word1))
   (let ((tok-categ (first outp-elem))
	 (oper-code (second outp-elem))
         (extra (rest (rest outp-elem)))
         savebuf res control)
      (cond ((numberp oper-code) 
	      (setq res
		(do ((remainder-count (second outp-elem) (1- remainder-count)))
	    	    ((= remainder-count 0)
	       	      (cond ((eq tok-categ 'flush) nil)
		     	    (t (out-conversion (nreverse word1) tok-categ))))
	    	    (setq savebuf (append1 savebuf (first word1)))
	    	    (setq word1 (rest word1)))))
	    ((eq oper-code '@)
; *** output until equal (not inclusive)
;     In this branch, word1 need to be in the correct sequence (i.e. reversed
;     with respect to its standard format)
	      (setq control (first extra))
	      (setq word1 (nreverse word1))
	      (setq res
		(do ()
		    ((or (null word1)
			 (eq (first word1) control))
		      (cond ((null word1) 
			       (prog1 nil
				      (setq savebuf (nreverse savebuf))))
		      	    (t (prog1 (out-conversion savebuf tok-categ)
			              (setq savebuf (nreverse word1))))))
	    	    (setq savebuf (append1 savebuf (first word1)))
	    	    (setq word1 (rest word1)))))
	    ((eq oper-code '$)
; *** output until equal (inclusive)
;     In this branch, word1 need to be in the correct sequence (i.e. reversed
;     with respect to its standard format)
	      (setq control (first extra))
	      (setq word1 (nreverse word1))
	      (setq res
		(do ()
		    ((eq (first word1) control)
		      (prog1 (out-conversion (append1 savebuf control) tok-categ)
			     (setq savebuf (nreverse (rest word1)))))
	    	    (setq savebuf (append1 savebuf (first word1)))
	    	    (setq word1 (rest word1)))))
; *** output until different
;     In this branch, word1 need to be in the correct sequence (i.e. reversed
;     with respect to its standard format)
	    ((eq oper-code '-)
	      (setq control (first extra))
	      (setq word1 (nreverse word1))
	      (setq res
		(do ()
		    ((or (null word1) 
			 (not (eq (first word1) control)))
		      (prog1 (out-conversion savebuf tok-categ)
			     (setq savebuf (nreverse word1))))
	    	    (setq savebuf (append1 savebuf (first word1)))
	    	    (setq word1 (rest word1))))))
; *** treatment of character substitution and adjoining
      (cond ((null (rest extra)) nil)
	    ((eq (first extra) '=)
	      (setq res (out-conversion (second extra) tok-categ)))
	    ((eq (first extra) '+)
	      (setq res (out-conversion (append (first res) (second extra))
				    tok-categ)))
	    (t (exception 'morpho-error
                       "PROC/tokenizer: in tok-outelem" extra)))
; *** word1 is reset to the characters which have not been output
      (setq word1 savebuf)
      res))

(defun out-conversion (charlist tok-categ)
  (cond ((null charlist) nil)
        (t (list charlist tok-categ))))

