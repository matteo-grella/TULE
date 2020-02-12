(In-package "USER")

;**********************************************************************
; *** This file contains the programs for comparing the automatic parse
;     result with a manually parsed (or corrected) file; it includes the
;     following sections and functions
; ****** READ INPUT DATA AND CONVERSION IN INTERNAL FORMAT ************
; --> compare-parses ()
; --> parse-single-f (tbf parsedf)
; ****** MAIN COMPARISON FUNCTION 
; ****** PRINTING ON OUTPUT FILE **************************************
; --> print-data-line (line outport error)

;**********************************************************************
; ****** READ INPUT DATA AND CONVERSION IN INTERNAL FORMAT
;**********************************************************************
; ***  It parses the sentences present in one or more files
(defun compare-parses ()
 (let (answ xautinput xmaninput xoutpt xerr filelist)
    (format t " ***** COMPARISON ******************** ~% ~%")
    (format t " Do you want to carry out the work on all the files of the tagged corpus? (y/n)~%")
    (setq answ (checkanswer '(y n)))
    (cond ((eq answ 'n)
	    (format t "~% Name of the input file containing the automatic data (between quotation marks) ~%")
	    (format t "     Home Directory: *HOME-DIR*/DATI ~%")
	    (setq xautinput (merge-pathnames (read)))
	    (format t "~% Name of the input file containing the manual data (between quotation marks) ~%")
	    (format t "     Home Directory: *HOME-DIR*/DATI ~%")
	    (setq xmaninput (merge-pathnames (read)))
; *** merge-pathnames is a system function
	    (setq xoutpt (merge-pathnames (change-extens xautinput ".cmp")))
	    (setq xerr (merge-pathnames (change-extens xautinput ".err")))
	    (setq xdis (merge-pathnames (change-extens xautinput ".dis")))
	    (setq xtag (merge-pathnames (change-extens xautinput ".tag")))
	    (compparse-single-f xautinput xmaninput xoutpt xerr xtag xdis))
	  (t (with-open-file (iport (build-file-name "DATI/tagcorpus.dat")
				:direction :input :if-does-not-exist :error)
	    (setq filelist (read iport))
	    (dolist (filen filelist)
                (format t " ~%  $$$$$$$$$$$$$ Next file: ~a $$$$$$$ ~%" filen)
		(compparse-single-f 
			(change-extens filen ".prs")
			(change-extens filen ".man")
			(change-extens filen ".cmp")
			(change-extens filen ".err")
			(change-extens filen ".tag")
			(change-extens filen ".dis")
                        )))))))

;**********************************************************************
; ***  It compares the parse results present in two files
;      It reads the file and stores the lines in 'buff' and 'data'in readable format
; ***  buff is organized as a list including all the lines; also non-data lines
;       are included; buff is used just for data output
; ***  data is organized as a two-dimensional array. Each line is associated with
;	a sentence, whose columns are the words. Non-data lines are skipped
;      Then the content of 'data' is parsed and the result is written on the
;	output file
;   autf is the name of the automatically parsed file
;   manf is the name of the manually parsed (or manually corrected) file
;   outf is the name of the output file
;   disf is the file where the statistics of the application of the tag-rules
;        are stored
; *** the optional noparse is t in case the function is used to compare a .prs file
;     with a .man file, starting from a manually corrected .tb file. However, even
;     in this case, a .dis file is required (in case the data come from a previous
;     parsing phase). The "noparse" variable tells the function to ignore possible
;     misaligments between the .tb file and the .dis file
(defun compparse-single-f (autf manf outf errf tagf disf &optional noparse)
 (with-open-file (autport autf :direction :input
                               :if-does-not-exist :error)
  (with-open-file (manport manf :direction :input
                                :if-does-not-exist :error)
   (with-open-file (outport outf :direction :output
                                 :if-exists :overwrite
                                 :if-does-not-exist :create)
    (with-open-file (tagport tagf :direction :output
                                  :if-exists :overwrite
                                  :if-does-not-exist :create)
     (with-open-file (disport disf :direction :input
                                  :if-does-not-exist :error)
    (format t "PARSER EVALUATION")
    (act-compparse autport manport outport errf tagport disport noparse)))))))

; ************************************************************************
(defun act-compparse (autport manport outport errf tagport disport noparse)
    (let ((link-errors 0) (trace-errors 0) (punct-errors 0) (syntinfo-errors 0)
	  (label-errors 0) (line-count 0) (trace-count 0) (puncts 0) (sent-count 0)
          (extra-line-errors 0) tag-rules (sent-grouping 5) (prep-errors 0)
   ; *** prep-errors added (4/8/2008) to collect statistics about prespositional attachment
          tot-extral-err stopaut stopman autfileheader autsentnumb manfileheader
          mansentnumb tot-err tot-link-err tot-prep-err tot-label-err tot-trace-err tot-punct-err
          tot-syntinfo-err tot-line-count tot-trace-count tot-puncts stopheader stopdis
          auttracebuff mantracebuff trace-result tag-rule-data (misalcount 0) (firstline? t))
	 (declare (special label-errors link-errors prep-errors trace-errors punct-errors 
		   syntinfo-errors extra-line-errors line-count trace-count
                   puncts misalcount firstline?))
    (setq tag-rule-data
      (do ((autline (read-line autport nil #\Escape)
                    (cond (stopaut autline)
			  (t (read-line autport nil #\Escape))))
           (manline (read-line manport nil #\Escape)
                    (cond (stopman manline)
                          (t (read-line manport nil #\Escape))))
           (disline (triple-read-line disport nil #\Escape)
                    (cond (stopdis disline)
                          (t (triple-read-line disport nil #\Escape))))
           (stoppedman nil (cond (stopman t) (t nil))))
   ; *** if stopaut or stopman are true, then do not advance on the respective file
          ((or (equal autline #\Escape) (equal manline #\Escape))
	     (print-parseresult errf sent-count sent-grouping 
			(append1 tot-line-count line-count)
			(append1 tot-trace-count trace-count)
			(append1 tot-puncts puncts)
			(append1 tot-err (+ link-errors label-errors syntinfo-errors))
			(append1 tot-link-err link-errors)
			(append1 tot-trace-err trace-errors)
			(append1 tot-extral-err extra-line-errors)
			(append1 tot-punct-err punct-errors)
			(append1 tot-syntinfo-err syntinfo-errors)
			(append1 tot-label-err label-errors)
			(append1 tot-prep-err prep-errors))
             tag-rules)
   ;(format t "MAN: ~s~%AUT: ~s~%    dis: ~s~%" manline autline disline)
; *** process lines ***********
      (cond ((equal disline #\Escape)
               (cond ((and (or (not (equal manline "")) (not (equal autline "")))
                           (not noparse))
                        (format t "Fine file dis in Eval-parse ~%")
                        (format t "Autline: ~s~%" autline)
                        (format t "Manline: ~s~%" manline)
                        (break "")))))
      (setq stopaut nil)
      (setq stopman nil)
      (setq stopdis nil)
      (let ((intmanline (loc-interp-newtb-line manline firstline? 'man))
	    (intautline (loc-interp-newtb-line autline firstline? 'aut)))
       (multiple-value-setq (autfileheader autsentnumb) 
			(is-sentence-heading autline firstline?))
       (multiple-value-setq (manfileheader mansentnumb) 
			(is-sentence-heading manline firstline?))
; *** a new sentence is found; update the statistics
       (setq firstline? nil)
   ; *** firstline? identifies the first line of the file, where a set leading chars
   ;     can be present to identify the character encoding scheme
       (cond ((and manfileheader (not stopheader))
	      (setq sent-count (1+ sent-count))
	      (cond ((and (= 1 (rem sent-count sent-grouping))
		          (not (= 1 sent-count)))
		      (setq tot-err (append1 tot-err 
				(+ link-errors label-errors syntinfo-errors)))
		      (setq tot-link-err (append1 tot-link-err link-errors))
		      (setq link-errors 0)
		      (setq tot-prep-err (append1 tot-prep-err prep-errors))
		      (setq prep-errors 0)
		      (setq tot-trace-err (append1 tot-trace-err trace-errors))
		      (setq trace-errors 0)
		      (setq tot-extral-err (append1 tot-trace-err extra-line-errors))
		      (setq extra-line-errors 0)
		      (setq tot-punct-err (append1 tot-punct-err punct-errors))
		      (setq punct-errors 0)
		      (setq tot-label-err (append1 tot-label-err label-errors))
		      (setq label-errors 0)
		      (setq tot-syntinfo-err 
			    (append1 tot-syntinfo-err syntinfo-errors))
		      (setq syntinfo-errors 0)
		      (setq tot-line-count (append1 tot-line-count line-count))
		      (setq line-count 0)
		      (setq tot-trace-count 
			    (append1 tot-trace-count trace-count))
		      (setq trace-count 0)
		      (setq tot-puncts (append1 tot-puncts puncts))
		      (setq puncts 0))))
   ; *** not a new sentence: update line count and, possibly trace and punct
	     ((and (not (stringp intmanline))
                   (not stoppedman))
	       (setq line-count (1+ line-count))
               (cond ((and 
			 (not (null (second (fourth intmanline))))
                         (null (spl (first (expl+cats (explode (second (fourth intmanline))) #\*))))
                                         ; *** the previous one to avoid the signalling of *TITLE labels
                         (not (member (second (fourth intmanline))
                                   '(VERB-OBJ/VERB-SUBJ VERB-SUBJ/VERB-INDCOMPL-AGENT 
                                     VERB-SUBJ/VERB-SUBJ+IMPERS NOUN-SUBJ/NOUN-INDCOMPL-AGENT))))
                       ; *** NOUN-SUBJ/NOUN-INDCOMPL-AGENT for "da parte di"
                       (format t " Non-existent label: ~a~%" 
                                 (second (fourth intmanline)))))
	       (cond ((is-a-synt-trace? intmanline)
		      (setq trace-count (1+ trace-count))
                      (setq stopdis t))
		     ((eq 'PUNCT (get-newtb-categ intmanline))
		       (setq puncts (1+ puncts))))))
; *** STARTING ACTUAL COMPARISON
       (cond ((eq-line intautline intmanline)
; *** the lines are equal; simply write it in the output file
;     if we were blocked on a header, we can now restart the standard header
;     processing
               (setq stopheader nil)
	       (prs-format outport "~a~%" manline)
               (cond ((or (equal intautline "") 
                          manfileheader
                          (is-a-synt-trace? intmanline))
                        (setq stopdis t))
                     (t
                        (setq tag-rules 
                            (update-ok-tag-data disline intmanline tag-rules))
                        ;(cond ((or (member nil tag-rules)
                        ;           (member nil (mapcar #'first tag-rules)))
                        ;         (setq xxx tag-rules)
                        ;         (break "tag-1")))
                             )))
             ((null-or-comm autline)
; *** different lines; the automatic one is a null or comment line
               (cond ((null-or-comm manline)
  ; *** also the manual line is null or comment: print it and go ahead, disregarding
  ;     the automatic line
	               (prs-format outport "~a~%" manline)
                       (setq stopdis t))
  ; *** if the manual one is a header, then stop advancement on the
  ;     automatic file (perhaps there are just some extra comments or null lines)
  ; *** stopheader is necessary, since at the next step this manual header will
  ;     be found again (we are waiting on the manual file), but the sentence
  ;     count must not be incremented
	      	     (manfileheader
		       (cond ((not stopheader) (setq sent-count (1+ sent-count))))
                       (setq stopheader t)
                       (setq stopman t)
                       (setq stopdis t))
  ; *** if it is a manual trace, it is missing in the automatic file. Increase the
  ;     counter of missing traces, output the line, and do not advance on autfile
		     ((is-a-synt-trace? intmanline)
		       (setq trace-errors (1+ trace-errors))
		       (print-prs-line intmanline outport '(missing))
                       (setq stopaut t)
                       (setq stopdis t))))
; *** autline is NOT null-or-comm !!!
	     (autfileheader
  ; *** it is a header
               (cond (manfileheader
  ; *** if also the manual one is a heading line, then if the lines carry the
  ;     same info (perhaps there is an extra blank or asterisk), then proceed,
  ;     otherwise signal the difference
		       (cond ((and (equal autfileheader manfileheader)
				   (equal autsentnumb mansentnumb))
                                (setq stopdis t)
				(format outport "~a~%" autline))
			     (t (setq misalcount (1+ misalcount))
                                (print-misalignm autline manline))))
  ; *** if the manual one is null or comment, then print it, and suspend
  ;     advancement on the automatic file
	      	     ((null-or-comm manline)
	               (prs-format outport "~a~%" manline)
                       (setq stopaut t)
                       (setq stopdis t))
  ; *** if the manual one is a trace, then increment trace counter print the line,
  ;     and suspend advancement on the automatic file
		     ((is-a-synt-trace? intmanline)
		       (print-prs-line intmanline outport '(missing))
                       (setq stopaut t)
                       (setq stopdis t))
  ; *** if the manual one is a regular line, signal misalignment
		     (t (setq misalcount (1+ misalcount))
                        (print-misalignm autline manline))))
; *** autline is NOT null-or-comm o a sentence header !!!
             ((is-a-synt-trace? intautline)
; *** if it is a trace
      	       (cond ((null-or-comm manline)
  ; *** if the manual one is null or comment, then print it and stop advancement
  ;     on the automatic file (perhaps there is just an extra comment or null line)
	               (prs-format outport "~a~%" manline)
                       (setq stopaut t)
                       (setq stopdis t))
  ; *** if the manual line is a header, there could be an extra trace in the
  ;     automatic file. Print header (if not already done: stopheader) and stop
  ;     advancement on the manual file
	             (manfileheader
                       (setq trace-errors (1+ trace-errors))    
	               (prs-format outport "~a~%" manline)
		       (cond ((not stopheader) (setq sent-count (1+ sent-count))))
                       (setq stopheader t)
                       (setq stopdis t)
                       (setq stopman t))
                     ((is-a-synt-trace? intmanline)
  ; *** they are both traces, but do not correspond; the manual line is printed
  ;     possibly signalling the error; the error is determined within
  ;     find-difference; it could not be a true error, if the only difference is
  ;     in two subsuming labels
		       (setq stopdis t)
		       (setq auttracebuff (list intautline))
		       (setq mantracebuff (list intmanline))
  ; *** since the order of traces is not fixed, collect all traces in the buffers
  ;     auttracebuff and mantracebuff, and then compare the buffers possibly
  ;     reordering the lines
                       (do ((templine (read-line autport nil #\Escape)
                   	              (cond (stopaut nil)
				            (t (read-line autport nil #\Escape)))))
                           (stopaut nil)
     ; *** the automatic line is interpreted
			  (cond ((null templine) 
				   (setq intautline nil))
				(t (setq intautline 
				      (loc-interp-newtb-line templine nil 'aut))))
     ; *** if it is  trace it is inserted in the buffer of automatic traces, otherwise
     ;     stop the advance on the automatic file
			  (cond ((null intautline) nil)
                                ((is-a-synt-trace? intautline)
				   (setq auttracebuff (append1 auttracebuff intautline)))
				(t (setq autline templine)
				   (setq stopaut t))))
     ; *** collect the manual traces
                       (do ((templine (read-line manport nil #\Escape)
                   	              (cond (stopman nil)
				            (t (read-line manport nil #\Escape)))))
                           (stopman nil)
     ; *** the manual line is interpreted
			  (cond ((null templine) 
				   (setq intmanline nil))
				(t (setq intmanline 
				      (loc-interp-newtb-line templine nil 'man))))
     ; *** if it is  trace it is inserted in the buffer of manual traces, otherwise
     ;     stop the advance on the manual file
			  (cond ((null intmanline) nil)
                                ((is-a-synt-trace? intmanline)
				   (setq mantracebuff (append1 mantracebuff intmanline)))
				(t (setq manline templine)
				   (setq stopman t))))
                       (setq trace-count (+ (1- (length mantracebuff)) trace-count))
                       (setq line-count (+ (length mantracebuff) line-count))
  ; *** line-count is incremented by the full length of the buffer, since the
  ;     next (first non-trace) line will not be counted
		       (setq trace-result 
                              (compare-tracebuff auttracebuff mantracebuff 0 0 outport))
                       (setq trace-errors (+ trace-errors (first trace-result)))
		       (cond ((eq 'punct (get-newtb-categ intmanline))
		       		(setq puncts (1+ puncts)))))
  ; *** the automatic is a trace, the manual is a regular line. Increment the
  ;     error counter and stop advancement on manual file
                     (t (setq trace-errors (1+ trace-errors))
                        (setq stopman t)
                        (setq stopdis t))))
; *** autline is NOT null-or-comm or a sentence header or a trace !!!
;    it is a regular data line
      	     ((null-or-comm manline)
  ; *** if the manual one is null or comment, then print it and stop advancement
  ;     on the automatic file (perhaps there is just an extra comment or null line)
	       (prs-format outport "~a~%" manline)
               (setq stopaut t)
               (setq stopdis t))
  ; *** if the manual line is a header, there could be an extra line in the
  ;     automatic file. Print header (if not already done: stopheader) and stop
  ;     advancement on the manual file
	     (manfileheader
               (setq extra-line-errors (1+ extra-line-errors))    
	       (prs-format outport "~a~%" manline)
	       (cond ((not stopheader) (setq sent-count (1+ sent-count))))
               (setq stopheader t)
               (setq stopman t)
               (setq stopdis t))
             ((is-a-synt-trace? intmanline)
  ; *** the automatic is a regular line, the manual is a trace; hypothesize an
  ;     missing trace: the manual line is printed signalling the error. The
  ;     advancement on the automatic file is blocked
	       (setq trace-errors (1+ trace-errors))
	       (print-prs-line intmanline outport '(missing))
               (setq stopaut t)
               (setq stopdis t))
  ; *** both lines are regular word lines
             ((eq-word (get-newtb-inpword intautline) 
		       (get-newtb-inpword intmanline))
  ; *** they refer to the same input word
		(cond ((eq-syntinfo (get-newtb-syntinfo intautline)
			            (get-newtb-syntinfo intmanline))
  ; *** they also carry the same syntactic information
  ; *** if the error is in a punctuation mark, increment the counter
                        (setq tag-rules 
                            (update-ok-tag-data disline intmanline tag-rules))
                        ;(cond ((or (member nil tag-rules)
                        ;           (member nil (mapcar #'first tag-rules)))
                        ;         (setq xxx tag-rules)
                        ;         (break "tag-2")))
			(cond ((eq 'punct (get-newtb-categ intautline))
				(setq punct-errors (1+ punct-errors))))
  ; *** since they are not traces and they have the same syntactic labelling, the
  ;     only difference can be in the link. However, the difference can be that
  ;     the manual label is more specific than the automatic one; this is not
  ;     an error! The check is made inside 'print-prs-line', which also
  ;     increments, when needed, the link-error counter
         ;(format t "~a ~%" manline)
         ;(format t "~a ~%" intmanline)
		        (print-prs-line intmanline outport 
				(find-difference intautline intmanline nil)))
  ; *** if the syntinfos are different, error and proceed; remember that the
  ;     input word could also be unknown, so the syntinfo is a string!
                      (t (setq syntinfo-errors (1+ syntinfo-errors))
			(print-prs-line intmanline outport 
                                `(syntinfo ,(first disline)))
                        (setq tag-rules 
                           (update-notok-tag-data disline tag-rules
		                          intautline intmanline)))))
  ; *** the first of disline is the applied disamb rule
  ; *** reference to different input words: misalignment
  ; *** if the automatic line had been erroneously split, stop the advancement
  ;     on the manual one. The error counter is not incremented, since in this
  ;     case the error was already found in the line before (ex. 'dei' prep+art
  ;     automatic vs. 'dei' art indef manual)
             ((and (= (get-newtb-linumb intmanline)
                      (1+ (get-newtb-linumb intautline)))
                   (not (atom (get-newtb-numb intautline))))
              ; (break "misalignment 1")
               (setq stopman t))
  ; *** the inverse than above
             ((and (= (get-newtb-linumb intautline)
                      (1+ (get-newtb-linumb intmanline)))
                   (not (atom (get-newtb-numb intmanline))))
              ; (break "misalignment 2")
               (setq stopaut t)
               (setq stopdis t))
	     (t 
              ; (break "misalignment 3")
                (setq misalcount (1+ misalcount))
	        (print-misalignm autline manline)
	        (print-prs-line intmanline outport '(misalignment)))))))
       (print-tag-data tag-rule-data tagport)))

; *********************************************************************
; *** source can be 'man (manual) or 'aut (automatic)
;     it is used to replace the "£" iso encoding with the utf-8 encoding
(defun loc-interp-newtb-line (line firstline? source)
 (declare (special *LISP-CHAR-SET-ID*))
 (let (tempinterp lemma tempsynt)
  (cond ((or (null-or-comm line) (is-sentence-heading line firstline?)) line)
	(t (setq tempinterp (interp-newtb-line line))
          ; (cond ((and (eq 'noun (get-synt-categ tempinterp))
          ;             (eq 'proper (get-synt-type tempinterp)))
          ;         (break "uno")))
           (cond ((and (eq source 'man) 
                     ; (eq 'UTF-8 *LISP-CHAR-SET-ID*)
                       )
                   ;(setq lemma (convert-base-atom-or-string-to-currlisp (get-newtb-word tempinterp)))
                   (setq lemma (get-newtb-word tempinterp))
                   (let ((categ (get-synt-categ tempinterp))
                         (type (get-synt-type tempinterp))
                         (lsynt (get-synt-syntinfo tempinterp))
                         sem adjvalue)
                      (cond ((and (eq categ 'adj) (eq type 'ordin))
                              (setq adjvalue (sixth lsynt))
                             ; (cond ((not (numberp adjvalue))
                             ;          (setq adjvalue 
                             ;               (convert-base-atom-or-string-to-currlisp adjvalue))))
                              (list (first tempinterp) (second tempinterp)
                                    (list lemma (second lsynt) (third lsynt)
                                            (fourth lsynt) (fifth lsynt) adjvalue)
                              (fourth tempinterp) (fifth tempinterp) (sixth tempinterp)))
                            ((and (eq categ 'noun) (eq type 'proper))
                              (setq sem (get-synt-semtype tempinterp))
                              (cond ((null sem) 
            ; *** this is a proper not appearing in the lexicon
                                      (list (first tempinterp) (second tempinterp)
                                            (cons lemma (rest lsynt))
                                            (fourth tempinterp) (fifth tempinterp)
                                            (sixth tempinterp)))
                                    (t ;(setq sem (convert-base-atom-or-string-to-currlisp sem))
                                       (setq tempsynt 
                                             (list lemma (second lsynt) (third lsynt)
                                                    (fourth lsynt) (fifth lsynt) sem))
                                       (cond ((not (null (seventh lsynt)))	; *** LOCUTION
                                               (setq tempsynt (append1 tempsynt (seventh lsynt)))))
                                       (list (first tempinterp) (second tempinterp)
                                             tempsynt
                                             (fourth tempinterp) (fifth tempinterp) 
                                             (sixth tempinterp)))))
                              (t (list (first tempinterp) (second tempinterp)
                                       (cons lemma (rest lsynt))
                                       (fourth tempinterp) (fifth tempinterp)
                                               (sixth tempinterp))))))
                 (t tempinterp))))))

; *********************************************************************
; *** compares two lines (disregarding comments)
(defun eq-line (intl1 intl2)
; *** if either of them is a string, they must be the same string (apart from
;     spaces and the 'return', which not always appears)
       ;    (cond ((and (not (stringp intl1))
       ;                (not (stringp intl2))
       ;                (eq 'noun (get-synt-categ intl1))
       ;                (eq 'proper (get-synt-type intl2)))
       ;            (break "due")))
  (cond ((stringp intl1)
  	   (and (stringp intl2)
		(string= (string-trim '(#\Space #\return) intl1)
  	            	 (string-trim '(#\Space #\return) intl2))))
        ((stringp intl2) nil)
  	((and (equal (first intl1) (first intl2))	; line number
       	      (cparse-eq-word (second intl1) (second intl2))	; input word
       	      (eq-syntinfo (third intl1) (third intl2)) ; syntinfo
       	      (equal (fourth intl1) (fourth intl2)))	; links
; *** the coreference index is tested just for traces
	  (cond ((is-a-synt-trace? intl1)
       	           (coref-equal (fifth intl1) (fifth intl2)))	; coref
	        (t t)))))

; *********************************************************************
; *** compares two syntinfos
(defun eq-syntinfo (ints1 ints2)
; *** if either of them is a string, they must be the same string
  (cond ((or (stringp ints1) (stringp ints2))
  	   (equal ints1 ints2))
        (t (and (cparse-eq-word (first ints1) (first ints2)) ; lemma
                (eq-other-synt (rest ints1) (rest ints2)))))) ; other syntinfos

; *********************************************************************
; *** compares two lemmas
; *** currently (09/11/18) the automatic lemma is in Lisp internal format
;     (basically fixed to utf-8) and the manual lemma is in the base
;     encoding. Moreover, the automatic lemma keeps the accented chars
;     downcase, while the manual one is uppercase
(defun cparse-eq-word (autl manl)
   (eq manl autl
      ; (base-uppercase 
      ;     (convert-currlisp-atom-or-string-to-base autl))
        ))

; *********************************************************************
; *** compares two syntinfos, apart from the lemma
;     They must be equal in everything, apart form the transitivity infos
(defun eq-other-synt (ints1 ints2)
 (let ((transinfo '(trans intrans refl)))
   (cond ((and (null ints1) (null ints2)) t)
         ((or (equal (first ints1) (first ints2))
              (and (member (first ints1) transinfo) 
                   (member (first ints2) transinfo)))
            (eq-other-synt (rest ints1) (rest ints2)))
         (t nil))))

; *********************************************************************
;     this compares two sequences of traces in order to find how many
;     traces in mantbuff have a correspondence in auttbuff.
; *** it returns a pair, wher the first element is the total number of errors
;     (missing, extra, or coref-different traces) that have been found, while
;     the second is the number of coref-difference errors
(defun compare-tracebuff (auttbuff mantbuff tr-err cor-err outport)
   (cond ((null mantbuff) (list tr-err cor-err))
         (t (let ((findres (find-aut-trace (first mantbuff) auttbuff outport)))
; *** find-aut-trace looks for a correspondence (among the automatic traces) of
;     a given manual trace; it returns a triple <err coref rembuf>, where:
;     - err is 1 if a trace error was found, 0 otherwise,
;     - coref is 1 if the error was a coreference error, 0 otherwise
;     - rembuf contains the traces remained unmatched in auttbuff
	    (compare-tracebuff 
		  (third findres) 
		  (rest mantbuff) 
		  (+ (first findres) tr-err)
		  (+ (second findres) cor-err)
		  outport)))))

; *********************************************************************
; *** this searches autbuff for a trace corresponding to 'mantrace'
(defun find-aut-trace (mantrace autbuff outport)
  (let ((manlabel (second (fourth mantrace)))
         remaut (found nil) (coref-er 0) (trace-er 0))
   (do ((nextaut (first autbuff) (first autbuff))
        (autbuff (rest autbuff) (rest autbuff)))
       ((or found (null nextaut))
  ; *** if the automatic buffer is empty, then a manual trace was not
  ;     found by the parser, i.e. it is missing
	 (cond ((not found) 
		  (print-prs-line mantrace outport '(missing))
	          (list 1 0 remaut))
  ; *** otherwise the error, if any, was already found and printed
	       (t (list trace-er coref-er remaut))))
       (cond ((lab-subsumes (second (fourth nextaut)) manlabel)
  ; *** we have found the corresponding automatic trace
	       (cond ((not (coref-equal (fifth nextaut) (fifth mantrace)))
    ; *** but the coreference pointer does not correspond
                        (setq coref-er 1)
                        (setq trace-er 1)
	                (print-prs-line mantrace outport 
					(list 'coref (fifth nextaut)))
		        (setq remaut (append remaut autbuff))
		        (setq found t))
    ; *** otherwise, no error
		     (t (print-prs-line mantrace outport nil)
			(setq remaut (append remaut autbuff))
		        (setq found t))))
             (t (setq remaut (append1 remaut nextaut)))))))

; *********************************************************************
(defun coref-equal (coref1 coref2)
  (cond ((eq 'empty coref1)
	   (or (eq 'empty coref2) (null coref2)))
	((null coref1)
	   (or (eq 'empty coref2) (null coref2)))
	((or (eq 'empty coref2) (null coref2)) nil)
  	(t (and (equal (first coref1) (first coref2))
       	        (equal (coref-upcase (second coref1)) 
		       (coref-upcase (second coref2)))))))

; *********************************************************************
(defun coref-upcase (coref-char)
  (cond ((char= coref-char #\p) #\P)
	((char= coref-char #\f) #\F)
	((char= coref-char #\w) #\W)
	(t coref-char)))

; *********************************************************************
(defun null-or-comm (line)
  (or (string= (string-trim '(#\Space #\return) line) "")
      (same-chars? (read-from-string line) #\?)))

; *********************************************************************
(defun print-misalignm (autline manline)
(declare (special misalcount))
  (format t "---- MISALIGNMENT ----~%")
  (format t "  Automatic line: ~a~%  Manual line: ~a~%" autline manline)
  (cond ((> misalcount 10)
           (setq misalcount 0)
           (break ""))))

; *********************************************************************
(defun find-difference (intautline intmanline trace?)
  ; *** since input word, lemma, and syntinfos have already been tested,
  ;     the difference con only be in links or coref
  (declare (special link-errors prep-errors label-errors trace-errors))
  (let ((autlabel (fourth intautline))
        (manlabel (fourth intmanline)))
  (cond ((not (equal (first autlabel) (first manlabel)))
  ; *** the 'first' is the pointer upward: linking error
          (cond (trace? (setq trace-errors (1+ trace-errors)))
	        (t (setq link-errors (1+ link-errors))
                   (cond ((eq (get-newtb-categ intmanline) 'prep) 
                            (setq prep-errors (1+ prep-errors))))))
	  (list 'link autlabel))
        ((not (lab-subsumes (second autlabel) (second manlabel)))
     ; *** lab-subsumes defined in *HOME-DIR*/PROC/ALLANG/subc-hier
          (cond (trace? (setq trace-errors (1+ trace-errors)))
	        (t (setq label-errors (1+ label-errors))))
	  (list 'label autlabel))
	((and trace? (not (coref-equal (fifth intautline) (fifth intmanline))))
          (setq trace-errors (1+ trace-errors))
	  (list 'coref (fifth intautline))))))

; ***************************************************************
;     WRITING ON OUTPUT FILE
; ***************************************************************

; ***************************************************************************
(defun print-prs-line (line outport error)
   (setq *print-pretty* nil)
   (setq *print-level* nil)
   (setq *print-length* nil)
; *** the traceinfo is displaced in the readable form (fourth position) with
;     respect to the output position (third)
   (let ((linenumb (first line))
	 (word (second line))
	 (syntinfo (third line))
	 (link (fourth line))
	 (traceinfo (fifth line))
	 (comments (sixth line)))
; *** writing the line number. If not a number, it must be of the form (n1 n2),
;     and the output must appear as n1.n2
       (cond ((numberp linenumb)
               (format outport "~a " linenumb))
	     (t (format outport "~a.~a " (first linenumb) (second linenumb))))
; *** writing the word
       (format outport "~a " word)
; *** if traceinfo not null, writing traceinfo:
       (cond ((not (null traceinfo))
; *** non-coindexed trace; output []
		(cond ((eq traceinfo 'empty)
			 (format outport "[] "))
; *** trace co-indexed with line N, with type of trace x: output [Nx]
		      ((numberp (first traceinfo))
		         (format outport "[~a~a] " 
					(first traceinfo) (second traceinfo)))
; *** trace co-indexed with line N.M, with type of trace x: output [N.Mx]
		      (t (format outport "[~a.~a~a] "
					(first (first traceinfo)) 
					(second (first traceinfo))
					(second traceinfo))))))
; *** writing the syntinfo
; *** the 'cond' used to force the writing in character format of the
;     punctuations and symbols
       (format outport "~a" #\()
       (cond ((characterp (first syntinfo))
               (format outport "~a~a~a" #\# #\\ (first syntinfo)))
             (t (format outport "~s" (first syntinfo))))
       (dolist (el (rest syntinfo))
               (format outport " ~s" el))
       (format outport "~a " #\))
; *** writing dependency info
       (format outport "[")
;(format t "Link: ~a~%" link)
;(break "")
       (cond ((not (null link))
               (cond ((or (null (first link)) (numberp (first link)))
                       (format outport "~a" (first link)))
	             (t (format outport "~a.~a" 
			         (first (first link)) (second (first link)))))
	       (format outport ";~s" (second link))))
       (format outport "~a " #\])
; *** writing comments
       (cond ((not (null comments))
		(format outport "	??? ~a" comments)))
; *** writing errors (if any)
       (cond ((not (null error))
		(case (first error)
       	   	   (extra (format outport " ??? >>> EXTRA TRACE"))
           	   (missing (format outport " ??? >>> MISSING TRACE"))
	   	   (syntinfo (format outport " ??? >>> DIFFERENT SYNTINFOS: ~a"
                                (second error)))
	   	   (link (format outport " ??? >>> ~a;~a" 
				   (first (second error)) (second (second error))))
	   	   (label (format outport " ??? >>> LABEL ~a" (second error)))
	   	   (coref (format outport " ??? >>> COREF: ~a" (second error)))
	   	   (misalignment (format outport " ??? >>> MISALIGNMENT")))))
       (format outport "~%")
   (setq *print-pretty* t)
   (setq *print-level* 5)
   (setq *print-length* 10)))

; ***************************************************************************
(defun print-parseresult (errf sent-count sent-grouping tot-line-count 
                        tot-trace-count tot-puncts tot-err tot-link-err
                        tot-trace-err tot-extral-err tot-punct-err
                        tot-syntinfo-err tot-label-err tot-prep-err)
 (with-open-file (errport errf :direction :output
                                 :if-exists :overwrite
                                 :if-does-not-exist :create)
;(format t "Prep errors: ~a" tot-prep-err)
;(break "")
  (let ((allin (add-all tot-line-count))
        (alltr (add-all tot-trace-count))
        (allpu (add-all tot-puncts))
        (aller (add-all tot-err))
        (alllie (add-all tot-link-err))
        (alltre (add-all tot-trace-err))
        (allpue (add-all tot-punct-err))
        (allsye (add-all tot-syntinfo-err))
        (allpre (add-all tot-prep-err))
        (alllab (add-all tot-label-err)))
   (format errport "~%Total number of sentences: ~s~%~%" sent-count)
   (format errport "----------------------------------------------------------------------------~%")
   (format errport " SENT   |lines|trace|punct** err **lkerr|label|trerr|puerr|syerr| %err | prep|~%")
   (format errport "--------|-----|-----|-----**-----**-----|-----|-----|-----|-----|------|-----|~%")
   (do ((first-sent 1 (+ first-sent sent-grouping))
	(lin (first tot-line-count) (first tot-line-count))
	(tot-line-count (rest tot-line-count) (rest tot-line-count))
	(trac (first tot-trace-count) (first tot-trace-count))
	(tot-trace-count (rest tot-trace-count) (rest tot-trace-count))
	(pun (first tot-puncts) (first tot-puncts))
	(tot-puncts (rest tot-puncts) (rest tot-puncts))
	(err (first tot-err) (first tot-err))
	(tot-err (rest tot-err) (rest tot-err))
	(lk (first tot-link-err) (first tot-link-err))
	(tot-link-err (rest tot-link-err) (rest tot-link-err))
	(tr (first tot-trace-err) (first tot-trace-err))
	(tot-trace-err (rest tot-trace-err) (rest tot-trace-err))
	(llab (first tot-label-err) (first tot-label-err))
	(tot-label-err (rest tot-label-err) (rest tot-label-err))
	(pue (first tot-punct-err) (first tot-punct-err))
	(tot-punct-err (rest tot-punct-err) (rest tot-punct-err))
	(pre (first tot-prep-err) (first tot-prep-err))
	(tot-prep-err (rest tot-prep-err) (rest tot-prep-err))
	(sye (first tot-syntinfo-err) (first tot-syntinfo-err))
	(tot-syntinfo-err (rest tot-syntinfo-err) (rest tot-syntinfo-err)))
       ((null tot-line-count)
      	   (format errport "~3d-~3d |~5d|~5d|~5d**~5d**~5d|~5d|~5d|~5d|~5d|~6,2f|~5d|~%" 
		first-sent sent-count lin trac pun err 
		lk llab tr pue sye (* 100. (/ (float err) (float (- lin trac)))) pre))
      (format errport "~3d-~3d |~5d|~5d|~5d**~5d**~5d|~5d|~5d|~5d|~5d|~6,2f|~5d|~%" 
		first-sent (1- (+ sent-grouping first-sent)) lin trac pun err 
		lk llab tr pue sye (* 100. (/ (float err) (float (- lin trac)))) pre))
   (format errport "--------|-----|-----|-----**-----**-----|-----|-----|-----|-----|------|------|~%")
   (format errport " TOTAL  |~5d|~5d|~5d**~5d**~5d|~5d|~5d|~5d|~5d|~6,2f|~5d|~%" 
  		allin alltr allpu aller alllie alllab alltre allpue allsye
		(* 100. (/ (float aller) (float (- allin alltr)))) allpre)
   (format errport "------------------------------------------------------------------------------~%"))))

; ***************************************************************************
(defun add-all (num-list)
  (cond ((null num-list) 0)
	(t (+ (first num-list) (add-all (rest num-list))))))

; ***************************************************************************
; *** this substitutes the return (^M) with a linefeed
(defun prs-format (port control line)
   (format port control (string-trim '(#\return) line)))

; ***************************************************************************
; *** this reads three lines (from a .dis file) e returns the third one,
;     which should contain the list of applied tag rules
(defun triple-read-line (disport dummy end)
 (let (val)
  (read-line disport dummy end)
  (read-line disport dummy end)
  (setq val (read disport dummy end))
  (read-line disport dummy end)
  val))

; ***************************************************************************
; *** this adds a list of succeeding rules to the tag-data statistics
(defun update-ok-tag-data (newdata worddata tag-data)
      ;(format t "Word: ~a; rules: ~a; OK~%" (get-newtb-word worddata) newdata)
  (cond ((not (equal newdata #\Escape))
          (let ((rules (cond ((atom (first newdata))
    ; *** (first newdata) can be a list, because multiple rules can be applied to the same item
    ;     Category, then type, etc
                             (cond ((memq (first newdata) '(random unambiguous))
                                      (list (first newdata)))
                                   ((eq (first newdata) 'morphological)
                                      '(non-existent))
                                   (t (cons (first newdata) (second newdata)))))
                           (t (first newdata)))))
            (dolist (nxtrule rules tag-data)
               ; (cond ((null nxtrule) (break "tagstat")))
                (setq tag-data (cons (list nxtrule 'ok) tag-data)))))
        (t tag-data)))
      
; ***************************************************************************
; *** this updates the list of applied tag rules, looking for the error
;     "newdata" is the description of the rules(s) applied for assigning the category and features
;     as appears in the ".dis" file. The description has the form
;     <Desc> --> <non-standard-desc> | <inter-categ-desc> | <intra-categ-desc> | <inter+intra-categ-desc>
;     <non-standard-desc> --> (unambiguous word) | (Random choice) | <random+intra-categ-desc> |
;                             (morphological failure) | (locution)
;     <inter-categ-desc> --> (<intercateg-rule-name>)
;     <intra-categ-desc> --> ((<intracateg-rule-name1> ... <intracateg-rule-nameN>))
;     <inter+intra-categ-desc> --> (<intercateg-rule-name> 
;                                         (<intracateg-rule-name1> ... <intracateg-rule-nameN>))
;     <random+intra-categ-desc> --> (Random choice (<intracateg-rule-name1> ... <intracateg-rule-nameN>))
(defun update-notok-tag-data (newdata tag-data autline manline)
  (let ((rulestart (first newdata))
        (autcat (get-newtb-categ autline))
        (mancat (get-newtb-categ manline))
        (auttype (get-newtb-type autline))
        (mantype (get-newtb-type manline))
        (result tag-data)
        (saverules newdata)
        intrarules)
             ; *** in applied-rule-data, the single components of the rule name:
             ;     ex. adv-compar-time-r01 --> (ADV COMPAR TIME R01)
  ; *** the next adjust the format of newdata in non-standard cases (unambiguous and random)
    (cond ((eq rulestart 'unambiguous)
             ; *** the actual form of the rule is "(unambiguous word)"
            (setq newdata '(unambiguous)))
          ((eq rulestart 'random)
             ; *** the actual form of the rule is "(Random choice)"
             ;     Note that this is different from intra-category random choices, that appear as:
             ;     "((RANDOM))". The two forms may be combined: "(Random choice (RANDOM))"
            ;(break "RANDOM")
            (setq newdata (cons 'random (rest (rest newdata))))))
  ; *** START of the actual check of differences ***************************************
    (cond ((eq autcat mancat)
  ; *** the category is ok; 
            (cond ((atom rulestart)
     ; *** if the first is an atom, it is a inter-category rule (e.g. NOUN-VERBR17), so add it
     ;     to the ok list, unless the applied rule is "locution". In this case the wrong locution 
     ;     has been identified, but, casually, it is of the same category of the right one
     ; *** the same may happen for morphological failures (it is a common noun
     ;     but it starts with a capitalized letter, so that after the morphological
     ;     failure it was classified as a default proper name)
     ; *** otherwise, it was just a intra-category ambiguity. so that no
     ;     inter-category rule was applied (e.g. "((VERB-AUX-MAINR01))")
                    (cond ((member rulestart '(locution morphological))
                             (setq intrarules nil))
                          (t (setq result (check-newtagdata newdata result 'ok saverules))
             ; *** proceed with possible intra-category rules
                             (setq intrarules (second newdata)))))
                  (t (setq intrarules rulestart)))
     ; *** put in intrarule-data the details of the rule name:
     ;     ex. adv-compar-time-r01 --> (ADV COMPAR TIME R01)
        ; *** in case of intra-category rules, the applied sequence depends on
        ;     the particular category
            (cond ((memq autcat '(VERB VERB+PRON VERB+ADV))
                     (let ((autmood (get-newtb-mood autline))
                           (manmood (get-newtb-mood manline))
                           (autnumb (get-newtb-number autline))
                           (mannumb (get-newtb-number manline))
                           (autpers (get-newtb-person autline))
                           (manpers (get-newtb-person manline)))
                        (cond ((eq auttype mantype)
                         ; *** the aux-main or main-mod rules were ok
                                (cond ((member 'MAIN (expl+cats (explode (first intrarules)) #\-))
                             ; *** it may happen that the "type" rules were not applied (if there
                             ;     was no type ambiguity in the inputs). Move ahead just in case
                             ;     they were applied (so that "main" appears in the rule name)
                                         (setq result (check-newtagdata intrarules result 'ok saverules))
                                         (setq intrarules (rest intrarules))))
                                (cond ((eq autmood manmood)
                             ; *** the mood rules were ok
                                         (cond ((member 'FIN (expl+cats (explode (first intrarules)) #\-))
                                  ; *** the fin-inf rule were applied; result ok
                                                 (setq result 
                                                     (check-newtagdata intrarules result 'ok saverules))
                                                 (setq intrarules (rest intrarules))))
                                         (cond ((intersection '(CONG IMPER PARTICIPLE) 
                                                              (expl+cats (explode (first intrarules)) #\-))
                                  ; *** also some other mood rule was applied; result ok
                                                 (setq result 
                                                     (check-newtagdata intrarules result 'ok saverules))
                                                 (setq intrarules (rest intrarules))))
                                         (cond ((eq autnumb mannumb)
                                                  (cond ((member 'SING 
                                                              (expl+cats (explode (first intrarules)) #\-))
                                  ; *** the number (pl-sing) rules were applied; result ok
                                                           (setq result 
                                                             (check-newtagdata intrarules result 'ok saverules))
                                                           (setq intrarules (rest intrarules))))
                                                  (cond ((eq autpers manpers)
                                       ; *** the person rules were ok: where is the error?
                                                           (setq result 
                                                              (check-locution-err 
                                                                    manline autline 'VERB result)))
                                       ; *** the person rules are responsible for the error
                                                        (t (setq result 
                                                              (check-newtagdata 
                                                                    intrarules result 'err saverules)))))
                                 ; *** the number (pl-sing) rules are responsible for the error
                                               (t (setq result 
                                                      (check-newtagdata intrarules result 'err saverules)))))
                             ; *** the mood rules are responsible for the error
                                      (t (setq result (check-newtagdata intrarules result 'err saverules)))))
                    ; *** the type (aux-main or main-mod) rules are responsible for the error
                           (t (setq result (check-newtagdata intrarules result 'err saverules))))))
                  ((eq autcat 'NOUN)
                     (cond ((eq auttype mantype)
                    ; *** the common-proper rules were ok
                              (cond ((member 'COMMON
                                        (expl+cats (explode (first intrarules)) #\-))
                        ; *** and they were actually used: save info
                                       (setq result (check-newtagdata intrarules result 'ok saverules))
                                       (setq intrarules (rest intrarules))))
                               (let ((autnumb (get-newtb-number autline))
                                     (mannumb (get-newtb-number manline)))
                                 (cond ((eq autnumb mannumb)
                           ; *** the number rules were ok
                                         (cond ((member 'SING
                                                     (expl+cats (explode (first intrarules)) #\-))
                               ; *** and they were actually used: save info
                                                  (setq result 
                                                      (check-newtagdata intrarules result 'ok saverules))
                                                  (setq intrarules (rest intrarules))))
                                         (let ((autgend (get-newtb-gender autline))
                                               (mangend (get-newtb-gender manline)))
                                            (cond ((eq autgend mangend)
                                   ; *** the gender rules were ok
                                                     (cond ((member 'F
                                                               (expl+cats (explode (first intrarules)) #\-))
                                        ; *** and they were actually used: save info
                                                              (setq result 
                                                                  (check-newtagdata 
                                                                      intrarules result 'ok saverules))
                                                              (setq intrarules (rest intrarules))))
                                                     (cond ((eq (get-newtb-word autline)
                                                                (get-newtb-word manline))
                                             ; *** it is the same word: break: where is the error?
                                                              (setq result 
                                                                  (check-locution-err 
                                                                        manline autline 'NOUN result)))
                    ; *** only difference in the lemma (e.g. Italian "principi")
                                                           (t (cons '(random err) result))))
                    ; *** the gender rules are responsible for the error
                                                  (t (setq result 
                                                        (check-newtagdata intrarules result 'err saverules))))))
                    ; *** the number rules are responsible for the error
                                       (t (setq result (check-newtagdata intrarules result 'err saverules))))))
                    ; *** the common-proper rules are responsible for the error
                             (t (setq result (check-newtagdata intrarules result 'err saverules)))))
                ((memq autcat '(ART PREP+ART))
                  (let ((autgend (get-newtb-gender autline))
                        (mangend (get-newtb-gender manline)))
                    (cond ((eq autgend mangend)
                    ; *** the gender rules were ok: break: where is the error?
                             (setq result (check-locution-err manline autline 'ART result)))
                    ; *** the gender rules are responsible for the error
                          (t (setq result (check-newtagdata intrarules result 'err saverules))))))
                ((eq autcat 'ADJ)
                  (let ((auttype (get-newtb-type autline))
                        (mantype (get-newtb-type manline))
                        (autgend (get-newtb-gender autline))
                        (mangend (get-newtb-gender manline))
                        (autnumb (get-newtb-numb autline))
                        (mannumb (get-newtb-numb manline)))
                    (cond ((eq auttype mantype)
                    ; *** the adjectival type rules were ok
                             (cond ((intersection '(QUALIF INTERR) (expl+cats (explode (first intrarules)) #\-))
                            ; *** and they were actually applied
                                      (setq result (check-newtagdata intrarules result 'ok saverules))
                                      (setq intrarules (rest intrarules))))
                             (cond ((eq autgend mangend)
                    ; *** the gender rules were ok
                                      (cond ((member 'F (expl+cats (explode (first intrarules)) #\-))
                            ; *** and they were actually applied
                                               (setq result (check-newtagdata intrarules result 'ok saverules))
                                               (setq intrarules (rest intrarules))))
                                      (cond ((eq autnumb mannumb)
                    ; *** the number rules were ok: where is the error?
                                               (setq result 
                                                   (check-locution-err manline autline 'ADJ result)))
                    ; *** the number rules are responsible for the error
                                            (t (setq result 
                                                   (check-newtagdata intrarules result 'err saverules)))))
                    ; *** the gender rules are responsible for the error
                                    (t (setq result (check-newtagdata intrarules result 'err saverules)))))
                    ; *** the adjectival type rules are responsible for the error
                          (t (setq result (check-newtagdata intrarules result 'err saverules))))))
                ((eq autcat 'ADV)
                  (let ((auttype (get-newtb-type autline))
                        (mantype (get-newtb-type manline)))
                    (cond ((eq auttype mantype)
                    ; *** perhaps a locution error
                             (setq result (check-locution-err manline autline 'ADV result)))
                    ; *** the type rules are responsible for the error
                          (t (setq result (check-newtagdata intrarules result 'err saverules))))))
                ((eq autcat 'CONJ)
                  (let ((auttype (get-newtb-type autline))
                        (mantype (get-newtb-type manline)))
                    (cond ((eq auttype mantype)
                    ; *** perhaps a locution error
                             (setq result (check-locution-err manline autline 'CONJ result)))
                    ; *** the type rules are responsible for the error
                          (t (setq result (check-newtagdata intrarules result 'err saverules))))))
                ((eq autcat 'PRON)
                  (let ((auttype (get-newtb-type autline))
                        (mantype (get-newtb-type manline)))
                    (cond ((eq auttype mantype)
                    ; *** the pronominal type rules were ok
                             (cond ((intersection '(RELAT PERS)
                                                (expl+cats (explode (first intrarules)) #\-))
                           ; *** and they were actually used
                                      (setq result (check-newtagdata intrarules result 'ok saverules))
                                      (setq intrarules (rest intrarules))))
                             (cond ((or (and (is-a-synt-clitic? manline)
                                             (is-a-synt-clitic? autline))
                                        (and (not (is-a-synt-clitic? manline))
                                             (not (is-a-synt-clitic? autline))))
                                     (setq result (check-newtagdata intrarules result 'ok saverules))
                                     (setq intrarules (rest intrarules))
                                     (cond ((or (null intrarules)
                                                (eq (first intrarules) 'random))
                         ; *** if at this point a random choice was made, probably
                         ;     the pronoun was ambiguous betweem "le indobj" and "le obj" (Italian)
                                            (setq result (cons '(random err) result)))
                         ; *** otherwise break: where is the error?
                                          (t (setq result (check-locution-err manline autline 'PRON result)))))
                      ; *** the clitic rules are responsible for the error
                                   (t (setq result (check-newtagdata intrarules result 'err saverules)))))
                    ; *** the pronominal type rules are responsible for the error
                          (t (setq result (check-newtagdata intrarules result 'err saverules))))))
                (t (cond ((or (eq (first intrarules) 'locution)
                              (and (is-a-synt-locution autline)
                                   (not (is-a-synt-locution manline)))
                              (and (is-a-synt-locution manline)
                                   (not (is-a-synt-locution autline))))
                           (setq result (check-newtagdata intrarules result 'err saverules)))
                         (t (break "update-tag-dat: error for unknown category"))))))
           (t (cond ((atom (first newdata))
     ; *** if there is a category error, include the inter-category rule in
     ;     the error list and ignore the other applied rules. If no
     ;     inter-category rule was applied, then break
                       (check-newtagdata newdata result 'err saverules))
                    ((and (eq mancat 'NOUN)
                          (eq (get-newtb-type manline) 'PROPER))
     ; *** if the manual line is a proper name, then it is an unknown proper
     ;     name, taken as something else (as "John Eat")
                       (cons '(unknown-proper err) result))
                    (t (break "update-tag-data: category difference but no rule applied")))))))
       
; ***************************************************************************
; *** the type rules were ok: maybe the manual line refers to a locution
;      not known or the parser has hypothesized a locution in the wrong place
(defun check-locution-err (manline autline categ prevresult)
   (cond ((or (is-a-synt-locution manline) (is-a-synt-locution autline))
            (cons '(locution err) prevresult))
         (t (format t "~%update-tag-data: error not found for a ~a~%" categ)
            (format t "AUTLINE: ~a; ~%MANLINE: ~a~%" autline manline)
            prevresult)))

; ***************************************************************************
(defun check-newtagdata (newrule olddata value allrules)
 ; **** newrule actually is a list of rules
 ;      olddata are the previously collected statistics
 ;      value is 'ok or 'fail
 ;      allrules are all applied rules for the involved item, and is included
 ;         just for debugging purposes
    (cond ((null newrule) 
            ;  (Break "Tag evaluation: A NIL rule is being added")
              olddata)
          (t (cons (list (first newrule) value) olddata))))
   
; ***************************************************************************
; *** this prints the data about the application of the tagging rules
; *** tag-rules is alist of pairs, whose first element is the rule name,
;     while the second is ok or err
(defun print-tag-data (tag-rules disport)
   (let ((sort-rules (sort tag-rules 
                          #'(lambda (x y) (alphalessp (first x) (first y)))))
         rule-names prevname numbok numberr blanks)
     (dolist (nxtrule sort-rules)
         (cond ((eq (first nxtrule) prevname)
      ; *** same rule as before
                 (cond ((eq (second nxtrule) 'ok) 
                          (setq numbok (cons (1+ (first numbok))
                                             (rest numbok))))
                       (t (setq numberr 
                               (cons (1+ (first numberr))
                                     (rest numberr))))))
      ; *** this is a new rule
               (t (setq prevname (first nxtrule))
                  (setq rule-names (cons prevname rule-names))
                  (cond ((eq (second nxtrule) 'ok) 
                           (setq numbok (cons 1 numbok))
                           (setq numberr (cons 0 numberr)))
                        (t (setq numbok (cons 0 numbok))
                           (setq numberr (cons 1 numberr)))))))
; *** end of collection of the statistics; actual writing on the output file
    (setq rule-names (reverse rule-names))
    (setq numbok (reverse numbok))
    (setq numberr (reverse numberr))
    (format disport "; *** evaluation of the tagging rules ~%")
    (format disport "; ***************************************~%")
    (format disport ";          Rule Name               ok err  ~%")
    (format disport "; ***************************************~%")
    (dolist (rule rule-names)
       (setq blanks (- 
                      (cond ((> (first numbok) 99) 33)
                            ((> (first numbok) 9) 34)
                            (t 35))
                      (length (explode rule))))
       (format disport "~a" rule)
       (print-blanks blanks disport)
       (format disport "  ~a  ~a~%" (first numbok) (first numberr))
       (setq numbok (rest numbok))
       (setq numberr (rest numberr)))))

(defun print-blanks (num file)
  (cond ((<= num 0) nil)
        (t (do ((count num (1- count)))
               ((eq count 0))
               (format file " ")))))


