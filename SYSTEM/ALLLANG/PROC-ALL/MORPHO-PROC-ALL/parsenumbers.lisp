(in-package "USER")

; ###############################################
;   THE FUNCTIONS IN THIS FILE CONVERT A NUMBER WRITTEN IN LETTERS
;      INTO THE CORRESPONDING LISP VALUE
; ###############################################

;*****************************************************************
; *** converts a number expressed in words as a sequence of ascii codes in a
;     numerical value
(defun parsenum (iword)
; - iword is a list of chars
; - the result is a binary number (if the input is a number in words),
;   otherwise nil
; **** the main loop splits the word in components (via numb-split) 
;      * If the first component is nil, the analysis is successfully completed
;        and the result of the accumulator (numbval) is returned
;      * If it is "fail", then no valid prefix has been recognized and the
;        function returns nil
;      * If it is "elision", it has been found a final apostrophe, so that
;        the analysis is ok (with a suitable next word), but the word must be
;        terminated, and the result is in numbval
;      * otherwise, go on with another component
  (let ((tempval 0) (n-state 'nu-0) (value 0) (failure nil) outval final-state
        trunchar oldval (tempord 0))
       (declare (special value tempval oldval tempord))
  (setq oldval value)
  (do* ((splitw (numb-split iword) (numb-split restw))
; *** the result of numb-split is: ((arclab val) rest poss-gend-numb)
;     but arclab can be (elision char) in case of elision (vent': (elision #\i))
;     - poss-gend-numb is nil or a pair (gend numb). It is non-nil for ordinals
;       primera --> (f sing)
	(firstcomp (first splitw) (first splitw))
	(restw (second splitw) (second splitw))
	(prevgendnumb nil gendnumb)
	(gendnumb (third splitw) (third splitw))
	(prevsuff nil suffinfo)
	(suffinfo (fourth splitw) (fourth splitw)))
; *** exit conditions
; *** 	- null firstcomp: the input word is terminated
;	- firstcomp=fail: the initial part of the word does not correspond
;         to a piece of number (according to the lower-level automaton)
;	- failure: no advancement possible (on this automaton)
       ((or (null firstcomp) 
            (eq firstcomp 'fail)
            failure)
	 (cond ((and (null firstcomp) (not failure) final-state)
                 (cond ((null trunchar) 
     ; *** if prevgendnumb is nil, then it is a non-inflected word
     ;     otherwise, it is inflected, so that a triple is returned
     ;     containing the value, the gender+number information, and
     ;     data about the suffix. This is a pair <numchar,suff>, where
     ;     numchar is the number of chars tu cut from the end of the
     ;     word, and suff is the suffix of the standard form, to be added
     ;     e.g. primera --> <1 (f sing) (a nil)>
     ;          dozenes --> <12 (f pl) (enes è>)
                          (cond ((null prevgendnumb) outval)
                                (t (list outval prevgendnumb prevsuff))))
     ; *** if trunchar is not null, in the previous step an elision was
     ;     recognized, and the truncated char was saved in trunchar
     ; *** N.B. The loop terminates not when the elision is found, but
     ;          at the next step, where this automaton completes the
     ;          evaluation of the numeric value, and the word is empty
                       (t (list outval trunchar))))
	       (t nil)))
       (let* ((arclabel (cond ((listp (first firstcomp))
                                (first (first firstcomp)))
                              (t (first firstcomp))))
	      (curval (second firstcomp))
	      (arcs (get n-state 'numb-arcs))
	      (foundarc (leggi arcs arclabel))
	      fin)
    ; ***  if the first of firstcomp is a list, then at the previous
    ;      step an elision was found; save the truncated char in trunchar
	     (declare (special curval))
             (cond ((listp (first firstcomp))
                      (setq trunchar (second (first firstcomp)))))
       	     (cond ((null foundarc)
   ; *** there is no defined arc to go on: failure
		      (setq failure t))
		   (t (eval (second foundarc))
   ; *** (second foundarc) could be, for istance, "(incrval)" (cento+sei) or
   ;     "(multval)" (sei*cento)
		      (setq n-state (first foundarc))
                      (cond ((eq n-state 'stop)
                              (setq outval value)
                              (setq final-state t))
	      	            (t (setq fin (get n-state 'final-info))
   ; *** the 'final-info' is something that one must do on the accumulator to
   ;     compute the final value at the end of the word. If the state is not a
   ;     final state, then 'final-info' is NIL
   ; *** so, fin can be one of:
   ;     - NIL: it is not a final state
   ;     - (final): it is a final state, but with no extra operations
   ;     - (final (op vals)): it is a final state with extra operations
		              (cond ((not (null fin))
			              (cond ((not (eq (first fin) 'final))
                                               (exception 'morpho-error
                                                          "PROC/numbers-ita: parsenum" fin))
				            ((null (rest fin))
				               (setq outval value)
				               (setq final-state t))
				            (t (setq outval (eval (ult fin)))
				               (setq final-state t)))))))))))))

; ****************************************************************************
; **** this travels across the low-level automaton to extract meaningful
;      prefixes
;      INPUT: a list of chars
;      OUTPUT: in case of success: ((uparclabel value) remainder)
;	     where "uparclabel" is the label to use to proceed on the upper
;	     level automaton, "value" is the numerical value associated with
;            the recognized prefix and "remainder" is what remains in the word
;            to be analysed
;	     in case di failure ('fail)
(defun numb-split (n-word)
 (let ((curstat 'nu-ch-0) (temp 0) foundarc oper curchar prevchar prev2char)
 (declare (special temp))
  (cond ((null n-word) nil)
; *** la seguente per trattare il numero "tre". Dispiace farlo in modo 
;     cosi' ad hoc, ma modifiche piu' sostanziali dell'automa sarebbero
;     necessarie. V. commenti vari qui e in numbautom. Nota che si 
;     tratta solo del "tre" finale, e non di trecento e tremila, che sono
;     trattati regolarmente
   	(t 
; *** temp is used in the automaton to keep the temporary numerical values
      (do ((arcs (get curstat 'nu-ch-arcs) (get curstat 'nu-ch-arcs)))
; *** end of loop if next-state = stop or no more arcs
	  ((or (eq curstat 'stop) (null arcs))
	    (cond ((eq curstat 'stop)
                    (cond ((listp (first oper))
        ; *** if the first of oper is a list, it must be of the form (elision char),
        ;     where char is the truncated character
                            (cond ((neq (first (first oper)) 'elision)
                                    (exception 'morpho-error
                                               "PROC/parsenumbers; number-split 1" oper)))))
		    (list (list (first oper)
	; *** (first oper) is the up arclabel, or 'elision', if the last char 
        ;     was an apostrophe
			        (eval (second oper)))		; up value
     ; *** the second element of the resulting list is the remainder; but it
     ;     must be expanded with the last char, if there has been a lookahead
     ;     This is determined by the presence of the keyword 'back' in the
     ;     current state (third oper)   
			  (cond ((null (third oper)) n-word)
				((eq 'back (third oper))
                                   (cond ((null curchar) n-word)
                                         (t (cons curchar n-word))))
				((eq '2-back (third oper)) 
                                   (cons prevchar (cons curchar n-word)))
				((eq '3-back (third oper)) 
                                   (cons prev2char 
                                        (cons prevchar (cons curchar n-word))))
                                (t (exception 'morpho-error
                                              "PROC/parsenumbers; number-split 2" oper)))
     ; *** in some cases, also gender and number values are returned. They appear as
     ;     third element of the resulting list. The sixth and seventh are infos
     ;     about the suffix
                          (cond ((null (fourth oper)) nil)
                                (t (list (fourth oper) (fifth oper))))
                          (cond ((null (fourth oper)) nil)
                                (t (list (sixth oper) (seventh oper))))))
		  (t (list 'fail))))
; *** no end of loop; continue
           (setq prev2char prevchar)
           (setq prevchar curchar)
	   (setq curchar (first n-word))
	   (setq foundarc (leggi arcs curchar))
           (cond ((null foundarc)
     ; *** no arc found; try with 'none (but without advancement)
	            (setq foundarc (leggi arcs 'none)))
                 (t (setq n-word (rest n-word))))
     ; *** no arc has been found corresponding to the next char
     ;     of the actions
	   (setq curstat (first foundarc))
	   (setq oper (second foundarc))
	   (cond ((and (not (null oper)) (neq curstat 'stop))
; *** if curstat = stop, then the computation of the total value has not to
;     be done here, but at the exit of the loop
		    (eval oper))))))))
