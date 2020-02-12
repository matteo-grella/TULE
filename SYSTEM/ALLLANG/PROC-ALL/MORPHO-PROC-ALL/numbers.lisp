(in-package "USER")

; ###############################################
;   THE FUNCTIONS IN THIS FILE CONVERT A STANDARD NUMBER EXPRESSED
;      IN NUMERICAL CODE DIGITS INTO THE CORRESPONDING LISP VALUE
; ###############################################

;*****************************************************************
; *** converts a number expressed as a sequence of numeric digit codes in a
;     number in internal lisp format. It handles commas and periods as
;     separators, both between thousands and between integral and decimal
;     part of the number.
; *** Both commas and periods are admitted as both kinds of separators.
; *** in case  1, 2, or 3 digits are followed by a single period, which,
;     in its turn, is followed by three digits, then the period is not
;     considered as a separator for decimals (37.666 --> 37666). The
;     opposite holds for commas (Italian notation: 37,666 --> 37.666) 
; *** of course, different situations are not ambiguous (12.33 --> 12.33;
;     13,5794 --> 13.5794; 1111.333 --> 1111.333)
; *** It also handles mixed notations: 12,354,777.55 --> 12354777.55;
;     12.354.777,55 --> 12354777.55
; *** Notice that the sequence has already been recognized as a number by
;     the tokenizer; so, this function will never receive as input
;     sequences as 22.33.44, or 22.33,44, etc.
; *** the conversion seems to work properly; the only problem I've found
;     refers to decimals. For instance, 1234567,59 --> 1234567.6 and
;     0,47 --> 0.46999997
;     Note, however, that 33,59 --> 33.59.

; *** the first basic step for all cases is to convert the numerical codes into
;     their tule-names counterpart; though most codes use the ASCII basic codes
;     for decimal digits, this seems safer
(defun from-tule-char-names-to-integers (tulenames)
  (let* ((separator (inspect-number-tulenames tulenames))
	 (int-dec (separate-number tulenames separator nil)))
     (+ (integral-part (first int-dec)) (decimal-part (second int-dec)))))

; ******************************************************************************
; *** this and next auxiliary function return the numerical value of an integer
;     expressed as a sequence of tule char names, e.g. (one seven three) --> 173
(defun integral-part (tulenames)
  (i-int-p (reverse tulenames)))

(defun i-int-p (tulenames)
  (cond ((null tulenames) 0)
	(t (+ (digit-tulename-val (first tulenames))
              (* 10 (i-int-p (rest tulenames)))))))
  
; ******************************************************************************
; *** this returns the value of a sequence of tutle char names intended as a
;     decimal part of a number, e.g. (one seven three) --> .173
(defun decimal-part (tulenames)
  (cond ((null tulenames) 0)
	(t (float (/ (+ (digit-tulename-val (first tulenames))
	      		(decimal-part (rest tulenames))) 10)))))

; ******************************************************************************
; *** this returns the numerical value of digits expressed as tule names
(defun digit-tulename-val (tulename)
  (cond ((eq tulename 'zero) 0)
        ((eq tulename 'one) 1)
        ((eq tulename 'two) 2)
        ((eq tulename 'three) 3)
        ((eq tulename 'four) 4)
        ((eq tulename 'five) 5)
        ((eq tulename 'six) 6)
        ((eq tulename 'seven) 7)
        ((eq tulename 'eight) 8)
        ((eq tulename 'nine) 9)
        (t (exception-nothrow "Non-digit char in MORPHO-PROC/numbers"))))

; ******************************************************************************
; *** returns a pair, where the first element is the tulenames list of the
;     integral part, and the second element is the tulenames list of the decimal
;     part
; *** it already knows if the decimal separator is the comma or the period
(defun separate-number (tulenames separator integracc)
  (cond ((null tulenames) (list integracc nil))
; *** is the comma or the period, but not the separator: skip it
	((or (eq (first tulenames) 'comma)
	     (eq (first tulenames) 'period))
	   (cond ((eq (first tulenames) separator)
	  	   (list integracc (rest tulenames)))
		 (t (separate-number (rest tulenames) separator integracc))))
	(t (separate-number (rest tulenames) 
			   separator 
			   (append1 integracc (first tulenames))))))

; ******************************************************************************
; *** this function inspects the tulenames sequence of a number to determine
;     which is the separator between integral and decimal part (if any)
; *** The correctness of the sequence has already been checked by the
;     tokenizer
; *** in principle, the last separator (comma or period) appearing in the
;     sequence is the separator
; *** However, sequences as 123.547 and 12,229 are ambiguous. In such cases,
;     the Italian form is preferred, so the period is interpreted just as
;     a separator for thousands, and the comma as a separator for decimals
(defun inspect-number-tulenames (tulenames)
  (let ((result 'all-digits) (count 0) (init0 nil) tempres)
  (setq tempres
     (dolist (nxt tulenames result)
	(cond ((eq nxt 'comma)    ; **********************************************
		(cond ((eq result 'all-digits)	; just digits until now
			(cond ((< count 4)
; *** if less than four digits before the comma, then ambiguity remains
				(setq result 'found-comma))
			      (t (setq result 'commafin))))
; *** if more than 3 digits before the comma, then it must be the separator
		      ((eq result 'foundcomma)	; already found a comma
; *** more than one comma (ex. 12,345,345): it cannot be the separator
			(setq result 'commanot))
		      ((or (eq result 'found-period)	; a comma after a period
		           (eq result 'periodposs)
		           (eq result 'periodnot))
			(setq result 'commafin)))
		(setq count 0))
; *** then, the comma must be the separator
	      ((eq nxt 'period)    ; **********************************************
		(cond ((eq result 'all-digits)	; just digits until now
			(cond ((and (= count 1) init0)
; *** the form is 0.xxx: the period is the separator
				(setq result 'periodfin))
			      ((< count 4)
; *** if less than four digits before the comma, then ambiguity remains
				(setq result 'found-period))
; *** if more than 3 digits before the period, then it must be the separator
			      (t (setq result 'periodfin))))
		      ((or (eq result 'found-period)	; already found a period
			   (eq result 'periodposs))
			(setq result 'periodnot))
		      ((or (eq result 'found-comma)	; a period after a comma
		           (eq result 'commanot))
			(setq result 'periodfin)))
		(setq count 0))
	      (t (cond ((and (= count 0) (eq result 'all-digits) (eq nxt 'zero))
; *** we are at the very beginning and there is a trailing 0;
			(setq init0 t))
		       ((and (= count 2) (eq result 'found-period))
; *** exactly three digits after a single period; possibly a period
			 (setq result 'periodposs))
		       ((= count 3)
			 (cond ((eq result 'found-comma)
				  (setq result 'commafin))
			       ((eq result 'periodposs)
				  (setq result 'periodfin)))))
		 (setq count (1+ count))))))	; one more digit
  (cond ((eq tempres 'all-digits) 0)
; *** more than three digits or a period before a comma or more than three
;     digits after a comma
	((eq tempres 'commafin) 'comma)	
; *** two or more commas
	((eq tempres 'commanot) 'period)
; *** less than four digits before a single comma: ambiguous; prefer separator
	((eq tempres 'found-comma) 'comma)
; *** more than three digits or a comma before a period or more than three
;     digits after a period
	((eq tempres 'periodfin) 'period)	
; *** two or more periods
	((eq tempres 'periodnot) 'comma)
; *** less than four digits before a single period and exactly three digits
;     after it: ambiguous; prefer not separator
	((eq tempres 'periodposs) 0)
; *** less than four digits before a single period, but less than three
;     digits after it: separator
	((eq tempres 'found-period) 'period))))

