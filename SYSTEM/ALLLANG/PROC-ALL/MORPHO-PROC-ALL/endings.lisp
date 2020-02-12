
(in-package "USER")

;*****************************************************************
; *** This initializes some global variables and calls 'transition
;     on the start node (n1)
(defun endings (word)
; *** double-encl is set to true if, after an enclitic, the character 'e'
;     is found
   (let ((chars (reverse (explode word)))
	     (truebeg t) (pos 0) tempres clitbuff start double-encl)
	   (declare (special chars truebeg tempres clitbuff pos start double-encl))
  ; *** the next includes in the result the empty suffix (as the singular
  ;     form for English)
	   (transition 'n1)
	   tempres))

;*****************************************************************
; *** this accomplishes the transition to a new node
; *** INPUT:
;   >>> node: the node to enter after the transition
(defun transition (node)
  (declare (special *SUFF-NET* start pos))
   (setq pos (1+ pos))
   (cond ((eq node 'n1)
            (setq start pos)
            (setoutp '@empt)))
  (let* ((nodeinfo (leggi *SUFF-NET* node))
	 (curchar (implode (list (first chars))))
         (chars (rest chars))
	 (acts (leggi nodeinfo curchar))
	 path endyn)
  (declare (special chars path endyn curchar))
    ; *** nodeinfo: the arcs exiting 'node'
    ; *** curchar: next character to analyze
    ; *** acts: arcs (actions) associated with the next charaxter
    ; *** endyn: 'final if this is a final node, nil otherwise
    ; *** path: used for bactracking
    ; *** start: says from which character starts the element being analyzed
    ;     in case there is one or more clitic, the analysis restarts from
    ;	  n1, but with a different 'start'
    (cond ((eq node 'n1) (setq start pos)))
    (cond ((not (null acts))
	     (setq path nil)
	     (setq endyn (first acts))
	     (setq acts (rest acts))
             (mapc 'eval acts)))))

;*****************************************************************
; *** It stores the values about the found suffixes in the variable
;     'tempres'
; *** INPUT:
;   >>> suffix: actual suffix found, apart from clitics
; *** OUTPUT (an element of tempres):
;   >>> (sub-element1 ... sub-elementN)
;       sub-elements refer to true entries or clitics, and are composed of:
;	   ((START END) ROOT SUFFIXINFO)
;       where START and END are the position in the (reversed) word of the
;	suffix (START is different from 1, if this element is followed by an
;		enclitic)
;	ROOT is the remaining part of the word (preceding the suffix)
;       SUFFIXINFO is the information retrieved from the table for that suffix
(defun setoutp (suffix &optional savechar)
   (declare (special *SUFF-TABLE* pos start tempres curchar chars clitbuff
                     *LANGUAGE*))
   (let ((suffixinfo (prendi *SUFF-TABLE* suffix))
         (root (cond ((null savechar) (implode (reverse chars)))
                     (t (implode (reverse (cons curchar chars)))))))
     (cond ((= start 1)
	; *** if 'start'=1, then the element found is not followed by an
	;     enclitic
             (setq tempres
                 (cons (list (list (list start pos) root suffixinfo suffix))
                       tempres)))
	; *** otherwise, add the new info to all clitics found
           (t 
             (cond ((and (eq *LANGUAGE* 'english)    ; this to accept "can't"
                         (eq (base-uppercase root) 'ca))
                      (setq root '|can|)))
             (setq tempres 
                (append tempres
                     (dropnil
                         (mapcar #'(lambda (x) 
                                      (add-info-to-clitic suffixinfo root suffix x))
                                clitbuff))))))))

;*****************************************************************
(defun add-info-to-clitic (suffixinfo root suffix prevclit)
  (declare (special start pos chars *LANGUAGE*))
  (cond ((eq start (1+ (second (first (first prevclit)))))
	; *** there is a correspondence between the start of the new element
        ;     and the end of the previous clitics
          (let ((possverb 
                   (filter-verb-with-clitic root (prendi suffixinfo 'verb)))
                actsuffinfo)
              (cond ((not (null possverb))
                       (setq actsuffinfo (list 'verb possverb))))
            ;  (cond ((and (eq *LANGUAGE* 'english)
            ;              (memq (uppercase root) '(I you he she it we they)))
                 ; *** the root is not an acceptable verb for a clitic,
                 ;     but the form could be I'd, which is acceptable
            ;           (setq actsuffinfo
            ;                (append actsuffinfo (list 'adj (prendi suffixinfo 'adj))))))
            ;                  ; *** adj is the morphological class of pronouns
              (cond ((not (null actsuffinfo))
                       (cons (list (list start pos) root actsuffinfo suffix)
                             prevclit))
                    (t nil))))
        (t nil)))
	     
;*****************************************************************
(defun filter-verb-with-clitic (root verbdat)
  ; *** verbdat is a list 
  ;		(class-code1 tense-codes1 ... class-codeN tense-codesN)
  (declare (special *LANGUAGE*))
  (cond ((null verbdat) nil)
        ((memq *LANGUAGE* '(italian catalan spanish))
           (let ((tense-codes (filter-tenses (second verbdat))))
               (cond ((null tense-codes)
                       (filter-verb-with-clitic root (rest (rest verbdat))))
                     (t (cons (first verbdat)
                              (cons tense-codes
                                   (filter-verb-with-clitic root 
                                          (rest (rest verbdat)))))))))
        ((eq *LANGUAGE* 'english)
           (cond ((member (base-uppercase root)
                            '(do doe did is was ha can could
                             ; I you he she it we they
                              )) ; these for I'd, you're, he's ..
                     verbdat)
                 (t nil)))
        (t (exception 'morpho-error "PROC/endings: Unknown language" *LANGUAGE*))))

;*****************************************************************
(defun filter-tenses (tensedat)
   ; *** clitics are admitted for imperatives (tense code = n),
   ;     past participles (tense code = i), gerunds (tense code = l)
   ;     and infinites (tense code = m)
  (cond ((null tensedat) nil)
        ((memq (first tensedat) '(i n))
          (cons (first tensedat)
                (cons (second tensedat)
                      (filter-tenses (rest (rest tensedat))))))
   ; *** gerunds and infinites lack the persnumb specification
        ((memq (first tensedat) '(l m))
          (cons (first tensedat)
                (filter-tenses (rest (rest tensedat)))))
        (t (filter-tenses (rest (rest tensedat))))))

;*****************************************************************
(defun with-enclit ()
  (declare (special clitbuff))
  (cond ((null clitbuff) nil)
        (t t)))

;*****************************************************************
(defun with-gliencl ()
  (declare (special clitbuff))
  (cond ((null clitbuff) nil)
        (t (memq (third (first (first clitbuff))) '(gli glie)))))

;*****************************************************************
(defun with-doublencl ()
  (declare (special clitbuff))
  (cond ((null clitbuff) nil)
        (t (eq (second (first (first clitbuff))) 'doublencl))))

;*****************************************************************
; *** it stores a new clitic in clitbuff
; *** structure of clitbuff:
;     (enclit-1 enclit-2 ... enclit-N)
;	   each enclit-i is
;	  (singlencl-i1 singlencl-i2 ...)
;	   each singlencl-ij is
;     ((start end) CLITIC encl)
;	   where 'encl' is lo, la, le, li, gli, mi, ti, vi, si, ci ...
(defun setencl (enclitic)
  (declare (special start tempres pos chars clitbuff *LANGUAGE*))
  (cond ((= start 1)
	; *** if 'start'=1, then the element found is not followed by an
	;     enclitic
	; *** other enclitics (ce ve te me se glie llo lla lli lle) are
        ;     not admitted at the end of the word; The doubled ones should
	;     find the buffer already containing the corresponding single
          (cond ((or 
                   (and (eq *LANGUAGE* 'italian)
                        (member enclitic '(ci vi ti mi si gli lo la li le ne)))
                   (and (eq *LANGUAGE* 'catalan)
                        (member enclitic 
                               '(hi ho l ls m n ns s t el els em en ens
                                 la les li lo los me ne nos se te vos)))
                   (and (eq *LANGUAGE* 'spanish)
                        (member enclitic '(la las lo los le me te nos os les se)))
                   (and (eq *LANGUAGE* 'english)
                        (eq enclitic 'not))
                  ;      (member enclitic '(am are not would))
                )
                  (setq clitbuff
		; *** the next list is for different outputs
                      (list 
		; *** the next list is for multiple length outputs
                          (list 
		; *** the next list is for the enclitic info
                              (list (list start pos) 
                              'clitic
                              enclitic)))))
                (t nil)))
        ((and (eq *LANGUAGE* 'italian)
              (member enclitic '(l m c t)))
	; *** this is for doubled enclitics (fa-llo, di-mme-lo, rifa-cci, etc.)
	;     the corresponding single enclitic must already be present in the
	;     buffer; it is left unchanged, but the <start, end> pair is
	;     changed, to account for the extended length
          (cond ((doubled-enclitic? chars)
                  (setq clitbuff 
                     (append
                        (mapcar #'(lambda (x) (double-clitic enclitic x)) clitbuff)
                        clitbuff)))
                (t nil)))
        ((or
           (and (eq *LANGUAGE* 'italian)
              (or (member enclitic '(ce ve te me se glie ne))
                  (and (member enclitic '(ci vi))
                       (eq (third (first (first clitbuff))) 'si))))
           (and (eq *LANGUAGE* 'catalan)
                (member enclitic '(m n ns la les li los me nos s se t te us vos)))
           (and (eq *LANGUAGE* 'spanish)
                (member enclitic '(me te nos os se))))
        ; *** otherwise, if the new clitic is of the right form, add it to
	;     all clitics found
        ; *** the second disjunct for forms as 'trovarcisi' 'farvisi'
          (setq clitbuff 
              (append
                 (mapcar #'(lambda (x) (add-clitic-to-clitic enclitic x))
                         clitbuff)
                 clitbuff)))
        (t nil)))

;*****************************************************************
; *** returns true if the verbal root requires doubled enclitic:
;     fallo, dallo, rifallo, ...
(defun doubled-enclitic? (chars)
   (member (base-uppercase (implode (reverse chars)))
           '(fa da di va rifa rida ridi riva sta)))

;*****************************************************************
(defun add-clitic-to-clitic (newclit prevclit)
  (declare (special start pos chars))
  (cond ((= start (1+ (second (first (first prevclit)))))
	; *** there is a correspondence between the start of the new clitic
        ;     and the end of the previous clitics
          (cons (list (list start pos) 'clitic newclit) prevclit))
        (t prevclit)))

;*****************************************************************
; *** checks if an existing clitic has as first character the one given as 
;     input; in fact, in case of doubled clitics (fa-lle), it is first
;     stored in clitbuff the simple clitic (-le), and then it is added -l.
; *** the clitic stored in the buffer (-le) is not changed, except that its
;     position is extended (e.g. from <1 2> to <1 3>)
(defun double-clitic (newclit prevclit)
  (declare (special start pos))
  (let ((firstprev (first prevclit)))
      (cond ((and (= start (1+ (second (first firstprev))))
                  (char= (first (explode newclit))
                         (first (explode (third firstprev)))))
 	; *** here, we could check that the remaining root is the one of a verb
	;     admitting doubling (fare, dire, dare, ri-fare, etc.)
              (cons (list (list (first (first firstprev)) pos)
                          'doublencl
                          (third firstprev))
                    (rest prevclit)))
            (t prevclit))))

;*****************************************************************
; *** this function controls non-deterministic branches.
;     It saves the status before following the first branch, and then
;     restores it before the second branch
; *** the status is represented by the following variables:
;     chars: the characters to be analyzed
;     curchar: the current character
;     pos: position (from the end) of the current character
;     start: position (from the end) where the current element began
(defun twoways (setfun1 setfun2)
  (declare (special chars curchar start pos))
  (let ((savechars chars)
        (savecurchar curchar)
        (savepos pos)
        (savestart start))
    (mapc 'eval setfun1)
    (setq chars savechars)
    (setq curchar savecurchar)
    (setq pos savepos)
    (setq start savestart)
    (mapc 'eval setfun2)))

;*****************************************************************
;   This function calls 'transition'. 
(defun nxt-state-lex (node)
  (declare (special truebeg path endyn))
	; *** truebeg signals the beginning of the word: any transition makes it
        ;     false
  (setq truebeg nil)
  (cond ((eq path 'stop) nil)
        ((eq node 'stop) nil)
        ((eq endyn 'final) (transition node))
        (t (transition node))))
  
; *******************************************************************
; *** lookahead on 'chars' (remaining characters of the word)
(defun set-double-encl ()
  (declare (special double-encl))
  (setq double-encl t))

; *******************************************************************
; *** lookahead on 'chars' (remaining characters of the word)
;     cha can be acharacter or 'vowel or 'consonant
(defun next-char (cha)
  (declare (special chars))
  (cond ((eq cha 'vowel)
           (is-next-char-vowel (first chars) (second chars)))
      ; *** is-next-char-vowel in char-funct.lisp
        ((eq cha 'consonant)
           (not (is-next-char-vowel (first chars) (second chars))))
        (t (equal cha (first chars)))))

; *******************************************************************
;(defun is-next-char-vowel ()
;  (declare (special chars *LANGUAGE*))
;  (let ((cod (char-code (first chars))))
;     (and (or (member (first chars) '(#\a #\e #\i #\o #\u))
;              (and (> cod 223) (< cod 231))
;           ; 224: à; 225: á; 226: â; 227: ã; 228: ä; 229: å; 230: æ; 
;              (and (> cod 231) (< cod 240))
;           ; 232: è; 233: é; 234: ê; 235: ë; 236: ì; 237: í; 238: î; 239: ï;
;              (and (> cod 241) (< cod 247))
;           ; 242: ò; 243: ó; 244: ô; 245: õ; 246: ö;
;              (and (> cod 248) (< cod 253)))
;           ; 249: ù; 250: ú; 251: û; 252: ü; 
;          (not (and (eq *LANGUAGE* 'catalan)      ; dyptongs are not vowels
;                    (or (and (memq (first chars) '(#\i #\í #\ï))
;                             (memq (second chars) 
;                                '(#\a #\à #\e #\è #\é #\o #\ó #\ò #\u #\ú)))
;                        (and (memq (first chars) '(#\u #\ú))
;                             (memq (second chars) 
;                                '(#\a #\à #\e #\è #\é #\i #\í #\ï #\o #\ó #\ò)))))))))
;      
;; *******************************************************************
; *** double lookahead on 'chars' (remaining characters of the word)
(defun next2-char (cha)
  (declare (special chars))
  (equal cha (second chars)))

; *******************************************************************
; *** for catalan enclitics
;     specifies in what cases the enclitics is separated by a hyphen 
(defun with-guio-encl ()
    ;(and (next-char 'consonant)
    ;     (not (next2-char #\-))
    ;     (not (next2-char #\')))
  t
     )
 
; *******************************************************************
; *** for catalan enclitics
;     specifies in what cases the enclitics is separated by an apostrophe
(defun with-apost-encl ()
    (not (and (next-char 'consonant)
              (not (next2-char #\-))
              (not (next2-char #\')))))
 
;************************************************************************
; *** this sets the value of start in order to advance on the clitic
;     "not" in forms as "doesn't"
(defun set-morph-start ()
  (declare (special start pos))
  (setq start (1+ pos)))

