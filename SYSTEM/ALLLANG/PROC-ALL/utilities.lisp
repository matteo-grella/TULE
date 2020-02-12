(in-package "USER")

;###########################################################################
;*** SHORTER NAMES FOR SOME LISP UTILITIES
;###########################################################################

;ppp resetta le variabili globali del LISP PRINT-LEVEL e PRINT-LENGTH
(defun ppp()
  (setq *print-level* nil)
  (setq *print-length* nil))

;spl: e` la symbol-plist
(defun spl(x)
  (symbol-plist x))

;npl: e` la naplist
(defun npl(x)
  (naplist x))

;###########################################################################
;*** INTERACTION WITH THE USER
;###########################################################################
;************************************************************
; *** This checks if the user answer is within the range of valid responses
;     range : set of valid responses
;     flag  : says if the answer must belong (flag=0) or be a subset of
;             (flag=1) the range
(defun check_risp (range flag)
  (do ((risp (read) (read)))
      ((or (and (= flag 0) (atom risp) (member risp range))
           (and (= flag 1) (listp risp) (subset risp range)))
        risp)
    (cond ((and (eq flag 0) (listp risp))
            (format t "The answer must not be a list; the range is: ~s ~%"
                      range))
          ((and (eq flag 1) (atom risp))
            (format t "The answer must be a list; the range is: ~s ~%" range))
          (t (format t "Wrong answer, the range is: ~s ~%" range) ))))

;!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!
; le funzioni che seguono servono per mantenere la compatibilita'
; tra FRANZ e COMMON
;!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!%!

(cond ((not (fboundp 'putprop))
        (defun putprop (atom val prop) 
	    (cond ((null atom) 
                     (exception 'generic-error "PROC/utilities: putprop to nil"))
	          (t (setf (get atom prop) val))))))

; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
; aggiunge il valore newval in testa alla proprieta' property
; (multivalued) del nodo node
; N.B. Si aggiunge solo se il valore non e` gia` presente!!!!!
(defun add-prop-val (node property newval)
  (cond ((null node)
            (exception 'generic-error "PROC/utilities: add-prop-val to nil"))
	((not (member newval (get node property) :test #'equal))
	     (putprop node (cons newval (get node property)) property))))

(defun terpr (&optional sstream) (terpri sstream))

(defun alphalessp (name1 name2)
  (string-lessp (symbol-name name1) (symbol-name name2)))

; *******************************************
; *** the same as above, but keeping the downcase-uppercase distinction
(defun alpha< (name1 name2)
  (string< (symbol-name name1) (symbol-name name2)))

(defun patom (a &optional p)
  (princ a p))

(defun tyi (&optional sstream eof-p eof-val)
  (char-code (read-char sstream eof-p eof-val)))

; *** adds an element to the end of a list
(defun append1 (lis el) (append lis (list el)))

; *** as append1, but if the element is nil, it is not added
(defun append2 (lis el)
   (cond ((or (null el) (null lis)) (append lis el))
	 (t (append lis (list el)))))

; *** as append2, but if the the list is nil, the element is
;     put in a list
(defun append3 (lis el)
   (cond ((null el) lis)
	 (t (append1 lis el))))

; *******************************************************
(defun setplist (s l) (setf (symbol-plist s) l))

(defun copy (c) (copy-tree c))          ; ok ??

(defun uconcat (&rest x)
  (cond (x (format nil "~{~a~}" x)) ))
    
(defun concat (&rest x) (identity (intern (string (apply #'uconcat x)))))

(defun concatl (l)
  (apply 'concat l))

;(defun times (a b) (* a b))             ;(*)

(defun plus (a b) (+ a b))              ;(*)
(defun add  (a b) (+ a b))              ;(*)

(defun diff (a b) (- a b))              ;(*)
 
(defun add1 (n) (1+ n))                 ;(*)
 
(defun sub1 (n) (1- n))                 ;(*)

(defun greaterp (x y) (> x y))          ;(*)

(defun neq (el1 el2) (not (eq el1 el2)))

(defun ncons (arg)
  (cons arg nil))

(defun memq (el l) (member el l :test #'eq))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; *** file extensions
;************************************************************************
; *** these three functions modify the extension of a file name
;       ex. pippo.dat --> pippo.dis
(defun change-extens (filename newext)
  (put-extens (implode (removext (explode filename))) newext))

(defun put-extens (x y)
  (string (concatl (list x y))))

(defun removext (explname)
  (cond ((null explname) nil)
        ((equal (first explname) #\.)
          (cond ((equal (second explname) #\.)
                  (cons (first explname)
                       (cons (second explname)
                            (removext (rest (rest explname))))))
                (t nil)))
        (t (cons (first explname) (removext (cdr explname))))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun checkanswer (altern)
;****** checks whether a user's answer is among the possible alternatives
  (prog (ans)
 retry (setq ans (read))
;*** 1. la risposta appartiene all'insieme delle possibilita'
;*** 2. la risposta e' un sottoinsieme di tutte le possilita'
    (cond ((member ans altern :test #'equal) (return ans))
	  ((and (listp ans) (subset ans altern)) (return ans))
	  (t (format t "Risposta non corretta. Le possibili alternative sono:
	      ~a ~%Prova ancora! ~%" altern)
	     (go retry)))))

;#####################################################################
;*** FUNZIONI PER OPERAZIONI SU LISTE, INSIEMI, E LISTE DI PROPRIETA`
;#####################################################################

;*****************************************************************
; *** returns true if all elements of 'l' are 'nil'
(defun all-null (l)
   (cond ((null l) t)
         ((null (first l)) (all-null (rest l)))
         (t nil)))

; *** as cons, but if el is nil, lis remains unchanged
(defun cons-nonil (el lis)
  (cond ((null el) lis)
        (t (cons el lis))))

; *** if el is a list, it is appended at the beginning of lis, otherwise,
;     it is cons-ed
(defun cons-or-append (el lis)
  (cond ((listp el) (append el lis))
        (t (cons el lis))))

; *** cons-es the first argument to all sublists of the second argument
(defun mult-cons (el lislis)
  (mapcar #'(lambda (x) (cons el x)) lislis))

; *** takes the first n elements of a list
;     if the list is shorter than n, it returns the full list
(defun first-n (n lista)  
  (cond ((null lista) nil)
        ((= n 0) nil) 
        (t (cons (first lista) (first-n (1- n) (rest lista))))))

;*** returns the last element of a list
(defun ult (li)
  (car (last li)))

;*** checks if two lists are set-equal
(defun equal-set (a b)
  (equal (sort (elimdup a) #'alphalessp)
         (sort (elimdup b) #'alphalessp)))

; *** returns the value of assoc list (N.B. the position of the arguments
;     is reversed, with respect to assoc)
(defun leggi (node value)
  (cdr (assoc value node)))

; **************************************************************
; *** as the one above, but trying all ways to put accents
(defun all-accent-leggi (data key)
  (let (up-extern alloptions)
     (multiple-value-setq (alloptions up-extern) (final-accent-and-uppercase key))
     (do* ((result (leggi data (first alloptions))
                   (leggi data (first alloptions)))
           (alloptions (rest alloptions) (rest alloptions)))
          ((or (null alloptions) result)
              result))))

; **************************************************************
(defun change-val (wordl prop newvalue)
;***** rimpiazza il valore della feature prop della lista wordl
;      con newvalue; se prop non esiste viene aggiunta col nuovo
;      valore; la funzione interna serve solo per saltare il primo
;      elemento della lista (che e` la forma normalizzata)
   (cons (car wordl) (ch-val1 (cdr wordl) prop newvalue)))

(defun ch-val1 (wordl prop newvalue)
   (cond ((null wordl) (list prop newvalue))
	 ((eq (car wordl) prop)
	    (cons prop (cons newvalue (cddr wordl))))
	 (t (cons (first wordl) 
		  (cons (second wordl)
			(ch-val1 (cddr wordl) prop newvalue))))))

(defun disallista (lista)
;*** serve a fare: ((a c) (b d)) ----> (a b) (per relazioni grammaticali)
 (mapcar 'car lista))

; *** appends all sublist of the given list
(defun flatten (l)
 (cond ((null l) nil)
       (t (append (first l) (flatten (rest l))))))

; *****************************************************************************
; *** this function transforms the list of lists 'listlist' into a flattened
;     list, but trying to preserve the order of the original lists. It first
;     takes all first elements, then all second elements, and so on
(defun merge-flatten (listlist)
  (cond ((null (subtrl listlist '(nil))) nil)
        (t (append (mapcar #'first listlist)
                   (merge-flatten (mapcar #'rest listlist))))))

;***** picks the element of the list l that comes after the search key
(defun prendi (l key)
  (do ((li l (rest li)))
      ((or (null li)
           (eq (first li) key))
        (second li))))

(defun gen-prendi (l key)
;***** returns all the elements in the sublists of the list l that 
;***** come after the search key
  (cond ((null l) nil)
        (t (append (list (prendi (first l) key))
                   (gen-prendi (rest l) key)))))

;************************************************************************
; *** as "prendi", but advancing two elements instead of one
(defun prendi1 (l key)
  (do ((li l (rest (rest li))))
      ((or (null li)
           (eq (first li) key))
        (second li))))

;************************************************************************
; *** eliminates the nils from a list
(defun dropnil (l)
  (cond ((null l) nil)
        ((null (car l)) (dropnil (cdr l)))
        (t (cons (car l) (dropnil (cdr l))))))

;************************************************************************
; *** as "dropnil", but this checks if the input is an atom
(defun atom-dropnil (el)
   (cond ((atom el) el)
         (t (dropnil el))))

;************************************************************************
; *** deletes a search key, and the value that follows it, from a list
(defun dropprop (list property)
  (cond ((null list) nil)
        ((null property) nil)
        ((eq  (first list) property) (rest (rest list)))
        (t (cons (first list) (dropprop (rest list) property)))))

;************************************************************************
; *** deletes the duplicates from a list
(defun elimdup (list)
  (cond ((null list) nil)
        ((member (first list) (rest list) :test #'equal) (elimdup (rest list)))
        (t (cons (first list) (elimdup (rest list))))))

(defun elimdup-by-key (list)
;***** elimina da una lista di liste tutte le sottoliste che hanno il primo
;      elemento in comune con un'altra sottolista (lasciandone una)
    (cond ((null list) nil)
	  ((member (caar list) (mapcar #'car (cdr list)))
	     (elimdup-by-key (cdr list)))
	  (t (cons (car list) (elimdup-by-key (cdr list))))))

(defun subset (l1 l2)
;***** determina se l1 e' un sottoinsieme di l2
    (cond ((null l1) t)
	  ((member (car l1) l2 :test #'equal) (subset (cdr l1) l2))
	  (t nil)))

(defun sublist-memb (el list)
;verifica se el e` contenuto in una sottolista di list
;NB: list e` una lista a due soli livelli
  (cond ((null list) nil)
        ((member el (car list)) t)
        (t (sublist-memb el (cdr list)))))

; *** this is my version of sublist (which is not defined in Lisp)
(defun part-of-list (l1 l2)
; *** checks if the elements of l1 occur (in the same order) in l2
   (cond ((null l2) nil)
	 ((equal (car l1) (car l2))
	    (cond ((rest-p-o-l (rest l1) (rest l2)) t)
		  (t (part-of-list l1 (rest l2)))))
	 (t (part-of-list l1 (rest l2)))))

(defun rest-p-o-l (l1 l2)
   (cond ((null l1) t)
	 ((equal (first l1) (first l2))
	   (rest-p-o-l (rest l1) (rest l2)))
	 (t nil)))

(defun inters (l1 l2)
;***** determina l'intersezione tra l1 e l2
  (cond ((null l1) nil)
	((member (car l1) l2) (cons (car l1) (inters (cdr l1) l2)))
	(t (inters (cdr l1) l2))))

(defun inlist (elem)
;***** if an element is non-null atom, it is put in a list
  (cond ((null elem) nil)
	((atom elem) (list elem))
	(t elem)))

(defun list-nonil (elem)
;***** adds a level of parentheses, unless elem is nil
  (cond ((null elem) nil)
	(t (list elem))))

(defun ishole (l)
;*** verifica la presenza di un nil(buco) in una lista
  (cond ((null l) nil)
	((null (car l)) t)
	(t (ishole (cdr l)))))

(defun subtrl (l1 l2)
;*** toglie gli elementi di l2 da l1 (l1-l2 insiemistico)
  (cond ((null l1) nil)
	((member (car l1) l2 :test #'equal) (subtrl (cdr l1) l2))
	(t (cons (car l1) (subtrl (cdr l1) l2)))))

(defun subtr-by-key (ll1 l2)
;*** toglie da ll1 (una lista di liste) gli elementi (sottoliste) che hanno
;    come primo elemento un elemento di l2
  (cond ((null ll1) nil)
	((member (caar ll1) l2 :test #'equal) (subtr-by-key (cdr ll1) l2))
	(t (cons (car ll1) (subtr-by-key (cdr ll1) l2)))))

(defun isin (el possl)
;***** verifica se un elemento appartiene a una lista
  (cond ((atom possl) (equal el possl))
	(t (member el possl))))

(defun memb-or-eq (el possl)
;***** come la precedente, ma il nome sembra più sensato
  (cond ((atom possl) (equal el possl))
	(t (member el possl))))

(defun unique (el lista)
;****** verifies whether "el" appears in "lista" just once
  (prog (temp)
    (setq temp (member el lista))
    (return (and temp (not (member el (cdr temp)))))))

(defun presentp (a s)
;*** funzione che verifica la presenza di un atomo in una lista a
;*** qualsiasi livello di annidamento
 (cond ((equal a s) t)
       ((atom s) nil)
       (t (or (presentp a (cdr s)) (presentp a (car s))))))

(defun all-equal (l)
; *** retuns true if l is empty or all of its elements are equal
  (cond ((null l) t)
        (t (int-all-equal (first l) (rest l)))))

(defun int-all-equal (el li)
  (cond ((null li) t)
        ((equal el (first li))
           (int-all-equal el (rest li)))
        (t nil)))

; *************************************************
; *** removes from a list 'count' elements, starting
;     fromt the start-th element (the first element is 1)
(defun rem-from-list (l start count)
   (append (first-n (1- start) l)
	   (nthcdr (1- (+ start count)) l)))

;*****************************************************************************
; *** builds a list of equal elements of length 'count'
(defun makeli (count el)
 (let (res)
  (do ((i 1 (1+ i)))
      ((> i count) res)
      (setq res (cons el res)))))

;**********************************************************************
; *** as above, but repeating the work for all sublists of list-of-lists
(defun mult-makeli (list-of-lists elem)
   (cond ((null list-of-lists) nil)
         (t (cons (makeli (length (first list-of-lists)) elem)
                  (mult-makeli (rest list-of-lists) elem)))))

;*****************************************************************************
; **** builds a list with the first n integers
(defun make-number-list (n)
   (do ((res nil (append1 res val))
	(val 1 (1+ val)))
       ((> val n) res)))

; *****************************************************************
(defun list-to-number (code-list)
; *** converts a list of digits into a number; the digits are represented as
;     ascii codes
  (cond ((null code-list)
           (exception 'morpho-error "PROC/tb-functions: In list-to-number"))
        (t (int-l-t-n code-list 0))))

; *****************************************************************
(defun int-l-t-n (code-list acc)
  (cond ((null code-list) acc)
        (t (int-l-t-n (cdr code-list) (+ (* 10 acc) (- (car code-list) 48))))))
; *** 48 is the char-code of zero

; *******************************************************************
; *** evaluates the union of the members of a list:
;	'((a b) (a c)) --> '(b c a)
(defun mult-union (ll)
  (cond ((null ll) nil)
        (t (union (car ll) (mult-union (cdr ll)) :test #'equal))))

; *******************************************************************
; *** builds a list whose first element is the mult-union of the first
;     elements of the sublists of ll, whose second element is the mult-union
;     of the second elements of ll, and so on. The sublists of ll are assumed
;     to be of equal length
;	'(((a b) (a d f)) ((a c) (b d))) --> '((b c a) (f a d b))
(defun mult-mult-union (ll)
  (cond ((null (car ll)) nil)
        (t (cons (mult-union (mapcar #'car ll))
                 (mult-mult-union (mapcar #'cdr ll))))))

; *******************************************************************
(defun permute (lis)
; *** builds all the list including the same element of lis, in all possible
;     orders
   (cond ((null (rest lis)) (list lis))
         (t (let (curres)
               (dolist (nextel lis curres)
                    (setq curres (append curres 
                                     (mapcar #'(lambda (x) (cons nextel x))
                                            (permute 
                                               (subtrl 
                                                    lis (list nextel)))))))))))

; *******************************************************************
; *** cartesian product of two lists
;     it returns a list of two-elements lists, not of dotted pairs
(defun cartes (l1 l2)
  (cond ((null l1) nil)
        (t (append (mapcar #'(lambda (x) (list (first l1) x)) l2)
                   (cartes (rest l1) l2)))))

; *******************************************************************
; *** cartesian product of n lists (given as sublists of the unique
;     argument:
;     ((a b) (c d) (e f)) --> ((a c e) (a c f) (a d e) (a d f) (b c e) ...)
; *** Note that (list-cartes '((a b c))) is ((a) (b) (c))
(defun list-cartes (l)
  (cond ((null l) nil)
        ((eq 1 (length l)) (mapcar #'list (first l)))
        ((eq 2 (length l)) (cartes (first l) (second l)))
        (t (let ((tempcart (cartes (first l) (second l))))
              (dolist (nxtset (rest (rest l)) tempcart)
                  (setq tempcart (cartes-int tempcart nxtset)))))))

; *******************************************************************
; *** the same as cartes, but the first argument is a list, so
;     append1 is needed instead of "list"
(defun cartes-int (l1 l2)
  (cond ((null l1) nil)
        (t (append (mapcar #'(lambda (x) (append1 (first l1) x)) l2)
                   (cartes-int (rest l1) l2)))))

; *******************************************************************
; *** adds an element to a list (if not already present)
(defun add-to-set (el l)
   (cond ((member el l) l)
         (t (cons el l))))

; *******************************************************************
; *** converts the time units into seconds
(defun print-time (timeval)
   (/ (float timeval) (float internal-time-units-per-second)))

; *******************************************************************
; *** this is to manage errors in different ways, according to the system's
;     context
; *** if the *SYSTEM-CONTEXT* is HOPS, then the server must go on running, and
;     a throw is made to the main
; *** otherwise (HOPS-DIAL or TULE) some debug is required, so the error message
;     is printed on the keyboard and the execution is suspended (break)
(defun exception (label descr &optional var var2)
  (declare (special *SYSTEM-CONTEXT*))
  (cond ((memq *SYSTEM-CONTEXT* '(hops flat-analysis))
          (throw label 
              (cond ((null var) (list descr))
                    ((null var2) (list descr var))
                    (t (list descr var var2)))))
	(t (format t descr)
           (cond ((not (null var))
                    (format t " ~a" var)))
           (cond ((not (null var2))
                    (format t " ~a" var2)))
           (break ""))))

; *******************************************************************
; *** as above, but if the context is HOPS, then no throw is made, and the
;     execution continues
(defun exception-nothrow (descr &optional var var2)
  (declare (special *SYSTEM-CONTEXT*))
  (cond ((memq *SYSTEM-CONTEXT* '(hops flat-analysis)) nil)
	(t (format t descr)
           (cond ((not (null var))
                    (format t " ~a" var)))
           (cond ((not (null var2))
                    (format t " ~a" var2)))
           (break ""))))

; *******************************************************************
; *** as above, but if the context is other than HOPS, there is no break
;     but just the throw.
; *** This is useful in case a possible failing function is included in
;     a loop or mapcar. In these cases, the single failure should not 
;     cause a true error, but just a jump to the right place.
(defun exception-internal (label descr &optional var var2)
  (declare (special *SYSTEM-CONTEXT*))
  (cond ((memq *SYSTEM-CONTEXT* '(hops flat-analysis)) nil)
        (t (throw label 
              (cond ((null var) (list descr))
                    ((null var2) (list descr var))
                    (t (list descr var var2)))))))

