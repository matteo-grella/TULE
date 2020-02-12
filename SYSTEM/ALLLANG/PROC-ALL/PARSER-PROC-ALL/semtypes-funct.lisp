
(in-package "USER")

;************************************************************************
; *** In this file, functions for the definition of some semantic properties
;     of lexical items. The data are in "lang/KB-lang/GRAMM-KB-lang/semtypes-lang.dat
;************************************************************************

;******************** INITIALIZATION FUNCTIONS ***************************
(defun put-semtype (semdefs)
 (declare (special *CHAR-SET-ID* *LISP-CHAR-SET-ID*))
 (let (fisd sesd)
  (dolist (sd semdefs)
      (setq fisd (first sd))
      (setq sesd (second sd))
 ;       (break "put-semtype")
      (cond ((neq *CHAR-SET-ID* *LISP-CHAR-SET-ID*)
              ;(eq 'UTF-8 *LISP-CHAR-SET-ID*)
               (setq fisd (convert-base-atom-or-string-to-currlisp fisd))
               (setq sesd (mapcar #'convert-base-atom-or-string-to-currlisp sesd))))
      (putprop fisd sesd 'semtdef))))
  
(defun put-semsubcl (subcldef)
 (declare (special *CHAR-SET-ID* *LISP-CHAR-SET-ID*))
 (let (fisc sesc)
  (dolist (sc subcldef)
      (setq fisc (first sc))
      (setq sesc (second sc))
      (cond ((neq *CHAR-SET-ID* *LISP-CHAR-SET-ID*)
              ;(eq 'UTF-8 *LISP-CHAR-SET-ID*)
              (setq fisc (convert-base-atom-or-string-to-currlisp fisc))
              (setq sesc (convert-base-atom-or-string-to-currlisp sesc))))
      (add-prop-val fisc 'sem-subcl sesc)
      (add-prop-val sesc 'sem-supercl fisc))))

;*************************** ACCESS FUNCTION *****************************
(defun inh-member (word class)
; *** class is a single class name or a list of them; if the input is a
;     direct or indirect member of one of the classes, then ok
  (dolist (singc (inlist class))
          (cond ((or (member word (get singc 'semtdef))
                     (inh-member word (get singc 'sem-subcl)))
                   (return t)))))

(defun superc-member (class class-list)
; *** class is a single class (e.g. £geogr-loc); if 'class' is equal to one of 
;     the members of 'class-list' or of any of the superclasses of the members
;     of 'class-list' then return t.
; *** for instance, if class-list is '(£city)' and class is '£geogr-loc', then
;     it returns t, since £geogr-loc is one of the superordinates of £city
  (declare (special *CHAR-SET-ID*))
  (and (not (null class-list))
       (or (member class class-list)
           (superc-member class 
		(dropnil 
		    (mapcan #'(lambda (x) (get x 'sem-supercl)) class-list))))))

