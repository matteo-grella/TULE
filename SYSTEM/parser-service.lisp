
(setf custom:*suppress-check-redefinition* t)

(defvar *HOME-DIR* "")

(defvar *LISP-CHAR-SET-ID* 'UTF-8)
; *** this encodes the character encoding scheme used by the Lisp machine

; (defvar *CHAR-SET-ID* 'ISO-8859-1) ; Iso Latin 1: ISO/IEC 8859-1
  (defvar *CHAR-SET-ID* 'UTF-8)
; *** this encodes the character encoding scheme of the input data

(defvar *SYSTEM-CONTEXT* 'tule)
;(defvar *SYSTEM-CONTEXT* 'hops-dial)
         ; other possible values are hops and hops-dial
; *** the next few functions are needed for "build-file-name", which is in
;     turn needed for loading all files, among which "utilities"

(defun uconcat (&rest x)
  (cond (x (format nil "~{~a~}" x)) ))

(defun concat (&rest x) (identity (intern (string (apply #'uconcat x)))))

(defun build-file-name (filen) (string (concat *HOME-DIR* filen)))

(defun build-subdir-file-name (filen sub) (string (concat *HOME-DIR* sub filen)))

(defun removePrefix (prefisso-stringa) (cond ((search "italian (1) " prefisso-stringa) (subseq prefisso-stringa 11))  (t prefisso-stringa)))

;********* General utilities ***************************************
(load (build-file-name "ALLLANG/PROC-ALL/utilities"))

(defvar loadresult)	         ; for testing the correct loading of knowledge bases
(defvar *LANGUAGE*)
(defvar *TEMP*)

     ; *** the three next variables hold the name of the lisp properties associated
     ;     with the dictionary entries
(defvar *MAIN-DICT*)
(defvar *PROPER-DICT*)
(defvar *SIGLE-DICT*)
(defvar *PRINT-LEV*)

(defvar *TREE-FORMAT*)	; may be "tut" or "avm"; set in main or in chunk-parser
(setq *TREE-FORMAT*  'avm)
;********* The variable associated with the ontology cache *********************
(defvar *ONTO-CACHE-FILE* (build-file-name "ALLLANG/KB-ALL/SEMANT-KB-ALL/onto-cache.dat"))


(setq *LANGUAGE* 'italian)
(setq *MAIN-DICT* 'diz-gw)
(setq *PROPER-DICT* 'diz-pn)
(setq *SIGLE-DICT* 'diz-sig)

(defun server ()

    (format t "Server loading...~%")
    (setq temp (concatenate 'string (first EXT:*ARGS*) "load-ita"))
    (load temp :extra-file-types EXT:*ARGS*)
    (format t "Server ready (port 2728).~%")
    (setq temp nil)

    (do	()(temp)
        (setq sock (socket-server (parse-integer  "2728") :BACKLOG 1000 ))
        (setq connection (socket-wait sock))

        (cond
            (connection 
                ;(format t "~%received call...")
                (setq stream (socket-accept sock :EXTERNAL-FORMAT charset:UTF-8))
                (setq line-val (removePrefix (read-line stream nil nil)))
                ;(setq line-val (read-line stream nil nil))			

                ;(format t "~line: ~A" line-val)
                ;(format t "~%charset: ~A" CUSTOM:*TERMINAL-ENCODING* )
				;(format t "~%charset: ~A" CUSTOM:*PATHNAME-ENCODING* )
				;(format t "~%charset: ~A" CUSTOM:*DEFAULT-FILE-ENCODING* )
				;(format t "~%charset: ~A" CUSTOM:*FOREIGN-ENCODING* )
				;(format t "~%charset: ~A" CUSTOM:*MISC-ENCODING* )
				;(setq pippo (CONVERT-STRING-TO-BYTES line-val charset:CP437))
				;(format t "~%BYTES: ~A" pippo)

                ;(setq tagres (ana-text+tag line-val))
                (setq tagres (hops-ana-text+tag line-val))
                ;(setq tagres (file-ana-text+tag line-val))

                ;(print (start-parse-sentences tagres))
                ;(print '^^^^^^^)
                ;(print (mergeresult (start-parse-sentences tagres)))

                (setq parseres (mergeresult (start-parse-sentences tagres))); Full Dependency Parsing

                ; (print parseres)
                ;(setq albero (reshuffle-tree parseres))
                (setq dependency-tree (plain-tree-to-parsit-json parseres)) ; Format converter
    
				;(print dependency-tree)
			
                (format stream "~a~%" dependency-tree)

                (force-output stream)
                ;(format t "~%sent to client client:~% ~a~%" albero) ;@dd
                (close stream)
                (socket-server-close sock))
                (t (setq temp 1))))

                ;	(socket-server-close sock)
  )

;;;;;;;;;;;;;;;;;;;;

(defun plain-tree-to-parsit-json (parseres)

	(let ((atoms (make-hash-table :test 'equal)) (count 0) jsentence json-ret (cid-count 0) (json-atom ""))
	
		(dolist (sentence parseres)
			(setq count 0)
			(setf jsentence ())
			
			(dolist (word sentence)
				(incf count)
				(setf strposit (format nil "~a"  (second (assoc 'posit word))))
				(setf (gethash strposit atoms) count)
				(setf jsentence (append1 jsentence (list :ID count :DATA word))))
			
			(setf (gethash "0" atoms) 0)
			
			(when (> cid-count 0)
				(setf json-ret (concatenate 'string json-ret ", ")))
				
			(setf json-ret (concatenate 'string json-ret "["))
				
			;; creo json della sentence corrente
			(dolist (word jsentence)
				(incf cid-count)
				(setf json-atom "")
				;(TREE ((PARENT 3) (LABEL VERB-SUBJ/VERB-INDCOMPL-AGENT)))))
				(setf (getf word :HEAD) (gethash (format nil "~a" (second (assoc 'PARENT (second (assoc 'TREE (getf word :DATA)))))) atoms))
			
				;(COREF ((LINE 2) (CTYPE #\p)))
				(when (assoc 'COREF (getf word :DATA))
					(unless (equal (second (assoc 'COREF (getf word :DATA))) 'EMPTY)
						(setf (getf word :COREF) (gethash (format nil "~a" (second (assoc 'LINE (second (assoc 'COREF (getf word :DATA)))))) atoms))))
						
				
				; DEPREL
				(setf (getf word :DEPREL) (format nil "~a" (second (assoc 'LABEL (second (assoc 'TREE (getf word :DATA))))) atoms))
				
				
				;; normalize deprel
				(setf list-deprel (string-list (getf word :DEPREL) #\/))
			
				(let ((new-deprel ""))
					(dolist (dep list-deprel)
						(if (search "-" dep)
							(setf new-deprel (concatenate 'string  new-deprel "/" (subseq dep (1+ (search "-" dep)))))
							(setf new-deprel (concatenate 'string  new-deprel "/" dep)))
							)
							
					(setf new-deprel (subseq new-deprel 1))
					
					(setf  (getf word :DEPREL) new-deprel))
				
				; POS
				(setf (getf word :POS) (format nil "~a" (second (assoc 'CAT (second (assoc 'SYN (getf word :DATA))))) atoms))
                (setf syn (second (assoc 'SYN (getf word :DATA))))
                (setf lemma (string-downcase (string (second (first syn)))))
                (setf form (string (second (assoc 'FORM (getf word :DATA)))))
                
                (print word)
                
				(setf json-atom 
					(format nil 
						"{\"cid\":~a, \"type\":\"word\", \"surface\":{\"form\":~a, \"lform\":~a, \"wnum\":~a, \"lexcat\":~a, \"nxtspace\":true,\"NVAL\":0.0}, \"morpho\":[{\"lemma\":~a}], \"syn\":{\"id\":~a,\"head\":~a,\"edge\":[{\"pos\":\"~a\",\"deprel\":\"~a\"}]},\"coref\":[],\"sem\":[]}"
						
						cid-count
						(to-json (string form))
						(to-json (string-downcase form))
                        (getf word :ID) ; TODO.. da sistemare "della"
                        
                        (if (and (equal form "t") (equal lemma "generic-t"))
                            (to-json (string "TRACE"))
                            (to-json (string "GW")))
                        
                        (to-json lemma)
						
                        (getf word :ID) ; TODO.. da sistemare "della"
						(getf word :HEAD)						
						(getf word :POS)
						(getf word :DEPREL)						
						
						))
				
				
				;; end loop
				(when (> (getf word :ID) 1)
					(setf json-ret (concatenate 'string json-ret ", ")))
					
				(setf json-ret (concatenate 'string json-ret json-atom))
				)
				
					

			(setf json-ret (concatenate 'string json-ret "]"))
			)
			
			
			
		(concatenate 'string "[" json-ret "]")))
			
(defmethod to-json ((string string))
  (with-output-to-string (stream)
    (flet ((write-characters (string)
             (loop for c across string
                do (write-char c stream))))
      (write-char #\" stream)
      (loop for char across string
         do (case char
              (#\newline (write-characters "\\n"))
              (#\return (write-characters "\\r"))
              (#\tab (write-characters "\\t"))
              (#\" (write-characters "\\\""))
              (#\\ (write-characters "\\\\"))
              (t (if
                  ;; assume our characterset contains the ascii
                  ;; characters in the same order as ascii and
                  ;; that they're presented in one block without
                  ;; other characters in between.  This seems to
                  ;; be a reasonable assumption to make.
                  (or (< (char-code char) (char-code #\ ))
                      (> (char-code char) (char-code #\~)))
                  (write-characters (format nil "\\u~4,'0X" (char-code char)))
                  (write-char char stream)))))
      (write-char #\" stream))))
		
;;;;;;;;;;;;;;;;;;;;

; STRING-LIST creates a list of words by a string of chars.
; string   -> name of the string to transform in list.
; sepchar  -> the separation character.
; flag     -> T= transforms multiple separators into one single, NIL= no.
; ritorna  -> the list of words that they compose the string.

(defun skip-char (string pos sepchar max)
    (loop
        (if (>= (setq pos (1+ pos)) max)
            (return (1- pos))
            (if (char/= (char string pos) sepchar)
                (return (1- pos))))))


(defun string-list (string sepchar &optional (flag t))
    (let* ((listw (list ""))
           (lens 0) (oldp -1) (i 0))
        (when flag
            (setf string (string-trim (string sepchar) string)))
        (if (eq (setf lens (length string)) 0)
            listw
            (progn
              (setf string
                    (concatenate 'string string (string sepchar)))
              (loop
                (when (char= (char string i) sepchar)
                    (nconc listw (list (subseq string (1+ oldp) i)))
                    (when flag (setq i (skip-char string i sepchar lens)))
                    (setq oldp i))
                (if (> (setq i (1+ i)) lens)
                    (return (rest listw))))))))
					
;;;;;;;;;;;;;;;;;;;;

(server)






