(in-package "USER")

(defun readsupsens ()
  (let (name listsec listthird entrylist sortedlist sortedthird sortedsec)
   (with-open-file 
     (iport "/home/lesmo/TULE/DOWNL/2010-FEB-TULE/SYSTEM/ITALIAN/KB-ITA/SEMANT-KB-ITA/my-test-sem.lisp"
            :direction :input :if-does-not-exist :error)
       (do ((linecount 1 (1+ linecount))
            (currline (read iport nil 'eof) (read iport nil 'eof)))
           ((eq currline 'eof) 'ok)
          (setq name (implode (explode (first currline))))
          (cond ((eq (rem linecount 1000) 0)
                   (format t "line number ~a: ~a~%" linecount currline)))
          (setq entrylist (cons name entrylist))
          (cond ((not (member (second currline) listsec :test #'equal))
                   (setq listsec (add-count-list (second currline) listsec))))
          (cond ((not (member (third currline) listthird :test #'equal))
                   (setq listthird (add-count-list (third currline) listthird))))
          (add-data name (rest currline))))
   (format t "---------- end reading data -----------")
   (setq sortedlist (sort entrylist #'alphalessp))
   (setq sortedlist (my-elimdup sortedlist))
   (setq sortedsec 
       (sort listsec #'(lambda (x y) (alphalessp (first x) (first y)))))
   (setq listthird (mapcar #'(lambda (x) (list (implode (explode (first x))) (second x)))
                                 listthird))
   (setq sortedthird 
       (sort listthird #'(lambda (x y) (alphalessp (first x) (first y)))))
   (with-open-file 
      (oport "/home/lesmo/TULE/DOWNL/2010-FEB-TULE/SYSTEM/ITALIAN/KB-ITA/SEMANT-KB-ITA/SUPERSENSE/super-categs.lisp"
             :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (do ((currcat (first sortedsec) (first sortedsec))
           (sortedsec (rest sortedsec) (rest sortedsec)))
          ((null currcat))
          (format oport "~a~%" currcat)))
   (with-open-file 
      (oport "/home/lesmo/TULE/DOWNL/2010-FEB-TULE/SYSTEM/ITALIAN/KB-ITA/SEMANT-KB-ITA/SUPERSENSE/super-sem.lisp"
             :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (do ((currsem (first sortedthird) (first sortedthird))
           (sortedthird (rest sortedthird) (rest sortedthird)))
          ((null currsem))
          (format oport "~a~%" currsem)))
   (with-open-file 
      (oport "/home/lesmo/TULE/DOWNL/2010-FEB-TULE/SYSTEM/ITALIAN/KB-ITA/SEMANT-KB-ITA/SUPERSENSE/super-semleo.lisp"
             :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (do ((curritem (first sortedlist) (first sortedlist))
           (sortedlist (rest sortedlist) (rest sortedlist)))
          ((null curritem))
          (write-item-data oport curritem)))))
   
; ***************************************************************************************************
(defun my-elimdup (li)
  (cond ((null li) nil)
        ((equal (first li) (second li))
           (my-elimdup (rest li)))
        (t (cons (first li) (my-elimdup (rest li))))))

; ***************************************************************************************************
(defun add-count-list (item countlist)
  (cond ((null countlist)
          (list (list item 1)))
        ((equal (first (first countlist)) item)
          (cons (list item (1+ (second (first countlist)))) (rest countlist)))
        (t (cons (first countlist) (add-count-list item (rest countlist))))))

; ***************************************************************************************************
(defun add-data (name data)
; *** name is an atom
; *** data is a list of three items:
;     - an extended syntactic category (es. NOUN.LOCATION)
;     - a semantic class (supersense?)
;     - a (possibly empty) list of semantic links, e.g. (HAS-PART ...)
; *** the internal representation is:
;     name--categs--> cat1 --senses--> sense11 (sem11 rels11)
;                                      sense12 (sem12 rels12)
;                     cat2 --senses--> sense21 (sem11 rels21)
;                                      sense22 (sem12 rels22)
;      .....
   (let ((prevdata (get name 'categs)) categdata found)
      (do ((nxtcat (first prevdata) (first prevdata))
           (prevdata (rest prevdata) (rest prevdata)))
          ((or (null nxtcat) found)
            (cond ((not found)
                     (putprop name (list (rest data)) (first data))
                     (putprop name (cons (first data) (get name 'categs)) 'categs))))
                     ; *** data is (cat sem rels)
                     ;     if no data for that categ, initialize as:
                     ;     atom--cat-->((sem rels))
          (cond ((equal nxtcat (first data))
              ; *** if nxtcat = first data, then this category was already present
              ;     for that name, so expand the associated data
                   (setq found t)
                   (setq categdata (get name nxtcat))
                   (setq categdata (expand-categ-data categdata (rest data)))
                   (putprop name categdata nxtcat))))))

; ***************************************************************************************************
; *** this adds the infos for a new semantic interpretation
(defun expand-categ-data (olddata newdata)
  (let (newnxtsem found (newsem (first newdata)) (restsem (second newdata)) mergeddata)
      (do* ((alldata nil (cons nxtsem alldata))
            (nxtsem (first olddata) (first olddata))
            (olddata (rest olddata) (rest olddata)))
          ((or (null nxtsem) found)
            (cond ((not found)
		     (reverse (cons newdata alldata)))
                  ((null nxtsem)
                     (reverse (cons newnxtsem (rest alldata))))
                  (t (append (append1 (reverse (cons newnxtsem (rest alldata))) nxtsem) olddata))))
          (cond ((equal (first nxtsem) newsem)
                   (setq found t)
                   (setq mergeddata (my-merge-data (second nxtsem) restsem))
                   (setq newnxtsem (list newsem mergeddata)))))))

; ***************************************************************************************************
(defun my-merge-data (olddata newdata)
  (cond ((null newdata) olddata)
        ((member (first newdata) olddata :test #'equal)
          (my-merge-data olddata (rest newdata)))
        (t (my-merge-data (cons (first newdata) olddata)  (rest newdata)))))

; ***************************************************************************************************
(defun write-item-data (oport curritem)
   (let ((itemdata (get curritem 'categs)))
       (format oport "~a~%" curritem)
       (dolist (nxtcat itemdata)
           (format oport "     ~a~%" nxtcat)
           (dolist (nxtsem (get curritem nxtcat))
               (format oport "          ~a~%" (first nxtsem))
               (dolist (seminfo (rest nxtsem))
                   (format oport "               ~a~%" seminfo))))))

