(in-package "USER")

; ********************************************************
; *** this converts an ontological query into a FOL formula
(defun sem-to-atlas-translation (inputquery &optional trace?)
 (declare (special *ANAPHORIC-CONTEXT*))
 (let (info-part par-result
  ; *** the next because inputquery has a double level of parentheses
  ;     (ambiguity? multiple sentences?)
       (semquery (first inputquery)))
  (cond ((eq (first semquery) 'about)
  ; ** this is the result of give-info
  ; ** the general form is "ABOUT c1 WHERE path", where c1 is currently restricted
  ;    to ££dialogue
          (setq info-part (generate-fol (fourth semquery) nil nil trace?)))
        ((eq (first semquery) 'select)
  ; ** this comes from "Vediamo le previsioni" (let's see the forecast), but the
  ;    generation of the top operato rin the translation need be revised
          (setq info-part (generate-fol (sixth semquery) nil nil trace?)))
        (t (exception 'parameter-error "PROC/buildatlas: Unknown expression" 
                     (first semquery))))
  (setq par-result (list (elimdup info-part)))
  par-result))

; ********************************************************
;     (el1 el2 ... elN) 
;       or
;     (el1 (and ((el11 el12 ... el1N1) (el21 el22 ... el22N2) ...)))
;     Here, the object to translate is normalized into:
;     ((el1 el2 ... elN)) or into:
;     ((el1 el11 el12 ... el1N1) (el1 el21 el22 ... el22N2) ...)
(defun generate-fol (path &optional prevresult frame-context trace?)
  (cond (trace?
          (format t "Generate-atlas-sem; Path = ~a~%     Frame context = ~a~%" path frame-context)
          (break "")))
  (let ((sem-result prevresult) conc-interp currframe output-done?
        incompatible-conjuncts new-sem-result saved-conc-interp compatib-frame)
   ; *** in case of subpaths (e.g. conjuncts of the AND operator), the frame
   ;     is provided from outside
   ; *** the next cond to start the search from the ££dialogue-topic
      (cond ((eq (first path) '££dialogue)
               (setq path (flush-generate-fol path '&has-dial-topic))))
     (do* ((previtem nil curritem)
           (curritem (first path) (first path))
           (path (rest path) (rest path)))
        ((null curritem) 
           (cond ((not (null saved-conc-interp))
                    (multiple-value-setq (sem-result currframe)
                           (eval-frame-output saved-conc-interp sem-result currframe trace?))))
           (reverse sem-result))
  (cond ((and trace? (not (is-a-structural-item curritem)))
          (format t "Generate-atlas-sem; curritem = ~a~%    saved-conc-interp: ~a~%" 
               curritem saved-conc-interp)
          (break "")))
        (cond ((and (atom curritem) (eq curritem 'has-subclass)) 
                 (setq output-done? nil))
      ; *** we are moving down along a subclass path; maybe, we will find a more specific concept
      ;     having an Atlas interpretation
              ((and (atom curritem) 
                    (not (is-a-structural-item curritem)))
      ; *** this is an ontology node (concept or relation); if it has an ATlas correspondence, 
      ;     save it. It is not used now, since it may happen that in the following path there
      ;     is a more specific concept that also has an Atlas interpretation (and in this case
      ;     the more specific one has to be preferred)
                 (setq conc-interp (leggi *OA-MAPPING* curritem))
                 (cond ((and (not (null conc-interp))
                             (not output-done?))
                          (cond ((null saved-conc-interp)
                                   (setq saved-conc-interp conc-interp))
                                (t (setq compatib-frame 
                                        (same-frame-item saved-conc-interp conc-interp))
                                   (cond ((eq compatib-frame 'replace)
                                            (setq saved-conc-interp conc-interp))
                                         ((eq compatib-frame 'add)
                                            (multiple-value-setq (sem-result currframe)
                                                (eval-frame-output 
                                                    saved-conc-interp sem-result currframe trace?))
                                            (setq saved-conc-interp conc-interp))
                                         ((eq compatib-frame 'substitute)
                                            (setq saved-conc-interp 
                                                 (list (first saved-conc-interp) conc-interp)))))))))
      ; ***  in all other cases, a subclass path has been completed (or it did not exist),
      ;      so the Atlas interpretation (if any) must by output
              (t (cond ((not (null saved-conc-interp))
         ;  (format t "    curritem (atom) = ~a~%" curritem)
         ;  (format t "    sem-result = ~a~%" sem-result)
         ;  (break "")
                         (multiple-value-setq (sem-result currframe)
                              (eval-frame-output saved-conc-interp sem-result currframe trace?))
                         (setq output-done? t)
      ; *** output-done? specifies that an output has been issued. This is used to avoid that
      ;     moving up along a subclass hierarchy, another output is produced for a more general
      ;     concept
                         (setq saved-conc-interp nil)))
     ; *** after the output, go on with the inspection of the current element of the path
                (cond ((atom curritem)
                        (cond ((eq curritem 'subclass-of) nil)
                              ((member curritem '(domain domain-of range range-of argument arg-of
                                                  value value-of))
                                 (setq output-done? nil))
                              (t (exception 'generic-error "Atom in a path of unknown type" curritem))))
                      ((eq (first curritem) 'temp-eq)
          ; *** currently, "temp-eq" is used in intensification, as in "molto mosso".
          ;     the interpretation of "mosso" (i.e. £mare-agitato) is included, but should
          ;     not be used, since it is superseded by "molto mosso" (£mare-molto-agitayo)
                         nil)
                      ((eq (first curritem) 'eq)
          ; *** the first item of the path is (eq instance)
          ;     "instance" must be associated with an attribute value, compatible with the
          ;     previous part of the graph
        ;  (format t "    curritem (not atom) = ~a~%" curritem)
        ;  (format t "    sem-result = ~a~%" sem-result)
        ;  (break "")
                        (setq conc-interp (leggi *OA-MAPPING* (second curritem)))
                        (cond ((null conc-interp) nil)
                          ; *** this may happen when a constant has been used as a selector for a
                          ;     sub-concept (e.g. £south for £southern-Italy)
                              ((atom (first (first conc-interp))) nil)
                          ; *** in this case, the interpretation is "simple", e.g. given as an
                          ;     attribute value or as a pair <attribute name, attribute value>
                              (t (setq conc-interp
                                      (choose-instance-interpr (first conc-interp) previtem))))
                          ; *** in this case, the interpretation is "ambiguous", in the sense
                          ;     that it depends on the previous ontology concept (previtem)
                          ;     The format is ((concept1 ...) (concept2 ...) ...)
                        (cond ((null conc-interp) nil)
                              ((null (second conc-interp))
                                (setq conc-interp (first conc-interp))
                                (cond ((eq (first conc-interp) 'attribute-value)
                                         (setq sem-result 
                                            (add-value sem-result (second conc-interp) trace?)))
                                      ((eq (first conc-interp) 'attr-value-modifier-val)
                                         (setq sem-result 
                                            (add-modifier-val sem-result 
                                                   (second conc-interp) trace?)))
                                      ((eq (first conc-interp) 'procedural)
                        ; *** in this case the value of "second conc-interp" is obtained by applying
                        ;     the function in "third conc-interp" to the input item, i.e. VAL in
                        ;     (eq VAL)
                                         (cond ((eq (second conc-interp) 'attribute-value)
                                                  (setq sem-result 
                                                     (add-value sem-result
                                                          (apply (third conc-interp)
								(list (second curritem) sem-result))
                                                          trace?)))
                                               (t (exception 'sem-conversion-error 
                                                        "Problems in procedural interpretation"
                                                        conc-interp))))
                                      (t (exception 'sem-conversion-error 
                                            "Problems 1 in EQ specification" conc-interp))))
                              (t (cond ((and (eq (first (first conc-interp)) 'attribute)
                                             (eq (first (second conc-interp)) 'attribute-value))
                                         (setq sem-result 
                                            (add-attribute sem-result (second (first conc-interp)) trace?))
                                         (setq sem-result 
                                            (add-value sem-result (second (second conc-interp)) trace?)))
                                       (t (exception 'sem-conversion-error 
                                            "Problems 2 in EQ specification" conc-interp))))))
                     ((eq (first curritem) 'and)
          ; *** the remaining part of the path has the form
          ;     (and ((item11 item12 ...)
          ;           (item21 item22 ...)
          ;           .... ))
     ;   (format t "Analysis of and ~a ~%" curritem)
     ;   (break "")
                        (setq incompatible-conjuncts (sem-incompat-conjunc (second curritem)))
     ;   (format t "Incompatible conjuncts ~a ~%" incompatible-conjuncts)
     ;   (break "")
                        (dolist (currpath (first incompatible-conjuncts))
                              (setq sem-result 
                                  (reverse
                                    (generate-atlas-sem currpath sem-result currframe trace?))))
             ; *** if there is more than one incompatibility list, for each of them create a new
             ;     frame and add it to the representation
                        (dolist (nextconjlist (rest incompatible-conjuncts))
                           (setq new-sem-result (list (list 'frame currframe)))
                           (dolist (currpath nextconjlist)
                              (setq new-sem-result 
                                  (reverse
                                    (generate-atlas-sem currpath new-sem-result currframe trace?))))
                           (setq sem-result
                               (append new-sem-result sem-result))))
         ; *** it is not an atom, but its first item is neither eq nor and
                     (t (exception 'sem-conversion-error 
                              "Unknown item format in a conceptual path" curritem))))))))

; ********************************************************
; *** for "ambiguous" instance interpretations: it chooses the right one
;     The format is ((concept1 ...) (concept2 ...) ...)
;     where "concept" is the name of the direct class of the instance
(defun choose-instance-interpr (conc-interp previtem)
  (cond ((null conc-interp) nil)
        ((eq (first (first conc-interp)) previtem)
           (rest (first conc-interp)))
        (t (choose-instance-interpr (rest conc-interp) previtem))))

; ********************************************************
; **** the possible outputs are:
;      --> replace: the previos interp must be ignored, and the new one takes its place
;      --> add: the previous interp is output, and the new one saved
;      --> substitute: the second item of previous interp is replaced by the new interp
(defun same-frame-item (prevconc newconc)
  (cond ((and (eq (length prevconc) 1)
              (eq (length newconc) 1))
   ; *** in this case, the new frame item must be a specification of the old one,
   ;     or the first one specifies only the attribute name and the new one a value
          (cond ((and (eq (first (first prevconc)) 'attribute-value)
                      (eq (first (first newconc)) 'attribute-value)) 'replace)
                ((and (eq (first (first prevconc)) 'attribute)
                      (eq (first (first newconc)) 'attribute-value)) 'add)
                (t (exception 'sem-conversion-error "In same-frame-item 1"))))
   ; *** in this case, the old frame item must include both the attribute name and
   ;     the attribute name and the attribute value, while the second one specifies
   ;     a new value
        ((and (eq (length prevconc) 2)
              (eq (length newconc) 1))
          (cond ((and (eq (first (first newconc)) 'attribute-value)
                      (attr-value-compatible (second (first prevconc)) (second newconc)))
                    'substitute)
                ((and (eq (first (first prevconc)) 'attribute)
                      (eq (first (second prevconc)) 'attribute-value)
                      (eq (first (first newconc)) 'attribute))
       ; *** the previous description is a complete attribute specification
                    'add)
                (t (exception 'sem-conversion-error "In same-frame-item 2"))))
   ; *** in this case, the newconc must include a first item that refers to the same
   ;     attribute as the one in prevconc
        ((and (eq (length prevconc) 1)
              (eq (length newconc) 2))
          (cond ((eq (first (first prevconc)) 'frame) 'add)
                ((eq (second (first newconc)) (second (first prevconc))) 'replace)
                (t (exception 'sem-conversion-error "In same-frame-item 3"))))
   ; *** finally, if both are of length 2, they either refer to the same attribute, and the
   ;     old one, being more generic, is thrown away, or are two separate specifications
        ((and (eq (length prevconc) 2)
              (eq (length newconc) 2))
          (cond ((eq (second (first newconc)) (second (first prevconc))) 'replace)
                (t 'add)))
        (t (exception 'sem-conversion-error "In same-frame-item 5"))))
 
           

; ********************************************************
; *** this expands the frame representation on the basis of the interpretation
;     of a concept found in *OA-MAPPING*
(defun eval-frame-output (conc-interp prevsem currframe trace?)
  (let ((atlas-value (first conc-interp)) (newframe currframe) semresult tempresult)
   (setq semresult
    (case (first atlas-value)
         (frame        ; *** the frame whose attributes need be filled
              (setq newframe (second atlas-value))
              (cons atlas-value prevsem))
         (attribute    ; *** an attribute is added to the structure
                       ;     after verifying the compatibility with
                       ;     the current frame
              (setq tempresult
                    (add-attribute prevsem (second atlas-value) trace?))
                       ; *** in some cases, an item is associated both
                       ;     with the attribute name and with the
                       ;     attribute value
              (cond ((null (second conc-interp)) tempresult)
                    ((eq (first (second conc-interp)) 'attribute-value)
                      (add-value tempresult (second (second conc-interp)) trace?))
                    (t (exception 'sem-conversion-error 
                           "Unknown second item in sem-to-atlas"))))
         (attribute-value
              (add-value prevsem (second atlas-value) trace?))
         (attr-value-modifier
              (add-value-mod prevsem (second atlas-value) trace?))
         (attr-value-modifier-val
              (add-modifier-val prevsem (second atlas-value) trace?))
         (otherwise (exception 
                  'sem-conversion-error 
                  "Unknown parameter in concept definition"
                  atlas-value))))
     (values semresult newframe)))

; ********************************************************
; *** this splits the conjuncts into compatible sublists.
;     The incompatibility is revealed by the presence of a semantic concept (e.g. a location
;     or a time), for which incompatible values are given.
;     The single valued concepts are currently assumed to be places and times, but this
;     deserves further study
(defun sem-incompat-conjunc (conjuncts)
   (let ((incomp-sublists (list (list (first conjuncts)))) found)
  ; *** the list of incompatible sublist is initialized with a list including th
  ;     first conjunct; then all conjuncts are inspected by the next do; each of
  ;     them either is included in an existing list, or produces the creation of
  ;     a new sublist
     (do ((nxtconj (first (rest conjuncts)) (first conjuncts))
          (conjuncts (rest (rest conjuncts)) (rest conjuncts)))
         ((null nxtconj) 
    ;  (format t "exiting; incomp-sublists: ~a~%" incomp-sublists) 
    ;  (break "") 
              incomp-sublists)
  ; *** the next do loops on all sublists, trying to find one with which the current
  ;     conjunct is compatible; if it finds one, the conjunct is included in it,
  ;     otherwise a new sublists is created
    ;  (format t "entering inner loop; incomp-sublists: ~a~% nxtconj: ~a ~%" 
    ;             incomp-sublists nxtconj) 
    ;  (break "") 
         (setq found nil)
         (do ((prevlists nil (append1 prevlists nxtlist))
              (nxtlist (first incomp-sublists) (first remlists))
              (remlists (rest incomp-sublists) (rest remlists)))
             ((or found (null nxtlist))
               (cond (found
                       (setq incomp-sublists
                            (append prevlists
                                    (cond ((null nxtlist) nil)
                                          (t (list nxtlist)))
                                    remlists)))
                     (t (setq incomp-sublists
                            (append1 prevlists (list nxtconj)))
    ;  (format t "exiting inner loop; incomp-sublists: ~a~%" incomp-sublists) 
    ;  (break "") 
                    )))
             (cond ((is-sem-compatible nxtlist nxtconj)
                      (setq found t)
                      (setq nxtlist (append1 nxtlist nxtconj))))))))
     
; ********************************************************
(defun is-sem-compatible (conjlist newconj)
   (cond ((null conjlist) t)
         ((sem-incompatible-paths (first conjlist) newconj) nil)
         (t (is-sem-compatible (rest conjlist) newconj))))

; ********************************************************
(defun sem-incompatible-paths (path1 path2)
   (let ((loc1 (get-sem-loc-time-spec path1 'loc))
         (time1 (get-sem-loc-time-spec path1 'time)))
  ; *** if path1 does not include either a location or time specification
  ;     there can be no incompatibility
   (cond ((and (null loc1) (null time1)) nil)
         ((null time1)
            (sem-incompatible-parts loc1 (get-sem-loc-time-spec path2 'loc)))
         ((null loc1)
            (sem-incompatible-parts time1 (get-sem-loc-time-spec path2 'time)))
         (t (or (sem-incompatible-parts loc1 (get-sem-loc-time-spec path2 'loc))
                (sem-incompatible-parts time1 (get-sem-loc-time-spec path2 'time)))))))
            
; ********************************************************
; *** a path includes a location specification if it includes the ££geographic-area concept
; *** a path includes a time specification if it includes the ££time-interval concept
;     if the concept is found, the remaining path must include a location (or time) value
; *** attribute can be either 'loc or 'time
(defun get-sem-loc-time-spec (path attribute)
  (let (conjuncts found)
    ;(break "Enter get-sem-loc-time-spec")
    (cond ((null path) nil)
          ((listp (first path))
             (cond ((eq (first (first path)) 'eq)
                      (get-sem-loc-time-spec (rest path) attribute)) 
                   ((eq (first (first path)) 'temp-eq)
                      (get-sem-loc-time-spec (rest path) attribute)) 
                   ((eq (first (first path)) 'and)
                      (setq conjuncts (second (first path)))
     ; *** if it is an and, look for a location or time spec in any conjunct
                      (do ((firstconj (first conjuncts) (first conjuncts))
                           (conjuncts (rest conjuncts) (rest conjuncts)))
                          ((or found (null firstconj)) found)
                          (setq found (get-sem-loc-time-spec firstconj attribute))))
                   (t (exception 'sem-conversion-error "Unknown non-atomic structure in a path" path))))
     ; *** this path concerns a location: look for the associated value
         ((or (and (eq attribute 'loc) (eq (first path) '££geographic-area))
              (and (eq attribute 'time) (eq (first path) '££time-interval)))
            (get-sem-loc-time-value (rest path)))
         (t (get-sem-loc-time-spec (rest path) attribute)))))

; ********************************************************
; *** here, I assume that in a path referring to a time or a location, the last item
;     identifies the involved constant
(defun get-sem-loc-time-value (path)
   (let ((lastitem (ult path)))
      (cond ((atom lastitem) lastitem)
            ((eq (first lastitem) 'eq)
               (second lastitem))
            (t (exception 'sem-conversion-error
                          "The last item of a path is not a value for a loc or time" path)))))

; ********************************************************
; *** the semantic representation in semrepr is reversed to facilitate processing
;     - semrepr is the current semantic representation
;       It is assumed that its format is:
;       ((FRAME ...)
;        (FRAME framename))
;     - attrname is an attribute name (e.g. TOPIC)
(defun add-attribute (semrepr attrname &optional trace?)
  (cond (trace? 
           (format t "Add-attribute; semrepr: ~a; attrname: ~a~%" semrepr attrname)
           (break "")))
  (let ((last-frame (compact-frame-attrs (first semrepr))))
      (cond ((eq (first last-frame) 'FRAME)
               (cond ((frame-compatible (second last-frame) attrname)
         ; *** (butlast semrepr) are all other possible previous frames
                       (cons (append1-merge last-frame (list attrname))
                             (rest semrepr)))
                     (t (exception 'sem-conversion-error "Attribute name not compatible with current frame"
                                  (list (second last-frame) attrname)))))
            (t (exception 'sem-conversion-error "The last item of the representation is not a frame")))))

; ********************************************************
; *** if an attribute missing the value already appears in the frame, remove it
(defun compact-frame-attrs (frame)
   (cond ((eq 2 (length frame)) frame)
         (t (let ((last-attr (ult frame)))
              (cond ((null (second last-attr))
                      (cond ((not (null (get-attribute (butlast frame) (first last-attr))))
                               (butlast frame))
                            (t frame)))
                    (t frame))))))

; ********************************************************
; *** "frame" is a standard frame in the form
;     (FRAME SITUATION-DESCRIPTION (STATUS INSTABILITÀ (DEGREE MODERATO)) (LOCATION SOUTH-REGIONS))
(defun get-attribute (frame attrname)
   (assoc attrname (rest (rest frame))))

; ********************************************************
(defun sem-incompatible-parts (item1 item2)
  (and (not (null item1))
       (not (null item2))
       (not (sem-part-of item1 item2))
       (not (sem-part-of item2 item1))))

; ********************************************************
; *** although this is highly questionable, I assume that in case of time and location
;     a subclass refers to a part of an upper class.
; *** For instance, ££southern-it-region is a subclass of ££it-region, and the area identified
;     by the former is a subarea of the one identified by the latter
; *** analogously for £Sicily and ££southern-it-region, or for £sunday and ££weekday
(defun sem-part-of (item1 item2)
   (or (and (is-a-class item1)
            (is-a-class item2)
            (is-subclass-of item1 item2))
       (and (is-an-instance item1)
            (is-a-class item2)
            (is-instance-of item1 item2))
       (and (is-an-instance item1)
            (is-an-instance item2)
            (is-part-of item1 item2))))

; ********************************************************
; *** this function has to be defined
(defun is-part-of (inst1 inst2)
   nil)

; ********************************************************
; *** this includes an attribute in a frame representation
;     In some cases it may happen that a previous ontology concept
;     already caused the introduction into the frame representation of the
;     involved attribute, so that no extension is needed
(defun append1-merge (frame attribute)
   (let ((lastattr (ult frame)))
      (cond ((atom lastattr)
              (append1 frame attribute))
            ((eq 1 (length lastattr))
              (cond ((equal lastattr attribute)
     ; *** the involved attribute has already been included previously
                      frame)
     ; *** actually, the next branch should cause an error, since the current frame
     ;     includes an attribute with non value (length = 1), but a new attribute
     ;     is being included
                    (t (append1 frame attribute))))
     ; *** in the next case, I assume that an attribute that already has a value is
     ;     a different one, or the the value is different from the one been included
     ;     now
            (t (append1 frame attribute)))))

; ********************************************************
(defun frame-compatible (frame attribute)
  (declare (special *ATLAS-FRAMES*))
  (member attribute (first (leggi *ATLAS-FRAMES* frame))))

; ********************************************************
; *** the semantic representation in semrepr is reversed to facilitate processing
;     - semrepr is the current semantic representation
;     - attrvalue is an attribute value (e.g. SEA)
; *** it is assumed that the current semrepr has the form
;     ((FRAME ...) 
;      (FRAME SITUATION-DESCRIPTION 
;             (STATUS ...) 
;             (EVENTS)))
;     and that attrname  (EVENTS) is compatible with attrvalue (e.g. PRECIPITAZIONI)
;     , so that the result be
;     ((FRAME ...) 
;      (FRAME SITUATION-DESCRIPTION 
;             (STATUS ...) 
;             (EVENTS PRECIPITAZIONI)))
(defun add-value (semrepr attrvalue &optional trace?)
  (cond (trace? 
           (format t "Add-value; semrepr: ~a; attrvalue: ~a~%" semrepr attrvalue)
           (break "")))
  (let ((last-frame (first semrepr)) last-attr new-attr-and-val)
      (cond ((eq (first last-frame) 'FRAME)
               (setq last-attr (ult last-frame))
               (setq new-attr-and-val (append1 last-attr attrvalue))
               (cond ((member new-attr-and-val (rest (rest last-frame)) :test #'equal)
      ; *** this branch corresponds to a situation where there are two passes on the
      ;     same concept. last-frame is (FRAME fr1 ... (attrX valX) ... (attrX))
      ;     and the new value is valX. In this case, the result is:
      ;     (FRAME fr1 ... (attrX valX) ... )
                       (cons (butlast last-frame) (rest semrepr)))
                     ((and (eq 1 (length last-attr))
                           (attr-value-compatible (first last-attr) attrvalue))
                       (cons (append1 (butlast last-frame)
                                  (list (first last-attr) attrvalue))
                             (rest semrepr)))
                    (t (exception 'sem-conversion-error "Attribute value not compatible with attribute name"
                                  (list (first last-attr) attrvalue)))))
            (t (exception 'sem-conversion-error "The last item of the representation is not a frame" last-frame)))))

; ********************************************************
(defun attr-value-compatible (attribute value)
  (declare (special *ATLAS-ATTRIBUTES*))
  (cond ((listp value)
          (cond ((eq (first value) 'not)
                   (setq value (second value))))))
  (member value (first (leggi *ATLAS-ATTRIBUTES* attribute))))

; ********************************************************
; *** adds to a represention an attribute value modifier (e.g. STRENGTH for PRECIPITAZIONI)
;     It is assumed that the current representation has the form:
;     ((FRAME ...) 
;      (FRAME SITUATION-DESCRIPTION 
;             (ATTRIBUTE ...) 
;             (ATTRIBUTE PRECIPITAZIONI)))
;      and the desired result is:
;     ((FRAME ...) 
;      (FRAME SITUATION-DESCRIPTION 
;             (ATTRIBUTE ...) 
;             (ATTRIBUTE PRECIPITAZIONI (STRENGTH))))
(defun add-value-mod (semrepr modname &optional trace?)
  (cond (trace? 
           (format t "Add-value-mod; semrepr: ~a; modname: ~a~%" semrepr modname)
           (break "")))
  (let ((last-frame (ult semrepr)) frame-attributes found)
      (cond ((eq (first last-frame) 'FRAME)
              (setq frame-attributes (reverse (rest (rest last-frame))))
   ; *** rest-rest excludes the keyword FRAME and the frame name. Reverse to start the
   ;     search from the more recent attributes
              (do ((prevattrs nil (append1 prevattrs attr))
                   (attr (first frame-attributes) (first frame-attributes))
                   (frame-attributes (rest frame-attributes) (rest frame-attributes)))
                  ((or (null attr) found)
                    (cond ((not found)
                             (exception 'sem-conversion-error "No attribute for an attribute modifier"
                                  (list (second attr) modname)))
                          (t (append1 (butlast semrepr) 
                               (cons 'frame 
                                   (cons (second last-frame)
   ; *** if the original attributes were A0 A1 A2 A3 A4 A5
   ;     and we found a compatibility between modname and A3
   ;     we now have: frame-attributes = A1 A0
   ;     attr: A2
   ;     prevattrs: A5 A4 A3
   ;     found: A3' (extended)
   ; *** the result is A0 A1 A2 A4 A5 A3'
                                         (append (reverse frame-attributes)
                                                 (cond ((null attr) nil)
                                                       (t (list attr)))
                                                 (reverse (butlast prevattrs))
                                                 (list found))))))))
                  (cond ((attr-modifier-compatible (second attr) modname)
       ; *** if the modifier is compatible with the attribute, then extend the attribute
                           (setq found
     ;                         (append1 (butlast semrepr) 
     ;                             (append1 (butlast prevattrs)
                                      (append1 attr (list modname)))))))
     ; ))
            (t (exception 'sem-conversion-error "The last item of the representation is not a frame")))))

; ********************************************************
(defun attr-modifier-compatible (attribute modifier)
  (declare (special *ATLAS-ATTRIBUTE-MODIFIERS*))
  (member modifier (mapcar #'first (first (leggi *ATLAS-ATTRIBUTE-MODIFIERS* attribute)))))

; ********************************************************
(defun add-modifier-val (semrepr modval &optional trace?)
;     ((FRAME ...) 
;      (FRAME SITUATION-DESCRIPTION 
;             (ATTRIBUTE1 ...) 
;             (EVENTS PRECIPITAZIONI (STRENGTH))))
;      and the desired result is:
;     ((FRAME ...) 
;      (FRAME SITUATION-DESCRIPTION 
;             (ATTRIBUTE1 ...) 
;             (EVENTS PRECIPITAZIONI (STRENGTH DEBOLE))))
  (cond (trace? 
           (format t "Add-modifier-val; semrepr: ~a; modval: ~a~%" semrepr modval)
           (break "")))
  (let ((last-frame (ult semrepr)) last-attr last-mod)
      (cond ((eq (first last-frame) 'FRAME)
              (setq last-attr (ult last-frame))
              (setq last-mod (ult last-attr))
              (cond ((atom last-mod)
                       (exception 'sem-conversion-error "Modifier value without modifier name" last-frame))
                    ((and (eq (length last-mod) 1)
                          (attr-mod-val-compatible (second last-attr) (first last-mod) modval))
                       (append1 (butlast semrepr)
                           (append1 (butlast last-frame)
                               (append1 (butlast last-attr)
                                   (list (first last-mod) modval)))))
                    (t (exception 'sem-conversion-error "Modifier value not compatible with modifier"
                                 (list (first last-mod) modval)))))
            (t (exception 'sem-conversion-error "The last item of the representation is not a frame")))))

; ********************************************************
(defun attr-mod-val-compatible (attribute-val modifier mod-val)
  (declare (special *ATLAS-ATTRIBUTE-MODIFIERS*))
  (let ((attr-mods (first (leggi *ATLAS-ATTRIBUTE-MODIFIERS* attribute-val))))
     (member mod-val (first (leggi attr-mods modifier)))))
     
; ********************************************************
(defun get-anaphoric-time (time-spec semrepr)
  (cond ((eq time-spec '--time-anaphoric-continuation)
              'today)
        (t (exception 'sem-conversion-error "in get-anaphoric-time" time-spec))))

; ********************************************************
; *** the assumption is that in the last frame of the current semrepr there is an "open"
;     (without value) attribute, and that in some previous frame the same attrinbute got
;     a value. The goal is to assign to the open attribute something as "NOT prev-value"
(defun get-other-items (other-spec semrepr)
  (cond ((eq other-spec '--deictic-specif-other)
          (let ((currframe (first semrepr)) lastattr found attrname)
             (setq lastattr (ult currframe))
             (cond ((null (second lastattr))
                     (setq attrname (first lastattr))
                     (do ((nxtframe (first (rest semrepr)) (first temprepr))
                          (temprepr (rest (rest semrepr)) (rest temprepr)))
                         ((or (null nxtframe) found) 
                           (cond (found (list 'not found))
                                 (t nil)))
                         (setq found (second (get-attribute nxtframe attrname)))))
                   (t semrepr))))
        (t (exception 'sem-conversion-error "in get-other-items" other-spec))))

