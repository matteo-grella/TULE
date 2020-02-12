
(in-package "USER")

; ***************************************************************
(defun insert-top-trace (data linkdata)
; *** this function handles different types of sequences where it is useful to
;     insert a top trace
; *** the first sequence is as "molto mosso lo Ionio", in case "mosso"
;     is attached to "Ionio" by inserting a trace and changing the links; the
;     operations are as follows:
;       1 Molto (MOLTO ADV QUANT) [2;ADVB+QUANTIF-RMOD]
;       2 mosso (MOSSO ADJ QUALIF M SING) [4;ADJC-RMOD]
;       3 lo (IL ART DEF M SING) [0;TOP-ART]
;       4 Ionio (IONIO NOUN PROPER M SING ££SEA) [3;DET+DEF-ARG]
;          ---->
;       1 Molto (MOLTO ADV QUANT) [2;ADVB+QUANTIF-RMOD]
;       2 mosso (MOSSO ADJ QUALIF M SING) [2.10;VERB-PREDCOMPL+SUBJ]
;       2.10 t [] (ESSERE VERB MAIN IND FUT INTRANS 3 SING) [0;TOP-VERB]
;       3 lo (IL ART DEF M SING) [2.10;VERB-SUBJ]
;       4 Ionio (IONIO NOUN PROPER M SING ££SEA) [3;DET+DEF-ARG]
; *** the second type of sequence is as "ancora qualche residua pioggia, ma"
;     This a bit more complex (this function is applied after the caseframe analysis
;     and after other attachment heuristics have been applied)
;       1 ancora (ANCORA ADV TIME) [9;RMOD]
;       2 qualche (QUALCHE ADJ INDEF ALLVAL SING) [9;VERB-PREDCOMPL+SUBJ]
;       3 residua (RESIDUO ADJ QUALIF F SING) [4;ADJC+QUALIF-RMOD]
;       4 pioggia (PIOGGIA NOUN COMMON F SING) [2;DET+QUANTIF-ARG]
;       5 , (#\, PUNCT) [9;SEPARATOR]
;       6 ma (MA CONJ COORD ADVERS) [9;RMOD]
;       7 la (IL ART DEF F SING) [9;VERB-SUBJ]
;       8 tendenza (TENDENZA NOUN COMMON F SING) [7;DET+DEF-ARG]
;       9 è (ESSERE VERB MAIN IND PRES INTRANS 3 SING) [0;TOP-VERB]
;       10 per (PER PREP MONO) [9;RMOD]
;          ---->
;       1 ancora (ANCORA ADV TIME) [1.10;ADVB-RMOD-TIME]		****
;       1.10 t [] (AVERE VERB MAIN IND FUT TRANS 1 PL) [0;TOP-VERB]	****
;       1.11 t [] (GENERIC-T PRON PERS ALLVAL PL 1) [1.10;VERB-SUBJ]	****
;       2 qualche (QUALCHE ADJ INDEF ALLVAL SING) [1.10;VERB-OBJ]	****
;       3 residua (RESIDUO ADJ QUALIF F SING) [4;ADJC+QUALIF-RMOD]
;       4 pioggia (PIOGGIA NOUN COMMON F SING) [2;DET+QUANTIF-ARG]
;       5 , (|,| PUNCT) [1.10;SEPARATOR]				****
;       6 ma (MA CONJ COORD ADVERS) [1.10;COORD+ADVERS]			****
;       7 la (IL ART DEF F SING) [9;VERB-SUBJ]
;       8 tendenza (TENDENZA NOUN COMMON F SING) [7;DET+DEF-ARG]
;       9 è (ESSERE VERB MAIN IND PRES INTRANS 3 SING) [6;COORD2ND+ADVERS]
;       10 per (PER PREP MONO) [9;RMOD]
;     The line with asterisks must be changed. The situation is identified by
;     an noun group ("qualche residua pioggia") plus zero or more adverbials
;     ("ancora") followed by a comma and by a CONJ ADVERS
; *** a third type of sequence is as "Domani tempo variabile ...". 
;      This is easy, since the change amounts in inserting a to-have+subj pair of traces
;      after the time adverb
;       1 Oggi (OGGI ADV TIME) [2;ADVB-RMOD-TIME]
;       2 ultima (ULTIMO ADJ ORDIN F SING £LAST) [3;ADJC+ORDIN-RMOD]
;       3 giornata (GIORNATA NOUN COMMON F SING) [0;TOP-NOUN]
;          ---->
;       1 Oggi (OGGI ADV TIME) [1.10;VERB-SUBJ]
;       1.10 t (AVERE VERB MAIN IND ALLVAL INTRANS 1 PL) [0;TOP-VERB]
;       1.11 t (GENERIC-T PRON PERS ALLVAL PL 1) [5.10;VERB-SUBJ]
;       2 ultima (ULTIMO ADJ ORDIN F SING £LAST) [3;ADJC+ORDIN-RMOD]
;       3 giornata (GIORNATA NOUN COMMON F SING) [1.10;VERB-PREDCOMPL+SUBJ]
;    N.B. Perhaps here the best trace is "to-be", but "to have" enables us to cover
;         many other cases
; *** the fourth type of sequence is as "Per la giornata di domani, tempo buono"
;     This, and similar, cases are identified by having the label of the noun
;     (tempo) UNKNOWN, and the noun preceded by some material such that:
;      a. There is a sentence initial preposition which is TOP-PREP
;      b. Between the prepositional group and the involved noun there
;         could be only adverbials and a separator
;       1 Al (A PREP MONO) [0;TOP-PREP] 			******
;       1.1 Al (IL ART DEF M SING) [1;PREP-ARG]
;       2 centro (CENTRO NOUN COMMON M SING) [1.1;DET+DEF-ARG]
;       3 e (E CONJ COORD COORD) [1;COORD+BASE]
;       4 al (A PREP MONO) [3;COORD2ND+BASE]
;       4.1 al (IL ART DEF M SING) [4;PREP-ARG]
;       5 sud (SUD NOUN COMMON M SING) [4.1;DET+DEF-ARG]
;       6 un (UN ART INDEF M SING) [3;UNKNOWN]			******
;       7 cielo (CIELO NOUN COMMON M SING) [6;DET+INDEF-ARG]
;       8 poco (POCO ADV QUANT) [9;ADVB+QUANTIF-RMOD]
;       9 nuvoloso (NUVOLOSO ADJ QUALIF M SING) [7;ADJC+QUALIF-RMOD]
;             -->
;       1 Al (A PREP MONO) [5.10;PREP-RMOD-LOC]
;       1.1 Al (IL ART DEF M SING) [1;PREP-ARG]
;       2 centro (CENTRO NOUN COMMON M SING) [1.1;DET+DEF-ARG]
;       3 e (E CONJ COORD COORD) [1;COORD+BASE]
;       4 al (A PREP MONO) [3;COORD2ND+BASE]
;       4.1 al (IL ART DEF M SING) [4;PREP-ARG]
;       5 sud (SUD NOUN COMMON M SING) [4.1;DET+DEF-ARG]
;       5.10 t (AVERE VERB MAIN IND ALLVAL INTRANS 1 PL) [0;TOP-VERB]
;       5.11 t (GENERIC-T PRON PERS ALLVAL PL 1) [5.10;VERB-SUBJ]
;       6 un (UN ART INDEF M SING) [3;VERB-OBJ]
;       7 cielo (CIELO NOUN COMMON M SING) [6;DET+INDEF-ARG]
;       8 poco (POCO ADV QUANT) [9;ADVB+QUANTIF-RMOD]
;       9 nuvoloso (NUVOLOSO ADJ QUALIF M SING) [7;ADJC+QUALIF-RMOD]
; *** the fifth type of sequence is as "ancora correnti settentrionali, molto 
;     mossi gli altri mari ..."
;     This is similar to the third type, and could also be merged as an extension of
;     the previous one, but here the condition includes the existence of a comma,
;     i.e. of a second conjunct
;       1 Ancora (ANCORA ADV TIME) [2;ADVB-RMOD]
;       2 correnti (CORRENTE NOUN COMMON F PL) [0;TOP-NOUN]
;       3 settentrionali (SETTENTRIONALE ADJ QUALIF ALLVAL PL) [2;ADJC+QUALIF-RMOD]
;       4 , (#\, PUNCT) [3;SEPARATOR]
;       5 molto (MOLTO ADV QUANT) [6;ADVB+QUANTIF-RMOD]
;       6 mosso (MOSSO ADJ QUALIF M SING) [8;ADJC-RMOD]
;       7 lo (IL ART DEF M SING) [6;UNKNOWN]
;       8 Ionio (IONIO NOUN PROPER M SING ££SEA) [7;DET+DEF-ARG]
;          ---->
;       1 ancora (ANCORA ADV TIME) [1.10;ADVB-RMOD-TIME]		****
;       1.10 t [] (AVERE VERB MAIN IND FUT TRANS 1 PL) [0;TOP-VERB]	****
;       1.11 t [] (GENERIC-T PRON PERS ALLVAL PL 1) [1.10;VERB-SUBJ]	****
;       2 correnti (CORRENTE NOUN COMMON F PL) [1.10;VERB-OBJ]		****
;       3 settentrionali (SETTENTRIONALE ADJ QUALIF ALLVAL PL) [2;ADJC+QUALIF-RMOD]
;       4 , (#\, PUNCT) [1.10;COORD]
;       5 molto (MOLTO ADV QUANT) [6;ADVB+QUANTIF-RMOD]
;       6 mosso (MOSSO ADJ QUALIF M SING) [6.10;PREDCOMPL-SUBJ]		****
;       6.10 t [] (ESSERE VERB MAIN IND FUT TRANS 3 SING) [4;COORD2ND]	****
;       7 lo (IL ART DEF M SING) [6.10;VERB-SUBJ]			****
;       8 Ionio (IONIO NOUN PROPER M SING ££SEA) [7;DET+DEF-ARG]
; *** a sixth type of sequence is as "Locali addensamenti potranno interessare 
;     il settore nord orientale, poi a partire dalla serata un aumento della 
;     nuvolosità su quello occidentale."
;     In this case, a trace must be inserted in the second conjunct. In other words,
:     "a partire dalla serata un aumento della nuvolosità su quello occidentale."
;     should be handled as an autonomous sentence. In the original parse hypothesis,
;     we have some links to the unique verb, i.e. "interessare":
;       ....
;       3 potranno (POTERE VERB MOD IND FUT INTRANS 3 PL) [0;TOP-VERB]
;       4 interessare (INTERESSARE VERB MAIN INFINITE PRES TRANS) [3;VERB+MODAL-INDCOMPL]
;       ....
;       9 , (#\, PUNCT) [4;SEPARATOR]
;       10 poi (POI ADV TIME) [4;RMOD]
;       11 a (A_PARTIRE_DA PREP POLI LOCUTION) [4;RMOD]
;       12 partire (A_PARTIRE_DA PREP POLI LOCUTION) [11;CONTIN+LOCUT]
;       13 dalla (A_PARTIRE_DA PREP POLI LOCUTION) [12;CONTIN+LOCUT]
;       13.1 dalla (IL ART DEF F SING) [11;PREP-ARG]
;       14 serata (SERATA NOUN COMMON F SING) [13.1;DET+DEF-ARG]
;       15 un (UN ART INDEF M SING) [4;RMOD]
;       16 aumento (AUMENTO NOUN COMMON M SING AUMENTARE TRANS) [15;DET+INDEF-ARG]
;       17 della (DI PREP MONO) [16;NOUN-OBJ]
;       17.1 della (IL ART DEF F SING) [17;PREP-ARG]
;       18 nuvolosità (NUVOLOSITÀ NOUN COMMON F ALLVAL) [17.1;DET+DEF-ARG]
;       19 su (SU PREP MONO) [4;RMOD]
;       20 quello (QUELLO PRON DEMONS M SING LSUBJ+LOBJ+OBL) [19;PREP-ARG]
;       21 occidentale (OCCIDENTALE ADJ QUALIF ALLVAL SING) [20;ADJC+QUALIF-RMOD]
;       22 . (#\. PUNCT) [3;END]
;          ---->
;       ....
;       3 potranno (POTERE VERB MOD IND FUT INTRANS 3 PL) [0;TOP-VERB]
;       4 interessare (INTERESSARE VERB MAIN INFINITE PRES TRANS) [3;VERB+MODAL-INDCOMPL]
;       ....
;       9 , (#\, PUNCT) [3;COORD]
;       9.10 t [] (AVERE VERB MAIN IND FUT TRANS 1 PL) [9;COORD2ND]	****
;       9.11 t [] (GENERIC-T PRON PERS ALLVAL PL 1) [9.10;VERB-SUBJ]	****
;       10 poi (POI ADV TIME) [9.10;ADVB-RMOD]				****
;       11 a (A_PARTIRE_DA PREP POLI LOCUTION) [9.10;RMOD]		****
;       12 partire (A_PARTIRE_DA PREP POLI LOCUTION) [11;CONTIN+LOCUT]
;       13 dalla (A_PARTIRE_DA PREP POLI LOCUTION) [12;CONTIN+LOCUT]
;       13.1 dalla (IL ART DEF F SING) [11;PREP-ARG]
;       14 serata (SERATA NOUN COMMON F SING) [13.1;DET+DEF-ARG]
;       15 un (UN ART INDEF M SING) [9.10;VERB-OBJ]			****
;       16 aumento (AUMENTO NOUN COMMON M SING AUMENTARE TRANS) [15;DET+INDEF-ARG]
;       17 della (DI PREP MONO) [16;NOUN-OBJ]
;       17.1 della (IL ART DEF F SING) [17;PREP-ARG]
;       18 nuvolosità (NUVOLOSITÀ NOUN COMMON F ALLVAL) [17.1;DET+DEF-ARG]
;       19 su (SU PREP MONO) [9.10;PREP-RMOD-LOC]			****
;       20 quello (QUELLO PRON DEMONS M SING LSUBJ+LOBJ+OBL) [19;PREP-ARG]
;       21 occidentale (OCCIDENTALE ADJ QUALIF ALLVAL SING) [20;ADJC+QUALIF-RMOD]
;       22 . (#\. PUNCT) [3;END]
   (declare (special *TREE-FORMAT*))
   (let (newdata newlinkdata data-and-link)
      (do ((nxtsent (first data) (first data))
           (data (rest data) (rest data))
           (nxtsentlinks (first linkdata) (first linkdata))
           (linkdata (rest linkdata) (rest linkdata)))
         ((null nxtsent) 
           (list (reverse newdata) (reverse newlinkdata)))
         (setq data-and-link (singsent-insert-top-trace nxtsent nxtsentlinks))
         (setq newdata (cons (first data-and-link) newdata))
         (setq newlinkdata (cons (second data-and-link) newlinkdata)))))

; ***************************************************************
(defun singsent-insert-top-trace (nxtsent nxtsentlinks &optional coordlinumb)
; *** this carries out the actual work on a single sentence
;     If coordlinumb is nil, this is the first part of a sentence,
;     but in cases such as "Ancora correnti settentrionali, molto mosso lo
;     ionio, ...", it calls itself recursively on "molto mosso lo Ionio, ..."
;     In this case, coordlinumb is the line number of the comma
 (declare (special *TREE-FORMAT*))
   (let (newlines newlinks tracelinumb adjline adjlink advline advlink
         traceline tracelink artline artlink pause? changed-top prevtops
         make-change? found-np fail toplinumb headline subjtrace traceindex
         found templinks comma-and-art commaline currlinumb subjlink doublepause?
         subjlinumb newtraceindex newdata-and-links templines nxtcateg nxt2categ
         found-deps ok-deps toplinelink acttoplinumb nxt3categ nxt2link nxt3link
         (prevline nil) (prevlink nil) nxt3line nxt2line subcase1 subcase2
         prep chunk-end advs newnewlines newnewlinks adv-oldlink newlineslinks
         (startlinumb (get-synt-numb (first nxtsent))))
      (do ((nxtline (first nxtsent) (first nxtsent))
           (nxtsent (rest nxtsent) (rest nxtsent))
           (nxtlink (first nxtsentlinks) (first nxtsentlinks))
           (nxtsentlinks (rest nxtsentlinks) (rest nxtsentlinks)))
          ((null nxtline) 
            (list (reverse newlines) (reverse newlinks)))
          (setq prevline (first newlines))
          (setq prevlink (first newlinks))
          (setq nxtcateg (get-synt-categ nxtline))
          (setq nxt2line (first nxtsent))
          (setq nxt2categ (get-synt-categ nxt2line))
          (setq nxt2link (first nxtsentlinks))
          (setq nxt3line (second nxtsent))
          (setq nxt3categ (get-synt-categ nxt3line))
          (setq nxt3link (second nxtsentlinks))
       ;  (format t "nxtline: ~a~% Nxtlink: ~a~%" nxtline nxtlink)
       ;  (format t "newlines: ~a~%" newlines)
       ;  (format t "newlinks: ~a~%" newlinks)
       ;  (break "Start insert top traces")
          (cond (doublepause? 
                   (setq doublepause? nil)
                   (setq pause? t))
                (pause? (setq pause? nil))
   ; ****** first case **********************************************************
                ((and (eq nxtcateg 'ADJ)
                      (or (and (eq nxt2categ 'ART)
                               (or (and (null coordlinumb)
                                        (eq (second nxt2link) 'TOP-ART))
                                   (and (not (null coordlinumb))
                                        (memq (second nxt2link) '(UNKNOWN COORD2ND)))))
                          (and (eq nxt2categ 'ADV)
                               (eq nxt3categ 'ART)
                               (or (and (null coordlinumb)
                                        (eq (second nxt3link) 'TOP-ART))
                                   (and (not (null coordlinumb))
                                        (memq (second nxt3link) '(UNKNOWN COORD2ND))))))
       ; *** in the first case below, the adj is linked to the noun after the art
       ; *** in the second one, it is linked to the something before the beginning of this
       ;     portion of sentence
                      (or (index-precedes (get-synt-numb nxt2line) (first nxtlink))
                          (index-precedes (first nxtlink) startlinumb)))
            ;  (format t "case 1; nxtline = ~a~%" nxtline)
            ;  (break "")
                  (setq tracelinumb (list (get-synt-linumb nxtline) 10))
                  (setq adjline nxtline)
                  (setq adjlink (make-link tracelinumb 'VERB-PREDCOMPL+SUBJ 'top-traces))
                  (setq traceline 
                       (cond ((eq *TREE-FORMAT* 'tut)
                                 (list tracelinumb '|t| 
                                     (list 'essere 'verb 'main 'ind 'allval 'intrans 3
                                           (get-synt-number adjline))))
                             (t		; *** it is 'avm
                                `((posit ,tracelinumb)
                                  (form #\t)
                                  (syn ((lemma essere) (cat verb) (type main)
                                        (mood ind) (tense allval) (trans intrans)
                                        (person 3) (number ,(get-synt-number adjline))))
                                  (sem nil)
                                  (coref empty)))))
                  (cond ((null coordlinumb)
                           (setq tracelink (make-link 0 'TOP-VERB 'top-traces))
                           (setq changed-top tracelinumb))
                        (t (setq tracelink (make-link coordlinumb 'COORD2ND 'top-traces))))
                  (cond ((eq nxt2categ 'ART)
                           (setq artline nxt2line)
                           (setq artlink (make-link tracelinumb 'VERB-SUBJ 'top-traces))
                           (setq newlines (append (list artline traceline adjline) newlines))
                           (setq newlinks (append (list artlink tracelink adjlink) newlinks))
                           (setq pause? t))
                        (t (setq advline nxt2line)
                           (setq artline nxt3line)
                           (setq advlink (make-link tracelinumb 'RMOD 'top-traces))
                           (setq artlink (make-link tracelinumb 'VERB-SUBJ 'top-traces))
                           (setq newlines 
                                 (append (list artline advline traceline adjline) newlines))
                           (setq newlinks 
                                 (append (list artlink advlink tracelink adjlink) newlinks))
                           (setq doublepause? t)))
         ; *** the same situation of this 'first case' could occur in the next part
       ;              (format t "Nxtsent: ~a~%" nxtsent)
       ;              (format t "Nxtsent: ~a~%" (nthcdr 10 nxtsent))
       ;              (format t "Nxtline: ~a~%" nxtline)
       ;    (break "nove")
                     (setq comma-and-art (get-next-comma-art nxtsent nxtsentlinks))
       ;              (format t "Nxtsent: ~a~%" nxtsent)
       ;              (format t "Nxtsent: ~a~%" (nthcdr 10 nxtsent))
       ;              (format t "Nxtline: ~a~%" nxtline)
       ;    (break "dieci")
         ; *** nxtline is the line of the adjective
         ; *** nxtsent begins with the line of the article
                     (cond ((not (null comma-and-art))
                             (setq commaline (first comma-and-art))
                             (do ((nxtl (first nxtsent) (first remsent))
                                  (remsent (rest nxtsent) (rest remsent))
                                  (nxtk (first nxtsentlinks) (first remsentlinks))
                                  (remsentlinks (rest nxtsentlinks) (rest remsentlinks)))
                                ((or (null nxtl)
                                     (equal nxtl commaline))
                                  (cond ((equal nxtl commaline)
                                          (cond ((null tracelinumb)
                                                   (break "No trace in insert top traces: case 1")))
                                          (setq newdata-and-links
                                             (singsent-insert-top-trace 
                                                remsent remsentlinks (get-synt-numb commaline)))
                                          (setq nxtsent
                                             (append 
                                ; *** the next is the previous part (including the coord)
                                                (reverse (cons nxtl templines))
                                ; *** the next is updated part after the coord
                                                (first newdata-and-links)))
                                          (setq nxtsentlinks 
                                             (append 
                                ; *** the next is the previous part (including the coord)
                                                (reverse 
                                                   (cons (make-link tracelinumb 'COORD 'top-traces) templinks))
                                ; *** the next is updated part after the coord
                                                (second newdata-and-links)))
                                        )
                                       (t (break "Insert-top-trace: comma line not found"))))
                                (setq templines (cons nxtl templines))
                                (setq templinks (cons nxtk templinks)))
                              (setq pause? t))
                           (t (setq pause? t))))
   ; ****** second case *********************************************************
                  ((and (null coordlinumb)	; *** this does not apply to fragments
                        (eq nxtcateg 'CONJ)
                        (eq (get-synt-type nxtline) 'COORD)
                        (eq (get-synt-semtype nxtline) 'ADVERS)
                        (eq (get-synt-word prevline) #\,))
             ; (format t "case 2; nxtline = ~a~%" nxtline)
             ; (break "")
                     (multiple-value-setq (prevtops toplinumb)
                                  (get-prev-toplines 
                                       (rest newlines) (rest newlinks) (get-synt-numb nxtline)))
          ; *** prevtops includes all the lines preceding the conj coord, that have link to a
          ;     word following the conj coord
                     (do ((nxttop (first prevtops) (first prevtops))
                          (prevtops (rest prevtops) (rest prevtops)))
                         ((null nxttop)
                            (cond ((and found-np (not fail))
                                    (setq make-change? t))))
                         (cond ((eq (get-synt-categ nxttop) 'adv) nil)
                               ((memq (get-synt-categ nxttop) '(adj art num noun))
                                  (cond ((null found-np)
                ; *** this is the first NP
                                           (setq found-np t))
                ; *** this is not the first NP: fail
                                        (t (setq fail t))))
                               (t (setq fail t))))
                ; *** if make-change? has been set to true, check if the prevtops are
                ;     linked to a verb or is the hypothesized top of the sentence
                     (cond (make-change?
                             (cond ((neq toplinumb 0)
                                      (setq headline (first (find-a-line `(position ,toplinumb)
                                                               nxtsent nxtsentlinks)))
                                      (cond ((neq (get-synt-categ headline) 'VERB)
                                               (setq make-change? nil)))))))
                ; *** if make-change? is still true, then modify the lines
                     (cond (make-change?
                            (setq traceline (make-to-have-trace))
                            (setq subjtrace (make-to-have-subj-trace))
                ; *** the two traces lack the line numbers, which must be established inside
                ;     modify-top-trace-links
                            (multiple-value-setq 
                                   (traceindex newlines newlinks)
                                   (modify-top-trace-links newlines newlinks prevtops 
                                         traceline subjtrace))
                ; *** now, all lines until the comma have been set; modify the links of the
                ;     comma and of the conj coord
                            (setq newlinks 
                                  (cons (make-link traceindex 'COORD+ADVERS 'top-traces)
                                        (cons (make-link traceindex 'SEPARATOR 'top-traces)
                                              (rest newlinks))))
                            (setq newlines (cons nxtline newlines))
                ; *** now, modify the previous top verb
                            (do ((nxtl (first nxtsent) (first remsent))
                                 (remsent (rest nxtsent) (rest remsent))
                                 (nxtk (first nxtsentlinks) (first remsentlinks))
                                 (remsentlinks (rest nxtsentlinks) (rest remsentlinks)))
                               ((or (null nxtl) found)
                                 (cond (found 
                                          (setq nxtsentlinks 
                                               (append (reverse templinks) 
                                                       (cons nxtk remsentlinks))))
                                       (t 
                 ; *** If no verb, insert another pair of traces
                 ;     traceline and subjtrace are the same as before
                 ; *** currlinumb is the line number of the conjunction
                                          (setq currlinumb (get-synt-numb nxtline)) 
                                          (setq newtraceindex (list currlinumb 10))
                                          (setq subjlinumb (list currlinumb 11))
                                          (setq tracelink (make-link currlinumb 'COORD2ND+ADVERS 'top-traces))
                                          (setq subjlink (make-link newtraceindex 'VERB-SUBJ 'top-traces))
                                          (setq newlines 
                                             (append (list (cons subjlinumb subjtrace)
                                                           (cons newtraceindex traceline))
                                                     newlines))
                                          (setq newlinks 
                                             (append (list subjlink tracelink)
                                                     newlinks))
                                          (setq nxtsentlinks 
                                                (modify-links-to-2nd-verb 
                                                     nxtsent nxtsentlinks newtraceindex)))))
                               (cond ((equal nxtl headline)
                                        (setq templinks 
                                             (cons (make-link (get-synt-numb nxtline) 
                                                              'COORD2ND+ADVERS 'top-traces)
                                                   templinks))
                                        (setq found t))
                                     (t (setq templinks (cons nxtk templinks)))))
                            (setq changed-top traceindex))
                  ; *** no change to make
                     (t (setq newlines (cons nxtline newlines))
                        (setq newlinks (cons nxtlink newlinks)))))
   ; ****** third case *********************************************************
;       1 Oggi (OGGI ADV TIME) [2;ADVB-RMOD-TIME]
;       2 ultima (ULTIMO ADJ ORDIN F SING £LAST) [3;ADJC+ORDIN-RMOD]
;       3 giornata (GIORNATA NOUN COMMON F SING) [0;TOP-NOUN]
;          ---->
;       1 Oggi (OGGI ADV TIME) [1.10;VERB-SUBJ]
;       1.10 t (AVERE VERB MAIN IND ALLVAL INTRANS 1 PL) [0;TOP-VERB]
;       1.11 t (GENERIC-T PRON PERS ALLVAL PL 1) [5.10;VERB-SUBJ]
;       2 ultima (ULTIMO ADJ ORDIN F SING £LAST) [3;ADJC+ORDIN-RMOD]
;       3 giornata (GIORNATA NOUN COMMON F SING) [1.10;VERB-PREDCOMPL+SUBJ]
                  ((and (null coordlinumb)	        ; *** this does not apply to fragments
                        (null (get-next-comma-art nxtsent nxtsentlinks))
                                                 ; to be in alternative with the fourth case
                        (or 
                           (and
                              (eq nxtcateg 'ADV)
                              (eq (get-synt-type nxtline) 'TIME)
                              (or (and (null newlines)		; *** we are the beginning
                                       (or (and (eq nxt2categ 'NOUN)	; domani tempo variabile
                                                (eq (first nxtlink) (get-synt-numb nxt2line))
                                                (eq (second nxt2link) 'TOP-NOUN))
                                           (and (eq nxt2categ 'ART)	; domani una giornata tranquilla
                                                (eq (second nxt2link) 'TOP-ART))
                                           (and (eq nxt2categ 'ADV)
                                                (eq (first nxtlink) (first nxt2link))
                                                (eq nxt3categ 'ART)	; domani ancora una giornata tranquilla
                                                (eq (second nxt3link) 'TOP-ART))
                                           (and (eq nxt2categ 'ADJ) 	; domani ultima giornata
                                                (eq nxt3categ 'NOUN)
                                                (eq (second nxt3link) 'TOP-NOUN))))))
                           (and
                              (eq nxtcateg 'NOUN)
                              (inh-member (get-synt-word nxtline) '£other-time-adv)
                              (or (and (null (rest newlines))
                                       (eq (get-synt-categ prevline) 'PREP)
                               ; *** Or we are on the second word and the first one was a prep
                               ;     "Per domani nubi sul Mediterraneo"
                               ;     prevline, prevlink: (1 PER ...) (0 TOP-PREP)
                               ;     nxtline, nxtlink: (2 DOMANI ...) (1 PREP-ARG)
                               ;     nxtl2ine, nxt2link: (3 NUBI ...) (1 UNKNOWN)
                                       (or (and (eq nxt2categ 'NOUN)	
                                                (eq (second prevlink) 'TOP-PREP)
                                                (eq (first nxtlink) (get-synt-numb (first newlines)))
                                                (eq (second nxt2link) 'UNKNOWN))
                               ; *** Or we are on the second word and the first one was a prep
                               ;     "Per domani quindi nubi sul Mediterraneo"
                               ;     prevline, prevlink: (1 PER ...) (0 TOP-PREP)
                               ;     nxtline, nxtlink: (2 DOMANI ...) (1 PREP-ARG)
                               ;     nxt2line, nxt2link: (3 QUINDI ...) (4 ADVB-RMOD)
                               ;     nxt3line, nxt3link: (4 NUBI ...) (1 UNKNOWN)
                                           (and (eq nxt2categ 'ADV)
                                                (eq nxt3categ 'NOUN)	
                                                (eq (second prevlink) 'TOP-PREP)
                                                (eq (first nxtlink) (get-synt-numb prevline))
                                                (eq (first nxt2link) (get-synt-numb nxt3line))
                                                (eq (second nxt3link) 'UNKNOWN))))))))
                     (setq currlinumb (get-synt-numb nxtline))
                     (setq traceindex (list currlinumb 10))
                     (setq subjlinumb (list currlinumb 11))
                     (setq traceline (add-synt-linumb traceindex (make-to-have-trace)))
                     (setq subjtrace (add-synt-linumb subjlinumb 
                                                 (make-to-have-subj-trace)))
           ; *** the two traces have been created
                     (cond ((null newlines) (setq subcase1 t))
                           (t (setq subcase2 t)))
           ; *** the two subcases (without or with an initial prep) has been set apart
                  ; *** First subcase: Domani giornata ...
                  ; *** newlines and newlinks contain
                  ;      (1.11 t [] ...)  (1.10 t [] to-have) (1 Domani ...) 
                  ;      (1.10 VERB-SUBJ) (0 TOP-VERB)        (1.10 ADVB-RMOD-TIME)
                     (cond (subcase1
                              (setq newlines (list subjtrace traceline nxtline))
                              (setq newlinks (list (make-link traceindex 'VERB-SUBJ 'top-traces)
                                                   (make-link 0 'TOP-VERB 'top-traces)
                                                   (make-link traceindex 'ADVB-RMOD-TIME 'top-traces))))
                  ; *** Second subcase: Per domani giornata ...
                  ; *** newlines and newlinks contain
                  ;      (2.11 t [] ...)  (2.10 t [] to-have) (2 domani ...) (1 Per ...) 
                  ;      (2.10 VERB-SUBJ) (0 TOP-VERB)        (1 PREP-ARG)   (2.10 PREP-RMOD-TIME)
                           (t (setq newlines (list subjtrace traceline nxtline prevline))
                              (setq newlinks (list (make-link traceindex 'VERB-SUBJ 'top-traces)
                                                   (make-link 0 'TOP-VERB 'top-traces)
                                                   nxtlink
                                                   (make-link traceindex 'PREP-RMOD-TIME 'top-traces)))))
            ;   (format t "case 3; nxtline = ~a~%       nxtlink = ~a~%" nxtline nxtlink)
            ;   (format t "        nxt2line = ~a~%       nxt2link = ~a~%" (first nxtsent) nxt2link)
            ;   (format t "        nxt3line = ~a~%       nxt3link = ~a~%" (second nxtsent) nxt3link)
            ;   (break "")
                     (cond ((or (and subcase1 (lab-subsumes 'TOP (second nxt2link)))
                                (and subcase2 (eq 'UNKNOWN (second nxt2link))))
                  ; *** if the sentence was "domani una giornata", or "Per domani una giornata"
                  ;     nxtsentlinks must contain 
                  ;        (x.10 VERB-OBJ) {for "una"} ... [where x is 1 or 2]
                              (setq nxtsentlinks 
                                   (cons (make-link traceindex 'VERB-OBJ 'top-traces)
                                         (rest nxtsentlinks))))
                           ((or (and subcase1 (lab-subsumes 'TOP (second nxt3link)))
                                (and subcase2 (eq 'UNKNOWN (second nxt3link))))
                  ; *** if the sentence was "[Per] domani ancora una giornata", nxtsentlinks contains
                  ;           (y ADV-RMOD-TIME) {for "ancora"} (x.10 VERB-OBJ) {for "una"} ...
                              (setq nxtsentlinks
                                  (cons nxt2link
                                      (cons (make-link traceindex 'VERB-OBJ 'top-traces)
                                            (rest (rest nxtsentlinks))))))
                           (t (break "Insert top traces: case three"))))
   ; ****** fourth case *********************************************************
                  ((and (null coordlinumb)	; *** this does not apply to fragments
                        (memq nxtcateg '(NOUN ART ADJ PRON))
                        (eq (second nxtlink) 'UNKNOWN))
                    (multiple-value-setq (prep chunk-end advs)
                         (PP-initial newlines newlinks))
                    (cond ((or (null prep)
                               (neq (second (ult newlinks)) 'TOP-PREP))
                            (setq newlines (cons nxtline newlines))
                            (setq newlinks (cons nxtlink newlinks)))
        ; *** all conditions have been met: insert the traces and modify the links
                          (t (setq traceindex (list chunk-end 10))
                             (setq subjlinumb (list chunk-end 11))
                             (setq traceline (add-synt-linumb traceindex (make-to-have-trace)))
                             (setq subjtrace (add-synt-linumb subjlinumb 
                                                         (make-to-have-subj-trace)))
                             (setq newlineslinks
                               (do ((nxtnewline (first newlines) (first newlines))
                                    (newlines (rest newlines) (rest newlines))
                                    (nxtnewlink (first newlinks) (first newlinks))
                                    (newlinks (rest newlinks) (rest newlinks)))
                                   ((null nxtnewline)
                                      (list (cons nxtline (reverse newnewlines))
                                            (cons (make-link traceindex 'VERB-OBJ 'top-traces)
                                                         (reverse newnewlinks))))
                                   (setq newnewlines (cons nxtnewline newnewlines))
                                   (cond ((eq (get-synt-categ nxtnewline) 'PUNCT)
                                            (setq newnewlinks 
                                                 (cons (make-link traceindex 'SEPARATOR 'top-traces) 
                                                       newnewlinks)))
                                         ((eq (get-synt-categ nxtnewline) 'ADV)
                                            (setq adv-oldlink
                                                  (find-instrace-adverb nxtnewline advs))
                                            (cond ((null adv-oldlink)
                                                     (setq newnewlinks (cons nxtnewlink newnewlinks)))
                                                  (t (setq newnewlinks 
                                                        (cons (make-link traceindex
                                                                        (second adv-oldlink) 'top-traces)
                                                              newnewlinks)))))
                                         ((equal nxtnewline prep)
                                            (setq newnewlinks 
                                                 (cons (make-link traceindex 'PREP-RMOD 'top-traces) 
                                                       newnewlinks)))
                                         (t (setq newnewlinks (cons nxtnewlink newnewlinks))))))
                             (setq newlines 
                                 (cons (first (first newlineslinks))
                                      (cons subjtrace 
                                           (cons traceline (rest (first newlineslinks))))))
                             (setq newlinks 
                                 (cons (first (second newlineslinks))
                                      (cons (make-link traceindex 'VERB-SUBJ 'top-traces)
                                           (cons (make-link 0 'TOP-VERB 'top-traces)
                                                (rest (second newlineslinks))))))
                             ;  (format t "Case four. Result: ~a~%~a~%" newlines newlinks)
                             ;  (break "")
                                  )))
   ; ****** fifth case *********************************************************
                  ((and (null coordlinumb)	; *** this does not apply to fragments
                        (or (and (eq nxtcateg 'NOUN)
                                 (eq (second nxtlink) 'TOP-NOUN))
                            (and (eq nxtcateg 'ADJ) (eq nxt2categ 'NOUN)
                                 (eq (second nxt2link) 'TOP-NOUN)))
                        (not (null newlines)))  ; just to exclude cases of initial nouns
                                                ; "Venti ancora deboli, mari ..."
             ; (format t "case 4; nxtline = ~a~%" nxtline)
             ; (break "")
                     (setq comma-and-art (get-next-comma-art nxtsent nxtsentlinks))
                     (cond ((null comma-and-art)
                             (setq newlines (cons nxtline newlines))
                             (setq newlinks (cons nxtlink newlinks)))
       ; *** since we have to make the change, collect in prevtops the lines that
       ;     need be linked to the new top trace; they are the involved noun (nxtline)
       ;     plus a possible adverbial preceding it and linked to it
                           (t (setq commaline (first comma-and-art))
                             (setq currlinumb (get-synt-numb (first newlines)))
                             (setq subjlinumb (list currlinumb 11))
                             (setq traceindex (list currlinumb 10))
                             (setq traceline (add-synt-linumb traceindex (make-to-have-trace)))
                             (setq subjtrace (add-synt-linumb subjlinumb 
                                                 (make-to-have-subj-trace)))
                             (cond ((and (eq (get-synt-categ (first newlines)) 'ADV)
                                         (eq (first (first newlinks)) (get-synt-numb nxtline)))
                                     (setq changed-top traceindex)
                                     (setq newlines 
                                         (cons nxtline
                                             (cons subjtrace
                                                  (cons traceline newlines))))
                                     (setq newlinks 
                                         (cons (make-link traceindex 'VERB-OBJ 'top-traces)
                                              (cons (make-link traceindex 'VERB-SUBJ 'top-traces)
                                                   (cons (make-link 0 'TOP-VERB 'top-traces)
                                                        (cons (make-link traceindex 'RMOD 'top-traces)
                                                              (rest newlinks)))))))
                                  (t (break "insert-top-trace-2")))
                ; *** now, all lines until the noun have been set; modify the links of the
                ;     comma, and of the next part (recursive call to this function)
                             (do ((nxtl (first nxtsent) (first remsent))
                                  (remsent (rest nxtsent) (rest remsent))
                                  (nxtk (first nxtsentlinks) (first remsentlinks))
                                  (remsentlinks (rest nxtsentlinks) (rest remsentlinks)))
                                ((or (null nxtl)
                                     (equal nxtl commaline))
                                  (cond ((equal nxtl commaline)
                                          (cond ((null traceindex)
                                                   (break "No trace in insert top traces: case 3")))
                                          (setq newdata-and-links
                                             (singsent-insert-top-trace 
                                                remsent remsentlinks (get-synt-numb commaline)))
                                          (setq nxtsent
                                             (append 
                                ; *** the next is the previous part (including the coord)
                                                (reverse (cons nxtl templines))
                                ; *** the next is updated part after the coord
                                                (first newdata-and-links)))
                                          (setq nxtsentlinks 
                                             (append 
                                ; *** the next is the previous part (including the coord)
                                                (reverse 
                                                   (cons (make-link traceindex 'COORD 'top-traces) templinks))
                                ; *** the next is updated part after the coord
                                                (second newdata-and-links))))
                                       (t (break "Insert-top-trace: comma line not found"))))
                                (setq templines (cons nxtl templines))
                                (setq templinks (cons nxtk templinks))))))
   ; ****** sixth case *********************************************************
   ; *** this is identified by a sequence of dependents from a previous verb of
   ;     the form <SEPARATOR ADV-TIME RMOD1 RMOD2 ... RMODk>
   ;     where the first three items must be adjacent
   ;     There must be at least 2 RMODi, and exactly one of them must be unmarked
                  ((and (null coordlinumb)	; *** this does not apply to fragments
                        (eq (get-synt-word nxtline) #\,)
                        (eq nxt2categ 'ADV)
                        (eq (get-synt-type (first nxtsent)) 'TIME)
                        (lab-subsumes 'RMOD (second nxt3link))
                        (eq (first nxtlink) (first nxt3link))
                        (eq (first nxt2link) (get-synt-numb (second nxtsent))))
      ; *** the comma and the prep have the same parent, while the adv depends on the prep
                    (setq toplinumb (first nxtlink))
      ; *** look for the upper line; if it is governed by a modal, set the coordination
      ;     link to the modal, instead of to the governed verb
             ;  (format t "Case five; nxtline: ~a~% toplinumb: ~a~% newlines: ~a~% newlinks: ~a~%"
             ;              nxtline toplinumb newlines newlinks)
             ;  (break "")
                    (setq toplinelink (find-a-line (list 'position toplinumb)
                                 newlines newlinks))
             ;  (format t "Case five; toplinelink: ~a~%" toplinelink)
             ;  (break "")
                    (cond ((eq (second (second toplinelink)) 'VERB+MODAL-INDCOMPL)
                             (setq acttoplinumb (first (second toplinelink)))
                             (setq toplinelink (find-a-line (list 'position acttoplinumb)
                                                      newlines newlinks))))
      ; *** now toplinumb is the governed verb and acttoplinumb is the governing modal
      ;     If the involved verb is the head of a relative, do nothing
                    (cond ((eq (second (second toplinelink)) 'VERB-RMOD+RELCL)
                             (setq newlines (cons nxtline newlines))
                             (setq newlinks (cons nxtlink newlinks)))
      ; *** the next loop checks the RMOD of the involved governing line
                        (t (do ((nnxtline (first (rest nxtsent)) (first remlines))
                                (remlines (rest (rest nxtsent)) (rest remlines))
                                (nnxtlink (first (rest nxtsentlinks)) (first remlinks))
                                (remlinks (rest (rest nxtsentlinks)) (rest remlinks)))
             ; *** now, we have:
             ;     nxtline: the comma line
             ;     nxtsent: the lines after the comma (used in the outer loop)
             ;     nnxtline: the lines after the comma (scanned inside the inner loop)
             ;     remlines: the lines remaining to be seen inside the inner loop
             ; *** analogously for the links
                              ((null nnxtline)
                                 (setq ok-deps (check-deps-for-trace found-deps nil nil)))
                              (cond ((and (eq (first nnxtlink) toplinumb)
                                          (lab-subsumes 'RMOD (second nnxtlink)))
                                      (setq found-deps (append1 found-deps nnxtline)))))
                           (cond ((not (null ok-deps))
             ; *** all conditions for the change have been met
                                    (setq tracelinumb (get-synt-numb nxtline))
                ; *** tracelinumb is the line of the comma
                     ;  (format t "Case four; ok-deps: ~a~%" ok-deps)
                     ;  (break "")
                ; *** the next changes the link of the comma
                                    (cond ((null acttoplinumb)
                                             (setq newlines (cons nxtline newlines))
                                             (setq newlinks (cons nxtlink newlinks))
                                            ; (break "No trace in insert top traces: case 4")
                                             )
                                          (t (setq nxtlink (list acttoplinumb 'COORD 'top-traces))
                                            (setq nxtsentlinks
                                               (append
                                                  (list 
                                                     (make-link tracelinumb 'COORD2ND 'top-traces)
                                                     (make-link (list tracelinumb 10) 'VERB-SUBJ 'top-traces)
                                                     (make-link (list tracelinumb 10) 'ADVB-RMOD 'top-traces))
                                                  (change-case-four-lines
                                                      ok-deps t
                                                      (list tracelinumb 10)
                                                      (rest nxtsent)
                                                      (rest nxtsentlinks))))
                     ;  (format t "Insert top-trace: fourth case~%  Links: ~a~%" nxtsentlinks)
                     ;  (break "")
                                            (setq nxtsent
                                               (append 
                                                  (list (add-synt-linumb (list tracelinumb 10) 
                                                              (make-to-have-trace))
                                                        (add-synt-linumb (list tracelinumb 11) 
                                                              (make-to-have-subj-trace))
                                                        (first nxtsent))
                                                  (rest nxtsent)))
                                             (setq newlines (cons nxtline newlines))
                                             (setq newlinks (cons nxtlink newlinks)))))
             ; *** the conditions have not been met
                                 (t (setq newlines (cons nxtline newlines))
                                    (setq newlinks (cons nxtlink newlinks)))))))
   ; ****** continuation *********************************************************
                  ((and (null coordlinumb)	; *** this does not apply to fragments
                        changed-top
                        (eq (second nxtlink) 'END))
                     (setq newlines (cons nxtline newlines))
                     (setq newlinks (cons (make-link changed-top 'END 'top-traces) newlinks)))
                  (t (setq newlines (cons nxtline newlines))
                     (setq newlinks (cons nxtlink newlinks)))))))

; ***************************************************************
; *** this checks if newlines is a (reversed) sequence of words that
;     is a PP-complex starting at the beginning of the sentence (end
;     of newlines). The PP-complex can begin after one or more
;     adverbials and/or punctuation marks
; *** OUTPUT: a triple composed of
;     a. The line of the preposition which is the chunk head
;     b. The line number of the last line of the chunk
;     c. A list of pairs <line link> for all intervening adverbials
(defun PP-initial (newlines newlinks)
  (let (found fail advs chunk-end (allines newlines) (allinks newlinks))
    (do ((nxtline (first newlines) (first newlines))
         (newlines (rest newlines) (rest newlines))
         (nxtlink (first newlinks) (first newlinks))
         (newlinks (rest newlinks) (rest newlinks)))
        ((or (null nxtline) found fail)
          (cond (found (values found chunk-end advs))
                (t nil)))
        (cond ((eq (get-synt-categ nxtline) 'PUNCT) nil)
              ((eq (get-synt-categ nxtline) 'ADV)
   ; *** an adverb can be part of the prepositional chunk
                 (setq found
                      (chunk-head-cat 'backward nil nil 
                           nxtline nxtlink newlines newlinks allines allinks '(prep) nil))
      ; *** if it is not part of the prepositional chunk, it is saved in "advs"
                 (cond ((not found)
                          (setq advs (cons (list nxtline nxtlink) advs)))
                       ((neq 1 (get-synt-numb found))
                          (setq fail t))
                       (t (setq chunk-end (get-synt-numb nxtline)))))
   ; *** any other word must be part of the prepositional chunk starting at the beginning
              (t (setq found
                      (chunk-head-cat 'backward nil nil 
                           nxtline nxtlink newlines newlinks allines allinks '(prep) nil))
      ; *** otherwise, failure
                 (cond ((or (not found) 
                            (neq 1 (get-synt-numb found)))
                          (setq fail t))
                       (t (setq chunk-end (get-synt-numb nxtline)))))))))

; ***************************************************************
; *** looks for an adverb line in the list built by the function above
;     if it finds the adverb, returns the adverb link, otherwise nil
(defun find-instrace-adverb (nxtnewline advs)
   (cond ((null advs) nil)
         ((equal nxtnewline (first (first advs)))
            (second (first advs)))
         (t (find-instrace-adverb nxtnewline (rest advs)))))

; ***************************************************************
(defun check-deps-for-trace (found-deps found-subj others)
; *** found-subj, initially nil, specifies if an unmarked dependent has
;     already been found
  (cond ((null found-deps)
           (cond ((null found-subj) nil)	; *** no unmarked dependent
                 (t (cons found-subj others))))
        ((not (memq (get-synt-categ (first found-deps)) '(PREP ADV)))
           (cond ((null found-subj)
                    (check-deps-for-trace (rest found-deps) 
                                     (first found-deps) others))
                 (t nil))) 	; *** more than one unmarked dependent
        (t (check-deps-for-trace (rest found-deps) 
                  found-subj (append1 others (first found-deps))))))

; **************************************************************
(defun change-case-four-lines (changes start? traceindex oldlines oldlinks)
; *** changes is originally of the following form:
;     (obj-line rmod-line1 ... rmod-linek)
;     obj-line can be in any position, while the rmod-lines are in order
; *** start is used because the first lines of changes must become the verb-obj
;     while the other ones remain as RMOD
; *** the functions recurs on the oldlines, looking for the changes
    (cond ((null changes) oldlinks)
          ((null oldlines) 
             (break "Change not found in change-case-four-lines"))
          (start?
             (cond ((eq (get-synt-numb (first oldlines)) (get-synt-numb (first changes)))
                 ; *** we have found the subject; remove it and go ahead on the
                 ;     remaining changes (if any)
                     (cons (make-link traceindex 'VERB-OBJ 'top-traces)
                           (change-case-four-lines 
                                 (rest changes) nil traceindex 
                                 (rest oldlines) (rest oldlinks))))
                   ((eq (get-synt-numb (first oldlines)) (get-synt-numb (second changes)))
                 ; *** this is not the subject, but one of the other dependents
                     (cons (make-link traceindex 'RMOD 'top-traces)
                           (change-case-four-lines 
                                 (cons (first changes) (rest (rest changes)))
                                 t traceindex (rest oldlines) (rest oldlinks))))
                   (t   ; *** this is a line not involved in the change
                     (cons (first oldlinks)
                           (change-case-four-lines 
                                 changes t traceindex (rest oldlines) (rest oldlinks))))))
          (t          ; *** the subject line has already been found
             (cond ((eq (get-synt-numb (first oldlines)) (get-synt-numb (first changes)))
                     (cons (make-link traceindex 'RMOD 'top-traces)
                           (change-case-four-lines 
                                 (rest changes) nil traceindex 
                                 (rest oldlines) (rest oldlinks))))
                   (t   ; *** this is a line not involved in the change
                     (cons (first oldlinks)
                           (change-case-four-lines 
                                 changes nil traceindex (rest oldlines) (rest oldlinks))))))))
         

; ***************************************************************
; *** this creates the internal representation of a trace of "to have"
;     (exclusive of the line number)
(defun make-to-have-trace ()
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (list '|t| '(avere verb main ind allval intrans 1 pl)))
        (t `((form #\t)
             (syn ((lemma avere) (cat verb) (type main) (mood ind)
                   (tense allval) (trans intrans) (person 1) (number pl)))
             (sem nil)
             (coref empty)))))

; ***************************************************************
; *** this creates the internal representation of a trace for the
;     subject (we) of "to have"
(defun make-to-have-subj-trace ()
  (declare (special *TREE-FORMAT*))
  (cond ((eq *TREE-FORMAT* 'tut)
           (list '|t| `(generic-t pron pers allval pl 1)))
        (t `((form #\t)
             (syn ((lemma generic-t) (cat pron) (type pers)
                   (gender allval) (number pl) (person 1)))
             (sem nil)
             (coref empty)))))

; ***************************************************************
(defun modify-links-to-2nd-verb (lines links traceindex)
  (let (newlinks found-obj)
    (do ((nxtline (first lines) (first lines))
         (lines (rest lines) (rest lines))
         (nxtlink (first links) (first links))
         (links (rest links) (rest links)))
        ((null nxtline) (reverse newlinks))
        (cond ((memq (second nxtlink) '(COORD2ND UNKNOWN))
                (cond ((memq (get-synt-categ nxtline) '(art adj noun pron))
                         (cond ((not found-obj)
                                 (setq newlinks 
                                    (cons (make-link traceindex 'VERB-OBJ 'top-traces) newlinks))
                                 (setq found-obj t))
                               (t (setq newlinks 
                                    (cons (make-link traceindex 'EXTRAOBJ 'top-traces) newlinks)))))
                      (t (setq newlinks (cons (make-link traceindex 'RMOD 'top-traces) newlinks)))))
              (t (setq newlinks (cons nxtlink newlinks)))))))

; ***************************************************************
; *** This looks for a sequence 
;     ... , ... ART, where the comma is a separator, and the ART is unattached
;     If it finds them, it returns the two lines comma and ART, nil otherwise
(defun get-next-comma-art (restlines restlinks)
  (let ((status 'start) comma-and-art)
      (do ((nxtline (first restlines) (first restlines))
           (restlines (rest restlines) (rest restlines))
           (nxtlink (first restlinks) (first restlinks))
           (restlinks (rest restlinks) (rest restlinks)))
        ((or (null nxtline) (eq status 'stop))
          (cond ((eq status 'stop) comma-and-art)
                (t nil)))
        (cond ((and (eq status 'start)
                    (eq (get-synt-word nxtline) #\,)
                    (memq (second nxtlink) '(SEPARATOR COORD)))
   ; *** we have found the comma 
                 (setq comma-and-art (list nxtline))
                 (setq status 'after-comma))
              ((and (eq status 'after-comma)
                    (eq (get-synt-categ nxtline) 'ART)
                    (or (null (second nxtlink))
                        (memq (second nxtlink) '(UNKNOWN COORD2ND))))
   ; *** we have found the ART
                 (setq comma-and-art (append1 comma-and-art nxtline))
                 (setq status 'stop))
              ((memq (get-synt-categ nxtline) '(punct verb))
   ; *** if we find a punctuation mark or a verb, stop the search
                 (setq comma-and-art nil)
                 (setq status 'stop))))))

; ***************************************************************
; *** returns all the lines in newlines that have a link to a line
;     following conjlinumb, checking that all the links have the
;     same pointer
(defun get-prev-toplines (newlines newlinks conjlinumb)
  (let (result linumb fail)
    (do ((nxtline (first newlines) (first newlines))
         (newlines (rest newlines) (rest newlines))
         (nxtlink (first newlinks) (first newlinks))
         (newlinks (rest newlinks) (rest newlinks)))
        ((or (null nxtline) fail)
           (cond (fail nil)
                 (t (values result linumb))))
        (cond ((null (first nxtlink))
                 (setq fail t))
              ((or (index-precedes conjlinumb (first nxtlink))
                   (eq 0 (first nxtlink)))
                 (cond ((or (null linumb) 
                            (equal linumb (first nxtlink)))
   ; *** it is the first such a line, or, if not, its link up is the
   ;     same of the previous one(s)
                          (setq linumb (first nxtlink))
                          (setq result (append1 result nxtline)))
                       (t (setq fail t))))))))

; ***************************************************************
; *** this changes all the links of the lines in prevtops, to make
;     them point to the new trace; also, it sets the (unique) NP
;     as the VERB-OBJ of the trace line
; *** the new traces have to be added imediately before the second item in prevtops
;     However, if it is an NP, the item in prevtops could be the head noun, possibly
;     preceded by an ADJ. So, what is done is:
;     1. If the first item is an ADV, put the traces immediately after it.
;     2. If the first item is the NP, put the traces immediately before the following
;        ADV (or at the end, if there is no ADV)
(defun modify-top-trace-links (lines links prevtops verbtrace subjtrace)
  (let ((lines (reverse lines)) (links (reverse links))
        (prevtops (reverse prevtops)) trace-added?
        add-trace-before-adv tracelinumb newlines newlinks prevlinumb
        prevtoplinumb)
     (do ((nxtline (first lines) (first lines))
          (lines (rest lines) (rest lines))
          (nxtlink (first links) (first links))
          (links (rest links) (rest links)))
         ((null lines)
   ; *** at the end, all items in prevtops must have been consumed
   ;     The loop is interrupted one step earlier, since the last
   ;     item is the comma, and traces that have not yet been included
   ;     must be inserted before it
           (cond ((null prevtops) 
        ; *** if the traces have already been added, return the results
                    (cond (trace-added? 
                            (values tracelinumb (cons nxtline newlines) 
                                                (cons nxtlink newlinks)))
        ; *** otherwise, insert the traces at the end of the result (at the
        ;     beginning, since the result is in inverse order)
                          (t (setq tracelinumb (list prevlinumb 10))
        ; *** the next, since, when the NP head link was changed the position of the new trace
        ;     was not yet known
                             (setq newlinks (adjust-np-pointer 
                                               newlines newlinks prevtoplinumb tracelinumb))
                             (values tracelinumb
                                   (append 
                                      (list
                                         nxtline
                                         (add-synt-linumb (list prevlinumb 11) subjtrace)
                                         (add-synt-linumb tracelinumb verbtrace))
                                      newlines)
                                   (append 
                                      (list
                                         nxtlink
                                         (make-link tracelinumb 'VERB-SUBJ 'top-traces)
                                         (make-link 0 'TOP-VERB 'top-traces))
                                      newlinks)))))
                 (t (break "modify-top-trace-links"))))
   ; *** BODY ***************
         (cond ((equal nxtline (first prevtops))
       ; *** we have found a line in prevtops; insert it in newlines, but change the link
                 (setq prevtops (rest prevtops))
                 (cond ((eq (get-synt-categ nxtline) 'ADV)
            ; *** if it is an ADV, but the traces have already been inserted, add it,
            ;     modifying the link upward, but keeping the label
                          (cond (trace-added?
                                  (setq newlines (cons nxtline newlines))
                                  (setq newlinks (cons (make-link tracelinumb (second nxtlink) 'top-traces)
                                                       newlinks)))
            ; *** if it is an ADV, the traces have not been inserted, and a previous NP
            ;     has already been met, insert the traces before the ADV
                                (add-trace-before-adv
                                  (setq trace-added? t)
                                  (setq tracelinumb (list prevtoplinumb 10))
                                  (setq newlines 
                                     (append (list nxtline 
                                                  (add-synt-linumb (list prevtoplinumb 11)
                                                                    subjtrace)
                                                  (add-synt-linumb tracelinumb verbtrace))
                                             newlines))
                                  (setq newlinks 
                                     (append (list (make-link tracelinumb (second nxtlink) 'top-traces)
                                                   (make-link tracelinumb 'VERB-OBJ 'top-traces)
                                                   (make-link 0 'TOP-VERB 'top-traces))
                                             newlinks)))
            ; *** otherwise, insert the traces after the ADV
                                (t (setq trace-added? t)
                                  (setq prevtoplinumb (get-synt-numb nxtline))
                                  (setq tracelinumb (list prevtoplinumb 10))
                                  (setq newlines 
                                     (append (list (add-synt-linumb (list prevtoplinumb 11)
                                                                    subjtrace)
                                                   (add-synt-linumb tracelinumb verbtrace)
                                                   nxtline)
                                             newlines))
                                  (setq newlinks 
                                     (append (list (make-link tracelinumb 'VERB-SUBJ 'top-traces)
                                                   (make-link 0 'TOP-VERB 'top-traces)
                                                   (make-link tracelinumb (second nxtlink) 'top-traces))
                                             newlinks)))))
                       (t    ; **** it must be the NP
                          (cond ((not trace-added?) 
                                   (setq prevtoplinumb (get-synt-numb nxtline))
                                   (setq add-trace-before-adv t)))
                          (setq newlines (cons nxtline newlines))
                          (setq newlinks (cons (make-link tracelinumb 'VERB-OBJ 'top-traces) newlinks)))))
      ; *** it is not a line in prevtops
               (t (setq newlines (cons nxtline newlines))
                  (setq newlinks (cons nxtlink newlinks))))
         (setq prevlinumb (get-synt-numb nxtline)))))

; ***************************************************************
(defun adjust-np-pointer (newlines newlinks prevtoplinumb tracelinumb)
  (cond ((null newlinks) (break "no item found in adjust-np-pointer"))
        ((equal (get-synt-numb (first newlines)) prevtoplinumb)
           (cons (make-link tracelinumb (second (first newlinks)) 'top-traces) (rest newlinks)))
        (t (cons (first newlinks) 
                 (adjust-np-pointer 
                      (rest newlines) (rest newlinks) prevtoplinumb tracelinumb)))))

; ***************************************************************
(defun add-synt-linumb (linumb restline)
  (declare (special *TREE-FORMAT*))
   (cond ((eq *TREE-FORMAT* 'tut) (cons linumb restline))
         (t (cons (list 'posit linumb) restline))))
 
