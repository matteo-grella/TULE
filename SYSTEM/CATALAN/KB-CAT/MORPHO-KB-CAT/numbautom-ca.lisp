
(in-package "USER")

(defun load-numb-arcs (autom)
   (mapc #'(lambda (x) 
		(putprop (first x) (rest (rest x)) 'numb-arcs)
		(putprop (first x) (second x) 'final-info))
	      autom))

(defun incrval ()
  (declare (special value curval))
  (setq value (+ value curval)))

(defun incrval+ord ()
  (declare (special value curval))
  (setq value (- 0 (+ value curval))))
  ; *** if a resulting value is negative, it refers to an ordinal

(defun multval+ord ()
  (declare (special value curval))
  (setq value (- 0 (* value curval))))
  ; *** if a resulting value is negative, it refers to an ordinal

(defun multval () 
  (declare (special value curval))
  (setq value (* value curval)))

(defun saveval () 
  (declare (special prevval value))
  (setq prevval value))

; *******************************************************************
; *** the next is the top-level automaton; the label of the arcs are
;     associated with the components recognized by the lower automaton
; *** Two global variables are used:
;     - Value, which holds the total value obtained until now
;     - curval, which is the value associated with the last component
;       recognized by the lower automaton
; *** the arc labels are:
;     n-uno
;     n-unit-ord; the associated value is a list <value back? gender number numchars>,
;                 where numchars is the number of characters that are part of the
;                 suffix (so they should not be returned)
(load-numb-arcs
'((nu-0 nil
   (n-unit-norm 	nu-2- 	(incrval))
   (n-dec 		nu-3- 	(incrval))
   (n-basic-dec-unit 	nu-4- 	(incrval))
   (n-cent 		nu-5- 	(incrval))
   (n-mil 		nu-6- 	(incrval))
   (n-unit-ord 	        stop	(incrval+ord))
   (n-dec-ord 		stop 	(incrval+ord))
   (n-cent-ord 		stop 	(incrval+ord))
   (elision 		nu-1 	(incrval)))
  (nu-1 (final))
  (nu-2- (final)
   (hyphen		nu-2    (saveval)))
  (nu-2 (final prevval)
   (n-cent-plur 	nu-5- 	(multval))
   (n-cent-ord 		stop 	(multval+ord))
   (n-mil 		nu-6- 	(multval)))
  (nu-3- (final)
   (hyphen		nu-3    (saveval)))
  (nu-3 (final prevval)
   (n-unit-norm 	nu-4- 	(incrval))
   (n-unit-ord 		stop 	(incrval+ord))
   (n-unit-1-ord 	stop 	(incrval+ord))
   (n-mil 		nu-6- 	(multval)))
  (nu-4- (final)
   (hyphen		nu-4	(saveval)))
  (nu-4 (final prevval)
   (n-mil 		nu-6- 	(multval)))
  (nu-5- (final)
   (hyphen		nu-5	(saveval)))
  (nu-5 (final prevval)
   (n-unit-norm 	nu-7- 	(incrval))
   (n-dec 		nu-8- 	(incrval))
   (n-basic-dec-unit 	nu-7- 	(incrval))
   (n-mil 		nu-6- 	(multval)))
  (nu-6- (final)
   (hyphen		nu-6	(saveval)))
  (nu-6 (final prevval)
   (n-unit-norm 	nu-9- 	(setq tempval curval))
   (n-dec 		nu-10- 	(setq tempval curval))
   (n-basic-dec-unit 	nu-11- 	(setq tempval curval))
   (n-cent 		nu-12- 	(setq tempval curval)))
  (nu-7- (final)
   (hyphen		nu-7	(saveval)))
  (nu-7 (final)
   (n-mil 		nu-6- 	(multval)))
  (nu-8- (final)
   (hyphen		nu-8	(saveval)))
  (nu-8 (final prevval)
   (n-unit-norm 	nu-7- 	(incrval))
   (n-mil 		nu-6- 	(multval)))
  (nu-9- (final)
   (hyphen		nu-9	(saveval)))
  (nu-9 (final (+ prevval tempval))
   (n-cent 		nu-12- 	(setq tempval (* tempval curval))))
  (nu-10- (final)
   (hyphen		nu-10	(saveval)))
  (nu-10 (final (+ prevval tempval))
   (n-unit-norm		nu-11- 	(setq tempval (+ tempval curval))))
  (nu-11- (final)
   (hyphen		nu-11	(saveval)))
  (nu-11 (final (+ prevval tempval)))
  (nu-12- (final)
   (hyphen		nu-12	(saveval)))
  (nu-12 (final (+ prevval tempval))
   (n-unit-norm 	nu-1 	(setq value (+ (+ value tempval) curval)))
   (n-dec 		nu-13 	(setq tempval (+ tempval curval)))
   (n-basic-dec-unit 	nu-1 	(setq value (+ (+ value tempval) curval))))
  (nu-13- (final)
   (hyphen		nu-13	(saveval)))
  (nu-13 (final (+ prevval tempval))
   (n-unit-norm		nu-1 	(setq value (+ (+ value tempval) curval))))))

; ***************************************************************
;   l'automa che segue ha il compito di riconoscere  i token elementari,
;   che vengono poi messi insieme dall'automa precedente.
;   Le categorie assegnate (usate sopra) sono le seguenti:
;   - n-uno: uno
;   - n-otto: otto
;   - n-unit-norm: due tre quattro cinque sei sette nove
;   - n-dec: venti trenta quaranta cinquanta sessanta settanta ottanta novanta
;   - n-basic-dec-unit: dieci undici dodici tredici quattordici quindici sedici
;		diciassette diciotto diciannove ventuno ventotto trentuno
;		trentotto quarantuno quarantotto cinquantuno cinquantotto
;		sessantuno sessantotto settantuno settantotto ottantuno
;		ottantotto novantuno novantotto
;   - n-cento: cento
;   - n-mila: mila
;   - n-mille: mille

(defun load-nch-arcs (autom)
   (mapc #'(lambda (x) (putprop (first x) (rest x) 'nu-ch-arcs)) autom))

(load-nch-arcs
 '((nu-ch-0 (#\c nu-ch-c)
	   (#\d nu-ch-d)
	   (#\h nu-ch-h)
	   (#\m nu-ch-m)
	   (#\n nu-ch-n)
	   (#\o nu-ch-o)
	   (#\p nu-ch-p)
	   (#\q nu-ch-q)
	   (#\s nu-ch-s)
	   (#\t nu-ch-t)
	   (#\u nu-ch-u)
	   (#\v nu-ch-v)
           (#\- stop (hyphen)))
  (nu-ch-c (#\a nu-ch-ca) 
           (#\e nu-ch-ce)
           (#\i nu-ch-ci))
  (nu-ch-ca (#\t nu-ch-cat))
  (nu-ch-cat (#\o nu-ch-cato))
  (nu-ch-cato (#\r nu-ch-cator))
  (nu-ch-cator (#\z nu-ch-catorz))
  (nu-ch-catorz (#\e nu-ch-catorze)	
                (#\è stop (n-unit-ord 14 nil m sing 0 nil)))
  (nu-ch-catorze (#\n nu-ch-ordsuff-2-en (setq tempord 14))
                 (none stop (n-basic-dec-unit 14 back)))	; 14
  (nu-ch-ce (#\n nu-ch-cen))
  (nu-ch-cen (#\t nu-ch-cent))
  (nu-ch-cent (#\è nu-ch-centè)
              (#\e nu-ch-ordcent (setq tempord 100))
              (#\s stop (n-cent-plur 100)) 			; 100
              (none stop (n-cent 100))) 			; 100
  (nu-ch-centè (#\s nu-ch-centès)
               (none stop (n-cent-ord 100 nil m sing)))
  (nu-ch-centès (#\i nu-ch-centèsi))
  (nu-ch-centèsi (#\m nu-ch-ordcent-1 (setq tempord 100)))
  (nu-ch-ci (#\n nu-ch-cin))
  (nu-ch-cin (#\c stop (n-unit-norm 5))			; 5
             (#\q nu-ch-cinq))
  (nu-ch-cinq (#\u nu-x+ord-2 (setq tempord 5)))
  (nu-ch-d (#\e nu-ch-de) 
           (#\è nu-ch-dè)
           (#\i nu-ch-di)
	   (#\o nu-ch-do)
	   (#\u nu-ch-du))
  (nu-ch-de (#\n nu-ch-den)
	    (#\s nu-ch-des)
	    (#\u stop (n-unit-norm 2))			; 2
	    (#\v nu-ch-dev))
  (nu-ch-den (#\o nu-ch-deno))
  (nu-ch-deno (#\u stop (n-basic-dec-unit 19))		; 19
              (#\v nu-ch-ordsuff-2 (setq tempord 19)))
  (nu-ch-des (#\s nu-ch-dess)
             (#\e nu-ch-ordsuff-2-e (setq tempord 10))
             (#\è stop (n-unit-ord 10 nil m sing 0 nil)))
  (nu-ch-dess (#\e nu-ch-desse))
  (nu-ch-desse (#\t nu-ch-desset))
  (nu-ch-desset (#\e nu-ch-ordsuff-2-e (setq tempord 17))
                (#\è stop (n-unit-ord 17 nil m sing 0 nil))
                (none stop (n-basic-dec-unit 17 back)))	; 17
  (nu-ch-dev (#\u nu-ch-devu))
  (nu-ch-devu (#\i nu-ch-devui))
  (nu-ch-devui (#\t nu-ch-devuit))
  (nu-ch-devuit (#\e nu-ch-ordsuff-2-e (setq tempord 18))
                (#\è stop (n-unit-ord 18 nil m sing 0 nil))
                (none stop (n-basic-dec-unit 18 back)))	; 87
  (nu-ch-dè (#\c nu-ch-dèc)
            (#\s nu-ch-des))
  (nu-ch-dèc (#\i nu-ch-dèci))
  (nu-ch-dèci (#\m nu-ch-orddec (setq tempord 10))) 
  (nu-ch-di (#\n nu-ch-den)
	    (#\s nu-ch-des)
	    (#\v nu-ch-dev))
  (nu-ch-do (#\s nu-ch-dos)
	    (#\t nu-ch-dot))
  (nu-ch-dos (#\è stop (n-unit-1-ord 2 nil m sing))		; un-
             (#\e nu-ch-ordsuff-3-e (setq tempord 2))
             (none stop (n-unit-norm 2)))			; 2
  (nu-ch-dot (#\z nu-ch-dotz))
  (nu-ch-dotz (#\e nu-ch-dotze)
              (#\è stop (n-unit-ord 12 nil m sing  0 nil)))
  (nu-ch-dotze (#\n nu-ch-ordsuff-2-en (setq tempord 12))
               (none stop (n-basic-dec-unit 12 back)))	; 12
  (nu-ch-du (#\e nu-ch-due))
  (nu-ch-due (#\s stop (n-unit-norm 2)))		; 2
  (nu-ch-h (#\u nu-ch-hu))
  (nu-ch-hu (#\i nu-ch-hui))
  (nu-ch-hui (#\t nu-ch-huit))
  (nu-ch-huit (#\a nu-x+ord-a (setq temp 80 tempord 80))
	      (#\- stop (n-unit-norm 8 back))		; 8
	      (#\c stop (n-unit-norm 8 back))		; 8
	      (#\m stop (n-unit-norm 8 back))		; 8
	      (none stop (n-unit-norm 8)))		; 8
  (nu-ch-m (#\i nu-ch-mi))
  (nu-ch-mi (#\l stop (n-mil 1000)))
  (nu-ch-n (#\o nu-ch-no))
  (nu-ch-no (#\n nu-ch-non)
            (#\r nu-x+ord-3 (setq temp 90))
            (#\u stop (n-unit-norm 9))			; 9
            (#\v nu-ch-ordsuff-2 (setq tempord 9)))
  (nu-ch-non (#\a nu-ch-nona))
  (nu-ch-nona (#\g nu-ch-ord-ig (setq tempord 90)))
  (nu-ch-o (#\c nu-ch-oc)
           (#\n nu-ch-on))
  (nu-ch-oc (#\t nu-ch-oct))
  (nu-ch-oct (#\a nu-ch-octa)
             (#\o nu-ch-octo))
  (nu-ch-octa (#\u nu-ch-octau)
              (#\v nu-ch-octav))
  (nu-ch-octau (#\s stop (n-unit-ord 8 nil m pl 1 nil))
               (none stop (n-unit-ord 8 back m sing 0 nil)))
  (nu-ch-octav (#\a stop (n-unit-ord 8 nil f sing 2 u))
               (#\e nu-ch-octave))
  (nu-ch-octave (#\s stop (n-unit-ord 8 nil f pl 3 u)))
  (nu-ch-octo (#\g nu-ch-ord-ig (setq tempord 80)))
  (nu-ch-on (#\z nu-ch-onz))
  (nu-ch-onz (#\e nu-ch-onze)				; 11
             (#\è stop (n-unit-ord 11 nil m sing 0 nil)))
  (nu-ch-onze (#\n nu-ch-ordsuff-2-en (setq tempord 11))
              (none stop (n-basic-dec-unit 11 back)))		; 11
  (nu-ch-p (#\r nu-ch-pr))
  (nu-ch-pr (#\i nu-ch-pri))
  (nu-ch-pri (#\m nu-ch-prim))
  (nu-ch-prim (#\e nu-ch-prime))
  (nu-ch-prime (#\r nu-ch-ordsuff (setq tempord 1)))
  (nu-ch-q (#\u nu-ch-qu))
  (nu-ch-qu (#\a nu-ch-qua)
            (#\i nu-ch-qui))
  (nu-ch-qua (#\r nu-x+ord (setq temp 40 tempord 40))
             (#\d nu-ch-quad)
             (#\t nu-ch-quat))
  (nu-ch-quad (#\r nu-ch-quadr))
  (nu-ch-quadr (#\a nu-ch-quadra))
  (nu-ch-quadra (#\g nu-ch-ord-ig (setq tempord 40)))
  (nu-ch-quat (#\r nu-ch-quatr))
  (nu-ch-quatr (#\e nu-ch-quatre)
               (#\è stop (n-unit-1-ord 4 nil m sing)))
  (nu-ch-quatre (#\n nu-ch-ordsuff-3-en (setq tempord 4))
                (none stop (n-unit-norm 4)))		; 4
  (nu-ch-qui (#\n nu-ch-quin))
  (nu-ch-quin (#\q nu-ch-quinq) 
              (#\z nu-ch-quinz)
              (#\t nu-ch-ordsuff (setq tempord 5)))
  (nu-ch-quinq (#\u nu-ch-quinqu)) 
  (nu-ch-quinqu (#\a nu-ch-quinqua)) 
  (nu-ch-quinqua (#\g nu-ch-ord-ig (setq tempord 50)))
  (nu-ch-quinz (#\e stop (n-basic-dec-unit 15)))	; 15
  (nu-ch-quinz (#\e nu-ch-quinze)	
               (#\è stop (n-unit-ord 15 nil m sing 0 nil)))
  (nu-ch-quinze (#\n nu-ch-ordsuff-2-en (setq tempord 15))
                (none stop (n-basic-dec-unit 15 back)))	; 15
  (nu-ch-s (#\e nu-ch-se)
           (#\è nu-ch-sè)
           (#\i nu-ch-si))
  (nu-ch-se (#\t nu-ch-set)
            (#\g nu-ch-seg)
            (#\i nu-ch-sei)
            (#\p nu-ch-sep)
            (#\x nu-ch-sex))
  (nu-ch-seg (#\o nu-ch-sego))
  (nu-ch-sego (#\n nu-ch-ordsuff (setq tempord 2)))
  (nu-ch-sei (#\x nu-x+ord-3 (setq temp 60 tempord 60)))
  (nu-ch-sep (#\t nu-ch-sept))
  (nu-ch-sept (#\u nu-ch-septu))
  (nu-ch-septu (#\a nu-ch-septua))
  (nu-ch-septua (#\g nu-ch-ord-ig (setq tempord 70)))
  (nu-ch-set (#\a nu-x-a (setq temp 70))
	     (#\c stop (n-unit-norm 7 back))		; 7
             (#\e nu-ch-ordsuff-2-e (setq tempord 7))
	     (#\m stop (n-unit-norm 7 back))		; 7
	     (#\z nu-ch-setz)
             (#\è stop (n-unit-ord 7 nil m sing 0 nil))
	     (#\- stop (n-unit-norm 7 back))		; 7
	     (none stop (n-unit-norm 7)))		; 7
  (nu-ch-setz (#\e nu-ch-setze)	
              (#\è stop (n-unit-ord 16 nil m sing 0 nil)))
  (nu-ch-setze (#\n nu-ch-ordsuff-2-en (setq tempord 16))
               (none stop (n-basic-dec-unit 16 back)))	; 16
  (nu-ch-sex (#\a nu-ch-sexa)
             (#\t nu-ch-sext (setq tempord 6)))
  (nu-ch-sext (#\a stop (n-unit-ord 6 nil f sing 1 nil))
              (#\e nu-ch-sexte)
              (#\o nu-ch-sexto)
              (none stop (n-unit-ord 6 nil m sing 0 nil)))
  (nu-ch-sexte (#\s stop (n-unit-ord 6 nil f pl 2 nil)))
  (nu-ch-sexto (#\s stop (n-unit-ord 6 nil m pl 2 nil)))
  (nu-ch-sexa (#\g nu-ch-ord-ig (setq tempord 60)))
  (nu-ch-si (#\s nu-ch-sis))
  (nu-ch-sis (#\è stop (n-unit-ord 6 nil m sing 0 nil))
             (#\e nu-ch-ordsuff-2-e (setq tempord 6))
             (none stop (n-unit-norm 6 back)))			; 6
  (nu-ch-sè (#\p nu-ch-sèp))
  (nu-ch-sèp (#\t nu-ch-sèpt))
  (nu-ch-sèpt (#\i nu-ch-sèpti))
  (nu-ch-sèpti (#\m nu-ch-ordsuff (setq tempord 7)))
  (nu-ch-t (#\e nu-ch-te)
           (#\r nu-ch-tr))
  (nu-ch-te (#\r nu-ch-ter))
  (nu-ch-ter (#\c nu-ch-terc))
  (nu-ch-terc (#\e nu-ch-terce))
  (nu-ch-terce (#\r nu-ch-ordsuff (setq tempord 3)))
  (nu-ch-tr (#\e nu-ch-tre)
            (#\i nu-ch-tri))
  (nu-ch-tre (#\s nu-ch-tres)
             (#\n nu-ch-tren)
             (#\t nu-ch-tret))
  (nu-ch-tren (#\t nu-ch-trent))
  (nu-ch-trent (#\a stop (n-dec 30))
               (#\e nu-ch-ordsuff-2-e (setq tempord 30))
               (#\è stop (n-unit-ord 30 nil m sing 0 nil)))
  (nu-ch-tres (#\è stop (n-unit-1-ord 3 nil m sing))		; un-
              (#\e nu-ch-ordsuff-3-e (setq tempord 3))
              (none stop (n-unit-norm 3)))			; 2
  (nu-ch-tret (#\z nu-ch-tretz))
  (nu-ch-tretz (#\e nu-ch-tretze)			; 11
               (#\è stop (n-unit-ord 13 nil m sing 0 nil)))
  (nu-ch-tretze (#\n nu-ch-ordsuff-2-en (setq tempord 13))
                (none stop (n-basic-dec-unit 13 back)))	; 11
  (nu-ch-tri (#\g nu-ch-ord-ig (setq tempord 30)))
  (nu-ch-u (#\n nu-ch-ordsuff-3 (setq tempord 1))
           (none stop (n-unit-norm 1 back)))		; 1
  (nu-ch-v (#\i nu-ch-vi)
           (#\u nu-ch-vu))
  (nu-ch-vi (#\g nu-ch-ord-ig (setq tempord 20))
            (#\n nu-ch-vin))
  (nu-ch-vin (#\t nu-ch-vint))
  (nu-ch-vint (#\e nu-ch-ordsuff-2-e (setq tempord 20))
              (#\è stop (n-unit-ord 20 nil m sing 0 nil))
              (#\- nu-ch-vint-)
	      (none stop (n-dec 20)))			; 20
  (nu-ch-vint- (#\i stop (n-dec 20))
               (none stop (n-dec 20 back)))
  (nu-ch-vu (#\i nu-ch-vui))
  (nu-ch-vui (#\t nu-ch-vuit))
  (nu-ch-vuit (#\a nu-x+ord-a (setq temp 80 tempord 80))
	      (#\c stop (n-unit-norm 8 back))		; 8
              (#\e nu-ch-ordsuff-2-e (setq tempord 8))
	      (#\m stop (n-unit-norm 8 back))		; 8
              (#\è stop (n-unit-ord 8 nil m sing 0 nil))
	      (#\- stop (n-unit-norm 8 back))		; 8
	      (none stop (n-unit-norm 8)))		; 8

; --------------- suffixes -anta --------------------------------------
  (nu-x (#\a nu-x-a))
  (nu-x-a (#\n nu-x-an))
  (nu-x-an (#\t nu-x-ant))
  (nu-x-ant (#\a stop (n-dec temp)))

  (nu-x+ord (#\a nu-x+ord-a)			; quar-, 
            (#\t nu-ch-ordsuff (setq tempord 4)))
  (nu-x+ord-a (#\n nu-x+ord-an))		; huita-, 
  (nu-x+ord-an (#\t nu-x+ord-ant))
  (nu-x+ord-ant (#\a stop (n-dec temp))
                (#\e nu-ch-ordsuff-2-e)
                (#\è stop (n-unit-ord tempord nil m sing 0 nil)))

; *** cinqu --> cinquè, cinquena, cinquens, cinquenes, cinquanta, cinquantè, cinquantena, ...
  (nu-x+ord-2 (#\è stop (n-unit-ord tempord nil m sing 0 nil))
              (#\e nu-ch-ordsuff-2-e)
              (#\a nu-x+ord-a (setq temp 50 tempord 50)))

; *** nor-, seix- --> noranta, norantè, norantena, norants, norantes
  (nu-x+ord-3 (#\a nu-x+ord-a))			; nora-, seixa- 

; --------------- suffixes ordinal (nil, a, s, es) --------------------------------------
; *** primer --> primer, primera, primers, primeres
  (nu-ch-ordsuff (#\a stop (n-unit-ord tempord nil f sing 1 nil))	; dècim, primer, quint,
                 (#\e nu-ch-ordsuff-e)				; segon, sext, sèptim, tercer
                 (#\s stop (n-unit-ord tempord nil m pl 1 nil))	; vigèsim-, trigèsim, ...
                 (none stop (n-unit-ord tempord back m sing 0 nil)))
  (nu-ch-ordsuff-e (#\s stop (n-unit-ord tempord nil f pl 2 nil)))

; *** vigèsim --> vigèsima, vigèsims, vigèsimes
  (nu-ch-orddec (#\a stop (n-dec-ord tempord nil f sing 1 nil))	; dècim, 
                 (#\e nu-ch-orddec-e)				
                 (#\s stop (n-dec-ord tempord nil m pl 1 nil))	; vigèsim-, trigèsim, ...
                 (none stop (n-dec-ord tempord back m sing 0 nil)))
  (nu-ch-orddec-e (#\s stop (n-dec-ord tempord nil f pl 2 nil)))

; *** cente --> centena, centens, centenes
  (nu-ch-ordcent (#\n nu-ch-ordcent-n))
  (nu-ch-ordcent-n (#\a stop (n-cent-ord tempord nil f sing 2 nil))
                   (#\e nu-ch-ordcent-ne)				
                   (#\s stop (n-cent-ord tempord nil m pl 1 nil)))
  (nu-ch-ordcent-ne (#\s stop (n-cent-ord tempord nil f pl 3 nil)))

; *** centèsim --> centèsim, centèsima, centèsims, centèsimes
  (nu-ch-ordcent-1 (#\a stop (n-cent-ord tempord nil f sing 1 nil))
                   (#\e nu-ch-ordcent-ne-2)				
                   (#\s stop (n-cent-ord tempord nil m pl 1 nil))
                   (none stop (n-cent-ord tempord nil m sing 0 nil)))
  (nu-ch-ordcent-ne-2 (#\s stop (n-cent-ord tempord nil f pl 2 nil)))

; --------------- suffixes ordinal (è, ena, ens, enes) ----------------------------------
  (nu-ch-ordsuff-2 (#\è stop (n-unit-ord tempord nil m sing 0 nil))		; nov-, denov-
                   (#\e nu-ch-ordsuff-2-e))
  (nu-ch-ordsuff-2-e (#\n nu-ch-ordsuff-2-en))			; dese-, dissete-, divuite-,
                                                                ; sete-, sise-, trente-, vinte-
; --------------- suffixes ordinal (nil, a, s, es) --------------------------------------
  (nu-ch-ordsuff-2-en (#\a stop (n-unit-ord tempord nil f sing 3 è))	; catorzen-, onzen-, dotzen-,
                      (#\e nu-ch-ordsuff-2-ene)				; tretzen-, quinzen-, setzen-,
                      (#\s stop (n-unit-ord tempord nil m pl 3 è)))
  (nu-ch-ordsuff-2-ene (#\s stop (n-unit-ord tempord nil f pl 4 è)))

   ; the next are kept apart, since unè, unena, etc. cannot be used alone
  (nu-ch-ordsuff-3 (#\è stop (n-unit-1-ord tempord nil m sing 0 nil))		; un-
                   (#\e nu-ch-ordsuff-3-e))
  (nu-ch-ordsuff-3-e (#\n nu-ch-ordsuff-3-en))
  (nu-ch-ordsuff-3-en (#\a stop (n-unit-1-ord tempord nil f sing 3 è))
                      (#\e nu-ch-ordsuff-3-ene)		
                      (#\s stop (n-unit-1-ord tempord nil m pl 3 è)))
  (nu-ch-ordsuff-3-ene (#\s stop (n-unit-1-ord tempord nil f pl 4 è)))

  (nu-ch-ord-ig (#\è nu-ch-ord-igè))		; nonag-
  (nu-ch-ord-igè (#\s nu-ch-ord-igès))
  (nu-ch-ord-igès (#\i nu-ch-ord-igèsi))
  (nu-ch-ord-igèsi (#\m nu-ch-orddec))
  ))

