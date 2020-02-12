(in-package "USER")

; *********************************************************
;   AUTOMATA FOR RECOGNIZING NUMBERS EXPRESSED IN WORDS
;   *** the procedures are in PROC/MORPHO/parsenumbers.lisp
; *********************************************************

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

(defun multval () 
  (declare (special value curval))
  (setq value (* value curval)))

(load-numb-arcs
'((nu-0 nil
   (n-unit-norm 	nu-2 	(incrval))   ; *** standard number (one. two, ... nine)
   (n-dec 		nu-3 	(incrval))   ; *** tenth (twenty, thirty, ... ninety)
   (n-basic-dec-unit 	nu-4 	(incrval))   ; *** ten, eleven, twelve, thirteen, ... nineteen
   (n-hundred 		nu-5 	(incrval))   ; *** hundred
   (n-thousand 		nu-6 	(incrval))   ; *** thousand
   (n-unit-ord  	stop 	(incrval+ord))  ; *** first, second, third
   (n-dec-ord 	 	stop 	(incrval+ord))  ; *** twentieth, thirtieth, ...
   (n-basic-dec-unit-ord  stop 	(incrval+ord)))  ; *** tenth, eleventh, ..., nineteenth
  (nu-2 (final)				; after normal unit
   (n-hundred 		nu-5 	(multval))
   (n-thousand 		nu-6 	(multval)))
  (nu-3 (final)				; after a multiple of ten (but not ten)
   (n-unit-norm 	nu-4 	(incrval))
   (n-unit-ord	 	stop 	(incrval+ord))
   (n-thousand 		nu-6 	(multval)))
  (nu-4 (final)				; after a number from ten to nineteen
   (n-hundred 		nu-5 	(multval))
   (n-thousand 		nu-6 	(multval)))
  (nu-5 (final)				; after unit+hundred 
   (n-unit-norm 	nu-7 	(incrval))
   (n-dec 		nu-8 	(incrval))
   (n-basic-dec-unit 	nu-7 	(incrval))
   (n-thousand 		nu-6 	(multval))
   (n-unit-ord  	stop 	(incrval+ord))  ; *** first, second, third
   (n-dec-ord 	 	stop 	(incrval+ord))  ; *** twentieth, thirtieth, ...
   (n-basic-dec-unit-ord  stop 	(incrval+ord)))  ; *** tenth, eleventh, ..., nineteenth
  (nu-6 (final)				; after something+thousand
   (n-unit-norm 	nu-9 	(setq tempval curval))
   (n-dec 		nu-10 	(setq tempval curval))
   (n-basic-dec-unit 	nu-11 	(setq tempval curval))
   (n-hundred 		nu-12 	(setq tempval curval))
   (n-unit-ord  	stop 	(incrval+ord))  ; *** first, second, third
   (n-dec-ord 	 	stop 	(incrval+ord))  ; *** twentieth, thirtieth, ...
   (n-basic-dec-unit-ord  stop 	(incrval+ord)))  ; *** tenth, eleventh, ..., nineteenth
  (nu-7 (final)				; after unit+hundred+unit-thousand
   (n-thousand 		nu-6 	(multval))
   (n-unit-ord  	stop 	(incrval+ord))  ; *** first, second, third
   (n-dec-ord 	 	stop 	(incrval+ord))  ; *** twentieth, thirtieth, ...
   (n-basic-dec-unit-ord  stop 	(incrval+ord)))  ; *** tenth, eleventh, ..., nineteenth
  (nu-8 (final)				; after unit+hundred+unit-thousand
   (n-unit-norm 	nu-7 	(incrval))
   (n-unit-ord	 	stop 	(incrval+ord))
   (n-thousand 		nu-6 	(multval)))
  (nu-9 (final (+ value tempval))
   (n-hundred 		nu-12 	(setq tempval (* tempval curval))))
  (nu-10 (final (+ value tempval))
   (n-unit-norm		nu-11 	(setq tempval (+ tempval curval)))
   (n-unit-ord	 	stop 	(progn
                                    (setq curval (+ tempval curval))
                                    (incrval+ord))))
  (nu-11 (final (+ value tempval)))
  (nu-12 (final (+ value tempval))
   (n-unit-norm 	stop 	(setq value (+ (+ value tempval) curval)))
   (n-dec 		nu-13 	(setq tempval (+ tempval curval)))
   (n-basic-dec-unit 	stop 	(setq value (+ (+ value tempval) curval)))
   (n-unit-ord  	stop 	(progn
                      	            (setq value (+ value tempval))
                                    (incrval+ord)))
   (n-dec-ord 	 	stop 	(progn
   				    (setq tempval (+ tempval curval))
                                    (incrval+ord)))
   (n-basic-dec-unit-ord  stop 	(progn
                      	            (setq value (+ value tempval))
                                    (incrval+ord))))
  (nu-13 (final (+ value tempval))
   (n-unit-norm		stop 	(setq value (+ (+ value tempval) curval)))
   (n-unit-ord  	stop 	(progn
                      	            (setq value (+ value tempval))
                                    (incrval+ord))))))

; ***************************************************************
;   the following automaton aims at recognizing the elementary tokens,
;   that are used and put together by the preceding automaton
; *** This automaton outputs one of the following categories:
;   - n-unit-norm. Standard units from one to nine
;   - n-dec: tenth from twenty to ninety
;   - n-basic-dec-unit: ten eleven twelve thirteen fourteen fifteen sixteen
;     seventeen eighteen nineteen
;   - n-hundred: hundred
;   - n-thousand: thousand

(defun load-nch-arcs (autom)
   (mapc #'(lambda (x) (putprop (first x) (rest x) 'nu-ch-arcs)) autom))

(load-nch-arcs
 '((nu-ch-0 (#\e nu-ch-e)			; eight, eleven, eighteen
	   (#\f nu-ch-f)			; four, five, fifteen, forty, fifty, first
	   (#\h nu-ch-h)			; hundred
	   (#\n nu-ch-n)			; nine
	   (#\o nu-ch-o)			; one
	   (#\s nu-ch-s)			; six, seven, second
	   (#\t nu-ch-t))			; two, three, ten, twelve, thirteen,
                                                ; twenty, thirty, third, th
  (nu-ch-e (#\i nu-ch-ei)			; eight, eighteen
	   (#\l nu-ch-el))			; eleven
  (nu-ch-ei (#\g nu-ch-eig))			; eight, eighteen
  (nu-ch-eig (#\h nu-ch-eigh))			; eight, eighteen
  (nu-ch-eigh (#\t nu-ch-eight))		; eight, eighteen
  (nu-ch-eight (#\e nu-x-te (setq temp 8))	; 18
	       (#\h nu-ch-eighth)
	       (#\t stop (n-unit-norm 8 back))	; 8
	       (#\y stop (n-dec 80))		; 80
               (#\i nu-x-ti (setq temp 80))	; eightieth
               (none stop (n-unit-norm 8)))		; eight
  (nu-ch-eighth (#\u stop (n-unit-norm 8 2-back))	; eight-hundred
                (none stop (n-unit-ord 8)))		; eighth
  (nu-ch-el (#\e nu-ch-ele))			; eleven
  (nu-ch-ele (#\v nu-ch-elev))			; eleven
  (nu-ch-elev (#\e nu-ch-eleve))		; eleven
  (nu-ch-eleve (#\n nu-ch-eleven))		 ; eleven
  (nu-ch-eleven (#\t nu-ord2-t (setq temp 1))   ; eleventh, eleventhousand
	        (#\h stop (n-unit-norm 11 back)); elevenhundred
                (none stop (n-unit-norm 11)))	; eleven
  (nu-ch-f (#\o nu-ch-fo)			; four, fourteen, forty
	   (#\i nu-ch-fi))			; five, fifteen, fifty, first
  (nu-ch-fo (#\u nu-ch-fou)			; four, fourteen
	    (#\r nu-ch-for))			; forty
  (nu-ch-fou (#\r nu-ch-four))			; four, fourteen
  (nu-ch-four (#\h stop (n-unit-norm 4 back))   ; fourhundred
              (#\t nu-x-t (setq temp 4)) 	; fourthousand, fourteen, fourth
              (none stop (n-unit-norm 4)))	; four
  (nu-ch-for (#\t nu-ch-fort))			; forty
  (nu-ch-fort (#\y stop (n-dec 40))		; forty
              (#\i nu-x-ti (setq temp 40)))	; fortieth
  (nu-ch-fi (#\v nu-ch-fiv)			; five
            (#\r nu-ch-fir)			; first
	    (#\f nu-ch-fif (setq temp 5)))	; fifty, fifteen
  (nu-ch-fiv (#\e stop (n-unit-norm 5)))	; five
  (nu-ch-fir (#\s nu-ch-firs))			; first
  (nu-ch-fif (#\t nu-ch-fift))			; fifty, fifteen, fifth
  (nu-ch-fift (#\e nu-x-te)			; thirte-en, fifte-en
              (#\h stop (n-unit-ord temp))	; fifth
              (#\y stop (n-dec (* temp 10)))	; fifty
              (#\i nu-x-ti (setq temp 50)))	; fiftieth
  (nu-ch-firs (#\t stop (n-unit-ord 1)))	; first
  (nu-ch-h (#\u nu-ch-hu))			; hundred
  (nu-ch-hu (#\n nu-ch-hun))			; hundred
  (nu-ch-hun (#\d nu-ch-hund))			; hundred
  (nu-ch-hund (#\r nu-ch-hundr))		; hundred
  (nu-ch-hundr (#\e nu-ch-hundre))		; hundred
  (nu-ch-hundre (#\d stop (n-hundred 100)))	; hundred
  (nu-ch-n (#\i nu-ch-ni))			; nine
  (nu-ch-ni (#\n nu-ch-nin))			; nine
  (nu-ch-nin (#\e nu-ch-nine)			; nine
             (#\t nu-ch-nint (setq temp 9)))    ; ninth
  (nu-ch-nint (#\h stop (n-unit-ord 9)))	; nine
  (nu-ch-nine (#\h stop (n-unit-norm 9 back))   ; ninehundred
              (#\t nu-ch-ninet (setq temp 9))   ; nineteen, ninety, ninethousand
              (none stop (n-unit-norm 9)))	; nine
  (nu-ch-ninet (#\h stop (n-unit-norm 9 2-back))  ; ninethousand
               (#\e nu-x-te)			; ninete-en
               (#\y stop (n-dec 90))		; ninety
               (#\i nu-x-ti (setq temp 90)))	; ninetieth
  (nu-ch-o (#\n nu-ch-on))			; one
  (nu-ch-on (#\e stop (n-unit-norm 1)))   	; one
  (nu-ch-s (#\i nu-ch-si)			; six
           (#\e nu-ch-se))			; seven, second
  (nu-ch-si (#\x nu-ch-six))			; six
  (nu-ch-six (#\h stop (n-unit-norm 6 back))	; sixhundred
             (#\t nu-x-t (setq temp 6))		; sixteen, sixty, sixthousand, sixth
             (none stop (n-unit-norm 6)))	; six
  (nu-ch-se (#\v nu-ch-sev)			; seven
            (#\c nu-ch-sec))			; second
  (nu-ch-sev (#\e nu-ch-seve))			; seven
  (nu-ch-seve (#\n nu-ch-seven))		; seven
  (nu-ch-seven (#\h stop (n-unit-norm 7 back))	; sevenhundred
               (#\t nu-x-t (setq temp 7))	; seventeen, seventy, seventhousand, seventh
               (none stop (n-unit-norm 7)))	; seven
  (nu-ch-sec (#\o nu-ch-seco))			; second
  (nu-ch-seco (#\n nu-ch-secon))		; second
  (nu-ch-secon (#\d stop (n-unit-ord 2)))	; second
  (nu-ch-t (#\e nu-ch-te)			; ten
	   (#\h nu-ch-th)			; three, thirteen, thirty, third
	   (#\w nu-ch-tw))			; two, twelve, twenty
  (nu-ch-te (#\n nu-ch-ten))			; ten
  (nu-ch-ten (#\t nu-ord2-t (setq temp 0))      ; tenth, tenthousand
             (none stop (n-unit-norm 10)))	; ten
  (nu-ch-th (#\i nu-ch-thi)			; thirty, thirteen, third
            (#\o nu-ch-tho)			; thousand
            (#\r nu-ch-thr))			; three
  (nu-ch-thi (#\r nu-ch-thir))			; thirty, thirteen, third
  (nu-ch-thir (#\t nu-ch-thirt (setq temp 3))	; thirty, thirteen
              (#\d stop (n-unit-ord 3)))	; third
  (nu-ch-thirt (#\e nu-x-te)			; thirte-en, fifte-en
               (#\y stop (n-dec (* temp 10)))	; thirty, fifty
               (#\i nu-x-ti (setq temp 30)))	; thirtieth
  (nu-ch-tho (#\u nu-ch-thou))			; thousand
  (nu-ch-thou (#\s nu-ch-thous))		; thousand
  (nu-ch-thous (#\a nu-ch-thousa))		; thousand
  (nu-ch-thousa (#\n nu-ch-thousan))		; thousand
  (nu-ch-thousan (#\d stop (n-thousand 1000)))	; thousand
  (nu-ch-thr (#\e nu-ch-thre))			; thirty, thirteen
  (nu-ch-thre (#\e stop (n-unit-norm 3)))	; three
  (nu-ch-tw (#\e nu-ch-twe)			; thirty, thirteen
            (#\o stop (n-unit-norm 2)))		; twelve, twenty
  (nu-ch-twe (#\l nu-ch-twel)			; twelve
             (#\n nu-ch-twen))			; twenty
  (nu-ch-twel (#\v nu-ch-twelv)			; twelve
              (#\f nu-ch-twelf))		; twelfth
  (nu-ch-twelv (#\e stop (n-basic-dec-unit 12))) ; twelve
  (nu-ch-twelf (#\t nu-ch-twelft))		; twelfth
  (nu-ch-twelft (#\h stop (n-unit-ord 12)))	; twelfth
  (nu-ch-twen (#\t nu-ch-twent))		; twenty
  (nu-ch-twent (#\y stop (n-dec 20))		; twenty
               (#\i nu-x-ti (setq temp 20)))	; twenttieth
; --------------- suffixes -teen, -ty --------------------------------
  (nu-x-t (#\e nu-x-te)				; xxxte-en
          (#\h nu-x-th)				; xxxthousand, xxxth
          (#\i nu-x-ti (setq temp (* temp 10)))	; xxxtieth
          (#\y stop (n-dec (* temp 10))))	; xxxty
			; after "fif" and "thir": no fifthousand or thirthousand
  (nu-x-th (#\o stop (n-unit-norm temp 3-back))	; xxxtho-usand, xxxth
           (none stop (n-unit-ord temp)))	; XXXth
  (nu-x-te (#\e nu-x-tee))			; xxxtee-n
  (nu-x-tee (#\n nu-x-teen))			; xxxteen
  (nu-x-teen (#\h stop (n-basic-dec-unit 
                          (+ temp 10) back))	; xxxteen-hundred
             (#\t nu-ord2-t)			; xxxteen-thousand, xxxteen-th
             (none stop (n-basic-dec-unit (+ temp 10)))) ; xxxteen
  (nu-x-ti (#\e nu-x-tie))			; xxxtie-th
  (nu-x-tie (#\t nu-x-tiet))			; xxxtiet-h
  (nu-x-tiet (#\h stop (n-dec-ord temp)))	; xxxtieth
  (nu-ord-t (#\h nu-ord-th))			; th or th-ousand
  (nu-ord-th (#\o stop (n-unit-norm temp 3-back)) ; xxxtho-usand
             (none stop (n-unit-ord temp)))	; XXXth
  (nu-ord2-t (#\h nu-ord2-th))			; th or th-ousand
  (nu-ord2-th (#\o stop (n-basic-dec-unit (+ temp 10) 3-back))	; xxxteen-hundred
              (none stop (n-basic-dec-unit-ord (+ temp 10))))	; xxxteen
))

