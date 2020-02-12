(in-package "USER")

(defun load-numb-arcs (autom)
   (mapc #'(lambda (x) 
		(putprop (first x) (rest (rest x)) 'numb-arcs)
		(putprop (first x) (second x) 'final-info))
	      autom))

(defun incrval ()
  (declare (special value curval))
  (setq value (+ value curval)))

(defun multval () 
  (declare (special value curval))
  (setq value (* value curval)))

one		eka             pahalA
two             xo              xUsarA
three           wIna            wIsarA
four            cAra            cOWA
five            pAzca           pAcavAz
six             CaH             CaTA
seven           sAwa            sAwavAz
eight           ATa             eighth
nine            nO              navAz
ten             xasa            xasavAz
eleven          gyAraha         gyArahavAz
twelve          bAraha          bArahavAz
thirteen        werahavAz       werahavAz
fourteen        cOxaha          cOxahavAz
fifteen         panxraha        panxrahavAz
sixteen         solaha          solahavAz
seventeen       sawwaraha/sawraha       sawwarahavAz/sawrahavAz
eighteen        aTTAraha        aTTArahavAz
nineteen        unnIsa          unnIsavAz

         twenty		thirty       forty	   fifty	sixty         seventy	      eighty	   ninety
------------------------------------------------------------------------------------------------------------------
-        b+Isa          w+Isa        c+Al+Isa      p+ac+Asa     s+ATa         s+awwara        assI	 nav+ve
one      ik+k+Isa	ik+aw+w+Isa  ik+aw+Al+Isa  ik+y+Av+ana  ik+a+s+aTa    ik+a+h+awwara   ik+y+AsI   ik+Anave
two      bA+isa		ba+w+w+Isa   ba+y+Al+Isa   bA+v+ana     bA+s+aTa      ba+h+Awwara     ba+AsI     ba+y+Anave
three    we+isa         weM+w+Isa    wEMw+Al+Isa   wre+p+ana    wre+s+aTA     wi+h+awwara     wi+r+AsI   wi+r+Anave
four     cO+b+Isa	cO+M+w+Isa   ca+v+Al+Isa   cO+vv+ana	cO+M+s+aTa    cO+h+awwara     cO+r+AsI   cO+r+Anave
five     pacc+Isa	pEM+w+Isa    pEMw+Al+Isa   paca+p+ana   pEM+s+aTa     pica+h+awwara   pic+y+AsI  pic+y+Anave
six      Cabb+Isa	Caw+w+Isa    Cay+Al+Isa    Capp+ana     CayA+s+aTa    Ci+h+awwara     Ci+y+AsI   Ci+y+Anave
seven    sawwA+isa	sEM+w+Isaa   sEMw+Al+Isa   saww+Av+ana  sadZa+s+aTa   sawa+h+awwara   saw+w+AsI  saw+Anave
eight    aTTA+isa	adZa+w+Isa   adZaw+Al+Isa  aTT+Av+ana   adZa+s+aTa    aTa+h+awwaraa   aTT+AsI    aT+Anave
nine     un+w+Isa	un+w+Al+Isa  un+ac+Asa	   un+a+saTa    un+ah+awwara  un+n+AsI        nav+AsI    niny+Anave

; *******************************************************************
; *** the next is the top-level automaton; the label of the arcs are
;     associated with the components recognized by the lower automaton
; *** Two global variables are used:
;     - Value, which holds the total value obtained until now
;     - curval, which is the value associated with the last component
;       recognized by the lower automaton
; *** the arc labels are:
;     n-uno
(load-numb-arcs
'((nu-0 nil
   (n-uno 		nu-1 	(incrval))
   (n-otto 		nu-2 	(incrval))
   (n-unit-norm 	nu-2 	(incrval))
   (n-dec 		nu-3 	(incrval))
   (n-basic-dec-unit 	nu-4 	(incrval))
   (n-cento 		nu-5 	(incrval))
   (n-mille 		nu-6 	(incrval))
   (elision 		nu-1 	(incrval)))
  (nu-1 (final))
  (nu-2 (final)
   (n-cento 		nu-5 	(multval))
   (n-mila 		nu-6 	(multval)))
  (nu-3 (final)
   (n-unit-norm 	nu-4 	(incrval))
   (n-mila 		nu-6 	(multval)))
  (nu-4 (final)
   (n-mila 		nu-6 	(multval)))
  (nu-5 (final)
   (n-uno 		nu-7 	(incrval))
   (n-otto 		nu-7 	(incrval))
   (n-unit-norm 	nu-7 	(incrval))
   (n-dec 		nu-8 	(incrval))
   (n-basic-dec-unit 	nu-7 	(incrval))
   (n-mila 		nu-6 	(multval)))
  (nu-6 (final)
   (n-uno 		nu-1 	(incrval))
   (n-otto 		nu-9 	(setq tempval curval))
   (n-unit-norm 	nu-9 	(setq tempval curval))
   (n-dec 		nu-10 	(setq tempval curval))
   (n-basic-dec-unit 	nu-11 	(setq tempval curval))
   (n-cento 		nu-12 	(setq tempval curval)))
  (nu-7 (final)
   (n-mila 		nu-6 	(multval)))
  (nu-8 (final)
   (n-unit-norm 	nu-7 	(incrval))
   (n-mila 		nu-6 	(multval)))
  (nu-9 (final (+ value tempval))
   (n-cento 		nu-12 	(setq tempval (* tempval curval))))
  (nu-10 (final (+ value tempval))
   (n-unit-norm		nu-11 	(setq tempval (+ tempval curval))))
  (nu-11 (final (+ value tempval)))
  (nu-12 (final (+ value tempval))
   (n-uno 		nu-1 	(setq value (+ (+ value tempval) curval)))
   (n-otto 		nu-1 	(setq value (+ (+ value tempval) curval)))
   (n-unit-norm 	nu-1 	(setq value (+ (+ value tempval) curval)))
   (n-dec 		nu-13 	(setq tempval (+ tempval curval)))
   (n-basic-dec-unit 	nu-1 	(setq value (+ (+ value tempval) curval))))
  (nu-13 (final (+ value tempval))
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
	   (#\m nu-ch-m)
	   (#\n nu-ch-n)
	   (#\o nu-ch-o)
	   (#\q nu-ch-q)
	   (#\s nu-ch-s)
	   (#\t nu-ch-t)
	   (#\u nu-ch-u)
	   (#\v nu-ch-v))
  (nu-ch-c (#\i nu-ch-ci)
	   (#\e nu-ch-ce))
  (nu-ch-ce (#\n nu-ch-cen))
  (nu-ch-cen (#\t nu-ch-cent))
  (nu-ch-cent (#\o stop (n-cento 100)) 			; 100
              (#\' stop ((elision #\o) 100)))		; 100
  (nu-ch-ci (#\n nu-ch-cin))
  (nu-ch-cin (#\q nu-ch-cinq))
  (nu-ch-cinq (#\u nu-ch-cinqu))
  (nu-ch-cinqu (#\e stop (n-unit-norm 5))		; 5
               (#\a nu-x-a (setq temp 50)))
  (nu-ch-d (#\i nu-ch-di)
	   (#\o nu-y (setq temp 12))			; 12
	   (#\u nu-ch-du))
  (nu-ch-di (#\c nu-ch-dic)
	    (#\e nu-ch-die))
  (nu-ch-dic (#\i nu-ch-dici))
  (nu-ch-dici (#\a nu-ch-dicia)
	      (#\o nu-ch-dicio))
  (nu-ch-dicia (#\n nu-ch-dician)
	       (#\s nu-ch-dicias))
  (nu-ch-dician (#\n nu-ch-diciann))
  (nu-ch-diciann (#\o nu-ch-dicianno))
  (nu-ch-dicianno (#\v nu-ch-diciannov))
  (nu-ch-diciannov (#\e stop (n-basic-dec-unit 19)))	; 19
  (nu-ch-dicias (#\s nu-ch-diciass))
  (nu-ch-diciass (#\e nu-ch-diciasse))
  (nu-ch-diciasse (#\t nu-ch-diciasset))
  (nu-ch-diciasset (#\t nu-ch-diciassett))
  (nu-ch-diciassett (#\e stop (n-basic-dec-unit 17)))	; 17
  (nu-ch-dicio (#\t nu-ch-diciot))
  (nu-ch-diciot (#\t nu-ch-diciott))
  (nu-ch-diciott (#\o stop (n-basic-dec-unit 18)) 	; 18
                 (#\' stop ((elision #\o) 18)))		; 18
  (nu-ch-die (#\c nu-ch-diec))
  (nu-ch-diec (#\i stop (n-basic-dec-unit 10)))		; 10
  (nu-ch-du (#\e stop (n-unit-norm 2)))			; 2
  (nu-ch-m (#\i nu-ch-mi))
  (nu-ch-mi (#\l nu-ch-mil))
  (nu-ch-mil (#\a stop (n-mila 1000))			; 1000
	     (#\l nu-ch-mill))
  (nu-ch-mill (#\e stop (n-mille 1000)) 		; 1000
              (#\' stop ((elision #\e) 1000)))		; 1000
  (nu-ch-n (#\o nu-ch-no))
  (nu-ch-no (#\v nu-ch-nov))
  (nu-ch-nov (#\e stop (n-unit-norm 9))			; 9
             (#\a nu-x-a (setq temp 90)))		; 90
  (nu-ch-o (#\t nu-ch-ot))
  (nu-ch-ot (#\t nu-ch-ott))
  (nu-ch-ott (#\o stop (n-otto 8))			; 8
             (#\a nu-x-a (setq temp 80)))		; 80
  (nu-ch-q (#\u nu-ch-qu))
  (nu-ch-qu (#\a nu-ch-qua)
	    (#\i nu-ch-qui))
  (nu-ch-qua (#\r nu-ch-quar)
	     (#\t nu-ch-quat))
  (nu-ch-quar (#\a nu-x-a (setq temp 40)))		; 40
  (nu-ch-quat (#\t nu-ch-quatt))
  (nu-ch-quatt (#\o nu-ch-quatto)
	       (#\r nu-ch-quattr))
  (nu-ch-quatto (#\r nu-y (setq temp 14)))		; 14
  (nu-ch-quattr (#\o stop (n-unit-norm 4)))		; 4
  (nu-ch-qui (#\n nu-y (setq temp 15)))	 		; 15
  (nu-ch-s (#\e nu-ch-se))
  (nu-ch-se (#\d nu-y-d (setq temp 16))			; 16
	    (#\i stop (n-unit-norm 6))			; 6
	    (#\s nu-ch-ses)
	    (#\t nu-ch-set))
  (nu-ch-ses (#\s nu-ch-sess))
  (nu-ch-sess (#\a nu-x-a (setq temp 60)))		; 60
  (nu-ch-set (#\t nu-ch-sett))
  (nu-ch-sett (#\e stop (n-unit-norm 7))		; 7
	      (#\a nu-x-a (setq temp 70)))		; 70
  (nu-ch-t (#\r nu-ch-tr))
  (nu-ch-tr (#\e nu-ch-tre)) 			; !!! tre e trenta !!!
; *** il caso di "tre" e' speciale, perche' la stringa puo' riferirsi
;     effettivamente al numero 3 (tre, tre-cento, tre-mila) o puo'
;     continuare in una sottostringa da gestire in questo automa (trenta,
;     trentuno, tredici).
; *** L'idea e' di andare avanti ancora di un carattere, in modo da
;     decidere. Nel caso che il carattere che segue sia d o n, si
;     procede normalmente. Se e' c (cento) o m (mila) ci si ferma con una
;     segnalazione speciale che fa rimettere il carattere nel "remainder"
;     della stringa. In caso contrario, non e' un numero.
  (nu-ch-tre (#\d nu-y-d (setq temp 13))		; 13
	     (#\n nu-x-an (setq temp 30))		; 30
	     (#\c stop (n-unit-norm 3 back))		; 3
	     (#\m stop (n-unit-norm 3 back))		; 3
	     (none stop (n-unit-norm 3)))		; 3
  (nu-ch-u (#\n nu-ch-un))
  (nu-ch-un (#\o stop (n-uno 1))			; 1
	    (#\d nu-y-d (setq temp 11)))		; 13
  (nu-ch-v (#\e nu-ch-ve))
  (nu-ch-ve (#\n nu-ch-ven))
  (nu-ch-ven (#\t nu-ch-vent))
  (nu-ch-vent (#\i stop (n-dec 20))			; 20
              (#\' stop ((elision #\i) 20))			; 20
	      (#\o nu-x-anto (setq temp 20))		; 20
	      (#\u nu-x-antu (setq temp 20)))		; 20
; --------------- suffissi -anta, -antuno, -antotto -------------------
  (nu-x-a (#\n nu-x-an))
  (nu-x-an (#\t nu-x-ant))
  (nu-x-ant (#\a stop (n-dec temp))
            (#\' stop ((elision #\a) temp))
	    (#\o nu-x-anto)
	    (#\u nu-x-antu))
  (nu-x-anto (#\t nu-x-antot))
  (nu-x-antot (#\t nu-x-antott))
  (nu-x-antott (#\o stop (n-basic-dec-unit (+ temp 8)))
               (#\' stop ((elision #\o) (+ temp 8))))
  (nu-x-antu (#\n nu-x-antun))
  (nu-x-antun (#\o stop (n-basic-dec-unit (+ temp 1)))
              (#\' stop ((elision #\o) (+ temp 1))))
; --------------- suffisso -dici --------------------------------------
  (nu-y (#\d nu-y-d))
  (nu-y-d (#\i nu-y-di))
  (nu-y-di (#\c nu-y-dic))
  (nu-y-dic (#\i stop (n-basic-dec-unit temp)))))
