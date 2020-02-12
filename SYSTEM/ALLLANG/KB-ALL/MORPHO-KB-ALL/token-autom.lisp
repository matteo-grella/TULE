(in-package "USER")

;******************************************************************
;    N.B. At the end of this file, a table of character codes is reported
;
;******************************************************************
; *** The automaton in this file is the manual compilation of a regular
;     grammar reported at the end of the file
;
; *** The structure of each state decription is as follows:
;     state --> (state-name transition*)
;     transition --> (arc-label next-state ?output)
;     output --> (scan-label move ?overwriting)
;     scan-label --> gw | nomep | sigla | segnointer | number | date | year |
;		     www | special
;
; *** next-state is just the name of a state
; *** scan-label is the name of a scan category
; *** move specifies how many characters must be left on the accumulator
;     after the output (see below)
; *** overwriting possibly specifies that the actual output must be different
;     from the input character(s)
;
; ************
; *** When an input character is read, it is appended at the end of an
;     accumulator (word1) and the proper transition is executed. Whenever
;     an output is required, the contents of the accumulator (possibly
;     excluding a number of characters at the end, as specified in "move")
;     are copied in appropriate format in the output file and are cancelled
;     from word1.
; *** When "overwrite" is present, word1 is emptied as above, but the 
;     output elements are the ones contained in "overwrite"
;
; ************
; *** The structure of the output is as follows:
;     output --> scan-interp | (scan-interp1 ... scan-interpn)
;     scan-interp --> (scan-elem1 ... scan-elemk)
;     scan-elem --> ((ascii-code1 ... ascii-codem) scanner-category)
; *** N.B. When the output is not ambiguous, one level of parentheses is
;          missing. This is bad, but reflects the old version.
; *** ex. (not ambiguous):
;          asino: (((97 115 105 110 111) GW))
;	  (ambiguous):
;          Asino: ((((97 115 105 110 111) GW)) (((65 115 105 110 111) NOMEP)))

(defun load-tok-arcs (autom)
   (mapc #'(lambda (x) (putprop (first x) (rest x) 'tok-arcs)) autom))
  
; ********************************** S-0 **************************
; S-0 is the initial state; it also corresponds to the state after a
; scan element has been successfully completed, so that the automaton
; must start the analysis of the next one
(load-tok-arcs
'((s-0
   (neutral s-0 ((flush 0)))
   (minlet s-gw-0)			; gw: general word
   (caplet s-gwp-0)			; gwp: general word or nomep
   (period s-dot-0)			; something beginning with a period
; *** the next distinction among numbers to enable the check that the day
;     in a date must be between 01 and 31
   (zero s-n-0-a)			; numbers starting with 0
   (one s-n-0-b)			; numbers starting with 1
   (two s-n-0-c)			; numbers starting with 2
   (three s-n-0-d)			; numbers starting with 3
   (four-to-nine s-n-0-e)		; numbers starting with 4-9
   (less-than s-min-0)
   (greater-than s-mag-0)
   (open-double-angle s-0 ((segnointer 0)))
   (closed-double-angle s-0 ((segnointer 0)))
   (grave-accent s-ac-0)			; only ``
   (apostrophe s-ap-0)				; '' or 'year
   (s-term-not-period s-eos ((segnointer 0)))	; eos: end of sentence
   (inv-question-mark s-0 ((segnointer 0)))	; ¿
   (hyphen s-hyp-0)				; -
   (w-termin s-0 ((segnointer 0)))		; " ( ) , < > [ ] « »
   (escape s-eof)				; eof: end of file
   (else s-0 ((special 0))))
; *** If two or more consecutive - then go-ahead; otherwise output the sequence of -
;     The second line for accounting for arrows (--->)
(s-hyp-0
   (hyphen s-hyp-0)
   (greater-than s-0 ((segnointer $ greater-than))) 
   (else s-0 ! ((segnointer 1))))
; *** If two consecutive < then output <<; otherwise output <
(s-min-0
   (less-than s-0 ((segnointer 0)))
   (else s-0 ! ((segnointer 1))))
; *** If two consecutive > then output >>; otherwise output >
(s-mag-0
   (greater-than s-0 ((segnointer 0)))
   (else s-0 ! ((segnointer 1))))
(s-dot-0			; *** after a period ***
   (neutral s-eos ((segnointer 1)))
   (minlet s-si-0)			; si: sigla
   (caplet s-gwp-0 ((segnointer 1)))	; new word; blank missing after .
   (zero s-n-0-a)			; numbers starting with 0
   (one s-n-0-b)			; numbers starting with 1
   (two s-n-0-c)			; numbers starting with 2
   (three s-n-0-d)			; numbers starting with 3
   (four-to-nine s-n-0-e)		; numbers starting with 4-9
   (period s-dot-1)			; dots
   (else s-0 ! ((segnointer 1))))	; period
(s-dot-1			; *** after two periods ***
   (minlet s-si-0)
   (caplet s-gwp-0 ((segnointer 1)))	; new word; blank missing after ..
   (zero s-n-0-a  ((segnointer 1)))	; numbers starting with 0
   (one s-n-0-b ((segnointer 1)))	; numbers starting with 1
   (two s-n-0-c ((segnointer 1)))	; numbers starting with 2
   (three s-n-0-d ((segnointer 1)))	; numbers starting with 3
   (four-to-nine s-n-0-e ((segnointer 1)))	; numbers starting with 4-9
   (period s-dot-2)
   (else s-0 ! ((segnointer 1))))	; dots
(s-dot-2
   (period s-dot-2)
   (else s-0 ! ((segnointer 1))))
(s-ac-0 				; *** after an accent (initial)
   (grave-accent s-0 ((segnointer 0)))	;     double accent ``
   (digit s-y-0)			;     year `97
   (else s-0 ! ((segnointer 1))))
(s-ac-1 				; *** after an accent (occurring after
					;     some alphanumeric characters)
   (grave-accent s-0 ((gw 2) (segnointer 0)))	;     double accent ``
   (else s-0 ! ((gw 1))))		;     word with accent
(s-ap-0 				; *** after an apostrophe (initial)
   (apostrophe s-0 ((segnointer 0)))	;     double apostrophe ''
   (digit s-y-0)			;     year '97
   (else s-0 ! ((segnointer 1))))
(s-ap-1 				; *** after an apostrophe (occurring after
					;     some alphanumeric characters)
   (apostrophe s-0 ((gw 2) (segnointer 0)))	;     double apostrophe ''
   (di-em-er-es-v s-ap-2)			; *** s --> genitive? r --> you're, we're? m: I'm
   (down-t s-ap-5)				; *** t --> Don't 
   (digit s-y-0 ((gw 2 + (apostrophe))))	;     year '97
		; the apostrophe is doubled (in output here, but also left in
		; the buffer), for cases as "nell'87"
   (else s-0 ! ((gw 1))			; word with apostrophe
	       ((gw 2) (segnointer 1))))
					;     or normal word plus ', as in 'quote'
(s-ap-2 				; *** after an apostrophe (occurring after
					;     some alphanumeric characters) and an s, an r, or an m:
					;     "John's"
   (w-termin s-0 
        (language english 
           (gw 3) (gw 1) (segnointer 0))   ; in English "boy's;"
                                        ;     considered as a gw, followed by a 
                                        ;     another gw (the genitive 's),
                                        ;     followed by another punctuation (the
                                        ;     w-termin completing the sequence)
        (language (italian catalan spanish) 
           (nomep 1) (segnointer 0)))   ; in Italian "boy's" considered as
					;     a full proper name
   (s-termin s-eos 
        (language english 
           (gw 3) (gw 1) (segnointer 0))   ; see comments above
        (language (italian catalan spanish) 
           (nomep 1) (segnointer 0)))
   (neutral s-0 
        (language english 
           (gw 3) (gw 1) (flush 0))   ; see comments above
        (language (italian catalan spanish) 
           (nomep 1) (flush 0)))
   (minlet s-gw-1 
        (language english (gw 3))	; the chars preceding the apostrophe are output:
        (language (italian catalan spanish) 
           (gw 2)))			; the chars preceding and including the apostrophe are output:
   (else s-gw-0 ! 		;     word with apostrophe
        (language english (gw 3))	; the chars preceding the apostrophe are output:
        (language (italian catalan spanish) 
           (gw 2))))			; the chars preceding and including the apostrophe are output:
(s-ap-3 				; *** after an apostrophe (occurring after
					;     some alphanumeric characters, with
					;     an initial caplet)
   (apostrophe s-0 ((gw 2) (segnointer 0))	;     double apostrophe ''
                   ((nomep 2) (segnointer 0)))
   (di-em-er-es-v s-ap-4)			; *** s --> genitive? r --> You're, We're? m: I'm; v --> I've
   (down-t s-ap-5)				; *** t --> Don't 
                                        ;     this is sent in s-ap-5, since the
                                        ;     initial capital is ignored
   (digit s-y-0 ((gw 2 + (apostrophe))))	;     year '97
		; the apostrophe is doubled (in output here, but also left in
		; the buffer), for cases as "nell'87"
   (else s-0 ! ((gw 1))
	       ((gw 2) (segnointer 1))
	       ((nomep 1))		; word with apostrophe
	       ((nomep 2) (segnointer 1))))
(s-ap-4 				; *** after an apostrophe (occurring after
					;     some alphanumeric characters with an
                                        ;     initial capital) and an s: "John's"
   (w-termin s-0 
        (language english 		; *** see comments in s-ap-2
           (gw 3) (gw 1) (segnointer 0))   ; in English "Boy's;"
        (language english 
           (nomep 3) (gw 1) (segnointer 0)) ; "John's;"
        (language (italian catalan spanish) 
           (nomep 1) (segnointer 0)))   ; in Italian "Boy's" considered as
					;     a full proper name
   (s-termin s-eos 
        (language english 
           (gw 3) (gw 1) (segnointer 0))   ; see comments above
        (language english 
           (nomep 3) (gw 1) (segnointer 0)) ; "John's;"
        (language (italian catalan spanish) 
           (nomep 1) (segnointer 0)))
   (neutral s-0 
        (language english 
           (gw 3) (gw 1) (flush 0))   ; see comments above
        (language english 
           (nomep 3) (gw 1) (flush 0)) ; "John's "
        (language (italian catalan spanish) 
           (nomep 1) (flush 0)))
   (minlet s-gw-1 
        (language english (gw 3))	; the chars preceding the apostrophe are output:
        (language english (nomep 3))	; (also with the proper namme hypothesis)
        (language (italian catalan spanish) 
           (gw 2)))			; the chars preceding and including the apostrophe are output:
                                        ; no nomep output, since no names including an apostroph are
                                        ; assumed to exist
   (else s-gw-0 ! 		        ;     word with apostrophe
        (language english (gw 3))	; the chars preceding the apostrophe are output:
        (language english (nomep 3))	;  (also with the proper namme hypothesis)
        (language (italian catalan spanish) 
           (gw 2))))			; the chars preceding and including the apostrophe are output:
(s-ap-5 				; *** after an apostrophe (occurring after
					;     some alphanumeric characters) and a t:
					;     "don't"
   (w-termin s-0 
        (language english 
            (gw 1) (segnointer 0))      ; *** in English "don't;"
                                        ;     considered as a full gw, followed by
                                        ;     the semicolon
        (language (italian catalan spanish) 
           (nomep 1) (segnointer 0)))   ; *** in Italian "don't" considered as
					;     a full proper name
   (s-termin s-eos 
        (language english 
            (gw 1) (segnointer 0))      ; see comments above
        (language (italian catalan spanish) 
           (nomep 1) (segnointer 0)))
   (neutral s-0 
        (language english 
            (gw 1) (flush 0))           ; see comments above
        (language (italian catalan spanish) 
           (nomep 1) (flush 0)))
   (else s-gw-0 ! ((gw 2))))		; word with apostrophe
(s-y-0					; *** after an apostrophe and a digit
   (digit s-y-1)
   (else s-0 ! ((year 1))))		; *** Probably an error: '7...
(s-y-1					; *** after an apostrophe and two digits
   (digit s-y-2)
   (else s-0 ! ((year 1))))		; *** OK
(s-y-2					; *** after an apostrophe and many digits
   (digit s-y-2)
   (else s-0 ! ((year 1))))		; *** OK

; ***************************** NUMBERS
(s-n-0-a				; after one digit, which is 0
   (zero s-n-1-c)			; 00: it cannot be a day, but it can be an hour
   (one-to-nine s-n-1-d)		; 0n: it can be a day and a hour
   (period s-n-6)
   (comma s-n-7)
   (colon s-n-h-0)			; "0:" could be a hour
   (nth s-0 ((number 0)))
   (else s-0 ! ((number 1))))
(s-n-0-b				; after one digit, which is 1
   (digit s-n-1-d)			; 1n can be a day or a hour
   (period s-n-6)
   (comma s-n-7)
   (colon s-n-h-0)			; "1:" could be a hour
   (slash s-d-0)			; d: dates
   (nth s-0 ((number 0)))
   (else s-0 ! ((number 1))))
(s-n-0-c				; after one digit, which is 2
   (zero-to-three s-n-1-d)		; 20, 21, 22, 23 can be a day or an hour
   (four-to-nine s-n-1-b)		; 2n (n = 4, 5, 6, 7, 8, 9) can only be days 
   (period s-n-6)
   (comma s-n-7)
   (colon s-n-h-0)			; "2:" could be a hour
   (slash s-d-0)			; d: dates
   (nth s-0 ((number 0)))
   (else s-0 ! ((number 1))))
(s-n-0-d				; after one digit, which is 3
   (zero s-n-1-b)			; 30 can be a day
   (one s-n-1-b)			; 31 can be a day
   (two-to-nine s-n-1-a)		; it cannot be a day
   (period s-n-6)
   (comma s-n-7)
   (colon s-n-h-0)			; "3:" could be a hour
   (slash s-d-0)			; d: dates
   (nth s-0 ((number 0)))
   (else s-0 ! ((number 1))))
(s-n-0-e				; after one digit, which is > 3
   (digit s-n-1-a)			; Xn cannot be a day
   (period s-n-6)
   (comma s-n-7)
   (colon s-n-h-0)			; "n:" could be a hour
   (slash s-d-0)			; d: dates
   (nth s-0 ((number 0)))
   (else s-0 ! ((number 1))))
(s-n-1-a				
		; after two digits, which cannot be days or hours (ex. 44, 90)
   (digit s-n-2)
   (period s-n-6)
   (comma s-n-7)
   (nth s-0 ((number 0)))
   (else s-0 ! ((number 1))))
(s-n-1-b				
		; after one or two digits, which can be a day, but not an hour (ex. 25, 31)
   (digit s-n-2)
   (period s-n-6)
   (comma s-n-7)
   (slash s-d-0)			; d: dates
   (nth s-0 ((number 0)))
   (else s-0 ! ((number 1))))
(s-n-1-c				
		; after two zeros, which can be an hour, but not a day (00:15)
   (digit s-n-2)
   (period s-n-6)
   (comma s-n-7)
   (colon s-n-h-0)			; "00:" could be a hour
   (slash s-d-0)			; d: dates
   (nth s-0 ((number 0)))
   (else s-0 ! ((number 1))))
(s-n-1-d				
		; after one or two digits, which can be both a day, and an hour (ex. 08, 22)
   (digit s-n-2)
   (period s-n-6)
   (comma s-n-7)
   (colon s-n-h-0)			; "08:" could be a hour
   (slash s-d-0)			; d: dates
   (nth s-0 ((number 0)))
   (else s-0 ! ((number 1))))
(s-n-2 					; after three digits
   (digit s-n-3)
   (period s-n-6)
   (comma s-n-7)
   (nth s-0 ((number 0)))
   (else s-0 ! ((number 1))))

(s-n-3			; *** more than three consecutive digits: no separator for thousands
   (digit s-n-3)
   (comma s-n-4)			; decimals or new word expected
   (period s-n-6)			; decimals or eos
   (nth s-0 ((number 0)))
   (up-l s-si-0)
   (else s-0 ! ((number 1))))
(s-n-4					; after a comma
   (digit s-n-5)			; inside decimals 
   (else s-0 ! ((number 2) (segnointer 1)))) ; the comma is not part of the number
(s-n-5					; inside decimals
   (digit s-n-5)
   (else s-0 ! ((number 1))))
; *** zero to three digits followed by a period; the period could be a marker
;     for decimals, for thousands or a separator.
(s-n-6
   (digit s-n-8 ;! ((number 2) (segnointer 1))
        )		; !!!! THIS IS THE ORIGINAL VERSION, FOUND IN THE BACKUP OF 2006
                        ;      WITH THE OUTPUT, NOW COMMENTED, ERROR IN NUMBERS AS 1.000 !!!!
   (neutral s-eos ((number 2) (segnointer 1) (flush 0))
		((number 1) (flush 0)))
			; "12. " assumed to be a 12 followed by a terminator
			; or a decimal number
   (period s-dot-1 ((number 2)))
   (else s-0 ! ((number 1))))
(s-n-8
   ; *** zero to three digits followed by a period followed by a digit
   (digit s-n-9)
   (period s-chap-1)
   (else s-0 ! ((number 1))
               ((paragraph-numb 1))))
(s-n-9
   ; *** zero to three digits followed by a period followed by two digits
   (digit s-n-10)
   (period s-chap-1)
   (else s-0 ! ((number 1))
               ((paragraph-numb 1))))
(s-n-10
   (digit s-n-5)		; more than three digits: decimals
   (period s-n-11)		; two periods with three digits in between:
				; periods used for thousands
   (comma s-n-5)		; the comma is used for decimals
   (else s-0 ! ((number 1))))
(s-n-11
   (digit s-n-12)
   (else s-0 ! ((number 1))))
(s-n-12
   (digit s-n-13)
   (else s-0 ! ((number 1))))
(s-n-13
   (digit s-n-14)
   (else s-0 ! ((number 1))))
(s-n-14
   (period s-n-11)		; new group of three digits expected
   (comma s-n-5)		; decimals expected
   (else s-0 ! ((number 1))))
; *** zero to three digits followed by a comma; the comma could be a marker
;     for decimals, for thousands or a separator.
(s-n-7
   (digit s-n-15)
; *** at least one digit after the comma is required, so if after the comma
;     there is no digit, then we output a number followed by the comma
;     intepreted as a separator
   (else s-0 ! ((number 2) (segnointer 1))))
(s-n-15
   (digit s-n-16)
   (else s-0 ! ((number 1))))
(s-n-16
   (digit s-n-17)
   (else s-0 ! ((number 1))))
; *** three digits found after a comma
(s-n-17
   (digit s-n-5)		; more than three digits: decimals
   (comma s-n-18)		; two commas with three digits in between:
				; commas used for thousands
   (period s-n-5)		; the period is used for decimals
   (else s-0 ! ((number 1))))
(s-n-18
   (digit s-n-19)
   (else s-0 ! ((number 1))))
(s-n-19
   (digit s-n-20)
   (else s-0 ! ((number 1))))
(s-n-20
   (digit s-n-21)
   (else s-0 ! ((number 1))))
(s-n-21
   (comma s-n-18)		; new group of three digits expected
   (period s-n-5)		; decimals expected
   (else s-0 ! ((number 1))))

; ***************************** DATES *******
; *** N.B.2 mm/yy dates are not recognized (ex. 09/95)
(s-d-0					; after the first slash
   (zero s-d-1)
   (one s-d-2)
   (two-to-nine s-d-3)		; months: 2 to 9
   (else s-0 ! ((number 2) (segnointer 1)))) ; it was not a date
(s-d-1				; months: 01 to 09
   (one-to-nine s-d-3)
   (else s-0 ! ((number 3) (segnointer 2) (number 1)))) ; it was not a date
(s-d-2
   (zero-to-two s-d-3)		; months: 10 to 12
   (slash s-d-4)		; months: 1
   (else s-0 ! ((number 3) (segnointer 2) (number 1)))) ; it was not a date
(s-d-3
   (slash s-d-4)
   (else s-0 ! ((number 3) (segnointer 2) (number 1)))) ; it was not a date
(s-d-4				; after the second slash
   (digit s-d-5)
   (else s-0 ! ((number 4) (segnointer 3) (number 2) (segnointer 1)))) 
		; it was not a date
		; !!!! The output is possibly wrong, since it is not possible
		;      to know whether the month was formed by one or two
		;      digits !!!!!!!!!
(s-d-5
   (digit s-d-6)
   (else s-0 ! ((number 4) (segnointer 3) (number 2) (segnointer 1)))) ;see above
(s-d-6				; year: after two digits it can terminate
   (digit s-d-7)
   (else s-0 ! ((date 1))))
(s-d-7				; year: after three digits it can terminate
   (digit s-d-8)
   (else s-0 ! ((date 1))))
(s-d-8				; year: after four digits it must terminate
   (digit s-d-8)		; however, more digits are appended to the year
   (else s-0 ! ((date 1))))

; ***************************** HOURS WITH COLONS *******
; *** 0:15, 23:00, 12:50:32
;     12:7 is not taken as hour specification, while 12:07:9 is ok
(s-n-h-0			; after "n:" or "nn:", where the value is between 0 and 23
   (zero-to-five s-n-h-1)	; it can really be a hour; here start the minutes
   (else s-eos ! ((number 2) (segnointer 1))))

(s-n-h-1			; after "n:k" or "nn:k", where k is between zero and five
   (digit s-n-h-2)	        ; second digit of the minutes
   (else s-eos !! ((number 3) (segnointer 2))))

(s-n-h-2			; after "n:kk" or "nn:kk", where kk is between 00 and 59
   (colon s-n-h-3)	        ; the seconds are expected
   (else s-0 ! ((hour 1))))

(s-n-h-3			; after "n:kk:" or "nn:kk:"
   (zero-to-five s-n-h-4)       ; first digit of the seconds
   (six-to-nine s-0 ! ((hour 1))) ; single digit of the seconds
   (else s-eos ! ((hour 2) (segnointer 1))))

(s-n-h-4			; after "n:kk:j" or "nn:kk:j" where j is between zero and five
   (digit s-0 ((hour 1)))       ; second digit of the seconds
   (else s-0 !! ((hour 2))))

;******************* GENERAL WORDS ***
; *** the first state - s-gw-0 - is entered after a minlet
(s-gw-0
   (minlet s-gw-1)
   (caplet s-si-00)		; mixed case: sigla, but in Hindi also GW
   (digit s-si-0)
   (period s-gw-4)
   (grave-accent s-ac-1)
   (apostrophe s-ap-1)
   (commerc-at s-www-0)
   (closed-par s-0 ((special 1) (segnointer 0)))
   (hyphen s-mi-0)
   (nth s-0 ((sigla 0)))
   (s-term-not-period s-eos ((gw 1) (segnointer 0)))
   (else s-0 ! ((gw 1))))
(s-gw-1				; after more than one minlet
   (minlet s-gw-1)
   (caplet s-si-00)		; mixed case: sigla, but in Hindi also GW
   (digit s-si-0)
   (period s-gw-2)
   (grave-accent s-ac-1)
   (apostrophe s-ap-1)
   (mid-dot s-gw-6)		; the catalan symbol · used to separate two "l"
   (hyphen s-mi-0)
   (commerc-at s-www-0)
   (s-term-not-period s-eos ((gw 1) (segnointer 0)))
   (else s-0 ! ((gw 1))))
(s-gw-2				; after more than one minlet and a period
   (neutral s-gw-3)
   (minlet s-si-0)
   (caplet s-si-0)
   (digit s-si-0)
   (period s-dot-1 ((gw 2)))
   (else s-eos ! ((sigla 1)) ((gw 2) (segnointer 1))))
(s-gw-3					; after a period and a space
   (neutral s-gw-3)
; *** the $ means that all chars must be output until the period (46) inclusive
; *** the @ means that same as $, but the period (46) is not included
; *** the - means that all chars must be output until something different from
;     46 is found (used to output the single period)
; *** I'm forced to assume in all cases that a gw+period interpretation is 
;     possible; even in cases when the period is followed by a minlet!
   (else s-eos ! ((sigla $ period) (flush 1))
		 ((gw @ period) (segnointer - period) (flush 1))))
(s-gw-4				; after a single minlet and a period
   (neutral s-eos ((sigla 1) (flush 0)) ((gw 2) (segnointer 1) (flush 0)))
   (minlet s-gw-5)
   (caplet s-si-0)
   (digit s-si-0)
   (period s-dot-1 ((gw 2)))
   (else s-eos ! ((sigla 1)) ((gw 2) (segnointer 1))))
(s-gw-5				;after a minlet, a period, and one or more
				; minlet
   (neutral s-0 ((sigla 1) (flush 0)))
   (minlet s-gw-5)
          ; *** "a.a", "a.ab", ...
   (caplet s-si-0)
   (digit s-si-0)
   (period s-si-2)
          ; *** "a.a.", "a.ab.", ...
   (sig-char s-si-1)
   (else s-0 ! ((sigla 1) (segnointer 0))))
(s-gw-6				; after a catalan ·. Actually, we test here that only the letter
                                ; after · is an "l", while it should be checked also the letter 
                                ; before it
   (down-l s-gw-1)
   (else s-0 ! ((gw 1) (segnointer 0))))

; *** after a minus (-) following small letters
(s-mi-0
   (minlet s-mi-1)
   (caplet s-mi-1)
   (digit s-si-0)
   (period s-0 ((sigla 1) (segnointer 0))
	       ((gw 2) (special 1) (segnointer 0)))
   (grave-accent s-ac-1)
   (apostrophe s-ap-1)
   (neutral s-0 ((sigla 1) (flush 0)) ((gw 2) (segnointer 1) (flush 0)))
   (hyphen s-0 ((sigla 0)) ((gw 2) (segnointer 1) (segnointer 0)))
   (else s-0 ! ((gw 2) (segnointer 1)) ((sigla 1))))
(s-mi-1		; after small letters, -, and then small letters
   (minlet s-mi-1)
   (caplet s-si-00)
   (digit s-si-0)
;   (period s-mi-1)
   (grave-accent s-ac-1)
   (apostrophe s-ap-1)
   (neutral s-0 ((gw 1) (flush 0))
		((gw @ hyphen)
		 (segnointer - hyphen)
		 (gw @ hyphen)
		 (segnointer - hyphen)
		 (gw 1)
		 (flush 0)))
; *** the second output above is intended to mean: 
;     - first output all chars in word1 up to and not including a 45 (minus)
;     - then output all chars remained in word1 until they are equal to 45
;     - then repeat the two outputs above
;     - then output a gw in the standard way (leaving a char in word1)
;    The repetition of the first pair enables to handle sequences of until
;     three words connected by -
   (hyphen s-mi-0)
   (else s-0 ! ((gw 1))
	       ((gw @ hyphen)
		(segnointer - hyphen)
		(gw @ hyphen)
		(segnointer - hyphen)
		(gw 1))))
; *** after a minus (-) following capitalized word
(s-mi-2
   (minlet s-mi-3)
   (caplet s-mi-3)
   (digit s-si-0)
   (period s-0 ((sigla 1) (segnointer 0))
	       ((nomep 2) (special 1) (segnointer 0))
	       ((gw 2) (special 1) (segnointer 0)))
   (grave-accent s-ac-1)
   (apostrophe s-ap-1)
   (neutral s-0 ((sigla 1) (flush 0))
		((nomep 2) (segnointer 1) (flush 0))
		((gw 2) (segnointer 1) (flush 0)))
   (hyphen s-0 ((sigla 0))
	      ((nomep 2) (segnointer 1) (segnointer 0))
	      ((gw 2) (segnointer 1) (segnointer 0)))
   (else s-0 ! ((gw 2) (segnointer 1)) ((nomep 2) (segnointer 1)) ((sigla 1))))
(s-mi-3		
   (minlet s-mi-3)
   (caplet s-mi-3)
   (digit s-si-0)
   (hyphen s-mi-2)
   (else s-0 ! ((gw 1))
	       ((nomep 1))
	       ((gw @ hyphen) 
		(segnointer - hyphen)
		(gw @ hyphen)
		(segnointer - hyphen)
		(gw 1))
; *** the output above is intended to mean: 
;     - first output all chars in word1 up to and not including a 45 (minus)
;     - then output all chars remained in word1 until they are equal to 45
;     - then repeat the two outputs above
;     - then output a gw in the standard way (leaving a char in word1)
;    The repetition of the first pair enables to handle sequences of until
;     three words connected by -
	       ((nomep @ hyphen)
		(segnointer - hyphen)
		(nomep @ hyphen)
		(segnointer - hyphen)
		(nomep 1))
	       ((nomep @ hyphen)
	        (segnointer - hyphen)
		(gw @ hyphen)
		(segnointer - hyphen)
		(gw 1))))

;***************** Almost certainly a SIGLA *********
;    counterexample: McCartney
(s-si-00			; after mixture of caplet and minlet
   (sig-char-not-period s-si-1)
   (period s-si-4)
   (minlet s-si-00)
   (caplet s-si-00)
   (digit s-si-0)
   (else s-0 ! ((sigla 1)) ((gw 1))))
(s-si-0				; after a letter, or minlet, period, caplet
   (sig-char-not-period s-si-1)
   (period s-si-4)
   (minlet s-si-0)
   (caplet s-si-0)
   (digit s-si-0)
   (else s-0 ! ((sigla 1))))
(s-si-1				; after a special character (& _ - .)
   (minlet s-si-0)
   (caplet s-si-0)
   (digit s-si-0)
   (else s-0 ! ((sigla 1)) ((sigla 2) (segnointer 1))))
(s-si-2	
           ; *** "A.B." (from s-gwp-6)
           ; *** "a.a.", "a.ab.", ... (from s-gw-5)
   (caplet s-si-3)
   (minlet s-si-3)
   (digit s-si-0)
   (period s-dot-1 ((sigla 2)))
   (else s-0 ! ((sigla 1))))
(s-si-3
           ; *** "A.B.A", "A.B.a" (from s-si-2)
           ; *** "a.a.C", "a.ab.C", ... (from s-si-2)
   (caplet s-si-0)
           ; *** "A.B.AC", "A.B.aC", "a.a.CE", "a.ab.CE", ... 
   (digit s-si-0)
           ; *** "A.B.Ac", "A.B.ac", "a.a.Ce", "a.ab.Ce", ... 
   (period s-si-2)
           ; *** "A.B.A.", "A.B.a.", "a.a.C.", "a.ab.C.", "a.abc.C., ... 
   (sig-char-not-period s-si-1)
   (else s-0 ! ((sigla 1))))
(s-si-4
   (minlet s-si-0)
   (caplet s-si-0)
   (digit s-si-0)
   (else s-eos ! ((sigla 1)) ((sigla 2) (segnointer 1))))

;******************* PROPER NAMES (including general words and siglas) ***
; *** the first state - s-gwp-0 - is entered after a caplet
(s-gwp-0
   (minlet s-gwp-1)
   (caplet s-gwp-2)
   (digit s-si-0)
   (period s-gwp-5)
   (ampersand s-si-1)
   (underscore s-si-1)
   (grave-accent s-0 ((nomep 0)) ((gw 0)))
   (apostrophe s-ap-3)
   (nth s-0 ((sigla 0)))
   (commerc-at s-www-0)
   (hyphen s-mi-2)
   (else s-0 ! ((sigla 1)) ((nomep 1)) ((gw 1))))
(s-gwp-1		; after an initial caplet and a sequence of minlet
   (minlet s-gwp-1)
   (caplet s-si-00)		; mixed case: sigla
   (digit s-si-0)
   (period s-gwp-3)
   (ampersand s-si-1)
   (underscore s-si-1)
   (grave-accent s-0 ((nomep 0)) ((gw 0)))
   (apostrophe s-ap-3)
   (mid-dot s-gwp-7)		; the catalan symbol · used to separate two "l"
   (commerc-at s-www-0)
   (hyphen s-mi-2)
   (else s-0 ! ((sigla 1)) ((nomep 1)) ((gw 1))))
(s-gwp-2		; after more than one initial caplets
   (minlet s-si-00)		; mixed case: sigla
   (caplet s-gwp-2)
   (digit s-si-0)
   (period s-gwp-3)
   (ampersand s-si-1)
   (underscore s-si-1)
   (grave-accent s-0 ((nomep 0)) ((gw 0)))
   (apostrophe s-ap-3)
   (commerc-at s-www-0)
   (hyphen s-mi-2)
   (else s-0 ! ((sigla 1)) ((nomep 1)) ((gw 1))))
(s-gwp-3	; after more than one initial caplets and a period
   (neutral s-gwp-4)
   (minlet s-si-0)
   (digit s-si-0)
   (caplet s-si-0)
   (period s-dot-1 ((sigla 2)) ((nomep 2)) ((gw 2)))
   (else s-0 ! ((sigla 1)) ((nomep 2) (segnointer 1)) ((gw 2) (segnointer 1)) 
		((sigla 2) (segnointer 1))))
(s-gwp-4		; after a period and some neutrals
   (neutral s-gwp-4)
; *** the $ means that all chars must be output until the period (46) inclusive
; *** the @ means that same as $, but the period (46) is not included
; *** the - means that all chars must be output until something different from
;     46 is found (used to output the single period)
; *** I'm forced to assume in all cases that a gw+period interpretation is 
;     possible; even in cases when the period is followed by a minlet!
   (else s-eos ! ((sigla $ period) (flush 1)) 
                 ((sigla @ period) (segnointer - period) (flush 1)) 
	         ((nomep @ period) (segnointer - period) (flush 1))
	         ((gw @ period) (segnointer - period) (flush 1))))
(s-gwp-5		; after a single caplet and a period
; *** this state is reached after a single caplet followed by a period
   (neutral s-eos ((sigla 1) (flush 0))
		((nomep 1) (flush 0))
; *** the preceding line for dotted proper nouns, e.g. John B. Smith
		((sigla 2) (segnointer 1) (flush 0))
; *** the preceding line for one-character siglas, as roman numbers I, V, X
		((gw 2) (segnointer 1) (flush 0)))
   (minlet s-gw-0)
   (caplet s-gwp-6)
   (digit s-si-0)
   (else s-0 ! ((sigla 1))
	       ((nomep 1))
	       ((gw 2) (segnointer 1)) 
	       ((sigla 2) (segnointer 1))))
(s-gwp-6		; after a caplet, a period and a caplet
   (neutral s-0 ((sigla 1) (flush 0)) ((gw 2) (segnointer 1) (flush 0)))
           ; *** "A.X "
   (minlet s-gwp-1 ((sigla 2) (nomep 2)))
           ; *** "A.Be"
   (caplet s-gwp-2 ((sigla 2) (nomep 2)))
           ; *** "A.BE"
   (digit s-si-0)
           ; *** "A.B7"
   (sig-char-not-period s-si-1)
           ; *** "A.B#"
   (period s-si-2)
           ; *** "A.B."
   (else s-0 ! ((sigla 1))))
(s-gwp-7			; after a catalan ·. Actually, we test here that only the letter
                                ; after · is an "l", while it should be checked also the letter 
                                ; before it
   (down-l s-gwp-1)
   (else s-0 ! ((sigla 1) (segnointer 0))
               ((nomep 1) (segnointer 0))
               ((gw 1) (segnointer 0))))
(s-chap-1			; after some digits, a period, some digits, another period
                                ; assumed to be a paragraph number or a real number followed
                                ; by a period; this, unless another digit follows
   (digit s-chap-2 ;((number 2) (segnointer 1))
                   ;((number 1))
                  )
   (neutral s-0 ((number 2) (segnointer 1) (flush 0))
                ((paragraph-numb 1) (flush 0)))
   (else s-0 ! ((number 2) (segnointer 1))
              ((paragraph-numb 1))))
(s-chap-2			; after some digits, a period, some digits, another period
                                ; another digit: assumed to be a sub-paragraph number
   (digit s-chap-2)
   (period s-chap-3)
   (neutral s-0 ((paragraph-numb 1) (flush 0)))
   (else s-0 ! ((paragraph-numb 1))))
(s-chap-3			; after some digits, a period, some digits, another period
                                ; another period, another digit; paragraph number
   (digit s-chap-2 ;((number 2) (segnointer 1))
                   ;((number 1))
                  )
   (neutral s-0 ((paragraph-numb 1) (flush 0)))
   (else s-0 ! ((paragraph-numb 1))))

  ))

;****************************************************************************
; *********  The grammar *******
;
;   gw --> minlet+ {' `} | caplet+ {' `} | caplet minlet* {' `} |
;	   minlet+ - minlet+ {' `} | caplet+ - caplet+ {' `} |
;	   caplet minlet* - minlet+ {' `}
;   nomep --> caplet minlet* {' `} | caplet+ {' `} 
;   sigla --> {.} (minlet caplet)+ [(. # & /) (minlet caplet)+]* {. # & /}
;   number --> digit+ | digit* . digit+ | digit+ , digit+ |
;	       digit+3 [. digit=3]+ , digit+ | digit+3 [, digit=3]+ . digit+
;   data --> digit {digit} / 0 digit / digit digit {digit} {digit}
;	     digit {digit} / 1 (0 1 2) / digit digit {digit} {digit}
;   www --> (caplet minlet)+ @ (caplet minlet)+ [. (caplet minlet+)]*
;   segnointer -->  ! " & ( ) , - . / : ; ? [ ] << >> .. ...
;   special --> # $ * + = \ ^ { | } ~

; *** Where (x1 ... xn) means "x1 or ... or xn"
;           [x1 ... xn] means "x1 followed-by ... followed-by xn"
;	    {x1 ... xn} means "x1 or ... or xn or nothing"
;	    X* means 0 or more occurrences of X
;	    X+ means 1 or more occurrences of X
;	    X+n means 1 to n occurrences of X
;	    X=n means exactly n occurrences of X
