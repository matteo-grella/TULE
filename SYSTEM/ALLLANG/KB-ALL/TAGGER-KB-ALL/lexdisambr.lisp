
(in-package "USER")

; $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;     THE PREDICATES APPEARING IN THE IF PART OF THE RULES ARE DESCRIBED
;     IN "POSTAGGER.LISP"
; $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

; **** RULES FOR DIFFERENT CATEGORIES ********
(defvar adj-advr01 nil)
(defvar adj-advr02i nil)			; ITALIAN
(defvar adj-advr03 nil)
(defvar adj-advr04 nil)
(defvar adj-advr05i nil)			; ITALIAN
(defvar adj-advr06e nil)			; ENGLISH
(defvar adj-advr07 nil)
(defvar adj-advr08i nil)			; ITALIAN
(defvar adj-advr09 nil)
(defvar adj-advr10e nil)			; ENGLISH
(defvar adj-advr11e nil)			; ENGLISH
;.....................................
(defvar adj-adv-nounr01 nil)
(defvar adj-adv-nounr02 nil)
(defvar adj-adv-nounr03e nil)
(defvar adj-adv-nounr04e nil)		; ENGLISH
(defvar adj-adv-nounr05 nil)
(defvar adj-adv-nounr06 nil)
(defvar adj-adv-nounr07 nil)
(defvar adj-adv-nounr08 nil)
;.....................................
(defvar adj-adv-noun-phrasr01 nil)	; "NO" (in english)
(defvar adj-adv-noun-phrasr02 nil)	; "NO" (in english)
(defvar adj-adv-noun-phrasr03 nil)	; "NO" (in english)
;.....................................
(defvar adj-adv-noun-prepr01 nil)
(defvar adj-adv-noun-prepr02e nil)	; ENGLISH (OVER)
(defvar adj-adv-noun-prepr03e nil)	; ENGLISH
(defvar adj-adv-noun-prepr04 nil)
(defvar adj-adv-noun-prepr05e nil)	; ENGLISH
(defvar adj-adv-noun-prepr06 nil)
;.....................................
(defvar adj-adv-noun-pronr01 nil)	; MORE, FIRST
(defvar adj-adv-noun-pronr02 nil)	; MORE, FIRST
(defvar adj-adv-noun-pronr03 nil)	; MORE, FIRST
(defvar adj-adv-noun-pronr04 nil)	; MORE, FIRST
;.....................................
(defvar adj-adv-noun-prep-verbr01e nil)	; ENGLISH (OUT)
(defvar adj-adv-noun-prep-verbr02e nil)	; ENGLISH (OUT)
(defvar adj-adv-noun-prep-verbr03e nil)	; ENGLISH (OUT)
;.....................................
(defvar adj-adv-noun-verbr01 nil)	; 
(defvar adj-adv-noun-verbr02 nil)
(defvar adj-adv-noun-verbr03e nil)	; ENGLISH
(defvar adj-adv-noun-verbr04 nil)	; 
(defvar adj-adv-noun-verbr05 nil)	; 
;.....................................
(defvar adj-adv-noun-prep-pronr01 nil)
(defvar adj-adv-noun-prep-pronr02 nil)
(defvar adj-adv-noun-prep-pronr03 nil)
(defvar adj-adv-noun-prep-pronr04 nil)
;.....................................
(defvar adj-adv-predetr01e nil)		; ENGLISH: SUCH
(defvar adj-adv-predetr02e nil)		; ENGLISH: SUCH
;.....................................
(defvar adj-adv-prepr01e nil)		; ENGLISH
(defvar adj-adv-prepr02 nil)
(defvar adj-adv-prepr03 nil)
(defvar adj-adv-prepr04 nil)
(defvar adj-adv-prepr05 nil)
(defvar adj-adv-prepr06 nil)
(defvar adj-adv-prepr07 nil)
;.....................................
(defvar adj-adv-prep-pronr01 nil)	; PIU'
(defvar adj-adv-prep-pronr02 nil)
(defvar adj-adv-prep-pronr03 nil)
(defvar adj-adv-prep-pronr04 nil)
(defvar adj-adv-prep-pronr05 nil)
;.....................................
(defvar adj-adv-prep-verbr01 nil)	; NEAR
(defvar adj-adv-prep-verbr02 nil)	; NEAR
(defvar adj-adv-prep-verbr03 nil)	; NEAR
;.....................................
(defvar adj-adv-pronr00 nil)
(defvar adj-adv-pronr01 nil)
(defvar adj-adv-pronr02 nil)
(defvar adj-adv-pronr03 nil)
(defvar adj-adv-pronr04 nil)
(defvar adj-adv-pronr05 nil)
(defvar adj-adv-pronr06 nil)
(defvar adj-adv-pronr07 nil)
(defvar adj-adv-pronr08 nil)
(defvar adj-adv-pronr09 nil)
(defvar adj-adv-pronr10e nil)		; ENGLISH: MORE
(defvar adj-adv-pronr11 nil)
(defvar adj-adv-pronr12 nil)
;.....................................
(defvar adj-adv-verbr01 nil)
(defvar adj-adv-verbr02 nil)
(defvar adj-adv-verbr03 nil)
(defvar adj-adv-verbr04 nil)
(defvar adj-adv-verbr05 nil)
(defvar adj-adv-verbr06 nil)
;.....................................
(defvar adj-predet-pronr01 nil)         ; ENGLISH: BOTH, ALL; SPANISH: TODO
(defvar adj-predet-pronr02 nil)         ; ENGLISH: BOTH, ALL; SPANISH: TODO
(defvar adj-predet-pronr03 nil)         ; ENGLISH: BOTH, ALL; SPANISH: TODO
;.....................................
(defvar adj-conj-noun-prep-pronr01 nil)
(defvar adj-conj-noun-prep-pronr02 nil)
;.....................................
(defvar adj-conj-pronr01 nil)   ; ENGLISH: THAT
(defvar adj-conj-pronr02 nil)		; CHE
(defvar adj-conj-pronr03e nil)
(defvar adj-conj-pronr04 nil)
(defvar adj-conj-pronr05 nil)
(defvar adj-conj-pronr06 nil)
(defvar adj-conj-pronr06-bis nil)
(defvar adj-conj-pronr06-ter nil)
(defvar adj-conj-pronr07 nil)
(defvar adj-conj-pronr08 nil)
(defvar adj-conj-pronr09 nil)
(defvar adj-conj-pronr10 nil)
(defvar adj-conj-pronr11 nil)
(defvar adj-conj-pronr12 nil)
(defvar adj-conj-pronr13 nil)
(defvar adj-conj-pronr14 nil)
(defvar adj-conj-pronr15 nil)
(defvar adj-conj-pronr16 nil)
(defvar adj-conj-pronr17 nil)
(defvar adj-conj-pronr18 nil)
(defvar adj-conj-pronr19 nil)
(defvar adj-conj-pronr20 nil)
(defvar adj-conj-pronr21 nil)
(defvar adj-conj-pronr22 nil)
(defvar adj-conj-pronr23 nil)
(defvar adj-conj-pronr24 nil)
(defvar adj-conj-pronr25 nil)
;.....................................
(defvar adj-nounr000 nil)
(defvar adj-nounr001 nil)
(defvar adj-nounr002 nil)
(defvar adj-nounr00 nil)
(defvar adj-nounr00e nil)	; ENGLISH
(defvar adj-nounr01e nil)	; ENGLISH
(defvar adj-nounr02 nil)
(defvar adj-nounr03 nil)
(defvar adj-nounr04 nil)
(defvar adj-nounr05 nil)
(defvar adj-nounr06 nil)
(defvar adj-nounr07 nil)
(defvar adj-nounr08 nil)
(defvar adj-nounr09 nil)
(defvar adj-nounr10 nil)
(defvar adj-nounr11 nil)
(defvar adj-nounr12 nil)
(defvar adj-nounr13i nil)	; ITALIAN
(defvar adj-nounr14 nil)
(defvar adj-nounr15 nil)
(defvar adj-nounr16 nil)
(defvar adj-nounr16-bis nil)
(defvar adj-nounr17 nil)
(defvar adj-nounr18i nil)	; ITALIAN
(defvar adj-nounr19i nil)	; ITALIAN
(defvar adj-nounr20i nil)	; ITALIAN
(defvar adj-nounr21 nil)
(defvar adj-nounr22 nil)
(defvar adj-nounr23ics nil)
(defvar adj-nounr24 nil)
(defvar adj-nounr25 nil)
(defvar adj-nounr26e nil)	; ENGLISH
(defvar adj-nounr27 nil)
(defvar adj-nounr28 nil)
(defvar adj-nounr29 nil)
(defvar adj-nounr30e nil)	; ENGLISH
(defvar adj-nounr30-2i nil)	; ITALIAN
(defvar adj-nounr30-3 nil)
(defvar adj-nounr31 nil)
(defvar adj-nounr32 nil)
(defvar adj-nounr33 nil)
(defvar adj-nounr34 nil)
(defvar adj-nounr35 nil)
(defvar adj-nounr36 nil)
(defvar adj-nounr37 nil)
(defvar adj-nounr38 nil)
(defvar adj-nounr39 nil)
(defvar adj-nounr40 nil)
;.....................................
(defvar adj-noun-prep-pronr01 nil)
(defvar adj-noun-prep-pronr02 nil)
(defvar adj-noun-prep-pronr03 nil)
(defvar adj-noun-prep-pronr04 nil)
(defvar adj-noun-prep-pronr05 nil)
(defvar adj-noun-prep-pronr06 nil)
(defvar adj-noun-prep-pronr07 nil)
(defvar adj-noun-prep-pronr08 nil)
;.....................................
(defvar adj-noun-prep-verbr01 nil)	; ENGLISH "like"
(defvar adj-noun-prep-verbr02 nil)	; ENGLISH "like"
(defvar adj-noun-prep-verbr03 nil)	; ENGLISH "like"
(defvar adj-noun-prep-verbr04 nil)	; ENGLISH "like"
(defvar adj-noun-prep-verbr05 nil)	; ENGLISH "like"
(defvar adj-noun-prep-verbr06 nil)	; ENGLISH "like"
(defvar adj-noun-prep-verbr07 nil)	; ENGLISH "like"
(defvar adj-noun-prep-verbr08 nil)	; ENGLISH "like"
;.....................................
(defvar adj-noun-pron-verbr01 nil)	; ENGLISH "second"
(defvar adj-noun-pron-verbr02 nil)	; ENGLISH "second"
(defvar adj-noun-pron-verbr03 nil)	; ENGLISH "second"
(defvar adj-noun-pron-verbr04 nil)	; ENGLISH "second"
(defvar adj-noun-pron-verbr05 nil)	; ENGLISH "second"
(defvar adj-noun-pron-verbr06 nil)	; ENGLISH "second"
;.....................................
(defvar adj-noun-verbr01is nil)		; ITALIAN, SPANISH
(defvar adj-noun-verbr02e nil)		; ENGLISH
(defvar adj-noun-verbr03e nil)		; ENGLISH
(defvar adj-noun-verbr03bis-e nil)	; ENGLISH
(defvar adj-noun-verbr04 nil)
(defvar adj-noun-verbr05 nil)
(defvar adj-noun-verbr06 nil)
(defvar adj-noun-verbr07 nil)
(defvar adj-noun-verbr08 nil)
(defvar adj-noun-verbr09 nil)
(defvar adj-noun-verbr10 nil)
(defvar adj-noun-verbr11 nil)
(defvar adj-noun-verbr12 nil)
(defvar adj-noun-verbr13 nil)
(defvar adj-noun-verbr14 nil)
(defvar adj-noun-verbr14bis-e nil)	; ENGLISH
(defvar adj-noun-verbr14ter-e nil)	; ENGLISH
(defvar adj-noun-verbr15ics nil)	; ITALIAN, CATALAN, SPANISH
(defvar adj-noun-verbr15e nil)		; ENGLISH
(defvar adj-noun-verbr16 nil)
(defvar adj-noun-verbr17 nil)
(defvar adj-noun-verbr18 nil)
(defvar adj-noun-verbr19 nil)
(defvar adj-noun-verbr20 nil)
(defvar adj-noun-verbr21 nil)
(defvar adj-noun-verbr22e nil)		; ENGLISH
(defvar adj-noun-verbr23 nil)
(defvar adj-noun-verbr24 nil)
(defvar adj-noun-verbr25e nil)		; ENGLISH
(defvar adj-noun-verbr26e nil)		; ENGLISH
(defvar adj-noun-verbr27i nil)		; ITALIAN
(defvar adj-noun-verbr28i nil)		; ITALIAN
(defvar adj-noun-verbr29 nil)
(defvar adj-noun-verbr30 nil)
(defvar adj-noun-verbr31 nil)
(defvar adj-noun-verbr32 nil)
(defvar adj-noun-verbr33 nil)
(defvar adj-noun-verbr34 nil)
(defvar adj-noun-verbr35 nil)
(defvar adj-noun-verbr36 nil)
;.....................................
(defvar adj-noun-verb+pronr01 nil)
(defvar adj-noun-verb+pronr02 nil)
(defvar adj-noun-verb+pronr03 nil)
(defvar adj-noun-verb+pronr04 nil)
(defvar adj-noun-verb+pronr05 nil)
(defvar adj-noun-verb+pronr06 nil)
;.....................................
(defvar adj-prepr01 nil)
(defvar adj-prepr02 nil)
(defvar adj-prepr03 nil)
(defvar adj-prepr04e nil)		; ENGLISH
;.....................................
(defvar adj-prep-verbr01 nil)
(defvar adj-prep-verbr02 nil)
(defvar adj-prep-verbr03 nil)
(defvar adj-prep-verbr04 nil)
;.....................................
(defvar adj-prep-pronr01 nil)		; QUALE, QUALI
(defvar adj-prep-pronr02 nil)
(defvar adj-prep-pronr03 nil)
;.....................................
(defvar adj-pronr03 nil)
(defvar adj-pronr04ics nil)	; ITALIAN, CATALAN, SPANISH
(defvar adj-pronr05e nil)	; ENGLISH
(defvar adj-pronr06 nil)
(defvar adj-pronr07 nil)
(defvar adj-pronr08 nil)
(defvar adj-pronr09 nil)
(defvar adj-pronr10 nil)
(defvar adj-pronr11 nil)
(defvar adj-pronr12 nil)
(defvar adj-pronr13 nil)
(defvar adj-pronr14i nil)	; ITALIAN
(defvar adj-pronr15e nil)	; ENGLISH
(defvar adj-pronr16e nil)	; ENGLISH
(defvar adj-pronr17i nil)	; ITALIAN
(defvar adj-pronr18i nil)	; ITALIAN
(defvar adj-pronr19 nil)
(defvar adj-pronr20ics nil)	; ITALIAN, CATALAN, SPANISH
(defvar adj-pronr21e nil)	; ENGLISH
(defvar adj-pronr22 nil)
(defvar adj-pronr23 nil)
(defvar adj-pronr24 nil)
(defvar adj-pronr25 nil)
(defvar adj-pronr26 nil)
;(defvar adj-pronr27i nil)	; ITALIAN
(defvar adj-pronr28e nil)	; ENGLISH
(defvar adj-pronr29e nil)	; ENGLISH
(defvar adj-pronr30c nil)	; CATALAN
;.....................................
(defvar adj-pron-verbr01 nil)
(defvar adj-pron-verbr02 nil)
(defvar adj-pron-verbr03 nil)
(defvar adj-pron-verbr04 nil)
(defvar adj-pron-verbr05 nil)
(defvar adj-pron-verbr06 nil)
(defvar adj-pron-verbr07 nil)
(defvar adj-pron-verbr08 nil)
(defvar adj-pron-verbr09 nil)
;.....................................
(defvar adj-verbr00i nil)	; ITALIAN
(defvar adj-verbr01 nil)
(defvar adj-verbr02e nil)	; ENGLISH
(defvar adj-verbr03 nil)
(defvar adj-verbr04 nil)
(defvar adj-verbr05 nil)
(defvar adj-verbr06 nil)
(defvar adj-verbr07 nil)
(defvar adj-verbr08e nil)	; ENGLISH
(defvar adj-verbr09ics nil)	; ITALIAN CATALAN SPANISH
(defvar adj-verbr10i nil)	; ITALIAN
(defvar adj-verbr11e nil)	; ENGLISH
(defvar adj-verbr12 nil)
(defvar adj-verbr13 nil)
(defvar adj-verbr14 nil)
(defvar adj-verbr15 nil)
(defvar adj-verbr16 nil)
(defvar adj-verbr17 nil)
(defvar adj-verbr18 nil)
(defvar adj-verbr19 nil)
(defvar adj-verbr20 nil)
(defvar adj-verbr21 nil)
(defvar adj-verbr22 nil)
(defvar adj-verbr23 nil)
(defvar adj-verbr24 nil)
(defvar adj-verbr25 nil)
(defvar adj-verbr26 nil)
(defvar adj-verbr27 nil)
(defvar adj-verbr28 nil)
(defvar adj-verbr29i nil)	; ITALIAN
(defvar adj-verbr30 nil)
(defvar adj-verbr31 nil)
(defvar adj-verbr32 nil)
(defvar adj-verbr33i nil)	; ITALIAN
(defvar adj-verbr34i nil)	; ITALIAN
(defvar adj-verbr35i nil)	; ITALIAN
(defvar adj-verbr36 nil)
;.....................................
(defvar adj-verb-verb+pronr01 nil)
(defvar adj-verb-verb+pronr02 nil)
;.....................................
(defvar adv-conjr01 nil)
(defvar adv-conjr02 nil)
(defvar adv-conjr03 nil)
(defvar adv-conjr04 nil)
(defvar adv-conjr05 nil)
(defvar adv-conjr05bis-e nil)
(defvar adv-conjr06 nil)
(defvar adv-conjr07 nil)
(defvar adv-conjr08 nil)
(defvar adv-conjr09 nil)
(defvar adv-conjr10 nil)
(defvar adv-conjr11 nil)
(defvar adv-conjr12 nil)
(defvar adv-conjr13 nil)
(defvar adv-conjr14 nil)
(defvar adv-conjr15 nil)
(defvar adv-conjr16 nil)
(defvar adv-conjr17 nil)
;.....................................
(defvar adv-conj-prepr00e nil)		; ENGLISH: AS
(defvar adv-conj-prepr01e nil)		; ENGLISH: AS
(defvar adv-conj-prepr02 nil)
(defvar adv-conj-prepr03 nil)
(defvar adv-conj-prepr04 nil)
(defvar adv-conj-prepr05 nil)
(defvar adv-conj-prepr06e nil)		; ENGLISH: AS
;.....................................
(defvar adv-conj-prep-pronr01 nil)
(defvar adv-conj-prep-pronr02 nil)
(defvar adv-conj-prep-pronr02a nil)
(defvar adv-conj-prep-pronr03 nil)
(defvar adv-conj-prep-pronr04 nil)
(defvar adv-conj-prep-pronr05 nil)	; COME
(defvar adv-conj-prep-pronr06 nil)	; COME
(defvar adv-conj-prep-pronr07 nil)	; COME
(defvar adv-conj-prep-pronr08 nil)	; COME
(defvar adv-conj-prep-pronr09 nil)	; COME
;.....................................
;(defvar adv-conj-pronr01s nil)	; with a leading ¿ character (spanish)
(defvar adv-conj-pronr02 nil)	; DOVE, PERCHE'
(defvar adv-conj-pronr03 nil)
(defvar adv-conj-pronr04 nil)
(defvar adv-conj-pronr05 nil)
(defvar adv-conj-pronr06 nil)
(defvar adv-conj-pronr07 nil)
(defvar adv-conj-pronr08 nil)
(defvar adv-conj-pronr09 nil)
(defvar adv-conj-pronr10 nil)
(defvar adv-conj-pronr11 nil)
;.....................................
(defvar adv-nounr01i nil)
(defvar adv-nounr02 nil)
(defvar adv-nounr03 nil)
(defvar adv-nounr04 nil)
(defvar adv-nounr05 nil)
(defvar adv-nounr06i nil)
;.....................................
(defvar adv-noun-prepr01 nil)
(defvar adv-noun-prepr02 nil)
(defvar adv-noun-prepr03 nil)
(defvar adv-noun-prepr04c nil)
(defvar adv-noun-prepr05c nil)
(defvar adv-noun-prepr06i nil)	; ITALIAN
;.....................................
(defvar adv-noun-verbr01 nil)
(defvar adv-noun-verbr02e nil)
;.....................................
(defvar adv-phrasr01 nil)	; CATALAN "NO"
;.....................................
(defvar adv-phras-pronr01 nil)	; SPANISH "SI'"
;.....................................
(defvar adv-predet-pronr01 nil)
(defvar adv-predet-pronr02 nil)
;.....................................
(defvar adv-prepr01e nil)		; ENGLISH PARTICLES
(defvar adv-prepr02 nil)
(defvar adv-prepr03 nil)
(defvar adv-prepr04 nil)
(defvar adv-prepr05 nil)
(defvar adv-prepr06 nil)
(defvar adv-prepr07 nil)
(defvar adv-prepr08 nil)
(defvar adv-prepr09 nil)
(defvar adv-prepr10 nil)
(defvar adv-prepr11 nil)
(defvar adv-prepr12 nil)
;.....................................
(defvar adv-prep-verbr01 nil)
(defvar adv-prep-verbr02 nil)
(defvar adv-prep-verbr03 nil)
;.....................................
(defvar adv-pronr01e nil)		; ENGLISH
(defvar adv-pronr02e nil)		; ENGLISH
(defvar adv-pronr03s nil)		; SPANISH
;.....................................
(defvar art-noun-pronr01 nil)
;(defvar art-noun-pronr02 nil)
(defvar art-noun-pronr03 nil)
(defvar art-noun-pronr04 nil)
(defvar art-noun-pronr05 nil)
(defvar art-noun-pronr06 nil)
(defvar art-noun-pronr07 nil)
(defvar art-noun-pronr08 nil)
(defvar art-noun-pronr09 nil)
(defvar art-noun-pronr10 nil)
(defvar art-noun-pronr11 nil)
(defvar art-noun-pronr12 nil)
(defvar art-noun-pronr13 nil)
;.....................................
(defvar art-num-pronr01 nil)
(defvar art-num-pronr02 nil)
(defvar art-num-pronr03 nil)
;.....................................
(defvar art-noun-prep+artr01 nil)	; dei
(defvar art-noun-prep+artr02 nil)	; dei
;.....................................
(defvar art-prep+artr01 nil)		; delle, delle, degli, dello
;(defvar art-prep+artr02 nil)		; delle, delle, degli, dello
(defvar art-prep+artr03 nil)		; delle, delle, degli, dello
(defvar art-prep+artr04 nil)		; delle, delle, degli, dello
;.....................................
(defvar art-prep-pronr01 nil)
(defvar art-prep-pronr02 nil)
(defvar art-prep-pronr03 nil)
;.....................................
(defvar art-pronr01iesf nil)
(defvar art-pronr02 nil)
(defvar art-pronr03 nil)
(defvar art-pronr04 nil)
(defvar art-pronr05 nil)
(defvar art-pronr06 nil)
(defvar art-pronr07 nil)
(defvar art-pronr08 nil)
(defvar art-pronr09 nil)
;.....................................
(defvar art-pron-verbr01s nil)		; SPANISH
;.....................................
(defvar art-verbr01 nil)		; for English genitives
(defvar art-verbr02 nil)
;.....................................
(defvar conj-phras-pronr01i nil)	; ITALIAN
;.....................................
(defvar conj-prepr01 nil)
(defvar conj-prepr02 nil)
(defvar conj-prepr03 nil)
(defvar conj-prepr04 nil)		; ENGLISH
(defvar conj-prepr05 nil)		; ENGLISH
;.....................................
(defvar conj-pronr00 nil)		; come se
(defvar conj-pronr01 nil)		; CHI
(defvar conj-pronr02i nil)		; ITALIAN
(defvar conj-pronr03i nil)		; ITALIAN
(defvar conj-pronr04c nil)		; CATALAN
(defvar conj-pronr05c nil)		; CATALAN
(defvar conj-pronr06i nil)		; ITALIAN
(defvar conj-pronr07i nil)		; ITALIAN
;.....................................
(defvar conj-verbr01 nil)		; SIA
(defvar conj-verbr02 nil)		; SIA
(defvar conj-verbr03 nil)		; SIA
(defvar conj-verbr04 nil)		; SIA
(defvar conj-verbr05 nil)		; SIA
(defvar conj-verbr06 nil)		; SIA
(defvar conj-verbr07 nil)		; SIA
;.....................................
(defvar date-numr01 nil)
;.....................................
(defvar interj-noun-verbr01 nil)
;.....................................
(defvar noun-numr01 nil)
(defvar noun-numr02 nil)
(defvar noun-numr03 nil)
;.....................................
(defvar noun-num-pronr01e nil)	; ENGLISH "one"
(defvar noun-num-pronr02e nil)	; ENGLISH "one"
(defvar noun-num-pronr03e nil)	; ENGLISH "one"
(defvar noun-num-pronr04e nil)	; ENGLISH "one"
;.....................................
(defvar noun-phrasr01 nil)		; ENGLISH "YES"
;.....................................
(defvar noun-phras-prepr01 nil)		; ITALIAN "GRAZIE"
(defvar noun-phras-prepr02 nil)
(defvar noun-phras-prepr03 nil)
(defvar noun-phras-prepr04 nil)
;.....................................
(defvar noun-prepr01 nil)
(defvar noun-prepr02 nil)
;.....................................
(defvar noun-prep+artr01 nil)
;.....................................
(defvar noun-prep+art-verb+pronr01 nil)
;.....................................
(defvar noun-prep-verbr001i nil)	; ITALIAN (for "verso sera")
(defvar noun-prep-verbr01s nil)		; SPANISH
(defvar noun-prep-verbr02 nil)
(defvar noun-prep-verbr03 nil)
(defvar noun-prep-verbr04 nil)
(defvar noun-prep-verbr05 nil)
(defvar noun-prep-verbr06 nil)
(defvar noun-prep-verbr07 nil)
(defvar noun-prep-verbr08 nil)
(defvar noun-prep-verbr09 nil)
;.....................................
(defvar noun-pronr01 nil)		 ; COSA; NOTHING
(defvar noun-pronr02 nil)		 ; COSA; NOTHING
(defvar noun-pronr03 nil)		 ; COSA; NOTHING
(defvar noun-pronr04 nil)		 ; COSA; NOTHING
;.....................................
(defvar noun-verbr000e nil)		; ENGLISH "phone number"
(defvar noun-verbr001e nil)
(defvar noun-verbr002i nil)		; ITALIAN "v. paragrafo 3"
(defvar noun-verbr003i nil)		; ITALIAN "segnale radio"
(defvar noun-verbr004 nil)
(defvar noun-verbr01 nil)
(defvar noun-verbr02 nil)
(defvar noun-verbr03 nil)
(defvar noun-verbr04e nil)		; ENGLISH
(defvar noun-verbr05 nil)
(defvar noun-verbr06 nil)
(defvar noun-verbr07e nil)		; ENGLISH
(defvar noun-verbr08e nil)		; ENGLISH
(defvar noun-verbr09e nil)		; ENGLISH
(defvar noun-verbr10 nil)
(defvar noun-verbr11 nil)
(defvar noun-verbr12i nil)		; ITALIAN
(defvar noun-verbr13i nil)		; ITALIAN
(defvar noun-verbr14e nil)		; ENGLISH
(defvar noun-verbr15e nil)		; ENGLISH
(defvar noun-verbr16 nil)
(defvar noun-verbr17 nil)
(defvar noun-verbr18 nil)
(defvar noun-verbr19 nil)
(defvar noun-verbr20i nil)		; ITALIAN
(defvar noun-verbr21 nil)
(defvar noun-verbr22 nil)
(defvar noun-verbr23 nil)
(defvar noun-verbr24 nil)
(defvar noun-verbr25 nil)
(defvar noun-verbr26ie nil)		; ITALIAN ENGLISH
(defvar noun-verbr27s nil)		; SPANISH
(defvar noun-verbr28 nil)
(defvar noun-verbr29 nil)
(defvar noun-verbr30 nil)
(defvar noun-verbr31 nil)
(defvar noun-verbr32 nil)
(defvar noun-verbr33 nil)
(defvar noun-verbr34 nil)
(defvar noun-verbr35ics nil)		; ITALIAN CATALAN SPANISH
(defvar noun-verbr36ics nil)		; ITALIAN CATALAN SPANISH
(defvar noun-verbr37 nil)
(defvar noun-verbr38e nil)		; ENGLISH
(defvar noun-verbr39e nil)		; ENGLISH
(defvar noun-verbr40e nil)		; ENGLISH
(defvar noun-verbr41i nil)		; ITALIAN
(defvar noun-verbr42e nil)		; ENGLISH
(defvar noun-verbr43ics nil)		; ITALIAN CATALAN SPANISH
(defvar noun-verbr44e nil)		; ENGLISH
(defvar noun-verbr45e nil)		; ENGLISH
(defvar noun-verbr46e nil)		; ENGLISH
(defvar noun-verbr47 nil)
(defvar noun-verbr48 nil)
(defvar noun-verbr49 nil)
(defvar noun-verbr50 nil)
(defvar noun-verbr51 nil)
(defvar noun-verbr52 nil)
(defvar noun-verbr53 nil)
(defvar noun-verbr54 nil)
(defvar noun-verbr55 nil)
(defvar noun-verbr56 nil)
(defvar noun-verbr57e nil)
(defvar noun-verbr58 nil)
(defvar noun-verbr59 nil)
(defvar noun-verbr60 nil)
(defvar noun-verbr61 nil)
(defvar noun-verbr62 nil)
(defvar noun-verbr63 nil)
(defvar noun-verbr64 nil)
(defvar noun-verbr65e nil)		; ENGLISH
(defvar noun-verbr66 nil)
(defvar noun-verbr67 nil)
(defvar noun-verbr68e nil)		; ENGLISH
(defvar noun-verbr69es nil)		; ENGLISH
(defvar noun-verbr70 nil)
(defvar noun-verbr71 nil)
(defvar noun-verbr72 nil)
(defvar noun-verbr73e nil)		; ENGLISH
(defvar noun-verbr73ics nil)		; ITALIAN CATALAN SPANISH
(defvar noun-verbr74 nil)
(defvar noun-verbr75 nil)
(defvar noun-verbr76 nil)
(defvar noun-verbr77 nil)
(defvar noun-verbr78i nil)		; ITALIAN
(defvar noun-verbr79e nil)		; ENGLISH
(defvar noun-verbr80e nil)		; ENGLISH
(defvar noun-verbr81e nil)		; ENGLISH
(defvar noun-verbr82e nil)		; ENGLISH
(defvar noun-verbr83e nil)		; ENGLISH
(defvar noun-verbr84e nil)		; ENGLISH
(defvar noun-verbr85e nil)		; ENGLISH
(defvar noun-verbr86 nil)
(defvar noun-verbr87 nil)
(defvar noun-verbr88 nil)
(defvar noun-verbr89 nil)
(defvar noun-verbr90 nil)
(defvar noun-verbr91 nil)
(defvar noun-verbr92 nil)
(defvar noun-verbr93i nil)		; ITALIAN
(defvar noun-verbr94e nil)		; ENGLISH
(defvar noun-verbr95 nil)
;.....................................
(defvar noun-verb+pronr01 nil)
;.....................................
(defvar num-paragraph-nr01 nil)
;.....................................
(defvar num-verbr01 nil)
(defvar num-verbr02 nil)
(defvar num-verbr03 nil)
;.....................................
(defvar phras-pronr01i nil)		; ITALIAN
;.....................................
(defvar predet-pronr01 nil)
(defvar predet-pronr02 nil)
;.....................................
(defvar prep-verbr01 nil)
(defvar prep-verbr02 nil)
(defvar prep-verbr03 nil)
;.....................................
(defvar prep+art-verbr01 nil)
;.....................................
(defvar pron-verbr01 nil)

; ....... ; **** RULES INSIDE THE SAME CATEGORY ********
(defvar general-f-m-r01 nil)
(defvar general-f-m-r02 nil)
(defvar general-f-m-r03 nil)
(defvar general-f-m-r04 nil)
(defvar general-f-m-r05 nil)
(defvar general-f-m-r06 nil)
;.....................................
(defvar general-pl-sing-r01 nil)
(defvar general-pl-sing-r02 nil)
(defvar general-pl-sing-r03 nil)
(defvar general-pl-sing-r04 nil)
(defvar general-pl-sing-r05e nil)		; ENGLISH
(defvar general-pl-sing-r06e nil)		; ENGLISH
(defvar general-pl-sing-r07e nil)		; ENGLISH
(defvar general-pl-sing-r08e nil)		; ENGLISH
(defvar general-pl-sing-r08bis-e nil)		; ENGLISH
(defvar general-pl-sing-r08ter-e nil)		; ENGLISH
(defvar general-pl-sing-r09 nil)
(defvar general-pl-sing-r10 nil)
(defvar general-pl-sing-r11 nil)
(defvar general-pl-sing-r12 nil)
(defvar general-pl-sing-r13 nil)
(defvar general-pl-sing-r14 nil)
(defvar general-pl-sing-r15 nil)
;.....................................
(defvar adj-indef-qualif-r01 nil)
(defvar adj-indef-qualif-r02 nil)
;.....................................
(defvar adj-poss-qualif-r01 nil)
;.....................................
(defvar adv-compar-time-r01 nil)
(defvar adv-compar-time-r02 nil)
(defvar adv-compar-time-r03 nil)
;.....................................
(defvar adv-loc-compar-r01 nil)
(defvar adv-loc-compar-r02 nil)
;.....................................
(defvar adv-quant-time-r01 nil)
(defvar adv-quant-time-r02 nil)
;.....................................
(defvar art-f-m-r01 nil)
(defvar art-f-m-r02 nil)
(defvar art-f-m-r03 nil)
;.....................................
(defvar conj-coord-subord-r01 nil)
(defvar conj-coord-subord-r02 nil)
(defvar conj-coord-subord-r03 nil)
;.....................................
(defvar conj-compar-coord-subord-r01 nil)
;.....................................
;(defvar noun-common-proper-r000 nil)
(defvar noun-common-proper-r001 nil)
(defvar noun-common-proper-r002 nil)
(defvar noun-common-proper-r003 nil)
(defvar noun-common-proper-r00 nil)
(defvar noun-common-proper-r01 nil)
(defvar noun-common-proper-r02 nil)
(defvar noun-common-proper-r03 nil)
(defvar noun-common-proper-r04 nil)
(defvar noun-common-proper-r05 nil)
(defvar noun-common-proper-r06 nil)
(defvar noun-common-proper-r07 nil)
(defvar noun-common-proper-r08 nil)
(defvar noun-common-proper-r09e nil)
(defvar noun-common-proper-r10 nil)
(defvar noun-common-proper-r11 nil)
(defvar noun-common-proper-r12 nil)
(defvar noun-common-proper-r13 nil)
(defvar noun-common-proper-r14 nil)
(defvar noun-common-proper-r15h nil)		; HINDI
(defvar noun-common-proper-r16 nil)
(defvar noun-common-proper-r17 nil)
;.....................................
(defvar pron-exclam-interr-relat-r01 nil)
(defvar pron-exclam-interr-relat-r02 nil)
(defvar pron-exclam-interr-relat-r03i nil)	; ITALIAN
(defvar pron-exclam-interr-relat-r04e nil)	; ENGLISH
(defvar pron-exclam-interr-relat-r05 nil)
(defvar pron-exclam-interr-relat-r06 nil)
(defvar pron-exclam-interr-relat-r07 nil)
;.....................................
(defvar pron-demons-relat-r01 nil)
;.....................................
(defvar pron-indef-relat-r01 nil)
(defvar pron-indef-relat-r02e nil)		; ENGLISH
;.....................................
(defvar pron-interr-relat-r01e nil)	; ENGLISH
(defvar pron-interr-relat-r02e nil)
(defvar pron-interr-relat-r03 nil)
(defvar pron-interr-relat-r04 nil)
;.....................................
(defvar pron-pers-loc-r01 nil)
(defvar pron-pers-loc-r02 nil)
(defvar pron-pers-loc-r03 nil)
(defvar pron-pers-loc-r04 nil)
;.....................................
(defvar pron-clitic-r01 nil)
(defvar pron-clitic-r02 nil)
;.....................................
(defvar verb-aux-main-r0001 nil)		; for ATLAS
(defvar verb-aux-main-r0002 nil)		; for ATLAS
(defvar verb-aux-main-r01 nil)
(defvar verb-aux-main-r02ics nil)	; ITALIAN CATALAN SPANISH
(defvar verb-aux-main-r02e nil)		; ENGLISH
(defvar verb-aux-main-r03ics nil)	; ITALIAN CATALAN SPANISH
(defvar verb-aux-main-r03e nil)		; ENGLISH
(defvar verb-aux-main-r04 nil)
(defvar verb-aux-main-r05ics nil)	; ITALIAN CATALAN SPANISH
(defvar verb-aux-main-r06e nil)		; ENGLISH
(defvar verb-aux-main-r07e nil)		; ENGLISH
(defvar verb-aux-main-r08 nil)
(defvar verb-aux-main-r09i nil)		; ITALIAN
(defvar verb-aux-main-r10e nil)		; ENGLISH
(defvar verb-aux-main-r11i nil)		; ITALIAN
(defvar verb-aux-main-r12 nil)
(defvar verb-aux-main-r13e nil)		; ENGLISH
(defvar verb-aux-main-r14e nil)		; ENGLISH
(defvar verb-aux-main-r15 nil)
(defvar verb-aux-main-r16 nil)
(defvar verb-aux-main-r17 nil)
;.....................................
(defvar verb-main-mod-r01 nil)
(defvar verb-main-mod-r02e nil) 		; ENGLISH
(defvar verb-main-mod-r03 nil)
(defvar verb-main-mod-r04 nil)
;.....................................
(defvar verb-1-2-3-r01 nil)
(defvar verb-1-2-3-r02 nil)
(defvar verb-1-2-3-r03 nil)
(defvar verb-1-2-3-r04 nil)
;.....................................
(defvar verb-1-3-r01 nil)
(defvar verb-1-3-r02 nil)
(defvar verb-1-3-r03 nil)
(defvar verb-1-3-r04 nil)
;.....................................
(defvar verb-fin-inf-r01 nil)
(defvar verb-fin-inf-r02 nil)
(defvar verb-fin-inf-r03 nil)
(defvar verb-fin-inf-r04e nil)		; ENGLISH
(defvar verb-fin-inf-r05e nil)		; ENGLISH
(defvar verb-fin-inf-r06e nil)		; ENGLISH
(defvar verb-fin-inf-r06bis-e nil)		; ENGLISH
(defvar verb-fin-inf-r07e nil)		; ENGLISH
(defvar verb-fin-inf-r08ics nil)		; ITALIAN CATALAN SPANISH
(defvar verb-fin-inf-r09e nil)		; ENGLISH
(defvar verb-fin-inf-r10e nil)		; ENGLISH
(defvar verb-fin-inf-r11i nil)		; ITALIAN
(defvar verb-fin-inf-r12i nil)		; ITALIAN
(defvar verb-fin-inf-r13i nil)		; ITALIAN
(defvar verb-fin-inf-r14 nil)
(defvar verb-fin-inf-r15 nil)
(defvar verb-fin-inf-r16 nil)
(defvar verb-fin-inf-r17 nil)
(defvar verb-fin-inf-r17bis-e nil)
(defvar verb-fin-inf-r17ter-e nil)
(defvar verb-fin-inf-r18 nil)
(defvar verb-fin-inf-r19 nil)
(defvar verb-fin-inf-r20 nil)
(defvar verb-fin-inf-r21e nil)		; ENGLISH
(defvar verb-fin-inf-r22e nil)		; ENGLISH
(defvar verb-fin-inf-r23e nil)		; ENGLISH
(defvar verb-fin-inf-r24e nil)		; ENGLISH
;.....................................
(defvar verb-cong-imper-ind-r01 nil)
(defvar verb-cong-imper-ind-r02i nil)	; ITALIAN
(defvar verb-cong-imper-ind-r03e nil)	; ENGLISH
(defvar verb-cong-imper-ind-r04e nil)	; ENGLISH
(defvar verb-cong-imper-ind-r05e nil)	; ENGLISH
(defvar verb-cong-imper-ind-r06e nil)	; ENGLISH
(defvar verb-cong-imper-ind-r07e nil)	; ENGLISH
(defvar verb-cong-imper-ind-r08e nil)	; ENGLISH
(defvar verb-cong-imper-ind-r09 nil)
;.....................................
(defvar verb-imper-ind-r01 nil)
(defvar verb-imper-ind-r02 nil)
(defvar verb-imper-ind-r03 nil)
;.....................................
(defvar verb-gerund-participle-r01 nil)  ; ENGLISH
(defvar verb-gerund-participle-r02 nil)  ; ENGLISH
(defvar verb-gerund-participle-r03 nil)  ; ENGLISH FRENCH
(defvar verb-gerund-participle-r04 nil)  ; ENGLISH FRENCH
;.....................................
(defvar verb-infinite-participle-r01 nil) ; ENGLISH: found, come, become 
(defvar verb-infinite-participle-r02 nil) ; ENGLISH: found, come, become
(defvar verb-infinite-participle-r03 nil) ; ENGLISH: found, come, become
;.....................................
(defvar atlas-locut-r01 nil) 		; ATLAS: locution "un po'"

; ....... ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; *** before loading the file, possible previous definitions are cancelled
(delete-rules)

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; 	LIST OF AMBIGUITIES
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;    272 rules for inter-category ambiguities
;*** adj V adv                                                     ---> 9 rules
;*** adj V adv V noun [MEGLIO, PEGGIO, BENE, MALE]                 ---> 5 rules
;*** adj V adv V noun V prep                                       ---> 1 rule
;*** adj V adv V noun V prep V pron [PRIMA]                        ---> 3 rules
;*** adj V adv V prep [LONTANO]                                    ---> 2 rules
;*** adj V adv V prep V pron [PIU']                                ---> 3 rules
;*** adj V adv V pron [PROPRIO, MOLTO, TANTO]                      ---> 5 rules
;*** adj V conj V pron [CHE]                                       ---> 15 rules
;*** adj V noun                                                    ---> 30 rules
;*** adj V noun V prep V pron [SECONDO]                            ---> 5 rules
;*** adj V noun V pron [PRIME]                                     ---> 0 rules
;*** adj V noun V verb [QUADRATO, CARICA, CHIUSA, VIVI, ...]       ---> 14 rules
;*** adj V noun V verb+pron [TENTATIVI, CAPITALE, ANIMALI]         ---> 2 rule
;*** adj V prep [FINO]                                             ---> 3 rules
;*** adj V prep V verb                                             ---> 3 rules
;*** adj V pron [QUESTO, QUELLO, ALTRO, ALCUNI, NESSUNO, SUO]      ---> 19 rules
;*** adj V pron V verb [STESSE, STESSI]                            ---> 6 rules
;*** adj V verb [TENERE, FA, SCORSO]                               ---> 23 rules
;*** adj V verb V verb+pron [SCONTATI]                             ---> 0 rules
;*** adj V verb+pron [CAMPALE, FIGURATIVI]                         ---> 0 rules
;*** adv V conj [INOLTRE, PERO`, APPENA, QUANDO]                   ---> 13 rules
;*** adv V conj V prep [DOPO]                                      ---> 2 rules
;*** adv V conj V prep V pron [COME]                               ---> 4 rules
;*** adv V conj V pron [DOVE, PERCHE']                             ---> 6 rules
;*** adv V noun [COSA, BENE, VIA, ORA]                             ---> 4 rules
;*** adv V noun V prep [INSIEME, INTORNO, TRAMITE]                 ---> 2 rules
;*** adv V noun V prep V verb [INCONTRO]                           ---> 0 rules
;*** adv V noun V verb [ANCORA, APPUNTO]                           ---> 1 rule
;*** adv V predet V pron [TUTTO]                                   ---> 2 rules
;*** adv V prep [INVECE, ACCANTO, ADDOSSO]                         ---> 7 rules
;*** adv V prep V verb [MENO, ATTRAVERSO]                          ---> 2 rules
;*** adv V verb [PRESTO, SUBITO]                                   ---> 0 rules
;*** art V noun V pron [LA]                                        ---> 7 rules
;*** art V num V pron [UNO]                                        ---> 0 rules
;*** art V pron [LO, LA, GLI, UNA, ...]                            ---> 6 rules
;*** conj V prep  [A]                                              ---> 1 rule
;*** conj V prep V verb [DI]                                       ---> 2 rule
;*** conj V pron [SE]                                              ---> 3 rules
;*** conj V verb [SIA]                                             ---> 2 rules
;*** noun V num [TRECENTO, ...]                                    ---> 2 rules
;*** noun V prep [TRAMITE]                                         ---> 1 rule
;*** noun V prep+art [NEI, DEI]                                    ---> 1 rule
;*** noun V prep V verb [RIGUARDO, RISPETTO, VERSO]                ---> 5 rules
;*** noun V verb                                                   ---> 43 rules
;*** noun V verb+pron [CAMPANE]                                    ---> 1 rule
;*** noun V verb V verb+pron [RISULTATI]                           ---> 0 rules
;*** num V verb [SEI]                                              ---> 3 rules
;*** predet V pron [TUTTI, AMBEDUE, ENTRAMBI]                      ---> 1 rule
;*** prep V verb                                                   ---> 2 rule
;*** pron V verb [COLORO]                                          ---> 1 rule
;*** verb V verb+pron [SFOLLATI, GUARDATI]                         ---> 0 rules 
;########################################################################
; 	WITHIN-CATEGORY AMBIGUITIES
;    62 rules for intra-category ambiguities
;*** general: male V female                                        ---> 6 rules
;*** general: singular V plural                                    ---> 4 rules
;*** adj: indef V qualif                                           ---> 2 rules
;*** adj: poss V qualif                                            ---> 1 rule
;*** adv; compar V time [PIU']                                     ---> 3 rules
;*** adv; loc V compar                                             ---> 2 rules
;*** adv; quant V time                                             ---> 2 rules
;*** art: male V female                                            ---> 0 rules
;*** conj: coord V subord                                          ---> 2 rules
;*** noun: common V proper                                         ---> 5 rules
;*** pron: exclam V interr V relat [CHI]                           ---> 5 rules
;*** pron: indef V relat [CHIUNQUE]                                ---> 1 rules
;*** pron: pers V loc                                              ---> 4 rules
;*** pron: pers V clitic                                           ---> 2 rules
;*** verb: aux V main                                              ---> 8 rules
;*** verb: aux+pron V main+pron                                    ---> 0 rules
;*** verb: modals V main                                           ---> 3 rules
;*** verb: person 1 V 2 V 3                                        ---> 2 rules
;*** verb: finite V infinite                                       ---> 9 rules
;*** verb: indicative V imperative                                 ---> 1 rules

(defstruct lexdisr
	(if nil)
	(then nil)
	(CF nil)
	(lang nil))

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;       REMINDER: to extract a field value from a structure:
;	(structurename-fieldname instance)
;	es: (setq antec (lexdisr-if adj-nounr1))
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;########################################################################
;		THE RULES
;########################################################################

;*** adj V adv (default: adv) [CERTO, SOLO; NEXT]

(putrule 'adj-adv
	'(adj-advr01 adj-advr02i adj-advr03 adj-advr04 adj-advr05i adj-advr06e
          adj-advr07 adj-advr08i adj-advr09 adj-advr10e adj-advr11e)
	'lexdisrules
	'(adj adv))

(putrule 'adj-adv 'adv 'defaultc)

;......................................................

(setq adj-advr01			; e' Certo che
   (make-lexdisr
	:if '(and (prevword-typ '&to-be)		; essere
                  (nextword-typ '&prep-subord-dep))	; che
	:then 'adj
	:CF 'A))
 
(setq adj-advr02i
   (make-lexdisr
	:if '(and (currword-typ '&indef-adj)		; certo
                  (prevcat-agr 'noun 'adj))
	:then 'adj
	:CF 'A
        :lang 'italian))

(setq adj-advr03			; uno Solo di
   (make-lexdisr
	:if '(and (currword-typ '&only)			; solo
                  (prevcat 'pron)
		  (nextword-typ '&partitive-prep))	; di
	:then 'adj
	:CF 'A))
 
(setq adj-advr04
   (make-lexdisr
	:if '(and (prevcat 'art)
                  (not (nextcat 'adj)))
	:then 'adj
	:CF 'A))
 
(setq adj-advr05i
   (make-lexdisr
	:if '(nextcat 'adj)
	:then 'adv
	:CF 'A
        :lang 'italian))
     
(setq adj-advr06e			; does not apply to "the next one"
   (make-lexdisr
	:if '(and (nextcat 'adj)
                  (not (nextcat 'pron)))
	:then 'adv
	:CF 'A
        :lang 'english))
     
(setq adj-advr07
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (nextcat-agr 'noun 'adj))
	:then 'adj
	:CF 'A))

(setq adj-advr08i
   (make-lexdisr
	:if '(and (prevcat-agr 'noun 'adj)
                  (nextcat 'punct)
                  (not (and (nextchar #\-)
                            (next2cat 'adj))))
	:then 'adj
	:CF 'A
        :lang 'italian))

(setq adj-advr09
   (make-lexdisr
	:if '(and (nextcat-agr 'noun 'adj)	; care must be paid for "non solo la"
                  (not (nextcat '(art adj (type indef)))))
	:then 'adj
	:CF 'U))

(setq adj-advr10e
   (make-lexdisr
	:if '(and (currword-typ '&only)
                  (or (prevword-typ '&to-be)
	              (and (prev2word-typ '&to-be)
                           (prevcat 'adv))))
	:then 'adv
	:CF 'A
        :lang 'english))
 
(setq adj-advr11e
   (make-lexdisr
	:if '(and (not (currword-typ '&only))
                  (or (prevword-typ '&to-be)
	              (and (prev2word-typ '&to-be)
                           (prevcat 'adv))))
	:then 'adj
	:CF 'A
        :lang 'english))
 
;******************************************************
;*** adj V adv V noun [MEGLIO, PEGGIO, BENE, MALE, NEO; ENGLISH: LITTLE]
;    (default: adj)

(putrule 'adj-adv-noun
	'(adj-adv-nounr01 adj-adv-nounr02 adj-adv-nounr03e adj-adv-nounr04e
          adj-adv-nounr05 adj-adv-nounr06 adj-adv-nounr07 adj-adv-nounr08)
	'lexdisrules
	'(adj noun adv))

(putrule 'adj-adv-noun 'adj 'defaultc)

;......................................................

(setq adj-adv-nounr01
   (make-lexdisr
	:if '(and (currword-typ '&aj-av-n-plan)		; piano
	          (nextcat 'adj))
	:then 'noun
	:CF 'A))

(setq adj-adv-nounr02		; il neo eletto
   (make-lexdisr
	:if '(and (currword-typ '&aj-av-n-neo)	; neo
	          (nextcat '(adj verb (mood participle))))
	:then 'adv
	:CF 'A))

(setq adj-adv-nounr03e
   (make-lexdisr
	:if '(and (prevcat+ '(adj conj art prep))
                  (nextcat-agr 'noun 'adj))
	:then 'adj
	:CF 'A
        :lang 'english))

(setq adj-adv-nounr04e
   (make-lexdisr
	:if '(and (prevcat+ '(adj conj art prep))
                  (nextcat 'conj)
                  (next2cat 'adj)
                  (not (next2cat 'prep)))  ; to avoid " ... and In ..."
	:then 'adj
	:CF 'A
        :lang 'english))

(setq adj-adv-nounr05
   (make-lexdisr
	:if '(and (prevcat+ '(adj conj art prep))
                  (not (nextcat-agr 'noun 'adj))
	     	  (not (and (nextchar #\-)
	     	            (next2cat-agr 'noun 'adj)))		; Super - perspective
                  (not (currtype 'indef)))	; Little more
	:then 'noun
	:CF 'A))

(setq adj-adv-nounr06
   (make-lexdisr
	:if '(and (prev2cat '(noun adj))	; l'ospedale più Vicino
						; l'evento teatrale piu' vicino
                  (prevcat 'adv)
                  (prevtype 'compar))
	:then 'adj
	:CF 'A))

(setq adj-adv-nounr07
   (make-lexdisr
	:if '(and (prevcat+ '(adv verb nil))
                  (not (nextcat 'noun)))
	:then 'adv
	:CF 'A))

(setq adj-adv-nounr08
   (make-lexdisr
	:if '(and (nextcat 'adv)
                  (currtype 'quant))
	:then 'adv
	:CF 'A))

;******************************************************
;*** adj V adv V noun V phras [ENGLISH "NO"]
;    (default: phras)

(putrule 'adj-adv-noun-phras
         '(adj-adv-noun-phrasr01 adj-adv-noun-phrasr02 adj-adv-noun-phrasr03)
         'lexdisrules
         '(adj adv noun phras))

(putrule 'adj-adv-noun-phras 'phras 'defaultc)

;......................................................

(setq adj-adv-noun-phrasr01
   (make-lexdisr
	:if '(nextcat '(noun pron))
	:then 'adj
	:CF 'A))

(setq adj-adv-noun-phrasr02
   (make-lexdisr
	:if '(and (nextcat 'adj)
	          (next2cat 'noun))
	:then 'adj
	:CF 'A))

(setq adj-adv-noun-phrasr03
   (make-lexdisr
	:if '(prevcat 'art)
	:then 'noun
	:CF 'A))

;******************************************************
;*** adj V adv V noun V prep [VICINO; OVER]
;    (default: adv)

(putrule 'adj-adv-noun-prep
         '(adj-adv-noun-prepr01		;prep
           adj-adv-noun-prepr02e	;prep
           adj-adv-nounr03e		;adj
           adj-adv-nounr05		;noun
           adj-adv-nounr06 		;adv
           adj-adv-noun-prepr03e	;adv
           adj-adv-noun-prepr04		;adv
           adj-adv-noun-prepr05e	;prep
           adj-adv-noun-prepr06)	;adj
         'lexdisrules
         '(adj adv noun prep))

(putrule 'adj-adv-noun-prep 'adj 'defaultc)

;......................................................

(setq adj-adv-noun-prepr01
   (make-lexdisr
	:if '(and (not (prevcat-agr 'art 'adj))
                  (not (prevcat 'adv))		; più vicino a 
                  (nextword-typ '&near-prep))	; a
	:then 'prep
	:CF 'A))

(setq adj-adv-noun-prepr02e
   (make-lexdisr
	:if '(and (prevcat 'verb)		; accumulated Over generations
                  (nextcat '(art noun predet)))
	:then 'prep
	:CF 'A
        :lang 'english))

(setq adj-adv-noun-prepr03e
   (make-lexdisr
	:if '(and (nextcat 'verb)		; nutrients over stimulate
                  (prevcat 'noun))
	:then 'adv
	:CF 'A
        :lang 'english))

(setq adj-adv-noun-prepr04
   (make-lexdisr
	:if '(and (prevcat+ '(adv verb nil))
                  (not (nextcat 'noun))
                  (not (nextword-typ '&near-prep))	; a
                  (not (currword-typ '&pref-prep-adj)))	    ; this for English "in"
	:then 'adv
	:CF 'A))

(setq adj-adv-noun-prepr05e
   (make-lexdisr
	:if '(currword-typ '&pref-prep-adj)	 	   ; this for English "in"
	:then 'prep
	:CF 'A))

(setq adj-adv-noun-prepr06		; il nostro VICINO sconosciuto
   (make-lexdisr
	:if '(and (prevcat '(adj (type poss)))
                  (nextcat 'adj))
	:then 'noun
	:CF 'A))

;******************************************************
;*** adj V adv V noun V pron [Ita: NIENTE, Eng: MORE, FIRST]
;    (default: adv)

(putrule 'adj-adv-noun-pron
         '(adj-adv-noun-pronr01		;adv
           adj-adv-noun-pronr02		;adj
           adj-adv-noun-pronr03		;adj
           adj-adv-noun-pronr04)	;adj
         'lexdisrules
         '(adj adv noun pron))

(putrule 'adj-adv-noun-pron 'pron 'defaultc)

;......................................................

(setq adj-adv-noun-pronr01
   (make-lexdisr
	:if '(nextcat 'adv)
	:then 'pron
	:CF 'A))

(setq adj-adv-noun-pronr02
   (make-lexdisr
	:if '(nextcat 'noun)
	:then 'adj
	:CF 'A))

(setq adj-adv-noun-pronr03		;  April first
   (make-lexdisr
	:if '(and (currtype 'ordin)
                  (prevsemtype '£month))
	:then 'adj
	:CF 'A))

(setq adj-adv-noun-pronr04		;  April first
   (make-lexdisr
	:if '(and (currtype 'ordin)
                  (or (nextcat 'noun)
                      (and (nextcat 'adj) (next2cat 'noun))))
	:then 'adj
	:CF 'A))

;******************************************************
;*** adj V adv V noun V prep V verb [OUT]
;    (default: adj)

(putrule 'adj-adv-noun-prep-verb
         '(adj-adv-noun-prep-verbr01e	;prep
           adj-adv-noun-prep-verbr02e	;adv
           adj-adv-noun-prep-verbr03e)	;adv
         'lexdisrules
         '(adj adv noun verb))

(putrule 'adj-adv-noun-prep-verb 'adj 'defaultc)

;......................................................

(setq adj-adv-noun-prep-verbr01e		; to be OUT of the
   (make-lexdisr
	:if '(prep-govern-next)
	:then 'prep
	:CF 'A
        :lang 'english))

(setq adj-adv-noun-prep-verbr02e
   (make-lexdisr
	:if '(prevcat 'verb)
	:then 'adv
	:CF 'A
        :lang 'english))

(setq adj-adv-noun-prep-verbr03e	; adequate conceptions of god STILL portray
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (nextcat 'verb))
	:then 'adv
	:CF 'A
        :lang 'english))

;******************************************************
;*** adj V adv V noun V verb [OUT, LEFT, FORWARD, DOWN, BETTER]
;    (default: adj)

(putrule 'adj-adv-noun-verb
         '(adj-adv-noun-verbr01		;verb
           adj-adv-noun-verbr02		;prep
           adj-adv-noun-verbr03e	;adv
           adj-adv-noun-verbr04		;verb
           adj-adv-noun-verbr05)	;verb
         'lexdisrules
         '(adj adv noun verb))

(putrule 'adj-adv-noun-verb 'adj 'defaultc)

;......................................................

(setq adj-adv-noun-verbr01
   (make-lexdisr
	:if '(and (prevcat '(verb (type aux)))
                  (currmood+tense 'participle 'past))
	:then 'verb
	:CF 'A))

(setq adj-adv-noun-verbr02
   (make-lexdisr
	:if '(prevcat 'verb)
	:then 'adv
	:CF 'A))

(setq adj-adv-noun-verbr03e	; adequate conceptions of god STILL portray
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (nextcat 'verb))
	:then 'adv
	:CF 'A
        :lang 'english))

(setq adj-adv-noun-verbr04		; I left some
   (make-lexdisr
	:if '(and (prevcat 'pron)
                  (nextcat 'adj))
	:then 'verb
	:CF 'A))

(setq adj-adv-noun-verbr05		; furniture left 
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (currmood 'participle)  ; to block "the way forward."
                  (or (not (nextcat '(adj art noun verb)))
                      (nextword-typ '&loc-prep-1)))		; to include also "in"
	:then 'verb
	:CF 'A))

;******************************************************
;*** adj V adv V noun V prep V pron [PRIMA]
;    (default: adv)
; *** PRIMA noun never chosen

(putrule 'adj-adv-noun-prep-pron
         '(adj-adv-noun-prep-pronr01
           adj-adv-noun-prep-pronr02
           adj-adv-noun-prep-pronr03
           adj-adv-noun-prep-pronr04)
         'lexdisrules
         '(adj adv noun prep pron))

(putrule 'adj-adv-noun-prep-pron 'adv 'defaultc)

(setq adj-adv-noun-prep-pronr01
   (make-lexdisr
	:if '(and (not (prevcat-agr 'art 'adj))
                  (nextword-typ '&compar-prep-2))
	:then 'prep
	:CF 'A))

(setq adj-adv-noun-prep-pronr02
  (make-lexdisr
    :if '(and (or (prevcat-agr 'art 'adj)
                  (and (prevcat-agr 'adj 'adj)
                       (prev2cat 'art)))
              (nextcat-agr '(adj noun) 'adj))
	:then 'adj
	:CF 'A))

(setq adj-adv-noun-prep-pronr03
   (make-lexdisr
	:if '(and (prevcat-agr 'art 'adj)
                  (not (nextcat-agr '(adj noun) 'adj)))
	:then 'pron
	:CF 'A))

(setq adj-adv-noun-prep-pronr04
  (make-lexdisr
    :if '(and (prevcat 'prep)
              (nextcat-agr '(adj noun) 'adj))
	:then 'adj
	:CF 'A))

;******************************************************
;*** adj V adv V predet [E: SUCH]
;    (default: adj)

(putrule 'adj-adv-predet
	'(adj-adv-predetr01e adj-adv-predetr02e)
	'lexdisrules
	'(adj adv predet))

(putrule 'adj-adv-predet 'adv 'defaultc)

;......................................................

(setq adj-adv-predetr01e
   (make-lexdisr
	:if '(nextcat 'art)
	:then 'predet
	:CF 'C))

(setq adj-adv-predetr02e
   (make-lexdisr
	:if '(nextcat '(adv adj noun pron num))
	:then 'adj
	:CF 'A))

;******************************************************
;*** adj V adv V prep [I: LONTANO; E: THROUGH]
;    (default: adj)

(putrule 'adj-adv-prep
	'(adj-adv-prepr01e adj-adv-prepr02 adj-adv-prepr03 adj-adv-prepr04 
          adj-adv-prepr05 adj-adv-prepr06 adj-adv-prepr07)
	'lexdisrules
	'(adj adv prep))

(putrule 'adj-adv-prep 'adv 'defaultc)

;......................................................

(setq adj-adv-prepr01e		; ENGLISH PARTICLES
   (make-lexdisr
	:if '(verb-govern-adv)
            ; *** checks if the immediately preceding verb can govern the
            ;     current word (as a particle)
	:then 'adv
	:CF 'A
	:lang 'english))

(setq adj-adv-prepr02
   (make-lexdisr
	:if '(currword-typ '&pref-prep-adj)
	:then 'prep
	:CF 'A))

(setq adj-adv-prepr03
   (make-lexdisr
	:if '(and (not (prevcat-agr 'art 'adj))
                  (nextword-typ '&far-prep))
	:then 'prep
	:CF 'C))

(setq adj-adv-prepr04
   (make-lexdisr
	:if '(and (not (currword-typ '&pref-prep-adj))
                  (prevcat-agr '(art noun) 'adj))
	:then 'adj
	:CF 'A))

(setq adj-adv-prepr05
   (make-lexdisr
	:if '(and (not (currword-typ '&pref-prep-adj))
	          (nextcat-agr '(adj noun) 'adj))
	:then 'adj
	:CF 'A))

(setq adj-adv-prepr06
   (make-lexdisr
	:if '(nextcat 'art)
	:then 'prep
	:CF 'A))

(setq adj-adv-prepr07
   (make-lexdisr
	:if '(and (nextcat 'adj)
	          (next2cat 'noun))
	:then 'prep
	:CF 'A))

;******************************************************
;*** adj V adv V prep V pron [PIU']
;    (default: adv)

(putrule 'adj-adv-prep-pron
	'(adj-adv-prep-pronr01 adj-adv-prep-pronr02 adj-adv-prep-pronr03 
	  adj-adv-prep-pronr04 adj-adv-prep-pronr05 adj-adv-pronr04
          adj-adv-pronr05 adj-adv-pronr06 adj-adv-pronr02 adj-adv-pronr07
          adj-adv-pronr08 adj-adv-pronr09 adj-adv-pronr11)
	'lexdisrules
	'(adj adv prep pron))

(putrule 'adj-adv-prep-pron 'adv 'defaultc)

;......................................................

(setq adj-adv-prep-pronr01	; uno o piu'
   (make-lexdisr
	:if '(and (prevcat '(conj (type coord)))
		  (prev2word-typ '&one))
	:then 'pron
	:CF 'A))
 
(setq adj-adv-prep-pronr02	; uno o piu'
   (make-lexdisr
	:if '(and (prevcat '(conj (type coord)))
		  (prev2cat 'num)
                  (nextcat 'noun))
	:then 'adj
	:CF 'A))
 
(setq adj-adv-prep-pronr03
   (make-lexdisr
	:if '(and (nextword-typ '&compar-prep-2)
                  (or (not (next2cat 'verb))
                      (not (next2mood 'infinite))))
	:then 'pron
	:CF 'A))

(setq adj-adv-prep-pronr04
   (make-lexdisr
	:if '(and (nextword-typ '&compar-prep-2)
                  (next2cat 'verb)
                  (next2mood 'infinite))
	:then 'prep
	:CF 'A))

(setq adj-adv-prep-pronr05
   (make-lexdisr
	:if '(and (prevcat '(verb (type aux)))
                  (nextcat 'verb)
                  (nextmood 'participle))
	:then 'adv
	:CF 'A))

;******************************************************
;*** adj V adv V prep V verb [NEAR UP]
;    (default: adv)

(putrule 'adj-adv-prep-verb
	'(adj-adv-prep-verbr01 adj-adv-prep-verbr02 adj-adv-prep-verbr03)
	'lexdisrules
	'(adj adv prep verb))

(putrule 'adj-adv-prep-verb 'adv 'defaultc)

;......................................................

(setq adj-adv-prep-verbr01	; near the house
   (make-lexdisr
	:if '(and (not (verb-govern-adv))
                  (nextcat '(noun art adj)))
	:then 'prep
	:CF 'A))

(setq adj-adv-prep-verbr02	; near the house
   (make-lexdisr
	:if '(and (prevcat '(noun verb))
	          (nextcat 'verb))
	:then 'adv
	:CF 'A))

(setq adj-adv-prep-verbr03		; ENGLISH PARTICLES
   (make-lexdisr
	:if '(verb-govern-adv)
            ; *** checks if the immediately preceding verb can govern the
            ;     current word (as a particle)
	:then 'adv
	:CF 'A
	:lang 'english))

;******************************************************
;*** adj V adv V prep V pron V verb [MENO]
;    (default: adv)
; !! MENO verb is never chosen

(putrule 'adj-adv-prep-pron-verb
	'(adj-adv-prep-pronr01 adj-adv-prep-pronr02 adj-adv-prep-pronr03 
	  adj-adv-prep-pronr04 adj-adv-prep-pronr05 adj-adv-pronr02
          adj-adv-pronr03 adj-adv-pronr05 adj-adv-pronr06
          adj-adv-pronr07 adj-adv-pronr08 adj-adv-pronr09)
	'lexdisrules
	'(adj adv prep pron verb))

(putrule 'adj-adv-prep-pron-verb 'adv 'defaultc)

;......................................................

;******************************************************
;*** adj V adv V pron (default: adv) [PROPRIO, MOLTO, TANTO; ENGLISH MORE]

(putrule 'adj-adv-pron
	'(adj-adv-pronr00
	  adj-adv-pronr01 adj-adv-pronr02 adj-adv-pronr03 adj-adv-pronr04
	  adj-adv-pronr05 adj-adv-pronr06 adj-adv-pronr07 adj-adv-pronr08
	  adj-adv-pronr09 adj-adv-pronr10e adj-adv-pronr11 adj-adv-pronr12)
	'lexdisrules
	'(adj adv pron))

(putrule 'adj-adv-pron 'adv 'defaultc)

;......................................................

(setq adj-adv-pronr00		; una vera e PROPRIA sfortuna
   (make-lexdisr
	:if '(and (currtype 'poss)
		  (prev2cat 'adj)
		  (prevcat 'conj)
		  (nextcat-agr 'noun 'adj))
	:then 'adj
	:CF 'A))
 
(setq adj-adv-pronr01
   (make-lexdisr
	:if '(and (currtype 'poss)
		  (prevcat-agr '(noun adj) 'adj)
		  (nextcat '(conj (type coord))))
	:then 'adj
	:CF 'A))
 
(setq adj-adv-pronr02
   (make-lexdisr
	:if '(and (not (prevcat '(art prep num)))
                  (not (nextcat '(conj (type compar) conj (type coord) 
                                  prep punct))))
	:then 'adv
	:CF 'A))
 
(setq adj-adv-pronr03
   (make-lexdisr
	:if '(and (prevcat '(art noun))	; il proprio diritto
                  (currword-typ '&own)
                  (nextcat 'noun))  
	:then 'adj
	:CF 'A))
 
(setq adj-adv-pronr04
   (make-lexdisr
	:if '(and (prevcat '(art noun))	; la più famosa
                  (nextcat 'adj))       ; aggregati più complessi
	:then 'adv
	:CF 'A))
 
(setq adj-adv-pronr05
   (make-lexdisr
	:if '(and (prevcat-agr 'noun 'adj)
                  (currtype 'poss))
	:then 'adj
	:CF 'A))
 
(setq adj-adv-pronr06
   (make-lexdisr
	:if '(and (nextcat-agr 'noun 'adj)
	          (not (beforeword-typ '&neg-adv))
		  (not (nextcat '(adj (number sing))))
                  (not (prevcat 'adv))
                  (not (and (currtype 'compar)
                            (nextword-typ '&distance-adj))) ; piu' vicino
                  (not (and (prevcat 'verb)
                            (nextcat 'prep))))	; were More like 
	:then 'adj
	:CF 'A))
 
(setq adj-adv-pronr07
   (make-lexdisr
	:if '(and (prevcat-agr 'art 'pron)
                  (not (nextcat 'adj)))
	:then 'pron
	:CF 'U))
 
(setq adj-adv-pronr08
   (make-lexdisr
	:if '(prevword-typ '&loc-metaph-prep)	; mettersi in proprio, in piu'
	:then 'pron
	:CF 'U))
 
(setq adj-adv-pronr09
   (make-lexdisr
	:if '(and (not (prevcat 'prep))
                  (nextcat '(adj predet pron verb prep art adv)))
	:then 'adv
	:CF 'U))

(setq adj-adv-pronr10e
   (make-lexdisr
	:if '(nextword-typ '&compar-prep)
	:then 'pron
	:CF 'U
        :lang 'english))

(setq adj-adv-pronr11
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (nextcat-agr 'noun 'adj))
	:then 'adj
	:CF 'U))

(setq adj-adv-pronr12		; di TANTO si tratta
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (nextcat '(pron (type refl-impers))))
	:then 'pron
	:CF 'U))

;******************************************************
;*** adj V adv V verb [ENGLISH EVEN, LONG]
;    (default: adj)

(putrule 'adj-adv-verb
	'(adj-adv-verbr01 adj-adv-verbr02 adj-adv-verbr03 adj-adv-verbr04 
          adj-adv-verbr05  adj-adv-verbr06)
	'lexdisrules
	'(adj adv verb))

(putrule 'adj-adv-verb 'adv 'defaultc)

;......................................................

(setq adj-adv-verbr01
   (make-lexdisr
	:if '(and (nextcat 'adv)
                  (not (and (nextcat 'prep)			; for English "in"
                            (nextword-typ '&pref-prep-adj))))
	:then 'adv
	:CF 'A))

(setq adj-adv-verbr02
   (make-lexdisr
	:if '(and (prevcat 'conj)
                  (nextcat 'noun))
	:then 'adj
	:CF 'A))

(setq adj-adv-verbr03 			; How long is the story?
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (prevword-typ '&how-much)
                  (nextcat 'verb))
	:then 'adj
	:CF 'A))

(setq adj-adv-verbr04 			; wash OFF plowed field
   (make-lexdisr
	:if '(verb-govern-adv)
	:then 'adv
	:CF 'A))

(setq adj-adv-verbr05 			; as long as you want
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (prevword-typ '&as))
	:then 'adj
	:CF 'A))

(setq adj-adv-verbr06 			; The long and patient effort
   (make-lexdisr
	:if '(and (nextcat 'conj)
                  (nexttype 'coord)
                  (next2cat 'adj))
	:then 'adj
	:CF 'A))

;******************************************************
;*** adj V art (default: art) [I: "il" o "primo"]

(putrule 'adj-art 'art 'defaultc)

;......................................................

;******************************************************
;*** adj V noun V  predet V pron (default: pron) [TOTS in Catalan]

; *** ADJ: both reasons are valid
; *** PREDET: both your friends
; *** PRON: both of you
(putrule 'adj-noun-predet-pron
	'(predet-pronr01 predet-pronr02 adj-predet-pronr01 adj-predet-pronr02
          adj-predet-pronr03)
	'lexdisrules
	'(adj noun predet pron))

(putrule 'adj-predet-pron 'predet 'defaultc)

;......................................................

;******************************************************
;*** adj V predet V pron (default: pron) [BOTH in English]

; *** ADJ: both reasons are valid
; *** PREDET: both your friends
; *** PRON: both of you
(putrule 'adj-predet-pron
	'(adj-predet-pronr01 adj-predet-pronr02 adj-predet-pronr03)
	'lexdisrules
	'(adj predet pron))

(putrule 'adj-predet-pron 'predet 'defaultc)

;......................................................

(setq adj-predet-pronr01
   (make-lexdisr
	:if '(and (prevcat '(verb prep))
                  (nextcat '(adj noun))
                  (not (nextcat 'prep)))	; to block "both in"
	:then 'adj
	:CF 'A
        :lang '(english spanish catalan)))

(setq adj-predet-pronr02
   (make-lexdisr
	:if '(or (nextword-typ '&partitive-prep)
                 (nextcat '(verb conj))
                 (and (not (nextcat '(art noun)))
                      (not (and (nextcat 'adj)
                                (next2cat 'noun)))))
	:then 'pron
	:CF 'A
        :lang '(english spanish catalan)))

(setq adj-predet-pronr03
   (make-lexdisr
	:if '(nextcat '(adj noun))
	:then 'adj
	:CF 'A
        :lang '(english spanish catalan)))

;******************************************************
;*** adj V conj (default: conj) [I: CATALAN]

(putrule 'adj-conj 'conj 'defaultc)

;......................................................

;******************************************************
;*** adj V conj V noun V prep V pron (default: prep) [SEGONS: CATALAN]

(putrule 'adj-conj-noun-prep-pron
	'(adj-conj-noun-prep-pronr01 adj-conj-noun-prep-pronr02)
	'lexdisrules
	'(adj conj noun prep pron))

(putrule 'adj-conj-noun-prep-pron 'conj 'defaultc)

;......................................................

(setq adj-conj-noun-prep-pronr01
   (make-lexdisr
	:if '(prevcat 'prep)
	:then 'pron
	:CF 'A))

(setq adj-conj-noun-prep-pronr02
   (make-lexdisr
	:if '(prevcat 'art)
	:then 'noun
	:CF 'A))

;******************************************************
;*** adj V conj V pron (default: pron) [CHE; THAT in English]

(putrule 'adj-conj-pron
	'(adj-conj-pronr01 adj-conj-pronr02 adj-conj-pronr03e
          adj-conj-pronr04 adj-conj-pronr05 adj-conj-pronr06 
          adj-conj-pronr06-bis adj-conj-pronr06-ter
          adj-conj-pronr07 adj-conj-pronr08 adj-conj-pronr09
          adj-conj-pronr10 adj-conj-pronr11 adj-conj-pronr12
          adj-conj-pronr13 adj-conj-pronr14 adj-conj-pronr15
	  adj-conj-pronr16 adj-conj-pronr17 adj-conj-pronr18
          adj-conj-pronr19 adj-conj-pronr20 adj-conj-pronr21 
          adj-conj-pronr22 adj-conj-pronr23 adj-conj-pronr24 
          adj-conj-pronr25)
	'lexdisrules
	'(adj conj pron))

(putrule 'adj-conj-pron 'pron 'defaultc)

;......................................................

; "I walked on that."
(setq adj-conj-pronr01
   (make-lexdisr
	:if '(and (prevcat 'prep)
		  (nextcat 'punct))
	:then 'pron
	:CF 'A
        :lang 'english))

; "il tuo amico che ho incontrato e che pensava di ..."
(setq adj-conj-pronr02
   (make-lexdisr
	:if '(and (prevcat 'conj)
		  (prevtype 'coord)
                  (beforecat '(pron (type relat))))
	:then 'pron
	:CF 'A))

; "Guarda che belle pesche ho comperato!"
; "Che bella macchina hai comprato!" 
;*"Che acute malattie renali colpiscono soprattutto individui anziani,
;		e' sicuro
;*"E' noto che acute malattie renali colpiscono individui anziani"
;*"E' noto che cani e gatti non vanno d'accordo" 
(setq adj-conj-pronr03e	; reserved to English because of 75% errors on Italian
   (make-lexdisr
	:if '(and (nextcat 'adj)
		  (nexttype 'qualif)
		  (not (nexttype 'poss))
		  (not (prevcat 'noun))
		  (not (and (prevcat 'verb)		; underlined That late payment
                            (prevword-typ '&subord-verb))))	
	:then 'adj
	:CF 'A
        :lang 'english))

; "Che il cane era ammalato, non lo sapevo"
; "Che tu eri partito, non lo sapevo"
; "Che spesso si dimenticava le cose, lo sapevano tutti"
; "Che di notte e' pericoloso guidare, lo dovresti sapere"
; "Che tre ragazzi erano partiti con te, non lo sapevo"
; "Che mio fratello era stanco, si vedeva benissimo"
; "Che Luigi era partito, me lo ha detto sua madre"
; "Che sia stanco, non c'e' dubbio"
; "Che andare al mare sia riposante, non c'e' dubbio"
; "Che andando al mare mi riposero', ne sono sicuro"
; "Sapevo che Luigi/il cane/mio fratello/di notte/spesso/egli stava male"
; "Accade spesso che Luigi/il cane/mio fratello/di notte/egli stia male"
; "Credo che parta subito"
; "Succede spesso che parta di notte"
; "E' bello che tu gli tenga compagnia"   
; "Ti avevo avvertito che correre sulla sabbia e' faticoso"          
;*"Luisa ti ha detto che fare in questi casi ?"                         
(setq adj-conj-pronr04
   (make-lexdisr
	:if '(and (prevcat '(punct noun pron date))
                  (not (prevcat+ '(nil noun)))
                  (not (prevcat+ 'adj))
                  (not (prevword-typ '&relat-head-pron))
                  (nextcat '(noun (proper yes) num adv prep verb conj punct
                            art pron)))
	:then 'pron
	:CF 'A))

(setq adj-conj-pronr05	; is That of a
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (nextcat 'prep))
	:then 'pron
	:CF 'A
        :lang 'english))

;(setq adj-conj-pronr06
;   (make-lexdisr
;	:if '(and (not (prevcat '(punct noun pron date prep art)))
;                  (not (prevcat+ '(nil noun adj)))
;                  (not (prevword-typ '&relat-head-pron))
;                  (not (currword-typ '&art-relat))
;                  (nextcat '(noun (proper yes) num adv prep verb conj punct
;                             art pron)))
;	:then 'conj
;	:CF 'A))

(setq adj-conj-pronr06
   (make-lexdisr
	:if '(or (and (prevcat 'verb)
                      (not (and (prevmood 'participle)
                                (prev2cat 'noun))))
	         (and (prevcat 'adv)
                      (prev2cat 'verb)
                      (not (prev2mood 'participle))))
	:then 'conj
	:CF 'A))

(setq adj-conj-pronr06-bis
   (make-lexdisr
	:if '(and (prevcat '(conj (type coord)))
                  (beforecat '(conj (type subord))))
	:then 'conj
	:CF 'A))

(setq adj-conj-pronr06-ter		; *** Non che la cosa mi interessi
   (make-lexdisr
	:if '(and (prevcat '(adv nil))
                  (prev2cat 'nil))
	:then 'conj
	:CF 'A))

; una politica passiva Che porta diritto a
(setq adj-conj-pronr07
   (make-lexdisr
	:if '(and (prev2cat 'noun)
		  (prevcat 'adj)
		  (nextcat 'verb))
	:then 'pron
	:CF 'A))

; "Quello che serve"
(setq adj-conj-pronr08
   (make-lexdisr
	:if '(and (prevword-typ '&that)
		  (nextcat 'verb))
	:then 'pron
	:CF 'A))

; "Che macchina hai comprato ?" 
;*"Che cane e gatto non vadano d'accordo, e' un luogo comune"
(setq adj-conj-pronr09
   (make-lexdisr
	:if '(and (not (prevcat+ 'noun))
		  (nextcat-agr '(noun (proper nil)) 'adj)
                  (not (prevword-typ '&prep-subord-head))
                  (not (prevword-typ '&subord-verb))
                  (not (and (prevchar #\,) (prev2word-typ '&subord-verb)))
		  (not (nextword-typ '&pref-not-noun)))
	:then 'adj
	:CF 'A))

; "Che vuoi fare ?" 
;*"Che volevi andare al mare non me lo hai detto" 
(setq adj-conj-pronr10
   (make-lexdisr
	:if '(and (prevcat nil)
		  (nextcat 'verb))
	:then 'pron
	:CF 'A))

(setq adj-conj-pronr11
   (make-lexdisr
	:if '(beforeword-typ '&both)
	:then 'conj
	:CF 'A))

(setq adj-conj-pronr12	; la congettura CHE, il fatto CHE ...
   (make-lexdisr
	:if '(and (prevsemtype '£sit-vgov)
                  (not (nextcat 'verb)))
	:then 'conj
	:CF 'A))

; "Ho visto la ragazza [bionda] che tu conosci" 
;                                   mio fratello conosce" 
;                                   tre ragazzi stanno cercando" 
;                                   da domani abitera' in paese" 
;                                   spesso viene a casa tua" 
;                                   il cane ha morso" 
;                                   Luigi ha conosciuto" 
;*"Ho detto alla ragazza [bionda] che tu andrai al mare" 
; "Ho visto una ragazza [bionda] che cercava Piero" 
;*"Hai detto alla ragazza [bionda] che hai visto ieri sera ?"
; "Quello che sta parlando e' mio cugino" 
; "Le persone, che sorridendo ottengono tutto, sono fortunate"
;*"A mia madre, che ho visto Luigi non l'ho detto" 
; "Ho detto alla ragazza [bionda], che andando al mare si divertira',
;     di  mandarmi una cartolina"
; "La cameriera guardava la ragazza che andando al mare vinse un cane"
;*"Ho detto alla ragazza [bionda] che andando al mare si divertira'"
; *** Il secondo ramo dell'or aggiunto per "e' vero che"
(setq adj-conj-pronr13
   (make-lexdisr
	:if '(prevcat '(noun pron))
	:then 'pron
	:CF 'A))

(setq adj-conj-pronr14
   (make-lexdisr
	:if '(and (prevcat 'adj) (prev2cat 'noun))
	:then 'pron
	:CF 'A))

(setq adj-conj-pronr15
   (make-lexdisr
	:if '(prevword-typ '&relat-head-pron)
	:then 'pron
	:CF 'A))

; pseudo-default: blocks the choice as a (relative?) pronoun at the beginning 
;    of the sentence
(setq adj-conj-pronr16
   (make-lexdisr
	:if '(prevcat nil)
	:then 'conj
	:CF 'U))

(setq adj-conj-pronr17
   (make-lexdisr
	:if '(prevword-typ '&more)
	:then 'conj	
	:CF 'A))

(setq adj-conj-pronr18
   (make-lexdisr
	:if '(prev2word-typ '&more)
	:then 'conj	
	:CF 'A))

; "E' impossibile che ..."
; "Non mi sembra probabile che ..."
(setq adj-conj-pronr19
   (make-lexdisr
	:if '(and (prevcat 'adj)
                  (prev2cat 'verb))
	:then 'conj
	:CF 'U))

(setq adj-conj-pronr20
   (make-lexdisr
	:if '(prevcat 'verb)
	:then 'conj
	:CF 'U))

; *** dopo che aveva visto
(setq adj-conj-pronr21
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (not (prevword-typ '&compar-prep-3))		; piuttosto che
                  (not (currword-typ '&mid-relat)))
	:then 'conj
	:CF 'U))

; "E' quindi impossibile che ..."
(setq adj-conj-pronr22
   (make-lexdisr
	:if '(and (prevcat 'adj)
                  (prev2cat 'adv)
		  (not (prev2type '(quant neg))))
	:then 'conj
	:CF 'U))

; sa benissimo che
(setq adj-conj-pronr23
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (prev2cat 'verb)
		  (prev2word-typ '&to-know-tell))
	:then 'conj
	:CF 'U))

; hanno fatto sì che
(setq adj-conj-pronr24
   (make-lexdisr
	:if '(and (prevcat 'phras)
                  (prev2cat 'verb))
	:then 'conj
	:CF 'U))

; il che è molto interessante
(setq adj-conj-pronr25
   (make-lexdisr
	:if '(prevcat '(art (type def) (gender m) (number sing)))
	:then 'pron
	:CF 'U))

;******************************************************
;*** adj V noun (default: adj)

(putrule 'adj-noun
	'(
          adj-nounr000 adj-nounr001 adj-nounr002
          adj-nounr00 adj-nounr00e
          adj-nounr01e adj-nounr02 adj-nounr05 adj-nounr06
          adj-nounr07 adj-nounr08 adj-nounr09 adj-nounr10
          adj-nounr11 adj-nounr12 adj-nounr13i adj-nounr03
          adj-nounr04 adj-nounr14
          adj-nounr19i adj-nounr32
	  adj-nounr20i 
          adj-nounr21 adj-nounr22 adj-nounr23ics
          adj-nounr24 adj-nounr25  adj-nounr26e adj-nounr27
          adj-nounr28 adj-nounr36 adj-nounr29 adj-nounr30e adj-nounr30-2i
          adj-nounr30-3
          adj-nounr31 adj-nounr15 adj-noun-verbr08 adj-noun-verbr09
          adj-nounr16 adj-nounr16-bis adj-nounr17  adj-nounr18i adj-nounr33
          adj-nounr34 adj-nounr35 adj-nounr37 adj-nounr38 adj-nounr39 
          adj-nounr40)
	'lexdisrules
	'(adj noun))

(putrule 'adj-noun 'adj 'defaultc)

;......................................................

(setq adj-nounr000	; for politicians' names (e.g. Prodi, Salvi)
   (make-lexdisr
	:if '(and (currproper)
                  (currword-typ '&polit-name))
	:then 'noun
	:CF 'A))
 
(setq adj-nounr001	; for quotations (Broad 1928)
   (make-lexdisr
	:if '(and (prevchar #\()
                  (nextcat 'num)
                  (or (next2char #\))
                      (next2char #\,)
                      (next2char #\&)))
	:then 'noun
	:CF 'A))
 
(setq adj-nounr002	; un CORDIALE buonasera
   (make-lexdisr
	:if '(and (prevcat 'art)
                  (nextcat 'interj)
                  (nextword-typ '&greetings))
	:then 'adj
	:CF 'A))
 
(setq adj-nounr00	; ad-hoc rule for "una Vecchia ottomana"
   (make-lexdisr
	:if '(nextonttype 'noun '££collectable-obj)
	:then 'adj
	:CF 'A))
 
(setq adj-nounr00e	; in Italian, 
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (nextcat 'punct))
	:then 'noun
	:CF 'A
        :lang 'english))
 
(setq adj-nounr01e
   (make-lexdisr
	:if '(and (prevcat 'art)
                  (nextcat '(pron (word-typ &one) verb nil))
                  (not (nextcat 'noun)))
	:then 'noun
	:CF 'A
        :lang 'english))
 
(setq adj-nounr02	; for european directives
   (make-lexdisr
	:if '(and (or (prevchar #\.)
                      (prevcat nil))
                  (nextcat 'num))
	:then 'noun
	:CF 'A))
 
(setq adj-nounr03
   (make-lexdisr
	:if '(and (prevcat-agr '(art adj) 'noun)
                  (nextcat '(adj (type interr) adv art prep 
                             pron (word-typ &base-relat) 
                             verb (not (word-typ &noun-verb-pref-noun)) nil)))
	:then 'noun
	:CF 'A))
 
(setq adj-nounr04
   (make-lexdisr
	:if '(and (prevchar '(#\' #\"))			; "
                  (prev2cat 'art)
                  (nextcat '(adj (type interr) adv art prep 
                             pron (word-typ &base-relat) verb nil)))
	:then 'noun
	:CF 'A))

(setq adj-nounr05
   (make-lexdisr
	:if '(and (currword-typ '&onorevole)
		  (prevcat 'art)
		  (nextcat '(noun (proper yes))))
	:then 'noun
	:CF 'A))
 
(setq adj-nounr06
   (make-lexdisr
	:if '(and (currword-typ '&saint)
		  (nextcat 'noun)
		  (not (prevcat 'art))
	          (not (prevword-typ '&institutional))
                   )
	:then 'adj
	:CF 'A))
 
(setq adj-nounr07
   (make-lexdisr
	:if '(prevword-typ '&saint)
	:then 'noun
	:CF 'A))
 
(setq adj-nounr08
   (make-lexdisr
	:if '(and (prevcat-agr 'adj 'noun)
                  (prevtype 'qualif)
                  (prev2cat 'noun)
	     	  (nextcat '(punct pron)))
	:then 'adj
	:CF 'A))
 
(setq adj-nounr09		; una Vera e propria guerra
   (make-lexdisr
	:if '(and (prevcat-agr '(art adj) 'adj)
	     	  (nextcat '(conj (type coord)))
                  (next2cat 'adj))
	:then 'adj
	:CF 'A))

(setq adj-nounr10
   (make-lexdisr
	:if '(and (prevcat-agr '(art adj) 'noun)
	     	  (or (not (nextcat '(noun phras)))		; un CORDIALE buonasera
		      (nextcat '(pron (form clitic))))		; to exclude "si"
	     	  (not (and (nextchar #\-)
	     	            (next2cat 'noun))))
	:then 'noun
	:CF 'A))
 
(setq adj-nounr11
   (make-lexdisr
	:if '(and (prevcat-agr '(art adj) 'adj)
	     	  (nextcat-agr 'noun 'adj)
                  (not (nextword-typ '&pref-not-noun))
	     	  (not (or (nextcat-agr 'adj 'adj)
                           (and (nextcat 'verb)
                                (nexttype 'mod))))
		  (not (currproper))
                  (not (and (next2cat 'pron)
                            (next2type 'refl))))
	:then 'adj
	:CF 'A))
 
(setq adj-nounr12
   (make-lexdisr			; quella particolare politica
	:if '(and (prevcat '(adj (type demons)))
	     	  (nextcat-agr 'noun 'adj)
	     	  (nextcat-agr 'adj 'adj)
		  (not (currproper)))
	:then 'adj
	:CF 'A))
 
(setq adj-nounr13i
   (make-lexdisr
	:if '(and (prevcat-agr '(art adj) 'adj)
	     	  (nextcat-agr 'adj 'noun)
	     	  (nextcat-agr 'adj 'adj)
                  (not (and (nextcat-agr 'noun 'adj)
	                    (currword-typ '&pre--adj)))	; ex mini maxi
		  (not (currproper)))
	:then 'noun
	:CF 'A
	:lang 'italian))
 
(setq adj-nounr14	; più alto di
   (make-lexdisr
	:if '(and (prevword-typ '&compar-adv)
                  (nextword-typ '&compar-prep-2))
	:then 'adj
	:CF 'A))
 
(setq adj-nounr15
   (make-lexdisr
	:if '(nextword-typ '&double-who)
	:then 'adj
	:CF 'U))
 
(setq adj-nounr16
   (make-lexdisr
	:if '(and (prevcat 'conj)
                  (prevtype 'coord)
                  (prev2cat 'adj))
	:then 'adj
	:CF 'U))
 
(setq adj-nounr16-bis
   (make-lexdisr
	:if '(and (prevcat 'conj)
                  (prevtype 'coord)
                  (prev2cat+ 'noun)
                  (not (nextcat 'noun)))
	:then 'noun
	:CF 'U))
 
(setq adj-nounr17		; vera e propria
   (make-lexdisr
	:if '(and (nextcat 'conj)
                  (nexttype 'coord)
                  (next2cat-agr 'adj 'adj))
	:then 'adj
	:CF 'U))
 
(setq adj-nounr18i
   (make-lexdisr
	:if '(and (or (not (nextcat 'noun))
                      (not (nextword-typ '&to-be)))		; di esplosivo ERA stata
                  (not (prevcat 'noun))
                  (or (not (prevchar '(#\' #\")))		; "
                      (not (prev2cat 'noun)))
                  (or (not (nextcat-agr 'noun 'adj))
	              (not (currword-typ '&pre--adj)))		; ex mini maxi
                  (not (prevword-typ '&to-be)))
	:then 'noun
	:CF 'U
        :lang 'italian))
 
(setq adj-nounr19i		; per il codice: "diritti morali"
   (make-lexdisr
	:if '(and (prevcat-agr '(art adj) 'adj)
	     	  (nextcat-agr 'noun 'adj)
	          (currword-typ '&right))
	:then 'noun
	:CF 'C
	:lang 'italian))
 
(setq adj-nounr20i
   (make-lexdisr
	:if '(and (not (currproper))		; Il Dott. Rossi
                  (or (prevcat-agr 'noun 'adj)
	              (and (prevchar '(#\' #\"))				; "
                           (prev2cat 'noun))))
	:then 'adj
	:CF 'U
        :lang 'italian))

(setq adj-nounr21
   (make-lexdisr
	:if '(and (not (nextword-typ '&pref-not-noun))	; per evitare 'la', 'nei', 'dio'
                  (nextcat-agr 'noun 'adj)
                  (not (nextcat-agr 'adj 'noun))
                  (not (currproper))
                  (not (and (prevword-typ '&loc-metaph-prep)	; "in inglese"
                            (currsemtype 'adj '£geogr)
                            (currnumber 'sing)))
                  (not (nextcat '(noun (proper yes)))))
	:then 'adj
	:CF 'U))

(setq adj-nounr22
   (make-lexdisr
	:if '(and (nextcat-agr 'noun 'adj)
                  (nextcat-agr 'adj 'adj)
                  (or (nextcat '(noun (proper yes)))
                      (currtype 'ordin)))
	:then 'adj
	:CF 'U))

(setq adj-nounr23ics
   (make-lexdisr
	:if '(and (nextcat-agr 'noun 'adj)
                  (nextcat-agr 'adj 'adj)
                  (not (nextcat '(noun (proper yes))))
                  (not (nextcat '(adj (type interr))))
                  (not (currtype 'ordin))
                  (not (currword-typ '&pre--adj)))		; ex mini maxi
	:then 'noun
	:CF 'U
        :lang '(italian spanish catalan)))

(setq adj-nounr24
   (make-lexdisr
	:if '(and (prevcat 'num)
                  (nextcat '(art punct prep nil)))
	:then 'noun
	:CF 'U))

(setq adj-nounr25
   (make-lexdisr
	:if '(and (prevword-typ '&to-be)
                  (not (prev2word-typ '&loc-pron))  ; to exclude "c'erano finanziarie che"
		  (nextword-typ '&prep-subord-dep))
	:then 'adj
	:CF 'U))

(setq adj-nounr26e
   (make-lexdisr
	:if '(and (prevword-typ '&to-be)
                  (not (prev2word-typ '&loc-pron)))
	:then 'adj
	:CF 'U
	:lang 'english))

(setq adj-nounr27
   (make-lexdisr
	:if '(and (not (prevcat-agr 'noun 'adj))
		  (nextcat '(adj (type interr) 
			     pron (word-typ &base-mid-relat) ; che cui
			     conj (type subord))))
	:then 'noun
	:CF 'U))

(setq adj-nounr28
   (make-lexdisr
	:if '(and (not (prevcat-agr 'noun 'adj))
		  (nextcat-agr '(art prep) 'noun) 
		  (next2cat '(pron (word-typ &art-relat-or-base) 
                              pron (word-typ &mid-relat))))
	:then 'noun
	:CF 'U))

(setq adj-nounr29
   (make-lexdisr
	:if '(and (not (prevcat-agr 'noun 'adj))
                  (nextcat nil))
	:then 'noun
	:CF 'U))

(setq adj-nounr30e
   (make-lexdisr
	:if '(and (prevcat-agr 'noun 'noun)
                  (nextcat '(adv prep punct)))
	:then 'noun
	:CF 'U
        :lang 'english))

(setq adj-nounr30-2i
   (make-lexdisr
	:if '(and (prevword-typ '&action-noun)
                  (not (prevcat-agr 'noun 'adj)))
	:then 'noun
	:CF 'U
        :lang 'italian))

(setq adj-nounr30-3
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (nextcat-agr 'adj 'noun)
                  (not (nextcat-agr 'noun 'adj)))
	:then 'noun
	:CF 'U))

(setq adj-nounr31
   (make-lexdisr
	:if '(and (not (prevcat-agr 'noun 'noun))   
                  (not (prevcat '(num prep)))
                  (not (and (prevcat '(verb punct))
                            (nextcat '(prep punct art))))
                  (not (and (prevcat 'verb)
                            (nextcat '(conj (type coord)))
                            (next2cat 'noun)
                            (not (next2cat 'adj))))
                  (not (and (prevcat 'verb)		; introdurre VARIABILI non legate
                            (nextcat 'adv)
                            (next2cat 'verb)
                            (next2mood 'participle)
                            (next2tense'past)))
                  (not (and (prevcat nil)
                            (nextcat '(verb prep))))
                  (not (and (or (prevcat nil)
                                (prevchar #\.))
                            (nextcat 'conj)
                            (next2cat 'noun)))
                  (not (and (prevcat 'conj)
                            (nextcat 'adj)
                            (next2cat 'noun)))
                  (not (and (prevcat 'conj)
                            (prevtype 'coord)
                            (prev2cat+ 'noun)))
                  (not (and (prevcat 'conj)
                            (nextcat 'adj)))
                  (not (and (prevchar #\,)
                            (prev2cat 'noun)
                            (nextcat '(conj (type coord)))
                            (next2cat 'noun)))
                  (not (nextcat '(adj (type interr) pron (type relat) 
                                  conj (type subord)))))
	:then 'adj
	:CF 'U))

(setq adj-nounr32
   (make-lexdisr
	:if '(prevcat 'adv)
	:then 'adj
	:CF 'U))

(setq adj-nounr33
   (make-lexdisr
	:if '(and (currproper)
                  (prevword-typ '&saint))
	:then 'noun
	:CF 'U))

(setq adj-nounr34
   (make-lexdisr
	:if '(and (all-capital)
                  (default-proper)
                  (prevcat 'art))
	:then 'adj
	:CF 'A))

(setq adj-nounr35
   (make-lexdisr
	:if '(and (currproper)
                  (or (not (currword-typ '&saint))
                      (currnumber 'pl)
                      (not (prevcat '(noun art))))	; Reparto San MArco
                  (or (prevcat '(noun (proper yes)))
                      (and (nextcat '(noun (proper yes)))
                           (or (not (nextcat '(noun (proper nil))))
                               (not (nextcat-agr '(noun (proper nil)) 'adj))))))
	:then 'noun
	:CF 'A))

(setq adj-nounr36
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (nextcat '(adv prep punct verb))
                  (not (and (nextcat-agr 'noun 'adj)
                            (prev2cat 'verb))))
	:then 'noun
	:CF 'A
        :lang 'italian))

(setq adj-nounr37
   (make-lexdisr
	:if '(and (or (prevcat nil)
	              (prevchar #\.))  ; for the numbered lists
                  (nextcat 'prep))
	:then 'noun
	:CF 'U))

(setq adj-nounr38
   (make-lexdisr
	:if '(and (or (nextcat nil)
	              (nextchar #\.))
                  (or (prevcat 'adj)
                      (and (prevchar '(#\" #\'))	; "
                           (prev2cat 'adj))))
	:then 'noun
	:CF 'U))

(setq adj-nounr39
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (currproper)
	          (prevword-typ '&title))
	:then 'noun
	:CF 'U))

(setq adj-nounr40
   (make-lexdisr
	:if '(and (prevchar '(#\' #\"))			; "
                  (prev2cat 'adj)
                  (nextcat nil))
	:then 'noun
	:CF 'A))

;******************************************************
;*** noun V num V pron (default: num) [english "ONE"]

(putrule 'noun-num-pron
         '(noun-num-pronr01e noun-num-pronr02e noun-num-pronr03e
           noun-num-pronr04e)
         'lexdisrules
         '(noun num pron))

(putrule 'noun-num-pron 'num 'defaultc)

(setq noun-num-pronr01e
   (make-lexdisr
	:if '(and (prevcat nil)
                  (or (nextcat '(nil noun))		; one estimate
                      (nextchar '(#\. #\! #\? #\:))))
	:then 'num
	:CF 'A
        :lang 'english))

(setq noun-num-pronr02e
   (make-lexdisr
	:if '(or (and (prevcat '(art adj))
                      (not (and (nextcat '(conj (type coord)))	; the one and unique
                                (next2cat 'adj)
                                (next2word-typ '&unique))))
                 (and (nextcat 'verb)
                      (not (and (nextcat 'adj)
                                (next2cat 'noun)))))
	:then 'pron
	:CF 'A
        :lang 'english))

(setq noun-num-pronr03e
   (make-lexdisr
	:if '(and (prevcat 'conj)		; more than ONE
                  (prevtype 'compar))
	:then 'num
	:CF 'A
        :lang 'english))

(setq noun-num-pronr04e
   (make-lexdisr				; ONE decentralized society
	:if '(or (nextcat 'noun)
                 (and (nextcat 'adj)
                      (next2cat 'noun)))
	:then 'num
	:CF 'A
        :lang 'english))

;******************************************************
;*** adj V noun V prep V pron (default: prep) [SECONDO, QUANTO]
; *** SECONDO pron is never selected!

(putrule 'adj-noun-prep-pron
         '(adj-noun-prep-pronr01 adj-noun-prep-pronr02 adj-noun-prep-pronr03
	   adj-noun-prep-pronr04 adj-noun-prep-pronr05 adj-noun-prep-pronr06
           adj-noun-prep-pronr07 adj-noun-prep-pronr08)
         'lexdisrules
         '(adj noun prep pron))

(putrule 'adj-noun-prep-pron 'prep 'defaultc)

(setq adj-noun-prep-pronr01
   (make-lexdisr
	:if '(and (not (currword-typ '&how-much))
                  (prevcat-agr 'art 'adj)
                  (nextcat-agr 'noun 'adj))
	:then 'adj
	:CF 'A))

(setq adj-noun-prep-pronr02
   (make-lexdisr
	:if '(and (not (currword-typ '&how-much))
                  (prevcat-agr 'adj 'adj)
                  (prevtype '(demons poss deitt))
                  (nextcat-agr 'noun 'adj))
	:then 'adj
	:CF 'A))

(setq adj-noun-prep-pronr03
   (make-lexdisr
	:if '(and (not (currword-typ '&how-much))
                  (prevcat-agr '(art adj) 'noun)
                  (not (prevtype 'def))		; il secondo: PRON
                  (not (nextcat-agr 'noun 'adj)))
	:then 'noun
	:CF 'A))

(setq adj-noun-prep-pronr04
   (make-lexdisr
	:if '(and (not (currword-typ '&how-much))
                  (nextcat 'conj)
                  (next2cat '(adj (type ordin))))
	:then 'noun
	:CF 'A))

(setq adj-noun-prep-pronr05
   (make-lexdisr
	:if '(and (or (currword-typ '&how-much)
	              (currword-typ '&second-ordin))
                  (prevcat '(nil prep verb))
                  (nextcat-agr 'noun 'adj)
                  (not (nextword-typ '&pref-not-noun))
                  (not (and (nextcat 'verb)
                            (nextmood 'participle)
                            (nexttense 'past))))
	:then 'adj
	:CF 'A))

(setq adj-noun-prep-pronr06
   (make-lexdisr
	:if '(and (currword-typ '&how-much)
                  (prevcat '(nil prep))
                  (or (nextcat 'verb)
                      (nextword-typ '&refl-pron)))
	:then 'pron
	:CF 'A))

(setq adj-noun-prep-pronr07
   (make-lexdisr
	:if '(and (currword-typ '&how-much)
                  (prevcat '(verb prep))
                  (not (nextcat 'noun)))
	:then 'pron
	:CF 'A))

(setq adj-noun-prep-pronr08
   (make-lexdisr
	:if '(and (currword-typ '&second-ordin)
                  (nextword-typ '&comma))
	:then 'adj
	:CF 'A))

;******************************************************
;*** adj V noun V prep V verb (default: prep) [ENGLISH: LIKE]
; *** pron is never selected!

(putrule 'adj-noun-prep-verb
         '(adj-noun-prep-verbr01 adj-noun-prep-verbr02 
           adj-noun-prep-verbr03 adj-noun-prep-verbr04
           adj-noun-prep-verbr05 adj-noun-prep-verbr06
           adj-noun-prep-verbr07 adj-noun-prep-verbr08)
         'lexdisrules
         '(adj noun prep verb))

(putrule 'adj-noun-prep-verb 'verb 'defaultc)

(setq adj-noun-prep-verbr01
   (make-lexdisr
	:if '(and (prevchar #\-)
                  (prev2cat 'noun))
	:then 'adj
	:CF 'A))

(setq adj-noun-prep-verbr02
   (make-lexdisr
	:if '(and (prevcat 'art)
                  (nextcat 'noun))
	:then 'adj
	:CF 'U))

(setq adj-noun-prep-verbr03
   (make-lexdisr
	:if '(and (prevcat 'art)
                  (not (nextcat 'noun)))
	:then 'noun
	:CF 'U))

(setq adj-noun-prep-verbr04
   (make-lexdisr
	:if '(and (prevcat 'verb)	; *I would like to know
                  (not (prevtype 'mod)))
	:then 'prep
	:CF 'U))

(setq adj-noun-prep-verbr05
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (prev2cat 'verb)
                  (not (prev2type 'mod)))
	:then 'prep
	:CF 'U))

(setq adj-noun-prep-verbr06
   (make-lexdisr
	:if '(and (nextcat 'conj)
                  (not (nextword-typ '&base-subord-prep)))
	:then 'prep
	:CF 'U))

(setq adj-noun-prep-verbr07		; of institutions like the
   (make-lexdisr
	:if '(and (nextcat 'art)
                  (prevcat 'noun)
                  (not (prev2cat '(art adj))))
	:then 'prep
	:CF 'U))

(setq adj-noun-prep-verbr08		; ... than like the
   (make-lexdisr
	:if '(and (prevcat '(conj (type compar)))
                  (nextcat 'art))
	:then 'prep
	:CF 'U))

;***********************************************************
;*** adj V noun V pron V verb (default: noun) [ENGLISH: SECOND; ITALIAN: ULTIMA]

(putrule 'adj-noun-pron-verb
         '(adj-noun-pron-verbr01 adj-noun-pron-verbr02 adj-noun-pron-verbr03
           adj-noun-pron-verbr04 adj-noun-pron-verbr05 adj-noun-pron-verbr06)
         'lexdisrules
         '(adj noun pron verb))

(putrule 'adj-noun-pron-verb 'noun 'defaultc)
;......................................................

(setq adj-noun-pron-verbr01
   (make-lexdisr
	:if '(and (prevcat 'art)
	          (prevtype 'def)
                  (nextcat '(adj noun pron)))
	:then 'adj
	:CF 'A))

(setq adj-noun-pron-verbr02
   (make-lexdisr
	:if '(and (prevcat 'art)
	          (prevtype 'def)
                  (not (nextcat '(adj noun pron))))
	:then 'pron
	:CF 'A))

(setq adj-noun-pron-verbr03		;  second April
   (make-lexdisr
	:if '(and (prevcat '(nil prep (word-typ &date-prep)))
                  (nextsemtype 'noun '£month))
	:then 'adj
	:CF 'A))

(setq adj-noun-pron-verbr04		;  second of April
   (make-lexdisr
	:if '(and (prevcat '(nil prep (word-typ &date-prep)))
                  (nextcat 'prep)
                  (next2semtype 'noun '£month))
	:then 'pron
	:CF 'A))

(setq adj-noun-pron-verbr05		;  April second
   (make-lexdisr
	:if '(and (currtype 'ordin)
                  (prevsemtype '£month))
	:then 'adj
	:CF 'A))

(setq adj-noun-pron-verbr06		; Ultima fermata
   (make-lexdisr
	:if '(nextcat 'noun)
	:then 'adj
	:CF 'U))

;***********************************************************
;*** adj V noun V pron (default: noun) [PRIME NULLA OTTAVA]

(putrule 'adj-noun-pron
         '(adj-pronr04ics adj-pronr05e adj-pronr06 adj-pronr07
           adj-pronr09 adj-pronr10 adj-pronr11 adj-pronr12
           adj-pronr13 adj-pronr14i adj-pronr15e  adj-pronr16e 
           adj-pronr17i adj-pronr18i adj-pronr19 adj-pronr20ics
           adj-pronr21e adj-pronr22 adj-pronr23)
         'lexdisrules
         '(adj noun pron))

(putrule 'adj-noun-pron 'noun 'defaultc)

;......................................................

;******************************************************
;*** adj V noun V verb (default: noun)
;    [QUADRATO, CARICA, CHIUSA, DIRETTO, VIVI, SOSTANTIVO, MEDIA, ESPRESSI, GRAVI]

(putrule 'adj-noun-verb
         '(adj-nounr06 adj-nounr07 adj-nounr34 adj-nounr35 adj-noun-verbr01is 
   ; *** the first four rules concerns Saints
           adj-noun-verbr02e adj-noun-verbr03e adj-noun-verbr03bis-e 
           adj-noun-verbr04 adj-noun-verbr05 adj-noun-verbr06
           adj-noun-verbr07 adj-noun-verbr08 adj-noun-verbr09 adj-noun-verbr10
           adj-noun-verbr11 adj-noun-verbr12 adj-noun-verbr13 adj-noun-verbr14 
           adj-noun-verbr14bis-e adj-noun-verbr14ter-e
           adj-noun-verbr15ics adj-noun-verbr15e 
           adj-noun-verbr16 adj-noun-verbr17 adj-noun-verbr18
           adj-noun-verbr19 adj-noun-verbr20 adj-noun-verbr21 adj-noun-verbr22e
           adj-noun-verbr23 adj-noun-verbr24 adj-noun-verbr25e adj-noun-verbr26e 
           adj-noun-verbr27i adj-noun-verbr28i adj-noun-verbr29 adj-noun-verbr30 
           adj-noun-verbr31 adj-noun-verbr32 adj-noun-verbr33 adj-noun-verbr34 
           adj-noun-verbr35 adj-noun-verbr36)
         'lexdisrules
         '(adj noun verb))

(putrule 'adj-noun-verb 'noun 'defaultc)

;......................................................

(setq adj-noun-verbr01is
   (make-lexdisr
	:if '(and (prevcat-agr 'noun 'adj)
                  (not (and (currword-typ '&saint)
                            (nextcat '(noun (proper yes)))))
                  (not (and (currmood+tense 'participle 'past)
                            (nextword-typ '&agent-compl-prep))))
	:then 'adj
	:CF 'A
        :lang '(italian spanish)))

(setq adj-noun-verbr02e
   (make-lexdisr
	:if '(and (prevcat-agr 'noun 'verb)    ; MUST
	          (currtype 'mod)
                  (nextcat 'verb))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq adj-noun-verbr03e
   (make-lexdisr
	:if '(and (prevcat 'prep)                 ; I want to GO to
                  (prevword-typ '&base-subord-prep)
	          (currmood 'infinite)
                  (nextcat 'prep))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq adj-noun-verbr03bis-e
   (make-lexdisr
	:if '(and (prevcat nil)                 ; Set aside
                  (nextcat 'adv))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq adj-noun-verbr04
   (make-lexdisr
	:if '(prevtype 'aux)
	:then 'verb
	:CF 'A))

(setq adj-noun-verbr05
   (make-lexdisr
; *** the second condition to prevent application to "la"
;     and when the next word is ambiguous wrt adjective
;     (ex. il Pubblico giapponese)
; *** the third for preventing application when the next word
;     is Spanish "de"
	:if '(and (nextcat-agr 'noun 'adj)
                  (not (and (currword-typ '&saint)
                            (or (prevword-typ '&institutional)
                                (prevcat 'art))
                            (nextcat '(noun (proper yes)))))
                  (not (nextcat '(adj (type qualif) art)))
                  (not (nextword-typ '&pref-not-noun)))
	:then 'adj
	:CF 'A))

(setq adj-noun-verbr06
   (make-lexdisr
	:if '(and (nextcat-agr 'noun 'adj)
                  (not (currword-typ '&adj-pref-noun))
                  (not (nextword-typ '&pref-not-noun))
                  (not (nextcat 'art))
                  (prevcat 'verb))
	:then 'noun
	:CF 'U))

(setq adj-noun-verbr07		; un Mini-intervento
   (make-lexdisr
	:if '(and (nextchar #\-)
                  (next2cat 'noun))
	:then 'adj
	:CF 'U))

(setq adj-noun-verbr08
   (make-lexdisr
	:if '(and (not (currword-typ '&noun-pref-adj))	; Italian "simile"
                  (or (prevchar #\,)
                      (and (prevcat 'conj) (prevtype 'coord)))
                  (prev2cat 'noun)
	          (or (nextcat 'punct)
                      (and (nextcat 'conj) (nexttype 'coord))))
	:then 'noun
	:CF 'U))

(setq adj-noun-verbr09
   (make-lexdisr
	:if '(and (or (prevchar #\,)
                      (and (prevcat 'conj) (prevtype 'coord)))
                  (prev2cat 'adj)
	          (or (nextcat 'punct)
                      (and (nextcat 'conj) (nexttype 'coord))))
	:then 'adj
	:CF 'U))

(setq adj-noun-verbr10
   (make-lexdisr
	:if '(and (prevcat 'pron)
		  (prevtype 'refl-impers))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr11
   (make-lexdisr
	:if '(and (prevcat 'adv)
		  (prev2type 'aux)
                  (currmood '(participle gerund)))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr12
   (make-lexdisr
	:if '(and (prevcat '(pron adv))
		  (nextcat 'art))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr13
   (make-lexdisr
	:if '(and (prevcat '(verb (type mod)))
		  (currmood 'infinite))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr14
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (prev2cat '(verb (type mod)))
		  (currmood 'infinite))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr14bis-e
   (make-lexdisr
	:if '(and (prevcat '(adv pron))
                  (prev2cat '(verb (type mod)))
		  (currmood 'infinite))
	:then 'verb
	:CF 'U
        :lang 'english))

(setq adj-noun-verbr14ter-e
   (make-lexdisr
	:if '(and (prevchar #\,)
	          (beforecat '(verb (mood imper)))
                  (currmood 'imper))
	:then 'verb
	:CF 'U
        :lang 'english))

(setq adj-noun-verbr15ics
   (make-lexdisr
	:if '(and (or (prevcat '(verb (not (type aux)) adv (not (type neg))))
		      (prevchar #\,))
		  (not (currmood 'participle)))
	:then 'adj
	:CF 'U
        :lang '(italian catalan spanish)))

(setq adj-noun-verbr15e
   (make-lexdisr
	:if '(and (or (prevcat '(verb (not (type aux)) adv (not (type neg))))
		      (prevchar #\,))
		  (not (and (prevmood 'participle)		; in case it is a verb
                            (nextcat 'prep)))
		  (not (currmood 'participle)))
	:then 'adj
	:CF 'U
        :lang 'english))

(setq adj-noun-verbr16
   (make-lexdisr
	:if '(and (or (prevcat '(pron verb (not (type aux)) (adv (not (type compar)))))
		      (prevchar #\,))
                  (not (prevmood 'participle))
                  (not (prevtense 'past))
		  (currmood 'participle)
		  (currtense 'past))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr17
   (make-lexdisr
	:if '(and (prevcat 'conj)
                  (prevtype 'coord)
		  (prev2cat 'adj))
	:then 'adj
	:CF 'U))

(setq adj-noun-verbr18		; il ministro Vive in
   (make-lexdisr
	:if '(and (prevcat 'noun)
		  (nextcat 'prep))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr19		; di voler alimentare
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (prevtype 'mod)
                  (currmood 'infinite))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr20		; era carica di
   (make-lexdisr
	:if '(prevword-typ '&to-be)
	:then 'adj
	:CF 'U))

(setq adj-noun-verbr21		; l'albania Vive ormai
   (make-lexdisr
	:if '(and (prevcat-agr 'noun 'verb)
	          (nextcat 'adv))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr22e		; the SECOND one
   (make-lexdisr
	:if '(and (prevcat 'art)
		  (nextword-typ '&one))
	:then 'adj
	:CF 'A
        :lang 'english))

(setq adj-noun-verbr23		; what we Mean by 
   (make-lexdisr
	:if '(and (prevcat '(pron (type pers)))
		  (nextcat 'prep))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr24		; it is necessary to Reverse
   (make-lexdisr
	:if '(and (prevword-typ '&base-subord-prep)
		  (currmood 'infinite))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr25e		; cannot go
   (make-lexdisr
	:if '(and (or (and (prevcat 'adv)
                           (prev2cat '(verb (type mod))))
                      (prevcat 'verb+adv))
		  (currmood 'infinite))
	:then 'verb
	:CF 'U
        :lang 'english))

(setq adj-noun-verbr26e		; of abstract values
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (nextcat 'noun))
	:then 'adj
	:CF 'U
        :lang 'english))

(setq adj-noun-verbr27i		; Dato l'item
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat 'art))
	:then 'verb
	:CF 'U
        :lang 'italian))

(setq adj-noun-verbr28i		; il palcoscenico più ambito
   (make-lexdisr
	:if '(and (prev2cat 'noun)
                  (prevcat '(adv (or (type compar) (type quant)))))
	:then 'adj
	:CF 'U
        :lang 'italian))

(setq adj-noun-verbr29		; la presente direttiva *** la media europea (!!!)
   (make-lexdisr
	:if '(and (prevcat-agr 'art 'adj)
	          (nextcat-agr 'noun 'adj)
                  (nextword-typ '&adj-pref-noun))
	:then 'adj
	:CF 'U))

(setq adj-noun-verbr30		; non parlate di
   (make-lexdisr
	:if '(and (prevcat '(adv (type neg)))
	          (prev2cat nil)
                  (nextcat 'prep))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr31		; molto diretti
   (make-lexdisr
	:if '(prevcat '(adv (type quant)))
	:then 'adj
	:CF 'U))

(setq adj-noun-verbr32		; Detto questo vediamo
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat 'pron)
                  (next2cat '(verb punct)))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr33		; mangio bene e VIVO male
   (make-lexdisr
	:if '(and (nextcat 'adj)
                  (nextcat 'adv)
                  (not (nextcat-agr 'adj 'noun)))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr34		; il diretto e indiretto investimento
   (make-lexdisr
	:if '(and (prevcat '(art adj (deitt)))
                  (nextcat '(conj (coord)))
                  (next2cat 'adj))
	:then 'adj
	:CF 'U))

(setq adj-noun-verbr35		; ad alimentare la base
   (make-lexdisr
	:if '(and (prevcat 'prep)
	          (prevword-typ '&theme-prep-1)
                  (currmood 'infinite)
                  (nextcat 'art))
	:then 'verb
	:CF 'U))

(setq adj-noun-verbr36		; chi vive sul fondo
   (make-lexdisr
	:if '(and (prevcat '(pron (type relat)))
	          (prevword-typ '&double-who))
	:then 'verb
	:CF 'U))

;******************************************************
;*** adj V noun V verb V verb+pron (per PRIVATI)
;    (default: noun)
; !!! Same rules as for adj-noun-verb: verb+pron never chosen!!!

(putrule 'adj-noun-verb-verb+pron
         '(adj-nounr34 adj-nounr35 adj-noun-verbr01is adj-noun-verbr02e
           adj-noun-verbr04 adj-noun-verbr05 adj-noun-verbr07 adj-noun-verbr08
           adj-noun-verbr09 adj-noun-verbr10 adj-noun-verbr11 adj-noun-verbr12
           adj-noun-verbr13 adj-noun-verbr14 adj-noun-verbr14bis-e 
           adj-noun-verbr14ter-e
           adj-noun-verbr15ics adj-noun-verbr15e adj-noun-verbr17
           adj-noun-verbr12 adj-noun-verbr19 adj-noun-verbr24)
         'lexdisrules
         '(adj noun verb verb+pron))

(putrule 'adj-noun-verb-verb+pron 'noun 'defaultc)

;......................................................

;******************************************************
;*** adj V noun V verb+pron (per TENTATIVI, CAPITALE, ANIMALI, LOCALE)
;    (default: noun)

(putrule 'adj-noun-verb+pron
         '(adj-noun-verb+pronr01 adj-noun-verb+pronr02 adj-noun-verb+pronr03 
           adj-noun-verb+pronr04 adj-noun-verb+pronr05 adj-nounr14 adj-noun-verb+pronr06)
         'lexdisrules
         '(adj noun verb+pron))

(putrule 'adj-noun-verb+pron 'noun 'defaultc)

;......................................................

(setq adj-noun-verb+pronr01
   (make-lexdisr
	:if '(and (currword-typ '&general)
                  (nextcat '(noun (proper yes))))
	:then 'noun
	:CF 'U))


(setq adj-noun-verb+pronr02
   (make-lexdisr
	:if '(or (prevcat-agr 'noun 'adj)
                 (and (nextcat-agr 'noun 'adj)
                      (not (nextcat '(adj (type qualif))))
                      (not (nextword-typ '&pref-not-noun)))
            ; la precedente per evitare agreement con 'dei'
                 (and (prevcat-agr '(adj (type 'qualif)) 'adj)
                      (prev2cat 'noun)))
	:then 'adj
	:CF 'U))

(setq adj-noun-verb+pronr03	; distanza non legale.
   (make-lexdisr
	:if '(and (prev2cat 'noun)
		  (prevcat 'adv)
                  (nextcat 'punct))
	:then 'adj
	:CF 'U))

(setq adj-noun-verb+pronr04	; settori agricoli e Industriali
   (make-lexdisr
	:if '(and (prev2cat 'adj)
		  (prevcat 'conj))
	:then 'adj
	:CF 'U))

(setq adj-noun-verb+pronr05	; qualche locale debole precipitazione
   (make-lexdisr
	:if '(and (prevcat '(adj (type indef)))
                  (nextcat '(adj (type qualif)))
                  (next2cat-agr 'noun 'adj)
                  (not (prevword-typ '&pref-not-noun)))
            ; la precedente per evitare agreement con 'dei'
	:then 'adj
	:CF 'U))

(setq adj-noun-verb+pronr06	; quelli amici
   (make-lexdisr
	:if '(and (prevcat '(pron (type demons)))
                  (nextcat 'punct))
	:then 'adj
	:CF 'U))

;******************************************************
;*** adj V prep (default: adj) [FINO, LUNGO; ENGLISH: IN]

(putrule 'adj-prep
         '(adv-prepr02 adj-prepr01 adj-prepr02 adj-prepr03 adj-prepr04e)
             ; *** rule adv-prepr02 specifies that, if the next element
             ;     is governable by the preposition, then this word is
             ;     a preposition
         'lexdisrules
         '(adj prep))

(putrule 'adj-prep 'adj 'defaultc)

;......................................................

(setq adj-prepr01
   (make-lexdisr
	:if '(and (not (prevcat-agr 'art 'adj))
                  (or (nextword-typ '&until-prep)
                      (nextcat 'art)))
	:then 'prep
	:CF 'A))

(setq adj-prepr02			; Lungo la frontiera
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat 'art))
	:then 'prep
	:CF 'A))

(setq adj-prepr03	; sono stati segnalati Lungo la strada; .. o lungo le strade
   (make-lexdisr
	:if '(and (prevcat '(conj verb))
                  (nextcat 'art))
	:then 'prep
	:CF 'A))

(setq adj-prepr04e	; "in" in English
   (make-lexdisr
	:if '(and (currword-typ '&loc-prep-1)
                  (or (not (nextcat 'punct))
                      (nextchar '(#\"))))	; virgolette aperte "  ; formerly also #\«
                                                ; but this is no more accepted by the compiler (09/11/17)
	:then 'prep
	:CF 'A
        :lang 'english))

;******************************************************
;*** adj V prep V verb (default: adj) [SALVO]

(putrule 'adj-prep-verb
         '(adj-prep-verbr01 adj-prep-verbr02 adj-prep-verbr03 adj-prep-verbr04)
         'lexdisrules
         '(adj prep verb))

(putrule 'adj-prep-verb 'adj 'defaultc)

;......................................................

(setq adj-prep-verbr01
   (make-lexdisr
	:if '(nextword-typ '&prep-subord-dep)
	:then 'prep
	:CF 'A))

(setq adj-prep-verbr02
   (make-lexdisr
	:if '(and (not (prevword-typ '&to-do))	; fatto salvo il ...
                  (nextcat 'art))
	:then 'adj
	:CF 'U))

(setq adj-prep-verbr03
   (make-lexdisr
	:if '(prevcat 'pron)
	:then 'verb
	:CF 'U))

(setq adj-prep-verbr04				; , Salvo quanto disposto
   (make-lexdisr
	:if '(and (prevcat 'punct)
                  (nextcat 'pron))
	:then 'prep
	:CF 'U))

;************************************************************
;*** adj V prep V pron (default: adj)
;    [QUALE]
; *** these are the same rules as adj-pron, since the prepositional
;     interpretation of "quale", "quali" is rare

(putrule 'adj-prep-pron
	'(adj-prep-pronr01 adj-prep-pronr02 adj-prep-pronr03 
          adj-pronr03 adj-pronr04ics adj-pronr05e
          adj-pronr06 adj-pronr07 adj-pronr08 adj-pronr09 adj-pronr10
          adj-pronr11 adj-pronr12 adj-pronr13 adj-pronr14i adj-pronr15e
          adj-pronr16e adj-pronr17i adj-pronr18i adj-pronr19 adj-pronr20ics
          adj-pronr21e adj-pronr22 adj-pronr23 adj-pronr24 adj-pronr25
          adj-pronr26 ;adj-pronr27i 
          adj-pronr28e adj-pronr29e adj-pronr30c)
	'lexdisrules
	'(adj prep pron))

(putrule 'adj-pron 'adj 'defaultc)

;......................................................

(setq adj-prep-pronr01
   (make-lexdisr
	:if '(and (prevcat-agr 'art 'pron))
	:then 'pron
	:CF 'C))

(setq adj-prep-pronr02
   (make-lexdisr
	:if '(and (not (nextcat-agr 'noun 'adj))
                  (not (nextcat '(noun (proper yes))))
                  (not (prevcat '(nil verb prep)))
                  (not (prevcat-agr 'art 'pron)))
	:then 'prep
	:CF 'C))

(setq adj-prep-pronr03
   (make-lexdisr
	:if '(and (not (nextcat-agr 'noun 'adj))
                  (prevcat '(nil verb prep))
                  (not (prevcat-agr 'art 'pron)))
	:then 'pron
	:CF 'C))

;************************************************************
;*** adj V pron (default: adj)
;    [QUESTO, QUELLO, SUO, ALTRO, QUANTO/QUANTI, ALCUNI, TANTI, NESSUNO,
;     ALTRUI; WHICH, WHAT]

(putrule 'adj-pron
	'(adj-pronr03 adj-pronr04ics adj-pronr05e
          adj-pronr06 adj-pronr07 adj-pronr08 adj-pronr09 adj-pronr10
          adj-pronr11 adj-pronr12 adj-pronr13 adj-pronr14i adj-pronr15e
          adj-pronr16e adj-pronr17i adj-pronr18i adj-pronr19 adj-pronr20ics
          adj-pronr21e adj-pronr22 adj-pronr23 adj-pronr24 adj-pronr25
          adj-pronr26 ;adj-pronr27i 
          adj-pronr28e adj-pronr29e adj-pronr30c)
	'lexdisrules
	'(adj pron))

(putrule 'adj-pron 'adj 'defaultc)

;......................................................


(setq adj-pronr03		; *** quello italiano
   (make-lexdisr
	:if '(and (currword-typ '&that)
                  (nextsemtype 'adj '£geogr))
	:then 'pron
	:CF 'A))

; the Italian version does not apply to "la", "nei", "dei"
; the rule also excludes application to "Vi amo", in the
; sigla interpretation of "Vi" as roman ordinal number "sixth"
(setq adj-pronr04ics 
   (make-lexdisr
	:if '(and (nextcat-agr 'noun 'adj)
                  (not (nextword-typ '&pref-not-noun))
                  (or (not (and (prevcat '(nil pron))
                                (currtype 'ordin)
                                (nextcat 'verb)))
                      (nextword-typ '&section)))
	:then 'adj
	:CF 'A
        :lang '(italian catalan spanish)))

(setq adj-pronr05e 
   (make-lexdisr
	:if '(and (not (currword-typ '&first-pers-pron))  ; I
                  (nextcat-agr 'noun 'adj)
                  (not (and (nextcat 'verb)
                            (nexttype 'mod)))
                  (not (and (nextcat 'verb)
                            (prevcat 'noun)
                            (currtype 'relat)))
                  (not (and (currtype 'relat)
                            (nextcat '(noun (proper yes))))))	
                                    ; * What John says
	:then 'adj
	:CF 'A
        :lang 'english))

(setq adj-pronr06
   (make-lexdisr
	:if '(and (nextword-typ '&base-relat)
                  (not (currtype 'poss)))	; la cosa sua che
	:then 'pron
	:CF 'A))

(setq adj-pronr07
   (make-lexdisr
	:if '(and (nextcat-agr 'adj 'adj)
                  (not (nextword-typ '&pref-prep-adj))
                  (next2cat 'noun))
	:then 'adj
	:CF 'A))

(setq adj-pronr08
   (make-lexdisr
	:if '(and (prevcat-agr 'noun 'adj)
                  (currtype 'deitt))
	:then 'adj
	:CF 'A))

(setq adj-pronr09	; i bisogni SUOI e della sua famiglia
   (make-lexdisr
	:if '(and (prevcat-agr 'noun 'adj)
                  (nextcat '(conj punct nil)))
	:then 'adj
	:CF 'A))

(setq adj-pronr10
   (make-lexdisr
	:if '(and (nextcat 'num)
                  (next2cat 'noun))
	:then 'adj
	:CF 'A))

(setq adj-pronr11
   (make-lexdisr
	:if '(and (nextchar #\')
                  (next2cat 'noun))
	:then 'adj
	:CF 'A))

(setq adj-pronr12		; questi ultimi cinque anni
   (make-lexdisr
	:if '(and (currtype 'demons)
                  (nextcat-agr '(adj (type ordin)) 'adj)
                  (next2cat '(num noun)))
	:then 'adj
	:CF 'A))

(setq adj-pronr13	; egli stesso
   (make-lexdisr
	:if '(and (prevcat-agr 'pron 'adj)
                  (currword-typ '&pron-adj))
	:then 'adj
	:CF 'A))

(setq adj-pronr14i
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (nextcat 'verb))
	:then 'adj
	:CF 'A
        :lang 'italian))

(setq adj-pronr15e
   (make-lexdisr
	:if '(and (not (nextcat-agr 'noun 'adj))
	          (not (and (nextcat 'verb)
	                    (nextmood 'participle)
                            (next2cat 'noun)))
	          (not (and (nextcat 'noun)
                            (next2cat 'noun)))
	          (not (and (nextcat 'adj)	; * these large-scale accidental discharges
                            (next2cat 'adj)))
	          (not (and (currtype 'poss)
                            (nextmood 'gerund))))
	:then 'pron
	:CF 'A
        :lang 'english))

(setq adj-pronr16e
   (make-lexdisr
	:if '(and (currtype 'poss)
                  (nextmood 'gerund))
	:then 'adj
	:CF 'A
        :lang 'english))

(setq adj-pronr17i
   (make-lexdisr
	:if '(and (not (prevcat-agr 'noun 'adj))
                  (not (and (prevcat-agr 'adj 'adj)	; confessioni religiose diverse
                            (prev2cat 'noun)))
                  (not (nextcat-agr 'noun 'adj))
                  (not (and (currtype 'demons) (nexttype 'ordin)))	; questi ultimi
                  (not (currword-typ '&adj-pref-noun))		; *** ottava
	          (not (nextcat 'num)))
	:then 'pron
	:CF 'A
        :lang 'italian))

(setq adj-pronr18i
   (make-lexdisr
	:if '(and (or (not (prevcat 'noun))
                      (not (currtype 'poss)))
                  (nextword-typ '&pref-not-noun))
	:then 'pron
	:CF 'A
        :lang 'italian))

(setq adj-pronr19
   (make-lexdisr
	:if '(and (nextcat '(prep pron (word-typ &base-relat) conj (type subord)
                         punct interj phras))
                  (or (not (nextchar '(#\")))	; virgolette aperte "  ; formerly also #\«
                                                ; but this is no more accepted by the compiler (09/11/17)
                      (not (next2cat 'noun)))
                  (not (currword-typ '&adj-pref-noun))		; *** ottava
                  (not (and (prevcat-agr 'adj 'adj)
                            (prev2cat 'noun)))
                  (not (prevcat-agr 'noun 'adj)))
	:then 'pron
	:CF 'A))

(setq adj-pronr20ics
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (not (nextcat '(pron (type ordin))))	; QUESTI ultimi
                  (not (nextcat 'prep)))		; dalla
	:then 'pron
	:CF 'A
        :lang '(italian catalan spanish)))

(setq adj-pronr21e
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (not (or (nextcat-agr 'noun 'adj)
                           (and (nextcat 'noun)
                                (next2cat 'noun)))))
	:then 'adj
	:CF 'A
        :lang 'english))

(setq adj-pronr22
   (make-lexdisr
	:if '(and (nextcat '(adv art))
                  (not (prevcat 'noun)))
	:then 'pron
	:CF 'A))

(setq adj-pronr23
   (make-lexdisr
	:if '(and (nextcat-agr 'adj 'pron)
                  (not (currword-typ '&adj-pref-noun))		; *** ottava
                  (not (next2cat 'noun)))
	:then 'pron
	:CF 'A))

(setq adj-pronr24
   (make-lexdisr
	:if '(and (currtype 'deitt)
                  (nextcat-agr 'noun 'adj))
	:then 'adj
	:CF 'A))

(setq adj-pronr25
   (make-lexdisr
	:if '(and (currtype 'deitt)
                  (and (nextcat-agr 'adj 'adj) 
                       (next2cat 'noun)))
	:then 'adj
	:CF 'A))

(setq adj-pronr26
   (make-lexdisr
	:if '(and (nextcat 'art)
                  (prevcat 'verb))
	:then 'pron
	:CF 'U))

;(setq adj-pronr27i
;   (make-lexdisr
;	:if '(and (nextcat '(prep conj pron verb punct interj phras))
;                  (not (and (prevcat '(nil art))
;                            (nextcat 'noun))))
;	:then 'pron
;	:CF 'U
;        :lang 'italian))

(setq adj-pronr28e
   (make-lexdisr
	:if '(and (nextcat '(prep conj pron verb punct interj phras))
	          (or (not (and (prevcat '(nil art prep (word-typ &date-prep)))
                                (nextcat 'noun)))
                      (currperson 'p1)))	; fifteenth MAY
                                                ; but ok for I want
	:then 'pron
	:CF 'U
        :lang 'english))

(setq adj-pronr29e	; are What wittgenstein calls
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (nextcat '(noun (proper yes))))
	:then 'pron
	:CF 'U
        :lang 'english))

(setq adj-pronr30c	; i aquests son
   (make-lexdisr
	:if '(and (prevcat '(conj (type coord)))
                  (not (prev2cat 'adj))
                  (nextcat 'verb))
	:then 'pron
	:CF 'U
        :lang 'catalan))

;************************************************************
;*** adj V pron V verb (default: adj) [STESSE, STESSI, ULTIMA, SEGUENTE]

(putrule 'adj-pron-verb
         '(adj-pron-verbr01 adj-pron-verbr02 adj-pron-verbr03 adj-pron-verbr04 
           adj-pron-verbr05 adj-pron-verbr06 adj-pron-verbr07 adj-pron-verbr08
           adj-pron-verbr09)
         'lexdisrules
         '(adj pron verb))

(putrule 'adj-pron-verb 'adj 'defaultc)

;......................................................

(setq adj-pron-verbr01
   (make-lexdisr
	:if '(and (prevcat-agr 'art 'adj)
                  (nextcat-agr 'noun 'adj))
	:then 'adj
	:CF 'A))

(setq adj-pron-verbr02
   (make-lexdisr
	:if '(nextcat-agr 'noun 'pron)
	:then 'adj
	:CF 'A))

(setq adj-pron-verbr03
   (make-lexdisr
	:if '(and (nextcat-agr 'adj 'pron)
                  (next2cat 'noun))
	:then 'adj
	:CF 'A))

(setq adj-pron-verbr04
   (make-lexdisr
	:if '(prevcat-agr '(pron noun) 'adj)
	:then 'adj
	:CF 'A))

(setq adj-pron-verbr05
   (make-lexdisr
	:if '(nextcat 'adv)
	:then 'verb
	:CF 'A))

(setq adj-pron-verbr06
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (nextmood 'gerund))
	:then 'verb
	:CF 'A))

(setq adj-pron-verbr07
   (make-lexdisr
	:if '(and (or (prevcat-agr 'art 'pron)
                      (prevcat-agr '(adj (type (deitt demons indef))) 'pron))
                  (nextcat 'verb))
	:then 'pron
	:CF 'U))

(setq adj-pron-verbr08
   (make-lexdisr
	:if '(and (prevcat-agr 'adj 'pron)
                  (nextword-typ '&base-relat))
	:then 'pron
	:CF 'A))

(setq adj-pron-verbr09
   (make-lexdisr
	:if '(and (or (prevcat-agr 'art 'pron)
                      (prevcat-agr '(adj (type (deitt demons indef))) 'pron))
                  (nextcat 'punct)
                  (or (not (nextchar #\:))
                      (next2cat nil)))
	:then 'pron
	:CF 'A))

;************************************************************
;*** adj V verb (default: verb) [TENERE, FA, SCORSO, CONTINUA; also adj vs.
;                                present participles in English]

(putrule 'adj-verb
         '(adj-verbr00i adj-verbr01 adj-verbr02e adj-verbr03 adj-verbr04
           adj-verbr05 adj-verbr06 adj-verbr07 adj-verbr09ics 
           adj-verbr08e adj-verbr10i adj-verbr11e adj-verbr12 
           adj-verbr13 adj-verbr14 adj-verbr15 adj-verbr16
           adj-verbr17 adj-verbr18 adj-verbr19 adj-verbr20
           adj-verbr21 adj-verbr22 adj-verbr23 adj-verbr24
           adj-verbr25 adj-verbr26 adj-verbr27 adj-verbr28 
           adj-verbr29i adj-verbr30 adj-verbr31 adj-verbr32 
           adj-verbr33i adj-verbr35i adj-verbr36)
         'lexdisrules
         '(adj verb))

(putrule 'adj-verb 'adj 'defaultc)

;......................................................

(setq adj-verbr00i		; Come FA ...
   (make-lexdisr
	:if '(and (currword-typ '&adj-after)
                  (not (prevsemtype '££timemeasure)))
	:then 'verb
	:CF 'A))

(setq adj-verbr01
   (make-lexdisr
	:if '(and (currword-typ '&to-continue)
                  (or (nextword-typ '&continue-prep)
                      (and (nextcat 'adv)
                           (next2word-typ '&continue-prep))))
	:then 'verb
	:CF 'A))

(setq adj-verbr02e
   (make-lexdisr
	:if '(and (prevword-typ '&vgov-prep)		'to'
                  (currmood 'infinite)
                  (not (nextcat 'noun)))
	:then 'verb
	:CF 'A
        :lang 'english))
 
(setq adj-verbr03
   (make-lexdisr
	:if '(and (currtype 'deitt)
                  (prevword-typ '&time-noun))
	:then 'adj
	:CF 'A))

(setq adj-verbr04
   (make-lexdisr
	:if '(and (currtype 'deitt)
                  (nextword-typ '&time-noun))
	:then 'adj
	:CF 'A))

(setq adj-verbr05		; la Scorsa Domenica
   (make-lexdisr
	:if '(and (currword-typ '&past-adj)
                  (prevword-typ '&time-ref-noun))
	:then 'adj
	:CF 'A))

(setq adj-verbr06
   (make-lexdisr
	:if '(and (not (currword-typ '&adj-after)) ; Fa parte
                  (nextcat-agr 'noun 'adj)
                  (not (nextword-typ '&pref-not-noun-1))
                  (not (and (currmood 'participle)
                            (currtense 'past)
                            (or (prevcat '(verb (type aux)))
                                (and (prevcat 'adv)
                                     (prev2cat '(verb (type aux))))))))
	:then 'adj
	:CF 'A))

(setq adj-verbr07
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
                  (nextword-typ '&agent-compl-prep))
	:then 'verb
	:CF 'A))

(setq adj-verbr08e
   (make-lexdisr
	:if '(and (prevcat 'noun)	; insolvencies threatening the
	          (currmood+tense 'participle 'pres)
                  (nextcat 'art))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq adj-verbr09ics
   (make-lexdisr
	:if '(prevcat-agr 'noun 'adj)
	:then 'adj
	:CF 'A
        :lang '(italian catalan spanish)))

(setq adj-verbr10i
   (make-lexdisr
 	:if '(currmood+tense 'participle 'pres)
	:then 'adj
	:CF 'A
        :lang 'italian))

(setq adj-verbr11e
   (make-lexdisr
 	:if '(and (currmood+tense 'participle 'pres)
                  (not (nextcat '(art pron adj (deitt indef anaph poss)))))
	:then 'adj
	:CF 'A
        :lang 'english))

(setq adj-verbr12
   (make-lexdisr
	:if '(and (nextcat 'art)
                  (not (prevword-typ '&predverb))
                  (not (nextword-typ '&partitive-art))
                  (not (and (eq *SYSTEM-CONTEXT* 'atlas)
                            (currsemtype 'adj '£weather-eval))))
	:then 'verb
	:CF 'A))

(setq adj-verbr13
   (make-lexdisr
	:if '(and (prevword-typ '&to-be)
                  (prevtype 'main)
                  (or (prevtense 'pres)
                      (prevtense 'imperf)))
	:then 'adj
	:CF 'A))

(setq adj-verbr14
   (make-lexdisr
	:if '(and (prevword-typ '&to-be)
                  (not (prevtype 'aux))
                  (currword-typ '&adj-pref-verb)
                  (not (prev2cat 'pron))
                  (or (prevtense 'pres)
                    ;  (prevtense 'imperf)
                                      ))
	:then 'adj
	:CF 'A))

(setq adj-verbr15	                     ; for "pubblica e privata"
   (make-lexdisr
	:if '(and (prevword-typ 'base-conj)	     ; base-conj: 'e' and 'o'
                  (prev2cat 'adj))
	:then 'adj
	:CF 'A))

(setq adj-verbr16	                     ; for "importante", "interessante", ...
   (make-lexdisr
 	:if '(currmood+tense 'participle 'pres)
	:then 'adj
	:CF 'A))

(setq adj-verbr17	                     ; for "e` molto popolare", ...
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (prev2word-typ '&to-be)
                  (not (currmood 'participle)))
	:then 'adj
	:CF 'A))

(setq adj-verbr18	                     ; for "piu' elevata"
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (prevtype '(quant compar)))
	:then 'adj
	:CF 'A))

(setq adj-verbr19
   (make-lexdisr
	:if '(and (nextcat 'verb)	; le FA vedere
                  (currword-typ '&past-adj-2))
	:then 'verb
	:CF 'U))

(setq adj-verbr20
   (make-lexdisr
	:if '(and (currtype 'deitt)
                  (prevcat 'pron))
	:then 'verb
	:CF 'U))

(setq adj-verbr21
   (make-lexdisr
	:if '(and (prevtype 'aux)	            ; ha deciso
                  (currmood '(participle gerund))
                  (not (and (eq *SYSTEM-CONTEXT* 'atlas)
                            (currsemtype 'adj '£weather-eval))))
	:then 'verb
	:CF 'U))

(setq adj-verbr22
   (make-lexdisr
	:if '(prevword-typ '&refl-pron-2)
	:then 'verb
	:CF 'U))

(setq adj-verbr23
   (make-lexdisr
	:if '(and (prev2cat 'verb)	; iniziavano a Circolare voci
                  (prevword-typ '&continue-prep)
                  (not (nextcat-agr 'noun 'adj)))
	:then 'verb
	:CF 'U))

(setq adj-verbr24
   (make-lexdisr
	:if '(and (prev2type 'aux)	; viene anche ispirata
                  (prevcat 'adv))
	:then 'verb
	:CF 'U))

(setq adj-verbr25
   (make-lexdisr
	:if '(and (prevcat 'prep)	; obiettivo di Tenere insieme
                  (nextcat 'adv)
                  (not (nextcat-agr 'noun 'adj)))
	:then 'verb
	:CF 'U))

(setq adj-verbr26
   (make-lexdisr
	:if '(and (prevcat 'pron)	; obiettivo che dura da
                  (prevtype 'relat)
                  (nextcat 'prep))
	:then 'verb
	:CF 'U))

(setq adj-verbr27
   (make-lexdisr
	:if '(and (nextcat 'conj)	; Nascosta o dissotterrata
                  (nexttype 'coord)
                  (next2cat 'verb))
	:then 'verb
	:CF 'U))

(setq adj-verbr28
   (make-lexdisr
	:if '(and (prevcat 'verb)	; sa Leggere o vedere
                  (nextcat 'conj)
                  (nexttype 'coord)
                  (next2cat 'verb))
	:then 'verb
	:CF 'U))

(setq adj-verbr29i
   (make-lexdisr
	:if '(prevchar #\-)		; la zingara - Continua - 
	:then 'verb
	:CF 'U
        :lang 'italian))

(setq adj-verbr30
   (make-lexdisr
	:if '(and (prevcat '(verb (type mod)))	; li vogliamo Tenere
                  (currmood 'infinite))
	:then 'verb
	:CF 'U))

(setq adj-verbr31	                     ; attentati COMPIUTI oggi in
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
                  (nextcat '(adv (type time))))
	:then 'verb
	:CF 'A))

(setq adj-verbr32
   (make-lexdisr
	:if '(and (prevcat '(conj (type coord)))	
                  (not (prevcat 'adj))
                  (not (prev2cat 'adj))
                  (not (and (prev2cat 'verb)
                            (prev2mood 'participle)
                            (not (currmood 'participle)))))
	:then 'verb
	:CF 'U))


(setq adj-verbr33i
   (make-lexdisr
	:if '(and (prevcat 'prep)		; di TUTELARE interessi privati
                  (not (nextcat-agr 'noun 'adj)))
	:then 'verb
	:CF 'U
        :lang 'italian))

(setq adj-verbr34i
   (make-lexdisr
	:if '(and (prev2cat 'verb)
                  (prev2type 'mod)
                  (prevcat 'adv)
                  (currmood 'infinite))
	:then 'verb
	:CF 'U
        :lang 'italian))

(setq adj-verbr35i		; era rimasta abbandonata
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
                  (or (prevcat 'verb)
		      (and (prevcat '(pron (form clitic)))
                           (prev2cat '(verb (type aux))))))
	:then 'verb
	:CF 'U
        :lang 'italian))

(setq adj-verbr36
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
                  (or (and (nextcat 'prep)
                           (verb-govern-next-prep))
		      (and (nextcat 'adv)
                           (next2cat 'prep)
                           (verb-govern-2next-prep))))
	:then 'verb
	:CF 'U))

;************************************************************
;*** adj V verb V verb+pron (default verb) [SCONTATI]
;    same rules as for adj V verb; verb+pron will never be chosen

(putrule 'adj-verb-verb+pron
	'(adj-verbr06 adj-verbr07 adj-verbr09ics 
          adj-verbr10i adj-verbr11e adj-verbr12 adj-verbr13 adj-verbr14
          adj-verbr15 adj-verbr17 adj-verbr18 adj-verb-verb+pronr01 
          adj-verb-verb+pronr02)
	'lexdisrules
	'(adj verb verb+pron))

(putrule 'adj-verb-verb+pron 'verb 'defaultc)

;......................................................

(setq adj-verb-verb+pronr01
    (make-lexdisr
	:if '(and (prevcat 'noun)
                  (nextcat '(punct nil)))
	:then 'adj
	:CF 'U))

(setq adj-verb-verb+pronr02
    (make-lexdisr
	:if '(and (eq *SYSTEM-CONTEXT* 'atlas)
                  (currsemtype 'adj '£weather-eval))
           ; *** "i venti saranno moderati"
	:then 'adj
	:CF 'A))

;************************************************************
;*** adj V verb+pron (default adj) [CAMPALE, FIGURATIVI]

(putrule 'adj-verb+pron 'adj 'defaultc)

;......................................................

;**********************************************************
;*** adv V conj (default adv) [INOLTRE, PERO`, APPENA, QUANDO]
;	(default: adv)
 
(putrule 'adv-conj
	'(adv-conjr01 adv-conjr02 adv-conjr03 adv-conjr04 adv-conjr05
          adv-conjr05bis-e
          adv-conjr06 adv-conjr07 adv-conjr08 adv-conjr09 adv-conjr10
          adv-conjr11 adv-conjr12 adv-conjr13 adv-conjr14 adv-conjr15 
          adv-conjr16 adv-conjr17)
	'lexdisrules
	'(adv conj))

(putrule 'adv-conj 'adv 'defaultc)

;......................................................

; "Quando apre il museo?"
(setq adv-conjr01
   (make-lexdisr
	:if '(and (currtype 'interr)
		  (prevcat nil)
                  (or (interr-sent)	    ; the last item is a question mark
                      (single-verb-sent)))  ; there is just one verb in the sentence
	:then 'adv
	:CF 'C))

; "Quando apre il museo, ci vado subito"
(setq adv-conjr02
   (make-lexdisr
	:if '(and (currtype 'interr)
		  (or (prevcat nil)
                      (and (prevcat 'prep)
                           (prev2cat nil)))
                  (not (interr-sent))	    ; the last item is a question mark
                  (not (single-verb-sent)))  ; there is just one verb in the sentence
	:then 'conj
	:CF 'C))

; "Vorrei sapere quando apre il museo?"
(setq adv-conjr03
   (make-lexdisr
	:if '(and (currtype 'interr)
		  (prevcat 'verb)
                  (prevword-typ '&to-know-tell))
                                            ; vorrei sapere quando
                                            ; mi può dire quando
	:then 'adv
	:CF 'A))

; "Vorrei sapere quando apre il museo?"
(setq adv-conjr04
   (make-lexdisr
	:if '(and (currtype 'interr)
		  (prevcat '(pron (form clitic)))
                  (prev2word-typ '&to-tell))    ; può dirmi quando
	:then 'adv
	:CF 'A))

; "Vorrei sapere perche`/quando Giulia intende trasferirsi
; "Dimmi come hai fatto a trovarmi"
; "Ho agito come mi hai insegnato"
; "Quando/perche` andare a cercarlo, proprio non saprei"
; "Non so come andare al mare"
; "Perche`/quando sia andato non lo so"
; "Si agitava come fosse ancora spaventato"
(setq adv-conjr05
   (make-lexdisr
	:if '(and (currtype 'interr)
		  (not (prevcat 'art))
		  (beforecat 'verb)		; a verb before
		  (nextcat 'verb)
  	     	  (or (nextmood 'infinite)
	     	      (nextmood 'ind)
	     	      (nextmood 'cong)))
	:then 'conj
	:CF 'C))

; "... when trying to ..."
(setq adv-conjr05bis-e
   (make-lexdisr
	:if '(and (currtype 'interr)
		  (not (prevcat 'art))
		  (beforecat 'verb)		; a verb before
		  (nextcat 'verb)
  	     	  (nextmood 'gerund))
	:then 'conj
	:CF 'C))

; "Avevo appena visto "
(setq adv-conjr06
   (make-lexdisr
	:if '(and (prevtype 'aux)
                  (nextcat 'verb))
	:then 'adv
	:CF 'A))

; "Avevo appena visto "
(setq adv-conjr07
   (make-lexdisr
	:if '(nextchar #\.)
	:then 'adv
	:CF 'A))

; "Quando lo hai incontrato, io non ero in casa"
; "Come ti e` sembrato Luigi ?"
;*"Quando le balene normalmente si riproducono ?"
(setq adv-conjr08
   (make-lexdisr
	:if '(and (currtype 'interr)
		  (not (prevcat 'art))
		  (beforecat 'verb)		; a verb before
  	     	  (not (nextcat 'verb))
  	     	  (not (nextword-typ '&vgov-prep)))	; di
	:then 'conj
	:CF 'A))

; "Soltanto quando la vedo sono felice"
(setq adv-conjr09
   (make-lexdisr
	:if '(and (currtype 'interr)
		  (prevcat 'adv))
	:then 'conj
	:CF 'U))

;(setq adv-conjr09
;   (make-lexdisr
;	:if '(and (currtype 'interr)
;	          (nextcat 'verb)
;	          (or (nextmood 'ind)
;	              (nextmood 'condiz)))
;	:then 'adv
;	:CF 'U))

; Anzi, la sua impressione ...
(setq adv-conjr10
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat 'punct))
	:then 'adv
	:CF 'A))

(setq adv-conjr11
   (make-lexdisr
	:if '(and (prevcat nil)
	          (not (nextcat 'punct)))
	:then 'conj
	:CF 'A))

(setq adv-conjr12                               ; for "Lo conosco appena"
   (make-lexdisr
	:if '(nextcat '(punct adj conj))
	:then 'adv
	:CF 'U))


(setq adv-conjr13                               ; for "L'ho appena visto"
   (make-lexdisr
	:if '(and (prevcat '(verb (or (type aux) (type mod))))
                  (nextcat 'verb))
	:then 'adv
	:CF 'U))

(setq adv-conjr14 				; ad Appena 80 chilometri
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (nextcat 'num))
	:then 'adv
	:CF 'U))

(setq adv-conjr15                               ; un'economia appena avviata
   (make-lexdisr
	:if '(and (currword-typ '&adv-conj-pref)
                  (prevcat 'noun)
                  (nextcat 'verb)
                  (nextmood 'participle))
	:then 'adv
	:CF 'U))

(setq adv-conjr16          ; sono andato a casa, Infatti ero là
   (make-lexdisr
	:if '(and (currword-typ '&conj-adv-pref)
	          (prevcat 'punct)
                  (not (nextcat 'punct)))
	:then 'conj
	:CF 'U))

(setq adv-conjr17          ; This is a kind of pre-default: "appena", "quando"
   (make-lexdisr
	:if '(currword-typ '&adv-conj-pref)
	:then 'conj
	:CF 'U))

;************************************************************
;*** adv V conj V prep (default adv) [DOPO; AS]
;*** 'prep' is never chosen ***

(putrule 'adv-conj-prep
         '(adv-conj-prepr00e adv-conj-prepr01e adv-conj-prepr02 adv-conj-prepr03 
           adv-conj-prepr04  adv-conj-prepr05 adv-conj-prepr06e adv-prepr02)
         'lexdisrules
         '(adv conj prep))

(putrule 'adv-conj-prep 'adv 'defaultc)

;......................................................

(setq adv-conj-prepr00e		; As long as you want
   (make-lexdisr
	:if '(and (currword-typ '&as)
                  (nextcat 'adj)
                  (afterword-typ '&as))	; afterword-typ looks ahead up to 10 words
	:then 'adv
	:CF 'A
        :lang 'english))

(setq adv-conj-prepr01e		; As philosophy is
   (make-lexdisr
	:if '(and (currword-typ '&as)
                  (prevcat nil))
	:then 'conj
	:CF 'A
        :lang 'english))

(setq adv-conj-prepr02
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (nextmood 'infinite))
	:then 'conj
	:CF 'A))

(setq adv-conj-prepr03
   (make-lexdisr
	:if '(nextcat '(adj noun art predet prep num))
	:then 'prep
	:CF 'A))

(setq adv-conj-prepr04
   (make-lexdisr
	:if '(and (nextcat 'pron)
                  (next2cat 'verb))
	:then 'conj
	:CF 'A))

(setq adv-conj-prepr05
   (make-lexdisr
	:if '(and (nextcat 'pron)
                  (not (next2cat 'verb)))
	:then 'prep
	:CF 'A))

(setq adv-conj-prepr06e		; this is the default for English
   (make-lexdisr
	:if '(currword-typ '&as)
	:then 'prep
	:CF 'U
        :lang 'english))

;******************************************************
;*** adv V conj V prep V pron (default: pron) [COME]

(putrule 'adv-conj-prep-pron
         '(adv-conj-prep-pronr01 adv-conj-prep-pronr02 adv-conj-prep-pronr02a
           adv-conj-prep-pronr03
           adv-conj-prep-pronr04 adv-conj-prep-pronr05 adv-conj-prep-pronr06
           adv-conj-prep-pronr07 adv-conj-prep-pronr08 adv-conj-prep-pronr09
           adv-conjr06 adv-conjr07)
         'lexdisrules
         '(adv conj prep pron))

(putrule 'adv-conj-prep-pron 'prep 'defaultc)

;......................................................

(setq adv-conj-prep-pronr01
   (make-lexdisr
	:if '(and (prevcat nil)
		  (interr-sent))
	:then 'adv
	:CF 'A))

; si e' visto come essi vogliano ...
(setq adv-conj-prep-pronr02
   (make-lexdisr
	:if '(or (nextword-typ '&pron-not-obl)   ; it should be PRON NOT OBL
                 (nextword-typ '&refl-pron-2))
	:then 'conj
	:CF 'A))

(setq adv-conj-prep-pronr02a
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (nexttype 'mod)		; come devo, come posso
                  (nextperson 'p1))
	:then 'adv
	:CF 'A))

; si e' visto come essi vogliano ...
(setq adv-conj-prep-pronr03
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (or (not (nexttype 'mod))		; come devo, come posso
                      (not (nextperson 'p1)))
                  (not (nextcat 'noun)))      ; for ambiguities: ex. PARTE
	:then 'conj
	:CF 'A))

; si e' visto come essi vogliano ...
(setq adv-conj-prep-pronr04
   (make-lexdisr
	:if '(prevword-typ '&so)   
	:then 'conj
	:CF 'A))

; si e' visto come essi vogliano ...
(setq adv-conj-prep-pronr05
   (make-lexdisr
	:if '(and (nextcat 'pron)
                  (not (nextcat 'art))	; in case of ambiguity of next word
                  (next2cat 'verb)
                  (not (prevcat 'conj)))
	:then 'conj
	:CF 'A))

(setq adv-conj-prep-pronr06		; COME era 
   (make-lexdisr
	:if '(and (nextcat '(noun art adj pron prep))
                  (not (nextword-typ '&noun-verb-pref-verb)))
	:then 'prep
	:CF 'A))

(setq adv-conj-prep-pronr07
   (make-lexdisr
	:if '(and (not (prevcat 'art))
                  (beforecat 'verb)
                  (not (nextcat 'verb))
                  (not (nextword-typ '&vgov-prep))
                  (prevmood 'participle))
	:then 'conj
	:CF 'A))

(setq adv-conj-prep-pronr08
   (make-lexdisr
	:if '(nextchar '(#\. #\! #\? #\; #\:))
	:then 'adv
	:CF 'A))

(setq adv-conj-prep-pronr09
   (make-lexdisr
	:if '(and (prev2cat 'conj)
                  (or (and (prevchar #\,)	         ; se, Come e quando
                           (nextcat '(conj (type coord))))
                      (prevcat '(conj (type coord)))))   ; se e Come
	:then 'conj
	:CF 'A))

;******************************************************
;*** adv V conj V pron (default: pron) [DOVE, PERCHE']

(putrule 'adv-conj-pron
	'(;adv-conj-pronr01s 
          adv-conj-pronr02 adv-conj-pronr03
          adv-conj-pronr04 adv-conj-pronr05 adv-conj-pronr06 adv-conj-pronr07
          adv-conj-pronr08 adv-conj-pronr09 adv-conj-pronr10 adv-conj-pronr11)
	'lexdisrules
	'(adv conj pron))

(putrule 'adv-conj-pron 'conj 'defaultc)

;......................................................

;(setq adv-conj-pronr01s	COMMENTED because the ¿ symbol is not accepted by the compiler
;   (make-lexdisr
;	:if '(prevchar #\¿)
;	:then 'adv
;	:CF 'A
;	:lang 'spanish))

; "Proprio non saprei dove andare a cercarlo"
; "Dove sia andato non lo so"
; "Mi vuoi dire ora dove vuoi andare"
(setq adv-conj-pronr02
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (prevsemtype '£interr-obj-verb))
	:then 'adv
	:CF 'A))

(setq adv-conj-pronr03
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat 'verb)
                  (nextmood '(ind condiz)))
	:then 'adv
	:CF 'A))

(setq adv-conj-pronr04
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat '(punct nil)))
	:then 'adv
	:CF 'A))

; "Proprio non saprei dove andare a cercarlo"
; "Dove sia andato non lo so"
; "Mi vuoi dire ora dove vuoi andare"
(setq adv-conj-pronr05
   (make-lexdisr
	:if '(and (nextmood '(infinite cong))
	          (not (nextmood 'ind)))
	:then 'adv
	:CF 'A))

; "Proprio non saprei dove andare a cercarlo"
; "Dove sia andato non lo so"
; "Mi vuoi dire ora dove vuoi andare"
(setq adv-conj-pronr06
   (make-lexdisr
	:if '(prevcat 'verb)
	:then 'conj
	:CF 'A))

; "Dove tu hai incontrato Luigi, stanno costruendo molti palazzi"
; "Spesso, dove io vai al mare, piove per giorni e giorni"
;*"Dove tu di solito vai al mare ?"
(setq adv-conj-pronr07
   (make-lexdisr
	:if '(and (not (prevcat+ 'noun))
                  (not (prevcat+ 'adj))
                  (not (nextcat 'verb))
                  (or (not (nextcat 'pron))
                      (not (next2cat 'verb))))
	:then 'conj
	:CF 'A))

; "La casa dove vado al mare e` molto vecchia"
; "Ho visto la  casa dove Giulia intende trasferirsi"
;*"Ho visto una casa molto bella dove mi ha portato mio padre"
;*"Il tramonto, dove vado al mare, e` molto suggestivo"
;*"I ragazzi, dove hanno dei bei ricordi, vogliono sempre ritornare"
;*"Tua sorella dove andra` in vacanza ?"
;*"La tua casa dove sara` costruita ?"
(setq adv-conj-pronr08
   (make-lexdisr
	:if '(and (prevcat+ '(noun adj))
                  (currword-typ '&where))
	:then 'pron
	:CF 'A))

; "e' stata messa in difficoltà proprio Perchè .."
(setq adv-conj-pronr09
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (prevtype 'streng))
	:then 'conj
	:CF 'A))

; "Dove vorresti andare per le vacanze ?"
;*"Dove vado in vacanza ci sono bellissimi boschi"
(setq adv-conj-pronr10
   (make-lexdisr
	:if '(and (not (prevcat+ 'noun))
                  (not (prevcat+ 'adj))
                  (nextcat 'verb)
                  (or (nextmood 'ind)
   	              (nextmood 'condiz)))
	:then 'adv
	:CF 'A))

; "Dove l'hai vista?"
(setq adv-conj-pronr11
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat 'pron)
                  (next2cat 'verb)
                  (or (next2mood 'ind)
                      (next2mood 'condiz)))
	:then 'adv
	:CF 'A))

;**********************************************************
;*** adv V noun (default: adv) [COSA (interr), BENE, VIA, ORA, WAY, ANCHE]
 
(putrule 'adv-noun
         '(adv-nounr01i adv-nounr02 adv-nounr03 adv-nounr04 adv-nounr05 adv-nounr06i)
         'lexdisrules
         '(adv noun))

(putrule 'adv-noun 'adv 'defaultc)

;......................................................

(setq adv-nounr01i
   (make-lexdisr
	:if '(and (not (prevcat 'art))
                  (currword-typ '&noun-pref-adv))  ; anche
	:then 'adv
	:CF 'A
        :lang 'italian))

(setq adv-nounr02
   (make-lexdisr
	:if '(prevcat nil)
	:then 'adv
	:CF 'A))

(setq adv-nounr03
   (make-lexdisr
	:if '(prevcat-agr '(art adj num) 'noun)
	:then 'noun
	:CF 'A))

(setq adv-nounr04
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (currword-typ '&what))
	:then 'noun
	:CF 'A))

(setq adv-nounr05
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (currsemtype 'noun '£other-time-adv))  ; oggi, domani, dopodomani
	:then 'noun
	:CF 'U))

(setq adv-nounr06i
   (make-lexdisr
	:if '(nextword-typ '&adv-prep-head)	; ancora
	:then 'noun
	:CF 'U
        :lang 'italian))

;******************************************************
;*** adv V noun V prep (default: adv) [INSIEME, INTORNO, TRAMITE; CATALAN: SOBRE]

(putrule 'adv-noun-prep
         '(adv-prepr02 adv-noun-prepr01 adv-noun-prepr02 adv-noun-prepr03 
           adv-noun-prepr04c adv-noun-prepr05c adv-noun-prepr06i)
         'lexdisrules
         '(adv noun prep))

(putrule 'adv-noun-prep 'adv 'defaultc)

;.......................................................

(setq adv-noun-prepr01
   (make-lexdisr
	:if '(and (prevcat-agr '(art adj prep) 'noun)
                  (not (currword-typ '&pref-not-noun)))
	:then 'noun
	:CF 'C))

(setq adv-noun-prepr02
   (make-lexdisr
	:if '(nextword-typ '&near-prep)
	:then 'prep
	:CF 'A))

(setq adv-noun-prepr03
   (make-lexdisr
	:if '(and (prevcat '(nil punct))
                  (nextcat 'num))
	:then 'prep
	:CF 'A))

(setq adv-noun-prepr04c
   (make-lexdisr
	:if '(nextcat 'art)
	:then 'prep
	:CF 'A
        :lang 'catalan))

(setq adv-noun-prepr05c		; informació sobre teatre
   (make-lexdisr
	:if '(and (prevcat 'noun)
	          (nextcat 'noun))
	:then 'prep
	:CF 'A
	:lang 'catalan))

(setq adv-noun-prepr06i		; informazioni VIA fax
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (nextcat '(noun (proper no))))
	:then 'prep
	:CF 'A
	:lang 'italian))

;******************************************************
;*** adv V noun V prep V verb (default: adv) [INCONTRO]
;    same rules as for adv V noun V prep (VERB will never be chosen)

(putrule 'adv-noun-prep-verb
	'(adv-noun-prepr01 adv-noun-prepr02)
	'lexdisrules
	'(adv noun prep verb))

(putrule 'adv-noun-prep-verb 'adv 'defaultc)

;.......................................................

;************************************************************
;*** adv V noun V verb (default: adv) [ANCORA, APPUNTO; English: PART]
 
(putrule 'adv-noun-verb
	'(adv-noun-verbr01 adv-noun-verbr02e)
	'lexdisrules
	'(adv noun verb))

(putrule 'adv-noun-verb 'adv 'defaultc)

;......................................................

(setq adv-noun-verbr01
   (make-lexdisr
	:if '(prevcat-agr 'art 'noun)
	:then 'noun
	:CF 'A))
 
(setq adv-noun-verbr02e
   (make-lexdisr
	:if '(and (prevcat 'verb)
	          (nextcat 'prep))
	:then 'noun
	:CF 'A
	:lang 'english))
 
;******************************************************
;*** adv V phras [CATALAN "NO"]
;    (default: phras)

(putrule 'adv-phras
         '(adv-phrasr01)
         'lexdisrules
         '(adv phras))

(putrule 'adv-phras 'phras 'defaultc)

;......................................................

(setq adv-phrasr01
   (make-lexdisr
	:if '(nextcat '(adj verb))
	:then 'adv
	:CF 'U))

;**********************************************************
;******************************************************
;*** adv V phras V pron [SPANISH "SI'"]
;    (default: phras)

(putrule 'adv-phras-pron
         '(adv-phras-pronr01)
         'lexdisrules
         '(adv phras pron))

(putrule 'adv-phras-pron 'phras 'defaultc)

;......................................................

(setq adv-phras-pronr01
   (make-lexdisr
	:if '(nextcat 'verb)
	:then 'pron
	:CF 'U))

;**********************************************************
;*** adv V predet V pron (default: pron) [TUTTO]
 
(putrule 'adv-predet-pron
         '(predet-pronr01 predet-pronr02 adv-predet-pronr01 adv-predet-pronr02)
         'lexdisrules
         '(adv predet pron))

(putrule 'adv-predet-pron 'pron 'defaultc)

;......................................................

(setq adv-predet-pronr01
   (make-lexdisr
	:if '(nextcat 'adj)
	:then 'adv
	:CF 'U))

(setq adv-predet-pronr02
   (make-lexdisr
	:if '(nextword-typ '&this)
	:then 'predet
	:CF 'U))

;**********************************************************
;*** adv V prep (default: adv) [INVECE, ACCANTO, ADDOSSO, CIRCA, OLTRE]
 
(putrule 'adv-prep
	'(adv-prepr01e adv-prepr02 adv-prepr03 adv-prepr04
          adv-prepr05 adv-prepr06 adv-prepr07 adv-prepr08
          adv-prepr09 adv-prepr10 adv-prepr11 adv-prepr12)
	'lexdisrules
	'(adv prep))

(putrule 'adv-prep 'adv 'defaultc)

;......................................................

(setq adv-prepr01e		; ENGLISH PARTICLES
   (make-lexdisr
	:if '(verb-govern-adv)
            ; *** checks if the immediately preceding verb can govern the
            ;     current word (as a particle)
	:then 'adv
	:CF 'A
	:lang 'english))

(setq adv-prepr02
   (make-lexdisr
	:if '(and (prep-govern-next)
                  (not (and (currtype '(compar quant))
                            (nextcat 'num))))
	:then 'prep
	:CF 'A))

(setq adv-prepr03
   (make-lexdisr
	:if '(and (currtype '(compar quant))
                  (nextcat 'num))
	:then 'adv
	:CF 'A))

(setq adv-prepr04
   (make-lexdisr
	:if '(prevcat 'prep)
	:then 'adv
	:CF 'A))

(setq adv-prepr05
   (make-lexdisr
	:if '(and (prevword-typ '&far-fuori-1)
                  (currword-typ '&far-fuori-2))
	:then 'adv
	:CF 'A
        :lang 'italian))

(setq adv-prepr06
   (make-lexdisr
	:if '(nextword-typ '&loc-pron)
	:then 'adv
	:CF 'U))

(setq adv-prepr07		; è Invece utile per
   (make-lexdisr
	:if '(and (prevword-typ '&to-be)
                  (nextcat 'adj))
	:then 'adv
	:CF 'U))

(setq adv-prepr08
   (make-lexdisr
	:if '(and (nextcat '(adj noun art predet pron num))
                  (not (nextword-typ '&noun-verb-pref-verb))		; era
                  (or (not (nextcat 'prep)) (nextword-typ '&more))
                  (prep-govern-next t))   ; this checks that the prep can govern an NP
	:then 'prep
	:CF 'U))

(setq adv-prepr09
   (make-lexdisr
	:if '(and (nextcat 'verb)
	          (nextmood 'infinite))
	:then 'prep
	:CF 'U))

(setq adv-prepr10
   (make-lexdisr
	:if '(and (nextcat '(conj (type coord)))
		  (next2cat 'prep))
	:then 'prep
	:CF 'U))

(setq adv-prepr11
   (make-lexdisr
	:if '(and (currword-typ '&advers-prep)
		  (nextcat '(conj (type compar))))
	:then 'prep
	:CF 'U))

(setq adv-prepr12
   (make-lexdisr
	:if '(nextcat '(verb (mood gerund)))
	:then 'prep
	:CF 'A
        :lang 'english))

;************************************************************
;*** adv V prep V verb (default: prep) [MENO, ATTRAVERSO]
; ***  Verb ('menare') is never chosen 

(putrule 'adv-prep-verb
	'(adv-prep-verbr01 adv-prep-verbr02 adv-prep-verbr03)
	'lexdisrules
	'(adv prep verb))

(putrule 'adv-prep-verb 'adv 'defaultc)

;......................................................

(setq adv-prep-verbr01
   (make-lexdisr
	:if '(nextword-typ '&compar-prep)
	:then 'prep
	:CF 'U))

(setq adv-prep-verbr02
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (currword-typ '&through)
                  (nextcat '(art adj noun)))
	:then 'prep
	:CF 'U))

(setq adv-prep-verbr03
   (make-lexdisr
	:if '(nextcat 'art)
	:then 'prep
	:CF 'U))

;****************************************************
;*** adv V pron (default: adv) [THERE and WHY in ENGLISH; DEBAJO in SPANISH]
 
(putrule 'adv-pron
	'(adv-pronr01e adv-pronr02e adv-pronr03s)
	'lexdisrules
	'(adv pron))

(putrule 'adv-pron 'adv 'defaultc)

;......................................................

(setq adv-pronr01e
   (make-lexdisr
	:if '(prevword-typ '&to-be)
	:then 'pron
	:CF 'A
        :lang 'english))

(setq adv-pronr02e
   (make-lexdisr
	:if '(and (or (nextword-typ '&to-be)
                      (nexttype 'mod))
                  (not (currtype 'interr)))
	:then 'pron
	:CF 'A
        :lang 'english))

(setq adv-pronr03s
   (make-lexdisr
        :if '(and (prevcat 'prep)
                  (nextcat 'prep))
        :then 'pron
        :CF 'A
        :lang 'spanish))

;************************************************************
;*** adv V verb (default: adv) [PRESTO, SUBITO]
       
(putrule 'adv-verb 'adv 'defaultc)

;......................................................

;****************************************************
;*** art V noun V pron (default: art) [LA]
 
(putrule 'art-noun-pron
         '(adj-nounr34 art-noun-pronr01 ;art-noun-pronr02
           art-noun-pronr03
           art-noun-pronr04 art-noun-pronr05 art-noun-pronr06 art-noun-pronr07
           art-noun-pronr08 art-noun-pronr09 art-noun-pronr10 art-noun-pronr11
           art-noun-pronr12 art-noun-pronr13)
         'lexdisrules
         '(art noun pron))
   ; *** rule adj-nounr34 selects the interpretation 'noun' within a proper
   ;     name; it applies in 'De La Pena'

(putrule 'art-noun-pron 'art 'defaultc)

;......................................................

(setq art-noun-pronr01
   (make-lexdisr
	:if '(and (prevcat '(noun (proper nil)))
                  (nextcat 'noun))
	:then 'art
	:CF 'A))

;(setq art-noun-pronr02
;   (make-lexdisr
;	:if '(and (currproper)
;                  (prevcat '(noun (proper yes))))
;	:then 'noun
;	:CF 'A))

(setq art-noun-pronr03
   (make-lexdisr
	:if '(and (currproper)
                  (prevcat '(noun (proper yes)))
                  (nextcat '(noun (proper yes)))
                  (or (not (nextcat '(noun (proper nil))))
                      (not (nextcat-agr '(noun (proper nil)) 'art))))
	:then 'noun
	:CF 'A))

(setq art-noun-pronr04
   (make-lexdisr
	:if '(and (nextword-typ '&art-mid-relat)
                  (currtype 'def))
	:then 'art
	:CF 'A))

(setq art-noun-pronr05
   (make-lexdisr
	:if '(and (nextword-typ '&compar-pron)
                  (next2cat 'adj))
	:then 'art
	:CF 'A))

(setq art-noun-pronr06
   (make-lexdisr
	:if '(and (nextcat-agr 'adj 'art)	; LA mancata emanazione
                  (next2cat 'noun))
	:then 'art
	:CF 'A))

(setq art-noun-pronr07
   (make-lexdisr
	:if '(and (nextcat '(verb pron))
                  (not (nextcat-agr 'noun 'art))
                  (not (nexttype '(deitt poss))))   ; LA loro casa ; LA stessa casa
	:then 'pron
	:CF 'A))

(setq art-noun-pronr08
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (not (nextcat '(verb pron))))
	:then 'art
	:CF 'C))

(setq art-noun-pronr09
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (nextcat-agr 'verb 'art)
                  (nextmood 'participle))
	:then 'art
	:CF 'C))

(setq art-noun-pronr10
   (make-lexdisr
	:if '(not (nextcat '(verb pron prep)))
	:then 'art
	:CF 'A))

(setq art-noun-pronr11
   (make-lexdisr
	:if '(and (nextcat-agr 'verb 'art)
                  (nextmood 'participle))
	:then 'art
	:CF 'A))

(setq art-noun-pronr12			; Una delle ("Una" noun because proper)
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat 'prep)
                  (not (nexttype 'ordin)))	; "La prima prevedeva" o "La prima casa"
	:then 'pron
	:CF 'A))

(setq art-noun-pronr13	
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat 'pron)		; "La prima prevedeva"
                  (next2cat 'verb))
	:then 'art
	:CF 'A))

;****************************************************
;*** art V noun V pron V verb (default: art) [SPANISH: UNA]
; *** N.B. The verb interpretation is never chosen
 
(putrule 'art-noun-pron-verb
         '(art-noun-pronr01 ;art-noun-pronr02
           art-noun-pronr03
           art-noun-pronr06 art-noun-pronr07 art-noun-pronr08
           art-noun-pronr09 art-noun-pronr10 art-noun-pronr11)
         'lexdisrules
         '(art noun pron verb))

(putrule 'art-noun-pron-verb 'art 'defaultc)

;......................................................

;************************************************************
;*** art V num (default: art) [SPANISH: UN]
;    NUM is never chosen 

(putrule 'art-num 'art 'defaultc)

;************************************************************
;*** art V num V pron  (default: art) [UNO]
;    NUM is never chosen 

(putrule 'art-num-pron
	'(art-num-pronr01 art-num-pronr02 art-num-pronr03)
	'lexdisrules
	'(art num pron))

(putrule 'art-num-pron 'art 'defaultc)

;......................................................

(setq art-num-pronr01
   (make-lexdisr
	:if '(and (prevcat nil)
                  (or (nextcat nil)
                      (nextchar '(#\. #\! #\? #\:))))
	:then 'num
	:CF 'A))

(setq art-num-pronr02
   (make-lexdisr
	:if '(and (currtype 'indef)
                  (not (and (prevcat nil)
                            (or (nextcat nil)
                                (nextchar '(#\. #\! #\? #\:)))))
                  (not (nextcat-agr '(noun adj) 'art))
                  (not (nextchar '(#\' #\"))))	; virgolette aperte "  ; formerly also #\«
                                                ; but this is no more accepted by the compiler (09/11/17)
	:then 'pron
	:CF 'A))

(setq art-num-pronr03	; uno solo dei
   (make-lexdisr
	:if '(and (currtype 'indef)
                  (nextword-typ '&only)
                  (next2word-typ '&partitive-prep))
	:then 'pron
	:CF 'A))

;****************************************************
;*** art V prep V pron (default: prep) [CATALAN: en]
 
(putrule 'art-prep-pron
	'(art-prep-pronr01 art-prep-pronr02 art-prep-pronr03)
	'lexdisrules
	'(art prep pron))

(putrule 'art-prep-pron 'prep 'defaultc)

;......................................................

(setq art-prep-pronr01
   (make-lexdisr
	:if '(prevword-typ '&noun-gov-prep)
	:then 'prep
	:CF 'A
        :lang 'catalan))

(setq art-prep-pronr02
   (make-lexdisr
	:if '(nextcat 'verb)
	:then 'pron
	:CF 'A
        :lang 'catalan))

(setq art-prep-pronr03
   (make-lexdisr
	:if '(nextcat-agr '(noun adj) 'art)
	:then 'art
	:CF 'A
        :lang 'catalan))

;************************************************************
;*** art V noun V prep+art  (default: prep+art) [DEI]

(putrule 'art-noun-prep+art
	'(art-noun-prep+artr01 art-noun-prep+artr02 
          art-prep+artr01 ; art-prep+artr02 
          art-prep+artr03 art-prep+artr04) 
	'lexdisrules
	'(art noun prep+art))

(putrule 'art-noun-prep+art 'prep+art 'defaultc)

;......................................................

(setq art-noun-prep+artr01
   (make-lexdisr
	:if '(prevcat '(art (adj (type deitt))))
	:then 'noun
	:CF 'A))

(setq art-noun-prep+artr02
   (make-lexdisr
	:if '(and (nextcat 'prep)
                  (not (nextword-typ '&more))		; più
                  (not (nextword-typ '&art-relat)))	; quale
	:then 'noun
	:CF 'A))

;************************************************************
;*** art V prep+art  (default: prep+art) [DELLA, DELLE, DEGLI, DELLO]

(putrule 'art-prep+art
	'(art-prep+artr01 ; art-prep+artr02 
          art-prep+artr03 art-prep+artr04) 
	'lexdisrules
	'(art prep+art))

(putrule 'art-prep+art 'prep+art 'defaultc)

;......................................................

(setq art-prep+artr01
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (not (prevword-typ '&to-be))
     ; *** the next condition for comparatives ("più trafficata del mondo")
                  (not (prev2word-typ '&compar-adv))
     ; *** the next condition to exclude intervening reduced relatives
                  (not (and (prevmood 'participle)
                            (prev2cat '(art noun))))		; ... il concedente dell'acqua
                  (not (has-prevverb-subcat-class 'topic-verbs)))
	:then 'art
	:CF 'A))

;(setq art-prep+artr02
;   (make-lexdisr
;	:if '(and (prevcat 'adv)
;                  (prev2cat '(conj verb)))
;	:then 'art
;	:CF 'A))

(setq art-prep+artr03
   (make-lexdisr
	:if '(and (prevcat '(conj (type coord)))
                  (beforecat '(art (word-typ &partitive-art))))
	:then 'art
	:CF 'A))

(setq art-prep+artr04
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (not (prev-prep-govern)))
	:then 'art
	:CF 'A))

;****************************************************
;*** art V pron (default: art) [LO, LE, GLI, UNA ...]
 
(putrule 'art-pron
	'(art-pronr01iesf art-pronr02 art-pronr03  art-pronr04 art-pronr05
          art-pronr06 art-pronr07 art-pronr08 art-pronr09
          )
	'lexdisrules
	'(art pron))

(putrule 'art-pron 'art 'defaultc)

;......................................................

(setq art-pronr01iesf
   (make-lexdisr
	:if '(and (currtype 'indef)
                  (not (nextcat-agr '(noun adj) 'art))
                  (not (nextchar '(#\' #\"))))	; virgolette aperte "  ; formerly also #\«
                                                ; but this is no more accepted by the compiler (09/11/17)
	:then 'pron
	:CF 'A
        :lang '(italian english spanish french)))

(setq art-pronr02
   (make-lexdisr
	:if '(nextword-typ '&art-pron)		; "GLI uni e gli altri"
	:then 'art
	:CF 'A))

(setq art-pronr03
   (make-lexdisr
	:if '(and (nextword-typ '&noun-verb-pref-verb)		; se L' era
                  (not (prevcat 'prep))				; dopo L'era
                  (not (nextmood 'participle)))			; Lo stato di emergenza
	:then 'pron
	:CF 'A))

(setq art-pronr04
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (not (nextcat-agr 'noun 'art))
                  (not (and (nextcat-agr 'adj 'art)
                            (next2cat '(noun adj num pron (type poss)))))
                  (not (and (nextcat-agr 'pron 'art)
                            (next2cat 'verb)))
                  (not (nextmood 'participle))
                  (not (nextmood 'infinite)))
	:then 'pron
	:CF 'A))

(setq art-pronr05
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (nextcat 'prep)
                  (not (nextcat 'adv)))
	:then 'pron
	:CF 'C))

(setq art-pronr06
   (make-lexdisr
	:if '(and (prevcat 'prep)
              (or (not (nextcat '(verb pron)))
                       (nextmood 'participle)))
	:then 'art
	:CF 'C))

(setq art-pronr07
   (make-lexdisr
	:if '(or (not (nextcat '(verb pron prep)))
                 (nextmood 'participle))
	:then 'art
	:CF 'A))

(setq art-pronr08
   (make-lexdisr
	:if '(and (prevcat '(pron (form clitic)))
                  (nextcat 'verb))
	:then 'pron
	:CF 'A))

(setq art-pronr09
   (make-lexdisr
	:if '(and (nextcat '(pron (type refl-impers)))
                  (not (nextmood 'participle))
                  (not (nextmood 'infinite)))
	:then 'pron
	:CF 'A))

;****************************************************
;*** art V pron V verb (default: art) [Spanish: Unas]
 
(putrule 'art-pron-verb
	'(art-pron-verbr01s)
	'lexdisrules
	'(art pron verb))

(putrule 'art-pron-verb 'art 'defaultc)

;......................................................

(setq art-pron-verbr01s	
   (make-lexdisr
	:if '(not (nextcat '(noun adj)))
	:then 'pron
        :lang 'spanish
	:CF 'A))

;****************************************************
;*** art V verb (default: art) [ENGLISH GENITIVES: 's]
 
(putrule 'art-verb
	'(art-verbr01 art-verbr02)
	'lexdisrules
	'(art verb))

(putrule 'art-verb 'art 'defaultc)

;......................................................

(setq art-verbr01		; *** John's friend; the boy's car
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (nextcat 'noun))
	:then 'art
	:CF 'A))

(setq art-verbr02		; *** There's, he's
   (make-lexdisr
	:if '(prevcat 'pron)
	:then 'verb
	:CF 'A))

;******************************************************
;*** adv V conj V noun V prep V pron V verb (SPANISH: COMO)
;    (default: prep)

(putrule 'adv-conj-noun-prep-pron-verb 'prep 'defaultc)

;......................................................

;******************************************************
;*** conj V noun (SPANISH: PERO)
;    (default: conj)

(putrule 'conj-noun 'conj 'defaultc)

;......................................................

;**********************************************************
;*** conj V phras V pron  (default: conj) [SE; for HOPS: "sì" recognized as "se"
 
(putrule 'conj-phras-pron
         '(conj-phras-pronr01i conj-pronr00 conj-pronr01 conj-pronr02i conj-pronr03i 
           conj-pronr06i conj-pronr07i)
         'lexdisrules
         '(conj phras pron))

(putrule 'conj-phras-pron 'conj 'defaultc)

;......................................................

(setq conj-phras-pronr01i
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat '(punct nil)))
	:then 'phras
	:CF 'A
        :lang 'italian))
 
;************************************************************
;*** conj V prep (default: prep) [DI, ANCORCHE', NONOSTANTE]

(putrule 'conj-prep
         '(conj-prepr01 conj-prepr02 conj-prepr03 conj-prepr04 conj-prepr05)
         'lexdisrules
         '(conj prep))

(putrule 'conj-prep 'prep 'defaultc)

;......................................................

(setq conj-prepr01
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (nextmood 'infinite)
                  (not (currword-typ '&vgov-prep)))	; di
	:then 'conj
	:CF 'U))

(setq conj-prepr02
   (make-lexdisr
	:if '(prevword-typ '&compar-pron)
	:then 'conj
	:CF 'A))

(setq conj-prepr03					; until
   (make-lexdisr
	:if '(currword-typ '&vprefer-conj-prep)
	:then 'conj
	:CF 'A))

(setq conj-prepr04					; as
   (make-lexdisr
	:if '(and (currword-typ '&vprefer-conj-prep2)
                  (prevcat nil))
	:then 'conj
	:CF 'A
        :lang 'english))

(setq conj-prepr05
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (nextmood 'cong)
                  (not (currword-typ '&vgov-prep)))	; di
	:then 'conj
	:CF 'U))

;******************************************************
;*** conj V pron (default: conj) [Italian SE; CAtalan QUE]

(putrule 'conj-pron
         '(conj-pronr00 conj-pronr01 conj-pronr02i conj-pronr03i conj-pronr04c conj-pronr05c
           conj-pronr06i conj-pronr07i)
         'lexdisrules
         '(conj pron))

(putrule 'conj-pron 'conj 'defaultc)

;......................................................

; "Se vuoi partire subito, dimmelo"
; "Ti telefonerei, se riuscissi a parlare con Paolo"
; "Non so se siano gia` partiti"
; "Carlo, se gli avessi telefonato, sarebbe subito partito"
;*"Telefonami, se ne vuoi parlare"
; "Se ne andarono appena ebbero finito il lavoro"
;*"Se ne vuoi parlare, telefonami"
(setq conj-pronr00
   (make-lexdisr
	:if '(prevword-typ '&as)	; come se
	:then 'conj
	:CF 'A))

(setq conj-pronr01
   (make-lexdisr
	:if '(nextword-typ '&pron-adj)	; stesso
	:then 'pron
	:CF 'C))

(setq conj-pronr02i
   (make-lexdisr
	:if '(nextword-typ '&partitive-pron)
	:then 'pron
	:CF 'A
        :lang 'italian))

(setq conj-pronr03i
   (make-lexdisr
	:if '(and (not (nextword-typ '&partitive-pron))
	          (not (and (nextcat 'pron)
                            (next2cat 'noun)		; anche SE vi sia
                            (next2word-typ '&noun-verb-pref-verb))))
	:then 'conj
	:CF 'A
        :lang 'italian))

(setq conj-pronr04c
   (make-lexdisr
	:if '(prevcat+ '(noun pron (type demons)))
	:then 'pron
	:CF 'A
        :lang 'catalan))

(setq conj-pronr05c
   (make-lexdisr
	:if '(and (prev2cat 'noun)
		  (prevcat 'adj))
	:then 'pron
	:CF 'A
        :lang 'catalan))

(setq conj-pronr06i
   (make-lexdisr
	:if '(nextword-typ '&partitive-pron)
	:then 'pron
	:CF 'A
        :lang 'italian))

(setq conj-pronr07i			; SE l'era rimangiato
   (make-lexdisr
	:if '(and (nextcat 'pron)
                  (next2word-typ '&noun-verb-pref-verb))
	:then 'pron
	:CF 'A
        :lang 'italian))

;******************************************************
;*** conj V verb (default: conj) [SIA]

(putrule 'conj-verb
         '(conj-verbr01 conj-verbr02 conj-verbr03 conj-verbr04 conj-verbr05 
           conj-verbr06 conj-verbr07)
         'lexdisrules
         '(conj verb))

(putrule 'conj-verb 'verb 'defaultc)

;......................................................

(setq conj-verbr01
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (nextmood 'participle))
	:then 'verb
	:CF 'A))

(setq conj-verbr02
   (make-lexdisr
	:if '(and (nextcat 'adv)
                  (next2cat 'verb)
                  (next2mood 'participle))
	:then 'verb
	:CF 'A))

(setq conj-verbr03
   (make-lexdisr
	:if '(prevword-typ '&loc-pron)	; ci vi
	:then 'verb
	:CF 'A))

(setq conj-verbr04	; che non sia qualificabile 
   (make-lexdisr
	:if '(and (prevcat 'adv)
		  (nextcat 'adj))
	:then 'verb
	:CF 'A))

(setq conj-verbr05
   (make-lexdisr
	:if '(nextcat 'prep)
	:then 'conj
	:CF 'U))

(setq conj-verbr06
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (nextcat 'adj)
                  (or (next2cat '(conj (type compar)))		; sia ... che
                      (next2cat '(conj (type coord)))))		; sia ... sia
	:then 'conj
	:CF 'U))

(setq conj-verbr07
   (make-lexdisr
	:if '(aheadcat 'conj 'coord)		  		; sia ... che
	:then 'conj
	:CF 'U))

;**********************************************************
;*** date V num (default: date) [l'11]
 
(putrule 'date-num
         '(date-numr01)
         'lexdisrules
         '(date num))

(putrule 'date-num 'date 'defaultc)

;......................................................

(setq date-numr01                                   ; le sette sfide
   (make-lexdisr
	:if '(nextword-typ '&month)
	:then 'num
	:CF 'A))

;**********************************************************
;*** interj V noun (default: noun) [Catalan: ALERTA]
 
(putrule 'interj-noun
         '(interj-noun-verbr01)
         'lexdisrules
         '(interj noun))

(putrule 'interj-noun 'noun 'defaultc)

;......................................................

;**********************************************************
;*** interj V verb (default: verb) [Spanish HE]
 
(putrule 'interj-verb
         '(interj-noun-verbr01)
         'lexdisrules
         '(interj verb))

(putrule 'interj-verb 'verb 'defaultc)

;......................................................

;**********************************************************
;*** interj V noun V verb (default: verb) [Catalan: HA]
 
(putrule 'interj-noun-verb
         '(interj-noun-verbr01)
         'lexdisrules
         '(interj noun verb))

(putrule 'interj-noun-verb 'verb 'defaultc)

;......................................................

(setq interj-noun-verbr01                                   ; Ha!
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat '(nil punct)))
	:then 'interj
	:CF 'A))

;**********************************************************
;*** noun V num (default: num) [VENTI, TRECENTO, ...]
 
(putrule 'noun-num
         '(noun-numr01 noun-numr02 noun-numr03)
         'lexdisrules
         '(noun num))

(putrule 'noun-num 'num 'defaultc)

;......................................................

(setq noun-numr01                                   ; le sette sfide
   (make-lexdisr
	:if '(and (prevcat 'art)
                  (prevnumber 'pl)
                  (nextcat '(noun adj)))
	:then 'num
	:CF 'A))

(setq noun-numr02
   (make-lexdisr
	:if '(prevcat-agr 'art 'noun)
	:then 'noun
	:CF 'U))

(setq noun-numr03
   (make-lexdisr				; venti leggeri
	:if '(and (prevcat nil)
                  (or (nextcat-agr 'adj 'noun)
	     	      (next2cat-agr 'adj 'noun)
                      (nextcat 'prep)))		; venti dal Nord
	:then 'noun
	:CF 'U))

;******************************************************
;*** noun V phras [ENGLISH "YES"]
;    (default: phras)

(putrule 'noun-phras
         '(noun-phrasr01)	;noun
         'lexdisrules
         '(noun phras))

(putrule 'noun-phras 'phras 'defaultc)

;......................................................

(setq noun-phrasr01
   (make-lexdisr
	:if '(prevcat 'art)
	:then 'noun
	:CF 'A))

;******************************************************
;*** noun V phras V prep [ITALIAN "GRAZIE"]
;    (default: phras)

(putrule 'noun-phras-prep
         '(noun-phrasr01 noun-phras-prepr01 noun-phras-prepr02
           noun-phras-prepr03 noun-phras-prepr04)
         'lexdisrules
         '(noun phras prep))

(putrule 'noun-phras-prep 'prep 'defaultc)

;......................................................

(setq noun-phras-prepr01
   (make-lexdisr
	:if '(prep-govern-next)
	:then 'prep
	:CF 'A))

(setq noun-phras-prepr02
   (make-lexdisr
	:if '(nextcat '(punct nil))
	:then 'phras
	:CF 'A))

(setq noun-phras-prepr03
   (make-lexdisr
	:if '(prevcat-agr 'art 'noun)
	:then 'noun
	:CF 'A))

(setq noun-phras-prepr04
   (make-lexdisr
	:if '(and (prevcat '(punct nil))
                  (nextcat 'conj))
	:then 'phras
	:CF 'A))

;******************************************************
;*** noun V prep (default: prep) [TRAMITE]

(putrule 'noun-prep
         '(adv-noun-prepr01 noun-prepr01 noun-prepr02)
         'lexdisrules
         '(noun prep))

(putrule 'noun-prep 'prep 'defaultc)

;......................................................

(setq noun-prepr01	; De Magistris, Di Marco
   (make-lexdisr
	:if '(and (currword-typ '&proper-prep)
	          (capitalized-w)		; a capitalized word not at the beginning
                  (nextcat '(noun (proper yes))))
	:then 'noun
	:CF 'A))

(setq noun-prepr02
   (make-lexdisr
	:if '(nextcat '(art adj))
	:then 'prep
	:CF 'A))

;**********************************************************
;*** noun V prep+art (default: prep+art) [NEI, DEI]
 
(putrule 'noun-prep+art
         '(noun-prep+artr01)
         'lexdisrules
         '(noun prep+art))

(putrule 'noun-prep+art 'prep+art 'defaultc)

;......................................................

(setq noun-prep+artr01
   (make-lexdisr
	:if '(prevcat-agr 'art 'noun)
	:then 'noun
	:CF 'A))
 
;**********************************************************
;*** noun V prep+art V verb+pron (default: prep+art) [DALLA (all-capital)]
 
(putrule 'noun-prep+art-verb+pron
         '(noun-prep+art-verb+pronr01)
         'lexdisrules
         '(noun prep+art verb+pron))

(putrule 'noun-prep+art-verb+pron 'prep+art 'defaultc)

;......................................................

(setq noun-prep+art-verb+pronr01
   (make-lexdisr
	:if '(prevcat-agr 'art 'noun)
	:then 'noun
	:CF 'A))
 
;******************************************************
;*** noun V prep V verb (default: verb) [RIGUARDO, RISPETTO, VERSO; SPANISH: DE]

(putrule 'noun-prep-verb
         '(noun-prep-verbr001i noun-prep-verbr01s 
           noun-prep-verbr02 noun-prep-verbr03 noun-prep-verbr04
           noun-prep-verbr05 noun-prep-verbr06 noun-prep-verbr07 noun-prep-verbr08
           noun-prep-verbr09)
         'lexdisrules
         '(noun prep verb))

(putrule 'noun-prep-verb 'verb 'defaultc)

;....................................................

(setq noun-prep-verbr001i  ; for "verso sera"
   (make-lexdisr
	:if '(nextsemtype 'noun '£approx-direction)
	:then 'prep
	:CF 'A
        :lang 'italian))

(setq noun-prep-verbr01s  ; this is a default for Spanish preposition "de"
   (make-lexdisr
	:if t
	:then 'prep
	:CF 'A
        :lang 'spanish))

(setq noun-prep-verbr02
   (make-lexdisr
	:if '(and (prevcat-agr '(adj art prep) 'noun)
	          (not (prep-govern-next))
                  (not (and (nextcat 'art)
                            (not (nextword-typ '&partitive-art)))))	; rispetto del
	:then 'noun
	:CF 'A))

(setq noun-prep-verbr03		;avere rispetto per, avere riguardo a
   (make-lexdisr
	:if '(prevword-typ '&tense-aux)	; avere
	:then 'noun
	:CF 'A))

(setq noun-prep-verbr04
   (make-lexdisr
	:if '(and (not (prevcat-agr '(art prep) 'noun))
                  (nextword-typ '&until-prep))
	:then 'prep
	:CF 'A))

(setq noun-prep-verbr05
   (make-lexdisr
	:if '(and (not (prevcat-agr '(art prep) 'noun))
                  (currword-typ '&toward)
                  (nextcat 'art))
	:then 'prep
	:CF 'A))

; *** the next is a kind of pre-default for cases as "avere rispetto"
(setq noun-prep-verbr06
   (make-lexdisr
	:if '(prevcat 'verb)
	:then 'noun
	:CF 'U))

(setq noun-prep-verbr07		; con RIGUARDO a
   (make-lexdisr
	:if '(prevcat 'prep)
	:then 'noun
	:CF 'U))

(setq noun-prep-verbr08
   (make-lexdisr
	:if '(prevcat nil)
	:then 'prep
	:CF 'U))

; *** English: times
(setq noun-prep-verbr09
   (make-lexdisr
	:if '(nextcat '(nil punct))
	:then 'noun
	:CF 'U))

;******************************************************
;*** noun V pron (default: noun) [COSA; ENGLISH: NOTHING]

(putrule 'noun-pron
         '(noun-pronr01 noun-pronr02 noun-pronr03 noun-pronr04)
         'lexdisrules
         '(noun pron))

(putrule 'noun-pron 'noun 'defaultc)

;......................................................

(setq noun-pronr01
   (make-lexdisr
	:if '(prevcat-agr 'adj 'noun)      ; la prima COSA che
	:then 'noun
	:CF 'A))

(setq noun-pronr02
   (make-lexdisr
	:if '(and (prevcat-agr 'art 'noun)      ; una COSA e' certa
                  (nextcat '(verb pron)))	; la COSA si presume
	:then 'noun
	:CF 'A))

(setq noun-pronr03
   (make-lexdisr
	:if '(and (nextcat '(verb pron))
                  (not (nexttype 'deitt))        ; LA loro casa
                  (not (nexttype 'poss)))       ; LA stessa casa
	:then 'pron
	:CF 'A))

(setq noun-pronr04
   (make-lexdisr
	:if '(and (prevcat 'prep)      ; in NOTHING beyond
                  (nextcat 'prep))
	:then 'pron
	:CF 'A))

;************************************************************
;*** noun V verb (default: noun)

(putrule 'noun-verb
	'(
          noun-verbr000e noun-verbr001e noun-verbr002i noun-verbr003i
          noun-verbr004
          noun-verbr01 noun-verbr02 noun-verbr03 noun-verbr04e
          noun-verbr05 noun-verbr06 noun-verbr07e noun-verbr08e
          noun-verbr09e noun-verbr50 noun-verbr10 noun-verbr11
          noun-verbr12i noun-verbr13i noun-verbr14e noun-verbr15e
          noun-verbr16 noun-verbr17 noun-verbr18 noun-verbr19
          noun-verbr20i noun-verbr21 noun-verbr22 noun-verbr23
          noun-verbr24 noun-verbr25 noun-verbr26ie noun-verbr27s
          noun-verbr28 noun-verbr29 noun-verbr30 noun-verbr31
          noun-verbr34 noun-verbr35ics noun-verbr36ics noun-verbr37
          noun-verbr38e noun-verbr39e noun-verbr40e noun-verbr41i
          noun-verbr42e noun-verbr43ics noun-verbr44e noun-verbr45e
          noun-verbr46e noun-verbr32 noun-verbr33 noun-verbr47
          noun-verbr48 noun-verbr49 noun-verbr51 noun-verbr52
          noun-verbr53 noun-verbr54 noun-verbr55 noun-verbr56
          noun-verbr57e noun-verbr58 noun-verbr59 noun-verbr60
          noun-verbr61 noun-verbr62 noun-verbr63 noun-verbr64
          noun-verbr65e noun-verbr66 noun-verbr67 noun-verbr68e
          noun-verbr69es noun-verbr70 noun-verbr71 noun-verbr72
          noun-verbr73e noun-verbr73ics
          noun-verbr74 noun-verbr75 noun-verbr76
          noun-verbr77 noun-verbr78i noun-verbr79e noun-verbr80e
          noun-verbr81e noun-verbr82e noun-verbr83e noun-verbr84e
          noun-verbr85e noun-verbr86 noun-verbr87 noun-verbr88 
          noun-verbr89 noun-verbr90 noun-verbr91 noun-verbr92 
          noun-verbr93i noun-verbr94e noun-verbr95)
	'lexdisrules
	'(noun verb))

(putrule 'noun-verb 'noun 'defaultc)

;......................................................

(setq noun-verbr000e			; this is for "phone number"
   (make-lexdisr
	:if '(and (prevword-typ '&has-number)
                  (currword-typ '&number))
	:then 'noun
	:CF 'A
        :lang 'english))

(setq noun-verbr001e			; this is for I'm, You're
   (make-lexdisr
	:if '(and (prevword-typ '&progressive-aux)
                  (currmood 'gerund))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr002i			; this is for "v."; should be applied just
   (make-lexdisr                        ; in case the name comes form a sigla interp
	:if '(and (or (prevcat nil)
                      (prevchar '(#\[ #\( #\>)))	;#\)
                  (default-proper)
 	          (currmood+pers 'ind 'p3)
		  ;(not (currword-typ '&action-noun))
                  (not (nextword-typ '&neutral-prep)))	; Raccolta di mobili
	:then 'verb
	:CF 'A
        :lang 'italian))

(setq noun-verbr003i			; this is for noun-noun modifications in Italian
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (noun-noun))
	:then 'noun
	:CF 'A
        :lang 'italian))

(setq noun-verbr004
   (make-lexdisr
	:if '(and (prevcat '(pron (form clitic)))
                  (not (prev2cat 'verb)))
	:then 'verb
	:CF 'A))

(setq noun-verbr01			; the word "essere"
   (make-lexdisr
	:if '(and (currword-typ '&to-be)
                  (currmood 'infinite)
                  (not (prevcat 'art)))
	:then 'verb
	:CF 'A))

(setq noun-verbr02
   (make-lexdisr
	:if '(and (currproper)
                  (prevword-typ '&saint))
	:then 'noun
	:CF 'A))

(setq noun-verbr03			; un maxi-Impegno
   (make-lexdisr
	:if '(and (prevchar #\-)
                  (prev2word-typ '&pre--adj))
	:then 'noun
	:CF 'A))

(setq noun-verbr04e			; mind-like
   (make-lexdisr
	:if '(and (nextchar #\-)
                  (next2word-typ '&like))
	:then 'noun
	:CF 'A
        :lang 'english))

(setq noun-verbr05			; programmi teatro ; concerti Torino
   (make-lexdisr			; elenco concerti; tessera tram
	:if '(and (prevcat nil)
                  (or (currword-typ '&program)
                      (currword-typ '&event)
                      (currword-typ '&card)
                      (currword-typ '&music)
                      (currword-typ '&list))
		  (nextcat 'noun))
	:then 'noun
	:CF 'A))

(setq noun-verbr06			; biglietti lingotto musica.
   (make-lexdisr	
	:if '(and (currword-typ '&music)
		  (prevcat 'noun)
                  (nextchar #\.))
	:then 'noun
	:CF 'A))

(setq noun-verbr07e			; By Making
   (make-lexdisr	
	:if '(and (prevword-typ '&by)
		  (currmood 'gerund))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr08e			;takes care
   (make-lexdisr	
	:if '(and (currword-typ '&verb-locut-21a)
	          (nextword-typ '&verb-locut-21b))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr09e			;can be
   (make-lexdisr	
	:if '(and (currword-typ '&can)
	          (nextcat 'verb)
                  (nextmood 'infinite))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr10			; maggioranze parlamentari Uscite da
   (make-lexdisr
	:if '(and (prevcat '(adj (type qualif)))
                  (prev2cat '(noun (proper nil)))
                  (currmood+tense 'participle 'past))
	:then 'verb
	:CF 'A))

(setq noun-verbr11			; SEE RULE NOUN-VERBR91
   (make-lexdisr
	:if '(and (prevcat-agr '(art num) 'noun)
                  (not (currword-typ '&noun-verb-pref-verb))		; era
                  (not (and (currmood 'participle)
                            (nextword-typ '&noun-verb-pref-noun))))     ; philosophizing subject
	:then 'noun
	:CF 'A))

(setq noun-verbr12i
   (make-lexdisr
	:if '(and (prevcat-agr 'adj 'noun)
                  (not (currword-typ '&noun-verb-pref-verb))			; era
                  (not (prev2cat '(noun (proper nil))))
                  (not (and (currword-typ '&to-be)
                            (nextcat 'verb)
                            (nextmood 'participle))))
	:then 'noun
	:CF 'A
        :lang 'italian))

(setq noun-verbr13i
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (not (currmood 'infinite))
                  (not (currword-typ '&noun-verb-pref-verb)))		; era
	:then 'noun
	:CF 'A
        :lang 'italian))

(setq noun-verbr14e
   (make-lexdisr
	:if '(and (prevcat-agr 'adj 'noun)
                  (not (prev2cat '(noun (proper nil))))
                  (not (and (currword-typ '&to-be)
                            (nextcat 'verb)
                            (nextmood 'participle)))
                  (not (and (currtype 'mod)
                            (nextcat 'verb)
                            (nextmood 'infinite)))
                  (not (and (currtype 'mod)
                            (nextcat 'pron)
                            (next2cat 'verb)
                            (next2mood 'infinite))))
	:then 'noun
	:CF 'A
        :lang 'english))

(setq noun-verbr15e
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (not (currmood 'infinite))
                  (not (currmood 'gerund)))
	:then 'noun
	:CF 'A
        :lang 'english))

(setq noun-verbr16
   (make-lexdisr
	:if '(and (prevcat-agr '(adj (type qualif)) 'noun)
                  (prev2cat 'noun)
                  (currperson 'p3))
	:then 'verb
	:CF 'A
        :lang 'italian))

(setq noun-verbr17
   (make-lexdisr
	:if '(and (prevtype 'aux)
                  (currtense 'past)
                  (currmood 'participle))
	:then 'verb
	:CF 'A))

(setq noun-verbr18
   (make-lexdisr			; stava cominciando
	:if '(and (prevtype 'aux)	; was beginning
                  (or (currmood 'gerund)
                      (currmood+tense 'participle 'pres)))
	:then 'verb
	:CF 'A))

(setq noun-verbr19
   (make-lexdisr
	:if '(and (prevcat 'verb)
		  (prevtype 'mod)
                  (not (currword-typ '&noun-verb-pref-noun))
	          (currmood 'infinite))
	:then 'verb
	:CF 'A))

(setq noun-verbr20i
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (not (and (currword-typ '&time-verb)
                            (nextcat 'num)))
                  (not (and (prevword-typ '&to-do)	; fa sapere
                            (currmood 'infinite)))
                  (not (currword-typ '&noun-verb-pref-verb))		; era
                        ; the next for 'va visto'; it should be va --> aux
                  (not (and (prevword-typ '&has-to)
                            (currmood+tense 'participle 'past))))
	:then 'noun
	:CF 'A
        :lang 'italian))

(setq noun-verbr21		; il non Uso è
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (not (nextword-typ '&verb-prep-ambig))	; dire, dare
                  (prevcat 'adv)
                  (prev2cat 'art))
	:then 'noun
	:CF 'A))

(setq noun-verbr22		; Limiti posti da
   (make-lexdisr
	:if '(and (prevcat '(nil art punct))
                  (nextcat 'verb)
                  (nextmood 'participle)
                  (nexttense 'past)
                  (next2word-typ '&agent-compl-prep))	; da
	:then 'noun
	:CF 'A))

(setq noun-verbr23
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (not (nextword-typ '&verb-prep-ambig))
                  (prevword-typ '&mid-relat)		; cui
                  (prev2cat 'art))
	:then 'noun
	:CF 'A))

(setq noun-verbr24
   (make-lexdisr
	:if '(and (not (prevcat '(noun nil pron (type refl-impers))))
                  (nextcat 'verb)
	          (nextmood 'participle)
                  (not (and (currtype 'mod) (nextmood 'infinite)))
		  (not (currtype 'aux))
                  (not (and (prevcat 'verb)
                            (prevtype '(aux mod))))
                  (not (and (prevchar '(#\' #\"))        ;"
                            (prev2cat 'art)))
                  (not (and (prevcat 'adv)
                            (prev2cat 'verb)
                            (prev2type '(aux mod)))))
	:then 'noun
	:CF 'A))

(setq noun-verbr25
   (make-lexdisr
	:if '(and (currtype 'aux) 
                  (nextcat 'verb)
	          (or (nextmood 'participle)
	              (nextmood 'gerund))
                  (not (nextword-typ '&verb-prep-ambig))
                  (not (and (prevchar '(#\' #\"))        ;"
                            (prev2cat 'art))))
	:then 'verb
	:CF 'A))

(setq noun-verbr26ie
   (make-lexdisr
	:if '(and (not (prevcat 'art))
	          (currmood+tense 'participle 'past)
	          (nextword-typ '&agent-compl-prep))
	:then 'verb
	:CF 'A
        :lang '(italian english)))

(setq noun-verbr27s
   (make-lexdisr
	:if '(and (not (prevcat 'art))
	          (currmood+tense 'participle 'past)
	          (nextword-typ '&agent-compl-prep))
	:then 'verb
	:CF 'A
        :lang 'spanish))

(setq noun-verbr28
   (make-lexdisr
	:if '(and (currproper)
                  (prevword-typ '&title)) ; ministro presidente signore commissione
	:then 'noun
	:CF 'A))

(setq noun-verbr29
   (make-lexdisr
	:if '(and (prevcat 'adv)
		  (prev2cat '(verb (type aux)))
                  (currmood 'participle))
	:then 'verb
	:CF 'A))

(setq noun-verbr30
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (not (nextcat '(prep punct num)))) ; "num" for "article 95"
	:then 'verb
	:CF 'A))

(setq noun-verbr31		; dove Danno "La ..."
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (prevtype 'interr)
		  (prev2cat nil)
	          (nextchar '(#\' #\")))			;"
	:then 'verb
	:CF 'A))

(setq noun-verbr32
   (make-lexdisr
	:if '(and (prevchar #\<)			; formerly #\« (not accepted by the compiler)
                  (not (nextcat '(art noun adv punct))))
	:then 'noun
	:CF 'A))

(setq noun-verbr33
   (make-lexdisr
	:if '(and (prevchar #\,)
		  (prev2cat 'noun)
		  (not (nextcat 'prep))
		  (not (currword-typ '&noun-verb-pref-verb))
		  (not (currmood 'participle))
		  (not (verb-govern-adv-next))
		  (not (and (currtype 'mod)
                            (nextmood 'infinite))))
	:then 'noun
	:CF 'A))

(setq noun-verbr34
   (make-lexdisr
	:if '(and (nextcat '(pron (type relat)))
	          (nextcat '(conj (type subord)))
                  (prevcat '(conj (type coord)))
                  (prev2cat 'noun))
	:then 'noun
	:CF 'A))

(setq noun-verbr35ics
   (make-lexdisr
	:if '(and (nextcat '(conj (type subord)))
                  (not (prevcat '(art prep verb adj (type demons)))))
	:then 'verb
	:CF 'A
        :lang '(italian catalan spanish)))

(setq noun-verbr36ics
   (make-lexdisr
	:if '(and (nextcat '(conj (type subord)))
                  (not (nextword-typ '&as))
                  (not (prevcat '(art adj (type demons))))
                  (currmood 'participle))
	:then 'verb
	:CF 'A
        :lang '(italian catalan spanish)))

(setq noun-verbr37
   (make-lexdisr
	:if '(and (prevword-typ '&mid-relat)
                  (prev2cat 'art))
	:then 'noun
	:CF 'A))

(setq noun-verbr38e
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (nextcat '(punct nil)))
	:then 'noun
	:CF 'A
        :lang 'english))

(setq noun-verbr39e
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (nextcat 'conj)
                  (not (currword-typ '&noun-verb-pref-verb))
                  (not (currmood 'gerund))
                  (not (next2cat 'verb)))
	:then 'noun
	:CF 'A
        :lang 'english))

(setq noun-verbr40e
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (nextcat 'verb)
                  (not (nextcat 'prep))    ; the universe looks like
                  (not (currtype '(aux mod)))
                  (not (verb-govern-adv-next)))
	:then 'noun
	:CF 'A
        :lang 'english))

(setq noun-verbr41i
   (make-lexdisr
	:if '(and (not (capitalized-w))		; a capitalized word not at the beginning
                      			; is preferred not to be a verb
                  (prevcat '(noun pron))
                  (not (and (prevcat 'pron) (prev2cat 'verb)))	; procurarsi armi
                  (not (and (prevcat 'noun)
                            (currtense 'pres)
                            (currmood 'ind)
                            (not (currmood 'participle))  ; if it can be a participle, ok
                        ; *** the next three conditions to avoid first person of conjunctives
                            (or (currperson 'p1)
                                (currperson 'p2))
                            (not (currperson 'p3))))
                  (not (currword-typ '&concert))  ; ci sono a Torino concerti
                  (not (and (prevcat '(noun (proper yes)))
                            (currproper)))
                  (not (nextcat '(adj pron (type relat))))
                  (not (and (nextcat 'prep)
                            (next2cat '(pron (type relat))))))
	:then 'verb
	:CF 'A
        :lang 'italian))

(setq noun-verbr42e
   (make-lexdisr
	:if '(and (prevcat '(noun pron))
                  (or (and (prevnumber 'sing)		; this is a kind of noun-verb
                           (not (currmood 'infinite)))  ; agreement for English
                      (and (prevnumber 'pl)
                           (currmood 'infinite)))
                  (not (currword-typ '&ticket))		; are There tickets?, discount t.
                  (not (currword-typ '&concert))        ; which cabaret concerts I
                  (not (and (prevcat '(adj (type ordin)))
                            (nextcat '(nil punct))))	; fifteenth may
                  (not (currproper))
                  (not (nextword-typ '&base-relat))	; a pron relat, but not "that"
                  (not (and (nextcat 'prep)
                            (next2cat '(pron (type relat))))))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr43ics
   (make-lexdisr
	:if '(and (nextcat 'art)
	          (not (nextcat 'prep))
                  (not (prevcat 'art)))
	:then 'verb
	:CF 'A
        :lang '(italian spanish french)))

(setq noun-verbr44e
   (make-lexdisr
	:if '(and (nextcat 'art)
                  (not (nexttype 'genitive))
                  (not (prevcat '(art prep))))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr45e
   (make-lexdisr
	:if '(and (nextcat 'art)
                  (not (prevcat 'art))
                  (prevword-typ '&base-subord-prep)
                  (currtense 'infinite))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr46e
   (make-lexdisr
	:if '(and (nextcat 'art)
                  (not (prevcat 'art))
                  (not (prevword-typ '&base-subord-prep))
                  (currtense 'gerund))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr47
   (make-lexdisr
	:if '(and (or (prevchar #\,) (prevchar #\"))	;"
	          (not (nextchar #\,))
                  (prev2cat 'noun)
                  (currmood+tense 'participle 'past))
	:then 'verb
	:CF 'A))

(setq noun-verbr48
   (make-lexdisr
	:if '(prevcat '(pron (word-typ &who-2)))
	:then 'verb
	:CF 'A))

(setq noun-verbr49
   (make-lexdisr
	:if '(and (currproper)
                  (nextcat '(noun (proper yes))))
	:then 'noun
	:CF 'A))

(setq noun-verbr50
   (make-lexdisr
	:if '(and (currword-typ '&progressive-aux)	; stare
                  (nextmood 'gerund))
	:then 'verb
	:CF 'A))

(setq noun-verbr51
   (make-lexdisr
	:if '(and (prevcat '(conj (semtype advers)))
                  (prevtype 'coord)
                  (prev2cat 'noun)
                  (nextcat 'adv))
	:then 'verb
	:CF 'A))

(setq noun-verbr52
   (make-lexdisr
	:if '(and (prevcat '(conj (type coord)))
                  (prev2cat 'noun)
                  (not (currword-typ '&noun-verb-pref-verb))		; era
                  (not (and (currtype 'mod)
                            (nextcat 'adv)
                            (next2cat 'verb))))
	:then 'noun
	:CF 'A))

(setq noun-verbr53
   (make-lexdisr
	:if '(prevcat '(adj (type (deitt demons))))
	:then 'noun
	:CF 'A))

(setq noun-verbr54
   (make-lexdisr
	:if '(and (prevcat '(noun (proper yes)))
                  (currproper))
	:then 'noun
	:CF 'A))

(setq noun-verbr55		                        ; il "lavoro sporco"
   (make-lexdisr
	:if '(and (prevchar '(#\' #\"))			;"
                  (prev2cat 'art))
	:then 'noun
	:CF 'A))

(setq noun-verbr56	                                 ; Vede, ...
   (make-lexdisr
	:if '(and (prevcat '(nil punct marker))
                  (nextchar #\,)
                  (not (next2cat 'noun))
                  (not (nexttype 'ordin))		; this for English dates
                                                  ; march third, may tenth
                  (not (and (prevchar #\-) (prev2cat '(noun adj)))))
	:then 'verb
	:CF 'A))

(setq noun-verbr57e	; restricted to English because bad performances for Italian   ; Vede, ...
   (make-lexdisr
	:if '(and (not (currword-typ '&noun-verb-pref-noun))     ; WATER pollution
                  (prevcat '(nil punct marker))
                  (nextcat '(art noun adv adj (type indef)))
                  (not (and (nextcat 'prep)		; for English "in"
                            (nextword-typ '&pref-prep-adj)))
                  (not (nexttype 'ordin))		; this for English dates
                                                  ; march third, may tenth
		  (not (nextword-typ '&action-noun))	; Trash removal
		  (not (currword-typ '&action-noun))	; Raccolta di mobili
                  (not (nextcat-agr 'verb 'noun))	; Marco vive qui
                  (not (and (prevchar #\-) (prev2cat '(noun adj)))))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr58
   (make-lexdisr
	:if '(and (prevcat 'conj)
                  (prevtype 'coord)
                  (prev2cat 'verb)
                  (or (and (currmood 'participle) (prev2mood 'participle))
                      (and (currmood 'ind) (prev2mood 'ind))
                      (and (currmood 'infinite) (prev2mood 'infinite))))
	:then 'verb
	:CF 'A))

(setq noun-verbr59
   (make-lexdisr
	:if '(and (prevchar #\-)
                  (not (prev2cat 'noun))
                  (currtype 'aux))
	:then 'verb
	:CF 'A))

(setq noun-verbr60
   (make-lexdisr
	:if '(and (prevchar #\,)
                  (not (prev2cat 'noun))
                  (nextcat 'conj)
                  (nexttype 'coord)
                  (next2cat 'noun)
                  (not (next2cat 'verb)))
	:then 'noun
	:CF 'A))

(setq noun-verbr61
   (make-lexdisr
	:if '(and (prevchar #\,)
                  (not (prev2cat 'noun))
                  (not (and (nextword-typ '&neutral-prep)
                            (next2cat 'noun))))
	:then 'verb
	:CF 'A))

(setq noun-verbr62
   (make-lexdisr
	:if '(and (or (prevcat 'marker) 
	              (prevchar #\<))			; formerly #\« (not accepted by the compiler)
   ; *** the next two conditions to avoid first person of conjunctives
                  (currperson 'p1)
                  (not (currperson 'p2)))
	:then 'verb
	:CF 'A))

(setq noun-verbr63
   (make-lexdisr
	:if '(and (prevcat nil)
   ; *** the next two conditions to avoid first person of conjunctives
                  (currperson 'p1)
                  (not (currperson 'p2)))
	:then 'noun
	:CF 'A))

(setq noun-verbr64
   (make-lexdisr
	:if '(and (prevcat 'pron)
		  (not (prevword-typ '&loc-pron))
	          (prev2word-typ '&to-be))
	:then 'verb
	:CF 'A))

(setq noun-verbr65e
   (make-lexdisr
	:if '(and (prevcat 'prep)
	          (prevword-typ '&base-subord-prep) 
	          (currmood 'infinite))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr66
   (make-lexdisr
	:if '(and (currword-typ '&command)		; stop
                  (prevcat nil) 
                  (or (nextcat nil)
                      (nextchar '(#\. #\!))))
	:then 'verb
	:CF 'A))

(setq noun-verbr67
   (make-lexdisr
	:if '(and (not (prevcat '(art adj num conj)))
	          (not (prevchar #\-))
                  (not (currword-typ '&noun-verb-pref-verb)) ; non ERA in grado
                  (not (nextcat '(art noun pron)))	; Let us go
                  (not (currtype 'mod))			; will vary
                  (not (and (nextcat 'adv)		; slowly FILLS in
	                    (verb-govern-adv-next)))
                  (not (and (currword-typ '&time-verb)	; trascorsi 12 mesi
                            (nextcat 'num)))
	          (not (and (prevcat 'pron)		; I Need to find
			    (nextcat '(prep adv))))	; I Throw away
	          (not (and (currmood 'gerund)		; the thing happening next
			    (prevcat 'noun)))
	          (not (and (currmood 'participle)	; persone comunque sottoposte
			    (prevcat 'adv)
                            (prev2cat 'noun)))
	          (not (and (currmood 'infinite)	; comincia a essere
			    (prevcat 'prep)))
	          (not (and (currmood+tense 'participle 'past)	; una missione Durata solo 3 gionri
			    (prevcat 'noun)
                            (nextcat 'adv)))
	          (not (and (nextchar #\")		; "
			    (next2cat '(art noun prep pron))))
	          (not (and (prevcat nil) 		; stop
                            (or (nextcat nil)
                                (nextchar '(#\. #\!)))))
	          (not (and (prevcat '(adv (type neg))) ; non Era di quelli
                            (prev2cat 'art))))
	:then 'noun
	:CF 'A))

(setq noun-verbr68e
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (nextcat 'noun))
	:then 'noun
	:CF 'A
        :lang 'english))

(setq noun-verbr69es
   (make-lexdisr
	:if '(and (prevcat 'adj)
	          (prevtype 'poss))
	:then 'noun
	:CF 'A
        :lang '(spanish english)))

(setq noun-verbr70
   (make-lexdisr
	:if '(and (not (currword-typ '&noun-verb-pref-noun))     ; WATER pollution
	          (nextcat '(noun adj (type indef) pron (type indef) predet adv))
                  (not (currword-typ '&concert))  ; ci sono a Torino concerti
                  (not (currword-typ '&action-noun))  ; raccolta mobili
		  (not (nextword-typ '&action-noun))	; Trash removal
                  (or (not (nextcat 'verb)) ; Matteo mangia e MARCO pesca
                      (nextcat 'adv))	    ; break down (down both verb and adv)
                  (not (nextcat 'prep)) ; this to avoid application when the next
                                        ; word is Spanish "de"
                  (not (and (prevchar #\-)	; variable-rate
                            (prev2cat 'adj)))
                  (not (and (nexttype 'ordin)   ; to block application to "March third"
                            (currsemtype 'noun '£month)))
                  (not (prevcat '(nil prep art)))
                  (not (prevcat-agr 'adj 'noun)))
	:then 'verb
	:CF 'U))

(setq noun-verbr71
   (make-lexdisr
	:if '(and (prevchar #\")		; "
	          (nextchar #\"))		; "
	:then 'noun
	:CF 'U))

(setq noun-verbr72
   (make-lexdisr
	:if '(and (prevcat '(conj (type subord)))
                  (nextcat '(adj (type (qualif interr)) conj (type subord) 
                             pron (type relat))))
	:then 'verb
	:CF 'U))

(setq noun-verbr73e	;different from the next, since it does not allow following adj
   (make-lexdisr
	:if '(and (not (prevcat '(conj (type subord))))
                  (and (not (and (prevcat '(verb (type mod)))	
                                 (currmood 'infinite)))
                       (not (prevcat 'pron))
                       (nextcat '(conj (type subord) 
                                  pron (type relat))))
                  (not (nextword-typ '&double-who)))
	:then 'noun
	:CF 'U
        :lang 'english))

(setq noun-verbr73ics
   (make-lexdisr
	:if '(and (not (prevcat '(conj (type subord))))
                  (not (and (prevcat '(verb (type mod)))
                            (currmood 'infinite)))
                  (not (and (prevcat '(conj (type coord)))
                            (prev2cat 'punct)))		; ..., e RESTA
                  (not (prevcat 'pron))
                  (not (and (prev2cat '(art adj (type poss))) ; l'atmosfera ERA carica
                            (prevcat 'noun)
                            (nextcat 'adj)))
                  (not (and (prevcat 'adj)
                            (not (prevcat-agr 'adj 'noun))
                            (nextcat 'adj)
                            (not (nextcat-agr 'adj 'noun))))
                  (nextcat '(adj (type (qualif interr))
                             conj (type subord) 
                             pron (type relat)))
                  (not (currword-typ '&noun-verb-pref-verb))
                  (not (nextword-typ '&double-who)))
	:then 'noun
	:CF 'U
        :lang '(italian catalan spanish)))

(setq noun-verbr74
   (make-lexdisr
	:if '(and (not (prevcat '(conj (type subord))))
                  (nextword-typ '&double-who))
	:then 'verb
	:CF 'U))

(setq noun-verbr75
   (make-lexdisr
	:if '(and (nextword-typ '&prep-subord-dep)	; che
                  (currmood+tense 'participle 'past)
                  (not (prevcat-agr '(art adj) 'noun)))
	:then 'verb
	:CF 'U))

(setq noun-verbr76
   (make-lexdisr
	:if '(and (prevcat '(pron (type relat) conj))
                  (not (prev2cat 'noun))
                  (nextcat '(prep adj (type deitt))))
	:then 'verb
	:CF 'U))

(setq noun-verbr77
   (make-lexdisr
	:if '(and (prevcat 'conj)
                  (prevtype 'coord)
                  (prev2cat 'adj)
                  (currmood+tense 'participle 'past))
	:then 'verb
	:CF 'U))

(setq noun-verbr78i
   (make-lexdisr
	:if '(and (currmood 'infinite)		; comincia a essere
		  (prevcat 'prep)
		  (prev2cat 'verb))
	:then 'verb
	:CF 'A
        :lang 'italian))

(setq noun-verbr79e
   (make-lexdisr
	:if '(and (currmood 'infinite)		; he wants to go ...
		  (prevcat 'prep)
	          (prevword-typ '&base-subord-prep))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr80e
   (make-lexdisr
	:if '(and (currmood 'gerund)		; he did it for going ...
		  (prevcat 'prep)
	          (not (prevword-typ '&base-subord-prep)))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr81e
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (not (currword-typ '&concert))        ; which cabaret concerts I
		  (not (nextcat 'noun))
		  (not (nextcat '(pron (type relat)))))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr82e
   (make-lexdisr
	:if '(and (not (currword-typ '&noun-verb-pref-noun))     ; WATER pollution
	          (prevcat nil)
		  (nextcat '(noun art pron))
                  (not (currsemtype 'noun '£month))
		  (not (nextword-typ '&action-noun))	; Trash removal
                  (not (nexttype 'ordin)))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr83e
   (make-lexdisr			; the thing will vary
	:if '(and (or (prevcat '(noun pron))
                      (and (prevchar #\")	;"
                           (prev2cat 'noun)))
                  (currtype 'mod)
                  (nextcat 'verb)
                  (nextmood 'infinite))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr84e		; can, may
   (make-lexdisr
	:if '(and (not (prevcat 'art))
	          (not (nextcat 'num))				; may 30
	          (not (nextcat '(adj (type ordin))))		; may thirtieth
                  (not (and (prevcat 'prep)
                            (prev2cat '(pron (type ordin)))))	; thirtieth of may
                  (not (and (prevcat 'prep)
                            (prev2cat '(adj (type ordinsuff))))) ; 30th of may
                  (currtype 'mod))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr85e			;can I go?
   (make-lexdisr	
	:if '(and (currword-typ '&can)
	          (nextcat 'pron)
	          (next2cat 'verb)
                  (next2mood 'infinite))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr86		; I Need to find 
   (make-lexdisr
	:if '(and (prevcat '(pron (type pers)))
                  (nextcat '(conj (type subord)))
                  (next2cat 'verb))
	:then 'verb
	:CF 'U))

(setq noun-verbr87		; non ERA più
   (make-lexdisr
	:if '(and (prevcat 'adv)
	          (nextcat 'adv))
	:then 'verb
	:CF 'U))

(setq noun-verbr88		; non ERA più
   (make-lexdisr
	:if '(and (currword-typ '&noun-verb-pref-verb)		; era essere
	          (not (currword-typ '&institutional))		; stato
                  (not (prevcat 'art)))				; l'era della
	:then 'verb
	:CF 'U))

(setq noun-verbr89		; , DECORSI 12 mesi da
   (make-lexdisr
	:if '(and (currword-typ '&time-verb)
                  (nextcat 'num))
	:then 'verb
	:CF 'U))

(setq noun-verbr90		; he saw in 
   (make-lexdisr
	:if '(and (prevcat '(pron (not (form clitic))))
                  (nextcat 'prep))
	:then 'verb
	:CF 'U))

(setq noun-verbr91			; SEE RULE NOUN-VERBR11
   (make-lexdisr
	:if '(and (currmood 'participle)
                  (nextword-typ '&noun-verb-pref-noun))     ; philosophizing subject
	:then 'verb
	:CF 'A))

(setq noun-verbr92			; Tenendo conto
   (make-lexdisr
	:if '(and (prevcat nil)
                  (default-proper)
                  (currmood 'gerund))
	:then 'verb
	:CF 'A))

(setq noun-verbr93i		; v. noun-verbr41i
   (make-lexdisr
	:if '(and (not (capitalized-w))		; a capitalized word not at the beginning
                      			; is preferred not to be a verb
                  (prevcat '(noun pron))
                  (not (and (prevcat 'pron) (prev2cat 'verb)))	; procurarsi armi
                  (and (prevcat 'noun)
                       (currtense 'pres)
                       (currmood 'ind)
                       (not (currmood 'participle))  ; if it can be a participle, ok
                        ; *** the next three conditions to avoid first person of conjunctives
                       (or (and (currperson 'p1)		; in Polonia comando io
                                (nextcat 'pron)
                                (nextperson 'p1))
                           (and (currperson 'p2))
                                (nextcat 'pron)
                                (nextperson 'p2))
                       (not (currperson 'p3)))
                  (not (currword-typ '&concert))  ; ci sono a Torino concerti
                  (not (and (prevcat '(noun (proper yes)))
                            (currproper)))
                  (not (nextcat '(pron (type relat))))
                  (not (and (nextcat 'prep)
                            (next2cat '(pron (type relat))))))
	:then 'verb
	:CF 'A
        :lang 'italian))

(setq noun-verbr94e			; , use up dissolved oxygen
   (make-lexdisr
	:if '(and (prevchar #\,)
                  (not (currmood 'participle))
                  (verb-govern-adv-next))
	:then 'verb
	:CF 'A
        :lang 'english))

(setq noun-verbr95			; persone comunque sottoposte
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (prev2cat 'noun)
                  (currmood 'participle))
	:then 'verb
	:CF 'U))

;************************************************************
;*** noun V verb+pron (default: noun)

(putrule 'noun-verb+pron
         '(noun-verb+pronr01)
         'lexdisrules
         '(noun verb+pron))

(putrule 'noun-verb+pron 'noun 'defaultc)

;......................................................................

(setq noun-verb+pronr01
   (make-lexdisr
	:if '(and (currproper)
                  (prevcat 'punct))
	:then 'verb+pron
	:CF 'U))

;************************************************************
;*** noun V verb V verb+pron (default: noun)
;   N.B. Same rules as for noun V verb; verb+pron is never chosen

(putrule 'noun-verb-verb+pron
         '(noun-verbr10 noun-verbr11 noun-verbr16 noun-verbr17
           noun-verbr18 noun-verbr19 noun-verbr20i noun-verbr21
           noun-verbr22 noun-verbr23 noun-verbr24 noun-verbr25
           noun-verbr26ie noun-verbr29 noun-verbr30 noun-verbr41i 
           noun-verbr68e noun-verbr69es
           noun-verbr70 noun-verbr37 noun-verbr38e noun-verbr39e
           noun-verbr40e noun-verbr71 noun-verbr72 noun-verbr73e 
           noun-verbr73ics noun-verbr74 noun-verbr61)
         'lexdisrules
         '(noun verb verb+pron))

(putrule 'noun-verb-verb+pron 'noun 'defaultc)

;......................................................................

;************************************************************
;*** num V paragraph-n (default: num) [3.2]

(putrule 'num-paragraph-n
         '(num-paragraph-nr01)
         'lexdisrules
         '(num paragraph-n))

(putrule 'num-paragraph-n 'num 'defaultc)

;......................................................

(setq num-paragraph-nr01
   (make-lexdisr
	:if '(prevcat nil)
	:then 'paragraph-n
	:CF 'U))

;************************************************************
;*** num V verb (default: verb) [SEI]

(putrule 'num-verb
         '(num-verbr01 num-verbr02 num-verbr03)
         'lexdisrules
         '(num verb))

(putrule 'num-verb 'verb 'defaultc)

;......................................................

(setq num-verbr01
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (nextmood 'participle)
                  (nexttense 'past))
	:then 'verb
	:CF 'U))

(setq num-verbr02
   (make-lexdisr
	:if '(and (nextcat 'adv)
                  (next2cat 'verb)
                  (next2mood 'participle))
	:then 'verb
	:CF 'U))

(setq num-verbr03
   (make-lexdisr
	:if '(nextcat '(nil noun punct))
	:then 'num
	:CF 'U))

;**********************************************************
;*** phras V pron (default: pron) [SI; for HOPS: "sì" recognized as "si"
 
(putrule 'phras-pron
         '(phras-pronr01i)
         'lexdisrules
         '(phras pron))

(putrule 'phras-pron 'pron 'defaultc)

;......................................................

(setq phras-pronr01i
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat '(punct nil))
                  (not (nextchar '(#\' #\"))))		; "
	:then 'phras
	:CF 'A
        :lang 'italian))
 
;**********************************************************
;*** predet V pron (default: pron) [TUTTI, AMBEDUE, ENTRAMBI]
 
(putrule 'predet-pron
         '(predet-pronr01 predet-pronr02)
         'lexdisrules
         '(predet pron))

(putrule 'predet-pron 'pron 'defaultc)

;......................................................

(setq predet-pronr01
   (make-lexdisr
	:if '(nextcat-agr 'art 'predet)
	:then 'predet
	:CF 'A))
 
(setq predet-pronr02
   (make-lexdisr
	:if '(and (nextcat-agr 'adj 'predet)
                  (nexttype 'demons))
	:then 'predet
	:CF 'A))
 
;************************************************************
;*** prep V verb (default: verb) [ENTRO]

(putrule 'prep-verb
         '(prep-verbr01 prep-verbr02 prep-verbr03)
         'lexdisrules
         '(prep verb))

(putrule 'prep-verb 'prep 'defaultc)

;......................................................

(setq prep-verbr01
   (make-lexdisr
	:if '(nextword-typ '&enter-prep)	; in
	:then 'verb
	:CF 'A))

(setq prep-verbr02
   (make-lexdisr
	:if '(prevword-typ '&first-pers-pron)
	:then 'verb
	:CF 'A))

(setq prep-verbr03
   (make-lexdisr
	:if '(nextcat 'art)
	:then 'prep
	:CF 'A))

;******************************************************
(putrule 'prep+art-verb
       '(prep+art-verbr01)
       'lexdisrules
       '(prep+art verb))

(putrule 'prep+art-verb 'prep 'defaultc)

;......................................................

(setq prep+art-verbr01
   (make-lexdisr
	:if '(nextcat '(prep art))
	:then 'verb
	:CF 'A))

;************************************************************
;*** pron V verb (default: verb) [COLORO]

(putrule 'pron-verb
         '(pron-verbr01)
         'lexdisrules
         '(pron verb))

(putrule 'pron-verb 'verb 'defaultc)

;......................................................

(setq pron-verbr01
   (make-lexdisr
	:if '(nextword-typ '&base-relat)
	:then 'pron
	:CF 'C))

;************************************************************
;*** verb V verb+pron (default: verb) [SFOLLATI, GUARDATI]
       
(putrule 'verb-verb+pron 'verb 'defaultc)

;......................................................

;########################################################################
; 	INSIDE-CATEGORY AMBIGUITIES
;########################################################################

;*** general: male V female (default: f)

(putrule 'general-f-m
         '(general-f-m-r01 general-f-m-r02 general-f-m-r03 general-f-m-r04
           general-f-m-r05 general-f-m-r06)
         'lexdisrules
         '(f m))

(putrule 'general-f-m 'f 'defaultc)

;......................................................

(setq general-f-m-r01
   (make-lexdisr
	:if '(and (prevgender 'm)
	          (not (prevgender 'f)))
	:then 'm
	:CF 'A))

(setq general-f-m-r02
   (make-lexdisr
	:if '(and (prevgender 'f)
	          (not (prevgender 'm)))
	:then 'f
	:CF 'A))

(setq general-f-m-r03
   (make-lexdisr
	:if '(and (nextgender 'm)
	          (not (nextgender 'f)))	; *** to avoid choices with allval
	:then 'm
	:CF 'A))

(setq general-f-m-r04
   (make-lexdisr
	:if '(and (nextgender 'allval)
	          (next2gender 'm)
	          (not (nextgender 'f)))	; *** to avoid choices with allval
	:then 'm
	:CF 'A))

(setq general-f-m-r05
   (make-lexdisr
	:if '(or (nextcat 'num)		; l'85 per cento
                 (nextcat 'date))
	:then 'm
	:CF 'A))

(setq general-f-m-r06
   (make-lexdisr
	:if '(and (prevcat 'num)	; i 10 morti
                  (prev2gender 'm)) 
	:then 'm
	:CF 'A))

;************************************************************
;*** general: singular V plural (default: pl)

(putrule 'general-pl-sing
         '(general-pl-sing-r01 general-pl-sing-r02 general-pl-sing-r03
           general-pl-sing-r04 general-pl-sing-r05e general-pl-sing-r06e 
           general-pl-sing-r07e general-pl-sing-r08e general-pl-sing-r08bis-e 
           general-pl-sing-r08ter-e general-pl-sing-r09
           general-pl-sing-r10 general-pl-sing-r11 general-pl-sing-r12
           general-pl-sing-r13 general-pl-sing-r14 general-pl-sing-r15)
         'lexdisrules
         '(pl sing))

(putrule 'general-pl-sing 'pl 'defaultc)

;......................................................

(setq general-pl-sing-r01	; what can I see
   (make-lexdisr
	:if '(and (currtype '(mod aux)) 
                  (nextcat 'pron)
                  (nexttype 'pers)
                  (nextnumber 'sing)
                  (or (nextperson 'p1)
                      (nextperson 'p2))
                  (nextcase 'lsubj))
	:then 'sing
	:CF 'A))

(setq general-pl-sing-r02
   (make-lexdisr
	:if '(prevnumber 'pl)
	:then 'pl
	:CF 'A))

(setq general-pl-sing-r03
   (make-lexdisr
	:if '(and (prevchar '(#\' #\"))		; "
                  (prev2number 'pl))
	:then 'pl
	:CF 'A))

(setq general-pl-sing-r04
   (make-lexdisr
	:if '(and (prevnumber 'sing)
                  (not (currword-typ '&to-be))
                  (not (currword-typ '&to-give)))
                     ; The second condition to avoid application to "sono"
                     ; The third condition to avoid application to "danno"
	:then 'sing
	:CF 'A
        :lang '(italian spanish catalan french)))

(setq general-pl-sing-r05e
; *** in English, it can be singular in the past tense, but for present
;     the ambiguity does not arise; so it can be singular just in case
;     the previous word is a pronoun (I or you)
   (make-lexdisr
	:if '(and (prevnumber 'sing)
                  (or (not (currcat 'verb))
                      (currtense 'past)		; this and the next can be true only for verbs
                      (currtype 'mod)
                      (and (currtense 'pres)
                           (prevcat 'pron))))
	:then 'sing
	:CF 'A
        :lang 'english))

(setq general-pl-sing-r06e
    (make-lexdisr
 	:if '(and (nextword-typ '&loc-pron)
                  (currword-typ '&to-be))
                      ; For "Are there ..."
 	:then 'pl
 	:CF 'A
        :lang 'english))

; *** inversion for interrogatives
(setq general-pl-sing-r07e
   (make-lexdisr
	:if '(and (nextnumber 'sing)
                  (interr-sent))	; the last item is a question mark
	:then 'sing
	:CF 'A
        :lang 'english))

; *** English imperatives (ambiguous between sing and plur)
(setq general-pl-sing-r08e
   (make-lexdisr
	:if '(and (prevcat nil)
                  (not (currword-typ '&question-verb)))
	:then 'sing
	:CF 'A
        :lang 'english))

; *** English imperatives (ambiguous between sing and plur)
(setq general-pl-sing-r08bis-e
   (make-lexdisr
	:if '(and (prevchar #\,)
	          (beforecat '(verb (mood imper) (number sing))))
	:then 'sing
	:CF 'A
        :lang 'english))

; *** English imperatives (ambiguous between sing and plur)
(setq general-pl-sing-r08ter-e
   (make-lexdisr
	:if '(and (prevchar #\,)
	          (prev2cat 'adv)
                  (is-third-word))
	:then 'sing
	:CF 'A
        :lang 'english))

(setq general-pl-sing-r09
   (make-lexdisr
	:if '(and (prevword-typ '&refl-pron)		; si
                  (currword-typ '&to-be))
	:then 'pl
	:CF 'C))

(setq general-pl-sing-r10
   (make-lexdisr
	:if '(and (prevword-typ '&partitive-pron)	; ne
                  (prev2word-typ '&refl-pron-3)		;se
                  (currword-typ '&to-be))
	:then 'pl
	:CF 'C))

; *** Proper names as subjects
(setq general-pl-sing-r11
   (make-lexdisr
	:if '(and (prevcat '(noun (proper yes)))
                  (not (currword-typ '&to-be)))
	:then 'sing
	:CF 'A))

(setq general-pl-sing-r12
   (make-lexdisr
	:if '(and (nextcat 'pron)		
                  (not (nexttype 'loc))		; there are; ci sono
                  (nextperson 'p1))
	:then 'sing
	:CF 'A))

(setq general-pl-sing-r13
   (make-lexdisr
	:if '(and (prevcat 'pron)
                  (not (nexttype 'loc))		; there are; ci sono
                  (prevperson 'p1))
	:then 'sing
	:CF 'A))

(setq general-pl-sing-r14		; sono interessato
   (make-lexdisr
	:if '(and (prevcat nil)
                  (currword-typ '&to-be)
                  (nextcat 'adj)
                  (nextnumber 'sing))
	:then 'sing
	:CF 'A))

(setq general-pl-sing-r15		; sono stato catturato
   (make-lexdisr
	:if '(and (currword-typ '&to-be)
	          (nextword-typ '&to-be)
                  (nextnumber 'sing)
                  (next2cat 'verb)
                  (next2mood 'participle))
	:then 'sing
	:CF 'A))

;******************************************************
;*** adj; exclamative, interrogative (default: interr) 
;     [CATALAN: QUINS]

(putrule 'adj-exclam-interr
	'(pron-exclam-interr-relat-r01)
	'lexdisrules
	'(exclam interr))

(putrule 'adj-exclam-interr 'interr 'defaultc)

;......................................................

;******************************************************
;*** adjectives: indefinite or qualificative (default: qualif) [CERTE, CERTI, DIVERSI]

(putrule 'adj-indef-qualif
         '(adj-indef-qualif-r01 adj-indef-qualif-r02) 
         'lexdisrules
         '(indef qualif))

(putrule 'adj-indef-qualif 'qualif 'defaultc)

;......................................................

; Diverse persone ...
(setq adj-indef-qualif-r01
   (make-lexdisr
	:if '(and (nextcat-agr 'noun 'adj)
                  (not (prevcat 'noun))
                  (not (prevcat 'num)))
	:then 'indef
	:CF 'A))

; persone diverse ...
(setq adj-indef-qualif-r02
   (make-lexdisr
	:if '(and (prevcat-agr 'noun 'adj)
                  (not (nextcat 'noun)))
	:then 'qualif
	:CF 'A))

;******************************************************
;*** adjectives: possessive or qualificative (default: poss) [PROPRIO, PROPRIA]

(putrule 'adj-poss-qualif
         '(adj-poss-qualif-r01)
         'lexdisrules
         '(poss qualif))

(putrule 'adj-poss-qualif 'poss 'defaultc)

;......................................................

; un vero e Proprio disastro
(setq adj-poss-qualif-r01
   (make-lexdisr
	:if '(and (nextcat-agr 'noun 'adj)
                  (prevcat '(conj (type coord)))
                  (prev2word-typ '&true))
	:then 'qualif
	:CF 'A))

;************************************************************
;*** adverb: comparative or temporal (default: compar) [PIU']

(putrule 'adv-compar-time
         '(adv-compar-time-r01 adv-compar-time-r02 adv-compar-time-r03)
         'lexdisrules
         '(compar time))

(putrule 'adv-compar-time 'compar 'defaultc)

;......................................................

(setq adv-compar-time-r01
   (make-lexdisr
	:if '(and (nextcat 'adj)
                  (nexttype 'qualif))
	:then 'compar
	:CF 'A))

(setq adv-compar-time-r02
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (not (nextword-typ '&prep-subord-dep))	; che
                  (not (nextcat 'adv)))			; piu' avanti
	:then 'time
	:CF 'U))

(setq adv-compar-time-r03
   (make-lexdisr
	:if '(or (prevword-typ '&compar-prep)		; di
                 (nextword-typ '&compar-prep-2))	; di che
	:then 'compar
	:CF 'U))

;************************************************************
;*** adverb: locative or comparative (default: loc) [OLTRE]

(putrule 'adv-loc-compar
         '(adv-loc-compar-r01 adv-loc-compar-r02)
         'lexdisrules
         '(loc quant))

(putrule 'adv-loc-compar 'loc 'defaultc)

;......................................................

(setq adv-loc-compar-r01
   (make-lexdisr
	:if '(nextcat 'num)
	:then 'compar
	:CF 'A))

(setq adv-loc-compar-r02
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (nextcat '(art (type indef))))
	:then 'compar
	:CF 'A))

;************************************************************
;*** adverb: quantif or temporal (default: temporal) [APPENA]

(putrule 'adv-quant-time
         '(adv-quant-time-r01 adv-quant-time-r02)
         'lexdisrules
         '(quant time))

(putrule 'adv-quant-time 'time 'defaultc)

;......................................................

(setq adv-quant-time-r01
   (make-lexdisr
	:if '(nextcat 'num)
	:then 'quant
	:CF 'A))

(setq adv-quant-time-r02
   (make-lexdisr
	:if '(nextcat '(adv (type time)))	; appena dopo
	:then 'quant
	:CF 'A))

;************************************************************
;*** adverb: manner or temporal (default: temporal) 
;       [cases where a derived adverb gets the "manner" type because of
;        adjectival derivation; e.g. previously]

(putrule 'adv-manner-time 'time 'defaultc)

;......................................................

;************************************************************
;*** articles: male V female (default: f)
;    unlike the general gender rules, these check the agreement only with the next
(putrule 'art-f-m
         '(general-f-m-r03 general-f-m-r04 general-f-m-r05 art-f-m-r01 art-f-m-r02 art-f-m-r03)
         'lexdisrules
         '(f m))

(putrule 'art-f-m 'f 'defaultc)

;......................................................

(setq art-f-m-r01
   (make-lexdisr
	:if '(nextcat '(verb (mood infinite)))	; l'accostarsi
	:then 'm
	:CF 'A))

(setq art-f-m-r02		; this is applied in case the next noun is masc
   (make-lexdisr                ; but thee is also another categ, who is feminine
	:if '(and (or (nextcat '(noun (gender m)))       ; e.g. l'animale
	              (nextcat '(pron (gender m))))      ; e.g. l'altro
	          (not (nextcat '(noun (gender f))))	
	          (not (nextcat '(pron (gender f)))))
	:then 'm
	:CF 'A))

(setq art-f-m-r03
   (make-lexdisr
	:if '(and (nextcat 'adj)		; L'attuale programma
	          (not (nextcat '(noun (gender f))))	
	          (not (nextcat '(pron (gender f))))	
                  (not (next2cat '(noun (gender f)))))
	:then 'm
	:CF 'A))

;******************************************************
;*** conjunction: coordinative or subordinative (default: subord) [MENTRE]

(putrule 'conj-coord-subord
         '(conj-coord-subord-r01 conj-coord-subord-r02 conj-coord-subord-r03)
         'lexdisrules
         '(coord subord))

(putrule 'conj-coord-subord 'subord 'defaultc)

;......................................................

; Mentre giocava, Carlo ...
(setq conj-coord-subord-r01
   (make-lexdisr
	:if '(or (prevcat nil)
                 (and (nextcat 'verb)
                      (not (nextword-typ '&adv-prep-head))))  ; for "mentre ancora" ATLAS
	:then 'subord
	:CF 'U))

; , mentre se tu vuoi possiamo
(setq conj-coord-subord-r02
   (make-lexdisr
	:if '(nextcat '(conj (type subord)))
	:then 'coord
	:CF 'U))

; , mentre se tu vuoi possiamo
; *** this is a sort of pre-default, to be applied in the Meteo environment of ATLAS
(setq conj-coord-subord-r03
   (make-lexdisr
	:if '(eq *SYSTEM-CONTEXT* 'atlas)
	:then 'coord
	:CF 'U))

;******************************************************
;*** conjunction: coordinative, subordinative or comparative (default: subord) [CHE]

(putrule 'conj-compar-coord-subord
         '(conj-compar-coord-subord-r01)
         'lexdisrules
         '(compar coord subord))

(putrule 'conj-compar-coord-subord 'subord 'defaultc)

;......................................................

(setq conj-compar-coord-subord-r01
   (make-lexdisr
	:if '(beforeword-typ '&both)	; *** sia
	:then 'coord
	:CF 'A))

;************************************************************
;*** noun: common or proper (default: proprio)

(putrule 'noun-common-proper
         '(noun-common-proper-r00 ;noun-common-proper-r000 
           noun-common-proper-r001 noun-common-proper-r002 noun-common-proper-r003
           noun-common-proper-r01 noun-common-proper-r02 noun-common-proper-r03
           noun-common-proper-r04 noun-common-proper-r05 noun-common-proper-r06
           noun-common-proper-r07 noun-common-proper-r08 noun-common-proper-r09e
           noun-common-proper-r10 noun-common-proper-r11 noun-common-proper-r12
           noun-common-proper-r13 noun-common-proper-r14 noun-common-proper-r15h
           noun-common-proper-r16 noun-common-proper-r17)
         'lexdisrules
         '(common proper))

(putrule 'noun-common-proper 'proper 'defaultc)

;......................................................

;(setq noun-common-proper-r000
;   (make-lexdisr
;	:if '(currword-typ '&institutional)
;	:then 'proper
;	:CF 'U))

(setq noun-common-proper-r001
   (make-lexdisr
	:if '(currword-typ '&current-proper)
	:then 'proper
	:CF 'U))

(setq noun-common-proper-r002
   (make-lexdisr
	:if '(prevcat '(art (type indef)))
	:then 'common
	:CF 'U))

(setq noun-common-proper-r003
   (make-lexdisr			; for citations (Smart 2001)
	:if '(and (prevcat nil)
                  (nextcat 'num)
                  (next2cat nil))
	:then 'proper
	:CF 'U))

(setq noun-common-proper-r00			; this is for "n." (number) 
   (make-lexdisr                        
	:if '(and (nextcat 'num)
                  (default-proper)
                  (not (and (prevcat nil)
                            (nextcat nil))))
	:then 'common
	:CF 'A))

(setq noun-common-proper-r01
   (make-lexdisr
	:if '(and (all-capital)
                  (default-proper))
	:then 'common
	:CF 'A))

(setq noun-common-proper-r02
   (make-lexdisr
	:if '(and (nextcat '(noun (proper yes)))
                  (or (currword-typ '&institutional)
                      (currword-typ '&member))) 	; Member States
	:then 'common
	:CF 'A))

(setq noun-common-proper-r03
   (make-lexdisr
	:if '(and (nextcat '(noun (proper yes)))
                  (not (currword-typ '&institutional))
                  (not (currsemtype 'noun '£geogr-area))
                  (not (nextword-typ '&institutional)) ; Conciliation Committee
                  (not (currword-typ '&member)))
	:then 'proper
	:CF 'A))

(setq noun-common-proper-r04
   (make-lexdisr
	:if '(and (or (prevcat-agr 'art '(noun (proper no)))
                      (prevcat-agr 'adj '(noun (proper no))))
                  (not (currword-typ '&current-proper))
                  (default-proper))
	:then 'common
	:CF 'U))

(setq noun-common-proper-r05
   (make-lexdisr
	:if '(prevcat 'num)
	:then 'common
	:CF 'U))

(setq noun-common-proper-r06
   (make-lexdisr
	:if '(and (prevcat 'marker)
                  (not (nextcat '(noun (proper yes)))))
	:then 'common
	:CF 'U))

(setq noun-common-proper-r07
   (make-lexdisr
	:if '(and (prevchar #\")		; "; nel "Paese delle ... "
                  (prev2cat '(art adj))
                  (not (nextcat '(noun (proper yes)))))
	:then 'common
	:CF 'U))

(setq noun-common-proper-r08
   (make-lexdisr
	:if '(and (prevcat 'prep)
	          (nextcat '(prep adj)))
	:then 'common
	:CF 'A))

(setq noun-common-proper-r09e	; on June 36
   (make-lexdisr
	:if '(and (prevcat '(prep (word-typ &date-prep)))
                  (nextcat 'num))
	:then 'common
	:CF 'A
        :lang 'english))

(setq noun-common-proper-r10
   (make-lexdisr
	:if '(prevword-typ '&saint)
	:then 'proper
	:CF 'A))

(setq noun-common-proper-r11
   (make-lexdisr
	:if '(or (currword-typ '&institutional)
	         (currsemtype 'noun '£geogr-area)
	         (currword-typ '&section))
	:then 'common
	:CF 'U))

(setq noun-common-proper-r12
   (make-lexdisr
	:if '(prevcat '(adj (type demons)))
	:then 'common
	:CF 'U))

(setq noun-common-proper-r13
   (make-lexdisr
	:if '(currword-typ '&current-proper)
	:then 'proper
	:CF 'U))

(setq noun-common-proper-r14
   (make-lexdisr
	:if '(and (prevchar #\")		; ""Disposizioni comuni"
                  (nextcat 'adj))
	:then 'common
	:CF 'U))

(setq noun-common-proper-r15h		; hindi
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (nextcat 'verb)
                  (default-proper))
	:then 'common
	:CF 'U
        :lang 'hindi))

(setq noun-common-proper-r16	
   (make-lexdisr                        
	:if '(and (or (prevcat nil) (prevchar #\.))	; #\. For titles
                  (default-proper))
	:then 'common
	:CF 'A))

(setq noun-common-proper-r17	; l'allora Ministro degli
   (make-lexdisr
	:if '(and (prev2cat 'art)
                  (not (currword-typ '&current-proper))
                  (not (prevword-typ '&title)) ; ministro presidente signore commissione
                  (default-proper))
	:then 'common
	:CF 'U))

;******************************************************
;*** pron; exclamative, interrogative, or relative (default: relat) 
;     [CHI; ENGLISH: WHAT]

(putrule 'pron-exclam-interr-relat
	'(pron-exclam-interr-relat-r01 pron-exclam-interr-relat-r02
          pron-exclam-interr-relat-r03i pron-exclam-interr-relat-r04e 
          pron-exclam-interr-relat-r05 pron-exclam-interr-relat-r06
	  pron-exclam-interr-relat-r07)
	'lexdisrules
	'(exclam interr relat))

(putrule 'pron-exclam-interr-relat 'relat 'defaultc)

;......................................................

(setq pron-exclam-interr-relat-r01
   (make-lexdisr
	:if '(and (prevcat nil)
                  (interr-sent))	; the last item is a question mark
	:then 'interr
	:CF 'U))

(setq pron-exclam-interr-relat-r02
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (nextcat 'verb))
	:then 'relat
	:CF 'U))

(setq pron-exclam-interr-relat-r03i
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (not (currword-typ '&art-relat))	; needs an art before
                  (not (and (nextcat 'verb) ; ha confermato quanto sostenuto
                            (nextmood 'participle))))
	:then 'relat
	:CF 'U
        :lang 'italian))

(setq pron-exclam-interr-relat-r04e	; are What wittgenstein calls
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (nextcat '(pron art noun)))
	:then 'relat
	:CF 'U
        :lang 'english))

(setq pron-exclam-interr-relat-r05
   (make-lexdisr
	:if '(beforecat 'verb)
	:then 'relat
	:CF 'U))

(setq pron-exclam-interr-relat-r06
   (make-lexdisr
	:if '(prevcat '(adj noun))
	:then 'relat
	:CF 'U))

(setq pron-exclam-interr-relat-r07			; chi vivra' vedra'
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (next2cat 'verb))
	:then 'relat
	:CF 'U))

;******************************************************
;*** pron; interrogative, or relative (default: relat) [QUALE, QUANTO; WHICH]

(putrule 'pron-interr-relat
	'(pron-interr-relat-r02e pron-interr-relat-r03
          pron-exclam-interr-relat-r01 pron-exclam-interr-relat-r03i
          pron-interr-relat-r01e pron-interr-relat-r04)
	'lexdisrules
	'(interr relat))

(putrule 'pron-interr-relat 'relat 'defaultc)

;......................................................

(setq pron-interr-relat-r01e			; to determine which of
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (nextcat '(prep verb)))
	:then 'interr
	:CF 'U
        :lang 'english))

(setq pron-interr-relat-r02e			; [puoi dirmi] quanto costano
   (make-lexdisr
	:if '(prevcat 'pron)
	:then 'relat
	:CF 'A
        :lang 'english))

(setq pron-interr-relat-r03			; [puoi dirmi] quanto costano
   (make-lexdisr
	:if '(prevcat '(nil pron))
	:then 'interr
	:CF 'U))

(setq pron-interr-relat-r04			; pre-default per 'quali'
   (make-lexdisr
	:if '(and (currword-typ '&art-relat)
                  (not (prevcat 'art)))
	:then 'interr
	:CF 'U))

;******************************************************
;*** pron; demonstrative or relative (default: relat) [ENGLISH: THAT]

(putrule 'pron-demons-relat
         '(pron-demons-relat-r01)
         'lexdisrules
         '(demons relat))

(putrule 'pron-demons-relat 'relat 'defaultc)

;......................................................

(setq pron-demons-relat-r01 	; the idea is That of
   (make-lexdisr
	:if '(and (nextcat 'prep)
                  (not (prevcat 'noun)))
	:then 'demons
	:CF 'A))

;******************************************************
;*** pron; indefinite or relative (default: relat) [CHIUNQUE; ENGLISH: THAT]

(putrule 'pron-indef-relat
         '(pron-indef-relat-r01 pron-indef-relat-r02e)
         'lexdisrules
         '(indef relat))

(putrule 'pron-indef-relat 'relat 'defaultc)

;......................................................

(setq pron-indef-relat-r01
   (make-lexdisr
	:if '(nextword-typ '&other)
	:then 'indef
	:CF 'A))

(setq pron-indef-relat-r02e		; is That of
   (make-lexdisr
	:if '(and (prevcat 'verb)
                  (nextcat 'prep))
	:then 'indef
	:CF 'A
        :lang 'english))

;************************************************************
;*** pron: personal or locative (default: pers)  CI, VI

(putrule 'pron-pers-loc
         '(pron-pers-loc-r01 pron-pers-loc-r02 pron-pers-loc-r03 pron-pers-loc-r04)
         'lexdisrules
         '(pers loc))

(putrule 'pron-pers-loc 'pers 'defaultc)

;......................................................

(setq pron-pers-loc-r01
   (make-lexdisr
	:if '(and (prevword-typ '&loc-pron)
                  (nextword-typ '&to-be-want))
	:then 'pers
	:CF 'A))

(setq pron-pers-loc-r02
   (make-lexdisr
	:if '(or (nextword-typ '&to-be-want)
                 (and (nextcat '(verb (type mod)))
                      (next2word-typ '&to-be-want)))
	:then 'loc
	:CF 'A))

(setq pron-pers-loc-r03
   (make-lexdisr
	:if '(and (nextword-typ '&partitive-pron)	; ce ne sono
                  (next2word-typ '&to-be-want))
	:then 'loc
	:CF 'A))

(setq pron-pers-loc-r04
   (make-lexdisr
	:if '(and (nextword-typ '&refl-pron)		; ci si
                  (next2cat 'verb))
	:then 'loc
	:CF 'A))

;**********************************************************
;*** pron: clitic or normal (default: nil) [ME, TE]

(putrule 'pron-clitic
         '(pron-clitic-r01 pron-clitic-r02)
         'lexdisrules
         '(notclit clitic))

(putrule 'pron-clitic 'notclit 'defaultc)

;......................................................

(setq pron-clitic-r01
   (make-lexdisr
	:if '(and (nextcat 'pron)
                  (next2cat 'verb)) 
        :then 'clitic			; clitic
        :CF 'A))
 
(setq pron-clitic-r02
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (not (nextcat '(verb pron))))
	:then 'notclit			; not clitic
	:CF 'C))

;************************************************************
;*** verb: auxiliaries or standard (default: aux)

(putrule 'verb-aux-main
         '(
           verb-aux-main-r0001 verb-aux-main-r0002
           verb-aux-main-r01 verb-aux-main-r02ics verb-aux-main-r02e 
           verb-aux-main-r03ics verb-aux-main-r03e verb-aux-main-r04
           verb-aux-main-r05ics verb-aux-main-r06e verb-aux-main-r07e verb-aux-main-r08
           verb-aux-main-r09i verb-aux-main-r10e verb-aux-main-r11i verb-aux-main-r12 
           verb-aux-main-r13e verb-aux-main-r14e verb-aux-main-r15 verb-aux-main-r16
           verb-aux-main-r17
          )
         'lexdisrules
         '(aux main))

(putrule 'verb-aux-main 'aux 'defaultc)

;......................................................

(setq verb-aux-main-r0001			; for ATLAS
   (make-lexdisr
	:if '(and (eq *SYSTEM-CONTEXT* 'atlas)
                  (nextcat 'adj)
                  (nextsemtype 'adj '£weather-eval))
	:then 'main
	:CF 'U))

(setq verb-aux-main-r0002			; for ATLAS
   (make-lexdisr
	:if '(and (eq *SYSTEM-CONTEXT* 'atlas)
                  (nextcat 'adv)
                  (next2cat 'adj)
                  (next2semtype 'adj '£weather-eval))
	:then 'main
	:CF 'U))

(setq verb-aux-main-r01
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (nextmood-simple 'participle)
  ; *** nextmood-simple to accept only non compound participles:
  ;     'Essi sono persone' ('sono' is not aux, even if 'persone'
  ;     has a verb+clitic reading)
                  (nexttense 'past)
  ; *** the next four lines to avoid application when the rule that
  ;     will be applied for the next word is ADJ-VERBR14, which will
  ;     select the ADJ reading, so that the sequence AUX-ADJ would
  ;     be inconsistent
	          (not (and (nextcat 'adj)
                            (nextword-typ '&adj-pref-verb)
                            (currword-typ '&to-be)
                            (not (prevcat 'pron))
                            (currtense 'pres)))
                  (not (currword-typ '&progressive-aux))
                              ; the next for "non ci siano riflessi"
                  (or (not (prevword-typ '&loc-pron))
                      (not (currword-typ '&to-be)))
                  (or (not (currword-typ '&tense-aux))
                      (not (prevword-typ '&tense-aux))))
	:then 'aux
	:CF 'U))

(setq verb-aux-main-r02ics
   (make-lexdisr
	:if '(and (or (nextcat 'adv)
                      (nextword-typ '&all-pron)
                      (nextchar '(#\")))	; virgolette aperte "  ; formerly also #\«
                                                ; but this is no more accepted by the compiler (09/11/17)
                  (next2cat 'verb)
                  (next2mood 'participle)
                  (next2tense 'past))
	:then 'aux
	:CF 'U
        :lang '(italian catalan spanish french)))

(setq verb-aux-main-r02e			; it is not surprising
   (make-lexdisr
	:if '(and (or (nextcat 'adv)
                      (nextword-typ '&all-pron)
                      (nextchar '(#\")))	; virgolette aperte "  ; formerly also #\«
                                                ; but this is no more accepted by the compiler (09/11/17)
                  (next2cat 'verb)
                  (not (next2cat 'adj))
                  (next2mood 'participle))
	:then 'aux
	:CF 'U
        :lang 'english))

(setq verb-aux-main-r03ics
   (make-lexdisr
	:if '(and (currword-typ '&progressive-aux)
                  (or (nextcat 'adv)
                      (nextchar '(#\")))	; virgolette aperte "  ; formerly also #\«
                                                ; but this is no more accepted by the compiler (09/11/17)
                  (next2cat 'verb)
                  (next2mood 'gerund))
	:then 'aux
	:CF 'U
        :lang '(italian catalan spanish french)))

(setq verb-aux-main-r03e
   (make-lexdisr
	:if '(and (currword-typ '&progressive-aux)
                  (or (nextcat 'adv)
                      (nextchar '(#\")))	; virgolette aperte "  ; formerly also #\«
                                                ; but this is no more accepted by the compiler (09/11/17)
                  (next2cat 'verb)
                  (not (next2cat 'adj))
                  (next2mood 'gerund))
	:then 'aux
	:CF 'U
        :lang 'english))

(setq verb-aux-main-r04				; erano state controllate 
   (make-lexdisr
	:if '(and (prevword-typ '&to-be)
                  (currword-typ '&to-be)
                  (or (and (nextcat 'verb)
                           (nextmood-simple 'participle)
                           (nexttense 'past))
                      (and (nextcat 'adv)
                           (next2cat 'verb)
                           (next2mood 'participle)
                           (next2tense 'past))))
	:then 'aux
	:CF 'U))

(setq verb-aux-main-r05ics
   (make-lexdisr
	:if '(and (or (not (nextcat 'verb))
                      (not (nextmood-simple 'participle)))
                  (or (not (nextcat 'adv))
                      (not (next2cat 'verb))
                      (not (and (next2mood 'participle)
                                (next2tense 'past))))
                  (or (not (nextchar '(#\' #\")))	; "
                      (not (next2cat 'verb))
                      (not (next2mood 'participle)))
                  (or (not (prevcat 'pron))		; si sia privato
                      (not (prevtype 'refl-impers))))
	:then 'main
	:CF 'U
        :lang '(italian catalan spanish french)))

(setq verb-aux-main-r06e
   (make-lexdisr
	:if '(and (or (not (nextcat 'verb))
                      (not (nextmood-simple 'participle)))
                  (or (not (nextcat 'adv))
                      (not (next2cat 'verb))
                      (not (next2mood '(gerund participle))))
                  (or (not (nextchar '(#\' #\")))	; "
                      (not (next2cat 'verb))
                      (not (next2mood 'participle)))
                  (or (not (nextcat 'pron))		; are they showing
                      (not (next2cat 'verb))
                      (not (next2mood '(gerund participle)))))
	:then 'main
	:CF 'U
        :lang 'english))

(setq verb-aux-main-r07e
   (make-lexdisr
	:if '(and (currword-typ '&to-be)
                  (not (prevcat 'pron))
                  (not (prevcat '(verb (type mod))))
                  (not (and (prevcat 'conj)
                            (nextcat '(verb (mood gerund)))))
                  (currtense 'pres)
                  (nextcat 'adj))
	:then 'main
	:CF 'U
        :lang 'english))

(setq verb-aux-main-r08
   (make-lexdisr
	:if '(and (currword-typ '&tense-aux)
                  (prevword-typ '&tense-aux))
	:then 'main
	:CF 'U))

(setq verb-aux-main-r09i
   (make-lexdisr
	:if '(and (currword-typ '&progressive-aux)
                  (nextcat 'verb)
                  (nextmood 'gerund))
	:then 'aux
	:CF 'A
        :lang '(italian french)))

(setq verb-aux-main-r10e
   (make-lexdisr
	:if '(and (currword-typ '&progressive-aux)
                  (nextcat 'verb)
                  (not (nextcat 'adj))
                  (nextmood 'gerund))
	:then 'aux
	:CF 'A
        :lang 'english))

(setq verb-aux-main-r11i
   ; *** this rule disambiguates the non-auxiliary forms of the verb
   ;     "andare"; since the default of this ambiguity class is "AUX",
   ;     this forces the opposite result for all cases where the verb
   ;     "andare" cannot be an auxiliary
   ; *** the two relevant cases are the ones of "va fatto" e "andava
   ;     pensando"
   (make-lexdisr
	:if '(and (currword-typ '&strong-pass-aux)
		  (not (and (nextcat 'verb)
                            (nextmood 'participle)
                            (currperson 'p3)))
		  (not (and (nextcat 'verb)
                            (nextmood 'gerund))))
	:then 'main
	:CF 'U
        :lang '(italian french)))

(setq verb-aux-main-r12
   (make-lexdisr
	:if '(and (currword-typ '&tense-aux)
                  (not (and (nextcat 'verb)
                  	    (nextmood 'participle)))
                  (nextcat 'noun))
	:then 'main
	:CF 'A))

(setq verb-aux-main-r13e
   (make-lexdisr
	:if '(and (currword-typ '&to-be)
                  (nextcat 'adj)
                  (nextword-typ '&adj-pref-verb))
	:then 'main
	:CF 'A
        :lang 'english))

(setq verb-aux-main-r14e
   (make-lexdisr
	:if '(and (currword-typ '&to-be)
                  (nextcat 'adv)
                  (next2cat 'adj)
                  (next2word-typ '&adj-pref-verb))
	:then 'main
	:CF 'A
        :lang 'english))

(setq verb-aux-main-r15
   (make-lexdisr
	:if '(and (nextcat 'adj)
                  (not (and (nextcat 'verb)
                  	    (nextmood 'participle)
                  	    (nexttense 'past)))
	          (not (and (nextcat 'adv)
                            (next2cat 'verb)
                            (next2mood 'participle)
                            (next2tense 'past))))
	:then 'main
	:CF 'U))

(setq verb-aux-main-r16
   (make-lexdisr
	:if '(and (nextcat '(conj art prep))
	          (not (and (nextcat 'adv)
                            (next2cat 'verb)
                            (next2mood 'participle)
                            (next2tense 'past))))
	:then 'main
	:CF 'U))

; E' anche inportante
(setq verb-aux-main-r17
   (make-lexdisr
	:if '(and (prevcat nil)
                  (or (and (nextcat 'adj)
                           (not (and (nextcat 'verb)
                                     (nextmood 'participle)
                                     (nexttense 'past))))
                      (and (nextcat 'adv)
                           (next2cat 'adj)
                           (not (and (next2cat 'verb)
                                     (next2mood 'participle)
                                     (next2tense 'past))))))
	:then 'main
	:CF 'U))

;************************************************************
;*** verb: auxiliaries+pron or standard+pron (default: aux+pron) [AVERLA]

(putrule 'verb-aux+pron-main+pron
         '(verb-aux-main-r01 verb-aux-main-r02ics  verb-aux-main-r02e 
           verb-aux-main-r03ics verb-aux-main-r03e verb-aux-main-r04
           verb-aux-main-r05ics verb-aux-main-r06e verb-aux-main-r07e verb-aux-main-r08
           verb-aux-main-r09i verb-aux-main-r10e verb-aux-main-r11i verb-aux-main-r12 
           verb-aux-main-r13e verb-aux-main-r14e verb-aux-main-r15 verb-aux-main-r16)
         'lexdisrules
         '(aux main))

(putrule 'verb-aux-main 'aux 'defaultc)

;......................................................

;************************************************************
;*** verb: modals or standard (default: mod)

(putrule 'verb-main-mod
         '(verb-main-mod-r01 verb-main-mod-r02e verb-main-mod-r03 verb-main-mod-r04)
         'lexdisrules
         '(mod main))

(putrule 'verb-main-mod 'mod 'defaultc)

;......................................................

(setq verb-main-mod-r01
   (make-lexdisr
	:if '(and (nextcat 'verb)
                  (nextmood 'infinite))
	:then 'mod
	:CF 'C))

(setq verb-main-mod-r02e
   (make-lexdisr
	:if '(and (nextcat 'pron) 
                  (next2cat 'verb)
                  (next2mood 'infinite))
	:then 'mod
	:CF 'C
        :lang 'english))

(setq verb-main-mod-r03
   (make-lexdisr
	:if '(or (not (nextcat 'verb))
	         (not (nextmood 'infinite)))
	:then 'main
	:CF 'U))

(setq verb-main-mod-r04
   (make-lexdisr
	:if '(and (nextcat 'adv)
                  (next2cat 'verb)
                  (next2mood 'infinite))
	:then 'mod
	:CF 'A))

;************************************************************
;*** verb: person p1 V p2 V p3 (default p3)

(putrule 'verb-1-2-3
         '(verb-1-2-3-r01 verb-1-2-3-r02 verb-1-2-3-r03 verb-1-2-3-r04)
         'lexdisrules
         '(p1 p2 p3))

(putrule 'verb-1-2-3 'p3 'defaultc)

;......................................................

(setq verb-1-2-3-r01
   (make-lexdisr
	:if '(currmood 'imper)
	:then 'p2
	:CF 'A))

(setq verb-1-2-3-r02
   (make-lexdisr
	:if '(or (prevword-typ '&first-pers-pron) 
                 (nextword-typ '&first-pers-pron))
	:then 'p1
	:CF 'C))

(setq verb-1-2-3-r03
   (make-lexdisr
	:if '(or (prevword-typ '&second-pers-pron) 
                 (nextword-typ '&second-pers-pron))
	:then 'p2
	:CF 'C))

; Note that ...
(setq verb-1-2-3-r04
   (make-lexdisr
	:if '(and (prevcat nil)
                  (or (nextword-typ '&base-subord-conj)
                      (and (nextcat 'pron)
                           (not (nextword-typ '&loc-pron)))))	; *Are there
	:then 'p2
	:CF 'A))

;************************************************************
;*** verb: person p1 V p2 (default p1) [AVESSI, FOSSI]

(putrule 'verb-1-2
         '(verb-1-2-3-r02 verb-1-2-3-r03)
         'lexdisrules
         '(p1 p2))

(putrule 'verb-1-2 'p1 'defaultc)

;......................................................

;************************************************************
;*** verb: person p1 V p3 (default p3) [SONO; CATALAN: VOLDRIA]

(putrule 'verb-1-3
         '(verb-1-2-3-r02 verb-1-3-r01 verb-1-3-r02 verb-1-3-r03 verb-1-3-r04)
         'lexdisrules
         '(p1 p3))

(putrule 'verb-1-3 'p3 'defaultc)
;......................................................

(setq verb-1-3-r01
   (make-lexdisr
	:if '(prevword-typ '&dat-pron)
	:then 'p1
	:CF 'U))

(setq verb-1-3-r02
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat 'noun))
	:then 'p1
	:CF 'U))

(setq verb-1-3-r03	; Voldria preguntar
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat 'verb))
	:then 'p1
	:CF 'U))

(setq verb-1-3-r04	; Hola, voldria informaciò
   (make-lexdisr
	:if '(and (prevchar #\,)
                  (prev2cat 'interj))
	:then 'p1
	:CF 'U))

;************************************************************
;*** verb: person p2 V p3 (default p2) [imperatives]

(putrule 'verb-2-3
         '()
         'lexdisrules
         '(p2 p3))

(putrule 'verb-2-3 'p2 'defaultc)
;......................................................

;************************************************************
;*** verb: finite V infinite (default: fin)
;    finite: ind, cong, condiz, imper
;    infinite: participle, gerund, infinite

(putrule 'verb-fin-inf
         '(verb-fin-inf-r01 verb-fin-inf-r02 verb-fin-inf-r03 verb-fin-inf-r04e verb-fin-inf-r05e
           verb-fin-inf-r06e verb-fin-inf-r06bis-e 
           verb-fin-inf-r07e verb-fin-inf-r08ics verb-fin-inf-r09e
           verb-fin-inf-r10e verb-fin-inf-r11i verb-fin-inf-r12i verb-fin-inf-r13i
           verb-fin-inf-r14 verb-fin-inf-r15 verb-fin-inf-r16 verb-fin-inf-r17 verb-fin-inf-r17bis-e 
           verb-fin-inf-r17ter-e
           verb-fin-inf-r18 verb-fin-inf-r19 verb-fin-inf-r20 verb-fin-inf-r21e verb-fin-inf-r22e
           verb-fin-inf-r23e verb-fin-inf-r24e)
         'lexdisrules
         '(fin inf))

(putrule 'verb-fin-inf 'fin 'defaultc)

;......................................................

(setq verb-fin-inf-r01
   (make-lexdisr
	:if '(or (prevcat '(verb (type mod)))
	         (prevcat '(verb (type aux))))
	:then 'inf
	:CF 'U))

(setq verb-fin-inf-r02
   (make-lexdisr
	:if '(and (prevcat 'adv)
                  (or (prev2cat '(verb (type mod)))
                      (prev2cat '(verb (type aux)))))
	:then 'inf
	:CF 'U))

(setq verb-fin-inf-r03
   (make-lexdisr
	:if '(and (prevword-typ '&all-pron)
                  (or (prev2cat '(verb (type mod)))
                      (prev2cat '(verb (type aux)))))
	:then 'inf
	:CF 'U))

; the show MUST go on
(setq verb-fin-inf-r04e
   (make-lexdisr
	:if '(and (prev2cat '(verb (type mod)))
                  (prevcat 'pron)
                  (currmood 'infinite))
	:then 'inf
	:CF 'A
        :lang 'english))

; the show MUST go on
(setq verb-fin-inf-r05e
   (make-lexdisr
	:if '(and (prevcat 'noun)
                  (currtype 'mod)
                  (nextcat '(adv verb)))
	:then 'fin
	:CF 'A
        :lang 'english))

; it is necessary to MAKE it
(setq verb-fin-inf-r06e
   (make-lexdisr
	:if '(and (prevcat 'prep)
                  (prevword-typ '&base-subord-prep))
	:then 'inf
	:CF 'A
        :lang 'english))

; in_order_to prepare
(setq verb-fin-inf-r06bis-e
   (make-lexdisr
	:if '(and (prevcat 'conj)
                  (prevtype 'subord)
                  (prevword-typ '&inf-subord-conj))
	:then 'inf
	:CF 'A
        :lang 'english))

; Note that ...
(setq verb-fin-inf-r07e
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextword-typ '&base-subord-conj))
	:then 'fin
	:CF 'A
        :lang 'english))

; le perle trovate da Lucia ...
; i racconti, inventati da Carlo, ...
(setq verb-fin-inf-r08ics
   (make-lexdisr
	:if '(and (or (prevcat '(noun (proper nil)))
                      (prevchar #\,))
                  (currmood 'participle))
	:then 'inf
	:CF 'U
        :lang '(italian spanish)))

(setq verb-fin-inf-r09e
   (make-lexdisr
	:if '(and (or (prevcat '(noun (proper nil)))
                      (prevchar #\,))
                  (currmood 'participle)
                  (not (currmood 'ind)))
	:then 'inf
	:CF 'U
        :lang 'english))

(setq verb-fin-inf-r10e
   (make-lexdisr
	:if '(and (or (prevcat '(noun (proper nil)))
                      (prevchar #\,))
                  (currmood 'participle)
                  (nextword-typ '&agent-compl-prep))
	:then 'inf
	:CF 'U
        :lang 'english))

; visto che ...
(setq verb-fin-inf-r11i
   (make-lexdisr
	:if '(nextword-typ '&prep-subord-dep)	; che
	:then 'inf
	:CF 'U
        :lang 'italian))

; visto che ...
(setq verb-fin-inf-r12i
   (make-lexdisr
	:if '(and (prevcat '(conj (type coord)))
                  (prev2mood '(infinite participle gerund)))
	:then 'inf
	:CF 'U
        :lang 'italian))

; vista la situazione
(setq verb-fin-inf-r13i
   (make-lexdisr
	:if '(currword-typ '&to-see)
	:then 'inf
	:CF 'U
        :lang 'italian))

; delle Contestate elezioni
(setq verb-fin-inf-r14
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
	          (currmood 'ind)
                  (prevcat 'art)
                  (nextcat 'noun))
	:then 'inf
	:CF 'U))

; l'annuncio, dato a Tirana
(setq verb-fin-inf-r15
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
	          (currmood 'ind)
	          (or (prevchar #\,) (prevchar #\'))
                  (prev2cat '(noun adj)))
	:then 'inf
	:CF 'U))

; dalle mafie locali collegate a
(setq verb-fin-inf-r16
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
	          (currmood 'ind)
                  (prevcat 'adj)
                  (prev2cat 'noun))
	:then 'inf
	:CF 'U))

; can be stopped or suspended
(setq verb-fin-inf-r17
   (make-lexdisr
	:if '(and (currmood 'participle)
                  (prevcat 'conj)
                  (prevtype 'coord)
                  (prev2mood 'participle))
	:then 'inf
	:CF 'U))

; can be stopped or suspended
(setq verb-fin-inf-r17bis-e
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
	          (currmood 'ind)
                  (prevcat 'adv)
                  (prev2cat 'prep)
                  (nextcat 'noun))
	:then 'inf
	:CF 'U
        :lang 'english))

; with bread soaked in buttermilk
(setq verb-fin-inf-r17ter-e
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
	          (currmood 'ind)
                  (prevcat 'noun)
                  (prev2cat 'prep)
                  (nextcat 'prep))
	:then 'inf
	:CF 'U
        :lang 'english))

; poi non successe granchè
(setq verb-fin-inf-r18
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
	          (currmood 'ind)
                  (not (prevword-typ '&that))
	          (not (prevword-typ '&noun-not-subj))		; see verb-fin-infr22
                  (not (prevcat 'verb))
                  (not (prevcat 'art))
                  (not (and (prevcat 'pron)
                            (prev2word-typ '&verb-governing-inf)))
                  (not (and (prevcat '(pron (type indef)))	; i pochi rimasti
                            (prev2cat 'art)))
                  (not (and (prevcat '(adj (type (deitt demons poss))))	; their assumed knowledge
                            (nextcat 'noun)))
                  (not (and (prevcat 'adv)			; subject-centred
                            (prev2cat '(adv punct pron adj))))
                  (not (and (prevcat 'adv)			; the previously mentioned case
                            (prev2cat 'art)
                            (nextcat 'noun)))
                  (not (and (prevchar #\-)			; subject-centred
                            (prev2cat 'noun)))
                  (not (and (currword-typ '&verb-prep-ambig)	; dire, dare (for Dato, Detto)
                            (prevcat nil)
                            (nextcat 'pron)
                            (next2cat '(verb punct))))
                  (not (and (prevcat 'prep)		; levels of dissolved oxygen
                            (nextcat 'noun)))
                  (not (and (prevcat 'adv)		; wash off plowed fields
                            (prev2cat 'verb)
                            (nextcat 'noun)))
                  (not (and (currmood 'infinite)	; we must understand the problems and become
                            (prevverb '(mood infinite))))
                  (not (nextword-typ '&agent-compl-prep))
                  (not (and (or (prevcat 'adv)
                                (prevchar '(#\' #\")))	;"
                            (or (prev2type 'aux)
				(prev2cat 'noun)))))
	:then 'fin
	:CF 'U))

(setq verb-fin-inf-r19
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
                  (prevword-typ '&that))
	:then 'inf
	:CF 'U))

; semi-default preference for past participles wrt indicatives
(setq verb-fin-inf-r20
   (make-lexdisr
	:if '(and (currmood+tense 'participle 'past)
	          (currmood 'ind)
                  (not (prevcat 'pron))
                  (not (prevcat '(verb (mood ind)))))
	:then 'inf
	:CF 'U))

(setq verb-fin-inf-r21e
   (make-lexdisr
  ; *** the second argument of beforecat stops the search
	:if '(or (beforecat '(verb (type mod)) '(verb (type main)))
	         (beforeword-typ '&verb-governing-inf))
	:then 'inf
	:CF 'U
        :lang 'english))

(setq verb-fin-inf-r22e
   (make-lexdisr
	:if '(and (prevcat '(adj (type poss)))
                  (currmood+tense 'participle 'past)
                  (nextcat 'noun))
	:then 'inf
	:CF 'U
        :lang 'english))

(setq verb-fin-inf-r23e
   (make-lexdisr
	:if '(and (prevword-typ '&predverb)
                  (currmood+tense 'participle 'past))
	:then 'inf
	:CF 'U
        :lang 'english))

(setq verb-fin-inf-r24e
   (make-lexdisr
  ; *** this is for HOPS: "furniture left", where the verb must be past part
	:if '(prevword-typ '&noun-not-subj)
	:then 'inf
	:CF 'U
        :lang 'english))

;************************************************************
;*** verb: conjunctive V imperative (default: cong)

(putrule 'verb-cong-imper 'cong 'defaultc)

;......................................................

;************************************************************
;*** verb: conjunctive V indicative (default: ind) [English modals]

(putrule 'verb-cong-ind 'ind 'defaultc)

;......................................................

;************************************************************
;*** verb: conjunctive V imperative V indicative (default: ind)

(putrule 'verb-cong-imper-ind
         '(verb-cong-imper-ind-r01 verb-cong-imper-ind-r02i verb-cong-imper-ind-r03e 
           verb-cong-imper-ind-r04e verb-cong-imper-ind-r05e verb-cong-imper-ind-r06e
           verb-cong-imper-ind-r07e verb-cong-imper-ind-r08e verb-cong-imper-ind-r09)
         'lexdisrules
         '(cong ind imper))

(putrule 'verb-cong-imper-ind 'ind 'defaultc)

;......................................................

;  Quit
(setq verb-cong-imper-ind-r01
   (make-lexdisr
	:if '(and (prevcat nil)
                  (or (nextcat nil)
                      (nextchar '(#\. #\!))))	
	:then 'imper
	:CF 'U))

; una legge che Disciplini la
(setq verb-cong-imper-ind-r02i
   (make-lexdisr
	:if '(and (prevcat '(pron (type relat)))
                  (nextcat '(art prep)))
	:then 'cong
	:CF 'U
        :lang 'italian))

; Can I ..?.; Do you ...?
(setq verb-cong-imper-ind-r03e
   (make-lexdisr
	:if '(and (prevcat nil)
                  (currtype 'mod)
                  (nextcat 'pron)
                  (nexttype 'pers))
	:then 'ind
	:CF 'A
        :lang 'english))

; Note that ...
(setq verb-cong-imper-ind-r04e
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextword-typ '&base-subord-conj))
	:then 'imper
	:CF 'A
        :lang 'english))

; Note that ...
(setq verb-cong-imper-ind-r05e
   (make-lexdisr
	:if '(and (prevcat nil)
                  (nextcat '(adv pron art prep)))
	:then 'imper
	:CF 'A
        :lang 'english))

(setq verb-cong-imper-ind-r06e
   (make-lexdisr
	:if '(and (prev2cat nil)
                  (prevcat 'adv)
                  (nextcat '(adv pron art prep)))
	:then 'imper
	:CF 'A
        :lang 'english))

(setq verb-cong-imper-ind-r07e
   (make-lexdisr
	:if '(and (or (prevchar #\,) (prevcat '(conj (type coord))))
	          (beforecat '(verb (mood imper))))
	:then 'imper
	:CF 'U
        :lang 'english))

(setq verb-cong-imper-ind-r08e
   (make-lexdisr
	:if '(and (prevchar #\,)
	          (prev2cat 'adv)
                  (is-third-word))
	:then 'imper
	:CF 'U
        :lang 'english))

; purchè ciò avvenga in tempo
(setq verb-cong-imper-ind-r09
   (make-lexdisr
	:if '(beforeword-typ '&conj-governing-subjunctive)
	:then 'cong
	:CF 'U))

;************************************************************
;*** verb: imperative V indicative (default: ind)

(putrule 'verb-imper-ind
         '(verb-imper-ind-r01 verb-imper-ind-r02 verb-imper-ind-r03)
         'lexdisrules
         '(ind imper))

(putrule 'verb-imper-ind 'ind 'defaultc)

;......................................................

; *** "(vedi la figura ...)"
(setq verb-imper-ind-r01
   (make-lexdisr
	:if '(prevchar #\()		;)
	:then 'imper
	:CF 'U))

(setq verb-imper-ind-r02
   (make-lexdisr
  ; *** this rule is for dialogues, where  single word can be
  ;     taken as an order (print, seleziona)
	:if '(and (prevcat nil)
                  (currword-typ '&system-op))
	:then 'imper
	:CF 'U))

(setq verb-imper-ind-r03
   (make-lexdisr
  ; *** this rule is for tocai, where most verbs are commands 
	:if '(and (or (eq *SYSTEM-CONTEXT* 'tocai)
	              (eq *SYSTEM-CONTEXT* 'tocai-test))
                  (not (beforecat '(verb (type (main mod)))))
                  (currword-typ '&system-op))
	:then 'imper
	:CF 'U))

;************************************************************
;*** verb: gerund V participle (default: gerund)
; *** this is for ENGLISH and for FRENCH

(putrule 'verb-gerund-participle
         '(verb-gerund-participle-r01 verb-gerund-participle-r02 verb-gerund-participle-r03
           verb-gerund-participle-r04)
         'lexdisrules
         '(gerund participle))

(putrule 'verb-gerund-participle 'gerund 'defaultc)

;......................................................

(setq verb-gerund-participle-r01
   (make-lexdisr
	:if '(prevcat 'prep)
	:then 'gerund
	:CF 'A
        :lang 'english))

(setq verb-gerund-participle-r02
   (make-lexdisr
	:if '(prevcat '(conj (type subord)))
	:then 'gerund
	:CF 'A
        :lang 'english))

(setq verb-gerund-participle-r03
   (make-lexdisr
	:if '(or (nextcat 'noun)
	         (prevcat 'noun))
	:then 'participle
	:CF 'U
        :lang '(english french)))

(setq verb-gerund-participle-r04
   (make-lexdisr
	:if '(and (prevchar #\,)
	          (prev2cat '(noun adj)))
	:then 'participle
	:CF 'U
        :lang '(english french)))

;************************************************************
;*** verb: infinite V participle (default: participle) [FOUND, BECOME]
; *** this is for ENGLISH

(putrule 'verb-infinite-participle
         '(verb-infinite-participle-r01 verb-infinite-participle-r02 verb-infinite-participle-r03)
         'lexdisrules
         '(infinite participle))

(putrule 'verb-infinite-participle 'participle 'defaultc)

;......................................................

(setq verb-infinite-participle-r01
   (make-lexdisr
	:if '(prevtype 'aux)
	:then 'participle
	:CF 'U))

(setq verb-infinite-participle-r02
   (make-lexdisr
	:if '(prevtype 'mod)
	:then 'infinite
	:CF 'U))

(setq verb-infinite-participle-r03
   (make-lexdisr
	:if '(prevverb '(mood infinite))
	:then 'infinite
	:CF 'U))

;************************************************************
; *** ATLAS: locution "un po'"

(putrule 'atlas-locut
         '(atlas-locut-r01)
         'lexdisrules
         '(locution-yes locution-no))

(putrule 'atlas-locut 'locution-yes 'defaultc)

;......................................................

(setq atlas-locut-r01
   (make-lexdisr
	:if '(and (currword-typ '&atlas-ambig-locut)	; un-po'
	          (after-loc-typ '&neutral-prep))
	:then 'locution-no
	:CF 'U))

