(in-package "USER")

; *********************************************************************
; *** REPRESENTATION OF CHARACTERS
; *********************************************************************
;     Currently, there are three different representation for chars
;     1. The "character" symbols in Lisp (e.g. #\a #\& ...)
;     2. The Tule names of symbols (e.g. down-a ampersand)
;     3. The numerical character codes (e.g. 97 38)
;        Unfortunately, the numerical codes are based on two different
;        encodings: Utf-8 and Iso-8859-1 (iso-latin-1). We also have
;        different types of data:
;        - The input files
;        - The Tule dictionaries
;        - The internal Lisp data structures
;        Since all of them can be encoded in any scheme, we have in 
;        principle eight possible situations, but the possibilities actually
;        are four, since the dictionaries are currently stored in Iso-8859-1.
;        Moreover, the new versions of Common Lisp are based on a Utf-8
;        encoding, so the only real differences stand in the input files
;        In order to make clear the situation, let's consider the word "più"
;        (more). 
;        - In order to access the dictionary data about it (ex. via
;          (get word 'diz-gw) or (get word 'locutdef))), then "word" (i.e. più)
;          must be encoded as (80 73 217), corresponding to uppercase 'p',
;          uppercase 'i', and lowercase 'ù', where 217 is the iso-8853-1
;          encoding of uppercase 'ù'. Since this encoding is fixed, it
;          is called, in the following, BASE-CODE. 
;        - In current versions of Common Lisp, (get 'più 'diz-gw)
;          does not work, since typed atoms are encoded in utf-8, so that
;          the first argument of 'get' above is exploded into (80 73 195 185),
;          which is a different atom. 
;        - Finally, from the keyboard, the word
;          in the dictionary is not shown correctly (it appears as PI¿).
; *** In order to keep apart the various cases, two global variables are set
;     in the loading files:
;     1. *LISP-CHAR-SET-ID*, which refers to the internal Lisp encoding, and
;        is now rather fixed to Utf-8
;     2. *CHAR-SET-ID*, which refers to the input data encoding
; *** the data about the mapping are kept in the table *SYMBOL-CODE-TABLE*
;     initialized by the function set-symbol-codes below.
; *** Some data structures are maintained to simplify the conversion:
;     - set-code-to-tule-table creates a structured table (*CODE-TO-TULE-TABLE*) that
;       holds the data about the mapping from 3 to 2
;     - set-lisp-to-tule-table creates a simple table (*LISP-TO-TULE-TABLE*) that
;       holds the data about the mapping from 1 to 2
;     - the properties "lisp-char" and "char-value" associated with the Tule
;       names hold the info about the mapping from 2 to 1 and from 2 to 3,
;       respectively
; *** The conversion between representations is one of the following functions: 
; --------- CONVERSION OF SINGLE CHARS ---------------------------------------
;     1 --> 2: The function "get-tule-char-name-from-lisp-char" takes a lisp char and
;              returns a tule name. Keeping the example of 'più', applying this
;              function to the exploded dictionary entry key, we get (UP-P UP-I UP-U-GRAVE)
;              If we apply it to the typed word, we get (UP-P UP-I UP-A-TILDE UP-ONE)
;     1 --> 3: The function "get-numcode-from-lisp-char" takes a Lisp char and
;              returns its numeric code, according to the chosen encoding.
;              If we apply it (mapcar) to the exploded dictionary entry for 'più', we
;              obtain (80 73 (195 153)); If we apply it to the exploded typed word,
;              we get (80 73 (195 131) (194 185))
;              Note that this is different from the Lisp function "char-code", 
;              which always returns the ISO-8859-1 code.
;     2 --> 1: The function "get-lisp-char-from-tule-char-name" takes a Tule name and
;              returns the corresponding Lisp char (if any), otherwise returns nil
;              If we apply it to the items in (UP-P UP-I UP-U-GRAVE), we obtain
;              (#\P #\I #\¿). 
;     2 --> 3: The function "get-numcode-from-tule-char-name" takes a Tule name and
;              returns a numeric code (according to the chosen encoding)
;     3 --> 1: The function "get-lisp-char-from-numcode" takes a numeric code and
;              returns the corresponding lisp char, according to the chosen encoding.
;              Note that this is different from the Lisp function "code-char", 
;              which always uses the ISO-8859-1 code.
;     3 --> 2: The function "get-tule-char-name-from-numcode" takes a one-, two- or
;              three-byte numerical code and returns the Tule char name
;     x --> x: The function "get-tule-char-name-from-base-code" takes a basecode and
;              returns the Tule char name. This assumes that the input data
;              is always in iso-8859-1
;     x --> x: The function "get-lisp-char-name-from-base-code" takes a basecode and
;              returns the Lisp char name. This assumes that the input data
;              is always in iso-8859-1
;     x --> x: The function "get-base-code-from-tule-char-name"
;     x --> x: The function "get-numcode-from-base-code"
; --- get-char-upperc: returns the uppercase; works also for accented chars.
;     if the input is already uppercase returns it
;        DOWN-A --> UP-A
;        DOWN-U-GRAVE --> UP-U-GRAVE
;        UP-A --> UP-A
; --- get-char-upperc-no-accent: as above, but accented chars are left unchanged.
;        DOWN-A --> UP-A
;        DOWN-U-GRAVE --> DOWN-U-GRAVE
; --- get-lisp-char-upperc: gets a lisp char code and returns the value of the
;     uppercase encoding. Works only for non-accented chars
;        #\a --> 65 (A)
;        #\A --> 65 (A)
;        #\ù --> lisp error (meaningless character name)
; --- get-char-lowerc: returns the lowercase; works also for accented chars.
;     if the input is already lowercase returns it
;        UP-A --> DOWN-A
;        UP-U-GRAVE --> DOWN-U-GRAVE
;        DOWN-A --> DOWN-A
; --- get-lisp-char-lowerc: gets a lisp char code and returns the value of the
;     lowercase encoding. Works only for non-accented chars
;        #\a --> 97 (a)
;        #\A --> 97 (a)
;        #\ù --> lisp error (meaningless character name)
; --------- CONVERSION OF ATOMS OR STRINGS ---------------------------------------
; *** In the list below, by "lisp atom" we mean what is written via keyboard
; ---  "convert-base-atom-or-string-to-currscheme": converts an atom or 
;      string in base code and converts to the current code. Useful for
;      converting dictionary entries into utf8
;           *CHAR-SET-ID* = utf-8
;        'più' dictionary entry key --> 'PIÙ' (i.e. (80 73 195 153))
;        'più  lisp atom -->  'PIÃ¹' (i.e. (80 73 195 131 194 185))
;        "più" lisp string -->  |piÃ¹| (i.e. (112 105 195 131 194 185))
;           *CHAR-SET-ID* = iso-8859-1
;         ????
; ---  "convert-atom-or-string-to-currscheme": takes as input an item that could
;      be a dictionary entry key and returns its current scheme encoding. The latter
;      is a printable (on screen) version of the former
;           *CHAR-SET-ID* = utf-8
;        'più' dictionary entry key --> 'PIÙ' (i.e. (80 73 195 153))
;        'più  lisp atom -->  'PIÃ¹' (i.e. (80 73 195 131 194 185))
;        "più" lisp string -->  |piÃ¹| (i.e. (112 105 195 131 194 185))
;           *CHAR-SET-ID* = iso-8859-1
;        'più' dictionary entry key --> 'PI ' (i.e. (80 73 217))
;        'più  lisp atom -->  'PIù' (i.e. (80 73 195 185))
;        "più" lisp string -->  |più| (i.e. (112 105 195 185))
; --- "line-to-char" 
;           *CHAR-SET-ID* = utf-8
;        'più' dictionary entry key --> ERROR 
;        'più  lisp atom --> (80 73 249 10)
;        "più" lisp string --> (112 105 249 10) 
;           *CHAR-SET-ID* = iso-8859-1
;        'più' dictionary entry key --> (80 73 217 10)
;        'più  lisp atom --> (80 73 195 185 10)
;        "più" lisp string --> (112 105 195 185 10)
; --- "line-to-tule-char"
;           *CHAR-SET-ID* = utf-8
;        'più' dictionary entry key --> ERROR
;        'più  lisp atom --> (UP-P UP-I DOWN-U-GRAVE)
;        "più" lisp string --> (UP-P UP-I DOWN-U-GRAVE)
;           *CHAR-SET-ID* = iso-8859-1
;        'più' dictionary entry key --> (UP-P UP-I UP-U-GRAVE)
;        'più  lisp atom --> (UP-P UP-I UP-A-TILDE UP-ONE)
;        "più" lisp string --> (DOWN-P DOWN-I UP-A-TILDE UP-ONE)
; --- "base-uppercase"
;        'più' dictionary entry key --> 'PI ' (i.e. (80 73 217))
;        'più  lisp atom --> PIù (i.e. (80 73 195 185))
;        "più" lisp string --> PIù (i.e. (80 73 195 185))
; --- "base-uppercase-no-accent"
;        'più' dictionary entry key --> 'PI ' (i.e. (80 73 217))
;        'più  lisp atom --> PIù (i.e. (80 73 195 185))
;        "più" lisp string --> PIù (i.e. (80 73 195 185))
; --- "base-lowercase"
;        'più' dictionary entry key --> 'pi ' (i.e. (112 105 249))
;        'più  lisp atom --> 'pi  ' (i.e. (112 105 227 185))
;        "più" lisp string --> 'pi  ' (i.e. (112 105 227 185))
;        
; --------- CONVERSION OF NUMBER (CODE) LISTS ------------------------------------
;     x --> x: "convert-base-codes-to-utf8-codes":
;              (80 73 217) --> (80 73 195 153)
;     x --> x: convert-tule-char-names-to-numcode
;              (UP-P UP-I UP-U-GRAVE) --> (80 73 217) [if *CHAR-SET-ID* = iso-8859-1]
;              (UP-P UP-I UP-U-GRAVE) --> (80 73 195 153) [if *CHAR-SET-ID* = utf-8]
;     x --> x: convert-tule-char-names-to-base-codes
;              (UP-P UP-I UP-U-GRAVE) --> (80 73 217) [independently of *CHAR-SET-ID*]
;     x --> x: convert-numcodes-to-tule-char-names
;              (80 73 217) --> (UP-P UP-I UP-U-GRAVE) [if *CHAR-SET-ID* = iso-8859-1]
;              (80 73 217) --> ERROR [if *CHAR-SET-ID* = utf-8]
;              (80 73 195 153) --> (UP-P UP-I UP-U-GRAVE) [if *CHAR-SET-ID* = utf-8]
;              (80 73 195 153) --> (UP-P UP-I UP-A-TILDE INTERNAL-Y) [if *CHAR-SET-ID* =
;                   iso-8859-1]
;     x --> x: convert-base-codes-to-tule-char-names
;              (80 73 217) --> (UP-P UP-I UP-U-GRAVE) 
;              (80 73 195 153) --> (UP-P UP-I UP-A-TILDE INTERNAL-Y)
; *** The char-value property is also used to keep the set of values
;     associated with some names of sets of characters used in the tokenization
;     and in some other parts of the parser (as "minlet" or "neutral")

; *********************************************************************
; *************** INITIALIZATION **************************************
; *********************************************************************

; *** the next function loads the table that holds all infos concerning the
;     various encodings of the characters. The data are listed according to
;     the alphabetical order of the tule character names
; *** at the end of the definition, various tables are reported as comments
;     to facilitate the inspection of the various encodings
; *** the third column (Lisp char name) is not actually used, since the
;     real name is obtained via the code-char function
(defun set-symbol-codes ()
 (declare (special *SYMBOL-CODE-TABLE* *CHAR-SET-ID* *LISP-CHAR-SET-ID* 
                   *NON-DICTIONARY-CHARS*))
 (setq *SYMBOL-CODE-TABLE*
 '(
; *** (tule-name 	unicode lisp iso-8859-1 utf-8)
   (acute-accent	#x00B4  #\´ 	180 	(194 180))
   (ampersand		#x0026  #\& 	38 	38)
   (apostrophe		#x0027  #\' 	39 	39)
   (ascii-mu		#x00B5  #\µ 	181 	(194 181))
   (asterisk		#x002A  #\* 	42 	42)
   (backslash		#x002F  #\\ 	92 	92)
   (cedilla		#x00B8  #\¸ 	184 	(194 184))
   (cent		#x00A2  #\¢ 	162 	(194 162))
   (circumflex		#x005E  #\^ 	94 	94)
   (closed-curly-par	#x007D  #\} 	125 	125)
   (closed-double-angle	#x00BB  #\» 	187 	(194 187))
   (closed-par		#x0029  #\) 	41 	41)
   (closed-square-par	#x005D  #\] 	93 	93)
   (colon		#x003A 	#\: 	58 	58)
   (comma		#x002C 	#\, 	44 	44)
   (commerc-at		#x0040  #\@ 	64 	64)
   (copyright		#x00A9  #\©     169 	(194 169))
   (cubed		#x00B3  #\³ 	179 	(194 179))
   (diaeresis		#x00A8  #\¨     168 	(194 168))
   (dollar		#x0024  #\$ 	36 	36)
   (down-a 		#x0061 	#\a 	97 	97)
   (down-a-acute 	#x00E1 	#\á 	225 	(195 161))
   (down-a-circum 	#x00E2 	#\â 	226 	(195 162))
   (down-a-diaer 	#x00E4 	#\ä 	228 	(195 164))
   (down-a-grave 	#x00E0 	#\à 	224 	(195 160))
   (down-a-ring 	#x00E5 	#\å 	229 	(195 165))
   (down-a-tilde 	#x00E3 	#\ã 	227 	(195 163))
   (down-ae 		#x00E6 	#\æ 	230 	(195 166))    ; icelandic
   (down-b 		#x0062 	#\b 	98 	98) 
   (down-c 		#x0063 	#\c 	99 	99) 
   (down-c-cedilla 	#x00E7 	#\ç 	231 	(195 167))
   (down-d 		#x0064 	#\d 	100 	100) 
   (down-e 		#x0065 	#\e 	101 	101)
   (down-e-acute 	#x00E9 	#\é 	233 	(195 169))
   (down-e-circum 	#x00EA 	#\ê 	234 	(195 170))
   (down-e-diaer 	#x00EB 	#\ë 	235 	(195 171))
   (down-e-grave 	#x00E8 	#\è 	232 	(195 168))
   (down-eth 		#x00F0 	#\ð 	240 	(195 176))    ; icelandic
   (down-f 		#x0066 	#\f 	102 	102)
   (down-g 		#x0067 	#\g 	103 	103)
   (down-h 		#x0068 	#\h 	104 	104)
   (down-i 		#x0069 	#\i 	105 	105)
   (down-i-acute 	#x00ED 	#\í 	237 	(195 173))
   (down-i-circum 	#x00EE 	#\î 	238 	(195 174))
   (down-i-diaer 	#x00EF 	#\ï 	239 	(195 175))
   (down-i-grave 	#x00EC 	#\ì 	236 	(195 172))
   (down-j 		#x006A 	#\j 	106 	106)
   (down-k 		#x006B 	#\k 	107 	107)
   (down-l 		#x006C 	#\l 	108 	108)
   (down-m 		#x006D 	#\m 	109 	109)
   (down-n 		#x006E 	#\n 	110 	110)
   (down-n-tilde 	#x00F1 	#\ñ 	241 	(195 177))
   (down-o 		#x006F 	#\o 	111 	111)
   (down-o-acute 	#x00F3 	#\ó 	243 	(195 179))
   (down-o-circum 	#x00F4 	#\ô 	244 	(195 180))
   (down-o-diaer 	#x00F6 	#\ö 	246 	(195 182))
   (down-o-grave 	#x00F2 	#\ò 	242 	(195 178))
   (down-o-stroke 	#x00F8 	#\ø 	248 	(195 184))
   (down-o-tilde 	#x00F5 	#\õ 	245 	(195 181))
   (down-p 		#x0070 	#\p 	112 	112)
   (down-q 		#x0071 	#\q 	113 	113)
   (down-r 		#x0072 	#\r 	114 	114)
   (down-s 		#x0073 	#\s 	115 	115)
   (down-t 		#x0074 	#\t 	116 	116)
   (down-thorn 		#x00FE 	#\þ 	254 	(195 190))    ; icelandic
   (down-u 		#x0075 	#\u 	117 	117)
   (down-u-acute 	#x00FA 	#\ú 	250 	(195 186))
   (down-u-circum 	#x00FB 	#\û 	251 	(195 187))
   (down-u-diaer 	#x00FC 	#\ü 	252 	(195 188))
   (down-u-grave 	#x00F9 	#\ù 	249 	(195 185))
   (down-v 		#x0076 	#\v 	118 	118)
   (down-w 		#x0077 	#\w 	119 	119)
   (down-x 		#x0078 	#\x 	120 	120)
   (down-y 		#x0079 	#\y 	121 	121)
   (down-y-acute 	#x00FD 	#\ý 	253 	(195 189))
   (down-y-diaer 	#x00FF 	#\ÿ 	255 	(195 191))
   (down-z 		#x007A 	#\z 	122 	122)
   (eight 		#x0038 	#\8 	56 	56)
   (end-of-paragraph	#x00B6  #\¶ 	182 	(194 182))
   (equal		#x003D  #\= 	61 	61)
   (escape 		#x001B 	#\Escape 27 	27)
   (exclamation-mark	#x0021 	#\! 	33 	33)
   (five 		#x0035 	#\5 	53 	53)
   (four 		#x0034 	#\4 	52 	52)
   (fraction		#x00F7  #\÷ 	247 	(195 183))
   (up-french-oe	#x0152  #\Œ 	338	(197 146))
   (down-french-oe	#x0153  #\œ 	339	(197 147))
   (grave-accent	#x0060  #\` 	96 	96)
   (greater-than	#x003E  #\> 	62 	62)
   (horiz-tab 		#x0009 	#\Tab 	9	 9)
   (hyphen		#x002D  #\- 	45 	45)
   (hyphen-short	#x00AD  #\­ 	173 	(194 173))
   (internal-null	#x0080  nil     128 	(194 128))
   (internal-a		#x0081  nil     129 	(194 129))
   (internal-b		#x0082  nil     130 	(194 130))
   (internal-c		#x0083  nil     131 	(194 131))
   (internal-d		#x0084  nil     132 	(194 132))
   (internal-e		#x0085  nil     133 	(194 133))
   (internal-f		#x0086  nil     134 	(194 134))
   (internal-bell	#x0087  nil   	135 	(194 135))
   (internal-backsp	#x0088  nil  	136 (194 136))
   (internal-tab	#x0089  nil     137 	(194 137))
   (internal-newline	#x008A  nil   	138 	(194 138))
   (internal-vert-tab	#x008B  nil     139 	(194 139))
   (internal-page	#x008C  nil     140 	(194 140))
   (internal-return	#x008D  nil   	141 	(194 141))
   (internal-n		#x008E  nil     142 	(194 142))
   (internal-o		#x008F  nil     143 	(194 143))
   (internal-p		#x0090  nil     144 	(194 144))
   (internal-q		#x0091  nil     145 	(194 145))
   (internal-r		#x0092  nil     146 	(194 146))
   (internal-s		#x0093  nil     147 	(194 147))
   (internal-t		#x0094  nil     148 	(194 148))
   (internal-u		#x0095  nil     149 	(194 149))
   (internal-v		#x0096  nil     150 	(194 150))
   (internal-w		#x0097  nil     151 	(194 151))
   (internal-x		#x0098  nil     152 	(194 152))
   (internal-y		#x0099  nil     153 	(194 153))
   (internal-z		#x009A  nil     154 	(194 154))
   (internal-escape	#x009B  nil     155 	(194 155))
   (internal-backslash	#x009C  nil     156 	(194 156))
   (internal-cl-sq-par	#x009D  nil     157 	(194 157))
   (internal-circum	#x009E  nil     158 	(194 158))
   (internal-undersc	#x009F  nil     159 	(194 159))
   (internal-space	#x00A0  nil 	160 	(194 160))
   (inv-exclam-mark	#x00A1  #\¡     161 	(194 161))
   (inv-question-mark	#x00BF 	#\¿ 	191 	(194 191))
   (light		#x00A4  #\¤     164 	(194 164))
   (less-than		#x003C  #\< 	60 	60)
   (logical-not		#x00AC  #\¬     172 	(194 172))
   (mid-dot		#x00B7  #\· 	183 	(194 183)) 
   (newline 		#x000A 	#\Newline 10 	10)
   (nine 		#x0039 	#\9 	57 	57)
   (nth			#x00B0  #\° 	176 	(194 176))
   (nth-a-underscore	#x00AA  #\ª 	170 	(194 170))
   (nth-underscore	#x00BA  #\º 	186 	(194 186))
   (number		#x0023  #\# 	35 	35)
   (one 		#x0031 	#\1 	49 	49)
   (one-fourth		#x00BC  #\¼ 	188 	(194 188))
   (one-half		#x00BD  #\½ 	189 	(194 189))
   (open-curly-par	#x007B  #\{ 	123 	123)
   (open-double-angle	#x00AB  #\« 	171 	(194 171))
   (open-par		#x0028 	#\( 	40 	40)
   (open-square-par	#x005B  #\[ 	91 	91)
   (paragraph		#x00A7  #\§ 	167 	(194 167))
   (percent		#x0025  #\% 	37 	37)
   (period		#x002E 	#\. 	46 	46)
   (pipe		#x00A6  #\¦     166 	(194 166))
   (plus		#x002B  #\+ 	43 	43)
   (plus-minus		#x00B1  #\± 	177 	(194 177))
   (pound		#x00A3  #\£ 	163 	(194 163))
   (product		#x00D7  #\× 	215 	(195 151))
   (question-mark	#x003F 	#\? 	63 	63)
   (quotation-m		#x0022  #\" 	34 	34)		; "
   (return 		#x000D 	#\Return 13 	13)
   (semicolon		#x003B 	#\; 	59 	59)
   (seven 		#x0037 	#\7 	55 	55)
   (sharp-s 		#x00DF 	#\ß 	223 	(195 159))
   (six 		#x0036 	#\6 	54 	54)
   (slash		#x002F  #\/ 	47 	47)
   (space 		#x0020 	#\Space 32 	32)
   (squared		#x00B2  #\² 	178 	(194 178))
   (start-utf-8		#xFEFF  NIL 	NIL 	(239 187 191))
   (three 		#x0033 	#\3 	51 	51)
   (three-fourths	#x00BE  #\¾ 	190 	(194 190))
   (tilde		#x007E  #\~ 	126 	126)
   (trademark		#x00AE  #\® 	174 	(194 174))
   (two 		#x0032 	#\2 	50 	50)
   (underscore		#x005F  #\_ 	95 	95)
   (up-a 		#x0041 	#\A 	65 	65)
   (up-a-acute		#x00C1 	#\Á 	193 	(195 129))
   (up-a-circum		#x00C2 	#\Â 	194 	(195 130))
   (up-a-diaer		#x00C4 	#\Ä 	196 	(195 132))
   (up-a-grave		#x00C0 	#\À 	192 	(195 128))
   (up-a-ring		#x00C5 	#\Å 	197 	(195 133))
   (up-a-tilde		#x00C3 	#\Ã 	195 	(195 131))
   (up-ae 		#x00C6 	#\Æ 	198 	(195 134))	; icelandic
   (up-b 		#x0042 	#\B 	66 	66)
   (up-c 		#x0043 	#\C 	67 	67)
   (up-c-cedilla	#x00C7 	#\Ç 	199 	(195 135))
   (up-d 		#x0044 	#\D 	68 	68)
   (up-e 		#x0045 	#\E 	69 	69)
   (up-e-acute		#x00C9 	#\É 	201 	(195 137))
   (up-e-circum		#x00CA 	#\Ê 	202 	(195 138))
   (up-e-diaer		#x00CB 	#\Ë 	203 	(195 139))
   (up-e-grave		#x00C8 	#\È 	200 	(195 136))
   (up-eth 		#x00D0 	#\Ð 	208 	(195 144))	; icelandic
   (up-f 		#x0046 	#\F 	70 	70)
   (up-g 		#x0047 	#\G 	71 	71)
   (up-h 		#x0048 	#\H 	72 	72)
   (up-i 		#x0049 	#\I 	73 	73)
   (up-i-acute		#x00CD 	#\Í 	205 	(195 141))
   (up-i-circum		#x00CE 	#\Î 	206 	(195 142))
   (up-i-diaer		#x00CF 	#\Ï 	207 	(195 143))
   (up-i-grave		#x00CC 	#\Ì 	204 	(195 140))
   (up-j 		#x004A 	#\J 	74 	74)
   (up-k 		#x004B 	#\K 	75 	75)
   (up-l 		#x004C 	#\L 	76 	76)
   (up-m 		#x004D 	#\M 	77 	77)
   (up-n 		#x004E 	#\N 	78 	78)
   (up-n-tilde		#x00D1 	#\Ñ 	209 	(195 145))
   (up-o 		#x004F 	#\O 	79 	79)
   (up-o-acute		#x00D3 	#\Ó 	211 	(195 147))
   (up-o-circum		#x00D4 	#\Ô 	212 	(195 148))
   (up-o-diaer		#x00D6 	#\Ö 	214 	(195 150))
   (up-o-grave		#x00D2 	#\Ò 	210 	(195 146))
   (up-o-stroke		#x00D8 	#\Ø 	216 	(195 152))
   (up-o-tilde		#x00D5 	#\Õ 	213 	(195 149))
   (up-one		#x00B9  #\¹ 	185 	(194 185))
   (up-p 		#x0050 	#\P 	80 	80)
   (up-q 		#x0051 	#\Q 	81 	81)
   (up-r 		#x0052 	#\R 	82 	82)
   (up-s 		#x0053 	#\S 	83 	83)
   (up-t 		#x0054 	#\T 	84 	84)
   (up-thorn 		#x00DE 	#\Þ 	222 	(195 158))	; icelandic
   (up-u 		#x0055 	#\U 	85 	85)
   (up-u-acute		#x00DA 	#\Ú 	218 	(195 154))
   (up-u-circum		#x00DB 	#\Û 	219 	(195 155))
   (up-u-diaer		#x00DC 	#\Ü 	220 	(195 156))
   (up-u-grave		#x00D9 	#\Ù 	217 	(195 153))
   (up-v 		#x0056 	#\V 	86 	86)
   (up-w 		#x0057 	#\W 	87 	87)
   (up-x 		#x0058 	#\X 	88 	88)
   (up-y 		#x0059 	#\Y 	89 	89)
   (up-y-acute		#x00DD 	#\Ý 	221 	(195 157))
   (up-z 		#x005A 	#\Z 	90 	90)
   (upperscore		#x00AF  #\¯ 	175 	(194 175))
   (vertical-bar	#x007C  #\| 	124 	124)
   (yen			#x00A5  #\¥     165 	(194 165))
   (zero		#x0030 	#\0 	48 	48)
   (down-alpha          #x03B1  #\α     945     (206 177))
   (down-alpha-acute    #x03AC  #\ά     nil     (206 172))
   (down-beta           #x03B2  #\β     nil     (206 178))
   (down-beta-curled    #x03D0  #\ϐ 	nil 	(207 144))
   (down-chi            #x03C7  #\χ     nil     (207 135))
   (down-delta          #x03B4  #\δ     nil     (206 180))
   (down-epsilon        #x03B5  #\ε     nil     (206 181))
   (down-epsilon-acute  #x03AD  #\έ     nil     (206 173))
   (down-epsilon-lunate #x03F5  #\ϵ 	nil 	(207 181))
   (down-eta            #x03B7  #\η     nil     (206 183))
   (down-eta-acute      #x03AE  #\ή     nil     (206 174))
   (down-gamma          #x03B3  #\γ     nil     (206 179))
   (down-iota           #x03B9  #\ι     nil     (206 185))
   (down-iota-acute     #x03AF  #\ί     nil     (206 175))
   (down-iota-diaer 	#x03CA  #\ϊ     nil     (207 138))
   (down-iota-mixed-stress #x0390 #\ΐ   nil     (206 144))
   (down-kappa          #x03BA  #\κ     nil     (206 186))
   (down-lambda         #x03BB  #\λ     nil     (206 187))
   (down-mu             #x03BC  #\μ     nil     (206 188))
   (down-nu             #x03BD  #\ν     nil     (206 189))
   (down-omega          #x03C9  #\ω     nil     (207 137))
   (down-omega-acute    #x03CE  #\ώ     nil     (207 142))
   (down-omicron        #x03BF  #\ο     nil     (206 191))
   (down-omicron-acute  #x03CC  #\ό     nil     (207 140))
   (down-phi            #x03C6  #\φ     nil     (207 134))
   (down-pi             #x03C0  #\π     nil     (207 128))
   (down-psi            #x03C8  #\ψ     nil     (207 136))
   (down-rho            #x03C1  #\ρ     nil     (207 129))
   (down-rho-curled 	#x03F1  #\ϱ 	nil 	(207 177))
   (down-sigma1         #x03C2  #\ς     nil     (207 130))
   (down-sigma2         #x03C3  #\σ     nil     (207 131))
   (down-tau            #x03C4  #\τ     nil     (207 132))
   (down-theta          #x03B8  #\θ     nil     (206 184))
   (down-upsilon        #x03C5  #\υ     nil     (207 133))
   (down-upsilon-acute  #x03CD  #\ύ     nil     (207 141))
   (down-upsilon-diaer 	#x03CB  #\ϋ     nil     (207 139))
   (down-upsilon-mixed-stress #x03B0 #\ΰ nil 	(206 176))
   (down-xi             #x03BE  #\ξ     nil     (206 190))
   (down-zeta           #x03B6  #\ζ     nil     (206 182))
   (up-alpha            #x0391  #\Α     nil     (206 145))
   (up-alpha-acute      #x0386  #\Ά     nil     (206 134))
   (up-beta             #x0392  #\Β     nil     (206 146))
   (up-chi              #x03A7  #\Χ     nil     (206 167))
   (up-delta            #x0394  #\Δ     nil     (206 148))
   (up-epsilon          #x0395  #\Ε     nil     (206 149))
   (up-epsilon-acute    #x0388  nil     nil     (206 136))
   (up-eta              #x0397  nil     nil     (206 151))
   (up-eta-acute        #x0389  nil     nil     (206 137))
   (up-gamma            #x0393  nil     nil     (206 147))
   (up-iota             #x0399  nil     nil     (206 153))
   (up-iota-acute       #x038A  nil     nil     (206 138))
   (up-iota-diaer   	#x03AA  nil     nil     (206 170))
   (up-kappa            #x039A  nil     nil     (206 154))
   (up-lambda           #x039B  nil     nil     (206 155))
   (up-mu               #x039C  nil     nil     (206 156))
   (up-nu               #x039D  nil     nil     (206 157))
   (up-omega            #x03A9  nil     nil     (206 169))
   (up-omega-acute      #x038F  nil     nil     (206 143))
   (up-omicron          #x039F  nil     nil     (206 159))
   (up-omicron-acute    #x038C  nil     nil     (206 140))
   (up-phi              #x03A6  nil     nil     (206 166))
   (up-phi-curled       #x03D5  nil     nil     (207 149))
   (up-pi               #x03A0  nil     nil     (206 160))
   (up-psi              #x03A8  nil     nil     (206 168))
   (up-rho              #x03A1  nil     nil     (206 161))
   (up-sigma            #x03A3  nil     nil     (206 163))
   (up-tau              #x03A4  nil     nil     (206 164))
   (up-theta            #x0398  nil     nil     (206 152))
   (up-upsilon          #x03A5  nil     nil     (206 165))
   (up-upsilon-acute    #x038E  nil     nil     (206 142))
   (up-upsilon-curled   #x03D2  nil     nil     (207 146))
   (up-upsilon-diaer 	#x03AB  nil     nil     (206 171))
   (up-xi               #x039E  nil     nil     (206 158))
   (up-zeta             #x0396  nil     nil     (206 150))
   (greek-apostrophe 	#x0384  nil 	nil 	(206 132))
   (greek-mixed-stress 	#x0385  nil 	nil 	(206 133))
   (greek-mid-dot 	#x0387  nil 	nil 	(206 135))
   (jap-yaman-1 	#x5C71  #\山 	23665 	(229 177 177))	; 山
   (jap-yaman-2 	#x624B  #\手 	25163 	(230 137 139))  ; 手
   (jap-yaman-3 	#x7DDA  #\線 	32218 	(231 183 154))  ; 線
   (jap-yaman-4 	#x8C4A  #\豊 	35914 	(232 177 138))
   (jap-yaman-5 	#x5CF6  #\島 	23798 	(229 179 182))
   (jap-down-o-macron 	#x014D  #\ō 	333 	(197 141))
   (jap-down-u-macron 	#x016B  #\ū 	363 	(197 171))
   (jap-up-o-macron 	#x014C  #\Ō 	332 	(197 140))
   (jap-up-u-macron 	#x016A  #\Ū 	362 	(197 170))
   (jap-hiragana-no 	#x306E  #\の 	12398 	(227 129 174))
   (jap-katakana-no 	#x30CE  #\ノ 	12494 	(227 131 142))
   (chi-wá		#x5A03  #\娃    23043   (229 168 131))
   (en-dash	 	#x2013  #\–	8211	(226 128 147))
   (fr-dash	 	#x2014  #\—	8212	(226 128 148))
          ))
 ; *** for each tule character name, sets the properties "lisp-char" (the lisp -------
 ;     character) and "char-value" (the numerical encoding) --------------------------
 (let (inp-char-funct lisp-char-funct)
  (putprop 'ISO-8859-1 nil 'leading-id-chars)
  (putprop 'UTF-8 '(down-i-diaer closed-double-angle inv-question-mark)
                  'leading-id-chars)		; "ï»¿"
  (cond ((eq *CHAR-SET-ID* 'ISO-8859-1)
          (setq inp-char-funct #'fourth))
        ((eq *CHAR-SET-ID* 'UTF-8)
          (setq inp-char-funct #'fifth))
        (t (exception-nothrow "char-funct: Unknown value for *CHAR-SET-ID*")))
  (cond ((eq *LISP-CHAR-SET-ID* 'ISO-8859-1)
          (setq lisp-char-funct #'fourth))
        ((eq *LISP-CHAR-SET-ID* 'UTF-8)
          (setq lisp-char-funct #'fifth))
        (t (exception-nothrow "char-funct: Unknown value for *LISP-CHAR-SET-ID*")))
  (dolist (char-def *SYMBOL-CODE-TABLE*)
         (putprop (first char-def) 		; the lisp name of the character
                  (code-char (second char-def))
                  'lisp-char)
         (putprop (first char-def) 		; the encoding in the current CHAR-SET-ID
                  (apply inp-char-funct (list char-def))
                  'char-value)
         (putprop (first char-def) 		; the encoding in the base scheme (= unicode)
                  (second char-def)
                 'char-base-value)
         (putprop (first char-def) 
                  (apply lisp-char-funct (list char-def))
                 'char-lisp-value))
   ; *** the next builds the table *BASE-CODE-TABLE*, that encodes the mapping between
   ;     ISO-8859-1 encoding (which is the one used in the dictionaries) and the chosen
   ;     encoding scheme
  (set-base-code-table)		; -----------------------------------------------------
   ; *** the next builds the table *CODE-TO-TULE-TABLE*, that encodes the mapping between
   ;     numerical encodings and tule character names.
  (set-code-to-tule-table)	; -----------------------------------------------------
   ; *** the next builds the table *CURRLISP-TO-TULE-TABLE*, that encodes the mapping between
   ;     the lisp numerical encodings and tule character names.
  (set-currlisp-to-tule-table)	; -----------------------------------------------------
   ; *** the next builds the table *LISP-TO-TULE-TABLE*, that encodes the mapping between
   ;     Lisp chars and tule character names.
  (set-lisp-to-tule-table)	; -----------------------------------------------------
   ; *** the next sets the properties "no-stress", "acute", "circumflex", "grave", ...
   ;     that define the mapping between stressed chars and their unstressed correspondent
  (set-accent-map)
   ; *** the next is a list of ISO-88259-1 numerical codes that, in the dictionaries,
   ;     are associated with the first byte of entries whose name must be kept
   ;     unchanged. This is currently used just for greek and japanese characters. 
  (setq *NON-DICTIONARY-CHARS* '(197 206 207 227 228 229 230 231 232))
    ))

; ****************************************************************
; *** loads the table *BASE-CODE-TABLE*, which holds the correspondence
;     between unicode encoding, the one chosen for use, and the Tule char names
(defun set-base-code-table ()
 (declare (special *SYMBOL-CODE-TABLE* *BASE-CODE-TABLE*))
   (let (templist)
     (dolist (nxtinfo *SYMBOL-CODE-TABLE*)
         (cond ((not (null (fourth nxtinfo)))
                  (setq templist 
                    (cons (list (fourth nxtinfo) (fifth nxtinfo) (first nxtinfo))
                          templist)))
  ; *** the value of an hexadecimal representation is the number (eval '#x03A3) = 931
               (t (setq templist 
                    (cons (list (second nxtinfo) (fifth nxtinfo) (first nxtinfo))
                          templist)))))
     (setq *BASE-CODE-TABLE* templist)))

; *******************************************************************
; *** this builds the table *CODE-TO-TULE-TABLE*, that encodes the mapping between
;     numerical encodings and tule character names.
; *** This function must be applied after "set-char-values"
; *** The format of the table is
;     (code11 val11 code12 val12 ... code1N val1N)
;     If code1k is a one-byte code, then val1k is the tule name (an atom)
;     If code1k is the first byte of a two-bytes code, then val1k
;        a list of possible second bytes in the form
;        (code2k1 val2k1 code2k2 val2k2 ....)
;        where code2kj are the possible second bytes and val2kj is the tule name
;              of the character whose code is <code1k cde2kj>
(defun set-code-to-tule-table ()
 (declare (special *SYMBOL-CODE-TABLE* *CODE-TO-TULE-TABLE*))
  (let (byte-table temptab)
   (dolist (nxtinfo *SYMBOL-CODE-TABLE*)
      (let* ((nxtname (first nxtinfo))
             (numcode (get nxtname 'char-value)))
          (cond ((null numcode) nil)
    ; *** no exception if a tule char has no code: it depends on the defined char-set
                ((atom numcode)
                  (setq temptab (cons (list numcode nxtname) temptab)))
                (t (setq byte-table
                        (add-to-byte-table byte-table numcode nxtname))))))
    ; *** if byte-table is non-empty, then the used encoding includes two-byte codes and
    ;     they must be added to *CODE-TO-TULE-TABLE*
  (setq *CODE-TO-TULE-TABLE*
        (sort (append temptab byte-table) #'(lambda (x y) (< (first x) (first y)))))))

; *******************************************************************
; The same as above, but for the current lisp encoding
(defun set-currlisp-to-tule-table ()
 (declare (special *SYMBOL-CODE-TABLE* *CURRLISP-TO-TULE-TABLE*))
  (let (byte-table temptab)
   (dolist (nxtinfo *SYMBOL-CODE-TABLE*)
      (let* ((nxtname (first nxtinfo))
             (numcode (get nxtname 'char-lisp-value)))
          (cond ((null numcode) nil)
    ; *** no exception if a tule char has no code: it depends on the defined char-set
                ((atom numcode)
                  (setq temptab (cons (list numcode nxtname) temptab)))
                (t (setq byte-table
                        (add-to-byte-table byte-table numcode nxtname))))))
    ; *** if byte-table is non-empty, then the used encoding includes two-byte codes and
    ;     they must be added to *CURRLISP-TO-TULE-TABLE*
  (setq *CURRLISP-TO-TULE-TABLE*
        (sort (append temptab byte-table) #'(lambda (x y) (< (first x) (first y)))))))

; *******************************************************************
; *** extends the content of the table for two-byte or three-byte encodings
(defun add-to-byte-table (byte-table bytes charname)
  (let ((byte1 (first bytes))
        (byte2 (second bytes))
        (byte3 (third bytes)))
  (cond ((null byte-table)             
            ; this is the first occurence of that first byte
          (cond ((null byte3)
            ; two bytes: builds ((byte1 ((byte2 charname))))
                   (list (list byte1 (list (list byte2 charname)))))
            ; three bytes: builds ((byte1 ((byte2 ((byte3 charname))))))
                (t (list (list byte1 (list (list byte2 (list (list byte3 charname)))))))))
        ((eq byte1 (first (first byte-table)))
            ; the entry for byte1 has been found. It has the form
            ;    (byte1 ((byte21 x1) (byte22 x2) ...))
            ; where xi can be 
            ;    charx                                    (two-byte encoding) or
            ;    ((byte3x charx) (byte3y chary) ...)      (three-byte encoding)
           (cons (list byte1
                      (add-to-byte-t-2nd (second (first byte-table)) charname byte2 byte3))
                 (rest byte-table)))
        (t (cons (first byte-table)
                 (add-to-byte-table (rest byte-table) bytes charname))))))

; ****************************************************************
(defun add-to-byte-t-2nd (byte1table charname byte2 byte3)
  (cond ((null byte1table)
            ; this is the first occurrence of that second byte after that first byte
           (cond ((null byte3)
                    (list (list byte2 charname)))
                 (t (list (list byte2 (list (list byte3 charname)))))))
        ((eq byte2 (first (first byte1table)))
            ; the entry for byte2 has been found. If it is a two-byte encoding,
            ; this is an error, since there are two chars with the same code
           (cond ((null byte3)
                    (exception-nothrow "duplicate chars in MORPHO/char-funct"))
            ; otherwise, add to table at third level
                 (t (cons (list byte2
                            (add-to-byte-t-3rd (second (first byte1table)) charname byte3))
                          (rest byte1table)))))
        (t (cons (first byte1table)
                 (add-to-byte-t-2nd (rest byte1table) charname byte2 byte3)))))

; ****************************************************************
(defun add-to-byte-t-3rd (byte2table charname byte3)
  (cond ((null byte2table)
            ; this is the first occurrence of that third byte after that second byte
           (list (list byte3 charname)))
        ((eq byte3 (first (first byte2table)))
            ; the entry for byte2 has been found. 
            ; this is an error, since there are two chars with the same code
           (exception-nothrow "duplicate chars in MORPHO/char-funct"))
        (t (cons (first byte2table)
                 (add-to-byte-t-3rd (rest byte2table) charname byte3)))))

; ****************************************************************
; *** loads the table *LISP-TO-TULE-TABLE*, which holds the correspondence
;     between lisp characters and tule character names
(defun set-lisp-to-tule-table ()
 (declare (special *SYMBOL-CODE-TABLE* *LISP-TO-TULE-TABLE*))
   (let (templist)
     (dolist (nxtinfo *SYMBOL-CODE-TABLE*)
         (cond ((not (null (third nxtinfo)))
                  (setq templist (cons (list (third nxtinfo) (first nxtinfo))
                                       templist)))))
     (setq *LISP-TO-TULE-TABLE* templist)))

; ****************************************************************
; *** puts in the property "char-value" of all labels used in the tokenize
;     automaton the list of Tule char names covered by that label
(defun set-char-set-values ()
  (let ((setnames-to-tulenames
         '((neutral (horiz-tab newline return space))
           (minlet (down-a down-a-acute down-a-circum down-a-diaer down-a-grave 
                    down-a-ring down-a-tilde down-ae down-b down-c down-c-cedilla
                    down-d down-e down-e-acute down-e-circum down-e-diaer down-e-grave
                    down-eth down-f down-g down-h down-i down-i-acute down-i-circum 
                    down-i-diaer down-i-grave down-j down-k down-l down-m down-n 
                    down-n-tilde down-o down-o-acute down-o-circum down-o-diaer 
                    down-o-grave down-o-stroke down-o-tilde down-p down-q down-r 
                    down-s down-t down-thorn down-u down-u-acute down-u-circum 
                    down-u-diaer down-u-grave down-v down-w down-x down-y down-y-acute
                    down-y-diaer down-z sharp-s down-french-oe
                ; greek
                    down-alpha down-alpha-acute down-beta down-beta-curled down-chi 
                    down-delta down-epsilon down-epsilon-acute down-epsilon-lunate 
                    down-eta down-eta-acute down-gamma down-iota down-iota-acute 
                    down-iota-diaer down-iota-mixed-stress down-kappa down-lambda 
                    down-mu down-nu down-omega down-omega-acute down-omicron
                    down-omicron-acute down-phi down-pi down-psi down-rho down-rho-curled
                    down-sigma1 down-sigma2 down-tau down-theta down-upsilon
                    down-upsilon-acute down-upsilon-diaer down-upsilon-mixed-stress 
                    down-xi down-zeta
                ; japanese
                    jap-down-u-macron jap-down-o-macron
                ; Used in siglas for 'micro'
                    ascii-mu))
           (caplet (up-a up-a-acute up-a-circum up-a-diaer up-a-grave up-a-ring
                    up-a-tilde up-ae up-b up-c up-c-cedilla up-d up-e up-e-acute
                    up-e-circum up-e-diaer up-e-grave up-eth up-f up-g up-h up-i 
                    up-i-acute up-i-circum up-i-diaer up-i-grave up-j up-k up-l up-m 
                    up-n up-n-tilde up-o up-o-acute up-o-circum up-o-diaer up-o-grave 
                    up-o-stroke up-p up-q up-r up-s up-t up-thorn up-u up-u-acute 
                    up-u-circum up-u-diaer up-u-grave up-v up-w up-x up-y up-z 
                    up-french-oe
                ; greek
                    up-alpha up-alpha-acute up-beta up-chi up-delta up-epsilon 
                    up-epsilon-acute up-eta up-eta-acute up-gamma up-iota up-iota-acute
                    up-iota-diaer up-kappa up-lambda up-mu up-nu 
                    up-omega up-omega-acute up-omicron up-omicron-acute up-phi
                    up-phi-curled up-pi up-psi up-rho up-sigma up-tau up-theta 
                    up-upsilon up-upsilon-acute up-upsilon-curled up-upsilon-diaer 
                    up-xi up-zeta
                ; japanese (I do not think ideograms have uppercase and lowercase)
                    jap-up-u-macron jap-up-o-macron
                    jap-yaman-1 jap-yaman-2 jap-yaman-3 jap-yaman-4 jap-yaman-5
                    jap-hiragana-no jap-katakana-no))
           (digit (zero one two three four five six seven eight nine))
           (zero-to-two (zero one two))
           (zero-to-three (zero one two three))
           (zero-to-five (zero one two three four five))
           (one-two (one two))
           (one-to-nine (one two three four five six seven eight nine))
           (two-to-nine (two three four five six seven eight nine))
           (four-to-nine (four five six seven eight nine))
           (six-to-nine (six seven eight nine))
           (s-termin (exclamation-mark period colon semicolon question-mark))
           (s-term-not-period (exclamation-mark colon semicolon question-mark))
           (w-termin (quotation-m open-par closed-par comma hyphen less-than greater-than
                     open-square-par closed-square-par open-double-angle closed-double-angle
                     en-dash fr-dash))
           (inter-not-s-termin (quotation-m open-par closed-par comma hyphen
                      open-square-par closed-square-par))
           (sig-char (ampersand hyphen period underscore))
           (sig-char-not-period (ampersand hyphen underscore))
           (special (number dollar asterisk plus equal backslash circumflex
                     open-curly-par closed-curly-par vertical-bar tilde))
           (di-em-er-es-v (down-d down-m down-r down-s down-v))
                        ; d used for I'd ; m used for I'm; r used for you're;
                        ; s used for genitives and for it's; v used for I've;
           (vowel-ita (down-a down-e down-i down-o down-u up-a up-e up-i up-o up-u
                   down-a-grave down-e-grave down-i-grave down-o-grave down-u-grave
                   up-a-grave up-e-grave up-i-grave up-o-grave up-u-grave
                   down-e-acute up-e-acute))	; used in top-level-fun:test-incompl
                        ; to assign the category "foreign word" (Evalita2009)
                    ))) 		; end of "let"
       (dolist (nxtname setnames-to-tulenames)
            (putprop (first nxtname) 
                     (second nxtname)
                     'charset-value))))

;*****************************************************************
; *** the next is an initialization function that stores in the properties "upcase"
;     and "lowcase" of the Tule names  of characters the related infos
(defun set-upcase-lowcase ()
   (let ((up-low-map
          '((up-a down-a) (up-b down-b) (up-c down-c) (up-d down-d) (up-e down-e)
            (up-f down-f) (up-g down-g) (up-h down-h) (up-i down-i) (up-j down-j)
            (up-k down-k) (up-l down-l) (up-m down-m) (up-n down-n) (up-o down-o)
            (up-p down-p) (up-q down-q) (up-r down-r) (up-s down-s) (up-t down-t)
            (up-u down-u) (up-v down-v) (up-w down-w) (up-x down-x) (up-y down-y)
            (up-z down-z) (up-ae down-ae) (up-eth down-eth) (up-thorn down-thorn)
            (up-a-grave down-a-grave) (up-a-acute down-a-acute) (up-a-circum down-a-circum)
            (up-a-tilde down-a-tilde) (up-a-diaer down-a-diaer) (up-a-ring down-a-ring)
            (up-c-cedilla down-c-cedilla) (up-e-grave down-e-grave) (up-e-acute down-e-acute)
            (up-e-circum down-e-circum) (up-e-diaer down-e-diaer) (up-i-grave down-i-grave)
            (up-i-acute down-i-acute) (up-i-circum down-i-circum) (up-i-diaer down-i-diaer)
            (up-n-tilde down-n-tilde) (up-o-grave down-o-grave) (up-o-acute down-o-acute)
            (up-o-circum down-o-circum-circum) (up-o-tilde down-o-tilde) (up-o-diaer down-o-diaer)
            (up-o-stroke down-o-stroke) (up-u-grave down-u-grave) (up-u-acute down-u-acute)
            (up-u-circum down-u-circum) (up-u-diaer down-u-diaer) (up-y-acute down-y-acute)
            (up-french-oe down-french-oe)
            (up-alpha down-alpha) (up-beta down-beta) (up-gamma down-gamma) 
            (up-delta down-delta) (up-epsilon down-epsilon) (up-zeta down-zeta)
            (up-eta down-eta) (up-theta down-theta) (up-iota down-iota) (up-kappa down-kappa)
            (up-lambda down-lambda) (up-mu  down-mu) (up-nu down-nu) (up-xi down-xi)
            (up-omicron down-omicron) (up-pi down-pi) (up-rho down-rho) (up-sigma down-sigma1)
            (up-sigma down-sigma2) (up-tau down-tau) (up-upsilon down-upsilon) (up-phi down-phi)
            (up-chi down-chi) (up-psi down-psi) (up-omega down-omega)
            (up-alpha-acute down-alpha-acute) (up-epsilon-acute down-epsilon-acute)
            (up-eta-acute down-eta-acute) (up-iota-acute down-iota-acute) 
            (up-iota-diaer down-iota-diaer) (up-omicron-acute down-omicron-acute) 
            (up-omega-acute down-omega-acute) (up-upsilon-acute down-upsilon-acute)
            (up-upsilon-diaer down-upsilon-diaer)
            (up-phi-curled down-phi) (up-upsilon-curled down-upsilon)
            (jap-up-u-macron jap-down-u-macron) (jap-up-o-macron jap-down-o-macron))))
        (dolist (ul up-low-map)
            (putprop (first ul) (second ul) 'lowcase)
            (putprop (second ul) (first ul) 'upcase))))

;*****************************************************************
; *** the next is an initialization function that stores in the properties 
;     "no-stress", "grave", "acute", ... of the Tule names of characters the related infos
(defun set-accent-map ()
 (let ((accent-map
       '((down-a down-a-acute down-a-circum down-a-diaer down-a-grave down-a-ring down-a-tilde)
  	 (down-e down-e-acute down-e-circum down-e-diaer down-e-grave)
	 (down-i down-i-acute down-i-circum down-i-diaer down-i-grave)
	 (down-o down-o-acute down-o-circum down-o-diaer down-o-grave nil	  down-o-tilde)
	 (down-u down-u-acute down-u-circum down-u-diaer down-u-grave)
	 (down-y down-y-acute nil	    down-y-diaer)
         (up-a   up-a-acute   up-a-circum   up-a-diaer   up-a-grave   up-a-ring	  up-a-tilde)
         (up-e   up-e-acute   up-e-circum   up-e-diaer   up-e-grave)
         (up-i 	 up-i-acute   up-i-circum   up-i-diaer   up-i-grave)
	 (up-u   up-u-acute   up-u-circum   up-u-diaer   up-u-grave)
         (up-y   up-y-acute)
         (down-alpha down-alpha-acute)
         (down-epsilon down-epsilon-acute)
         (down-eta down-eta-acute)
         (down-iota down-iota-acute nil down-iota-diaer nil nil nil down-iota-mixed-stress)
         (down-omega down-omega-acute)
         (down-omicron down-omicron-acute)
         (down-upsilon down-upsilon-acute nil down-upsilon-diaer nil nil nil down-upsilon-mixed-stress)
         (up-alpha up-alpha-acute)
         (up-epsilon up-epsilon-acute)
         (up-eta up-eta-acute)
         (up-iota up-iota-diaer)
         (up-omega up-omega-acute)
         (up-omicron up-omicron-acute)
         (up-upsilon up-upsilon-acute nil up-upsilon-diaer))))
        (dolist (ul accent-map)
            (cond ((not (null (second ul)))
                     (putprop (first ul) (second ul) 'acute)
                     (putprop (second ul) (first ul) 'no-stress)))
            (cond ((not (null (third ul)))
                     (putprop (first ul) (third ul) 'circumflex)
                     (putprop (third ul) (first ul) 'no-stress)))
            (cond ((not (null (fourth ul)))
                     (putprop (first ul) (fourth ul) 'diaeresis)
                     (putprop (fourth ul) (first ul) 'no-stress)))
            (cond ((not (null (fifth ul)))
                     (putprop (first ul) (fifth ul) 'grave)
                     (putprop (fifth ul) (first ul) 'no-stress)))
            (cond ((not (null (sixth ul)))
                     (putprop (first ul) (sixth ul) 'ring)
                     (putprop (sixth ul) (first ul) 'no-stress)))
            (cond ((not (null (seventh ul)))
                     (putprop (first ul) (seventh ul) 'tilde)
                     (putprop (seventh ul) (first ul) 'no-stress)))
            (cond ((not (null (eighth ul)))
                     (putprop (first ul) (eighth ul) 'mixed-stress)
                     (putprop (eighth ul) (first ul) 'no-stress))))))

; *********************************************************************
; *************** CONVERSIONS *****************************************
; *******************************************************************
; *** given a lisp character, this function returns the tule system name
;     for that character
(defun get-tule-char-name-from-lisp-char (ch)
  (declare (special *LISP-TO-TULE-TABLE*))
    (second (assoc ch *LISP-TO-TULE-TABLE*)))

; *******************************************************************
; **** given a numeric character code, it returns the tule char name
;      It needs two numbers, in order to account for the variable-length
;      encodings (as UTF-8). It returns two values:
;      - the tule character name
;      - the number of bytes consumed (1 or 2)
(defun get-tule-char-name-from-currlisp (bytes)
   (declare (special *CURRLISP-TO-TULE-TABLE*))
   (let* ((byte1 (first bytes))
          (byte2 (second bytes))
          (byte3 (third bytes))
          (first-byte (first (leggi *CURRLISP-TO-TULE-TABLE* byte1)))
          second-byte third-byte)
       (cond ((null first-byte) nil)
             ((atom first-byte)
                (values first-byte 1))
             (t (setq second-byte (first (leggi first-byte byte2)))
                (cond ((null second-byte) nil)
                      ((atom second-byte)
                         (values second-byte 2))
                      (t (setq third-byte (first (leggi second-byte byte3)))
                         (cond ((null third-byte) nil)
                               (t (values third-byte 3)))))))))

; *******************************************************************
; **** given a numeric character code, it returns the tule char name
;      It needs two numbers, in order to account for the variable-length
;      encodings (as UTF-8). It returns two values:
;      - the tule character name
;      - the number of bytes consumed (1 or 2 or 3)
(defun get-tule-char-name-from-numcode (bytes)
   (declare (special *CODE-TO-TULE-TABLE*))
   (let* ((byte1 (first bytes))
          (byte2 (second bytes))
          (byte3 (third bytes))
          (first-byte (first (leggi *CODE-TO-TULE-TABLE* byte1)))
          second-byte third-byte)
       (cond ((null first-byte) nil)
             ((atom first-byte)
                (values first-byte 1))
             (t (setq second-byte (first (leggi first-byte byte2)))
                (cond ((null second-byte) nil)
                      ((atom second-byte)
                         (values second-byte 2))
                      (t (setq third-byte (first (leggi second-byte byte3)))
                         (cond ((null third-byte) nil)
                               (t (values third-byte 3)))))))))

;*****************************************************************
; *** This returns the Tule character name and a value (1 or 2)
;     specifying how many bytes of the input have been consumed
;     starting from the ISO-8859-1 code (used for dictionary lookup)
; *** in case the encoding is outside the range of ISO-8859-1, then use the
;     enforced encoding
(defun get-tule-char-name-from-base-code (base-codes)
  (declare (special *BASE-CODE-TABLE* *NON-DICTIONARY-CHARS*))
  (cond ((memq (first base-codes) *NON-DICTIONARY-CHARS*)
           (let (poss-mult consumed)
                (multiple-value-setq (poss-mult consumed)
                    (get-tule-char-name-from-numcode base-codes))
                (cond ((null poss-mult) 
                         (values (third (assoc (first base-codes) *BASE-CODE-TABLE*)) 1))
                      (t (values poss-mult consumed)))))
        (t (values (third (assoc (first base-codes) *BASE-CODE-TABLE*)) 1))))

;*****************************************************************
; *** This returns the lisp character
;     starting from the ISO-8859-1 code (currently, equivalent to code-char)
(defun get-lisp-char-from-base-code (base-code)
    (code-char base-code))

;*****************************************************************
; *** returns the single character code associated with the Tule name of a char
;          down-e --> 101
;     the actual returned value depends on the chosen encoding scheme
; >>> INPUT: TULE
; >>> OUTPUT: CODE
(defun get-numcode-from-tule-char-name (name)
  (get name 'char-value))

;*****************************************************************
; *** This returns the ISO-8859-1 code starting from the tule char name
;     (used for dictionary search)
(defun get-base-code-from-tule-char-name (tule-name)
  (get tule-name 'char-base-value))

;*****************************************************************
; *** returns the single character code associated with the Tule name of a char
;          down-e --> 101
;     the actual returned value depends on the chosen encoding scheme
; >>> INPUT: TULE
; >>> OUTPUT: CODE
(defun get-lispcode-from-tule-char-name (tule-name)
  (get tule-name 'char-lisp-value))

;*****************************************************************
; *** returns the single character code associated with the Lisp name of a char
;          #\e --> 101
;     the actual returned value depends on the chosen encoding scheme
;              Note that this is different from the Lisp function "char-code", 
;              which always returns the ISO-8859-1 code.
; >>> INPUT: LISP
; >>> OUTPUT: CODE
(defun get-numcode-from-lisp-char (name)
   (get-numcode-from-tule-char-name
       (get-tule-char-name-from-lisp-char name)))

;*****************************************************************
; *** returns the Lisp char (if any) corresponding to the given Tule character
;     name. If the Lisp char does not exist, returnns nil
(defun get-lisp-char-from-tule-char-name (tule-name)
   (get tule-name 'lisp-char))
    
;*****************************************************************
; *** This function takes a numeric character code, consisting in one or two bytes,
;     and returns two values:
;      - the tule character name
;      - the number of bytes consumed (1 or 2)
; *** for one-byte codes, the second byte is ignored
;        Note that this is different from the Lisp function "code-char", 
;        which always uses the ISO-8859-1 code.
(defun get-lisp-char-from-numcode (bytes)
  (let (tule-name consumed)
      (multiple-value-setq (tule-name consumed)
                  (get-tule-char-name-from-numcode bytes))
      (cond ((null tule-name) 
               (exception-nothrow "Undefined character code 1: MORPHO/char-funct"))
            (t (values (get-lisp-char-from-tule-char-name tule-name) consumed)))))
        
;*****************************************************************
; *** This returns the numerical code in the encoding currently enforced
;     starting from the ISO-8859-1 code (used for dictionary loading)
(defun get-numcode-from-base-code (base-code)
  (declare (special *BASE-CODE-TABLE*))
  (second (assoc base-code *BASE-CODE-TABLE*)))

;*****************************************************************
; *** given a list of numbers, corresponding to iso numeric codes
;     returns the list of numbers corresponding to the utf-8 encoding
;     For example, for "città":
;      (99 105 116 116 224) --> (99 105 116 116 195 160)
; >>> INPUT: List of iso codes
; >>> OUTPUT: List of utf8 codes
; !!! Note that the Utf-8 based versions of Lisp do not recognize #\à
;     as a meaningful character name, and that "città" is represented,
;     internally, as the second list above
(defun convert-base-codes-to-utf8-codes (base-codes)
  (let (tulen)
    (setq tulen (convert-base-codes-to-tule-char-names base-codes))
    (convert-tule-char-names-to-numcodes tulen)))

; ****************************************************************
; *** the next function is useful to make readable the data loaded form
;     files. This enables to check the values from keyboard
(defun convert-atom-or-string-to-currscheme (atom)
  (let ((explinp (mapcar #'char-code (explode atom))))
   ; (break "convert")
   (implode (convert-base-codes-to-utf8-codes explinp))))
    
; ****************************************************************
; *** the next function takes a name expressed in base encoding
;     (iso-8859-1) and converts it into the chosen encoding
(defun convert-base-atom-or-string-to-currscheme (atom)
  (declare (special *CHAR-SET-ID*))
  (cond ((eq *CHAR-SET-ID* 'iso-8859-1) atom)
        (t (implode
              (convert-tule-char-names-to-numcodes
                 (convert-base-codes-to-tule-char-names
                    (mapcar #'char-code (explode atom))))))))
    
; ****************************************************************
; *** the next function takes a name expressed in base encoding
;     (iso-8859-1) and converts it into the current Lisp encoding
(defun convert-base-atom-or-string-to-currlisp (atom)
  (declare (special *LISP-CHAR-SET-ID*))
  (cond ((eq *LISP-CHAR-SET-ID* 'iso-8859-1) atom)
        (t (implode
              (convert-tule-char-names-to-currlisp
                 (convert-base-codes-to-tule-char-names
                    (mapcar #'char-code (explode atom))))))))

; ****************************************************************
; *** the next function makes the inverse: takes a name expressed 
;     in the current Lisp encoding and converts it into base encoding
;     (iso-8859-1) 
(defun convert-currlisp-atom-or-string-to-base (atom)
  (declare (special *LISP-CHAR-SET-ID*))
  (cond ((eq *LISP-CHAR-SET-ID* 'iso-8859-1) atom)
        (t (implode
              (convert-tule-char-names-to-base-codes
                 (convert-currlisp-to-tule-char-names
                    (mapcar #'char-code (explode atom))))))))
    
; ****************************************************************
; *** the next function takes a dictionary key (expressed in base
;     encoding and with accented chars uppercase) and returns the
;     corresponding item in the current encoding and with accented
;     chars lowercase
(defun convert-dict-key-to-currscheme (atom)
   (convert-base-atom-or-string-to-currscheme
      (base-uppercase-no-accent
          (base-lowercase atom))))
    
; ****************************************************************
; *** the next function takes a dictionary key (expressed in base
;     encoding and with accented chars uppercase) and returns the
;     corresponding item in the current Lisp encoding and with accented
;     chars lowercase
(defun convert-dict-key-to-currlisp (atom)
   (convert-base-atom-or-string-to-currlisp
      (base-uppercase-no-accent
          (base-lowercase atom))))
    
;*****************************************************************
; *** given a list of tule names, returns the corresponding list of numeric codes
;     (according to the chosen encoding)
;          (up-e down-alpha down-a) --> (69 206 177 97) 	[in UTF-8]
; >>> INPUT: List of TULE
; >>> OUTPUT: List of CODE
(defun convert-tule-char-names-to-currlisp (tulenames)
  (let (result nxtcode)
     (dolist (nxtname tulenames (reverse result))
        (setq nxtcode (get nxtname 'char-lisp-value))
        (cond ((null nxtcode)
                 (exception-nothrow "Undefined numerical code in MORPHO/char-funct"))
              ((atom nxtcode)
                 (setq result (cons nxtcode result)))
              (t (setq result (append (reverse nxtcode) result)))))))

;*****************************************************************
; *** given a list of tule names, returns the corresponding list of numeric codes
;     (according to the chosen encoding)
;          (up-e down-alpha down-a) --> (69 206 177 97) 	[in UTF-8]
; >>> INPUT: List of TULE
; >>> OUTPUT: List of CODE
(defun convert-tule-char-names-to-numcodes (tulenames)
  (let (result nxtcode)
     (dolist (nxtname tulenames (reverse result))
        (setq nxtcode (get nxtname 'char-value))
        (cond ((null nxtcode)
                 (exception-nothrow "Undefined numerical code in MORPHO/char-funct"))
              ((atom nxtcode)
                 (setq result (cons nxtcode result)))
              (t (setq result (append (reverse nxtcode) result)))))))

;*****************************************************************
; *** given a list of tule names, returns the corresponding list of numeric codes
;     (according to the base - ISO-8859-1 - encoding)
; *** the function returns the base encoding for chars in the ISO-8859-1 set, but
;     the enforced encoding for other cars. So, 
;          (up-e down-a apostrophe) --> (69 97 39) 	
;          (up-e down-a down-e-grave) --> (69 97 233) 	
;     but
;          (up-e down-alpha down-a) --> (69 206 177 97)
; *** this is used for dictionary access, since the dictionary includes entries in
;     base form for extended ascii, but not for other Unicode chars
; >>> INPUT: List of TULE
; >>> OUTPUT: List of CODE
(defun convert-tule-char-names-to-base-codes (tulenames)
  (let (result nxtcode)
     (dolist (nxtname tulenames (reverse result))
        (setq nxtcode (get nxtname 'char-base-value))
        (cond ((null nxtcode)
                (setq nxtcode (get nxtname 'char-value))
                (cond ((null nxtcode)
                          (exception-nothrow "No code for a character in MORPHO/char-funct"))
                      (t (setq result (append (reverse nxtcode) result)))))
              (t (setq result (cons nxtcode result)))))))

; *******************************************************************
; >>> INPUT: List of CODE
; >>> OUTPUT: List of TULE
;     The two lists can be of uneven length (the OUTPUT can be shorter)
(defun convert-numcodes-to-tule-char-names (numcodes)
  (let (nexttule consumed)
    (do ((result nil (cons nexttule result))
         (chars numcodes (cond ((= consumed 1) (rest chars))
                               ((= consumed 2) (rest (rest chars)))
                               (t (rest (rest (rest chars)))))))
        ((null chars) (reverse result))
        (multiple-value-setq (nexttule consumed)
              (get-tule-char-name-from-numcode chars))
        (cond ((null nexttule)
                 (exception-nothrow "Undefined character code 2: MORPHO/char-funct"))))))

; *******************************************************************
; >>> INPUT: List of CODE
; >>> OUTPUT: List of TULE
;     The two lists can be of uneven length (the OUTPUT can be shorter)
; *** this differs from the previous one, since the non-ascii-base chars (e.g. #\é)
;     are interpreted as ISO-8859-1, while the others via the chosen encoding
(defun convert-base-codes-to-tule-char-names (numcodes)
  (let (nexttule consumed)
    (do ((result nil (cons nexttule result))
         (chars numcodes (cond ((= consumed 1) (rest chars))
                               ((= consumed 2) (rest (rest chars)))
                               (t (rest (rest (rest chars)))))))
        ((null chars) (reverse result))
        (multiple-value-setq (nexttule consumed)
              (get-tule-char-name-from-base-code chars))
        (cond ((null nexttule) 
                  (break "Unknown char. char-funct: convert-base-codes-to-tule-char-names"))))))

; *******************************************************************
; >>> INPUT: List of CODE
; >>> OUTPUT: List of TULE
;     The two lists can be of uneven length (the OUTPUT can be shorter)
; *** this differs from the previous one, since the non-ascii-base chars (e.g. #\é)
;     are interpreted according to the current lisp encoding
(defun convert-currlisp-to-tule-char-names (numcodes)
  (let (nexttule consumed)
    (do ((result nil (cons nexttule result))
         (chars numcodes (cond ((= consumed 1) (rest chars))
                               ((= consumed 2) (rest (rest chars)))
                               (t (rest (rest (rest chars)))))))
        ((null chars) (reverse result))
        (multiple-value-setq (nexttule consumed)
              (get-tule-char-name-from-currlisp chars)))))

; *******************************************************
; *** "name" is any atom name. It returns the corresponding list of characters codes,
;     in the set currently in use
;     "d'Ant" 
;         --explode--> 
;     (#\d #\' #\A #\n #\t) 
;         --get-tule-char-name-from-lisp-char-->
;     (down-di apostrophe up-a down-n down-ti) 
;         --convert-tule-char-names-to-numcodes-->
;     (100 39 65 110 116)
; *** actually, the final result may change depending on the char encoding scheme in use
; *** converts a string into a list of character codes
; >>> INPUT: LISP atom
; >>> OUTPUT: List of CODE
(defun line-to-char (str)
  (let ((newlinecode (get 'newline 'char-value)))
   (append (convert-tule-char-names-to-base-codes 
               (convert-numcodes-to-tule-char-names
                  (mapcar #'char-code (explode str))))
           (cond ((atom newlinecode) (list newlinecode))
                 (t newlinecode)))))

; *******************************************************************
; >>> INPUT: LISP atom or string
; >>> OUTPUT: List of TULE
(defun line-to-tule-char (name)
   (declare (special *LISP-CHAR-SET-ID*))
   (cond ((eq *LISP-CHAR-SET-ID* 'iso-8859-1)
            (convert-numcodes-to-tule-char-names (mapcar #'char-code (explode name))))
         ((eq *LISP-CHAR-SET-ID* 'utf-8)
   ; *** when LISP (both Allegro and Clisp) runs as UTF-8, the
   ;     result of explode+char-code are unicode codes (i.e. base codes, in my
   ;     terminology)
            (convert-base-codes-to-tule-char-names (mapcar #'char-code (explode name))))
         (t (exception 'morpho "Unknown char set id: char-funct: line-to-tule-char"))))

;*****************************************************************
; *** returns the set of character codes associated with the name of a charset
;     e.g. neutral --> (Tab Newline Return Space)
; >>> INPUT: tokenizer label
; >>> OUTPUT: list of TULE
(defun get-charset-value (name)
  (get name 'charset-value))

;*****************************************************************
; *** returns the uppercase version of a single character (Tule name)
; >>> INPUT: TULE
; >>> OUTPUT: TULE
(defun get-char-upperc (name)
  (let ((upc (get name 'upcase)))
       (cond ((null upc) name)
             (t upc))))

;*****************************************************************
; *** returns the uppercase version of a single character (Tule name)
;     but only in case the char is not stressed, otherwise it is left
;     unchanged
; >>> INPUT: TULE
; >>> OUTPUT: TULE
;     If 'no-stress is NIL, then the character is not stressed and
;     the function tries to get the uppercase version. If it does not
;     exist, it is char which is already uppercase
;     If 'no-stress is not NIL, then the character is stressed, and the
;     function looks for its lowercase version
(defun get-char-upperc-no-accent (name)
  (let ((upc (cond ((null (get name 'no-stress)) (get name 'upcase))
                   (t (get name 'lowcase)))))
       (cond ((null upc) name)
             (t upc))))

;*****************************************************************
; *** returns the uppercase version of a single character (Lisp char)
; >>> INPUT: LISP
; >>> OUTPUT: CODE
(defun get-lisp-char-upperc (name)
  (get (get-char-upperc (get-tule-char-name-from-lisp-char name)) 'char-value))

;*****************************************************************
; *** returns the lowercase version of a single character (Tule name)
; >>> INPUT: TULE
; >>> OUTPUT: TULE
(defun get-char-lowerc (name)
  (let ((lwc (get name 'lowcase)))
       (cond ((null lwc) name)
             (t lwc))))

;*****************************************************************
; *** returns the lowercase version of a single character (Lisp char)
; >>> INPUT: LISP
; >>> OUTPUT: CODE
(defun get-lisp-char-lowerc (name)
  (get (get-char-lowerc (get-tule-char-name-from-lisp-char name)) 'char-value))

;*****************************************************************
; *** returns the no-stress version of a single character (Tule name)
; >>> INPUT: TULE
; >>> OUTPUT: TULE
(defun get-char-nostress (name)
  (let ((nsc (get name 'no-stress)))
       (cond ((null nsc) name)
             (t nsc))))

;*****************************************************************
; *** returns the version of a single character with grave accent (Tule name)
; >>> INPUT: TULE
; >>> OUTPUT: TULE
(defun get-char-grave (name)
  (let ((lwc (get name 'grave)))
       (cond ((null lwc) name)
             (t lwc))))

;*****************************************************************
; *** returns the version of a single character with acute accent (Tule name)
; >>> INPUT: TULE
; >>> OUTPUT: TULE
(defun get-char-acute (name)
  (let ((lwc (get name 'acute)))
       (cond ((null lwc) name)
             (t lwc))))

;*****************************************************************
; *** converts a word from lowercase to uppercase
;     It works on base
;     codes (ISO-8859-1), ignoring the chosen scheme. This is due to the 
;     need to match dictionary entries
;     w is any lisp name; so, after "explode" it is a list of Lisp chars
; >>> INPUT: LISP atom
; >>> OUTPUT: LISP atom
(defun base-uppercase (w)
  (implode
     (convert-tule-char-names-to-base-codes
        (mapcar #'get-char-upperc
           (convert-base-codes-to-tule-char-names 
              (mapcar #'char-code (explode w)))))))

;*****************************************************************
; *** converts a word from lowercase to uppercase, keeping the
;     accented char lowercase. It works on base
;     codes (ISO-8859-1), ignoring the chosen scheme. This is due to the 
;     need to match dictionary entries
;     w is any lisp name; so, after "explode" it is a list of Lisp chars
; >>> INPUT: LISP atom
; >>> OUTPUT: LISP atom
(defun base-uppercase-no-accent (w)
  (implode
     (convert-tule-char-names-to-base-codes
        (mapcar #'get-char-upperc-no-accent
           (convert-base-codes-to-tule-char-names 
              (mapcar #'char-code (explode w)))))))

;*****************************************************************
; *** converts a word from uppercase to lowercase
;     It works on base codes (ISO-8859-1), ignoring the chosen scheme.
;     This is due to the need to match dictionary entries
;     w is any lisp name; so, after "explode" it is a list of Lisp chars
; >>> INPUT: LISP atom
; >>> OUTPUT: LISP atom
(defun base-lowercase (w)
  (implode 
     (convert-tule-char-names-to-base-codes
        (mapcar #'get-char-lowerc
           (convert-base-codes-to-tule-char-names
              (mapcar #'char-code (explode w)))))))

;*****************************************************************
; *** converts a word from lowercase to uppercase
;     It assumes the input in  base codes (ISO-8859-1),
;     the output is in the current scheme
(defun convert-base-to-currscheme-uppercase-no-accent (w)
  (implode
     (convert-tule-char-names-to-numcodes
         (mapcar #'get-char-upperc-no-accent
             (convert-base-codes-to-tule-char-names
                 (mapcar #'char-code (explode w)))))))

;*****************************************************************
; *** converts a word from lowercase to uppercase
;     It assumes the input in  base codes (ISO-8859-1),
;     the output is in the current scheme
(defun convert-base-to-currscheme-uppercase (w)
  (implode
     (convert-tule-char-names-to-numcodes
         (mapcar #'get-char-upperc
             (convert-base-codes-to-tule-char-names
                 (mapcar #'char-code (explode w)))))))

;*****************************************************************
; *** converts a word from lowercase to uppercase
;     It assumes that both the input and the output are in the
;     current scheme
(defun currscheme-uppercase (w)
  (implode
     (convert-tule-char-names-to-numcodes
         (mapcar #'get-char-upperc
             (convert-numcodes-to-tule-char-names
                 (mapcar #'char-code (explode w)))))))

;*****************************************************************
; *** converts a word from lowercase to uppercase; the accented
;     chars are kept lowercase
;     It assumes that both the input and the output are in the
;     current scheme
(defun currscheme-uppercase-no-accent (w)
  (implode
     (convert-tule-char-names-to-numcodes
         (mapcar #'get-char-upperc-no-accent
             (convert-numcodes-to-tule-char-names
                 (mapcar #'char-code (explode w)))))))

;*****************************************************************
; *** this takes a list of atoms (roots) and extracts 'property
;     for all of them
(defun all-accent-get (roots property)
  (let (result)
    (dolist (entry roots result)
        (let ((val (get entry property)))
            (cond ((listp val)
                     (setq result (append result val)))
                  (t (setq result (append1 result val))))))))

;*****************************************************************
; *** this function generates various uppercase/lowercase/with or
;     without accent versions of a word. It is used for dictionary access:
;     if all dictionaries were uniform, it would not be needed
; *** This works on the basis of Tule char names, in order to obtain the lowercase-
;     uppercase mapping. It takes as input an atom, corresponding to the
;     ISO-8859-1 encoding of the involved word. This is troublesome, since, if
;     teh enforced encoding is different, then some characters are encoded in a
;     different way, with respect to the dictionary (which, anyway, is ISO-8859-1)
;     What may happen is that stressed chars (as #\à) are encoded in the dictionary
;     as 224 (ISO-8859-1), but arrive in input as (195 160) (UTF-8, if this is the 
;     chosen encoding scheme). In order to enable for dictionary access, the following
;     conversions are needed:
;     a. From input word to Lisp chars (explode)
;        |cittÃ¡| --> (#\c #\i #\t #\t #\%upperc #\%!)
;     b. From Lisp chars to ISO-8859-1 numcodes (base codes)
;        (#\c #\i #\t #\t #\%upperc #\%!) --> (99 105 116 116 195 161)
;     c. From these codes to Tule names, but using the chosen encoding (e.g. UTF-8)
;        (99 105 116 116 195 161) --> (DOWN-C DOWN-I DOWN-T DOWN-T DOWN-A-GRAVE)
;        Note that variable-length encoding can change the length of the output
;     d. From Tule names, to the required change (ex. from lowercase to uppercase)
;        (DOWN-C DOWN-I DOWN-T DOWN-T DOWN-A-GRAVE) --> (UP-C UP-I UP-T UP-T UP-A-GRAVE)
;     e. From Tule names to char-codes (using the enforced encoding)
;        (UP-C UP-I UP-T UP-T UP-A-GRAVE) --> (67 73 84 84 195 160)
;     f. Implode the result
;        (67 73 84 84 195 160) --> CITTÃ 
; *** given a word, it generates:
;     1. The version with a final stressed char (if any). Grave accent.
;        carita' --> carità;  							[grave]
;     2. The version with a final stressed char (if any). Acute accent.
;        perche' --> perché;  							[acute]
;     3. The version with an external accent (apostrophe):
;        carità --> carita'; perché --> perche'					[extern]
;     4. The lowercase version:
;        CARITà --> carità; PERCHé --> perché					[down]
;     5. The lowercase version with a final stressed char (if any). Grave accent.
;        CARITA' --> carità; PERCHé --> perchè					[down-grave]
;     6. The lowercase version with a final stressed char (if any). Acute accent.
;        PERCHé --> perché							[down-acute]
;     7. The lowercase version with an external accent (apostrophe):
;        CARITà --> carita'; PERCHé --> perche'					[down-extern]
;     8. The uppercase version;
;        casa --> CASA; carità --> CARITà; carita' --> CARITA'			[up]
;        it is assumed that the only stressed characters actually occurring at end
;        of a word are à, è, é, ì, ò, ù, È, É; the other chars (a, i, o, u with
;        acute stress and capitalized accented words) being excluded
;     9. The uppercase version with a final stressed char (if any). Grave accent.
;        carita' --> CARITà;							[up-grave]
;     10.The uppercase version with a final stressed char (if any). Acute accent.
;        perche' --> PERCHé;							[up-acute]
;     11.The uppercase version with an external accent (apostrophe):
;        carità --> CARITA'; perché --> PERCHE'					[up-extern]
;     12.The uppercase version with a final uppercase stressed char (if any). Grave accent.
;        perche' --> PERCHÈ;							[all-up-grave]
;     13.The uppercase version with a final uppercase stressed char (if any). Acute accent.
;        perche' --> PERCHÉ;							[all-up-acute]
; *** in all versions above, any middle accent is removed: ancòra --> ancora
;     This could be troublesome for French, but the original word is maintained
;     anyway
(defun final-accent-and-uppercase (root)
  (let* ((expl-root (explode root))				; *** STEP a ********
         (numcode-expl-root (mapcar #'char-code expl-root))	; *** STEP b ********
         (tule-expl-root (convert-base-codes-to-tule-char-names numcode-expl-root))
                                                                ; *** STEP c ********
         (inv-tule-root (reverse tule-expl-root))
         out-accent last-vowel no-mid-accent up-rest-rest up-no-mid-rest-rest down-rest-rest
         prefix last-up last-down last-no-stress down-no-mid-rest-rest grave-last down-no-mid-rest 
         up-no-mid-rest acute-last no-mid-rest last-char down-all up-all up-mid-low-rest-rest
         grave-last-down acute-last-down up-extern-res allforms finalres up-all-mid-low
           ; returned values
         up up-mid-low down no-mid up-no-mid down-no-mid all-up
         grave up-grave up-mid-low-grave down-grave grave-no-mid up-grave-no-mid down-grave-no-mid all-up-grave
         acute up-acute down-acute up-mid-low-acute acute-no-mid up-acute-no-mid down-acute-no-mid all-up-acute
         extern extern-no-mid up-extern up-mid-low-extern up-extern-no-mid down-extern down-extern-no-mid)
    (cond ((memq (first inv-tule-root) '(apostrophe grave-accent))
   ; *** the word ends with a separate accent
            (setq out-accent (first inv-tule-root))
            (setq last-vowel (second inv-tule-root))
            (setq prefix (rest (rest inv-tule-root)))
            (setq no-mid-accent (rem-middle-accent prefix))
            (setq up-rest-rest (mapcar #'get-char-upperc prefix))
            (setq up-mid-low-rest-rest (mapcar #'get-char-upperc-no-accent prefix))
            (setq up-no-mid-rest-rest  (mapcar #'get-char-upperc no-mid-accent))
            (setq down-rest-rest (mapcar #'get-char-lowerc prefix))
            (setq down-no-mid-rest-rest (mapcar #'get-char-lowerc no-mid-accent))
            (cond ((member last-vowel '(down-a down-e down-i down-o down-u))
                 ; *** upcase and downcase of the original form
                    (setq last-up (get-char-upperc last-vowel))
                    (setq up (cons out-accent (cons last-up up-rest-rest)))          ; CiXòna' --> CIXÒNA'
                    (setq up-mid-low (cons out-accent (cons last-up up-mid-low-rest-rest)))  ; CiXòna' --> CIXòNA'
                    (setq down (cons out-accent (cons last-vowel down-rest-rest)))   ; CiXòna' --> cixòna'
                 ; *** original form, upcase and downcase of the original form, but without middle accents
                    (setq no-mid (cons out-accent (cons last-vowel no-mid-accent)))  ; CiXòna' --> CiXona'
                    (setq up-no-mid (cons out-accent (cons last-up up-no-mid-rest-rest))) ; CiXòna' --> CIXONA'
                    (setq down-no-mid (cons out-accent (cons last-vowel down-no-mid-rest-rest)))
                                                                                     ; CiXòna' --> cixona'
                    (setq all-up up-no-mid)                                          ; CiXòna' --> CIXONA'
                 ; *** upcase and downcase of the form, with insertion of the last vowel with grave accent
                    (setq grave-last (get-char-grave last-vowel))
                    (setq grave (cons grave-last prefix))                            ; CiXòna' --> CiXònà
                    (setq up-grave (cons grave-last up-rest-rest))                   ; CiXòna' --> CIXÒNà
                    (setq up-mid-low-grave (cons grave-last up-mid-low-rest-rest))   ; CiXòna' --> CIXòNà
                    (setq down-grave (cons grave-last down-rest-rest))               ; CiXòna' --> cixònà
                    (setq grave-no-mid (cons grave-last no-mid-accent))              ; CiXòna' --> CiXonà
                    (setq up-grave-no-mid (cons grave-last up-no-mid-rest-rest))     ; CiXòna' --> CIXONà
                    (setq down-grave-no-mid (cons grave-last down-no-mid-rest-rest)) ; CiXòna' --> cixonà
                    (setq all-up-grave                                               ; CiXòna' --> CIXÒNÀ
                         (cons (get-char-grave (get-char-upperc grave-last)) up-no-mid-rest-rest))
                                            
                 ; *** upcase and downcase of the form, with insertion of the last vowel with acute accent
                    (setq acute-last (get-char-acute last-vowel))
                    (setq acute (cons acute-last prefix))                            ; CiXòna' --> CiXòná
                    (setq up-acute (cons acute-last up-rest-rest))                   ; CiXòna' --> CIXÒNá
                    (setq up-mid-low-acute (cons acute-last up-mid-low-rest-rest))   ; CiXòna' --> CIXòNá
                    (setq down-acute (cons acute-last down-rest-rest))               ; CiXòna' --> cixòná
                    (setq acute-no-mid (cons acute-last no-mid-accent))              ; CiXòna' --> CiXoná
                    (setq up-acute-no-mid (cons acute-last up-no-mid-rest-rest))     ; CiXòna' --> CIXÒNá
                    (setq down-acute-no-mid (cons acute-last down-no-mid-rest-rest)) ; CiXòna' --> cixoná
                    (setq all-up-acute                                               ; CiXòna' --> CIXONÁ
                         (cons (get-char-acute (get-char-upperc acute-last)) up-no-mid-rest-rest)))
                  ((member last-vowel '(up-a up-e up-i up-o up-u))
                 ; *** upcase and downcase of the original form
                    (setq last-down (get-char-lowerc last-vowel))
                    (setq up (cons out-accent (cons last-vowel up-rest-rest)))       ; CiXònA' --> CIXÒNA'
                    (setq up-mid-low (cons out-accent (cons last-vowel up-mid-low-rest-rest))); CiXònA' --> CIXÒNA'
                    (setq down (cons out-accent (cons last-down down-rest-rest)))    ; CiXònA' --> cixòna'
                 ; *** original form, upcase and downcase of the original form, but without middle accents
                    (setq no-mid (cons out-accent (cons last-vowel no-mid-accent)))  ; CiXònA' --> CiXonA'
                    (setq up-no-mid (cons out-accent (cons last-vowel up-no-mid-rest-rest))) ; CiXònA' --> CIXONA'
                    (setq down-no-mid (cons out-accent (cons last-down down-no-mid-rest-rest))) ; CiXòna' --> cixona'
                    (setq all-up up-no-mid)                                          ; CiXòna' --> CIXONA'
                 ; *** upcase and downcase of the form, with insertion of the last vowel with grave accent
                    (setq grave-last (get-char-grave last-vowel))
                    (setq grave-last-down (get-char-lowerc grave-last))
                    (setq grave (cons grave-last prefix))                            ; CiXònA' --> CiXònÀ
                    (setq up-grave (cons grave-last up-rest-rest))                   ; CiXònA' --> CIXÒNÀ
                    (setq up-mid-low-grave (cons grave-last-down up-mid-low-rest-rest)) ; CiXòna' --> CIXòNà
                    (setq down-grave (cons grave-last-down down-no-mid-rest-rest))   ; CiXònA' --> cixònà
                    (setq grave-no-mid (cons grave-last no-mid-accent))              ; CiXòna' --> CiXonÀ
                    (setq up-grave-no-mid (cons grave-last up-no-mid-rest-rest))     ; CiXònA' --> CIXONÀ
                    (setq down-grave-no-mid (cons grave-last-down down-no-mid-rest-rest)) ; CiXònA' --> cixonà
                    (setq all-up-grave (cons grave-last up-no-mid-rest-rest))        ; CiXònA' --> CIXONÀ
                 ; *** upcase and downcase of the form, with insertion of the last vowel with acute accent
                    (setq acute-last (get-char-acute last-vowel))
                    (setq acute-last-down (get-char-lowerc acute-last))
                    (setq acute (cons acute-last no-mid-accent))                     ; CiXònA' --> CiXònÁ
                    (setq up-acute (cons acute-last up-no-mid-rest-rest))            ; CiXònA' --> CIXÒNÁ
                    (setq up-mid-low-acute (cons acute-last-down up-mid-low-rest-rest))   ; CiXòna' --> CIXòNá
                    (setq down-acute (cons acute-last-down down-rest-rest))          ; CiXònA' --> cixòná
                    (setq acute-no-mid (cons acute-last no-mid-accent))              ; CiXònA' --> CiXonÁ
                    (setq up-acute-no-mid (cons acute-last up-no-mid-rest-rest))     ; CiXònA' --> CIXONÁ
                    (setq down-acute-no-mid (cons acute-last down-no-mid-rest-rest)) ; CiXònA' --> cixoná
                    (setq all-up-acute (cons acute-last up-no-mid-rest-rest)))       ; CiXònA' --> CIXONÁ
                  (t		; *** actually, in this case, last-vowel is not a vowel
                 ; *** upcase and downcase of the original form
                    (setq up (cons out-accent                                        ; CiXònc' --> CIXÒNC'
                                   (cons (get-char-upperc last-vowel) up-rest-rest)))
                    (setq up-mid-low (cons out-accent 
                                          (cons (get-char-upperc last-vowel) 
                                                up-mid-low-rest-rest)))		     ; CiXònc' --> CIXòNC'
                    (setq down (cons out-accent                                      ; CiXònc' --> cixònc'
                                   (cons (get-char-lowerc last-vowel) down-rest-rest)))
                 ; *** original form, upcase and downcase of the original form, but without middle accents
                    (setq no-mid (cons out-accent (cons last-vowel no-mid-accent)))  ; CiXònc' --> CiXonc'
                    (setq up-no-mid (cons out-accent                                 ; CiXònc' --> CIXONC'
                                         (cons (get-char-upperc last-vowel) up-no-mid-rest-rest)))
                    (setq down-no-mid (cons out-accent                               ; CiXònc' --> cixonc'
                                         (cons (get-char-lowerc last-vowel) down-no-mid-rest-rest)))
                    (setq all-up up-no-mid)                                          ; CiXònc' --> CIXONC'
                 ; *** upcase and downcase of the form, with insertion of the last vowel with grave accent
                    (setq grave inv-tule-root)                                       ; CiXònc' --> CiXÒnc'
                    (setq up-grave up)                                               ; CiXònc' --> CIXÒNC'
                    (setq up-mid-low-grave up-mid-low)                               ; CiXònc' --> CIXòNC'
                    (setq down-grave down)                                           ; CiXònc' --> cixònc'
                    (setq grave-no-mid no-mid)                                       ; CiXònc' --> CiXonc'
                    (setq up-grave-no-mid up-no-mid)                                 ; CiXònc' --> CIXONC'
                    (setq down-grave-no-mid down-no-mid)                             ; CiXònc' --> cixonc'
                    (setq all-up-grave up)                                           ; CiXònc' --> CIXÒNC'
                 ; *** upcase and downcase of the form, with insertion of the last vowel with acute accent
                    (setq acute inv-tule-root)                                       ; CiXònc' --> CiXÒnc'
                    (setq up-acute up)                                               ; CiXònc' --> CIXÒNC'
                    (setq up-mid-low-acute up-mid-low)                               ; CiXònc' --> CIXòNC'
                    (setq down-acute down)                                           ; CiXònc' --> cixònc'
                    (setq acute-no-mid no-mid)                                       ; CiXònc' --> CiXonc'
                    (setq up-acute-no-mid up-no-mid)                                 ; CiXònc' --> CIXONC'
                    (setq down-acute-no-mid down-no-mid)                             ; CiXònc' --> cixonc'
                    (setq all-up-acute up)                                           ; CiXònc' --> CIXÒNC'
            (setq extern (cons out-accent (cons last-vowel prefix)))
            (setq extern-no-mid (cons out-accent (cons last-vowel no-mid-accent)))
            (setq down-extern (cons out-accent (cons (get-char-lowerc last-vowel) down-rest-rest)))
            (setq down-extern-no-mid (cons out-accent (cons (get-char-lowerc last-vowel) down-no-mid-rest-rest)))
            (setq up-extern (cons out-accent (cons (get-char-upperc last-vowel) up-rest-rest)))
            (setq up-mid-low-extern (cons out-accent (cons (get-char-upperc last-vowel) up-mid-low-rest-rest)))
            (setq up-extern-no-mid (cons out-accent (cons (get-char-upperc last-vowel) up-no-mid-rest-rest))))))
    ; *** the last char of the word is not an accent
          (t (setq last-char (first inv-tule-root)) 
            (setq up-all (mapcar #'get-char-upperc inv-tule-root))
            (setq up-all-mid-low (mapcar #'get-char-upperc-no-accent inv-tule-root))
            (setq down-all (mapcar #'get-char-lowerc inv-tule-root))
            (setq no-mid-rest (rem-middle-accent (rest inv-tule-root)))
            (setq up-no-mid-rest (mapcar #'get-char-upperc no-mid-rest))
            (setq down-no-mid-rest (mapcar #'get-char-lowerc no-mid-rest))
            (cond ((member last-char 
                        '(down-a-grave down-e-grave down-i-grave down-o-grave down-u-grave))
                    (setq last-no-stress (get-char-nostress last-char))
                       ; *** base
                    (setq up (cons last-char (rest up-all)))
                    (setq up-mid-low up-all-mid-low)
                    (setq down down-all)
                    (setq no-mid (cons last-char no-mid-rest))
                    (setq up-no-mid (cons last-char up-no-mid-rest))
                    (setq down-no-mid (cons last-char down-no-mid-rest))
                    (setq all-up up-all)
                       ; *** grave
                    (setq grave inv-tule-root)
                    (setq up-grave up)
                    (setq up-mid-low-grave up-mid-low)
                    (setq down-grave down)
                    (setq grave-no-mid no-mid)
                    (setq up-grave-no-mid up-no-mid)
                    (setq down-grave-no-mid down-no-mid)
                    (setq all-up-grave (cons (get-char-upperc-no-accent last-char) (rest up-all)))
                       ; *** acute
                    (setq acute-last (get-char-acute last-no-stress))
                    (setq acute (cons acute-last (rest inv-tule-root)))
                    (setq up-acute (cons acute-last (rest up-all)))
                    (setq up-mid-low-acute (cons acute-last (rest up-mid-low)))
                    (setq down-acute (cons acute-last (rest down-all)))
                    (setq acute-no-mid (cons acute-last no-mid-rest))
                    (setq up-acute-no-mid (cons acute-last up-no-mid-rest))
                    (setq down-acute-no-mid (cons acute-last down-no-mid-rest))
                    (setq all-up-acute (cons (get-char-upperc acute-last) up-no-mid-rest))
                       ; *** external
                    (setq extern (cons 'apostrophe (cons last-no-stress (rest inv-tule-root))))
                    (setq extern-no-mid (cons 'apostrophe (cons last-no-stress no-mid-rest)))
                    (setq up-extern (cons 'apostrophe (cons (get-char-upperc last-no-stress)
                                                            (rest up-all))))
                    (setq up-mid-low-extern (cons 'apostrophe (cons (get-char-upperc last-no-stress)
                                                            (rest up-all-mid-low))))
                    (setq up-extern-no-mid (cons 'apostrophe (cons (get-char-upperc last-no-stress)
                                                            up-no-mid-rest)))
                    (setq down-extern (cons 'apostrophe (cons (get-char-lowerc last-no-stress)
                                                            (rest down-all))))
                    (setq down-extern-no-mid (cons 'apostrophe (cons (get-char-lowerc last-no-stress) 
                                                            down-no-mid-rest))))
                  ((member last-char 
                        '(down-a-acute down-e-acute down-i-acute down-o-acute down-u-acute))
                    (setq last-no-stress (get-char-nostress last-char))
                       ; *** base
                    (setq up (cons last-char (rest up-all)))
                    (setq up-mid-low up-all-mid-low)
                    (setq down down-all)
                    (setq no-mid (cons last-char no-mid-rest))
                    (setq up-no-mid (cons last-char up-no-mid-rest))
                    (setq down-no-mid (cons last-char down-no-mid-rest))
                    (setq all-up (cons (get-char-upperc last-char) (rest up-all)))
                       ; *** grave
                    (setq grave-last (get-char-grave last-no-stress))
                    (setq grave (cons grave-last (rest inv-tule-root)))
                    (setq up-grave (cons grave-last (rest up-all)))
                    (setq up-mid-low-grave (cons grave-last (rest up-mid-low)))
                    (setq down-grave (cons grave-last (rest down-all)))
                    (setq grave-no-mid (cons grave-last no-mid-rest))
                    (setq up-grave-no-mid (cons grave-last up-no-mid-rest))
                    (setq down-grave-no-mid (cons grave-last down-no-mid-rest))
                    (setq all-up-grave (cons (get-char-upperc grave-last) up-no-mid-rest))
                       ; *** acute
                    (setq acute inv-tule-root)
                    (setq up-acute up)
                    (setq up-mid-low-acute up-mid-low)
                    (setq down-acute down)
                    (setq acute-no-mid no-mid)
                    (setq up-acute-no-mid up-no-mid)
                    (setq down-acute-no-mid down-no-mid)
                    (setq all-up-acute up-all)
                       ; *** external
                    (setq extern (cons 'apostrophe (cons last-no-stress (rest inv-tule-root))))
                    (setq extern-no-mid (cons 'apostrophe (cons last-no-stress no-mid-rest)))
                    (setq up-extern (cons 'apostrophe (cons (get-char-upperc last-no-stress)
                                                            (rest up-all))))
                    (setq up-mid-low-extern (cons 'apostrophe (cons (get-char-upperc last-no-stress)
                                                            (rest up-all-mid-low))))
                    (setq up-extern-no-mid (cons 'apostrophe (cons (get-char-upperc last-no-stress)
                                                            up-no-mid-rest)))
                    (setq down-extern (cons 'apostrophe (cons (get-char-lowerc last-no-stress)
                                                            (rest down-all))))
                    (setq down-extern-no-mid (cons 'apostrophe (cons (get-char-lowerc last-no-stress) 
                                                            down-no-mid-rest))))

                  ((member last-char 
                        '(up-a-grave up-e-grave up-i-grave up-o-grave up-u-grave))
                    (setq last-no-stress (get-char-nostress last-char))
                    (setq last-down (get-char-lowerc last-no-stress))
                       ; *** base
                    (setq down down-all)
                    (setq up (cons last-down (rest up-all)))
                    (setq up-mid-low (cons last-down (rest up-all-mid-low)))
                    (setq no-mid (cons last-down no-mid-rest))
                    (setq up-no-mid (cons last-down up-no-mid-rest))
                    (setq down-no-mid (cons last-down down-no-mid-rest))
                    (setq all-up up-all)
                       ; *** grave
                    (setq grave-last (get-char-grave last-no-stress))
                    (setq grave-last-down (get-char-lowerc grave-last))
                    (setq grave (cons grave-last-down (rest inv-tule-root)))
                    (setq up-grave (cons grave-last-down (rest up-all)))
                    (setq up-mid-low-grave up-mid-low)
                    (setq down-grave (cons grave-last-down (rest down-all)))
                    (setq grave-no-mid (cons grave-last-down no-mid-rest))
                    (setq up-grave-no-mid (cons grave-last-down up-no-mid-rest))
                    (setq down-grave-no-mid (cons grave-last-down down-no-mid-rest))
                    (setq all-up-grave up-all)
                       ; *** acute
                    (setq acute-last (get-char-acute last-down))
                    (setq acute (cons acute-last (rest inv-tule-root)))
                    (setq up-acute (cons acute-last (rest up-all)))
                    (setq up-mid-low-acute (cons (get-char-lowerc acute-last) (rest up-mid-low)))
                    (setq down-acute (cons acute-last (rest down-all)))
                    (setq acute-no-mid (cons acute-last no-mid-rest))
                    (setq up-acute-no-mid (cons acute-last up-no-mid-rest))
                    (setq down-acute-no-mid (cons acute-last down-no-mid-rest))
                    (setq all-up-acute (cons (get-char-upperc acute-last) (rest up-all)))
                       ; *** external
                    (setq extern (cons 'apostrophe (cons last-no-stress (rest inv-tule-root))))
                    (setq extern-no-mid (cons 'apostrophe (cons last-no-stress no-mid-rest)))
                    (setq up-extern (cons 'apostrophe (cons last-no-stress (rest up-all))))
                    (setq up-mid-low-extern (cons 'apostrophe (cons last-no-stress (rest up-all-mid-low))))
                    (setq up-extern-no-mid (cons 'apostrophe (cons last-no-stress up-no-mid-rest)))
                    (setq down-extern (cons 'apostrophe (cons last-down (rest down-all))))
                    (setq down-extern-no-mid (cons 'apostrophe (cons last-down down-no-mid-rest))))
                  ((member last-char 
                        '(up-a-acute up-e-acute up-i-acute up-o-acute up-u-acute))
                    (setq last-no-stress (get-char-nostress last-char))
                    (setq last-down (get-char-lowerc last-no-stress))
                       ; *** base
                    (setq up (cons last-down (rest up-all)))
                    (setq up-mid-low up-all-mid-low)
                    (setq down down-all)
                    (setq no-mid (cons last-down no-mid-rest))
                    (setq up-no-mid (cons last-down up-no-mid-rest))
                    (setq down-no-mid (cons last-down down-no-mid-rest))
                    (setq all-up up-all)
                       ; *** grave
                    (setq grave-last (get-char-grave last-down))
                    (setq grave (cons grave-last (rest inv-tule-root)))
                    (setq up-grave (cons grave-last (rest up-all)))
                    (setq up-mid-low-grave (cons grave-last (rest up-all-mid-low)))
                    (setq down-grave (cons grave-last (rest down-all)))
                    (setq grave-no-mid (cons grave-last no-mid-rest))
                    (setq up-grave-no-mid (cons grave-last up-no-mid-rest))
                    (setq down-grave-no-mid (cons grave-last down-no-mid-rest))
                    (setq all-up-grave (cons (get-char-upperc grave-last) (rest up-all-mid-low)))
                       ; *** acute
                    (setq acute-last (get-char-acute last-no-stress))
                    (setq acute-last-down (get-char-lowerc acute-last))
                    (setq acute (cons acute-last-down (rest inv-tule-root)))
                    (setq up-acute (cons acute-last-down (rest up-all)))
                    (setq up-mid-low-acute up-mid-low)
                    (setq down-acute (cons acute-last-down (rest down-all)))
                    (setq acute-no-mid (cons acute-last-down no-mid-rest))
                    (setq up-acute-no-mid (cons acute-last-down up-no-mid-rest))
                    (setq down-acute-no-mid (cons acute-last-down down-no-mid-rest))
                    (setq all-up-acute up-all)
                       ; *** external
                    (setq extern (cons 'apostrophe (cons last-no-stress (rest inv-tule-root))))
                    (setq extern-no-mid (cons 'apostrophe (cons last-no-stress no-mid-rest)))
                    (setq up-extern (cons 'apostrophe (cons last-no-stress (rest up-all))))
                    (setq up-mid-low-extern (cons 'apostrophe (cons last-no-stress (rest up-all-mid-low))))
                    (setq up-extern-no-mid (cons 'apostrophe (cons last-no-stress up-no-mid-rest)))
                    (setq down-extern (cons 'apostrophe (cons last-down (rest down-all))))
                    (setq down-extern-no-mid (cons 'apostrophe (cons last-down down-no-mid-rest))))
                 (t		; *** the last char of the word is not a stressed vowel
                       ; *** base
                    (setq up up-all)
                    (setq down down-all)
                    (setq up-mid-low up-all-mid-low)
                    (setq no-mid (cons (first inv-tule-root) no-mid-rest))
                    (setq up-no-mid (cons (first up-all) up-no-mid-rest))
                    (setq down-no-mid (cons (first down-all) down-no-mid-rest))
                    (setq all-up up-all)
                       ; *** grave
                    (setq grave inv-tule-root)
                    (setq up-grave up-all)
                    (setq up-mid-low-grave up-all-mid-low)
                    (setq down-grave down-all)
                    (setq grave-no-mid no-mid)
                    (setq up-grave-no-mid up-no-mid)
                    (setq down-grave-no-mid down-no-mid)
                    (setq all-up-grave up-all)
                       ; *** acute
                    (setq acute inv-tule-root)
                    (setq up-acute up-all)
                    (setq up-mid-low-acute up-all-mid-low)
                    (setq down-acute down-all)
                    (setq acute-no-mid no-mid)
                    (setq up-acute-no-mid up-no-mid)
                    (setq down-acute-no-mid down-no-mid)
                    (setq all-up-acute up-all)
                       ; *** external
                    (setq extern inv-tule-root)
                    (setq extern-no-mid no-mid)
                    (setq up-extern up-all)
                    (setq up-mid-low-extern up-all-mid-low)
                    (setq up-extern-no-mid up-no-mid)
                    (setq down-extern down-all)
                    (setq down-extern-no-mid down-no-mid)))))
  ; *** the next because in some place the up-extern form is required, so that it must be returned
  ;     separately
     (setq up-extern-res 
               (implode (convert-tule-char-names-to-base-codes (reverse (dropnil up-extern)))))
     (setq allforms (elimdup (dropnil (list up down up-mid-low no-mid up-no-mid down-no-mid all-up
         grave up-grave up-mid-low-grave down-grave grave-no-mid up-grave-no-mid down-grave-no-mid all-up-grave
         acute up-acute up-mid-low-acute down-acute acute-no-mid up-acute-no-mid down-acute-no-mid all-up-acute
         extern extern-no-mid up-extern up-mid-low-extern up-extern-no-mid down-extern down-extern-no-mid))))
     (setq finalres
          (mapcar #'(lambda (x) 
                        (implode (convert-tule-char-names-to-base-codes (dropnil (reverse x)))))
                  allforms))
     (values finalres up-extern-res)))

;*****************************************************************
; *** all accented letter are replaced by the unstressed one plus a '.
;     The grave accent is replaced by the apostrophe (acute?)
(defun up-extern-accent (root)
  (let (all-res up-ext)
     (multiple-value-setq (all-res up-ext) (final-accent-and-uppercase root))
     up-ext))

; ****************************************************************
; *** substitutes stressed chars occurring in the middle of a word
;     (for Italian and Spanish)
(defun rem-middle-accent (charlist)
  (declare (special *LANGUAGE*))
  (cond ((or (eq *LANGUAGE* 'italian)
             (eq *LANGUAGE* 'spanish))
           (mapcar #'get-char-nostress charlist))
        (t charlist)))

; *******************************************************************
; *** returns true if firstch is a vowel ***********************
(defun is-next-char-vowel (firstch secondch)
  (declare (special *LANGUAGE*))
   (and (member firstch '(#\a #\e #\i #\o #\u #\à #\á #\â #\ã #\ä #\å #\æ
                          #\è #\é #\ê #\ë #\ì #\í #\î #\ï #\ò #\ó #\ô #\õ
                          #\ö #\ù #\ú #\û #\ü #\ø))
        (not (and (eq *LANGUAGE* 'catalan)      ; dyptongs are not vowels
                  (or (and (memq firstch '(#\i #\í #\ï))
                           (memq secondch
                             '(#\a #\à #\e #\è #\é #\o #\ó #\ò #\u #\ú)))
                       (and (memq firstch '(#\u #\ú))
                            (memq secondch
                               '(#\a #\à #\e #\è #\é #\i #\í #\ï #\o #\ó #\ò))))))))

; *******************************************************
; *** explode takes any Lisp symbol or number and produces a list including the
;     lisp characters of the object name.
;     alfa    --> (#\A #\L #\F #\A)
;     |alfa|  --> (#\a #\l #\f #\a)
;     "alf.." --> (#\a #\l #\f #\. #\.)
;     127.3   --> (#\1 #\2 #\7 #\. #\3)
;     #\j     --> (#\j)
(defun explode (a)
 (cond ((numberp a) (coerce (format nil "~a" a) 'list))
       (t (coerce (string a) 'list))))

; *******************************************************
; *** implode takes any list including numbers and Lisp characters and converts it
;     into a single symbol name. Numbers are interpreted as character codes
; *** This function only works if the numbers are interpretable as codes
;     of Lisp characters. So, code-char is left, instead of using the Tule
;     internal character notation
;     (#\A #\L #\F #\A) --> ALFA 
;     (#\a #\l #\f #\a) --> |alfa|
;     (#\a #\l #\f #\. #\.) --> |alf..|
;     (#\1 #\2 #\7 #\. #\3) --> |127.3|
;     (#\j) --> |j|
;     (97 98 99) --> |abc|
;     (97 #\b 99) --> |abc|
; *** with respect to non-ASCII codes, they are anyway composed of a sequence of
;     bytes, each of which is interpreted as a single Lisp char (if in the range 0-255)
;     This means that a sequence ('down-alpha 'down-beta), which corresponds to the encoding
;     (206 177 206 178) is "imploded" to "Î±Î²", which becomes the tule internal name
;     of the word composed of the two characters greek alpha, greek beta.
(defun implode (lista)
  (let ((lis (mapcar #'(lambda (x) (cond ((numberp x) (code-char x)) (t x)))
                    lista)))
      (intern (format nil "~a~{~a~}" (first lis) (rest lis)))))

