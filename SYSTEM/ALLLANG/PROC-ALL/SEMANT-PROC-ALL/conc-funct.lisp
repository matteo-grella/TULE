(in-package "USER")

(defvar *MONTHDATA*
; *** for each month, the quadruple contains:
;     - the month number
;     - the month length
;     - the month length in bixestile years
;     - the distance from the beginning of the year to the first day of the month
;     - the distance from the first day of the month to the end of the year
;     - the same as above in case of bixestile years
  '((1 31 31 0 0 31 31 365 366)
    (2 28 29 31 31 59 60 334 335)
    (3 31 31 59 60 90 91 306 306)
    (4 30 30 90 91 120 121 275 275)
    (5 31 31 120 121 151 152 245 245)
    (6 30 30 151 152 181 182 214 214)
    (7 31 31 181 182 212 213 184 184)
    (8 31 31 212 213 243 244 153 153)
    (9 30 30 243 244 273 274 122 122)
    (10 31 31 273 274 304 305 92 92)
    (11 30 30 304 305 334 335 61 61)
    (12 31 31 334 335 365 366 31 31)))

; &&&&&&&&&&&&&&&& THESE FUNCTIONS ARE USED INSIDE THE MAPPING TO THE
; &&&&&&&&&&&&&&&& BACKEND

; ***********************************************
(defun get-next-value (prevquery eq-spec currconc)
  ; *** eq-spec should be a pair (eq value)
   (cond ((eq (first eq-spec) 'eq)
            (second eq-spec))
         ((eq (first (first eq-spec)) 'eq)
            (second (first eq-spec)))
         (t (exception 'semantic-error
                       "PROC/conc-funct: get-next-spec" (first eq-spec)))))

; &&&&&&&&&&&&&&&&& THE TWO MAIN FUNCTIONS CONVERT THE STANDARD &&&&&&&&&&&&
; &&&&&&&&&&&&&&&&& REPRESENTATION OF A DAY (DAY+MONTH+YEAR) &&&&&&&&&&&&&&&
; &&&&&&&&&&&&&&&&& INTO AN INTEGER VALUE REPRESENTING THE SEQUENCE &&&&&&&&
; &&&&&&&&&&&&&&&&& NUMBER OF THE DAY STARTING FROM THE FIRST OF &&&&&&&&&&&
; &&&&&&&&&&&&&&&&& JANUARY OF YEAR 0 (AND VICEVERSA) &&&&&&&&&&&&&&&&&&&&&&

; **************************************************************************
; *** the current day is converted into an absolute number representing
;     the day number starting from january 1st, year 0 a.d.
; !!!!!!! N.B. Years are assumed to start from 0, so the 1st of january
;              of year 0 corresponds to the absolute value 0.
;         N.B.2 Possibly, the function works also for negative years,
;              but I did not check it out
(defun get-absolute-day (day month year)
  (declare (special *MONTHDATA*))
  (cond ((check-date-descr day month year)
           (+ (* 365 year)	                    ; *** days of standard years
              (floor (/ ( + 3 year) 4))           ; *** extra days for bixestile
     ; *** now, we are at the beginning of the current year
              (fourth (assoc month *MONTHDATA*))  ; *** from the beginning of the year
                                            ;     to the beginning of this month
              (cond ((and (eq 0 (rem year 4))     ; *** a possible extra day if we are
                          (> month 2))            ;     in a bixestile year and in a 
                       1)                         ;     month after february
                    (t 0))
              (1- day)))
        (t (exception 'semantic-error 
                      "PROC/conc-funct: Incorrect date: "
                      (list day month year)))))

; ********************************************************************
; *** checks if a date is correct (month between 1 and 12 and day included
;    in the number of days of that month)
(defun check-date-descr (day month year)
  (declare (special *MONTHDATA*))
  (cond ((and (> month 0) (< month 13))
         (let ((actmonth (assoc month *MONTHDATA*)))
             (cond ((eq 0 (rem year 4))
                     (and (> day 0) (<= day (third actmonth))))
                   (t (and (> day 0) (<= day (second actmonth)))))))
        (t nil)))
        
; **************************************************************************
; *** an absolute day (see above) is converted into a <day, month, year> triple
(defun get-standard-day (absolute-day-expr)
   (let ((year-div-4 (floor (/ absolute-day-expr 1461)))
         (rem-day-div-4 (rem absolute-day-expr 1461))
         year-in-4-tuple day-in-year day month year)
  ; *** 1461 is the number of days included in a four-tuple of years with
  ;     a bixestile one
      (cond ((< rem-day-div-4 366) 
               (setq year-in-4-tuple 0)
               (setq day-in-year rem-day-div-4))
            ((< rem-day-div-4 731)
               (setq year-in-4-tuple 1)
               (setq day-in-year (- rem-day-div-4 366)))
            ((< rem-day-div-4 1096)
               (setq year-in-4-tuple 2)
               (setq day-in-year (- rem-day-div-4 731)))
            (t (setq year-in-4-tuple 3)
               (setq day-in-year (- rem-day-div-4 1096))))
      (setq year 
          (+ (* 4 year-div-4) year-in-4-tuple))     ; *** get the year number
      (multiple-value-setq (month day)
          (find-month-day day-in-year (= 0 year-in-4-tuple)))
      (values day month year)))

; ***********************************************
; *** given the day position in the year, returns a pair <month day-in-month>
;  INPUT:
;     day-in-year: the day number inside the year (0 is January 1st)
;     bixest?: t, if the year in question is bixestile. False, otherwise
(defun find-month-day (day-in-year bixest?)
  (let ((prev-end 0))
     (do ((nxtmonth (first *MONTHDATA*) (first locmonthdata))
          (locmonthdata (rest *MONTHDATA*) (rest locmonthdata)))
         ((< day-in-year
             (cond (bixest? (seventh nxtmonth))
                   (t (sixth nxtmonth))))
            (values (first nxtmonth) (1+ (- day-in-year prev-end))))
        (setq prev-end 
             (cond (bixest? (seventh nxtmonth))
                   (t (sixth nxtmonth)))))))

; ***********************************************
; *** returns the standard day value for the day which is n day
;     after today
(defun get-nth-future-mday (n)
  (let (second minute hour month-day month year x y z
        nth-day nth-month nth-year)
     (multiple-value-setq  
           (second minute hour month-day month year x y z)
           (get-decoded-time))
     (multiple-value-setq (nth-day nth-month nth-year)
        (get-standard-day (+ n (get-absolute-day month-day month year))))
     nth-day))

(defun get-nth-future-month (n)
  (let (second minute hour month-day month year x y z
        nth-day nth-month nth-year)
     (multiple-value-setq  
           (second minute hour month-day month year x y z)
           (get-decoded-time))
     (multiple-value-setq (nth-day nth-month nth-year)
        (get-standard-day (+ n (get-absolute-day month-day month year))))
     nth-month))

(defun get-nth-future-year (n)
  (let (second minute hour month-day month year x y z
        nth-day nth-month nth-year)
     (multiple-value-setq  
           (second minute hour month-day month year x y z)
           (get-decoded-time))
     (multiple-value-setq (nth-day nth-month nth-year)
        (get-standard-day (+ n (get-absolute-day month-day month year))))
     nth-year))

; ***********************************************
; *** functions for TODAY 
(defun get-today-date ()
   (make-out-date (get-today-mday) (get-today-month) (get-today-year)))

(defun get-today-mday ()
   (get-nth-future-mday 0))

(defun get-today-month ()
   (get-nth-future-month 0))

(defun get-today-year ()
   (get-nth-future-year 0))

; ***********************************************
; *** functions for TOMORROW 
(defun get-tomorrow-date ()
   (make-out-date (get-tomorrow-mday) (get-tomorrow-month) (get-tomorrow-year)))

(defun get-tomorrow-mday ()
   (get-nth-future-mday 1))

(defun get-tomorrow-month ()
   (get-nth-future-month 1))

(defun get-tomorrow-year ()
   (get-nth-future-year 1))

; ***********************************************
; *** functions for (next) WEEKDAYS
; *** It is assumed that if the user says "friday", s/he means
;     "The next friday after now"
(defun get-next-wday (daypos)
; *** daypos is the day mentioned by the user:
;     0 - monday
;     1 - tuesday
;     2 - wednesday
;     3 - thursday
;     4 - friday
;     5 - saturday
;     6 - sunday
  (let (second minute hour month-day month year weekday y z distance)
     (multiple-value-setq  
           (second minute hour month-day month year weekday y z)
           (get-decoded-time))
      ; *** the weekday is numbered 0 to 6, 0 being monday
     (setq distance (- daypos weekday))
     (cond ((>= 0 distance) 
              (setq distance (+ distance 7))))
     (make-out-date 
           (get-nth-future-mday distance)
           (get-nth-future-month distance)
           (get-nth-future-year distance))))

; ***********************************************
; *** functions for AFTER-TOMORROW 
(defun get-after-tomorrow-mday ()
   (get-nth-future-mday 2))

; ***********************************************
; *** THE NEXT SIX FUNCTIONS GET THE EXTREMES OF A TIME INTERVAL
;     (A WEEK, A MONTH) REFERRED TO AS "THE NEXT". THE RESULT IS
;     EXPRESSED AS MIN AND MAX VALUES OF STANDARD DAY REPRESENTATIONS
; ***********************************************
; 
; **************************************************************
; *** gets the date (in standard format) of the beginning of
;     "next period", where "period" can be "week", "month" or "year"
; *** The reference period is determined on the basis of the concepts
;     appearing in the semantic query
(defun get-next-min-date (prevpath restquery currconc)
 (let (second minute hour month-day today-month today-year weekday y z
       day month year)
  (multiple-value-setq  
           (second minute hour month-day today-month today-year weekday y z)
           (get-decoded-time))
  (cond ((member '££week (append prevpath (list currconc) restquery))
           (multiple-value-setq (day month year)
                   (get-standard-day (get-start-day-of-next-week)))
           (make-out-date day month year))
        ((member '££month (append prevpath (list currconc) restquery))
           (make-out-date 
              1      				; first 
              (1+ (rem today-month 12))		; next month
              (cond ((= 1 (1+ (rem today-month 12))) (1+ today-year))
                    (t today-year))))		; this or next year
        ((member '££year (append prevpath (list currconc) restquery))
           (make-out-date 
              1 				; first
              1					; january
              (1+ today-year)))			; next year
        (t (exception 'semantic-error 
                 "Unknown deictic time descr: conc-funct.lisp")))))

; **************************************************************
; *** gets the date (in standard format) of the end of
;     "next period", where "period" can be "week", "month" or "year"
; *** The reference period is determined on the basis of the concepts
;     appearing in the semantic query
(defun get-next-max-date (prevpath restquery currconc)
 (let (second minute hour month-day today-month today-year weekday y z
       day month year)
  (multiple-value-setq  
           (second minute hour month-day today-month today-year weekday y z)
           (get-decoded-time))
  (cond ((member '££week (append prevpath (list currconc) restquery))
           (multiple-value-setq (day month year)
                   (get-standard-day (get-end-day-of-next-week)))
           (make-out-date day month year))
        ((member '££month (append prevpath (list currconc) restquery))
           (make-out-date 
                   ; *** last day of the next month (depends on month)
                   ;     this is taken from the *MONTHDATA* table
              (cond ((= 0 (rem today-year 4))
                      (third (assoc (1+ (rem today-month 12)) *MONTHDATA*)))
                    (t (second (assoc (1+ (rem today-month 12)) *MONTHDATA*))))
              (1+ (rem today-month 12))		; next month
              (cond ((= 1 (1+ (rem today-month 12))) (1+ today-year))
                    (t today-year))))		; this or next year
        ((member '££year (append prevpath (list currconc) restquery))
           (make-out-date 
              31 				; 31st
              12				; december
              (1+ today-year)))			; next year
        (t (exception 'semantic-error 
                 "Unknown deictic time descr: conc-funct.lisp")))))

; **************************************************************
; *** gets the date (in standard format) of the end of
;     "this period", where "period" can be "week", "month" or "year"
; *** The reference period is determined on the basis of the concepts
;     appearing in the semantic query
(defun get-this-max-date (prevpath restquery currconc)
 (let (second minute hour month-day today-month today-year weekday y z
       day month year)
  (multiple-value-setq  
           (second minute hour month-day today-month today-year weekday y z)
           (get-decoded-time))
  (cond ((member '££week (append prevpath (list currconc) restquery))
           (multiple-value-setq (day month year)
                   (get-standard-day (- (get-end-day-of-next-week) 7)))
           (make-out-date day month year))
        ((member '££month (append prevpath (list currconc) restquery))
           (make-out-date 
                   ; *** last day of the this month (depends on month)
                   ;     this is taken from the *MONTHDATA* table
              (cond ((= 0 (rem today-year 4))
                      (third (assoc today-month *MONTHDATA*)))
                    (t (second (assoc today-month *MONTHDATA*))))
              today-month 			; this month
              today-year))			; this year
        ((member '££year (append prevpath (list currconc) restquery))
           (make-out-date 
              31 				; 31st
              12				; december
              today-year))			; this year
        (t (exception 'semantic-error 
                 "Unknown deictic time descr: conc-funct.lisp")))))

; **************************************************************
; *** gets the absolute day number of the beginning of next week
(defun get-start-day-of-next-week ()
  ; *** "next week" is assumed to mean the first period monday-sunday
  ;     starting after today. If today is sunday, "next week" means
  ;     "from tomorrow to next sunday". If today is monday, "next
  ;     week" means from next monday to the following sunday
  (let (dist-to-monday second minute hour month-day month year weekday y z)
     (multiple-value-setq  
           (second minute hour month-day month year weekday y z)
           (get-decoded-time))
     (setq dist-to-monday (- 7 weekday))
      ; *** the weekday is numbered 0 to 6, 0 being monday
      ;     So, the distance to next monday is 7 if today is monday,
      ;     6 if today is tuesday, ..., 1 if today is sunday
     (+ dist-to-monday (get-absolute-day month-day month year))))

; **************************************************************
; *** gets the absolute day number of the end of next week
;     "next week" is defined as in the function above
(defun get-end-day-of-next-week ()
     (+ 6 (get-start-day-of-next-week)))

; ***********************************************
; *** THE FOUR NEXT FUNCTIONS EVALUATE A STANDARD DAY DESCRIPTION
; *** ON THE BASIS OF A WEEKDAY NAME
; *** The input is, for instance, "next tuesday"
; ***********************************************

; *** this is a general function, that returns the absolute day code
;     for the next "weekday". The value of "weekday" is extracted
;     from the ontological query
(defun get-next-weekday (prevpath restquery currconc)
  (let (second minute hour month-day month year weekday y z
        (wholequery (append prevpath (list currconc) restquery))
        next-weekday dist-from-today)
      ; *** the next sets the values of today (in particular "weekday")
     (multiple-value-setq
           (second minute hour month-day month year weekday y z)
           (get-decoded-time))
      ; *** the weekday is numbered 0 to 6, 0 being monday
    (setq next-weekday
        (cond ((member '££monday wholequery) 0)
              ((member '££tuesday wholequery) 1)
              ((member '££wednesday wholequery) 2)
              ((member '££thursday wholequery) 3)
              ((member '££friday wholequery) 4)
              ((member '££saturday wholequery) 5)
              ((member '££sunday wholequery) 6)
              (t 0)))
    (setq dist-from-today (- next-weekday weekday))
      ; *** if one uses a day name (e.g. tuesday) and today is that day, then
      ;     it is assumed that s/he refers to that day in the next week
    (cond ((< dist-from-today 1)
             (setq dist-from-today (+ 7 dist-from-today))))
      ; *** return the absolute day count for the found day
    (+ dist-from-today (get-absolute-day month-day month year))))
  
; ***********************************************
; *** returns the in-month day of the day obtained by the previous function
(defun get-next-time-val-day (prevpath restquery currconc)
  (let (month-day month year)
    (multiple-value-setq (month-day month year)
       (get-standard-day (get-next-weekday prevpath restquery currconc)))
    month-day))

; ***********************************************
; *** returns the month name of the day obtained by the previous function
(defun get-next-time-val-month (prevpath restquery currconc)
  (let (month-day month year)
    (multiple-value-setq (month-day month year)
       (get-standard-day (get-next-weekday prevpath restquery currconc)))
    month))

; ***********************************************
; *** returns the year of the day obtained by the previous function
(defun get-next-time-val-year (prevpath restquery currconc)
  (let (month-day month year)
    (multiple-value-setq (month-day month year)
       (get-standard-day (get-next-weekday prevpath restquery currconc)))
    year))

; ****************************************************************
; *** this converts a day, month and year in the output format
(defun make-out-date (day month year)
    (implode 
       (append 
          (explode year)
          (add-zero (explode month))
          (add-zero (explode day)))))
   ; **** old format
   ;   (implode (append day '(#\/) month '(#\/) year))

; **********************************************
; *** adds a trailing zero in case we have a single digit
(defun add-zero (chars)
  (cond ((= 1 (length chars))
           (cons #\0 chars))
        (t chars)))


