;; SRFI-19: Time Data Types and Procedures.
;; 
;; Copyright (C) Neodesic Corporation (2000). All Rights Reserved. 
;; 
;; This document and translations of it may be copied and furnished to others, 
;; and derivative works that comment on or otherwise explain it or assist in its 
;; implementation may be prepared, copied, published and distributed, in whole or 
;; in part, without restriction of any kind, provided that the above copyright 
;; notice and this paragraph are included on all such copies and derivative works. 
;; However, this document itself may not be modified in any way, such as by 
;; removing the copyright notice or references to the Scheme Request For 
;; Implementation process or editors, except as needed for the purpose of 
;; developing SRFIs in which case the procedures for copyrights defined in the SRFI 
;; process must be followed, or as required to translate it into languages other 
;; than English. 
;; 
;; The limited permissions granted above are perpetual and will not be revoked 
;; by the authors or their successors or assigns. 
;; 
;; This document and the information contained herein is provided on an "AS IS" 
;; basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL WARRANTIES, EXPRESS OR 
;; IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTY THAT THE USE OF THE 
;; INFORMATION HEREIN WILL NOT INFRINGE ANY RIGHTS OR ANY IMPLIED WARRANTIES OF 
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. 

;; -- MzScheme implementation
;;
;; The only MzScheme specific features of this implementation is
;; CURRENT-SECONDS, the DEFINE-STRUCT procedure (SRFI 9: Defining Record Types
;; could be used), and the constants tm:time-at-zero-seconds
;; and tm:julian-day-at-zero-seconds, which refer to the '0' of CURRENT-SECONDS.
;; 
;; SRFI-6, String Ports, and SRFI-8, RECEIVE: Binding Multiple Values, 
;; are also used. MzScheme has String Ports built-in. The RECIEVE form
;; is copied below.
;;
; srfi-8: receive
;(require-library "synrule.ss") -- PLT doesn't like DEFINE-SYNTAX. 

;(define-syntax receive
;  (syntax-rules ()
;    ((receive formals expression body ...)
;     (call-with-values (lambda () expression)
;                       (lambda formals body ...)))))

;; -- Multiple helper procedures. TM:xxx procedures are meant to be
;; internal.

(define (tm:leap-year? year)
  (or (= (modulo year 400) 0)
      (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))))

(define  tm:month-assoc '((1 . 31) (2 . 28) (3 . 31) (4 . 30)
			(5 . 31) (6 . 30) (7 . 31) (8 . 31)
			(9 . 30) (10 . 31) (11 . 30) (12 . 31)))

(define (tm:year-day day month year)
  (let ((days (+ day -1
		 (do ( (month-assoc tm:month-assoc (cdr month-assoc))
		       (idays 0 (+ idays (cdar month-assoc))) )
		     ( (or (null? month-assoc)
			   (>= (caar month-assoc) month))
		       (if (null? month-assoc)
			   (error 'tm:year-day "Invalid month specification: ~S" month)
			   idays))))) )
    (if (and (tm:leap-year? year) (> month 2))
	(+ days 1)
	days)))

(define (tm:day-month year-day year)
  (letrec ((tm:day-month* 
	    (lambda (day-accum month-accum)
	      (let* ( (days-this-month (tm:days-this-month month-accum year))
		      (year-day-at-eom (+ day-accum days-this-month)) )
		(cond
		 ((< year-day year-day-at-eom)
		  (values  (- days-this-month (- year-day-at-eom year-day 1))  month-accum))
		 (else (tm:day-month* (+ day-accum days-this-month) (+ month-accum 1))))))))
    (tm:day-month* 0 1)))


(define (tm:days-this-month month year)
    (if (and (tm:leap-year? year) (= month 2))
	29
	(let ((spec (assoc month tm:month-assoc)))
	  (if (not spec)
	      (error 'tm:days-this-month "Invalid month specification: ~S" month)
	      (cdr spec)))))

(define (tm:seconds-this-month month year)
  (* (tm:days-this-month month year) 24 60 60))

(define (tm:days-this-year year)
  (if (tm:leap-year? year) 366 365))

(define (tm:seconds-this-year year)
  (* (tm:days-this-year year) 24 60 60))

(define (tm:day-seconds second minute hour)
  (+ (* 60 60 hour) (* 60 minute) second))

;; from calendar faq       
(define (tm:week-day day month year)
  (let* ((a (quotient (- 14 month) 12))
	 (y (- year a))
	 (m (+ month (* 12 a) -2)))
  (modulo (+ day y (quotient y 4) (- (quotient y 100))
	     (quotient y 400) (quotient (* 31 m) 12))
	  7)))

(define (tm:split-real second)
  (if (integer? second) (values second 0)
      (let ((l (inexact->exact (truncate second))))
	(values l (- second l)))))

(define (tm:local-time-zone-offset)
  (date-time-zone-offset (seconds->date (current-seconds))))

(define-struct time (second minute hour day month year zone-offset))

;; from calendar faq

(define (tm:encode-julian-day-number day month year)
  (let* ((a (quotient (- 14 month) 12))
	 (y (- (+ year 4800) a (if (negative? year) -1  0)))
	 (m (- (+ month (* 12 a)) 3)))
    (+ day
       (quotient (+ (* 153 m) 2) 5)
       (* 365 y)
       (quotient y 4)
       (- (quotient y 100))
       (quotient y 400)
       -32045)))

(define (tm:decode-julian-day-number jdn)
  (let* ((a (+ jdn 32044))
	 (b (quotient (+ (* 4 a) 3) 146097))
	 (c (- a (quotient (* 146097 b) 4)))
	 (d (quotient (+ (* 4 c) 3) 1461))
	 (e (- c (quotient (* 1461 d) 4)))
	 (m (quotient (+ (* 5 e) 2) 153))
	 (y (+ (* 100 b) d -4800 (quotient m 10))))
    (values ; date month year
     (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
     (+ m 3 (* -12 (quotient m 10)))
     (if (>= 0 y) (- y 1) y))
     ))

(define (tm:decode-partial-day fraction)
  (let ((s (* fraction (* 24 60 60))))
    (receive 
     (dseconds remainder) (tm:split-real s)
     (let* ( (hour (quotient dseconds (* 60 60)))
	     (sec-remaining (modulo dseconds (* 60 60)))
	     (minute (quotient sec-remaining 60))
	     (second (modulo sec-remaining 60)))
       (values  (+ second remainder) minute hour)))))
		
(define tm:time-at-zero-seconds (make-time 0 0 0 1 1 1970 0))
 
(define tm:julian-day-at-zero-seconds 
  (tm:encode-julian-day-number 1 1 1970))

;; if we cross the 0 border, we must adjust ...
(define (tm:add-to-years base addition)
  (if (= base 0)
      (error 'tm:add-to-years "Invalid year specification: ~S" base))
  (let ((sum (+ base addition))
	(signum (lambda (n) (cond ((zero? n) 0)
				  ((positive? n) 1)
				  ((negative? n) -1)))))
    (if (= (signum sum) (signum base)) sum
	(+ sum (signum addition)))))

;; sort of like quotient/modulo
(define (tm:days->days+years start-year days)
  (if (positive? days)
      (do ( (this-year start-year (tm:add-to-years this-year 1))
	    (days-left days (- days-left (tm:days-this-year this-year)))
	    (years     0 (+ years 1)) )
	  ((< days-left (tm:days-this-year this-year))
	   (values years days-left)))
      (do ( (this-year start-year (tm:add-to-years this-year -1))
	    (days-left (- days) (- days-left (tm:days-this-year this-year)))
	    (years     0 (+ years 1)) )
	  ((< days-left (tm:days-this-year this-year))
	   (values (- years) (- days-left))))))


(define (current-time) 
  (seconds->time (current-seconds)))


;; readers

; TIME

;(define  time-second date-second) ; real (0 to 61.0)
;(define  time-minute date-minute) ;=> integer (0 to 59)
;(define  time-hour date-hour)     ;   => integer (0 to 23)
;(define  time-day date-day)       ;=> integer (1 to 31) 
;(define  time-month date-month)   ;=> integer (1 to 12) (January=1, etc.)
;(define  time-year date-year)     ;=> integer 
;(define  time-week-day date-week-day) ; => integer (0 to 6) (Sunday=0 to Saturday=6).

(define (time-week-day time)
  (tm:week-day (time-day time) (time-month time) (time-year time)))

;(define  time-year-day date-year-day) ; => integer (0 to 365)
(define (time-year-day time)
  (tm:year-day (time-day time) (time-month time) (time-year time)))

(define (time-day-seconds time)
  (tm:day-seconds (time-second time) 
		  (time-minute-time)
		  (time-hour time)))
		  
;(define  time-zone-offset date-time-zone-offset); => integer

;; part of define-struct 

;; converters. all strings are ISO formats.

(define (seconds->time seconds)
  (let ((time (copy-time tm:time-at-zero-seconds)))
    (set-time-zone-offset! time (tm:local-time-zone-offset))
    (add-seconds! time seconds)
    (add-seconds! time (time-zone-offset time))
    time))
    
(define (copy-time time)
  (make-time (time-second time)
	     (time-minute time)
	     (time-hour time)
	     (time-day time)
	     (time-month time)
	     (time-year time)
	     (time-zone-offset time)))


(define (add-seconds! time seconds)
  (if (>= seconds 0)
      (let ((second (+ (time-second time) seconds))
	    (minute (time-minute time))
	    (hour (time-hour time))
	    (year-day (time-year-day time))
	    (year (time-year time)))
	(receive 
	 (sec-int sec-rem) (tm:split-real second)
	 (if (>= sec-int 60) ; yikes! what about leap seconds?
	     (begin 
	       (set! minute (+ minute (quotient sec-int 60)))
	       (set! sec-int (modulo sec-int 60))))
	 (if (>= minute 60)
	     (begin 
	       (set! hour (+ hour (quotient minute 60)))
	       (set! minute (modulo minute 60))))
	 (if (>= hour 24)
	     (begin 
	       (set! year-day (+ year-day (quotient hour 24)))
	       (set! hour (modulo hour 24))))
	 (if (>= year-day (tm:days-this-year year))
	     (receive (add-yr remaining-year-day)
		      (tm:days->days+years year year-day)
		      (set! year (tm:add-to-years year add-yr))
		      (set! year-day remaining-year-day)))
	 (receive 
	  (d m) (tm:day-month year-day year)
	  (set-time-second! time (+ sec-int sec-rem))
	  (set-time-minute! time minute)
	  (set-time-hour! time hour)
	  (set-time-day! time d)
	  (set-time-month! time m)
	  (set-time-year! time year))))
      (let ((second (+ (time-second time) seconds))
	    (minute (time-minute time))
	    (hour (time-hour time))
	    (year-day (time-year-day time))
	    (year (time-year time)))
	(receive 
	 (sec-int sec-rem) (tm:split-real second)
	 (if (< sec-int 0)
	     (begin 
	       (set! minute (+ minute -1 (quotient sec-int 60)))
	       (set! sec-int (modulo sec-int 60))))
	 (if (< minute 0)
	     (begin 
	       (set! hour (+ hour -1 (quotient minute 60)))
	       (set! minute (modulo minute 60))))
	 (if (< hour 0)
	     (begin 
	       (set! year-day (+ -1 year-day (quotient hour 24)))
	       (set! hour (modulo hour 24))))
	 (if (< year-day 0)
	     (receive (sub-yr remaining-year-day)
		      (tm:days->days+years year year-day)
		      (set! year (tm:add-to-years year sub-yr))
		      (set! year-day remaining-year-day)))
	 (receive 
	  (d m) (tm:day-month year-day year)
	  (set-time-second! time (+ sec-int sec-rem))
	  (set-time-minute! time minute)
	  (set-time-hour! time hour)
	  (set-time-day! time d)
	  (set-time-month! time m)
	  (set-time-year! time year)))))
  time)

(define (add-seconds time seconds)
  (add-seconds! (copy-time time) seconds))
  
(define (time->universal-time! time)
  (begin
    (add-seconds! time (- (time-zone-offset time)))
    (set-time-zone-offset! time 0)
    time))

(define (time->universal-time time)
  (if (= 0 (time-zone-offset time))
      time
      (time->universal-time! (copy-time time))))

(define (julian-day time)
  (+ (tm:encode-julian-day-number 
      (time-day time)
      (time-month time)
      (time-year time))
     (/ (+ (tm:day-seconds (time-second time)
			   (time-minute time)
			   (time-hour time))
	   (- (* 12 60 60)))
	(* 24 60 60))))

(define (modified-julian-day time)
  (- (+ (tm:encode-julian-day-number 
	 (time-day time)
	 (time-month time)
	 (time-year time))
	(/ (tm:day-seconds (time-second time)
			   (time-minute time)
			   (time-hour time))
	   (* 24 60 60)))
     2400001)) 


(define (time-difference time1 time2)
  (if (not (and (time? time1)
		(time? time2)))
      (error 'time-difference "expects parameters of type <time>"))
  (* (- (julian-day time1) (julian-day time2))
     24 60 60))

(define (julian-day->time julian-day)
  (receive 
   (day partial-day) (tm:split-real julian-day)
   (receive 
    (day month year) (tm:decode-julian-day-number day)
    (receive 
     (second minute hour)
     (tm:decode-partial-day partial-day)
     (add-seconds! (make-time second minute hour day month year 0)
		   (* 12 60 60))))))

(define (modified-julian-day->time modified-julian-day)
  (receive 
   (day partial-day) (tm:split-real (+ modified-julian-day 2400001))
   (receive 
    (day month year) (tm:decode-julian-day-number day)
    (receive 
     (second minute hour)
     (tm:decode-partial-day partial-day)
     (make-time second minute hour day month year 0)))))


(define (time->seconds time)
  (if (time<? time tm:time-at-zero-seconds) #f
      (+ (* 24 60 60 (- (tm:encode-julian-day-number (time-day time) 
					      (time-month time)
					      (time-year time))
			tm:julian-day-at-zero-seconds))
	 (* 60 60 (time-hour time))
	 (* 60 (time-minute time))
	 (time-second time)
	 (- (time-zone-offset time)))))

;;-- these could be more elegant perhaps, but on the other hand
;;   they're going to be messy in any case.

(define (time->string time . include-time-zone-offset?)
  (let ((second (time-second time))
	(minute (time-minute time))
	(hour (time-hour time))
	(day (time-day time))
	(month (time-month time))
	(year (time-year time))
	(time-zone-offset (time-zone-offset time))
	(include-time-zone-offset? 
	 (if (null? include-time-zone-offset?) 
	     #t 
	     (car include-time-zone-offset?))))
    (let ((str (open-output-string)))
      (display (quotient year 1000) str)
      (set! year (modulo year 1000))
      (display (quotient year 100) str)
      (set! year (modulo year 100))
      (display (quotient year 10) str)
      (set! year (modulo year 10))
      (display year str)
      ;;
      (display #\- str)
      (display (quotient month 10) str)
      (set! month (modulo month 10))
      (display month str)
      (display #\- str)
      (display (quotient day 10) str)
      (set! day (modulo day 10))
      (display day str)
      (display #\T str)
      (display (quotient hour 10) str)
      (set! hour (modulo hour 10))
      (display hour str)
      (display #\: str)
      (display (quotient minute 10) str)
      (set! minute (modulo minute 10))
      (display minute str)
      (display #\: str)
      (display (quotient second 10) str)
      (set! second (modulo second 10))
      (display second str)
      (if include-time-zone-offset?
	  (begin
	    (display #\Z str)
	    (if (< time-zone-offset 0) 
		(begin 
		  (display #\- str)
		  (set! time-zone-offset (- time-zone-offset)))
		(display #\+ str))
	    (let ((tdz-hour (quotient time-zone-offset (* 60 60)))
		  (tdz-sec  (modulo   time-zone-offset (* 60 60))))
	      (display (quotient tdz-hour 10) str)
	      (set! tdz-hour (modulo tdz-hour 10))
	      (display tdz-hour str)
	      (display #\: str)
	      (display (quotient tdz-sec 10) str)
	      (set! tdz-sec (modulo tdz-sec 10))
	      (display tdz-sec str))))
      (get-output-string str))))


(define (time->date-string time)
  (let ((day (time-day time))
	(month (time-month time))
	(year (time-year time)))
    (let ((str (open-output-string)))
      (display (quotient year 1000) str)
      (set! year (modulo year 1000))
      (display (quotient year 100) str)
      (set! year (modulo year 100))
      (display (quotient year 10) str)
      (set! year (modulo year 10))
      (display year str)
      ;;
      (display #\- str)
      (display (quotient month 10) str)
      (set! month (modulo month 10))
      (display month str)
      (display #\- str)
      (display (quotient day 10) str)
      (set! day (modulo day 10))
      (display day str)
      (get-output-string str))))

(define (time->hour-string time)
  (let ((second (time-second time))
	(minute (time-minute time))
	(hour (time-hour time)))
    (let ((str (open-output-string)))
      (display (quotient hour 10) str)
      (set! hour (modulo hour 10))
      (display hour str)
      (display #\: str)
      (display (quotient minute 10) str)
      (set! minute (modulo minute 10))
      (display minute str)
      (display #\: str)
      (display (quotient second 10) str)
      (set! second (modulo second 10))
      (display second str)
      (get-output-string str))))

(define (string->time string)
  (let ((index 0)
	(len (string-length string))
	(char->int 
	 (lambda (i)
	   (let ((ch (string-ref string i)))
	     (cond
	      ((char=? ch #\0) 0)
	      ((char=? ch #\1) 1)
	      ((char=? ch #\2) 2)
	      ((char=? ch #\3) 3)
	      ((char=? ch #\4) 4)
	      ((char=? ch #\5) 5)
	      ((char=? ch #\6) 6)
	      ((char=? ch #\7) 7)
	      ((char=? ch #\8) 8)
	      ((char=? ch #\9) 9)
	      (else (error "Non-integer character" ch i)))))))
    (let ((accumulate-int
	   (lambda ()
	     (if (or (>= index len)
		     (not (char-numeric? (string-ref string index))))
		 #f
		 (do ((acc (char->int index) (+ (* acc 10) (char->int index))))
		     ((or (>= (+ index 1) len)
			  (not (char-numeric? (string-ref string (+ index 1)))))
		      (begin (set! index (+ index 1)) 
			     acc))
		   (set! index (+ index 1))))))
	  (accumulate-frac
	   (lambda ()
	     (if (or (>= index len)
		     (not (char-numeric? (string-ref string index))))
		 #f
		 (do ((acc (/ (char->int index) 10) (+ acc (/ (char->int index) dix)))
		      (dix 100 (* dix 10)))
		     ((or (>= (+ index 1) len)
			  (not (char-numeric? (string-ref string (+ index 1)))))
		      (begin 
			(set! index (+ index 1)) 
			acc))
		   (set! index (+ index 1)))))))
      (let ((second #f) (minute #f) (hour #f) (day #f) (month #f) (year #f) (tzhour #f) (tzminute #f))
	(set! year (accumulate-int))
	(if (eq? year #f) (error "Invalid ISO time string [year]" index string))
	(set! index (+ index 1))
	(set! month (accumulate-int))
	(if (eq? month #f)(error "Invalid ISO time string [month]" index string))
	(set! index (+ index 1))
	(set! day (accumulate-int))
	(if (eq? day #f) (error "Invalid ISO time string [day]" index string))
	(set! index (+ index 1))
	(set! hour (accumulate-int))
	(if (eq? hour #f) (error "Invalid ISO time string [hour]" index string))
	(set! index (+ index 1))
	(set! minute (accumulate-int))
	(if (eq? minute #f) (error "Invalid ISO time string [minute]" index string))
	(set! index (+ index 1))
	(set! second (accumulate-int))
	(if (eq? second #f) (error "Invalid ISO time string [second]" index string))
	(if (and (< index (string-length string))
		 (char=? (string-ref string index)
			 #\.))
	    (let ((frac #f))
	      (set! index (+ index 1))
	      (set! frac (accumulate-frac))
	      (if (eq? frac #f) (error "Invalid ISO time string [fractional second]" index string)
		  (set! second (+ second frac)))))
	;; now, check for time zone
	(if (and (< index len)
		 (or (char=? (string-ref string index) #\Z)
		     (char=? (string-ref string index) #\z)))
	    (if (>= index len)
		(make-time second minute hour day month year (tm:local-time-zone-offset))
		;; we have a time zone
		(begin
		  (set! index (+ index 1))
		  (if (>= index len)
		      (error "Invalid ISO time string [no time zone offset after Z]" index string))
		  (let ((pm-char (string-ref string index)))
		    (set! index (+ index 1)); skip over +/-
		    (set! tzhour (accumulate-int))
		    (if (and (eq? tzhour #f) (= index len)); must be the end
			(make-time second minute hour day month year (tm:local-time-zone-offset))
			(if (eq? tzhour #f) (error "Invalid ISO time string [time zone hour]" index string)
			    (begin
			      (set! index (+ index 1))
			      (set! tzminute (accumulate-int))
			      (if (eq? tzminute #f) (error "Invalid ISO time string [time zone minute]" index string))
			      (if (char=? pm-char #\-)
				  (make-time second minute hour day month year (- (identity (+ (* tzhour 60 60) (* tzminute 60)))))
				  (if (char=? pm-char #\+)
				      (make-time second minute hour day month year (+ (* tzhour 60 60) (* tzminute 60)))
				      (error "Invalid ISO time string [time zone +/-]" pm-char index string)))))))))
	    (make-time second minute hour day month year (tm:local-time-zone-offset))
	    )))))


;; comparing procedures


(define (tm:same-time-zone-time=? time1 time2) ;  => boolean
  (and (= (time-year time1) (time-year time2))
       (= (time-month time1) (time-month time2))
       (= (time-day time1) (time-day time2))
       (= (time-hour time1) (time-hour time2))
       (= (time-minute time1) (time-minute time2))
       (= (time-second time1) (time-second time2))))

(define (tm:same-time-zone-time>? time1 time2) ;  => boolean
  (or (> (time-year time1) (time-year time2))
      (and 
       (>= (time-year time1) (time-year time2))
       (> (time-year-day time1) (time-year-day time2)))
      (and 
       (>= (time-year time1) (time-year time2))
       (>= (time-year-day time1) (time-year-day time2))
       (> (tm:day-seconds (time-second time1)
			  (time-minute time1)
			  (time-hour  time1))
	  (tm:day-seconds (time-second time2)
			  (time-minute time2)
			  (time-hour  time2))))))

(define (tm:same-time-zone-time<? time1 time2)
   (tm:same-time-zone-time>? time2 time1))

(define (tm:same-time-zone-time>=? time1 time2)
  (or (tm:same-time-zone-time=? time1 time2)
      (tm:same-time-zone-time>? time1 time2)))

(define (tm:same-time-zone-time<=? time1 time2)
  (or (tm:same-time-zone-time=? time1 time2)
      (tm:same-time-zone-time>? time2 time1)))

(define (time=? time1 time2)
  (if (not (and (time? time1)
		(time? time2)))
      (error 'time=? "expects parameters of type <time>"))
  (if (= (time-zone-offset time1)
	 (time-zone-offset time2))
      (tm:same-time-zone-time=? time1 time2)
      (tm:same-time-zone-time=? (time->universal-time time1)
				(time->universal-time time2))))
(define (time>? time1 time2)
  (if (not (and (time? time1)
		(time? time2)))
      (error 'time>? "expects parameters of type <time>"))
  (if (= (time-zone-offset time1)
	 (time-zone-offset time2))
      (tm:same-time-zone-time>? time1 time2)
      (tm:same-time-zone-time>? (time->universal-time time1)
				(time->universal-time time2))))

(define (time<? time1 time2)
  (if (not (and (time? time1)
		(time? time2)))
      (error 'time<? "expects parameters of type <time>"))
  (if (= (time-zone-offset time1)
	 (time-zone-offset time2))
      (tm:same-time-zone-time<? time1 time2)
      (tm:same-time-zone-time<? (time->universal-time time1)
				(time->universal-time time2))))

(define (time>=? time1 time2)
  (if (not (and (time? time1)
		(time? time2)))
      (error 'time>=? "expects parameters of type <time>"))
  (if (= (time-zone-offset time1)
	 (time-zone-offset time2))
      (tm:same-time-zone-time>=? time1 time2)
      (tm:same-time-zone-time>=? (time->universal-time time1)
				 (time->universal-time time2))))

(define (time<=? time1 time2)
  (if (not (and (time? time1)
		(time? time2)))
      (error 'time<=? "expects parameters of type <time>"))
  (if (= (time-zone-offset time1)
	 (time-zone-offset time2))
      (tm:same-time-zone-time<=? time1 time2)
      (tm:same-time-zone-time<=? (time->universal-time time1)
			      (time->universal-time time2))))

