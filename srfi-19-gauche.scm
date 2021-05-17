;;
;; Adapter to load srfi-19.scm into Gacuhe
;;
;; To run the test suite:
;;   $ gosh -I. -usrfi-19-gauche ./srfi-19-test-suite.scm
;;
;; You'll see warnings on redefining setters, since the reference implementation
;; mimics immutable fields by clobbering setters.  You can ignore them.
;;
(define-module srfi-19-gauche
  (use gauche.record)
  (use util.match)
  (export time-tai time-utc time-monotonic time-thread
          time-process time-duration current-time time-resolution
          make-time time? time-type time-second time-nanosecond
          set-time-type! set-time-second! set-time-nanosecond! copy-time
          time=? time<? time<=? time>? time>=?
          time-difference time-difference! add-duration add-duration!
          subtract-duration subtract-duration!
          make-date date? date-nanosecond date-second date-minute
          date-hour date-day date-month date-year date-zone-offset
          date-year-day date-week-day date-week-number current-date
          current-julian-day current-modified-julian-day
          date->julian-day date->modified-julian-day date->time-monotonic
          date->time-tai date->time-utc
          julian-day->date julian-day->time-monotonic
          julian-day->time-tai julian-day->time-utc
          modified-julian-day->date modified-julian-day->time-monotonic
          modified-julian-day->time-tai modified-julian-day->time-utc
          time-monotonic->date time-monotonic->julian-day
          time-monotonic->modified-julian-day
          time-monotonic->time-tai time-monotonic->time-tai!
          time-monotonic->time-utc time-monotonic->time-utc!
          time-utc->date time-utc->julian-day
          time-utc->modified-julian-day
          time-utc->time-monotonic time-utc->time-monotonic!
          time-utc->time-tai time-utc->time-tai!
          time-tai->date time-tai->julian-day
          time-tai->modified-julian-day
          time-tai->time-monotonic time-tai->time-monotonic!
          time-tai->time-utc time-tai->time-utc!
          date->string string->date))
(select-module srfi-19-gauche)

;; Translate define-struct to defne-record.
(define-syntax define-struct
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ name (fields ...) (_))
        (let ((field-specs (map (^[field]
                                  `(,field
                                    ,(symbol-append name '- field)
                                    ,(symbol-append 'set- name '- field '!)))
                                fields)))
          (quasirename r
            `(define-record-type ,name
               ,(symbol-append 'make- name)
               ,(symbol-append name '?)
               ,@field-specs)))]))))

(define (current-seconds) (values-ref (sys-gettimeofday) 0))
(define (current-milliseconds) (values-ref (sys-gettimeofday) 1))

(define (current-process-milliseconds)
  (let* ([times (sys-times)]
         [cpu   (+ (car times) (cadr times))]
         [tick  (list-ref times 4)])
    (* cpu (/ 1e6 tick))))
(define (current-gc-milliseconds) 0) ; used in srfi-19.scm, but not in spec

(include "srfi-19")
