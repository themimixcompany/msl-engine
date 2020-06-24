;;;; tests.lisp

(uiop:define-package #:streams/tests
  (:use #:cl
        #:fiveam
        #:streams/classes
        #:streams/common
        #:streams/parser
        #:streams/unparser
        #:streams/dispatcher
        #:marie))

(in-package #:streams/tests)

(def-suite all-tests)
(in-suite all-tests)

(defun uncomment (expr)
  "Return a new string from EXPR without the comment."
  (cl-ppcre:regex-replace-all " ?//.*[^)]" expr ""))

(defun extract (expr &optional clear)
  "Return the string representation of EXPR after dispatching it. If optional
argument CLEAR is true, clear the universe prior to evaluation."
  (when clear (clear))
  (dispatch* expr nil)
  (car (collect)))

(defun %eqv (expr value flag)
  "Return true if EXPR is equivalent to the collected dispatch on itself. If VALUE is present, compare the collected dispatch against it. If FLAG is true, the universe is cleared prior to the evaluation of EXPR."
  (let ((expr (uncomment expr)))
    (string= (extract expr flag) (or expr value))))

(defun eqv (expr &optional value)
  "Call %EQV without value accumulation"
  (let* ((e (uncomment expr))
         (v (extract expr t)))
    (string= v (or e value))))

(defun eqv* (expr value)
  "Call %EQV with value accumulation"
  (let* ((e (uncomment expr))
         (v (extract e nil)))
    (string= v value)))

(test parser-tests-1 "Test the values returned by the parser and unparser, without value accumulation."
  (is (eqv "(@WALT //key only)"))
  (is (eqv "(@WALT Walt Disney //key value)"))
  (is (eqv "(@WALT /wregex/wenv wconsume //key regex)"))
  (is (eqv "(@WALT Walt Disney /wregex/wenv wconsume //key value regex)"))
  (is (eqv "(@WALT [wt1] //key transform)"))
  (is (eqv "(@WALT Walt Disney [wt1] //key value transform)"))
  (is (eqv "(@WALT (f wformat) //key format)"))
  (is (eqv "(@WALT Walt Disney (f wformat) //key value format)"))
  (is (eqv "(@WALT (d wtype) //key type)"))
  (is (eqv "(@WALT Walt Disney (d wtype) //key value type)"))
  ;;(is (null (eqv "(@WALT :wife //key meta)")))
  (is (eqv "(@WALT :wife Lillian //key meta value)"))
  (is (eqv "(@WALT Walt Disney :wife Lillian //key value meta value)"))
  (is (eqv "(@WALT (f wformat) //key format)"))
  (is (eqv "(@WALT (d wtype) //key type)"))
  (is (eqv "(@WALT Walt Disney (f wformat) //key value format)"))
  (is (eqv "(@WALT Walt Disney (d wtype) //key value type)"))
  (is (eqv "(@WALT Walt Disney (d wtype) (f wformat) //key value type format)"))
  (is (eqv "(@WALT /wregex/wenv wconsume [wt1] (f wformat) (d wtype) //key regex transform format type)"))
  (is (eqv "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) //key value regex transform format type)"))
  (is (eqv "(@WALT :wife /lregex/lenv lconsume //key meta regex)"))
  (is (eqv "(@WALT :wife [lt1] //key meta transform)"))
  (is (eqv "(@WALT :wife (f lformat) //key meta format)"))
  (is (eqv "(@WALT :wife (d ltype) //key meta type)"))
  (is (eqv "(@WALT :wife (d ltype) (f lformat) //key meta type format)"))
  (is (eqv "(@WALT :wife Lillian (f lformat) (d ltype) //key meta value format type)"))
  (is (eqv "(@WALT :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key meta regex transform format type)"))
  (is (eqv "(@WALT Walt Disney :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value meta regex transform format type)"))
  (is (eqv "(@WALT Walt Disney :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value meta value regex transform format type)"))
  (is (eqv "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian //key value regex transform format type meta value)"))
  (is (eqv "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value regex transform format type meta regex transform format type)"))
  (is (eqv "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value regex transform format type meta value regex transform format type)"))
  (is (eqv "(@WALT :wife Lillian :birthday 1901 //key meta value meta value)"))
  (is (eqv "(@WALT Walt Disney :wife Lillian :birthday 1901 //key value meta value meta value)"))
  (is (eqv "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) :birthday 1901 /bregex/benv bconsume (d btype) (f bformat) //key value regex transform format type meta value regex transform format type meta value regex type format)")))

(test parser-tests-2 "Test the values returned by the parser and unparser, with value accumulation."
  ;;(is (eqv "(@WALT //key only)"))

  (is (eqv* "(@WALT Walt Disney //key value)"
            "(@WALT Walt Disney)"))

  (is (eqv* "(@WALT /wregex/wenv wconsume //key regex)"
            "(@WALT Walt Disney /wregex/wenv wconsume)"))

  (is (eqv "(@WALT Walt Disney /wregex/wenv wconsume //key value regex)"
           "(@WALT Walt Disney /wregex/wenv wconsume)"))

  (is (eqv* "(@WALT [wt1] //key transform)"
            "(@WALT Walt Disney /wregex/wenv wconsume [wt1])"))

  (is (eqv* "(@WALT Walt Disney [wt1] //key value transform)"
            "(@WALT Walt Disney /wregex/wenv wconsume [wt1])"))

  (is (eqv* "(@WALT (f wformat) //key format)"
            "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat))"))

  (is (eqv* "(@WALT Walt Disney (f wformat) //key value format)"
            "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat))"))

  (is (eqv* "(@WALT (d wtype) //key type)"
            "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype))"))

  (is (eqv* "(@WALT Walt Disney (d wtype) //key value type)"
            "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype))"))

  ;;(is (null (eqv "(@WALT :wife //key meta)")))

  ;; (is (eqv* "(@WALT :wife Lillian //key meta value)"))
  ;; (is (eqv* "(@WALT Walt Disney :wife Lillian //key value meta value)"))
  ;; (is (eqv* "(@WALT (f wformat) //key format)"))
  ;; (is (eqv* "(@WALT (d wtype) //key type)"))
  ;; (is (eqv* "(@WALT Walt Disney (f wformat) //key value format)"))
  ;; (is (eqv* "(@WALT Walt Disney (d wtype) //key value type)"))
  ;; (is (eqv* "(@WALT Walt Disney (d wtype) (f wformat) //key value type format)"))
  ;; (is (eqv* "(@WALT /wregex/wenv wconsume [wt1] (f wformat) (d wtype) //key regex transform format type)"))
  ;; (is (eqv* "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) //key value regex transform format type)"))
  ;; (is (eqv* "(@WALT :wife /lregex/lenv lconsume //key meta regex)"))
  ;; (is (eqv* "(@WALT :wife [lt1] //key meta transform)"))
  ;; (is (eqv* "(@WALT :wife (f lformat) //key meta format)"))
  ;; (is (eqv* "(@WALT :wife (d ltype) //key meta type)"))
  ;; (is (eqv* "(@WALT :wife (d ltype) (f lformat) //key meta type format)"))
  ;; (is (eqv* "(@WALT :wife Lillian (f lformat) (d ltype) //key meta value format type)"))
  ;; (is (eqv* "(@WALT :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key meta regex transform format type)"))
  ;; (is (eqv* "(@WALT Walt Disney :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value meta regex transform format type)"))
  ;; (is (eqv* "(@WALT Walt Disney :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value meta value regex transform format type)"))
  ;; (is (eqv* "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian //key value regex transform format type meta value)"))
  ;; (is (eqv* "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value regex transform format type meta regex transform format type)"))
  ;; (is (eqv* "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value regex transform format type meta value regex transform format type)"))
  ;; (is (eqv* "(@WALT :wife Lillian :birthday 1901 //key meta value meta value)"))
  ;; (is (eqv* "(@WALT Walt Disney :wife Lillian :birthday 1901 //key value meta value meta value)"))
  ;; (is (eqv* "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) :birthday 1901 /bregex/benv bconsume (d btype) (f bformat) //key value regex transform format type meta value regex transform format type meta value regex type format)"))
  )

(defun* run-tests ()
  "Run all the tests defined in the suite."
  ;;(run! 'all-tests)
  (let ((streams:*universe* (make-universe)))
    (run! 'parser-tests-1)
    (clear-universe)
    (run! 'parser-tests-2)))
