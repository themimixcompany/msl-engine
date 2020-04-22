;;;; tests.lisp

(uiop:define-package #:streams/tests
  (:use #:cl
        #:fiveam
        #:streams/common
        #:streams/parser
        #:streams/unparser
        #:streams/dispatcher)
  (:export #:run-tests))

(in-package #:streams/tests)

(def-suite all-tests)
(in-suite all-tests)

(defun run-tests ()
  "Run all the tests defined in the suite."
  (run! 'all-tests))

(defun extract (expr &optional clear)
  "Return the string representation of EXPR after dispatching it."
  (when clear (clear))
  (dispatch expr)
  (car (collect)))

(defun uncomment (expr)
  "Return a new string from EXPR without the comment."
  (cl-ppcre:regex-replace-all " ?//.*[^)]" expr ""))

(defun extract= (expr &optional value)
  "Return true if EXPR is equivalent to the collected dispatch on itself. If
VALUE is present, compare the collected dispatch against it."
  (let ((expr (uncomment expr)))
    (string= (extract expr t) (or value expr))))

(test parser-tests-1 "Test the values returned by the parser and unparser, without accumulation."
  (is (null (extract= "(@WALT //key only)")))
  (is (extract= "(@WALT Walt Disney //key value)"))
  (is (extract= "(@WALT /wregex/wenv wconsume //key regex)"))
  (is (extract= "(@WALT Walt Disney /wregex/wenv wconsume //key value regex)"))
  (is (extract= "(@WALT [wt1] //key transform)"))
  (is (extract= "(@WALT Walt Disney [wt1] //key value transform)"))
  ;; (is (extract= "(@WALT (f wformat) //key format)"))
  ;; (is (extract= "(@WALT Walt Disney (f wformat) //key value format)" ...))
  ;; (is (extract= "(@WALT (d wtype) //key type)"))
  ;; (is (extract= "(@WALT Walt Disney (d wtype) //key value type)" ...))
  (is (null (extract= "(@WALT :wife //key meta)")))
  (is (extract= "(@WALT :wife Lillian //key meta value)"))
  (is (extract= "(@WALT Walt Disney :wife Lillian //key value meta value)"))
  ;; (is (extract= "(@WALT (f wformat) //key format)" ...))
  ;; (is (extract= "(@WALT (d wtype) //key type)" ...))
  ;; (is (extract= "(@WALT Walt Disney (f wformat) //key value format)" ...))
  ;; (is (extract= "(@WALT Walt Disney (d wtype) //key value type)" ...))
  ;; (is (extract= "(@WALT Walt Disney (d wtype) (f wformat) //key value type format)" ...))
  ;; (is (extract= "(@WALT /wregex/wenv wconsume [wt1] (f wformat) (d wtype) //key regex transform format type)" ...))
  ;; (is (extract= "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) //key value regex transform format type)" ...))
  (is (extract= "(@WALT :wife /lregex/lenv lconsume //key meta regex)"))
  (is (extract= "(@WALT :wife [lt1] //key meta transform)"))
  ;; (is (extract= "(@WALT :wife (f lformat) //key meta format)" ...))
  ;; (is (extract= "(@WALT :wife (d ltype) //key meta type)" ...))
  ;; (is (extract= "(@WALT :wife (d ltype) (f lformat) //key meta type format)" ...))
  ;; (is (extract= "(@WALT :wife Lillian (f lformat) (d ltype) //key meta value format type)" ...))
  ;; (is (extract= "(@WALT :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key meta regex transform format type)"))
  ;; (is (extract= "(@WALT Walt Disney :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value meta regex transform format type)" ...))
  ;; (is (extract= "(@WALT Walt Disney :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value meta value regex transform format type)" ...))
  ;; (is (extract= "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian //key value regex transform format type meta value)" ...))
  ;; (is (extract= "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value regex transform format type meta regex transform format type)" ...))
  ;; (is (extract= "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value regex transform format type meta value regex transform format type)" ...))
  (is (extract= "(@WALT :wife Lillian :birthday 1901 //key meta value meta value)"))
  (is (extract= "(@WALT Walt Disney :wife Lillian :birthday 1901 //key value meta value meta value)"))
  ;; (is (extract= "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) :birthday 1901 /bregex/benv bconsume (d btype) (f bformat) //key value regex transform format type meta value regex transform format type meta value regex type format)" ...))
  )
