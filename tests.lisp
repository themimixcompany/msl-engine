;;;; tests.lisp

(uiop:define-package #:streams/tests
  (:use #:cl
        #:fiveam
        #:streams/classes
        #:streams/common
        #:streams/parser
        #:streams/unparser
        #:streams/dispatcher
        #:streams/json
        #:marie))

(in-package #:streams/tests)

(def-suite all-tests)
(in-suite all-tests)

(defun extract (expr &optional clear)
  "Return the string representation of EXPR after dispatching it. If optional argument CLEAR is true, clear the universe prior to evaluation."
  (when clear (clear))
  (dispatch expr :log nil)
  (recall-expr expr :dispatch nil))

(defun eqv (expr)
  "Test equivalence without value accumulation"
  (when-let* ((e (uncomment expr))
              (v (extract e t)))
    (string= v e)))

(defun eqv* (expr value)
  "Test equivalence with value accumulation"
  (when-let* ((e (uncomment expr))
              (v (extract e nil)))
    (string= v value)))

(test parser-tests-1 "Test the system without value accumulation."
  (is (null (eqv "(@WALT //key only)")))
  (is (eqv "(@WALT Walt Disney //key value)"))
  (is (eqv "(@WALT /wregex/wenv wconsume //key regex)"))
  (is (eqv "(@WALT Walt Disney /wregex/wenv wconsume //key value regex)"))
  (is (eqv "(@WALT [wt1] //key transform)"))
  (is (eqv "(@WALT Walt Disney [wt1] //key value transform)"))
  (is (eqv "(@WALT Walt Disney (f wformat) //key value format)"))
  (is (eqv "(@WALT Walt Disney (d wtype) //key value type)"))
  (is (null (eqv "(@WALT :wife //key meta)")))
  (is (eqv "(@WALT :wife Lillian //key meta value)"))
  (is (eqv "(@WALT Walt Disney :wife Lillian //key value meta value)"))
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

(test parser-tests-2 "Test the system with value accumulation.")

(defun* run-tests ()
  "Run all the tests defined in the suite."
  ;; (run! 'all-tests)
  (with-fresh-universe                  ;this may affect cumulative tests
    (run! 'parser-tests-1)
    (clear-universe)))
