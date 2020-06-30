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
  "Return the string representation of EXPR after dispatching it. If optional argument CLEAR is true, clear the universe prior to evaluation."
  (when clear (clear))
  (dispatch expr nil)
  (recall-expr expr))

(defun eqv (expr &optional value)
  "Test equivalence without value accumulation"
  (declare (ignorable value))
  (when-let* ((e (uncomment expr))
              (v (extract e t)))
    (string= v (or e value))))

(defun eqv* (expr value)
  "Test equivalence with value accumulation"
  (declare (ignorable value))
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
  ;;;(is (eqv "(@WALT (f wformat) //key format)"))
  (is (eqv "(@WALT Walt Disney (f wformat) //key value format)"))
  ;;;(is (eqv "(@WALT (d wtype) //key type)"))
  (is (eqv "(@WALT Walt Disney (d wtype) //key value type)"))

  ;; work on this
  ;;(is (null (eqv "(@WALT :wife //key meta)")))

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

;; (test parser-tests-2 "Test the system with value accumulation."
;;   ;;(is (eqv "(@WALT //key only)"))

;;   (is (eqv* "(@WALT Walt Disney //key value)"
;;             "(@WALT Walt Disney)"))

;;   (is (eqv* "(@WALT /wregex/wenv wconsume //key regex)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume)"))

;;   (is (eqv "(@WALT Walt Disney /wregex/wenv wconsume //key value regex)"
;;            "(@WALT Walt Disney /wregex/wenv wconsume)"))

;;   (is (eqv* "(@WALT [wt1] //key transform)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1])"))

;;   (is (eqv* "(@WALT Walt Disney [wt1] //key value transform)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1])"))

;;   (is (eqv* "(@WALT (f wformat) //key format)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat))"))

;;   (is (eqv* "(@WALT Walt Disney (f wformat) //key value format)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat))"))

;;   (is (eqv* "(@WALT (d wtype) //key type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype))"))

;;   (is (eqv* "(@WALT Walt Disney (d wtype) //key value type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype))"))

;;   ;;(is (null (eqv "(@WALT :wife //key meta)")))

;;   (is (eqv* "(@WALT :wife Lillian //key meta value)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian)"))

;;   (is (eqv* "(@WALT Walt Disney :wife Lillian //key value meta value)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian)"))

;;   (is (eqv* "(@WALT Walt Disney (f wformat) //key value format)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian)"))

;;   (is (eqv* "(@WALT Walt Disney (d wtype) //key value type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian)"))

;;   (is (eqv* "(@WALT Walt Disney (d wtype) (f wformat) //key value type format)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian)"))

;;   (is (eqv* "(@WALT /wregex/wenv wconsume [wt1] (f wformat) (d wtype) //key regex transform format type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian)"))

;;   (is (eqv* "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) //key value regex transform format type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian)"))

;;   (is (eqv* "(@WALT :wife /lregex/lenv lconsume //key meta regex)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume)"))

;;   (is (eqv* "(@WALT :wife [lt1] //key meta transform)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1])"))

;;   (is (eqv* "(@WALT :wife (f lformat) //key meta format)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat))"))

;;   (is (eqv* "(@WALT :wife (d ltype) //key meta type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype))"))

;;   (is (eqv* "(@WALT :wife (d ltype) (f lformat) //key meta type format)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype))"))

;;   (is (eqv* "(@WALT :wife Lillian (f lformat) (d ltype) //key meta value format type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype))"))

;;   (is (eqv* "(@WALT :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key meta regex transform format type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype))"))

;;   (is (eqv* "(@WALT Walt Disney :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value meta regex transform format type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype))"))

;;   (is (eqv* "(@WALT Walt Disney :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value meta value regex transform format type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype))"))

;;   (is (eqv* "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian //key value regex transform format type meta value)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype))"))

;;   (is (eqv* "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value regex transform format type meta regex transform format type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype))"))

;;   (is (eqv* "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) //key value regex transform format type meta value regex transform format type)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype))"))

;;   (is (eqv* "(@WALT :wife Lillian :birthday 1901 //key meta value meta value)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) :birthday 1901)"))

;;   (is (eqv* "(@WALT Walt Disney :wife Lillian :birthday 1901 //key value meta value meta value)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) :birthday 1901)"))

;;   (is (eqv* "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) :birthday 1901 /bregex/benv bconsume (d btype) (f bformat) //key value regex transform format type meta value regex transform format type meta value regex type format)"
;;             "(@WALT Walt Disney /wregex/wenv wconsume [wt1] (f wformat) (d wtype) :wife Lillian /lregex/lenv lconsume [lt1] (f lformat) (d ltype) :birthday 1901 /bregex/benv bconsume (d btype) (f bformat))")))

(defun* run-tests ()
  "Run all the tests defined in the suite."
  ;;(run! 'all-tests)
  (let ((streams:*universe* (make-universe)))
    (run! 'parser-tests-1)
    (clear-universe)
    ;;(run! 'parser-tests-2)
    ))
