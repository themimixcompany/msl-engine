;;;; streams.asd

#-ASDF3.1 (error "ASDF 3.1 or bust!")

(defpackage #:streams-system
  (:use #:cl #:asdf))

(in-package #:streams-system)

(defsystem #:streams
  :description "streams"
  :author "The Mimix Company <code@mimix.io>"
  :license "BlueOak-1.0.0"
  :version "1.4.1"
  :class :package-inferred-system
  :depends-on (#:cl-ppcre
               #:mof
               #:clack
               #:clack-handler-hunchentoot
               #:websocket-driver
               #:alexandria
               #:bordeaux-threads
               #:closer-mop
               #:named-readtables
               "streams/common"
               "streams/reader"
               "streams/ethers"
               "streams/channels"
               "streams/core"
               "streams/serve"
               "streams/build"
               "streams/initialize")
  ;; :in-order-to ((test-op (test-op "streams/tests")))
  )

;; (defsystem #:streams-tests
;;   :description "streams-tests"
;;   :author "The Mimix Company <code@mimix.io>"
;;   :license "BlueOak-1.0.0"
;;   :version "1.0.0"
;;   :class :package-inferred-system
;;   :depends-on (#:streams
;;                #:fiveam
;;                ;; "streams/tests"
;;                )
;;   ;; :perform (test-op (o s) (uiop:symbol-call :fiveam :run! 'streams/tests:all-tests))
;;   )
