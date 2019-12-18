;;;; packages.lisp

(defpackage #:streams/core
  (:use #:cl)
  (:export #:start-websocket-server
           #:stop-websocket-server
           #:main
           #:build))

(defpackage #:streams/tests
  (:use #:cl
        #:streams/core))
