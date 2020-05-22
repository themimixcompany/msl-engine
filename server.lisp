;;;; server.lisp

(uiop:define-package #:streams/server
  (:use #:cl
        #:websocket-driver
        #:streams/specials
        #:streams/parser
        #:streams/unparser
        #:streams/dispatcher
        #:marie)
  (:export #:server))

(in-package #:streams/server)

(defun echo-message (connection message)
  "Echo back MESSAGE to CONNECTION."
  (send connection message))

(defclass server ()
  ((name :initarg :name
         :initform ""
         :accessor name
         :documentation "The free-form name of the server.")
   (form :initarg :form
         :initform nil
         :accessor form
         :documentation "The symbol indicating the form of the server. Valid values are MSL and ADMIN.")
   (port :initarg :port
         :initform nil
         :accessor port
         :documentation "The communication port for the server.")
   (open-handler :initarg :open-handler
                 :initform nil
                 :accessor open-handler
                 :documentation "The function for handling open commands.")
   (message-handler :initarg :message-handler
                    :initform nil
                    :accessor message-handler
                    :documentation "The function for handling message commands.")
   (close-handler :initarg :close-handler
                  :initform nil
                  :accessor close-handler
                  :documentation "The function for handling close commands."))
  (:documentation "Class for containing information about a server."))

(defun* make-server-instance (name form port &optional open-handler message-handler close-handler)
  "Return a new SERVER instance."
  (make-instance 'server :name name :form form :port port
                         :open-handler open-handler
                         :message-handler message-handler
                         :close-handler close-handler))

(defvar *servers* nil
  "The list of active servers.")

(defun start-clack (server port)
  "Start the clack server SERVER under port PORT."
  (clack:clackup server :port port))

(defun stop-clack (server)
  "Stop the clack server SERVER."
  (clack:stop server))

;;; pass to this the results of evaluating a SERVER instance
(defmacro* define-event-handlers (name form port open-handler message-handler close-handler)
  ""
  (declare (ignorable form))
  (flet ((make-name (&rest args)
           (apply #'hyphenate-intern nil args)))
    (let* ((server-name (make-name name "server"))                    ;msl-server
           (start-server-name (make-name "start" server-name))        ;start-msl-server
           (stop-server-name (make-name "stop" server-name))          ;stop-msl-server
           (server-symbol-name (intern (cat "*" server-name "*"))))   ;*msl-server*
      `(progn
         (defvar ,server-symbol-name nil)
         (defun ,server-name (env)
           (let ((server (websocket-driver:make-server env)))
             (websocket-driver:on :open server ,open-handler)
             (websocket-driver:on :message server ,message-handler)
             (websocket-driver:on :close server ,close-handler)
             (lambda (_)
               (declare (ignore _))
               (websocket-driver:start-connection server))))
         (defun ,start-server-name ()
           (let ((server (start-clack #',server-name ,port)))
             (setf ,server-symbol-name server)
             server))
         (defun ,stop-server-name ()
           (stop-clack ,server-symbol-name)
           (setf ,server-symbol-name nil))))))

#| this means that prior to opening the msl wire, the admin wire has to be open first because it’s going to use it for sending narrative values

;;; msl
(lambda () (handle-open-connection server))

(lambda (message) (dispatch message) (send msl-connection (recall-expr message)) (send admin-connection (recall-value message)))

(lambda (&key _ __) (declare (ignore _ __)) (handle-close-connection server))


;;; admin
(lambda () (handle-open-connection ws))

(lambda (message) (send server message))

(lambda (&key _ __) (declare (ignore _ __)) (handle-close-connection server))
|#

(defvar *user-connections* (make-hash-table)
  "The table of connections, where the keys are server instances while the values are the user IDs.")

(defvar *user-base-id* 1000
  "The base integer user ID for connections.")

(defun get-new-user-id ()
  "Return a new fresh user ID."
  (incf *user-base-id*)
  *user-base-id*)

(defun format-msl (type text)
  "Return a string formatted for MSL."
  (format nil "(@~A ~A)" type text))

(defun handle-open-connection (connection)
  "Process incoming connection CONNECTION."
  (let ((uid (get-new-user-id))
        (message (format-msl "VER" *system-version*)))
    (setf (gethash connection *user-connections*)
          (format nil "~A" uid))
    (send connection message)))

(defun echo-message (connection message)
  "Send back MESSAGE to CONNECTION."
  (send connection message))

(defun respond-expr (connection message)
  "Return the full MSL expression from MESSAGE."
  (let ((value (recall-expr message)))
    (send connection value)))

(defun respond-value (connection message)
  "Return the default expression value from MESSAGE."
  (let ((value (recall-value message)))
    (send connection value)))

(defun handle-close-connection (connection)
  "Process connection CONNECTION when it closes."
  (let ((message (format nil " ... ~A has left."
                         (gethash connection *user-connections*))))
    (remhash connection *user-connections*)
    (loop :for con :being :the :hash-key :of *user-connections*
          :do (send con message))))

;;; can these be abstacted away?
;; (defun start-websocket-servers ()
;;   "Start all the WebSocket servers."
;;   (loop :for server :in (list #'start-msl-server #'start-admin-server)
;;         :do (progn (funcall server)
;;                    (push server *websocket-servers*))))
;; (defun stop-websocket-servers ()
;;   "Stop all the WebSocket servers."
;;   (with-muffled-debugger
;;     (format *error-output* "Aborting...~%")
;;     (loop :for server :in *websocket-servers* :do (clack:stop server))
;;     (setf *websocket-servers* nil)
;;     (uiop:quit)))

(defun* serve ()
  "The main entrypoint of the server."
  (format t "streams v~A~%" *system-version*)
  ;; (start-websocket-servers)
  ;; (handler-case (bt:join-thread (find-if (lambda (thread)
  ;;                                          (search "hunchentoot" (bt:thread-name thread)))
  ;;                                        (bt:all-threads)))
  ;;   (#+sbcl sb-sys:interactive-interrupt
  ;;    #+ccl ccl:interrupt-signal-condition
  ;;    #+clisp system::simple-interrupt-condition
  ;;    #+ecl ext:interactive-interrupt
  ;;    #+allegro excl:interrupt-signal
  ;;    #+lispworks mp:process-interrupt
  ;;    () (stop-websocket-servers))
  ;;   (error (c)
  ;;     (format t "Oops, an unknown error occured:~&~A~&" c)))
  nil)
