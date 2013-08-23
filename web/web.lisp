(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :hunchentoot)
  (require :bordeaux-threads)
  (require :trivial-dump-core)
  (require :cl-json))

(defpackage :webserver
  (:use #:common-lisp #:hunchentoot #:bordeaux-threads #:json)
  (:export #:start #:stop #:dump #:run)
  (:shadow #:start #:stop))

(in-package :webserver)

(defvar *content-type* "application/json")

(defvar *access-log* "/tmp/router-access.log")
(defvar *message-log* "/tmp/router-message.log")

(setf *show-lisp-errors-p* t
      *show-lisp-backtraces-p* t
      *dispatch-table* '(dispatch-easy-handlers))

(define-easy-handler (router :uri "/") (name)
  (no-cache)
  (setf (content-type*) *content-type*)
  (let ((h (make-hash-table)))
    (setf (gethash 'name h) name)    
    (format nil "~A~%" (json:encode-json-to-string h))))

(defparameter *acceptor* nil "web-server instance")

(defvar *acceptor-lock* (make-lock))
(defvar *acceptor-variable* (make-condition-variable))

(defun start (&key (port 8888) (wait nil))
  (hunchentoot:start (setf *acceptor* (make-instance 'easy-acceptor :port port)))
  (setf *default-connection-timeout* 10)
  (setf (acceptor-access-log-destination *acceptor*) *access-log*)
  (setf (acceptor-message-log-destination *acceptor*) *message-log*)
  (log-message* :info "webserver listening on port ~D...~%" port)
  (unless (not wait)
    (with-lock-held (*acceptor-lock*)
      (condition-wait *acceptor-variable* *acceptor-lock*))))

(defun stop ()
  (unless (not *acceptor*)
    (hunchentoot:stop *acceptor*))
    (setf *acceptor* nil))

(defun sig-handler (signal code context)
  (declare (ignore signal code context))
  (unless (not *acceptor*)
    (stop)
    (condition-notify *acceptor-variable*)))

(defun run ()
  (start :port 8888 :wait t))

(defun dump ()
  ;; http://sourceforge.net/projects/privbind/
  ;; su -c "privbind -u irocha ./web"
  ;; curl -v http://localhost:8888/?name=ivan+ribeiro
  ;; (sb-ext:save-lisp-and-die "web" :toplevel #'webserver:run :executable t))
  (trivial-dump-core:save-executable "web" #'webserver:run))

(sb-sys:enable-interrupt sb-unix:sigterm #'sig-handler)
