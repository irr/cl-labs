(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :hunchentoot)
  (require :bordeaux-threads)
  (require :cl-json)
  (require :cl-redis))

(defpackage :webserver
  (:use #:common-lisp #:hunchentoot #:bordeaux-threads #:json)
  (:export #:start #:stop #:dump #:run)
  (:shadow #:start #:stop))

(in-package :webserver)

(defvar *content-type* "application/json")

(defvar *access-log* "/tmp/router-access.log")
(defvar *message-log* "/tmp/router-message.log")

(defparameter *acceptor* nil "web-server instance")

(defvar *acceptor-lock* (make-lock))
(defvar *handler-lock* (make-lock))
(defvar *acceptor-variable* (make-condition-variable))

(setf *show-lisp-errors-p* t
      *show-lisp-backtraces-p* t
      *dispatch-table* '(dispatch-easy-handlers))

(defun redis-call (name)
  (with-lock-held (*handler-lock*)
    (and name (redis:with-connection () (redis:red-incr name)))))

(define-easy-handler (router :uri "/") (name)
  (no-cache)
  (setf (content-type*) *content-type*)
  (let ((h (make-hash-table)))
    (setf (gethash 'name h) name)
    (handler-case 
      (progn 
        (redis-call name)
        (format nil "~A~%" (json:encode-json-to-string h)))
      (redis:redis-error (e) 
        (format nil "~A~%" (json:encode-json-to-string (format nil "~A~%" e)))))))
    

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
  ;; (trivial-dump-core:save-executable "web" #'webserver:run)
  (sb-ext:save-lisp-and-die "web" :toplevel #'webserver:run :executable t))

(sb-sys:enable-interrupt sb-unix:sigterm #'sig-handler)
