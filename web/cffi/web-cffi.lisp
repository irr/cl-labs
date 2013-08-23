;; httperf --server localhost --port 8888 --uri /?name=ivan --num-call=100 --num-conn 100 --timeout 1

;; curl -v http://localhost:8888/?name=ivan+ribeiro
;; http://sourceforge.net/projects/privbind/
;; su -c "privbind -u irocha ./web"

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :hunchentoot)
  (require :bordeaux-threads)
  (require :cl-json)
  (require :cffi))

(defpackage :webserver
  (:use #:common-lisp #:hunchentoot #:bordeaux-threads #:json #:cffi)
  (:export #:start #:stop #:dump #:run)
  (:shadow #:start #:stop))

(in-package :webserver)

(defparameter *acceptor* nil "web-server instance")
(defparameter *content-type* "application/json")

(defparameter *access-log* "/tmp/router-access.log")
(defparameter *message-log* "/tmp/router-message.log")

(defvar *acceptor-lock* (make-lock))
(defvar *handler-lock* (make-lock))
(defvar *acceptor-variable* (make-condition-variable))

(setf *show-lisp-errors-p* t
      *show-lisp-backtraces-p* t
      *dispatch-table* '(dispatch-easy-handlers))

;; (defun time-to-string (call-fn)
;;  (format nil "~A" (with-output-to-string (*trace-output*) (time (funcall call-fn)))))
;; ...
;; (time-to-string #'(lambda () (cffi-call name))))

;; CFFI-BEGIN
;; LD_LIBRARY_PATH=/home/irocha/lisp/web/cffi:$LD_LIBRARY_PATH rlwrap sbcl --load web-cffi.lisp

(define-foreign-library libtestlib
    (:unix (:or "libtestlib.so"))
    (t (:default "libtestlib")))

(use-foreign-library libtestlib)

(defcfun "staticTest" :void)
(defcfun "dynamicTest" :int (v :int))

;; (staticTest)
;; (dynamicTest 1972)

;; CFFI-END

(defun cffi-call (name)
  (with-lock-held (*handler-lock*)
    (format nil "~A~%" name)))

(define-easy-handler (router :uri "/") (name)
  (no-cache)
  (setf (content-type*) *content-type*)
  (let ((h (make-hash-table)))
    (setf (gethash 'name h) name)
    (handler-case 
      (progn 
        (cffi-call name)
        (staticTest)
        (format nil "~A~%" (encode-json-to-string h)))
      (error (e) 
        (format nil "~A~%" (encode-json-to-string (format nil "~A~%" e)))))))
    

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
  (sb-ext:save-lisp-and-die "web" :toplevel #'webserver:run :executable t))

(sb-sys:enable-interrupt sb-unix:sigterm #'sig-handler)
