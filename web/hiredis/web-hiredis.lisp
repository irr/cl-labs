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
  (:use :common-lisp :hunchentoot :bordeaux-threads :json :cffi)
  (:export :start :stop :dump :run)
  (:shadow :start :stop))

(in-package :webserver)

(defparameter *acceptor* nil "webserver instance")
(defparameter *content-type* "application/json")

(defparameter *access-log* "/tmp/router-access.log")
(defparameter *message-log* "/tmp/router-message.log")

(defvar *acceptor-lock* (make-lock))
(defvar *acceptor-variable* (make-condition-variable))

(setf *show-lisp-errors-p* t
      *show-lisp-backtraces-p* t
      *dispatch-table* '(dispatch-easy-handlers))

;; ============================================================================================
;; CFFI-BEGIN
;; LD_LIBRARY_PATH=/home/irocha/lisp/web/cffi:$LD_LIBRARY_PATH rlwrap sbcl --load web-cffi.lisp
;; ============================================================================================

(define-foreign-library libhiredis (:unix (:or "libhiredis.so" "libhiredis.so.10"))
  (t (:default "libhiredis")))

(use-foreign-library libhiredis)

(defcfun "redisConnectUnix" :pointer (path :string))
(defcfun "redisFree" :void (context :pointer))

;; typedef struct redisReply {
;;     int type; /* REDIS_REPLY_* */
;;     long long integer; /* The integer when type is REDIS_REPLY_INTEGER */
;;     int len; /* Length of string */
;;     char *str; /* Used for both REDIS_REPLY_ERROR and REDIS_REPLY_STRING */
;;     size_t elements; /* number of elements, for REDIS_REPLY_ARRAY */
;;     struct redisReply **element; /* elements vector for REDIS_REPLY_ARRAY */
;; } redisReply;

;; #define REDIS_REPLY_STRING 1
;; #define REDIS_REPLY_ARRAY 2
;; #define REDIS_REPLY_INTEGER 3
;; #define REDIS_REPLY_NIL 4
;; #define REDIS_REPLY_STATUS 5
;; #define REDIS_REPLY_ERROR 6

(defcstruct redisReply (type :int)
  (integer :long-long)
  (len :int)
  (str :string)
  (elements :unsigned-int)
  (element :pointer))

;; ============================================================================================
;; CFFI-END
;; ============================================================================================

(defparameter *redis* "/tmp/redis.sock")

(defun cffi-call (name)
  (let ((redis-context nil)
        (redis-reply nil))
    (setf redis-context (redisConnectUnix *redis*)
          redis-reply (foreign-funcall "redisCommand"
                                       :pointer redis-context
                                       :string "INCR %s"
                                       :string name
                                       redisReply))
    (redisFree redis-context)
    (with-foreign-slots ((type integer) redis-reply redisReply)
      (cond ((= type 3) (format nil "ok:~A~%" integer))
            (t (error (format nil "error: redis-reply=~A~%" type))))
      )))

(define-easy-handler (router :uri "/") (name)
  (no-cache)
  (setf (content-type*) *content-type*)
  (let ((h (make-hash-table)))
    (setf (gethash 'name h) name)
    (handler-case
        (progn
          (setf (gethash 'value h) (cffi-call name))
          (format nil "~A~%" (encode-json-to-string h)))
      (error (e)
        (format nil "~A~%" (encode-json-to-string (format nil "~A~%" e)))))))


(defun start (&key (port 8888) (wait nil))
  (hunchentoot:start (setf *acceptor* (make-instance 'easy-acceptor :port port)))
  (setf *default-connection-timeout* 10
        (acceptor-access-log-destination *acceptor*) *access-log*
        (acceptor-message-log-destination *acceptor*) *message-log*)
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
