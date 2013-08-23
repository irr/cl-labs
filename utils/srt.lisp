; lisp --noinform --script srt.lisp <file>

(let ((quicklisp-init #P"/opt/lisp/quicklisp/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "trivial-shell")
(ql:quickload "trivial-dump-core")
(ql:quickload "cl-ppcre")

(defpackage :srt
  (:use #:common-lisp #:trivial-shell #:cl-ppcre)
  (:export #:run #:dump)
  (:shadow #:trim #:file-string #:string-file #:file-encoding #:convert))

(in-package :srt)

(defun trim (s)
    (string-trim '(#\Space #\Tab #\Newline) s))

(defun file-string (path kw)
  (with-open-file (stream path :external-format kw)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun string-file (path content)
        (with-open-file (stream path
            :direction :output
            :if-exists :supersede
            :if-does-not-exist :create
            :external-format :utf-8)
        (format stream content)))

(defun file-encoding (path) 
    (let ((output nil)
          (error-output nil)
          (exit-status nil))
        (multiple-value-setq (output error-output exit-status)
            (trivial-shell:shell-command (format nil "file -bi \"~A\"" path)))
        (string-upcase (trim (second (cl-ppcre:split ".*?charset=(.*?)" output))))))

(defun shell (cmd)
    (let* ((output nil)
           (error-output nil)
           (exit-status nil))
        (multiple-value-setq (output error-output exit-status)
            (trivial-shell:shell-command cmd))
        output))

(defun iconv (kw path)
    (shell (format nil "iconv -f ~A -t UTF-8//TRANSLIT < \"~A\" > \"~A.bak\"" kw path path)))

(defun perl (path)
    (shell (format nil "perl -pe 's/<.*?i>|<.*?b>|<.*?u>//gi' \"~A.bak\" > \"~A\"; rm -rf \"~A.bak\"" path path path)))
    
(defun convert (path) 
    (let* ((kw (intern (file-encoding path) "KEYWORD")))
        (iconv kw path)
        (perl path)))

(defun srt (path)
    (convert path))

(defun run ()
    (if (> (length sb-ext:*posix-argv*) 1)
        (srt (second sb-ext:*posix-argv*))))

(defun dump ()
  (trivial-dump-core:save-executable "srt" #'srt:run))

(run)
