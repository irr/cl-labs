;(add-to-list 'load-path "~/.emacs.d/")

(setenv "LANG" "en_US.UTF-8")
(set-language-environment "utf-8")

(setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix)))
(setq slime-net-coding-system 'utf-8-unix)

(load (expand-file-name "/opt/lisp/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
 (setq inferior-lisp-program "/usr/bin/sbcl")

(setq save-abbrevs nil)

(require 'cua-base)
(cua-mode t)

(tool-bar-mode nil)
(show-paren-mode t)
(column-number-mode t)
(line-number-mode t)

(setq-default
 indent-tabs-mode nil
 frame-title-format
 (list
  '((buffer-file-name
     " %f"
     (dired-directory
      dired-directory
      (revert-buffer-function
       " %b"
       ("%b - Dir: " default-directory)))))))

(setq
 c-basic-offset 4
 tab-width 4
 default-tab-width 4
 inhibit-startup-message t
 delete-auto-save-files t
 delete-old-versions t
 backup-inhibited t
 make-backup-files nil
 auto-save-default nil
 auto-save-mode nil
 compilation-read-command nil)

(blink-cursor-mode -1)

(global-set-key [(control home)] 'beginning-of-buffer)
(global-set-key [(control end)] 'end-of-buffer)
(global-set-key "\M-g" 'goto-line)
(global-set-key [f2] 'save-buffer)
(global-set-key [f6] 'delete-other-windows)
(global-set-key [f8] 'indent-buffer)
(global-set-key [f9] 'compile)

(defun indent-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "unknown" :family "JetBrains Mono")))))

