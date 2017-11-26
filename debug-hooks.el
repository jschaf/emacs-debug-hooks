;;; debug-hooks.el --- Debug all the hooks -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Joe Schafer

;; Author: Joe Schafer <joe@jschaf.com>
;; Maintainer:  Joe Schafer <joe@jschaf.com>
;; Created: 22 Nov 2017
;; URL: http://github.com/jschaf/emacs-debug-hooks
;; Version:  0.1
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))
;; Keywords: convenience, processes

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(defvar debug-hooks-buffer "*debug-hooks*"
  "The buffer to output hook debug information.")

(defvar debug-hooks--current-hook nil
  "The current hook that's running.")

;; Output

;; Most Recent at top.

;; Command: self-insert-command
;; At: %t:%6n
;; 100ms post-command-hooks
;;    25ms pch-1
;;    25ms pch-1
;; find-file-modification-hooks
;;    33ms ffmh

(defvar debug-hooks-all-hooks
  '(
    ;; activate-mark-hook
    ;; deactivate-mark-hook
    ;; after-change-functions
    ;; before-change-functions
    ;; first-change-hook
    ;; after-change-major-mode-hook
    ;; change-major-mode-after-body-hook
    ;; after-init-hook
    ;; before-init-hook
    ;; emacs-startup-hook
    ;; window-setup-hook
    ;; after-insert-file-functions
    ;; write-region-annotate-functions
    ;; write-region-post-annotation-function
    ;; after-make-frame-functions
    ;; before-make-frame-hook
    ;; after-save-hook
    ;; before-save-hook
    ;; write-contents-functions
    ;; write-file-functions
    ;; after-setting-font-hook
    ;; auto-save-hook
    ;; before-hack-local-variables-hook
    ;; hack-local-variables-hook
    ;; buffer-access-fontify-functions
    ;; buffer-list-update-hook
    ;; buffer-quit-function
    ;; change-major-mode-hook
    ;; command-line-functions
    ;; delayed-warnings-hook
    ;; focus-in-hook
    ;; focus-out-hook
    ;; delete-frame-functions
    ;; delete-terminal-functions
    ;; pop-up-frame-function
    ;; split-window-preferred-function
    ;; echo-area-clear-hook
    ;; find-file-hook
    ;; find-file-not-found-functions
    ;; font-lock-extend-after-change-region-function
    ;; font-lock-extend-region-functions
    ;; font-lock-fontify-buffer-function
    ;; font-lock-fontify-region-function
    ;; font-lock-mark-block-function
    ;; font-lock-unfontify-buffer-function
    ;; font-lock-unfontify-region-function
    ;; fontification-functions
    ;; frame-auto-hide-function
    ;; kill-buffer-hook
    ;; kill-buffer-query-functions
    ;; kill-emacs-hook
    ;; kill-emacs-query-functions
    ;; menu-bar-update-hook
    ;; minibuffer-setup-hook
    ;; minibuffer-exit-hook
    ;; mouse-leave-buffer-hook
    ;; mouse-position-function
    ;; prefix-command-echo-keystrokes-functions
    ;; prefix-command-preserve-state-hook
    ;; pre-redisplay-functions
    post-command-hook
    ;; pre-command-hook
    ;; post-gc-hook
    ;; post-self-insert-hook
    ;; suspend-hook
    ;; suspend-resume-hook
    ;; suspend-tty-functions
    ;; resume-tty-functions
    ;; syntax-begin-function
    ;; syntax-propertize-extend-region-functions
    ;; syntax-propertize-function
    ;; font-lock-syntactic-face-function
    ;; temp-buffer-setup-hook
    ;; temp-buffer-show-function
    ;; temp-buffer-show-hook
    ;; tty-setup-hook
    ;; window-configuration-change-hook
    ;; window-scroll-functions
    ;; window-size-change-functions
    ;; window-text-change-functions
    )
  )

(defvar debug-hooks-mode nil
  "Whether or not `debug-hooks-mode' is enabled.")

;; Not using a minor mode, because these hooks are global.  It doesn't
;; make sense to toggle them per-buffer.
(defun debug-hooks-mode (&optional arg)
  "Toggle `debug-hooks-mode' on and off."
  (interactive)
  (cond
   ((null arg) (if debug-hooks-mode
                   (debug-hooks-disable)
                 (debug-hooks-enable)))
   ((> arg 0) (debug-hooks-enable))
   ((<= arg 0) (debug-hooks-disable))
   (debug-hooks-mode (debug-hooks-disable))
   (t (debug-hooks-enable))))

(defun debug-hooks-enable ()
  "Enable `debug-hooks-mode'."
  (setq debug-hooks-mode t)
  (debug-hooks-advise-hooks debug-hooks-all-hooks)
  (message "debug-hooks enabled"))

(defun debug-hooks-disable ()
  "Disable `debug-hooks-mode'."
  (setq debug-hooks-mode nil)
  (debug-hooks-unadvise-hooks debug-hooks-all-hooks)
  (message "debug-hooks disabled"))

(defun debug-hooks-stopwatch-start ()
  "Start a stop-watch and return the current time.
The time format matches `current-time'."
  (current-time))

(defun debug-hooks-stopwatch-stop-in-millis (start-time)
  "Return the number of milliseconds elapsed since START-TIME.
START-TIME is the normal elisp representation of an instant in
time.  The representation is a 4 element list of integers.  See
the `current-time' documentation for details."
  (* (time-to-seconds
      (time-subtract (current-time) start-time))
     1000))

;;;###autoload
(defun debug-hooks-advise-hooks (hooks)
  "Advise all HOOKS to log debug output."
  (cl-loop for hook in hooks
           do (debug-hooks-map-over-hook
               #'debug-hooks-advise-single-function
               hook)))

(defun debug-hooks-advise-single-function (wrapped-fn parent-hook)
  "Advise a single WRAPPED-FN to log debug output.
PARENT-HOOK is hook that called WRAPPED-FN and is used for
debuggin purposes."
  (let ((advisor-symbol (debug-hooks--make-advisor-name wrapped-fn parent-hook))
        (advisor-fn
         (debug-hooks--make-function-advisor wrapped-fn parent-hook)))
    (eval advisor-fn)
    (advice-add wrapped-fn :around advisor-symbol)))

(defun debug-hooks--make-function-advisor (wrapped-fn parent-hook)
  "Create a function to advise WRAPPED-FN.
PARENT-HOOK is the hook that triggered WRAPPED-FN.  This function
is necessary because when advising a function, the advisor
doesn't have access to the function name and there's not a way to
parameterize advice."
  (let ((advisor-symbol
         (debug-hooks--make-advisor-name wrapped-fn parent-hook)))
    `(defun ,advisor-symbol (orig-fn &rest args)
       "Advise around ORIG-FN write debug output."
       (let* ((start-time (debug-hooks-stopwatch-start))
              (result (apply orig-fn args))
              (latency (debug-hooks-stopwatch-stop-in-millis start-time)))
         ;; TODO: do we want to check the `debug-hooks--current-hook'
         (debug-hooks-write-log-info
          (quote ,wrapped-fn) (quote ,parent-hook) latency)
         
         result))))

(defun debug-hooks--make-advisor-name (wrapped-fn parent-hook)
  "Make a name for advice of WRAPPED-FN."
  (intern (format "debug-hooks--wrap-%s-%s" wrapped-fn parent-hook)))

(defun debug-hooks-write-log-info (func parent-hook latency)
  "Write log info."
  (with-current-buffer (get-buffer-create debug-hooks-buffer)
    (let ((inhibit-modification-hooks t))
      (when (> (buffer-size) 10000)
        (delete-region (line-beginning-position 100) (point-max))
        (goto-char (point-min)))

      ;; TODO: this doesn't scroll the buffer to the correct spot.
      ;; Maybe call interactively.
      (goto-char (point-min))
      (insert (format "%.3f ms:  %s - %s\n"
                      latency
                      parent-hook
                      (symbol-name func))))))

(defun debug-hooks-map-over-hook (func hook)
  "Apply FUNC to every function in HOOK.
FUNC is a lambda that takes two inputs, a function symbol and the
parent hook symbol.  Return a list of the output of each FUNC."
  (cond
   ;; A hook like `post-command-hook'
   ((and (boundp hook) (listp (symbol-value hook)))
    (cl-loop for hook-elem in (symbol-value hook)
             collect (funcall func hook-elem hook)))

   ;; A hook like `buffer-quit-function'.
   ((functionp (symbol-value hook))
    (list (funcall func (symbol-value hook) hook)))))

(defun debug-hooks-unadvise-hooks (hooks)
  "Remove all advice from HOOKS."
  (cl-loop for hook in hooks
           do (debug-hooks-map-over-hook
               #'debug-hooks-unadvise-single-function
               hook)))

(defun debug-hooks-unadvise-single-function (func &optional parent-hook)
  "Remove all debug-hooks advice from FUNC."
  (advice-mapc (lambda (added-func props)
                 (when (string-prefix-p "debug-hooks" (symbol-name added-func))
                   (advice-remove func added-func)))
               func))

(provide 'debug-hooks)
;;; debug-hooks.el ends here
