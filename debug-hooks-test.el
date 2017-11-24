;;; debug-hooks-test.el --- tests for esup -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for debug-hooks-test.el

;;; Code:
(eval-when-compile
  (require 'cl))

;; (setq aa '((fake-hook . (fake-hook-fn-1 fake-hook-fn2))
;;            (fake-function . 'fake-function-impl)))

;; (dht//make-all-hook-initialization aa)

;; (with-temp-hooks aa
;;                  (with-current-buffer "panic"))

;; (dht//make-uninit-single-hook 'a 'b)

;; (dht//make-uninit-single-hook 'a '(d e f))

(defmacro dht/with-temp-hooks (hooks-alist &rest forms)
  "Defines a new variable for each element in HOOKS-ALIST.
Errors if the variable is already in use.  For each hook created,
also defines the list of hooks functions."
  (let ((hooks (eval hooks-alist)))
    `(progn
       ,@(dht//make-init-all-hooks hooks)
       (unwind-protect
           (progn ,@forms)
         ;; Remove advice
         ;; Remove advisor functions
         ,@(dht//make-uninit-all-advised-fns hooks)
         ,@(dht//make-uninit-all-hooks hooks)))))

(defun dht//make-init-all-hooks (hooks-alist)
  "Create initialization code for HOOKS-ALIST."
  (cl-loop for (hook-var . hook-def) in hooks-alist
           append (dht//make-init-single-hook hook-var hook-def)))

(defun dht//make-init-single-hook (hook-var hook-def)
  "Create initialization code HOOK-VAR and HOOK-DEF.
If HOOK-DEF is a list of symbols, creates a new function for each
symbol.  If HOOK-DEF is a single symbol, creates a single new
function."
  (append
   (list `(defvar ,hook-var nil))

   (cond
    ((symbolp hook-def)
     `((defun ,hook-def ())
       (setq ,hook-var ',hook-def)))

    ((listp hook-def)
     (cl-loop for fn-symbol in hook-def
              collect `(defun ,fn-symbol (orig-fn &rest args)) into def-list
              collect fn-symbol into setq-list
              finally
              (return
               (append def-list (list `(setq ,hook-var ',setq-list)))))))))

(defun dht//make-uninit-all-hooks (hooks-alist)
  "Create cleanup code for all functions and variables in HOOKS-ALIST."
  (cl-loop for (hook-var . hook-def) in hooks-alist
           append (dht//make-uninit-single-hook hook-var hook-def)))

(defun dht//make-uninit-single-hook (hook-var hook-def)
  "Create cleanup code for HOOK-VAR and HOOK-DEF."
  (append
   (list `(makunbound ',hook-var))

   (cond
    ((symbolp hook-def) `((fmakunbound ',hook-def)))

    ((listp hook-def)
     (cl-loop for fn-symbol in hook-def collect `(fmakunbound ',fn-symbol))))))

(defun dht//make-uninit-all-advised-fns (hooks-alist)
  "Create cleanup code for all advised functions in HOOKS-ALIST."
  (cl-loop for (hook-var . hook-def) in hooks-alist
           append (dht//make-uninit-single-advised-fn hook-def)))

(defun dht//make-uninit-single-advised-fn (hook-def)
  "Create cleanup code for the functions specified by HOOK-DEF."
  (cond
   ((symbolp hook-def) `((dht//unadvise ',hook-def)))

   ((listp hook-def)
    (cl-loop for fn-symbol in hook-def collect `(dht//unadvise ',fn-symbol)))))

(defun dht//unadvise (advised-fn)
  "Remove advice from ADVISED-FN."
  (advice-mapc (lambda (added-func props) (fmakeunbound added-func))
               advised-fn))


(provide 'debug-hooks-test)
;;; debug-hooks-test.el end here

(ert-deftest debug-hooks-test/function-advised ()
  ""
  (should (equal 1 1)))
