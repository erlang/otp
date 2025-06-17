;;;    -*- Emacs-Lisp -*- 
;;;
;;; %CopyrightBegin%
;;;
;;; SPDX-License-Identifier: Apache-2.0
;;;
;;; Copyright Ericsson AB 2010-2025. All Rights Reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;
;;; %CopyrightEnd%
;;;
;; erlang-flymake.el
;;
;; Syntax check erlang source code on the fly (integrates with flymake).
;;
;; Start using flymake with erlang by putting the following somewhere
;; in your .emacs file:
;;
;;     (require 'erlang-flymake)
;;
;; Flymake is rather eager and does its syntax checks frequently by
;; default and if you are bothered by this, you might want to put the
;; following in your .emacs as well:
;;
;;     (erlang-flymake-only-on-save)
;;
;; There are a couple of variables which control the compilation options:
;; * erlang-flymake-get-code-path-dirs-function
;; * erlang-flymake-get-include-dirs-function
;; * erlang-flymake-extra-opts
;;
;; This code is inspired by http://www.emacswiki.org/emacs/FlymakeErlang.

(require 'flymake)
(eval-when-compile
  (require 'cl-lib))

(defvar erlang-flymake-command
  "erlc"
  "The command that will be used to perform the syntax check")

(defvar erlang-flymake-get-code-path-dirs-function
  'erlang-flymake-get-code-path-dirs
  "Return a list of ebin directories to add to the code path.")

(defvar erlang-flymake-get-include-dirs-function
  'erlang-flymake-get-include-dirs
  "Return a list of include directories to add to the compiler options.")

(defvar erlang-flymake-extra-opts
  (list "+warn_unused_import"
        "+warn_shadow_vars"
        "+warn_export_vars"
        "+strong_validation"
        "+report")
  "A list of options that will be passed to the compiler")

(defun erlang-flymake-only-on-save ()
  "Trigger flymake only when the buffer is saved (disables syntax
check on newline and when there are no changes)."
  (interactive)
  ;; There doesn't seem to be a way of disabling this; set to the
  ;; largest int available as a workaround (most-positive-fixnum
  ;; equates to 8.5 years on my machine, so it ought to be enough ;-) )
  (setq flymake-no-changes-timeout most-positive-fixnum)
  (setq flymake-start-syntax-check-on-newline nil))


(defun erlang-flymake-get-code-path-dirs ()
  (list (concat (erlang-flymake-get-app-dir) "ebin")))

(defun erlang-flymake-get-include-dirs ()
  (list (concat (erlang-flymake-get-app-dir) "include")
        (concat (erlang-flymake-get-app-dir) "deps")))

(defun erlang-flymake-get-app-dir ()
  (let ((src-path (file-name-directory (buffer-file-name))))
    (file-name-directory (directory-file-name src-path))))

(defun erlang-flymake-init ()
  (let* ((temp-file
          (cl-letf (((symbol-function 'flymake-get-temp-dir) #'erlang-flymake-temp-dir))
            (flymake-proc-init-create-temp-buffer-copy
             'flymake-create-temp-with-folder-structure)))
         (code-dir-opts
          (erlang-flymake-flatten
           (mapcar (lambda (dir) (list "-pa" dir))
                   (funcall erlang-flymake-get-code-path-dirs-function))))
         (inc-dir-opts
          (erlang-flymake-flatten
           (mapcar (lambda (dir) (list "-I" dir))
                   (funcall erlang-flymake-get-include-dirs-function))))
         (compile-opts
          (append inc-dir-opts
                  code-dir-opts
                  erlang-flymake-extra-opts)))
    (list erlang-flymake-command (append compile-opts (list temp-file)))))

(defun erlang-flymake-temp-dir ()
  ;; Squeeze the user's name in there in order to make sure that files
  ;; for two users who are working on the same computer (like a linux
  ;; box) don't collide
  (format "%s/flymake-%s" temporary-file-directory user-login-name))

(defun erlang-flymake-flatten (list)
  (apply #'append list))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.erl\\'" erlang-flymake-init))
(add-hook 'erlang-mode-hook 'flymake-mode)

(provide 'erlang-flymake)
;; erlang-flymake ends here
