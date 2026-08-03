;;; erlang-flymake.el   -*-  lexical-binding: t; -*-
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
;; There are a couple of variables which control the compilation options:
;; * erlang-flymake-get-code-path-dirs-function
;; * erlang-flymake-get-include-dirs-function
;; * erlang-flymake-extra-opts
;;
;; This code is inspired by http://www.emacswiki.org/emacs/FlymakeErlang.

(require 'flymake)

(defvar erlang-flymake-command
  "erlc"
  "The command that will be used to perform the syntax check.")

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
  "A list of options that will be passed to the compiler.")

(defun erlang-flymake-get-code-path-dirs ()
  (list (concat (erlang-flymake-get-app-dir) "ebin")))

(defun erlang-flymake-get-include-dirs ()
  (list (concat (erlang-flymake-get-app-dir) "include")
        (concat (erlang-flymake-get-app-dir) "deps")))

(defun erlang-flymake-get-app-dir ()
  (let ((src-path (file-name-directory (buffer-file-name))))
    (file-name-directory (directory-file-name src-path))))

(defvar-local erlang-flymake--proc nil
  "Internal variable for the Erlang flymake backend process.")

(defun erlang-flymake-backend (report-fn &rest _args)
  "Flymake backend for Erlang using erlc.
REPORT-FN is the flymake callback."
  (when (process-live-p erlang-flymake--proc)
    (kill-process erlang-flymake--proc))
  (let* ((source (current-buffer))
         (filename (buffer-file-name source))
         (code-dir-opts
          (apply #'append
                 (mapcar (lambda (dir) (list "-pa" dir))
                         (funcall erlang-flymake-get-code-path-dirs-function))))
         (inc-dir-opts
          (apply #'append
                 (mapcar (lambda (dir) (list "-I" dir))
                         (funcall erlang-flymake-get-include-dirs-function))))
         (compile-opts (append inc-dir-opts
                               code-dir-opts
                               erlang-flymake-extra-opts
                               (list filename))))
    (save-restriction
      (widen)
      (setq erlang-flymake--proc
            (make-process
             :name "erlang-flymake"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer " *erlang-flymake*")
             :command (cons erlang-flymake-command compile-opts)
             :sentinel
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (unwind-protect
                     (when (with-current-buffer source
                             (eq proc erlang-flymake--proc))
                       (with-current-buffer (process-buffer proc)
                         (goto-char (point-min))
                         (let (diagnostics)
                           (while (search-forward-regexp
                                   "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):\\s-*\\(warning\\|error\\):\\s-*\\(.*\\)$"
                                   nil t)
                             (let* ((file (match-string 1))
                                    (line (string-to-number (match-string 2)))
                                    (col (string-to-number (match-string 3)))
                                    (type (if (string= (match-string 4) "warning")
                                             :warning
                                           :error))
                                    (msg (match-string 5))
                                    (region (flymake-diag-region source line col)))
                               (when (and region
                                          (string= (file-truename file)
                                                   (file-truename filename)))
                                 (push (flymake-make-diagnostic
                                        source (car region) (cdr region)
                                        type msg)
                                       diagnostics))))
                           ;; Also match old-style output without column:
                           ;; file.erl:LINE: warning/error: message
                           (goto-char (point-min))
                           (while (search-forward-regexp
                                   "^\\(.*\\):\\([0-9]+\\):\\s-*\\(warning\\|error\\):\\s-*\\(.*\\)$"
                                   nil t)
                             (let* ((file (match-string 1))
                                    (line (string-to-number (match-string 2)))
                                    (type (if (string= (match-string 3) "warning")
                                             :warning
                                           :error))
                                    (msg (match-string 4))
                                    (region (flymake-diag-region source line)))
                               (when (and region
                                          (string= (file-truename file)
                                                   (file-truename filename)))
                                 (push (flymake-make-diagnostic
                                        source (car region) (cdr region)
                                        type msg)
                                       diagnostics))))
                           (funcall report-fn (nreverse diagnostics)))))
                   (kill-buffer (process-buffer proc))))))))))

(defun erlang-flymake-setup ()
  "Set up the Erlang flymake backend for the current buffer."
  (add-hook 'flymake-diagnostic-functions #'erlang-flymake-backend nil t))

(add-hook 'erlang-mode-hook #'erlang-flymake-setup)
(add-hook 'erlang-mode-hook #'flymake-mode)

(provide 'erlang-flymake)
;; erlang-flymake ends here
