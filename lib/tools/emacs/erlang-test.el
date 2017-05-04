;;; erlang-test.el -*- lexical-binding: t; coding: utf-8-unix -*-

;;; Unit tests for erlang.el.

;; Author: Johan Claesson
;; Created: 2016-05-07
;; Keywords: erlang, languages

;; %CopyrightBegin%
;;
;; Copyright Ericsson AB 2016-2017. All Rights Reserved.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; %CopyrightEnd%


;;; Commentary:

;; This library require GNU Emacs 25 or later.
;;
;; There are two ways to run emacs unit tests.
;;
;; 1. Within a running emacs process.  Load this file.  Then to run
;; all defined test cases:
;;
;; M-x ert RET t RET
;;
;; To run only the erlang test cases:
;;
;; M-x ert RET "^erlang" RET
;;
;;
;; 2. In a new stand-alone emacs process.  This process exits
;; when it executed the tests.  For example:
;;
;; emacs -Q -batch -L . -l erlang.el -l erlang-test.el \
;;       -f ert-run-tests-batch-and-exit
;;
;; The -L option adds a directory to the load-path.  It should be the
;; directory containing erlang.el and erlang-test.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'erlang)

(defvar erlang-test-code
  '((nil . "-module(erlang_test).")
    (nil . "-import(lists, [map/2]).")
    (nil . "-compile(export_all).")
    ("SYMBOL" . "-define(SYMBOL, value).")
    ("MACRO" . "-define(MACRO(X), X + X).")
    ("struct" . "-record(struct, {until,maps,are,everywhere}).")
    ("function". "function() -> #struct{}."))
  "Alist of erlang test code.
Each entry have the format (TAGNAME . ERLANG_CODE).  If TAGNAME
is nil there is no definitions in the ERLANG_CODE.  The
ERLANG_CODE is a single line of erlang code.  These lines will be
concatenated to form an erlang file to test on.")


(ert-deftest erlang-test-tags ()
  (let* ((dir (make-temp-file "erlang-test" t))
         (erlang-file (expand-file-name "erlang_test.erl" dir))
         (tags-file (expand-file-name "TAGS" dir))
         (old-tags-file-name (default-value 'tags-file-name))
         (old-tags-table-list (default-value 'tags-table-list))
         tags-file-name
         tags-table-list
         tags-table-set-list
         tags-add-tables
         tags-completion-table
         erlang-buffer
         erlang-mode-hook
         prog-mode-hook
         erlang-shell-mode-hook)
    (unwind-protect
        (progn
          (setq-default tags-file-name nil)
          (setq-default tags-table-list nil)
          (erlang-test-create-erlang-file erlang-file)
          (erlang-test-compile-tags erlang-file tags-file)
          (setq erlang-buffer (find-file-noselect erlang-file))
          (if (< emacs-major-version 26)
              (progn
                (with-current-buffer erlang-buffer
                  (setq-local tags-file-name tags-file))
                ;; Setting global tags-file-name is a workaround for
                ;; GNU Emacs bug#23164.
                (setq tags-file-name tags-file))
            (visit-tags-table tags-file t))
          (erlang-test-complete-at-point tags-file)
          (erlang-test-completion-table)
          (erlang-test-xref-find-definitions erlang-file erlang-buffer))
      (when (buffer-live-p erlang-buffer)
        (kill-buffer erlang-buffer))
      (let ((tags-buffer (find-buffer-visiting tags-file)))
        (when (buffer-live-p tags-buffer)
          (kill-buffer tags-buffer)))
      (when (file-exists-p dir)
        (delete-directory dir t))
      (setq-default tags-file-name old-tags-file-name)
      (setq-default tags-table-list old-tags-table-list))))

(defun erlang-test-create-erlang-file (erlang-file)
  (with-temp-file erlang-file
    (cl-loop for (_ . code) in erlang-test-code
             do (insert code "\n"))))

(defun erlang-test-compile-tags (erlang-file tags-file)
  (should (zerop (call-process "etags" nil nil nil
                               "-o" tags-file
                               erlang-file))))

(defun erlang-test-completion-table ()
  (let ((erlang-replace-etags-tags-completion-table t))
    (setq tags-completion-table nil)
    (tags-completion-table))
  (should (equal (sort tags-completion-table #'string-lessp)
                 (sort (erlang-expected-completion-table) #'string-lessp))))

(defun erlang-expected-completion-table ()
  (append (cl-loop for (symbol . _) in erlang-test-code
                   when (stringp symbol)
                   append (list symbol (concat "erlang_test:" symbol)))
          (list "erlang_test:" "erlang_test:module_info")))

(defun erlang-test-xref-find-definitions (erlang-file erlang-buffer)
  (cl-loop for (tagname . code) in erlang-test-code
           for line = 1 then (1+ line)
           do (when tagname
                (switch-to-buffer erlang-buffer)
                (erlang-test-xref-jump tagname erlang-file line)
                (erlang-test-xref-jump (concat "erlang_test:" tagname)
                                       erlang-file line)))
  (erlang-test-xref-jump "erlang_test:" erlang-file 1))

(defun erlang-test-xref-jump (id expected-file expected-line)
  (goto-char (point-max))
  (insert "\n%% " id)
  (save-buffer)
  (if (fboundp 'xref-find-definitions)
      (xref-find-definitions (erlang-id-to-string
                              (erlang-get-identifier-at-point)))
    (error "xref-find-definitions not defined (too old emacs?)"))
  (erlang-test-verify-pos expected-file expected-line))

(defun erlang-test-verify-pos (expected-file expected-line)
  (should (string-equal (file-truename expected-file)
                        (file-truename (buffer-file-name))))
  (should (eq expected-line (line-number-at-pos)))
  (should (= (point-at-bol) (point))))

(defun erlang-test-complete-at-point (tags-file)
  (with-temp-buffer
    (erlang-mode)
    (setq-local tags-file-name tags-file)
    (insert "\nerlang_test:fun")
    (erlang-complete-tag)
    (should (looking-back "erlang_test:function" (point-at-bol)))
    (insert "\nfun")
    (erlang-complete-tag)
    (should (looking-back "function" (point-at-bol)))
    (insert "\nerlang_")
    (erlang-complete-tag)
    (should (looking-back "erlang_test:" (point-at-bol)))))


(ert-deftest erlang-test-compile-options ()
  (erlang-test-format-opt t
                          "t")
  (erlang-test-format-opt nil
                          "nil")
  (erlang-test-format-opt (cons 1 2)
                          "{1, 2}")
  (erlang-test-format-opt (list 1)
                          "[1]")
  (erlang-test-format-opt (list 1 2)
                          "[1, 2]")
  (erlang-test-format-opt (list 1 2 3)
                          "[1, 2, 3]")
  (erlang-test-format-opt 'symbol
                          "symbol")
  (erlang-test-format-opt "string"
                          "\"string\"")
  (erlang-test-format-opt []
                          "{}")
  (erlang-test-format-opt [1]
                          "{1}")
  (erlang-test-format-opt [1 2]
                          "{1, 2}")
  (erlang-test-format-opt [1 2 (3 [4 5 6] 7)]
                          "{1, 2, [3, {4, 5, 6}, 7]}"))

(defun erlang-test-format-opt (elisp &optional expected-erlang)
  (let ((erlang (inferior-erlang-format-opt elisp)))
    (message "%s -> %s" elisp erlang)
    (when expected-erlang
      (should (equal erlang expected-erlang)))
    erlang))


(ert-deftest erlang-test-parse-id ()
  (cl-loop for id-string in '("fun/10"
                              "qualified-function module:fun/10"
                              "record reko"
                              "macro _SYMBOL"
                              "macro MACRO/10"
                              "module modula"
                              "macro"
                              nil)
           for id-list in '((nil nil "fun" 10)
                            (qualified-function "module" "fun" 10)
                            (record nil "reko" nil)
                            (macro nil "_SYMBOL" nil)
                            (macro nil "MACRO" 10)
                            (module nil "modula" nil)
                            (nil nil "macro" nil)
                            nil)
           for id-list2 = (erlang-id-to-list id-string)
           do (should (equal id-list id-list2))
           for id-string2 = (erlang-id-to-string id-list)
           do (should (equal id-string id-string2))
           collect id-list2))


(provide 'erlang-test)

;;; erlang-test.el ends here
