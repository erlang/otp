;;; erlang-test.el -*- lexical-binding: t; coding: utf-8-unix -*-

;;; Unit tests for erlang.el.

;; Author:   Johan Claesson
;; Created: 2016-05-07
;; Keywords: erlang, languages

;; %CopyrightBegin%
;;
;; Copyright Ericsson AB 2016. All Rights Reserved.
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
         erlang-buffer
         erlang-mode-hook
         prog-mode-hook
         erlang-shell-mode-hook
         tags-add-tables)
    (unwind-protect
        (progn
          (setq-default tags-file-name nil)
          (setq-default tags-table-list nil)
          (erlang-test-create-erlang-file erlang-file)
          (erlang-test-compile-tags erlang-file tags-file)
          (setq erlang-buffer (find-file-noselect erlang-file))
          (with-current-buffer erlang-buffer
            (setq-local tags-file-name tags-file))
          ;; Setting global tags-file-name is a workaround for
          ;; GNU Emacs bug#23164.
          (setq tags-file-name tags-file)
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
                (xref-find-definitions tagname)
                (erlang-test-verify-pos erlang-file line)
                (xref-find-definitions (concat "erlang_test:" tagname))
                (erlang-test-verify-pos erlang-file line)))
  (xref-find-definitions "erlang_test:")
  (erlang-test-verify-pos erlang-file 1))

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
    (should (looking-back "erlang_test:function"))
    (insert "\nfun")
    (erlang-complete-tag)
    (should (looking-back "function"))
    (insert "\nerlang_")
    (erlang-complete-tag)
    (should (looking-back "erlang_test:"))))


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


(provide 'erlang-test)

;;; erlang-test.el ends here
