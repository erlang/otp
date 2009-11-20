;;
;; %CopyrightBegin%
;; 
;; Copyright Ericsson AB 2009. All Rights Reserved.
;; 
;; The contents of this file are subject to the Erlang Public License,
;; Version 1.1, (the "License"); you may not use this file except in
;; compliance with the License. You should have received a copy of the
;; Erlang Public License along with this software. If not, it can be
;; retrieved online at http://www.erlang.org/.
;; 
;; Software distributed under the License is distributed on an "AS IS"
;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
;; the License for the specific language governing rights and limitations
;; under the License.
;; 
;; %CopyrightEnd%
;;;
;;; Purpose: Provide EUnit utilities.
;;;
;;; Author: Klas Johansson

(defvar erlang-eunit-separate-src-and-test-directories t
  "*Whether or not to keep source and EUnit test files in separate directories")

;;;
;;; Switch between src/EUnit test buffers
;;;
(defun erlang-eunit-toggle-src-and-test-file-other-window ()
  "Switch to the src file if the EUnit test file is the current
buffer and vice versa"
  (interactive)
  (if (erlang-eunit-test-file-p buffer-file-name)
      (erlang-eunit-open-src-file-other-window buffer-file-name)
    (erlang-eunit-open-test-file-other-window buffer-file-name)))

;;;
;;; Open the EUnit test file which corresponds to a src file
;;;
(defun erlang-eunit-open-test-file-other-window (src-file-path)
  "Open the EUnit test file which corresponds to a src file"
  (find-file-other-window (erlang-eunit-test-filename src-file-path)))


;;;
;;; Open the src file which corresponds to the an EUnit test file
;;;
(defun erlang-eunit-open-src-file-other-window (test-file-path)
  "Open the src file which corresponds to the an EUnit test file"
    (find-file-other-window (erlang-eunit-src-filename test-file-path)))

;;; Return the name and path of the EUnit test file
;;, (input may be either the source filename itself or the EUnit test filename)
(defun erlang-eunit-test-filename (file-path)
  (erlang-eunit-rewrite-filename file-path "test" "_tests"))

;;; Return the name and path of the source file
;;, (input may be either the source filename itself or the EUnit test filename)
(defun erlang-eunit-src-filename (file-path)
  (erlang-eunit-rewrite-filename file-path "src" ""))

;;; Rewrite a filename from the src or test filename to the other
(defun erlang-eunit-rewrite-filename (orig-file-path dest-dirname dest-suffix)
  (let* ((root-dir-name    (erlang-eunit-file-root-dir-name orig-file-path))
	 (src-module-name  (erlang-eunit-source-module-name orig-file-path))
	 (dest-base-name   (concat src-module-name dest-suffix ".erl"))
	 (dest-dir-name-1  (file-name-directory orig-file-path))
	 (dest-dir-name-2  (filename-join root-dir-name dest-dirname))
	 (dest-file-name-1 (filename-join dest-dir-name-1 dest-base-name))
	 (dest-file-name-2 (filename-join dest-dir-name-2 dest-base-name)))
    ;; This function tries to be a bit intelligent: 
    ;; * if there already is a test (or source) file in the same
    ;;   directory as a source (or test) file, it'll be picked
    ;; * if there already is a test (or source) file in a separate
    ;;   test (or src) directory, it'll be picked
    ;; * otherwise it'll resort to whatever alternative (same or
    ;;   separate directories) that the user has chosen
    (cond ((file-readable-p dest-file-name-1) 
	   dest-file-name-1)
	  ((file-readable-p dest-file-name-2) 
	   dest-file-name-2)
	  (erlang-eunit-separate-src-and-test-directories
	   dest-file-name-2)
	  (t
	   dest-file-name-1))))

;;; Checks whether a file is a EUnit test file or not
(defun erlang-eunit-test-file-p (file-path)
  (erlang-eunit-string-match-p "^\\(.+\\)_tests.erl$" file-path))

;;; Return the module name of the source file
;;;     /tmp/foo/src/x.erl        --> x
;;;     /tmp/foo/test/x_tests.erl --> x
(defun erlang-eunit-source-module-name (file-path)
  (interactive)
  (let* ((file-name (file-name-nondirectory file-path))
	 (base-name (file-name-sans-extension file-name)))
    (if (string-match "^\\(.+\\)_tests$" base-name)
	(substring base-name (match-beginning 1) (match-end 1))
      base-name)))

;;; Return the directory name which is common to both src and test
;;;     /tmp/foo/src/x.erl        --> /tmp/foo
;;;     /tmp/foo/test/x_tests.erl --> /tmp/foo
(defun erlang-eunit-file-root-dir-name (file-path)
  (erlang-eunit-dir-parent-dirname (file-name-directory file-path)))

;;; Return the parent directory name of a directory
;;;     /tmp/foo/ --> /tmp
;;;     /tmp/foo  --> /tmp
(defun erlang-eunit-dir-parent-dirname (dir-name)
  (file-name-directory (directory-file-name dir-name)))

;;; Older emacsen don't have string-match-p.
(defun erlang-eunit-string-match-p (regexp string &optional start)
  (if (fboundp 'string-match-p) ;; appeared in emacs 23
      (string-match-p regexp string start)
    (save-match-data ;; fallback for earlier versions of emacs
      (string-match regexp string start))))

;;; Join filenames
(defun filename-join (dir file)
  (if (or (= (elt file 0) ?/)
	  (= (car (last (append dir nil))) ?/))
      (concat dir file)
    (concat dir "/" file)))

;;; Run EUnit tests for the current module
(defun erlang-eunit-run-tests ()
  "Run the EUnit test suite for the current module.

With prefix arg, runs tests with the verbose flag set."
  (interactive)
  (let* ((module-name (erlang-add-quotes-if-needed
		       (erlang-eunit-source-module-name buffer-file-name)))
	 (opts        (if current-prefix-arg ", [verbose]" ""))
	 (command     (format "eunit:test(%s%s)." module-name opts)))
    (erlang-eunit-inferior-erlang-send-command command)))

;;; Compile source and EUnit test file and finally run EUnit tests for
;;; the current module
(defun erlang-eunit-compile-and-run-tests ()
  "Compile the source and test files and run the EUnit test suite.

With prefix arg, compiles for debug and runs tests with the verbose flag set."
  (interactive)
  (let ((src-filename  (erlang-eunit-src-filename  buffer-file-name))
	(test-filename (erlang-eunit-test-filename buffer-file-name)))

    ;; The purpose of out-maneuvering `save-some-buffers', as is done
    ;; below, is to ask the question about saving buffers only once,
    ;; instead of possibly several: one for each file to compile,
    ;; for instance for both x.erl and x_tests.erl.
    (save-some-buffers)
    (flet ((save-some-buffers (&optional any) nil))

      ;; Compilation of the source file is mandatory (the file must
      ;; exist, otherwise the procedure is aborted).  Compilation of the
      ;; test file on the other hand, is optional, since eunit tests may
      ;; be placed in the source file instead.  Any compilation error
      ;; will prevent the subsequent steps to be run (hence the `and')
      (and (erlang-eunit-compile-file src-filename)
	   (if (file-readable-p test-filename)
	       (erlang-eunit-compile-file test-filename)
	     t)
	   (erlang-eunit-run-tests)))))

(defun erlang-eunit-compile-file (file-path)
  (if (file-readable-p file-path)
      (save-excursion
	(set-buffer (find-file-noselect file-path))
	(erlang-compile)
	(erlang-eunit-last-compilation-successful-p))
    (let ((msg (format "Could not read %s" file-path)))
      (erlang-eunit-inferior-erlang-send-command 
       (format "%% WARNING: %s" msg))
      (error msg))))
  
(defun erlang-eunit-last-compilation-successful-p ()
  (save-excursion
    (set-buffer inferior-erlang-buffer)
    (goto-char compilation-parsing-end)
    (erlang-eunit-all-list-elems-fulfill-p
     (lambda (re) (let ((continue t)
			(result   t))
		    (while continue ; ignore warnings, stop at errors
		      (if (re-search-forward re (point-max) t) 
			  (if (erlang-eunit-is-compilation-warning)
			      t
			    (setq result nil)
			    (setq continue nil))
			(setq result t)
			(setq continue nil)))
		    result))
     (mapcar (lambda (e) (car e)) erlang-error-regexp-alist))))

(defun erlang-eunit-is-compilation-warning ()
  (erlang-eunit-string-match-p 
   "[0-9]+: Warning:"
   (buffer-substring (line-beginning-position) (line-end-position))))

(defun erlang-eunit-all-list-elems-fulfill-p (pred list)
  (let ((matches-p t))
    (while (and list matches-p)
      (if (not (funcall pred (car list)))
	  (setq matches-p nil))
      (setq list (cdr list)))
    matches-p))

;;; Evaluate a command in an erlang buffer
(defun erlang-eunit-inferior-erlang-send-command (command)
  "Evaluate a command in an erlang buffer."
  (interactive "P")
  (inferior-erlang-prepare-for-input)
  (inferior-erlang-send-command command)
  (sit-for 0) ;; redisplay
  (inferior-erlang-wait-prompt))


;;;====================================================================
;;; Key bindings
;;;====================================================================

(defvar erlang-eunit-toggle-src-and-test-file-other-window-key "\C-c\C-et"
  "*Key to which the `erlang-eunit-toggle-src-and-test-file-other-window' 
function will be bound.")
(defvar erlang-eunit-compile-and-run-tests-key "\C-c\C-ek"
  "*Key to which the `erlang-eunit-compile-and-run-tests'
function will be bound.")

(defun erlang-eunit-add-key-bindings ()
  (erlang-eunit-ensure-keymap-for-key
   erlang-eunit-toggle-src-and-test-file-other-window-key)
  (local-set-key erlang-eunit-toggle-src-and-test-file-other-window-key
		 'erlang-eunit-toggle-src-and-test-file-other-window)
  (erlang-eunit-ensure-keymap-for-key
   erlang-eunit-compile-and-run-tests-key)
  (local-set-key erlang-eunit-compile-and-run-tests-key
		 'erlang-eunit-compile-and-run-tests))

(defun erlang-eunit-ensure-keymap-for-key (key-seq)
  (let ((prefix-keys (butlast (append key-seq nil)))
	(prefix-seq  ""))
    (while prefix-keys
      (setq prefix-seq (concat prefix-seq (make-string 1 (car prefix-keys))))
      (setq prefix-keys (cdr prefix-keys))
      (if (not (keymapp (lookup-key (current-local-map) prefix-seq)))
	  (local-set-key prefix-seq (make-sparse-keymap))))))

(add-hook 'erlang-mode-hook 'erlang-eunit-add-key-bindings)


(provide 'erlang-eunit)
;; erlang-eunit ends here
