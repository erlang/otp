;;; erldoc.el --- browse Erlang/OTP documentation    -*- lexical-binding: t; -*-

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

;; Crawl Erlang/OTP HTML documentation and generate lookup tables.
;;
;; This package depends on `cl-lib', `pcase' and
;; `libxml-parse-html-region'.  Emacs 24+ compiled with libxml2 should
;; work.  On Emacs 24.1 and 24.2 do `M-x package-install RET cl-lib
;; RET' to install `cl-lib'.
;;
;; Please customise `erldoc-man-index' to point to your local OTP
;; documentation.
;;
;; To use:
;;
;;   (define-key help-map "u" 'erldoc-browse)
;;   (define-key help-map "t" 'erldoc-browse-topic)
;;   (define-key help-map "a" 'erldoc-apropos)
;;
;; Note: these commands trigger indexing OTP documentation on first
;; run with cache to disk which may take 1-2 minutes.


;;; Examples:

;; 1. `M-x erldoc-browse RET erlang:integer_to_binary/2 RET' opens the
;;    `erlang' manual anchored on the entry for `integer_to_binary/2'.
;;
;; 2. `M-x erldoc-apropos RET first RET' list all MFAs matching
;;    substring `first'.
;;
;; 3. `M-x erldoc-browse-topic RET efficiency_guide#Introduction RET'
;;    opens chapter `Introduction' of the `Efficiency Guide' in the
;;    browser.

;;; History:

;; Written in December 2013 as a temporary solution to help me browse
;; the rich Erlang/OTP documentation. Three years on I find myself
;; still using it every day.  - Leo (2016)

;;; Code:

(eval-when-compile (require 'url-parse))
(require 'cl-lib)
(require 'erlang)

(eval-and-compile                       ;for emacs < 24.3
  (or (fboundp 'user-error) (defalias 'user-error 'error)))

(defgroup erldoc nil
  "Browse Erlang document."
  :group 'help)

(defcustom erldoc-man-index "http://www.erlang.org/doc/man_index.html"
  "The URL to the man_index.html page.
Note it is advisable to customise this to a local URL for example
`file:///usr/local/19.1/lib/erlang/doc/man_index.html' to speed
up the indexing."
  :type 'string
  :group 'erldoc)

(defcustom erldoc-verify-man-path nil
  "If non-nil verify man path existence for `file://'."
  :type 'boolean
  :group 'erldoc)

(defcustom erldoc-output-file (locate-user-emacs-file "cache/erldoc")
  "File to store the parsed results."
  :type 'file
  :group 'erldoc)

(defcustom erldoc-no-signature-function #'ignore
  "Notification function called if no function signature was found."
  :type '(choice (function-item :tag "Ignore" ignore)
                 (function-item :tag "Warn" warn)
                 (function-item :tag "Error" error))
  :group 'erldoc)

(defun erldoc-strip-string (s)
  (let* ((re "[ \t\n\r\f\v\u00a0]+")
         (from (if (string-match (concat "\\`" re) s) (match-end 0) 0))
         (to (and (string-match (concat re "\\'") s) (match-beginning 0))))
    (substring s from (and to (max to from)))))

;; Note: don't know how to get the BASE-URL to
;; `libxml-parse-html-region' to work.
(defun erldoc-expand-url (url base-url)
  (if (url-type (url-generic-parse-url url))
      url
    (let* ((base (url-generic-parse-url base-url))
           (dir (directory-file-name (file-name-directory (url-filename base)))))
      (setf (url-filename base) (expand-file-name url dir))
      (url-recreate-url base))))

(defun erldoc-parse-html (url)
  (with-temp-buffer
    (url-insert-file-contents url)
    (libxml-parse-html-region (point-min) (point-max))))

(defalias 'erldoc-dom-text-node-p #'stringp)

(defun erldoc-dom-attributes (dom)
  (and (not (erldoc-dom-text-node-p dom)) (cadr dom)))

(defun erldoc-dom-get-attribute (dom attrib-name)
  (cdr (assq attrib-name (erldoc-dom-attributes dom))))

(defun erldoc-dom-children (dom)
  (and (not (erldoc-dom-text-node-p dom)) (cddr dom)))

(defun erldoc-dom-get-text (dom)
  (let ((text (car (last (erldoc-dom-children dom)))))
    (and (erldoc-dom-text-node-p text) text)))

(defvar erldoc-dom-walk-parent nil)
(defvar erldoc-dom-walk-siblings nil)

(defun erldoc-dom-walk (dom k)
  (funcall k dom)
  (let ((erldoc-dom-walk-parent dom)
        (erldoc-dom-walk-siblings (unless (erldoc-dom-text-node-p dom)
                                    (cddr dom))))
    (dolist (child erldoc-dom-walk-siblings)
      (erldoc-dom-walk child k))))

(defun erldoc-dom-get-element (dom element-name)
  (catch 'return
    (erldoc-dom-walk dom (lambda (d)
                           (when (eq (car-safe d) element-name)
                             (throw 'return d))))))

(defun erldoc-dom-get-element-by-id (dom id)
  (catch 'return
    (erldoc-dom-walk dom (lambda (d)
                           (when (equal (erldoc-dom-get-attribute d 'id) id)
                             (throw 'return d))))))

(defun erldoc-dom-get-elements-by-id (dom id)
  (let (result)
    (erldoc-dom-walk dom (lambda (d)
                           (when (equal (erldoc-dom-get-attribute d 'id) id)
                             (push d result))))
    (nreverse result)))

(defun erldoc-fix-path (url)
  (if (and erldoc-verify-man-path
           ;; Could only verify local files
           (equal (url-type (url-generic-parse-url url)) "file"))
      (let* ((obj (url-generic-parse-url url))
             (new (car (file-expand-wildcards
                        (replace-regexp-in-string
                         "-[0-9]+\\(?:[.][0-9]+\\)*" "*"
                         (url-filename obj))))))
        (or new (error "File %s does not exist" (url-filename obj)))
        (setf (url-filename obj) new)
        (url-recreate-url obj))
    url))

(defun erldoc-parse-man-index (url)
  (let ((table (erldoc-dom-get-element (erldoc-parse-html url) 'table))
        (mans))
    (erldoc-dom-walk
     table
     (lambda (d)
       (when (eq (car-safe d) 'a)
         (let ((href (erldoc-dom-get-attribute d 'href)))
           (when (and href (not (string-match-p "index\\.html\\'" href)))
             (with-demoted-errors "erldoc-parse-man-index: %S"
               (push (cons (erldoc-dom-get-text d)
                           (erldoc-fix-path (erldoc-expand-url href url)))
                     mans)))))))
    (nreverse mans)))

(defun erldoc-parse-man (man)
  (let ((dom (erldoc-parse-html (cdr man)))
        (table (make-hash-table :test #'equal)))
    (erldoc-dom-walk
     (erldoc-dom-get-element-by-id dom "loadscrollpos")
     (lambda (d)
       (let ((href (erldoc-dom-get-attribute d 'href)))
         (when (and href (string-match "#" href))
           (puthash (substring href (match-end 0))
                    (list (concat (car man) ":" (erldoc-strip-string
                                                 (erldoc-dom-get-text d)))
                          (erldoc-expand-url href (cdr man)))
                    table)))))
    (let ((span-content
           (lambda (span)
             (let ((texts))
               (erldoc-dom-walk span
                                (lambda (d)
                                  (and (erldoc-dom-text-node-p d)
                                       (push (erldoc-strip-string d) texts))))
               (and texts (mapconcat 'identity (nreverse texts) " ")))))
          entries)
      (erldoc-dom-walk
       dom
       (lambda (d)
         ;; Get the full function signature.
         (when (and (eq (car-safe d) 'a)
                    (gethash (erldoc-dom-get-attribute d 'name) table))
           (let* ((name (erldoc-dom-get-attribute d 'name))
                  (mfa-url (gethash name table))
                  (mfa (car mfa-url))
                  (sig (or (funcall span-content d)
                           (funcall span-content
                                    (or (erldoc-dom-get-element d 'span)
                                        (cadr
                                         (memq d erldoc-dom-walk-siblings))))
                           (progn
                             (funcall erldoc-no-signature-function
                                      "erldoc-parse-man: no sig for %s"
                                      mfa)
                             nil))))
             (push (append mfa-url (list sig))
                   entries)))
         ;; Get data types
         (when (and (eq (car-safe d) 'a)
                    (string-prefix-p "type-"
                                     (or (erldoc-dom-get-attribute d 'name) "")))
           (push (list (concat (car man) ":" (funcall span-content d))
                       (concat (cdr man) "#" (erldoc-dom-get-attribute d 'name))
                       (funcall span-content erldoc-dom-walk-parent))
                 entries))))
      entries)))

(defun erldoc-parse-all (man-index output &optional json)
  (let* ((output (expand-file-name output))
         (table (make-hash-table :size 11503 :test #'equal))
         (mans (erldoc-parse-man-index man-index))
         (progress 1)
         (reporter (make-progress-reporter "Parsing Erlang/OTP documentation"
                                           progress (length mans)))
         fails all)
    (dolist (man mans)
      (condition-case err
          (push (erldoc-parse-man man) all)
        (error (push (error-message-string err) fails)))
      (accept-process-output nil 0.01)
      (progress-reporter-update reporter (cl-incf progress)))
    (when fails
      (display-warning 'erldoc-parse-all
                       (format "\n\n%s" (mapconcat #'identity fails "\n"))
                       :error))
    (progress-reporter-done reporter)
    (mapc (lambda (x) (puthash (car x) (cdr x) table))
          (apply #'nconc (nreverse all)))
    (with-temp-buffer
      (if (not json)
          (pp table (current-buffer))
        (eval-and-compile (require 'json))
        (let ((json-encoding-pretty-print t))
          (insert (json-encode table))))
      (unless (file-directory-p (file-name-directory output))
        (make-directory (file-name-directory output) t))
      (write-region nil nil output nil nil nil 'ask))))

(defun erldoc-otp-release ()
  "Get the otp release version (as string) or nil if not found."
  (let ((otp (erldoc-dom-get-text
              (erldoc-dom-get-element
               (erldoc-parse-html
                (erldoc-expand-url "index.html" erldoc-man-index))
               'title))))
    (and (string-match "[0-9.]+\\'" otp) (match-string 0 otp))))

(defvar erldoc-browse-history nil)
(defvar erldoc-lookup-table nil)

(defun erldoc-lookup-table ()
  (or erldoc-lookup-table
      (progn
        (unless (file-exists-p erldoc-output-file)
          (let ((of (pcase (erldoc-otp-release)
                      (`nil erldoc-output-file)
                      (ver (concat erldoc-output-file "-" ver)))))
            (unless (file-exists-p of)
              (erldoc-parse-all erldoc-man-index of))
            (unless (string= erldoc-output-file of)
              (make-symbolic-link (expand-file-name of) erldoc-output-file))))
        (setq erldoc-lookup-table
              (with-temp-buffer
                (insert-file-contents erldoc-output-file)
                (read (current-buffer)))))))

(defun erldoc-best-matches (mfa)
  (pcase mfa
    ((and `(,m ,f) (let a (erlang-get-function-arity)))
     (let ((mfa (format "%s:%s/%s" m f a)))
       (cond ((gethash mfa (erldoc-lookup-table)) (list mfa))
             (m (all-completions (concat m ":" f "/") (erldoc-lookup-table)))
             (t (let* ((mod (erlang-get-module))
                       (mf1 (and mod (concat mod ":" f "/")))
                       (mf2 (concat "erlang:" f "/"))
                       (re (concat ":" (regexp-quote f) "/")))
                  (or (and mf1 (all-completions mf1 (erldoc-lookup-table)))
                      (all-completions mf2 (erldoc-lookup-table))
                      (cl-loop for k being the hash-keys of (erldoc-lookup-table)
                               when (string-match-p re k)
                               collect k)))))))))

;;;###autoload
(defun erldoc-browse (mfa)
  (interactive
   (let ((default
           ;; `erlang-mode-syntax-table' is lazily initialised.
           (with-syntax-table (or erlang-mode-syntax-table (standard-syntax-table))
             (ignore-errors
               (erldoc-best-matches
                (or (erlang-get-function-under-point)
                    (save-excursion
                      (goto-char (or (cadr (syntax-ppss)) (point)))
                      (erlang-get-function-under-point))))))))
     (list (completing-read (format (if default "Function {%d %s} (default %s): "
                                      "Function: ")
                                    (length default)
                                    (if (= (length default) 1) "guess" "guesses")
                                    (car default))
                            (erldoc-lookup-table)
                            nil t nil 'erldoc-browse-history default))))
  (or (stringp mfa)
      (signal 'wrong-type-argument (list 'string mfa 'mfa)))
  (browse-url (or (car (gethash mfa (erldoc-lookup-table)))
                  (user-error "No documentation for %s" mfa))))

;;;###autoload
(defun erldoc-apropos (pattern)
  (interactive "sPattern: ")
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (princ (concat "Erldoc apropos pattern: " pattern "\n\n"))
      (maphash (lambda (k v)
                 (when (string-match-p pattern k)
                   (insert-text-button k :type 'help-url
                                       'help-args (list (car v)))
                   (insert "\n")))
               (erldoc-lookup-table)))))

(defun erldoc-tokenize-signature (sig)
  ;; Divide SIG into (MF ARGLIST RETTYPE)
  (let ((from (if (string-match "\\`.+?(" sig)
                  (1- (match-end 0))
                0))
        (to (and (string-match "\\s-*->\\s-*.*?\\'" sig) (match-beginning 0))))
    (list (erldoc-strip-string (substring sig 0 from))
          (erldoc-strip-string (substring sig from (and to (max from to))))
          (and to (erldoc-strip-string (substring sig to))))))

(defun erldoc-format-signature (mod fn)
  (when (and mod fn (or erldoc-lookup-table
                        (file-exists-p erldoc-output-file)))
    (let ((re (concat "\\`" mod ":" fn "/\\([0-9]+\\)\\'"))
          (sigs))
      (maphash (lambda (k v)
                 (when (string-match re k)
                   (if (cadr v)
                       (push (cons (string-to-number (match-string 1 k))
                                   (cdr (erldoc-tokenize-signature (cadr v))))
                             sigs)
                     (funcall erldoc-no-signature-function
                              "erldoc-format-signature: No sig for %s" k))))
               (erldoc-lookup-table))
      (when sigs
        ;; Mostly single return type but there are exceptions such as
        ;; `beam_lib:chunks/2,3'.
        (let ((single-rettype
               (cl-reduce (lambda (x1 x2) (and x1 x2 (equal x1 x2) x1))
                          sigs :key #'cl-caddr))
              (sigs (sort sigs #'car-less-than-car)))
          (if single-rettype
              (concat mod ":" fn (mapconcat #'cadr sigs " | ") " " single-rettype)
            (mapconcat (lambda (x) (concat mod ":" fn (nth 1 x) " " (nth 2 x)))
                       sigs "\n")))))))

;;;###autoload
(defun erldoc-eldoc-function ()
  "A function suitable for `eldoc-documentation-function'."
  (save-excursion
    (pcase (erlang-get-function-under-point)
      (`(,_ nil) )
      (`(nil ,fn)  (erldoc-format-signature "erlang" fn))
      (`(,mod ,fn) (erldoc-format-signature mod fn)))))

(defun erldoc-parse-eeps-index ()
  (let* ((url "http://www.erlang.org/eeps/")
         (table (catch 'return
                  (erldoc-dom-walk (erldoc-parse-html url)
                                   (lambda (d)
                                     (and (eq (car-safe d) 'table)
                                          (equal (erldoc-dom-get-attribute d 'summary)
                                                 "Numerical Index of EEPs")
                                          (throw 'return d))))))
         (fix-title (lambda (title)
                      (replace-regexp-in-string
                       "`` *" "" (replace-regexp-in-string " *``, *" " by " title))))
         (result))
    (erldoc-dom-walk
     table (lambda (d)
             (when (eq (car-safe d) 'a)
               (push (cons (funcall fix-title (erldoc-dom-get-attribute d 'title))
                           (erldoc-expand-url
                            (erldoc-dom-get-attribute d 'href)
                            url))
                     result))))
    (nreverse result)))

(defvar erldoc-user-guides nil)

(defvar erldoc-missing-user-guides
  '("compiler" "hipe" "kernel" "os_mon" "parsetools")
  "List of standard Erlang applications with no user guides.")

;; Search in `code:lib_dir/0' using find LIB_DIR -type f -name
;; '*_app.html'.
(defvar erldoc-app-manuals '("crypto" "diameter" "erl_docgen"
                             "kernel" "observer" "os_mon"
                             "runtime_tools" "sasl" "snmp"
                             "ssl" "test_server"
                             ("ssh" . "SSH") ("stdlib" . "STDLIB")
                             ("hipe" . "HiPE"))
  "List of applications that come with a manual.")

(defun erldoc-user-guide-chapters (user-guide)
  (pcase-let ((`(,name . ,url) user-guide))
    (unless (member name erldoc-missing-user-guides)
      (let ((chaps (erldoc-dom-get-elements-by-id
                    (erldoc-dom-get-element-by-id (erldoc-parse-html url) "leftnav")
                    "no")))
        (or chaps (warn "erldoc-user-guide-chapters no chapters found for `%s'"
                        (cdr user-guide)))
        (mapcar (lambda (li)
                  (cons (concat name "#" (erldoc-dom-get-attribute li 'title))
                        (erldoc-expand-url (erldoc-dom-get-attribute
                                            (erldoc-dom-get-element li 'a) 'href)
                                           url)))
                chaps)))))

(defun erldoc-user-guides-1 ()
  (let ((url (erldoc-expand-url "applications.html" erldoc-man-index))
        app-guides app-mans)
    (erldoc-dom-walk
     (erldoc-parse-html url)
     (lambda (d)
       (when (and (eq (car-safe d) 'a)
                  (not (string-match-p "\\`[0-9.]+\\'" (erldoc-dom-get-text d))))
         (with-demoted-errors "erldoc-user-guides-1: %S"
           (let ((name (erldoc-strip-string (erldoc-dom-get-text d)))
                 (index-page (erldoc-fix-path (erldoc-expand-url
                                               (erldoc-dom-get-attribute d 'href) url))))
             (push (cons name (if (member name erldoc-missing-user-guides)
                                  index-page
                                (erldoc-expand-url "users_guide.html" index-page)))
                   app-guides)
             ;; Collect application manuals.
             (pcase (assoc name (mapcar (lambda (x) (if (consp x) x (cons x x)))
                                        erldoc-app-manuals))
               (`(,_ . ,manual)
                (push (cons name
                            (erldoc-expand-url (format "%s_app.html" manual)
                                               index-page))
                      app-mans))))))))
    (list (nreverse app-guides)
          (nreverse app-mans))))

(defun erldoc-user-guides ()
  (or erldoc-user-guides
      (let ((file (concat erldoc-output-file "-topics")))
        (unless (file-exists-p file)
          (unless (file-directory-p (file-name-directory file))
            (make-directory (file-name-directory file) t))
          (with-temp-buffer
            (pcase-let ((`(,guides ,mans) (erldoc-user-guides-1)))
              (pp (append (cl-mapcan #'erldoc-user-guide-chapters
                                     (append (mapcar
                                              (lambda (dir)
                                                (cons dir (erldoc-expand-url
                                                           (concat dir "/users_guide.html")
                                                           erldoc-man-index)))
                                              '("design_principles"
                                                "efficiency_guide"
                                                "embedded"
                                                "getting_started"
                                                "installation_guide"
                                                "oam"
                                                "programming_examples"
                                                "reference_manual"
                                                "system_architecture_intro"
                                                "system_principles"
                                                "tutorial"))
                                             guides))
                          (mapcar (lambda (man)
                                    (pcase-let ((`(,name . ,url) man))
                                      (cons (concat name " (App)") url)))
                                  mans)
                          (erldoc-parse-eeps-index))
                  (current-buffer)))
            (write-region nil nil file nil nil nil 'ask)))
        (setq erldoc-user-guides (with-temp-buffer (insert-file-contents file)
                                                   (read (current-buffer)))))))

;;;###autoload
(defun erldoc-browse-topic (topic)
  (interactive
   (list (completing-read "User guide: " (erldoc-user-guides) nil t)))
  (browse-url (cdr (assoc topic (erldoc-user-guides)))))

(provide 'erldoc)

;; Local variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; erldoc.el ends here
