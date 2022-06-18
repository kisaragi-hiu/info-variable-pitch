;;; info-variable-pitch.el --- Proportional fonts in Info only when appropriate -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (s "1.12.0"))
;; Homepage: https://github.com/kisaragi-hiu/info-variable-pitch
;; Keywords: faces


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Like org-variable-pitch but for Info.

;;; Code:

(require 'face-remap)
(require 's)

(defgroup info-variable-pitch nil
  "Settings for info-variable-pitch."
  :group 'info
  :prefix "info-variable-pitch-")

(defcustom info-variable-pitch-want-upper-as-variable t
  "Whether to fontify upper case as a variable.

This makes Emacs Lisp docs look nicer at the expense of
highlighting the likes of IEEE, GNU, REPL, FSF, etc. as if they
are variables.

This should be set before the package is loaded."
  :group 'info-variable-pitch
  :type 'boolean)
(defvar-local info-variable-pitch--face-remap-entries nil)

(defvar info-variable-pitch--font-lock-keywords
  `(;; Indented stuff
    (,(rx bol (+ " ")
          (or "|" "-" ; diagrams
              "error→" ; error messages
              "nil"
              "$" ; shell
              "#"
              ;; (elisp)Integer Basics
              (seq " " (opt (any "+-")) digit (any ". "))
              ;; Lisp code blocks
              (seq "("
                   ;; If the first character is upper case, this is
                   ;; more likely to be prose. Most callables don't
                   ;; start with an upper case letter.
                   (not (any upper))
                   ;; If this "form" is closed by the second
                   ;; character, it's probably a bullet like (1).
                   (not ")")))
          (* any))
     (0
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (put-text-property
         start end
         'face 'fixed-pitch))))
    ;; 8+ spaces must be a code block, I hope...
    ("^[ \t]\\{8,\\}\\(.*\\)"
     (0
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (unless (memq 'invisible (text-properties-at start))
          (put-text-property
           start end
           'face 'fixed-pitch)))))
    ,@(when info-variable-pitch-want-upper-as-variable
        ;; Treat all UPPER-CASE to be variables
        '(("[[:upper:]][[:upper:]][[:upper:][:digit:]-]*" . 'fixed-pitch)))
    ;; See (info "(elisp)Buffer Text Notation")
    ;; Point location indicator.
    ("★" . 'font-lock-constant-face)
    ;; (,(rx (or "&optional" "&rest" "&key"))
    ;;  . 'font-lock-type-face)
    ;; Leading spaces or the " -- " for " -- Function: "
    (,(rx bol (or " -- " (+ " "))) . 'fixed-pitch)
    (,(rx bol " -- \\(?:Variable\\|User Option\\): "
          (group (* any)))
     (1 (put-text-property
         (match-beginning 1)
         (match-end 1)
         'face '(fixed-pitch
                 font-lock-variable-name-face))))
    (,(rx bol " -- "
          (or "Function" "Macro" "Command" "Special Form")
          ": "
          (group (* (not (any " "))))
          (group (opt " " (* any))))
     (1 (put-text-property
         (match-beginning 1)
         (match-end 1)
         'face '(fixed-pitch
                 font-lock-function-name-face)))
     (2
      ;; -- Function: foo integer1 &optional integer2 &rest integers
      ;; &optional and &rest are highlighted with
      ;; `font-lock-type-face', the rest is highlighted with info-variable-pitch-fixed-face.
      (let ((start (match-beginning 2))
            (end (match-end 2))
            (str (match-string 2)))
        (put-text-property
         start end
         'face 'fixed-pitch)
        (dolist (pair (s-matched-positions-all
                       ,(rx (or "&rest" "&optional"
                                "&key" "&allow-other-keys"))
                       str))
          (put-text-property
           (+ start (car pair))
           (+ start (cdr pair))
           'face '(fixed-pitch
                   font-lock-type-face))))))))

(defun info-variable-pitch-mode--on ()
  "Do the actual work when enabling `info-variable-pitch-mode'."
  (variable-pitch-mode)
  (font-lock-add-keywords nil info-variable-pitch--font-lock-keywords)
  ;; This doesn't disable case fold for whatever reason:
  ;;   (setq-local font-lock-defaults
  ;;               '(Info-mode-font-lock-keywords t nil))
  ;; Just... set it directly. Sigh.
  (setq-local font-lock-keywords-case-fold-search nil)
  (dolist (face '(font-lock-keyword-face
                  Info-quoted))
    (when (facep face)
      (push (face-remap-add-relative face 'fixed-pitch)
            info-variable-pitch--face-remap-entries)))
  (font-lock-flush))

(defun info-variable-pitch-mode--off ()
  "Do the actual work when disabling `info-variable-pitch-mode'."
  (variable-pitch-mode -1)
  (setq-local font-lock-keywords-case-fold-search t)
  (mapc #'face-remap-remove-relative info-variable-pitch--face-remap-entries)
  (setq info-variable-pitch--face-remap-entries nil)
  ;; This is useful during development to allow
  ;; `info-variable-pitch--font-lock-keywords' to change without
  ;; making it impossible to undo our changes, but it's wrong as
  ;; Info's default keywords may not have stayed the same. Leaving it
  ;; here commented out for convenience.
  ;;
  ;; (setq font-lock-keywords
  ;;       '(t
  ;;         (("‘\\([‘’]\\|[^‘’]*\\)’"
  ;;           (1 'Info-quoted)))
  ;;         ("‘\\([‘’]\\|[^‘’]*\\)’"
  ;;          (1 'Info-quoted))))
  ;;
  (font-lock-remove-keywords nil info-variable-pitch--font-lock-keywords))

;;;###autoload
(define-minor-mode info-variable-pitch-mode
  "Info variable-pitch mode."
  :global nil :lighter IVP
  (if info-variable-pitch-mode
      (info-variable-pitch-mode--on)
    (info-variable-pitch-mode--off)))

(provide 'info-variable-pitch)

;;; info-variable-pitch.el ends here
