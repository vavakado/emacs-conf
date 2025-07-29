;;; blip-mode.el --- Major mode for editing Blip language -*- lexical-binding: t; -*-

;; Author: vavakado
;; Version: 0.1
;; Keywords: languages
;; URL: none

;;; Commentary:
;; Major mode for editing the Blip language
;; Supports syntax highlighting for parentheses, strings, numbers, and binary operators (+, -, *).

;;; Code:

(defvar blip-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Whitespace
    (modify-syntax-entry ?\s " " st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?\t " " st)
    ;; Parentheses
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    ;; Operators
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?* "." st)
    st)
  "Syntax table for `blip-mode'.")

(defconst blip-font-lock-keywords
  `(("\\([0-9]+\\)" 1 font-lock-constant-face) ;; Numbers
    ("\\(\\+\\|-\\|\\*\\)" 1 font-lock-keyword-face) ;; Operators
    ("\\(\".*?\"\\)" 1 font-lock-string-face) ;; Strings
    ("(\\|)" 0 font-lock-type-face)) ;; Parentheses
  "Font lock keywords for `blip-mode'.")

(defun blip-indent-line ()
  "Indent current line for `blip-mode'."
  (interactive)
  (let ((indent-col 0)
        (current-pos (point))
        (indent-step 2))
    (save-excursion
      (beginning-of-line)
      (let ((parse-state (syntax-ppss)))
        (setq indent-col (* (car parse-state) indent-step))))
    (indent-line-to indent-col)))

(define-derived-mode blip-mode prog-mode "Blip"
  "Major mode for editing the Blip language."
  :syntax-table blip-mode-syntax-table
  (setq font-lock-defaults '(blip-font-lock-keywords))
  (setq-local indent-line-function 'blip-indent-line)
  (setq-local comment-start ";; ")
  (setq-local comment-end ""))

(add-to-list 'auto-mode-alist '("\\.blip\\'" . blip-mode))

(provide 'blip-mode)

;;; blip-mode.el ends here
