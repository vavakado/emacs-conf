;;; blip-mode.el --- Major mode for editing Blip language -*- lexical-binding: t; -*-

;; Author: vavakado
;; Version: 0.1
;; Keywords: languages
;; URL: none

;;; Commentary:
;; Major mode for editing the Blip language
;; Supports syntax highlighting for parentheses, strings, numbers, binary operators (+, -, *, /), and identifiers.

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
    (modify-syntax-entry ?/ "." st)
    ;; Identifiers
    (modify-syntax-entry ?_ "w" st)
    ;; Comments
    (modify-syntax-entry ?\; "<" st)
    st)
  "Syntax table for `blip-mode'.")

(defconst blip-font-lock-keywords
  `(("\\([0-9]+\\)" 1 font-lock-constant-face) ;; Numbers
    ("\\(\\+\\|-\\|\\*\\|/\\)" 1 font-lock-keyword-face) ;; Operators
    ("\\(\".*?\"\\)" 1 font-lock-string-face) ;; Strings
    ("(\\|)" 0 font-lock-type-face) ;; Parentheses
    (";.*$" 0 font-lock-comment-face) ;; Comments
    ("\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-variable-name-face)) ;; Identifiers
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
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local c-basic-offset 2)
  (setq-local tab-width 2)
  ;; Keybindings for evaluation
  (define-key blip-mode-map (kbd "C-c C-c") 'blip-eval-buffer)
  (define-key blip-mode-map (kbd "C-c C-r") 'blip-eval-region))

(defun blip-eval-buffer ()
  "Evaluate the current Blip buffer and display the output."
  (interactive)
  (let* ((code (buffer-string))
         (output-buffer (get-buffer-create "*Blip Output*"))
         (blip-executable "blip")
         (temp-file (make-temp-file "blip-eval-")))
    (with-temp-buffer
      (insert code)
      (set-buffer-file-coding-system 'utf-8-unix)
      (write-region nil nil temp-file nil 'silent))
    (with-current-buffer output-buffer
      (erase-buffer)
      (let ((result (call-process blip-executable nil output-buffer nil temp-file)))
        (if (= result 0)
            (message "Blip evaluation successful!")
          (message "Blip evaluation failed with code %d" result))))
    (delete-file temp-file)
    (display-buffer output-buffer)))

(defun blip-eval-region (start end)
  "Evaluate the selected region of Blip code and display the output."
  (interactive "r")
  (let* ((code (buffer-substring start end))
         (output-buffer (get-buffer-create "*Blip Output*"))
         (blip-executable "blip")
         (temp-file (make-temp-file "blip-eval-")))
    (with-temp-buffer
      (insert code)
      (set-buffer-file-coding-system 'utf-8-unix)
      (write-region nil nil temp-file nil 'silent))
    (with-current-buffer output-buffer
      (erase-buffer)
      (let ((result (call-process blip-executable nil output-buffer nil temp-file)))
        (if (= result 0)
            (message "Blip region evaluation successful!")
          (message "Blip region evaluation failed with code %d" result))))
    (delete-file temp-file)
    (display-buffer output-buffer)))

(add-to-list 'auto-mode-alist '("\\.blip\\'" . blip-mode))

(provide 'blip-mode)

;;; blip-mode.el ends here
