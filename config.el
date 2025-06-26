;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 18)
      doom-big-font (font-spec :family "VictorMono NFM" :size 28 :weight 'semi-bold))

(setq +doom-dashboard-banner-dir "/home/vavakado/Downloads/"
      +doom-dashboard-banner-file "4ea3b58e342ee8599bf7e02a512ee6a8.jpg"
      +doom-dashboard-functions '(doom-dashboard-widget-banner
                                  doom-dashboard-widget-shortmenu
                                  doom-dashboard-widget-loaded))



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(add-to-list 'org-modules 'org-habit t)
(setq org-directory "~/Documents/notes/")
(setq org-startup-with-inline-images t)
;; (setq org-agenda-files (directory-files-recursively "~/Documents/notes/" "\\.org$"))

(setq org-agenda-files "~/.cache/org-agenda-files.txt")

(defun my/org-agenda-files-track-init ()
  "(Re)initialize dynamic agenda files.

This can take a long time, so it is recommended to run this only
on installation and when first tasks are added to many files via
methods the save hook cannot detect, like file synchronization."
  (interactive)
  ;; ;; uncomment if storing org-agenda-files in file
  (make-empty-file org-agenda-files 'force)
  (org-store-new-agenda-file-list
   (directory-files-recursively
    org-directory (rx ".org" eos) nil
    ;; ignore hidden directories like .git and .attach
    (lambda (subdir)
      (not (eq ?. (string-to-char (file-name-nondirectory subdir)))))))
  ;; use ql here if desired
  (org-agenda-files-track-cleanup-files 'full)
  (message "Initialized agenda files"))

(after! org-agenda-files-track
  (defun my/org-agenda-files-track-predicate ()
    "Only track files with specific TODO keywords, checkbox markers,
 or SCHEDULED/DEADLINE entries."
    (let ((pattern (regexp-opt '("TODO" "STRT" "WAIT" "IDEA" "PROJ" "[ ]" "[-]" "[?]" "SCHEDULED" "DEADLINE"))))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward pattern nil t))))

  (setq org-agenda-files-track-predicate #'my/org-agenda-files-track-predicate))



;; I prefer to log TODO creation also
(setq org-treat-insert-todo-heading-as-state-change t)
;; log into LOGBOOK drawer
(setq org-log-into-drawer t)
;; refile into org-roam
;; (setq myroamfiles (directory-files "~/Documents/notes/roam/" t "org$"))
;; (setq org-refile-targets '((org-agenda-files :maxlevel . 5) (myroamfiles :ma level . 5)))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! org-agenda-files-track)
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(setq +lookup-provider-url-alist
      '(("Searxng"           "https://searxng.vavakado.xyz/search?q=%s")
        ("Doom issues"       "https://github.com/orgs/doomemacs/projects/2/views/30?filterQuery=%s")
        ("Doom discourse"    "https://discourse.doomemacs.org/search?q=%s")
        ("Google"            +lookup--online-backend-google "https://google.com/search?q=%s")
        ("oogle images"     "https://www.google.com/images?q=%s")
        ("Google maps"       "https://maps.google.com/maps?q=%s")
        ("Kagi"              "https://kagi.com/search?q=%s")
        ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
        ("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
        ("DevDocs.io"        "https://devdocs.io/#q=%s")
        ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
        ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
        ("Youtube"           "https://youtube.com/results?aq=f&oq=&search_query=%s")
        ("Wolfram alpha"     "https://wolframalpha.com/input/?i=%s")
        ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
        ("MDN"               "https://developer.mozilla.org/en-US/search?q=%s")
        ("Internet archive"  "https://web.archive.org/web/*/%s")
        ("Sourcegraph"       "https://sourcegraph.com/search?q=context:global+%s&patternType=literal")))


(defun org-calc-sleep-duration ()
  "Calculate sleep duration based on SleepTime and WakeTime properties."
  (let ((sleep (org-entry-get (point) "SleepTime"))
        (wake (org-entry-get (point) "WakeTime")))
    (when (and sleep wake)
      (let* ((sleep-parts (mapcar 'string-to-number (split-string sleep ":")))
             (wake-parts (mapcar 'string-to-number (split-string wake ":")))
             (sleep-min (+ (* (car sleep-parts) 60) (cadr sleep-parts)))
             (wake-min (+ (* (car wake-parts) 60) (cadr wake-parts)))
             (diff (if (< wake-min sleep-min)
                       (- (+ wake-min 1440) sleep-min)
                     (- wake-min sleep-min))))
        (format "%02d:%02d" (/ diff 60) (% diff 60))))))
(defun org-insert-sleep-duration ()
  "Calculate and insert SleepDuration property."
  (interactive)
  (let ((duration (org-calc-sleep-duration)))
    (when duration
      (org-entry-put (point) "SleepDuration" duration)
      (message "Inserted sleep duration: %s" duration))))

(map! :leader :desc "Insert sleep duration" "n C-s"  #'org-insert-sleep-duration)

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* Sleep\n:PROPERTIES:\n:SleepTime: xx:xx\n:WakeTime:  xx:xx\n:END:\n- slept well? [ ]\n* %?"
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

(setq org-enable-priority-commands t
      org-highest-priority ?A
      org-default-priority ?C
      org-lowest-priority ?D)

(require 'org-clock) ;; forcibly load org-clock

(defun my/org-agenda-effort-percentage ()
  "Return percentage of effort completed for the current Org item."
  (let* ((effort-str (org-entry-get nil "EFFORT" t))
         (effort (and effort-str (org-duration-to-minutes effort-str)))
         (clocked (org-clock-sum-current-item))
         (pct (and effort (> effort 0) (min 100 (/ (* 100 clocked) effort)))))
    (if pct
        (format "%d%%" pct)
      "--%")))

(defun my/org-agenda-skip-if-has-tag (tag)
  "Skip current entry if it has TAG."
  (let ((tags (org-get-tags)))
    (when (member tag tags)
      (or (outline-next-heading) (point-max)))))


(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
        ("g" "Currently Playing Games"
         todo "PLAYING"
         ((org-agenda-overriding-header "In Progress Games")
          (org-agenda-sorting-strategy '(priority-down effort-up))))
        ("h" "Daily Rituals"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-start-day "+0d")
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-show-log t)
                   (org-agenda-include-diary nil)
                   (org-habit-show-all-today t)
                   (org-agenda-overriding-header " Schedule +  Habits")))
          (tags "+books+TODO=\"STRT\""
                ((org-agenda-overriding-header " Currently Reading Books")
                 (org-agenda-prefix-format "   %?-13t %s")))
          (todo "STRT"
                ((org-agenda-overriding-header "󰦖 Currently doing")
                 (org-agenda-prefix-format "  %?-13t %s")
                 (org-agenda-skip-function '(my/org-agenda-skip-if-has-tag "books"))))
          (todo "PLAYING"
                ((org-agenda-overriding-header "󰮂 Currently Playing Games")
                 (org-agenda-prefix-format '((todo . "%?-13t %s [%e] (%(my/org-agenda-effort-percentage)) ")))
                 (org-agenda-sorting-strategy (quote (priority-down effort-up)))))))

        ("u" "Unsorted TODOs"
         ;; Unsorted TODO items
         ((todo ""))
         ((org-agenda-category-filter-preset
           (list "+planner" "+linguistics" "+vault")
           (org-agenda-overriding-header)
           "* Stray  TODOs:"
           (org-agenda-skip-function)
           '(org-agenda-skip-entry-if 'deadline 'scheduled)
           (org-default-priority org-lowest-priority)
           (org-agenda-sorting-strategy '(priority-down alpha-up)))))))

(after! org
  (setq org-crypt-disable-auto-save t)

  (setq org-agenda-span 14
        org-agenda-start-day nil
        org-agenda-start-on-weekday 0)
  (setq org-modern-table nil
        org-modern-todo nil
        org-modern-tag nil
        org-modern-progress nil
        org-modern-fold-stars '(("✦" . "✧")   ; sparkle closed, sparkle open
                                ("✶" . "✷")   ; twinkly star variants
                                ("♥" . "♡")   ; hearts
                                ("★" . "☆")   ; five-ended star
                                ("♦" . "♢"))))  ; diamond suit

(setq org-scheduled-past-days 0) ;; agenda not show missed SCHEDULED items

;; todo keywords
(setq org-todo-keywords
      '((sequence
         "TODO(t)"  ; A task that needs doing & is ready to do
         "STRT(s!)"  ; A task that is in progress
         "WAIT(w@/!)"  ; Something external is holding up this task
         "IDEA(i)"  ; An unconfirmed and unapproved task or notion
         "PROJ(p)"  ; Project
         "|"
         "DONE(d!)"  ; Task successfully completed
         "KILL(k@!)") ; Task was cancelled, aborted, or is no longer applicable
        (sequence
         "[ ](T)"   ; A task that needs doing
         "[-](S)"   ; Task is in progress
         "[?](W)"   ; Task is being held up or paused
         "|"
         "[X](D)"))  ; Task was completed

      org-todo-keyword-faces
      '(("[-]"  . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("[?]"  . +org-todo-onhold)
        ("WAIT" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)
        ("NO"   . +org-todo-cancel)
        ("KILL" . +org-todo-cancel))
      org-todo-keywords-for-agenda
      '((sequence
         "TODO(t)"  ; A task that needs doing & is ready to do
         "STRT(s!)"  ; A task that is in progress
         "WAIT(w@/!)"  ; Something external is holding up this task
         "IDEA(i)"  ; An unconfirmed and unapproved task or notion
         "PROJ(p)"  ; Project
         "|"
         "DONE(d!)"  ; Task successfully completed
         "KILL(k@/!)") ; Task was cancelled, aborted, or is no longer applicable
        (sequence
         "[ ](T)"   ; A task that needs doing
         "[-](S)"   ; Task is in progress
         "[?](W)"   ; Task is being held up or paused
         "|"
         "[X](D)")))
