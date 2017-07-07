;;; package --- Summary

;;; Commentary: Personalized init file for Emacs Prelude.

;; Copyright (c) 2017 Ryan Griffith
;;
;; Author: Ryan Griffith <ryan@sitesonix.net>
;; URL: https://sitesonix.net/
;; Version: 1.2
;; Keywords: dvorak web gtd

;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Sections:

;; 0. Prelude Require Packages
;; 1. Basic Editor Preferences
;; 2. Terminals and Shells
;; 3. Theme Preferences
;; 4. Dired and Ibuffer Tweaks
;; 5. Mode Activation/Keybindings
;; 6. Org Mode Configuration
;; 7. Web Development
;; 8. Miscellaneous Stuff

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 0. Prelude Require Packages @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-packages '(ac-emmet
                            ac-js2
                            apache-mode
                            auto-complete
                            autopair
                            bbdb
                            company-web
                            ctags
                            dired+
                            emmet-mode
                            gnome-calendar
                            gnus-desktop-notify
                            htmlize
                            impatient-mode
                            js2-mode
                            js2-refactor
                            lorem-ipsum
                            multi-term
                            multiple-cursors
                            org2blog
                            org-password-manager
                            org-projectile
                            paredit
                            project-explorer
                            simple-httpd
                            skewer-mode
                            twittering-mode
                            web-beautify
                            ztree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Basic Editor Preferences @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set user details
(setq full-name "Ryan Griffith")
(setq user-mail-address "ryan@sitesonix.net")

;; Customize the scratch buffer (for me)
(setq initial-scratch-message
      ";; The GNU GPL was not designed to be open source. - RMS\n\n")

;; Set the default coding system to UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Set the font size
(set-face-attribute 'default nil :height 100)

;; Enable cursor blinking
(blink-cursor-mode t)

;; Use modeline flash as a visible bell
(defun rtg/flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))
(setq-default
 ring-bell-function 'rtg/flash-mode-line)

;; The scroll bar isn't necessary
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Display date and time in the mode line
(setq display-time-day-and-date t
      display-time-12hr-format t)
(display-time)

;; Redefine the default Emacs suspend keybinding...
(global-unset-key (kbd "C-z")) ; windmove prefix
(global-set-key (kbd "C-z C-z") 'suspend-frame)
;; so that windmove plays nice with org-mode
(global-set-key (kbd "C-z <left>") 'windmove-left)
(global-set-key (kbd "C-z <down>") 'windmove-down)
(global-set-key (kbd "C-z <up>") 'windmove-up)
(global-set-key (kbd "C-z <right>") 'windmove-right)
;; where S <left/right/up/down> still works outside org

;; Enable ivy mode completion everywhere
(ivy-mode 1)

;; Basic ivy settings
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; minibuffer evaluations for paredit
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

;; Set frame title to show filename plus working directory
(setq uniquify-buffer-name-style 'reverse)
(setq inhibit-default-init t)
(setq-default frame-title-format "%b (%f)")
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")

;; Mods and tweaks in the echo area
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell nil)

;; Move default tilde ~ backup files to a backups directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Cleanup whitespace before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; yes is always y
(fset 'yes-or-no-p 'y-or-n-p)

;; Add a new line at the end of a file
(setq require-final-newline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Terminals and Shells @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup bash for use with multi-term
(setq multi-term-program "/bin/bash")

;; Setup multi-term for practical use
(when (require 'multi-term nil t)
  (global-set-key (kbd "C-x t") 'multi-term)
  (global-set-key (kbd "<C-next>") 'multi-term-next)
  (global-set-key (kbd "<C-prior>") 'multi-term-prev)
  (setq multi-term-buffer-name "mterm"
        multi-term-program "/bin/bash"))

;; Multi-term keybindings
(when (require 'term nil t) ; only if term can be loaded..
  (setq term-bind-key-alist
        (list (cons "C-c C-c" 'term-interrupt-subjob)
              (cons "C-p" 'previous-line)
              (cons "C-n" 'next-line)
              (cons "M-f" 'term-send-forward-word)
              (cons "M-b" 'term-send-backward-word)
              (cons "C-c C-j" 'term-line-mode)
              (cons "C-c C-k" 'term-char-mode)
              (cons "M-DEL" 'term-send-backward-kill-word)
              (cons "M-d" 'term-send-forward-kill-word)
              (cons "<C-left>" 'term-send-backward-word)
              (cons "<C-right>" 'term-send-forward-word)
              (cons "C-r" 'term-send-reverse-search-history)
              (cons "M-p" 'term-send-raw-meta)
              (cons "M-y" 'term-send-raw-meta)
              (cons "C-y" 'term-send-raw))))

;; Prompt for password and then open files owned by root
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@gnutop:" buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. Theme Preferences @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Because I use Emacs built-in customize for themes
;; (usually the sanityinc tomorrow night theme but not always...)
(disable-theme 'zenburn)

;; Enable transparency for frames and windows
(set-frame-parameter (selected-frame) 'alpha '(92 50))
(add-to-list 'default-frame-alist '(alpha 92 50))
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(92 50))))
(defun transparency (92)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
;; Toggle the transparency with F5
(global-set-key [f5] 'toggle-transparency)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. Dired and Ibuffer Tweaks @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dired: human readable sizes and sort by size
(setq dired-listing-switches "-alh")

;; Work better with files in different directories
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;; Ibuffer: Use Gnus-style grouping for list
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("www" (or
                       (mode . web-mode)
                       (mode . js-mode)
                       (mode . js2-mode)
                       (mode . css-mode)))
               ("org" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           (mode . org-mode)))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")))
               ("eww" (or
                       (mode . eww-mode)
                       (mode . eww-bookmark-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Ibuffer: use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Mode Specific (general) @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable guru-mode because arrow keys are sometimes useful
(setq prelude-guru nil)

;; Enable winner-mode
(winner-mode 1)

;; Enable which-key mode
(which-key-mode)

;; Enable undo-tree-mode visualization with C-x u
(global-undo-tree-mode)

;; Enable toggle for project explorer
(global-set-key (kbd "C-c SPC") 'project-explorer-toggle)
;; NOTE: when outside of a project I like to use the built-in M-x speedbar

;; Enable and set ztree keybindings
(global-set-key (kbd "C-c z") 'ztree-diff)
(global-set-key (kbd "C-c Z") 'ztree-dir)

;; Enable and set multiple cursors keybindings
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Org-mode Setup @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First ensure that auto fill mode is an always option for org other text docs
(add-hook 'text-mode-hook
          (lambda ()
            (when (y-or-n-p "Auto Fill mode? ")
              (turn-on-auto-fill))))
;; and set the keybinding
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Setup org mode agenda
(add-to-list 'load-path "~/emacs/org")
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Main files are found here. Add new project files to the list as needed
(setq org-agenda-files
      (list "~/org/gtd.org"
            "~/org/work.org"
            "~/org/personal.org"))

;; Interactive gtd file
(defun gtd ()
  (interactive)
  (find-file "~/org/gtd.org")
  )

;; Set return to activate a link
(setq org-return-follows-link t)

;; Custom org-agenda commands
(setq org-agenda-custom-commands
      '(("w" todo "WAITING" nil)
        ("n" todo "NEXT" nil)
        ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT"))))
      )

;; function to capture a todo
(defun rtg/org-capture-todo ()
  (interactive)
  "Capture a TODO item"
  (org-capture nil "t"))
;; bind
(define-key global-map (kbd "C-7") 'rtg/org-capture-todo)

;; Org-projectile for per-repo TODO files
(require 'org-projectile)
(org-projectile:per-repo)
(setq org-projectile:per-repo-filename "project.org")
(setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c n p") 'org-projectile:project-todo-completing-read)

;; Org-capture to personal.org file
(setq org-default-notes-file "~/org/personal.org")
;; Org-capture keybinding
(global-set-key (kbd "C-c c") 'org-capture)

;; Refile: show all headings from all agenda files
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 5))))

;; Extra org modules and export backends
(setq org-modules '(org-bbdb
                    org-gnus))
(eval-after-load 'org
  '(org-load-modules-maybe t))

;; Prepare stuff for org-export-backends
(setq org-export-backends '(org latex html ascii))

;; The following org-agenda hacks are borrowed from Sacha Chua's config
;; http://pages.sachachua.com/.emacs.d/Sacha.html#org6eefca2
;; What I was trying to do was already out there. :-)

;; Mark TODO as done by simply hitting 'x'
(defun rtg/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'rtg/org-agenda-done)

;; Mark TODO as done with 'X' and then create new task at same level
(defun rtg/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))
;; Override the key definition
(define-key org-agenda-mode-map "X" 'rtg/org-agenda-mark-done-and-add-followup)

;; Capture something based on the agenda
(defun rtg/org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))
;; New key assignment
(define-key org-agenda-mode-map "N" 'rtg/org-agenda-new)

;; Keep track of unscheduled tasks and stuck projects
(defun rtg/org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
         (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Web Development  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

;; Enable emmet for web mode and css mode
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; Get company-mode to work with web-mode
(eval-after-load 'company-etags
  '(progn
     (add-to-list 'company-etags-modes 'web-mode)))

(eval-after-load 'web-mode '(define-key web-mode-map (kbd "s-d") 'php-jump))

;; Set default mode for Javascript files
(add-to-list 'auto-mode-alist '("\\.js" . js-mode))

;; Set default mode for JSON files
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; Add hooks for JS2 mode and auto-complete
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; yasnippet should be loaded before auto complete so that they can work together
;; Load yasnippets
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; yasnippet expansion and completion with dropdown
(setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))

;; Auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start 3)
(add-to-list 'ac-modes 'web-mode)
(setq ac-ignore-case t)
(setq ac-auto-start nil)
(global-set-key (kbd "C-<tab>") 'auto-complete)

;;; auto complete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; Require web-mode for mixed templates with HTML, CSS and PHP
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist
            '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

;; make web-mode play nice with smartparens
(setq web-mode-enable-auto-pairing nil)

;; Require auto-complete for web-mode
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start 3)
(add-to-list 'ac-modes 'web-mode)
(setq ac-ignore-case t)
(setq ac-auto-start nil)
(global-set-key (kbd "C-<tab>") 'auto-complete)

;; Require impatient mode to start after invoking simple httpd mode
(add-to-list 'load-path "~/.emacs.d/impatient-mode")
(require 'impatient-mode)

;; Add hooks for skewer mode
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'web-mode-hook 'skewer-css-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

;; NOTE: Prelude also has good programming language defaults via modules.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. Miscellaneous Stuff @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; Delighted: no clutter - C-h m instead for active minor modes
(delight '((auto-complete-mode nil "auto-complete")
           (beacon-mode nil "beacon")
           (company-mode nil "company")
           (emmet-mode nil "emmet-mode")
           (flycheck-mode nil "flycheck")
           (flyspell-mode nil "flyspell")
           (prelude-mode nil "prelude-mode")
           (projectile-mode nil "projectile")
           (smartparens-mode nil "smartparens")
           (skewer-mode nil "skewer-mode")
           (skewer-css-mode nil "skewer-css")
           (skewer-html-mode nil "skewer-html")
           (which-key-mode nil "which-key")
           (whitespace-mode nil "whitespace")
           (yas-minor-mode nil "yasnippet")
           (emacs-lisp-mode "EL" :major)))

;; BBDB: Allow for adding birthdays to records
(defadvice bbdb-read-new-record (after wicked activate)
  "Prompt for the birthdate as well."
  (bbdb-record-putprop ad-return-value 'birthdate
                       (bbdb-read-string "Birthdate (YYYY.MM.DD): ")))

;; Browsing choices: EWW or Firefox
(defalias 'gk-urls-external-browser 'browse-url-xdg-open)
(defun gk-browse-url (&rest args)
  "Prompt for whether or not to browse with EWW, if no browse
with external browser."
  (apply
   (if (y-or-n-p "Browse with EWW? ")
       'eww-browse-url
     'gk-urls-external-browser)
   args))
(setq browse-url-browser-function #'gk-browse-url)

;; End init file...
(provide 'rtg-init)
;;; rtg-init.el ends here
