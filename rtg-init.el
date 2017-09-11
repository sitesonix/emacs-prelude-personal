;;; package --- Summary

;;; Commentary: Personalized init file for Emacs Prelude.

;; Copyright (c) 2017 Ryan Griffith
;;
;; Author: Ryan Griffith <ryan@sitesonix.net>
;; URL: https://sitesonix.net/
;; Version: 1.2
;; Keywords: org gtd web

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

;; 01. Introduction
;; 02. Prelude Require Packages
;; 03. Basic Editor Preferences
;; 04. Terminals and Shells
;; 05. Theme Preferences
;; 06. Dired and Ibuffer Tweaks
;; 07. Mode Activation/Keybindings
;; 08. Org Mode Configuration
;; 09. Web Development
;; 10. Miscellaneous Stuff
;; 11. Close Init

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
                            ivy
                            js2-mode
                            js2-refactor
                            lorem-ipsum
                            multi-term
                            multiple-cursors
                            olivetti
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

(setq full-name "Ryan Griffith")
(setq user-mail-address "ryan@sitesonix.net")

(setq initial-scratch-message
      ";; The GNU GPL was not designed to be open source. - RMS\n\n")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(set-face-attribute 'default nil :height 100)

(blink-cursor-mode t)

(defun rtg/flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))
(setq-default
 ring-bell-function 'rtg/flash-mode-line)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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

(setq tab-always-indent 'complete)

(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

(setq uniquify-buffer-name-style 'reverse)
(setq inhibit-default-init t)
(setq-default frame-title-format "%b (%f)")
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell nil)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(add-hook 'before-save-hook 'whitespace-cleanup)

(fset 'yes-or-no-p 'y-or-n-p)

(setq require-final-newline t)

(setq multi-term-program "/bin/bash")

(when (require 'multi-term nil t)
  (global-set-key (kbd "C-x t") 'multi-term)
  (global-set-key (kbd "<C-next>") 'multi-term-next)
  (global-set-key (kbd "<C-prior>") 'multi-term-prev)
  (setq multi-term-buffer-name "mterm"
        multi-term-program "/bin/bash"))

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

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@gnutop:" buffer-file-name))))

(disable-theme 'zenburn)

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

(setq dired-listing-switches "-alh")

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

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

(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

(ivy-mode 1)

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(setq prelude-guru nil)

(winner-mode 1)

(which-key-mode)

(global-undo-tree-mode)

(global-set-key (kbd "C-c SPC") 'project-explorer-toggle)
;; NOTE: when outside of a project I like to use the built-in M-x speedbar

(global-set-key (kbd "C-c z") 'ztree-diff)
(global-set-key (kbd "C-c Z") 'ztree-dir)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq org-agenda-files
      (list "~/org/gtd.org"
            "~/org/work.org"
            "~/org/personal.org"))

(defun gtd ()
  (interactive)
  (find-file "~/org/gtd.org")
  )

(add-hook 'text-mode-hook
          (lambda ()
            (when (y-or-n-p "Auto Fill mode? ")
              (turn-on-auto-fill))))
;; and set the keybinding
(global-set-key (kbd "C-c q") 'auto-fill-mode)
;; otherwise set visual-line-mode or olivetti for distraction-free writing

(add-to-list 'load-path "~/emacs/org")
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-custom-commands
      '(("w" todo "WAITING" nil)
        ("n" todo "NEXT" nil)
        ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT"))))
      )

(defun rtg/org-capture-todo ()
  (interactive)
  "Capture a TODO item"
  (org-capture nil "t"))
;; bind
(define-key global-map (kbd "C-7") 'rtg/org-capture-todo)

;; Org-projectile for per-repo TODO files -- package is broken
;; (require 'org-projectile)
;; (org-projectile:per-repo)
;; (setq org-projectile:per-repo-filename "project.org")
;; (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
;; (global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-c n p") 'org-projectile:project-todo-completing-read)

(setq org-default-notes-file "~/org/personal.org")
;; Org-capture keybinding
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 5))))

(setq org-return-follows-link t)

(setq org-modules '(org-bbdb
                    org-gnus))
(eval-after-load 'org
  '(org-load-modules-maybe t))

(setq org-export-backends '(org latex html ascii))

(defun rtg/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'rtg/org-agenda-done)

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

(defun rtg/org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))
;; New key assignment
(define-key org-agenda-mode-map "N" 'rtg/org-agenda-new)

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

(global-set-key (kbd "C-x \\") #'align-regexp)

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(eval-after-load 'company-etags
  '(progn
     (add-to-list 'company-etags-modes 'web-mode)))

(eval-after-load 'web-mode '(define-key web-mode-map (kbd "s-d") 'php-jump))

(add-to-list 'auto-mode-alist '("\\.js" . js-mode))

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))

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

(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist
            '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

(setq web-mode-enable-auto-pairing nil)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start 3)
(add-to-list 'ac-modes 'web-mode)
(setq ac-ignore-case t)
(setq ac-auto-start nil)
(global-set-key (kbd "C-<tab>") 'auto-complete)

(add-to-list 'load-path "~/.emacs.d/impatient-mode")
(require 'impatient-mode)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'web-mode-hook 'skewer-css-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

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

(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

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

(defadvice bbdb-read-new-record (after wicked activate)
  "Prompt for the birthdate as well."
  (bbdb-record-putprop ad-return-value 'birthdate
                       (bbdb-read-string "Birthdate (YYYY.MM.DD): ")))

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

(defun eww-new ()
  (interactive)
  (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
    (switch-to-buffer (generate-new-buffer "eww"))
    (eww-mode)
    (eww url)))

;; End init file...
(provide 'rtg-init)
;;; rtg-init.el ends here
