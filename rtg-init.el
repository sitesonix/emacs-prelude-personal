;;; package --- Summary

;;; Commentary: Personalized init file for Emacs Prelude.

;; Copyright (c) 2017 Ryan Griffith
;;
;; Author: Ryan Griffith <ryan@sitesonix.net>
;; URL: https://sitesonix.net/
;; Version: 2.5
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

;; 00. Introduction
;; 01. Prelude Require Packages
;; 02. Basic Editor Preferences
;; 03. Theme Preferences
;; 04. Mode Specific (General)
;; 05. Dired and Ibuffer Tweaks
;; 06. Terminals and Shells
;; 07. Org Mode Configuration
;; 08. Web Development
;; 09. Miscellaneous Stuff
;; 10. Close Init

(prelude-require-packages '(apache-mode
                            company-web
                            counsel
                            ctags
                            dash
                            dired+
                            emmet-mode
                            flymd
                            htmlize
                            impatient-mode
                            ivy
                            js2-mode
                            js2-refactor
                            multi-term
                            multiple-cursors
                            olivetti
                            org2blog
                            org-password-manager
                            php-mode
                            project-explorer
                            s
                            simple-httpd
                            skewer-mode
                            swiper
                            twittering-mode
                            web-beautify
                            ztree))

(setq full-name "Ryan Griffith")
(setq user-mail-address "ryan@sitesonix.net")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(set-face-attribute 'default nil :height 100)

(blink-cursor-mode t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq uniquify-buffer-name-style 'reverse)
(setq inhibit-default-init t)
(setq-default frame-title-format "%b (%f)")
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")

(setq display-time-day-and-date t
      display-time-12hr-format t)
(display-time)

(defun my-fortune-scratch-message ()
  (interactive)
  (let ((fortune
         (when (executable-find "fortune")
           (with-temp-buffer
             (shell-command "fortune" t)
             (let ((comment-start ";;"))
               (comment-region (point-min) (point-max)))
             (delete-trailing-whitespace (point-min) (point-max))
             (concat (buffer-string) "\n")))))
    (if (called-interactively-p 'any)
        (insert fortune)
      fortune)))

;; initial-scratch-message
(let ((fortune (my-fortune-scratch-message)))
  (when fortune
    (setq initial-scratch-message fortune)))

(defun rtg/flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))
(setq-default
 ring-bell-function 'rtg/flash-mode-line)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell nil)

(setq tab-always-indent 'complete)

(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Redefine the default Emacs suspend keybinding...
(global-unset-key (kbd "C-z")) ; windmove prefix
(global-set-key (kbd "C-z C-z") 'suspend-frame)
;; so that windmove plays nice with org-mode
(global-set-key (kbd "C-z <left>") 'windmove-left)
(global-set-key (kbd "C-z <down>") 'windmove-down)
(global-set-key (kbd "C-z <up>") 'windmove-up)
(global-set-key (kbd "C-z <right>") 'windmove-right)
;; where S <left/right/up/down> still works outside org

(setq require-final-newline t)

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

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(setq prelude-guru nil)

(winner-mode 1)

(which-key-mode)

(global-undo-tree-mode)

(global-set-key (kbd "C-c SPC") 'project-explorer-toggle)

(global-set-key (kbd "C-c z") 'ztree-diff)
(global-set-key (kbd "C-c Z") 'ztree-dir)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq dired-listing-switches "-alh")

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("web" (or
                       (mode . css-mode)
                       (mode . scss-mode)
                       (mode . js2-mode)
                       (mode . ruby-mode)
                       (mode . web-mode)))
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

(setq org-agenda-files
      (list "~/org/gtd.org"
            "~/org/work.org"
            "~/org/personal.org"))

(defun gtd ()
  (interactive)
  (find-file "~/org/gtd.org")
  )

(setq org-default-notes-file "~/org/personal.org")
;; Org-capture keybinding
(global-set-key (kbd "C-c c") 'org-capture)

(defun rtg/org-capture-todo ()
  (interactive)
  "Capture a TODO item"
  (org-capture nil "t"))
;; bind
(define-key global-map (kbd "C-7") 'rtg/org-capture-todo)

(setq org-return-follows-link t)

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

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 5))))

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

(setq org-modules '(org-bbdb
                    org-gnus))
(eval-after-load 'org
  '(org-load-modules-maybe t))

(setq org-export-backends '(org latex html ascii))

(global-set-key (kbd "C-x \\") #'align-regexp)

(eval-after-load 'dash '(dash-enable-font-lock))

(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(eval-after-load 'company-etags
  '(progn
     (add-to-list 'company-etags-modes 'web-mode)))

(add-hook 'php-mode-hook 'php-enable-wordpress-coding-style)

(add-hook 'js2-mode-hook #'js2-refactor-mode)

(setq js2-skip-preprocessor-directives t)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(add-to-list 'load-path "~/.emacs.d/impatient-mode")
(require 'impatient-mode)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'web-mode-hook 'skewer-css-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

(defun put-date ()
  (interactive)
  (insert (shell-command-to-string "date")))

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
