;;; package --- Summary
;;; Commentary: Personalized init file for Emacs Prelude -- required packages
;;; and editor prefs are found here.  I have two other init files: one for
;;; web dev (rtg-web.el) and another for org-mail-other (rtg-org.el).
;;; Having these separate keeps my configuration more organized.
;;; Code:

;; Require extra packages
(prelude-require-packages '(ac-emmet
                            ac-js2
                            apache-mode
                            auto-complete
                            autopair
                            company-web
                            ctags
                            dash
                            delight
                            dired+
                            elfeed
                            emmet-mode
                            epl
                            htmlize
                            impatient-mode
                            js2-mode
                            js2-refactor
                            lorem-ipsum
                            multiple-cursors
                            org2blog
                            paredit
                            project-explorer
                            simple-httpd
                            smart-mode-line
                            smart-mode-line-powerline-theme
                            skewer-mode
                            twittering-mode
                            undo-tree
                            web-beautify
                            which-key
                            workgroups2
                            yasnippet
                            ztree))

;; Set the default coding system to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Set user details
(setq user-full-name "Ryan Griffith")
(setq user-mail-address "ryan@sitesonix.net")

;; Customize the scratch buffer (for me)
(setq initial-scratch-message
    ";; The GNU GPL was not designed to be open source. - RMS\n\n")

;; Because I use Emacs built-in customize for themes (custom.el)
(disable-theme 'zenburn)

;; Enable smart-mode-line and set the theme
(setq sml/theme 'powerline)
(sml/setup)

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

;; Enable cursor blinking
(blink-cursor-mode t)

;; Disable the irritating bell
(setq ring-bell-function 'ignore)

;; The scroll bar isn't necessary
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Display date and time in the mode line
(setq display-time-day-and-date t
      display-time-12hr-format t)
(display-time)

;; Minibuffer auto-completion
(icomplete-mode 1)

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

;; Disable guru-mode because arrow keys are often useful
(setq prelude-guru nil)

;; Enable winner-mode
(winner-mode 1)

;; Move default tilde ~ backup files to a backups directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; yes is always y
(fset 'yes-or-no-p 'y-or-n-p)

;; Multiple cursors - keybindings
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Enable which-key mode
(which-key-mode)

;; Cleanup whitespace before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; dired: human readable sizes and sort by size
(setq dired-listing-switches "-alh")

;; Work better with files in different directories
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;; Intuitive undo/redo visualization with C-x u
(global-undo-tree-mode)

;; Prompt for password and then open files owned by root
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@gnutop:" buffer-file-name))))

;; Load yasnippets
(add-to-list 'load-path
             "~/.emacs.d/personal/snippets")
(require 'yasnippet)
(yas-global-mode 1)

;; Toggle project explorer
(global-set-key (kbd "C-c SPC") 'project-explorer-toggle)

;; ztree keybindings
(global-set-key (kbd "C-c z") 'ztree-diff)
(global-set-key (kbd "C-c Z") 'ztree-dir)

;; workgroups2 prefix key
(setq wg-prefix-key (kbd "C-c W"))

;; workgroups2: Emacs exit / workgroups-mode exit
(setq wg-emacs-exit-save-behavior           'ask)
(setq wg-workgroups-mode-exit-save-behavior 'ask)

;; End init file...
(provide 'rtg-init)
;;; rtg-init.el ends here
