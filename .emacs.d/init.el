;; Load facebook .emacs.
(defconst master-dir "~/.emacs.d")
(load-library (expand-file-name "master.emacs" master-dir))

;; -----------------------------------------------------------------------------
;; Customize stuff
;; -----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-program-name "aspell"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Inconsolata")))))

;; -----------------------------------------------------------------------------
;; Global settings
;; -----------------------------------------------------------------------------
(require 'cl)
(require 'dired-x)
(require 'ido)
(require 'windmove)

(ido-mode t)
(setq ido-ignore-files '("\\.orig"))
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq visible-bell t)
(global-font-lock-mode 1)
(column-number-mode t)
(setq grep-command "grep -i -nH -R -e")
(setq confirm-kill-emacs 'yes-or-no-p)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)

;; turn off version control
;;
;; vc-git hangs when I visit files on sshfs volumes anyway...
(setq vc-handled-backends nil)

;; (setq transient-mark-mode nil)
(show-paren-mode)
(windmove-default-keybindings 'super)
(put 'narrow-to-region 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fix our PATH to include some extra directories.
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/jcarreiro/bin"))
(setq exec-path (append exec-path '("/usr/local/bin" "/Users/jcarreiro/bin")))

;; Hacky work-around for sshfs lock files. See ls-for-dired.sh.
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "ls-for-dired.sh")

;; This didn't work:
;; (setq ls-lisp-use-insert-directory-program t)
;; (setq insert-directory-program "/usr/local/bin/gls")
;; (setq dired-listing-switches "-aBhl  --ignore='.#*' --group-directories-first")

;; -----------------------------------------------------------------------------
;; Package mode
;; -----------------------------------------------------------------------------
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize) ;; after this point we can use ELPA packages...

;; -----------------------------------------------------------------------------
;; Packages
;; -----------------------------------------------------------------------------
(require 'color-theme)
(require 'fill-column-indicator)
(color-theme-solarized-dark)

;; autocomplete
(require 'auto-complete)

;; fb-specific config -- see master.emacs.
(require 'ac-hh-complete-config)
(add-to-list 'ac-modes 'xhp-mode)
(auto-complete-mode t)
(eldoc-mode t)
(ac-hh-complete-mode-setup)

;; orgmode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
; (setq org-agenda-files '("~/todo.org" "~/journal/"))
; (setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; mediawiki
(require 'mediawiki)

;; -----------------------------------------------------------------------------
;; Functions
;; -----------------------------------------------------------------------------
(defun clip-file ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      (message "%s copied" filename)
      (x-select-text filename))))

(defun clip-file-as-kill ()
  "Put the current file path on the kill ring"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      (message "%s copied" filename)
      (kill-new filename))))

(defun number-lines-region (start end &optional beg)
  (interactive "*r\np")
  (let* ((lines (count-lines start end))
         (from (or beg 1))
         (to (+ lines (1- from)))
         (numbers (number-sequence from to))
         (width (max (length (int-to-string lines))
                     (length (int-to-string from)))))
    (if (= start (point))
        (setq numbers (reverse numbers)))
    (goto-char start)
    (dolist (n numbers)
      (beginning-of-line)
      (save-match-data
        (if (looking-at " *-?[0-9]+\\. ")
            (replace-match "")))
      (insert (format (concat "%" (int-to-string width) "d. ") n))
      (forward-line))))

(defun join-lines-region (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))

(defun unfill-paragraph ()
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive)
      (let ((fill-column (point-max)))
        (fill-paragraph nil)))

(define-key global-map "\M-Q" 'unfill-paragraph)

(defun unfill-region (beg end)
      "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
      (interactive "*r")
      (let ((fill-column (point-max)))
        (fill-region beg end)))

;; -----------------------------------------------------------------------------
;; Extra facebook stuff!
;;
;; Set up tramp mode to recognize the devserver password prompt, and make it
;; possible to open a file in a local emacs using emacsclient on the devserver.
;; -----------------------------------------------------------------------------
(setq tramp-default-method "sshx")
(setq tramp-password-prompt-regexp
      (concat
       "^.*"
       (regexp-opt
	'("passcode" "Passcode"
	  "password" "Password") t) ".*:\0? *"))


;; Make fbgs work by running it on my devserver.
;;
;; XXX It almost works in TRAMP buffers already:
;;
;;       fbgs --smartdir -- ASSOC_OP_DELETE
;;       /bin/sh: fbgs: command not found
;;
;;     I tried linking .profile -> .bash_profile, thinking that that
;;     would fix the path issue, but it didn't work... :(
;;
;; XXX This didn't work either -- it runs ssh all right, but there doesn't seem
;;     to be any way to do the 2fa?
;;
;; (setq xbgX-command-prefix "ssh dev384 /home/engshare/admin/scripts/")
;; (setq xbgX-exec-dir "~")
;; (setq xbgX-common-args "--forcedir '/ssh:jcarreiro@dev84:www'")

;; (setq server-use-tcp 1)
;; (setq server-host "dev309.prn2.facebook.com")

;; Don't try to use vc when we're accessing files via tramp -- this causes emacs
;; to hang for some reason...
(defadvice vc-svn-registered (around my-vc-svn-registered-tramp activate)
  "Don't try to use SVN on files accessed via TRAMP."
  (if (and (fboundp 'tramp-tramp-file-p)
	   (tramp-tramp-file-p (ad-get-arg 0)))
      nil
    ad-do-it))

(defadvice dvc-current-active-dvc (around dvc-current-active-dvc-no-tramp activate)
  "Don't try to use DVC on files accessed via TRAMP."
  (if (and (fboundp 'tramp-tramp-file-p)
	   (tramp-tramp-file-p (dvc-uniquify-file-name default-directory)))
      nil
    ad-do-it))

;; (let (large-file-warning-threshold 'nil) (visit-tags-table "~/devserver/www/TAGS"))

;; xhp-mode
;;
;; Highlight unary negation so that it stands out
(font-lock-add-keywords 'xhp-mode '(("\\(!\\)[^=]" 1 font-lock-negation-char-face)))

;; mode hook
(defun my-xhp-mode-hook ()
  (setq fill-column 80)
  (auto-complete-mode t)
  (fci-mode)
  (c-toggle-auto-newline 1)
  (c-toggle-hungry-state 1)
;;  (linum-mode 1)
)

(add-hook 'xhp-mode-hook 'my-xhp-mode-hook)

;; cc-mode
(defun my-c-mode-common-hook ()
  (c-set-style "fb-c-style")
  (c-toggle-hungry-state t)
  (c-toggle-auto-newline t)
  (setq fill-column 80)
  (fci-mode t)
  (auto-complete-mode t)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(put 'scroll-left 'disabled nil)

;; js-mode
;; (defun my-js-mode-common-hook ()
;;   (setq js-indent-level 4)
;; )
;;
;; (add-hook 'js-mode-hook 'my-js-mode-common-hook)

;; js2-mode
(add-hook 'js2-mode-hook (lambda () (hungry-delete-mode) (fci-mode)))
