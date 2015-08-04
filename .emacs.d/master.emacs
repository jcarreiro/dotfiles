;; -*- emacs-lisp -*-

;; please uncomment these two lines to your personal ~/.emacs file:
;;(defvar master-dir (getenv "ADMIN_SCRIPTS"))
;;(load-library (concat master-dir "/master.emacs"))
;; this keeps you up-to-date with the latest master.emacs changes; if you opt
;; not to, you'll be on your own for keeping up with general changes

;; Find emacs-packages
(add-to-list 'load-path (concat master-dir "/emacs-packages"))
(eval-after-load "cc-mode"
  '(require 'fb-coding-style))

;; Have highlighting all the time
(global-font-lock-mode 1)

;; use spaces, not tabs for indenting
(setq-default indent-tabs-mode nil)

;; git (try `git-blame-mode' and `(global-set-key "\C-xxb" 'git-blame-mode)')
(require 'git)
(setq git-blame-log-message-format "format:%an (%ar): %s (%h) ")
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

;; hg integration is buggy with our mercurial extensions, so disable it
(delq 'Hg vc-handled-backends)

;; PHP mode for .phpt files
(autoload 'php-mode "php-mode" nil t nil)

(autoload 'xhp-mode "xhp-mode"
  "Major mode for editing PHP code including XHP support." t)
(setq auto-mode-alist (append '(("\\.phpt?$" . xhp-mode))
                              auto-mode-alist))

;; Set PHP mode based on the #! line
(add-to-list 'interpreter-mode-alist '("php" . xhp-mode))

;; On-the-fly compilation js2-mode for editing .js files
(autoload 'js2-mode "js2-mode" nil t nil)
(setq auto-mode-alist (append '(("\\.js$" . js2-mode))
                              auto-mode-alist))
(require 'highlight-80+)
(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-basic-offset 2)
            (highlight-80+-mode t)
            ))

;; Thrift mode for .thrift files
(autoload 'thrift-mode "thrift" nil t nil)
(setq auto-mode-alist (append '(("\\.thrift$" . thrift-mode))
                              auto-mode-alist))

(if (locate-library "python")
    (require 'python)) ;; python mode available in emacs >= 22

(add-hook 'python-mode-hook
          (lambda ()
            (highlight-80+-mode t)
            ))

;; python mode for SConscript and SConstruct files
(setq auto-mode-alist (cons '("\\SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\SConscript" . python-mode) auto-mode-alist))

;; python mode for TARGETS files
(setq auto-mode-alist (cons '("\\/TARGETS$" . python-mode) auto-mode-alist))

;; python mode for .cconf, .cinc, and .ctest files, i.e. configerator files
(setq auto-mode-alist (cons '("\\.cconf" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cinc" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ctest" . python-mode) auto-mode-alist))
;; Same for tupperware .tw files.
(setq auto-mode-alist (cons '("\\.tw" . python-mode) auto-mode-alist))

;; d mode
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

;; sps mode
(autoload 'sps-mode "sps-mode" "Mode for Speakeasy code." t)
(add-to-list 'auto-mode-alist '("\\.sps\\'" . sps-mode))

;; show trailing whitespace ...
(set-face-background 'trailing-whitespace "#900000")
(setq-default show-trailing-whitespace t)
;; ... and terminate with extreme prejudice
(defun delete-trailing-whitespace-sometimes () ""
  (if (not (eq major-mode 'diff-mode))
      (delete-trailing-whitespace)))
(add-hook 'write-file-hooks 'delete-trailing-whitespace-sometimes)

;; don't show trailing whitespaces on terminals!
(add-hook 'term-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

(autoload 'graphql-mode "graphql" "GraphQL DSL mode")

;;=============================================================
;; SUBVERSION COMMIT HOOK
;;=============================================================
;; type "M-u" to insert svn commit template
;; (global-set-key "\M-u"
;;                 '(lambda ()
;;                    "Inserting Subversion template"
;;                    (interactive)
;;                    (insert-file-contents  (concat master-dir "/templates/svn-commit-template.txt"))
;;                    ))

;; Automatic insertion of template when entering subversion commit mode
(add-hook 'svncommit-mode-hook
          (function (lambda ()
                      "Inserting Subversion template"
                      (interactive)
                      (insert-file-contents  (concat master-dir "/templates/svn-commit-template.txt"))
                      )))


(define-derived-mode svncommit-mode
  text-mode
  "Subversion commit"
  "So you think you can commit code huh!")

(setq auto-mode-alist (append '(("svn-commit" . svncommit-mode))
                              auto-mode-alist))


;;=========================================================
;;PHP Doc Function template
;;========================================================
(defun php-doc ()
    "Inserts a phpdoc function template"
    (interactive)
    (insert-file-contents (concat master-dir "/templates/php-doc-template.txt")))


;;=========================================================
;;C++ Function documentation template
;;========================================================
(defun c++-doc ()
    "Inserts a C++ doc function template"
    (interactive)
    (insert-file-contents (concat master-dir "/templates/c++-doc-template.txt")))

;;=========================================================
;;Add a "--read-only" command line option
;;========================================================
(setq command-switch-alist
      (cons '("--read-only" . find-file-read-only-command-line-arg) ; "-R"
            command-switch-alist))

(defun find-file-read-only-command-line-arg (switch)
  "Visit next command line argument (after SWITCH) as a read-only file."
  ;; (prog1 (car x) (setq x (cdr x))) == (pop x):
  (find-file-read-only (prog1 (car command-line-args-left)
                         (setq command-line-args-left
                               (cdr command-line-args-left)))))

(defcustom hh-complete-enabled 'nil
  "How hh-complete mode is enabled.
Can be `auto-complete' -- use the fancy auto-complete mode with popup menu etc.
       `simple' -- use a basic ido-complete handler to complete
       `nil' -- disable this"
  :type '(choice (const auto-complete) (const simple) (const nil))
  :group 'hh-complete)

;; make this loadable in general
(add-to-list 'load-path (concat master-dir "/emacs-packages/third-party/auto-complete"))

(defun hh-complete-hook ()
  "Enables hh-complete mode according to the `hh-complete-enabled' variable"
  ;; if you have global auto complete mode turned on it will act badly
  ;; unless you do this
  (case hh-complete-enabled
    ;; fancy auto-complete setup
    ('auto-complete
     (require 'auto-complete)
     (require 'ac-hh-complete-config)
     (add-to-list 'ac-modes 'xhp-mode)
     (auto-complete-mode t)
     (eldoc-mode t)
     (ac-hh-complete-mode-setup))
    ;; vanilla completion
    ('simple
     (hh-complete-mode t))
    ))

(require 'highlight-80+)
(let
    ((mode-hook (lambda ()
            (c-set-style "fb-php-style")
            (highlight-80+-mode t)
            ; php-mode.el disables this, but that conflicts with arc lint
            (set (make-local-variable 'require-final-newline) t)
            ; Turn on pfff-flymake by default.  pfff-flymake-enabled is the
            ; killswitch, defining it and setting to nil forces pfff-flymake
            ; off.
            (when (or (not (boundp 'pfff-flymake-enabled))
                      pfff-flymake-enabled)
              (pfff-flymake-mode) ;; (pfff-flymake-mode t)
              )
            ; turn on autocomplete
            (hh-complete-hook)
            )))
  (add-hook 'php-mode-hook mode-hook)
  (add-hook 'xhp-mode-hook mode-hook))

;;=========================================================
;;PHP New File Template
;;========================================================
(require 'autoinsert)
(auto-insert-mode t)
(setq auto-insert-query nil)
(define-skeleton fb-php-skeleton
  "Facebook PHP skeleton"
  nil
  "<?hh\n"
  "// Copyright 2004-present Facebook. All Rights Reserved.\n"
  "\n")
(define-auto-insert 'php-mode 'fb-php-skeleton)
(define-auto-insert 'xhp-mode 'fb-php-skeleton)

;;=========================================================
;;C, C++, Objective-C, and Java Setup
;;========================================================

(require 'cc-mode)
(add-hook 'c-mode-common-hook
  (lambda ()
    (c-set-style "fb-c-style")
    (highlight-80+-mode t)))

(add-hook 'java-mode-hook
  (lambda ()
    (c-set-style "fb-java-style")
    (highlight-80+-mode t)))

;;=========================================================
;;Python Indentation Style
;;========================================================
(add-hook 'python-mode-hook
          (lambda ()
            (set-variable 'python-indent 4)
            (highlight-80+-mode t)
            ))

;; Automatically select the appropriate mode based on matching the
;; text at the beginning of the file.
(if (boundp 'magic-mode-alist)
    (setq magic-mode-alist
          (append (list
                   '("\\(.\\|\n\\)*\n@implementation" . objc-mode)
                   '("\\(.\\|\n\\)*\n@interface" . objc-mode)
                   '("\\(.\\|\n\\)*\n@protocol" . objc-mode)
                   '("\\(.\\|\n\\)*\nnamespace.*{" . c++-mode)
                   '("<\\?php\\s " . xhp-mode))
                  magic-mode-alist))
)


;; unique buffer names using path
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; tbgX commands to search the codebase
(autoload 'tbgr "tbgX" nil t nil)
(autoload 'tbgs "tbgX" nil t nil)
(autoload 'fbgr "tbgX" nil t nil)
(autoload 'fbgs "tbgX" nil t nil)
(autoload 'pbgr "tbgX" nil t nil)
(autoload 'pbgs "tbgX" nil t nil)
(autoload 'gg "tbgX" nil t nil)
(autoload 'sgrep "tbgX" nil t nil)

(autoload 'hphpd "hphpd" nil t nil)
(autoload 'pfff-flymake-mode "pfff-flymake" nil t nil)
(autoload 'hh-complete-mode "hh-complete" nil t nil)

(autoload 'pfff-prolog-find-tag "pfff_prolog" nil t nil)
(autoload 'pfff-prolog-find-callers "pfff_prolog" nil t nil)
(autoload 'pfff-prolog-find-classes-with-method "pfff_prolog" nil t nil)
(autoload 'pfff-prolog-find-extendors-of-class "pfff_prolog" nil t nil)
(autoload 'pfff-prolog-find-users-of-trait "pfff_prolog" nil t nil)
(autoload 'pfff-prolog-find-trait-that-implements-method "pfff_prolog" nil t nil)
(autoload 'pfff-prolog-shell "pfff_prolog" nil t nil)
(autoload 'phabricator-diffusion-url "phabricator" nil t nil)
