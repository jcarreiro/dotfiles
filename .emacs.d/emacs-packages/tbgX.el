;; $Id$
;;; Copyright (C) 2009 Facebook Inc.
;;; Author: Greg J. Badros <badros@facebook.com
;;;
;;; Simple interactive commands for [fpt][gr] e.g., tbgr, tbgs, fbgr,
;;; etc.  Modeled after grep major mode, which itself uses compilation
;;; minor mode.
;;;
;;; Tested using GNU Emacs 22.3.2 on Linux.
;;;
;;; For customization, see the variables in grep.el (or
;;; describe-variable on grep-*)
;;;
;;; For keybindings, see compile.el.
;;;
;;; TODO(badros): consider specializations of those grep customization
;;; variables for just this mode.
;;;
;;; TODO(badros): factor the functions out better by defining a macro
;;; that can define each of the six functions (eliminate cut & paste).

(eval-when-compile
  (when (fboundp #'byte-compile-disable-warning)
    (byte-compile-disable-warning 'cl-functions)
    (byte-compile-disable-warning 'obsolete)
    (byte-compile-disable-warning 'interactive-only)))

(require 'grep)
(require 'compile)
(require 'cl)
(require 'vc-git)

(defgroup facebook-grep-utils nil
  "Various options that affect the tbgX, fbgX and pbgX commands."
  :group 'tools
  :group 'processes)

(defcustom tbgX-compilation-search-path (list (expand-file-name "~"))
  "Lists of search paths to try prepending to the output of a tbgX command like \\[tbgs]."
  :type '(repeat directory)
  :group 'facebook-grep-utils)
(defcustom fbgX-compilation-search-path (list (expand-file-name "~"))
  "Lists of search paths to try prepending to the output of a fbgX command like \\[fbgs]."
  :type '(repeat directory)
  :group 'facebook-grep-utils)
(defcustom pbgX-compilation-search-path (list (expand-file-name "~"))
  "Lists of search paths to try prepending to the output of a pbgX command like \\[pbgs]."
  :type '(repeat directory)
  :group 'facebook-grep-utils)
(defcustom xbgX-common-args "--smartdir"
  "arguement string to pass to tbgX, fbgX or pbgX commands"
  :type 'string
  :group 'facebook-grep-utils)
(defcustom xbgX-command-prefix ""
  "prefix to xbgX commands, e.g. 'ssh my-devserver '"
  :type 'string
  :group 'facebook-grep-utils)
(defcustom xbgX-exec-dir nil
  "Execution dir for xbgX, overrides default buffer directory"
  :type 'string
  :group 'facebook-grep-utils)

;; For testing:
;; (setq compilation-search-path tbgX-compilation-search-path)
;; (setq compilation-search-path nil)

(defun tbgX-do-grep (args)
  "Run grep with args, doing quoting"
  (let ((default-directory (or xbgX-exec-dir default-directory)))
    (grep (concat grep-command " -- " (shell-quote-argument args)))))

;; inspired by igrep.el's igrep-read-regex
(defun tbgX-read-string (prompt)
  "Read and return a string from the minibuffer.
PROMPT is used as the prompt."
  (let* ((symbol (symbol-at-point))
         (default-value (if symbol (symbol-name symbol) nil)))
    (list
     (read-string (concat prompt ": ") default-value 'tbgX-history))))

;;;###autoload
(defun tbgr (regex)
  "Run tbgr, like grep, but using tbgX-compilation-search-path.
tbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches tfb-trunk in SVN for a regular expression."
  (interactive (tbgX-read-string "stbgr (regexp)"))
  (message "Using %s" regex)
  (let ((grep-use-null-device nil)
        (grep-command (concat "tbgr " xbgX-common-args)))
    (tbgX-do-grep regex)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) tbgX-compilation-search-path))))

;;;###autoload
(defun tbgs (string)
  "Run tbgs, like grep, but using tbgX-compilation-search-path.
tbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches tfb-trunk in SVN for a string."
  (interactive (tbgX-read-string "stbgs (string)"))
  (let ((grep-use-null-device nil)
        (grep-command (concat xbgX-command-prefix "tbgs " xbgX-common-args)))
    (tbgX-do-grep string)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) tbgX-compilation-search-path))))

;;;###autoload
(defun fbgr (regex)
  "Run fbgr, like grep, but using fbgX-compilation-search-path.
fbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches fbcode-trunk in SVN for a regular expression."
  (interactive (tbgX-read-string "sfbgr (regexp)"))
  (let ((grep-use-null-device nil)
        (grep-command (concat xbgX-command-prefix "fbgr " xbgX-common-args)))
    (tbgX-do-grep regex)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) fbgX-compilation-search-path))))

;;;###autoload
(defun fbgs (string)
  "Run fbgs, like grep, but using fbgX-compilation-search-path.
fbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches fbcode-trunk in SVN for a string."
  (interactive (tbgX-read-string "sfbgs (string)"))
  (let ((grep-use-null-device nil)
        (grep-command (concat xbgX-command-prefix "fbgs " xbgX-common-args)))
    (tbgX-do-grep string)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) fbgX-compilation-search-path))))

;;;###autoload
(defun pbgr (regex)
  "Run pbgr, like grep, but using pbgX-compilation-search-path.
pbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches projects in SVN for a regular expression."
  (interactive (tbgX-read-string "spbgr (regexp)"))
  (let ((grep-use-null-device nil)
        (grep-command (concat xbgX-command-prefix "pbgs " xbgX-common-args)))
    (tbgX-do-grep regex)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) pbgX-compilation-search-path))))

;;;###autoload
(defun pbgs (string)
  "Run pbgs, like grep, but using pbgX-compilation-search-path.
pbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches projects in SVN for a string."
  (interactive (tbgX-read-string "spbgs (string)"))
  (let ((grep-use-null-device nil)
        (grep-command (concat xbgX-command-prefix "pbgs " xbgX-common-args)))
    (tbgX-do-grep string)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) pbgX-compilation-search-path))))

(defcustom gg-args ""
  "argument string to pass to git grep"
  :type 'string
  :group 'facebook-grep-utils)

;;;###autoload
(defun gg (regex)
  "run git grep interactively"
  (interactive (tbgX-read-string "regex"))
  (vc-git-grep regex "*" (fb-find-repo-root (buffer-file-name))))

(defun run-git-grep (cmd regex)
  (let ((grep-use-null-device nil)
        (grep-command cmd)
        (root-dir (fb-find-repo-root (buffer-file-name))))
    (let ((default-directory root-dir))
      (tbgX-do-grep regex))
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) (list root-dir)))))

(defcustom sgrep-path "sgrep_php"
  "path to sgrep"
  :type 'string
  :group 'facebook-grep-utils)

(defun fb-argmax (ffun lst)
  (if (not lst) nil
    (reduce (lambda (lhs rhs)
              (let ((lhsval (funcall ffun lhs))
                    (rhsval (funcall ffun rhs)))
                (if (> lhsval rhsval) lhs rhs)))
            (cdr lst)
            :initial-value (car lst))))

;;;###autoload
(defun sgrep (pattern)
  "run sgrep interactively"
  (interactive (tbgX-read-string "pattern"))
  ;; heuristic to figure out what the inteded filter string is.  Take
  ;; the longest identifier-like thing
  (let ((prefilter (fb-argmax 'length (split-string pattern "[^a-zA-Z_0-9]+"))))
    (run-git-grep
     (concat "find . \\( -name \".git\" -o -name \".hg\" \\)  -prune -o -name \"*.php\" -print0 | "
             "xargs --null grep -l " (shell-quote-argument prefilter) " | "
             "xargs " sgrep-path " -emacs -e ")
     pattern)))


(defun fb-parent-directory (filename)
  (file-name-directory (directory-file-name filename)))

(defun fb-get-repo-type (dir)
  (cond
   ((file-directory-p (concat (file-name-as-directory dir) ".hg")) "hg")
   ((file-directory-p (concat (file-name-as-directory dir) ".git")) "git")
   ((file-directory-p (concat (file-name-as-directory dir) ".svn")) "svn")
   (t nil)))

(defun fb-find-repo-root-and-type (filename-or-dir)
  (let ((type nil))
    (cond
     ((equal filename-or-dir "/") nil)
     ((setq type (fb-get-repo-type filename-or-dir))
      (cons (file-truename filename-or-dir) type))
     (t (fb-find-repo-root-and-type (fb-parent-directory filename-or-dir))))))

(defun fb-find-repo-root (filename-or-dir)
  (car (fb-find-repo-root-and-type filename-or-dir)))

(defun fb-find-repo-type (filename-or-dir)
  (cdr (fb-find-repo-root-and-type filename-or-dir)))

(provide 'tbgX)
