;;; hphpd.el --- gud derived mode to debug php via hphpd
;;; Copyright (C) 2011 Facebook Inc.
;;; Author: Aaron Brady <abrady0@fb.com>
;;; https://github.com/facebook/hiphop-php/blob/master/doc/debugger.start
;;; Usage:
;;   Load this library and type M-x hphpd to start.
;;
;;; Commentary: This file implements a gud mode for the hphpd
;;   debugger. The functions of interest are (accessed via C-x C-a <char>:
;;   - gud-next (n)
;;   - gud-cont (c)
;;   - gud-finish (f)
;;   - gud-up (<) : up the stack
;;   - gud-down (>) : down stack
;;   - gud-print (p) : expression at point
;;   - gud-jump (j) : jump to specified line
;;   - gud-fbreak : break at specified function
;;
;;;  Other functions you may find useful, but are not bound to any keys:
;;     - hphpd-gud-break-func : will set a breakpoint at a specific function
;;     - hphpd-linenum : copy the line and file to the kill ring for you
;;     - hphpd-send-line : send to end of line
;;     - hphpd-send-stmt : send up to the next semicolon
;;     - hphpd-send-region : set a region and run this, it will send
;;     - hphpd-require-module : same as running require_module on the
;;       command line, but auto-completes whatever library contains the
;;       current buffer it is called in
;;     - hphpd-kill-process : if hphpd just stops responding, this will kill it
;;
(require 'ansi-color)
(require 'gud)

(eval-when-compile
  (when (fboundp #'byte-compile-disable-warning)
    (byte-compile-disable-warning 'obsolete)))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-hphpd-marker-filter (string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output "")
        ;; what hphpi prints for next,step, etc.
        (file-group)
        (line-group))

    ;; Process all the complete markers in this chunk.
    (while (or
            ;; printed for (n)ext and (s)tep commands
            (and
             ;; setting breakpoints looks like hitting except for the 'set' part
             (not (string-match "Breakpoint [0-9]+ set on" gud-marker-acc))
             (string-match "Break.*on line \\([0-9]+\\) of \\(/.*\.phpt?\\)"
                           gud-marker-acc) (setq file-group 2 line-group 1))
            ;; printed when up frame or down frame commands
            ;; NOTE: this code means the (w)here command
            ;; will always pop you to the top of the stack.
            (and
             (string-match
              "#[0-9]+  .*\n.*at \\(.*\\):\\([0-9]+\\)"
              gud-marker-acc)
             (setq file-group 1 line-group 2)
             )
            )
          (setq
           ;; Extract the frame position from the marker.
           gud-last-frame
           (let ((file (match-string file-group
                                     gud-marker-acc))
                 (line (string-to-number
                        (match-string line-group
                                      gud-marker-acc))))
             (cons file line))

           ;; Output everything instead of the below
           output (concat output (substring gud-marker-acc 0 (match-end 0)))

           ;; Set the accumulator to the remaining text.
           gud-marker-acc (substring gud-marker-acc (match-end 0))
           )
        )
    (setq output (concat output gud-marker-acc)
          gud-marker-acc "")
    output)
  )


(defun hphpd-gud-break-func (func-name)
  "set a breakpoint at a named function, e.g. Foo::bar"
  (interactive (list (read-string "func: " (thing-at-point 'sexp))))
  (gud-call (format "break %s()" func-name))
  )

(defun hphpd-cond-break (cond)
  "set a conditional breakpoint at the current line/file"
  (interactive "sCondition:") ;; TODO(abrady) : get completions from subprocess
  (gud-call (format "break %s if (%s)" (hphpd-linenum) cond)))

(defun hphpd-linenum ()
  "get the current file and linenumber in foo.php:12, stick it in the kill ring"
  (interactive)
  (let ((str (format "%s:%d" (buffer-file-name) (line-number-at-pos (point)))))
    (kill-new str)
    (message str))
  )

(defun hphpd-send-line ()
  "in a php source file, send the current line to the debugger for exection
e.g. calling this on the following line would assign $a the value of 2:
$a = 1 + 1; "
  (interactive)
    (let ((s))
      (setq s (buffer-substring (line-beginning-position) (line-end-position)))
      (message s)
      (gud-call (format "= %s" s))
      (forward-line 1)
      )
  )


(defun hphpd-send-stmt ()
  "Similar to `hphpd-send-line', sends up to the next semicolon to the debugger
for evaluation"
  (interactive)
  (beginning-of-line)
    (let ((s))
      (setq s (buffer-substring (line-beginning-position)
                                (and (search-forward ";") (point)))
            )
      (message s)
      (gud-call (format "<? %s ?>" s))
      )
  )

(defun hphpd-send-region (start end)
  "send the current region to the debugger for exection"
  (interactive "r")
  (save-excursion
    (let ((s (buffer-substring start end)))
      (message s)
      (if (= 1 (count-lines start end))
          (gud-call (format "p %s" s))
        (gud-call (format "<? %s ?>" s))
        )
      )
    )
  )

(defun hphpd-send-string (str)
  "send passed string to hphpd if it is running"
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (if (and proc (equal (car (process-command proc)) "hphpd"))
        (with-current-buffer gud-comint-buffer
          (gud-basic-call str)
          t))))

(defun hphpd-require-module (module)
  "require a module. will try to smart-complete if you are in a buffer that is
under flib e.g.: current-buffer is /flib/foo/bar/baz.php will suggest 'foo/bar'"
  (interactive (list (read-string "module: "
                                  (let ((s (buffer-file-name)))
                                    (if (string-match "/flib/\\(.*\\)/.*" s)
                                        (match-string 1 s)
                                      "")))
                     )
               )
  (gud-call (format "require_module('%s')" module))
  )

(defun hphpd-kill-process ()
  "kill a runaway hphpd process"
  (interactive)
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (kill-process proc)
    )
  )

(defun hphpd-run ()
  "require a module. will try to smart-complete if you are in a buffer that is
under flib e.g.: current-buffer is /flib/foo/bar/baz.php will suggest 'foo/bar'"
  (interactive)
  (gud-call "run")
  )

(defun hphpd-complete-command ()
  "Perform completion on the hphpd command preceding point.
This is implemented using the hphpd's `complete' command"
  (interactive)
  (let* ((command-word (save-excursion
                         (and
                          (re-search-backward "[ \t=]\\(.*\\)" (comint-line-beginning-position) t)
                          (match-string 1))))
         (complete-list))
    (if (< 0 (length command-word))
        (progn
          (setq complete-list
                (gud-gdb-run-command-fetch-lines (concat "complete " command-word)
                                                 (current-buffer)))
          ;; Sort the list like readline.
          (setq complete-list (sort complete-list (function string-lessp)))

          ;; Let comint handle the rest.
          (comint-dynamic-simple-complete command-word complete-list)))))

;;;###autoload
(defun hphpd (&optional command-line)
  "Run hphpd debugger as a subprocess of emacs"
  (interactive)
  (save-excursion
    ;; default to localhost debugging
    (if (not command-line)
        (setq command-line "hphpd -h localhost"))
    (gud-common-init command-line nil 'gud-hphpd-marker-filter)
    (set (make-local-variable 'gud-minor-mode) 'hphpd)

    (gud-def gud-break  "break %d%f:%l"  "\C-b" "Set breakpoint current line.")
    (gud-def gud-step   "step"         "\C-s" "Step one source line, display.")
    (gud-def gud-next   "next"         "\C-n" "Step one line (skip functions).")
    (gud-def gud-cont   "continue"     "\C-r" "Continue with display.")
    (gud-def gud-finish "out"          "\C-f" "Finish current function.")
    (gud-def gud-up     "up"           "<" "Up one stack frame.")
    (gud-def gud-down   "down"         ">" "Down one stack frame.")
    (gud-def gud-print  "p %e"         "\C-p" "Evaluate PHP at point.")
    (gud-def gud-jump   "jump %f:%l"   "\C-j" "jump to the specified line")
    (defalias 'gud-fbreak 'hphpd-gud-break-func)

    ;; (setq comint-prompt-regexp "^(.*pdb[+]?) *")
    (local-set-key "\C-i" 'hphpd-complete-command)
    (setq comint-prompt-regexp "^\\(hphpd\\|localhost\\|[0-9.]+\\)> ")
    (setq paragraph-start comint-prompt-regexp)
    (ansi-color-for-comint-mode-on)
    (set-window-dedicated-p nil t)
    (run-hooks 'hphpd-mode-hook)
    )
  )

(defun hphpd-unittest (file &optional filter)
   "run a unit test under hphpd"
  (interactive
   (list (read-string "target: " (buffer-file-name))))

  ;; kill if running
  (let*
      ((existing-buffer (concat "*gud-" (file-name-nondirectory file) "*")))
    (if (get-buffer existing-buffer)
        (kill-buffer existing-buffer)))

  (hphpd (format "t --debugger %s%s" file
                  (if (< 0 (length filter)) (format " --filter=%s" filter) "")))
   (gud-call "c")
  (with-current-buffer gud-comint-buffer
    (goto-char (point-max))
    (insert "$t->run()")))


(defun hphpd-script (file-or-dir)
  "run a unit test under hphpd"
  (interactive "ftarget script:")
  (hphpd (format "hphpd -f %s" file-or-dir))
)

(provide 'hphpd)
