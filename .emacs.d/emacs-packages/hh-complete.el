;;; hh-complete.el --- Provides auto complete functionality for hack

;; Copyright (C) 2013  Aaron Brady

;; Author: Aaron Brady <abrady@facebook.com>
;; Keywords: tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; provides auto complete for hack and php files. This does three things:
;; - run the hh_client process and put the results in a buffer
;; - parse the buffer to create the structured results
;; - handle the results in an appropriate manner (insert results, query user etc.)

;; NOTE: requires a running instance of hack server,
;; e.g. /home/engshare/tools/hh_server ~/www

;;; Code:

(eval-when-compile
  (when (fboundp #'byte-compile-disable-warning)
    (byte-compile-disable-warning 'cl-functions)))

(require 'cl)
(require 'subprocess)

(defgroup hh nil
  "Hack configuration"
  :prefix "hh-"
  :group 'development)

(defcustom hh-client-remote-path "/home/engshare/tools/hh_client"
  "the binary that will talk to the hack server for completion information"
  :group 'hh)

(defcustom hh-complete-process-wait-time .2
  "amount of time to wait for hh_server to get back with results. Keep this low for the interface
to be responsive even if the hack server isn't, raise it if you're not getting your results in time"
  :group 'hh)

(defvar hh-complete-act-on-results 'hh-complete-act-on-results-default
  "Function to handle the results of a complete call. Set this to something else if you want
your own results handler.
function is of form (lambda (RESULTS)) where RESULTS is a list of HHCompleteResult ")

(defun hh-complete-run-process ()
  "Helper:
- launches the complete process on the current buffer
- waits for it to complete
returns the SubProcess object if successful
"
  (let
      ((subprocess (make-SubProcess
              :remote-path hh-client-remote-path
              :args (list "--from" "emacs" "--auto-complete")))
       (proc))
    (setq proc (SubProcess-start subprocess))
    (process-send-region proc (point-min) (point))
    ;; magic token for where to complete
    (process-send-string proc "AUTO332")
    (process-send-region proc (point) (point-max))
    (process-send-eof proc)

    (if (SubProcess-waitfor
         subprocess
         hh-complete-process-wait-time
         :kill-after-wait t)
        subprocess)))

(defstruct HHCompleteResult name type context)

(defvar hh-complete-last-results nil
  "stores last set of results processed by `hh-complete-parse-process-results'")

(defun hh-complete-parse-process-results (bufname)
  "helper for turning the results from hh_cleint into a list of
  HHCompleteResult structs
"
  (with-current-buffer bufname
    (goto-char (point-min))
    (let
        ((names)
         (max (or
               (save-excursion (re-search-forward "^$" (point-max) t))
               (point-max))))
      ;; the word at the beginning of the line is the name being completed
      ;; after that is type information (bool, (function: int) etc.)
      (setq hh-complete-last-results
        (loop while (< (point) max)
            ;; Match each line to the type of results.
            ;; NOTE: order matters here

            ;; match error from subprocess first so we can error out
            if (looking-at "Error: .*") do (error (match-string 0))

            ;; match function: name (function(args): return)
            else if (looking-at "\\(.*?\\) (function(\\(.*?\\)): \\(.*\\))")
              collect (make-HHCompleteResult
               :name (match-string-no-properties 1)
               :type 'method
               :context (list
                         (match-string-no-properties 2)  ;; args
                         (match-string-no-properties 3))) ;; return type

            ;; match member: name type
            else if (looking-at "\\(.+?\\) \\(.+\\)")
              collect (make-HHCompleteResult
                       :name (match-string-no-properties 1)
                       :type 'member
                       :context (match-string-no-properties 2))
            ;; match: variable of some kind
            else if (looking-at "\\(.+?\\)$")
              collect (make-HHCompleteResult
                       :name (match-string-no-properties 1)
                       :type 'variable
                       :context (match-string-no-properties 2))
            do (forward-line 1))))))

(defun hh-complete-insert-result (result)
  "handle inserting the passed HHCompleteResult into the current
point in the buffer, taking care of any cleanup required"
  ;; part 1: delete existing typed values
  (case (HHCompleteResult-type result)
   ;; if this is variable replacement, delete back to $
    ('variable (if (looking-back "\\([ ]\\|^\\).*" (line-beginning-position))
                   (delete-region (match-end 1) (point))))
   ;; else member function or variable or best guess
   (t
    (delete-region
     (or
      (and
       (save-excursion (re-search-backward "->" (line-beginning-position) t))
       (match-end 0))
      (point))
     (point))))

  ;; part 2: insert new result
  (insert (HHCompleteResult-name result)))

(defun hh-complete-act-on-results-default (results)
  "Helper, determines what action should result from the auto complete results:
- insert automatically: if only one result
- prompt user to choose if multiple

RESULTS is a list of HHCompleteResult

return t if results are handled
"
  (cond
   ((= 1 (length results)) (hh-complete-insert-result (car results)))
   (t
    (let*
        ((search-back-for-str (lambda (str)
                     (save-excursion (re-search-backward str (line-beginning-position) t))))
         (arrow_point (and (funcall search-back-for-str "->") (match-end 0)))
         (dollar_point (and (funcall search-back-for-str "\\$") (match-beginning 0)))
         ;; just the closest of the two points ->foo or $foo, or
         ;; current point if neither is found (which shouldn't happen if we have results)
         ;; also relies on (buffer-substring (point) (point)) being empty string
         (pick-initial-text-start (lambda ()
                                    (if (and arrow_point dollar_point)
                                        (max arrow_point dollar_point)
                                      (or arrow_point dollar_point (point)))))
         (initial_text (buffer-substring-no-properties
                        (funcall pick-initial-text-start)
                        (point)))
         (choice))
      ;; offer completion prompt, including what has been typed so far
      (setq choice (ido-completing-read
                    ""
                    (mapcar 'HHCompleteResult-name results)
                    nil
                    t
                    initial_text))
      (hh-complete-insert-result (find
                                  choice
                                  results
                                  :test (lambda (choice result)
                                          (equal
                                           choice
                                           (HHCompleteResult-name result)))))))
   ))


(defun hh-complete ()
  "try to complete at the current point in the file"
  (interactive)
  (let
      ((subprocess)
       (results))
    (if (and
         (setq subprocess (hh-complete-run-process))
         (setq results (hh-complete-parse-process-results
                        (SubProcess-buffername subprocess))))
        (progn
          (funcall hh-complete-act-on-results results)
          t))))

(defvar hh-complete-mode-map (make-sparse-keymap)
  "hh-complete mode map.")

(defcustom hh-complete-trigger-key "TAB"
  "key for triggering an hh-complete. if you change this make sure to update the mode map")

(defun hh-complete-key-pressed ()
  "Function invoked when the complete key is pressed"
  (interactive)
  (unless (and
             ;; rough heuristic:
             ;; don't try to complete unless we're looking at whitespace and we've typed something
             (looking-back "[^ ]") (not (looking-back "^")) ;; some non-whitespace before cursor
             (looking-at "\\([ ]\\|$\\)") ;; whitespace or end of line after cursor
             (hh-complete))
      ;; fallback to mode TAB key
      (let*
          ;; turn off the mode and get the command that would have been called
          ((hh-complete-mode nil)
           (keys (this-command-keys-vector))
           (command (if keys (key-binding keys))))
        (if (commandp command)
            (call-interactively command)))))

(define-key hh-complete-mode-map (read-kbd-macro hh-complete-trigger-key)
  'hh-complete-key-pressed)

(define-minor-mode hh-complete-mode
  "Hack mode"
  :keymap hh-complete-mode-map
  :group 'hh-complete
  )

(provide 'hh-complete)
;;; hh-complete.el ends here
