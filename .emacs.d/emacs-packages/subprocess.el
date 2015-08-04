;;; subprocess.el --- Set of helper functions for running processes under emacs

;; Copyright (C) 2013  Aaron Brady

;; Author: Aaron Brady <abrady@dev1735.prn1.facebook.com>
;; Keywords: tools

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

;; This module is designed to help you run subprocesses under emacs in a consistant way.
;; Typically you'll want to use this if you have a process you run repeatedly on, say,
;; whatever php file you're using currently, and would like to reuse the parameters,
;; output buffer, etc.

;; Contains the following set of tools for running subprocesses
;; - copying and running locally (as opposed to over NFS)
;; - process starting and stopping

;;; Code:

(eval-when-compile
  (require 'cl))

(defstruct SubProcess
  remote-path
  (copy-to-local-p t)
  (kill-previous-on-run-p t)
  proc
  args)

(defun SubProcess-local-path (subprocess)
  (unless (SubProcess-p subprocess) (error "need to pass SubProcess structure"))
  (concat "/tmp/" (file-name-nondirectory (SubProcess-remote-path subprocess))))

(defun SubProcess-path (subprocess)
  (unless (SubProcess-p subprocess) (error "need to pass SubProcess structure"))
  (if (SubProcess-copy-to-local-p subprocess)
      (SubProcess-local-path subprocess)
    (SubProcess-remote-path subprocess)))

(defun SubProcess-buffername (subprocess)
  (format "*SubProcess-%s*"
          (file-name-sans-extension
           (file-name-nondirectory
            (SubProcess-remote-path subprocess)))))

(defun SubProcess-copy-to-local (subprocess)
  "Since we're caching a local copy of the subprocess we don't want to wait on copy
every time, but we still run the copy process in the background so that the process binary
will be updated the next time it runs

If the file doesn't exist locally this blocks on copy"
  (if (SubProcess-copy-to-local-p subprocess)
      (let*
          ((local-exists-p (file-exists-p (SubProcess-local-path subprocess)))
           (cp-proc
             (make-SubProcess
              :remote-path "cp"
              :copy-to-local-p nil
              :args (list
                     "--archive"
                     "--update"
                     ;; delete the file if necessary. this will cause
                     ;; the existing file to be removed after it is
                     ;; done running.
                     "-f"
                     (SubProcess-remote-path subprocess)
                     (SubProcess-local-path subprocess)
                     ))))
        (SubProcess-start cp-proc)
        (if (not local-exists-p)
            (SubProcess-waitfor cp-proc 1)))
    ))

(defun SubProcess-running-p (subprocess)
  (and (SubProcess-proc subprocess)
       (eq (process-status (SubProcess-proc subprocess)) 'run)))

(defun SubProcess-start (subprocess)
  "Start an instance of a subprocess, return the process on success"
  ;; copy if required
  (SubProcess-copy-to-local subprocess)

  ;; kill running
  (if (SubProcess-kill-previous-on-run-p subprocess)
      (SubProcess-kill subprocess))

  ;; clear the buffer
  (if (get-buffer (SubProcess-buffername subprocess))
      (with-current-buffer (SubProcess-buffername subprocess)
        (erase-buffer)))

  ;; start new
  (let
      ((args (list
              (SubProcess-path subprocess)
              (SubProcess-buffername subprocess)
              (SubProcess-path subprocess))))
    (if (SubProcess-args subprocess)
        (setq args (append args (SubProcess-args subprocess))))
    (setf (SubProcess-proc subprocess) (apply 'start-process args)))

  ;; make it so emacs doesn't tell you that this is running when you
  ;; exit
  (set-process-query-on-exit-flag (SubProcess-proc subprocess) nil)
  (SubProcess-proc subprocess))

(defun SubProcess-waitfor (subprocess duration &optional &key kill-after-wait)
  "Helper for blocking and waiting a given period for the process to complete. returns t if process exited"
  (let
      (res)
    (setq res (loop with dt = 0 while (< dt duration)
        if (not (SubProcess-running-p subprocess)) return t
        do
        (sleep-for 0.05)
        (setq dt (+ dt .05))
        ))
    (if kill-after-wait
        (SubProcess-kill subprocess))
    res))

(defun SubProcess-kill (subprocess)
  (if (SubProcess-running-p subprocess)
      (kill-process (SubProcess-proc subprocess))))

(provide 'subprocess)
;;; subprocess.el ends here
