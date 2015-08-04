;;; subprocess-tests.el --- test file for the subprocess module

;; Copyright (C) 2013  Aaron Brady

;; Author: Aaron Brady <abrady@dev1735.prn1.facebook.com>
;; Keywords:

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

;; Unit tests for the SubProcess module. M-x eval-buffer to run

;;; Code:

(defun test (cond-p err-msg)
  (unless cond-p (error err-msg)))

(progn
  ;; do some simple tests with 'echo'
  (let*
      ((sp (make-SubProcess :remote-path "/bin/echo" :args '("one" "two" "three"))))
    (test (equal (SubProcess-local-path sp) "/tmp/echo") "local path broken")
    (test (equal (SubProcess-path sp) (SubProcess-local-path sp)) "path broken")
    (test (equal (SubProcess-buffername sp) "*SubProcess-echo*") "buffername broken")
    (if (file-exists-p (SubProcess-local-path sp))
        (SubProcess-local-path sp))
    (SubProcess-copy-to-local sp)
    (test (file-exists-p (SubProcess-local-path sp)) "copy-to-local broken"))
  ;; run 'sleep', then make sure it is running
  (let* ((running-proc (make-SubProcess
                         :remote-path "/bin/sleep"
                         :args '("1")
                         :copy-to-local-p nil))) ;; isolate the test a little
    (SubProcess-start running-proc)
    (test (SubProcess-running-p running-proc) "running-p 0 broken")
    (test (SubProcess-waitfor running-proc 10) "waitfor broken")
    (test (not (SubProcess-running-p running-proc)) "running-p 1 broken")))

;;; subprocess-tests.el ends here
