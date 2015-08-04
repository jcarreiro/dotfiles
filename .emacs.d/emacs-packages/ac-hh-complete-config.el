;;; ac-hh-complete.el --- Set of functions for running hh-complete under auto-complete

;; Copyright (C) 2013  Aaron Brady

;; Author: Aaron Brady <abrady@facebook.com>
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

;; Helper that defines the functionality necessary to run hh-complete
;; under auto-complete mode the functions in here follow the name and
;; form of other auto-complete configurations. (see
;; auto-complete-config.el for details)

;;; Code:

(eval-when-compile
  (when (fboundp #'byte-compile-disable-warning)
    (byte-compile-disable-warning 'cl-functions)
    (byte-compile-disable-warning 'obsolete)
    (byte-compile-disable-warning 'interactive-only)))

(require 'cl)
(require 'hh-complete)
(require 'auto-complete)

(declare-function xhp-mode-beginning-of-statement "xhp-mode")

(defvar ac-hh-complete-last-results nil
  "holds the results from the last ac-hh-complete call")
(make-local-variable 'ac-hh-complete-last-results)

(defun ac-hh-complete ()
  "return a set of strings for auto-complete mode to use for completion"
  (interactive)
  (let
      ((subprocess))
    (if (and
         (setq subprocess (hh-complete-run-process))
         (setq ac-hh-complete-last-results (hh-complete-parse-process-results
                        (SubProcess-buffername subprocess))))
        (mapcar 'HHCompleteResult-name ac-hh-complete-last-results))))

(defun ac-hh-complete-prefix ()
  "what prefix to match for doing an autocomplete"
  (or
   (ac-prefix-cc-member) ;; match -> and ::
   (ac-prefix-symbol)))  ;; match any symbol

(defun ac-hh-complete-docs-for-symbol (symbol)
  "helper, returns the 'documentation' for the passed symbol if it exists"
  (let
      ((item (find
              symbol
              ac-hh-complete-last-results
              :test (lambda (symbol elt)
                      (equal (HHCompleteResult-name elt) symbol)))
             )
       )
    (if item
        (car (HHCompleteResult-context item)))))

(defun ac-hh-complete-doc (symbol)
  "show documentation about the passed symbol using the match data"
  (ac-hh-complete-docs-for-symbol symbol))

(defvar ac-hh-complete-eldoc-last-result nil
  "cons of the last symbol-doc pair that matched")
(make-local-variable 'ac-hh-complete-eldoc-last-result)

(defun ac-hh-complete-eldoc-documentation-function ()
  (if (and (boundp 'ac-last-completion) ac-last-completion)
           (let*
               (
                (symbol-pos (car ac-last-completion))
                (symbol (cdr ac-last-completion))
                (docstr (ac-hh-complete-docs-for-symbol symbol)))
             (cond
              ;; probably only interested in doc if we're on the same line
              ;; at some position after the symbol that was completed
              ((not (and
                 (> (point) symbol-pos)
                 (= (line-number-at-pos symbol-pos)
                    (line-number-at-pos (xhp-mode-beginning-of-statement)))))
               nil)
              (docstr
               (setq ac-hh-complete-eldoc-last-result (cons symbol docstr))
               docstr)
              ((equal symbol (car ac-hh-complete-eldoc-last-result))
               (cdr ac-hh-complete-eldoc-last-result))))))

(setq eldoc-documentation-function 'ac-hh-complete-eldoc-documentation-function)

(ac-define-source hh-complete
  '((candidates . ac-hh-complete)
    (requires . 0) ;; # characters to type to trigger completion
    ;; TODO: add annotation types, i.e. function variable, etc.
    ;;(symbol . "s")
    (prefix . ac-hh-complete-prefix)
    (document . ac-hh-complete-doc)
    ))

(defun ac-hh-complete-mode-setup ()
  "set up completion hooks for hh complete"
  (setq ac-sources '(ac-source-hh-complete ac-source-words-in-buffer))

  ;; Note: with auto-start you shouldn't need to trigger
  ;; auto complete, so I've chosen not to bind this.
  ;; (ac-set-trigger-key "TAB")

  ;; these can be set to a delay value as well
  ;; but this feels more responsive to me
  (setq ac-auto-start t)
  (setq ac-auto-show-menu t)

  (setq ac-delay .01) ;; arbitrary but small number
  (ac-flyspell-workaround)
  )

(provide 'ac-hh-complete-config)
;;; ac-hh-complete-config.el ends here
