;;; phabricator.el --- utils for jumping/navigating to phabricator

(require 'tbgX) ;; fb-find-repo-root
(require 'dired)

;;;###autoload

(defvar phabricator-diffusion-repo-alist
  '(("admin"        . "E/browse/admin/")
    ("www"          . "E/browse/tfb/trunk/www/")
    ("www-git"      . "E/browse/tfb/trunk/www/")
    ("www-hg"       . "E/browse/tfb/trunk/www/")
    ("fbcode"       . "FBCODE/browse/master/")
    ("fbobjc"       . "FBOBJC/browse/master/")
    ("opsfiles"     . "O/browse/opsfiles/branches/PROD/")
    ("opsfiles-git" . "O/browse/opsfiles/branches/PROD/")
    ("fbandroid"    . "FA/browse/master/"))
  "repo-alist for determining the appropriate path to phabricator")

(defun phabricator-diffusion-url (beg end)
  "Retrieves the corresponding diffusion (browse) URL for the current file,
   prints it in the minibuffer (via message), and puts the URL on the
   clipboard."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                   (list nil nil)))
  (flet ((line-num (pt)
                   (save-excursion
                     (goto-char pt)
                     (beginning-of-line)
                     (number-to-string (1+ (count-lines 1 (point)))))))
    (let* ((file-name (file-truename
                       (or (buffer-file-name) (dired-file-name-at-point))))
           (repo-path (directory-file-name (fb-find-repo-root file-name)))
           (repo-name (file-name-nondirectory repo-path))
           (rel-path (substring file-name (1+ (length repo-path))))
           (diffusion-prefix "https://phabricator.fb.com/diffusion/")
           (lines (if beg (concat "$" (line-num beg) "-" (line-num end)) ""))
           (url (concat diffusion-prefix
                        (cdr (assoc repo-name phabricator-diffusion-repo-alist))
                        rel-path lines)))
      (with-temp-buffer
        (message url)
        (insert url)
        (clipboard-kill-ring-save (point-min) (point-max))))))

(provide 'phabricator)
;;; phabricator.el ends here
