;; syntax for sps
(defun regexp-opt-ident (strings)
  (concat "\\_<" (regexp-opt strings 'true) "\\_>"))
(require 'generic-x)
;;;###autoload
(define-generic-mode 'sps-mode
  ;; comment
  '("//")
  ;; keywords
  '("alias" "constants" "edge" "enum" "externals" "if" "else" "for" "implements" "in" "interface" "node" "prop" "sources" "override" "let")
  ;; font-lock
  `(
    ( ,(regexp-opt-ident '("Bool" "Int" "Set" "String" "Pair")) . 'font-lock-type-face)
    ( ,(regexp-opt-ident '("true" "false" "null")) . 'font-lock-constant-face)

    ( ,(regexp-opt-ident '("this" "viewer" "viewer_context")) . 'font-lock-variable-name-face)

    ("\"[^\"]*\"" . 'font-lock-string-face)
    ("'[^']*'" . 'font-lock-string-face)
  )
  ;; activation
  '("\\.sps$")
  ;; other functions
  nil
  ;; doc
  "Highlight for SPS files")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sps\\'" . sps-mode))

(provide 'sps-mode)
