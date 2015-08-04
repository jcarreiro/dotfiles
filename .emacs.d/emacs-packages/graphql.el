;; GraphQL mode for .graphql files.
;; See https://tools.facebook.com/dex/graphql/graphqldata-on-ios

(require 'generic)
;;;###autoload
(define-generic-mode
    'graphql-mode
  '("//")
  '("Query" "QueryFragment" "as")
  '(("@[A-Z][A-Za-z0-9_\\-:]*" . font-lock-variable-name-face)
    ("QueryFragment[\s-]*\\([A-Z][A-Za-z0-9_\-:]*\\)" . (1 font-lock-function-name-face))
    ("QueryFragment[\s-]*\\([A-Z][A-Za-z0-9_\-:]*\\)[\s-]*:[\s-]*\\([A-Z][A-Za-z0-9_\-:]*\\)" . (2 font-lock-type-face)))
  '("\\.graphql")
  nil
  "GraphQL DSL mode")
(provide 'graphql)
