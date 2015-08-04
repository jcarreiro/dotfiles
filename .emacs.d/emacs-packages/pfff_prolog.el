;;; pfff-prolog.el --- an interface to pfff prolog for code introspection
;;
;; Author: Aaron Brady <abrady@fb.com>
;; Version: 0.1
;;
;;; Commentary:

;; @pad made a very cool tool that allows one to ask interesting
;; questions about the codebase using prolog, see
;; https://our.intern.facebook.com/intern/wiki/index.php/Pfff/PrologWww
;; This package provides an interface for running
;; commands with that tool and then easily using the results in emacs

;; - pfff-prolog-shell : will open the prolog shell for buildilng queries
;; - pfff-prolog-run-last-shell-cmd-as-find: if you run one that is
;;   useful you can call this and have its output be browsable and
;;   selectable. See documentation for this
;; - pfff-prolog-shell-insert-procedure: may make building queries
;;   easier for you, it will prompt you for and insert prolog
;;   functions at the command line
;;
;; NOTE: When using this code 'X' is, by convention, the variable used
;; for context, and FILE and LINE are the variables that identify the
;; filename and location of the context.  in. BE SURE TO USE 'X', 'FILE' and 'LINE' IN
;; YOUR QUERIES.

;; For example, you might want to find functions that call functions x
;; and y (including xhp), after calling  you could enter:
;;; M-x pfff-prolog-shell
;;; ?- docall(X,':m:timeline:unit-actor',_), docall(X,':m:timeline:message:robotext', _), at(X,FILE, LINE).
;;; M-x pfff-prolog-run-last-shell-cmd-as-find
;;; see list of classes and functions where functions from these classes are called

;; other cool examples:
;; - kind((X, 'fooBar'), method), kind(X, trait), at(X,FILE, LINE). %% traits that implement method fooBar
;; - implements('EntOpenGraphObject', X), implements('EntPersonalUser', X), at(X,FILE, LINE). %% what interfaces do these two classes have in common?
;; - kind(X, 'interface'), kind((X, 'getDescription'), method), implements('EntOpenGraphObject', X), at(X,FILE, LINE). %% what interface of EntOpenGraphObject has getDescription
;; - kind((Y, 'getDescription'), method), kind(Y, interface), extends_or_implements(X, Y), at(X,FILE, LINE). %% find classes X that implement getDescription from some interface Y

;; elisp functions you may find helpful:
;; - pfff-prolog-find-tag : just like find-tag (usually bound to M-.), also supports fully qualified (Foo::barBaz) syntax)
;; - pfff-prolog-find-callers : find locations that invoke a given function
;; - pfff-prolog-find-classes-with-method
;; - pfff-prolog-find-extendors-of-class
;; - pfff-prolog-find-users-of-trait
;; - pfff-prolog-find-trait-that-implements-method

;; To save typing, these aliases are defined:
;; - pft : pfff-prolog-find-tag
;; - pfe : pfff-prolog-find-extendors-of-class
;; - pfc : pfff-prolog-find-callers
;; if enabled you can say M-x pft and find a tag
;; (controlled by pfff-prolog-define-aliases)

;; other notes:
;; /home/engshare/pfff/database_code.pl : where functions live. use it to help with your freeform queries.
;; /home/engshare/pfff/facts.pl

;; TODOS
;;; - recognize and disambiguate parent::
;;; - find location up hierarchy, e.g. FriendsEntQuery::genIDs => EntQuery::genIDs
;;; - reload facts.pl when it changes
;;; - integrate with tags as well as gtags
;;; - when grabbing input string, drop any quotes at beginning or end
;;; - find class that implements that takes a child class:
;;;   extends_or_implements('MEgoRedirectController', X), at((X,'getRedirectURI'),FILE,LINE).
;;; Globals and constants

(eval-when-compile
  (when (fboundp #'byte-compile-disable-warning)
    (byte-compile-disable-warning 'cl-functions)))

(require 'xhp-semantic)
(require 'etags)
(require 'comint)
(require 'cl)

(defvar pfff-prolog-process nil
  "the single running pfff_ process"
  )

(defgroup pfff-prolog nil "Customizations for PFFF Prolog query."
  :prefix "pfff-prolog-"
  :group 'development)

(defcustom pfff-prolog-binary-name "cmf"
  "the binary that will do the prolog tag searching"
  :group 'pfff-prolog)

(defcustom pfff-prolog-binary-args "--prolog"
  "arguments to the searching process"
  :group 'pfff-prolog)

(defcustom pfff-prolog-prompt-regexp "^\\(\\?- +\\|| +\\|?- | +\\)"
  "matches the prompt"
  :group 'pfff-prolog)

(defcustom pfff-prolog-www-root "~/www"
  "the www root directory"
  :group 'pfff-prolog)

(defcustom pfff-prolog-on-tag-found-cb 'pfff-prolog-smart-on-tag-found
  "function to be invoked when a result is selected, usually used to integrate with
a tags system so that you can pop back to where you last were"
  :group 'pfff-prolog)

(defcustom pfff-prolog-define-aliases t
  "if t, define a set of three letter aliases of functions for
  quickly running pfff queries. e.g. M-x pft =>
  pfff-prolog-find-tag"
  :group 'pfff-prolog)

;;; Helpers

(defun pfff-prolog-symbol-bounds-at-point ()
  "get the token at point if it is php or xhp"
  (if (looking-at ":[^:]")
      (forward-char 1))
  (let*
      (
       (find-start
        (lambda ()
          (cond
           ((re-search-backward "[^a-z0-9-_:]" (line-beginning-position) t) (1+ (match-beginning 0)))
           (t (line-beginning-position)))))
       (find-end
        (lambda ()
          (and
           ;; match xhp and php, second regexp simulates not ending with xhp
           ;; valid chars like -, so we don't match ->
           (re-search-forward "[a-z-0-9_:]*[a-z_:]+" (line-end-position) t)
           (match-end 0)))))
    (save-excursion
      (delq  ;; filter out nil results
       nil
       (list
        (funcall find-start)
        (funcall find-end))))))

(defun pfff-prolog-symbol-at-point ()
  "helper that grabs the symbol around the cursor in an educated way"
  (let
      ((bounds (pfff-prolog-symbol-bounds-at-point)))
    (let
        ((tag (if (eq (length bounds) 2)
        (apply 'buffer-substring-no-properties bounds))))
      ;; if this looks like xhp prepend ':' to match class name
      (setq tag (if (and tag (string-match "^[^:]+:[^:]" tag))
                    (concat ":" tag)
                  tag))
      tag)))

(defun pfff-prolog-read-word-or-region ()
  "Read the word at point, or region if active."
  (let
      ((tag))
    (setq tag
          (if mark-active
              (buffer-substring (region-beginning) (region-end))
            (pfff-prolog-symbol-at-point)))
    ))

(defvar pfff-prolog-process-status nil
  "this guarantees atomicity of the queries for the prolog subprocess.
Its value is set to the state of results currently being collected, nil if not collecting")

(defstruct PfffResult filename line context)

(defvar pfff-prolog-last-query nil
  "the command that created the results from the last query (may be wrong if query fails)")

(defvar pfff-prolog-results nil
  "the results from the last query, list of type PfffResult")

(defvar pfff-prolog-result-context-formatter nil
  "callback function for formatting the context field of a result.
takes a single string argument and returns the formatted results.
e.g. 'Preparable,gen' => Preparable::gen")

(defun pfff-prolog-smart-on-tag-found (filename line starting-buf)
  "tries to figure out which method a user normally uses to find tags
e.g. gtags, or etags, and saves the position so that e.g. pop-tag-mark or gtags-pop-stack will take the user back to their last position"
  (cond
   ((assoc 'gtags-mode minor-mode-alist)
    (pfff-prolog-gtags-on-tag-found filename line starting-buf))
   ((boundp 'find-tag-marker-ring) (pfff-prolog-etags-on-tag-found filename line starting-buf))
   (t (pfff-prolog-basic-on-tag-found filename line starting-buf))))

(defun pfff-prolog-basic-on-tag-found (filename line starting-buf)
  "jump to the specified file and line"
  (let
      ((fn (concat (file-name-as-directory pfff-prolog-www-root) filename)))
    (if (file-exists-p fn)
        (progn
          (find-file fn)
          (if line
              (progn
                (goto-char (point-min))
                (forward-line (1- (string-to-number line)))))
          t
          )
      (message "file %s doesn't exist" fn))))

(declare-function gtags-push-context "gtags")

(defun pfff-prolog-gtags-on-tag-found (filename line starting-buf)
  "jump to the specified file and line using gtags"
  (if (pfff-prolog-basic-on-tag-found filename line starting-buf)
      (with-current-buffer starting-buf ;; don't want the output buffer
        (gtags-push-context))))

(defun pfff-prolog-etags-on-tag-found (filename line starting-buf)
  "jump to the specified file and line using etags"
  (if (pfff-prolog-basic-on-tag-found filename line starting-buf)
      (with-current-buffer starting-buf ;; don't want the output buffer
        (ring-insert find-tag-marker-ring (point-marker)))))

(defun pfff-prolog-on-query-complete (starting-buf)
  "invoked after we've gotten all the results from a query.
uses `pfff-prolog-results'"
  (with-current-buffer "*pfff-prolog-find-results*"
    (save-excursion
      (goto-char (point-max))
      (read-only-mode 0)
      (insert "\nquery finished.")))
  (cond
   ;; just one result: jump to it
   ((eq 1 (length pfff-prolog-results))
      (let*
          (
           (result (car pfff-prolog-results))
           (fn (PfffResult-filename result))
           (line (PfffResult-line result))
           )
        (if fn
            (funcall pfff-prolog-on-tag-found-cb fn line starting-buf)
            )))
   ;; multiple results:
   ;; maybe a little wonky, but I like the feel of only opening the results window
   ;; once the results are in, alternatively this could happen once the first set
   ;; of results come in and we know there are multiple results
   (t (switch-to-buffer-other-window "*pfff-prolog-find-results*"))))

(defun pfff-prolog-display-init (query)
  "get the results display buffer ready to receive results"
  (with-current-buffer (get-buffer-create "*pfff-prolog-find-results*")
    (cd pfff-prolog-www-root)
    (fundamental-mode)
    (read-only-mode 0)
    (erase-buffer)
    (insert (format "results for '%s':\n\n" query))
    ))

(defun pfff-prolog-display-add-results (results)
  "put the results of the last pfff call in a *grep* style buffer"
  (with-current-buffer (get-buffer-create "*pfff-prolog-find-results*")
    ;; save-excursion so users can interact with results buffer
    ;; while more results are coming in
    (save-excursion
      (fundamental-mode)
      (read-only-mode 0)
      (goto-char (point-min))
      (loop for result in results do
            (insert (format "%s:%s:\t%s\n"
                            (PfffResult-filename result)
                            (PfffResult-line result)
                            (or (PfffResult-context result) ""))))
      (grep-mode)
      )))

;; query lifecycle
;; - kill pfff process if previous query still running
;; - erase *pfff-prolog* buffer
;; - accum output in buffer until query finished
;; - process all results
(defun pfff-prolog-output-checker  (proc output)
  "handles the output from the prolog process"
  (let*
      ((starting-buf (current-buffer)))
       (with-current-buffer (get-buffer-create "*pfff-prolog*")
         (let
             (
              (terms)
              (context-formatter
               (or
                pfff-prolog-result-context-formatter
                'pfff-prolog-result-context-method-formatter))
              (results)
              )
           (insert output)

           ;; process accumulated lines
           (goto-char (point-min))

           ;; catch any errors before processing, the result tags
           ;; may be embedded in the error message lines
           (if (save-excursion (re-search-forward "^ERROR: \\(.*\\)" nil t))
               (progn
                 ;; error of some kind, likely caused by invalid query
                 ;; bail out
                 (setq pfff-prolog-process-status nil)
                 (error "pfff query failed '%s', check '%s' buffer for more info" (match-string 1) (buffer-name))))

           (while (re-search-forward "RESULT=\\(.*?\\);\\(.*?\\);\\(.*\\)=ENDRESULT" nil t)
             (add-to-list
              'results
              (make-PfffResult
               :filename (match-string 1)
               :line (match-string 2)
               :context (funcall context-formatter (match-string 3)))
              t)
             (forward-line 1))
           ;; remove any processed results
           (delete-region (point-min) (point))

           ;; accumulate and add results to display buffer
           (setq pfff-prolog-results (append pfff-prolog-results results))
           (pfff-prolog-display-add-results results)

           ;; implicit assumption: 'false.' will be the last
           ;; statement regardless of results
           (if (save-excursion (re-search-forward "^false.$" nil t))
               (progn
               ;; we've reached the prompt, so the process is done outputting data.
               ;; scrape up the collected output
                 (pfff-prolog-on-query-complete starting-buf)
                 (message "pfff query finished.")
                 (setq pfff-prolog-process-status nil)))
           ))))

(defun pfff-prolog-process-running ()
  (and pfff-prolog-process (eq (process-status pfff-prolog-process) 'run)))

(defun pfff-prolog-launch ()
  "make sure process is running"
  (if (not (pfff-prolog-process-running))
      (progn
        (setq pfff-prolog-process
              (start-process pfff-prolog-binary-name "*pfff-prolog*" pfff-prolog-binary-name pfff-prolog-binary-args))
        (set-process-filter pfff-prolog-process 'pfff-prolog-output-checker))))

(defun pfff-prolog-kill ()
  "kill the running prolog process. fails silently"
  (interactive)
  (if (pfff-prolog-process-running)
      (kill-process pfff-prolog-process))
  (setq pfff-prolog-process nil)
  (setq pfff-prolog-process-status nil))

;;; Main utility function to interact with the prolog subshell

(defun pfff-prolog-send-cmd (cmd &optional result-context-formatter)
  "send a command to the prolog subprocess. also do any setting up
 of state so the query can be processed properly"
  (if pfff-prolog-process-status
      (if (not (y-or-n-p "previous process is still running, kill it?"))
          (error "prevous pfff query still running")
        (message "killing existing pfff process")
        (pfff-prolog-kill)))
  (pfff-prolog-launch)

  ;; set up state for query
  (with-current-buffer "*pfff-prolog*" (erase-buffer))
  (setq pfff-prolog-results nil)
  (setq pfff-prolog-last-query cmd)
  (pfff-prolog-display-init cmd)

  ;; format and send command to swipl.
  ;; output handler expects results of the following form for each line:
  ;; RESULT=<file>;<line>;<context>=ENDRESULT
  (setq cmd (concat
             cmd
             ",writef('RESULT=%w;%w;%w=ENDRESULT\\n',[FILE, LINE, X]), fail.\n"))
  (setq pfff-prolog-result-context-formatter result-context-formatter)
  (setq pfff-prolog-process-status 'running)

  ;; get process going
  (process-send-string pfff-prolog-process cmd))

(defun pfff-prolog-result-context-method-formatter (s)
  "turns results of the form Foo,Bar into Foo::Bar"
  (let
      ((strs (split-string s ",")))
    (cond
     ((= (length strs) 2) (format "%s::%s" (nth 0 strs) (nth 1 strs)))
     (t s))))

(defun pfff-prolog-split-class-and-method (class-and-method)
  (if (string-match "\\(.*\\)::\\(.*\\)" class-and-method)
      (let ((res (cons (match-string 1 class-and-method)
                       (match-string 2 class-and-method))))
        (if (and (equal (car res) "self") (xhp-semantic-get-class-for-point))
            (cons (xhp-semantic-get-class-for-point) (cdr res))
          res))))

(defun pfff-prologify-method-match (var tag)
  "helper for making an expression that can match yieldFoo if it is possible that the tag
could be a dynamic yield member"
  (let
      ((method-expr-maker (lambda (method-name)
                           (format "%s=(CLASSNAME,'%s')" var method-name))))
  (if (string-match "^\\(gen\\|get\\)\\(.*\\)" tag)
      (concat
       "("
       ;; match yieldFoo
       (funcall method-expr-maker (format "yield%s" (match-string 2 tag)))
       ";" ;; 'or'
       ;; getFoo/genFoo (whatever was passed in
       (funcall method-expr-maker tag)
       ")"
       )
    (funcall method-expr-maker tag))))


(defun pfff-prologify-tag (tag &optional var)
  "helper function for converting a php `tag' into the foundation of a prolog query
to find its location.
`tag' is processed slightly:
-  If it has two semicolons (::) in the tag, e.g. it looks like Foo::Bar, this function will try to find the location of a class method or property, including looking in mixins.
- self::Foo will try to infer the class name from context.
- Otherwise it will try to find the passed `tag' in all locations including as members, properties, or as standalone functions.
"
  (setq var (or var "X"))
  (let
      ((class-method-pair (pfff-prolog-split-class-and-method tag))
       (classname-query "") ;; assigns CLASSNAME variable
       (var-query "")       ;; assigns X, or `var' if passed in
       )
    ;; set up queries by what we know about the tag
    (cond
     ;; if a class name is found or specified
     ;; add a class or parent matching component to the query
     ;; and remove the classname from the tag
     (class-method-pair
        (let ((classname (car class-method-pair)))
          (setq tag (cdr class-method-pair))
          (setq classname-query (format
                                 "(CLASSNAME = '%s'; children('%s', CLASSNAME)),"
                                 classname classname)
                ;; we want X to be all terminals for a method/property in a class
                ;; that includes mixins
                ;; (X=(CLASSNAME,'gen');(mixins(CLASSNAME,MIXIN),X=(MIXIN,'gen')))
                ;;(format "%s=(CLASSNAME,'%s')" var tag)
                var-query
                ;; (trying to make this readable)
                (concat "("
                          (pfff-prologify-method-match var tag)  ;; match method/property
                          ";"                                    ;; OR
                          "("                                    ;; for all mixins
                            "mixins(CLASSNAME,MIXIN),"           ;; of matched classes match
                            (format "%s=(MIXIN,'%s')" var tag)   ;; mixin method/property
                          ")"
                        ")"
                ))
          ))
     ;; DEFAULT: we don't know anything about the tag, try to match
     ;; as broadly as possible
     (t
      (setq var-query (format "(%s='%s';%s=(CLASSNAME,'%s'))" var tag var tag))
      ))
    ;; if we have a class, simplify the

    ;; match any function or method by this name
    ;; if classname-query is bound it will restrict to just methods
    (concat
     classname-query
     var-query
     )))

;;; Specialiazed queries shortcuts
;;;###autoload
(defun pfff-prolog-find-tag (tag)
  "find tag similar to other find tag commands. Note that this
supports Foo::Bar syntax if you want to find a specific member
function, attribute, etc."
  (interactive (list (read-string "The string to search for:"
                                  (pfff-prolog-read-word-or-region))))
  (let*
      ((query (pfff-prologify-tag tag))
       (class-method-pair (pfff-prolog-split-class-and-method tag))
       (class (if class-method-pair (car class-method-pair) nil))
       (method (if class-method-pair (cdr class-method-pair) tag)))
    (pfff-prolog-send-cmd (format "%s, at(X,FILE,LINE)" query))))

;;;###autoload
(defun pfff-prolog-find-callers (tag)
  "find class::members and functions that call a specific method
This will also make an educated guess about dynamic yield methods and will try to find yieldFoo if genFoo is called"
  (interactive (list (read-string "The method/function/class to search for:"
                                  (pfff-prolog-read-word-or-region))))
  (let ((tags))
    ;; strip prefix
    (if (string-match "\\(?:yield\\|gen\\|get\\|prep\\)\\(.*\\)" tag)
        (setq tag (match-string 1 tag)))
    ;; build permutations of valid function name:
    ;; "(Y=genDefaultEnts; Y=getDefaultEnts; Y=prepDefaultEnts)"
    (setq tags
          (concat
           "("
           (reduce
            (lambda (a b) (concat a "; " b))
            (loop with acc for i in '("gen" "get" "prep")
                  collect (format "Y=%s" (concat i tag))))
           ")"))
  (pfff-prolog-send-cmd
   (format "%s, docall(X, Y, _), at(X,FILE,LINE)" tags)
   'pfff-prolog-result-context-method-formatter)))

;;;###autoload
(defun pfff-prolog-find-classes-with-method (method)
  "find classes, traits, and interfaces with a given method"
  (interactive (list (read-string "The method/function to find the classes for:"
                                  (pfff-prolog-read-word-or-region))))
  (pfff-prolog-send-cmd
   (format "at((X,'%s'),FILE,LINE)" method)
   'pfff-prolog-result-context-method-formatter))

;;;###autoload
(defun pfff-prolog-find-extendors-of-class (class)
  (interactive (list (read-string "class to get extendors of:"
                                  (pfff-prolog-read-word-or-region))))
  (pfff-prolog-send-cmd
   (format "extends_or_implements(X, '%s'), at(X,FILE,LINE)" class)))

;;;###autoload
(defun pfff-prolog-find-all-extendors-of-class (class)
  "Gives a complete list of children or grandchildren of a class"
  (interactive (list (read-string "class to get extendors of:"
                                  (pfff-prolog-read-word-or-region))))
  (pfff-prolog-send-cmd
   (format "children(X, '%s'), at(X,FILE,LINE)" class)))

;;;###autoload
(defun pfff-prolog-find-users-of-trait (trait)
  (interactive (list (read-string "trait to get users of:"
                                  (pfff-prolog-read-word-or-region))))
  (pfff-prolog-send-cmd
   (format "mixins(X, '%s'), at(X,FILE,LINE)" trait)))

;;;###autoload
(defun pfff-prolog-find-trait-that-implements-method (class-and-method)
  "find a trait that implements a given method. useful for
matching an interface with a trait for a given class. argument
can be fully qualified 'Foo::bar', which will result in the trait
for a specific class being matched, or for a method in general
'baz', in which case all traits that implement a method of this
name will be matched"
  (interactive (list (read-string "find trait that implements method in a given class (Foo::fooBar):"
                                  (pfff-prolog-read-word-or-region))))
  (let*
      ((class-and-method-pair (pfff-prolog-split-class-and-method class-and-method))
       (class (if class-and-method-pair (format "extends_or_mixins('%s', X)," (car class-and-method-pair)) ""))
       (method (if class-and-method-pair (cdr class-and-method-pair) class-and-method)))
    (pfff-prolog-send-cmd
     (format "kind((X, '%s'), method), kind(X, trait), %s at(X,FILE,LINE)" method class))))

;;;###autoload
(defun pfff-prolog-find-where-class-method-is-implemented (classname method)
  (interactive
   (let
       (class-and-type inferred-classname inferred-method)
     (if mark-active
         (setq class-and-type (pfff-prolog-split-class-and-method
                               (buffer-substring (region-beginning) (region-end)))))
     ;; figure out what to show user
     (if class-and-type
         (setq inferred-classname (car class-and-type)
               inferred-method    (cdr class-and-type))
       (setq inferred-classname (xhp-semantic-get-class-for-point)
             inferred-method    (pfff-prolog-read-word-or-region)))
     (list
      (read-string "name of class: " inferred-classname)
      (read-string "name of method: " inferred-method)))
   )
  (pfff-prolog-send-cmd
   (concat
    ;; because this is prolog, this means get parents of 'classname'
    (format "children('%s', X), " classname)
    "(at((X,'gen'),FILE,LINE);(mixins(X,Y),at((Y,'gen'),FILE,LINE)))")
           ))

(defun pfff-prolog-input-finder (i)
  (let ((s (comint-previous-input-string i)))
    (cond
     ((> (length s) 5) s)
     ((> i 5) (error "no input to send found"))
     (t (pfff-prolog-input-finder (1+ i))))))

(defun pfff-prolog-run-last-shell-cmd-as-find ()
  "helper for grabbing the last shell query and running it as a
'find' op so you can get the grep-style output and easily
navigate to results. NOTE: this assumes that X is the variable to
display, and FILE and LINE are variables that match the file and
line number.

For example:
- enter this command in the shell (Note the use of X, FILE, LINE):
?- kind(('Ent', X), method), at(('Ent',X), FILE, LINE).
X = load,
FILE = flib/entity/core/Ent.php,
LINE = 101 ...

- send this command to the results buffer:
M-x pfff-prolog-run-last-shell-cmd-as-find
"
  (interactive)
  (let*
      ((input
        (with-current-buffer "*pfff-prolog-shell*"
          (pfff-prolog-input-finder 0))))
    ;; strip off any output formatting, (assuming it comes last and
    ;; everything after it is unnecessary
    (if (string-match "\\(.*\\)\\(\\.\\|, *writeln\\)" input)
        (setq input (match-string 1 input)))
    (message "sending input %s" input)
    (pfff-prolog-send-cmd input)))

(defun pfff-prolog-shell-insert-procedure (c)
  "Helper for building prolog queries, useful when you're coming
up to speed. database_code.pl should be consulted as well for the
fancier procedures"
  (interactive "c(c)alls (e)xtends (i)mplements mi(x)ins (a)t (m)ethod")
  (let
      ((insert-helper (lambda (pre post) (insert pre) (save-excursion (insert post)))))
    (case c
      (?a (funcall insert-helper "at((X, '" "'), FILE, LINE),"))
      (?c (funcall insert-helper "docall(X, '" "', _),"))
      (?e (funcall insert-helper "extends_or_implements(X, '" "'),"))
      (?i (funcall insert-helper "implements(X, '" "'),"))
      (?m (funcall insert-helper "kind((X, '" "'), method),"))
      (?x (funcall insert-helper "mixins(X, '" "'),"))
    )))

(progn
  (defun pfff-prolog-shell-insert-proc-abbrev ()
    (call-interactively 'pfff-prolog-shell-insert-proc)
    t) ;; nil means no space
  (put 'pfff-prolog-shell-insert-proc-abbrev 'no-self-insert t))

;;;###autoload
(define-derived-mode pfff-prolog-shell-mode comint-mode "pfff-prolog-shell-mode"
  "mode for the prolog shell"
  (define-abbrev-table 'pfff-prolog-shell-mode-abbrev-table nil)
  (let
      ((abbrevs (list (cons "aproc" 'pfff-prolog-shell-insert-proc-abbrev))))
    (loop for i in abbrevs do
          (define-abbrev pfff-prolog-shell-mode-abbrev-table (car i) "" (cdr i))))
  (abbrev-mode t))

;;;###autoload
(defun pfff-prolog-shell ()
  (interactive)
  (with-current-buffer (get-buffer-create "*pfff-prolog-shell*")
    (setq comint-prompt-regexp pfff-prolog-prompt-regexp)
    (setq comint-input-ignoredups t)
    (let (proc)
      (make-comint "pfff-prolog-shell" "cmf" nil "--prolog")
      )
    (switch-to-buffer (current-buffer))
    (pfff-prolog-shell-mode)
    ))


;; purely for helping me with development, ignore this
;; https://our.intern.facebook.com/intern/wiki/index.php/Pfff/PrologWww#Building_the_database_yourself
;; $ ulimit -s 50000 # need a bigger stack when running ocaml programs
;; $ cp -a /home/engshare/pfff/PHP_STDLIB /path/to/www #to also index builtins
;; $ cd pfff/
;; $ ./pfff_db_heavy -metapath /tmp/mydb /path/to/www
;; $ ./pfff_db_heavy  -gen_prolog_db /tmp/mydb/ /tmp/facts.pl
;; $ ~pad/packages/Linux/bin/swipl -c /tmp/facts.pl h_program-lang/database_code.pl
;; $ mv a.out my_own_prolog_db

(declare-function ocamldebug "ocaml")

(defun ocamldebug-pfff ()
  (ocamldebug "/home/abrady/pfff/facebook/check_module/checkModule" "--indent_php" "/home/abrady/nobackup/bar.php"))


(if pfff-prolog-define-aliases
    (progn
      (defalias 'pft 'pfff-prolog-find-tag)
      (defalias 'pfe 'pfff-prolog-find-extendors-of-class)
      (defalias 'pfc 'pfff-prolog-find-callers)))

(provide 'pfff-prolog)
