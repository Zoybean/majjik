;;; -*- lexical-binding: t -*-

;; Require

;; [[file:majjik.org::*Require][Require:1]]
(require 'dash)
(require 's)
(require 'eieio)
;; Require:1 ends here

;; macros

;; [[file:majjik.org::*macros][macros:1]]
(defmacro with-error-context (formatter-fn &rest body)
  "If BODY signals an error condition, modify its message by calling FORMATTER-FN on the original message. FORMATTER-FN should take and return a string."
  (declare (indent 1))
  `(handler-bind ((error (lambda (err)
                           (cl-callf ,formatter-fn
                               (cadr err)))))
     (progn ,@body)))

(defmacro with-error-format (format-str &rest body)
  "If BODY signals an error condition, reformat the original message by calling `format' with FORMAT-STR and the original message."
  (declare (indent 1))
  `(with-error-context (lambda (msg) (format ,format-str msg))
     ,@body))
;; macros:1 ends here

;; argument utils

;; [[file:majjik.org::*argument utils][argument utils:1]]
(defun jj--quote-argument (name)
  "Return NAME in double-quotes, with nested double-quotes and backslashes escaped."
  (format "\"%s\""
          (replace-regexp-in-string
           ;; I think the only special characters for jj
           ;; that function in inner double-quotes
           ;; are the quote and the escape itself
           (rx (any ?\" ?\\))
           "\\\\\\&" name)))

(defun jj--toml-quote-string (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (cl-loop while (re-search-forward (rx (any control)) nil :noerr)
             for old = (aref (match-string 0) 0)
             do (replace-match (format "\\x%02x" old) nil :literal))
    (buffer-string)))

(defun jj--if-arg (arg formatter &optional flag)
  "Helper for building command-line arguments.

If ARG is nil, returns nil. Otherwise returns the list containing FLAG (if provided) and the result of calling FORMATTER on argument ARG. If formatter is nil or returns nil, the ARG is omitted from the return value."
  (and arg
       `(,@(opt flag)
         ,@(opt (and formatter
                     (funcall formatter arg))))))

(ert-deftest jj--test-if-arg ()
  (should (equal `("--" "foo-bar")
                 (jj--if-arg "foo"
                             (lambda (x)
                               (format "%s-bar" x))
                             "--")))
  (should (equal `()
                 (jj--if-arg nil
                             (lambda (x)
                               (error "should not be called for nil arg"))
                             "--")))
  (should (equal `("foo-bar")
                 (jj--if-arg "foo"
                             (lambda (x)
                               (format "%s-bar" x)))))
  (should (equal `("--no-args-flag")
                 (jj--if-arg t
                             #'ignore
                             "--no-args-flag")))
  (should (equal `("--no-args-flag")
                 (jj--if-arg t
                             nil
                             "--no-args-flag"))))
;; argument utils:1 ends here

;; process utils

;; [[file:majjik.org::*process utils][process utils:1]]
(defun jj--sticky-insert (marker insert-fn sticky-top)
  "Move to MARKER, call INSERT-FN, update the marker, and move point if it's at the marker. If STICKY-TOP and point is at the beginning of the buffer, it will not move even if it is at the marker."
  (let ((moving (and (= (point) marker)
                     (not (and sticky-top (bobp))))))
    (save-excursion
      (goto-char marker)
      (let ((inhibit-read-only t))
        (funcall insert-fn))
      (set-marker marker (point)))
    (when moving (goto-char marker))))

(defun make-sticky-process-filter (&optional initially-stay)
  "Make a process filter that outputs to buffer without moving point.
If INITIALLY-STAY is non-nil, point stays in place if it is at `bobp' even if this is also the position of the `process-mark'"
  (lambda (proc string)
    (:documentation (format "Process filter that outputs to buffer without moving point. If point %s, it follows the insertion. Otherwise it stays in place."
                            (if initially-stay
                                "is at the position that text is inserted, and is not at the beginning of the buffer"
                              "is at the position that text is inserted")))
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (jj--sticky-insert (process-mark proc)
                           (lambda () (insert string))
                           initially-stay)))))

(defun jj--handle-proc-end (target-buf event error-buffer &rest other-buffers)
  "Call from a process sentinel on process exit to manage error message, status message, and buffer disposal."
  (unwind-protect
      (when (buffer-live-p target-buf)
        ;; exited abnormally
        (unless (string= event "finished\n")
          (with-current-buffer target-buf
            (goto-char (point-max))
            ;; add error status to output buffer
            (let ((inhibit-read-only t))
              (unless (bolp)
                (insert "\n"))
              (insert "\n")
              (insert (propertize event 'face '(:foreground "red")))
              (insert "\n")
              (insert-buffer-substring error-buffer)))))
    ;; always clean up the auxiliary buffers
    (kill-buffer error-buffer)
    (mapcar #'kill-buffer other-buffers)))

(defun make-jj-simple-sentinel (error-buffer &rest other-buffers)
  "Make a simple process sentinel, to insert ERROR-BUFFER's contents if the process ends unexpectedly, then kill it and all OTHER-BUFFERS."
  (apply #'make-jj-callback-sentinel nil error-buffer other-buffers))

(defun make-jj-callback-sentinel (end-callback error-buffer &rest other-buffers)
  "Make a simple process sentinel, to insert ERROR-BUFFER's contents if the process ends unexpectedly, then kill it and all OTHER-BUFFERS, and finally call END-CALLBACK with a success indicator (t if ok, nil if error)."
  (lambda (proc event)
    (:documentation (format "Process sentinel that kills its auxiliary buffers when the process ends. If the process ends unsuccessfully, inserts the error buffer's content.%s" (if end-callback (format " Then finally calls %s" end-callback) "")))
    (unless (process-live-p proc)
      (apply #'jj--handle-proc-end
             (process-buffer proc)
             event
             error-buffer
             other-buffers)
      (when end-callback
        (funcall end-callback (string= event "finished\n"))))))

(defun make-jj-generic-buffered-filter (intermediate-buffer read-next callback)
  "Make a process filter for an arbitrary process.
Every time there is new output, the filter adds it to the INTERMEDIATE-BUFFER, then calls READ-NEXT from the buffer beginning until it returns nil, then calls CALLBACK with the list of new non-nil values, and deletes the text before point (i.e. the text that was read).

READ-NEXT should be a function that reads forward from `point', and moves `point' past whatever has been read.
CALLBACK should be a function of one argument - the list of non-nil values returned by READ-NEXT."
  (lambda (proc string)
    (:documentation (format "This filter adds output to its intermediate buffer, then calls `%s' until it returns nil, then calls `%s' with the list of new non-nil values, and deletes the text before point (i.e. the text that was read)." read-next callback))
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer intermediate-buffer
        (insert string)
        ;; process any new entries
        (save-excursion
          (goto-char (point-min))
          ;; check if we have any new full entries in the buffer
          (when-let ((news (collect-repeat (funcall read-next))))
            ;; delete them and send the list to the callback
            (funcall callback news)
            (delete-region (point-min) (point))))))))
;; process utils:1 ends here

;; log utils

;; [[file:majjik.org::*log utils][log utils:1]]
(defun jj-partition-runs (list &optional test)
  "Split LIST into segments where either all elements are equal, or no consecutive elements are equal, using function TEST to compare for equality, or `eq' if not provided."
  (let ((parts)
        (running)
        (test (or test #'eq)))
    (cl-labels ((run (x)
                  ;; start or add to a run
                  (cond ((and parts
                              ;; check that it's *the same run*, and not a distinct but consecutive run
                              (funcall test (caar parts) x)
                              running)
                         ;; add to the last part
                         (push x (car parts))
                         )
                        (t
                         ;; make a new part
                         (push (list x)
                               parts)
                         (setq running t))))
                (end (x)
                  ;; start or add to a non-run
                  (cond ((and parts
                              (not running))
                         ;; add to the last part
                         (push x (car parts)))
                        (t
                         ;; make a new part
                         (push (list x)
                               parts)
                         (setq running nil)))))
      (cl-loop for place on list
               for first = t then nil
               do (pcase-exhaustive place
                    (`(,_ . nil))
                    ((and `(,a ,b . ,_)
                          (guard (funcall test a b)))
                     (when first
                       (run a))
                     (run b))
                    ((and `(,a ,b ,c . ,_)
                          (guard (funcall test b c)))
                     (when first
                       (end a))
                     (run b))
                    (`(,a ,b . ,_)
                     (when first
                       (end a))
                     (end b)))
               finally return (nreverse (mapcar #'nreverse parts))))))

(ert-deftest jj-test-partition-runs ()
  "Check that `jj-partition-runs' gives expected results for all 4-tuples with `eq'."
  (cl-loop for (set . exp) in '(((a b c d) (a b c d))
                                ((a a b c) (a a) (b c))
                                ((a b b c) (a) (b b) (c))
                                ((a b c c) (a b) (c c))
                                ((a a b b) (a a) (b b))
                                ((a a a b) (a a a) (b))
                                ((a b b b) (a) (b b b)))
           collect (should (equal exp
                                  (jj-partition-runs set)))))
(ert-deftest jj-test-partition-runs-strings ()
  "Check that `jj-partition-runs' gives expected results for all 4-tuples with a test function."
  (cl-loop for (set . exp) in '((("a" "b" "c" "d") ("a" "b" "c" "d"))
                                (("a" "a" "b" "c") ("a" "a") ("b" "c"))
                                (("a" "b" "b" "c") ("a") ("b" "b") ("c"))
                                (("a" "b" "c" "c") ("a" "b") ("c" "c"))
                                (("a" "a" "b" "b") ("a" "a") ("b" "b"))
                                (("a" "a" "a" "b") ("a" "a" "a") ("b"))
                                (("a" "b" "b" "b") ("a") ("b" "b" "b")))
           collect (should (equal exp
                                  (jj-partition-runs set #'string=)))))
;; log utils:1 ends here

;; regex and reader utils

;; [[file:majjik.org::*regex and reader utils][regex and reader utils:1]]
(defun jj--re-step-over (regexp &optional bound no-error)
  "Like `re-search-forward' but the match-start must be point. Like `looking-at' but moves point."
  (if-let* ((matched (looking-at regexp))
            (end (match-end 0))
            (ok (or (not bound)
                    (<= end bound))))
      (prog1 ok
        (goto-char end))
    (unless no-error
      (error "anchored search failed: %s" regexp))))

(defun jj--forgiving-read (reader)
  "Call READER, and ignore errors. If it fails, reset point."
  (let ((marker))
    (prog1
        (save-excursion
          (condition-case e
              (funcall reader)
            (error ())
            ;; set the marker because we succeeded.
            ;; we need a marker to save the position for after the save-excursion.
            (:success
             ;; return the original result
             (prog1 e
               (setq marker (point-marker))))))
      (when marker
        (goto-char marker)))))

(defun jj--try-read-each (&rest readers)
  "Try each of READERS until one returns non-nil. Each call to a reader is wrapped in `jj--forgiving-read', resetting point on failure."
  (cl-loop for reader in readers
           for val = (jj--forgiving-read reader)
           when val return val
           finally return nil))
;; regex and reader utils:1 ends here

;; json utils

;; [[file:majjik.org::*json utils][json utils:1]]
(defun jj--parse-json-entire-string (string)
  "Parse a json object that is the entirety of STRING (besides whitespace)."
  (with-temp-buffer
    (save-excursion (insert (string-trim string)))
    (prog1 (json-read)
      (unless (eobp)
        (error "string did not wholly encode a single json object: %s" string)))))
;; json utils:1 ends here

;; collect-repeat

;; [[file:majjik.org::*collect-repeat][collect-repeat:1]]
(defmacro collect-repeat (&rest body)
  "Call FN repeatedly until it returns nil. Return the list of non-nil values."
  (let ((vals-sym (gensym "vals")))
    `(let ((,vals-sym))
       (while-let ((val (progn ,@body)))
         (push val ,vals-sym))
       (nreverse ,vals-sym))))

(ert-deftest jj-test-collect-repeat ()
  (should (eq nil
              (collect-repeat nil)))
  (should (equal '(2 1 0)
                 (let ((x 3))
                   (collect-repeat (when (< 0 x)
                                     (cl-decf x)))))))
;; collect-repeat:1 ends here

;; Fileset

;; [[file:majjik.org::*Fileset][Fileset:1]]
(defun jj-files-as-fileset (&rest files)
  "Returns a fileset that matches all the given FILES. No globbing is applied, and all are considered relative to cwd."
  (jj-fileset `(or ,@(cl-loop for file in files
                              collect `(:cwd-file ,file)))))

(defmacro jj-compile-fileset (sexp)
  "Compile a fileset SEXP into a string suitable for use as a jj fileset argument. See `jj-fileset'"
  (jj-fileset sexp))

(defun jj-fileset (sexp)
  "Convert a fileset SEXP into a string suitable for use as a jj fileset argument.
Operators and patterns are called as lisp functions.
Operators are `and', `or' and `not'.
Patterns all have keyword names, and all keywords are assumed to be patterns.
See URL `https://docs.jj-vcs.dev/latest/filesets/' for more info."
  (cl-labels ((render-unpack (sexp)
                (pcase (render sexp)
                  ((or `(,(and needlessly-nested form))
                       form)
                   (format "%s" form))
                  (unrecognised
                   (error "inner fileset render call produced invalid result: %S" unrecognised))))
              (render-str (sexp)
                (format "%s" (render sexp)))
              (render (sexp)
                (pcase sexp
                  ;; file patterns: bare path
                  ((and path (pred stringp))
                   (jj--quote-argument path))
                  ;; file patterns: flag and path/glob
                  (`(,(and flag
                           (pred keywordp))
                     ,(and arg (pred stringp)))
                   (format "%s:%s"
                           (substring (symbol-name flag) 1)
                           (jj--quote-argument arg)))
                  ;; operators: or, none (empty sum)
                  ((or `(or)
                       `(none))
                   "none()")
                  (`(or . ,forms)
                   ;; trick to only nest in quotes if an outer form requires it
                   ;; outermost caller will unwrap any toplevel list
                   (list (mapconcat #'render-str forms "|")))
                  ;; operators: not
                  (`(not ,form)
                   (format "~%s"
                           (render form)))
                  ;; operators: and, all (empty product)
                  ((or `(and)
                       `(all))
                   "all()")
                  ;; both shorthands for "A and not B"
                  ((or `(and ,form (not ,form-neg))
                       `(and (not ,form-neg) ,form))
                   ;; trick to only nest in quotes if an outer form requires it
                   ;; outermost caller will unwrap any toplevel list
                   (list (format "%s~%s"
                                 (render form)
                                 (render form-neg))))
                  (`(and . ,forms)
                   ;; trick to only nest in quotes if an outer form requires it
                   ;; outermost caller will unwrap any toplevel list
                   (list (mapconcat #'render-str forms "&")))
                  (unknown (user-error "Unknown fileset operator form: %S" unknown)))))
    (render-unpack sexp)))

(ert-deftest jj--test-render-fileset ()
  (should (string=
           ;; using format here to do the extra quoting for me.
           ;; the argument to format is what you'd type at the console.
           "\"foo\""
           (jj-fileset "foo")))

  ;; optimisation: top-level parens are elided
  (should (string=
           "glob:\"foo/**\"|\"bar\""
           (jj-fileset '(or (:glob "foo/**") "bar"))))
  (should (string=
           "~glob:\"foo/**\"|\"bar\""
           (jj-fileset '(or (not (:glob "foo/**")) "bar"))))

  ;; optimisations: (A) and (not B) => (A)~(B)
  ;; optimisations: (not B) and (A) => (A)~(B)
  (should (string=
           "\"bar\"~glob:\"foo/**\""
           (jj-fileset '(and "bar" (not (:glob "foo/**"))))))
  (should (string=
           "\"bar\"~glob:\"foo/**\""
           (jj-fileset '(and (not (:glob "foo/**")) "bar"))))

  ;; metacharacters are quoted
  (should (string=
           "\"~/path/under/home\""
           (jj-fileset "~/path/under/home")))
  (should (string=
           "\"'single-quoted-name'\""
           (jj-fileset "'single-quoted-name'"))))
;; Fileset:1 ends here

;; Revset

;; [[file:majjik.org::*Revset][Revset:1]]
(defun jj-revs-as-revset (&rest revs)
  "Returns a revset that matches all the given REVS. No globbing is applied, and all are considered relative to cwd."
  (jj-revset `(or ,@revs)))

(defmacro jj-compile-revset (sexp)
  "Compile a revset SEXP into a string suitable for use as a jj revset argument. See `jj-revset'"
  (jj-revset sexp))

(defun jj-revset (sexp)
  "Convert a revset SEXP into a string suitable for use as a jj revset argument.
Operators, functions, and patterns are called as lisp functions.
Operators are `and', `or', `not', `-', `+', `::', `..'. One-sided and no-sided versions of `::' and `..' are achieved with the placeholder symbol `_'.
Patterns all have keyword names, and `::' is the only keyword not assumed to be a pattern.
Any unrecognised names are assumed to be functions and aliases.
See URL `https://docs.jj-vcs.dev/latest/revsets/' for more info."
  (let ((infix '(:: ..))
        (suffix '(- +))
        (prefix '(~)))
  (cl-labels ((render-unpack (sexp)
                (pcase (render sexp)
                  ((or `(,(and needlessly-nested form))
                       form)
                   (format "%s" form))
                  (unrecognised
                   (error "inner revset render call produced invalid result: %S" unrecognised))))
              (render-str (sexp)
                (format "%s" (render sexp)))
              (render (sexp)
                (pcase sexp
                    ;; operators: infix
                    (`(,(and op (guard (memq op infix))) . ,rest)
                     (list (pcase rest
                             ((or `(_ _) `(_) `())
                              (format "%s"
                                      op))
                             ((or `(,l _) `(,l))
                              (format "%s%s"
                                      (render l)
                                      op))
                             (`(_ ,r)
                              (format "%s%s"
                                      op
                                      (render r)))
                             (`(,l ,r)
                              (format "%s%s%s"
                                      (render l)
                                      op
                                      (render r)))
                             (unrecognised (error "invalid operands to %s: %S" op unrecognised)))))
                    ;; operators: suffix
                    (`(,(and op (guard (memq op suffix))) ,arg)
                     (format "%s%s" arg op))
                    ;; operators: prefix
                    (`(,(and op (guard (memq op prefix))) ,arg)
                     (format "%s%s" op arg))
                    ;; numbers
                  ((and num (pred numberp))
                   num)
                  ;; string patterns: bare path
                  ((and path (pred stringp))
                   (jj--quote-argument path))
                  ;; string patterns: flag and path/glob
                  (`(,(and flag
                           (pred keywordp))
                     ,(and arg (pred stringp)))
                   (format "%s:%s"
                           (substring (symbol-name flag) 1)
                           (jj--quote-argument arg)))
                    ;; operator aliases
                    (`(parents ,form)
                     (render `(- ,form)))
                    (`(children ,form)
                     (render `(+ ,form)))
                    (`(descendants ,form)
                     (render `(:: ,form _)))
                  ((or `(non-ancestors ,form)
                         `(not (ancestors ,form)))
                     (render `(.. ,form _)))
                    (`(ancestors ,form)
                     (render `(:: _ ,form)))
                    ;; operators: singleton and/or (identity)
                    (`(,(or 'or 'and) ,form)
                     (render form))
                    ;; operators: empty or (empty sum = none)
                  ((or `(or)
                       `(none))
                   "none()")
                  (`(or . ,forms)
                   ;; trick to only nest in quotes if an outer form requires it
                   ;; outermost caller will unwrap any toplevel list
                   (list (mapconcat #'render-str forms "|")))
                  ;; operators: not
                  (`(not ,form)
                   (format "~%s"
                           (render form)))
                    ;; operators: empty and (empty product = all)
                  ((or `(and)
                       `(all))
                   "all()")
                  ;; both shorthands for "A and not B"
                  ((or `(and ,form (not ,form-neg))
                       `(and (not ,form-neg) ,form))
                   (list (format "%s~%s"
                                 (render form)
                                 (render form-neg))))
                  (`(and . ,forms)
                   ;; trick to only nest in quotes if an outer form requires it
                   ;; outermost caller will unwrap any toplevel list
                   (list (mapconcat #'render-str forms "&")))
                  ;; functions
                  (`(,(and fun (pred symbolp)) . ,args)
                   (format "%s(%s)" fun (mapconcat #'render-unpack args ",")))
                  ;; assume anything else is an alias
                  (alias alias)
                  ;; this will never occur with the alias catch-all
                  ;; still leaving in in-case i remove the catch-all
                  (unknown (user-error "Unknown revset operator form: %S" unknown)))))
      (render-unpack sexp))))

(ert-deftest jj--test-render-revset ()
  (should (string=
           "\"foo\""
           (jj-revset "foo")))

  ;; optimisation: top-level parens are elided
  (should (string=
           "glob:\"foo*\"|\"bar\""
           (jj-revset '(or (:glob "foo*") "bar"))))
  (should (string=
           "~glob:\"foo*\"|\"bar\""
           (jj-revset '(or (not (:glob "foo*")) "bar"))))

  ;; optimisations: (A) and (not B) => (A)~(B)
  ;; optimisations: (not B) and (A) => (A)~(B)
  (should (string=
           "\"bar\"~glob:\"foo*\""
           (jj-revset '(and "bar" (not (:glob "foo*"))))))
  (should (string=
           "\"bar\"~glob:\"foo*\""
           (jj-revset '(and (not (:glob "foo*")) "bar"))))

  ;; metacharacters are quoted
  (should (string=
           "\"~/path/under/home\""
           (jj-revset "~/path/under/home")))
  (should (string=
           "\"'single-quoted-name'\""
           (jj-revset "'single-quoted-name'")))
  ;; [revset-aliases]
  ;; 'default_immutable_heads()'
  (should (string=
           "trunk()|tags()|untracked_remote_bookmarks()"
           (jj-revset '(or (trunk) (tags) (untracked_remote_bookmarks)))))

  ;; 'immutable_heads()'
  (should (string=
           "default_immutable_heads()|(working_copies()~@)"
           (jj-revset '(or (default_immutable_heads) (and (working_copies) (not @))))))

  ;; 'local()'
  (should (string=
           "~::remote_bookmarks()"
           (jj-revset '(:: ~ (remote_bookmarks)))))

  ;; 'limb()'
  ;; this could be optimised if the function knew about operator binding strength
  (should (string=
           "((::@)&local())::"
           (jj-revset '(descendants (and (ancestors @) (local))))))
  (should (string=
           "((::@)&local())::"
           (jj-revset '(:: (and (:: _ @) (local)) _))))

  ;; 'graft()'
  (should (string=
           "ancestors(limb(),2)"
           (jj-revset '(ancestors (limb) 2))))

  ;; 'relevant()'
  (should (string=
           "ancestors(local()|mutable(),2)|trunk()"
           (jj-revset '(or (ancestors (or (local) (mutable)) 2) (trunk)))))

  ;; 'leaves()'
  (should (string=
           "heads(mutable()|@)"
           (jj-revset '(heads (or (mutable) @))))))
;; Revset:1 ends here

;; Template

;; [[file:majjik.org::*Template][Template:1]]
(defmacro jj-compile-template (sexp)
  "Compile a template SEXP into a string suitable for use as a jj template argument. See `jj-template'"
  (jj-template sexp))

(defun jj-template (sexp)
  "Convert a template SEXP into a string suitable for use as a jj template argument.
Operators, functions, methods, and patterns are called as lisp functions.
Methods' names all begins with a `.'.
Special form (:chain target (.method args...)...) can be used to chain multiple methods oo-style.
Special form (:get target field) can be used to get a field (if jj ever adds that concept) of an object.
Operators are `and' `or' `-' `!' `+' `*' `/' `%' `>=' `>' `<=' `<' `==' `!=' `&&' `||' `++'.
Patterns all have keyword names, and `::' is the only keyword not assumed to be a pattern.
Any unrecognised names are assumed to be functions and aliases.
See URL `https://docs.jj-vcs.dev/latest/templates/' for more info."
  (let ((infix-2 '(< <= > >= == !=))
        (infix-2+ '(* / % + - || && ++))
        (prefix '(- !)))
    (cl-labels ((render-unpack (sexp)
                  (pcase (render sexp)
                    ((or `(,(and needlessly-nested form))
                         form)
                     (format "%s" form))
                    (unrecognised
                     (error "inner template render call produced invalid result: %S" unrecognised))))
                (render-str (sexp)
                  (format "%s" (render sexp)))
                (render (sexp)
                  (pcase sexp
                    ;; special method call syntax. I'm basically implementing a macro here
                    (`(:chain ,(and target (pred symbolp)) . ,calls)
                     (render (cl-loop for call in calls
                                      for self = target then call-form
                                      for call-form = (pcase call
                                                        ;; method calls
                                                        (`(,(and
                                                             method
                                                             (pred symbolp)
                                                             (guard (string-prefix-p "." (symbol-name method))))
                                                           . ,(and args (pred listp)))
                                                         `(,method ,self . ,args))
                                                        ;; chained function calls
                                                        (`(,(and
                                                             function
                                                             (pred symbolp))
                                                           . ,(and args (pred listp)))
                                                         `(,function ,self . ,args))
                                                        ;; chained field access
                                                        ((and field (pred symbolp) (guard (string-prefix-p "." (symbol-name field))))
                                                         `(:get ,self ,field)))
                                      finally return call-form)))
                    ;; special field access syntax
                    (`(:get ,(and target (pred symbolp))
                            ,(and field (pred symbolp) (guard (string-prefix-p "." (symbol-name field)))))
                     (format "%s%s" target field))
                    ;; special syntax to embed a string template.
                    ;; vulnerable to bobby tables attacks - make sure it's a well-formed template before use
                    ;; nonetheless, wrapped in parens for some safety.
                    (`(:lit ,(and str (pred stringp)))
                     (list str))
                    ;; string literals
                    ((and str (pred stringp))
                     (jj--quote-argument str))
                    ;; number literals
                    ((and num (pred numberp))
                     num)
                    ;; lambdas
                    (`(lambda ,(and args (pred listp)) ,body-exp)
                     (list (format "|%s|%s"
                                   (mapconcat #'symbol-name args ",")
                                   (render-unpack body-exp))))
                    ;; operators: prefix
                    (`(,(and op (pred symbolp)
                             (guard (memq op prefix)))
                       ,arg)
                     (list (format "%s%s" op arg)))
                    ;; operators: infix-2
                    (`(,(and op (pred symbolp)
                             (guard (memq op infix-2)))
                       ,l ,r)
                     (list (format "%s%s%s"
                                   (render l)
                                   op
                                   (render r))))
                    ;; operators: infix-2+
                    (`(,(and op (pred symbolp)
                             (guard (memq op infix-2+)))
                       . ,(and args
                               (guard (<= 2 (length args)))))
                     (list (mapconcat #'render-str args (symbol-name op))))
                    (`(,(and op (pred symbolp)
                             (guard (memq op infix-2+)))
                       . ,args)
                     (user-error "operator %s requires at least 2 operands, got %d: %s" op (length args) args))
                    ;; operators: or, false (empty sum)
                    ((or `(or)
                         'nil
                         'false)
                     "false")
                    (`(or . ,forms)
                     ;; trick to only nest in quotes if an outer form requires it
                     ;; outermost caller will unwrap any toplevel list
                     (list (mapconcat #'render-str forms "||")))
                    ;; operators: not
                    (`(not ,form)
                     (render `(! ,form)))
                    ;; operators: and, true (empty product)
                    ((or `(and)
                         't
                         'true)
                     "true")
                    (`(and . ,forms)
                     (list (mapconcat #'render-str forms "&&")))
                    (`(,(and method
                             (pred symbolp)
                             (guard (string-prefix-p "." (symbol-name method))))
                       ,self . ,args)
                     (format "%s%s(%s)"
                             (render self)
                             method
                             (mapconcat #'render-unpack args ",")))
                    (`(,(and function (pred symbolp)) . ,args)
                     (format "%s(%s)"
                             function
                             (mapconcat #'render-unpack args ",")))
                    ((and alias (pred symbolp)) alias)
                    (unknown (user-error "Unknown template operator form: %S" unknown)))))
      (render-unpack sexp))))

(ert-deftest jj--test-render-template ()
  ;; infix
  (should (string=
           "commit_id++\" \"++change_id++\"\n\""
           (jj-template '(++ commit_id " " change_id "\n"))))
  ;; lambda
  (should (string=
           "parents.map(|c|c.commit_id().short()).join(\",\")"
           (jj-template '(.join (.map parents
                                      (lambda (c)
                                        (.short (.commit_id c))))
                                ","))))
  ;; chain syntax
  (should (string=
           "parents.map(|c|c.commit_id().short()).join(\",\")"
           (jj-template '(:chain parents
                                 (.map 
                                  (lambda (c)
                                    (:chain c
                                            (.commit_id)
                                            (.short))))
                                 (.join ",")))))
  ;; chain syntax
  (should (string=
           "self.id().short()"
           (jj-template '(:chain self
                                 (.id)
                                 (.short)))))
  ;; chain non-method functions
  (should (string=
           "format_timestamp(self.committer().timestamp())"
           (jj-template '(:chain self (.committer) (.timestamp) (format_timestamp)))))

  ;; field syntax
  (should (string=
           "self.id.short()"
           (jj-template '(:chain self
                                 .id
                                 (.short)))))
  ;; function call
  (should (string=
           "coalesce(description,\"(no description set)\n\")"
           (jj-template '(coalesce description "(no description set)\n"))))

  ;; function call
  (should (string=
           "concat(format_field(\"Commit ID\",commit_id),format_field(\"Change ID\",change_id))"
           (jj-template '(concat (format_field "Commit ID" commit_id)
                                 (format_field "Change ID" change_id)))))
  (should (string=
           "\"ID: \"++self.id().short().substr(0,1)++label(\"id short\",\"<redacted>\")"
           (jj-template '(++ "ID: "
                             (:chain self (.id) (.short) (.substr 0 1))
                             (label "id short" "<redacted>")))))
  (should (string=
           "if(root,tidy_root_commit(self),label(if(current_working_copy,\"working_copy\"),separate(\" \",min_id,if(conflict,label(\"conflict\",\"conflict\")),content)++\"\n\"))"
           (jj-template '(if root
                             (tidy_root_commit self)
                           (label (if current_working_copy "working_copy")
                                  (++ (separate " "
                                                min_id
                                                (if conflict (label "conflict" "conflict"))
                                                content)
                                      "\n"))))))
  )
;; Template:1 ends here

;; Log format

;; [[file:majjik.org::*Log format][Log format:1]]
(defconst jj--count-graph-lines 4
  "Number of lines of graph to sample for each commit in the log output. Fewer lines will give scrappier output, but 4 should be enough for any graph configuration.
I've hardcoded other areas to expect exactly 4, so changing this will not break anything but the output will change slightly, specifically in which lines are considered mandatory.")
(defconst jj--major-delim "\x1E"
  "Delimiter for separating the graph from the records in the jj-log template.
  By default, this is the ascii record separator character.")
(defconst jj--delim "\x1F"
  "Delimiter for separating fields in the jj-log template.
  By default, this is the ascii unit separator character.")

(defmacro define-jj-log-format (&rest fields)
  "Define the format to be used for jj log parsing and formatting.
  Accepts a list of FIELDS in the form (NAME . PLIST), where PLIST accepts the following keys:
  - `:form' specifies the sexpression used to produce the field's log template, produced with `jj-template'. (So far there's no way to use a string template directly)
  - `:printer' specifies how to print the data out to a buffer. It should be a function of 2 arguments, with the first being the field and the second the entire structure.
  - `:face' specifies the face to use for formatting this entry in the log buffer. This is applied to the result of PRINTER if supplied.
  - `:separator' the separator to insert before this field, rather than a space (or empty for the first field). Only inserted if the value is present."
  `(progn
     (require 'json)
     (define-short-documentation-group jj-entry
       (define-jj-log-format
        :no-manual t)
       (read-jj-entry
        :no-manual t)
       (insert-jj-entry
        :no-manual t)
       (read-jj-elided
        :no-manual t)
       (insert-jj-elided
        :no-manual t)
       (make-jj-entry
        :args (&key header graph)
        :no-manual t)
       (make-jj-graph
        :no-manual t)
       (make-jj-header
        :args (&key ,@(mapcar #'car fields))
        :no-manual t))
     (defvar jj-entry-regex
       ,(let* ((content `(not (any ,jj--delim ,jj--major-delim "\r\n"))))
          `(rx line-start
               ;; graph pre
               (* ,content)
               ;; graph node and suf
               (= 2 
                  ,jj--delim
                  (* ,content))
               (seq
                ;; header delimiter
                ,jj--major-delim
                ;; header elements
                (* ,content)
                (= ,(1- (length fields))
                   ,jj--delim
                   (* ,content))
                "\n"
                ;; remaining graph
                (= ,(1- jj--count-graph-lines)
                   (* ,content)
                   ,jj--major-delim
                   "\n"))))
       "Regex to match a full `jj-log' entry. This *should* match exactly the same content that `read-jj-entry' will parse.")
     (defvar jj-elided-regex
       ,(let* ((content `(not (any ,jj--delim ,jj--major-delim "\r\n"))))
          `(rx line-start
               ;; graph pre
               (* ,content)
               ;; graph node and suf
               (= 2 
                  ,jj--delim
                  (* ,content))
               "(elided revisions)\n"
               ;; remaining graph
               (* (* ,content)
                  "\n")))
       "Regex to match an elided section of `jj-log'. This *should* match exactly the same content that `read-jj-elided' will parse.")
     (defvar jj-parseable-template
       (jj-template '(++ ,jj--major-delim
                         (join ,jj--delim
                               ,@(cl-loop for (name . props) in fields
                                          for form = (plist-get props :form)
                                          collect form))
                         ;; these lines only exist to get the shape of the graph
                         ;; there's already one with the header line, so count 1 fewer
                         ,@(cl-loop for n upfrom 1 below jj--count-graph-lines
                                    append `("\n" ,jj--major-delim))))
       "Commit template to produce log entries parseable by `read-jj-entry'.")
     (defun read-jj-entry ()
       "With point at the beginning of the first line of a `jj-log-parseable' entry, parse the entire entry into a `jj-entry' struct."
       ,(let ((content `(not (any ,jj--delim ,jj--major-delim "\r\n"))))
          `(cl-loop with (graph-pre graph-node graph-suf) = (progn
                                                              (should (bolp))
                                                              (should (jj--re-step-over (rx (group (* ,content))
                                                                                            ,jj--delim
                                                                                            (group (* ,content))
                                                                                            ,jj--delim
                                                                                            (group (* ,content))
                                                                                            ,jj--major-delim)))
                                                              (list (match-string 1)
                                                                    (match-string 2)
                                                                    (match-string 3)))
                    ;; entry line
                    for (key . quoted) in ',(cl-loop for (name . props) in fields
                                                     for key = (intern (format ":%s" name))
                                                     for quoted = (plist-get props :quoted)
                                                     collect `(,key . ,quoted))
                    ;; no delimiter for first field
                    for first = t then nil
                    for field-rx = (rx (group (* ,content))) then (rx ,jj--delim (group (* ,content)))
                    when (jj--re-step-over field-rx)
                    nconc `(,key ,(if quoted
                                      (save-match-data
                                         (funcall #'json-parse-string (match-string 1)))
                                    (match-string 1)))
                    into struct-props
                    finally return
                    (cl-loop initially (jj--re-step-over "\n")
                             for ix upfrom 1 below jj--count-graph-lines
                             ;; at this point, we ought to be at the start or end of a line,
                             ;; having just read either a commit or a graph-line
                             do (should (or (eolp)
                                            (bolp)))
                             if (jj--re-step-over (rx line-start
                                                      ;; since we started at eol
                                                      ;; since this starts with line-start,
                                                      ;; and matches everything up to a delimiter
                                                      ;; it's guaranteed to skip over only one character
                                                      ;; (the newline we started at)
                                                      (group (* ,content))
                                                      ,jj--major-delim
                                                      "\n"))
                             collect (match-string 1) into graph-tail
                             else return (error "failed to parse graph shape on line %d of commit %s" ix plist)
                             finally return (make-jj-entry :header (apply #'make-jj-header struct-props)
                                                           :graph (apply #'make-jj-graph graph-pre graph-node graph-suf graph-tail))))))
     (defun read-jj-elided ()
       "Read the graph portion of an elided section. This is just a `jj-graph' struct, but it's not read in the same."
       ,(let ((content `(not (any ,jj--delim ,jj--major-delim "\r\n"))))
          `(cl-loop with (graph-pre graph-node graph-suf) = (progn
                                                              (should (bolp))
                                                              (should (jj--re-step-over (rx (group (* ,content))
                                                                                            ,jj--delim
                                                                                            (group (* ,content))
                                                                                            ,jj--delim
                                                                                            (group (* ,content))
                                                                                            "(elided revisions)\n")))
                                                              (list (match-string 1)
                                                                    (match-string 2)
                                                                    (match-string 3)))
                    while (jj--re-step-over (rx line-start
                                                ;; since we started at eol
                                                ;; since this starts with line-start,
                                                ;; and matches everything up to a delimiter
                                                ;; it's guaranteed to skip over only one character
                                                ;; (the newline we started at)
                                                (group (* ,content))
                                                "\n")
                                            nil :noerr)
                    collect (match-string 1) into graph-tail
                    finally return (make--jj-graph :first-line-prefix graph-pre
                                                   :first-line-node graph-node
                                                   :first-line-suffix graph-suf
                                                   :mandatory-segments graph-tail))))
     (defun insert-jj-entry (entry)
       "Insert the ENTRY, formatted as a jj log entry."
       ,(cl-labels ((field (name-sym)
                      `(,(intern (format "%s-%s" 'jj-header name-sym))
                        header)))
          `(let ((target-buffer (current-buffer))
                 (header (jj-entry-header entry))
                 (graph (jj-entry-graph entry)))
             ;; open a buffer to make a mess in
             ;; we'll insert its contents later
             (with-temp-buffer
               (cl-loop for (name val printer face sep) in (list ,@(cl-loop  
                                                            for (name . props) in fields
                                                            for first = t then nil
                                                            for sep = (if-let ((sep (plist-get props :separator)))
                                                                          sep
                                                                        (cond
                                                                         (first "")
                                                                         (t " ")))
                                                            for face = (plist-get props :face)
                                                            for printer = (plist-get props :printer)
                                                            collect `(list ',name ,(field name) ,printer ,face ,sep)))
                        do (when-let ((printed (and (s-present? val)
                                                    (s-presence
                                                     (if printer
                                                         (funcall printer val header)
                                                       val)))))
                             (insert sep (propertize printed
                                                     'help-echo (symbol-name name)
                                                     'face face))))
               ;; ensure commit text ends on a newline
               (unless (bolp)
                 (insert "\n"))
               ;; add field to all the commit text (including newlines)
               ;; pointing to the header struct
               (add-text-properties (point-min) (point-max) `(jj-object ,header))
               ;; insert the mandatory graph prefix segments
               ;; these will add new lines if there arent enough already
               (cl-loop initially (progn
                                    (goto-char (point-min))
                                    (insert (jj-graph-first-line-prefix graph)
                                            (propertize (jj-graph-first-line-node graph)
                                                        'face '(:foreground "cyan")
                                                        'jj-object header)
                                            (jj-graph-first-line-suffix graph))
                                    (forward-line 1))
                        for (prefix . rest) on (jj-graph-mandatory-segments graph)
                        do (progn
                             (cond ((and
                                     ;; last nonempty mandatory segment
                                     (not (cdr rest))
                                     (not (string= "" (string-trim prefix)))
                                     ;; no repeatable segments
                                     (not (jj-graph-repeatable-segment graph)))
                                    (insert (propertize prefix 'face '(:foreground "grey"))))
                                   (t
                                    (insert prefix)))
                             (forward-line 1)
                             ;; while there's more mandatory graph segments, put them on new lines if you have to
                             (unless (bolp)
                               (insert "\n"))))
               ;; insert the repeatable graph prefix segments
               ;; these are added to all remaining lines, but no new lines are added
               (cl-loop with tail = (or (jj-graph-repeatable-segment graph)
                                        (car (last (jj-graph-mandatory-segments graph))))
                        while (and (bolp)
                                   (not (eobp)))
                        do (progn (insert tail)
                                  (forward-line 1)))
               ;; insert the content of this temp buffer into the target buffer
               ;; we can't just return it from the with-temp-buffer block,
               ;; as by that point it's been disposed
               (let ((content-buffer (current-buffer)))
                 (with-current-buffer target-buffer
                   (insert-buffer-substring content-buffer)))))))
     (defun insert-jj-elided (graph)
       "Insert the GRAPH and an \"elided revisions\" label, formatted as a jj log entry."
       (let ((target-buffer (current-buffer)))
         ;; open a buffer to make a mess in
         ;; we'll insert its contents later
         (with-temp-buffer
           (insert (propertize "(elided revisions)\n" 'face '(:foreground "grey")))
           ;; insert the mandatory graph segments
           ;; these will add new lines if there arent enough already
           (cl-loop initially (progn
                                (goto-char (point-min))
                                (insert (jj-graph-first-line-prefix graph)
                                        (propertize (jj-graph-first-line-node graph)
                                                    'face '(:foreground "grey"))
                                        (jj-graph-first-line-suffix graph))
                                (forward-line 1))
                    for prefix in (jj-graph-mandatory-segments graph)
                    do (progn
                         (insert prefix)
                         (forward-line 1)
                         ;; while there's more mandatory graph segments, put them on new lines if you have to
                         (unless (bolp)
                           (insert "\n"))))
           ;; insert the content of this temp buffer into the target buffer
           ;; we can't just return it from the with-temp-buffer block,
           ;; as by that point it's been disposed
           (let ((content-buffer (current-buffer)))
             (with-current-buffer target-buffer
               (insert-buffer-substring content-buffer))))))
     (cl-defstruct jj-header
       ;; semantic fields
       ,@(cl-loop for (name . props) in fields
                  collect name))))

(cl-defstruct jj-entry
  "A jj log entry with commit info and graph prefixes."
  header
  graph)

(defun make-jj-graph (pre node suff &rest graph-prefixes)
  "Make a jj graph struct from the given PRE NODE SUFF identifying the position of the node icon in the first line, and GRAPH-PREFIXES identifying the remainder of the graph."
  ;; figure out what part of the graph we can repeat, and what parts we can skip
  (-let* ((graph-partitioned
           (jj-partition-runs
            ;; notice edge between runs and non-runs
            graph-prefixes
            #'string=))
          ((graph-mandatory . resolved-tail)
           (pcase-exhaustive graph-partitioned
             ((and (or `(,tail-list)
                       `(,graph-mandatory ,tail-list))
                   (let `(,resolved-tail)
                     ;; only a single run of repeated elements (and they're all duplicates so only need the one)
                     (cl-remove-duplicates tail-list :test #'string=)))
              ;; either no mandatory list, or a mandatory list and a repeatable list
              ;; repeatable list is identified by being a run
              `(,graph-mandatory . ,resolved-tail))
             (`(,graph-mandatory)
              ;; no tail, so assume the repeatable list is empty. in `insert-jj-entry', assume the last mandatory is repeatable.
              ;; special case used to identify (and print in grey) the truncated marker of fully-separate subtrees
              ;; - this implementation is brittle AF. I really should set the truncation marker and search for it.
              `(,graph-mandatory))
             (illegal (error "I thought that jj graph output would always have at most one run in a set of jj--count-graph-lines lines, and that run would be at the end, but apparently not. graph segments: %s" `(,graph-mandatory . ,graph-tails))))))
    (make--jj-graph
     :first-line-prefix pre
     :first-line-node node
     :first-line-suffix suff
     :mandatory-segments graph-mandatory
     :repeatable-segment resolved-tail)))

(cl-defstruct (jj-graph (:constructor make--jj-graph))
  "Specification for the line-prefixes needed to annotate any number of commit-info lines with the graph for that region of the log."
  first-line-prefix
  first-line-node
  first-line-suffix
  mandatory-segments
  repeatable-segment)

(defvar jj-log-node-template
  (jj-template `(++ ,jj--delim
                    (coalesce 
                     (if !self "~")
                     (if current_working_copy "@")
                     (if immutable "+")
                     (if conflict "×")
                     "o")
                    ,jj--delim))
  "Node format to ensure log nodes can be parsed.")

(define-jj-log-format
 (change-id
  :face '(:foreground "magenta")
  :form (:chain self (format_short_change_id_with_change_offset)))
 (author
  :face '(:foreground "yellow")
  :quoted t
  :form (:chain self (.author) (.email) (stringify) (.escape_json)))
 (timestamp
  :face '(:foreground "cyan")
  :form (:chain self (.committer) (.timestamp) (.local) (.format "%Y-%m-%d %H:%M:%S")))
 (bookmarks
  :face '(:foreground "magenta")
  :form (:chain self (.bookmarks)))
 (tags
  :face '(:foreground "yellow")
  :form (:chain self (.tags)))
 (working-copies
  :face '(:foreground "green")
  :form (:chain self (.working_copies)))
 (commit-id
  :face '(:foreground "light blue")
  :form (:chain self (.commit_id) (format_short_commit_id)))
 (conflict
  :face '(:foreground "red")
  :form (if (:chain self (.conflict)) "conflict"))
 (empty
  :face '(:foreground "green")
  :printer (lambda (empty _entry)
             (when (string= "true" empty)
                 "(empty)"))
  :separator "\n"
  :form (:chain self (.empty)))
 (description
  :quoted t
  :separator "\n"
  :form (:chain self (.description) (.escape_json))))

(ert-deftest jj-round-trip-truncate-overflow ()
  (let ((entry (with-temp-buffer
                 
                 (insert "@  puvwmkxr\"zoeyhewll@gmail.com\"2025-12-18 18:07:32f610054a\"bic\\n*big\\nmultiline\\nmessage\\nwith\\nhonestly,\\ntoo much \\ntext\\nright here\\n\"\n│  \n~  \n   \n   \n")
                 (goto-char (point-min))
                 (read-jj-entry))))
    (should (equal entry
                   #s(jj-entry
                      #s(jj-header "puvwmkxr"
                                   "zoeyhewll@gmail.com"
                                   "2025-12-18 18:07:32"
                                   ""
                                   ""
                                   ""
                                   "f610054a"
                                   ""
                                   "bic\n*big\nmultiline\nmessage\nwith\nhonestly,\ntoo much \ntext\nright here\n")
                      #s(jj-graph "" "@" "  "
                                  ("│  "
                                   "~  "
                                   "   ")
                                  nil))))
    (with-temp-buffer
      (insert-jj-entry entry)
      (should (string= (substring-no-properties (buffer-string))
                       "@  puvwmkxr zoeyhewll@gmail.com 2025-12-18 18:07:32 f610054a\n│  bic\n~  *big\n   multiline\n   message\n   with\n   honestly,\n   too much \n   text\n   right here\n"
                       )))))

(ert-deftest jj-round-trip-truncate-underflow ()
  (let ((entry (with-temp-buffer
                 (insert "@  puvwmkxr\"zoeyhewll@gmail.com\"2025-12-18 18:07:32f610054a\"\"\n│  \n~  \n   \n   \n")
                 (goto-char (point-min))
                 (read-jj-entry))))
    (should (equal entry
                   #s(jj-entry
                      #s(jj-header "puvwmkxr"
                                   "zoeyhewll@gmail.com"
                                   "2025-12-18 18:07:32"
                                   ""
                                   ""
                                   ""
                                   "f610054a"
                                   ""
                                   "")
                      #s(jj-graph "" "@" "  "
                                  ("│  "
                                   "~  "
                                   "   ")
                                  nil))))
    (with-temp-buffer
      (insert-jj-entry entry)
      (should (string= (substring-no-properties (buffer-string))
                       "@  puvwmkxr zoeyhewll@gmail.com 2025-12-18 18:07:32 f610054a\n│  \n~  \n   \n")))))

(ert-deftest jj-round-trip-simple-underflow ()
  (let ((entry (with-temp-buffer
                 (insert "@  puvwmkxr\"zoeyhewll@gmail.com\"2025-12-18 18:07:32f610054a\"\"\n│  \n│  \n│  \n")
                 (should (string-match-p jj-entry-regex (buffer-string)))
                 (goto-char (point-min))
                 (read-jj-entry))))
    (should (equal entry
                   #s(jj-entry
                      #s(jj-header "puvwmkxr"
                                   "zoeyhewll@gmail.com"
                                   "2025-12-18 18:07:32"
                                   ""
                                   ""
                                   ""
                                   "f610054a"
                                   ""
                                   "")
                      #s(jj-graph "" "@" "  "
                                  ()
                                  "│  "))))
    (with-temp-buffer
      (insert-jj-entry entry)
      (should (string= (substring-no-properties (buffer-string))
                       "@  puvwmkxr zoeyhewll@gmail.com 2025-12-18 18:07:32 f610054a\n")))))
;; Log format:1 ends here

;; Generic format macro

;; [[file:majjik.org::*Generic format macro][Generic format macro:1]]
(defmacro define-jj-plain-format (type-name &rest fields)
  "Define the format to be used for parsing and formatting various jj output.
  Accepts a list of FIELDS in the form (FIELD-NAME . PLIST), where PLIST accepts the following keys:
  - `:form' specifies the sexpression used to produce the field's log template, produced with `jj-template'. (So far there's no way to use a string template directly)
  - `:quoted' specifies whether the field should be considered un-trusted, and therefore decoded from json when bring read. If non-nil, its machine-readable `:form' should call escape_json as its last step.
  - `:printer' specifies how to print the data out to a buffer. It should be a function of 2 arguments, with the first being the field and the second the entire structure.
  - `:face' specifies the face to use for formatting this entry.
  - `:separator' the separator to insert before this field, rather than a space (or empty for the first field). Only inserted if the value is present."
  (declare (indent 1))
  `(progn
     (require 'json)
     (define-short-documentation-group ,(intern (format "jj-%s" type-name))
       (define-jj-plain-format
           :no-manual t)
       (,(intern (format "read-jj-%s" type-name))
        :no-manual t)
       (,(intern (format "insert-jj-%s" type-name))
        :no-manual t)
       (,(intern (format "jj-%s-template" type-name))
        :no-manual t)
       (,(intern (format "make-jj-%s" type-name))
        :args (&key ,@(mapcar #'car fields))
        :no-manual t))
     (defvar ,(intern (format "jj-%s-regex" type-name))
       ,(let* ((content `(not (any ,jj--delim ,jj--major-delim "\r\n"))))
          `(rx line-start
               (seq
                ,(format "%s" type-name)
                ;; struct delimiter
                ,jj--major-delim
                ;; struct elements
                (* ,content)
                (= ,(1- (length fields))
                   ,jj--delim
                   (* ,content))
                "\n")))
       ,(format "Regex to match a full `jj-%1$s' entry. This *should* match exactly the same content that `read-jj-%1$s' will parse." type-name))
     (defun ,(intern (format "jj-%s-template" type-name)) (self)
       ,(format "Get a commit template to produce entries parseable by `read-jj-%s'. SELF is the symbol to use for the self-type; usually this will just be `self', but if using the template in a lambda you may want a different type-name." type-name)
       (jj-template (cl-subst self 'self
                              '(++ ,(format "%S" type-name)
                                   ,jj--major-delim
                                   (join ,jj--delim
                                         ,@(cl-loop for (field-name . props) in fields
                                                    for form = (plist-get props :form)
                                                    collect form))
                                   ))))
     (defun ,(intern (format "read-jj-%s" type-name)) ()
       ,(format "With point at the beginning of a `jj-%1$s' entry, parse the entry into a `jj-%1$s' struct." type-name)
       ,(let ((content `(not (any ,jj--delim ,jj--major-delim "\r\n"))))
          `(cl-loop initially (with-error-context (lambda (msg)
                                                    (format "failed to read struct label %s: %s" ',type-name msg))
                                (jj--re-step-over (rx ,(format "%s" type-name) ,jj--major-delim)))
                    for (key . quoted) in ',(cl-loop for (field-name . props) in fields
                                                     for key = (intern (format ":%s" field-name))
                                                     for quoted = (plist-get props :quoted)
                                                     collect `(,key . ,quoted))
                    ;; no delimiter for first field
                    for first = t then nil
                    for field-rx = (rx (group (* ,content))) then (rx ,jj--delim (group (* ,content)))
                    when (with-error-context (lambda (msg)
                                               (format "failed to read field %s: %s" field-name msg))
                           (jj--re-step-over field-rx))
                    nconc `(,key ,(string-trim
                                   (if quoted
                                       (save-match-data
                                         (funcall #'json-parse-string (match-string 1)))
                                     (match-string 1))))
                    into struct-props
                    finally return (apply #',(intern (format "make-jj-%s" type-name)) struct-props))))
     (defun ,(intern (format "insert-jj-%s" type-name)) (entry)
       ,(format "Insert the ENTRY, formatted as a `jj-%s' entry." type-name)
       ,(cl-labels ((field (name-sym)
                      `(,(intern (format "jj-%s-%s" type-name name-sym))
                        entry)))
          `(let ((target-buffer (current-buffer)))
             ;; open a buffer to make a mess in
             ;; we'll insert its contents later
             (with-temp-buffer
               (cl-loop for (field-name val printer face sep) in (list ,@(cl-loop  
                                                                  for (field-name . props) in fields
                                                                  for first = t then nil
                                                                  for sep = (if-let ((sep (plist-get props :separator)))
                                                                                sep
                                                                              (cond
                                                                               (first "")
                                                                               (t " ")))
                                                                  for face = (plist-get props :face)
                                                                  for printer = (plist-get props :printer)
                                                                  collect `(list ',field-name ,(field field-name) ,printer ,face ,sep)))
                        do (when-let ((printed (and (s-present? val)
                                                    (s-presence
                                                     (if printer
                                                         (funcall printer val entry)
                                                       val)))))
                             (insert sep (propertize printed
                                                     'help-echo (symbol-name field-name)
                                                     'face face))))
               ;; ensure commit text ends on a newline
               (unless (bolp)
                 (insert "\n"))
               ;; add field to all the commit text (including newlines)
               ;; pointing to the entry struct
               (add-text-properties (point-min) (point-max) `(jj-object ,entry))
               ;; insert the content of this temp buffer into the target buffer
               ;; we can't just return it from the with-temp-buffer block,
               ;; as by that point it's been disposed
               (let ((content-buffer (current-buffer)))
                 (with-current-buffer target-buffer
                   (insert-buffer-substring content-buffer)))))))
     (cl-defstruct ,(intern (format "jj-%s" type-name))
       ;; semantic fields
       ,@(cl-loop for (field-name . props) in fields
                  collect field-name))))
;; Generic format macro:1 ends here

;; Status format

;; [[file:majjik.org::*Status format][Status format:1]]
(define-jj-plain-format status-lineage-entry
  (change-id
   :face '(:foreground "magenta")
   :form (:chain self (format_short_change_id_with_change_offset)))
  (commit-id
   :face '(:foreground "light blue")
   :form (:chain self (.commit_id) (format_short_commit_id)))
  (bookmarks
   :face '(:foreground "magenta")
   :form (:chain self (.bookmarks)))
  (tags
   :face '(:foreground "yellow")
   :form (:chain self (.tags)))
  (conflict
   :face '(:foreground "red")
   :form (if (:chain self (.conflict)) "conflict"))
  (description
   :quoted t
   :form (:chain self (.description) (.trim) (.escape_json))))

(define-jj-plain-format status-wc-change
  (status
   :form (:chain self (.status)))
  (path-source
   :quoted t
   :form (:chain self (.source) (.path) (.display) (.escape_json)))
  (path-target
   :quoted t
   :form (:chain self (.target) (.path) (.display) (.escape_json))))

(define-jj-plain-format status-file-conflict
  (path
   :face '(:foreground "red")
   :quoted t
   :form (:chain f (.path) (.display) (.escape_json)))
  ;; (num-sides) ;; todo once it's representable in a template. for now, always unknown.
  )

(define-jj-plain-format status-file-untracked
  (path
   :face '(:foreground "magenta")
   :quoted t
   :form (++ (:chain self (.display t) (.escape_json)) "\n")))

(define-jj-plain-format status-bookmark-conflict
  (name
   :face '(:foreground "magenta")
   :form (:chain self (.name) ))
  (remote
   :face '(:foreground "magenta")
   :separator "@"
   :form (++ (:chain self (.remote)) "\n")))
;; Status format:1 ends here

;; Default args

;; [[file:majjik.org::*Default args][Default args:1]]
(defvar jj-global-default-args
  '(;; never auto-track new files
    "--config" "snapshot.auto-track='none()'"
    ;; never colourise output
    "--color=never"
    ;; never request a pager
    "--no-pager"
    ;; never print secondary output
    "--quiet"
    ))
;; Default args:1 ends here

;; Readers

;; [[file:majjik.org::*Readers][Readers:1]]
(defvar jj-revset-history nil "History for jj revsets")
(defun jj-read-revset-sexp (&optional prompt)
  (when-let ((sexp (read-from-minibuffer (or prompt "revset sexp: ") nil nil nil 'jj-revset-history)))
    (unless (string= "" sexp)
      (jj-revset (read sexp)))))
(defun jj-read-revset (&optional prompt)
  (when-let ((str (read-from-minibuffer (or prompt "revset: ") nil nil nil 'jj-revset-history)))
    (unless (string= "" str) str)))
(defun jj-read-revision (&optional prompt revset)
  (when-let ((str (completing-read (or prompt "revision: ") (jj-match-revisions revset) nil nil nil 'jj-revset-history)))
    (unless (string= "" str) str)))

(defvar jj-fileset-history nil "History for jj filesets")
(defun jj-read-fileset-sexp (&optional prompt)
  (when-let ((sexp (read-from-minibuffer (or prompt "fileset sexp: ") nil nil nil 'jj-fileset-history)))
    (unless (string= "" sexp)
      (jj-fileset (read sexp)))))
(defun jj-read-fileset (&optional prompt)
  (when-let ((str (read-from-minibuffer (or prompt "fileset: ") nil nil nil 'jj-fileset-history)))
    (unless (string= "" str) str)))
(defvar jj-template-history nil "History for jj templates")
(defun jj-read-template-sexp (&optional prompt)
  (when-let ((sexp (read-from-minibuffer (or prompt "template sexp: ") nil nil nil 'jj-template-history)))
    (unless (string= "" sexp)
      (jj-template (read sexp)))))
(defun jj-read-template (&optional prompt)
  (when-let ((str (read-from-minibuffer (or prompt "template: ") nil nil nil 'jj-template-history)))
    (unless (string= "" str) str)))
;; Readers:1 ends here

;; Keymaps

;; [[file:majjik.org::*Keymaps][Keymaps:1]]
(defvar-keymap jj-inspect-mode-map
  :parent special-mode-map
  "," #'jj-inspect-sexp-at-point)

(defvar-keymap jj-dashboard-mode-map
  :parent jj-inspect-mode-map
  "C-/" #'jj-undo
  "C-?" #'jj-redo
  "C-_" #'jj-undo
  "C-M-_" #'jj-redo
  "C-x u" #'jj-undo
  "c e" #'jj-edit-dwim
  "c n e" #'jj-edit-dwim
  "c n n" #'jj-new-on-dwim
  "c n -" #'jj-new-before-dwim
  "c n +" #'jj-new-after-dwim
  "c n b" #'jj-new-before-dwim
  "c n a" #'jj-new-after-dwim
  "c n i" #'jj-new-insert
  "c k" #'jj-drop-dwim
  "c w" #'jj-desc-dwim
  "c a" #'jj-amend-into-dwim
  "c s" #'jj-squash-down-dwim
  "F" #'jj-git-fetch
  "P" #'jj-git-push
  "b n" #'jj-bookmark-new-dwim
  "b m" #'jj-bookmark-move-dwim
  "b r" #'jj-bookmark-rename
  "b d" #'jj-bookmark-delete
  "b f" #'jj-bookmark-forget
  "b t" #'jj-bookmark-track
  "b u" #'jj-bookmark-untrack
  "f t" #'jj-file-track-dwim)

(keymap-global-set "C-x j" #'jj-dash)
;; Keymaps:1 ends here

;; Dashboard buffer

;; [[file:majjik.org::*Dashboard buffer][Dashboard buffer:1]]
(define-derived-mode jj-inspect-mode special-mode "jj-inspect"
  "Parent mode for most jj modes, defining basic operations")

(define-derived-mode jj-dashboard-mode jj-inspect-mode "jj-dash"
  "Major mode for jj dashboard")

(defvar-local jj--indirect-buffers nil
  "Indirect buffers into the current buffer. Ought to be killed if we're reverting.")

(defun replace-buffer-contents-and-properties (source &optional max-secs max-costs)
  (let ((buf (current-buffer)))
    (replace-buffer-contents source max-secs max-costs)
    (with-current-buffer source
      (cl-loop for start = (point-min) then pos
               for pos = (next-property-change start)
               for end = (or pos
                             (point-max))
               for props = (text-properties-at start)
               do (set-text-properties start end props buf)
               while pos))))

(defun jj-dash--revert-async (&optional and-then)
  "Asynchronously get the new status, and reverts the buffer contents when those processes complete.
Reverted buffer is the one that was active when this function was called."
  (if-let ((dash-buf (current-buffer))
           (temp-buf (generate-new-buffer "*jj-dash-replacement*")))
      (cl-labels ((end-ok ()
                    (unwind-protect
                        (when (buffer-live-p dash-buf)
                          (with-current-buffer dash-buf
                            (let ((inhibit-read-only t))
                              (replace-buffer-contents-and-properties temp-buf))
                            (setq jj--current-status
                                  (buffer-local-value 'jj--current-status temp-buf))))
                      (cleanup)))
                  (end-err (errs)
                    (dbg errs)
                    (unwind-protect
                        (error "jj status update failed: %s" errs)
                      (cleanup)))
                  (cleanup ()
                    (kill-buffer temp-buf)
                    (when and-then
                      (funcall and-then))))
        (with-current-buffer temp-buf
          (start-jj-dash-async #'end-ok #'end-err)))
    (message "no jj dash buffer to update")))

(defun jj-dash--revert (&rest _)
  ;; todo: wait on this somehow?
  (jj-dash--revert-async))

(defun jj-dash--revert-and-goto (pred)
  (jj-dash--revert-async
   (lambda ()
     (when pred
       (jj-jump-find-object pred)))))

(defun jj-jump-find-object (pred)
  (cl-loop for pos = (point-min) then (next-single-property-change pos 'jj-object)
           while pos
           for obj = (get-text-property pos 'jj-object)
           when (funcall pred obj) return (goto-char pos)))

(defun jj-jump-to-thing (thing &optional test)
  (let ((test (or test #'eq)))
    (jj-jump-find-object (lambda (obj) (funcall test thing obj)))))

(defun jj-dash-buffer (repo-dir)
  (format "*jj-dash %s*" repo-dir))

(defun jj-dash (repo-dir)
  (interactive (list default-directory))
  (let ((main-buf (get-buffer-create (jj-dash-buffer repo-dir))))
    (with-current-buffer main-buf
      (jj-dashboard-mode)
      (setq-local default-directory repo-dir
                  revert-buffer-function #'jj-dash--revert)
      (start-jj-dash-blocking))
    (pop-to-buffer main-buf)))

(defun jj-project-dash ()
  "Run `jj-dash' in the current project's root."
  (interactive)
  (if (fboundp 'project-root)
      (jj-dash (project-root (project-current t)))
    (user-error "`jj-project-dash' requires `project' 0.3.0 or greater")))

(defun jj-make-section-buffer (section-name &optional header trailer)
  "Make a jj section called SECTION-NAME which is narrowed down to the current value of point. Returns the buffer.
 HEADER is the text to insert before the section, and TRAILER is the text to insert after it."
  (let ((buf (clone-indirect-buffer
              (format "*jj-%s-section %s*" section-name default-directory)
              nil :norecord)))
    (prog1 buf
      (insert header)
      ;; make sure there's something between end-of-buffer and where we'll be inserting stuff
      (save-excursion
        (insert trailer)
        (push-mark nil :nomsg))
      ;; record point from the original buffer
      ;; seems there's a race condition to update it in the indirect buffer
      (let ((p (point)))
        (with-current-buffer buf
          (narrow-to-region p p)))
      ;; step over the added character
      (goto-char (mark))
      (pop-mark))))
;; Dashboard buffer:1 ends here

;; jj-show for status section
;; the ~jj status~ command itself is not very machine-readable, but turns out i can show most of what I want from ~jj status~ via ~jj show~.
;; so far, the only significant differences in parity that I'm aware of, are:
;; - I cannot count the sides of a file conflict
;; - I can only show ref conflicts for the current commit, not the whole repo. but I can probably just call jj bookmark list -c for that.


;; [[file:majjik.org::*jj-show for status section][jj-show for status section:1]]
(defun jj-show--revert ()
  (start-jj-show-status default-directory
                        jj--last-revs
                        jj--last-files))

(defun jj-show-status (repo-dir &optional revset fileset)
  "Run jj show asynchronously in REPO-DIR, with the given REVSET, FILESET, and TEMPLATE string arguments. Any OTHER-ARGS must be passed as strings. Returns the process and opens the corresponding buffer."
  (interactive (list (read-directory-name "jj repo: ")
                     (jj-read-revset-sexp)
                     (jj-read-fileset-sexp)))
  (let* ((repo-dir (expand-file-name repo-dir))
         (buf (get-buffer-create (format "*jj-show: %s*" repo-dir))))
    (prog1
        (with-current-buffer buf
          (jj-inspect-mode)
          (setq-local default-directory repo-dir
                      jj--last-revs revset
                      jj--last-files fileset
                      revert-buffer-function #'jj-show-revert)
          (start-jj-show-status revset fileset))
      (pop-to-buffer buf))))

(defun start-jj-show-status (&optional revset fileset)
  (let ((inhibit-read-only t))
    ;; erase while respecting narrowing
    (delete-region (point-min) (point-max)))
  (let* ((err (generate-new-buffer "*jj-show-status-stderr*"))
         (sentinel (make-jj-simple-sentinel err))
         (filter (make-sticky-process-filter :sticky)))
    (make-process
     :name "jj-show-status"
     :buffer (current-buffer)
     :stderr err
     :filter filter
     :sentinel sentinel
     :noquery t
     :command `("jj" "show"
                "--no-patch"
                "-T" ,(jj-show-status-template 'self)
                ,@jj-global-default-args))))
;; jj-show for status section:1 ends here

;; jj-show for diff section


;; [[file:majjik.org::*jj-show for diff section][jj-show for diff section:1]]
(defun jj-show-status--revert ()
  (start-jj-show-diff default-directory
                      jj--last-revs
                      jj--last-files))

(defun jj-show-diff (repo-dir &optional revset fileset)
  "Run jj show asynchronously in REPO-DIR, with the given REVSET, FILESET, and TEMPLATE string arguments. Any OTHER-ARGS must be passed as strings. Returns the process and opens the corresponding buffer."
  (interactive (list (read-directory-name "jj repo: ")
                     (jj-read-revset-sexp)
                     (jj-read-fileset-sexp)))
  (let* ((repo-dir (expand-file-name repo-dir))
         (buf (get-buffer-create (format "*jj-show: %s*" repo-dir))))
    (prog1
        (with-current-buffer buf
          (jj-inspect-mode)
          (setq-local default-directory repo-dir
                      jj--last-revs revset
                      jj--last-files fileset
                      revert-buffer-function #'jj-show-revert)
          (start-jj-show-diff revset fileset))
      (pop-to-buffer buf))))

(defun start-jj-show-diff (&optional revset fileset)
  (let ((inhibit-read-only t))
    ;; erase while respecting narrowing
    (delete-region (point-min) (point-max)))
  (let* ((err (generate-new-buffer "*jj-show-diff-stderr*"))
         (sentinel (make-jj-simple-sentinel err))
         (filter (make-sticky-process-filter :sticky)))
    (make-process
     :name "jj-status-diff"
     :buffer (current-buffer)
     :stderr err
     :filter filter
     :sentinel sentinel
     :noquery t
     :command `("jj" "show"
                "--no-patch"
                "-T" ,(jj-status-diff-template 'self)
                ,@jj-global-default-args))))
;; jj-show for diff section:1 ends here

;; jj-file for untracked files


;; [[file:majjik.org::*jj-file for untracked files][jj-file for untracked files:1]]
(defvar jj-file-untracked-regex
  (rx "? " ?\" (* nonl) ?\" "\n")
  "Regex matching a line that looks like it is a jj untracked-file entry.")

(defun jj-file-untracked--revert (&rest _)
  (start-jj-file-untracked))

(defun jj-file-untracked (repo-dir)
  "Run jj file list-untracked asynchronously in REPO-DIR, with the given REVSET, FILESET, and TEMPLATE string arguments. Returns the process and opens the corresponding buffer."
  (interactive (list (read-directory-name "jj repo: ")
                     (jj-read-revset-sexp)))
  (let* ((repo-dir (expand-file-name repo-dir))
         (buf (get-buffer-create (format "*jj-file-untracked: %s*" repo-dir))))
    (prog1
        (with-current-buffer buf
          (jj-inspect-mode)
          (setq-local default-directory repo-dir
                      jj--last-revs revset
                      revert-buffer-function #'jj-file-untracked--revert)
          (start-jj-file-untracked))
      (pop-to-buffer buf))))

(defun start-jj-file-untracked ()
  "Make a jj file-untracked in the current buffer, without setting up modes or keymaps. For use with jj-status in an indirect buffer."
  (let ((inhibit-read-only t))
    ;; erase while respecting narrowing
    (delete-region (point-min) (point-max)))
  (let* ((err (generate-new-buffer "*jj-file-untracked-stderr*"))
         (sentinel (make-jj-simple-sentinel err))
         (filter (make-sticky-process-filter :sticky)))
    (make-process
     :name "jj-file-untracked"
     :buffer (current-buffer)
     :stderr err
     :filter filter
     :sentinel sentinel
     :noquery t
     :command `("jj" "file" "list-untracked"
                "-T" ,(jj-status-file-untracked-template 'self)
                ,@jj-global-default-args))))
;; jj-file for untracked files:1 ends here

;; jj-bookmark-list for bookmark conflicts


;; [[file:majjik.org::*jj-bookmark-list for bookmark conflicts][jj-bookmark-list for bookmark conflicts:1]]
(defun jj-bookmark-list--revert ()
  (start-jj-bookmark-list default-directory jj--last-revs jj--last-files))

(defun jj-bookmark-list (repo-dir &optional revset fileset &rest other-args)
  "Run jj bookmark asynchronously in REPO-DIR, with the given REVSET, FILESET, and TEMPLATE string arguments. Returns the process and opens the corresponding buffer."
  (interactive (list (read-directory-name "jj repo: ")
                     (jj-read-revset-sexp)
                     (jj-read-fileset-sexp)))
  (let* ((repo-dir (expand-file-name repo-dir))
         (buf (get-buffer-create (format "*jj-bookmark-list: %s*" repo-dir))))
    (prog1
        (with-current-buffer buf
          (jj-inspect-mode)
          (setq-local default-directory repo-dir
                      jj--last-revs revset
                      jj--last-files fileset
                      revert-buffer-function #'jj-bookmark-list-revert)
          (start-jj-bookmark-list revset fileset other-args))
      (pop-to-buffer buf))))

(defun start-jj-bookmark-list (&optional revset fileset &rest other-args)
  "Make a jj bookmark-list in the current buffer, without setting up modes or keymaps. For use with jj-status in an indirect buffer."
  (let ((inhibit-read-only t))
    ;; erase while respecting narrowing
    (delete-region (point-min) (point-max)))
  (let* ((err (generate-new-buffer "*jj-bookmark-list-stderr*"))
         (sentinel (make-jj-simple-sentinel err))
         (filter (make-sticky-process-filter :sticky)))
    (make-process
     :name "jj-bookmark-list"
     :buffer (current-buffer)
     :stderr err
     :filter filter
     :sentinel sentinel
     :noquery t
     :command `("jj" "bookmark" "list"
                "-T" ,(jj-status-bookmark-conflict-template 'self)
                ,@(jj--if-arg revset #'identity "-r")
                ,@(jj--if-arg fileset #'identity "--")
                ,@other-args
                ,@jj-global-default-args))))
;; jj-bookmark-list for bookmark conflicts:1 ends here

;; Combined struct and output formatter

;; [[file:majjik.org::*Combined struct and output formatter][Combined struct and output formatter:1]]
(defvar-local jj--current-status nil
  "In a jj dashboard buffer, this is the most recent status object.")

(defun start-jj-status ()
  "Start the component processes for jj dashboard's status section. Returns the list of processes."
  (save-excursion
    (let ((procs))
      (cl-labels ((proc (proc)
                    (push proc procs)
                    proc))
        (with-current-buffer
            (jj-make-section-buffer "status" "\n" "\n")
          (proc (start-jj-show-status)))
        (with-current-buffer
            (jj-make-section-buffer "bookmark-conflicts" "\n" "\n")
          (proc (start-jj-bookmark-list nil nil "--conflicted")))
        (with-current-buffer
            (jj-make-section-buffer "untracked" "\n" "\n")
          (proc (start-jj-file-untracked)))

        ;; (with-current-buffer
        ;;     (jj-make-section-buffer "diff" "Diff:\n" "\n")
        ;;   (insert "diff-content\n")
        ;;   )
        )
      procs)))

(cl-defstruct jj-status
  "Aggregate of all of the jj status information from various command sources."
  files-untracked
  files-changed
  commit-working-copy
  commits-parent
  files-conflict
  bookmarks-conflict)

(defun read-jj-status ()
  (cl-macrolet ((try (fn) `(jj--forgiving-read #',fn))
                (line (&rest body) `(prog1 (progn ,@body)
                                      (unless (bolp)
                                        (forward-char 1)))))
    (make-jj-status
     :files-changed (cl-loop for entry = (line (try read-jj-status-wc-change))
                             while entry
                             collect entry)
     :commit-working-copy (line (read-jj-status-lineage-entry))
     :commits-parent (cl-loop for entry = (line (try read-jj-status-lineage-entry))
                              while entry
                              collect entry)
     :files-conflict (cl-loop for entry = (line (try read-jj-status-file-conflict))
                              while entry
                              collect entry)
     :bookmarks-conflict (cl-loop for entry = (line (try read-jj-status-bookmark-conflict))
                                  while entry
                                  collect entry)
     :files-untracked (cl-loop for entry = (line (try read-jj-status-file-untracked))
                               while entry
                               collect entry))))

(defmacro slot-values (object slots)
  (declare (indent 1))
  `(with-slots ,slots ,object
     (list ,@slots)))

(defun insert-jj-status (status)
  (with-slots (files-untracked
               files-changed
               commit-working-copy
               commits-parent
               files-conflict
               bookmarks-conflict)
      status
    (let ((list-prefix (propertize "- " 'face '(:foreground "grey"))))
      (cond (files-changed
             (insert "Working copy changes:\n")
             (cl-loop for change in files-changed
                      for (type from to) = (slot-values change
                                             (status path-source path-target))
                      for (color . elems) = (pcase-exhaustive type
                                              ("modified" `(cyan "M" ,to))
                                              ("added" `(green "A" ,to))
                                              ("removed" `(red "D" ,from))
                                              ("copied" `(green "C" "{" ,from "=>" ,to "}"))
                                              ("renamed" `(cyan "R" "{" ,from "=>" ,to "}")))
                      do (insert list-prefix)
                      (insert (propertize (mapconcat #'identity elems " ")
                                          'face `(:foreground ,(format "%s" color))
                                          'jj-object change)
                              "\n")))
            (t (insert "Working copy unchanged\n")))
      (insert "Working copy  (@) : ")
      (insert-jj-status-lineage-entry commit-working-copy)
      (cl-loop for parent in commits-parent
               do (insert "Parent commit (@-): ")
               (insert-jj-status-lineage-entry parent))
      (when files-conflict
        (insert "Unresolved file conflicts:\n")
        (cl-loop for conflict in files-conflict
                 do (insert list-prefix)
                 (insert-jj-status-file-conflict conflict)))
      (when bookmarks-conflict
        (insert "Unresolved bookmark conflicts:\n")
        (cl-loop for conflict in bookmarks-conflict
                 do (insert list-prefix)
                 (insert-jj-status-bookmark-conflict conflict)))
      (when files-untracked
        (insert "Untracked files:\n")
        (cl-loop for file in files-untracked
                 do (insert list-prefix)
                 (insert-jj-status-file-untracked file))))))

(defun jj-show-status-template (self)
  (jj-template
   (cl-subst self 'self
             `(separate "\n"
                        (:chain self
                                (.diff)
                                (.files)
                                (.map (lambda (f)
                                        (:lit ,(jj-status-wc-change-template 'f))))
                                (.join "\n"))
                        (:lit ,(jj-status-lineage-entry-template 'self))
                        (:chain self (.parents)
                                (.map (lambda (p)
                                        (:lit ,(jj-status-lineage-entry-template 'p))))
                                (.join "\n"))
                        (:chain self
                                (.files)
                                (.filter (lambda (f) (:chain f (.conflict))))
                                (.map (lambda (f)
                                        (:lit ,(jj-status-file-conflict-template 'f))))
                                (.join "\n"))))))

(ert-deftest jj-test-read-status ()
  (with-temp-buffer
    (save-excursion
      (insert "status-wc-changedeleted\"hello\"\"hello\"
status-wc-changeadded\"world\"\"world\"
status-wc-changerenamed\"hello\"\"world\"
status-lineage-entryzsrpuxsq/0fb53c784conflict\"diverge 1\"
status-lineage-entryznpwrsztf55e1a64conflict\"\"
status-file-conflict\"foo\"
status-file-conflict\"bar\"
status-bookmark-conflictfooorigin
status-bookmark-conflictbar
status-file-untracked\"file-untracked\"
status-file-untracked\"dir-untracked/\"
"))
    (should (equal (read-jj-status)
                   #s(jj-status
                      (#s(jj-status-file-untracked "file-untracked")
                       #s(jj-status-file-untracked "dir-untracked/"))
                      (#s(jj-status-wc-change "deleted" "hello" "hello")
                       #s(jj-status-wc-change "added" "world" "world")
                       #s(jj-status-wc-change "renamed" "hello" "world"))
                      #s(jj-status-lineage-entry "zsrpuxsq/0" "fb53c784" "" "" "conflict" "diverge 1")
                      (#s(jj-status-lineage-entry "znpwrszt" "f55e1a64" "" "" "conflict" ""))
                      (#s(jj-status-file-conflict "foo")
                       #s(jj-status-file-conflict "bar"))
                      (#s(jj-status-bookmark-conflict "foo" "origin")
                       #s(jj-status-bookmark-conflict "bar" "")))))))

(ert-deftest jj-test-read-status-2 ()
  (with-temp-buffer
    (save-excursion
      (insert "status-wc-changemodified\"README.md\"\"README.md\"
status-wc-changeremoved\"foo\"\"foo\"
status-wc-changerenamed\"signal-resumable.el\"\"signal-rework-2.el\"
status-wc-changeadded\"signal-rework.el\"\"signal-rework.el\"
status-wc-changeadded\"world\"\"world\"
status-lineage-entryzsrpuxsq/04f46f09c\"diverge 1\"
status-lineage-entryznpwrsztf55e1a64conflict\"\"
status-bookmark-conflictmain
status-bookmark-conflictmaingit
status-bookmark-conflictmainorigin
status-file-untracked\"\\u001b30mesc-in-name\\u001b0m\"
status-file-untracked\"\\u001b[30mesc-in-name\\u001b[0m\"
status-file-untracked\"baz\"
status-file-untracked\"newline\\nin\\nname\"
status-file-untracked\"untracked newline\\nin\\nname\"
"))
    (should (equal (read-jj-status)
                   #s(jj-status
                      (#s(jj-status-file-untracked "30mesc-in-name0m")
                       #s(jj-status-file-untracked "[30mesc-in-name[0m")
                       #s(jj-status-file-untracked "baz")
                       #s(jj-status-file-untracked "newline\nin\nname")
                       #s(jj-status-file-untracked "untracked newline\nin\nname"))
                      (#s(jj-status-wc-change "modified" "README.md" "README.md")
                       #s(jj-status-wc-change "removed" "foo" "foo")
                       #s(jj-status-wc-change "renamed" "signal-resumable.el" "signal-rework-2.el")
                       #s(jj-status-wc-change "added" "signal-rework.el" "signal-rework.el")
                       #s(jj-status-wc-change "added" "world" "world"))
                      #s(jj-status-lineage-entry "zsrpuxsq/0" "4f46f09c" "" "" "" "diverge 1")
                      (#s(jj-status-lineage-entry "znpwrszt" "f55e1a64" "" "" "conflict" ""))
                      ()
                      (#s(jj-status-bookmark-conflict "main" "")
                       #s(jj-status-bookmark-conflict "main" "git")
                       #s(jj-status-bookmark-conflict "main" "origin")))))))

(ert-deftest jj-test-insert-status ()
  (with-temp-buffer
    (insert-jj-status 
     (make-jj-status
      :files-untracked
      '(#s(jj-status-file-untracked "hello")
        #s(jj-status-file-untracked "world"))
      :files-changed
      '(#s(jj-status-wc-change "removed" "hello" "hello")
        #s(jj-status-wc-change "added" "world" "world")
        #s(jj-status-wc-change "renamed" "hello" "world"))
      :commit-working-copy
      #s(jj-status-lineage-entry "zsrpuxsq/0" "fb53c784" "" "" "conflict" "diverge 1")
      :commits-parent
      '(#s(jj-status-lineage-entry "znpwrszt" "f55e1a64" "" "" "conflict" ""))
      :files-conflict
      '(#s(jj-status-file-conflict "foo")
        #s(jj-status-file-conflict "bar"))
      :bookmarks-conflict
      '(#s(jj-status-bookmark-conflict "foo" "")
        #s(jj-status-bookmark-conflict "bar" "origin"))))

    (should (string= (substring-no-properties (buffer-string))
                     "Working copy changes:
- D hello
- A world
- R { hello => world }
Working copy  (@) : zsrpuxsq/0 fb53c784 conflict diverge 1
Parent commit (@-): znpwrszt f55e1a64 conflict
Unresolved file conflicts:
- foo
- bar
Unresolved bookmark conflicts:
- foo
- bar@origin
Untracked files:
- hello
- world
"))))
;; Combined struct and output formatter:1 ends here

;; status piping fns

;; [[file:majjik.org::*status piping fns][status piping fns:1]]
(defun get-jj-status-async (callback-ok callback-err)
  "Get jj status asynchronously, e.g. to be called within a sentinel callback. Calls CALLBACK-OK with the result of `read-jj-status', or calls CALLBACK-ERR with a list of failed processes. If it finishes successfully but the buffer is killed, calls CALLBACK-ERR with the symbol `:buffer'."
  (let ((repo default-directory)
        (buf (generate-new-buffer "*jj-dash-status*")))
    (with-current-buffer buf
      (setq default-directory repo)
      (let* ((procs (start-jj-status))
             (running (seq-copy procs))
             (failed ()))
        (cl-labels ((died (proc event)
                      ;; when one process dies, mark it not running
                      (setf running (cl-delete proc running))
                      ;; maybe mark it failed
                      (unless (string= event "finished\n")
                        (push proc failed))
                      ;; and if it was the last one, clean up.
                      (unless running
                        (cleanup)))
                    (cleanup ()
                      ;; once all processes finished, kill their buffers and call the appropriate callback
                      (mapc #'kill-buffer (mapcar #'process-buffer procs))
                      (unwind-protect
                          (cond
                           (failed
                            (funcall callback-err failed))
                           ((buffer-live-p buf)
                            (with-current-buffer buf
                              ;; need to remove empty lines so we can read-jj-status
                              (delete-matching-lines (rx line-start line-end) (point-min) (point-max))
                              (goto-char (point-min))
                              (funcall callback-ok (read-jj-status))))
                           (t
                            (funcall callback-err :buffer)))
                        ;; finally kill own output buffer
                        (kill-buffer buf))))
          (cl-loop for proc in procs
                   ;; advise each sentinel to call `died'
                   do (add-function :after (process-sentinel proc) #'died)))))))

(defun get-jj-status-blocking ()
  "blocks to get jj status synchronously, calling `accept-process-output'. Cannot be called within a sentinel callback. this is a problem if I want to update status buffer in a sentinel."
  (let ((repo default-directory))
    (with-temp-buffer
      (setq default-directory repo)
      (save-excursion
        (let ((procs (start-jj-status)))
          (while (-any #'process-live-p procs)
            (mapc (lambda (proc)
                    (accept-process-output proc 0.0 nil :only))
                  procs))
          (mapc #'kill-buffer (mapcar #'process-buffer procs))
          (delete-matching-lines (rx line-start line-end) (point-min) (point-max))))
      (read-jj-status))))

(defun start-jj-dash-async (callback-ok callback-err)
  "Start the jj dashboard in the current buffer, and call CALLBACK-OK once all processes finish successfully, or CALLBACK-ERR with a list of failed processes.

Also sets `jj--current-status' in the initial buffer when the status process completes."
  (assert-jj)
  (let ((inhibit-read-only t))
    ;; erase while respecting narrowing
    (delete-region (point-min) (point-max)))
  (let ((buf (current-buffer))
        (running ())
        (fails ()))
    (cl-labels ((cleanup ()
                  (cond (fails
                         (funcall callback-err fails))
                        (t
                         (funcall callback-ok)))))
      (with-current-buffer
          (jj-make-section-buffer "log" "Log:\n" "\n")
        (cl-labels ((died (proc event)
                      ;; when log process dies, mark it not running
                      (setf running (cl-delete proc running))
                      (unless (string= event "finished\n")
                        (push proc fails))
                      (unless running
                        (cleanup))))
          (let ((log (start-jj-log)))
            (add-function :after (process-sentinel log)
                          #'died)
            (push log running))))
      (cl-labels ((die (&optional errs)
                    (setf running (cl-delete :status running))
                    (pcase errs
                      ('nil
                       ;; no errors
                       )
                      (:buffer
                       ;; buffer deleted
                       (push "intermediate status buffer killed"
                             fails))
                      ((pred listp)
                       ;; process errors
                       (setq fails
                             (nconc errs fails)))
                      (_
                       ;; unexpected form
                       (push `(:unexpected-form ,errs) fails))))
                  (fail (errs)
                    (die errs)
                    (unless running
                      (cleanup)))
                  (ok (stat)
                    (die)
                    (if (buffer-live-p buf)
                        (with-current-buffer buf
                          (setq-local jj--current-status stat)
                          (goto-char (point-min))
                          (let ((inhibit-read-only t))
                            (insert-jj-status stat)))
                      (push (format "dash output buffer %s deleted" buf)
                            fails))
                    (unless running
                      (cleanup))))
        (push :status running)
        (get-jj-status-async #'ok #'fail)))))

(defun start-jj-dash-blocking ()
  "Start the jj dashboard, and block until the status section and the first part of the log are completed"
  (assert-jj)
  (let ((inhibit-read-only t))
    ;; erase while respecting narrowing
    (delete-region (point-min) (point-max)))
  (mapc #'kill-buffer jj--indirect-buffers)
  (setq jj--indirect-buffers nil)
  (cl-labels ((ind-buf (buffer)
                (push buffer jj--indirect-buffers)
                buffer))
    (save-excursion
      (let ((inhibit-read-only t)
            (log))
        (save-excursion
          ;; start async procs before waiting on main status
          
          ;; (with-current-buffer
          ;;     (indirect (jj-make-section-buffer "diff" "Diff:\n" "\n"))
          ;;   (insert "diff-content\n")
          ;;   )

          (with-current-buffer
              (ind-buf (jj-make-section-buffer "log" "Log:\n" "\n"))
            (setq log (start-jj-log))))
        
        ;; wait on main status
        (let ((stat (get-jj-status-blocking)))
          (setq-local jj--current-status stat)
          (insert-jj-status stat))
        ;; wait for first part of log output
        (accept-process-output log nil nil :only)))))
;; status piping fns:1 ends here

;; jj-log

;; [[file:majjik.org::*jj-log][jj-log:1]]
(defvar-local jj--last-revs nil
  "The revset last used in this buffer")
(defvar-local jj--last-files nil
  "The fileset last used in this buffer")
(defun jj-log--revert (&rest _)
  (start-jj-log jj--last-revs jj--last-files))

(defun jj-log (repo-dir &optional revset fileset)
  "Run jj log asynchronously in REPO-DIR, with the given REVSET, FILESET, and TEMPLATE string arguments. Returns the process and opens the corresponding buffer."
  (interactive (list (read-directory-name "jj repo: ")
                     (jj-read-revset)
                     (jj-read-fileset)))
  (let* ((repo-dir (expand-file-name repo-dir))
         (buf (get-buffer-create (format "*jj-log: %s*" repo-dir))))
    (prog1
        (with-current-buffer buf
          (jj-inspect-mode)
          (setq-local default-directory repo-dir
                      jj--last-revs revset
                      jj--last-files fileset
                      revert-buffer-function #'jj-log--revert)
          (start-jj-log revset fileset))
      (pop-to-buffer buf))))

(defun start-jj-log (&optional revset fileset)
  "Make a jj log in the current buffer, without setting up modes or keymaps. For use with jj-status in an indirect buffer. Ignores `jj--last-revs' and `jj--last-files'."
  (let ((inhibit-read-only t))
    ;; erase while respecting narrowing
    (delete-region (point-min) (point-max)))
  (let* ((buf (current-buffer))
         (temp (generate-new-buffer "*jj-log-temp*"))
         (err (generate-new-buffer "*jj-log-stderr*"))
         (sentinel (make-jj-simple-sentinel err temp))
         (filter (cl-labels ((read-next ()
                               (jj--try-read-each #'read-jj-entry #'read-jj-elided))
                             (print-entries (news)
                               (with-current-buffer buf
                                 (let ((inhibit-read-only t))
                                   (cl-loop for new in news
                                            do (pcase new
                                                 ((pred jj-graph-p)
                                                  (insert-jj-elided new))
                                                 ((pred jj-entry-p)
                                                  (insert-jj-entry new))))))))
                   (make-jj-generic-buffered-filter temp #'read-next #'print-entries))))
    (make-process
     :name "jj-log"
     :buffer buf
     :stderr err
     :filter filter
     :sentinel sentinel
     :noquery t
     :command `("jj" "log"
                "-T" ,jj-parseable-template
                ,@(jj--if-arg revset #'identity "-r")
                ,@(jj--if-arg fileset #'identity "--")
                ,@jj-global-default-args
                "--config" ,(format "templates.log_node='%s'"
                                    (jj--toml-quote-string jj-log-node-template))))))
;; jj-log:1 ends here

;; find and update dash buffer

;; [[file:majjik.org::*find and update dash buffer][find and update dash buffer:1]]
(defun jj-revert-dash-buffer (dir)
  "Revert the jj dash buffer (if it exists) for DIR."
  (when-let ((buf (get-buffer (jj-dash-buffer
                               dir))))
    (with-current-buffer buf
      (jj-dash--revert))))

(defun jj-revert-dash-buffer-async (dir)
  "Asynchronously revert the jj dash buffer (if it exists) for DIR. Suitable for use as an async callback for process filters and sentinels."
  (when-let ((buf (get-buffer (jj-dash-buffer
                               dir))))
    (with-current-buffer buf
      (jj-dash--revert-async))))
;; find and update dash buffer:1 ends here

;; repo query commands

;; [[file:majjik.org::*repo query commands][repo query commands:1]]
(defun jj-list-bookmarks ()
  "List all bookmarks."
  (string-lines (call-cmd `("jj" "bookmark" "list" "-T" "name ++ \"\n\"") nil :string) :omit))

(defun jj-match-revisions (&optional revset)
  "List all revisions matching REVSET, or all visible by default."
  (string-lines (call-cmd `("jj" "log" "--no-graph" "-r" ,(or revset "all()") "-T" "change_id ++ \"\n\"") nil :string) :omit))

(defun jj-workspace-root ()
  "Return the root of the current jj reposotory."
  (string-trim (call-cmd `("jj" "workspace" "root" ,@jj-global-default-args) nil :string)
               nil
               ;; only trim a single trailing newline
               "\n"))

(defalias 'assert-jj 'jj-workspace-root
  "Throw an error unless we're in a jj repo.")
;; repo query commands:1 ends here

;; status query utils

;; [[file:majjik.org::*status query utils][status query utils:1]]
(defun jj-get-revset-dwim (&optional prompt)
  "Get a revision or revset based on context. E.g. from around point. If no contextual value is apparent, prompt the user explicitly with PROMPT."
  (pcase (jj-thing-at-point)
    ((and cmt (pred jj-entry-p))
     (jj-header-change-id (jj-entry-header cmt)))
    ((and cmt (pred jj-header-p))
     (jj-header-change-id cmt))
    (unmatched (jj-read-revset prompt))))

(defun jj-get-untracked-file-dwim (&optional prompt)
  "Get an untracked filename based on context. E.g. from around point. If no contextual value is apparent, prompt the user explicitly with PROMPT."
  (pcase (jj-thing-at-point)
    ((and file (pred jj-status-file-untracked-p))
     (jj-status-file-untracked-path file))
    (unmatched (read-file-name prompt))))

(defun jj-get-revision-dwim (&optional prompt mutable)
  "Get a revision or revset based on context. E.g. from around point. If no contextual value is apparent, prompt the user explicitly with PROMPT. If MUTABLE, only include mutable commits in the completion options."
  (pcase (jj-thing-at-point)
    ((and cmt (pred jj-entry-p))
     (jj-header-change-id (jj-entry-header cmt)))
    ((and cmt (pred jj-header-p))
     (jj-header-change-id cmt))
    (unmatched (jj-read-revision prompt (when mutable "~immutable()")))))

(defun jj-rev-wc-p (rev)
  "Returns true if REV (a string) is the current working copy commit."
  (let ((stat (jj-status-commit-working-copy jj--current-status)))
    (or (equal rev (jj-status-lineage-entry-change-id stat))
        (equal rev (jj-status-lineage-entry-commit-id stat)))))

(defun jj-obj-wc-p (obj)
  "Returns true if OBJ is the current working copy commit."
  (and (jj-header-p obj)
       (jj-rev-wc-p (jj-header-change-id obj))))

(cl-defmacro jj-compatible-ident (a b specs &optional equal)
  "Check if objects A and B can be considered equal by some means of producing an identifier. SPECS is a list of (TYPE-P GETTER) where an object matching TYPE-P can be identified by GETTER. Only the first matching type is considered. The objects need not be of the same type, they only need to produce an identifier considered equal according to EQUAL."
  (cl-labels ((get-id (ob)
                `(cond ,@(cl-loop for (type-p getter) in specs
                                  collect `((,type-p ,ob) (list t (,getter ,ob))))
                       (t (list nil nil)))))
    `(-let (((ma id-a) ,(get-id a))
            ((mb id-b) ,(get-id b)))
       (and ma mb
            (,(or equal #'eq) id-a id-b)))))

(defun jj-theseus-p (old new)
  "Return true if OLD and NEW (both jj objects) could refer to the same object after a modifying operation."
  (or (equal old new)
      (jj-compatible-ident old new
                           ((jj-status-wc-change-p jj-status-wc-change-path-target)
                            (jj-status-file-conflict-p jj-status-file-conflict-path)
                            (jj-status-file-untracked-p jj-status-file-untracked-path))
                           equal)
      (jj-compatible-ident old new
                           ((jj-header-p jj-header-change-id))
                           equal)
      (jj-compatible-ident old new
                           ((jj-status-lineage-entry-p jj-status-lineage-entry-change-id))
                           equal)))
;; status query utils:1 ends here

;; thing at point

;; [[file:majjik.org::*thing at point][thing at point:1]]
(defun jj-thing-at-point ()
  (get-text-property (point) 'jj-object))

(defun jj-inspect-sexp-at-point ()
  "When point is in a jj object, show that object's data in its own buffer."
  (interactive)
  (pcase (jj-thing-at-point)
    ('nil (user-error "Not at an inspectable object"))
    (object
     (let ((buf (get-buffer-create "*jj-sexp*")))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "%S" object))))
       (pop-to-buffer buf)
       (special-mode)))))
;; thing at point:1 ends here

;; sync command utils

;; [[file:majjik.org::*sync command utils][sync command utils:1]]
(defun jj-cmd-sync (cmd &optional no-revert)
  "Call jj with the given CMD, passing the default args first, and returning the output as a string. Signals an error if the command returns a nonzero exit code. When the command completes successfully, reverts the dash buffer for the repo (if there is one)."
  (let ((cmd `("jj" ,@jj-global-default-args ,@cmd)))
    (prog1
        (call-cmd cmd nil :string)
      (unless no-revert
        (jj-revert-dash-buffer default-directory)))))
;; sync command utils:1 ends here

;; jj undo

;; [[file:majjik.org::*jj undo][jj undo:1]]
(cl-defun jj-undo ()
  (interactive)
  (jj-cmd-sync `("undo")))
;; jj undo:1 ends here

;; jj new

;; [[file:majjik.org::*jj new][jj new:1]]
(cl-defun jj-new (&key rev before after message no-edit)
  (when (and rev (or before after))
    (user-error "cannot supply REV with BEFORE or AFTER"))
  (unless (or rev before after)
    (user-error "must supply at least one of REV, BEFORE, or AFTER"))
  (jj-cmd-sync `("new"
                 ,@(jj--if-arg rev #'identity "-r")
                 ,@(jj--if-arg before #'identity "--before")
                 ,@(jj--if-arg after #'identity "--after")
                 ,@(jj--if-arg no-edit nil "--no-edit")
                 ,@(jj--if-arg message #'identity "-m"))))

(cl-defun jj-new-on-dwim (parents-revset &key message no-edit)
  "Create a new commit after the chosen PARENTS-REVSET, with no children."
  (interactive (list (jj-get-revset-dwim "parent revs: ")))
  (jj-new :rev parents-revset :message message :no-edit no-edit))

(cl-defun jj-new-before-dwim (children-revset &key message no-edit)
  "Insert a new commit before the chosen CHILDREN-REVSET, and after its parents."
  (interactive (list (jj-get-revset-dwim "child revs: ")))
  (jj-new :before children-revset :message message :no-edit no-edit))

(cl-defun jj-new-after-dwim (parents-revset &key message no-edit)
  "Insert a new commit after the chosen PARENTS-REVSET, and before its children."
  (interactive (list (jj-get-revset-dwim "parent revs: ")))
  (jj-new :after parents-revset :message message :no-edit no-edit))

(cl-defun jj-new-insert (children-revset parents-revset &key message no-edit)
  "Insert a new commit before the chosen CHILDREN-REVSET, and after the chosen PARENTS-REVSET."
  (interactive (list (jj-read-revset "parent revs: ")
                     (jj-read-revset "child revs: ")))
  (jj-new :before children-revset :after parents-revset :message message :no-edit no-edit))
;; jj new:1 ends here

;; jj edit

;; [[file:majjik.org::*jj edit][jj edit:1]]
(cl-defun jj-edit-dwim (rev &optional ignore-immutable)
  (interactive (list (jj-get-revset-dwim "edit: ")))
  (jj-cmd-sync `("edit"
                 "-r" ,rev
                 ,@(jj--if-arg ignore-immutable nil "--ignore-immutable"))))
;; jj edit:1 ends here

;; jj desc

;; [[file:majjik.org::*jj desc][jj desc:1]]
(defun jj-desc-dwim (revset message)
  (interactive (list (jj-get-revset-dwim "revs to describe: ")
                     (read-string "message: ")))
  (jj-cmd-sync `("describe"
                 "-r" ,revset
                 "-m" ,message)))
;; jj desc:1 ends here

;; jj drop

;; [[file:majjik.org::*jj drop][jj drop:1]]
(defun jj-drop-dwim (revset &optional noconfirm)
  (interactive (list (jj-get-revset-dwim "revs to abandon: ")))
  (unless (or noconfirm (yes-or-no-p (format "abandon %s?" revset)))
    (user-error "cancelled"))
  (jj-cmd-sync `("abandon"
                 "-r" ,revset)))
;; jj drop:1 ends here

;; jj git init

;; [[file:majjik.org::*jj git init][jj git init:1]]
(defun jj-git-init (root-dir colocate)
  (interactive (list (expand-file-name (read-directory-name "repository root: "))
                     (yes-or-no-p "colocated repository?")))
  (jj-cmd-sync `("git" "init"
                 ,@(jj--if-arg colocate nil "--colocate")
                 "--" ,root-dir)
               :no-revert)
  (jj-dash root-dir))
;; jj git init:1 ends here

;; new

;; [[file:majjik.org::*new][new:1]]
(defun jj-bookmark-new-dwim (bookmark rev)
  "Create new BOOKMARK pointing at revision REV."
  (interactive (list (read-string "New bookmark: ")
                     (jj-get-revision-dwim "At rev: ")))
  (jj-cmd-sync `("bookmark" "create" ,bookmark
                 "-r" ,rev)))
;; new:1 ends here

;; move

;; [[file:majjik.org::*move][move:1]]
(defun jj-bookmark-move-dwim (bookmark to-rev &optional allow-backwards)
  "Move BOOKMARK to point to revision TO-REV. If used with a prefix arg, allow the bookmark to move backwards or sideways."
  (interactive (list (completing-read "Move bookmark: " (jj-list-bookmarks))
                     (jj-get-revision-dwim "move to: ")
                     current-prefix-arg))
  (jj-cmd-sync `("bookmark" "move" ,bookmark
                 "--to" ,to-rev
                 ,@(jj--if-arg allow-backwards nil "--allow-backwards"))))
;; move:1 ends here

;; rename

;; [[file:majjik.org::*rename][rename:1]]
(defun jj-bookmark-rename (bookmark new-name)
  "Create new BOOKMARK pointing at revision REV."
  (interactive (list (completing-read "Rename bookmark: " (jj-list-bookmarks))
                     (read-string "New name: ")))
  (jj-cmd-sync `("bookmark" "rename" ,bookmark ,new-name)))
;; rename:1 ends here

;; delete

;; [[file:majjik.org::*delete][delete:1]]
(defun jj-bookmark-delete (bookmark &optional noconfirm)
  "Delete BOOKMARK."
  (interactive (list (completing-read "Rename delete: " (jj-list-bookmarks))
                     current-prefix-arg))
  (unless (or noconfirm (yes-or-no-p (format "delete bookmark %s?" bookmark)))
    (user-error "cancelled"))
  (jj-cmd-sync `("bookmark" "delete" ,bookmark)))
;; delete:1 ends here

;; forget

;; [[file:majjik.org::*forget][forget:1]]
(defun jj-bookmark-forget (bookmark &optional noconfirm)
  "Delete BOOKMARK."
  (interactive (list (completing-read "Rename forget: " (jj-list-bookmarks))
                     current-prefix-arg))
  (unless (or noconfirm (yes-or-no-p (format "forget bookmark %s?" bookmark)))
    (user-error "cancelled"))
  (jj-cmd-sync `("bookmark" "forget" ,bookmark)))
;; forget:1 ends here

;; track

;; [[file:majjik.org::*track][track:1]]
(defun jj-file-track-dwim (file)
  (interactive (list (jj-get-untracked-file-dwim "File to track")))
  (jj-cmd-sync `("file" "track" ,(jj-files-as-fileset file))))
;; track:1 ends here

;; jj squash/amend

;; [[file:majjik.org::*jj squash/amend][jj squash/amend:1]]
(cl-defun jj-squash-down-dwim (rev &optional noconfirm ignore-immutable)
  "Squash changes from REV into its single parent."
  (interactive (list (jj-get-revision-dwim "squash rev: ")
                     nil
                     current-prefix-arg))
  (unless (or noconfirm (yes-or-no-p (format "squash %s into its parent?" rev)))
    (user-error "cancelled"))
  (with-editor
    (jj-cmd-sync `("squash"
                   "-r" ,rev
                   ,@(jj--if-arg ignore-immutable nil "--ignore-immutable")))))

(cl-defun jj-amend-into-dwim (rev &optional noconfirm ignore-immutable)
  "Squash changes from @ into the chosen revision."
  (interactive (list (jj-get-revision-dwim "squash into rev: ")
                     nil
                     current-prefix-arg))
  (unless (or noconfirm (yes-or-no-p (format "squash @ into %s?" rev)))
    (user-error "cancelled"))
  (with-editor
    (jj-cmd-sync `("squash"
                   "--into" ,rev
                   ,@(jj--if-arg ignore-immutable nil "--ignore-immutable")))))
;; jj squash/amend:1 ends here

;; jj git push

;; [[file:majjik.org::*jj git push][jj git push:1]]
(defun jj-git-push ()
  "Push to git in the background."
  (interactive)
  (let* ((repo-dir default-directory)
         (buf (get-buffer-create (format "*jj-git-push: %s*" repo-dir))))
    (prog1
        (with-current-buffer buf
          (jj-inspect-mode)
          (setq-local default-directory repo-dir)
          (let ((inhibit-read-only t))
            ;; erase while respecting narrowing
            (delete-region (point-min) (point-max)))
          (let* ((buf (current-buffer))
                 (err (generate-new-buffer "*jj-git-push-stderr*"))
                 (sentinel (make-jj-callback-sentinel
                            (lambda (ok)
                              (when ok
                                (jj-revert-dash-buffer-async repo-dir)
                                (message "push ok")))
                            err))
                 (filter (make-sticky-process-filter :sticky)))
            (make-process
             :name "jj-git-push"
             :buffer buf
             :stderr err
             :filter filter
             :sentinel sentinel
             :noquery t
             :command `("jj" "git" "push"
                        ,@jj-global-default-args)))))))
;; jj git push:1 ends here

;; jj git fetch

;; [[file:majjik.org::*jj git fetch][jj git fetch:1]]
(defun jj-git-fetch ()
  "Fetch from git in the background."
  (interactive)
  (let* ((repo-dir default-directory)
         (buf (get-buffer-create (format "*jj-git-fetch: %s*" repo-dir))))
    (prog1
        (with-current-buffer buf
          (jj-inspect-mode)
          (setq-local default-directory repo-dir)
          (let ((inhibit-read-only t))
            ;; erase while respecting narrowing
            (delete-region (point-min) (point-max)))
          (let* ((buf (current-buffer))
                 (err (generate-new-buffer "*jj-git-fetch-stderr*"))
                 (sentinel (make-jj-callback-sentinel
                            (lambda (ok)
                              (when ok
                                (jj-revert-dash-buffer-async repo-dir)
                                (message "fetch ok")))
                            err))
                 (filter (make-sticky-process-filter :sticky)))
            (make-process
             :name "jj-git-fetch"
             :buffer buf
             :stderr err
             :filter filter
             :sentinel sentinel
             :noquery t
             :command `("jj" "git" "fetch"
                        ,@jj-global-default-args)))))))
;; jj git fetch:1 ends here

;; Provide

;; [[file:majjik.org::*Provide][Provide:1]]
(provide 'majjik)
;; Provide:1 ends here
