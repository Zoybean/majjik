;;; majjik.el --- Magit-inspired jujutsu interface for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Zoey Hewll

;; Author: Zoey Hewll <zoeyhewll@gmail.com>
;; Created: 02 Jan 2026
;; Version: 0.1.0
;; Keywords: vc
;; URL: https://github.com/Zoybean/majjik
;; Package-Requires: (dash s eieio with-editor magit-section promise transient llama)

;;; Commentary:

;;; Code:

;; Require

;; [[file:majjik.org::*Require][Require:1]]
(require 'dash)
(require 's)
(require 'eieio)
(require 'with-editor)
(require 'promise)
(require 'transient)
(require 'llama)
(require 'string-edit)
(require 'magit-section)
;; Require:1 ends here

;; collect-repeat

;; [[file:majjik.org::*collect-repeat][collect-repeat:1]]
(defmacro collect-repeat (&rest body)
  "Evaluate BODY repeatedly until it returns nil. Return the list of non-nil values."
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

;; error-context

;; [[file:majjik.org::*error-context][error-context:1]]
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

(defmacro with-error-label (name &rest body)
  "If BODY signals an error condition, reformat the original message by prepending a NAME to the original message."
  (declare (indent 1))
  `(with-error-format ,(cond ((stringp name) (concat name ": %s"))
                             (t `(concat ,name ": %s")))
     ,@body))
;; error-context:1 ends here

;; let-match

;; [[file:majjik.org::*let-match][let-match:1]]
(defmacro let-match (spec-list &rest body)
  "Bind SPEC-LIST to the numbered matching groups, and execute BODY.

SPEC-LIST is of a form similar to let.  For example:
  ((VAR1 GROUP1)
   (VAR2)
   VAR3
   (VAR4 GROUP4))

Where each VAR is the local variable, and each GROUP is a numeric literal or variable specifying a matching group in the current match data. If any GROUP is nil or unspecified, count up from the last number, or from 1."
  (declare (indent 1))
  `(let ,(cl-loop for last-num = 0 then num
                  for spec in spec-list
                  for (name num) = (pcase spec
                                     (`(,name ,num)
                                      spec)
                                     ((or `(,name) name)
                                      `(,name ,(1+ last-num))))
                  collect `(,name (match-string ,num)))
     ,@body))

(defmacro let-match-string (spec-list string &rest body)
  "Bind SPEC-LIST to the numbered matching groups in STRING, and execute BODY.

SPEC-LIST is of a form similar to let.  For example:
  ((VAR1 GROUP1)
   (VAR2)
   VAR3
   (VAR4 GROUP4))

Where each VAR is the local variable, and each GROUP is a numeric literal or variable specifying a matching group in the current match data. If any GROUP is nil or unspecified, count up from the last number, or from 1."
  (declare (indent 2))
  `(let ,(cl-loop for last-num = 0 then num
                  for spec in spec-list
                  for (name num) = (pcase spec
                                     (`(,name ,num)
                                      `(,name ,(or num (1+ last-num))))
                                     ((or `(,name) name)
                                      `(,name ,(1+ last-num))))
                  collect `(,name (match-string ,num ,string)))
     ,@body))

(ert-deftest jj-test-let-match-string ()
  (should (string-match (rx (group "f" anychar anychar)
                    " "
                    (group (+ (not (any " "))))
                    " "
                    (group "baz"))
                        "foo bar baz"))
  (let-match-string ((foo 1)
                     (bar 2)
                     (baz 3))
      "foo bar baz"
    (should (equal (list foo bar baz)
                   (list "foo" "bar" "baz")))))

(ert-deftest jj-test-let-match-string-elided ()
  (should (string-match (rx (group "f" anychar anychar)
                    " "
                    (group (+ (not (any " "))))
                    " "
                    (group "baz"))
                        "foo bar baz"))
  (let-match-string ((foo)
                     (bar)
                     (baz))
      "foo bar baz"
    (should (equal (list foo bar baz)
                   (list "foo" "bar" "baz")))))

(ert-deftest jj-test-let-match-string-flat ()
  (should (string-match (rx (group "f" anychar anychar)
                    " "
                    (group (+ (not (any " "))))
                    " "
                    (group "baz"))
                        "foo bar baz"))
  (let-match-string (foo
                     bar
                     baz)
      "foo bar baz"
    (should (equal (list foo bar baz)
                   (list "foo" "bar" "baz")))))

(ert-deftest jj-test-let-match-string-disordered ()
  (should (string-match (rx (group "f" anychar anychar)
                    " "
                    (group (+ (not (any " "))))
                    " "
                    (group "baz"))
                        "foo bar baz"))
  (let-match-string ((bar 2)
                     baz ;; same as (baz 3)
                     (foo 1))
      "foo bar baz"
    (should (equal (list foo bar baz)
                   (list "foo" "bar" "baz")))))
;; let-match:1 ends here

;; with-insert-temp-buffer

;; [[file:majjik.org::*with-insert-temp-buffer][with-insert-temp-buffer:1]]
(defmacro with-insert-temp-buffer (&rest body)
  "Spin up a temp buffer, evaluate BODY, and then (if it returned normally) insert the temp buffer's contents at point."
  (cl-with-gensyms (target-buffer content-buffer)
    `(let ((,target-buffer (current-buffer)))
       ;; open a buffer to make a mess in
       ;; we'll insert its contents later
       (with-temp-buffer
         ,@body
         ;; insert the content of this temp buffer into the target buffer
         ;; we can't just return it from the with-temp-buffer block,
         ;; as by that point it's been disposed
         (let ((,content-buffer (current-buffer)))
           (with-current-buffer ,target-buffer
             (insert-buffer-substring ,content-buffer)))))))
;; with-insert-temp-buffer:1 ends here

;; opt

;; [[file:majjik.org::*opt][opt:1]]
(defun opt (&optional item)
  "Helper for splicing optional items into backquoted lists.

Use me with comma-at!"
  (when item `(,item)))
;; opt:1 ends here

;; promise debugging

;; [[file:majjik.org::*promise debugging][promise debugging:1]]
(defmacro pdbg (px)
  (cl-labels ((debug (arrow)
                `(lambda (val)
                   (message ,(concat "%s " arrow " %S") ',px val)
                   val))
              (debug-err (arrow)
                `(lambda (err)
                   (message ,(concat "%s " arrow " %S") ',px err)
                   (promise-reject err))))
    `(promise-then ,px
                   ,(debug "=>")
                   ,(debug-err "=/>"))))
;; promise debugging:1 ends here

;; cartesian product

;; [[file:majjik.org::*cartesian product][cartesian product:1]]
(defmacro prod-cartes (&rest sets)
  "Return the cartesian product of SETS, that is, all the ways to take one element from each SET.
Each SET should be a list."
  (let ((syms (cl-loop for ix from 0 below (length sets)
                       collect (gensym (format "elem-%d-" ix)))))
    (cl-loop for set in sets
             for ix upfrom 0
             for sym in syms
             for first = t then nil
             for form = `(cl-loop for ,sym in ,set
                                  collect (list ,@syms))
             then `(cl-loop for ,sym in ,set
                            nconc ,form)
             finally return form)))
;; cartesian product:1 ends here

;; argument utils

;; [[file:majjik.org::*argument utils][argument utils:1]]
(defun jj--cmd-abbrev (args)
  (mapconcat #'jj--maybe-quote-argument args " "))

(defun jj--maybe-quote-argument (name)
  "If NAME needs quoting, return NAME in double-quotes, with nested double-quotes and backslashes escaped. Otherwise, return it unmodified.
It doesn't need quoting if it is nonempty and composed only of alphanumeric characters, hyphens, and underscores."
  (if (or (string-empty-p name)
          (string-match-p (rx (not (any "a-z" "A-Z" "0-9" "-" "_"))) name))
      (jj--quote-argument name)
    name))

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

;; sync processes

;; [[file:majjik.org::*sync processes][sync processes:1]]
(defun call-cmd (cmd &optional infile destination display noerror)
  "Call CMD via `call-process', with some changes.
If DESTINATION is `:string', include the program output in the return value and any error messages, and ignore DISPLAY.
If NOERROR is nil and the process has a nonzero exit code, signal an error, citing the exit code.
If NOERROR is non-nil, include the exit code in the return value.
When returning both a string result and an exit code, they are returned as a cons (CODE . OUTPUT)."
  (pcase destination
    (:string
     (let ((dir default-directory))
       (with-temp-buffer
         (-let* ((default-directory dir)
                 ((prog . args) cmd)
                 (res (apply #'call-process prog infile t nil args))
                 (out (buffer-string)))
           (if noerror
               (cons res out)
             (unless (= res 0)
               (error "process exited with nonzero exit code %d: %s" res out))
             out)))))
    (_
     (-let* (((prog . args) cmd)
             (res (apply #'call-process prog infile destination display args)))
       (if noerror
           res
         (unless (= res 0)
           (error "process exited with nonzero exit code %d" res)))))))
;; sync processes:1 ends here

;; command-log management 

;; [[file:majjik.org::*command-log management][command-log management:1]]
(define-fringe-bitmap 'jj-fringe-bitmap>
  [#b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000
   #b00000000])

(define-fringe-bitmap 'jj-fringe-bitmapv
  [#b00000000
   #b10000010
   #b11000110
   #b01101100
   #b00111000
   #b00010000
   #b00000000
   #b00000000])

(cl-defstruct jj--process-log-entry
  "The buffers representing the various sections of a jj process' output"
  (process
   nil
   :type process
   :documentation "The process object itself.")
  (name
   nil
   :type string
   :documentation "Description of the process, e.g. the command line used to invoke it.")
  (buf-code
   nil
   :type buffer
   :documentation "Buffer containing only the area where exit code of the process will be written.")
  (buf-stdout
   nil
   :type buffer
   :documentation "Buffer for process standard output.")
  (buf-stderr
   nil
   :type buffer
   :documentation "Buffer for process standard error.")
  (ovl-control
   nil
   :type overlay
   :documentation "Overlay for the area through which the user may interact with the process.")
  (ovl-args
   nil
   :type overlay
   :documentation "Overlay for the default args string, which should be hidden when collapsed.")
  (ovl-collapse
   nil
   :type overlay
   :documentation "Overlay for the area which should be collapsible.")
  (ovl-err
   nil
   :type overlay
   :documentation "Overlay for the process standard error.")
  (verbosity
   nil
   :type '(member critical error user system debug)
   :documentation "Verbosity level for the log entry. User settings can show or hide entries by verbosity."))

(defun jj--make-process-log-entry (name cmd &optional min-cmd hide-args)
  "Return a `jj--process-log-entry' for indirectly writing to the jj log buffer for the current repo. CODE contains the process status info, and STDOUT and STDERR the respective streams. If provided, MIN-CMD is a shorter version of CMD, e.g. omitting majjik's default arguments."
  (let ((repo-dir default-directory))
    (with-current-buffer (jj--get-command-log-buf repo-dir)
      (let ((inhibit-read-only t)
            (display-name (jj--replace-newlines name))
            ;; pretending to be a zero-width-space
            (inv (propertize " " 'display ""))
            ;; real zero-width-space
            (zws "\u200B"))
        (goto-char (point-max))
        (map-let (:start
                  :code 
                  :args
                  :cmd
                  :content
                  :stdout
                  :stderr
                  :end)
            (jj--insert-sectioned
             `(:start
               (:code . "   ")
               "> jj "
               (:args . ,(propertize (jj--replace-newlines
                                      (mapconcat #'shell-quote-argument hide-args " "))
                                     'font-lock-face 'shadow))
               " "
               (:cmd . ,(jj--replace-newlines
                         (mapconcat #'shell-quote-argument cmd " ")))
               "\n"
               :content
               :stdout
               ,inv
               :stderr
               "\n"
               :end))
          (let* ((buf-code (jj--make-narrowed-indirect (format "*code-section %s*" display-name) (car code) (cdr code)))
                 (stdout (jj--make-narrowed-indirect (format "*stdout-section %s*" display-name) stdout stdout))
                 (stderr (jj--make-narrowed-indirect (format "*stderr-section %s*" display-name) stderr stderr))
                 ;; these need to be overlays,
                 ;; so we can toggle their properties as a unit
                 (ovl-args (make-overlay (car args) (cdr args)))
                 (ovl-collapse (make-overlay content end))
                 ;; these all also need to be overlays,
                 ;; so they keep applying as text is added
                 (ovl-control (make-overlay start end)))
            (make-jj--process-log-entry
             :name name
             :buf-code buf-code
             :buf-stdout stdout
             :buf-stderr stderr
             :ovl-control ovl-control
             :ovl-collapse ovl-collapse
             :ovl-args ovl-args)))))))

(defun jj--set-initial-run-status (code-buf)
  (with-current-buffer code-buf
    (let ((inhibit-read-only t))
      (replace-region-contents
       (point-min) (point-max)
       (lambda () (propertize "run" 'font-lock-face '(:foreground "yellow")))))))

(defun jj--make-update-exit-code-sentinel (code-buf)
  "Process sentinel to update the contents of CODE-BUF (a buffer) with the exit status of the process."
  (lambda (proc event)
    (with-current-buffer code-buf
      (let ((inhibit-read-only t))
        (replace-region-contents
         (point-min) (point-max)
         (lambda ()
           (cond ((process-live-p proc)
                  ;; process is still running
                  ;; what's it doing?
                  (propertize
                   (format "%s" (process-status proc))
                   'font-lock-face `(:foreground
                                     "yellow")))
                 (t
                  ;; process is done. how'd it exit?
                  (let ((code (process-exit-status proc)))
                    (propertize
                     (format "%3d" code)
                     'font-lock-face `(:foreground
                                       ,(pcase code
                                          (0 "green")
                                          (_ "red")))))))))))))
;; command-log management:1 ends here

;; sentinels and filters

;; [[file:majjik.org::*sentinels and filters][sentinels and filters:1]]
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
(defun jj--insert-crash-event (stderr-buf event)
  "Insert into STDERR-BUF the final EVENT after the corresponding process crashed."
  (with-current-buffer stderr-buf
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (unless (bolp)
        (insert "\n"))
      (insert
       (propertize (s-chomp event) 'font-lock-face '(:foreground "red"))
       "\n"))))

(defun jj--make-print-status-sentinel (buffer)
  (lambda (proc event)
    (unless (process-live-p proc)
      (when (buffer-live-p buffer)
        (unless (= (process-exit-status proc) 0)
          ;; exited abnormally
          (jj--insert-crash-event buffer event))))))

(defun jj--make-cleanup-sentinel (&rest buffers)
  "Kill BUFFERS if process is no longer live."
  (lambda (proc event)
    (unless (process-live-p proc)
      (mapcar #'kill-buffer buffers))))

(defun make-jj-simple-sentinel (error-buffer &rest other-buffers)
  "Make a simple process sentinel, to insert ERROR-BUFFER's contents if the process ends unexpectedly, then kill it and all OTHER-BUFFERS."
  (let ((status (jj--make-print-status-sentinel error-buffer))
        (cleanup (apply #'jj--make-cleanup-sentinel error-buffer other-buffers)))
    (lambda (proc event)
      (funcall status proc event)
      (funcall cleanup proc event))))

(defun make-jj-callback-sentinel (end-callback)
  "Make a simple process sentinel, to call END-CALLBACK with 2 arguments: the exit code and end event."
  (lambda (proc event)
    (funcall end-callback
             (process-exit-status proc)
             event)))

(defun make-jj-generic-buffered-filter (intermediate-buffer read-next callback)
  "Make a process filter for an arbitrary process.
Every time there is new output, the filter adds it to the INTERMEDIATE-BUFFER, then calls READ-NEXT from the buffer beginning until it returns nil, then calls CALLBACK with the list of new non-nil values, and deletes the text before point (i.e. the text that was read).

READ-NEXT should be a function that reads forward from `point', and moves `point' past whatever has been read.
CALLBACK should be a function of one argument - the list of non-nil values returned by READ-NEXT."
  (lambda (proc string)
    (:documentation (format "This filter adds output to its intermediate buffer, then calls `%s' until it returns nil, then calls `%s' with the list of new non-nil values, and deletes the text before point (i.e. the text that was read)." read-next callback))
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer intermediate-buffer
        (insert (ansi-color-apply string))
        ;; process any new entries
        (save-excursion
          (goto-char (point-min))
          ;; check if we have any new full entries in the buffer
          (when-let ((news (collect-repeat (funcall read-next))))
            ;; delete them and send the list to the callback
            (funcall callback news)
            (delete-region (point-min) (point))))))))
;; sentinels and filters:1 ends here

;; promise utils
;; this might not actually work. I'm big sus actually.

;; [[file:majjik.org::*promise utils][promise utils:1]]
(defun jj--promise-wait-sync (promise)
  "Sit until PROMISE completes, and return:
- (:ok . VAL) if it succeeded
- (:err . REASON) if it failed
- (:fault . DATA) if it had some esoteric failure."
  (unless (promise-class-p promise)
    (error "not a promise: %s" promise))
  (let ((state :run)
        (result))
    (promise-chain promise
      (then (lambda (val)
              (setq state :ok)
              (setq result val)))
      (catch (lambda (err)
               (setq state :err)
               (setq result err)))
      (finally (lambda ()
                 (if (eq state :run)
                     (setq state :fault)))))
    (with-local-quit
      ;; ensure it can't hijack user input
      (while (eq :run state)
        (sit-for 0.1)))
    `(,state . ,result)))

(ert-deftest jj-test-promise-wait-sync ()
  (should (equal '(:ok . foo)
                 (promise-wait-sync
                  (promise-new (lambda (res rej)
                                 (sit-for 0.5)
                                 (funcall res 'foo))))))
  (should (equal '(:ok)
                 (promise-wait-sync
                  (promise-new (lambda (res rej)
                                 (sit-for 0.5)
                                 (funcall res nil))))))
  (should (equal '(:err . bar)
                 (promise-wait-sync
                  (promise-new (lambda (res rej)
                                 (sit-for 0.5)
                                 (funcall rej 'bar))))))
  (should (equal '(:err)
                 (promise-wait-sync
                  (promise-new (lambda (res rej)
                                 (sit-for 0.5)
                                 (funcall rej nil)))))))
;; promise utils:1 ends here

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
      (error "anchored search failed: %s" (jj--quote-argument regexp)))))

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

;; rendering utils

;; [[file:majjik.org::*rendering utils][rendering utils:1]]
(defun erase-accessible-buffer ()
  ;; erase while respecting narrowing
  (delete-region (point-min) (point-max)))

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

(defun jj--entitize-newlines (string)
  "Propertize all newlines in STRING with the corresponding escape glyph, with the `escape-glyph' face."
  (let* ((replacements `(("\n" . "^J")
                         ("\r" . "^M"))))
    (replace-regexp-in-string
     (regexp-opt (mapcar 'car replacements))
     (lambda (it)
       (let ((rep (s--aget replacements it)))
         (propertize it
                     'display rep
                     'face 'escape-glyph
                     'font-lock-face 'escape-glyph)))
     string t t)))

(defun jj--replace-newlines (string)
  "Propertize all newlines in STRING with the corresponding escape glyph, with the `escape-glyph' face."
  (let* ((replacements `(("\n" . "^J")
                         ("\r" . "^M"))))
    (replace-regexp-in-string
     (regexp-opt (mapcar 'car replacements))
     (lambda (it)
       (propertize (s--aget replacements it)
                   'face 'escape-glyph
                   'font-lock-face 'escape-glyph))
     string t t)))

(ert-deftest jj-test-entitize-newlines ()
  (should (string= "foo\r\nbar"
                   (jj--entitize-newlines "foo\r\nbar"))))

;; modified from subr-x.el `add-display-text-property'
(defun push-text-property (start end prop value
                                 &optional object)
  "Push VALUE to the property PROP in the text from START to END.
If any text in the region has a non-nil PROP property, those
properties are retained as the `cdr' of the new property value, with VALUE as the `car'.

If OBJECT is non-nil, it should be a string or a buffer.  If nil,
this defaults to the current buffer."
  (let ((sub-start start)
        (sub-end 0)
        ;; assuming single-threaded, I can just call this once.
        (end (if (stringp object)
                 (min (length object) end)
               (min end (point-max))))
        prop-list)
    (while (< sub-end end)
      ;; step over all the various instances of the PROP property in the range
      (setq sub-end (next-single-property-change sub-start prop object end))
      ;; get the current properties
      (setq prop-list (get-text-property sub-start prop object))
      ;; update the list
      ;; this works even if the old value was not a list, since `push' conses on the front
      (push value prop-list)
      ;; update the property
      (put-text-property sub-start sub-end prop prop-list object)
      (setq sub-start sub-end))))

(defun jj--insert-sectioned (parts &optional insert-func)
  "Insert all of PARTS, which are either strings, pairs (LABEL . STRING), or symbols to label without inserting.
Returns an alist of buffer regions. For each LABEL on a string, the returned alist contains an
entry (LABEL START . END) where START and END are markers to where the
corresponding labeled string starts and ends. For each LABEL on its own, the returned alist contains an
entry (LABEL . POS) where POS is a marker."
  (let ((regions))
    (cl-loop for part in parts
             for ix upfrom 0
             for str = (or (cdr-safe part)
                           (and (stringp part)
                                part))
             for label = (or (car-safe part)
                             (and (symbolp part)
                                  part))
             collect (length str) into lengths
             concat str into combined
             when label
             collect `(,ix  ,str . ,label) into pointers
             finally do
             (progn
               (let ((end))
                 (save-excursion
                   (funcall (or insert-func #'insert) combined)
                   (setq end (point-marker)))
                 (setq regions
                       (cl-loop for len in lengths
                                for ix upfrom 0
                                for (str . name) = (alist-get ix pointers)
                                for r-start = (point-marker)
                                for r-end = (progn
                                              (forward-char len)
                                              (point-marker))
                                when name
                                collect (if str
                                            `(,name ,r-start . ,r-end)
                                          `(,name . ,r-start))))
                 (goto-char end))))
    regions))

(defun jj--make-narrowed-indirect (name start end)
  "Make an indirect buffer named NAME, narrowed to START and END."
  (let ((buf (clone-indirect-buffer
              name nil :norecord)))
    (prog1 buf
      (with-current-buffer buf
        (narrow-to-region start end)))))
;; rendering utils:1 ends here

;; string-edit

;; [[file:majjik.org::*string-edit][string-edit:1]]
(defvar-keymap string-edit-history-mode-map
  "C-c C-p" #'string-edit-history-previous
  "C-c C-n" #'string-edit-history-next)

(define-minor-mode string-edit-history-mode
  "Minor mode for navigating input history within a string-edit prompt.")

(defun string-edit-history--editable-region-start ()
  (save-excursion
    (goto-char (point-min))
    ;; Skip past the help text.
    (text-property-search-forward 'string-edit--prompt)
    (point)))

(defun string-edit-history--current ()
  (buffer-substring (string-edit-history--editable-region-start) (point-max)))

(defun string-edit-history--save-current (mode)
  "Save the editable region as current element and return the new value for MODE.
Assumes the current buffer is a string-edit input buffer."
  (-let (((list pos) mode))
    (setf (nth pos list) (string-edit-history--current))
    `(,list ,pos)))

(defun string-edit-history--offset (mode offset)
  "Replace the editable region with the previous history item and return the new value for MODE.
Assumes the current buffer is a string-edit input buffer."
  (-if-let* (((list pos) mode)
             (npos (+ pos offset))
             (_ (<= 0 npos))
             (current (or (nth npos list)
                          (progn (setq npos pos)
                                 nil))))
      (progn
        (replace-region-contents
         (string-edit-history--editable-region-start)
         (point-max)
         (cl-constantly current))
        `(,list ,npos))
    (beep)
    (message "No more history")
    mode))

(defun string-edit-history-previous ()
  (interactive nil string-edit-history-mode)
  (cl-callf string-edit-history--save-current
      string-edit-history-mode)
  (cl-callf string-edit-history--offset
      string-edit-history-mode 1))

(defun string-edit-history-next ()
  (interactive nil string-edit-history-mode)
  (cl-callf string-edit-history--save-current
      string-edit-history-mode)
  (cl-callf string-edit-history--offset
      string-edit-history-mode -1))

(defun string-edit-history--start (hist-var &optional initial)
  "If HIST-VAR is non-nil, enter `string-edit-history-mode' using VAR for the history."
  (interactive)
  (string-edit-history-mode)
  (setq string-edit-history-mode
        (and hist-var
             (list (cons (or initial "")
                         (seq-copy (symbol-value hist-var)))
                   0))))

(defun read-string-from-buffer-with-history (prompt string hist-var)
  "Switch to a new buffer to edit STRING in a recursive edit.
The user finishes editing with \\<string-edit-mode-map>\\[string-edit-done], or aborts with \\<string-edit-mode-map>\\[string-edit-abort]).

PROMPT will be inserted at the start of the buffer, but won't be
included in the resulting string.  If nil, no prompt will be
inserted in the buffer.

If HIST-VAR is non-nil, it should be a symbol whose value cell
is a list. \\<string-edit-history-mode-map>\\[string-edit-history-next] and \\<string-edit-history-mode-map>\\[string-edit-history-previous] can be used to traverse the
history list in the buffer.

When the user exits recursive edit, this function returns the
edited STRING.

Also see `string-edit'."
  (with-current-buffer
      (jj--string-edit
       prompt
       (or string "")
       (lambda (edited)
         (setq string edited)
         (exit-recursive-edit))
       :abort-callback (lambda ()
                         (throw 'exit "Aborted edit")))
    (string-edit-history--start hist-var string))
  (recursive-edit)
  (when hist-var
    (add-to-history hist-var string))
  string)

(cl-defun jj--string-edit (prompt string success-callback
                                  &key abort-callback)
  "Switch to a new buffer to edit STRING.
When the user finishes editing (with \\<string-edit-mode-map>\\[string-edit-done]), SUCCESS-CALLBACK
is called with the resulting string.

If the user aborts (with \\<string-edit-mode-map>\\[string-edit-abort]), ABORT-CALLBACK (if any) is
called with no parameters.

PROMPT will be inserted at the start of the buffer, but won't be
included in the resulting string.  If PROMPT is nil, no help text
will be inserted.

Also see `read-string-from-buffer'."
  (with-current-buffer (generate-new-buffer "*edit string*")
    (when prompt
      (let ((inhibit-read-only t))
        (insert prompt)
        (ensure-empty-lines 0)
        (add-text-properties (point-min) (point)
                             (list 'intangible t
                                   'face 'string-edit-prompt
                                   'read-only t))
        (insert (propertize (make-separator-line)
                            'read-only t 'rear-nonsticky t))
        (add-text-properties (point-min) (point)
                             (list 'string-edit--prompt t))))
    (save-excursion
      (insert string))

    ;; Use `fit-window-to-buffer' after the buffer is filled with text.
    (pop-to-buffer (current-buffer)
                   '(display-buffer-below-selected
                     (window-height . (lambda (window)
                                        (fit-window-to-buffer window nil 10)))))

    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)
    (string-edit-mode)
    (setq-local string-edit--success-callback success-callback)
    (setq-local string-edit--abort-callback abort-callback)
    (setq-local header-line-format
                (substitute-command-keys
                 "Type \\<string-edit-mode-map>\\[string-edit-done] when you've finished editing or \\[string-edit-abort] to abort"))
    (message "%s" (substitute-command-keys
                   "Type \\<string-edit-mode-map>\\[string-edit-done] when you've finished editing"))
    (current-buffer)))

(define-advice transient--recursive-edit (:override (fn) jj--transient-recursive-edit-advice)
  "Vendor the upstream fix from Transient."
  (transient--debug 'recursive-edit)
  (if (not transient--prefix)
      (funcall fn)
    (transient--suspend-override (bound-and-true-p edebug-active))
    (condition-case err
        (unwind-protect
            (funcall fn)
          (cond
           ((memq this-command '(top-level abort-recursive-edit))
            (setq transient--exitp t)
            (transient--post-exit this-command)
            (transient--delete-window)
            (transient--debug "     abort recursive-edit and menu "))
           (transient--prefix
            (transient--resume-override)
            (transient--debug "     exit recursive-edit and resumed menu"))))
      (error (if (and (eq (car err) 'error)
                      (stringp (cadr err))
                      (string-prefix-p "Abort" (cadr err)))
                 (message "%s" (cadr err))
               (message "transient--recursive-edit: %S" err))))))
;; string-edit:1 ends here

;; Fileset

;; [[file:majjik.org::*Fileset][Fileset:1]]
(defun jj-files-as-fileset (&rest files)
  "Returns a fileset that matches all the given FILES. No globbing is applied, and all are considered relative to cwd."
  (jj-fileset `(or ,@(cl-loop for file in files
                              collect `(:cwd-file ,file)))))

(defun jj--escape-glob (path)
  "Wrap all glob patterns in PATH with square brackets, to prevent their special interpretation in glob-enabled fileset patterns."
  (replace-regexp-in-string (rx (any "*[]?{}"))
                            (lambda (c)
                              (format "[%s]" c))
                            path))

(defun jj-paths-as-fileset (&rest paths)
  "Returns a fileset that matches all the given PATHS, which may be directories (in which case their contents are also matched). No globbing is applied to filenames, and all are considered relative to cwd."
  (jj-fileset `(or ,@(cl-loop for path in paths
                              for escaped = (jj--escape-glob path)
                              collect `(:cwd-prefix-glob ,escaped)))))

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
                  ((or `(form)
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
                  ;; identity operators
                  (`(,(or 'or
                          'and)
                     ,form)
                   ;; trick to only nest in quotes if an outer form requires it
                   ;; outermost caller will unwrap any toplevel list
                   (render form))
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
                  ((or `(,form)
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
                    ((or `(,form)
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
(eval-and-compile
  (defconst jj--count-graph-lines 4
  "Number of lines of graph to sample for each commit in the log output. Fewer lines will give scrappier output, but 4 should be enough for any graph configuration.
I've hardcoded other areas to expect exactly 4, so changing this will not break anything but the output will change slightly, specifically in which lines are considered mandatory."))
(eval-and-compile
  (defconst jj--major-delim "\x1E"
  "Delimiter for separating the graph from the records in the jj-log template.
  By default, this is the ascii record separator character."))
(eval-and-compile
  (defconst jj--delim "\x1F"
  "Delimiter for separating fields in the jj-log template.
  By default, this is the ascii unit separator character."))

(cl-defstruct jj-log-entry
  "A jj log entry with commit info and graph prefixes."
  header
  graph)

(defun make-jj-log-graph (pre node suff &rest graph-prefixes)
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
              ;; no tail, so assume the repeatable list is empty. in `insert-jj-log-entry', assume the last mandatory is repeatable.
              ;; special case used to identify (and print in grey) the truncated marker of fully-separate subtrees
              ;; - this implementation is brittle AF. I really should set the truncation marker and search for it.
              `(,graph-mandatory))
             (illegal (error "I thought that jj graph output would always have at most one run in a set of `jj--count-graph-lines' lines, and that run would be at the end, but apparently not. graph segments: %s" graph-prefixes)))))
    (make--jj-log-graph
     :first-line-prefix pre
     :first-line-node node
     :first-line-suffix suff
     :mandatory-segments graph-mandatory
     :repeatable-segment resolved-tail)))

(cl-defstruct (jj-log-graph (:constructor make--jj-log-graph))
  "Specification for the line-prefixes needed to annotate any number of commit-info lines with the graph for that region of the log."
  first-line-prefix
  first-line-node
  first-line-suffix
  mandatory-segments
  repeatable-segment)

(defvar jj-log-node-template
  (jj-template `(++ ,jj--delim
                    (coalesce 
                     ;; elided 
                     (if !self "~")
                     ;; head
                     (if current_working_copy "@")
                     (if immutable "+")
                     (if conflict "×")
                     ;; anything else
                     "o")
                    ,jj--delim))
  "Node format to ensure log nodes can be parsed.")

(defun jj--make-list-parser (sep)
  "Make a function to parse a list, separated by SEP."
  (lambda (s)
    (s-split sep s :omit)))

(defun jj--make-list-printer (sep)
  "Make a function to format a list in the log, separated by SEP."
  (lambda (vals _header)
    (s-join sep vals)))

(defun jj--parse-json-bool (s)
  "Parse a json-encoded boolean"
  (json-parse-string s :false-object nil))

(defun jj--make-opt-resolver (&optional present-override absent-override)
  "Make a function to resolve an optional item in the log, for use in parsers, faces, or printers. If PRESENT-OVERRIDE is non-nil, it is returned instead of a non-nil value. If ABSENT-OVERRIDE is non-nil, it is returned instead of nil."
  (lambda (val &optional _entry)
    (if val
        (or present-override val)
      absent-override)))

(ert-deftest jj-round-trip-truncate-overflow ()
  (let ((entry (with-temp-buffer
                 
                 (insert "@  puvwmkxr\"zoeyhewll@gmail.com\"2025-12-18 18:07:32f610054a\"bic\\n*big\\nmultiline\\nmessage\\nwith\\nhonestly,\\ntoo much \\ntext\\nright here\\n\"\n│  \n~  \n   \n   \n")
                 (goto-char (point-min))
                 (read-jj-log-entry))))
    (should (equal entry
                   #s(jj-log-entry
                      #s(jj-log-header "puvwmkxr"
                                   "zoeyhewll@gmail.com"
                                   "2025-12-18 18:07:32"
                                   ""
                                   ""
                                   ""
                                   "f610054a"
                                   ""
                                   "bic\n*big\nmultiline\nmessage\nwith\nhonestly,\ntoo much \ntext\nright here\n")
                      #s(jj-log-graph "" "@" "  "
                                  ("│  "
                                   "~  "
                                   "   ")
                                  nil))))
    (with-temp-buffer
      (insert-jj-log-entry entry)
      (should (string= (substring-no-properties (buffer-string))
                       "@  puvwmkxr zoeyhewll@gmail.com 2025-12-18 18:07:32 f610054a\n│  bic\n~  *big\n   multiline\n   message\n   with\n   honestly,\n   too much \n   text\n   right here\n"
                       )))))

(ert-deftest jj-round-trip-truncate-underflow ()
  (let ((entry (with-temp-buffer
                 (insert "@  puvwmkxr\"zoeyhewll@gmail.com\"2025-12-18 18:07:32f610054a\"\"\n│  \n~  \n   \n   \n")
                 (goto-char (point-min))
                 (read-jj-log-entry))))
    (should (equal entry
                   #s(jj-log-entry
                      #s(jj-log-header "puvwmkxr"
                                   "zoeyhewll@gmail.com"
                                   "2025-12-18 18:07:32"
                                   ""
                                   ""
                                   ""
                                   "f610054a"
                                   ""
                                   "")
                      #s(jj-log-graph "" "@" "  "
                                  ("│  "
                                   "~  "
                                   "   ")
                                  nil))))
    (with-temp-buffer
      (insert-jj-log-entry entry)
      (should (string= (substring-no-properties (buffer-string))
                       "@  puvwmkxr zoeyhewll@gmail.com 2025-12-18 18:07:32 f610054a\n│  \n~  \n   \n")))))

(ert-deftest jj-round-trip-simple-underflow ()
  (let ((entry (with-temp-buffer
                 (insert "@  puvwmkxr\"zoeyhewll@gmail.com\"2025-12-18 18:07:32f610054a\"\"\n│  \n│  \n│  \n")
                 (goto-char (point-min))
                 (read-jj-log-entry))))
    (should (equal entry
                   #s(jj-log-entry
                      #s(jj-log-header "puvwmkxr"
                                   "zoeyhewll@gmail.com"
                                   "2025-12-18 18:07:32"
                                   ""
                                   ""
                                   ""
                                   "f610054a"
                                   ""
                                   "")
                      #s(jj-log-graph "" "@" "  "
                                  ()
                                  "│  "))))
    (with-temp-buffer
      (insert-jj-log-entry entry)
      (should (string= (substring-no-properties (buffer-string))
                       "@  puvwmkxr zoeyhewll@gmail.com 2025-12-18 18:07:32 f610054a\n")))))
;; Log format:1 ends here

;; utils

;; [[file:majjik.org::*utils][utils:1]]
(cl-defmacro with-stream-to-string ((stream) &body body)
  (unless (symbolp stream)
    (user-error "stream variable must be a symbol, instead got %S" stream))
  `(let ((,stream (generate-new-buffer ,(format " *string-output-%s*" stream) t)))
     (unwind-protect
         (progn
           ,@body
           (with-current-buffer ,stream (buffer-string)))
       (kill-buffer ,stream))))

(defun prc (x)
  "Format X to string as per `princ'."
  (with-stream-to-string (s)
                         (princ x s)))

(defun pr1 (x)
  "Format X to string as per `prin1'."
  (with-stream-to-string (s)
                         (prin1 x s)))

(defmacro push-errors (list &rest body)
  (cl-with-gensyms (e)
    `(condition-case ,e (progn ,@body)
       (error (push ,e ,list)
              nil))))
;; utils:1 ends here

;; plumbing

;; [[file:majjik.org::*plumbing][plumbing:1]]
(defun jj-read-elided ()
  (jj--re-step-over (rx (group "(elided revisions)")
                        "\n"))
  (match-string 1))

(defun jj-read-graph-and-maybe-elided ()
  (let ((errors))
    (or (push-errors errors (with-error-label
                                "read log node"
                              (jj-read-graph-and-entry)))
        (push-errors errors (with-error-label
                                "read elided revisions"
                              (jj-read-graph-and-elided)))
        (error "Line is not a recognised part of a jj log. %s: %s"
               (nreverse errors)
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

(defun jj-read-graph-and-elided ()
  (prog1 (with-error-label
             "read elided graph"
           (save-excursion (jj-parse-erase-elided-prefix)))
    (jj-read-elided)))

(defun jj-read-graph-and-entry ()
  (make-jj-log-entry :graph (with-error-label
                                "read log graph"
                              (save-excursion (jj-parse-erase-graph-prefix)))
                     :header (with-error-label
                                 "read log entry"
                               (prog1
                                   (read-jj-log-header)
                                 (jj--re-step-over "\n")))))

(defun jj-parse-erase-graph-prefix ()
  "Strip the graph prefix of the node starting at point. Return the graph as a vector of the form [[PRE NODE POST] . TAIL] where PRE, NODE, and POST are strings representing the 3 parts of the first line, and TAIL is a sequence of `jj--count-graph-lines' strings for the subsequent lines.
If the line is an elided entry, returns a single string, which is the prefix before the "
  (let ((1st)
        (rest)
        (regions))
    (with-error-label "1st graph line"
      (jj--re-step-over (rx
                         (group (* (not control)))
                         (literal jj--delim)
                         (group (* (not control)))
                         (literal jj--delim)
                         (group (* (not control)))
                         (literal jj--major-delim))))
    (let-match (pre node suf)
      (setq 1st `(,pre ,node ,suf)))
    (push (list (match-beginning 0) (match-end 0)) regions)
    ;; skip over arbitrary content here.
    (forward-line 1)
    (with-error-label "graph tail"
      (cl-loop for x from 1 below jj--count-graph-lines
               ;; skip counting the first, which we matched just now
               ;; all the tail lines are empty besides the graph prefix
               do (progn
                    (jj--re-step-over (rx (group (* (not control)))
                                          (literal jj--major-delim)
                                          "\n"))
                    (push (match-string 1) rest)
                    (push (list (match-beginning 0) (match-end 0)) regions))))
    (prog1
        (apply #'make-jj-log-graph `(,@1st ,@(nreverse rest)))
      ;; delete the matched regions at the end
      ;; so if we're not ready, it's ok.
      (cl-loop for reg in regions
               do (apply #'delete-region reg)))))

(defun jj-parse-erase-elided-prefix ()
  (let ((graph-pre)
        (graph-node)
        (graph-suf)
        (graph-tail)
        (regions))
    (jj--re-step-over (rx line-start
                          (group (* (not control)))
                          (literal jj--delim)
                          (group (* (not control)))
                          (literal jj--delim)
                          (group (* (not control)))
                          "(elided revisions)"
                          "\n"))
    (let-match (pre node suf)
      (setq graph-pre pre
            graph-node node
            graph-suf suf))
    (push (list (match-beginning 0) (match-end 3)) regions)
    (cl-loop while (jj--re-step-over (rx (group
                                          line-start
                                          (* (not control))
                                          line-end)
                                         "\n")
                                     nil :no-err)
             do (progn
                  (push (match-string 1) graph-tail)
                  (push (list (match-beginning 0) (match-end 0)) regions)))
    ;; assumption: an elided revision is never the last entry in a log
    ;; so expect the next graph section before we cut anything.
    ;; this handles streaming data.
    (should (looking-at (rx
                         (* anychar)
                         (literal jj--major-delim))))
    (prog1
        (make--jj-log-graph :first-line-prefix graph-pre
                            :first-line-node graph-node
                            :first-line-suffix graph-suf
                            :mandatory-segments graph-tail)
      (cl-loop for reg in regions
               do (apply #'delete-region reg)))))

(defun insert-jj-log-elided (graph)
  "Insert the GRAPH and an \"elided revisions\" label, formatted as a jj log entry."
  (with-insert-temp-buffer
   (insert (propertize "(elided revisions)\n" 'font-lock-face '(:foreground "grey")))
   ;; insert the mandatory graph segments
   ;; these will add new lines if there arent enough already
   (cl-loop initially (progn
                        (goto-char (point-min))
                        (insert (jj-log-graph-first-line-prefix graph)
                                (propertize (jj-log-graph-first-line-node graph)
                                            'font-lock-face '(:foreground "grey"))
                                (jj-log-graph-first-line-suffix graph))
                        (forward-line 1))
            for prefix in (jj-log-graph-mandatory-segments graph)
            do (progn
                 (insert prefix)
                 (forward-line 1)
                 ;; while there's more mandatory graph segments, put them on new lines if you have to
                 (unless (bolp)
                   (insert "\n"))))))

(defun insert-jj-log-graph-prefix (graph)
  "Add the GRAPH prefix to the entry starting at point. assume that the buffer is narrowed so as to end at the end of the entry."
  ;; insert the mandatory graph prefix segments
  ;; these will add new lines if there arent enough already
  (cl-loop initially (progn
                       (insert (jj-log-graph-first-line-prefix graph)
                               (propertize (jj-log-graph-first-line-node graph)
                                           'font-lock-face '(:foreground "cyan"))
                               (jj-log-graph-first-line-suffix graph))
                       (forward-line 1))
           for (prefix . rest) on (jj-log-graph-mandatory-segments graph)
           do (progn
                (cond ((and
                        ;; last nonempty mandatory segment
                        (not (cdr rest))
                        (not (string= "" (string-trim prefix)))
                        ;; no repeatable segments
                        (not (jj-log-graph-repeatable-segment graph)))
                       (insert (propertize prefix 'font-lock-face '(:foreground "grey"))))
                      (t
                       (insert prefix)))
                (forward-line 1)
                ;; while there's more mandatory graph segments, put them on new lines if you have to
                (unless (bolp)
                  (insert "\n"))))
  ;; insert the repeatable graph prefix segments
  ;; these are added to all remaining lines, but no new lines are added
  (cl-loop with tail = (or (jj-log-graph-repeatable-segment graph)
                           (car (last (jj-log-graph-mandatory-segments graph))))
           while (and (bolp)
                      (not (eobp)))
           do (progn (insert tail)
                     (forward-line 1))))

(defclass jj-log-entry-section (magit-section)
  ((data :initarg :data)))

(defun insert-jj-graph-log-maybe-elided (entry)
  (pcase entry
    ((pred jj-log-graph-p)
     (magit-insert-section sec
       (elided)
       (magit-insert-heading (with-temp-buffer
                               (insert-jj-log-elided entry)
                               (s-chomp (buffer-string))))))
    ((pred jj-log-entry-p)
     (let ((header (jj-log-entry-header entry)))
       (-let (((line-0 line-1 . rest)
               (s-split "\n" (with-temp-buffer
                               (save-excursion (insert-jj-log-header header))
                               (insert-jj-log-graph-prefix (jj-log-entry-graph entry))
                               (add-text-properties (point-min) (point-max) `(jj-object ,header))
                               (s-chomp (buffer-string))))))
         (jj-insert-log-entry-section entry line-0 line-1 rest))))))

(defun jj-insert-log-entry-section (entry line-0 &optional line-1 rest)
  (magit-insert-section sec
    (jj-log-entry-section entry :collapsed)
    (oset sec data entry)
    (magit-insert-heading (concat (s-join "\n" `(,line-0 ,@(opt line-1)))
                                  "\n"))
    (when rest
      (magit-insert-section-body
        (dolist (line rest)
          (insert line "\n"))))))
;; plumbing:1 ends here

;; formats

;; [[file:majjik.org::*formats][formats:1]]
(defmacro define-jj-format (type-name &rest field-specs)
  "Define the format to be used for parsing and formatting various jj output.
Accepts a list of FIELDS in the form (FIELD-NAME . PLIST), where PLIST accepts the following keys:
- `:form' specifies the sexpression used to produce the field's log template, produced with `jj-template'. (So far there's no way to use a string template directly)
- `:parser' specifies how to read the data in to lisp.
- `:printer' specifies how to print the data out to a buffer. It should be a function of 2 arguments, with the first being the field and the second the entire structure.
- `:face' specifies the face to use for formatting this entry in the log buffer. This is applied to the result of PRINTER if supplied.
- `:separator' the separator to insert before this field, rather than a space (or empty for the first field). Only inserted if the value is present."
  (declare (indent 1))
  (let ((fields (-filter (-lambda ((name . rest)) name)
                         field-specs)))
    `(progn
       (require 'json)
       (define-short-documentation-group ,(intern (format "jj-%s" type-name))
         (define-jj-format
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
       ;; (defvar ,(intern (format "jj-%s-regex" type-name))
       ;;   ,(let* ((content `(not (any ,jj--delim ,jj--major-delim "\r\n"))))
       ;;      `(rx line-start
       ;;           (seq
       ;;            ,(format "%s" type-name)
       ;;            ;; struct delimiter
       ;;            ,jj--major-delim
       ;;            ;; struct elements
       ;;            (* ,content)
       ;;            (= ,(1- (length fields))
       ;;               ,jj--delim
       ;;               (* ,content))
       ;;            "\n")))
       ;;   ,(format "Regex to match a full `jj-%1$s' entry. This *should* match exactly the same content that `read-jj-%1$s' will parse." type-name))
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
            `(cl-loop initially (with-error-label ,(format "read struct label %s" type-name)
                                  (jj--re-step-over (rx ,(format "%s" type-name) ,jj--major-delim)))
                      for (field-name key parser) in (list ,@(cl-loop for (field-name . props) in fields
                                                                      for key = (intern (format ":%s" field-name))
                                                                      for parser = (plist-get props :parser)
                                                                      collect `(list ',field-name ,key ,parser)))
                      ;; no delimiter for first field
                      for first = t then nil
                      for field-rx = (rx (group (* ,content))) then (rx ,jj--delim (group (* ,content)))
                      when (with-error-label (format "read field %s" field-name)
                             (jj--re-step-over field-rx))
                      for parsed = (if parser
                                       (save-match-data
                                         (funcall parser (match-string 1)))
                                     (match-string 1))
                      nconc `(,key ,parsed)
                      into struct-props
                      finally return (apply #',(intern (format "make-jj-%s" type-name)) struct-props))))
       (defun ,(intern (format "insert-jj-%s" type-name)) (entry)
         ,(format "Insert the ENTRY, formatted as a `jj-%s' entry." type-name)
         ,(cl-labels ((field (name-sym)
                        `(,(intern (format "jj-%s-%s" type-name name-sym))
                          entry)))
            `(with-insert-temp-buffer
              (cl-loop for (field-name val printer face first sep) in (list ,@(cl-loop  
                                                                               for (field-name . props) in field-specs
                                                                               for first = (if-let ((c (plist-member props :first)))
                                                                                               ;; if it's explicitly set (even nil), use that
                                                                                               (car c)
                                                                                             ;; otherwise, it's the first, so set it
                                                                                             t)
                                                                               ;; otherwise, it's not first, so unset it
                                                                               then (plist-get props :first)
                                                                               for sep = (plist-get props :separator)
                                                                               for face = (plist-get props :face)
                                                                               for printer = (plist-get props :printer)
                                                                               collect `(list
                                                                                         ',field-name
                                                                                         ,(and field-name (field field-name))
                                                                                         ,printer
                                                                                         ,face
                                                                                         ,first
                                                                                         ,sep)))
                       with effective-first
                       do (when first
                            (setq effective-first t))
                       for sep = (or sep
                                     (cond
                                      (effective-first "")
                                      (t " ")))
                       for face = (cond ((functionp face)
                                         (funcall face val entry))
                                        (:else
                                         face))
                       do (when-let ((printed (s-presence
                                               (if printer
                                                   (funcall printer val entry)
                                                 val))))
                            (setq effective-first nil)
                            (insert sep (apply #'propertize
                                               `(,printed
                                                 help-echo ,(symbol-name field-name)
                                                 ,@(jj--if-arg face #'identity 'font-lock-face))))))
              ;; ensure commit text ends on a newline
              (unless (bolp)
                (insert "\n"))
              ;; add field to all the commit text (including newlines)
              ;; pointing to the entry struct
              (add-text-properties (point-min) (point-max) `(jj-object ,entry)))))
       (cl-defstruct ,(intern (format "jj-%s" type-name))
         ;; semantic fields
         ,@(cl-loop for (field-name . props) in fields
                    collect field-name)))))

(define-jj-format log-header
  (change-id-min
   :face '(ansi-color-bold (:foreground "light pink"))
   :form (:chain self (.change_id) (.shortest 8) (.prefix)))
  (change-id-tail
   :separator ""
   :face '(:foreground "dim gray")
   :form (:chain self (.change_id) (.shortest 8) (.rest)))
  (change-offset
   :printer (lambda (off _ent)
              (unless (string= "0" off)
                off))
   :form (:chain self (.change_offset)))
  (change-id
   :printer (cl-constantly nil)
   :form (:chain self (.change_id) (.short 16)))
  (author
   :face '(:foreground "gold")
   :parser #'json-parse-string
   :form (:chain self (.author) (.email) (stringify) (.escape_json)))
  (timestamp
   :face '(:foreground "dark turquoise")
   :form (:chain self (.committer) (.timestamp) (.local) (.format "%Y-%m-%d %H:%M:%S")))
  (bookmarks
   :face '(:foreground "medium orchid")
   :parser (jj--make-list-parser " ")
   :printer (jj--make-list-printer " ")
   :form (:chain self (.bookmarks)))
  (tags
   :face '(:foreground "goldenrod")
   :parser (jj--make-list-parser " ")
   :printer (jj--make-list-printer " ")
   :form (:chain self (.tags)))
  (working-copies
   :face '(:foreground "green")
   :parser (jj--make-list-parser " ")
   :printer (jj--make-list-printer " ")
   :form (:chain self (.working_copies)))
  (commit-id-min
   :face '(ansi-color-bold (:foreground "dodger blue"))
   :form (:chain self (.commit_id) (.shortest 8) (.prefix)))
  (commit-id-tail
   :separator ""
   :face '(:foreground "dim gray")
   :form (:chain self (.commit_id) (.shortest 8) (.rest)))
  (commit-id
   :printer (cl-constantly nil)
   :form (:chain self (.commit_id) (.short 16)))
  (conflict
   :face '(:foreground "red")
   :parser #'s-presence
   ;; :printer (jj--make-opt-resolver "conflict")
   :form (if (:chain self (.conflict)) "conflict"))
  (current-working-copy
   ;; not sure why, but for now adding this entry seems to break everything.
   :parser (-compose (jj--make-opt-resolver :current-wc) #'s-presence)
   :printer (cl-constantly nil)
   :form (if (:chain self (.current_working_copy)) "@"))
  (nil
   ;; empty non-field element for splitting the header from the desc and empty markers
   :parser #'s-presence
   :printer (cl-constantly "\n"))
  (empty
   :first t
   :face '(ansi-color-bold (:foreground "medium sea green"))
   :parser (-compose (jj--make-opt-resolver :empty) #'s-presence)
   :printer (jj--make-opt-resolver "(empty)")
   :form (if (:chain self (.empty)) "empty"))
  (description
   :parser (lambda (s)
             (s-presence (json-parse-string s)))
   :face (lambda (desc ent)
           (cond (desc
                  ;; present description is unformatted
                  nil)
                 ((jj-log-header-empty ent)
                  ;; if both commit and desc are empty, they're both green
                  '(ansi-color-bold (:foreground "medium sea green")))
                 (:else
                  ;; if just desc is empty, it's gold
                  '(:foreground "gold"))))
   :printer (jj--make-opt-resolver
             nil
             "(no description)")
   :form (:chain self (.description) (.escape_json))))


(defun jj-augment-template-for-graph (edge-delim tail-lines log-template)
  (jj-template `(++ ,edge-delim
                    (:lit ,log-template)
                    ;; these lines only exist to get the shape of the graph
                    ;; there's already one with the header line, so count 1 fewer
                    ,@(cl-loop for n upfrom 1 below tail-lines
                               append `("\n" ,edge-delim)))))

(defun jj-parseable-template (self)
  (jj-augment-template-for-graph jj--major-delim jj--count-graph-lines (jj-log-header-template self)))
;; formats:1 ends here

;; tests

;; [[file:majjik.org::*tests][tests:1]]
(ert-deftest jj-test-header-log-full-parsing ()
  (with-temp-buffer
    (save-excursion
      (insert "@    log-headeruorrwztk\"zoeyhewll@gmail.com\"2026-02-22 14:23:10f831a9edconflictempty\"\"
├─╮  
│ │  
│ │  
│ o  log-headertqqxztyu\"zoeyhewll@gmail.com\"2025-12-24 18:48:22main??1ae95f23\"long\\nmultiline\\nmessage\\nhere\\nand\\nhere\\n\"
│ │  
│ │  
│ │  
× │  log-headerzsrpuxsq/0\"zoeyhewll@gmail.com\"2026-01-11 18:58:04d4b4e2fcconflict\"diverge 2\\n\"
│ │  
│ │  
│ │  
│ │ o  log-headerzsrpuxsq/9\"zoeyhewll@gmail.com\"2025-12-29 22:14:42c33a9601\"diverge 1\\n\"
├───╯  
│ │    
│ │    
× │    log-headerznpwrszt\"zoeyhewll@gmail.com\"2025-12-29 22:14:42a2fd03adconflictempty\"\"
├───╮  
│ │ │  
│ │ │  
│ │ o  log-headeruwoorszk\"zoeyhewll@gmail.com\"2025-12-29 22:14:353c9908cd\"foo\\n\"
│ │ │  
│ │ │  
│ │ │  
o │ │  log-headernqzyvomm\"zoeyhewll@gmail.com\"2025-12-29 22:14:420c0b58a0\"foo\\n\"
│ │ │  
│ │ │  
│ │ │  
o │ │  log-headeroqnyxnnn\"zoeyhewll@gmail.com\"2025-12-26 23:28:3452c23b7fempty\"foo sample commit to edit\\n\"
├───╯  
│ │    
│ │    
+ │  log-headerwuvynqqs\"zoeyhewll@gmail.com\"2025-03-26 14:08:11main?? main@originba86ecc3\"add basic restart-case and handler-case\\n\"
│ │  
│ │  
│ │  
! │  (elided revisions)
├─╯
│ o  log-headerzwxrqwwv\"zoeyhewll@gmail.com\"2025-12-21 22:36:01foo087ec1fbempty\"\"
├─╯  
│    
│    
+  log-headerzzzzzzzz\"\"1970-01-01 08:00:0000000000empty\"\"
   
   
   
"))
    (should (equal (cl-loop while (not (eobp))
                            for n = (let ((errors))
                                      (or (push-errors errors (list :entry (with-error-label
                                                                               "read log node"
                                                                             (jj-read-graph-and-entry))))
                                          (push-errors errors (list :elided (with-error-label
                                                                                "read elided revisions"
                                                                              (jj-read-graph-and-elided))))
                                          (error "Line is not a recognised part of a jj log. %s: %s"
                                                 (nreverse errors)
                                                 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                            while n
                            collect n)
                   '((:entry #s(jj-log-entry
                      #s(jj-log-header "uorrwztk" "zoeyhewll@gmail.com" "2026-02-22 14:23:10" nil nil nil "f831a9ed" "conflict" "empty" nil)
                      #s(jj-log-graph "" "@" "    "
                       ("├─╮  ") "│ │  ")))
                     (:entry #s(jj-log-entry
                      #s(jj-log-header "tqqxztyu" "zoeyhewll@gmail.com" "2025-12-24 18:48:22"
                                      ("main??")
                                      nil nil "1ae95f23" nil nil "long
multiline
message
here
and
here
")
                      #s(jj-log-graph "│ " "o" "  "
                       () "│ │  ")))
                     (:entry #s(jj-log-entry
                      #s(jj-log-header "zsrpuxsq/0" "zoeyhewll@gmail.com" "2026-01-11 18:58:04" nil nil nil "d4b4e2fc" "conflict" nil "diverge 2
")
                      #s(jj-log-graph "" "×" " │  "
                       () "│ │  ")))
                     (:entry #s(jj-log-entry
                      #s(jj-log-header "zsrpuxsq/9" "zoeyhewll@gmail.com" "2025-12-29 22:14:42" nil nil nil "c33a9601" nil nil "diverge 1
")
                      #s(jj-log-graph "│ │ " "o" "  "
                       ("├───╯  ") "│ │    ")))
                     (:entry #s(jj-log-entry
                      #s(jj-log-header "znpwrszt" "zoeyhewll@gmail.com" "2025-12-29 22:14:42" nil nil nil "a2fd03ad" "conflict" "empty" nil)
                      #s(jj-log-graph "" "×" " │    "
                       ("├───╮  ") "│ │ │  ")))
                     (:entry #s(jj-log-entry
                      #s(jj-log-header "uwoorszk" "zoeyhewll@gmail.com" "2025-12-29 22:14:35" nil nil nil "3c9908cd" nil nil "foo
")
                      #s(jj-log-graph "│ │ " "o" "  "
                       () "│ │ │  ")))
                     (:entry #s(jj-log-entry
                      #s(jj-log-header "nqzyvomm" "zoeyhewll@gmail.com" "2025-12-29 22:14:42" nil nil nil "0c0b58a0" nil nil "foo
")
                      #s(jj-log-graph "" "o" " │ │  "
                       () "│ │ │  ")))
                     (:entry #s(jj-log-entry
                      #s(jj-log-header "oqnyxnnn" "zoeyhewll@gmail.com" "2025-12-26 23:28:34" nil nil nil "52c23b7f" nil "empty" "foo sample commit to edit
")
                      #s(jj-log-graph "" "o" " │ │  "
                       ("├───╯  ") "│ │    ")))
                     (:entry #s(jj-log-entry
                      #s(jj-log-header "wuvynqqs" "zoeyhewll@gmail.com" "2025-03-26 14:08:11"
                                      ("main??" "main@origin")
                                      nil nil "ba86ecc3" nil nil "add basic restart-case and handler-case
")
                      #s(jj-log-graph "" "+" " │  "
                       () "│ │  ")))
                     (:elided #s(jj-log-graph "" "!" " │  "
                       ("├─╯") nil))
                     (:entry #s(jj-log-entry
                      #s(jj-log-header "zwxrqwwv" "zoeyhewll@gmail.com" "2025-12-21 22:36:01"
                                      ("foo")
                                      nil nil "087ec1fb" nil "empty" nil)
                      #s(jj-log-graph "│ " "o" "  "
                       ("├─╯  ") "│    ")))
                     (:entry #s(jj-log-entry
                      #s(jj-log-header "zzzzzzzz" "" "1970-01-01 08:00:00" nil nil nil "00000000" nil "empty" nil)
                      #s(jj-log-graph "" "+" "  "
                       () "   "))))))))

(ert-deftest jj-test-header-log-parsing ()
  (with-temp-buffer
    (save-excursion
      (insert "log-headeruorrwztk\"zoeyhewll@gmail.com\"2026-02-22 14:23:10f831a9edconflictempty\"\"
log-headertqqxztyu\"zoeyhewll@gmail.com\"2025-12-24 18:48:22main??1ae95f23\"long\\nmultiline\\nmessage\\nhere\\nand\\nhere\\n\"
log-headerzsrpuxsq/0\"zoeyhewll@gmail.com\"2026-01-11 18:58:04d4b4e2fcconflict\"diverge 2\\n\"
log-headerzsrpuxsq/9\"zoeyhewll@gmail.com\"2025-12-29 22:14:42c33a9601\"diverge 1\\n\"
log-headerznpwrszt\"zoeyhewll@gmail.com\"2025-12-29 22:14:42a2fd03adconflictempty\"\"
log-headeruwoorszk\"zoeyhewll@gmail.com\"2025-12-29 22:14:353c9908cd\"foo\\n\"
log-headernqzyvomm\"zoeyhewll@gmail.com\"2025-12-29 22:14:420c0b58a0\"foo\\n\"
log-headeroqnyxnnn\"zoeyhewll@gmail.com\"2025-12-26 23:28:3452c23b7fempty\"foo sample commit to edit\\n\"
log-headerwuvynqqs\"zoeyhewll@gmail.com\"2025-03-26 14:08:11main?? main@originba86ecc3\"add basic restart-case and handler-case\\n\"
(elided revisions)
log-headerzwxrqwwv\"zoeyhewll@gmail.com\"2025-12-21 22:36:01foo087ec1fbempty\"\"
log-headerzzzzzzzz\"\"1970-01-01 08:00:0000000000empty\"\"
"))
    (should (equal (cl-loop while (not (eobp))
                            for n = (let ((errors))
                                      (or (push-errors errors (list :entry (prog1
                                                                               (read-jj-log-header)
                                                                             (jj--re-step-over "\n"))))
                                          (push-errors errors (list :elided (jj-read-elided)))
                                          (error "Line is not a recognised part of a jj log. %s: %s"
                                                 (nreverse errors)
                                                 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                            while n
                            collect n)
                   '((:entry
                      #s(jj-log-header "uorrwztk" "zoeyhewll@gmail.com" "2026-02-22 14:23:10" nil nil nil "f831a9ed" "conflict" "empty" nil))
                     (:entry
                      #s(jj-log-header "tqqxztyu" "zoeyhewll@gmail.com" "2025-12-24 18:48:22"
                                      ("main??")
                                      nil nil "1ae95f23" nil nil "long
multiline
message
here
and
here
"))
                     (:entry
                      #s(jj-log-header "zsrpuxsq/0" "zoeyhewll@gmail.com" "2026-01-11 18:58:04" nil nil nil "d4b4e2fc" "conflict" nil "diverge 2
"))
                     (:entry
                      #s(jj-log-header "zsrpuxsq/9" "zoeyhewll@gmail.com" "2025-12-29 22:14:42" nil nil nil "c33a9601" nil nil "diverge 1
"))
                     (:entry
                      #s(jj-log-header "znpwrszt" "zoeyhewll@gmail.com" "2025-12-29 22:14:42" nil nil nil "a2fd03ad" "conflict" "empty" nil))
                     (:entry
                      #s(jj-log-header "uwoorszk" "zoeyhewll@gmail.com" "2025-12-29 22:14:35" nil nil nil "3c9908cd" nil nil "foo
"))
                     (:entry
                      #s(jj-log-header "nqzyvomm" "zoeyhewll@gmail.com" "2025-12-29 22:14:42" nil nil nil "0c0b58a0" nil nil "foo
"))
                     (:entry
                      #s(jj-log-header "oqnyxnnn" "zoeyhewll@gmail.com" "2025-12-26 23:28:34" nil nil nil "52c23b7f" nil "empty" "foo sample commit to edit
"))
                     (:entry
                      #s(jj-log-header "wuvynqqs" "zoeyhewll@gmail.com" "2025-03-26 14:08:11"
                                      ("main??" "main@origin")
                                      nil nil "ba86ecc3" nil nil "add basic restart-case and handler-case
"))
                     (:elided
                      "(elided revisions)")
                     (:entry
                      #s(jj-log-header "zwxrqwwv" "zoeyhewll@gmail.com" "2025-12-21 22:36:01"
                                      ("foo")
                                      nil nil "087ec1fb" nil "empty" nil))
                     (:entry
                      #s(jj-log-header "zzzzzzzz" "" "1970-01-01 08:00:00" nil nil nil "00000000" nil "empty" nil)))))))

(ert-deftest jj-test-header-log-graph-split ()
  (with-temp-buffer
    (save-excursion
      (insert "@    log-headeruorrwztk\"zoeyhewll@gmail.com\"2026-02-22 14:23:10f831a9edconflictempty\"\"
├─╮  
│ │  
│ │  
│ o  log-headertqqxztyu\"zoeyhewll@gmail.com\"2025-12-24 18:48:22main??1ae95f23\"long\\nmultiline\\nmessage\\nhere\\nand\\nhere\\n\"
│ │  
│ │  
│ │  
× │  log-headerzsrpuxsq/0\"zoeyhewll@gmail.com\"2026-01-11 18:58:04d4b4e2fcconflict\"diverge 2\\n\"
│ │  
│ │  
│ │  
│ │ o  log-headerzsrpuxsq/9\"zoeyhewll@gmail.com\"2025-12-29 22:14:42c33a9601\"diverge 1\\n\"
├───╯  
│ │    
│ │    
× │    log-headerznpwrszt\"zoeyhewll@gmail.com\"2025-12-29 22:14:42a2fd03adconflictempty\"\"
├───╮  
│ │ │  
│ │ │  
│ │ o  log-headeruwoorszk\"zoeyhewll@gmail.com\"2025-12-29 22:14:353c9908cd\"foo\\n\"
│ │ │  
│ │ │  
│ │ │  
o │ │  log-headernqzyvomm\"zoeyhewll@gmail.com\"2025-12-29 22:14:420c0b58a0\"foo\\n\"
│ │ │  
│ │ │  
│ │ │  
o │ │  log-headeroqnyxnnn\"zoeyhewll@gmail.com\"2025-12-26 23:28:3452c23b7fempty\"foo sample commit to edit\\n\"
├───╯  
│ │    
│ │    
+ │  log-headerwuvynqqs\"zoeyhewll@gmail.com\"2025-03-26 14:08:11main?? main@originba86ecc3\"add basic restart-case and handler-case\\n\"
│ │  
│ │  
│ │  
! │  (elided revisions)
├─╯
│ o  log-headerzwxrqwwv\"zoeyhewll@gmail.com\"2025-12-21 22:36:01foo087ec1fbempty\"\"
├─╯  
│    
│    
+  log-headerzzzzzzzz\"\"1970-01-01 08:00:0000000000empty\"\"
   
   
   
"))
    (should (equal (cl-loop while (not (eobp))
                            for n = (let ((errors))
                                      (or (push-errors errors (list :entry (with-error-label
                                                                               "read log node graph"
                                                                             (jj-parse-erase-graph-prefix))))
                                          (push-errors errors (list :elided (with-error-label
                                                                                "read elided graph"
                                                                              (jj-parse-erase-elided-prefix))))
                                          (error "Line is not recognised as having a jj log graph prefix. %s: %s"
                                                 (nreverse errors)
                                                 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                            while n
                            collect n)
                   '((:entry
                      #s(jj-log-graph "" "@" "    "
                       ("├─╮  ") "│ │  "))
                     (:entry
                      #s(jj-log-graph "│ " "o" "  "
                       () "│ │  "))
                     (:entry
                      #s(jj-log-graph "" "×" " │  "
                       () "│ │  "))
                     (:entry
                      #s(jj-log-graph "│ │ " "o" "  "
                       ("├───╯  ") "│ │    "))
                     (:entry
                      #s(jj-log-graph "" "×" " │    "
                       ("├───╮  ") "│ │ │  "))
                     (:entry
                      #s(jj-log-graph "│ │ " "o" "  "
                       () "│ │ │  "))
                     (:entry
                      #s(jj-log-graph "" "o" " │ │  "
                       () "│ │ │  "))
                     (:entry
                      #s(jj-log-graph "" "o" " │ │  "
                       ("├───╯  ") "│ │    "))
                     (:entry
                      #s(jj-log-graph "" "+" " │  "
                       () "│ │  "))
                     (:elided
                      #s(jj-log-graph "" "!" " │  "
                       ("├─╯") nil))
                     (:entry
                      #s(jj-log-graph "│ " "o" "  "
                       ("├─╯  ") "│    "))
                     (:entry
                      #s(jj-log-graph "" "+" "  "
                       () "   ")))))
    (should (string=
             (buffer-string)
             "log-headeruorrwztk\"zoeyhewll@gmail.com\"2026-02-22 14:23:10f831a9edconflictempty\"\"
log-headertqqxztyu\"zoeyhewll@gmail.com\"2025-12-24 18:48:22main??1ae95f23\"long\\nmultiline\\nmessage\\nhere\\nand\\nhere\\n\"
log-headerzsrpuxsq/0\"zoeyhewll@gmail.com\"2026-01-11 18:58:04d4b4e2fcconflict\"diverge 2\\n\"
log-headerzsrpuxsq/9\"zoeyhewll@gmail.com\"2025-12-29 22:14:42c33a9601\"diverge 1\\n\"
log-headerznpwrszt\"zoeyhewll@gmail.com\"2025-12-29 22:14:42a2fd03adconflictempty\"\"
log-headeruwoorszk\"zoeyhewll@gmail.com\"2025-12-29 22:14:353c9908cd\"foo\\n\"
log-headernqzyvomm\"zoeyhewll@gmail.com\"2025-12-29 22:14:420c0b58a0\"foo\\n\"
log-headeroqnyxnnn\"zoeyhewll@gmail.com\"2025-12-26 23:28:3452c23b7fempty\"foo sample commit to edit\\n\"
log-headerwuvynqqs\"zoeyhewll@gmail.com\"2025-03-26 14:08:11main?? main@originba86ecc3\"add basic restart-case and handler-case\\n\"
(elided revisions)
log-headerzwxrqwwv\"zoeyhewll@gmail.com\"2025-12-21 22:36:01foo087ec1fbempty\"\"
log-headerzzzzzzzz\"\"1970-01-01 08:00:0000000000empty\"\"
"))))
;; tests:1 ends here

;; Generic format macro

;; [[file:majjik.org::*Generic format macro][Generic format macro:1]]

;; Generic format macro:1 ends here

;; Status format

;; [[file:majjik.org::*Status format][Status format:1]]
(define-jj-format status-lineage-entry
  (change-id
   :face '(:foreground "magenta")
   :form (:chain self (format_short_change_id_with_change_offset)))
  (commit-id
   :face '(:foreground "light blue")
   :form (:chain self (.commit_id) (format_short_commit_id)))
  (bookmarks
   :face '(:foreground "magenta")
   :parser (jj--make-list-parser " ")
   :printer (jj--make-list-printer " ")
   :form (:chain self (.bookmarks)))
  (tags
   :face '(:foreground "yellow")
   :form (:chain self (.tags)))
  (conflict
   :face '(:foreground "red")
   :form (if (:chain self (.conflict)) "conflict"))
  (description
   :parser #'json-parse-string
   :form (:chain self (.description) (.first_line) (.trim) (.escape_json))))

(define-jj-format status-wc-change
  (status
   :form (:chain self (.status)))
  (path-source
   :parser #'json-parse-string
   :form (:chain self (.source) (.path) (.display) (.escape_json)))
  (path-target
   :parser #'json-parse-string
   :form (:chain self (.target) (.path) (.display) (.escape_json))))

(define-jj-format status-file-conflict
  (path
   :face '(:foreground "red")
   :parser #'json-parse-string
   :form (:chain f (.path) (.display) (.escape_json)))
  ;; (num-sides) ;; todo once it's representable in a template. for now, always unknown.
  )

(define-jj-format status-file-untracked
  (path
   :face '(:foreground "magenta")
   :parser #'json-parse-string
   :form (++ (:chain self (.display t) (.escape_json)) "\n")))

(define-jj-format status-file-tracked
  (path
   :parser #'json-parse-string
   :form (++ (:chain self (.path) (.display) (.escape_json)) "\n")))

(define-jj-format status-bookmark-conflict
  (name
   :face '(:foreground "magenta")
   :form (:chain self (.name) ))
  (remote
   :face '(:foreground "magenta")
   :separator "@"
   :form (++ (:chain self (.remote)) "\n")))
;; Status format:1 ends here

;; Command log

;; [[file:majjik.org::*Command log][Command log:1]]
(defvar jj--cmd-log-buf-name-prefix "jj-command-log"
  "Tag to use to identify jj command-log buffers.
This is concatenated with an identifier for the repository to define the buffer name for the command log. Let-bind this in order to temporarily use a different buffer for a particular log entry.")

(defvar jj--cmd-show-verbosity 'system-error
  "Verbosity level to show in the command-log. This level and all less-verbose levels are visible. This value is only used for modeline display and should only be set via `jj-set-verbosity-level'.")
(defvar jj--cmd-verbosity 'user
  "Verbosity of each entry-to-be. Dynamically bound around invocation to affect particular commands.")
(defvar jj--cmd-error-verbosity 'error
  "Verbosity of each entry-to-be when it results in an error. Dynamically bound around invocation to affect particular commands.")
(defconst jj--cmd-verbosity-sequence '((critical "Critical errors.")
                                       (error "Errors from user-issued commands")
                                       (user "Output of user-issued commands")
                                       (system-error "Errors from system commands, e.g. dash regeneration")
                                       (system "System messages, e.g. from dash regeneration")
                                       (debug "Verbose messages, for debugging")))

(defun jj-get-verbosity-invisibility-spec (v)
  "Get the levels that ought to be invisible, if the verbosity level is V."
  (mapcar #'car
          (cdr (cl-member v jj--cmd-verbosity-sequence
                          :key #'car))))

(defun jj--get-numeric-verbosity-level (v)
  "Return the numeric equivalent of the named verbosity level V."
  (cl-position v jj--cmd-verbosity-sequence
               :key #'car))

(defun jj-set-verbosity-level (v)
  ;; (progn
  ;;                (when-let ((ev (this-command-keys-vector)))
  ;;                  (cl-loop for x across ev
  ;;                           when (mouse-event-p x)
  ;;                           return (select-window ))
  ;;                (list (intern-soft (completing-read "Verbosity level: " jj--cmd-verbosity-sequence nil nil nil t))
  ;;                    )))
  (interactive (list (intern-soft (completing-read "Verbosity level: " jj--cmd-verbosity-sequence))))
  (setq jj--cmd-show-verbosity v)
  (mapc #'remove-from-invisibility-spec (mapcar #'car jj--cmd-verbosity-sequence))
  (mapc #'add-to-invisibility-spec (jj-get-verbosity-invisibility-spec v))
  buffer-invisibility-spec)

(defun jj-pop-to-command-log (repo-dir)
  "Open the command-log buffer for the current repo."
  (interactive (list default-directory))
  (pop-to-buffer (jj--get-command-log-buf repo-dir)))

(defun jj--get-command-log-buf (repo-dir)
  "Get or create the command-log buffer for the given REPO-DIR, and ensure it is in the correct mode."
  (let ((buf (get-buffer-create (format "*%s:%s*" jj--cmd-log-buf-name-prefix (expand-file-name repo-dir)))))
    (with-current-buffer buf
      (unless (derived-mode-p 'jj-process-mode)
        (jj-process-mode)
        ;; for now this ruins colours in the process log buffer
        ;; as my colours are not set via font-lock
        ;; which is enabled in magit-section mode.
        (cursor-intangible-mode t)))
    buf))
;; Command log:1 ends here

;; Default args

;; [[file:majjik.org::*Default args][Default args:1]]
(defvar jj-global-default-args
  '(;; never auto-track new files
    "--config" "snapshot.auto-track='none()'"
    ;; never request a pager
    "--no-pager"))
(defvar jj-parsing-default-args
  '(;; omit extra output
    "--quiet"))
(defvar jj-logging-default-args
  '("--color=always"))
(defvar jj-global-debug-args
  '("--debug")
  "List of flags (if any) to debug jj commands.")
(defvar jj-current-dynamic-args nil
  "Vehicle for additional default command-line arguments to be supplied by the dynamic environment.")
(defvar jj-do-debug nil
  "If non-nil, add `jj-global-debug-args' to all commands")

(defun jj--toggle-debug ()
  "Toggle debugging of jj commands."
  (interactive)
  (setq jj-do-debug (not jj-do-debug))
  (cond (jj-do-debug
         (message "enabled debug output for jj commands"))
        (t (message "disabled debug output for jj commands"))))
;; Default args:1 ends here

;; Readers

;; [[file:majjik.org::*Readers][Readers:1]]
(defvar jj-revset-history nil "History for jj revsets")
(defun jj-read-revset-sexp (&optional prompt)
  (when-let ((sexp (read-from-minibuffer (or prompt "revset sexp: ") nil nil nil 'jj-revset-history)))
    (unless (string= "" sexp)
      (jj-revset (read sexp)))))

(defun jj--rev-at-point-option (thing)
  (when-let ((revs (jj--point-revision thing)))
    `("^" ,@revs)))

(defun jj--point-revision (thing)
  (when-let ((chg (jj--get-change thing))
             (cmt (jj--get-commit thing)))
    (list chg cmt (jj--get-bookmarks thing))))

(defun jj--list-relevant-revisions (&optional rev-at-pt)
  "List all revisions, as well as all bookmarks. if REV-AT-PT is provided, it should be a list of a change id and commit id for the commit at point."
  `(,@(opt rev-at-pt)
    ("@" ,@(jj-wc-revision))
    ,@(cl-loop for (cm ci . bms) in (jj-match-revisions-annotated)
               collect `(,cm ,cm ,ci ,bms)
               collect `(,ci ,cm ,ci ,bms)
               nconc (cl-loop for bm in bms
                              collect `(,bm ,cm ,ci ,bms)))
    ))

(defun jj--annotated-ref-table (options)
  "Return an annotated completion table for OPTIONS"
  (completion-table-with-annotation
   (completion-table-dynamic (cl-constantly options))
   (jj--annotate-refs options)))

(defun jj--annotate-refs (options)
  "return an annotator for the given OPTIONS alist"
  (lambda (c)
    (-let (((chg cmt bmks) (alist-get c options nil nil #'string=)))
      (format "\t%s\t%s\t%s"
              (propertize chg 'face '(:foreground "magenta"))
              (propertize cmt 'face '(:foreground "light blue"))
              (propertize (s-join " " bmks) 'face '(:foreground "magenta"))))))

(defun jj-read-single-revision (prompt initial-input history)
  (let* ((thing (jj-thing-at-point))
         (rev-pt (jj--get-change thing))
         (pt-ann (jj--rev-at-point-option thing))
         (answer (completing-read
                  prompt (jj--annotated-ref-table
                          (jj--list-relevant-revisions pt-ann))
                  nil nil initial-input history)))
    (pcase answer
      ("^" rev-pt)
      (_ answer))))

(defun jj-read-multi-revision (prompt initial-input history)
  (let ((crm-separator (rx (* blank)
                           (any ",| ")
                           (* blank))))
    (let* ((thing (jj-thing-at-point))
           (rev-pt (jj--get-change thing))
           (pt-ann (jj--rev-at-point-option thing))
           (answers (completing-read-multiple
                     prompt (jj--annotated-ref-table
                             (jj--list-relevant-revisions pt-ann))
                     nil nil initial-input history)))
      (mapcar (lambda (ans)
                (pcase ans
                  ("^" rev-pt)
                  (_ ans)))
              answers))))

(defun jj-read-revset (&optional prompt)
  (when-let ((str (read-from-minibuffer (or prompt "revset: ") nil nil nil 'jj-revset-history)))
    (unless (string= "" str) str)))

(defvar jj-revision-history nil "History for jj revisions")

(defun jj-read-revision (&optional prompt revset)
  (when-let ((str (completing-read (or prompt "revision: ") (jj-match-revisions revset) nil nil nil 'jj-revision-history)))
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
  :parent magit-section-mode-map
  "," #'jj-inspect-sexp-at-point
  "RET" #'jj-inspect-thing-at-point)

(defvar-keymap jj-dashboard-mode-map
  :parent jj-inspect-mode-map
  "Q" #'jj-cmd
  "H" #'jj-help
  "G" #'jj-git-prefix
  "C-/" #'jj-undo
  "C-?" #'jj-redo
  "C-_" #'jj-undo
  "C-M-_" #'jj-redo
  "C-x u" #'jj-undo
  "C" #'jj-commit-entry-prefix
  "c e" #'jj-edit-dwim
  "c n" #'jj-new-on-dwim
  "c k" #'jj-drop-dwim
  "c w" #'jj-desc-dwim
  "c a" #'jj-amend-into-dwim
  "c s" #'jj-squash-down-dwim
  "d" #'jj-diff-entry-prefix
  "F" #'jj-git-fetch-prefix
  "P" #'jj-git-push-prefix
  "B" #'jj-bookmark-prefix
  "b n" #'jj-bookmark-new-dwim
  "b m" #'jj-bookmark-move-dwim
  "b !" #'jj-bookmark-set-dwim
  "b r" #'jj-bookmark-rename
  "b k" #'jj-bookmark-delete
  "b DEL" #'jj-bookmark-forget
  "b t" #'jj-bookmark-track-for-remote
  "b u" #'jj-bookmark-untrack
  "b l" #'jj-bookmark-list
  "b b" #'jj-new-on-bookmark
  "f t" #'jj-file-track-dwim
  "f u" #'jj-file-untrack-dwim
  "f k" #'jj-file-delete-dwim
  "$" #'jj-pop-to-command-log)

(keymap-global-set "C-x j" #'jj-dash--async)
;; Keymaps:1 ends here

;; General buffer-locals

;; [[file:majjik.org::*General buffer-locals][General buffer-locals:1]]
(defvar-local jj--last-revs nil
  "The revset last used in this buffer")
(defvar-local jj--last-files nil
  "The fileset last used in this buffer")
;; General buffer-locals:1 ends here

;; Dashboard buffer

;; [[file:majjik.org::*Dashboard buffer][Dashboard buffer:1]]
(defvar jj--silent-revert nil
  "If non-nil, don't display messages for reverting the status buffer unless there is an error.")

(define-derived-mode jj-inspect-mode magit-section-mode "jj-inspect"
  "Parent mode for most jj modes, defining basic operations")

(define-derived-mode jj-dashboard-mode jj-inspect-mode "jj-dash"
  "Major mode for jj dashboard")

(defvar-local jj--current-status nil
  "In a jj dashboard buffer, this is the most recent status object.")

(defvar-local jj--indirect-buffers nil
  "Indirect buffers into the current buffer. Ought to be killed if we're reverting.")

(defun jj-dash--revert-async ()
  "Asynchronously get the new status, and reverts the buffer contents when those processes complete.
Reverted buffer is the one that was active when this function was called."
  (let ((silent jj--silent-revert)
        (dash-buf (current-buffer)))
    (cl-labels ((end-ok (_ok)
                  (unless silent
                    (message "%s" (jj--format-simple-ok "status"))))
                (end-err (errs)
                  (message "%s" (jj--format-simple-fail "status"))))
      (unless silent
        (message "`jj status'..."))
      (promise-then
       (with-current-buffer dash-buf
         (start-jj-dash-async))
       #'end-ok
       #'end-err))))

(defun jj-dash--revert (&rest _)
  ;; todo: wait on this somehow?
  (jj-dash--revert-async))

(defun jj-jump-find-object (pred)
  (cl-loop for pos = (point-min) then (next-single-property-change pos 'jj-object)
           while pos
           for obj = (jj-thing-at pos)
           when (funcall pred obj) return (goto-char pos)))

(defun jj-jump-to-thing (thing &optional test)
  (let ((test (or test #'eq)))
    (jj-jump-find-object (lambda (obj) (funcall test thing obj)))))

(defun jj-dash-buffer (dir)
  "Return the dashboard buffer for DIR, which is assumed to be a workspace root, but is not assumed to be normalised."
  (format "*jj-dash %s*" (abbreviate-file-name (expand-file-name dir))))

(defun jj--workspace-root-or-create (repo-dir)
  "To supply the arguments for `jj-dash--async'. If there's no repo above REPO-DIR, then offer to create it.
If rejected, propagate the original error signal.
If accepted, return the dir for the new repo, and the promise for the initialisation.
If repo already existed, just return its path (as a singleton list)."
  (cl-block nil
    (handler-bind 
        ((jj-repo-missing-above
          (lambda (e)
            (when (yes-or-no-p (format "No repo at or around `%s'. Create one?" repo-dir))
              (let ((root-dir (expand-file-name (read-directory-name "repository root: " repo-dir)))
                    (colocate (yes-or-no-p "colocated repository?")))
                (cl-return
                 (list root-dir (jj-git-init root-dir colocate))))))))
      (list (jj-workspace-root repo-dir)))))

;;;###autoload
(defun jj-dash--async (repo-dir &optional init-promise)
  "Show the status of the current jj repository in a buffer. Async - pops to the new buffer once the status is ready."
  (interactive (jj--workspace-root-or-create default-directory))
  (let ((main-buf (get-buffer-create (jj-dash-buffer repo-dir))))
    (cl-labels ((setup-buf-promise (&optional _)
                  (with-current-buffer main-buf
                    (jj-dashboard-mode)
                    (setq-local default-directory repo-dir
                                revert-buffer-function #'jj-dash--revert)
                    (message "getting jj status")
                    (jj-dash--revert-async))))
      (promise-then
       (if init-promise
           (promise-then init-promise
                         #'setup-buf-promise)
         (setup-buf-promise))
       (lambda (_ok)
         (message "jj dash ok")
         (pop-to-buffer main-buf))
       (lambda (err)
         (message "error: %s" err))))))

(defun jj--ensure-repo-async (repo-dir)
  "To supply the arguments for `jj-dash--async'. If REPO-DIR is not a jj repo, then offer to create it.
If rejected, signal `jj-repo-missing-at'.
If accepted, return the dir for the new repo, and the promise for the initialisation.
If repo already existed, just return its path (as a singleton list)."
  (cond ((file-exists-p (file-name-concat repo-dir ".jj"))
         (list repo-dir))
        ((yes-or-no-p (format "No repo at `%s'. Create one?" repo-dir))
         (let ((root-dir (expand-file-name (read-directory-name "repository root: " repo-dir)))
               (colocate (yes-or-no-p "colocated repository?")))
           (list root-dir (jj-git-init root-dir colocate))))
        (t (signal 'jj-repo-missing-at repo-dir))))

;;;###autoload
(defun jj-project-dash ()
  "Run `jj-dash' in the current project's root."
  (interactive)
  (if (fboundp 'project-root)
      (apply #'jj-dash--async (jj--ensure-repo-async (project-root (project-current t))))
    (user-error "`jj-project-dash' requires `project' 0.3.0 or greater")))

(defun jj-make-section-buffer (section-name header trailer)
  "Make a jj section called SECTION-NAME which is narrowed down to the current value of point. Returns the buffer.
 HEADER is the text to insert before the section, and TRAILER is the text to insert after it."
  (let ((buf (clone-indirect-buffer
              (format "*jj-%s-section %s*" section-name default-directory)
              nil :norecord))
        (marker (make-marker)))
    (prog1 buf
      (when header (insert header))
      ;; make sure there's something between end-of-buffer and where we'll be inserting stuff
      (save-excursion
        (when (string= "" trailer)
          (warn "empty section trailer for %s" section-name))
        (insert trailer)
        (setf (marker-position marker) (point)))
      ;; record point from the original buffer
      ;; seems there's a race condition to update it in the indirect buffer
      (let ((p (point)))
        (with-current-buffer buf
          (narrow-to-region p p)))
      ;; step over the added trailer
      (goto-char marker))))
;; Dashboard buffer:1 ends here

;; jj-file for tracked, set ops for untracked

;; [[file:majjik.org::*jj-file for tracked, set ops for untracked][jj-file for tracked, set ops for untracked:1]]
(defun jj--list-tree-untracked (root tracked-paths &optional full-search absolute-paths)
  "Given a ROOT directory and a list TRACKED-PATHS of file paths, return the list of all untracked file paths.
When FULL-SEARCH, list all untracked files. Otherwise, return directory name instead of contents whenever all of a directory's contents are untracked.
When ABSOLUTE-PATHS, return fully expanded file names. Otherwise, return paths relative to ROOT."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (untracked))
    ;; populate the untracked file list
    (let* ((pending)
           (tracked (mapcar (lambda (trk)
                              (expand-file-name trk root))
                            tracked-paths))
           (check (lambda (file)
                    ;; Check if FILE is tracked or not, and if it is a directory or not,
                    ;; and handle it appropriately according to FULL-SEARCH.
                    (pcase file
                      ((and (pred file-directory-p)
                            ;; ensure there's a trailing slash
                            ;; so that we can be sure a string prefix is a path prefix
                            (let dir (file-name-as-directory file)))
                       (if (or full-search
                               (-any (lambda (trk)
                                       (string-prefix-p dir trk))
                                     tracked))
                           ;; partly-tracked subdir or searching all dirs, revisit
                           (push dir pending)
                         ;; fully untracked subdir and we're cutting search there
                         (push dir untracked)))
                      (_ (if (-any (lambda (trk)
                                     (string= file trk))
                                   tracked)
                             ;; tracked file, ignore
                             ()
                           ;; untracked file, collect
                           (push file untracked)))))))
      (cl-loop with no-rel-rx = (rx  (|
                                      ;; non-dot covers any normal path
                                      ;; that might include a ".", or not
                                      (not ".")
                                      ;; 3 (or more, implicitly) covers any case
                                      ;; where a path is only "."s,
                                      ;; but it's still not a relative component
                                      (= 3 ".")))
               for dir = root then (pop pending)
               for contents = (directory-files dir :full no-rel-rx)
               do (mapc check contents)
               while pending))
    ;; process the filenames if we want relative paths
    (if absolute-paths
        untracked
      (mapcar (lambda (path)
                (string-trim-left path (rx (literal root))))
              untracked))))

(ert-deftest jj-test-tree-untracked ()
  (let* ((default-directory (make-temp-file "test-track-" :dir))
         (tracked '("ta" "dt/ta" "dt/tb" "dp/ta"))
         (untracked '("ua" "dp/ua" "dp/du/ua" "dp/du/ub" "du/ua" "du/ub"))
         (untracked-trim '("ua" "dp/ua" "dp/du/" "du/"))
         (untracked-abs (mapcar (lambda (u)
                                  (expand-file-name u))
                                untracked))
         (untracked-trim-abs (mapcar (lambda (u)
                                       (expand-file-name u))
                                     untracked-trim))
         (all-files `(,@tracked ,@untracked)))
    (mapcar (lambda (f)
              (make-empty-file f :parents))
            all-files)
    (let ((actual (jj--list-tree-untracked "." tracked)))
      (should (seq-set-equal-p untracked-trim actual #'string=)))
    (let ((actual (jj--list-tree-untracked "." tracked nil :abs)))
      (should (seq-set-equal-p untracked-trim-abs actual #'string=)))
    (let ((actual (jj--list-tree-untracked "." tracked :all :abs)))
      (should (seq-set-equal-p untracked-abs actual #'string=)))
    (let ((actual (jj--list-tree-untracked "." tracked :all)))
      (should (seq-set-equal-p untracked actual #'string=)))
    (delete-directory default-directory :rec)
    :ok))

(defun jj-tracked-files-async ()
  "Return the list of all tracked files in the current repo."
  (promise-then
   (jj-cmd-async--for-status "file-list"
     `("file" "list"
       "-T" ,(jj-status-file-tracked-template 'self)))
   (lambda (proc)
     (prog1
         (with-current-buffer (process-buffer proc)
           (jj--read-tracked-files))
       (kill-buffer (process-buffer proc))))))

(defun jj--read-tracked-files ()
  "Read all `jj-status-file-tracked' in the buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (progn
                     (unless (bolp)
                       (forward-char 1))
                     (not (eobp)))
             for entry = (read-jj-status-file-tracked)
             collect entry)))

(defun jj--check-files-ignored (untracked)
  (promise-then (jj-cmd-async--for-status "git-ignored"
                  `("util" "exec" "--"
                    "git" "ls-files"
                    "--others" "--ignored" "--exclude-standard" "--directory" "-z" "--" ,@untracked))
                (lambda (proc)
                  (with-current-buffer (process-buffer proc)
                    (prog1
                        (let ((ignored (cl-loop initially (goto-char (point-min))
                                                while (not (eobp))
                                                do (jj--re-step-over
                                                    (rx (group (+ (not (any ?\0))))
                                                        ?\0))
                                                collect (match-string 1))))
                          (cons ignored untracked))
                      (kill-buffer (process-buffer proc))))))
  )


(defun jj-untracked-files-async (root-dir)
  "List untracked (non-ignored) files in repository ROOT-DIR."
  (let ((p-tracked (jj-tracked-files-async)))
    (promise-chain p-tracked
      (then (lambda (tracked)
              (jj--list-tree-untracked
               root-dir
               (mapcar #'jj-status-file-tracked-path tracked)
               nil nil)))
      (then (lambda (untracked)
              ;; outer `then' implicitly flattens inner promises
              (let ((default-directory root-dir))
                (jj--check-files-ignored untracked))))
      (then (-lambda ((ignored . untracked))
              (cl-set-difference untracked `(".git" ".jj" ,@ignored)
                                 :test (lambda (u i)
                                         ;; if an ignored dir is a prefix,
                                         ;; then remove it from the untracked
                                         (string-prefix-p i u))
                                 :key #'file-name-as-directory))))))
;; jj-file for tracked, set ops for untracked:1 ends here

;; jj-bookmark-list for bookmark conflicts


;; [[file:majjik.org::*jj-bookmark-list for bookmark conflicts][jj-bookmark-list for bookmark conflicts:1]]
(defun start-jj-bookmark-list (&optional revset &rest other-args)
  "Make a jj bookmark-list in the current buffer, without setting up modes or keymaps. For use with jj-status in an indirect buffer."
  (let ((inhibit-read-only t))
    (erase-accessible-buffer))
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
                ,@(jj--if-arg revset #'identity "-r")
                ,@other-args
                ,@jj-global-default-args
                ,@(and jj-do-debug jj-global-debug-args)
                ,@jj-parsing-default-args))))

(defun jj--status-bookmark-conflicts ()
  (promise-then (jj-cmd-async--for-status "bookmarks-conflicted"
                  `("bookmark" "list"
                    "--conflicted"
                    "-T" ,(jj-status-bookmark-conflict-template 'self)))
                (lambda (proc)
                  (with-current-buffer (process-buffer proc)
                    (prog1
                        (cl-loop initially (goto-char (point-min))
                                 while (progn
                                         (unless (bolp)
                                           (forward-char 1))
                                         (not (eobp)))
                                 for entry = (read-jj-status-bookmark-conflict)
                                 collect entry)
                      (kill-buffer (process-buffer proc)))))))
;; jj-bookmark-list for bookmark conflicts:1 ends here

;; Combined struct and output formatter

;; [[file:majjik.org::*Combined struct and output formatter][Combined struct and output formatter:1]]
(cl-defstruct jj-status
  "Aggregate of all of the jj status information from various command sources."
  files-untracked
  files-changed
  commit-working-copy
  commits-parent
  files-conflict
  bookmarks-conflict)

(defmacro slot-values (object slots)
  (declare (indent 1))
  `(with-slots ,slots ,object
     (list ,@slots)))

(defun jj--status-main-status ()
  "Return part of the alist required to make a jj-status object."
  (promise-then (jj-cmd-async--for-status "show-status"
                  `("show"
                    "--no-patch"
                    "-T" ,(jj-show-status-delim-template 'self)))
                (lambda (proc)
                  (with-current-buffer (process-buffer proc)
                    (prog1
                        (with-error-label "read main status"
                          (with-error-context (lambda (e)
                                                (format "err: %s:\n%s\n%s><"
                                                        e
                                                        (buffer-substring (line-beginning-position) (line-end-position))
                                                        (make-string (- (point) (line-beginning-position)) ?\s)))
                            (cl-labels ((more ()
                                          (with-error-label "not at line bounds"
                                            (jj--re-step-over (rx (or line-start string-end "\n"))))
                                          (not (or (eobp)
                                                   (looking-at (rx (opt "\n")
                                                                   "--\n")))))
                                        (step-section ()
                                          (more)
                                          (with-error-label "section separator"
                                            (jj--re-step-over (rx (opt "\n")
                                                                  "--\n")))))
                              `(:files-changed
                                ,(with-error-label "files-changed"
                                   (cl-loop initially (goto-char (point-min))
                                            while (more)
                                            for entry = (read-jj-status-wc-change)
                                            collect entry))
                                :commit-working-copy
                                ,(with-error-label "commit-working-copy"
                                   (progn (step-section)
                                          (read-jj-status-lineage-entry)))
                                :commits-parent
                                ,(with-error-label "commits-parent"
                                   (cl-loop initially (step-section)
                                            while (more)
                                            for entry = (read-jj-status-lineage-entry)
                                            collect entry))
                                :files-conflict
                                ,(with-error-label "files-conflict"
                                   (cl-loop initially (step-section)
                                            while (more)
                                            for entry = (read-jj-status-file-conflict)
                                            collect entry))))))
                      (should (eobp))
                      (kill-buffer))))))

(defun jj-show-status-delim-template (self)
  (jj-template
   (cl-subst self 'self
             `(join "\n--\n"
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

(defun insert-jj-status (status)
  (with-slots (files-untracked
               files-changed
               commit-working-copy
               commits-parent
               files-conflict
               bookmarks-conflict)
      status
    (let ((list-prefix (propertize "- " 'font-lock-face '(:foreground "grey"))))
      (magit-insert-section sec
        (jj-status-files-changed files-changed)
        (cond (files-changed
               (magit-insert-heading "Working copy changes:\n")
               (magit-insert-section-body
                 (cl-loop for change in files-changed
                          for (type from to) = (slot-values change
                                                 (status path-source path-target))
                          for (color . elems) = (pcase-exhaustive type
                                                  ("modified" `(cyan "M" ,to))
                                                  ("added" `(green "A" ,to))
                                                  ("removed" `(red "D" ,from))
                                                  ("copied" `(green "C" "{" ,from "=>" ,to "}"))
                                                  ("renamed" `(cyan "R" "{" ,from "=>" ,to "}")))
                          do (magit-insert-section sec
                               (jj-status-wc-change change)
                               (magit-insert-heading
                                 (propertize (concat list-prefix
                                                     (mapconcat #'identity elems " ")
                                                     "\n")
                                             'font-lock-face `(:foreground ,(format "%s" color))
                                             'jj-object change))
                               ))))
              (t (magit-insert-heading "Working copy unchanged\n"))))
      (magit-insert-section sec
        (jj-status-lineage-entry)
        (magit-insert-heading "Lineage:")
        (magit-insert-section-body
          (magit-insert-section sec
            (jj-status-lineage-entry commit-working-copy)
            (insert "Working copy  (@) : ")
            (insert-jj-status-lineage-entry commit-working-copy))
          (cl-loop for parent in commits-parent
                   do (magit-insert-section sec
                        (jj-status-lineage-entry parent)
                        (insert "Parent commit (@-): ")
                        (insert-jj-status-lineage-entry parent)))))
      (when files-conflict
        (magit-insert-section sec
          (jj-status-files-conflict files-conflict)
          (magit-insert-heading "Unresolved file conflicts:\n")
          (magit-insert-section-body
            (cl-loop for conflict in files-conflict
                     do (magit-insert-section sec
                          (jj-status-file-conflict conflict)
                          (insert list-prefix)
                          (insert-jj-status-file-conflict conflict))))))
      (when bookmarks-conflict
        (magit-insert-section sec
          (jj-status-bookmarks-conflict bookmarks-conflict)
          (magit-insert-heading "Unresolved bookmark conflicts:\n")
          (magit-insert-section-body
            (cl-loop for conflict in bookmarks-conflict
                     do (magit-insert-section sec
                          (jj-status-bookmark-conflict conflict)
                          (insert list-prefix)
                          (insert-jj-status-bookmark-conflict conflict))))))
      (when files-untracked
        (magit-insert-section sec
          (jj-status-files-untracked files-untracked)
          (magit-insert-heading "Untracked files:\n")
          (magit-insert-section-body
            (cl-loop for file in files-untracked
                     do (magit-insert-section sec
                          (jj-status-file-untracked file)
                          (insert list-prefix)
                          (insert-jj-status-file-untracked file)))))))))

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
(defun jj-get-status-async ()
  (promise-then
   (promise-all `[,(jj--status-main-status)
                  ,(jj-untracked-files-async default-directory)
                  ,(jj--status-bookmark-conflicts)])
   (-lambda ([main utck b-cnfl])
     (apply #'make-jj-status :files-untracked (mapcar (lambda (f)
                                                        (make-jj-status-file-untracked :path f))
                                                      utck)
            :bookmarks-conflict b-cnfl
            main))))

(defun start-jj-dash-async ()
  "Start the jj dashboard in the current buffer
Also sets `jj--current-status' in the initial buffer when the status process completes."
  (let ((buf (current-buffer)))
    (promise-then
     (jj--get-dash-data)
     (lambda (results)
       (unless (buffer-live-p buf)
         (error (format "dash output buffer %s deleted" buf)))
       (with-current-buffer buf
         (-let (([log-entries stat] results)
                (line (line-number-at-pos))
                (col (- (point) (pos-bol))))
           (jj-dash--insert stat log-entries)
           (goto-char (point-min))
           (forward-line (1- line))
           (forward-char (min col (- (pos-eol) (pos-bol))))
           (magit-section-update-highlight t)))))))

(defun jj--get-dash-data ()
  (let ((jj--cmd-verbosity 'system)
        (jj--cmd-error-verbosity 'system-error))
    (promise-all
     `[,(jj-log-entries-async)
       ,(jj-get-status-async)])))

(defun jj-dash--insert (stat log)
  (setq-local jj--current-status stat)
  (let ((inhibit-read-only t))
    (erase-accessible-buffer)
    (setq-local jj--current-status stat)
    (magit-insert-section (root)
      (magit-insert-section-body
        (magit-insert-section sec
          (jj-status-section stat)
          (magit-insert-heading "Status:\n")
          (magit-insert-section-body
            (insert-jj-status stat)))
        (magit-insert-section sec
          (jj-log-section log)
          (magit-insert-heading "Log:\n")
          (when log
            (magit-insert-section-body
              (dolist (entry log)
                (insert-jj-graph-log-maybe-elided entry)))))))))
;; status piping fns:1 ends here

;; section

;; [[file:majjik.org::*section][section:1]]
(defun jj-log-entries-async (&optional revset fileset)
  "Make a jj log in the current buffer, without setting up modes or keymaps. For use with jj-status in an indirect buffer. Ignores `jj--last-revs' and `jj--last-files'."
  (let ((entries nil))
    (promise-new
     (lambda (resolve reject)
       (let* ((buf (current-buffer))
              (temp (generate-new-buffer "*jj-log-temp*"))
              (err (generate-new-buffer "*jj-log-stderr*"))
              (sentinel (make-jj-simple-sentinel err temp))
              (filter (cl-labels ((read-next ()
                                    (ignore-errors (jj-read-graph-and-maybe-elided)))
                                  (push-entries (news)
                                    (setf entries (nconc entries news))))
                        (make-jj-generic-buffered-filter temp #'read-next #'push-entries))))
         (let ((proc (make-process
                      :name "jj-log"
                      :buffer buf
                      :stderr err
                      :filter filter
                      :sentinel sentinel
                      :noquery t
                      :command `("jj" "log"
                                 "-T" ,(jj-parseable-template 'self)
                                 ,@(jj--if-arg revset #'identity "-r")
                                 ,@(jj--if-arg fileset #'identity "--")
                                 ,@jj-global-default-args
                                 ,@(and jj-do-debug jj-global-debug-args)
                                 ,@jj-parsing-default-args
                                 "--config" ,(format "templates.log_node='%s'"
                                                     (jj--toml-quote-string jj-log-node-template))))))
           ;; handle the promise state
           (add-function :after (process-sentinel proc)
                         (make-jj-callback-sentinel
                          (lambda (exit-status event)
                            (if (eq exit-status 0)
                                (funcall resolve entries)
                              (funcall reject (cons proc event))))))))))))
;; section:1 ends here

;; find and update dash buffer

;; [[file:majjik.org::*find and update dash buffer][find and update dash buffer:1]]
(defun jj-revert-dash-buffer (dir)
  "Revert the jj dash buffer (if it exists) for DIR."
  (when-let ((buf (get-buffer (jj-dash-buffer (jj-workspace-root dir)))))
    (with-current-buffer buf
      (jj-dash--revert))))

(defun jj-revert-dash-buffer-async (dir)
  "Asynchronously revert the jj dash buffer (if it exists) for DIR. Suitable for use as an async callback for process filters and sentinels."
  (when-let ((buf (get-buffer (jj-dash-buffer
                               (jj-workspace-root dir)))))
    (with-current-buffer buf
      (jj-dash--revert-async))))
;; find and update dash buffer:1 ends here

;; repo query commands

;; [[file:majjik.org::*repo query commands][repo query commands:1]]
(defun jj-rev-list-template (self)
  (jj-template
   (cl-subst self 'self
             `(++ (:chain self (.change_id))
                  "\n"))))

(defun jj-rev-list-verbose-template (self)
  (jj-template
   (cl-subst self 'self
             `(++ (join " "
                            (:chain self (.change_id))
                            (:chain self (.commit_id))
                            (:chain self (.bookmarks) (.join " ")))
                  "\n"))))

(defun jj-name-list-template (self)
  (jj-template
   (cl-subst self 'self
             `(++ (:chain self (.name))
                  "\n"))))

(defun jj-bookmark-list-template (self)
  (jj-template
   (cl-subst self 'self
             `(++ (separate
                   "@"
                   (:chain self (.name))
                   (:chain self (.remote)))
                  "\n"))))

(defun jj-bookmark-list-verbose-template (self)
  "List bookmarks and their corresponding ids as (BOOKMARK CHANGE_ID COMMIT_ID)."
  (jj-template
   (cl-subst self 'self
             '(++ (separate " "
                            (:chain self (.name))
                            (if (:chain self (.normal_target))
                                (separate " "
                                          (:chain self
                                                  (.normal_target)
                                                  (.change_id))
                                          (:chain self
                                                  (.normal_target)
                                                  (.commit_id)))))
                  "\n"))))


(defun jj-list-bookmarks-annotated ()
  "List all bookmarks, and their respective ids."
  (mapcar
   (## s-split " " %)
   (string-lines
    (jj-cmd-sync
     `("bookmark" "list"
       "-T" ,(jj-template
              '(++ (separate " "
                             (:chain self (.name))
                             (if (:chain self (.normal_target))
                                 (separate " "
                                           (:chain self
                                                   (.normal_target)
                                                   (.change_id))
                                           (:chain self
                                                   (.normal_target)
                                                   (.commit_id)))))
                   "\n")))
     :no-revert)
    :omit)))


(defun jj-list-bookmarks ()
  "List all bookmarks."
  (string-lines (jj-cmd-sync `("bookmark" "list"
                               "-T" ,(jj-bookmark-list-template 'self))
                             :no-revert)
                :omit))

(defun jj-list-tracked-bookmarks (&optional remote)
  "List tracked bookmarks, optionally specific to a given REMOTE."
  (string-lines (jj-cmd-sync `("bookmark" "list"
                               
                               ,@(jj--if-arg remote #'identity "--remote")
                               "-T" ,(jj-bookmark-list-template 'self))
                             :no-revert)
                :omit))

(defun jj-list-local-bookmarks (&optional tracking)
  "List local bookmarks. If TRACKING is nil, list all local bookmarks. If it is a string, list bookmarks tracking that remote. Otherwise, list bookmarks tracking any remote. "
  (let* ((tracked (and tracking
                       (not (stringp tracking))))
         (remote (and (stringp tracking)
                      tracking))
         (all (not tracking)))
    (cl-loop for line in (string-lines (jj-cmd-sync `("bookmark" "list"
                                                      ,@(jj--if-arg tracked nil "--tracked")
                                                      ,@(jj--if-arg remote #'identity "--remote")
                                                      ,@(jj--if-arg all nil "--all-remotes")
                                                      "-T" ,(jj-bookmark-list-template 'self))
                                                    :no-revert)
                                       :omit)
             unless (string-match (rx "@") line)
             collect line)))

(defun jj-list-non-tracking-local-bookmarks (&optional remote)
  "List local bookmarks that are not tracking any remote, or not tracking REMOTE if provided."
  (let ((all (jj-list-local-bookmarks nil))
        (tracked (jj-list-local-bookmarks (or remote :tracking))))
    (cl-set-difference all tracked :test #'equal)))

(defun jj-list-remote-bookmarks (&optional remote tracked not-git)
  "List remote bookmarks, optionally limited to tracked, and optionally specific to a given REMOTE. If NOT-GIT, ignore the colocated git repo as a remote."
  (cl-loop for line in (string-lines (jj-cmd-sync `("bookmark" "list"
                                                    ,@(jj--if-arg tracked nil "--tracked")
                                                    ,@(jj--if-arg remote #'identity "--remote")
                                                    ,@(jj--if-arg (not (or remote tracked)) nil "--all-remotes")
                                                    "-T" ,(jj-bookmark-list-template 'self))
                                                  :no-revert)
                                     :omit)
           nconc (when (string-match (rx string-start
                                         (group (+ (not (any "\r\n @"))))
                                         "@"
                                         (group (+ (not (any "\r\n @"))))
                                         string-end)
                                     line)
                   (let-match-string ((name)
                                      (remote))
                       line
                     (unless (and not-git (string= remote "git"))
                       (list (list name remote)))))))

(defun jj-list-untracked-remote-bookmarks (&optional remote not-git)
  "List untracked remote bookmarks, and optionally specific to a given REMOTE. If NOT-GIT, ignore the colocated git repo as a remote."
  (let ((all (jj-list-remote-bookmarks remote nil not-git))
        (tracked (jj-list-remote-bookmarks remote :tracked not-git)))
    (cl-set-difference all tracked :test #'equal)))

(defun jj-list-git-remotes ()
  "List all git remotes."
  (mapcar (lambda (s) (string-split s " " :omit))
          (string-lines (jj-cmd-sync `("git" "remote" "list")
                                     :no-revert)
                        :omit)))

(defun jj-match-revisions (&optional revset)
  "List all revisions matching REVSET, or all visible by default."
  (string-lines (jj-cmd-sync `("log" "--no-graph"
                               "-r" ,(or revset "all()")
                               "-T" ,(jj-rev-list-template 'self))
                             :no-revert)
                :omit))

(defun jj-match-revisions-annotated (&optional revset)
  "List all revisions matching REVSET, or all visible by default, as a list of (CHG-ID CMT-ID . BOOKMARKS)."
  (mapcar (lambda (s) (s-split " " s :omit))
          (string-lines (jj-cmd-sync `("log" "--no-graph"
                                       "-r" ,(or revset "all()")
                                       "-T" ,(jj-rev-list-verbose-template 'self))
                                     :no-revert)
                        :omit)))

(defun jj--workspace-root-process (&optional dir)
  "Return the root of the jj repository containing DIR, or `default-directory' if not provided. Uses jj to locate the root, rather than traversing in lisp code."
  (let ((default-directory (or dir default-directory)))
    (-let (((code . message) (jj-cmd-sync `("workspace" "root") :no-revert :no-error)))
      (pcase code
        (0 (s-chomp message))
        ;; this command uses code 1 to signal a missing repo. maybe all commands?
        (1 (signal 'jj-repo-missing-above (list default-directory)))
        (_ (error "process exited with nonzero exit code %d" code))))))

(defun jj--workspace-root-lisp (dir)
  "Return the root of the jj repository containing DIR. Performs the traversal in lisp, rather than calling on jj."
  (or (locate-dominating-file dir ".jj")
      (signal 'jj-repo-missing-above (list default-directory))))

(defalias 'jj-workspace-root 'jj--workspace-root-lisp
  "Return the root of the jj repository containing DIR, or `default-directory' if not provided.")

(defalias 'assert-jj 'jj-workspace-root
  "Throw an error unless we're in a jj repo.")

(define-error 'jj-repo-missing-above "Not within a jj repo")
(define-error 'jj-repo-missing-at "Not at a jj repo root")
;; repo query commands:1 ends here

;; generic

;; [[file:majjik.org::*generic][generic:1]]
(cl-defgeneric jj--get-revset (thing &optional default)
  "Get a revision or revset from THING, or use DEFAULT if not available."
  default)
(cl-defmethod jj--get-revset ((cmt jj-log-entry) &optional default)
  (jj-log-header-change-id (jj-log-entry-header cmt)))
(cl-defmethod jj--get-revset ((cmt jj-log-header) &optional default)
  (jj-log-header-change-id cmt))
(cl-defmethod jj--get-revset ((lin jj-status-lineage-entry) &optional default)
  (jj-status-lineage-entry-change-id lin))

(cl-defgeneric jj--get-change (thing &optional default)
  "Get a change id from THING, or use DEFAULT if not available."
  default)
(cl-defmethod jj--get-change ((cmt jj-log-entry) &optional default)
  (jj-log-header-change-id (jj-log-entry-header cmt)))
(cl-defmethod jj--get-change ((cmt jj-log-header) &optional default)
  (jj-log-header-change-id cmt))
(cl-defmethod jj--get-change ((lin jj-status-lineage-entry) &optional default)
  (jj-status-lineage-entry-change-id lin))

(cl-defgeneric jj--get-commit (thing &optional default)
  "Get a commit id from THING, or use DEFAULT if not available."
  default)
(cl-defmethod jj--get-commit ((cmt jj-log-entry) &optional default)
  (jj-log-header-commit-id (jj-log-entry-header cmt)))
(cl-defmethod jj--get-commit ((cmt jj-log-header) &optional default)
  (jj-log-header-commit-id cmt))
(cl-defmethod jj--get-commit ((lin jj-status-lineage-entry) &optional default)
  (jj-status-lineage-entry-commit-id lin))

(cl-defgeneric jj--get-description (thing &optional default)
  "Get a description from THING, or use DEFAULT if not available."
  default)
(cl-defmethod jj--get-description ((cmt jj-log-entry) &optional default)
  (jj-log-header-description (jj-log-entry-header cmt)))
(cl-defmethod jj--get-description ((cmt jj-log-header) &optional default)
  (jj-log-header-description cmt))
(cl-defmethod jj--get-description ((lin jj-status-lineage-entry) &optional default)
  (jj-status-lineage-entry-description lin))

(cl-defgeneric jj--get-bookmarks (thing &optional default)
  "Get a list of bookmarks from THING, or use DEFAULT if not available."
  default)
(cl-defmethod jj--get-bookmarks ((cmt jj-log-entry) &optional default)
  (jj-log-header-bookmarks (jj-log-entry-header cmt)))
(cl-defmethod jj--get-bookmarks ((cmt jj-log-header) &optional default)
  (jj-log-header-bookmarks cmt))
(cl-defmethod jj--get-bookmarks ((lin jj-status-lineage-entry) &optional default)
  (jj-status-lineage-entry-bookmarks lin))
;; generic:1 ends here

;; revset

;; [[file:majjik.org::*revset][revset:1]]
(defun jj-get-revset-dwim (&optional prompt)
  "Get a revision or revset based on context. E.g. from around point. If no contextual value is apparent, prompt the user explicitly with PROMPT."
  (or (jj--get-revset (jj-thing-at-point))
      (jj-read-multi-revision prompt nil nil)))
;; revset:1 ends here

;; single revision

;; [[file:majjik.org::*single revision][single revision:1]]
(defun jj-wc-revision ()
  "Returns the current working copy commit, as a list (CHG-ID CMT-ID . BMKS)."
  (let ((stat (jj-status-commit-working-copy jj--current-status)))
    `(,(jj-status-lineage-entry-change-id stat)
      ,(jj-status-lineage-entry-commit-id stat)
      ,@(jj-status-lineage-entry-bookmarks stat))))

(defun jj-get-revision-dwim (&optional prompt mutable)
  "Get a revision based on context. E.g. from around point. If no contextual value is apparent, prompt the user explicitly with PROMPT. If MUTABLE, only include mutable commits in the completion options."
  (or (jj--get-change (jj-thing-at-point))
      (jj-read-revision prompt (when mutable "~immutable()"))))
;; single revision:1 ends here

;; rev is wc

;; [[file:majjik.org::*rev is wc][rev is wc:1]]
(defun jj-rev-wc-p (rev)
  "Returns true if REV (a string) is the current working copy commit."
  (let ((stat (jj-status-commit-working-copy jj--current-status)))
    (or (equal rev (jj-status-lineage-entry-change-id stat))
        (equal rev (jj-status-lineage-entry-commit-id stat)))))

(defun jj-obj-wc-p (obj)
  "Returns true if OBJ is the current working copy commit."
  (and (jj-log-header-p obj)
       (jj-rev-wc-p (jj-log-header-change-id obj))))
;; rev is wc:1 ends here

;; any

;; [[file:majjik.org::*any][any:1]]
(defun jj-get-file-dwim (&optional prompt)
  "Get a filename based on context. E.g. from around point. If no contextual value is apparent, prompt the user explicitly with PROMPT."
  (pcase (jj-thing-at-point)
    ((and file (pred jj-status-file-untracked-p))
     (jj-status-file-untracked-path file))
    ((and file (pred jj-status-wc-change-p))
     (jj-status-wc-change-path-target file))
    (unmatched (read-file-name prompt))))
;; any:1 ends here

;; untracked

;; [[file:majjik.org::*untracked][untracked:1]]
(defun jj-get-untracked-file-dwim (&optional prompt)
  "Get an untracked filename based on context. E.g. from around point. If no contextual value is apparent, prompt the user explicitly with PROMPT."
  (pcase (jj-thing-at-point)
    ((and file (pred jj-status-file-untracked-p))
     (jj-status-file-untracked-path file))
    (unmatched (read-file-name prompt))))
;; untracked:1 ends here

;; tracked

;; [[file:majjik.org::*tracked][tracked:1]]
(defun jj-get-tracked-file-dwim (&optional prompt)
  "Get a tracked filename based on context. E.g. from around point. If no contextual value is apparent, prompt the user explicitly with PROMPT."
  (pcase (jj-thing-at-point)
    ((and file (pred jj-status-wc-change-p))
     (jj-status-wc-change-path-target file))
    (unmatched (read-file-name prompt))))
;; tracked:1 ends here

;; theseus

;; [[file:majjik.org::*theseus][theseus:1]]
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
                           ((jj-log-header-p jj-log-header-change-id))
                           equal)
      (jj-compatible-ident old new
                           ((jj-status-lineage-entry-p jj-status-lineage-entry-change-id))
                           equal)))
;; theseus:1 ends here

;; thing at point

;; [[file:majjik.org::*thing at point][thing at point:1]]
(defun jj-thing-at (pos)
  (or (get-pos-property pos 'jj-object)
      (if-let ((sec (magit-section-at pos)))
          (oref sec value))))

(defun jj-thing-at-point ()
  (or (jj-thing-at (point))
      (user-error "Nothing relevant at point")))

(defun jj-field-at-point ()
  (get-pos-property (point) 'jj-field))

(defun jj-struct-slot-value (slot inst)
  (cl-struct-slot-value
   ;; assumes first index into the structure identifies the type
   (aref inst 0)
   slot inst))

(defun jj-inspect-sexp-at-point (object &optional field)
  "When point is in a jj OBJECT, show that OBJECT's data in its own buffer.
When it is additionally on a FIELD of the OBJECT, also print that FIELD's name and value."
  (interactive (list (jj-thing-at-point) (jj-field-at-point)))
  (unless object (user-error "Not at a jj object"))
  (let ((buf (get-buffer-create "*jj-sexp*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (jj--entitize-newlines (format "%S" object)))
        (when field
          (insert "\n"
                  (format-message "field `%s': " field)
                  (jj--entitize-newlines
                   (format "%S"
                           (jj-struct-slot-value
                            field
                            object)))))))
    (pop-to-buffer buf)
    (special-mode)))

(defun jj-inspect-thing-at-point (thing)
  "When point is in an inspectable jj object, show that object in its own buffer."
  (interactive (list (jj-thing-at-point)))
  (jj-inspect-thing thing))

(cl-defgeneric jj-inspect-thing (thing)
  "Inspect THING in its own buffer if we have a method to do so."
  (user-error "Not at an inspectable jj object"))

(cl-defmethod jj-inspect-thing ((thing jj-status-file-untracked))
  "View the file at point."
  (view-file (jj-status-file-untracked-path thing)))

(cl-defmethod jj-inspect-thing ((thing jj-status-wc-change))
  "View the diff for the file at point."
  (jj-diff-at "@" (jj-files-as-fileset (jj-status-wc-change-path-target thing))))

(cl-defmethod jj-inspect-thing ((thing jj-status-lineage-entry))
  "Show the change at point."
  (jj-show (jj-status-lineage-entry-commit-id thing)))

(cl-defmethod jj-inspect-thing ((thing jj-log-entry))
  "Show the change at point."
  (jj-show (jj-log-header-commit-id (jj-log-entry-header thing))))

(cl-defmethod jj-inspect-thing ((thing jj-log-header))
  "Show the change at point."
  (jj-show (jj-log-header-commit-id thing)))
;; thing at point:1 ends here

;; swap buffers, same dir
;; might actually not use these as I need a lexical variable anyway in most cases where i'm following this pattern

;; [[file:majjik.org::*swap buffers, same dir][swap buffers, same dir:1]]
(cl-defmacro with-current-buffer-set-dir (buffer &rest body)
  "Like `with-current-buffer', but sets the `default-directory' in BUFFER to whatever is current at the call site. Does not let-bind `default-directory' - it is set persistently."
  (let ((dir-sym (gensym "buffer")))
    `(let* ((,dir-sym default-directory))
       (with-current-buffer ,buffer
         (setq-local default-directory ,dir-sym)
         ,@body))))

(cl-defmacro with-current-buffer-let-dir (buffer &rest body)
  "Like `with-current-buffer', but let-binds the `default-directory' in BUFFER for the duration of BODY to whatever is current at the call site."
  (let ((dir-sym (gensym "buffer")))
    `(let* ((,dir-sym default-directory))
       (with-current-buffer ,buffer
         (let ((default-directory ,dir-sym))
           ,@body)))))
;; swap buffers, same dir:1 ends here

;; sync command utils

;; [[file:majjik.org::*sync command utils][sync command utils:1]]
(defun jj-cmd-sync (cmd &optional no-revert no-error)
  "Call jj with the given CMD, passing the default args first, and returning the output as a string. Signals an error if the command returns a nonzero exit code. When the command completes successfully, reverts the dash buffer for the repo (if there is one, and NO-REVERT was nil).
When NO-ERROR, return the error code instead of raising an error. See `call-cmd' for details."
  (let ((cmd `("jj"
               ,@jj-global-default-args
               ;; never debug, because it has combined output and error streams
               ;; ,@(and jj-do-debug jj-global-debug-args)
               ,@jj-parsing-default-args
               ,@cmd)))
    (prog1
        (call-cmd cmd nil :string nil no-error)
      (unless no-revert
        (jj-revert-dash-buffer default-directory)))))
;; sync command utils:1 ends here

;; jj bookmark list

;; [[file:majjik.org::*jj bookmark list][jj bookmark list:1]]
(defun jj-bookmark-list ()
  (interactive)
  (message "%s"
           (s-chomp
            (jj-cmd-sync `("bookmark" "list")
                         :no-revert))))
;; jj bookmark list:1 ends here

;; emacsclient as editor

;; [[file:majjik.org::*emacsclient as editor][emacsclient as editor:1]]
(defun jj--editor-path ()
  (if (string-prefix-p "~" with-editor-emacsclient-executable)
      (concat (expand-file-name "~")
              (substring with-editor-emacsclient-executable 1))
    with-editor-emacsclient-executable))

(defun jj--editor-server-path-arg ()
  (if server-use-tcp
      (concat "--server-file="
              (expand-file-name server-name
                                server-auth-dir))
    (concat "--socket-name="
            (expand-file-name server-name
                              server-socket-dir))))

(defun jj--config-arg (key value)
  "Format a KEY VALUE pair as a config argument for jj.
Both KEY and VALUE can be strings or lists. If KEY is a list, it is formatted as a dotted path, with path elements quoted if needed. If VALUE is a list, it is formatted as a toml list of strings."
  (format "--config=%s=%s"
          (jj--config-key key)
          (jj--config-value value)))
(cl-defmethod jj--config-key ((key string))
  key)
(cl-defmethod jj--config-key ((key list))
  (mapconcat #'jj--maybe-quote-argument key "."))

(cl-defmethod jj--config-value ((value string))
  (prin1-to-string value))
(cl-defmethod jj--config-value ((value list))
  (jj--toml-list
   (mapcar #'prin1-to-string value)))

(defun jj--merge-tool-args (entry-point-exp)
  "Returns the arguments needed to register emacs as the merge tool for a JJ command.
ENTRY-POINT-EXP must be a quoted sexpression that can handle the file arguments for emacs to run as a mergetool. `jj--call-from-cli' is designed for this purpose.
Returns a plist of arguments to jj: --config to set up a merge-tool, and --tool to select it."
  ;; this is almost right.
  ;; left and right are actually directories most of the time, not files.
  (let* ((program (jj--editor-path))
         (edit-args
          `(,(jj--editor-server-path-arg)
            ;; need a new frame, or `jj--call-from-cli' won't work
            "-c"
            "-e" ,(prin1-to-string
                   entry-point-exp)
            ;; always pass the files as the last arguments
            "--" "$left" "$right")))
    `(,(jj--config-arg "merge-tools.emacs.program" program)
      ,(jj--config-arg "merge-tools.emacs.edit-args" edit-args)
      "--tool" "emacs")))

(defmacro jj-with-editor (&rest body)
  "Specify emacs as the editor for jj commands invoked from BODY."
  `(dlet ((jj-current-dynamic-args
           `(,@(jj--editor-args)
             ,@jj-current-dynamic-args)))
     ,@body))

(defun jj--editor-args ()
  "Returns the arguments needed to register emacs as the editor for a JJ command."
  (let* ((program (jj--editor-path))
         (server (jj--editor-server-path-arg))
         (editor `(,program ,server)))
    `(,(jj--config-arg "ui.editor" editor))))

(defun jj--toml-list (list)
  (format "[%s]"
          (s-join "," list)))
;; emacsclient as editor:1 ends here

;; post-command tasks

;; [[file:majjik.org::*post-command tasks][post-command tasks:1]]
(defun jj--proc-revert-silently (proc)
  (jj--revert-silently
     (buffer-local-value 'default-directory
                         (process-buffer (jj-process-process proc)))))
(defun jj--revert-silently (dir)
  (let ((jj--silent-revert t))
    ;; set silent revert, rather than inhibiting messages
    ;; because it's asynchronous, so the binding wouldnt last, and
    ;; because we only want to prevent non-error messages
    ;; and the revert function looks for silent-revert to suppress those.
    (jj-revert-dash-buffer-async dir)))

(defun jj--proc-post-message (&rest builders)
  (lambda (proc)
    (message "%s" (mapconcat (lambda (fn)
                               (funcall fn proc))
                             builders))))

(defun jj--proc-trim-with-editor (proc)
  "Delete all occurrences of the \"Waiting for Emacs\" message from the start of PROC's output buffer."
  (let ((buf (process-buffer (jj-process-process proc))))
    (with-current-buffer buf
      (jj--trim-with-editor))))
(defun jj--trim-with-editor ()
  "Delete all occurrences of the \"Waiting for Emacs\" message from the start of the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (looking-at (rx line-start
                           "Waiting for Emacs..."
                           line-end))
      (delete-line))))

(defun jj--proc-format-simple-ok (proc)
  (jj--format-simple-ok (process-name (jj-process-process proc))))
(defun jj--format-simple-ok (name)
  (format-message "`jj %s' %s. " (jj--replace-newlines name)
          (propertize "ok" 'face 'success)))

(defun jj--proc-format-simple-fail (proc)
  (jj--format-simple-fail (process-name (jj-process-process proc))))
(defun jj--format-simple-fail (name)
  (format-message "`jj %s' %s. " (jj--replace-newlines name)
          (propertize "failed" 'face 'error)))

(defun jj--proc-format-see-log (_proc)
  (jj--format-see-log))
(defun jj--format-see-log ()
  (let ((msg (substitute-command-keys "\\[jj-pop-to-command-log]")))
    (format "Type %s to see full logs." msg)))

(defun jj--proc-format-see-secret-log (_proc)
  (jj--format-see-secret-log))
(defun jj--format-see-secret-log ()
  (let ((msg (substitute-command-keys "\\[jj-pop-to-command-log] \\<jj-process-mode-map>\\[jj-set-verbosity-level]")))
    (format "Type %s to see system error logs." msg)))

(defun jj--proc-format-stderr-verbose (proc)
  (jj--format-buffer-content
   (process-buffer (jj-process-process proc))
   (lambda ()
     (format "no error output. %s" (jj-process-event proc)))))

(defun jj--proc-format-stderr (proc)
  (jj--format-buffer-content (jj-process-stderr proc)))

(defun jj--proc-format-trimmed (log-referrer &rest builders)
  (declare (indent 1))
  (lambda (proc)
    (let ((string)
          (truncated))
      (with-temp-buffer
        (mapc (lambda (fn)
                (insert (funcall fn proc)))
              builders)
        (goto-char (point-min))
        (forward-line 10)
        (setq string
              (buffer-substring (point-min) (point)))
        (setq truncated (not (eobp))))
      (cond ((jj--empty-string-p string)
             "")
            (truncated
             (format "%s\n%s\n%s"
                     (s-chomp string)
                     (propertize "... truncated" 'face 'shadow)
                     (funcall log-referrer proc)))
            (:else
             (s-chomp string))))))

(defun jj--proc-format-stdout-verbose (proc)
  (jj--format-buffer-content
   (process-buffer (jj-process-process proc))
   (lambda ()
     (format "no standard output. %s" (jj-process-event proc)))))

(defun jj--proc-format-stdout (proc)
  (jj--format-buffer-content (process-buffer (jj-process-process proc))))

(defun jj--format-buffer-content (buf &optional on-empty)
  (let ((str (with-current-buffer buf
               (jj--apply-font-lock-properties
                (buffer-string)))))
    (if (jj--empty-string-p str)
        (if on-empty
            (funcall on-empty)
          "")
      (concat "\n" (s-chomp str)))))

(defun jj--proc-kill-output (proc)
  (kill-buffer (process-buffer (jj-process-process proc))))
(defun jj--proc-kill-error (proc)
  (kill-buffer (jj-process-stderr proc)))

(defun jj--proc-print-crash (proc)
  (jj--insert-crash-event (jj-process-stderr proc) (jj-process-event proc)))

(defun jj--proc-view-output (proc)
  (jj--view-buf (process-buffer (jj-process-process proc))))
(defun jj--proc-view-error (proc)
  (jj--view-buf (jj-process-stderr proc)))
(defun jj--view-buf (buf)
  (if (jj--empty-buffer-p buf)
      (kill-buffer buf)
    (with-current-buffer buf
      (font-lock-mode)
      (view-mode-enter nil #'kill-buffer))
    (pop-to-buffer buf)))
;; post-command tasks:1 ends here

;; helpers

;; [[file:majjik.org::*helpers][helpers:1]]
(defun jj--finally (promise f)
  "Like `promise-finally', but call F with an argument - the value or error from the PROMISE. If F is nil, just return the PROMISE."
  (if f
      (promise-then
       promise
       (lambda (value)
         (promise-then (promise-resolve (funcall f value))
                       (lambda (_) value)))
       (lambda (err)
         (promise-then (promise-resolve (funcall f err))
                       (lambda (_)
                         (promise-reject err)))))
    promise))

(defmacro jj--call-each (&rest pipeline)
  "Return a lambda that gives its argument to each non-nil function in PIPELINE, then returns that argument."
  `(lambda (x)
     (prog1 x
       ,@(cl-loop for func in pipeline
                  collect `(when-let ((f ,func))
                             (funcall f x))))))

(defun jj--const (ok)
  "Return a function that runs OK on its argument before returning the argument unmodified."
  (lambda (val)
    (prog1 val
      (funcall ok val))))

(defun jj--const-err (err)
  "Return a function that runs ERR on its argument before returning the async rejection of the argument."
  (lambda (val)
    (prog1 (promise-reject val)
      (funcall err val))))

(defun jj--peek (promise &optional ok err final)
  "Run PROMISE, calling OK on success, ERR on error, and FINALLY regardless, and return the unmodified result of PROMISE."
  (jj--finally
   (promise-then
    promise
    (and ok (jj--const ok))
    (and err (jj--const-err err)))
   final))
;; helpers:1 ends here

;; entry point


;; [[file:majjik.org::*entry point][entry point:1]]
(defun jj-cmd-async (cmd &optional output-buffer silent)
  "Run jj command CMD asynchronously."
  (let* ((name (jj--cmd-abbrev cmd))
         (buf (or output-buffer (generate-new-buffer (format "*jj %s*" name)))))
    (jj-cmd-async-named name cmd buf silent)))

(defalias 'jj-cmd-async-named 'jj-cmd-promise)
;; entry point:1 ends here

;; async command plumbing

;; [[file:majjik.org::*async command plumbing][async command plumbing:1]]
(cl-defstruct jj-process
  "A `process' object and various associated data that isn't otherwise stored directly, for easier use in async programs."
  (process nil
           :type process
           :documentation "The process object itself."
           :read-only t)
  (stderr nil
          :type buffer
          :documentation "The buffer assigned to process stderr output when it was created."
          :read-only t)
  (log-entry nil
             :type jj--process-log-entry
             :documentation "The handle to the process's log entry"
             :read-only t)
  (event nil
         :type string
         :documentation "The final event after the process has exited, otherwise nil.")
  (repo nil
        :type string
        :documentation "The repository in which the corresponding command was run"))

(defun jj-cmd-promise (name cmd &optional output-buffer silent)
  "Run CMD asynchronously, returning a promise of its completion.

On success, returns the (PROCESS ERR-BUF). On error, returns (PROCESS ERR-BUF EVENT). If OUTPUT-BUFFER, use that buffer for stdout instead of a temp indirect buffer."
  (declare (indent 2))
  (unless silent
    (message "`jj %s'..." (jj--replace-newlines name)))
  (jj-cmd--promise name cmd output-buffer))

(defun jj-cmd-futur (name cmd &optional output-buffer silent)
  "Run CMD asynchronously, returning a futur of its completion.

On success, returns the (PROCESS ERR-BUF). On error, returns (PROCESS ERR-BUF EVENT). If OUTPUT-BUFFER, use that buffer for stdout instead of a temp indirect buffer."
  (declare (indent 2))
  (unless silent
    (message "`jj %s'..." name))
  (jj-cmd--futur name cmd output-buffer))

(defun jj--apply-font-lock-properties (string)
  "Within STRING, add a `face' property for every `font-lock-face' property."
  (with-temp-buffer
    (insert string)
    (cl-loop for start = (point-min) then pos
             for pos = (next-property-change start)
             for end = (or pos
                           (point-max))
             for props = (text-properties-at start)
             do (when-let ((face (plist-get props 'font-lock-face)))
                  (plist-put props 'face face))
             while pos)
    (buffer-string)))

;; copied and heavily modified from `comint-osc-process-output'
(defun jj--make-ansi-color-multi-filter (buffers)
  "Process filter for writing to multiple output buffers. Converts ANSI color sequences in the output.
Does not use `process-mark', but instead manages internal alist of markers per buffer."
  (let ((buf-proc-mark-alist))
    ;; alist mapping buffers to sets of markers.
    ;; each such set is a cons of a marker and an alist.
    ;; the first marker is the position of point at the time of this call, serving as the default position.
    ;; each of the alists is mapping processes to markers within that buffer.
    (dolist (buf buffers)
      ;; set the default marker for each buffer
      (push `(,buf ,(copy-marker (point)) . nil)
            buf-proc-mark-alist))
    (cl-labels ((marker (buf proc)
                  (-let* ((m-pma (alist-get buf buf-proc-mark-alist))
                          ((default-mark . proc-mark-alist)
                           m-pma))
                    (if-let ((proc-mark (alist-get proc proc-mark-alist)))
                        proc-mark
                      (let ((mark (copy-marker default-mark)))
                        (push `(,proc . ,mark)
                              ;; need to push to an existing object
                              ;; otherwise I'm just mutating a local
                              (cdr m-pma))
                        mark)))))
      (lambda (proc string)
        (dolist (buf buffers)
          ;; insert in each buffer
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (let* ((inhibit-read-only t)
                     ;; get the process marker for the current buffer
                     (mark (marker buf proc)))
                (save-excursion
                  (goto-char mark)
                  ;; this already handles partial sequences, and assumes
                  ;; sequential calls apply to contiguous chunks of output.
                  ;; it must be run in the buffer it's inserting into.
                  (insert (ansi-color-apply string))
                  ;; advance the process marker in this buffer
                  (set-marker mark (point)))))))))))

(defun jj-cmd--with-standard-args (cmd)
  "Return CMD with added arguments for using emacs as editor, and all the applicable configured arguments for a logging command."
  `(,@(jj-cmd--standard-args)
    ,@cmd))

(defun jj-cmd--standard-args ()
  "Return CMD with added arguments for using emacs as editor, and all the applicable configured arguments for a logging command."
  `(,@jj-current-dynamic-args
    ,@jj-global-default-args
    ,@(and jj-do-debug jj-global-debug-args)
    ,@jj-logging-default-args))

(defun jj-cmd--promise (name cmd &optional output-buffer)
  "Run CMD asynchronously, returning a promise that is resolved (returning the process) on completion."
  (declare (indent 2))
  (let ((repo-dir default-directory))
    (let ((full-cmd `(,@(jj-cmd--with-standard-args cmd)))
          (min-cmd `(,@cmd))
          (hide-args (jj-cmd--standard-args))
          (error-verbosity jj--cmd-error-verbosity))
      (pcase-let (((and proc-entry
                        (cl-struct jj--process-log-entry
                                   buf-code
                                   buf-stdout
                                   buf-stderr
                                   ovl-control
                                   ovl-collapse
                                   ovl-err))
                   (jj--make-process-log-entry name cmd min-cmd hide-args)))
        (promise-new
         (lambda (resolve reject)
           (let* ((proc (make-process
                         :name name
                         :buffer (or output-buffer
                                     buf-stdout)
                         :stderr buf-stderr
                         :sentinel (jj--make-update-exit-code-sentinel buf-code)
                         :filter (jj--make-ansi-color-multi-filter `(,buf-stdout
                                                                     ,@(opt output-buffer)))
                         :noquery t
                         :command `("jj" ,@full-cmd)))
                  (jj-proc (make-jj-process :process proc
                                            :stderr buf-stderr
                                            :log-entry proc-entry
                                            :repo repo-dir)))
             (setf (jj--process-log-entry-process proc-entry) proc)
             (overlay-put ovl-control 'jj-object proc-entry)
             (jj--set-collapse-process proc-entry t)
             ;; (overlay-put ovl-err 'font-lock-face '(:foreground "grey"))
             (jj--set-initial-run-status buf-code)
             (jj--set-entry-verbosity proc-entry jj--cmd-verbosity)

             (let ((stderr (get-buffer-process buf-stderr)))
               ;; print any abnormal process termination
               (set-process-sentinel stderr
                                     (jj--make-print-status-sentinel buf-stderr))
               (set-process-filter stderr
                                   (jj--make-ansi-color-multi-filter `(,buf-stderr))))
             ;; this always runs before the subsequent promise callbacks,
             ;; which means `resolve' and `reject' are not themselves callbacks
             (add-function :after (process-sentinel proc)
                           (apply #'jj--make-cleanup-sentinel
                                  ;; also kill the stdout buffer when an output buffer is specified
                                  `(,buf-code
                                    ,@(opt (when output-buffer
                                             buf-stdout)))))
             
             (add-function :after (process-sentinel proc)
                           (make-jj-callback-sentinel
                            (lambda (exit-status event)
                              ;; record the process-end event
                              (setf (jj-process-event jj-proc) event)
                              ;; handle the promise state. this is basically the return-value
                              (if (eq exit-status 0)
                                  (funcall resolve jj-proc)
                                ;; change visibility on error
                                (jj--set-entry-verbosity proc-entry error-verbosity)
                                (funcall reject jj-proc))))))))))))

(defun jj--set-entry-verbosity (proc-entry verbosity)
  "Set the verbosity level for PROC-ENTRY to VERBOSITY."
  (overlay-put (jj--process-log-entry-ovl-control proc-entry)
               'invisible verbosity))

(defun jj-cmd--futur (name cmd &optional output-buffer)
  "Run CMD asynchronously, returning a futur that is resolved (returning the process) on completion."
  (declare (indent 2))
  (let ((repo-dir default-directory))
    (let ((full-cmd `("jj" ,@(jj-cmd--with-standard-args cmd)))
          (min-cmd `("jj" ,@cmd)))
      (pcase-let (((and proc-entry
                        (cl-struct jj--process-log-entry
                                   buf-code
                                   buf-stdout
                                   buf-stderr
                                   ovl-control
                                   ovl-collapse
                                   ovl-err))
                   (jj--make-process-log-entry jj--cmd-verbosity name full-cmd min-cmd)))
        (futur-new
         (lambda (f)
           (let ((proc (make-process
                        :name name
                        :buffer (or output-buffer buf-stdout)
                        :stderr buf-stderr
                        :sentinel (jj--make-update-exit-code-sentinel buf-code)
                        :filter #'jj--ansi-color-filter
                        :noquery t
                        :command full-cmd))
                 (jj-proc (make-jj-process :process proc
                                           :stderr buf-stderr
                                           :log-entry proc-entry)))
             (setf (jj--process-log-entry-process proc-entry) proc)
             (overlay-put ovl-control 'jj-object proc-entry)
             (jj--set-collapse-process proc-entry t)
             ;; (overlay-put ovl-err 'font-lock-face '(:foreground "grey"))
             (jj--set-initial-run-status buf-code)

             (when output-buffer
               (kill-buffer buf-stdout))
             (let ((stderr (get-buffer-process buf-stderr)))
               ;; print any abnormal process termination
               (set-process-sentinel stderr
                                     (jj--make-print-status-sentinel buf-stderr))
               (set-process-filter stderr
                                   #'jj--ansi-color-filter))
             ;; this always runs before the subsequent futurs,
             ;; which means they are not themselves callbacks
             (add-function :after (process-sentinel proc)
                           (jj--make-cleanup-sentinel buf-code))
             ;; handle the promise state
             (add-function :after (process-sentinel proc)
                           (make-jj-callback-sentinel
                            (lambda (exit-status event)
                              ;; record the process-end event
                              (setf (jj-process-event jj-proc) event)
                              ;; handle the futur state. this is basically the return-value
                              (if (eq exit-status 0)
                                  (futur-deliver-value f jj-proc)
                                (futur-blocker-abort f jj-proc))))))))))))
;; async command plumbing:1 ends here

;; transient command utils

;; [[file:majjik.org::*transient command utils][transient command utils:1]]
(defun transient--any-on-p (&rest switches)
  (transient--with-emergency-exit :get-value
    (-some (lambda (obj)
             (and (not (oref obj inactive))
                  (not (oref obj inapt))
                  ;; it is an infix, so it has an argument slot
                  (obj-of-class-p obj 'transient-infix)
                  ;; argument is one we asked for
                  (cl-member
                   (oref obj argument)
                   switches
                   :test #'string=)
                  ;; value is set
                  (oref obj value)))
           transient--suffixes)))

(defun transient--all-on-p (&rest switches)
  "Returns if any of SWITCHES exist for this prefix, and are not set.
Note: this returns possibly-unexpected results for nonexistent switches - they are always on."
  (transient--with-emergency-exit :get-value
    (-all-p (lambda (obj)
              ;; value is set
              (oref obj value))
            (-filter (lambda (obj)
                       (and (not (oref obj inactive))
                            (not (oref obj inapt))
                            ;; it is an infix, so it has an argument slot
                            (obj-of-class-p obj 'transient-infix)
                            ;; argument is one we asked for
                            (cl-member
                             (oref obj argument)
                             switches
                             :test #'string=)))
                     transient--suffixes))))

(transient-define-argument jj-single-revision-argument ()
  :class 'transient-option
  :multi-value nil
  :always-read nil
  :reader #'jj-read-single-revision)

(transient-define-argument jj-multi-revision-argument ()
  :class 'transient-option
  :multi-value 'repeat
  :always-read nil
  :reader #'jj-read-multi-revision)

(defun jj--ensure-arg (args arg reader formatter)
  "If ARG is not present in ARGS, push the result of calling READER and FORMATTER.
READER should be a function of no args.
FORMATTER should be a function of 2 arguments: the ARG, and the value returned by READER."
  (if (transient-arg-value arg args)
      args
    (cons (funcall formatter arg (funcall reader)) args)))

(defun jj--transient-args ()
  (transient-args (oref transient-current-prefix command)))
;; transient command utils:1 ends here

;; command log mode

;; [[file:majjik.org::*command log mode][command log mode:1]]
(defvar-keymap jj-process-mode-map
  :parent jj-inspect-mode-map
  "TAB" #'jj--toggle-collapse-process-at-point
  "k" #'jj--kill-process-at-point
  "$" #'jj-set-verbosity-level)

(defvar-keymap jj-process-mode--verbosity-modeline-map
  "<mode-line> <mouse-1>" #'jj-set-verbosity-level)

(define-derived-mode jj-process-mode jj-inspect-mode
  `("JJ process"
    (:propertize (:eval (format "/%s" (jj--get-numeric-verbosity-level jj--cmd-show-verbosity)))
                 help-echo (format "Verbosity level: %s" jj--cmd-show-verbosity)
		 face warning
		 mouse-face mode-line-highlight
                 local-map ,jj-process-mode--verbosity-modeline-map))
  "Major mode for jj process buffer."
  (jj-set-verbosity-level jj--cmd-show-verbosity))

(defun jj--empty-string-p (string)
  "Returns non-nil when STRING is only unicode whitespace."
  ;; negate match. no non-space = all space
  (not (string-match-p
        ;; match any non-space anywhere
        (rx (not (any space)))
        string)))

(defun jj--empty-buffer-p (buffer)
  "Returns non-nil when BUFFER contains only unicode whitespace."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      ;; negate match. no non-space = all space
      (not (re-search-forward
            ;; match any non-space anywhere
            (rx (not (any space)))
            nil :noerrror)))))

(defun jj--overlay-text (ovl)
  (with-current-buffer (overlay-buffer ovl)
    (buffer-substring (overlay-start ovl) (overlay-end ovl))))

(defun jj--kill-process-at-point (pos)
  "Try to kill the process at point.
Sometimes this does not actually succeed at killing the process."
  (interactive "d")
  (jj--signal-process-at-point pos 'SIGKILL))

(defun jj--interrupt-process-at-point (pos)
  "Try to interrupt the process at point."
  (interactive "d")
  (jj--signal-process-at-point pos 'SIGINT))

(defun jj--signal-process-at-point (pos signal)
  "Send SIGNAL to the process at POS."
  (interactive "d")
  (if-let ((proc-entry (jj-thing-at pos)))
      (if (jj--process-log-entry-p proc-entry)
          (if-let ((proc (jj--process-log-entry-process proc-entry)))
              (if (process-live-p proc)
                  (progn
                    (pcase (process-running-child-p proc)
                      ('nil (message "proc has no child"))
                      ('t (message "proc has anonymous child process"))
                      (child (message "proc has child process %s" child)
                             (when (yes-or-no-p "signal child?")
                               (signal-process child signal))))
                    (message "sending %s to `%s'" signal proc)
                    (signal-process proc signal))
                (user-error "process `%s' already stopped" proc))
            (user-error "process %s has not started" (jj--process-log-entry-name proc-entry)))
        (user-error "not a process: %s" proc-entry))
    (user-error "not at a jj object")))

(defun jj--toggle-collapse-process-at-point (pos)
  "Given a `jj--process-log-entry' at POS, toggle its collapsed state, or collapse it if it is empty."
  (interactive "d")
  (jj--toggle-collapse-process
   (or (jj-thing-at pos)
       (user-error "No process at point"))))

(defun jj--toggle-collapse-process (proc-entry)
  "Given a `jj--process-log-entry' PROC-ENTRY, toggle its collapsed state, or collapse it if it is empty."
  (pcase-exhaustive proc-entry
    ((cl-struct jj--process-log-entry
                ovl-control
                ovl-collapse)
     (let ((collapsed (overlay-get ovl-collapse 'jj--section-collapsed)))
       ;; update value
       (setq collapsed (or
                        ;; if output is empty, always collapse
                        (jj--empty-string-p (jj--overlay-text ovl-collapse))
                        ;; otherwise, toggle. by default, initially collapsed
                        (not collapsed)))
       ;; set properties to action the new `collapsed' value
       (jj--set-collapse-process proc-entry collapsed)))))

(defun jj--set-collapse-process (proc-entry collapsed)
  "Given a `jj--process-log-entry' PROC-ENTRY, set it to collapse if COLLAPSED, otherwise expand it."
  (pcase-exhaustive proc-entry
    ((cl-struct jj--process-log-entry
                ovl-control
                ovl-collapse
                ovl-args)
     ;; store for next time
     (overlay-put ovl-collapse
                  'jj--section-collapsed
                  collapsed)
     ;; the cursor jumping is because of this.
     ;; commented out, no jumping.
     (overlay-put ovl-control
                  'before-string
                  (concat
                   ;; i've put 2 hidden spaces to work around some display quirks.
                   ;; for some reason, if the before-string is invisible,
                   ;; and the display property is nil (conditional or otherwise),
                   ;; then the colour disappears from the exit code.
                   (propertize " " 'display
                               ;; hide fringe when content empty
                               `(when (not (jj--empty-string-p (jj--overlay-text ,ovl-collapse)))
                                  left-fringe
                                  ;; update fringe to match expand-collapse state.
                                  ,(if collapsed 'jj-fringe-bitmap> 'jj-fringe-bitmapv)
                                  fringe
                                  )
                               'invisible t
                               'cursor-intangible t)
                   (propertize " " 'display "")))
     (overlay-put ovl-collapse
                  'display
                  ;; there's no way to remove an overlay property except by
                  ;; setting it nil. this still overrides the corresponding
                  ;; text properties, unfortunately
                  (and collapsed jj--process-ellipsis))
     (overlay-put ovl-args
                  'display
                  ;; there's no way to remove an overlay property except by
                  ;; setting it nil. this still overrides the corresponding
                  ;; text properties, unfortunately
                  (and collapsed (propertize "..." 'face 'shadow))))))

(defvar jj--process-ellipsis ""
  "String to show instead of process output when collapsing.")
;; command log mode:1 ends here

;; primary command invokers
;; main functions intended to run commands for various purposes:
;; - for producing the dashboard buffer:
;;   - silent start
;;   - silent on ok
;;   - stdout is only killed on error - the command is run for its output
;;   - errors direct user to check secret buffer
;;   - stderr is killed always
;; - for showing in the echo area
;;   - verbose ok message, showing stdout and stderr
;;   - stdout is killed always
;;   - stderr is killed always
;; - for showing in own buffer
;;   - simple ok message, showing stderr only
;;   - show output in buffer, which is killed on close
;;   - stdout is only killed on error, unless empty
;;   - stderr is killed always

;; [[file:majjik.org::*primary command invokers][primary command invokers:1]]
(defun jj-cmd-async--for-status (name cmd)
  "Run CMD for the purposes of getting its output buffer, for producing the jj dhashboard.

This means on failure refer to secret log and kill associated buffers, and on success, just kill the error buffer.
Returns the raw process, not the combined handler."
  (declare (indent 1))
  (let ((jj--cmd-verbosity 'system)
        (jj--cmd-error-verbosity 'system-error))
    (promise-then (jj--peek
                   (jj-cmd-async-named name cmd nil :silent)
                   nil
                   (jj--call-each
                    (jj--proc-post-message
                     (jj--proc-format-trimmed
                         #'jj--proc-format-see-secret-log
                       #'jj--proc-format-simple-fail
                       #'jj--proc-format-stderr))
                    #'jj--proc-print-crash
                    #'jj--proc-kill-output)
                   #'jj--proc-kill-error)
                  #'jj-process-process)))

;; TODO there's some lingering buffers left around, possibly by this.
;; look into it.
(defun jj-cmd-async-view (cmd &optional no-revert verbose-error)
  "Run jj command CMD asynchronously, view the stdout output in its own buffer (if there is any), and message the stderr output in the echo area."
  (declare (indent 1))
  (jj--peek
   (jj-cmd-async cmd)
   (jj--call-each
    #'jj--proc-trim-with-editor
    (jj--proc-post-message
     (jj--proc-format-trimmed
         #'jj--proc-format-see-log
       #'jj--proc-format-simple-ok
       #'jj--proc-format-stderr))
    #'jj--proc-view-output)
   (jj--call-each
    (if verbose-error
        (jj--proc-post-message
         #'jj--proc-format-simple-fail
         #'jj--proc-format-stderr)
      (jj--proc-post-message
       (jj--proc-format-trimmed
           #'jj--proc-format-see-log
         #'jj--proc-format-simple-fail
         #'jj--proc-format-stderr)))
    #'jj--proc-print-crash
    #'jj--proc-kill-output)
   (jj--call-each
    (unless no-revert
      #'jj--proc-revert-silently)
    #'jj--proc-kill-error)))

(defun jj-cmd-async-message (cmd &optional no-revert verbose-error)
  "Run jj command CMD asynchronously, and message the combined stdout and stderr output in the echo area."
  (declare (indent 1))
  (jj--peek
   (jj-cmd-async cmd)
   (jj--call-each
    #'jj--proc-trim-with-editor
    (jj--proc-post-message
     (jj--proc-format-trimmed
         #'jj--proc-format-see-log
       #'jj--proc-format-simple-ok
       #'jj--proc-format-stdout
       #'jj--proc-format-stderr)))
   (jj--call-each
    (if verbose-error
        (jj--proc-post-message
         #'jj--proc-format-simple-fail
         #'jj--proc-format-stderr)
      (jj--proc-post-message
       (jj--proc-format-trimmed
           #'jj--proc-format-see-log
         #'jj--proc-format-simple-fail
         #'jj--proc-format-stderr)))
    #'jj--proc-print-crash)
   (jj--call-each
    (unless no-revert
      #'jj--proc-revert-silently)
    #'jj--proc-kill-error
    #'jj--proc-kill-output)))
;; primary command invokers:1 ends here

;; entry

;; [[file:majjik.org::*entry][entry:1]]
(transient-define-prefix jj-diff-entry-prefix ()
  [["diff"
    ("d" "diff" jj-diff-prefix)
    ("i" "interdiff" jj-interdiff-prefix)]
   ["show"
    ("s" "show" jj-show-prefix)]])
;; entry:1 ends here

;; common

;; [[file:majjik.org::*common][common:1]]
(transient-define-group jj-diff-formats-group
  ["summary formats"
   ("-T" "template" "--template=")
   ("-s" "summary" "--summary")
   ("-S" "stat" "--stat")
   ("-Y" "types" "--types")
   ("-N" "name-only" "--name-only")]
  ["diff formats"
   ("-T" "template" "--template=")
   ("-g" "git" "--git")
   ("-l" "tool" "--tool=")
   ("-c" "color-words" "--color-words")]
  ["meta"
   ("-C" "context" "--context="
    :reader (lambda (p s h)
              (number-to-string (read-number p s h))))
   ("-w" "ignore all space" "--ignore-all-space")
   ("-b" "ignore space change" "--ignore-space-change")])

(defvar jj-diff-format-incompatibilities
  `(("--template=" "--summary" "--stat" "--types" "--name-only")
    ("--template=" "--tool=" "--git" "--color-words")
    ("--ignore-all-space" "--ignore-space-change")))
;; common:1 ends here

;; diff

;; [[file:majjik.org::*diff][diff:1]]
(transient-define-prefix jj-diff-prefix ()
  :refresh-suffixes t
  :incompatible `(,@(prod-cartes '("-r") '("-f" "-t"))
                  ,@jj-diff-format-incompatibilities)
  jj-global-group
  [["targets"
    ("-r" "revisions" jj-multi-revision-argument
     :argument "-r")
    ("-f" "from" jj-single-revision-argument
     :argument "-f")
    ;; TODO fileset argument: --
    ("-t" "to" jj-single-revision-argument
     :argument "-t")]
   jj-diff-formats-group]
  ["go"
   ("d" "diff" (lambda (args)
                 (interactive (list (jj--transient-args)))
                 (jj-cmd-async-view `("diff" ,@args)))
    :inapt-if-not (lambda () (or (transient--all-on-p "-f" "-t")
                                 (transient--any-on-p "-r"))))])
;; diff:1 ends here

;; interdiff

;; [[file:majjik.org::*interdiff][interdiff:1]]
(transient-define-prefix jj-interdiff-prefix ()
  :refresh-suffixes t
  :incompatible jj-diff-format-incompatibilities
  jj-global-group
  [["targets"
    ("-f" "from" jj-single-revision-argument
     :argument "-f")
    ;; TODO fileset argument: --
    ("-t" "to" jj-single-revision-argument
     :argument "-t")]
   jj-diff-formats-group]
  ["go"
   ("i" "interdiff" (lambda (args)
                 (interactive (list (jj--transient-args)))
                 (jj-cmd-async-view `("interdiff" ,@args)))
    :inapt-if-not (lambda () (transient--all-on-p "-f" "-t")))])
;; interdiff:1 ends here

;; show

;; [[file:majjik.org::*show][show:1]]
(transient-define-prefix jj-show-prefix ()
  :refresh-suffixes t
  :incompatible jj-diff-format-incompatibilities
  jj-global-group
  [["targets"
    ("-r" "revision" jj-single-revision-argument
     :argument "-r")]
   jj-diff-formats-group]
  ["go"
   ("s" "show" (lambda (args)
                   (interactive (list (jj--transient-args)))
                   (jj-cmd-async-view `("show" ,@args)))
    :inapt-if-not (lambda () (transient--any-on-p "-r")))])
;; show:1 ends here

;; jj diff

;; [[file:majjik.org::*jj diff][jj diff:1]]
(cl-defun jj-diff (&key at from to fileset)
  "View the diff for AT, or between FROM and TO, optionally limited to files in FILESET."
  (jj-cmd-async-view
   `("diff"
     "--git"
     ,@(jj--if-arg at #'identity "--revisions")
     ,@(jj--if-arg from #'identity "--from")
     ,@(jj--if-arg to #'identity "--to")
     "--"
     ,@(jj--if-arg fileset #'identity nil))
   :no-revert))

(defun jj-diff-at (revset &optional fileset)
  "View the diff for REVISION, optionally limited to files in FILESET."
  (jj-diff :at revset :fileset fileset))

(defun jj-diff-from-to (from to &optional fileset)
  "View the diff between FROM and TO, optionally limited to files in FILESET."
  (jj-diff :from from :to to :fileset fileset))
;; jj diff:1 ends here

;; jj show

;; [[file:majjik.org::*jj show][jj show:1]]
(cl-defun jj-show (commit &optional fileset)
  "View COMMIT, optionally limited to files in FILESET."
  (jj-cmd-async-view
   `("show"
     "--git"
     "-r" ,commit
     ,@(jj--if-arg fileset #'identity "--"))
   :no-revert))
;; jj show:1 ends here

;; anything
;; run arbitrary jj commands. View the output if there is any.
;; technically run any arbitrary command, since ~jj util exec~ exists

;; seems to sometimes have the sentinel snipe the filter, so e.g. ~jj util exec echo hello~ registers no output. but the sentinel *always* waits for the filter, unless the filter yields. so this is a mystery.

;; after rebooting emacs, it no longer occurs. maybe an ephemeral glitch.

;; [[file:majjik.org::*anything][anything:1]]
(defvar jj-cmd-hist nil
  "History for `jj-cmd' command.")

(defun jj-cmd (args)
  "Run jj with arbitrary command line ARGS, which is a list of strings. If there is any output, view it in its own buffer.
Will likely fail for any interactive command."
  (interactive (list
                (split-string-shell-command
                 (read-from-minibuffer "command line: jj " nil nil nil 'jj-cmd-hist))))
  (jj-with-editor
   (jj-cmd-async-view args
                      nil :verbose-err)))
;; anything:1 ends here

;; jj help

;; [[file:majjik.org::*jj help][jj help:1]]
(defvar jj-help-hist nil
  "History for `jj-help' command.")

(defun jj-help (args)
  "Get help for anything in jj."
  (interactive (list
                (split-string-shell-command
                 (read-from-minibuffer "jj help " nil nil nil 'jj-help-hist))))
  (jj-cmd-async-view `("help" ,@args)
                     :no-revert :verbose-err))
;; jj help:1 ends here

;; jj undo

;; [[file:majjik.org::*jj undo][jj undo:1]]
(cl-defun jj-undo ()
  (interactive)
  (jj-cmd-async-view `("undo")))
;; jj undo:1 ends here

;; jj redo

;; [[file:majjik.org::*jj redo][jj redo:1]]
(cl-defun jj-redo ()
  (interactive)
  (jj-cmd-async-view `("redo")))
;; jj redo:1 ends here

;; common

;; [[file:majjik.org::*common][common:1]]
(defun jj-any-destination-provided ()
  (transient--any-on-p "-o" "-A" "-B"))

(defvar jj-revision-destination-incompatibilities
  (prod-cartes '("-o") '("-B" "-A")))

(transient-define-group jj-revision-destination-multi-group
  ["destinations"
   ("-o" "on revisions" jj-multi-revision-argument
    :argument "-o")
   ("-A" "insert after" jj-multi-revision-argument
    :argument "-A")
   ("-B" "insert before" jj-multi-revision-argument
    :argument "-B")])

(transient-define-group jj-revision-destination-single-group
  ["destination"
   ("-o" "on revision" jj-single-revision-argument
    :argument "-o")
   ("-A" "insert after" jj-single-revision-argument
    :argument "-A")
   ("-B" "insert before" jj-single-revision-argument
    :argument "-B")])

(defun jj-any-source-provided ()
  (transient--any-on-p "-r" "-s" "-b"))

(defvar jj-revision-source-incompatibilities
  '(("-r" "-s" "-b")))

(transient-define-group jj-revision-source-multi-group
  ["sources"
   ("-r" "revisions" jj-multi-revision-argument
    :argument "-r")
   ("-s" "source" jj-multi-revision-argument
    :argument "-s")
   ("-b" "branch" jj-multi-revision-argument
    :argument "-b")])
;; common:1 ends here

;; entry point

;; [[file:majjik.org::*entry point][entry point:1]]
(transient-define-prefix jj-commit-entry-prefix ()
  [["work"
    ("n" "new" jj-commit-new-prefix)
    ("e" "edit" jj-commit-edit-prefix)
    ("w" "describe" jj-commit-describe-prefix)
    ("m" "metaedit" jj-commit-metaedit-prefix)
    ]
   ["rework"
    ("a" "absorb" jj-commit-absorb-prefix)
    ("s" "squash" jj-commit-squash-prefix)
    ("x" "restore" jj-commit-restore-prefix)
    ("k" "abandon" jj-commit-abandon-prefix)
    ]
   ["rearrange"
    ("r" "rebase" jj-commit-rebase-prefix)
    ("c" "copy (duplicate/revert)" jj-commit-copy-prefix)
    ("z" "parallelize" jj-commit-parallelize-prefix)
    ("p" "simplify-parents" jj-commit-simplify-parents-prefix)]])
;; entry point:1 ends here

;; squash

;; [[file:majjik.org::*squash][squash:1]]
(transient-define-prefix jj-commit-squash-prefix ()
  :refresh-suffixes t
  :incompatible `(,@(prod-cartes '("-r") '("-f" "-t"))
                  ("-m" "-u"))
  jj-global-group
  [["targets"
    ("-r" "revision" jj-single-revision-argument
     :argument "-r")
    ("-f" "from" jj-multi-revision-argument
     :argument "-f")
    ("-t" "to" jj-single-revision-argument
     :argument "-t")]
   ["meta"
    ("-m" "message" "--message="
     :always-read t
     :history-key jj-description-history
     :reader (lambda (p s h)
               (condition-case e
                   (read-string-from-buffer-with-history p s h)
                 (error (progn
                          (message "%s" e)
                          s)))))
    ("-u" "use dest message" "-u")
    ("-k" "keep emptied" "-k")]]
  ["go"
   ("s" "squash" (lambda (args)
                   (interactive (list (jj--transient-args)))
                   (jj-with-editor
                    (jj-cmd-async-message `("squash" ,@args))))
    :inapt-if-not (lambda () (transient--all-on-p "-f" "-t")))
   ("d" "squash down" (lambda (args)
                        (interactive (list (jj--transient-args)))
                        (jj-with-editor
                         (jj-cmd-async-message `("squash" ,@args))))
    :inapt-if-not (lambda () (and (transient--any-on-p "-r")
                                  (not (transient--any-on-p "-t" "-f")))))
   ("a" "amend @ into" (lambda (args)
                         (interactive (list (jj--transient-args)))
                         (jj-with-editor
                          (jj-cmd-async-message `("squash" ,@args))))
    :inapt-if-not (lambda () (and (transient--any-on-p "-r" "-t")
                                  (not (transient--any-on-p "-f")))))])
;; squash:1 ends here

;; absorb

;; [[file:majjik.org::*absorb][absorb:1]]
(transient-define-prefix jj-commit-absorb-prefix ()
  :refresh-suffixes t
  jj-global-group
  [["targets"
    ("-f" "from" jj-single-revision-argument
     :argument "-f")
    ("-t" "to" jj-multi-revision-argument
     :argument "-t")]]
  ["go"
   ("a" "absorb" (lambda (args)
                   (interactive (list (jj--transient-args)))
                   (jj-cmd-async-view `("absorb" ,@args)))
    :inapt-if-not (lambda () (transient--all-on-p "-f" "-t")))])
;; absorb:1 ends here

;; abandon

;; [[file:majjik.org::*abandon][abandon:1]]
(transient-define-prefix jj-commit-abandon-prefix ()
  :refresh-suffixes t
  jj-global-group
  ["targets"
   ("-r" "revisions" jj-multi-revision-argument
    :argument "-r")]
  ["go"
   ("k" "abandon" (lambda (args)
                    (interactive (list (jj--transient-args)))
                    (jj-cmd-async-view `("abandon" ,@args)))
    :inapt-if-not (lambda () (transient--any-on-p "-r")))])
;; abandon:1 ends here

;; describe

;; [[file:majjik.org::*describe][describe:1]]
(transient-define-prefix jj-commit-describe-prefix ()
  :refresh-suffixes t
  jj-global-group
  [["targets"
    ("-r" "revision" jj-single-revision-argument
     :argument "-r")]
   ["meta"
    ("-m" "message" "--message="
     :always-read t
     :history-key jj-description-history
     :reader (lambda (p s h)
               (let ((prev (jj--get-description (jj-thing-at-point))))
                 (condition-case e
                     (read-string-from-buffer-with-history p (or s prev) h)
                   (error (progn
                            (message "%s" e)
                            s))))))]]
  ["go"
   ("w" "describe" (lambda (args)
                     (interactive (list (jj--transient-args)))
                     (jj-with-editor (jj-cmd-async-view `("describe" ,@args))))
    :inapt-if-not (lambda () (transient--any-on-p "-r")))
   ("c" "commit" (lambda (args)
                   (interactive (list (jj--transient-args)))
                   (jj-with-editor (jj-cmd-async-view `("commit" ,@args))))
    :inapt-if (lambda () (transient--any-on-p "-r")))])
;; describe:1 ends here

;; metaedit

;; [[file:majjik.org::*metaedit][metaedit:1]]
(transient-define-prefix jj-commit-metaedit-prefix ()
  :refresh-suffixes t
  jj-global-group
  [["targets"
    ("-r" "revisions" jj-multi-revision-argument
     :argument "-r")]
   ["set"
    ("-m" "message" "--message="
     :always-read t
     :history-key jj-description-history
     :reader (lambda (p s h)
               (let ((prev (jj--get-description (jj-thing-at-point))))
                 (condition-case e
                     (read-string-from-buffer-with-history p (or s prev) h)
                   (error (progn
                            (message "%s" e)
                            s))))))
    
    ("-a" "set author" "--author=")
    ("-t" "set timestamp" "--author-timestamp=")]
   ["regen"
    ("-uc" "update change id" "--update-change-id")
    ("-ut" "update timestamp" "--update-author-timestamp")
    ("-ua" "update author" "--update-author")
    ("-uf" "force-rewrite" "--force-rewrite")]
   ["go"
    ("m" "metaedit"
     (lambda (args)
       (interactive (list (jj--transient-args)))
       (jj-cmd-async-view `("metaedit" ,@args)))
     :inapt-if-not (lambda ()
                     (transient--any-on-p "-r")))]])
;; metaedit:1 ends here

;; new

;; [[file:majjik.org::*new][new:1]]
(transient-define-prefix jj-commit-new-prefix ()
  :refresh-suffixes t
  :incompatible jj-revision-destination-incompatibilities
  jj-global-group
  [jj-revision-destination-multi-group
   ["meta"
    ("-m" "message" "--message="
     :always-read t
     :history-key jj-description-history
     :reader (lambda (p s h)
               (condition-case e
                   (read-string-from-buffer-with-history p s h)
                 (error (progn
                          (message "%s" e)
                          s)))))
    ("-N" "no edit" "--no-edit")]]
  ["go"
   ("n" "new" (lambda (args)
                (interactive (list (jj--transient-args)))
                (jj-cmd-async-view `("new" ,@args)))
    :inapt-if-not (lambda ()
                    (jj-any-destination-provided)))
   ("N" "next" (lambda ()
                 (interactive)
                 (jj-cmd-async-view `("next")))
    :inapt-if (lambda ()
                (transient--any-on-p "-B" "-A" "-r" "-N" "-m")))
   ("P" "prev" (lambda ()
                 (interactive)
                 (jj-cmd-async-view `("prev")))
    :inapt-if (lambda ()
                (transient--any-on-p "-B" "-A" "-r" "-N" "-m")))])
;; new:1 ends here

;; edit

;; [[file:majjik.org::*edit][edit:1]]
(transient-define-prefix jj-commit-edit-prefix ()
  :refresh-suffixes t
  jj-global-group
  ["targets"
   ("-r" "revision" jj-single-revision-argument
    :argument "-r")
   ]
  ["go"
   ("e" "edit" (lambda (args)
                 (interactive (list (jj--transient-args)))
                 (jj-cmd-async-view `("edit" ,@args)))
    :inapt-if-not (lambda () (transient--any-on-p "-r")))
   ("N" "next --edit" (lambda ()
                        (interactive)
                        (jj-cmd-async-view `("next" "--edit")))
    :inapt-if (lambda ()
                (transient--any-on-p "-B" "-A" "-r" "-N" "-m")))
   ("P" "prev --edit" (lambda ()
                        (interactive)
                        (jj-cmd-async-view `("prev" "--edit")))
    :inapt-if (lambda ()
                (transient--any-on-p "-B" "-A" "-r" "-N" "-m")))])
;; edit:1 ends here

;; rebase

;; [[file:majjik.org::*rebase][rebase:1]]
(transient-define-prefix jj-commit-rebase-prefix ()
  :refresh-suffixes t
  :incompatible `(,@jj-revision-source-incompatibilities
                  ,@jj-revision-destination-incompatibilities)
  jj-global-group
  [jj-revision-source-multi-group
   jj-revision-destination-multi-group
   ["meta"
    ("-S" "skip emptied" "--skip-emptied")
    ("-D" "keep divergent" "--keep-divergent")]]
  ["go"
   ("r" "rebase" (lambda (args)
                   (interactive (list (jj--transient-args)))
                   (jj-cmd-async-view `("rebase" ,@args)))
    :inapt-if-not (lambda ()
                    (and (jj-any-destination-provided)
                         (jj-any-source-provided))))
   ])
;; rebase:1 ends here

;; restore

;; [[file:majjik.org::*restore][restore:1]]
(transient-define-prefix jj-commit-restore-prefix ()
  :refresh-suffixes t
  :incompatible (prod-cartes '("-c") '("-f" "-t"))
  jj-global-group
  [["targets"
    ("-c" "changes in" jj-single-revision-argument
     :argument "-c")
    ("-f" "from" jj-single-revision-argument
     :argument "-f")
    ("-t" "to" jj-single-revision-argument
     :argument "-t")]
   ;; ["files"] ;; requires positional-argument processing
   ["meta"
    ("-D" "restore descendents" "--restore-descendants")
    ;; ("-i" "interactive" "-i")
    ;; ("-T" "tool" "--tool=")
    ]]
  ["go"
   ("x" "restore" (lambda (args)
                    (interactive (list (jj--transient-args)))
                    (jj-cmd-async-view `("restore" ,@args)))
    :inapt-if-not (lambda () (or
                              (transient--all-on-p "-f" "-t")
                              (transient--any-on-p "-c"))))
   ])
;; restore:1 ends here

;; duplicate/revert

;; [[file:majjik.org::*duplicate/revert][duplicate/revert:1]]
(transient-define-prefix jj-commit-copy-prefix ()
  :refresh-suffixes t
  :incompatible (prod-cartes '("-o") '("-B" "-A"))
  jj-global-group
  [["sources"
    ("-r" "revisions" jj-multi-revision-argument
     :argument "-r")]
   jj-revision-destination-multi-group]
  ["go"
   ("c" "duplicate" (lambda (args)
                      (interactive (list (jj--transient-args)))
                      (jj-cmd-async-view `("duplicate" ,@args)))
    :inapt-if-not (lambda ()
                    (and (jj-any-destination-provided)
                         (transient--any-on-p "-r"))))
   ("v" "revert" (lambda (args)
                   (interactive (list (jj--transient-args)))
                   (jj-cmd-async-view `("revert" ,@args)))
    :inapt-if-not (lambda ()
                    (and (jj-any-destination-provided)
                         (transient--any-on-p "-r"))))])
;; duplicate/revert:1 ends here

;; parallelize

;; [[file:majjik.org::*parallelize][parallelize:1]]
(transient-define-prefix jj-commit-parallelize-prefix ()
  :refresh-suffixes t
  jj-global-group
  [["targets"
    ("-r" "revisions" jj-multi-revision-argument
     :argument "-r")]]
  ["go"
   ("p" "parallelize"
    (lambda (args)
      (interactive (list (jj--transient-args)))
      (jj-cmd-async-view `("parallelize" ,@args)))
    :inapt-if-not (lambda ()
                    (transient--any-on-p "-r")))])
;; parallelize:1 ends here

;; simplify-parents

;; [[file:majjik.org::*simplify-parents][simplify-parents:1]]
(transient-define-prefix jj-commit-simplify-parents-prefix ()
  :refresh-suffixes t
  jj-global-group
  [["targets"
    ("-s" "sources" jj-multi-revision-argument
     :argument "-s")
    ("-r" "revisions" jj-multi-revision-argument
     :argument "-r")]]
  ["go"
   ("p" "simplify-parents"
    (lambda (args)
      (interactive (list (jj--transient-args)))
      (jj-cmd-async-view `("simplify-parents" ,@args)))
    :inapt-if-not (lambda ()
                    (transient--any-on-p "-r" "-s")))])
;; simplify-parents:1 ends here

;; global
;; I just need a way to optionally persist this now, so arguments are set once for a repo and stay until unset.

;; [[file:majjik.org::*global][global:1]]
(defvar jj-global-argument-prefix "!")
(defvar jj-show-global-arguments nil
  "Whether to always show the common global jj arguments")

(transient-define-group jj-global-group
  [:hide (lambda ()
           (defvar transient--redisplay-key)
           (and (not (equal (vconcat transient--redisplay-key)
                            (read-kbd-macro jj-global-argument-prefix)))
                (not jj-show-global-arguments)
                (not (transient--any-on-p "--repository="
                                          "--ignore-working-copy"
                                          "--ignore-immutable"
                                          "--at-operation="
                                          "--debug"
                                          "--config="
                                          "--config-file="))))
         ["Global arguments"
          ("!R" "repository" "--repository=")
          ("!iw" "ignore working copy" "--ignore-working-copy")
          ("!im" "ignore immutable" "--ignore-immutable")
          ("!o" "at op" "--at-operation=")
          ("!d" "debug" "--debug")
          ("!cs" "config" "--config=")
          ("!cf" "config file" "--config-file=")]])
;; global:1 ends here

;; jj new

;; [[file:majjik.org::*jj new][jj new:1]]
(cl-defun jj-new (&key rev before after message no-edit)
  (when (and rev (or before after))
    (user-error "cannot supply REV with BEFORE or AFTER"))
  (unless (or rev before after)
    (user-error "must supply at least one of REV, BEFORE, or AFTER"))
  (jj-cmd-async-view
      `("new"
        ,@(jj--if-arg rev #'identity "-r")
        ,@(jj--if-arg before #'identity "--before")
        ,@(jj--if-arg after #'identity "--after")
        ,@(jj--if-arg no-edit nil "--no-edit")
        ,@(jj--if-arg message #'identity "-m"))))

(cl-defun jj-new-on-dwim (parents-revset &key message no-edit)
  "Create a new commit after the chosen PARENTS-REVSET, with no children."
  (interactive (list (jj-get-revset-dwim "parent revs: ")))
  (jj-new :rev parents-revset :message message :no-edit no-edit))

(cl-defun jj-new-on-bookmark (bookmark-name &key message no-edit)
  "Create a new commit after the chosen BOOKMARK-NAME, with no children."
  (interactive (list (completing-read "New on bookmark: " (jj-list-bookmarks))))
  (jj-new :rev bookmark-name :message message :no-edit no-edit))

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
  (interactive (list (jj-read-multi-revision "parent revs: ")
                     (jj-read-multi-revision "child revs: ")))
  (jj-new :before children-revset :after parents-revset :message message :no-edit no-edit))
;; jj new:1 ends here

;; jj edit

;; [[file:majjik.org::*jj edit][jj edit:1]]
(cl-defun jj-edit-dwim (rev &optional ignore-immutable)
  (interactive (list (jj-get-revset-dwim "edit: ")))
  (jj-cmd-async-view
      `("edit"
        "-r" ,rev
        ,@(jj--if-arg ignore-immutable nil "--ignore-immutable"))))
;; jj edit:1 ends here

;; jj desc

;; [[file:majjik.org::*jj desc][jj desc:1]]
(defun jj-desc-dwim-oneline (revset message)
  (interactive (list (jj-get-revset-dwim "revs to describe: ")
                     (read-string "message: ")))
  (jj-cmd-async-view
   `("describe"
     "-r" ,revset
     ,(concat "--message=" message))))

(defun jj-desc-dwim (revset)
  (interactive (list (jj-get-revset-dwim "revs to describe: ")))
  (jj-with-editor
   (jj-cmd-async-view
    `("describe"
      "-r" ,revset))))
;; jj desc:1 ends here

;; jj drop

;; [[file:majjik.org::*jj drop][jj drop:1]]
(defun jj-drop-dwim (revset &optional noconfirm)
  (interactive (list (jj-get-revset-dwim "revs to abandon: ")))
  (unless (or noconfirm (yes-or-no-p (format "abandon %s?" revset)))
    (user-error "cancelled"))
  (jj-cmd-async-view
      `("abandon"
        "-r" ,revset)))
;; jj drop:1 ends here

;; simple

;; [[file:majjik.org::*simple][simple:1]]
(defun jj--git-init (dir args)
  (let* ((cmd `("git" "init"
               ,(expand-file-name dir)
               ,@args)))
    (promise-then
     (jj-cmd-async-view cmd :no-rev :verbose-error)
     (lambda (proc)
       (jj-dash--async dir)))))

;;;###autoload
(defun jj-git-init (root-dir colocate)
  (interactive (list (expand-file-name (read-directory-name "repository root: "))
                     (yes-or-no-p "colocated repository?")))
  (jj--git-init root-dir
                    `(,@(jj--if-arg colocate nil "--colocate")
                      ,@(jj--if-arg (not colocate) nil "--no-colocate"))))

(defun jj-git--init-sync (root-dir colocate)
  (interactive (list (expand-file-name (read-directory-name "repository root: "))
                     (yes-or-no-p "colocated repository?")))
  (jj-cmd-sync
   `("git" "init"
     ,@(jj--if-arg colocate nil "--colocate")
     ,@(jj--if-arg (not colocate) nil "--no-colocate")
     "--" ,root-dir)
   :no-revert))
;; simple:1 ends here

;; transient

;; [[file:majjik.org::*transient][transient:1]]
(transient-define-prefix jj-git-init-prefix ()
  :refresh-suffixes t
  :incompatible (prod-cartes '("--colocate") '("--git-repo=" "--no-colocate"))
  jj-global-group
  ["targets"
   ("-c" "colocate" "--colocate")
   ("-n" "no-colocate" "--no-colocate")
   ("-g" "git-repo" "--git-repo="
    :reader transient-read-existing-directory)]
  ["go"
   ("i" "init" (lambda (path args)
                 (interactive (list (read-directory-name "init in: ")
                                    (jj--transient-args)))
                 (jj--git-init path args)))])
;; transient:1 ends here

;; new

;; [[file:majjik.org::*new][new:1]]
(defun jj-bookmark-new-dwim (bookmark rev)
  "Create new BOOKMARK pointing at revision REV."
  (interactive (list (read-string "New bookmark: ")
                     (jj-get-revision-dwim "At rev: ")))
  (jj-cmd-async-view
      `("bookmark" "create" ,bookmark
        "-r" ,rev)))
;; new:1 ends here

;; move

;; [[file:majjik.org::*move][move:1]]
(defun jj-bookmark-move-dwim (bookmark to-rev &optional allow-backwards)
  "Move BOOKMARK to point to revision TO-REV. If used with a prefix arg, allow the bookmark to move backwards or sideways."
  (interactive (list (completing-read "Move bookmark: " (jj-list-local-bookmarks))
                     (jj-get-revision-dwim "move to: ")
                     current-prefix-arg))
  (jj-cmd-async-view
      `("bookmark" "move" ,bookmark
        "--to" ,to-rev
        ,@(jj--if-arg allow-backwards nil "--allow-backwards"))))
;; move:1 ends here

;; set

;; [[file:majjik.org::*set][set:1]]
(defun jj-bookmark-set-dwim (bookmark rev &optional allow-backwards)
  "Create or move BOOKMARK to point to revision REV. If used with a prefix arg, allow the bookmark to move backwards or sideways.

Can be used to recreate a deleted bookmark, unlike `jj-bookmark-move-dwim' and `jj-bookmark-new-dwim'."
  (interactive (list (completing-read "Set bookmark: " (jj-list-local-bookmarks))
                     (jj-get-revision-dwim "Set to: ")
                     current-prefix-arg))
  (jj-cmd-async-view
      `("bookmark" "set" ,bookmark
        "-r" ,rev
        ,@(jj--if-arg allow-backwards nil "--allow-backwards"))))
;; set:1 ends here

;; rename

;; [[file:majjik.org::*rename][rename:1]]
(defun jj-bookmark-rename (bookmark new-name)
  "Create new BOOKMARK pointing at revision REV."
  (interactive (list (completing-read "Rename bookmark: " (jj-list-bookmarks))
                     (read-string "New name: ")))
  (jj-cmd-async-view
      `("bookmark" "rename" ,bookmark ,new-name)))
;; rename:1 ends here

;; delete

;; [[file:majjik.org::*delete][delete:1]]
(defun jj-bookmark-delete (bookmark &optional noconfirm)
  "Delete BOOKMARK."
  (interactive (list (completing-read "Delete bookmark: " (jj-list-bookmarks))
                     current-prefix-arg))
  (unless (or noconfirm (yes-or-no-p (format "delete bookmark %s?" bookmark)))
    (user-error "cancelled"))
  (jj-cmd-async-view
      `("bookmark" "delete" ,bookmark)))
;; delete:1 ends here

;; forget

;; [[file:majjik.org::*forget][forget:1]]
(defun jj-bookmark-forget (bookmark &optional noconfirm)
  "Delete BOOKMARK, but don't mark it for deletion on the remote."
  (interactive (list (completing-read "Forget bookmark: " (jj-list-bookmarks))
                     current-prefix-arg))
  (unless (or noconfirm (yes-or-no-p (format "forget bookmark %s?" bookmark)))
    (user-error "cancelled"))
  (jj-cmd-async-view
      `("bookmark" "forget" ,bookmark)))
;; forget:1 ends here

;; track

;; [[file:majjik.org::*track][track:1]]
(defun jj-bookmark-track (remote bookmark)
  "Track BOOKMARK at REMOTE."
  (interactive (let* ((candidates (or `(,@(jj-list-untracked-remote-bookmarks nil :not-git)
                                        ,@(jj-list-local-bookmarks))
                                      (user-error "All bookmarks tracked.")))
                      (bookmark (completing-read "Bookmark to track: " candidates))
                      (remote (completing-read "From remote: " (jj-list-git-remotes))))
                 (list remote bookmark)))
  (jj-cmd-async-view
   `("bookmark" "track"
        "--remote" ,remote
        "--" ,bookmark)))

(defun jj-bookmark-track-for-remote (remote bookmark)
  "Track BOOKMARK which is from REMOTE. Only prompts for untracked bookmarks at the given REMOTE, and local bookmarks that aren't tracking the given REMOTE."
  (declare (interactive-only jj-bookmark-track))
  (interactive (let* ((remote (completing-read "From remote: " (jj-list-git-remotes)))
                      (candidates (or `(,@(jj-list-untracked-remote-bookmarks remote :not-git)
                                        ,@(jj-list-non-tracking-local-bookmarks remote))
                                      (user-error "All bookmarks tracked for this remote.")))
                      (bookmark (completing-read "Bookmark to track: " candidates)))
                 (list remote bookmark)))
  (jj-cmd-async-view
   `("bookmark" "track"
     "--remote" ,remote
     "--" ,bookmark)))

(defun jj-bookmark-track-local (remote bookmark)
  "Track local BOOKMARK on REMOTE. Only prompts for local bookmarks."
  (declare (interactive-only jj-bookmark-track))
  (interactive (let ((bookmark (completing-read "Bookmark to track: " (jj-list-local-bookmarks)))
                     (remote (completing-read "To remote: " (jj-list-git-remotes))))
                 (list remote bookmark)))
  (jj-cmd-async-view
      `("bookmark" "track"
        "--remote" ,remote
        "--" ,bookmark)))
;; track:1 ends here

;; untrack

;; [[file:majjik.org::*untrack][untrack:1]]
(defun jj-bookmark-untrack (bookmark &optional remotes)
  "Untrack BOOKMARK from REMOTES if provided, otherwise all remotes."
  (interactive (let* ((bookmarks (jj-list-remote-bookmarks nil :tracked :not-git))
                      (bookmark (completing-read "Bookmark to untrack: " (mapcar #'car bookmarks) nil t))
                      (remotes (completing-read-multiple
                                "From remotes: "
                                (cl-loop for (b r) in bookmarks
                                         when (string= b bookmark)
                                         collect r)
                                nil t)))
                 (list bookmark remotes)))
  (jj-cmd-async-view
      `("bookmark" "untrack" ,bookmark
        ,@(jj--if-arg remotes (apply-partially #'apply #'jj-revs-as-revset) "--remote"))))
;; untrack:1 ends here

;; entry

;; [[file:majjik.org::*entry][entry:1]]
(transient-define-prefix jj-bookmark-prefix ()
  [["local"
    ("c" "create" jj-bookmark-create-prefix)
    ("m" "move" jj-bookmark-move-prefix)
    ("r" "rename" jj-bookmark-rename-prefix)
    ("x" "set" jj-bookmark-set-prefix)
    ("DEL" "forget" jj-bookmark-forget-prefix)]
   ["remote"
    ("t" "track" jj-bookmark-track-prefix)
    ("u" "untrack" jj-bookmark-untrack-prefix)
    ("d" "delete" jj-bookmark-delete-prefix)]
   ["list"
    ("l" "list" jj-bookmark-list-prefix)]])
;; entry:1 ends here

;; create

;; [[file:majjik.org::*create][create:1]]
(transient-define-prefix jj-bookmark-create-prefix ()
  :refresh-suffixes t
  jj-global-group
  ["targets"
   ("-r" "revision" jj-multi-revision-argument
    :multi-value repeat
    :argument "-r")]
  ["go"
   ("n" "new" (lambda (names args)
                (interactive (list (let ((crm-separator (rx (* blank)
                                                            (any ", ")
                                                            (* blank))))
                                     (completing-read-multiple "new bookmark names: " ()))
                                   (jj--transient-args)))
                (jj-cmd-async-view
                 `("bookmark" "create" ,@args "--" ,@names)
                 nil :verbose-err))
    :inapt-if-not (lambda () (transient--any-on-p "-r")))])
;; create:1 ends here

;; list
;; this feels a little redundant - the completing-read-multiple lists the bookmarks while you're performing input for the transient. but, the list produced by jj is more thorough than the crm window.

;; [[file:majjik.org::*list][list:1]]
(transient-define-prefix jj-bookmark-list-prefix ()
  :refresh-suffixes t
  :incompatible '(("--all-remotes" "--remote="))
  jj-global-group
  ["targets"
   ("-a" "all-remotes" "--all-remotes")
   ("-m"  "remote" "--remote=")
   ("-t" "tracked" "--tracked")
   ("-c" "conflicted" "--conflicted")
   ("-r" "revisions" jj-multi-revision-argument
    :multi-value repeat
    :argument "-r")
   ("-T" "template" "--template=")
   ("-S"   "sort" "--sort=")]
  ["go"
   ("l" "list" (lambda (names args)
                 (interactive (list (let ((crm-separator (rx (* blank)
                                                             (any ", ")
                                                             (* blank))))
                                      (completing-read-multiple "list bookmark names matching: " (jj-list-bookmarks)))
                                    (jj--transient-args)))
                 (jj-cmd-async-view
                  `("bookmark" "list" ,@args "--" ,@names)
                  nil :verbose-err)))])
;; list:1 ends here

;; set

;; [[file:majjik.org::*set][set:1]]
(transient-define-prefix jj-bookmark-set-prefix ()
  :refresh-suffixes t
  jj-global-group
  [["targets"
    ("-r" "revision" jj-multi-revision-argument
     :multi-value repeat
     :argument "-r")]
   ["meta"
    ("-B" "allow backwards" "-B")]]
  ["go"
   ("x" "set" (lambda (names args)
                (interactive (list (let ((crm-separator (rx (* blank)
                                                            (any ", ")
                                                            (* blank))))
                                     (completing-read-multiple "set bookmarks: " (jj-list-local-bookmarks)))
                                   (jj--transient-args)))
                (jj-cmd-async-view
                 `("bookmark" "set" ,@args "--" ,@names)
                 nil :verbose-err))
    :inapt-if-not (lambda () (transient--any-on-p "-r")))])
;; set:1 ends here

;; move

;; [[file:majjik.org::*move][move:1]]
(transient-define-prefix jj-bookmark-move-prefix ()
  :refresh-suffixes t
  jj-global-group
  [["targets"
    ;; for now, this needs some way for me to only prompt
    ;; for branch name if this argument is not provided.
    ;; it doesn't fit the current `jj--ensure-arg' interface,
    ;; so I can't be bothered for now.
    ;; ("-f" "from" jj-multi-revision-argument
    ;;  :multi-value repeat
    ;;  :argument "-f")
    ("-t" "to" jj-single-revision-argument
     :argument "-t")]
   ["meta"
    ("-B" "allow backwards" "-B")]]
  ["go"
   ("m" "move" (lambda (names args)
                 (interactive (list (let ((crm-separator (rx (* blank)
                                                             (any ", ")
                                                             (* blank))))
                                      (completing-read-multiple "move bookmarks: " (jj-list-local-bookmarks)))
                                    (jj--transient-args)))
                 (jj-cmd-async-view
                  `("bookmark" "move" ,@args "--" ,@names)
                  nil :verbose-err))
    :inapt-if-not (lambda () (transient--any-on-p "-t")))])
;; move:1 ends here

;; track

;; [[file:majjik.org::*track][track:1]]
(transient-define-prefix jj-bookmark-track-prefix ()
  :refresh-suffixes t
  jj-global-group
  ["args"
   ("-m" "remote" "--remote="
    :reader (lambda (p s h)
              (completing-read p (jj-list-git-remotes) nil t s h)))]
  ["go"
   ("t" "track" (lambda (names args)
                  (interactive (let* ((crm-separator (rx (* blank)
                                                         (any ", ")
                                                         (* blank)))
                                      (args (jj--transient-args))
                                      (remote (transient-arg-value "--remote=" args))
                                      (candidates (or `(,@(jj-list-untracked-remote-bookmarks remote :not-git)
                                                        ,@(jj-list-non-tracking-local-bookmarks remote))
                                                      (user-error "All bookmarks tracked for this remote."))))
                                 (list (completing-read-multiple "track bookmarks: " candidates)
                                       args)))
                  (jj-cmd-async-view `("bookmark" "track" ,@args "--" ,names)))
    :inapt-if-not (lambda () (transient--any-on-p "--remote=")))])
;; track:1 ends here

;; untrack

;; [[file:majjik.org::*untrack][untrack:1]]
(transient-define-prefix jj-bookmark-untrack-prefix ()
  :refresh-suffixes t
  jj-global-group
  ["args"
   ("-m" "remote" "--remote="
    :reader (lambda (p s h)
              (completing-read p (jj-list-git-remotes) nil t s h)))]
  ["go"
   ("u" "untrack" (lambda (names args)
                    (interactive (let* ((crm-separator (rx (* blank)
                                                           (any ", ")
                                                           (* blank)))
                                        (args (jj--transient-args))
                                        (remote (transient-arg-value "--remote=" args))
                                        (candidates (or (jj-list-local-bookmarks remote)
                                                        (user-error "No tracked bookmarks on this remote."))))
                                   (list (completing-read-multiple "untrack bookmarks: " candidates)
                                         (jj--transient-args))))
                    (jj-cmd-async-view `("bookmark" "untrack" ,@args "--" ,@names)))
    :inapt-if-not (lambda () (transient--any-on-p "--remote=")))])

(transient-define-prefix jj-bookmark-rename-prefix ()
  jj-global-group
  ["go"
   ("r" "rename" (lambda (old new)
                   (interactive (list (completing-read "rename bookmark: " (jj-list-bookmarks))
                                      (completing-read "new name: " (jj-list-bookmarks))))
                   (jj-cmd-async-view `("bookmark" "rename" "--" ,old ,new))))])
;; untrack:1 ends here

;; delete

;; [[file:majjik.org::*delete][delete:1]]
(transient-define-prefix jj-bookmark-delete-prefix ()
  jj-global-group
  ["go"
   ("k" "delete" (lambda (names)
                   (interactive (list (let ((crm-separator (rx (* blank)
                                                               (any ", ")
                                                               (* blank))))
                                        (completing-read-multiple "delete bookmarks: " (jj-list-bookmarks)))))
                   (jj-cmd-async-view `("bookmark" "delete" "--" ,@names))))])
;; delete:1 ends here

;; forget

;; [[file:majjik.org::*forget][forget:1]]
(transient-define-prefix jj-bookmark-forget-prefix ()
  jj-global-group
  ["args"
   ("-M" "include remotes" "--include-remotes")]
  ["go"
   ("DEL" "forget" (lambda (names args)
                     (interactive (list (let ((crm-separator (rx (* blank)
                                                                 (any ", ")
                                                                 (* blank))))
                                          (completing-read-multiple "forget bookmarks: " (jj-list-bookmarks)))
                                        (jj--transient-args)))
                     (jj-cmd-async-view `("bookmark" "forget" ,@args "--" ,@names))))])
;; forget:1 ends here

;; track

;; [[file:majjik.org::*track][track:1]]
(defun jj-file-track-dwim (file)
  "Track FILE."
  (interactive (list (jj-get-untracked-file-dwim "File to track")))
  (jj-cmd-async-view
      `("file" "track" ,(jj-paths-as-fileset file))))
;; track:1 ends here

;; untrack

;; [[file:majjik.org::*untrack][untrack:1]]
(defun jj-file-untrack-dwim (file)
  "Untrack FILE."
  (interactive (list (jj-get-tracked-file-dwim "File to untrack")))
  (jj-cmd-async-view
      `("file" "untrack" ,(jj-paths-as-fileset file))))
;; untrack:1 ends here

;; delete

;; [[file:majjik.org::*delete][delete:1]]
(defun jj-file-delete-dwim (file &optional noconfirm)
  "Delete FILE."
  (interactive (list (jj-get-file-dwim "File to untrack")
                     current-prefix-arg))
  (unless (or noconfirm (yes-or-no-p (format "delete file %s?" file)))
    (user-error "cancelled"))
  (jj-cmd-async-view
      `("util" "exec" "rm" ,file)))
;; delete:1 ends here

;; jj squash/amend

;; [[file:majjik.org::*jj squash/amend][jj squash/amend:1]]
(cl-defun jj-squash-down-dwim (rev &optional noconfirm ignore-immutable)
  "Squash changes from REV into its single parent."
  (interactive (list (jj-get-revision-dwim "squash rev: ")
                     nil
                     current-prefix-arg))
  (unless (or noconfirm (yes-or-no-p (format "squash %s into its parent?" rev)))
    (user-error "cancelled"))
  (jj-with-editor
    (jj-cmd-async-message
        `("squash"
          "-r" ,rev
          ,@(jj--if-arg ignore-immutable nil "--ignore-immutable")))))

(cl-defun jj-amend-into-dwim (rev &optional noconfirm ignore-immutable)
  "Squash changes from @ into the chosen revision."
  (interactive (list (jj-get-revision-dwim "squash into rev: ")
                     nil
                     current-prefix-arg))
  (unless (or noconfirm (yes-or-no-p (format "squash @ into %s?" rev)))
    (user-error "cancelled"))
  (jj-with-editor
    (jj-cmd-async-message
        `("squash"
          "--into" ,rev
          ,@(jj--if-arg ignore-immutable nil "--ignore-immutable")))))
;; jj squash/amend:1 ends here

;; simple

;; [[file:majjik.org::*simple][simple:1]]
(defun jj-git-push (&rest args)
  "Push to git in the background."
  (interactive)
  (jj-cmd-async-view `("git" "push" ,@args) nil :verbose-error))
;; simple:1 ends here

;; transient

;; [[file:majjik.org::*transient][transient:1]]
(transient-define-suffix jj--git-push-suffix (args)
  "do a jj git push."
  (interactive (list (jj--transient-args)))
  (apply #'jj-git-push args))

(defun completion-table-with-annotation (table annotator)
  "Decorate an existing TABLE completion function to add (or replace) an annotation function without modifying the other behaviours."
  (lambda (str pred flag)
    (let ((delegate (funcall table str pred flag)))
      (pcase flag
        ('metadata
         `(metadata (annotation-function . ,annotator)
                    ,@(cdr delegate)))
        (_ delegate)))))

(defun make--jj-named-infix-table (options)
  "For use as a completion table for the `jj-git-push--named-infix'.
This table is annotated assuming the options are valid for `jj--annotate-refs'."
  (let* ((annotator
          (let ((anno-raw (jj--annotate-refs options)))
            (lambda (opt)
              ;; the completion option passed to the annotator
              ;; is the string which will include an = sign
              ;; so we trim that prefix to allow us to find
              ;; the corresponding candidate from the original options list
              (funcall anno-raw (s-replace-regexp (rx string-start (* anychar) "=") "" opt)))))
         (completer (lambda (str)
                      (when (string-match
                             (rx string-start
                                 (group (* (not (any "=")))
                                        "=")
                                 (group (* (not (any "="))))
                                 string-end)
                             str)
                        (let-match-string
                            ((pref)
                             (suf))
                            str
                          (cl-labels ((lens-cand (op)
                                        ;; return a lensed version of OP, which applies OP to
                                        ;; either a string or the first element of a list of strings.
                                        (lambda (x)
                                          (pcase x
                                            (`(,(and str (pred stringp)) . ,rest)
                                             (funcall op str))
                                            ((and str (pred stringp))
                                             (funcall op str))))))
                            (->> options
                                 (-filter (lens-cand (## string-prefix-p suf %)))
                                 (mapcar (lens-cand (## concat pref %)))))))))
         (table (completion-table-dynamic completer)))
    (completion-table-with-annotation table annotator)))

(transient-define-argument jj-git-push--named-infix ()
  "Push a new bookmark NAME at REVISION."
  :class 'transient-option
  :multi-value 'repeat
  :reader (lambda (prompt initial-input history)
            (let* ((thing (jj-thing-at-point))
                   (rev-pt (jj--get-change thing))
                   (pt-ann (jj--rev-at-point-option thing))
                   (crm-separator (rx (* blank)
                                      (any ",| ")
                                      (* blank)))
                   (answers (completing-read-multiple
                             prompt
                             (make--jj-named-infix-table (jj--list-relevant-revisions pt-ann))
                             nil nil initial-input history)))
              (mapcar (lambda (ans)
                        (pcase ans
                          ("^" rev-pt)
                          (_ ans)))
                      answers))))

(transient-define-prefix jj-git-push-prefix ()
  ;; these flags unset one-another
  :incompatible (let ((coarse-flags '("--all" "--deleted" "--tracked"))
                      (fine-flags '("--bookmark=" "--change=" "--revisions=" "--named=")))
                  ;; all the fine flags are incompatible with all the coarse flags,
                  ;; but they are mostly compatible with each other.
                  `(("--all" "--tracked")
                    ,@(prod-cartes coarse-flags fine-flags)))
  jj-global-group
  [["coarse"
    ;; just a layout option
    :pad-keys t
    ("-d" "deleted" "--deleted")
    ("-a" "all" "--all")
    ("-t" "tracked" "--tracked")]
   ["fine"
    ;; just a layout option
    :pad-keys t
    ("-b" "bookmarks" "--bookmark="
     ;; push the named bookmark
     :prompt "bookmarks: "
     :multi-value repeat
     ;; really, this should read a limited revset
     ;; By default, the specified pattern matches branch names with glob syntax, but only `*` is expanded. Other wildcard characters such as `?` are *not* supported. Patterns can be
     ;; repeated or combined with [logical operators] to specify multiple branches, but only union and negative intersection are supported.
     :reader (lambda (prompt initial-input history)
               (let ((crm-separator (rx (* blank)
                                        (any ",| ")
                                        (* blank))))
                 (let ((options (jj-list-bookmarks-annotated)))
                   (completing-read-multiple
                    prompt (completion-table-with-annotation
                            (completion-table-dynamic (cl-constantly options))
                            (lambda (c)
                              (-let (((chg cmt) (alist-get c options nil nil #'string=)))
                                (format "\t%s\t%s"
                                        (propertize chg 'face '(:foreground "magenta"))
                                        (propertize cmt 'face '(:foreground "light blue"))))))
                    nil nil initial-input history)))))
    ("-r" "bookmarks in revset" "--revisions="
     ;; push existing bookmarks that are within the given revset
     :multi-value repeat
     :reader (lambda (prompt initial-input history)
               (let ((crm-separator (rx (* blank)
                                        (any ",| ")
                                        (* blank))))
                 (let ((options (mapcar (-lambda ((bk chg cmt))
                                          `(,chg ,cmt ,bk))
                                        (jj-list-bookmarks-annotated))))
                   (completing-read-multiple
                    prompt (completion-table-with-annotation
                            (completion-table-dynamic (cl-constantly options))
                            (lambda (c)
                              (-let (((cmt bk) (alist-get c options nil nil #'string=)))
                                (format "\t%s\t%s"
                                        (propertize cmt 'face '(:foreground "light blue"))
                                        (propertize bk 'face '(:foreground "magenta"))))))
                    nil nil initial-input history))
                 )))
    ("-c" "new on change" "--change="
     ;; push a change, adding a new bookmark there
     :multi-value repeat
     :reader (lambda (prompt initial-input history)
               (let ((crm-separator (rx (* blank)
                                        (any ",| ")
                                        (* blank))))
                 (completing-read-multiple
                  prompt `("@"
                           ,@(jj-match-revisions))
                  nil nil initial-input history))))
    ("-n" "new named on change" jj-git-push--named-infix
     ;; push a change, adding a new bookmark with the given name
     :argument "--named=")
    ]
   ["remote"
    ;; just a layout option
    :pad-keys t
    ("-m" "remote" "--remote="
     :prompt "remote: "
     :reader (lambda (prompt initial-input history)
               (completing-read
                prompt (jj-list-git-remotes)
                nil
                ;; only named remotes are supported
                :req-match
                initial-input history))
     )]
   ["misc"
    ;; just a layout option
    :pad-keys t
    ("-D" "dry-run" "--dry-run")
    ("-E" "allow undescribed" "--allow-empty-description")
    ("-P" "allow private" "--allow-private")]
   ]
  ["push"
   ("P" "push" jj--git-push-suffix :transient nil)])
;; transient:1 ends here

;; simple

;; [[file:majjik.org::*simple][simple:1]]
(defun jj-git-fetch (&rest args)
  "Fetch from git in the background."
  (interactive)
  (jj-cmd-async-view `("git" "fetch" ,@args) nil :verbose-error))
;; simple:1 ends here

;; transient

;; [[file:majjik.org::*transient][transient:1]]
(transient-define-suffix jj--git-fetch-suffix (args)
  "do a jj git fetch."
  (interactive (list (jj--transient-args)))
  (apply #'jj-git-fetch args))

(transient-define-prefix jj-git-fetch-prefix ()
  ;; these flags unset one-another
  :incompatible `(("--remote=" "--all-remotes")
                  ("--bookmark=" "--tracked"))
  jj-global-group
  [["bookmarks"
    ;; just a layout option
    :pad-keys t
    
    ("-t" "tracked" "--tracked")
    ("-b" "bookmarks" "--bookmark="
     :prompt "bookmarks: "
     :multi-value repeat
     ;; really, this should read a limited revset
     ;; By default, the specified pattern matches branch names with glob syntax, but only `*` is expanded. Other wildcard characters such as `?` are *not* supported. Patterns can be
     ;; repeated or combined with [logical operators] to specify multiple branches, but only union and negative intersection are supported.
     :reader (lambda (prompt initial-input history)
               (let ((crm-separator (rx (* blank)
                                        (any ",| ")
                                        (* blank))))
                 (completing-read-multiple
                  prompt (jj-list-bookmarks)
                  nil nil initial-input history))))]
   ["remotes" :pad-keys t
    ("-a" "all remotes" "--all-remotes") ;; list
    ("-m" "remotes" "--remote="
     :prompt "remotes: "
     :multi-value repeat
     :reader (lambda (prompt initial-input history)
               (let ((crm-separator (rx (* blank)
                                        (any ",| ")
                                        (* blank))))
                 (completing-read-multiple
                  prompt (jj-list-git-remotes)
                  nil
                  ;; only named remotes are supported
                  :req-match
                  initial-input history)))
     )]]
  ["fetch"
   ("F" "fetch" jj--git-fetch-suffix :transient nil)])
;; transient:1 ends here

;; entry

;; [[file:majjik.org::*entry][entry:1]]
(transient-define-prefix jj-config-prefix ()
  :refresh-suffixes t
  [["edit"
    ("e" "edit" jj-config-edit-prefix)
    ;; ("s" "set")
    ;; ("u" "unset")
    ]
   ;; ["view"
   ;;  ("g" "get")
   ;;  ("l" "list")]
   ;; ["meta"
   ;;  ("p" "path")]
   ])
;; entry:1 ends here

;; edit

;; [[file:majjik.org::*edit][edit:1]]
(transient-define-prefix jj-config-edit-prefix ()
  :refresh-suffixes t
  :incompatible '(("--user" "--repo" "--workspace"))
  jj-global-group
  ["targets"
   ("-u" "user" "--user")
   ("-r" "repo" "--repo")
   ("-w" "workspace" "--workspace")]
  ["go"
   ("e" "edit" (lambda (args)
                 (interactive (list (jj--transient-args)))
                 (jj-with-editor
                  (jj-cmd-async-message `("config" "edit" ,@args))))
    :inapt-if-not (lambda ()
                    (transient--any-on-p "--user" "--repo" "--workspace")))
   ])
;; edit:1 ends here

;; entry

;; [[file:majjik.org::*entry][entry:1]]
(transient-define-prefix jj-git-prefix ()
  :refresh-suffixes t
  jj-global-group
  [["remote"
    ("p" "push" jj-git-push-prefix)
    ("f" "fetch" jj-git-fetch-prefix)
    ("m" "remote" jj-git-remote-prefix)
    ]
   ["manage"
    ("l" "colocation" jj-git-colocation-prefix)
    ("r" "root" (lambda ()
                  (interactive)
                  (jj-cmd-async-view `("git" "root")))
     :transient t)
    ("ge" "export"
     (lambda ()
       (interactive)
       (jj-cmd-async-view '("git" "export"))))
    ("gi" "import"
     (lambda ()
       (interactive)
       (jj-cmd-async-view '("git" "import"))))
    ]
   ["misc"
    ("G" "magit" magit-status)
    ("i" "init" jj-git-init-prefix)
    ("C" "clone" jj-git-clone-prefix)]])
;; entry:1 ends here

;; colocation

;; [[file:majjik.org::*colocation][colocation:1]]
(transient-define-prefix jj-git-colocation-prefix ()
  jj-global-group
  ["go"
   ("d" "disable"
    (lambda ()
      (interactive)
      (jj-cmd-async-view '("git" "colocation" "disable"))))
   ("e" "enable"
    (lambda ()
      (interactive)
      (jj-cmd-async-view '("git" "colocation" "enable"))))
   ("s" "status"
    (lambda ()
      (interactive)
      (jj-cmd-async-message '("git" "colocation" "status")))
    :transient t)
   ])
;; colocation:1 ends here

;; remote

;; [[file:majjik.org::*remote][remote:1]]
(transient-define-prefix jj-git-remote-prefix ()
  :refresh-suffixes t
  jj-global-group
  ["go"
   ("a" "add" jj-git-remote-add-prefix)
   ("l" "list" (lambda ()
                 (interactive)
                 (jj-cmd-async-message `("git" "remote" "list")))
    :transient t)
   ("k" "remove" (lambda (name)
                   (interactive (list (completing-read "Remove remote: " (jj-list-git-remotes) nil t)))
                   (jj-cmd-async-view `("git" "remote" "remove" ,name))))
   ("r" "rename" (lambda (old-name new-name)
                   (interactive (list (completing-read "Rename remote: " (jj-list-git-remotes) nil t)
                                      (read-string "New name: ")))
                   (jj-cmd-async-view `("git" "remote" "rename" ,old-name ,new-name))))
   ("u" "set-url" jj-git-remote-set-url-prefix)
   ])
;; remote:1 ends here

;; git remote add

;; [[file:majjik.org::*git remote add][git remote add:1]]
(transient-define-prefix jj-git-remote-add-prefix ()
  jj-global-group
  ["arguments"
   ("-p" "push-url" "--push-url=")
   ("-f" "fetch-tags" "--fetch-tags="
    :class transient-switches
    :argument-format "--fetch-tags=%s"
    :argument-regexp "\\(--fetch-tags=\\(all\\|included\\|none\\)\\)"
    :choices ("all" "included" "none"))]
  ["go"
   ("a" "add" (lambda (args name url)
                (interactive (list (jj--transient-args)
                                   (read-string "New name: ")
                                   (read-string "New url: ")))
                (jj-cmd-async-view `("git" "remote" "add" ,@args ,name ,url)))
    )])
;; git remote add:1 ends here

;; git remote set-url

;; [[file:majjik.org::*git remote set-url][git remote set-url:1]]
(transient-define-prefix jj-git-remote-set-url-prefix ()
  jj-global-group
  ["arguments"
   ("-p" "push" "--push=")
   ("-f" "fetch" "--fetch=")
   ]
  ["go"
   ("u" "set-url"
    (lambda (args name)
      (interactive (list (jj--transient-args)
                         (completing-read "Update remote: "
                                          (jj-list-git-remotes) nil t)
                         ))
      (jj-git-remote-set-url args name nil))
    )
   ("U" "set-url (prompt for url)"
    (lambda (args name url)
      (interactive (list (jj--transient-args)
                         (completing-read "Update remote: "
                                          (jj-list-git-remotes) nil t)
                         (read-string "New url: ")))
      (jj-git-remote-set-url nil name url))
    )
   ])

(defun jj-git-remote-set-url (args name url)
  (jj-cmd-async-view `("git" "remote" "set-url" ,@args ,name ,@(opt url))))
;; git remote set-url:1 ends here

;; clone

;; [[file:majjik.org::*clone][clone:1]]
(defun jj--git-clone (url dir args)
  (let* ((cmd `("git" "clone"
                ,url
                ,(expand-file-name dir)
                ,@args)))
    (promise-then
     (jj-cmd-async-view cmd :no-rev :verbose-error)
     (lambda (proc)
       (jj-dash--async dir)))))

(transient-define-prefix jj-git-clone-prefix ()
  :refresh-suffixes t
  :incompatible (prod-cartes '("--colocate") '("--git-repo=" "--no-colocate"))
  jj-global-group
  ["targets"
   ("-m" "remote" "--remote=")
   ("-c" "colocate" "--colocate")
   ("-n" "no-colocate" "--no-colocate")
   ("-d" "depth" "--depth="
    :reader transient-read-number-N+)
   ("-f" "fetch-tags" "--fetch-tags="
    :class transient-switches
    :argument-format "--fetch-tags=%s"
    :argument-regexp "\\(--fetch-tags=\\(all\\|included\\|none\\)\\)"
    :choices ("all" "included" "none"))
   ("-b" "branch" "--branch=")
   ]
  ["go"
   ("c" "clone" (lambda (url path args)
                  (interactive (list (read-string "clone url: ")
                                     (read-directory-name "clone into: ")
                                     (jj--transient-args)))
                  (jj--git-clone url path args)))])
;; clone:1 ends here

;; Provide

;; [[file:majjik.org::*Provide][Provide:1]]
(provide 'majjik)
;; Provide:1 ends here

;;; majjik.el ends here
