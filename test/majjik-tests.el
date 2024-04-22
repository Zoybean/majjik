;;; majjik-tests.el --- Tests for Majjik  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2008-2023 The Majjik Project Contributors

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Majjik is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Majjik is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Majjik.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ert)
(require 'tramp)
(require 'tramp-sh)

(require 'majjik)

(defun majjik-test-init-repo (dir &rest args)
  (let ((majjik-jj-global-arguments
         (nconc (list "-c" "init.defaultBranch=master")
                majjik-jj-global-arguments)))
    (majjik-jj "init" args dir)))

(defmacro majjik-with-test-directory (&rest body)
  (declare (indent 0) (debug t))
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (file-name-as-directory (make-temp-file "majjik-" t)))
           (process-environment process-environment)
           (majjik-jj-global-arguments
            (nconc (list "-c" "protocol.file.allow=always")
                   majjik-jj-global-arguments)))
       (push "JJ_AUTHOR_NAME=A U Thor" process-environment)
       (push "JJ_AUTHOR_EMAIL=a.u.thor@example.com" process-environment)
       (condition-case err
           (cl-letf (((symbol-function #'message) (lambda (&rest _))))
             (let ((default-directory (file-truename ,dir)))
               ,@body))
         (error (message "Keeping test directory:\n  %s" ,dir)
                (signal (car err) (cdr err))))
       (delete-directory ,dir t))))

(defmacro majjik-with-test-repository (&rest body)
  (declare (indent 0) (debug t))
  `(majjik-with-test-directory (majjik-test-init-repo ".") ,@body))

(defmacro majjik-with-bare-test-repository (&rest body)
  (declare (indent 1) (debug t))
  `(majjik-with-test-directory (majjik-test-init-repo "." "--bare") ,@body))

(defun majjik-test-visible-text (&optional raw)
  (save-excursion
    (let (chunks)
      (goto-char (point-min))
      (while (let ((to (next-single-char-property-change (point) 'invisible)))
               (unless (invisible-p (point))
                 (push (buffer-substring-no-properties (point) to) chunks))
               (goto-char to)
               (< (point) (point-max))))
      (let ((result (mapconcat #'identity (nreverse chunks) nil)))
        (unless raw
          (setq result (string-trim result)))
        result))))

;;; jj

(ert-deftest majjik--with-safe-default-directory ()
  (majjik-with-test-directory
    (let ((find-file-visit-truename nil))
      (should (equal (majjik-toplevel "repo/")
                     (majjik-toplevel (expand-file-name "repo/"))))
      (should (equal (majjik-toplevel "repo")
                     (majjik-toplevel (expand-file-name "repo/")))))))

(ert-deftest majjik-toplevel:basic ()
  (let ((find-file-visit-truename nil))
    (majjik-with-test-directory
      (majjik-test-init-repo "repo")
      (majjik-test-majjik-toplevel)
      (should (equal (majjik-toplevel   "repo/.jj/")
                     (expand-file-name "repo/")))
      (should (equal (majjik-toplevel   "repo/.jj/objects/")
                     (expand-file-name "repo/")))
      (should (equal (majjik-toplevel   "repo-link/.jj/")
                     (expand-file-name "repo-link/")))
      (should (equal (majjik-toplevel   "repo-link/.jj/objects/")
                     ;; We could theoretically return "repo-link/"
                     ;; here by going up until `--jj-dir' gives us
                     ;; "." .  But that would be a bit risky and Majjik
                     ;; never goes there anyway, so it's not worth it.
                     ;; But in the doc-string we say we cannot do it.
                     (expand-file-name "repo/"))))))

(ert-deftest majjik-toplevel:submodule ()
  (let ((find-file-visit-truename nil))
    (majjik-with-test-directory
      (majjik-test-init-repo "remote")
      (let ((default-directory (expand-file-name "remote/")))
        (majjik-jj "commit" "-m" "init" "--allow-empty"))
      (majjik-test-init-repo "super")
      (setq default-directory (expand-file-name "super/"))
      (majjik-jj "submodule" "add" "../remote" "repo/")
      (majjik-test-majjik-toplevel)
      (should (equal (majjik-toplevel   ".jj/modules/repo/")
                     (expand-file-name "repo/")))
      (should (equal (majjik-toplevel   ".jj/modules/repo/objects/")
                     (expand-file-name "repo/"))))))

(defun majjik-test-majjik-toplevel ()
  ;; repo
  (make-directory "repo/subdir/subsubdir" t)
  (should (equal (majjik-toplevel   "repo/")
                 (expand-file-name "repo/")))
  (should (equal (majjik-toplevel   "repo/")
                 (expand-file-name "repo/")))
  (should (equal (majjik-toplevel   "repo/subdir/")
                 (expand-file-name "repo/")))
  (should (equal (majjik-toplevel   "repo/subdir/subsubdir/")
                 (expand-file-name "repo/")))
  ;; repo-link
  (make-symbolic-link "repo" "repo-link")
  (should (equal (majjik-toplevel   "repo-link/")
                 (expand-file-name "repo-link/")))
  (should (equal (majjik-toplevel   "repo-link/subdir/")
                 (expand-file-name "repo-link/")))
  (should (equal (majjik-toplevel   "repo-link/subdir/subsubdir/")
                 (expand-file-name "repo-link/")))
  ;; *subdir-link
  (make-symbolic-link "repo/subdir"           "subdir-link")
  (make-symbolic-link "repo/subdir/subsubdir" "subsubdir-link")
  (should (equal (majjik-toplevel   "subdir-link/")
                 (expand-file-name "repo/")))
  (should (equal (majjik-toplevel   "subdir-link/subsubdir/")
                 (expand-file-name "repo/")))
  (should (equal (majjik-toplevel   "subsubdir-link")
                 (expand-file-name "repo/")))
  ;; subdir-link-indirect
  (make-symbolic-link "subdir-link" "subdir-link-indirect")
  (should (equal (majjik-toplevel   "subdir-link-indirect")
                 (expand-file-name "repo/")))
  ;; wrap/*link
  (majjik-test-init-repo "wrap")
  (make-symbolic-link "../repo"                  "wrap/repo-link")
  (make-symbolic-link "../repo/subdir"           "wrap/subdir-link")
  (make-symbolic-link "../repo/subdir/subsubdir" "wrap/subsubdir-link")
  (should (equal (majjik-toplevel   "wrap/repo-link/")
                 (expand-file-name "wrap/repo-link/")))
  (should (equal (majjik-toplevel   "wrap/subdir-link")
                 (expand-file-name "repo/")))
  (should (equal (majjik-toplevel   "wrap/subsubdir-link")
                 (expand-file-name "repo/"))))

(defun majjik-test-majjik-get ()
  (should (equal (majjik-get-all "a.b") '("val1" "val2")))
  (should (equal (majjik-get "a.b") "val2"))
  (let ((default-directory (expand-file-name "../remote/")))
    (should (equal (majjik-get "a.b") "remote-value")))
  (should (equal (majjik-get "CAM.El.Case.VAR") "value"))
  (should (equal (majjik-get "a.b2") "line1\nline2")))

(ert-deftest majjik-get ()
  (majjik-with-test-directory
   (majjik-test-init-repo "remote")
   (let ((default-directory (expand-file-name "remote/")))
     (majjik-jj "commit" "-m" "init" "--allow-empty")
     (majjik-jj "config" "a.b" "remote-value"))
   (majjik-test-init-repo "super")
   (setq default-directory (expand-file-name "super/"))
   ;; Some tricky cases:
   ;; Multiple config values.
   (majjik-jj "config" "a.b" "val1")
   (majjik-jj "config" "--add" "a.b" "val2")
   ;; CamelCase variable names.
   (majjik-jj "config" "Cam.El.Case.Var" "value")
   ;; Values with newlines.
   (majjik-jj "config" "a.b2" "line1\nline2")
   ;; Config variables in submodules.
   (majjik-jj "submodule" "add" "../remote" "repo/")

   (majjik-test-majjik-get)
   (let ((majjik--refresh-cache (list (cons 0 0))))
     (majjik-test-majjik-get))))

(ert-deftest majjik-get-boolean ()
  (majjik-with-test-repository
    (majjik-jj "config" "a.b" "true")
    (should     (majjik-get-boolean "a.b"))
    (should     (majjik-get-boolean "a" "b"))
    (majjik-jj "config" "a.b" "false")
    (should-not (majjik-get-boolean "a.b"))
    (should-not (majjik-get-boolean "a" "b"))
    ;; Multiple values, last one wins.
    (majjik-jj "config" "--add" "a.b" "true")
    (should     (majjik-get-boolean "a.b"))
    (let ((majjik--refresh-cache (list (cons 0 0))))
     (should    (majjik-get-boolean "a.b")))))

(ert-deftest majjik-get-{current|next}-tag ()
  (majjik-with-test-repository
    (majjik-jj "commit" "-m" "1" "--allow-empty")
    (should (equal (majjik-get-current-tag) nil))
    (should (equal (majjik-get-next-tag)    nil))
    (majjik-jj "tag" "1")
    (should (equal (majjik-get-current-tag) "1"))
    (should (equal (majjik-get-next-tag)    nil))
    (majjik-jj "commit" "-m" "2" "--allow-empty")
    (majjik-jj "tag" "2")
    (should (equal (majjik-get-current-tag) "2"))
    (should (equal (majjik-get-next-tag)    nil))
    (majjik-jj "commit" "-m" "3" "--allow-empty")
    (should (equal (majjik-get-current-tag) "2"))
    (should (equal (majjik-get-next-tag)    nil))
    (majjik-jj "commit" "-m" "4" "--allow-empty")
    (majjik-jj "tag" "4")
    (majjik-jj "reset" "HEAD~")
    (should (equal (majjik-get-current-tag) "2"))
    (should (equal (majjik-get-next-tag)    "4"))))

(ert-deftest majjik-list-{|local-|remote-}branch-names ()
  (majjik-with-test-repository
    (majjik-jj "commit" "-m" "init" "--allow-empty")
    (majjik-jj "update-ref" "refs/remotes/foobar/master" "master")
    (majjik-jj "update-ref" "refs/remotes/origin/master" "master")
    (should (equal (majjik-list-branch-names)
                   (list "master" "foobar/master" "origin/master")))
    (should (equal (majjik-list-local-branch-names)
                   (list "master")))
    (should (equal (majjik-list-remote-branch-names)
                   (list "foobar/master" "origin/master")))
    (should (equal (majjik-list-remote-branch-names "origin")
                   (list "origin/master")))
    (should (equal (majjik-list-remote-branch-names "origin" t)
                   (list "master")))))

(ert-deftest majjik-process:match-prompt-nil-when-no-match ()
  (should (null (majjik-process-match-prompt '("^foo: ?$") "bar: "))))

(ert-deftest majjik-process:match-prompt-non-nil-when-match ()
  (should (majjik-process-match-prompt '("^foo: ?$") "foo: ")))

(ert-deftest majjik-process:match-prompt-match-non-first-prompt ()
  (should (majjik-process-match-prompt '("^bar: ?$ " "^foo: ?$") "foo: ")))

(ert-deftest majjik-process:match-prompt-suffixes-prompt ()
  (let ((prompts '("^foo: ?$")))
    (should (equal (majjik-process-match-prompt prompts "foo:")  "foo: "))
    (should (equal (majjik-process-match-prompt prompts "foo: ") "foo: "))))

(ert-deftest majjik-process:match-prompt-preserves-match-group ()
  (let* ((prompts '("^foo '\\(?99:.*\\)': ?$"))
         (prompt (majjik-process-match-prompt prompts "foo 'bar':")))
    (should (equal prompt "foo 'bar': "))
    (should (equal (match-string 99 "foo 'bar':") "bar"))))

(ert-deftest majjik-process:password-prompt ()
  (let ((majjik-process-find-password-functions
         (list (lambda (host) (when (string= host "www.host.com") "mypasswd")))))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_process string) string)))
      (should (string-equal (majjik-process-password-prompt
                             nil "Password for 'www.host.com':")
                            "mypasswd\n")))))

(ert-deftest majjik-process:password-prompt-observed ()
  (with-temp-buffer
    (cl-letf* ((test-proc (start-process
                           "dummy-proc" (current-buffer)
                           (concat invocation-directory invocation-name)
                           "-Q" "--batch" "--eval" "(read-string \"\")"))
               ((symbol-function 'read-passwd)
                (lambda (_) "mypasswd"))
               (sent-strings nil)
               ((symbol-function 'process-send-string)
                (lambda (_proc string) (push string sent-strings))))
      ;; Don't get stuck when we close the buffer.
      (set-process-query-on-exit-flag test-proc nil)
      ;; Try some example passphrase prompts, reported by users.
      (dolist (prompt '("
Enter passphrase for key '/home/user/.ssh/id_rsa': "
                        ;; Openssh 8.0 sends carriage return.
                        "\
\rEnter passphrase for key '/home/user/.ssh/id_ed25519': "))
        (majjik-process-filter test-proc prompt)
        (should (equal (pop sent-strings) "mypasswd\n")))
      (should (null sent-strings)))))

;;; Clone

(ert-deftest majjik-clone:--name-to-url-format-defaults ()
  (majjik-with-test-repository
   (majjik-jj "config" "--add" "sourcehut.user" "~shuser")
   (majjik-jj "config" "--add" "jjhub.user" "ghuser")
   (majjik-jj "config" "--add" "jjlab.user" "gluser")
   ;; No explicit service
   (should (string-equal (majjik-clone--name-to-url "a/b")
                         "jj@jjhub.com:a/b.jj"))
   (should (string-equal (majjik-clone--name-to-url "b")
                         "jj@jjhub.com:ghuser/b.jj"))
   ;; User in config
   (should (string-equal (majjik-clone--name-to-url "gh:b")
                         "jj@jjhub.com:ghuser/b.jj"))
   (should (string-equal (majjik-clone--name-to-url "gl:n")
                         "jj@jjlab.com:gluser/n.jj"))
   (should (string-equal (majjik-clone--name-to-url "sh:l")
                         "jj@jj.sr.ht:~shuser/l"))
   ;; Explicit user (abbreviated service names)
   (should (string-equal (majjik-clone--name-to-url "gh:a/b")
                         "jj@jjhub.com:a/b.jj"))
   (should (string-equal (majjik-clone--name-to-url "gl:t/s")
                         "jj@jjlab.com:t/s.jj"))
   (should (string-equal (majjik-clone--name-to-url "sh:~x/y")
                         "jj@jj.sr.ht:~x/y"))
   ;; Explicit user (long service names)
   (should (string-equal (majjik-clone--name-to-url "jjhub:a1/b1")
                         "jj@jjhub.com:a1/b1.jj"))
   (should (string-equal (majjik-clone--name-to-url "jjlab:t1/s1")
                         "jj@jjlab.com:t1/s1.jj"))
   (should (string-equal (majjik-clone--name-to-url "sourcehut:~x1/y1")
                         "jj@jj.sr.ht:~x1/y1"))))

(ert-deftest majjik-clone:--name-to-url-format-single-string ()
  (let ((majjik-clone-url-format "bird@%h:%n.jj")
        (majjik-clone-name-alist
         '(("\\`\\(?:jjhub:\\|gh:\\)?\\([^:]+\\)\\'" "jjhub.com" "u")
           ("\\`\\(?:jjlab:\\|gl:\\)\\([^:]+\\)\\'" "jjlab.com" "u"))))
    (should (string-equal (majjik-clone--name-to-url "gh:a/b")
                          "bird@jjhub.com:a/b.jj"))
    (should (string-equal (majjik-clone--name-to-url "gl:a/b")
                          "bird@jjlab.com:a/b.jj"))
    (should (string-equal (majjik-clone--name-to-url "jjhub:c/d")
                          "bird@jjhub.com:c/d.jj"))
    (should (string-equal (majjik-clone--name-to-url "jjlab:c/d")
                          "bird@jjlab.com:c/d.jj"))))

(ert-deftest majjik-clone:--name-to-url-format-bad-type-throws-error ()
  (let ((majjik-clone-url-format 3))
    (should-error (majjik-clone--name-to-url "gh:a/b")
                  :type 'user-error)))

(ert-deftest majjik-clone:--name-to-url-format-alist-different-urls-per-hostname ()
  (let ((majjik-clone-name-alist
         '(("\\`\\(?:example:\\|ex:\\)\\([^:]+\\)\\'" "jj.example.com" "foouser")
           ("\\`\\(?:gh:\\)?\\([^:]+\\)\\'" "jjhub.com" "u")))
        (majjik-clone-url-format
         '(("jj.example.com" . "cow@%h:~%n")
           (t . "jj@%h:%n.jj"))))
    (should (string-equal (majjik-clone--name-to-url "gh:a/b")
                          "jj@jjhub.com:a/b.jj"))
    (should (string-equal (majjik-clone--name-to-url "ex:a/b")
                          "cow@jj.example.com:~a/b"))
    (should (string-equal (majjik-clone--name-to-url "example:x/y")
                          "cow@jj.example.com:~x/y"))
    (should (string-equal (majjik-clone--name-to-url "ex:c")
                          "cow@jj.example.com:~foouser/c"))))

(ert-deftest majjik-clone:--name-to-url-format-alist-no-fallback-throws-error ()
  (let ((majjik-clone-url-format '(("fail.example.com" . "jj@%h:~%n"))))
    (should-error (majjik-clone--name-to-url "gh:a/b")
                  :type 'user-error)))

;;; Status

(defun majjik-test-get-section (list file)
  (majjik-status-setup-buffer default-directory)
  (--first (equal (oref it value) file)
           (oref (majjik-get-section `(,list (status)))
                 children)))

(ert-deftest majjik-status:file-sections ()
  (majjik-with-test-repository
    (cl-flet ((modify (file) (with-temp-file file
                               (insert (make-temp-name "content")))))
      (modify "file")
      (modify "file with space")
      (modify "file with äöüéλ")
      (should (majjik-test-get-section '(untracked) "file"))
      (should (majjik-test-get-section '(untracked) "file with space"))
      (should (majjik-test-get-section '(untracked) "file with äöüéλ"))
      (majjik-stage-modified t)
      (should (majjik-test-get-section '(staged) "file"))
      (should (majjik-test-get-section '(staged) "file with space"))
      (should (majjik-test-get-section '(staged) "file with äöüéλ"))
      (majjik-jj "add" ".")
      (modify "file")
      (modify "file with space")
      (modify "file with äöüéλ")
      (should (majjik-test-get-section '(unstaged) "file"))
      (should (majjik-test-get-section '(unstaged) "file with space"))
      (should (majjik-test-get-section '(unstaged) "file with äöüéλ")))))

(ert-deftest majjik-status:log-sections ()
  (majjik-with-test-repository
    (majjik-jj "commit" "-m" "common" "--allow-empty")
    (majjik-jj "commit" "-m" "unpulled" "--allow-empty")
    (majjik-jj "remote" "add" "origin" "/origin")
    (majjik-jj "update-ref" "refs/remotes/origin/master" "master")
    (majjik-jj "branch" "--set-upstream-to=origin/master")
    (majjik-jj "reset" "--hard" "HEAD~")
    (majjik-jj "commit" "-m" "unpushed" "--allow-empty")
    (should (majjik-test-get-section
             '(unpulled . "..@{upstream}")
             (majjik-rev-parse "--short" "origin/master")))
    (should (majjik-test-get-section
             '(unpushed . "@{upstream}..")
             (majjik-rev-parse "--short" "master")))))

(ert-deftest majjik-status:section-commands ()
  (majjik-with-test-repository
    (majjik-jj "commit" "-m" "dummy" "--allow-empty")
    (with-current-buffer (majjik-status-setup-buffer)
      (majjik-section-show-level-1-all)
      (should (string-match-p
               "\\`Head:[[:space:]]+master dummy\n\nRecent commits\\'"
               (majjik-test-visible-text)))
      (majjik-section-show-level-2-all)
      (should (string-match-p
               "\\`Head:[[:space:]]+master dummy\n
Recent commits\n[[:xdijj:]]\\{7,\\} master dummy\\'"
               (majjik-test-visible-text)))
      (goto-char (point-min))
      (search-forward "Recent")
      (majjik-section-show-level-1)
      (should (string-match-p
               "\\`Head:[[:space:]]+master dummy\n\nRecent commits\\'"
               (majjik-test-visible-text))))))

;;; libjj

(ert-deftest majjik-in-bare-repo ()
  "Test `majjik-bare-repo-p' in a bare repository."
  (majjik-with-bare-test-repository
    (should (majjik-bare-repo-p))))

(ert-deftest majjik-in-non-bare-repo ()
  "Test `majjik-bare-repo-p' in a non-bare repository."
  (majjik-with-test-repository
    (should-not (majjik-bare-repo-p))))

;;; Utils

(ert-deftest majjik-utils:add-face-text-property ()
  (let ((str (concat (propertize "ab" 'font-lock-face 'highlight) "cd")))
    (majjik--add-face-text-property 0 (length str) 'bold nil str)
    (should (equal (get-text-property 0 'font-lock-face str) '(bold highlight)))
    (should (equal (get-text-property 2 'font-lock-face str) '(bold)))))

(ert-deftest majjik-base:ellipsis-default-values ()
  (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
    (should (equal "…" (majjik--ellipsis 'margin)))
    (should (equal "…" (majjik--ellipsis))))
  (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) nil)))
    (should (equal ">" (majjik--ellipsis 'margin)))
    (should (equal "..." (majjik--ellipsis)))))

(ert-deftest majjik-base:ellipsis-customisations-are-respected ()
  (let ((majjik-ellipsis '((margin (?· . "!")) (t (?. . ">")))))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
      (should (equal "·" (majjik--ellipsis 'margin)))
      (should (equal "." (majjik--ellipsis))))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) nil)))
      (should (equal "!" (majjik--ellipsis 'margin)))
      (should (equal ">" (majjik--ellipsis))))))

(ert-deftest majjik-base:ellipsis-fancy-nil-defaults-to-universal ()
  (let ((majjik-ellipsis '((margin (nil . "...")) (t (nil . "^^^")))))
    (should (equal "..." (majjik--ellipsis 'margin)))
    (should (equal "^^^" (majjik--ellipsis)))))

(ert-deftest majjik-base:ellipsis-legacy-type-allowed ()
  (let ((majjik-ellipsis "⋮"))
    (should (equal "⋮" (majjik--ellipsis 'margin)))
    (should (equal "⋮" (majjik--ellipsis)))))

(ert-deftest majjik-base:ellipsis-malformed-customisation-no-default ()
  (let ((majjik-ellipsis '((margin (?· . "!")))))
    (should-error (majjik--ellipsis)
                  :type 'user-error)))

(ert-deftest majjik-base:ellipsis-unknown-use-case-defaults-to-default ()
  (let ((majjik-ellipsis '((margin (?· . "!")) (t (?. . ">")))))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
      (should (equal (majjik--ellipsis 'foo) (majjik--ellipsis))))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) nil)))
      (should (equal (majjik--ellipsis 'foo) (majjik--ellipsis))))))

;;; majjik-tests.el ends soon
(provide 'majjik-tests)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; majjik-tests.el ends here
