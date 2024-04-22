;;; majjik-clone.el --- Clone a repository  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2024 The Magit Project Contributors

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements clone commands.

;;; Code:

(require 'majjik)

;;; Options

(defcustom magit-clone-always-transient nil
  "Whether `magit-clone' always acts as a transient prefix command.
If nil, then a prefix argument has to be used to show the transient
popup instead of invoking the default suffix `magit-clone-regular'
directly."
  :package-version '(magit . "3.0.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-clone-name-alist
  '(("\\`\\(?:github:\\|gh:\\)?\\([^:]+\\)\\'" "github.com" "github.user")
    ("\\`\\(?:gitlab:\\|gl:\\)\\([^:]+\\)\\'"  "gitlab.com" "gitlab.user")
    ("\\`\\(?:sourcehut:\\|sh:\\)\\([^:]+\\)\\'" "git.sr.ht" "sourcehut.user"))
  "Alist mapping repository names to repository urls.

Each element has the form (REGEXP HOSTNAME USER).  When the user
enters a name when a cloning command asks for a name or url, then
that is looked up in this list.  The first element whose REGEXP
matches is used.

The format specified by option `magit-clone-url-format' is used
to turn the name into an url, using HOSTNAME and the repository
name.  If the provided name contains a slash, then that is used.
Otherwise if the name omits the owner of the repository, then the
default user specified in the matched entry is used.

If USER contains a dot, then it is treated as a Git variable and
the value of that is used as the username.  Otherwise it is used
as the username itself."
  :package-version '(magit . "4.0.0")
  :group 'magit-commands
  :type '(repeat (list regexp
                       (string :tag "Hostname")
                       (string :tag "User name or git variable"))))

(defcustom magit-clone-url-format
  '(("git.sr.ht" . "git@%h:%n")
    (t . "git@%h:%n.git"))
  "Format(s) used when turning repository names into urls.

In a format string, %h is the hostname and %n is the repository
name, including the name of the owner.

The value can be a string (representing a single static format)
or an alist with elements (HOSTNAME . FORMAT) mapping hostnames
to formats.  When an alist is used, the t key represents the
default.  Also see `magit-clone-name-alist'."
  :package-version '(magit . "4.0.0")
  :group 'magit-commands
  :type '(choice (string :tag "Format")
                 (alist :key-type (choice (string :tag "Host")
                                          (const :tag "Default" t))
                        :value-type (string :tag "Format"))))

(defcustom magit-post-clone-hook nil
  "Hook run after the repository has been successfully cloned.

When the hook is called, `default-directory' is let-bound to the
directory where the repository has been cloned."
  :package-version '(magit . "4.0.0")
  :group 'magit-commands
  :type 'hook)

;;; Commands

;;;###autoload (autoload 'majjik-clone "majjik-clone" nil t)
(transient-define-prefix majjik-clone (&optional transient)
  "Clone a repository."
  :man-page "jj-clone"
  ; ["Fetch arguments"
  ;  ("-B" "Clone a single branch"  "--single-branch")
  ;  ("-n" "Do not clone tags"      "--no-tags")
  ;  ("-S" "Clones submodules"      "--recurse-submodules" :level 6)
  ;  ("-l" "Do not optimize"        "--no-local" :level 7)]
  ["Setup arguments"
   ("--colocate" "Whether or not to colocate the Jujutsu repo with the git repo" "--colocate")]
   ; ("-o" "Set name of remote"     ("-o" "--origin="))
   ; ("-b" "Set HEAD branch"        ("-b" "--branch="))
  ;  (magit-clone:--filter
  ;   :if (lambda () (magit-git-version>= "2.17.0"))
  ;   :level 7)
  ;  ("-g" "Separate git directory" "--separate-git-dir="
  ;   transient-read-directory :level 7)
  ;  ("-t" "Use template directory" "--template="
  ;   transient-read-existing-directory :level 6)]
  ; ["Local sharing arguments"
  ;  ("-s" "Share objects"          ("-s" "--shared" :level 7))
  ;  ("-h" "Do not use hardlinks"   "--no-hardlinks")]
  ["Clone"
   ("C" "regular"            majjik-clone-regular)
   ; ("s" "shallow"            magit-clone-shallow)
   ; ("d" "shallow since date" magit-clone-shallow-since :level 7)
   ; ("e" "shallow excluding"  magit-clone-shallow-exclude :level 7)
   ; (">" "sparse checkout"    magit-clone-sparse
   ;  :if (lambda () (magit-git-version>= "2.25.0"))
   ;  :level 6)
   ; ("b" "bare"               magit-clone-bare)
   ; ("m" "mirror"             magit-clone-mirror)
  ]
  (interactive (list (or magit-clone-always-transient current-prefix-arg)))
  (if transient
      (transient-setup 'magit-clone)
    (call-interactively #'majjik-clone-regular)))

; (transient-define-argument magit-clone:--filter ()
;   :description "Filter some objects"
;   :class 'transient-option
;   :key "-f"
;   :argument "--filter="
;   :reader #'magit-clone-read-filter)

; (defun magit-clone-read-filter (prompt initial-input history)
;   (magit-completing-read prompt
;                          (list "blob:none" "tree:0")
;                          nil nil initial-input history))

;;;###autoload
(defun magit-clone-regular (repository directory args)
  "Create a clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository."
  (interactive (magit-clone-read-args))
  (magit-clone-internal repository directory args))

(defun magit-clone-internal (repository directory args &optional sparse)
  (let* ((checkout (not (member (car args) '("--bare" "--mirror"))))
         (remote (or (transient-arg-value "--origin=" args)
                     (magit-get "clone.defaultRemote")
                     "origin"))
         (set-push-default
          (and checkout
               (or (eq  magit-clone-set-remote.pushDefault t)
                   (and magit-clone-set-remote.pushDefault
                        (y-or-n-p (format "Set `remote.pushDefault' to %S? "
                                          remote)))))))
    (run-hooks 'magit-credential-hook)
    (setq directory (file-name-as-directory (expand-file-name directory)))
    (when (file-exists-p directory)
      (if (file-directory-p directory)
          (when (length> (directory-files directory) 2)
            (let ((name (magit-clone--url-to-name repository)))
              (unless (and name
                           (setq directory (file-name-as-directory
                                            (expand-file-name name directory)))
                           (not (file-exists-p directory)))
                (user-error "%s already exists" directory))))
        (user-error "%s already exists and is not a directory" directory)))
    (magit-run-git-async "clone" args "--" repository
                         (magit-convert-filename-for-git directory))
    ;; Don't refresh the buffer we're calling from.
    (process-put magit-this-process 'inhibit-refresh t)
    (set-process-sentinel
     magit-this-process
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (let ((magit-process-raise-error t))
           (magit-process-sentinel process event)))
       (when (and (eq (process-status process) 'exit)
                  (= (process-exit-status process) 0))
         (when checkout
           (let ((default-directory directory))
             (when set-push-default
               (setf (magit-get "remote.pushDefault") remote))
             (unless magit-clone-set-remote-head
               (magit-remote-unset-head remote))))
         (when (and sparse checkout)
           (when (magit-git-version< "2.25.0")
             (user-error
              "`git sparse-checkout' not available until Git v2.25"))
           (let ((default-directory directory))
             (magit-call-git "sparse-checkout" "init" "--cone")
             (magit-call-git "checkout" (magit-get-current-branch))))
         (let ((default-directory directory))
           (run-hooks 'magit-post-clone-hook))
         (with-current-buffer (process-get process 'command-buf)
           (magit-status-setup-buffer directory)))))))

(defun magit-clone-read-args ()
  (let ((repo (magit-clone-read-repository)))
    (list repo
          (read-directory-name
           "Clone to: "
           (if (functionp magit-clone-default-directory)
               (funcall magit-clone-default-directory repo)
             magit-clone-default-directory)
           nil nil
           (magit-clone--url-to-name repo))
          (transient-args 'magit-clone))))

(defun magit-clone-read-repository ()
  (magit-read-char-case "Clone from " nil
    (?u "[u]rl or name"
        (let ((str (magit-read-string-ns "Clone from url or name")))
          (if (string-match-p "\\(://\\|@\\)" str)
              str
            (magit-clone--name-to-url str))))
    (?p "[p]ath"
        (magit-convert-filename-for-git
         (read-directory-name "Clone repository: ")))
    (?l "[l]ocal url"
        (concat "file://"
                (magit-convert-filename-for-git
                 (read-directory-name "Clone repository: file://"))))
    (?b "or [b]undle"
        (magit-convert-filename-for-git
         (read-file-name "Clone from bundle: ")))))

(defun magit-clone--url-to-name (url)
  (and (string-match "\\([^/:]+?\\)\\(/?\\.git\\)?$" url)
       (match-string 1 url)))

(defun magit-clone--name-to-url (name)
  (or (seq-some
       (pcase-lambda (`(,re ,host ,user))
         (and (string-match re name)
              (let ((repo (match-string 1 name)))
                (magit-clone--format-url host user repo))))
       magit-clone-name-alist)
      (user-error "Not an url and no matching entry in `%s'"
                  'magit-clone-name-alist)))

(defun magit-clone--format-url (host user repo)
  (if-let ((url-format
            (cond ((listp magit-clone-url-format)
                   (cdr (or (assoc host magit-clone-url-format)
                            (assoc t magit-clone-url-format))))
                  ((stringp magit-clone-url-format)
                   magit-clone-url-format))))
      (format-spec
       url-format
       `((?h . ,host)
         (?n . ,(if (string-search "/" repo)
                    repo
                  (if (string-search "." user)
                      (if-let ((user (magit-get user)))
                          (concat user "/" repo)
                        (user-error "Set %S or specify owner explicitly" user))
                    (concat user "/" repo))))))
    (user-error
     "Bogus `magit-clone-url-format' (bad type or missing default)")))

;;; _
(provide 'majjik-clone)
;;; majjik-clone.el ends here
