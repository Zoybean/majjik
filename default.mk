TOP := $(dir $(lastword $(MAKEFILE_LIST)))

## User options ######################################################
#
# You can override these settings in "config.mk" or on the command
# line.
#
# You might also want to set LOAD_PATH.  If you do, then it must
# contain "-L .".
#
# If you don't do so, then the default is set in the "Load-Path"
# section below.  The default assumes that all dependencies are
# installed either at "../<DEPENDENCY>", or when using package.el
# at "ELPA_DIR/<DEPENDENCY>-<HIGHEST-VERSION>".

PREFIX   ?= /usr/local
sharedir ?= $(PREFIX)/share
lispdir  ?= $(sharedir)/emacs/site-lisp/majjik
infodir  ?= $(sharedir)/info
docdir   ?= $(sharedir)/doc/majjik

CP       ?= install -p -m 644
MKDIR    ?= install -p -m 755 -d
RMDIR    ?= rm -rf
TAR      ?= tar
SED      ?= sed

EMACS      ?= emacs
EMACS_ARGS ?=
BATCH       = $(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH)

LISP_EXTRA_TARGETS ?= check-declare

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

GITSTATS      ?= gitstats
GITSTATS_DIR  ?= $(TOP)docs/stats
GITSTATS_ARGS ?= -c style=https://magit.vc/assets/stats.css \
                 -c max_authors=180 -c graph_max_authors=7

BUILD_MAJJIK_LIBGIT ?= false

## Files #############################################################

PKG       = majjik
PACKAGES  = majjik majjik-section git-commit

TEXIPAGES = $(addsuffix .texi,$(filter-out git-commit,$(PACKAGES)))
INFOPAGES = $(addsuffix .info,$(filter-out git-commit,$(PACKAGES)))
HTMLFILES = $(addsuffix .html,$(filter-out git-commit,$(PACKAGES)))
HTMLDIRS  = $(filter-out git-commit,$(PACKAGES))
PDFFILES  = $(addsuffix .pdf,$(filter-out git-commit,$(PACKAGES)))
EPUBFILES = $(addsuffix .epub,$(filter-out git-commit,$(PACKAGES)))

ELS  = git-commit.el
ELS += majjik-section.el
ELS += majjik-base.el
ifeq "$(BUILD_MAJJIK_LIBGIT)" "true"
ELS += majjik-libgit.el
endif
ELS += majjik-git.el
ELS += majjik-mode.el
ELS += majjik-margin.el
ELS += majjik-process.el
ELS += majjik-transient.el
ELS += majjik-autorevert.el
ELS += majjik-core.el
ELS += majjik-diff.el
ELS += majjik-log.el
ELS += majjik-wip.el
ELS += majjik-reflog.el
ELS += majjik-apply.el
ELS += majjik-repos.el
ELS += majjik.el
ELS += majjik-status.el
ELS += majjik-refs.el
ELS += majjik-files.el
ELS += majjik-reset.el
ELS += majjik-branch.el
ELS += majjik-merge.el
ELS += majjik-tag.el
ELS += majjik-worktree.el
ELS += majjik-notes.el
ELS += majjik-sequence.el
ELS += majjik-commit.el
ELS += majjik-remote.el
ELS += majjik-clone.el
ELS += majjik-fetch.el
ELS += majjik-pull.el
ELS += majjik-push.el
ELS += majjik-patch.el
ELS += majjik-bisect.el
ELS += majjik-stash.el
ELS += majjik-blame.el
ELS += majjik-sparse-checkout.el
ELS += majjik-submodule.el
ELS += majjik-subtree.el
ELS += majjik-ediff.el
ELS += majjik-gitignore.el
ELS += majjik-bundle.el
ELS += majjik-extras.el
ELS += git-rebase.el
ELS += majjik-bookmark.el
ELCS = $(ELS:.el=.elc)
ELMS = majjik.el $(filter-out $(addsuffix .el,$(PACKAGES)),$(ELS))
ELGS = majjik-autoloads.el majjik-version.el

## Versions ##########################################################

VERSION ?= $(shell \
  test -e $(TOP).git && \
  git describe --tags --abbrev=0 --always | cut -c2-)

COMPAT_VERSION        = 29.1.4.4
DASH_VERSION          = 2.19.1
GIT_COMMIT_VERSION    = $(VERSION)
LIBGIT_VERSION        = 0
MAJJIK_VERSION         = $(VERSION)
MAJJIK_LIBGIT_VERSION  = $(VERSION)
MAJJIK_SECTION_VERSION = $(VERSION)
SEQ_VERSION           = 2.24
TRANSIENT_VERSION     = 0.5.0
WITH_EDITOR_VERSION   = 3.3.2

COMPAT_SNAPSHOT              = $(COMPAT_VERSION)
DASH_MELPA_SNAPSHOT          = 20221013
GIT_COMMIT_MELPA_SNAPSHOT    = 20231030
LIBGIT_MELPA_SNAPSHOT        = 0
MAJJIK_MELPA_SNAPSHOT         = 20231202
MAJJIK_LIBGIT_MELPA_SNAPSHOT  = 20230924
MAJJIK_SECTION_MELPA_SNAPSHOT = 20231202
SEQ_MELPA_SNAPSHOT           = $(SEQ_VERSION)
TRANSIENT_MELPA_SNAPSHOT     = 20231204
WITH_EDITOR_MELPA_SNAPSHOT   = 20230917

DEV_VERSION_SUFFIX = .50-git

EMACS_VERSION        = 25.1
LIBGIT_EMACS_VERSION = 26.1

EMACSOLD := $(shell $(BATCH) --eval \
  "(and (version< emacs-version \"$(EMACS_VERSION)\") (princ \"true\"))")
ifeq "$(EMACSOLD)" "true"
  $(error At least version $(EMACS_VERSION) of Emacs is required)
endif

## Load-Path #########################################################

# Remember to also update majjik-emacs-Q-command!

ifndef LOAD_PATH

USER_EMACS_DIR = $(HOME)/.emacs.d
ifeq "$(wildcard $(USER_EMACS_DIR))" ""
  XDG_CONFIG_DIR = $(or $(XDG_CONFIG_HOME),$(HOME)/.config)
  ifneq "$(wildcard $(XDG_CONFIG_DIR)/emacs)" ""
    USER_EMACS_DIR = $(XDG_CONFIG_DIR)/emacs
  endif
endif

ELPA_DIR ?= $(USER_EMACS_DIR)/elpa

COMPAT_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/compat-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(COMPAT_DIR)" ""
  COMPAT_DIR = $(TOP)../compat
endif

DASH_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/dash-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(DASH_DIR)" ""
  DASH_DIR = $(TOP)../dash
endif

LIBGIT_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/libgit-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(LIBGIT_DIR)" ""
  LIBGIT_DIR = $(TOP)../libgit
endif

SEQ_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/seq-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(SEQ_DIR)" ""
  SEQ_DIR = $(TOP)../seq
endif

TRANSIENT_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/transient-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(TRANSIENT_DIR)" ""
  TRANSIENT_DIR = $(TOP)../transient/lisp
endif

WITH_EDITOR_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/with-editor-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(WITH_EDITOR_DIR)" ""
  WITH_EDITOR_DIR = $(TOP)../with-editor/lisp
endif

MAJJIK_SECTION_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/majjik-section-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)

SYSTYPE := $(shell $(EMACS) -Q --batch --eval "(princ system-type)")
ifeq ($(SYSTYPE), windows-nt)
  CYGPATH := $(shell cygpath --version 2>/dev/null)
endif

LOAD_PATH = -L $(TOP)lisp

# When making changes here, then don't forget to adjust "Makefile",
# ".github/workflows/test.yml", ".github/ISSUE_TEMPLATE/bug_report.md",
# `majjik-emacs-Q-command' and the "Installing from the Git Repository"
# info node accordingly.  Also don't forget to "rgrep \b<pkg>\b".

ifdef CYGPATH
  LOAD_PATH += -L $(shell cygpath --mixed $(COMPAT_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(DASH_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(LIBGIT_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(SEQ_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(TRANSIENT_DIR))
  LOAD_PATH += -L $(shell cygpath --mixed $(WITH_EDITOR_DIR))
  ifneq "$(MAJJIK_SECTION_DIR)" ""
    LOAD_PATH += -L $(shell cygpath --mixed $(MAJJIK_SECTION_DIR))
  endif
else
  LOAD_PATH += -L $(COMPAT_DIR)
  LOAD_PATH += -L $(DASH_DIR)
  LOAD_PATH += -L $(LIBGIT_DIR)
  LOAD_PATH += -L $(SEQ_DIR)
  LOAD_PATH += -L $(TRANSIENT_DIR)
  LOAD_PATH += -L $(WITH_EDITOR_DIR)
  ifneq "$(MAJJIK_SECTION_DIR)" ""
    LOAD_PATH += -L $(MAJJIK_SECTION_DIR)
  endif
endif

endif # ifndef LOAD_PATH

ifndef ORG_LOAD_PATH
ORG_LOAD_PATH = -L ../../org/lisp
endif

## Dependencies ######################################################

# This isn't used by make, but is needed for the Compile ci workflow.

DEPS  = compat
DEPS += dash
DEPS += seq
DEPS += transient/lisp
DEPS += vterm
DEPS += with-editor/lisp

## Publish ###########################################################

DOMAIN      ?= magit.vc
CFRONT_DIST ?= E2LUHBKU1FBV02

PUBLISH_TARGETS ?= html html-dir pdf

DOCBOOK_XSL ?= /usr/share/xml/docbook/stylesheet/docbook-xsl/epub/docbook.xsl

EPUBTRASH = epub.xml META-INF OEBPS
