<h1 align="center">It's Majjik</h1>
<h2 align="center">Jujutsu within Emacs</h2>

***

Majjik.el is a [jujutsu][jj] interface within [Emacs].
It is a fork of [Magit] (but this branch shares none of the history).

Eventually, I hope Majjik will be a complete jj interface, but for now
it's just my public elisp project. Please be patient.

## Project Status

- [x] Fork Magit
- [x] Realise you've dramatically underestimated Magit's scope
- [x] start from scratch after a year hiatus
- [ ] Add jj's main commands
  - [x] `jj git init`
  - [x] `jj git fetch/push`
  - [ ] `jj git remote`
  - [x] `jj undo/redo`
  - [x] `jj new/edit`
  - [x] `jj desc`
  - [x] `jj abandon`
  - [x] `jj git init`
  - [x] `jj bookmark *`
    - [x] `create`
    - [x] `move`
    - [x] `set`
    - [x] `rename`
    - [x] `delete`
    - [x] `forget`
    - [x] `track`
    - [x] `untrack`
  - [x] `jj file *`
    - [x] `track`
    - [x] `untrack`
    - [ ] `chmod`
    - [ ] `annotate`
  - [x] `jj squash`
  - [ ] `jj split`
  - [ ] `jj rebase`
  - [ ] `jj restore`
  - [ ] `jj op *`
  - [ ] `jj evolog`
  - [ ] `jj diffedit`
  - [ ] `jj interdiff`
  - [ ] `jj parallelize`
  - [ ] `jj next/prev`
  - [ ] `jj resolve`
  - [ ] `jj revert`
  - [ ] `jj simplify-parents`
- [ ] Add uncommon commands
  - [ ] `jj absorb`
  - [ ] `jj fix`
  - [ ] `jj metaedit`
  - [ ] `jj sign/unsign`
  - [ ] `jj sparse`
  - [ ] `jj tag`
  - [ ] `jj util *`
  - [ ] `jj workspace`
- [ ] Add a snazzy dashboard buffer
  - [ ] Add proper collapsible sections for summary, log
  - [ ] Allow expanding elided log segments
  - [ ] RET to inspect from dashboard
    - [x] revisions - show revision
    - [ ] files
      - [x] tracked - show file diff
      - [x] untracked - view file
      - [ ] conflicted - ??
    - [ ] bookmarks?
- [ ] Make commands transient where it makes sense
- [x] Dogfood it

## Getting Started
Currently, this requires a minor fork of jj to run: https://github.com/Zoybean/jj/tree/list-untracked. I intend to merge a PR for this.

## Acknowledgments

Majjik was started by [Zoey Hewll][zoey], initially as a fork of Magit (but I've ended up starting entirely from scratch).

[zoey]:  https://github.com/Zoybean
[jj]:    https://docs.jj-vcs.dev/
[emacs]: https://www.gnu.org/software/emacs
[magit]: https://magit.vc
