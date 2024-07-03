# org-lean-mode
WIP: An Experimental Org/Lean4 Polymode
----------------------
Which enables `lean4-mode` (tested with `eglot` version, see below) in `org` source blocks.
N.B. does not use/need babel or an ob-lean for lean execution as that is taken care of in `lean4-mode` and LSP integration. The hope is that this can generate all types of literate Lean via say pandoc or org-export.
```
#+begin_src lean
def add1 (x : Nat) :=
  x + 1
#check add1
#+end_src
```
Depends on: 
- polymode (with patch) : [https://github.com/sebeaumont/polymode/tree/seb/move-vars]
- lean4-mode (tested with this fork) : [https://github.com/mekeor/emacs-lean4.git] 
- org (shipped with emacs - but watch this space also for upcoming latex processing improvements)

Tested: emacs 29.1 with `works for me` software assurance. YMMV.

TODO: 
- The source/block/file lines are not lining up. Lean is told the whole file is the source code so we need to
  create a temporary file and set the buffer to that when switching to lean "tangling" all the blocks in the file. 
- make source block tags customizable
- remove debug messages (once I think of something useful to do in mode switch hooks)
- test with lsp-mode (not too likely atm)
  
