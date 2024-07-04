# org-lean-mode
WIP: An Experimental Org/Lean4 Polymode
----------------------
Which enables `lean4-mode` (tested with `eglot` version, see below) in
`org` source blocks.  N.B. does not use/need babel or an ob-lean for
lean execution as that is taken care of in `lean4-mode` and LSP
integration. The hope is that this can generate all types of literate
Lean via say pandoc or org-export as well.

Because lean4-mode is quite clever and evaluates the buffer as you
type (live-coding) and places any output in the `*Lean Goal*` buffer
this requires lean4-mode to work on a seperate buffer (indeed file)
which is a sparse representation of the org-file (to preserve line
numbers and point for LSP) all a bit tricky but maybe not impossible.

Watch this space.

```
#+begin_src lean
def add1 (x : Nat) :=
  x + 1
#check add1
#+end_src
```

Warning this is pre-alpha and may not work at all.

Depends on: 
- polymode
- lean4-mode (tested with this fork) : [https://github.com/mekeor/emacs-lean4.git] 
- org 

TODO: 
- decide how to do this properly
- make source block tags customizable
- test with lsp-mode (not too likely atm)
  
