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
this requires lean4-mode to work on a seperate buffer which is a valid
lean4 representation of the org-file (to preserve line numbers and
point).

The hack is to roundtrip lean-to-org and org-to-lean in a scratch
buffer and then update the real buffer with its contents depending
what mode we are currently in. This is a bit disconcerting but it works
for me as the main point is to focus on each at a time and switching
is taken care of by polymode. 

I doubt it scales particularly well using `replace-buffer-contents`,
as it must, to preserve existing markers, overlays, etc. maybe we
could create a custom version of that which is aware of our
roundtripping transformation (which basically consists in commenting
all the non lean code bits and back again).

Having played with this a bit -- the visual distractions are a bit too
intrusive but maybe this can be overcome -- having said that even
markdown fences are acting strange this morning. Maybe I'm just
more sensitive to visual artefacts now. There's lots of weirdness
lurking I'm sure.


    #+begin_src lean
    def add1 (x : Nat) :=
    x + 1
    #check add1
    #+end_src


Depends on: 
- polymode   (currently needs a patch but can be fixed here)
- lean4-mode (tested with this fork) : [https://github.com/mekeor/emacs-lean4.git] 
- org 

TODO: 
- make sure we can use standard polymode by transferring `buffer-file-truename`
- test with lsp-mode (not too likely atm)

