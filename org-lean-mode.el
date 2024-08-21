;;; org-lean-mode.el --- Major mode for working with literate Org Lean files
;;; -*- lexical-binding: t

;;; Commentary:

;;; A Major polymode for editing Lean code embedded in Org files
;;; Copyright (c) 2024 Simon Beaumont.  All Rights Reserved.
;;; Experimental.

;;; Code:

(require 'polymode)
(require 'lean4-mode)
(require 'org)

(defgroup org-lean-mode nil
  "Some org-lean-mode customisations."
  :group 'languages)

(define-hostmode poly-org-lean-hostmode
  :mode 'org-mode
  :keep-in-mode 'host)


;; REs for org-code fences

(rx-define code-start
  (seq (zero-or-more blank)
       "#+begin_"
       (zero-or-more alpha)
       (zero-or-more blank)
       "lean4"))

(rx-define code-end
  (seq (zero-or-more blank)
       "#+end_src"))

(define-innermode poly-org-lean-innermode
  :mode 'lean4-mode
  :head-matcher (rx code-start)
  :tail-matcher (rx code-end)
  :head-mode 'org-mode
  :tail-mode 'org-mode)

(define-polymode org-lean-mode
  :hostmode 'poly-org-lean-hostmode
  :innermodes '(poly-org-lean-innermode))

;;;###autoload
;;(add-to-list 'auto-mode-alist '("\\.lean.org" . org-lean-mode))

(provide 'org-lean-mode)
;;; org-lean-mode.el ends here
