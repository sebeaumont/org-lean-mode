;;; org-lean-mode.el --- Major mode for working with literate Org Lean files
;;; -*- lexical-binding: t

;;; Commentary:

;; A Major polymode for editing Lean code embedded in Org files
;; Experimental 

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

(define-innermode poly-org-lean-innermode
  :mode 'lean4-mode
  :head-matcher "#\\+begin_src lean"
  :tail-matcher "#\\+end_src"
  ;; Keep the code block wrappers in Org mode, so they can be folded, etc.
  :head-mode 'org-mode
  :tail-mode 'org-mode
                             
  ;; Disable font-lock-mode 
  ;; and undo the change to indent-line-function Polymode makes.
  :init-functions
  '((lambda (_) (font-lock-mode 0))
    (lambda (_) (setq indent-line-function #'indent-relative))))

(define-polymode org-lean-mode
  :hostmode 'poly-org-lean-hostmode
  :innermodes '(poly-org-lean-innermode)
  (setq-local org-src-fontify-natively t)
  (setq-local polymode-after-switch-buffer-hook
              (append '(after-switch-hook) polymode-after-switch-buffer-hook)))

(defun after-switch-hook (_ new)
  "The after buffer switch hook run with NEW buffer."
  (when (bufferp new)
    (let ((new-mode (buffer-local-value 'major-mode new)))
      ;;(message "switch to: %s" new-mode)
      (cond ((eq new-mode 'lean4-mode) (lean-mode-hook new))
            ((eq new-mode 'org-mode) (org-mode-hook new))))))

(defun org-mode-hook (buf)
  "Hook run after entering `org-mode` with BUF."
  (font-lock-update)
  (if (buffer-modified-p buf)
      (message "dirty-org")
    (message "clean-org")))

(defun lean-mode-hook (buf)
  "Hook run after entering `lean4-mode` with BUF."
  (font-lock-update)
  (if (buffer-modified-p buf)
      (message "dirty-lean")
    (message "clean-lean")))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lean.org" . org-lean-mode))

(provide 'org-lean-mode)
;;; org-lean-mode.el ends here
