;;; org-lean-mode.el --- Major mode for working with literate Org Lean files
;;; -*- lexical-binding: t

;;; Commentary:

;; A Major polymode for editing Lean code embedded in Org files
;; Copyright (c) 2024 Simon Beaumont. All Rights Reserved.

;; Experimental not for distribution

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

;; TODO
;; investigate some org-mode minor-modes in the lean4 mode which dont make sense
;; e.g. indentation is ok, but prettyentites are confusing.
;; be simpified here as this was a carry over from Agda IIRC.

;; derived REs

;; these are tricky due to way lean4 mode re-interprets underscores as unicode
;; subscript intros -- we need to provide these as alternatives to "_src"

(rx-define code-start
  (seq (zero-or-more "--")
       (zero-or-more blank)
       "#+begin_"
       (one-or-more alpha)
       (one-or-more blank)
       "lean"))

(rx-define code-end
  (seq (zero-or-more "--")
       (zero-or-more blank)
       "#+end"))


;; TODO ensure transfer of buffer-file-truename to lean4-mode

(define-innermode poly-org-lean-innermode
  :mode 'lean4-mode
  :head-matcher (rx code-start)
  :tail-matcher (rx code-end)
  
  ;; Keep the code block fences in Org mode, so they can be folded, etc.
  :head-mode 'org-mode
  :tail-mode 'org-mode
  :init-functions
  '((lambda (_) (font-lock-mode 0))
    (lambda (_) (setq indent-line-function #'indent-relative))
    )
  )

;; mainly setup hooks...

(define-polymode org-lean-mode
  :hostmode 'poly-org-lean-hostmode
  :innermodes '(poly-org-lean-innermode)
  (setq-local org-src-fontify-natively t)
  (setq-local polymode-after-switch-buffer-hook
              (append '(olm/after-switch-hook)
                      polymode-after-switch-buffer-hook))
  (setq-local polymode-before-switch-buffer-hook
              (append '(olm/before-switch-hook)
                      polymode-before-switch-buffer-hook)))

;;; TODO combine these round trip functions?

(defun olm/org-to-lean (src dst)
  (with-current-buffer src
    (save-excursion
      (goto-char (point-min))
      (let ((inchunkp nil))
        (olm/erase-that-buffer dst)
        (while (not (eobp))
          (let ((line (buffer-substring
                       (point)
                       (progn (forward-line 1) (point)))))
            ;; for each line in buffer
            (cond
             ;; code ends fence
             ((string-match-p (rx code-end) line)
              (setq inchunkp nil)
              (olm/write-buffer dst (concat "-- " line)))

             ;; code starts fence
             ((string-match-p (rx code-start) line) 
              (setq inchunkp 't)
              (olm/write-buffer dst (concat "-- " line)))

             ;; code
             (inchunkp (olm/write-buffer dst line))
             
             ;; blank lines
             ((string-blank-p line) (olm/write-buffer dst line))
             
             ;; make text into comments
             (t (olm/write-buffer dst (concat "-- " line))))))))))

(defun olm/lean-to-org (src dst)
  (with-current-buffer src
    (save-excursion
      (goto-char (point-min))
      (let ((inchunkp nil))
        (olm/erase-that-buffer dst)
        (while (not (eobp))
          (let ((line (buffer-substring
                       (point)
                       (progn (forward-line 1) (point)))))
            (cond
             ;; code end fence
             ((string-match-p (rx code-end) line)
              (setq inchunkp nil)
              (olm/write-buffer dst (string-trim-left line "-- ")))
             ;; fence start
             ((string-match-p (rx code-start) line)
              (setq inchunkp 't)
              (olm/write-buffer dst (string-trim-left line "-- ")))
             ;; code as is
             (inchunkp (olm/write-buffer dst line))
             ;; blank lines as is 
             ((string-blank-p line) (olm/write-buffer dst line))
             ;; uncomment text
             (t (olm/write-buffer dst (string-trim-left line "-- "))))))))))


(defun olm/overwrite-buffer (src dst)
  (with-current-buffer dst
    (replace-buffer-contents src)))

(defun olm/erase-that-buffer (buf)
  (with-current-buffer buf
    (erase-buffer)))

(defun olm/write-buffer (buf data)
  (with-current-buffer buf
    (goto-char (point-max))
    (insert data)))


;;; hooks...

(defvar scratch-buffer "*org-lean-scratch*")

(defun olm/before-switch-hook (old new)
  (message "switching from: %s to: %s" old new)
  (let ((scratch-buf (get-buffer-create scratch-buffer)))
    (cond
     ;; switch to lean4 buffer
     ((eq (buffer-local-value 'major-mode new) 'lean4-mode)
      (olm/org-to-lean old scratch-buf)
      (olm/overwrite-buffer scratch-buf new)
      )
     ;; switch to org-mode
     ((eq (buffer-local-value 'major-mode new) 'org-mode)
      (olm/lean-to-org old scratch-buf)
      (olm/overwrite-buffer scratch-buf new)
     )
     
    )))


;;; after swtich hooks...

(defun olm/after-switch-hook (_ new)
  "The after buffer switch hook run with NEW buffer."
  (when (bufferp new)
    (let ((new-mode (buffer-local-value 'major-mode new)))
      ;;(message "after switch to: %s" new-mode)
      (cond ((eq new-mode 'lean4-mode) (olm/lean-mode-hook new))
            ((eq new-mode 'org-mode) (olm/org-mode-hook new))))))

(defun olm/org-mode-hook (buf)
  "Hook run after entering `org-mode` with BUF."
  (font-lock-update)
  (if (buffer-modified-p buf)
      (message "dirty-org")
    (message "clean-org")))

(defun olm/lean-mode-hook (buf)
  "Hook run after entering `lean4-mode` with BUF."
  (font-lock-update)
  (if (buffer-modified-p buf)
      (message "dirty-lean")
    (message "clean-lean")))



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lean.org" . org-lean-mode))

(provide 'org-lean-mode)
;;; org-lean-mode.el ends here
