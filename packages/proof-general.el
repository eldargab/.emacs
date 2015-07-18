
(require 'proof-site "ProofGeneral/generic/proof-site.el")

(add-to-list 'completion-ignored-extensions ".v.d")

(defun my-move-proof-to-point ()
  (interactive)
  (if (> (proof-queue-or-locked-end) (point))
      (save-excursion
        (proof-retract-until-point))
    (progn
      (save-excursion
        (if (proof-only-whitespace-to-locked-region-p)
            (proof-assert-next-command-interactive)
          (proof-assert-until-point)))
      (proof-maybe-follow-locked-end))))

(defun my-proof-go-back ()
  (interactive)
  (proof-undo-last-successful-command)
  (goto-char (proof-unprocessed-begin)))

(defun my-coq-jump-to-definition ()
  (interactive)
  (execute-kbd-macro (kbd "M-x coq-Print RET RET")))

(defun my-coq-docs ()
  (interactive)
  (execute-kbd-macro (kbd "M-x coq-Check RET RET")))

(add-hook 'coq-mode-hook
          '(lambda ()
             (setq-local 'electric-indent-chars '(?\n ?| ?.))
             (setq-local f-eval 'my-move-proof-to-point)
             (setq-local f-jump-to-definition 'my-coq-jump-to-definition)
             (setq-local f-docs 'my-coq-docs)
             (setq-local f-apropos 'coq-SearchAbout)
             (define-key coq-mode-map (kbd "<M-C-return>") 'my-proof-go-back)
             (define-key coq-mode-map (kbd "M-p") 'coq-Print)
             (define-key coq-mode-map (kbd "M-c") 'coq-Check)
             (define-key coq-mode-map (kbd "M-l") 'proof-layout-windows)
             ))

(setq proof-follow-mode 'followdown)
(setq proof-splash-enable nil)
(setq coq-compile-before-require t)

(provide 'proof-general)
