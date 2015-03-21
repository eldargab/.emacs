(if (not (getenv "TERM_PROGRAM"))
    (let ((path (shell-command-to-string
                 "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
      (setenv "PATH" path)))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(defvar my-packages
  '(slime
    smart-tab
    idris-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(ido-mode t)

(setq ido-enable-flex-matching t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(require 'smart-tab)
(global-smart-tab-mode 1)
(setq dabbrev-check-all-buffers nil)

(show-paren-mode 1)
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil)
(setq cursor-type 'bar
      line-spacing 2
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      require-final-newline t
      ring-bell-function 'ignore
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; customize scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-step 1)

(setq inferior-lisp-program "/usr/local/bin/ccl")

(load-file (concat user-emacs-directory "ProofGeneral/generic/proof-site.el"))

(setq coq-prog-name "/usr/local/bin/coqtop")

;; Navigation
(global-set-key (kbd "<C-tab>") 'ido-switch-buffer)
(global-set-key (kbd "<C-s-tab>") 'ibuffer)

;; (define-key ido-completion-map "\t" nil)
(global-set-key (kbd "C-q") 'keyboard-escape-quit)
(global-set-key (kbd "s-o") 'ido-find-file)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") '(lambda ()
                               (interactive)
                               (split-window-right)
                               (windmove-right)))
(global-set-key (kbd "s-3") '(lambda ()
                               (interactive)
                               (split-window-below)
                               (windmove-down)))
(global-set-key [s-left] 'windmove-left) 
(global-set-key [s-right] 'windmove-right) 
(global-set-key [s-up] 'windmove-up) 
(global-set-key [s-down] 'windmove-down)

(when (fboundp 'winner-mode)
  (winner-mode 1))

(global-set-key (kbd "M-q") 'winner-undo)
(global-set-key (kbd "s-M-q") 'winner-redo)

;; commenting
(defun my-toggle-comment ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))

(global-set-key (kbd "s-/") 'my-toggle-comment)

;; search
(global-set-key (kbd "s-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "s-f") 'isearch-repeat-forward)

;; eval, compilation and stuf
(global-set-key (kbd "<f5>") 'eshell)

(setq k-eval (kbd "<s-return>"))
(setq k-compile (kbd "<f8>"))
(setq k-docs (kbd "<f4>"))
(setq k-jump-to-definition (kbd "<double-mouse-1>"))
(setq k-jump-back (kbd "<s-double-mouse-1>>"))

(defmacro case-sel (no-sel sel)
  `(lambda ()
     (interactive)
     (if
         (region-active-p)
         ,sel
       ,no-sel)))

(global-set-key (kbd "s-p") '(lambda ()
                               (interactive)
                               (indent-region 0 (buffer-size))))

;; elisp
(define-key emacs-lisp-mode-map k-eval (case-sel (eval-defun nil) (eval-region
                                                                   (region-beginning)
                                                                   (region-end)
                                                                   t)))
(define-key emacs-lisp-mode-map k-compile 'eval-buffer)
(define-key emacs-lisp-mode-map k-jump-to-definition 'find-function-at-point)

;; Proof General
(defun move-proof-to-point ()
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

(add-hook 'coq-mode-hook
          '(lambda ()
             (define-key coq-mode-map k-eval 'move-proof-to-point)
             (define-key coq-mode-map (kbd "<M-s-return>") 'proof-undo-last-successful-command)
             (define-key coq-mode-map k-compile 'coq-Compile)
             (define-key coq-mode-map k-jump-to-definition '(lambda ()
                                                              (interactive)
                                                              (execute-kbd-macro
                                                               (kbd "M-x coq-Print RET RET"))))
             (define-key coq-mode-map k-docs '(lambda ()
                                                (interactive)
                                                (execute-kbd-macro
                                                 (kbd "M-x coq-Check RET RET"))))))

(setq proof-follow-mode 'followdown)
(setq proof-splash-enable nil)

;; Slime
(add-hook 'slime-mode-hook
          '(lambda ()
             (define-key slime-mode-map k-eval (case-sel
                                                (slime-eval-defun)
                                                (slime-eval-region
                                                 (region-beginning)
                                                 (region-end))))
             (define-key slime-mode-map k-compile 'slime-eval-buffer)
             (define-key slime-mode-map k-docs 'slime-documentation)
             (define-key slime-mode-map k-jump-to-definition 'slime-edit-definition)
             (define-key slime-mode-map k-jump-back 'slime-pop-find-definition-stack)))

(load-file (concat user-emacs-directory "eldar-theme.el"))

;; IDRIS
(setq idris-interpreter-path "/Users/eldar/Library/Haskell/bin/idris")

(add-hook 'idris-mode-hook
          '(lambda ()
             (define-key idris-mode-map k-compile 'idris-load-file)
             (define-key idris-mode-map k-docs 'idris-docs-at-point)
             (define-key idris-mode-map (kbd "<s-f1>") 'idris-apropos)
             ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(cursor-type (quote bar)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
