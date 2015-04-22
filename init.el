(if (not (getenv "TERM_PROGRAM"))
    (let ((path (shell-command-to-string
                 "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
      (setenv "PATH" path)
      (setq exec-path (append exec-path (split-string path ":")))))

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(defvar my-packages
  '(slime
    smart-tab
    smartparens
    idris-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(show-paren-mode 1)
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'bar)
(setq-default line-spacing 2)

(setq inhibit-startup-screen t
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      require-final-newline t
      ring-bell-function 'ignore
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups")))
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-step 1)

(load-file (concat user-emacs-directory "eldar-theme.el"))

;; Ido setup
(ido-mode t)
(setq ido-ignore-extensions t)
(setq ido-enable-flex-matching t)

(add-to-list 'ido-ignore-buffers "\\`*")
(add-to-list 'ido-ignore-buffers "\.gz")
(add-to-list 'ido-ignore-buffers "\.v\.d")

(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)
(defun ensure-buffer-name-ends-in-slash ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat name "/") t))))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(require 'smart-tab)
(global-smart-tab-mode 1)

(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-strict-mode)
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-z"))

;; Windows and navigation

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(when (fboundp 'winner-mode)
  (winner-mode 1))

(setq dabbrev-check-all-buffers nil)

(defun my-split-window-right ()
  (interactive) 
  (split-window-right)
  (windmove-right))

(defun my-split-window-below ()
  (interactive)
  (split-window-below)
  (windmove-down))

(global-set-key (kbd "M-q") 'winner-undo)
(global-set-key (kbd "s-M-q") 'winner-redo)
(global-set-key (kbd "<s-left>") 'windmove-left) 
(global-set-key (kbd "<s-right>") 'windmove-right) 
(global-set-key (kbd "<s-up>") 'windmove-up) 
(global-set-key (kbd "<s-down>") 'windmove-down)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'my-split-window-right)
(global-set-key (kbd "s-3") 'my-split-window-below)


(global-set-key (kbd "s-o") 'ido-find-file)
(global-set-key (kbd "<C-tab>") 'ido-switch-buffer)
(global-set-key (kbd "<C-s-tab>") 'ibuffer)

(global-set-key (kbd "s-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
(define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<s-return>") 'isearch-exit)

;; Pretty printing

(defun my-indent ()
  (interactive)
  (if (region-active-p)
      (indent-region (region-beginning) (region-end))
    (indent-region (line-beginning-position) (line-end-position))))

(defun my-indent-file ()
  (interactive)
  (indent-region 0 (buffer-size)))

(global-set-key (kbd "s-p") 'my-indent)
(global-set-key (kbd "s-M-π") 'my-indent-file)

;; General editing commands

(defun my-new-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun my-new-line-above ()
  (interactive)
  (previous-line)
  (move-end-of-line nil)
  (newline-and-indent))

(defun my-kill-whole-line ()
  (interactive)
  (kill-whole-line)
  (indent-according-to-mode))

(defun my-join-line ()
  (interactive)
  (delete-indentation 1))

(defun my-backward-kill ()
  (interactive)
  (let ((p (point))
        (b (progn (skip-chars-backward " \t\n") (point))))
    (if (> p b)
        (kill-region b p)
      (kill-region p (progn (backward-word) (point))))))

(global-set-key (kbd "<M-return>") 'my-new-line)
(global-set-key (kbd "<M-C-return>") 'my-new-line-above)
(global-set-key (kbd "s-k") 'my-kill-whole-line)
(global-set-key (kbd "s-j") 'my-join-line)
(global-set-key (kbd "<s-backspace>") 'my-backward-kill)

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

;; eval, compilation and stuf
(global-set-key (kbd "<f5>") 'eshell)
(setq k-eval (kbd "<s-return>"))
(setq k-compile (kbd "<f8>"))
(setq k-docs (kbd "<f4>"))
(setq k-jump-to-definition (kbd "<double-mouse-1>"))
(setq k-jump-back (kbd "<s-double-mouse-1>>"))
(setq k-apropos (kbd "<s-f1>"))


;; elisp
(defun my-elisp-eval ()
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end) t)
    (eval-defun nil)))

(define-key emacs-lisp-mode-map k-eval 'my-elisp-eval)
(define-key emacs-lisp-mode-map k-compile 'eval-buffer)
(define-key emacs-lisp-mode-map k-jump-to-definition 'find-function-at-point)

;; Slime
(setq inferior-lisp-program "ccl")

(defun my-slime-eval ()
  (interactive)
  (if (region-active-p)
      (slime-eval-region (region-beginning) (region-end) t)
    (slime-eval-defun nil)))

(add-hook 'slime-mode-hook
          '(lambda ()
             (define-key slime-mode-map k-eval 'my-slime-eval)
             (define-key slime-mode-map k-compile 'slime-eval-buffer)
             (define-key slime-mode-map k-docs 'slime-documentation)
             (define-key slime-mode-map k-jump-to-definition 'slime-edit-definition)
             (define-key slime-mode-map k-jump-back 'slime-pop-find-definition-stack)))


;; Proof General
(load-file (concat user-emacs-directory "ProofGeneral/generic/proof-site.el"))

(add-to-list 'completion-ignored-extensions ".v.d")

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

(defun my-coq-jump-to-definition ()
  (interactive)
  (execute-kbd-macro (kbd "M-x coq-Print RET RET")))

(defun my-coq-docs ()
  (interactive)
  
  (execute-kbd-macro (kbd "M-x coq-Check RET RET")))

(add-hook 'coq-mode-hook
          '(lambda ()
             (set (make-local-variable 'electric-indent-chars) '(?\n ?| ?.))
             (define-key coq-mode-map k-eval 'move-proof-to-point)
             (define-key coq-mode-map (kbd "<s-C-return>") 'proof-undo-last-successful-command)
             (define-key coq-mode-map k-compile 'coq-Compile)
             (define-key coq-mode-map k-jump-to-definition 'my-coq-jump-to-definition)
             (define-key coq-mode-map k-docs 'my-coq-docs)
             (define-key coq-mode-map k-apropos 'coq-SearchAbout)
             (define-key coq-mode-map (kbd "M-p") 'coq-Print)
             (define-key coq-mode-map (kbd "M-c") 'coq-Check)
             (define-key coq-mode-map (kbd "M-l") 'proof-layout-windows)
             ))

(setq proof-follow-mode 'followdown)
(setq proof-splash-enable nil)

;; IDRIS
(setq-default idris-packages '("effects" "contrib"))

(add-to-list 'completion-ignored-extensions ".ibc")

(add-hook 'idris-mode-hook
          '(lambda ()
             (define-key idris-mode-map k-compile 'idris-load-file)
             (define-key idris-mode-map k-docs 'idris-docs-at-point)
             (define-key idris-mode-map k-apropos 'idris-apropos)
             ))
