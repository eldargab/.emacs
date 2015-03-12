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
    smart-tab))

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
(global-set-key (kbd "<s-M-tab>") 'ibuffer)

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
(defun toggle-comment ()
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

(global-set-key (kbd "s-/") 'toggle-comment)

;; search
(global-set-key (kbd "s-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "s-f") 'isearch-repeat-forward)

(global-set-key (kbd "s-[") 'indent-region)

;; eval, compilation and stuf
(define-key emacs-lisp-mode-map (kbd "<s-return>") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "<f8>") 'eval-buffer)

(add-hook 'coq-mode-hook
          '(lambda ()
             (define-key coq-mode-map (kbd "<s-return>") 'move-proof-to-point)
             (define-key coq-mode-map (kbd "<f8>") 'coq-Compile)
             ))

(setq proof-follow-mode 'followdown)
(defun move-proof-to-point ()
  (interactive)
  (if (> (proof-queue-or-locked-end) (point))
      (save-excursion
        (proof-retract-until-point))
    (progn
      (save-excursion
        (move-end-of-line nil)
        (proof-assert-until-point))
      (proof-maybe-follow-locked-end)
      (next-line))))

(setq proof-splash-enable nil)

(load-file (concat user-emacs-directory "eldar-theme.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
