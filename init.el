(if (not (getenv "TERM_PROGRAM"))
    (let ((path (shell-command-to-string
                 "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
      (setenv "PATH" path)
      (setq exec-path (append exec-path (split-string path ":")))))


(setq lib (concat user-emacs-directory "packages/"))

(add-to-list 'load-path lib)
(setq custom-theme-directory lib)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(setq pkg-list-not-refreshed t)

(defun use-pkg (name)
  (when (not (package-installed-p name))
    (when pkg-list-not-refreshed
      (package-refresh-contents)
      (setq pkg-list-not-refreshed nil))
    (package-install name)))

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
      mouse-drag-copy-region nil
      x-select-enable-clipboard nil
      double-click-fuzz 6
      scroll-step 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(show-paren-mode 1)
(electric-pair-mode)
(transient-mark-mode 0)
(global-auto-revert-mode t)

(load-theme 'eldar t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Ido setup

(ido-mode t)
(setq ido-ignore-extensions t)
(setq ido-enable-flex-matching t)

(add-to-list 'ido-ignore-buffers "\\`*")
(add-to-list 'ido-ignore-buffers "\.gz")
(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)

(defun ensure-buffer-name-ends-in-slash ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat name "/") t))))

;; Windows and files

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(winner-mode 1)

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
(global-set-key (kbd "<C-M-s-left>") 'windmove-left)
(global-set-key (kbd "<C-M-s-right>") 'windmove-right)
(global-set-key (kbd "<C-M-s-up>") 'windmove-up)
(global-set-key (kbd "<C-M-s-down>") 'windmove-down)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'my-split-window-right)
(global-set-key (kbd "s-3") 'my-split-window-below)

(global-set-key (kbd "s-o") 'ido-find-file)
(global-set-key (kbd "<C-tab>") 'ido-switch-buffer)
(global-set-key (kbd "<C-s-tab>") 'ibuffer)

;; Isearch

(add-hook 'isearch-mode-hook 'my-isearch-update-mode)

(defun my-isearch-update-mode ()
  (setq my-isearch-forward isearch-forward))

(defun my-isearch-repeat ()
  (interactive)
  (isearch-repeat (if my-isearch-forward
                      'forward
                    'backward)))

(defun my-isearch-repeat-backward ()
  (interactive)
  (let ((mode my-isearch-forward))
    (isearch-repeat (if mode
                        'backward
                      'forward))
    (setq my-isearch-forward mode)))

(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
(define-key isearch-mode-map (kbd "<return>") 'my-isearch-repeat)
(define-key isearch-mode-map (kbd "<M-return>") 'my-isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<s-return>") 'isearch-exit)

;; Text navigation - selection
(use-pkg 'ace-jump-mode)
(require 'ace-jump-mode)
(require 'view)

(setq ace-jump-mode-scope 'window)

(global-set-key (kbd "s-l") 'ace-jump-word-mode)

(global-set-key (kbd "<s-backspace>") 'pop-to-mark-command)

(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)

;; Editing

(defun my-backward-delete ()
  (interactive)
  (let ((p (point))
        (b (progn (skip-chars-backward " \t\n") (point))))
    (if (> p b)
        (delete-region b p)
      (delete-region p (progn (backward-word) (point))))))

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

(global-set-key (kbd "s-k") 'my-kill-whole-line)
(global-set-key (kbd "s-j") 'my-join-line)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete)
(global-set-key (kbd "<s-return>") 'my-new-line)
(global-set-key (kbd "<s-M-return>") 'my-new-line-above)

;; Pretty printing

(defun my-indent ()
  (interactive)
  (if (region-active-p)
      (indent-region (region-beginning) (region-end))
    (indent-region 0 (buffer-size))))

(defun my-indent-line ()
  (interactive)
  (indent-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "s-P") 'my-indent-line)
(global-set-key (kbd "s-p") 'my-indent)

;; Commenting

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

(global-set-key (kbd "s-`") 'my-toggle-comment)

;; Autocomplete
(use-pkg 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)
(setq ac-auto-show-menu nil)
(setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-buffer))

(global-set-key (kbd "s-/") 'auto-complete)


;; eval, compilation and stuf

(defun noop ())

(global-set-key (kbd "<f5>") 'eshell)

(setq-default f-eval 'noop)
(setq-default f-eval-file 'noop)
(setq-default f-docs 'noop)
(setq-default f-jump-to-definition 'noop)
(setq-default f-apropos 'noop)

(defun my-eval () (interactive) (funcall f-eval))
(defun my-eval-file () (interactive) (funcall f-eval-file))
(defun my-docs () (interactive) (funcall f-docs))
(defun my-jump-to-definition () (interactive) (funcall f-jump-to-definition))
(defun my-apropos () (interactive) (funcall f-apropos))


(global-set-key (kbd "<M-return>") 'my-eval)
(global-set-key (kbd "<f8>") 'my-eval-file)
(global-set-key (kbd "<f4>") 'my-docs)
(global-set-key (kbd "<double-mouse-1>") 'my-jump-to-definition)
(global-set-key (kbd "<s-f1>") 'my-apropos)

;; elisp
(defun my-elisp-eval ()
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end) t)
    (eval-defun nil)))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (setq-local f-eval 'my-elisp-eval)
             (setq-local f-eval-file 'eval-buffer)
             (setq-local f-jump-to-definition 'find-function-at-point)))

(require 'proof-general)
