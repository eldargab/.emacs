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
    auto-complete
    idris-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

;; Evil mode & editing

(require 'evil)
(require 'ace-jump-mode)

(evil-mode 1)

(defun evil-visual-update-x-selection (&optional buffer) 
  "Don't do this!"
  nil)

(setq ace-jump-mode-scope 'window)

;; Escaping
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-normal-state-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-insert-state-map (kbd "<escape>") 'evil-force-normal-state)
(global-set-key (kbd "s-x") 'evil-force-normal-state)
(global-set-key (kbd "s-s") 'my-save)
(defun my-save ()
  (interactive)
  (save-buffer)
  (evil-force-normal-state))

;; Non-kill-ring deletion
(evil-define-operator my-delete (beg end type)
  (evil-delete beg end type ?_))

(evil-define-operator my-delete-line (beg end type)
  :motion nil
  :keep-visual t
  (evil-delete-line beg end type ?_))

(evil-define-operator my-delete-char (beg end type)
  :motion evil-forward-char
  (evil-delete beg end type ?_))

(evil-define-operator my-change (beg end type)
  (evil-change beg end type ?_))

(defun my-backward-kill ()
  (interactive)
  (let ((p (point))
        (b (progn (skip-chars-backward " \t\n") (point))))
    (if (> p b)
        (delete-region b p)
      (delete-region p (progn (backward-word) (point))))))

;; Better movement

(defun forward-evil-word (&optional count)
  (let ((init-point (point)))
    (forward-word (or count 1))
    (if (= (point) init-point)
        count 0)))

(defun forward-evil-WORD (&optional count)
  (let ((init-point (point)))
    (forward-symbol (or count 1))
    (if (= (point) init-point)
        count 0)))

(evil-define-command my-go-back ()
  :keep-visual t
  :repeat nil
  :type exclusive
  (interactive)
  (evil-goto-mark ?`))

(defun my-new-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun my-new-line-above ()
  (interactive)
  (previous-line)
  (move-end-of-line nil)
  (newline-and-indent))

(defun my-paste ()
  (interactive)
  (let ((text (current-kill 0)))
    (when text
      (if (string-suffix-p "\n" text)
          (progn
            (my-new-line)
            (insert-for-yank (substring text 0 -1)))
        (insert-for-yank text)))))

;; Mappings
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "f") 'ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "t") 'ace-jump-line-mode)
(define-key evil-normal-state-map (kbd "<backspace>") 'my-go-back)
(define-key evil-normal-state-map (kbd "<M-backspace>") 'evil-jump-forward)
(define-key evil-normal-state-map (kbd "U") 'undo-tree-redo)

(define-key evil-normal-state-map (kbd "j") 'evil-forward-char)
(define-key evil-visual-state-map (kbd "j") 'evil-forward-char)
(define-key evil-normal-state-map (kbd "l") 'next-line)
(define-key evil-visual-state-map (kbd "l") 'next-line)

(define-key evil-normal-state-map (kbd "d") 'my-delete)
(define-key evil-normal-state-map (kbd "D") 'my-delete-line)
(define-key evil-normal-state-map (kbd "x") 'my-delete-char)
(define-key evil-normal-state-map (kbd "X") 'evil-delete)
(define-key evil-normal-state-map (kbd "c") 'my-change)
(global-set-key (kbd "<s-backspace>") 'my-backward-kill)

(define-key evil-normal-state-map (kbd "n") 'my-new-line)
(define-key evil-normal-state-map (kbd "N") 'my-new-line-above)
(global-set-key (kbd "<M-return>") 'my-new-line)
(global-set-key (kbd "<M-C-return>") 'my-new-line-above)

(define-key evil-normal-state-map (kbd "s") 'my-paste)

;; Pretty printing

(defun my-indent ()
  (interactive)
  (if (region-active-p)
      (indent-region (region-beginning) (region-end))
    (indent-region 0 (buffer-size))))

(defun my-indent-line ()
  (interactive)
  (indent-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "s-p") 'my-indent-line)
(define-key evil-normal-state-map (kbd "p") 'my-indent)

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

(global-set-key (kbd "s-/") 'my-toggle-comment)

;; Autocomplete

(require 'auto-complete-config)

(ac-config-default)

(setq ac-auto-show-menu nil)
(setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-buffer))


;; eval, compilation and stuf
(global-set-key (kbd "<f5>") 'eshell)
(setq k-eval (kbd "<s-return>"))
(setq k-eval-file (kbd "<f8>"))
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
(define-key emacs-lisp-mode-map k-eval-file 'eval-buffer)
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
             (define-key slime-mode-map k-eval-file 'slime-eval-buffer)
             (define-key slime-mode-map k-docs 'slime-documentation)
             (define-key slime-mode-map k-jump-to-definition 'slime-edit-definition)
             (define-key slime-mode-map k-jump-back 'slime-pop-find-definition-stack)))


;; Proof General
(load-file (concat user-emacs-directory "ProofGeneral/generic/proof-site.el"))

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
             (set (make-local-variable 'electric-indent-chars) '(?\n ?| ?.))
             (define-key coq-mode-map k-eval 'my-move-proof-to-point)
             (define-key coq-mode-map (kbd "<s-M-return>") 'my-proof-go-back)
             (define-key coq-mode-map k-eval-file 'coq-Compile)
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
             (define-key idris-mode-map k-eval-file 'idris-load-file)
             (define-key idris-mode-map k-docs 'idris-docs-at-point)
             (define-key idris-mode-map k-apropos 'idris-apropos)
             ))
