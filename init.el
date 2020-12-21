(setq inhibit-startup-message t) ; Removes Emacs's default menu bar

(scroll-bar-mode -1)             ; Removes the scroll bar
(tool-bar-mode -1)               ; Removes the toolbar
(tooltip-mode -1)                ; Removes the tooltip
(set-fringe-mode 1)             ; Removes the black bars on buffer limits 
(menu-bar-mode -1)               ; Disable menu bar
(setq visible-bell t)            ; Disable sound bell
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 140)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )

(defun compile-project ()
  (interactive)
  (let* ((make-directory (locate-dominating-file (buffer-file-name)
                                                  "Makefile"))
          (command (concat "make -k -C "
                           (shell-quote-argument make-directory))))
  (compile command)))

(global-set-key (kbd "<f5>") 'compile-project)

(add-hook 'sh-mode-hook (lambda ()
                          (setq tab-width 4)))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq backup-by-copying t)
(prefer-coding-system 'utf-8)
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
(delete-selection-mode)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

(use-package fira-code-mode
  :demand
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-major-mode-color-icon 1)
	   (doom-modeline-display-default-persp-name t)
	   (doom-modeline-lsp t)
	   (doom-modeline-github t)
	   (doom-modeline-github-interval (* 30 60))
	   (doom-modeline-height 10)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-w" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)
  (yas-reload-all)
  :diminish yas-minor-mode)

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-city-lights t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(add-hook 'c-mode-hook 'development-mode)

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-l")
  :hook ((c-mode . lsp)
         (lsp-enable-which-key-integration)
         (lsp-enable-snippet t)
         (lsp-enable-indentation t))
  :commands lsp)
;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package company
  :after lsp-mode
  :pin melpa
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(setq company-tooltip-maximum-width 70)

(use-package flycheck)

(use-package smart-tab)

(use-package company-box
  :hook (company-mode . company-box-mode))

(defun development-mode
    (interactive)
  (setq c-basic-offset 4
        c-label-offset 4
        tab-width 4
        c-default-style "linux")
  (lsp t)
  (company-mode t)
  (company-box-mode t)
  (setq show-trailing-whitespace t)
  (setq indent-tabs-mode nil))

(setq yas-after-exit-snippet-hook
      (lambda ()
	(lsp-format-region yas-snippet-beg yas-snippet-end)))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(put 'scroll-left 'disabled nil)
