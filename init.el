;; Emacs General configuration

(setq inhibit-startup-message t)        ; Disables the startup message of Emacs
(scroll-bar-mode -1)                    ; Disables the scroll bar on the right


(menu-bar-mode -1)                      ; Removes the menu bar
(tool-bar-mode -1)                      ; Removes the tool bar
(tooltip-mode -1)                       ; Removed the tooltip
(set-fringe-mode 10)                    ; Adds some padding around the text area for editing

(setq visible-bell t)                   ; Removed sound for a visible bell.

;; Font settings

					; No I don't have a high DPI resolution screen, I'm just completely blind.
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)

;; Package Management and sources

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

;; Key bindings 

(use-package general
  :config
  (general-create-definer sda/leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Escape quit prompts

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Minibuffer completion with Ivy and Swiper

					; Swiper
(use-package swiper :ensure t)

					; Ivy 
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

(use-package ivy-rich
  :init (ivy-rich-mode 1))

					; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

;; Aestetics
(use-package all-the-icons)
					; Theme with Doom Theme <3
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

					; Modeline with Doom Modeline <3
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 35)))

					; Line numbers


(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode nil))))

					; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Documentation

					; Which key
(use-package which-key
  :init (which-key-mode)
  :diminish (which-key-mode)
  :config (setq which-key-idle-delay 0.7))


;; Configuration of menus
(sda/leader-def
  "f"  '(:ignore t :which-key "Files")
  "fs" 'save-buffer)

;; One keys map.

(sda/leader-def
  "p" '(projectile-command-map :which-key "Projects")
  "w"  '(evil-window-map :which-key "Windows")
  "," '(switch-to-buffer :which-key "Switch buffers in Workspace") ; Need to add workspaces
  "<" '(switch-to-buffer :which-key "Switch buffers")
  "SPC" '(projectile-find-file :which-key "Find file in project")
  "." '(find-file :which-key "Find file"))
  
;; Projects

;; Projectile

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/Workspace")
    (setq projectile-project-search-path '(("~/Workspace/" . 3))))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
