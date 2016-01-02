(require 'cl)
(require 'package)

;;-----------------------------------------------;;
;; Configure MELPA                               ;;
;;-----------------------------------------------;;

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;;-----------------------------------------------;;
;; Ensure that required packages are installed   ;;
;;-----------------------------------------------;;

(defvar required-packages
  '(
    ace-jump-mode
    ace-jump-zap
    ace-window
    async
    avy
    auto-complete
    base16-theme
    browse-kill-ring
    caps-lock
    chess
    circe
    dash
    diminish
    elfeed
    emmet-mode
    epl
    epoch-view
    evil
    evil-leader
    expand-region
    fill-column-indicator
    fireplace
    flycheck
    git-commit
    helm
    helm-circe
    helm-core
    helm-descbinds
    helm-firefox
    helm-google
    helm-itunes
    helm-projectile
    helm-robe
    helm-spotify
    jedi
    linum-relative
    magit
    magit-popup
    markdown-mode
    monokai-theme
    multiple-cursors
    neotree
    nlinum
    nyan-mode
    org-cliplink
    paredit
    page-break-lines
    pkg-info
    powerline
    projectile
    python-mode
    rainbow-delimiters
    rainbow-mode
    restclient
    reveal-in-osx-finder
    robe
    scratch
    shell-pop
    smart-mode-line-powerline-theme
    smart-mode-line
    smartparens
    spaceline
    twittering-mode
    undo-tree
    vi-tilde-fringe
    virtualenvwrapper
    volatile-highlights
    web-mode
    which-key
    whitespace-cleanup-mode
    xkcd
    yaml-mode
    yasnippet
    zenburn-theme
  ) "a list of packages to ensure are installed at launch.")

;; Check packages are installed.
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; Install missing packages.
(unless (packages-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;-----------------------------------------------;;
;; User Information                              ;;
;;-----------------------------------------------;;

(setq user-full-name "Oliver Oyston")
(setq user-mail-address "")

;;-----------------------------------------------;;
;; UI enhancements                               ;;
;;-----------------------------------------------;;

(setq ring-bell-function 'ignore)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (mouse-wheel-mode t)))

(unless (eq system-type 'darwin)
  (menu-bar-mode -1)
)

(blink-cursor-mode -1)

(setq initial-scratch-message nil)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "oliveroyston")

(line-number-mode 1)
(column-number-mode 1)

(setq initial-frame-alist '((width . 120) (height . 52)))
(setq default-frame-alist '((width . 120) (height . 52)))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(autoload 'ibuffer "ibuffer" "List buffers." t)

(global-set-key (kbd "C-x p i") 'org-cliplink)

(global-set-key [(control x) (k)] 'kill-this-buffer)

(setq default-directory "~/")

(setq initial-major-mode 'org-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.01
      use-dialog-box nil
      visible-bell t)

(setq default-major-mode 'text-mode)

(setq-default indent-tabs-mode nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(global-prettify-symbols-mode 1)

;;-----------------------------------------------;;
;; Backups                                       ;;
;;-----------------------------------------------;;

(setq make-backup-files nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;-----------------------------------------------;;
;; Theming                                       ;;
;;-----------------------------------------------;;

(load-theme 'monokai t)

;;-----------------------------------------------;;
;; Default fonts                                 ;;
;;-----------------------------------------------;;

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))))
 ((string-equal system-type "darwin")     ; Mac OS X
  (when (member "Source Code Pro" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Source Code Pro 12"))
    (add-to-list 'default-frame-alist '(font . "Source Code Pro 12"))))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))))

;;-----------------------------------------------;;
;; Helm / Helm Extensions                        ;;
;;-----------------------------------------------;;

(require 'helm-config)
(require 'helm)

(global-set-key (kbd "M-x") 'helm-M-x)

(helm-mode 1)

(require 'helm-descbinds)

(helm-descbinds-mode)

(defun oli/spotify ()
  "wrapper for calling spotify from keyboard shortcut and removing possibility for error"
  (interactive)
  (setq debug-on-error t)
  (helm-spotify)
  (setq debug-on-error nil))

;;(global-set-key (kbd "C-x M-s") 'oli/spotify)

;;-----------------------------------------------;;
;; Which Key                                     ;;
;;-----------------------------------------------;;

(require 'which-key)

(which-key-mode)

;;-----------------------------------------------;;
;; Browse Kill Ring                              ;;
;;-----------------------------------------------;;

(require 'browse-kill-ring)

(browse-kill-ring-default-keybindings)

;;-----------------------------------------------;;
;; Projectile                                    ;;
;;-----------------------------------------------;;

(projectile-global-mode)

;;-----------------------------------------------;;
;; Line Numbering                                ;;
;;-----------------------------------------------;;

;;(global-linum-mode t)
;;(add-hook 'prog-mode-hook 'linum-mode)

;;(setq linum-format "%4d \u2502 ")
;;(setq linum-format " %2d")

(add-hook 'prog-mode-hook
    (lambda()
        (linum-mode 1)
        ;;(setq indicate-empty-lines t)
        ))

(add-hook 'org-mode-hook
    (lambda()
        (linum-mode 1)
        ;;(setq indicate-empty-lines t)
        ))

;;-----------------------------------------------;;
;; Vi-style tilde for empty lines                ;;
;;-----------------------------------------------;;

(if (display-graphic-p)
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)))

(if (display-graphic-p)
(progn
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)))

;;-----------------------------------------------;;
;; Magit                                         ;;
;;-----------------------------------------------;;

(global-set-key (kbd "C-x g") 'magit-status)

;;-----------------------------------------------;;
;; Paren Mode                                    ;;
;;-----------------------------------------------;;

(setq show-paren-delay 0)

(show-paren-mode t)

;; Workaround for show-paren-mode messing with the line number colors!
(custom-set-faces '(linum ((t (:inherit default :foreground "#75715E"  :weight bold)))))

;; Highlight initial paren when on the closing paren (rather than just after it).
(defadvice show-paren-function 
  (around show-paren-closing-before
          activate compile)
  (if (eq (syntax-class (syntax-after (point))) 5)
      (save-excursion
        (forward-char)
        ad-do-it)
    ad-do-it))

;;-----------------------------------------------;;
;; Rainbow Delimiters                            ;;
;;-----------------------------------------------;;

(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;-----------------------------------------------;;
;; Enable Disabled Commands                      ;;
;;-----------------------------------------------;;

(put 'upcase-region 'disabled nil)    ;; same as M-u but on whole regions C-x C-u
(put 'downcase-region 'disabled nil)  ;; same as M-l but on whole regions C-x C-l

;;-----------------------------------------------;;
;; Volatile Highlights                           ;;
;;-----------------------------------------------;;

(when (require 'volatile-highlights nil 'noerror)
  (volatile-highlights-mode t))

(delete-selection-mode 1)

(setq-default truncate-lines t)

;;-----------------------------------------------;;
;; Mode line custmomization                      ;;
;;-----------------------------------------------;;

(setq ns-use-srgb-colorspace nil)

(require 'spaceline-config)

(spaceline-emacs-theme)

;;-----------------------------------------------;;
;; Ace Jump / Ace Window                         ;;
;;-----------------------------------------------;;

(require 'ace-jump-mode)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "M-p") 'ace-window)

;;-----------------------------------------------;;
;; Neotree                                       ;;
;;-----------------------------------------------;;

(require 'neotree)

(global-set-key [f8] 'neotree-toggle)

;;-----------------------------------------------;;
;; Page Break Lines                              ;;
;;-----------------------------------------------;;

(require 'page-break-lines)

(global-page-break-lines-mode)

;;-----------------------------------------------;;
;; Snippets                                      ;;
;;-----------------------------------------------;;

(require 'yasnippet)

;;(yas-global-mode 1)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

;;(setq ac-source-yasnippet nil)

;;-----------------------------------------------;;
;; Auto-Complete                                 ;;
;;-----------------------------------------------;;

(ac-config-default)

(ac-linum-workaround)

(defun my/ac-linum-workaround ()
  "linum-mode tries to display the line numbers even for the
completion menu. This workaround stops that annoying behavior."
  (interactive)
  (defadvice linum-update (around ac-linum-update-workaround activate)
    (unless ac-completing
      ad-do-it)))

;;-----------------------------------------------;;
;; Shell Pop                                     ;;
;;-----------------------------------------------;;

(require 'shell-pop)

'(shell-pop-default-directory "/Users/oliveroyston")
'(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
'(shell-pop-term-shell "/bin/bash")
'(shell-pop-window-size 50)
'(shell-pop-full-span t)
'(shell-pop-window-position "bottom")

(global-set-key [f7] 'shell-pop)

;;-----------------------------------------------;;
;; Flycheck                                      ;;
;;-----------------------------------------------;;

(add-hook 'after-init-hook #'global-flycheck-mode)

;;-----------------------------------------------;;
;; Transparency                                  ;;
;;-----------------------------------------------;;

(set-frame-parameter (selected-frame) 'alpha '(98 98)) ;; Active / Inactive
(add-to-list 'default-frame-alist '(alpha 98 98))

(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(98 98))))

(global-set-key (kbd "C-c t") 'toggle-transparency)

;;-----------------------------------------------;;
;; Fullscreen                                    ;;
;;-----------------------------------------------;;

(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)

;;-----------------------------------------------;;
;; Multiple Cursors                              ;;
;;-----------------------------------------------;;

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;-----------------------------------------------;;
;; Elfeed (RSS reader)                           ;;
;;-----------------------------------------------;;

(require 'elfeed)

(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '("http://feeds.bbci.co.uk/news/rss.xml"
        "http://www.theregister.co.uk/headlines.atom"
        "https://www.schneier.com/blog/atom.xml"))

;;-----------------------------------------------;;
;; Easy shell access                             ;;
;;-----------------------------------------------;;

(defun new-shell (name)
  
  "Opens a new shell buffer with the given name in asterisks (*name*) in the
   current directory and changes the prompt to 'name>'."
  
  (interactive "sName: ")
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer)) 
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))

(global-set-key (kbd "C-c s") 'new-shell)

;;-----------------------------------------------;;
;; Disable the arrow keys                        ;;
;;-----------------------------------------------;;

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

;;-----------------------------------------------;;
;; Recent files                                  ;;
;;-----------------------------------------------;;

(require 'recentf)

(recentf-mode 1)
(setq recentf-max-menu-items 25)

(global-set-key "\C-c\ \C-r" 'recentf-open-files)

;;-----------------------------------------------;;
;; Undo Tree                                     ;;
;;-----------------------------------------------;;

(global-undo-tree-mode)

;;-----------------------------------------------;;
;; Expand Region                                 ;;
;;-----------------------------------------------;;

(require 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)

;;-----------------------------------------------;;
;; Fill Column Indicator                         ;;
;;-----------------------------------------------;;

(require 'fill-column-indicator)

(setq fci-rule-color "darkred")
(setq fci-rule-column 100)

(defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

;;-----------------------------------------------;;
;; Diminish                                      ;;
;;-----------------------------------------------;;

(diminish 'projectile-mode)
(diminish 'helm-mode)
(diminish 'which-key-mode)
(diminish 'volatile-highlights-mode)
(diminish 'auto-complete-mode)
(diminish 'page-break-lines-mode)
(diminish 'undo-tree-mode)
(diminish 'flycheck-mode)

;;-----------------------------------------------;;
;; Paredit                                       ;;
;;-----------------------------------------------;;

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;-----------------------------------------------;;
;; Miscellaneous                                 ;;
;;-----------------------------------------------;;

(winner-mode 1)

(server-start)

;;-------------------------------------------------------------------------------------------------;;
;; Python Customizations                                                                           ;;
;;-------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------;;
;; Prettify Symbols                              ;;
;;-----------------------------------------------;;

(defun my-python-prettify-symbols ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955)  ; λ
          ;;("->"     . 8594) ; →
          ;;("=>"     . 8658) ; ⇒
          ;;("map"    . 8614) ; ↦
          )))

(add-hook 'python-mode-hook 'my-python-prettify-symbols)

;;-----------------------------------------------;;
;; Virtualenv Support                            ;;
;;-----------------------------------------------;;

(require 'virtualenvwrapper)

(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "/path/to/your/virtualenvs/")

;;(setq venv-location '("/path/to/project1-env/"
;;                      "/path/to/ptoject2-env/"))

;;-----------------------------------------------;;
;; Jedi                                          ;;
;;-----------------------------------------------;;

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;-------------------------------------------------------------------------------------------------;;
;; Ruby Customizations                                                                             ;;
;;-------------------------------------------------------------------------------------------------;;
;;-----------------------------------------------;;
;; Prettify Symbols                              ;;
;;-----------------------------------------------;;

(defun my-ruby-prettify-symbols ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955)  ; λ
          ;;("->"     . 8594) ; →
          ;;("=>"     . 8658) ; ⇒
          ;;("map"    . 8614) ; ↦
          )))

(add-hook 'ruby-mode-hook 'my-ruby-prettify-symbols)

;;-----------------------------------------------;;
;; Robe                                          ;;
;;-----------------------------------------------;;

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;;-------------------------------------------------------------------------------------------------;;
;; End of initialization                                                                           ;;
;;-------------------------------------------------------------------------------------------------;;

(message "GNU Emacs")
