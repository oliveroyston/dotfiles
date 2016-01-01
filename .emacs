(require 'cl)
(require 'package)

;; Configure melpa
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; set default font in initial window and for any new window
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

(defvar required-packages
  '(
    ace-jump-mode
    ace-jump-zap
    ace-window
    async
    avy
    aurora-theme
    auto-complete
    base16-theme
    browse-kill-ring
    caps-lock
    chess
    circe
    dash
    diminish
    elfeed
    epl
    epoch-view
    evil
    evil-leader
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
    rainbow-delimiters
    rainbow-mode
    restclient
    reveal-in-osx-finder
    robe
    scratch
    shell-pop
    smart-mode-line-powerline-theme
    smart-mode-line
    spaceline
    twittering-mode
    ubuntu-theme
    ujelly-theme
    undo-tree
    vi-tilde-fringe
    virtualenvwrapper
    volatile-highlights
    warm-night-theme
    web-mode
    which-key
    whitespace-cleanup-mode
    xkcd
    yaml-mode
    yasnippet
    zenburn-theme
  ) "a list of packages to ensure are installed at launch.")

; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(setq user-full-name "Oliver Oyston")
(setq user-mail-address "")

;;-------------------------------
;; UI enhancements:
;;-------------------------------

(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(unless (eq system-type 'darwin)
  (menu-bar-mode -1)
)
(scroll-bar-mode -1)

;;(blink-cursor-mode -1)

;; Don't show a scratch message.
(setq initial-scratch-message nil)

;; Don't show the welcome screen / messages.
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "oliveroyston")

;; Have line numbers globally.
;;(global-linum-mode t)

;;(add-hook 'prog-mode-hook 'linum-mode)

(require 'helm-config)
(require 'helm)

(global-set-key (kbd "M-x") 'helm-M-x)

(helm-mode 1)

(require 'which-key)
(which-key-mode)

(require 'browse-kill-ring)

(projectile-global-mode)

;; Show line-number in the mode line.
(line-number-mode 1)

;; Show column-number in the mode line.
(column-number-mode 1)

;; Enable mouse wheel scrolling.
(mouse-wheel-mode t)

;; Prevent emacs from making backup files - use git instead.
(setq make-backup-files nil)

;; Treat new buffers as text.
(setq default-major-mode 'text-mode)

;; Don't indent with tabs.
(setq-default indent-tabs-mode nil)

;; Highlight the current line.
;;(global-hl-line-mode 1)


;;(setq linum-format "%4d \u2502 ")
;;(setq linum-format " %2d")

(add-hook 'prog-mode-hook
    (lambda()
;;        (hl-line-mode 1)
        (linum-mode 1)
        (setq indicate-empty-lines t)))

(add-hook 'org-mode-hook
    (lambda()
        (linum-mode 1)
        (setq indicate-empty-lines t)))


;; Show empty lines.
;;(setq-default indicate-empty-lines 1)

;;(setq-default indicate-empty-lines t)
;;(when (not indicate-empty-lines)
;;  (toggle-indicate-empty-lines))

;; Use a vi-style tilde for empty lines.
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))

;; Set the tilde color.
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)

;; Set the initial major mode.
(setq initial-major-mode 'org-mode)

;; Just use y/n instead of the more annoying yes/no prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x g") 'magit-status)


(setq echo-keystrokes 0.01
      use-dialog-box nil
      visible-bell t)

(setq show-paren-delay 0)
(show-paren-mode t)

;; Highlight initial paren when on the closing paren (rather than just after it).
(defadvice show-paren-function 
  (around show-paren-closing-before
          activate compile)
  (if (eq (syntax-class (syntax-after (point))) 5)
      (save-excursion
        (forward-char)
        ad-do-it)
    ad-do-it))

;;(set-face-background 'show-paren-match-face "#aaaaaa")
;;(set-face-attribute 'show-paren-match-face nil 
;;                    :weight 'bold :underline nil :overline nil :slant 'normal)

;;(require 'paren)
;;    (set-face-background 'show-paren-match (face-background 'default))
;;    (set-face-foreground 'show-paren-match "#def")
;;    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Enable some disabled commands.
(put 'upcase-region 'disabled nil)    ;; same as M-u but on whole regions C-x C-u
(put 'downcase-region 'disabled nil)  ;; same as M-l but on whole regions C-x C-l

(when (require 'volatile-highlights nil 'noerror)
    (volatile-highlights-mode t))

(delete-selection-mode 1)

(setq-default truncate-lines t)

;; Workaround / fix for problems with some modeline separators (e.g. powerline).
(setq ns-use-srgb-colorspace nil)

;; Load preferred theme.
(load-theme 'monokai t)
;;(load-theme 'base16-default-dark t)


;; These two lines you really need.
;;(setq sml/theme 'powerline)

;;(setq sml/no-confirm-load-theme t)
;;(setq sml/theme 'dark)
;;(sml/setup)

(require 'spaceline-config)
(spaceline-emacs-theme)

;;(require 'powerline)
;;(powerline-default-theme)


(set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))

(global-set-key (kbd "M-p") 'ace-window)

(require 'ace-jump-mode)
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'page-break-lines)
(global-page-break-lines-mode)


(ac-config-default)

(browse-kill-ring-default-keybindings)

(require 'shell-pop)

'(shell-pop-default-directory "/Users/oliveroyston")
'(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
'(shell-pop-term-shell "/bin/bash")
'(shell-pop-window-size 50)
'(shell-pop-full-span t)
'(shell-pop-window-position "bottom")

(global-set-key [f7] 'shell-pop)

(ac-linum-workaround)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)  ;; optional

;; initial window
(setq initial-frame-alist
      '(
        (width . 120) ; character
        (height . 56) ; lines
        ))

;; default/sebsequent window
(setq default-frame-alist
      '(
        (width . 100) ; character
        (height . 52) ; lines
        ))

;; Workaround for show-paren-mode messing with the line number colors!
(custom-set-faces '(linum ((t (:inherit default :foreground "#75715E"  :weight bold)))))

;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
 (set-frame-parameter (selected-frame) 'alpha '(98 98))
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
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;(add-to-list 'display-buffer-alist
;;                    `(,(rx bos "*helm" (* not-newline) "*" eos)
;;                         (display-buffer-in-side-window)
;;                         (inhibit-same-window . t)
;;                         (window-height . 0.4)))

(require 'elfeed)
(global-set-key (kbd "C-x w") 'elfeed)


(setq elfeed-feeds
      '("http://feeds.bbci.co.uk/news/rss.xml"
        "http://www.theregister.co.uk/headlines.atom"
        "https://www.schneier.com/blog/atom.xml"))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(global-set-key (kbd "C-x p i") 'org-cliplink)

(global-set-key [(control x) (k)] 'kill-this-buffer)

;; Added for a better experience on Windows where the default directory was the emacs bin folder.
(setq default-directory "~/")


;;(defun oli-spotify ()
;;  "wrapper for calling spotify from keyboard shortcut and removing possibility for error"
;;  (interactive)
;;  (setq debug-on-error t)
;;  (helm-spotify)
;;  (setq debug-on-error nil))
;;(global-set-key (kbd "C-x M-s") 'oli-spotify)

(winner-mode 1)

(server-start)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(require 'helm-descbinds)
(helm-descbinds-mode)


;; I want an easy command for opening new shells:
(defun new-shell (name)
  "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory and changes the
prompt to 'name>'."
  (interactive "sName: ")
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer)) 
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
(global-set-key (kbd "C-c s") 'new-shell)

;; Disable the arrow keys!
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

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-c\ \C-r" 'recentf-open-files)

(global-undo-tree-mode)

;; Hide my 'standard' minor from the mode line
(diminish 'projectile-mode)
(diminish 'helm-mode)
(diminish 'which-key-mode)
(diminish 'volatile-highlights-mode)
(diminish 'auto-complete-mode)
(diminish 'page-break-lines-mode)
(diminish 'undo-tree-mode)

(message "GNU Emacs")
