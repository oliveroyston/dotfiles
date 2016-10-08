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
    avy-zap
    auto-complete
    base16-theme
    browse-kill-ring
    bm
    caps-lock
    chess
    circe
    color-theme-sanityinc-tomorrow
    counsel
    dash
    diminish
    elfeed
    emmet-mode
    epl
    epoch-view
    evil
    evil-leader
    expand-region
    f
    fill-column-indicator
    fireplace
    flycheck
    git-commit
    git-timemachine
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
    ivy
    jedi
    json-mode
    leuven-theme
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
    s
    scratch
    shell-pop
    smart-mode-line-powerline-theme
    smart-mode-line
    smartparens
    spaceline
    swiper
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
(setq default-directory "~/")

;;-----------------------------------------------;;
;; UI / behaviour enhancements                   ;;
;;-----------------------------------------------;;

;; Turn off annoying sounds.
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; If running in a GUI, hide scrollbar/toolbar and enable mousewheel.
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (mouse-wheel-mode t)))

;; Hide the menu bar, unless on OS X
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

;; Don't blink the cursor.
(blink-cursor-mode -1)

;; Set a distinct cursor color.
;;(set-cursor-color "yellow") 

;; Empty scratch message
(setq initial-scratch-message nil)

;; Don't show the splash screen.
(setq inhibit-splash-screen t)

;; Show a custom message after startup (in my case, nothing)
(defun display-startup-echo-area-message () (message ""))

;; Display line numbers in the mode line.
(line-number-mode 1)

;; Display column numbers in the mode line.
(column-number-mode 1)

;; Show size indication in mode line.
(size-indication-mode t)

(setq initial-frame-alist '((width . 120) (height . 52)))
(setq default-frame-alist '((width . 120) (height . 52)))

;; Use ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Kill buffer without being questioned.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Start with an ORG buffer
(setq initial-major-mode 'org-mode)

;; Can't be bothered to type more than necessary.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Echo multi-key commands immediately in the echo area
(setq echo-keystrokes 0.01)

;; Avoid GUI popup messages.
(setq use-dialog-box nil)

(setq default-major-mode 'text-mode)

;; Don't use tabs for indentation.
(setq-default indent-tabs-mode nil)

;; Prettify symbols.
(global-prettify-symbols-mode 1)

;; Follow sybolic links to version contolled files.
(setq vc-follow-symlinks t)

; UTF-8
(setq locale-coding-system 'utf-8) 
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Don't break lines.
(setq-default truncate-lines t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;(desktop-save-mode 1)

;;-----------------------------------------------;;
;; Global Keybindings                            ;;
;;-----------------------------------------------;;

;; Have undo/redo on familiar keybindings.
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-)") 'text-scale-adjust)

(setq sentence-end-double-space nil)

;;-----------------------------------------------;;
;; Uniquify                                      ;;
;;-----------------------------------------------;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;-----------------------------------------------;;
;; Backups                                       ;;
;;-----------------------------------------------;;

(setq make-backup-files nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;-----------------------------------------------;;
;; MAC OS Customizations                         ;;
;;-----------------------------------------------;;

(defun prelude-swap-meta-and-super ()
  "Swap the mapping of Meta and Super. Very useful for people using their Mac with a
   Windows external keyboard from time to time."
  (interactive)
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (message "Command is now bound to META and Option is bound to SUPER."))
    (progn
      (setq mac-command-modifier 'super)
      (setq mac-option-modifier 'meta)
      (message "Command is now bound to SUPER and Option is bound to META."))))

(if (eq system-type 'darwin)

  (prelude-swap-meta-and-super)
  (global-set-key (kbd "C-c w") 'prelude-swap-meta-and-super)
)

;;-----------------------------------------------;;
;; Theming                                       ;;
;;-----------------------------------------------;;

(load-theme 'monokai t)

(set-face-foreground 'font-lock-comment-face "grey45")
(set-face-foreground 'font-lock-comment-delimiter-face "grey45")

;; Change the selected region color to make it more obvious.
(set-face-attribute 'region nil :background "#8b0000")

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
;; Org Mode / Org Mode Extensions                ;;
;;-----------------------------------------------;;

(global-set-key (kbd "C-x p i") 'org-cliplink)

;;-----------------------------------------------;;
;; Helm / Helm Extensions                        ;;
;;-----------------------------------------------;;

(defvar *helm-enabled* nil)

(when *helm-enabled*
  (require 'helm-config)
  (require 'helm)
  
  (global-set-key (kbd "M-x") 'helm-M-x)
  
  (global-set-key (kbd "M-x") 'undefined)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  
  (helm-mode 1)
  
  (defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
    (if (file-directory-p (helm-get-selection))
        (apply orig-fun args)
      (helm-maybe-exit-minibuffer)))
  
  (advice-add 'helm-execute-persistent-action :around #'fu/helm-find-files-navigate-forward)
  (define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)
  
  (defun fu/helm-find-files-navigate-back (orig-fun &rest args)
    (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
        (helm-find-files-up-one-level 1)
      (apply orig-fun args)))
  
  (advice-add 'helm-ff-delete-char-backward :around #'fu/helm-find-files-navigate-back)
  (define-key helm-map (kbd "<backspace>") 'helm-ff-delete-char-backward)
  
  ;;(require 'helm-descbinds)
  
  ;;(helm-descbinds-mode)
  
  (defun oli/spotify ()
    "wrapper for calling spotify from keyboard shortcut and removing possibility for error"
    (interactive)
    (setq debug-on-error t)
    (helm-spotify)
    (setq debug-on-error nil))
  
  ;;(global-set-key (kbd "C-x M-s") 'oli/spotify)
  )

;;-----------------------------------------------;;
;; Ivy                                           ;;
;;-----------------------------------------------;;

(defvar *ivy-enabled* t)

(when *ivy-enabled*
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-load-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char))

;;-----------------------------------------------;;
;; Folding                                       ;;
;;-----------------------------------------------;;

(require 'vimish-fold)
;;(global-set-key (kbd "<menu> v f") #'vimish-fold)
;;(global-set-key (kbd "<menu> v v") #'vimish-fold-delete)

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

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'org-mode-hook  'linum-mode)

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

;; Highlight initial paren when on the closing paren (rather than just after it).
(defadvice show-paren-function 
  (around show-paren-closing-before
          activate compile)
  (if (eq (syntax-class (syntax-after (point))) 5)
      (save-excursion
        (forward-char)
        ad-do-it)
    ad-do-it))

;; Workaround for show-paren-mode messing with the line number colors!
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit default :foreground "#75715E" :weight bold)))))

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

;;-----------------------------------------------;;
;; Mode line custmomization                      ;;
;;-----------------------------------------------;;

(setq ns-use-srgb-colorspace nil)

(require 'spaceline-config)

(spaceline-emacs-theme)

(require 'avy-zap)

;;-----------------------------------------------;;
;; Ace Jump / Ace Window                         ;;
;;-----------------------------------------------;;

(require 'ace-jump-mode)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "M-p") 'ace-window)

;;-----------------------------------------------;;
;; Avy                                           ;;
;;-----------------------------------------------;;

(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)
(avy-setup-default)

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
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

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
;; Bookmarks (bm)                                ;;
;;-----------------------------------------------;;

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

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
;; Disable the arrow keys / shift selection      ;;
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

(setq shift-select-mode nil)

;;-----------------------------------------------;;
;; Recent files                                  ;;
;;-----------------------------------------------;;

(require 'recentf)

(recentf-mode 1)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 25)

(global-set-key (kbd "C-c C-r") 'recentf-open-files)

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
(diminish 'yas-minor-mode)
(diminish 'ivy-mode)

;;-----------------------------------------------;;
;; Paredit                                       ;;
;;-----------------------------------------------;;

(defvar *paredit-enabled* nil) ;; Disable for now, as I find it pretty annoying!

(when *paredit-enabled*
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

;;-----------------------------------------------;;
;; Winner Mode                                   ;;
;;-----------------------------------------------;;

(winner-mode 1)

;;-----------------------------------------------;;
;; Emacs Server                                  ;;
;;-----------------------------------------------;;

(server-start)

;;-----------------------------------------------;;
;; HTML Bookmarks                                ;;
;;-----------------------------------------------;;

(require 's)
(require 'f)

(defvar oli/bookmark-data
  (--map (s-split "|" it)  (s-split "\n" (f-read "~/data.txt") t)))

(defvar oli/bookmark-source
      `((name . "Bookmark Manager")
        (candidates . ,(mapcar 'car oli/bookmark-data))
        (action . (lambda (candidate)
                    (browse-url (format "%s" (car (cdr (assoc candidate oli/bookmark-data)))))))))

(defun oli/bookmarks ()
    "Allow my HTML bookmarks to be opened / searched via helm."
    (interactive)
    (helm :sources '(oli/bookmark-source)))

;;-----------------------------------------------;;
;; Mark Current Word                             ;;
;;-----------------------------------------------;;

(defun my-mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (region-active-p)))
      (set-mark
       (save-excursion
         (when (< (mark) (point))
           (setq arg (- arg)))
         (goto-char (mark))
         (forward-word arg)
         (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
        (error "No word at point"))
      (if (>= arg 0)
          (goto-char (car wbounds))
        (goto-char (cdr wbounds)))
      (push-mark (save-excursion
                   (forward-word arg)
                   (point)))
      (activate-mark))))

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
          ("=>"     . 8658) ; ⇒
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

