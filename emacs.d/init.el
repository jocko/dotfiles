(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(defun require-package (package &optional min-version no-refresh)
    "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(package-initialize)

(require-package 'solarized-theme)
(load-theme 'solarized-light t)

(setq inhibit-startup-screen t)

(blink-cursor-mode 0)

(defalias 'yes-or-no-p 'y-or-n-p)

; DISABLED getting this weird behaviour when doing c-g when in m-x, the next time i do m-x the text is clipped and offset to the right
(require-package 'linum-relative)
;; Set relative line numbers (like relativenumber in vim)
; (linum-relative-global-mode)
;; Current line displays absolute line number
(setq linum-relative-current-symbol "")

(require-package 'projectile)
(setq projectile-completion-system 'ivy)

(require-package 'counsel)
(ivy-mode 1)
; (global-set-key (kbd "M-x") 'counsel-M-x)

(require-package 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)

; (require-package 'yasnippet)
; (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
; (yas-global-mode 1)

(require-package 'paredit)

(require-package 'clojure-mode)

(require-package 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (projectile-mode)
  (linum-relative-mode)
  (paredit-mode)
  (linum-mode))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

; (require-package 'cider)

(require-package 'evil-leader)
(global-evil-leader-mode 1)

(evil-leader/set-leader "SPC")
(evil-leader/set-key
  ; "b" 'switch-to-buffer
  ; "f" 'find-file
  ; "j" 'projectile-find-tag
  "j" 'evil-avy-goto-word-1-below
  "k" 'evil-avy-goto-word-1-above
  "o" 'projectile-find-file
  "O" 'projectile-find-file-other-window
  "w" 'save-buffer
  "x" 'counsel-M-x)

(require-package 'evil)
(evil-mode t)

; (require-package 'erlang)
; (add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))

; (setq x-select-enable-clipboard t)

; (require-package 'haml-mode)
; (require 'haml-mode)

; (require-package 'ws-butler)

; (require-package 'ag)
; (require 'ag)

; (require-package 'alchemist)
; (require 'alchemist)

; (require-package 'dockerfile-mode)
; (require 'dockerfile-mode)
; (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

; (blink-cursor-mode -1)
; (setq evil-operator-state-cursor '(box "white"))

; (require-package 'clojure-mode)
; (require-package 'cider)

; (set-default 'tags-case-fold-search nil)

; (global-auto-revert-mode t)

; (setq evil-want-C-u-scroll t
;       evil-want-C-w-in-emacs-state t)

; (require-package 'evil)

; (global-unset-key (kbd "C-x m"))
; (global-unset-key (kbd "M-x"))
; (global-set-key (kbd "C-x m") 'execute-extended-command)

; (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
;     (when (fboundp mode) (funcall mode -1)))

; (setq echo-keystrokes 0.1)

; (defalias 'yes-or-no-p 'y-or-n-p)

; (setq linum-format "%d ")
; (global-linum-mode t)

; (setq make-backup-files nil)
; (setq auto-save-default nil)
; (setq scroll-margin 5)
;       ; scroll-preserve-screen-position 1)

; ; (require-package 'tags-table)
; ; (require 'tags-table)

; (setq tags-revert-without-query 1)

; (require-package 'ctags-update)
; (require 'ctags-update)
; (add-hook 'ruby-mode-hook  'turn-on-ctags-auto-update-mode)
; ; (ctags-update-minor-mode 1)

; (require 'uniquify)
; (setq uniquify-buffer-name-style 'forward)

; (require-package 'darktooth-theme)
; (load-theme 'darktooth t)

; (setq dired-use-ls-dired nil)

; (defmacro rename-modeline (package-name mode new-name)
;   `(eval-after-load ,package-name
;      '(defadvice ,mode (after rename-modeline activate)
; 	(setq mode-name ,new-name))))
; ; (rename-modeline "clojure-mode" clojure-mode "Clj")

; (defmacro after-load (feature &rest body)
;   "After FEATURE is loaded, evaluate BODY."
;   (declare (indent defun))
;   `(eval-after-load ,feature
;      '(progn ,@body)))

; (require 'saveplace)
;   (setq-default save-place t)

; (global-set-key "\C-w" 'backward-kill-word)

; (setq enh-ruby-check-syntax nil)
; (setq enh-ruby-deep-indent-paren nil)
; (require-package 'enh-ruby-mode)
; (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
; (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
; (add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
; (add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
; (add-to-list 'auto-mode-alist '("\\.pryrc$" . enh-ruby-mode))
; (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
; (defun enh-ruby-mode-faces ()
;   "Lazily set faces"
;   ; (set-face-attribute 'erm-syn-errline nil :box nil)
;   ; (set-face-attribute 'erm-syn-warnline nil :box nil)
;   (set-face-attribute 'enh-ruby-op-face nil :foreground "white" :inherit 'default))
;   ; (set-face-attribute 'enh-ruby-string-delimiter-face nil :foreground "#dc322f" :background nil)
;   ; (set-face-attribute 'enh-ruby-regexp-delimiter-face nil :foreground "#dc322f" :background nil)
;   ; (set-face-attribute 'enh-ruby-heredoc-delimiter-face nil :foreground "#dc322f" :background nil))

; ; (add-hook 'enh-ruby-mode-hook 'bw/turn-on-subword-mode)
; (add-hook 'enh-ruby-mode-hook 'enh-ruby-mode-faces)

; (add-hook 'enh-ruby-mode-hook  'turn-on-ctags-auto-update-mode)
; ; (setq ruby-use-smie nil)
; ; (setq ruby-use-smie t)
; (add-hook 'ruby-mode-hook 'subword-mode)

; (require-package 'ace-jump-mode)

; (require-package 'expand-region)
; (require 'expand-region)
; (require 'ruby-mode-expansions)
; (global-set-key (kbd "C-=") 'er/expand-region)

; (require-package 'flx-ido)
; (require 'flx-ido)

; (require-package 'ido-vertical-mode)
; (require 'ido-vertical-mode)
; (ido-vertical-mode 1)
; (setq ido-enable-flex-matching t)
; (setq ido-everywhere t)
; (ido-mode 1)
; (flx-ido-mode 1)
; (setq ido-enable-flex-matching t)
; (setq ido-use-faces nil)


; (require-package 'projectile)
; (require 'projectile)
; (projectile-global-mode)
; (add-hook 'ruby-mode-hook 'projectile-mode)

; (require-package 'diminish)
; (require 'diminish)
; (after-load 'diminish (diminish 'projectile-mode))
; (after-load 'undo-tree (diminish 'undo-tree-mode))
; (after-load 'ctags-update (diminish 'ctags-auto-update-mode))
; (after-load 'smartparens (diminish 'smartparens-mode))
; (after-load 'evil-commentary (diminish 'evil-commentary-mode))
; (diminish 'abbrev-mode)
; (rename-modeline "enh-ruby-mode" enh-ruby-mode "Ruby")

; (require-package 'yaml-mode)
; (require 'yaml-mode)
; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

; ; (setq evil-snipe-scope 'visible)
; ; (setq evil-snipe-repeat-scope 'whole-visible)
; ; (require-package 'evil-snipe)
; ; (require 'evil-snipe)
; ; (evil-snipe-mode 1)
; ; (evil-snipe-override-mode 1)

; (require-package 'evil-surround)
; (require 'evil-surround)
; (global-evil-surround-mode 1)

; (require-package 'evil-smartparens)
; (require 'evil-smartparens)
; (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
; ; (smartparens-global-mode t)
; (require 'smartparens-config)

; (require-package 'rainbow-delimiters)
; (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

; ; (require-package 'evil-matchit)
; ; (require 'evil-matchit)
; ; (global-evil-matchit-mode 1)

; (require-package 'evil-leader)
; (global-evil-leader-mode 1)

; (evil-leader/set-leader "SPC")
; (evil-leader/set-key
;   "b" 'switch-to-buffer
;   "f" 'find-file
;   "j" 'projectile-find-tag
;   "o" 'projectile-find-file
;   "O" 'projectile-find-file-other-window
;   "w" 'save-buffer)

; (require-package 'evil-commentary)
; (evil-commentary-mode)

; (evil-mode t)

; (evil-set-initial-state 'fundamental-mode 'emacs)
; (evil-set-initial-state 'cider-repl-mode 'emacs)
; (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
; (evil-set-initial-state 'dired-mode 'emacs)

; (define-key evil-normal-state-map (kbd "s") 'ace-jump-mode)

