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

(require-package 'spacemacs-theme)
(require-package 'spacegray-theme)
(require-package 'dracula-theme)
(load-theme 'spacemacs-dark t)
; (load-theme 'spacegray t)
; (load-theme 'dracula t)

(setq inhibit-startup-screen t)
(setq scroll-margin 5)

(blink-cursor-mode 0)

(set-face-attribute 'default nil
		    :family "Hack"
		    :height 120)

(defalias 'yes-or-no-p 'y-or-n-p)

(cd "~/Repos")

(require-package 'spaceline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-toggle-buffer-position-off)
(spaceline-toggle-buffer-encoding-abbrev-off)
(spaceline-toggle-version-control-off)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-line-column-off)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(setq powerline-default-separator 'nil)
(spaceline-compile)

;; Disable tool bar, menu bar, scroll bar and tool tips.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode tooltip-mode))
  (when (fboundp mode) (funcall mode -1)))

(require 'saveplace)
(save-place-mode 1)

;; F5 is mapped to "meta tap" on my keyboard
;; Not needed anymore, meta-tap now maps to Menu
; (global-set-key (kbd "<f5>") 'counsel-M-x)

; DISABLED getting this weird behaviour when doing c-g when in m-x, the next time i do m-x the text is clipped and offset to the right
(require-package 'linum-relative)
;; Set relative line numbers (like relativenumber in vim)
; (linum-relative-global-mode)
;; Current line displays absolute line number
(setq linum-relative-current-symbol "")

; (require-package 'projectile)
; (setq projectile-completion-system 'ivy)

; (global-set-key (kbd "C-x p") 'projectile-switch-project)

(require-package 'counsel)
(ivy-mode 1)

(setq ivy-use-virtual-buffers t)

;; TODO
(defun foo-action (file)
  (find-file-other-window (cdr file)))
(ivy-set-actions
 'find-file-in-project
 '(("j" foo-action "other window")))

;; this makes swiper behave more like vim search
(setq swiper-goto-start-of-match t)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

; (global-set-key (kbd "M-x") 'counsel-M-x)

; (require-package 'avy)
; (global-set-key (kbd "C-:") 'avy-goto-char)

(require-package 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(require-package 'find-file-in-project)
;; TODO Ambivalent about this...
; (global-set-key (kbd "C-x o") 'find-file-in-project)

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(global-set-key (kbd "C-w") 'backward-kill-word)

(setq evil-want-C-u-scroll t)

(require-package 'paredit)

(require-package 'clojure-mode)

(require-package 'clj-refactor)

(require-package 'rainbow-delimiters)

(require-package 'highlight-parentheses)

(require-package 'paredit)
;; (require-package 'lispy)
(require-package 'evil-cleverparens)
;; (add-hook 'lispy-mode-hook #'evil-cleverparens-mode)
(add-hook 'paredit-mode-hook #'evil-cleverparens-mode)
;; (setq evil-cleverparens-use-additional-movement-keys nil)
;; (require-package 'lispyville)
;; (add-hook 'lispy-mode-hook #'lispyville-mode)
;; (with-eval-after-load 'lispyville
;;   (lispyville-set-key-theme '(operators
;; 			      (slurp/barf-cp)
;; 			      (additional-movement normal visual motion))))

(defun my-emacs-lisp-mode-hook ()
  (linum-relative-mode)
  (linum-mode)
  (rainbow-delimiters-mode)
  (highlight-parentheses-mode)
  ;; (lispy-mode)
  (paredit-mode))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  ;; Check out nlinum (supposedly faster)
  (linum-mode)
  (linum-relative-mode)
  (paredit-mode)
  ;; (lispy-mode)
  (rainbow-delimiters-mode)
  (highlight-parentheses-mode))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

; (require-package 'cider)

(require-package 'evil-surround)
(global-evil-surround-mode 1)

(require-package 'evil-commentary)
(evil-commentary-mode)


;; (eval-after-load "evil-maps"
;;   (define-key evil-motion-state-map "K" nil))
;; (general-define-key :keymaps 'clojure-mode-map
;; 		    "K" 'cider-doc)

;; (require-package 'evil-leader)
;; (global-evil-leader-mode 1)

;; (evil-leader/set-leader "SPC")
;; (evil-leader/set-key
;;   "b" 'switch-to-buffer
;;   ; "f" 'find-file
;;   ; "j" 'projectile-find-tag
;;   ;; TODO Maybe an emacs binding for these two instead
;;   "j" 'evil-avy-goto-word-1-below
;;   "k" 'evil-avy-goto-word-1-above
;;   "o" 'find-file-in-project
;;   ; "o" 'projectile-find-file
;;   ; "O" 'projectile-find-file-other-window
;;   "w" 'save-buffer
;;   ; "x" 'counsel-M-x
;;   )

;; (evil-leader/set-key-for-mode 'clojure-mode
;;   ;; TODO e => eval sexp
;;   ;;      f => eval defn
;;   "e" 'cider-eval-last-sexp
;;   "f" 'cider-eval-defun-at-point
;;   "n" 'cider-eval-ns-form
;;   ;; Note to self: "m" is for major mode
;;   ;; "mr?" 'cljr-describe-refactoring
;;   "ril" 'cljr-introduce-let
;;   "rel" 'cljr-expand-let)

(require-package 'diminish)
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "linum-relative" '(diminish 'linum-relative-mode))
(eval-after-load "clj-refactor" '(diminish 'clj-refactor-mode))
(eval-after-load "lispy" '(diminish 'lispy-mode))
;; (eval-after-load "lispyville" '(diminish 'lispyville-mode))
(eval-after-load "evil-commentary" '(diminish 'evil-commentary-mode))
(eval-after-load "evil-cleverparens" '(diminish 'evil-cleverparens-mode))
(eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
                    '(defadvice ,mode (after rename-modeline activate)
                                (setq mode-name ,new-name))))
(rename-modeline "clojure-mode" clojure-mode  "Clj")

;; Do not change shape of cursor while in operator pending mode
(setq evil-operator-state-cursor '(box "white"))

;; This is a bit of a hack to make evil a bit more emacs
;; compatible. For example, evaluating lisp code is typically done by
;; positioning the cursor AFTER the closing paren of the form (which
;; is obviously hard to do in vim if this paren happens to be the last
;; char on the line)
(setq evil-move-beyond-eol t)

(require-package 'evil)
(evil-mode t)

(require-package 'general)

;; (setq general-default-keymaps 'evil-normal-state-map)
;; (setq my-leader "SPC")

(general-define-key :prefix "SPC"
		    :keymaps '(normal motion dired-mode-map)
		    ;; :states '(normal)
		    "" nil
		    "b" 'switch-to-buffer
		    "j" 'evil-avy-goto-word-1-below
		    "k" 'evil-avy-goto-word-1-above
		    "o" 'find-file-in-project
		    "w" 'save-buffer)

;; Seems like I need to unbind comma before it can be used as "local leader"
;; (eval-after-load "evil-maps"
;;   (define-key evil-motion-state-map "," nil))
;; (setq my-localleader ",")

(general-define-key :prefix ","
		    :keymaps 'clojure-mode-map
		    :states '(normal visual motion)
		    "" nil
		    "e" 'cider-eval-last-sexp
		    "f" 'cider-eval-defun-at-point
		    "n" 'cider-eval-ns-form
		    "v" 'cider-eval-sexp-at-point
		    "da" 'cider-apropos
		    "dd" 'cider-doc
		    "df" 'cider-apropos-documentation)

;; XXX A failed experiment
;; (require-package 'evil-extra-operator)
;; For lispy modes, evaluate movement/textobj
;; (define-key evil-motion-state-map "gp" 'evil-operator-eval)

(defun silence ()
  (interactive))
;; (define-key evil-motion-state-map [down-mouse-1] 'silence)
;; (define-key evil-motion-state-map [mouse-1] 'silence)

(evil-set-initial-state 'fundamental-mode 'emacs)
(evil-set-initial-state 'cider-repl-mode 'emacs)
(evil-set-initial-state 'cider-stacktrace-mode 'emacs)
(evil-set-initial-state 'cider-docview-mode 'motion)
;; Unsure about this
; (evil-set-initial-state 'lisp-interaction-mode 'emacs)

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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
