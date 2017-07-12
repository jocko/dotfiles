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
;; I need more contrast for line numbers
(custom-theme-set-faces 'spacemacs-dark
                        '(linum ((t :inherit default
                                    :foreground "#888888"))))

(setq inhibit-startup-screen t)
(setq scroll-margin 5)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq echo-keystrokes 0.1)
(setq mouse-yank-at-point t)

(blink-cursor-mode 0)

(global-auto-revert-mode)

(set-face-attribute 'default nil
                    :family "Hack"
                    :height 120)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq evil-search-module 'evil-search)

;; Treat underscore as a word character in all modes
(modify-syntax-entry ?_ "w" (standard-syntax-table))

;; Mouse selection copies to primary (i.e. can later be pasted with
;; middle mouse), but NOT to clipboard (i.e. <C-v>)
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard nil)

(cd "~/Repos")

(require-package 'spaceline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-toggle-buffer-position-off)
(spaceline-toggle-buffer-encoding-abbrev-off)
(spaceline-toggle-version-control-off)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-line-column-off)
(spaceline-toggle-hud-off)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(setq powerline-default-separator 'nil)
(spaceline-compile)

;; Disable tool bar, menu bar, scroll bar and tool tips.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode tooltip-mode))
  (when (fboundp mode) (funcall mode -1)))

(require 'saveplace)
(save-place-mode 1)

(global-eldoc-mode -1)

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

(require-package 'diff-hl)
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)

(require-package 'magit)
(setq magit-status-sections-hook
      '(magit-insert-merge-log
        magit-insert-rebase-sequence
        magit-insert-am-sequence
        magit-insert-sequencer-sequence
        magit-insert-bisect-output
        magit-insert-bisect-rest
        magit-insert-bisect-log
        magit-insert-untracked-files
        magit-insert-unstaged-changes
        magit-insert-staged-changes
        magit-insert-unpulled-from-upstream
        magit-insert-unpulled-from-pushremote
        magit-insert-unpushed-to-upstream
        magit-insert-unpushed-to-pushremote
        magit-insert-recent-commits))

; (require-package 'avy)
; (global-set-key (kbd "C-:") 'avy-goto-char)

(require-package 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(require-package 'find-file-in-project)
;; TODO Ambivalent about this...
; (global-set-key (kbd "C-x o") 'find-file-in-project)

;; XXX This seem to be broken. I sometimes get "unrecognized entry in undo list undo-tree-canary"
;; (setq undo-tree-auto-save-history t)
;; (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(setq evil-want-C-u-scroll t)

;; (require-package 'paredit)

(require-package 'clojure-mode)

(require-package 'clj-refactor)

(require-package 'rainbow-delimiters)

(require-package 'highlight-parentheses)

;; (require-package 'paredit)
;; (require-package 'lispy)
(require-package 'smartparens)
(require 'smartparens-config)
;; (require-package 'evil-smartparens)
;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(require-package 'evil-cleverparens)

(require 'evil-cleverparens-text-objects)
(define-key evil-outer-text-objects-map "f" #'evil-cp-a-form)
(define-key evil-inner-text-objects-map "f" #'evil-cp-inner-form)
(define-key evil-outer-text-objects-map "c" #'evil-cp-a-comment)
(define-key evil-inner-text-objects-map "c" #'evil-cp-inner-comment)
(define-key evil-outer-text-objects-map "d" #'evil-cp-a-defun)
(define-key evil-inner-text-objects-map "d" #'evil-cp-inner-defun)

(require-package 'evil-smartparens)
;; (add-hook 'smartparens-enabled-hook #'evil-cleverparens-mode)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
;; (setq evil-cleverparens-move-skip-delimiters nil)
(setq evil-cleverparens-use-additional-movement-keys nil)
;; (evil-define-key 'normal evil-cleverparens-mode-map "c" nil)

;; (require-package 'evil-cleverparens)
;; (add-hook 'lispy-mode-hook #'evil-cleverparens-mode)
;; (add-hook 'paredit-mode-hook #'evil-cleverparens-mode)
;; (setq evil-cleverparens-use-additional-movement-keys nil)
;; (require-package 'lispyville)
;; (add-hook 'lispy-mode-hook #'lispyville-mode)
;; (with-eval-after-load 'lispyville
;;   (lispyville-set-key-theme '(operators
;; 			      (slurp/barf-cp)
;; 			      (additional-movement normal visual motion))))

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-relative-mode)

(defun my-emacs-lisp-mode-hook ()
  ;; (linum-relative-mode)
  ;; (linum-mode)
  (rainbow-delimiters-mode)
  (show-paren-mode)
  ;; TODO This mode didn't work as I expected
  ;; (highlight-parentheses-mode)
  (smartparens-strict-mode)
  ;; (lispy-mode)
  ;; (paredit-mode)
  )

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)

;; Skip prompt for interactive commands that require a symbol (e.g. cider-doc)
(setq cider-prompt-for-symbol nil)
;; Display eval result with overlays only (I get so confused when a
;; giant echo area appears at the bottom of my screen)
(setq cider-use-overlays t)

;; TODO For some reason, this doesn't work on my mac
;; (define-clojure-indent
;;  (match 1))

(defun my-clojure-mode-hook ()
  ;; (yas-reload-all)
  ;; (yas-minor-mode)
  (clj-refactor-mode 1)
  ;; Check out nlinum (supposedly faster)
  ;; (linum-mode)
  ;; (linum-relative-mode)
  ;; (paredit-mode)
  ;; (lispy-mode)
  (smartparens-strict-mode)
  (rainbow-delimiters-mode)
  ;; TODO This mode didn't work as I expected
  ;; (highlight-parentheses-mode)
  (show-paren-mode)
  ;; Make hyphens into word characters
  (modify-syntax-entry ?- "w" clojure-mode-syntax-table)
  ;; and !
  (modify-syntax-entry ?! "w" clojure-mode-syntax-table)
  ;; and :
  (modify-syntax-entry ?: "w" clojure-mode-syntax-table)
  (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
  )

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

; (require-package 'cider)

;; Prevent cider-repl to steal focus when launched
;; (setq cider-repl-pop-to-buffer-on-connect 'display-only)

;; Do not show cider-repl on jack in
(setq cider-repl-pop-to-buffer-on-connect nil)

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

; (require-package 'enh-ruby-mode)
; (add-to-list 'load-path "(path-to)/Enhanced-Ruby-Mode") ; must be added after any path containing old ruby-mode
; (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
; (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
; (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
; (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)
; (custom-theme-set-faces 'spacemacs-dark
;                        '(enh-ruby-heredoc-delimiter-face ((t (:inherit font-lock-string-face)))))

; (require-package 'robe)
; (add-hook 'enh-ruby-mode-hook 'robe-mode)

(require-package 'diminish)
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "linum-relative" '(diminish 'linum-relative-mode))
(eval-after-load "clj-refactor" '(diminish 'clj-refactor-mode))
(eval-after-load "lispy" '(diminish 'lispy-mode))
(eval-after-load "lispyville" '(diminish 'lispyville-mode))
(eval-after-load "evil-commentary" '(diminish 'evil-commentary-mode))
(eval-after-load "evil-cleverparens" '(diminish 'evil-cleverparens-mode))
(eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "evil-smartparens" '(diminish 'evil-smartparens-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "company" '(diminish 'company-mode))
; (eval-after-load "robe" '(diminish 'robe-mode))
; (diminish 'abbrev-mode)
(diminish 'auto-revert-mode)

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
                    '(defadvice ,mode (after rename-modeline activate)
                                (setq mode-name ,new-name))))
(rename-modeline "clojure-mode" clojure-mode  "Clj")
; (rename-modeline "enh-ruby-mode" enh-ruby-mode "Ruby")

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

;; Switch to emacs state in cider debug so we don't mess up the
;; debug mappings (stepping, inspecting etc). When we are done, we
;; return to normal state.
(defun my-cider-debug-toggle-insert-state ()
  (if cider--debug-mode
      (evil-emacs-state)
    (evil-normal-state)))


(add-hook 'cider--debug-mode-hook 'my-cider-debug-toggle-insert-state)

(require-package 'company)
(add-hook 'cider-mode-hook #'company-mode)
(setq company-idle-delay nil)

(require-package 'general)

;; (evil-define-key 'normal smartparens-mode-map (kbd ">") 'paredit-forward-slurp-sexp)
;; (evil-define-key 'normal smartparens-mode-map (kbd "[") 'sp-backward-sexp)
;; (evil-define-key 'normal smartparens-mode-map (kbd "]") 'sp-forward-sexp)

;; (setq general-default-keymaps 'evil-normal-state-map)
;; (setq my-leader "SPC")

;; TODO Cannot decide which one is better, this one or the leader
;; mapping. For the moment, the leader mapping is winning
;; (global-set-key (kbd "<C-tab>") 'other-window)

(general-define-key :prefix "SPC"
                    :keymaps '(normal motion dired-mode-map)
                    ;; :states '(normal)
                    "" nil
                    ;; Jump to mark "a" (set mark with "ma")
                    "a" '(lambda (&optional arg)
                           (interactive "P")
                           (evil-set-jump)
                           (evil-goto-mark ?a))
                    "b" 'switch-to-buffer
                    ;; "j" 'evil-avy-goto-word-1-below
                    ;; "k" 'evil-avy-goto-word-1-above
                    ;; TODO It always confuses me when I use this in a
                    ;; non-vcs project and get a cryptic error
                    "TAB" 'other-window
                    "o" 'find-file-in-project
                    "w" 'save-buffer
                    "g" 'magit-status)

;; Seems like I need to unbind comma before it can be used as "local leader"
;; (eval-after-load "evil-maps"
;;   (define-key evil-motion-state-map "," nil))
;; (setq my-localleader ",")

(evil-define-key 'normal clojure-mode-map
  (kbd "K") 'cider-doc)
(evil-define-key 'normal evil-smartparens-mode-map
  "(" 'sp-backward-sexp)
(evil-define-key 'normal evil-smartparens-mode-map
  ")" 'sp-forward-sexp)
(evil-define-key 'normal evil-smartparens-mode-map
  "{" 'sp-backward-up-sexp)
(evil-define-key 'normal evil-smartparens-mode-map
  "}" 'sp-up-sexp)
(evil-define-key 'normal evil-smartparens-mode-map
  ">" 'sp-forward-slurp-sexp)
(evil-define-key 'normal evil-smartparens-mode-map
  "<" 'sp-forward-barf-sexp)
;; (evil-define-key 'normal clojure-mode-map
;;   (kbd "C-<") 'sp-backward-slurp-sexp)
;; (evil-define-key 'normal clojure-mode-map
;;   (kbd "C->") 'sp-backward-barf-sexp)

(require-package 'evil-extra-operator)
(define-key evil-motion-state-map "gr" 'evil-operator-eval)
; (setq evil-extra-operator-eval-modes-alist
;       '((enh-ruby-mode ruby-send-region)))

(defun my-eval-form ()
  (interactive)
  (let ((region (evil-cp-a-form)))
    (evil-operator-eval (nth 0 region) (nth 1 region))))

(evil-define-operator evil-cider-eval (beg end)
  (cider-eval-region beg end))

;; TODO Maybe rename these two to something like my-cider-eval-sexp
;; (defun cider-eval-sexp ()
;;   (interactive)
;;   (let ((ok (sp-get-sexp t)))
;;     (sp-get ok (message "%s-%s %s" :beg :end :op))))

;; TODO Should make this work for elisp as well, it is awesome
;; (require 'cl)
(defun my-cider-eval-form ()
  (interactive)
  (let ((region (evil-cp-a-form)))
    (evil-cider-eval (nth 0 region) (nth 1 region))))

;; "Local leader" mappings
(setq my-local-leader ",")

(general-define-key :prefix my-local-leader
                    :keymaps 'clojure-mode-map
                    :states '(normal visual motion)
                    "" nil
                    "s" 'cider-scratch
                    "c" 'cider-connect
                    "q" 'cider-quit
                    "d" 'cider-eval-defun-at-point
                    "f" 'my-cider-eval-form
                    ;; "s" 'cider-eval-sexp
                    ;; XXX Probably not as useful as I first though
                    ;; "n" 'cider-eval-ns-form
                    ;; "v" 'cider-eval-sexp-at-point
                    "k" 'cider-load-buffer
                    ;; TODO Chose one of these two and map to... a?
                    ;; "ca" 'cider-apropos
                    ;; Lookup doc but prompt for symbol
                    "a" '(lambda (&optional arg) (interactive "P") (cider-grimoire t))
                    "x" 'cider-refresh
                    ;; "o" 'sp-splice-sexp-killing-backward
                    "o" 'sp-raise-sexp
                    "t" 'cider-test-run-ns-tests
                    ;; TODO Would be nice to be able to shave off one keystroke here
                    ;; EDIT: The double tap thing kind of works
                    ;; Ooooor, maybe make a hydra for it... maybe
                    ",ar" 'cljr-add-require-to-ns
                    ",am" 'cljr-add-missing-libspec
                    ",cn" 'cljr-clean-ns
                    ",il" 'cljr-introduce-let
                    ",el" 'cljr-expand-let
                    ",ef" 'cljr-extract-function
                    ",tf" 'clojure-thread-first-all
                    ",tl" 'clojure-thread-last-all
                    ",cp" 'cljr-cycle-privacy
                    ",ad" 'cljr-add-declaration
                    "(" '(lambda (&optional arg)
                           (interactive "P")
                           (sp-wrap-with-pair "(")
                           (evil-insert-state)))


(general-define-key :prefix ","
                    :keymaps 'emacs-lisp-mode-map
                    :states '(normal visual motion)
                    "" nil
                    ;; "d" 'cider-eval-defun-at-point
                    "f" 'my-eval-form)
;; XXX A failed experiment
;; (require-package 'evil-extra-operator)
;; For lispy modes, evaluate movement/textobj
;; (define-key evil-motion-state-map "gp" 'evil-operator-eval)

;; (defun silence ()
;;   (interactive))
;; (define-key evil-motion-state-map [down-mouse-1] 'silence)
;; (define-key evil-motion-state-map [mouse-1] 'silence)

(evil-set-initial-state 'fundamental-mode 'emacs)
(evil-set-initial-state 'cider-repl-mode 'emacs)
(evil-set-initial-state 'cider-stacktrace-mode 'emacs)
(evil-set-initial-state 'cider-docview-mode 'motion)
(evil-set-initial-state 'cider-popup-buffer-mode 'motion)
;; Unsure about this, motion or emacs???
(evil-set-initial-state 'cider-inspector-mode 'emacs)
;; Unsure about this
; (evil-set-initial-state 'lisp-interaction-mode 'emacs)


;; This makes the *-search execute with the vim defintion of a word
;; (e.g. 'my-word' instead of just 'my')
;; TODO Is this neccessary when I redefine forward-evil-word??
(setq-default evil-symbol-word-search 'symbol)





; (setq x-select-enable-clipboard t)


; (defmacro after-load (feature &rest body)
;   "After FEATURE is loaded, evaluate BODY."
;   (declare (indent defun))
;   `(eval-after-load ,feature
;      '(progn ,@body)))


(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(require-package 'markdown-mode)

;; Why doesn't this work :/
;; (setq whitespace-style '(trailing tabs tab-mark))
;; (add-hook 'prog-mode-hook 'whitespace-mode)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
