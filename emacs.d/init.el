(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;; from purcell/emacs.d
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

(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "M-x"))
(global-set-key (kbd "C-x m") 'execute-extended-command)

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

(setq echo-keystrokes 0.1)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require-package 'darktooth-theme)
(load-theme 'darktooth t)

(setq dired-use-ls-dired nil)

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
                    '(defadvice ,mode (after rename-modeline activate)
                                (setq mode-name ,new-name))))
; (rename-modeline "clojure-mode" clojure-mode "Clj")

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(require 'saveplace)
  (setq-default save-place t)

(global-set-key "\C-w" 'backward-kill-word)

(after-load 'ruby-mode
            (add-hook 'ruby-mode-hook (lambda () (subword-mode 1))))

(require-package 'ace-jump-mode)

(require-package 'evil)

(require-package 'evil-leader)
(global-evil-leader-mode 1)

(evil-leader/set-leader "SPC")

(evil-set-initial-state 'fundamental-mode 'emacs)

(evil-mode t)

; (setq inhibit-startup-screen t
;       initial-scratch-message nil)
; (setq split-height-threshold nil)
; (setq split-width-threshold 0)
; ;; (setq split-height-threshold 0)
; ;; (setq split-width-threshold nil)
; (menu-bar-mode -1)

; (setq auto-save-default nil)
; ;; (setq helm-split-window-default-side 'right)
; ;; (setq helm-split-window-in-side-p t)

; ;; (require-package 'gruvbox-theme)
; ;; (load-theme 'gruvbox t)
; (require-package 'darktooth-theme)
; (load-theme 'darktooth t)

; (require-package 'helm)
; (require 'helm-config)

; (require-package 'helm-projectile)
; (require 'helm-projectile)
; (helm-projectile-on)

; (require-package 'projectile)
; (projectile-global-mode)
; (setq projectile-completion-system 'helm)

; (require-package 'hydra)
; (require 'hydra)

; (defhydra hydra-file (:color blue)
;   ("o" find-file)
;   ("f" helm-find-files)
;   ("m" helm-mini)
;   ("d" dired))

; (setq evil-search-module 'evil-search
;       evil-want-C-u-scroll t
;       evil-want-C-w-in-emacs-state t)

; (require-package 'evil-leader)
; (global-evil-leader-mode t)

; ;; (set-face-attribute 'avy-lead-face nil :foreground "white" :background nil)
; (require-package 'avy)
; ;; (require 'avy)
; (with-eval-after-load 'avy
;   (setq
;    avy-background t)
;    ;; avy-style 'de-bruijn
;    ;; avy-keys (string-to-list "jfkdls;aurieowncpqmxzb"))
;   (set-face-attribute 'avy-lead-face nil :foreground "brightwhite" :background "cyan")
;   (set-face-attribute 'avy-lead-face-0 nil :foreground "brightwhite" :background "magenta"))
;   ;; (set-face-background 'avy-lead-face nil)
;   ;; (set-face-foreground 'avy-lead-face-0 "#dc322f")
;   ;; (set-face-foreground 'avy-lead-face "#b58900")
;   ;; (set-face-attribute 'avy-lead-face nil :weight 'normal)
;   ;; (set-face-attribute 'avy-lead-face-0 nil :weight 'extra-bold)
;   ;; (set-face-foreground 'avy-background-face "#586e75")
;   ;; (set-face-background 'avy-lead-face-1 nil)
;   ;; (set-face-foreground 'avy-lead-face-1 "#839493"))

; ;;(set-face-attribute 'avy-lead-face-0 nil :background "red" :foreground "brightwhite")
;  ;; '(avy-lead-face ((t (:background "cyan" :foreground "brightwhite"))))
;  ;; '(avy-lead-face-0 ((t (:background "red" :foreground "brightwhite")))))

; (require-package 'evil-commentary)
; (evil-commentary-mode)

; (evil-leader/set-leader "<SPC>")
; (evil-leader/set-key
;   "w" 'save-buffer
;   "e" 'eval-last-sexp
;   "f" 'helm-find-files
;   ;; "f o" 'find-file
;   ;; "f f" 'helm-find-files
;   ;; "f m" 'helm-mini
;   ;; "f d" 'dired
;   ;; "f" 'hydra-file/body
;   "b d" 'kill-this-buffer
;   "l" 'avy-goto-line)
; ;; (evil-leader/set-key "e" 'find-file)
; ;; (evil-leader/set-key "j" 'avy-goto-line)

; (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

; (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
; (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
; (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
; (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

; (require-package 'evil)

; (mapc
;   (lambda (mode-hook)
;     (add-hook mode-hook 'turn-on-evil-mode))
;   '(text-mode-hook
;      prog-mode-hook
;      comint-mode-hook
;      conf-mode-hook))
; (mapc
;   (lambda (mode-hook)
;     (add-hook mode-hook 'turn-off-evil-mode))
;   '(Info-mode-hook))

; (setq evil-want-C-u-scroll t)

; (require 'evil)
; ;; (evil-mode t)
