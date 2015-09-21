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

(setq inhibit-startup-screen t
      initial-scratch-message nil)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
;; (setq split-height-threshold 0)
;; (setq split-width-threshold nil)
(menu-bar-mode -1)

(setq auto-save-default nil)
;; (setq helm-split-window-default-side 'right)
;; (setq helm-split-window-in-side-p t)

(require-package 'gruvbox-theme)
(load-theme 'gruvbox t)

(require-package 'helm)
(require 'helm-config)

(require-package 'helm-projectile)
(require 'helm-projectile)
(helm-projectile-on)

(require-package 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

(require-package 'evil-leader)
(global-evil-leader-mode t)

(require-package 'avy)

(require-package 'evil-commentary)
(evil-commentary-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "w" 'save-buffer
  "e" 'eval-last-sexp
  "f" 'helm-find-files
  "b d" 'kill-this-buffer
  "l" 'avy-goto-line)
;; (evil-leader/set-key "e" 'find-file)
;; (evil-leader/set-key "j" 'avy-goto-line)

(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(require-package 'evil)

(setq evil-want-C-u-scroll t)

(require 'evil)
(evil-mode t)
