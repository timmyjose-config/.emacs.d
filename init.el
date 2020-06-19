(require 'package)

(setq package-archives
      '(("melpa stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("melpa" . 100)
	("melpa stable" . 50)
        ("elpa" . 25)))


;;; setup the required packages
;;; and install them if not found
(setq packages-list
      '(company
        exec-path-from-shell
        flycheck
        flycheck-package
        helm
        helm-ag
        helm-company
        helm-projectile
        magit
        markdown-mode
        package-lint
	paredit
        projectile
	slime
        zig-mode))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package packages-list)
  (unless (package-installed-p package)
    (package-install package)))

;; flycheck-package config
(eval-after-load 'flycheck
    '(flycheck-package-setup))

;;; Common Setup

;; load up cl.el
(eval-when-compile (require 'cl))

;; hide menubars. toolbars, and scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; start emacs with maximised screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start splits vertically
(setq
 split-width-threshold 0
 split-height-threshold nil)

;; use windmove everywhere

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun ignore-windmove-error (fn)
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

(global-set-key [s-left] (ignore-windmove-error 'windmove-left))
(global-set-key [s-right] (ignore-windmove-error 'windmove-right))
(global-set-key [s-up] (ignore-windmove-error 'windmove-up))
(global-set-key [s-down] (ignore-windmove-error 'windmove-down))

;; set preferred font

(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-14"))

;; no backups allowed
(setq make-backup-files nil)

;; deal with PATH hell on macOS in a sane way
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; auto-focus on help window
(setq help-window-select t)


;;; Language-specific Setup

;; SBCL setup
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")


;;; Global Hooks

;; enable helm-mode and bind M-x to helm-M-x
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
; fix annoying quirky TAB behaviour 
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

;; use company-mode everywhere
(add-hook 'after-init-hook 'global-company-mode)

;; enable projectile mode
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(define-key projectile-mode-map (kbd "M-t") 'helm-projectile)

;; use helm with Silver Searcher
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (package-lint magit markdown-mode markdown helm-ag helm-projectile projectile projectile-mode helm-company helm company company-mode exec-path-from-shell slime))))

(global-set-key (kbd "M-g") 'helm-ag)

;; use electric-pair mode everywhere (unless overridden
;; by specific modes)
(electric-pair-mode 1)

;; highlight matching parens
(show-paren-mode 1)

;; enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Language-specific Hooks

;; elisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (paredit-mode 1)
	    (setq indent-tabs-mode nil)
	    (define-key emacs-lisp-mode-map
	      "\C-x\C-e" 'pp-eval-last-sexp)))

;; Common Lisp
(add-hook 'common-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)
            (setq indent-tabs-mode nil)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
