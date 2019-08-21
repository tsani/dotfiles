;;;;; PACKAGE MANAGEMENT ;;;;;

(require 'package)


(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'package-archives
  '("melpa-stable" . "http://stable.melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(debug-on-error nil)
 '(evil-shift-width 2)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (proof-general agda2-mode omnisharp highlight-parentheses highlight-parentheses-mode idris-mode helm-ag csharp-mode rudel yaml-mode frames-only-mode solarized-theme neotree intero helm markdown-mode use-package evil-visual-mark-mode)))
 '(proof-multiple-frames-enable t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beluga-holes ((t (:background "pink" :slant italic)))))

(setq package-enable-at-startup nil)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package frames-only-mode
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package helm
  :ensure t)
(use-package evil
  :init
  ;; makes evil cooperate with proof general
  ;; specifically, switching to normal mode after typing certain keys
  ;; will trigger autocomplete / snippet insertion unless this is set.
  (setq evil-want-abbrev-expand-on-insert-exit nil)

  ;; so that C-u will scroll up, as in Vim
  (setq evil-want-C-u-scroll t)
  :ensure t)
(use-package intero
  :ensure t)
(use-package neotree
  :init
    (global-set-key [f8] 'neotree-toggle)
  :ensure t)
(use-package solarized-theme
  :ensure t)
(use-package csharp-mode
  :ensure t)
(use-package helm-ag
  :ensure t)
(use-package idris-mode
  :ensure t)
(use-package highlight-parentheses
  :config
  (setq hl-paren-colors '("black"))
  (setq hl-paren-background-colors '("red" "orange" "yellow" "green" "blue" "violet"))
  (setq hl-paren-delay 0.05)
  :ensure t)
(use-package omnisharp
  :ensure t)
(use-package proof-general
  :ensure t)
(use-package auctex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))
(use-package company
  :ensure t)

;;;;; LOADING PACKAGES ;;;;;

(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line

(let ((home (getenv "HOME"))
      (dirs
       '("/projects/Beluga/tools"
         "/projects/Beluga/tools/emacs"
         "/.opam/system/share/emacs/site-lisp"
         "/projects/agda/src/data/emacs-mode")))
  (dolist (path dirs)
    (add-to-list 'load-path (concat home path))))

(require 'beluga-mode)
(require 'agda2-mode)

(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook #'flycheck-mode)
(add-hook 'agda2-mode-hook
          (lambda () (define-key evil-normal-state-map (kbd "g d") 'agda2-goto-definition-keyboard)))
(add-hook 'merlin-mode-hook
          (lambda () (define-key evil-normal-state-map (kbd "g d") 'merlin-locate)))
(evil-mode t)

(push "/home/tsani/.opam/system/share/emacs/site-lisp" load-path)
(autoload 'merlin-mode "merlin" nil t nil)
(add-hook 'tuareg-mode-hook 'merlin-mode t)

(helm-mode 1)
(frames-only-mode 1)
(setq focus-follows-mouse t)
;; ^ so emacs will warp the mouse when using a frame-select command

;; So company will use C-RET to select an option instead of RET.
;; This default behaviour is cancer.
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-<return>") #'company-complete-selection)
  (define-key company-active-map (kbd "C-RET") #'company-complete-selection)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-w") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;;;;; KEYBINDINGS ;;;;;

(define-key evil-normal-state-map (kbd ", w") 'save-buffer)
(define-key evil-normal-state-map (kbd ", q") 'evil-quit)

(add-hook 'neotree-mode-hook
  (lambda ()
    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;;;;; TEXT MODE ;;;;;
; Options that influence the writing of text.

(add-hook 'text-mode-hook
	  '(lambda ()
	     (set-fill-column 80)
	     (turn-on-auto-fill)))

; make insert-mode C-u delete to beginning of line
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))

(setq-default
  indent-tabs-mode nil
  tab-width 2)
(setq c-basic-offset 2)
(setq c-default-style "linux")

(defun jake-c-setup-indent ()
  (message "setting up indentation!")
  (c-set-style "bsd")
  (setq tab-width 2)
  (setq substatement-open 0)
  (setq-default c-basic-offset 2)
  (setq c-basic-offset 2))
(add-hook 'csharp-mode-hook 'jake-c-setup-indent)

(setq-default require-final-newline t)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

; A nice idea, but unless it's my own code, this ends up creating
; spurious whitespace changes in other people's code.
;; (add-hook 'before-save-hook
;;           'delete-trailing-whitespace)

;;;;; AUTO-SCROLLING ;;;;;

; Do not center point in window when scrolling.
(setq scroll-conservatively 1)
; Leave a margin of 7 lines when scrolling.
(setq scroll-margin 7)

;;;;; INTEGRATIONS ;;;;;

(setq vc-follow-symlinks t)
(setq x-select-enable-primary t)

;;;;; VISUALS ;;;;;

(load-theme 'solarized-light)
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq jake-preferred-font "mononoki-10")
(set-default-font jake-preferred-font)
(add-to-list 'default-frame-alist
             `(font . ,jake-preferred-font))
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;;;;; AUTO-SAVING AND BACKUPS ;;;;;

(setq backup-directory-alist `(("." . "~/.saves"))
      backup-by-copying t
      kept-new-versions 10
      kept-old-version 0
      delete-old-versions t
      version-control t)

(define-key evil-normal-state-map (kbd "C-p") 'universal-argument)

;;;;; HACKS ;;;;;

;; Tuareg is currently broken:
;; https://github.com/ocaml/tuareg/issues/162
;; The workaround is to define my own `tuareg-abbrev-hook` function
;; that does nothing.
(defun tuareg-abbrev-hook ())
