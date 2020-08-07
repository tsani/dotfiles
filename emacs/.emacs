;;;;; PACKAGE MANAGEMENT ;;;;;

(require 'package)


(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
  '("melpa-stable" . "https://stable.melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beluga-interpreter-name "/home/tsani/projects/Beluga/bin/beluga")
 '(compilation-read-command nil)
 '(custom-safe-themes
   (quote
    ("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(debug-on-error nil)
 '(doc-view-resolution 300)
 '(evil-shift-width 2)
 '(haskell-compile-cabal-build-command "stack build --ghc-options=-ferror-spans")
 '(haskell-compile-ignore-cabal nil)
 '(helm-completion-style (quote helm-fuzzy))
 '(inhibit-startup-screen t)
 '(lsp-enable-snippet nil)
 '(lsp-prefer-flymake nil)
 '(lsp-ui-doc-enable t)
 '(lsp-ui-flycheck-enable t)
 '(lua-indent-level 2)
 '(lua-prefix-key "C-c")
 '(merlin-type-after-locate t)
 '(package-selected-packages
   (quote
    (zones web-mode lua-mode company evil yasnippet latex-extra lsp-haskell lsp-ui lsp-mode proof-general agda2-mode omnisharp highlight-parentheses highlight-parentheses-mode idris-mode helm-ag csharp-mode rudel yaml-mode frames-only-mode solarized-theme neotree helm markdown-mode use-package evil-visual-mark-mode)))
 '(proof-multiple-frames-enable t)
 '(tramp-remote-path
   (quote
    (tramp-own-remote-path tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin")))
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beluga-holes ((t (:background "pink" :slant italic))))
 '(lsp-ui-doc-background ((t (:background "gray91")))))

(setq package-enable-at-startup nil)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package lua-mode
  :ensure t)
(use-package web-mode
  :mode ("\\.jsx?$" . web-mode)
  :config (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  :ensure t)
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  ; (add-hook 'haskell-mode-hook #'lsp)
  :bind
  (:map haskell-mode-map
        ("C-c C-c" . haskell-compile)
        ("C-c C-h" . hoogle)
        ("C-c C-l" . haskell-process-load-file)
        ("C-c C-n" . haskell-goto-next-error)
        ("C-c C-p" . haskell-goto-prev-error)
        )
  :ensure t)
(use-package lsp-mode
  :ensure t)
(use-package lsp-ui
  :ensure t)
(use-package lsp-haskell
  :after lsp
  :ensure t)
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
(use-package latex-extra
  :ensure t)
(use-package yasnippet
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
(use-package flycheck
  :config
  (add-hook 'haskell-mode-hook #'flycheck-mode)
  (setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

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

(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc/")
(require 'mozc)
(evil-define-key nil evil-normal-state-map
  (kbd "C-c C-j") 'mozc-mode)
(evil-define-key nil evil-insert-state-map
  (kbd "C-c C-j") 'mozc-mode)

(defun jake-goto-definition (&rest arg-list)
  "Overridden by hooks and integrated with evil to get customizable
goto-definition behaviours."
  nil)
(make-variable-buffer-local 'jake-goto-definition)

(defun jake-dune-compile ()
  "Compiles using dune in the directory of the dune-project file, if
one can be found in any parent directory. Otherwise simply invokes
compile"
  (interactive)
  (if-let ( (dir (locate-dominating-file "." "dune-project")) )
      (let ( (default-directory dir) )
        (call-interactively #'compile))
    (compile)))

; (add-hook 'csharp-mode-hook 'omnisharp-mode)
; (add-hook 'csharp-mode-hook #'flycheck-mode)
(add-hook 'agda2-mode-hook
          (lambda ()
            (evil-define-key 'normal agda2-mode-map
              "gd" 'agda2-goto-definition-keyboard)))
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'tuareg-mode-hook
          (lambda ()
            (evil-define-key
              'normal
              tuareg-mode-map
              (kbd "C-c C-c")
              'jake-dune-compile)
            (setq compile-command "opam exec dune build ")))
(add-hook 'merlin-mode-hook
          (lambda ()
            (evil-define-key
              'normal
              merlin-mode-map
              "gd"
              'merlin-locate)))
; (add-hook 'lsp-mode-hook
;           (lambda ()
;             (evil-define-key 'normal lsp-mode-map
;               "g d" 'lsp-find-definition)))
(evil-mode t)

(push "/home/tsani/.opam/system/share/emacs/site-lisp" load-path)
(autoload 'merlin-mode "merlin" nil t nil)

(helm-mode 1)
(frames-only-mode 1)
(setq focus-follows-mouse t)
;; ^ so emacs will warp the mouse when using a frame-select command

(setq lsp-haskell-process-path-hie "hie-wrapper")

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

;;;;; DOCVIEW MODE ;;;;;

(with-eval-after-load 'doc-view
  (define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)
  (define-key doc-view-mode-map (kbd "C-u") 'doc-view-previous-page)
  (define-key doc-view-mode-map (kbd "C-d") 'doc-view-next-page))

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
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;;;;; AUTO-SCROLLING ;;;;;

; Do not center point in window when scrolling.
(setq scroll-conservatively 1)
; Leave a margin of 7 lines when scrolling.
(setq scroll-margin 5)

;;;;; INTEGRATIONS ;;;;;

(setq vc-follow-symlinks t)
(setq x-select-enable-primary t)

;;;;; VISUALS ;;;;;

(load-theme 'solarized-light)
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq jake-preferred-font "mononoki-12")
(set-default-font jake-preferred-font)
(add-to-list 'default-frame-alist
             `(font . ,jake-preferred-font))
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;;;;; AUTO-SAVING AND BACKUPS ;;;;;

(setq backup-directory-alist `(("." . "~/.saves/"))
      backup-by-copying t
      kept-new-versions 10
      kept-old-version 0
      delete-old-versions t
      version-control t)
(setq auto-save-file-name-transforms
      `((".*" "~/.saves/" t)))

(setq create-lockfiles nil) ;; unnecessary bc this is my own computer

;;;;; HACKS ;;;;;

(define-key evil-normal-state-map (kbd "C-p") 'universal-argument)

;; Tuareg is currently broken:
;; https://github.com/ocaml/tuareg/issues/162
;; The workaround is to define my own `tuareg-abbrev-hook` function
;; that does nothing.
(defun tuareg-abbrev-hook ())
