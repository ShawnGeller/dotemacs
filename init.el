;; Shawn's init.el. First load package libraries

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/use-package-2.4.5")
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diminish
(use-package diminish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode

(global-visual-line-mode t)
(require 'undo-tree)
(global-undo-tree-mode)

(use-package evil
  :ensure t
  :config
    (setq evil-want-C-i-jump nil)
    (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-d-scroll t)
    (evil-mode 1)
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    :diminish (evil-mode undo-tree-mode)
  )

(use-package evil-surround
  :requires evil
  :config 
    (global-evil-surround-mode 1)
    (defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
    (let ((inner-name (make-symbol (concat "evil-inner-" name)))
            (outer-name (make-symbol (concat "evil-a-" name))))
        `(progn
        (evil-define-text-object ,inner-name (count &optional beg end type)
            (evil-select-paren ,start-regex ,end-regex beg end type count nil))
        (evil-define-text-object ,outer-name (count &optional beg end type)
            (evil-select-paren ,start-regex ,end-regex beg end type count t))
        (define-key evil-inner-text-objects-map ,key #',inner-name)
        (define-key evil-outer-text-objects-map ,key #',outer-name))))
  )

(use-package general
    :ensure t
    :config
      (general-create-definer my-leader-def
        ;; :prefix my-leader
        :prefix "\\")
      (general-evil-setup)
  )

;; esc quits
(defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
	(setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline
(use-package powerline
  :config
    (display-time-mode t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy, counsel, swiper

(use-package ivy
  :ensure t
  :bind
  (:map ivy-minibuffer-map      ([escape] . 'minibuffer-keyboard-quit))
  (:map ivy-switch-buffer-map   ([escape] . 'minibuffer-keyboard-quit))
  (:map ivy-mode-map            ([escape] . 'minibuffer-keyboard-quit))
  (:map ivy-occur-mode-map      ([escape] . 'minibuffer-keyboard-quit))
  (:map ivy-occur-grep-mode-map ([escape] . 'minibuffer-keyboard-quit))
  :init
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-wrap t)

    (defun ivy-yank-action (x)
    (kill-new x))

    (defun ivy-copy-to-buffer-action (x)
    (with-ivy-window
        (insert x)))

    (ivy-set-actions
    t
    '(("i" ivy-copy-to-buffer-action "insert")
    ("y" ivy-yank-action "yank")))
    :diminish ivy-mode
  )
(use-package counsel
  :ensure t
  :bind
    ("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file)
    ("<f1> f" . counsel-describe-function)
    ("<f1> v" . counsel-describe-variable)
    ("<f1> l" . counsel-find-library)
    ("<f2> i" . counsel-info-lookup-symbol)
    ("<f2> u" . counsel-unicode-char)
    ("M-x"     . counsel-M-x)
    ("C-x C-f" . counsel-find-file)
  )

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company mode
(use-package company
  :commands (company-mode company-indent-or-complete-common)
  :hook (after-init . global-company-mode)
  :general ("C-p" 'counsel-company)
  :diminish company-mode
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; material theme
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key

(use-package which-key
  :ensure t
  :init
    (which-key-setup-minibuffer)
    (which-key-mode)
  :config
    (setq which-key-show-operator-state-maps t)
    (setq which-key-allow-evil-operators t)
  :diminish
  )

(use-package rainbow-delimiters)

;; Highlights matching parenthesis

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
(use-package org
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python     . t)))
  (org-display-inline-images))
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :diminish)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc.
;; comments
(global-set-key (kbd "C-;") 'comment-line)


;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))


;; Hard-to-categorize customizations
;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "chartreuse3")))))
(put 'narrow-to-page 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-electric-sub-and-superscript nil)
 '(coffee-tab-width 2)
 '(custom-safe-themes
   '("93f9654f91d31e9a9ec6ea2fcffcfcab38353a9588673f2b750e591f704cd633" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))
 '(debug-on-error t)
 '(display-time-mode t)
 '(evil-undo-system 'undo-tree)
 '(fill-column 80)
 '(global-font-lock-mode t)
 '(ispell-dictionary nil)
 '(magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
 '(org-agenda-files '("~/Desktop/research/kaufman/sensing/notes.org"))
 '(package-selected-packages
   '(ivy-bibtex rg counsel-projectile flycheck powerline rainbow-delimiters web-mode matlab-mode prescient ivy-prescient wolfram-mode undo-tree fireplace fzf lsp-java ivy-xref ccls racer rust-mode web-beautify auctex company-auctex general column-enforce-mode diminish neotree ob-diagrams company-lsp gnu-elpa-keyring-update counsel-tramp tagedit paredit f evil-visualstar evil-visual-replace evil-surround evil-org evil-magit calfw-org))
 '(python-shell-interpreter "python3")
 '(show-paren-mode t))
(put 'narrow-to-region 'disabled nil)
