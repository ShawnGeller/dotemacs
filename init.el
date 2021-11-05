;; Shawn's init.el. First load package libraries

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(setq inhibit-startup-message t)
(set-scroll-bar-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-2.4")
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diminish
(use-package diminish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode

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

(require 'general)
(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "\\")
(general-evil-setup)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; column enforce
;; (use-package column-enforce-mode :ensure t
;;  :hook (after-init . global-column-enforce-mode)
;;  :config (setq column-enforce-column 100)
;;  :diminish column-enforce-mode
;; )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; material theme
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck

(use-package flycheck
  :config
    (setq flycheck-check-syntax-automatically '(save mode-enable))
    (global-flycheck-mode +1)
  :diminish flycheck-mode
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auctex
(use-package tex-site                   ; auctex
  :load-path "site-lisp/auctex/"
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . (lambda ()
                  (define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$")
                  (define-and-bind-quoted-text-object "lr-pair" "\\" "\\\\left"
                    "\\\\right")
                  (define-and-bind-quoted-text-object "escaped-braces" "|" "\\\\{" "\\\\}")
                  ))
  :init
  (setq reftex-plug-into-AUCTeX t)
  (setenv "PATH" (concat "/Library/TeX/texbin:"
                         (getenv "PATH")))
  (add-to-list 'exec-path "/Library/TeX/texbin")
  :config
  (setq font-latex-fontify-script nil)
  (defun latex-help-get-cmd-alist ()    ;corrected version:
    "Scoop up the commands in the index of the latex info manual.
   The values are saved in `latex-help-cmd-alist' for speed."
    ;; mm, does it contain any cached entries
    (if (not (assoc "\\begin" latex-help-cmd-alist))
        (save-window-excursion
          (setq latex-help-cmd-alist nil)
          (Info-goto-node (concat latex-help-file "Command Index"))
          (goto-char (point-max))
          (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
            (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
                  (value (buffer-substring (match-beginning 2)
                                           (match-end 2))))
              (add-to-list 'latex-help-cmd-alist (cons key value))))))
    latex-help-cmd-alist)

  (use-package latex
    :defer t
    :config
    (use-package preview)
    (add-hook 'LaTeX-mode-hook 'reftex-mode)
    ))


;; (use-package auctex
;;   :mode ("\\.tex\\'" . TeX-latex-mode)
;;   :hook (LaTeX-mode . LaTeX-math-mode)
;;   :config
;;   (setq TeX-electric-sub-and-superscript t)
;;   (setq TeX-electric-math t)
;;   (defun latex-help-get-cmd-alist ()    ;corrected version:
;;     "Scoop up the commands in the index of the latex info manual.
;;    The values are saved in `latex-help-cmd-alist' for speed."
;;     ;; mm, does it contain any cached entries
;;     (if (not (assoc "\\begin" latex-help-cmd-alist))
;;         (save-window-excursion
;;           (setq latex-help-cmd-alist nil)
;;           (Info-goto-node (concat latex-help-file "Command Index"))
;;           (goto-char (point-max))
;;           (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
;;             (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
;;                   (value (buffer-substring (match-beginning 2)
;;                                            (match-end 2))))
;;               (add-to-list 'latex-help-cmd-alist (cons key value))))))
;;     latex-help-cmd-alist)

;;   (add-hook 'TeX-after-compilation-finished-functions
;;             #'TeX-revert-document-buffer))

(use-package company-auctex
  :after (company latex))

;; (use-package latex
;;   :after auctex
;;   :config
;;   (require 'preview)
;;   (load (emacs-path "site-lisp/auctex/style/minted"))
;;   (info-lookup-add-help :mode 'LaTeX-mode
;;                         :regexp ".*"
;;                         :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
;;                         :doc-spec '(("(latex2e)Concept Index")
;;                                     ("(latex2e)Command Index")))
;;   )

;; (add-to-list 'ispell-local-dictionary-alist '("english-hunspell"
;;                                               "[[:alpha:]]"
;;                                               "[^[:alpha:]]"
;;                                               "[']"
;;                                               t
;;                                               ("-d" "en_US")
;;                                               nil
;;                                               iso-8859-1))

;; (setq ispell-program-name "hunspell"
;;       ispell-dictionary "english-hunspell")

(use-package reftex
  :after auctex
  :hook (LaTeX-mode . reftex-mode))

(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :config
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pyvenv
;; ;; (use-package pyvenv-mode :hook (python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matlab
(use-package matlab
  :ensure matlab-mode
  :config
  (add-to-list
   'auto-mode-alist
   '("\\.m\\'" . matlab-mode))
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web mode
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (general-nmap :prefix "\\ w" :prefix-map 'web-mode-map)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp
;; (require 'lsp-java)
;; (use-package lsp-mode
;;   :config
;;   (setq lsp-response-timeout 2)
;;   (setq lsp-eldoc-enable-hover nil)
;;   (setq lsp-eldoc-render-all nil)
;;   (setq
;;     lsp-signature-auto-activate t
;;     lsp-signature-doc-lines 1)
;;   (setq lsp-gopls-hover-kind "NoDocumentation")
;;   (setq lsp-rust-server 'rls)
;;   (general-nmap :prefix "\\ l" :prefix-map 'lsp-command-map)
;;   :commands (lsp lsp-deferred)
;;   :hook (python-mode . lsp-deferred)
;;         (ruby-mode . lsp-deferred)
;;         (html-mode . lsp-deferred)
;;         (shell-mode . lsp-deferred)
;;         (julia-mode . lsp-deferred)
;;         (java-mode . lsp-deferred)
;;         (lsp-mode . lsp-enable-which-key-integration)
;;         (rust-mode . lsp-deferred)
;;         )
;; (use-package company-lsp :commands company-lsp)
;; (use-package lsp-ivy
;;   :requires (lsp ivy)
;;   :commands lsp-ivy-workspace-symbol
;;   :config
;;     (define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol)
;;   )

;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))
;; (setq ccls-executable "/opt/ccls/Release/ccls")
;; (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))

;; (use-package ivy-xref
;;   :ensure t
;;   :init
;;   ;; xref initialization is different in Emacs 27 - there are two different
;;   ;; variables which can be set rather than just one
;;   (when (>= emacs-major-version 27)
;;     (setq xref-show-definitions-function #'ivy-xref-show-defs))
;;   ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
;;   ;; commands other than xref-find-definitions (e.g. project-find-regexp)
;;   ;; as well
;;   (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; (use-package lsp-python-ms
;;   :ensure t
;;   :requires lsp
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :config (setq lsp-python-ms-executable
;;       "/opt/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer")
;;   :hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key

(use-package which-key
  :init
    (which-key-setup-minibuffer)
    (which-key-mode)
  :config
    (setq which-key-show-operator-state-maps t)
    (setq which-key-allow-evil-operators t)
  :diminish
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile
(use-package projectile
  :init
    (projectile-mode +1)
    ;; (evil-leader/set-key "p" 'projectile-command-map)
  :config
    (general-nmap :prefix "\\ p" :prefix-map 'projectile-command-map)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel-projectile
(use-package counsel-projectile
  :after projectile
  :init
    (counsel-projectile-mode)
  :diminish
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parens
(use-package paredit
  :init
    (show-paren-mode 1)
  )
(use-package rainbow-delimiters)

;; Highlights matching parenthesis

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree
(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  )

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
;; saveplace
;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)


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
;; For editing lisps
;; Automatically load paredit when editing a lisp file
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My machine specific
(find-file "~/Desktop/scratch/todo/todo.org")
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
 '(TeX-electric-math nil)
 '(TeX-electric-sub-and-superscript nil)
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("93f9654f91d31e9a9ec6ea2fcffcfcab38353a9588673f2b750e591f704cd633" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(debug-on-error t)
 '(display-time-mode t)
 '(evil-undo-system (quote undo-tree))
 '(fzf/executable "/usr/local/bin/fzf")
 '(global-font-lock-mode t)
 '(package-selected-packages
   (quote
    (flycheck powerline rainbow-delimiters web-mode matlab-mode prescient ivy-prescient wolfram-mode undo-tree fireplace fzf lsp-java ivy-xref ccls racer rust-mode web-beautify auctex company-auctex general column-enforce-mode diminish neotree ob-diagrams company-lsp gnu-elpa-keyring-update counsel-tramp tagedit paredit f evil-visualstar evil-visual-replace evil-surround evil-org evil-magit calfw-org)))
 '(python-shell-interpreter "python3")
 '(show-paren-mode t))
