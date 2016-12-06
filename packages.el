;;;

(require 'cl)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar required-packages
  '(
    yasnippet
    monokai-theme
    doremi
    doremi-cmd
    color-theme
    iedit
    evil
    magit
    key-chord
    multiple-cursors
    helm
    helm-swoop
    helm-gtags
    helm-flyspell
    sr-speedbar
    xcscope
    company
    default-text-scale
    flycheck
    company
    undo-tree
    ) "a list of packages to ensure are installed at launch.")

;; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ;; check for new packages (package versions)

  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;loading packages
(require 'doremi)

(require 'color-theme)

(require 'magit)

(define-key global-map (kbd "C-c m") 'magit-status)

(require 'key-chord)
(setq key-chord-two-keys-delay .05
      key-chord-one-key-delay .020)

(key-chord-mode 1)
(key-chord-define-global "qw"     "~") ;;
(key-chord-define-global "df"     "\C-m") ;;
(key-chord-define-global "jk"     "\C-m") ;;Ret
;;(key-chord-define-global "io"     "\C-i") ;;
(key-chord-define-global "we"     "\C-i") ;;Tab



(key-chord-define-global " q"     'sr-speedbar-toggle)

;;Macros
(key-chord-define-global "12"     'kmacro-start-macro)
(key-chord-define-global "90"     'kmacro-end-or-call-macro)
(key-chord-define-global "-="     "\M-0\C-xe") ;;Play to end.
;;(key-chord-define-global "=1"     "") ;;Find way to undo all.

;; Wind-move
(key-chord-define-global "2f"     "\C-x2")
(key-chord-define-global "3f"     "\C-x3")
(key-chord-define-global "1f"     "\C-x0")
(key-chord-define-global "4f"     "\C-xk")
(key-chord-define-global "sd"     'windmove-left)
(key-chord-define-global "kl"     'windmove-right)
(key-chord-define-global "sf"     'windmove-down)
(key-chord-define-global "jl"     'windmove-up)

;;Org
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(require 'org-install)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . true) (python . true))
 )

(setq org-src-fontify-natively t)



;; Enable mouse support--
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
			      (interactive)
			      (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
			      (interactive)
			      (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  ;;(setq select-active-regions nil)
  ;;(setq mouse-drag-copy-region t)
  )


(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(global-set-key (kbd "C-x b") 'helm-buffers-list) ; make Replace buffer list
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ; make Replace buffer list

(require 'helm-swoop)
(global-set-key (kbd "M-s") 'helm-swoop)

(require 'helm-gtags)

(require 'xcscope)

(require 'company)

(require 'default-text-scale)

(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'tramp)
(add-hook 'find-file-hook
          (lambda ()
            (unless (tramp-tramp-file-p (buffer-file-name))
              (flycheck-mode))))

(setq org-hide-leading-stars t)

(require 'yasnippet)
(yas-reload-all)

(add-hook 'c-mode-common-hook #'yas-minor-mode)
(provide 'init-yasnippet)

(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key [backtab] 'tab-indent-or-complete)


(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;turn on everywhere
(global-undo-tree-mode 1)
;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)

(require 'helm-flyspell)
(key-chord-define-global "ji"     'helm-flyspell-correct) ;;Spelling
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

