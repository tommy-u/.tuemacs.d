;;;

(require 'cl)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar required-packages
  '(
    nyan-mode
    avy
    aggressive-indent
    monokai-theme
    magit
    key-chord
    multiple-cursors
    helm
    helm-swoop
    helm-gtags
    helm-flyspell
    helm-descbinds
    diff-hl
    helm-projectile 
    sr-speedbar
    default-text-scale
    flycheck
    undo-tree
    ) "A list of packages to ensure are installed at launch.")

(defun packages-installed-p ()
  "Method to check if all packages are installed."
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

(require 'aggressive-indent)

(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
(add-to-list 'aggressive-indent-excluded-modes 'python-mode)

(require 'magit)

(define-key global-map (kbd "C-c m") 'magit-status)

;; Maybe this would make a good hydra?
(require 'key-chord)
(setq key-chord-two-keys-delay .05
      key-chord-one-key-delay .020)

(key-chord-mode 1)
(key-chord-define-global "qw"     "~")
(key-chord-define-global "df"     "\C-m") ;;Ret
(key-chord-define-global "we"     "\C-i") ;;Tab
(key-chord-define-global " q"     'sr-speedbar-toggle)

;;Macros
(key-chord-define-global "12"     'kmacro-start-macro)
(key-chord-define-global "90"     'kmacro-end-or-call-macro)
(key-chord-define-global "-="     "\M-0\C-xe") ;;Play to end.

;; Wind-move
(key-chord-define-global "2f"     "\C-x2")
(key-chord-define-global "3f"     "\C-x3")
(key-chord-define-global "1f"     "\C-x0")
(key-chord-define-global "4f"     "\C-xk")
(key-chord-define-global "sd"     'windmove-left)
(key-chord-define-global "kl"     'windmove-right)
(key-chord-define-global "sf"     'windmove-down)
(key-chord-define-global "jl"     'windmove-up)

(require 'avy)
(key-chord-define-global "jk"     'avy-goto-word-1)

(require 'org-install)
(setq org-hide-leading-stars t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . true) (python . true))
 )

(setq org-src-fontify-natively t)

;; Enable some mouse support--
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
  )

(require 'helm)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal

;;Cleanup helm sizing.
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)

(require 'helm-gtags)

(require 'diff-hl)
(global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(require 'helm-swoop)
(global-set-key (kbd "C-s") 'helm-swoop)

(require 'helm-flyspell)
(key-chord-define-global "ji"     'helm-flyspell-correct) ;;Spelling
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(require 'helm-projectile)

(global-set-key (kbd "C-h f") 'helm-apropos)
(setq projectile-completion-system 'helm)

;; enable Helm version of Projectile with replacement commands
(helm-projectile-on)


(require 'default-text-scale)

(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)
(default-text-scale-increase)
(default-text-scale-increase)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'find-file-hook
          (lambda ()
            (unless (tramp-tramp-file-p (buffer-file-name))
              (flycheck-mode))))

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

(provide 'packages)
;;; packages.el ends here
