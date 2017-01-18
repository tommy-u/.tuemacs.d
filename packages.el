;;;

(require 'cl)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar required-packages
  '(
    ace-window
    expand-region
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
    hydra
    company
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
;;(key-chord-define-global "

(require 'hydra)

(key-chord-define-global
 "jk"
 (defhydra hydra-move
   (:pre (set-cursor-color "#40e0d0")
	 :post (set-cursor-color "#ffffff"))
   "move"
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)
   ("h" backward-char)
   ("J" (lambda () (interactive) (forward-line  8)))
   ("K" (lambda () (interactive) (forward-line -8)))
   ("L" forward-word)
   ("H" backward-word)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("i" kill-line "kill-ln")
   ("y" yank)
   ("m" set-mark-command)
   ("w" kill-region)
   ("x" er/expand-region "exp")
   ("c" er/contract-region)
   ("o" recenter-top-bottom "center")
   ("u" undo-tree-undo)
   ("f" avy-goto-word-1 :exit t)
   ("r" delete-char )
   ("d" nil "quit")
   ))

(defhydra hydra-org-clock
  (   :pre (set-cursor-color "#bdb76b")
	   :post (set-cursor-color "#ffffff"))
  "clock"
  ("i" org-clock-in :exit t)
  ("o" org-clock-out :exit t)
  ("e" org-set-effort :exit t)
  ("m" org-clock-modify-effort-estimate :exit t)
  ("d" nil "quit" :exit t))

(defhydra hydra-org-agenda
  (   :pre (set-cursor-color "#228b22")
	   :post (set-cursor-color "#ffffff"))
  "agenda"
  ("s" org-schedule :exit t)
  ("d" nil "quit" :exit t))


(key-chord-define-global
 "kl"
 (defhydra hydra-org
   (:pre (set-cursor-color "#cf5300")
	 :post (set-cursor-color "#ffffff"))
   "org"
   ("t" org-todo "todo")
   ("k" org-insert-heading "ins head" :exit t)
   ("K" org-insert-todo-heading :exit t)
   ("f" org-insert-subheading :exit t)
   ("F" org-insert-todo-subheading :exit t)
   ("u" org-metaup "metup")
   ("j" org-metadown)
   ("l" org-do-demote)
   ("h" org-do-promote)
   ("p" org-priority-up "prior up")
   ("n" org-priority-down)
   ("v" org-ctrl-c-ctrl-c :exit t)

   ("s" org-show-todo-tree :exit t)
   ("i" org-todo-list :exit t)

   ;; Nested hydras.
   ("c" hydra-org-clock/body "clock" :exit t)
   ("a" hydra-org-agenda/body "agenda" :exit t)
   ("d" nil "quit")
   ))

(defhydra hydra-ace-window
  (:pre (set-cursor-color "#ff0000")
	:post (set-cursor-color "#ffffff"))
  "ace-window"
  ("w" ace-window "win")
  ("s" ace-swap-window "swap")
  ("j" ace-select-window "sel")
  ("x" ace-delete-window "del")
  ("f" ace-maximize-window "max")
  ("m" ace-window-display-mode "mark wins")
  
  ("d" nil "quit")
  )

(key-chord-define-global
 "dk"
 (defhydra hydra-window (:color red)
   "window mv"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("L" (lambda ()
	  (interactive)
	  (split-window-right)
	  (windmove-right)))
   ("J" (lambda ()
	  (interactive)
	  (split-window-below)
	  (windmove-down)))
   ("v" split-window-right)
   ("x" split-window-below)

   ("w" ace-window "win")
   ("s" ace-swap-window "swap")
   ("x" ace-delete-window "del")
   ("f" ace-maximize-window "max")
   ("m" ace-window-display-mode "mark wins")
   ("d" nil "quit")))



(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'expand-region)
(pending-delete-mode t)


(require 'aggressive-indent)

(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'python-mode)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

(require 'magit)

(define-key global-map (kbd "C-c m") 'magit-status)

;; Maybe this would make a good hydra?
(require 'key-chord)
(setq key-chord-two-keys-delay .05
      key-chord-one-key-delay .18)

(key-chord-mode 1)
(key-chord-define-global "qw"     "~")
(key-chord-define-global "df"     "\C-m") ;;Ret
;;(key-chord-define-global "we"     "\C-i") ;;Tab
(key-chord-define-global " q"     'sr-speedbar-toggle)
(key-chord-define-global "jo"     'delete-backward-char)
(key-chord-define-global "JO"     'backward-kill-word)
(key-chord-define-global "fw"     'delete-forward-char)
(key-chord-define-global "jj"     'compile)
(key-chord-define-global "kk"     'comment-or-uncomment-region)

;;Commonly used fns.
(key-chord-define-global "uu"     'undo-tree-visualize)
(key-chord-define-global "xx"     'helm-M-x)
(key-chord-define-global "cx"     'magit-status)
(key-chord-define-global "DD"     'helm-descbinds)
(key-chord-define-global "yy"     'helm-show-kill-ring)
(key-chord-define-global "vv"     'eval-last-sexp)
(key-chord-define-global "hh"     'finder-by-keyword)
(key-chord-define-global "ww"     'save-buffer)
(key-chord-define-global "IO"     'er/expand-region)

;;Macros
(key-chord-define-global "12"     'kmacro-start-macro)
(key-chord-define-global "90"     'kmacro-end-or-call-macro)
(key-chord-define-global "-="     "\M-0\C-xe") ;;Play to end.

;; Wind-move
(key-chord-define-global "2f"     "\C-x2")
(key-chord-define-global "3f"     "\C-x3")
(key-chord-define-global "1f"     "\C-x0")
(key-chord-define-global "4f"     "\C-xk")

;;Should be replaced by avy.
;;(key-chord-define-global "sd"     'windmove-left)
;;(key-chord-define-global "kl"     'windmove-right)
;;(key-chord-define-global "sf"     'windmove-down)
;;(key-chord-define-global "jl"     'windmove-up)

(require 'avy)
(key-chord-define-global "jf"     'avy-goto-word-1)

(require 'org-install)
(setq org-clock-idle-time 15)

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "DONE")))
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
