;;Configuration of vanilla emacs. No external package.

(package-initialize)

(setq backup-directory-alist
      `((".*" . ,"~/.emacs_backups")))

(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs_backups" t)))

;; Should do this once a day. 
(setq confirm-kill-emacs 'y-or-n-p)

;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;Resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-j") 'keyboard-quit)

;;Compile.
(global-set-key (kbd "C-c j") 'compile)

;;Show selected text.
(setq-default transient-mark-mode t)

;;Package Management
(load "~/.tuemacs.d/packages.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f81a9aabc6a70441e4a742dfd6d10b2bae1088830dc7aba9c9922f4b1bd2ba50" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(package-selected-packages
   (quote
    (monokai-theme solarized-theme doremi yasnippet magit color-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'monokai t)

;;Note this is no good in terminal mode.
(global-set-key (kbd "C-S-n")
		(lambda () (interactive) (forward-line  7)))
(global-set-key (kbd "C-S-p")
		(lambda () (interactive) (forward-line -7)))

;;Modifiers
(when (eq system-type 'darwin)  ; mac specific bindings
  (setq mac-right-command-modifier 'meta));Right command is meta

;;Allows closing all gud buffers.
(defvar all-gud-modes
  '(gud-mode comint-mode gdb-locals-mode gdb-frames-mode  gdb-breakpoints-mode)
  "A list of modes when using gdb")
(defun kill-all-gud-buffers ()
  "Kill all gud buffers including Debugger, Locals, Frames, Breakpoints.
Do this after `q` in Debugger buffer."
  (interactive)
  (save-excursion
        (let ((count 0))
          (dolist (buffer (buffer-list))
                (set-buffer buffer)
                (when (member major-mode all-gud-modes)
                  (setq count (1+ count))
                  (kill-buffer buffer)
                  (delete-other-windows))) ;; fix the remaining two windows issue
          (message "Killed %i buffer(s)." count))))

(setq doc-view-continuous t)


;;scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;;(setq scroll-step 1) ;; keyboard scroll one line at a time


(show-paren-mode 1)
(setq show-paren-delay 0)

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
	 (matching-text (and cb
			     (char-equal (char-syntax cb) ?\) )
			     (blink-matching-open))))
    (when matching-text (message matching-text))))

;;If it ain't broke ...
;;When in minibuffer, don't garbage collect.
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

;;Until you leave it.
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(provide 'init)
;;; init.el ends here
