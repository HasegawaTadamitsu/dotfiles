(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(setq inhibit-startup-screen t)

(menu-bar-mode 0)

(cond
 ((eq window-system nil)
 (tool-bar-mode -1) 
)

(setq-default indent-tabs-mode nil)
(setq-default transient-mark-mode t)

(partial-completion-mode 1)
(column-number-mode t)
(line-number-mode t)

(setq scroll-step 1)

(define-key global-map (kbd "C-t") 'other-window)


(add-to-list 'backup-directory-alist
 (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
 `((".*",(expand-file-name "~/.emacs.d/backups/") t)))


(when (eq window-system `w32)
  (set-file-name-coding-system `cp932)
  (setq local-coding-system `cp932))

(add-to-list 'load-path "~/.emacs.d/elips")