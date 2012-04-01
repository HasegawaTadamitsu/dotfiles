(require 'cl)

(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))


;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
     (dolist (path paths paths)
       (let ((default-directory
           (expand-file-name (concat user-emacs-directory path))))
         (add-to-list 'load-path default-directory)
         (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
             (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")



(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(setq inhibit-startup-screen t)

(menu-bar-mode 0)

(when window-system
 (tool-bar-mode 0) 
 (scroll-bar-mode 0) )

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq-default transient-mark-mode t)

(partial-completion-mode 1)
(column-number-mode t)
(line-number-mode t)

(setq scroll-step 1)

;; (global-hl-line-mode)


(define-key global-map (kbd "C-t") 'other-window)


(add-to-list 'backup-directory-alist
 (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
 `((".*",(expand-file-name "~/.emacs.d/backups/") t)))


(when (eq window-system `w32)
  (set-file-name-coding-system `cp932)
  (setq local-coding-system `cp932))


(when (eq window-system 'ns)
  ;; asciiフォントをMenloに
  (set-face-attribute 'default nil
                      :family "Menlo"
                      :height 120)
  ;; 日本語フォントをヒラギノ明朝 Proに
  (set-fontset-font
       nil 'japanese-jisx0208
       ;; 英語名の場合
       ;; (font-spec :family "Hiragino Mincho Pro"))
       (font-spec :family "ヒラギノ明朝 Pro"))
      ;; ひらがなとカタカナをモトヤシーダに
      ;; U+3000-303FCJKの記号および句読点
      ;; U+3040-309Fひらがな
      ;; U+30A0-30FFカタカナ
        (set-fontset-font
            nil '(#x3040 . #x30ff)
            (font-spec :family "NfMotoyaCedar"))
      ;; フォントの横幅を調節する
      (setq face-font-rescale-alist
         '((".*Menlo.*" . 1.0)
          (".*Hiragino_Mincho_Pro.*" . 1.2)
          (".*nfmotoyacedar-bold.*" . 1.2)
          (".*nfmotoyacedar-medium.*" . 1.2)
          ("-cdac$" . 1.3))))

(when (eq system-type 'windows-nt)
    ;; asciiフォントをConsolasに
    (set-face-attribute 'default nil
           :family "Consolas"
           :height 120)
      ;; 日本語フォントをメイリオに
      (set-fontset-font
          nil
         'japanese-jisx0208
         (font-spec :family "メイリオ"))
      ;; フォントの横幅を調節する
        (setq face-font-rescale-alist
            '((".*Consolas.*" . 1.0)
             (".*メイリオ.*" . 1.15)
             ("-cdac$" . 1.3))))



;; auto-installの設定
(when (require 'auto-install nil t)
    ;; 2●インストールディレクトリを設定する 初期値は ~/.emacs.d/auto-install/
    (setq auto-install-directory "~/.emacs.d/elisp/")
      ;; EmacsWikiに登録されているelisp の名前を取得する
      (auto-install-update-emacswiki-package-name t)
        ;; 必要であればプロキシの設定を行う
        ;; (setq url-proxy-services '(("http" . "localhost:8339")))
        ;; 3●install-elisp の関数を利用可能にする
        (auto-install-compatibility-setup))
