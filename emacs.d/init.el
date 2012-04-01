(require 'cl)

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




