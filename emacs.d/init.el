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
        (auto-install-compatibility-setup)
        (setq auto-install-wget-command "hoge.sh") ;; ?
)


;; ▼要拡張機能インストール▼（ただし、Emacs24からはインストール不要）
;;; P115-116 Emacs Lisp Package Archive（ELPA）──Emacs Lispパッケージマネージャ
;; package.elの設定
(when (require 'package nil t)

  ;; パッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  ;; インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.2 統一したインタフェースでの操作                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ▼要拡張機能インストール▼
;;; P122-129 候補選択型インタフェース──Anything
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.3
   ;; タイプして再描写するまでの時間。デフォルトは0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数。デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補が多いときに体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;; root権限でアクションを実行するときのコマンド
    ;; デフォルトは"su"
    ;; (setq anything-su-or-sudo "sudo")
  )
  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    ;; lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install))

    (define-key global-map (kbd "C-x b") 'anything)
    (define-key global-map (kbd "M-y") 'anything-show-kill-ring)

)

(when (require 'anything-c-moccur nil t)
  (setq
   ;; anything-c-moccur用 `anything-idle-delay'
   anything-c-moccur-anything-idle-delay 0.1
   ;; バッファの情報をハイライトする
   anything-c-moccur-higligt-info-line-flag t
   ;; 現在選択中の候補の位置をほかのwindowに表示する
   anything-c-moccur-enable-auto-look-flag t
   ;; 起動時にポイントの位置の単語を初期パターンにする
   anything-c-moccur-enable-initial-pattern t)
  ;; C-M-oにanything-c-moccur-occur-by-moccurを割り当てる
  (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur)
)


;; ▼要拡張機能インストール▼
;;; P133-134 moccurの結果を直接編集──moccur-edit
;; moccur-editの設定
(require 'moccur-edit nil t)
;; moccur-edit-finish-editと同時にファイルを保存する
;; (defadvice moccur-edit-change-file
;;   (after save-after-moccur-edit-buffer activate)
;;   (save-buffer))

;; ▼要拡張機能インストール▼
;;; P136 grepの結果を直接編集──wgrep
;; wgrepの設定
(require 'wgrep nil t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.3 入力の効率化                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ▼要拡張機能インストール▼
;;; P130-131 利用可能にする
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories 
    "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.9 シェルの利用                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ▼要拡張機能インストール▼
;;; ターミナルの利用 multi-term
;; multi-termの設定
(when (require 'multi-term nil t)
  ;; 使用するシェルを指定
  (setq multi-term-program "/usr/local/bin/zsh"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.8 特殊な範囲の編集                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; P151 矩形編集──cua-mode
;; cua-modeの設定
(cua-mode t) ; cua-modeをオン
(setq cua-enable-cua-keys nil) ; CUAキーバインドを無効にする
;; terminal の為、C-RETが使えない
(define-key global-map (kbd "C-X SPC") 'cua-set-rectangle-mark)



;; ▼要拡張機能インストール▼
;;; P192-193 Anythingとタグの連携
;; AnythingからTAGSを利用しやすくするコマンド作成
(when (and (require 'anything-exuberant-ctags nil t)
           (require 'anything-gtags nil t))
  ;; anything-for-tags用のソースを定義
  (setq anything-for-tags
        (list anything-c-source-imenu
              anything-c-source-gtags-select
              ;; etagsを利用する場合はコメントを外す
              ;; anything-c-source-etags-select
              anything-c-source-exuberant-ctags-select
              ))

  ;; anything-for-tagsコマンドを作成
  (defun anything-for-tags ()
    "Preconfigured `anything' for anything-for-tags."
    (interactive)
    (anything anything-for-tags
              (thing-at-point 'symbol)
              nil nil nil "*anything for tags*"))
  
  ;; M-tにanything-for-currentを割り当て
  (define-key global-map (kbd "M-t") 'anything-for-tags))

