;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  For emacs26-nox (no gui version)      
;;  
;;  oishi@ynl.t.u-tokyo.ac.jp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; package-archivesを上書き
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

;; 初期化
(package-initialize)


;; パッケージ情報の更新
(unless package-archive-contents (package-refresh-contents))

;; インストールするパッケージ
(defvar my/favorite-packages
  '(
    ;;;; for auto-complete
    auto-complete auto-complete-clang

    ;;;; for Japanese
    mozc

    ;;;; explorer
    neotree
    
    ;;;; git
    git-gutter
    ))

;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))



;; (setq my-settings "~/.emacs.d/my_settings.el")
;; (load my-settings t)

;; カラーテーマ
(load-theme 'wombat t)

;; 日本語 UTF-8
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; スタート画面のメッセージを消す
(setq inhibit-startup-message t)

;; バックアップファイルを作らない
(setq make-backup-files nil)

;; 終了時オートセーブファイル削除
(setq delete-auto-save-files t)

;; tab -> space*4
(setq-default tab-width 4 indent-tabs-mode nil)

;; 改行コードの表示
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; ウィンドウの透明化
;; active / not-active (= alpha)
;;(add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))



;; 列数の表示
(column-number-mode t)

;; 行数の表示
;; 4桁分の領域を確保
;; [f6]で行数表示の切り替え
;;(global-linum-mode t)
;;(setq linum-format"%4d ")
;;(global-set-key [f6] 'linum-mode)
(if (version<= "26.0.50" emacs-version)
      (global-display-line-numbers-mode))

;; 対応する()を光らせる
(show-paren-mode 1)

;; space tab の可視化
;; (global-whitespace-mode 1)

;; 複数ウィンドウを禁止する
(setq ns-pop-up-frames nil)


;; スクロール行数
(setq scroll-conservatively 1)
(setq scroll-margin 10)
;;(setq next-screen-context-lines 10)
(setq scroll-preserve-screen-position t)

;; yes, no -> y, n
(fset 'yes-or-no-p 'y-or-n-p)

;; buffer
(global-set-key "\C-x\C-b" 'bs-show)

;; window move shift+矢印
(windmove-default-keybindings)

;; ピープ音
(setq ring-bell-function 'ignore)


;; mozc
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)

;; 非アクティブウィンドウの背景色の変更
;(require 'hiwin)
;(hiwin-activate)
;(set-face-background 'hiwin-face "gray30")

;; 現在ポインタのある関数をモードラインに表示
(which-function-mode 1)

;; リージョンのハイライト
(transient-mark-mode 1)

;; タイトルにフルパス
(setq frame-title-format "%f")

;; current directry の表示
(let ((ls (member 'mode-line-buffer-identification
                  mode-line-format)))
  (setcdr ls
          (cons '(:eval (concat " ("
                                (abbreviate-file-name default-directory)
                                ")"))
                (cdr ls))))

;; auto complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode)

;; elscreen (tab)
;;(require 'elscreen)
;;(elscreen-start)

;; neo tree (side bar)
(require 'neotree)
;; [f5] で切り替え
(global-set-key [f5] 'neotree-toggle)

;; 括弧を閉じる
(electric-pair-mode 1)

;; C-kで行全体を削除する
;;(setq kill-whole-line t)

;; C-h -> backspace
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key key-translation-map (kbd "M-h") (kbd "<C-backspace>"))
;;(define-key key-translation-map (kbd "DEL") (kbd "C-h"))
;;(define-key key-translation-map (kbd "M-DEL") (kbd "M-h"))

;; 空白を一度に削除
(if (fboundp 'global-hungry-delete-mode)
    (global-hungry-delete-mode 1))

;; png, jpg などを表示
(setq auto-image-file-mode t)


;; mで1左, oで1右
;;(define-key key-translation-map (kbd "C-m") (kbd "C-f"))
;;(define-key key-translation-map (kbd "C-o") (kbd "C-b"))

(require 'auto-complete-clang)

;; *scratch* の初期メッセージ
(setq initial-scratch-message "")


;; git-gutter
;; 差分の可視化
(global-git-gutter-mode t)


;; tex
;; (setq tex-settings "~/.emacs.d/tex_settings.el")
;; (load tex-settings t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auto-complete-clang neotree mozc git-gutter auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
