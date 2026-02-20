;; -*- mode: Emacs-Lisp -*-

(global-set-key "\C-h" 'backward-delete-char)
(global-unset-key "\C-o" )
;; use C-q as going to the matching parenthesis
(require 'paren)
(show-paren-mode 1)
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        )
  )
(global-set-key "\C-Q" 'match-paren)

(setq visible-bell t)
(setq ring-bell-function 'ignore)
(line-number-mode t)
(column-number-mode t)

(when t
  ;; does not allow use hard tab.
  (setq-default indent-tabs-mode nil)
  )

;; Japanese
;; uncommented by ueda. beacuse in shell buffer, they invokes mozibake
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(setq enable-double-n-syntax t)

;; shell mode
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq explicit-shell-file-name shell-file-name)
(setq shell-command-option "-c")
(setq system-uses-terminfo nil)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; add color space,tab,zenkaku-space
(unless (and (boundp '*do-not-show-space*) *do-not-show-space*)
  (defface my-face-b-1 '((t (:background "gray"))) nil)
  (defface my-face-b-2 '((t (:background "red"))) nil)
  (defface my-face-u-1 '((t (:background "red"))) nil)
  (defvar my-face-b-1 'my-face-b-1)
  (defvar my-face-b-2 'my-face-b-2)
  (defvar my-face-u-1 'my-face-u-1)

  (defadvice font-lock-mode (before my-font-lock-mode ())
    (font-lock-add-keywords
     major-mode
     '(
       ("\t" 0 my-face-b-1 append)
       ("　" 0 my-face-b-2 append)
       ("[ \t]+$" 0 my-face-u-1 append)
       )))
  (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
  (ad-activate 'font-lock-mode))

(setq auto-mode-alist (cons (cons "\\.launch$" 'xml-mode) auto-mode-alist))

;;sugikazu75
(setq scroll-conservatively 1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-auto-revert-mode 1)
(setq vc-follow-symlinks nil)
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "bash")

(set-face-background 'mode-line "brightred")
(set-face-foreground 'mode-line "gray95")
(global-set-key (kbd "<M-right>") 'windmove-right)
(global-set-key (kbd "<M-left>") 'windmove-left)
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)

;; package settings
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; rosemacs
(defun get-ubuntu-version ()
  "Get Ubuntu version."
  (let ((version-string (shell-command-to-string "lsb_release -r -s")))
    (string-to-number (car (split-string version-string "\\.")))))
(defun rosemacs-add-to-load-path ()
  "Check Ubuntu version and perform actions accordingly."
  (let ((ubuntu-version (get-ubuntu-version)))
    (cond
     ((= ubuntu-version 24)
      (add-to-list 'load-path "/opt/ros/one/share/emacs/site-lisp"))
     ((= ubuntu-version 22)
      (add-to-list 'load-path "/opt/ros/one/share/emacs/site-lisp"))
     ((= ubuntu-version 20)
      (add-to-list 'load-path "/opt/ros/noetic/share/emacs/site-lisp"))
     ((= ubuntu-version 18)
      (add-to-list 'load-path "/opt/ros/melodic/share/emacs/site-lisp"))
     (t
      (message "Your Ubuntu version is too old.")))))
(rosemacs-add-to-load-path)
(require 'rosemacs-config)

;; xclip sharing emacs buffer and system buffer
(straight-use-package 'xclip)
(xclip-mode 1)

;; company: intelisence for emacs
(straight-use-package 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 3) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
(define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設定
(define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
(define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fで候補を設定
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う

;; diff-hl
(straight-use-package 'diff-hl)
;; diff-hl-margin-local-mode が実行された直後に幅を上書きする関数
(defun my-override-diff-hl-margin-width (&rest _args)
  (when diff-hl-margin-local-mode
    ;; diff-hl-side（left か right）に合わせて変数を特定
    (let ((width-var (intern (format "%s-margin-width" diff-hl-side))))
      ;; set width to 2
      (set width-var 2)
      ;; ウィンドウの表示を更新して反映させる
      (dolist (win (get-buffer-window-list))
        (set-window-buffer win (current-buffer))))))
;; diff-hl-margin-local-mode の後ろ（:after）にこの処理を引っ掛ける
(advice-add 'diff-hl-margin-local-mode :after #'my-override-diff-hl-margin-width)

(custom-set-faces
 '(diff-hl-change ((t (:foreground "cyan" :background "cyan"))))
 '(diff-hl-insert ((t (:foreground "green" :background "green"))))
 '(diff-hl-delete ((t (:foreground "red" :background "red")))))
(setq diff-hl-margin-symbols-alist
      '((insert . "  ")
        (delete . "  ")
        (change . "  ")
        (unknown . "  ")
        (ignored . "  ")))
(global-diff-hl-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(unless (window-system) (diff-hl-margin-mode))
(diff-hl-flydiff-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; magit
(straight-use-package 'magit)

;; copilot
(straight-use-package
 '(copilot :host github
           :repo "sugikazu75/copilot.el"
           :files ("*.el")))
(add-hook 'prog-mode-hook 'copilot-mode)
(with-eval-after-load 'copilot
  (define-key copilot-completion-map (kbd "C-m") 'copilot-accept-completion))
;; (setq copilot-lsp-settings '(:github-enterprise (:uri "https://example2.ghe.com"))) ;; alternatively
