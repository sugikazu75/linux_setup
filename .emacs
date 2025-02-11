;; -*- mode: Emacs-Lisp -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; automaticaly install required package https://qiita.com/sijiaoh/items/057b682dd29fbbdadd52
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; automatically install required package and add setting if install is successed
(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))


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


;; rosemacs
(defun get-ubuntu-version ()
  "Get Ubuntu version."
  (let ((version-string (shell-command-to-string "lsb_release -r -s")))
    (string-to-number (car (split-string version-string "\\.")))))
(defun rosemacs-add-to-load-path ()
  "Check Ubuntu version and perform actions accordingly."
  (let ((ubuntu-version (get-ubuntu-version)))
    (cond
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

;; tex (yatex)
(when (maybe-require-package 'yatex)
  (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
  (setq auto-mode-alist
        (append '(("\\.tex$" . yatex-mode)
                  ("\\.ltx$" . yatex-mode)
                  ("\\.sty$" . yatex-mode)) auto-mode-alist))
  (require-package 'company)
  (setq YaTeX-kanji-code 4) ; UTF-8 の設定
  (add-hook 'yatex-mode-hook
            '(lambda ()
               (setq YaTeX-use-AMS-LaTeX t) ; align で数式モードになる
               (setq YaTeX-use-hilit19 nil
                     YateX-use-font-lock t)
               (setq tex-command "em-latexmk.sh") ; typeset command
               (setq dvi2-command "evince") ; preview command
               (setq tex-pdfview-command "xdg-open"))) ; preview command
  (setq YaTeX-inhibit-prefix-letter t)
  )

;; xclip sharing emacs buffer and system buffer
(when (maybe-require-package 'xclip)
  (xclip-mode 1)
  )

;; company: intelisence for emacs
(when (maybe-require-package 'company)
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
  )

;; git-gutter
(add-to-list 'load-path "~/.emacs.d/manual-packages/git-gutter")
(require 'git-gutter)
(global-git-gutter-mode 1)
(setq git-gutter:added-sign "  ")
(setq git-gutter:deleted-sign "  ")
(setq git-gutter:modified-sign "  ")
(setq git-gutter:unchanged-sign "  ")
(set-face-background 'git-gutter:added "green")
(set-face-background 'git-gutter:deleted "red")
(set-face-background 'git-gutter:modified "cyan")
(set-face-background 'git-gutter:unchanged "black")

;; magit
(add-to-list 'load-path "~/.emacs.d/manual-packages/compat")
(add-to-list 'load-path "~/.emacs.d/manual-packages/magit/lisp")
(add-to-list 'load-path "~/.emacs.d/manual-packages/transient/lisp")
(add-to-list 'load-path "~/.emacs.d/manual-packages/with-editor/lisp")
(add-to-list 'load-path "~/.emacs.d/manual-packages/dash.el")
(require 'compat)
(require 'magit)
