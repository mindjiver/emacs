;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs configuration file
;;
;; Peter Joensson <peter.joensson@gmail.com>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some nice hotkeys for frequently used stuff.
;; Courtesy of Mr. Hallin

;; qostjoa
;; Workaround for 5 sec lag when running set-default-font
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; F9-F12 does not work on OS X due to expose and the
;; eject button
(global-set-key [f1] 'find-file)
(global-set-key [(shift f1)] 'save-buffer)
(global-set-key [f2] 'other-window)
(global-set-key [f2] 'switch-to-buffer)
(global-set-key [(control f2)] 'switch-to-buffer)

(global-set-key [f3] 'split-window-vertically)
(global-set-key [(shift f3)] 'split-window-horizontally)
(global-set-key [f4] 'delete-window)
(global-set-key [(shift f4)] 'kill-buffer)

(global-set-key [f5] 'set-mark-command)
(global-set-key [f6] 'copy-region-as-kill)
(global-set-key [(shift f6)] 'kill-region)
(global-set-key [(control f6)] 'delete-region)

(global-set-key [f7] 'yank)
(global-set-key [f8] 'undo)

(global-set-key [f9] 'comment-region)
(global-set-key [(shift f9)] 'uncomment-region)
(global-set-key [f10] 'isearch-forward)

(global-set-key [f11] 'gdb)
(global-set-key [f12] 'compile)
(global-set-key [(shift f12)] 'compile-goto-error)

;; No annoying messages at startup, thank you very much.
(setq inhibit-default-init t)
(setq inhibit-startup-message t)

;; Highlighting is good, though...
(global-font-lock-mode t)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Stop creating annoying backups
(setq make-backup-files nil)

;; Misc. things
(setq truncate-partial-width-windows nil)
(setq enable-recursive-minibuffers   t)
(setq line-number-mode               t)
(setq column-number-mode             t)
(setq find-file-visit-truename       t)
(setq search-highlight               t)
(setq query-replace-highlight        t)
(auto-compression-mode               1)
;;(auto-show-mode                      1)
(setq auto-save-list-file-name nil)
(setq cast-fold-search t)
(tool-bar-mode nil)

;; And prettify all frames.
(setq default-frame-alist
      '(
;;        (font . "-B&H-LucidaTypewriter-Medium-R-Normal-Sans-10-120-75-75-M-70-ISO8859-1")
        (font . "-B&H-LucidaTypewriter-Medium-R-Normal-Sans-12-*-*-*-*-*-*-*")
;;        (font . "-B&H-LucidaTypewriter-Medium-R-Normal-Sans-10-100-75-75-M-60-ISO8869-1")
;;	(font . "terminus")
	))

;; add directories to load path
(setq load-path (cons "~/emacs/" load-path))

;; load erlang support
;;(setq load-path (cons "/proj/swdi/tools/lib/tools-2.6.6.4/emacs" load-path))
;;(setq erlang-root-dir "/proj/swdi/tools/")
;;(setq exec-path (cons "/usr/swdi/tools//otp/bin" exec-path))
;;(require 'erlang-start)

;;(require 'php-mode)

;; load print package
(require 'a2ps-print)
(define-key function-key-map [f22] [print])   ;display f22 as print (screen)
(global-set-key [print] 'a2ps-buffer)
(global-set-key [(shift print)] 'a2ps-region) ;print selected text

;;Emacs menu
(define-key global-map [menu-bar files a2ps-buffer]
  '("a2ps Print Buffer" . a2ps-buffer))

;; load the color-theme package and select a nice color theme
(require 'color-theme)
;;(color-theme-classic)
;;(color-theme-clarity)
;;(color-theme-subtle-hacker)
;;(color-theme-dark-blue2)
(color-theme-ld-dark)
(color-theme-salmon-font-lock)

;; load speed bar on startup
;; (speedbar t)

;; load imenu and menu index for TCL files
(require 'imenu)
(add-hook 'tcl-mode-hook 'imenu-add-menubar-index)

;; load php support
;;(require 'php-mode)

;; use TCL mode
(autoload 'tcl-mode "tcl" "Tcl mode." t)
(autoload 'groovy-mode "groovy-mode" "Groovy mode." t)
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)

;; set some information at the bottom of the screen
(setq column-number-mode t
      line-number-mode t
      display-time-24hr-format t
      mouse-yank-at-point t
      inhibit-startup-message t
      display-time-day-and-date t
      next-line-add-newlines nil
      tab-width 3)
(delete-selection-mode t)
(display-time)

;; display the filename in the title bar
(setq frame-background-mode nil
      column-number-mode t
      show-paren-mode nil
      frame-title-format (concat invocation-name "@" (system-name) " [%f]"))

;; indent the way Linus wants it!
;; was 8 here before.
;;(setq-default c-basic-offset 8)
;;(setq c-default-style "K&R")

;; PEP 8 settings.
(setq-default python-basic-offset 4)
(setq tab-width 4)
(setq default-tab-width 4)

;; indent TCL/Expect the way Liz wants it!
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; remove annoying emacs21 stuff
(setq emacs21 (> emacs-major-version 21))
(when emacs21
	(blink-cursor-mode -1)
	(tool-bar-mode -1)
	(global-set-key [home] 'beginning-of-buffer)
	(global-set-key [end] 'end-of-buffer)
	(setq rmail-confirm-expunge nil)
	(setq show-paren-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Settings for different programming languages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auctex-mode
(setq TeX-auto-save t) 
(setq TeX-parse-self t) 
(setq-default TeX-master nil) 

;; Haskell-mode
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)

;; use c-mode for Tiny-OS files
(setq auto-mode-alist (cons '("\\.nc$" . c-mode) auto-mode-alist))

;; use c-mode for obelix files
(setq auto-mode-alist (cons '("\\.obx$" . c-mode) auto-mode-alist))

;; use c-mode for .spec files
(setq auto-mode-alist (cons '("\\.spec$" . c-mode) auto-mode-alist))

;; use c-mode for trac42 files
(setq auto-mode-alist (cons '("\\.t42$" . c-mode) auto-mode-alist))

;; use sh-mode for .moshellrc
(setq auto-mode-alist (cons '("\\.moshellrc" . sh-mode) auto-mode-alist))

;; use sh-mode for ATest config
(setq auto-mode-alist (cons '("\\config" . sh-mode) auto-mode-alist))

;; use sh-mode for mos files
(setq auto-mode-alist (cons '("\\.mos$" . sh-mode) auto-mode-alist))

;; use sh-mode for mos files
(setq auto-mode-alist (cons '("\\.spec$" . makefile-mode) auto-mode-alist))

;; use groovy mode for groovy files 
(setq auto-mode-alist (cons '("\\.groovy$" . groovy-mode) auto-mode-alist))

;; use ruby mode for ruby files 
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))

;; use ruby mode for ruby files 
(setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))

;; set sh-mode for config files
(setq auto-mode-list (cons '("\\.config$" . sh-mode) auto-mode-alist))
(custom-set-variables
 '(load-home-init-file t t)
 '(ps-paper-type (quote a4))
 '(ps-print-color-p nil))
(custom-set-faces)

(set-background-color "black")

(put 'upcase-region 'disabled nil)
(setq load-home-init-file t) ; don't load init file from ~/.xemacs/init.el

(put 'downcase-region 'disabled nil)
