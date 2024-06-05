;;; init.el --- init.el with leaf  -*- lexical-binding: t; -*-
;; Author: Shunsuke Honda <syskbks11@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.3"))
;; Keywords: init.el, leaf, emacs

;;; Commentary:
;; This is my init.el file with leaf package manager.

;;; Code:

;; Measure startup time (see "*Messages*" for performance)
(defconst my/before-load-init-time (current-time))

(defun my/load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((time1 (float-time
                (time-subtract after-init-time my/before-load-init-time)))
        (time2 (float-time
                (time-subtract (current-time) my/before-load-init-time))))
    (message (concat "Loading init files: %.0f [msec], "
                     "of which %.f [msec] for `after-init-hook'.")
             (* 1000 time1) (* 1000 (- time2 time1)))))
(add-hook 'after-init-hook #'my/load-init-time t)

(defun my/emacs-init-time ()
  "Emacs booting time in msec."
  (interactive)
  (let ((time1 (float-time
                (time-subtract after-init-time before-init-time))))
    (message "Emacs booting time: %.0f [msec] = `emacs-init-time'."
             (* 1000 time1))))
(add-hook 'after-init-hook #'my/emacs-init-time)

;; Disable magic file name during startup (for speedup)
(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable garbage collection during startup (for speedup)
(setq gc-cons-threshold most-positive-fixnum)

;; Remove Scratch buffer
(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*"))

;; This enables this running method
;;  emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

;; ;; NativeCompile setup (which needs to assign the option by yourself when compiling emacs)
;; (with-eval-after-load 'comp
;;   (setq native-comp-async-jobs-number 8)
;;   (setq native-comp-speed 3))
;; (native-compile-async "~/.emacs.d/init.el")

;; Installation leaf
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  ;; (when (version< emacs-version "27.0")
  ;;   (require 'package)
  ;;   (package-initialize))
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (leaf leaf
    :ensure t
    )
  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    (leaf diminish :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)
    )
  )

;; leaf setups >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;; (leaf leaf-tree
;;   :doc "show leaf package blocks in side bar"
;;   :ensure t
;;   :custom
;;   (imenu-list-size . 0.3)
;;   (imenu-list-position . 'left)
;;   :bind
;;   ("<f8>" . imenu-list-smart-toggle)
;;   )

;; (leaf leaf-convert
;;   :doc "package to convert use-package to leaf"
;;   :ensure t
;;   )

;; (leaf macrostep
;;   :doc "interactively expanding and editing Emacs Lisp macros"
;;   :ensure t
;;   :bind
;;   ("C-c e" . macrostep-expand)
;;   )

;; Remove from modeline
(leaf eldoc
  :diminish t)

;; Keybinds
(leaf-keys
 (("C-h" . delete-backward-char)
  ("C-z" . nil)
  ("C-c \\" . hideshow-toggle-invisible)
  ("C-c ;" . comment-dwim)
  ("C-c r" . restart-emacs)
  ("C-c l" . global-display-line-numbers-mode)
  ("C-c i" . highlight-indent-guides-mode)
  ("C-c s" . copilot-mode)
  ))

;; Configurations called during emacs initialization
;; This affects Emacs globally in any modes (-> affects starting time duration).
(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :custom
  (user-full-name . "Shunsuke Honda")
  (user-mail-address . "syskbks11@gmail.com")
  (user-login-name . "shonda")
  (menu-bar-mode . nil)    ;; hide menu bar
  (tool-bar-mode . nil)    ;; hide tool bar
  (scroll-bar-mode . nil)  ;; hide scroll bar
  (inhibit-startup-screen . t)
  (inhibit-startup-message . t)
  (inhibit-splash-screen . t)
  (inhibit-startup-echo-area-message . t)  (initial-scratch-message . nil)
  )

(leaf basic
  :doc "define basic settings in emacs"
  :custom
  (cua-enable-cua-keys . nil)  ;; disable cua keybinds
  (tab-width           . 2)    ;; space width to be replaed from tab
  (use-short-answers   . t)    ;; use "(y or n)"
  (require-final-newline . t)  ;; add empty line at the last row
  (scroll-margin . 5)          ;; lines to be left when starting to scroll
  (scroll-step . 1)            ;; lines to be scrolled at once
  (indent-tabs-mode . nil)     ;; tab --> space
  (truncate-lines . nil)       ;; wrap line if t / truncate line if nil (C-x x t)
  (read-buffer-completion-ignore-case . t)    ;; buffer completion doesn't care capital/small cases
  (read-file-name-completion-ignore-case . t) ;; filename completion doesn't care capital/small cases
  :config
  (indent-tabs-mode    nil)            ;; no indent with tab
  (global-display-line-numbers-mode t) ;; show line numbers on left
  (column-number-mode t)               ;; show (row,col) in mode line
  (cua-mode t)                         ;; enable rectangle-selection
  (electric-pair-mode t)               ;; automatically write bracket pair
  (which-function-mode t)              ;; show current function name
  (prefer-coding-system 'utf-8)        ;; coding
  (set-default-coding-systems 'utf-8)  ;; coding
  (set-terminal-coding-system 'utf-8)  ;; coding
  ;; (set-face-foreground 'font-lock-comment-face "#E58A8B") ;; C98F4F
  ;; (set-face-foreground 'font-lock-keyword-face "#5CC4F8")
  ;; (set-face-foreground 'font-lock-function-name-face "#DAB352")
  (custom-set-faces
   '(line-number ((t (:foreground "dim grey" :background "unspecified-bg" :italic t))))
   '(line-number-current-line ((t (:foreground "#7FFFFF" :background "unspecified-bg" :italic t :bold t)))))
  )

(leaf files
  :doc "backup file input and output commands for Emacs"
  :custom
  (auto-save-timeout . 15)
  (auto-save-interval . 60)
  `(auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))
  `(auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
  `(backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                               (,tramp-file-name-regexp . nil)))
  (version-control . t)
  (delete-old-versions . t)
  )

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom
  (auto-revert-interval . 0.1)
  :global-minor-mode global-auto-revert-mode
  )

(leaf savehist
  :doc "keep file-access history"
  :ensure t
  :custom
  (savehist-file . "~/.emacs.d/savehist")
  (savehist-additional-variables . '(search-ring regexp-search-ring))
  (savehist-autosave-interval . 60) ;; save every minute
  :config
  (savehist-mode 1) ;; enable savehist mode
  )

(leaf electric
  :ensure t
  :hook (text-hook . (lambda () (electric-indent-local-mode -1))))

(leaf paren
  :custom
  (show-paren-style  . 'mixed)
  :hook
  (after-init-hook . show-paren-mode)
  )

(leaf highlight-indent-guides
  :ensure t
  :diminish t
  :hook (((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode))
  :custom (
           (highlight-indent-guides-method . 'character)
           (highlight-indent-guides-auto-enabled . t)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-character . ?\⋮)
           )
  :config
  (set-face-background 'highlight-indent-guides-odd-face "lightgray")
  (set-face-background 'highlight-indent-guides-even-face "lightgray")
  (set-face-foreground 'highlight-indent-guides-character-face "lightgray")
  )

(leaf whitespace
  :doc "highlight unnecessary spaces and tabs"
  :ensure t
  :diminish t
  :custom
  (whitespace-style . '(face spaces tabs trailing empty space-mark tab-mark))
  (whitespace-display-mappings . '((space-mark ?\u3000 [?\u25a1])
                                   (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (whitespace-space-regexp . "\\(\u00A0+\\)")
  :hook
  (prog-mode-hook . whitespace-mode) ; Enable whitespace-mode in programming modes
  :config
  (set-face-attribute 'whitespace-trailing nil
                      :background "#b14770"
                      :foreground "#b14770"
                      :underline nil)
  (set-face-attribute 'whitespace-tab nil
                      :background "unspecified"
                      :foreground "LightSkyBlue"
                      :underline t)
  (set-face-attribute 'whitespace-space nil ;; This is not applied somehow
                      :background "yellow green"
                      :foreground "yellow green"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                      :background "#b14770")
  )

(leaf powerline
  :ensure t
  :config
  (powerline-default-theme))

(leaf tramp
  :ensure t)

;; ;; Better interface of mini-buffer + completion
;; (leaf ivy
;;   :doc "Incremental Vertical completYon"
;;   :req "emacs-24.5"
;;   :tag "matching" "emacs>=24.5"
;;   :url "https://github.com/abo-abo/swiper"
;;   :emacs>= 24.5
;;   :ensure t
;;   :blackout t
;;   :leaf-defer nil
;;   :custom ((ivy-initial-inputs-alist . nil)
;;            (ivy-use-selectable-prompt . t))
;;   :global-minor-mode t
;;   :config
;;   (leaf swiper
;;     :doc "Isearch with an overview. Oh, man!"
;;     :req "emacs-24.5" "ivy-0.13.0"
;;     :tag "matching" "emacs>=24.5"
;;     :url "https://github.com/abo-abo/swiper"
;;     :emacs>= 24.5
;;     :ensure t
;;     :bind (("C-s" . swiper)))
;;   (leaf counsel
;;     :doc "Various completion functions using Ivy"
;;     :req "emacs-24.5" "swiper-0.13.0"
;;     :tag "tools" "matching" "convenience" "emacs>=24.5"
;;     :url "https://github.com/abo-abo/swiper"
;;     :emacs>= 24.5
;;     :ensure t
;;     :blackout t
;;     :bind (("C-S-s" . counsel-imenu)
;;            ("C-x C-r" . counsel-recentf))
;;     :custom `((counsel-yank-pop-separator . "\n----------\n")
;;               (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
;;     :global-minor-mode t))
;; (leaf prescient
;;   :doc "Better sorting and filtering"
;;   :req "emacs-25.1"
;;   :tag "extensions" "emacs>=25.1"
;;   :url "https://github.com/raxod502/prescient.el"
;;   :emacs>= 25.1
;;   :ensure t
;;   :custom ((prescient-aggressive-file-save . t))
;;   :global-minor-mode prescient-persist-mode)
;; (leaf ivy-prescient
;;   :doc "prescient.el + Ivy"
;;   :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
;;   :tag "extensions" "emacs>=25.1"
;;   :url "https://github.com/raxod502/prescient.el"
;;   :emacs>= 25.1
;;   :ensure t
;;   :after prescient ivy
;;   :custom ((ivy-prescient-retain-classic-highlighting . t))
;;   :global-minor-mode t)

;; Programming completions

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :leaf-defer nil
  :diminish t
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :custom-face
  (company-tooltip . '((t (:inherit default :background "dim gray"))))
  (company-tooltip-selection . '((t (:inherit font-lock-function-name-face))))
  (company-tooltip-common . '((t (:inherit font-lock-constant-face))))
  (company-tooltip-annotation . '((t (:inherit font-lock-builtin-face))))
  :global-minor-mode global-company-mode
  )

;; python

(setenv "PYTHONPATH" "/Users/shonda/.pyenv/versions/3.11.2/lib/python3.11/site-packages")

(leaf company-jedi
  :ensure t
  :after company
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'jedi:setup))

;; copilot leaf with automatic starting

(leaf copilot-setup
  :require t
  :load-path "~/.emacs.d/lisp/"
  :config
  (leaf f :ensure t)
  (leaf editorconfig :ensure t)
  (leaf s :ensure t)
  (leaf dash :ensure t)
  ;; :hook (prog-mode-hook . copilot-mode)
  )

;; ===========================================================================

;; Re-activate magic file name
(setq file-name-handler-alist my-saved-file-name-handler-alist)

;; Re-activate garbage collection
(setq gc-cons-threshold 16777216) ; 16mb

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company flycheck whitespace highlight-indent-guides electric savehist diminish blackout el-get hydra leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:foreground "dim grey" :background "unspecified-bg" :italic t))))
 '(line-number-current-line ((t (:foreground "#7FFFFF" :background "unspecified-bg" :italic t :bold t)))))
