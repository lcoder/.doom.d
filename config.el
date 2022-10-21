;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "lcoder"
      user-mail-address "mtingfeng@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
     ;; doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-moonlight)
(setq doom-font (font-spec :family "Menlo" :size 15))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)

;; (setq peep-dired-enable-on-directories t)
;; 参考资料：https://github.com/asok/peep-dired
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)

(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(map! :leader
      ;; :desc "Eshell" "e s" #'eshell
      ;; :desc "Eshell popup toggle" "e t" #'+eshell/toggle
      ;; :desc "Counsel eshell history" "e h" #'counsel-esh-history
      :desc "Vterm popup toggle" "v t" #'+vterm/toggle)

;; vscode rename symbol
(evil-define-key 'normal tide-mode-map
  (kbd "<f2>") 'tide-rename-symbol
  )

;; org-modern
;; ellipsis https://endlessparentheses.com/changing-the-org-mode-ellipsis.html
(setq org-ellipsis " ⤵ ")

(defun orgfile (name)
  (concat "~/org/" name))

(defun gtdfile (name)
  (orgfile (concat "gtd/" name)))

(defconst gtd-inbox (gtdfile "inbox.org"))
(defconst gtd-mobile-inbox (gtdfile "mobile-inbox.org"))
(defconst gtd-projects (gtdfile "projects.org"))
(defconst gtd-tickler (gtdfile "tickler.org"))
(defconst gtd-someday (gtdfile "someday.org"))

(use-package! org
  :config
  (setq org-agenda-files (list gtd-inbox
                               gtd-mobile-inbox
                               gtd-projects
                               gtd-tickler))
  (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                 (file+headline ,gtd-inbox "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline ,gtd-tickler "Tickler")
                                 "* %i%? \n %U")))
  (setq org-refile-targets `((,gtd-projects :maxlevel . 3)
                             (,gtd-someday :level . 1)
                             (,gtd-tickler :maxlevel . 2)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)"
                                      "CANCELLED(c)")))
  (setq org-html-doctype "html5")
  (setq org-html-html5-fancy t)
  (defun org-archive-finished-tasks ()
    "Archive all tasks in any finished state."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE|CANCELLED" 'agenda)))

;; font configuration
;; refer https://github.com/jakebox/jake-emacs
;; (setq text-scale-mode-step 1.1) ;; How much to adjust text scale by when using `text-scale-mode'
;; (set-frame-font "SF Mono:size=16" nil t)
;; Float height value (1.0) makes fixed-pitch take height 1.0 * height of default
;; This means it will scale along with default when the text is zoomed
;; (set-face-attribute 'fixed-pitch nil :font "Roboto Mono" :weight 'regular :height 1.0)
;; Height of 160 seems to match perfectly with 12-point on Google Docs
;; (set-face-attribute 'variable-pitch nil :family "Times New Roman" :height 160)
;; (set-face-attribute 'variable-pitch nil :slant 'normal :weight 'normal :height 180 :width 'normal :foundry "nil" :family "Nunito Sans")
;; (set-face-attribute 'variable-pitch nil :slant 'normal :weight 'normal :height 180 :width 'normal :foundry "nil" :family "Nunito Sans")

;; insert-mode 中模拟emacs操作
;; https://emacs-china.org/t/evil-mode-insert-mode-emacs-easy-c-n-c-p/22512/5
;; (define-key evil-insert-state-map (kbd "C-n") 'next-line)
;; (define-key evil-insert-state-map (kbd "C-p") 'previous-line)

;; 下面这个放弃了，貌似不生效 https://github.com/noctuid/evil-guide#use-some-emacs-keybindings
;; (setq evil-disable-insert-state-bindings t)

;; 改用这个 kj -> esc
(after! evil-escape
  (setq-default evil-escape-delay 0.3)
  (setq evil-escape-excluded-major-modes '(dired-mode))
  (delete 'visual evil-escape-excluded-states)
  (setq-default evil-escape-key-sequence "kj"))

;; 缩进
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
