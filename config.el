;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Source Code Pro ;; Menlo ;; Source Code Pro ;; SF Mono ;; Sarasa Term SC Nerd
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 15))

;; --- start of 展示当前key的日志 ---
(defvar dw/command-window-frame nil)
(defun dw/toggle-command-window ()
  (interactive)
  (require 'posframe)
  (require 'command-log-mode)
  (if dw/command-window-frame
      (progn
        (posframe-delete-frame clm/command-log-buffer)
        (setq dw/command-window-frame nil))
    (progn
      (global-command-log-mode t)
      (setq clm/command-log-buffer (get-buffer-create " *command-log*"))
      (with-current-buffer clm/command-log-buffer
        (setq-local face-remapping-alist
                    (cons '(default :height 120)
                          face-remapping-alist)))
      (let* ((cols 35)
             (lines 4)
             (border 2)
             (cw (frame-char-width))
             (ch (frame-char-height))
             (posw (+ (* cols cw) (* 2 border)))
             (x (- (frame-pixel-width) 10 posw))
             (y 10))
        (setq dw/command-window-frame
              (posframe-show
               clm/command-log-buffer
               :position (cons x y)
               :width cols
               :height lines
               :min-width cols
               :min-height lines
               :internal-border-width border
               :internal-border-color "#c792ea"))))))
;; Bind a convenient key (SPC t k) to toggle the command window
(map! :leader
      :desc "Toggle command log window"
      "t k" #'dw/toggle-command-window)
;; --- end of 展示当前key的日志 ---

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "～/Library/Mobile Documents/com~apple~CloudDocs/org/")
;; org-modern SF Mono
;; ellipsis https://endlessparentheses.com/changing-the-org-mode-ellipsis.html
(after! org
  (setq org-ellipsis " ⤵")
  (custom-set-faces
      '(org-ellipsis ((t (:foreground "#E6DC88")))))
  ;; org-mode 使用 Sarasa Term SC Nerd，其他模式保持默认 FiraCode Nerd Font
  (add-hook 'org-mode-hook
            (lambda ()
              (face-remap-add-relative 'default :family "Sarasa Term SC Nerd"))))

;; 改用这个 jk -> esc
;; 参考：https://emacs-china.org/t/evil-mode-insert-mode-emacs-easy-c-n-c-p/22512/11
(after! evil-escape
  (setq-default evil-escape-delay 0.3)
  (setq evil-escape-excluded-major-modes '(dired-mode))
  (delete 'visual evil-escape-excluded-states)
  (setq-default evil-escape-key-sequence "jk"))


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
