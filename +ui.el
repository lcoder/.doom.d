;;; +ui.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(defvar-local my/dev-org-font-cookie nil)

(defun my/first-available-font-family (families)
  "Return the first available font in FAMILIES."
  (seq-find (lambda (family) (find-font (font-spec :family family))) families))

(defun my/dev-ui-apply-fonts ()
  "Choose a font available on this frame without assuming an installation."
  (when (display-graphic-p)
    (let ((family (or (my/first-available-font-family my/dev-font-families)
                      (face-attribute 'default :family))))
      (when (stringp family)
        (setq doom-font (font-spec :family family :size my/dev-font-size))))))

(when (featurep 'neomacs)
  (defun my/neomacs-normalize-font-a (original font)
    "Parse sized FONT directly when Neomacs already supplies its family.
Doom's XLFD lookup cannot match even the active font on Neomacs 0.0.20."
    (let* ((name (cond ((fontp font) (font-xlfd-name font))
                       ((stringp font) font)))
           (spec (and name (font-spec :name name))))
      (if (and spec (font-get spec :family) (font-get spec :size))
          spec
        (funcall original font))))
  (advice-add 'doom-normalize-font :around #'my/neomacs-normalize-font-a))

(when (featurep 'neomacs)
  (defun my/neomacs-org-fold-marker (overlay)
    "Display the Org ellipsis with its own face on a hidden fold OVERLAY."
    (when (and (overlay-buffer overlay)
               (eq (overlay-get overlay 'isearch-open-invisible)
                   #'org-fold-core--isearch-show))
      (with-current-buffer (overlay-buffer overlay)
        (let* ((spec (overlay-get overlay 'invisible))
               (marker (and spec (eq (invisible-p spec) 2)
                            (org-fold-core-get-folding-spec-property spec :ellipsis))))
          ;; Neomacs 0.0.20 drops the face from display-table ellipsis glyphs.
          (overlay-put overlay 'display
                       (when (stringp marker) (propertize marker 'face 'org-ellipsis)))))))

  (defun my/neomacs-org-fold-region-a (from to fold &optional _spec)
    (when (and fold (eq org-fold-core-style 'overlays))
      (dolist (overlay (overlays-at from))
        (when (and (= from (overlay-start overlay)) (= to (overlay-end overlay)))
          (my/neomacs-org-fold-marker overlay)))))

  (defun my/neomacs-org-refresh-fold-markers-h ()
    (when (derived-mode-p 'org-mode)
      (save-restriction
        (widen)
        (mapc #'my/neomacs-org-fold-marker (overlays-in (point-min) (point-max))))))

  (after! org
    (advice-add 'org-fold-core-region :after #'my/neomacs-org-fold-region-a)
    ;; Isearch temporarily reveals a fold by toggling its existing overlay.
    (advice-add 'isearch-open-overlay-temporary :after #'my/neomacs-org-fold-marker)
    (advice-add 'isearch-open-necessary-overlays :after #'my/neomacs-org-fold-marker)
    (add-hook 'visible-mode-hook #'my/neomacs-org-refresh-fold-markers-h)))

(defun my/dev-ui-maple-fontset-h (&optional _context)
  "Use Maple CJK glyphs after Doom initializes fonts or creates a frame."
  (let (configured-fontsets)
    (dolist (frame (frame-list))
      (when (display-graphic-p frame)
        (let ((family (face-attribute 'default :family frame))
              (fontset (face-attribute 'default :fontset frame)))
          (when (and (equal family "Maple Mono NF CN")
                     (stringp fontset)
                     (not (member fontset configured-fontsets)))
            ;; macOS 默认字体集会优先选择系统中文字体，需在 Maple 字体集内覆盖。
            (dolist (characters '(han (#x3000 . #x303f) (#xff00 . #xffef)))
              (set-fontset-font fontset characters
                                (font-spec :family family) frame))
            (push fontset configured-fontsets)))))))

(defun my/dev-ui-corfu-faces-h ()
  "Keep Corfu selection and annotations readable in dark and light themes."
  (when (and (facep 'corfu-current)
             (facep 'corfu-annotations)
             (fboundp 'doom-blend))
    (let* ((doom-palette-p
            (and (fboundp 'doom-color)
                 (seq-some (lambda (theme)
                             (string-prefix-p "doom-" (symbol-name theme)))
                           custom-enabled-themes)))
           (accent (or (and doom-palette-p (doom-color 'highlight))
                       (and doom-palette-p (doom-color 'blue))
                       (face-attribute 'highlight :background nil t)))
           (background (face-attribute 'corfu-default :background nil t))
           (foreground (face-attribute 'corfu-default :foreground nil t))
           (light-p (eq (frame-parameter nil 'background-mode) 'light)))
      (when (and (stringp accent)
                 (stringp background)
                 (stringp foreground)
                 (color-defined-p accent)
                 (color-defined-p background)
                 (color-defined-p foreground))
        (set-face-attribute 'corfu-current nil
                            :background (doom-blend accent background
                                                    (if light-p 0.18 0.35))
                            :foreground foreground
                            :extend t)
        (set-face-attribute 'corfu-annotations nil
                            :foreground (doom-blend foreground background 0.9))))))

(defcustom my/zen-focus-foreground-ratio 0.6
  "Proportion of the theme foreground retained in unfocused text."
  :type 'float
  :group 'faces)

(defun my/zen-focus-faces-h ()
  "Blend unfocused text with the theme background for the writing focus view."
  (when (facep 'focus-unfocused)
    (require 'color)
    (let* ((frame (if (display-graphic-p)
                      (selected-frame)
                    (seq-find #'display-graphic-p (frame-list))))
           (doom-palette-p
            (and (fboundp 'doom-color)
                 (seq-some (lambda (theme)
                             (string-prefix-p "doom-" (symbol-name theme)))
                           custom-enabled-themes)))
           (foreground (or (and doom-palette-p (doom-color 'fg))
                           (face-attribute 'default :foreground frame t)))
           (background (or (and doom-palette-p (doom-color 'bg))
                           (face-attribute 'default :background frame t))))
      (when (and (stringp foreground) (color-defined-p foreground)
                 (stringp background) (color-defined-p background))
        (let ((color
               (apply #'color-rgb-to-hex
                      (append
                       (cl-mapcar (lambda (fg bg)
                                    (+ (* fg my/zen-focus-foreground-ratio)
                                       (* bg (- 1 my/zen-focus-foreground-ratio))))
                                  (color-name-to-rgb foreground)
                                  (color-name-to-rgb background))
                       '(2)))))
          (set-face-attribute 'focus-unfocused nil :foreground color :inherit nil)
          (set-face-attribute 'focus-unfocused t :foreground color :inherit nil))))))

(after! focus
  (my/zen-focus-faces-h))

(defun my/dev-org-font-h ()
  "Apply an Org font without modifying the global fontset."
  (when (bound-and-true-p my/dev-org-font-cookie)
    (face-remap-remove-relative my/dev-org-font-cookie))
  (setq-local my/dev-org-font-cookie
              (when-let ((family (my/first-available-font-family
                                  my/dev-org-font-families)))
                (face-remap-add-relative 'default :family family))))

;; Keep Org's native indentation scheduler from leaving orphan idle timers.
(defvar org-indent-agentized-buffers)
(defvar org-indent-agent-timer)
(defvar org-indent-agent-resume-timer)

(defun my/org-indent--normalize-timers ()
  "Retire orphan indentation timers while retaining pending native work.
Keep at most one repeating agent and its referenced one-shot continuation.
Other idle timers are never changed."
  (setq org-indent-agentized-buffers
        (seq-filter (lambda (buffer)
                      (and (buffer-live-p buffer)
                           (buffer-local-value 'org-indent-mode buffer)))
                    org-indent-agentized-buffers))
  (let* ((timers (seq-filter
                  (lambda (timer)
                    (eq (timer--function timer) #'org-indent-initialize-agent))
                  timer-idle-list))
         (agent
          (when org-indent-agentized-buffers
            (or (and (memq org-indent-agent-timer timers)
                     (timer--repeat-delay org-indent-agent-timer)
                     org-indent-agent-timer)
                (seq-find #'timer--repeat-delay timers))))
         (resume
          (and org-indent-agentized-buffers
               (memq org-indent-agent-resume-timer timers)
               (not (timer--repeat-delay org-indent-agent-resume-timer))
               org-indent-agent-resume-timer)))
    (dolist (timer timers)
      (unless (or (eq timer agent) (eq timer resume))
        (cancel-timer timer)))
    (setq org-indent-agent-timer agent
          org-indent-agent-resume-timer resume)))

(defun my/org-indent--guard-agent-a (function &rest args)
  "Run FUNCTION with ARGS only while native indentation work remains.
Wrap the Valign after-advice too, so empty idle callbacks never realign tables."
  (my/org-indent--normalize-timers)
  (when org-indent-agentized-buffers
    (unwind-protect
        (apply function args)
      (my/org-indent--normalize-timers))))

(defun my/org-indent--install-h ()
  "Install the reload-safe indentation timer guard and clean existing timers."
  (when (fboundp 'org-indent-initialize-agent)
    ;; Valign adds its advice again for each buffer; keep the guard outside it.
    (advice-add 'org-indent-initialize-agent :around
                #'my/org-indent--guard-agent-a '((depth . -100)))
    (add-hook 'org-indent-mode-hook #'my/org-indent--normalize-timers 90)
    (my/org-indent--normalize-timers)))

(after! org-indent
  (my/org-indent--install-h))

(defun my/org-directory ()
  "Return the configured accessible notes directory."
  (unless (and (boundp 'org-directory) (stringp org-directory)
               (file-accessible-directory-p org-directory))
    (user-error "请在本机 local.el 设置 my/dev-notes-directory 为已有笔记目录"))
  org-directory)

(defvar my/dev-dirvish-warning-shown nil)
(defun my/dirvish-ignore-missing-filename-a (fn &rest args)
  "Handle the known nil filename redisplay case with one diagnostic."
  (condition-case err (apply fn args)
    (wrong-type-argument
     (if (equal err '(wrong-type-argument stringp nil))
         (unless my/dev-dirvish-warning-shown
           (setq my/dev-dirvish-warning-shown t)
           (display-warning 'my/dev
                            "Dirvish 遇到空文件名，已跳过本次重绘。" :warning))
       (signal (car err) (cdr err))))))

(defun my/dev-select-doom-fonts-h (&optional reload)
  "Choose available fonts without replacing Doom's active font adjustments.
Preserve the current font on RELOAD and while a size adjustment is active."
  (unless (or reload (get 'doom-font 'initial-value))
    (condition-case err
        (my/dev-ui-apply-fonts)
      (error (display-warning 'my/dev
                              (format "保留当前字体：%s" (error-message-string err))
                              :warning)))))

(when (fboundp 'doom-init-fonts-h)
  (advice-add 'doom-init-fonts-h :before #'my/dev-select-doom-fonts-h)
  (advice-add 'doom-init-fonts-h :after #'my/dev-ui-maple-fontset-h))
;; 普通新窗口不触发服务端字体初始化钩子，单独补齐其字符映射。
(add-hook 'after-make-frame-functions #'my/dev-ui-maple-fontset-h 100)
(my/dev-select-doom-fonts-h)
(add-hook 'doom-load-theme-hook #'my/dev-ui-corfu-faces-h)
(add-hook 'doom-load-theme-hook #'my/zen-focus-faces-h)
(after! corfu
  (my/dev-ui-corfu-faces-h))
(add-hook 'org-mode-hook #'my/dev-org-font-h)
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'org-mode) (my/dev-org-font-h))))
(with-eval-after-load 'dirvish
  (when (fboundp 'dirvish--redisplay)
    (advice-add 'dirvish--redisplay :around #'my/dirvish-ignore-missing-filename-a)))

;; Limit automatic templates to the existing Org rules, including exclusions.
(setq +file-templates-alist
      (seq-filter (lambda (rule)
                    (memq (car rule) '(org-mode org-journal-mode doom-docs-org-mode)))
                  +file-templates-alist))

;;; +ui.el ends here
