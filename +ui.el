;;; +ui.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(defvar-local my/dev-org-font-cookie nil)
(defvar clm/command-log-buffer)
(defvar clm/recent-history-string)
(defvar clm/command-repetitions)
(defvar clm/last-keyboard-command)

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

(defun my/dev-ui-field-weight-h ()
  "Use a medium weight for field names while preserving other face attributes."
  (when (facep 'font-lock-property-name-face)
    (set-face-attribute 'font-lock-property-name-face nil :weight 'medium)))

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

;; Zen: keep surrounding code readable and focus Rust on whole definitions.
(defcustom my/zen-focus-foreground-ratio 0.6
  "Proportion of the theme foreground retained in unfocused text."
  :type 'float
  :group 'faces)

(defvar-local my/zen-focus--previous-thing nil
  "Localness and value of the focus scope before the Rust override.")

(defun my/zen-focus-faces-h ()
  "Blend unfocused text with the theme background without changing syntax faces."
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

(defun my/zen-rust-definition-bounds ()
  "Return the native Rust definition at point, or the current line as a fallback."
  (save-excursion
    (save-match-data
      (let* ((origin (point))
             (bounds (condition-case nil
                         (bounds-of-thing-at-point 'defun)
                       (error nil))))
        (if (and bounds (<= (car bounds) origin) (<= origin (cdr bounds)))
            bounds
          (cons (line-beginning-position)
                (min (point-max) (1+ (line-end-position)))))))))

(defun my/zen-rust-focus-h ()
  "Use native Rust definition bounds while Focus is enabled, then restore its scope."
  (when (derived-mode-p 'rust-mode 'rustic-mode 'rust-ts-mode)
    (if (bound-and-true-p focus-mode)
        (unless my/zen-focus--previous-thing
          (setq-local my/zen-focus--previous-thing
                      (cons (local-variable-p 'focus-current-thing)
                            focus-current-thing))
          (setq-local focus-current-thing 'my/zen-rust-definition))
      (when my/zen-focus--previous-thing
        (if (car my/zen-focus--previous-thing)
            (setq-local focus-current-thing (cdr my/zen-focus--previous-thing))
          (kill-local-variable 'focus-current-thing))
        (kill-local-variable 'my/zen-focus--previous-thing)))))

(after! focus
  (put 'my/zen-rust-definition 'bounds-of-thing-at-point
       #'my/zen-rust-definition-bounds)
  (add-hook 'focus-mode-hook #'my/zen-rust-focus-h)
  (my/zen-focus-faces-h)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p focus-mode)
        (my/zen-rust-focus-h)
        (focus-update)))))

(defun my/dev-org-font-h ()
  "Apply an Org font without modifying the global fontset."
  (when (bound-and-true-p my/dev-org-font-cookie)
    (face-remap-remove-relative my/dev-org-font-cookie))
  (setq-local my/dev-org-font-cookie
              (when-let ((family (my/first-available-font-family
                                  my/dev-org-font-families)))
                (face-remap-add-relative 'default :family family))))

(defun my/dev-existing-notes-directory ()
  "Retain an existing notes location; never create a competing notes library."
  (if my/dev-notes-directory
      (expand-file-name my/dev-notes-directory)
    (seq-find #'file-accessible-directory-p
              (delq nil (list (and (boundp 'org-directory) org-directory)
                              (expand-file-name
                               "Library/Mobile Documents/com~apple~CloudDocs/org/" "~")
                              (expand-file-name "org/" "~"))))))

(defun my/org-directory ()
  "Return the configured accessible notes directory."
  (unless (and (boundp 'org-directory) (stringp org-directory)
               (file-accessible-directory-p org-directory))
    (user-error "请在本机 local.el 设置 my/dev-notes-directory 为已有笔记目录"))
  org-directory)

(defun my/dev-state-directory ()
  "Return a machine-local state directory, outside the synced configuration."
  (expand-file-name "dev/"
                    (if (and (boundp 'doom-cache-dir) (stringp doom-cache-dir))
                        doom-cache-dir
                      (expand-file-name "var/" user-emacs-directory))))

(defvar my/org-crash-log-file nil)
(setq my/org-crash-log-file
      (expand-file-name "org-errors.log" (my/dev-state-directory)))
(defvar my/org--command-error-fn nil)
(defvar my/org--ignored-errors
  '(quit beginning-of-line end-of-line beginning-of-buffer end-of-buffer))
(defvar my/org-crash-debug-on-error nil)

(defun my/dev-log-rotate (file incoming-bytes)
  "Rotate FILE before appending INCOMING-BYTES, retaining bounded history."
  (when (and (file-exists-p file)
             (> (+ (file-attribute-size (file-attributes file)) incoming-bytes)
                my/dev-log-max-bytes))
    (if (<= my/dev-log-history-count 0)
        (delete-file file)
      (let ((oldest (format "%s.%d" file my/dev-log-history-count)))
        (when (file-exists-p oldest) (delete-file oldest)))
      (cl-loop for i downfrom my/dev-log-history-count to 1
               for source = (if (= i 1) file (format "%s.%d" file (1- i)))
               for target = (format "%s.%d" file i)
               when (file-exists-p source) do (rename-file source target t)))))

(defun my/org--log (fmt &rest args)
  "Append one bounded record to the local Org error log."
  (let* ((record (concat (format-time-string "[%Y-%m-%d %H:%M:%S] ")
                         (apply #'format fmt args) "\n"))
         (record (my/dev-log--bounded-string record
                                             (min 32768 my/dev-log-max-bytes))))
    (make-directory (file-name-directory my/org-crash-log-file) t)
    (my/dev-log-rotate my/org-crash-log-file (string-bytes record))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region record nil my/org-crash-log-file t 'silent))))

(defun my/dev-log--bounded-string (string limit)
  "Limit STRING to LIMIT UTF-8 bytes without cutting a multibyte character."
  (let ((low 0) (high (length string)))
    (while (< low high)
      (let ((middle (/ (+ low high 1) 2)))
        (if (<= (string-bytes (encode-coding-string (substring string 0 middle) 'utf-8)) limit)
            (setq low middle)
          (setq high (1- middle)))))
    (substring string 0 low)))

(defun my/org--ignorable-error-p (err)
  (memq (car-safe err) my/org--ignored-errors))

(defun my/org--log-error (err context caller)
  "Record useful error context without copying unrelated Messages contents."
  (my/org--log "error: %s\ncommand: %s caller: %s context: %s\nmode: %s point: %s\nemacs: %s system: %s\nbacktrace:\n%s"
               (error-message-string err) (or this-command last-command "N/A")
               caller context major-mode (point) emacs-version system-type
               (with-temp-buffer (backtrace) (buffer-string))))

(defun my/org-command-error-logger (data context caller)
  (when (and (derived-mode-p 'org-mode) (not (my/org--ignorable-error-p data)))
    (condition-case nil (my/org--log-error data context caller) (error nil)))
  (when (and my/org--command-error-fn
             (not (eq my/org--command-error-fn #'my/org-command-error-logger)))
    (funcall my/org--command-error-fn data context caller)))

(defun my/dev-org-debug-h ()
  (setq-local debug-on-error my/org-crash-debug-on-error))

(defvar dw/command-window-frame nil)
(defun my/dev-command-log-stop ()
  "Stop both command logging and the package's text collection hooks."
  (when (fboundp 'global-command-log-mode) (global-command-log-mode -1))
  (remove-hook 'post-self-insert-hook #'clm/recent-history)
  (remove-hook 'post-command-hook #'clm/zap-recent-history)
  (when (boundp 'clm/recent-history-string) (setq clm/recent-history-string ""))
  (when (boundp 'clm/command-repetitions) (setq clm/command-repetitions 0))
  (when (boundp 'clm/last-keyboard-command) (setq clm/last-keyboard-command nil)))

(defun dw/toggle-command-window ()
  "Toggle the command display and its recording together."
  (interactive)
  (require 'posframe)
  (require 'command-log-mode)
  (if (and dw/command-window-frame (frame-live-p dw/command-window-frame))
      (progn
        (my/dev-command-log-stop)
        (when (buffer-live-p clm/command-log-buffer)
          (posframe-delete clm/command-log-buffer)
          (kill-buffer clm/command-log-buffer))
        (setq dw/command-window-frame nil clm/command-log-buffer nil))
    (my/dev-command-log-stop)
    (add-hook 'post-self-insert-hook #'clm/recent-history)
    (add-hook 'post-command-hook #'clm/zap-recent-history)
    (setq clm/command-log-buffer (get-buffer-create " *command-log*"))
    (global-command-log-mode 1)
    (setq dw/command-window-frame
          (posframe-show clm/command-log-buffer :position '(0 . 0)
                         :poshandler #'posframe-poshandler-frame-top-right-corner
                         :width 35 :height 4 :internal-border-width 2
                         :internal-border-color "#c792ea"))))

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

(remove-hook 'after-make-frame-functions #'my/dev-after-frame-font-h)
(when (fboundp 'doom-init-fonts-h)
  (advice-add 'doom-init-fonts-h :before #'my/dev-select-doom-fonts-h))
(my/dev-select-doom-fonts-h)
(add-hook 'doom-load-theme-hook #'my/dev-ui-field-weight-h)
(my/dev-ui-field-weight-h)
(add-hook 'doom-load-theme-hook #'my/dev-ui-corfu-faces-h)
(add-hook 'doom-load-theme-hook #'my/zen-focus-faces-h)
(after! corfu
  (my/dev-ui-corfu-faces-h))
(when-let ((directory (my/dev-existing-notes-directory)))
  (setq org-directory directory))
(unless (eq command-error-function #'my/org-command-error-logger)
  (setq my/org--command-error-fn command-error-function))
(setq command-error-function #'my/org-command-error-logger)
(add-hook 'org-mode-hook #'my/dev-org-debug-h)
(add-hook 'org-mode-hook #'my/dev-org-font-h)
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'org-mode) (my/dev-org-font-h))))
(with-eval-after-load 'command-log-mode
  (unless (and dw/command-window-frame (frame-live-p dw/command-window-frame))
    (my/dev-command-log-stop)))
(with-eval-after-load 'projectile
  (when my/dev-project-search-directories
    (setq projectile-project-search-path
          (mapcar #'expand-file-name my/dev-project-search-directories))))
(with-eval-after-load 'dirvish
  (when (fboundp 'dirvish--redisplay)
    (advice-add 'dirvish--redisplay :around #'my/dirvish-ignore-missing-filename-a)))

;; Show signature help only when explicitly requested.
(after! lsp-mode
  (setq lsp-signature-auto-activate nil))

;; Keep code lenses on their own visual line so wrapping cannot obscure indentation.
(after! lsp-lens
  (setq lsp-lens-place-position 'above-line))

;;; +ui.el ends here
