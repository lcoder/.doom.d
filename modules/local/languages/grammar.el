;;; local/languages/grammar.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(defvar +local-languages--grammar-jobs (make-hash-table :test #'eq))
(defvar +local-languages--switching nil)
(defvar +local-languages--specs nil
  "Enabled language entries: (FALLBACK NATIVE GRAMMARS FILE-REGEXP).")

(defun +local-languages--tsx-fallback ()
  "Keep JSX/TSX editable through Doom's existing web-mode fallback."
  (require 'web-mode)
  (web-mode)
  (setq-local web-mode-content-type "jsx"))

(defun +local-languages--spec (&optional buffer)
  "Return the structural-editing entry applicable to BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cl-find-if
     (lambda (spec)
       (or (memq major-mode (list (nth 0 spec) (nth 1 spec)))
           (and buffer-file-name (nth 3 spec)
                (string-match-p (nth 3 spec) buffer-file-name))))
     +local-languages--specs)))

(defun +local-languages--grammar-directory ()
  "Use the active Doom profile's native grammar cache."
  (if (fboundp 'doom-profile-data-dir)
      (doom-profile-data-dir t "tree-sitter")
    (expand-file-name "tree-sitter/" (if (boundp 'doom-data-dir) doom-data-dir
                                     (locate-user-emacs-file ".cache/")))))

(defun +local-languages--grammar-ready-p (language)
  "Check LANGUAGE compatibility without invoking an installer."
  (and (fboundp 'treesit-available-p) (treesit-available-p)
       (ignore-errors (treesit-language-available-p language))))

(defun +local-languages--grammar-fingerprint (language)
  "Identify conditions that permit retrying a failed LANGUAGE preparation."
  (secure-hash
   'sha256
   (prin1-to-string
    (list emacs-version
          (and (fboundp 'treesit-library-abi-version) (treesit-library-abi-version))
          (assq language treesit-language-source-alist)
          (executable-find "git") (or (executable-find "cc") (executable-find "gcc"))
          (ignore-errors (network-interface-list))))))

(defun +local-languages--upgrade (buffer)
  "Enable a prepared native mode without changing BUFFER's editing state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((spec (+local-languages--spec))
                  (mode (nth 1 spec)))
        (when (and (not (eq major-mode mode)) (fboundp mode)
                   (cl-every #'+local-languages--grammar-ready-p (nth 2 spec)))
          (let ((position (point)) (undo buffer-undo-list)
                (minimum (point-min)) (maximum (point-max))
                (modified (buffer-modified-p)) (read-only buffer-read-only)
                (text (save-restriction
                        (widen)
                        (buffer-substring-no-properties (point-min) (point-max))))
                (+local-languages--switching t))
            (put mode '+tree-sitter-mode nil)
            (save-restriction
              (widen)
              (let ((buffer-undo-list t))
                (condition-case nil
                    (funcall mode)
                  (error
                   (funcall (nth 0 spec))
                   (unless (get mode '+local-languages-failed)
                     (put mode '+local-languages-failed t)
                     (message "Native mode %s is incompatible; its classic mode remains available" mode)))))
              ;; Major-mode changes normally preserve text.  Do not let a
              ;; third-party mode hook silently replace the user's contents.
              (unless (equal text (buffer-substring-no-properties (point-min) (point-max)))
                (let ((inhibit-read-only t) (inhibit-modification-hooks t))
                  (erase-buffer) (insert text))))
            (widen)
            (narrow-to-region minimum maximum)
            (setq buffer-undo-list undo buffer-read-only read-only)
            (set-buffer-modified-p modified)
            (goto-char (min position (point-max)))
            (when (fboundp '+local-languages--mode-ready)
              (+local-languages--mode-ready))))))))

(defun +local-languages--grammar-finished (process _event)
  "Apply the outcome of the isolated native installer PROCESS."
  (when (memq (process-status process) '(exit signal))
    (let* ((language (process-get process 'language))
           (job (gethash language +local-languages--grammar-jobs))
           (ready (and (eq (process-status process) 'exit)
                       (zerop (process-exit-status process))
                       (+local-languages--grammar-ready-p language))))
      (when (eq (plist-get job :process) process)
        (setq job (plist-put job :state (if ready 'ready 'failed)))
        (puthash language job +local-languages--grammar-jobs)
        (if ready
            (dolist (buffer (plist-get job :buffers)) (+local-languages--upgrade buffer))
          (unless (plist-get job :notified)
            (puthash language (plist-put job :notified t) +local-languages--grammar-jobs)
            (message "Grammar %s could not be prepared; basic editing remains available" language))))
      (when-let ((directory (process-get process 'worker-directory)))
        (when (file-directory-p directory) (delete-directory directory t))))))

(defun +local-languages--grammar-worker (language recipe)
  "Prepare LANGUAGE with RECIPE in an isolated instance of this Emacs.
Only the native treesit library and the current Doom recipe are loaded."
  (let* ((destination (+local-languages--grammar-directory))
         (emacs (expand-file-name invocation-name invocation-directory))
         (worker-directory (progn (make-directory destination t)
                                  (make-temp-file (expand-file-name ".worker-" destination) t)))
         (script (expand-file-name "install.el" worker-directory))
         (buffer (get-buffer-create (format " *grammar install %s*" language)))
         (process-environment
          (cl-remove-if (lambda (entry)
                          (string-match-p "\\`\\(?:EMACS_SERVER_\\|DISPLAY=\\|WAYLAND_DISPLAY=\\)" entry))
                        (copy-sequence (default-value 'process-environment))))
         (exec-path (copy-sequence (default-value 'exec-path)))
         (default-directory worker-directory))
    (unless (file-executable-p emacs) (error "The active Emacs executable is unavailable"))
    (with-temp-file script
      (prin1 `(progn
                (require 'treesit)
                (setq treesit-language-source-alist ',(list recipe)
                      treesit-extra-load-path ',(list destination))
                (treesit-install-language-grammar ',language ,destination)
                (unless (treesit-language-available-p ',language)
                  (error "Grammar is incompatible with this Emacs")))
             (current-buffer)))
    (let ((process (make-process :name (format "grammar-%s" language) :buffer buffer
                                :command (list emacs "-Q" "--batch" "--load" script)
                                :connection-type 'pipe :noquery t
                                :sentinel #'+local-languages--grammar-finished)))
      (process-put process 'language language)
      (process-put process 'worker-directory worker-directory)
      process)))

(defun +local-languages--prepare-grammar (language buffer)
  "Queue LANGUAGE for BUFFER, deduplicating work and unchanged failures."
  (unless (+local-languages--grammar-ready-p language)
    (let* ((fingerprint (+local-languages--grammar-fingerprint language))
           (job (gethash language +local-languages--grammar-jobs)))
      (if (and job (or (eq (plist-get job :state) 'pending)
                       (and (equal fingerprint (plist-get job :fingerprint))
                            (< (- (float-time) (or (plist-get job :started) 0)) 300))))
          (puthash language (plist-put job :buffers
                                      (cl-adjoin buffer (plist-get job :buffers)))
                   +local-languages--grammar-jobs)
        (setq job (list :fingerprint fingerprint :state 'pending :started (float-time)
                        :notified (and (equal fingerprint (plist-get job :fingerprint))
                                       (plist-get job :notified))
                        :buffers (cl-adjoin buffer (plist-get job :buffers))))
        (puthash language job +local-languages--grammar-jobs)
        (condition-case nil
            (let ((recipe (assq language treesit-language-source-alist)))
              (unless (and recipe (executable-find "git")
                           (or (executable-find "cc") (executable-find "gcc")))
                (error "No compatible recipe or compiler"))
              (setq job (plist-put job :process (+local-languages--grammar-worker language recipe)))
              (puthash language job +local-languages--grammar-jobs))
          (error
           (puthash language (plist-put (plist-put job :state 'failed) :notified t)
                    +local-languages--grammar-jobs)
           (unless (plist-get job :notified)
             (message "Grammar %s is unavailable; basic editing remains available" language))))))))

(defun +local-languages--prepare-buffer ()
  "Prepare structural editing only for the current buffer's actual language."
  (unless +local-languages--switching
    (when-let ((spec (+local-languages--spec)))
      (when (and (fboundp 'treesit-available-p) (treesit-available-p))
        (require 'treesit)
        (dolist (language (nth 2 spec))
          (+local-languages--prepare-grammar language (current-buffer)))
        (+local-languages--upgrade (current-buffer))))))

(defun +local-languages--register-remaps ()
  "Keep Doom's normal mode remapping and classic fallbacks."
  (setq treesit-auto-install-grammar 'never)
  (dolist (spec +local-languages--specs)
    (when (and (fboundp 'set-tree-sitter!) (fboundp (nth 1 spec)))
      (set-tree-sitter! (nth 0 spec) (nth 1 spec) (nth 2 spec))))
  (when (cl-find '+local-languages--tsx-fallback +local-languages--specs :key #'car)
    (let ((entry '("\\.[tj]sx\\'" . +local-languages--tsx-fallback)))
      (setq auto-mode-alist (cons entry (delete entry auto-mode-alist)))))
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . conf-toml-mode)))

(provide '+local-languages-grammar)
