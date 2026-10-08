;;; local/environment/+watch.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'filenotify nil t)

(defun +local-env--live-buffers (record)
  "Return only live buffers still subscribed to RECORD."
  (cl-remove-if-not
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (equal +local-env--buffer-key (plist-get record :key)))))
   (plist-get record :buffers)))

(defun +local-env--watch-parent (path)
  "Find a readable existing directory that can watch a future PATH."
  (let ((path (unless (file-remote-p path)
                (if (file-directory-p path) path (file-name-directory path)))))
    (while (and path (not (file-directory-p path)))
      (let ((parent (file-name-directory (directory-file-name path))))
        (setq path (unless (equal parent path) parent))))
    (and path (file-readable-p path) (file-name-as-directory (file-truename path)))))

(defun +local-env--interesting-event-p (file record)
  "Whether FILE may change RECORD's effective configuration."
  (or (member file (plist-get record :watch-files))
      (and (or (string-match-p +local-env--config-name-regexp (file-name-nondirectory file))
               (member (file-name-nondirectory (directory-file-name file))
                       '(".mise" ".config" "mise" "conf.d")))
           (cl-some (lambda (directory)
                      (or (equal (+local-env--directory (file-name-directory file)) directory)
                          (file-in-directory-p directory (file-name-directory file))))
                    (plist-get record :directories)))
      (cl-some (lambda (reference)
                 (and (string-match-p "[*?]" reference)
                      (string-match-p (wildcard-to-regexp reference) file)))
               (plist-get record :watch-files))))

(defun +local-env--mark-dirty (key)
  "Coalesce notification bursts before preparing affected live buffers."
  (puthash key t +local-env--dirty)
  (unless (timerp +local-env--change-timer)
    (setq +local-env--change-timer (run-with-timer 0.2 nil #'+local-env--flush-dirty))))

(defun +local-env--flush-dirty ()
  "Asynchronously refresh every component touched by a config event."
  (setq +local-env--change-timer nil)
  (let ((keys (hash-table-keys +local-env--dirty))
        (seen (make-hash-table :test #'equal))
        (descriptions (make-hash-table :test #'equal)))
    (clrhash +local-env--dirty)
    (dolist (key keys)
      (when-let ((record (gethash key +local-env--cache)))
        (dolist (buffer (+local-env--live-buffers record))
          (with-current-buffer buffer
            (let* ((directory +local-env--buffer-directory)
                   (descriptor (+local-env--batch-descriptor directory descriptions))
                   (new-key (plist-get descriptor :key)))
              (+local-env--ensure directory nil (not (gethash new-key seen)) descriptor)
              (puthash new-key t seen))))))))

(defun +local-env--batch-descriptor (directory descriptions)
  "Describe DIRECTORY once in the synchronous batch table DESCRIPTIONS.
Callers must keep this table local to one refresh, never an async callback."
  (or (gethash directory descriptions)
      (puthash directory (+local-env--describe directory) descriptions)))

(defun +local-env--notify (path event)
  "Handle one native file notification from PATH, without starting a tool inline."
  (when +local-env-mode
    (let ((watch (gethash path +local-env--watchers)))
      (if (eq (cadr event) 'stopped)
          (remhash path +local-env--watchers)
        (dolist (key (plist-get watch :keys))
          (when-let ((record (gethash key +local-env--cache)))
            (when (cl-some (lambda (file)
                             (and (stringp file) (+local-env--interesting-event-p file record)))
                           (cddr event))
              (+local-env--mark-dirty key))))))))

(defun +local-env--watch-record (key record)
  "Share each directory watcher across active configuration contexts."
  (when (and +local-env-mode (fboundp 'file-notify-add-watch)
             (+local-env--live-buffers record))
    (let ((paths (delete-dups
                  (delq nil
                        (mapcar #'+local-env--watch-parent
                                (append (plist-get record :watch-files)
                                        (plist-get (plist-get record :descriptor) :folders)))))))
      (dolist (path paths)
        (let ((watch (gethash path +local-env--watchers)))
          (unless watch
            (when-let ((descriptor
                        (ignore-errors
                          (file-notify-add-watch
                           path '(change attribute-change)
                           (lambda (event) (+local-env--notify path event))))))
              (setq watch (list :descriptor descriptor :keys nil))
              (puthash path watch +local-env--watchers)))
          (when watch (cl-pushnew key (plist-get watch :keys) :test #'equal)))))))

(defun +local-env--prune-watchers ()
  "Release watchers whose contexts no longer have live subscribers."
  (let (remove)
    (maphash
     (lambda (path watch)
       (setf (plist-get watch :keys)
             (cl-remove-if-not
              (lambda (key)
                (when-let ((record (gethash key +local-env--cache)))
                  (+local-env--live-buffers record)))
              (plist-get watch :keys)))
       (unless (plist-get watch :keys) (push path remove)))
     +local-env--watchers)
    (dolist (path remove)
      (when-let ((descriptor (plist-get (gethash path +local-env--watchers) :descriptor)))
        (ignore-errors (file-notify-rm-watch descriptor)))
      (remhash path +local-env--watchers))))

(defun +local-env--file-h ()
  "Prepare a visited local file without waiting for mise or launching consumers."
  (when (and +local-env-mode buffer-file-name (not (file-remote-p buffer-file-name)))
    (+local-env-ensure (file-name-directory buffer-file-name))))

(defun +local-env--mode-h ()
  "Start preparation after a mode body, before Doom's local-variable hooks."
  (+local-env--file-h))

(defun +local-env--save-h ()
  "Invalidate saved configuration sources; ordinary source saves do no work."
  (when (and +local-env-mode buffer-file-name (not (file-remote-p buffer-file-name)))
    (maphash
     (lambda (key record)
       (when (+local-env--interesting-event-p buffer-file-name record)
         (+local-env--mark-dirty key)))
     +local-env--cache)))

(defun +local-env--kill-h ()
  "Detach this buffer and release its unused notification registrations."
  (when-let ((record (gethash +local-env--buffer-key +local-env--cache)))
    (setf (plist-get record :buffers) (delq (current-buffer) (plist-get record :buffers))))
  (+local-env--prune-watchers))

(defun +local-env--recheck (&optional force)
  "Check active buffers with bounded retries; never wait for a process."
  (when +local-env-mode
    (+local-env--capture-base)
    (let ((seen (make-hash-table :test #'equal))
          (descriptions (make-hash-table :test #'equal)))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and +local-env--buffer-directory
                     (not (file-remote-p +local-env--buffer-directory)))
            (let* ((directory +local-env--buffer-directory)
                   (descriptor (+local-env--batch-descriptor directory descriptions))
                   (key (plist-get descriptor :key)))
              (+local-env--ensure directory nil (and force (not (gethash key seen))) descriptor)
              (puthash key t seen))))))
    (+local-env--prune-watchers)))

(defun +local-env--focus-h ()
  "Refresh config/tool availability when Emacs regains focus."
  (+local-env--recheck))

(defun +local-env--stop ()
  "Cancel only this module's private timers, requests and file notifications."
  (dolist (timer (list +local-env--idle-timer +local-env--change-timer))
    (when (timerp timer) (cancel-timer timer)))
  (setq +local-env--idle-timer nil +local-env--change-timer nil)
  (maphash (lambda (_ request) (when-let ((cancel (plist-get request :cancel))) (funcall cancel)))
           +local-env--requests)
  (clrhash +local-env--requests)
  (maphash (lambda (_ watch)
             (ignore-errors (file-notify-rm-watch (plist-get watch :descriptor))))
           +local-env--watchers)
  (clrhash +local-env--watchers)
  (clrhash +local-env--dirty)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when +local-env--applied-context
        (if +local-env--native-local-environment-p
            (setq-local process-environment (copy-sequence +local-env--native-environment))
          (kill-local-variable 'process-environment))
        (if +local-env--native-local-exec-path-p
            (setq-local exec-path (copy-sequence +local-env--native-exec-path))
          (kill-local-variable 'exec-path))
        (setq-local +local-env--applied-context nil
                    +local-env--buffer-directory nil +local-env--buffer-key nil
                    +local-env--native-environment nil +local-env--native-exec-path nil))))
  (clrhash +local-env--cache)
  (clrhash +local-env--directories))

(define-minor-mode +local-env-mode
  "Asynchronously maintain local file-buffer environments using mise."
  :global t :group '+local-env
  (let ((hooks '((after-change-major-mode-hook . +local-env--mode-h)
                 (hack-local-variables-hook . +local-env--file-h)
                 (find-file-hook . +local-env--file-h)
                 (after-save-hook . +local-env--save-h)
                 (kill-buffer-hook . +local-env--kill-h)
                 (focus-in-hook . +local-env--focus-h))))
    (if +local-env-mode
        (progn
          (+local-env--capture-base)
          (dolist (entry hooks) (add-hook (car entry) (cdr entry) -90))
          (when (timerp +local-env--idle-timer) (cancel-timer +local-env--idle-timer))
          (setq +local-env--idle-timer
                (run-with-idle-timer +local-env-recheck-interval t #'+local-env--recheck)))
      (dolist (entry hooks) (remove-hook (car entry) (cdr entry)))
      (+local-env--stop))))
