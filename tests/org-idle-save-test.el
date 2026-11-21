;;; org-idle-save-test.el -*- lexical-binding: t; -*-

(require 'ert-x)
(require 'org)
(require 'super-save)

(defmacro my/org-idle-test--with-file (name initial &rest body)
  "Visit a temporary NAME with INITIAL contents and run BODY in its buffer.
A nil INITIAL leaves the file uncreated for the first-save case."
  (declare (indent 2) (debug (form form body)))
  `(let ((super-save-all-buffers nil))
     (ert-with-temp-directory directory :suffix "-org"
       (let ((file (expand-file-name ,name directory))
             (contents ,initial))
         (when contents
           (with-temp-file file (insert contents)))
         (let ((buf (find-file-noselect file)))
           (unwind-protect
               (with-current-buffer buf ,@body)
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf)))))))

(defun my/org-idle-test--disk-bytes ()
  "Read the current test file exactly as stored."
  (let ((file buffer-file-name))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (buffer-string))))

(ert-deftest my/org-idle-save-preserves-text-bytes ()
  (my/org-idle-test--with-file "note.org" "* 笔记\n"
    (should (derived-mode-p 'org-mode))
    (should super-save-mode)
    (should super-save-auto-save-when-idle)
    (should (= super-save-idle-duration 30))
    (goto-char (point-max))
    (insert "第一行  \n第二行\t")
    (let ((expected (encode-coding-string (buffer-string) 'utf-8-unix)))
      (super-save-command-idle)
      (should-not (buffer-modified-p))
      (should (equal expected (my/org-idle-test--disk-bytes))))))

(ert-deftest my/org-idle-save-skips-new-files ()
  (my/org-idle-test--with-file "draft.org" nil
    (insert "* Draft\n")
    (super-save-command-idle)
    (should (buffer-modified-p))
    (should-not (file-exists-p buffer-file-name))))

(ert-deftest my/org-idle-save-skips-plain-text-and-read-only-org ()
  (dolist (name '("note.txt" "read-only.org"))
    (ert-info (name)
      (my/org-idle-test--with-file name "original\n"
        (insert "draft\n")
        (when (derived-mode-p 'org-mode)
          (setq buffer-read-only t))
        (super-save-command-idle)
        (should (buffer-modified-p))
        (should (equal (my/org-idle-test--disk-bytes) "original\n"))))))

(ert-deftest my/org-idle-save-skips-external-changes ()
  (my/org-idle-test--with-file "shared.org" "original\n"
    (insert "local draft\n")
    (let ((file buffer-file-name)
          (old-time (visited-file-modtime)))
      (with-temp-file file (insert "external revision\n"))
      ;; Ensure distinct mtimes even on filesystems with coarse timestamps.
      (set-file-times file (time-subtract old-time 60)))
    (should-not (verify-visited-file-modtime (current-buffer)))
    (cl-letf (((symbol-function 'yes-or-no-p) #'ert-fail)
              ((symbol-function 'y-or-n-p) #'ert-fail))
      (super-save-command-idle))
    (should (buffer-modified-p))
    (should (equal (my/org-idle-test--disk-bytes) "external revision\n"))))

;;; org-idle-save-test.el ends here
