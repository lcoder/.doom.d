;;; org-indent-timers-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org-indent)
(require 'valign)

(defmacro my/org-indent-test--isolated (&rest body)
  "Run BODY with private Org queues and timer registries."
  (declare (indent 0) (debug t))
  `(let ((timer-idle-list nil)
         (timer-list nil)
         (org-indent-agentized-buffers nil)
         (org-indent-agent-timer nil)
         (org-indent-agent-resume-timer nil)
         (org-indent-mode-hook (copy-sequence org-indent-mode-hook)))
     (unwind-protect
         (progn ,@body)
       (dolist (timer (append timer-idle-list timer-list))
         (cancel-timer timer)))))

(defmacro my/org-indent-test--with-buffer (&rest body)
  "Run BODY in a temporary Org buffer without personal mode hooks."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((org-inhibit-startup t))
       (delay-mode-hooks (org-mode)))
     (insert "* 标题\n正文\n** 子标题\n内容\n")
     ,@body))

(defun my/org-indent-test--timer (&optional repeat function)
  "Return a private idle timer with REPEAT and FUNCTION."
  (run-with-idle-timer 3600 repeat
                       (or function #'org-indent-initialize-agent)))

(defun my/org-indent-test--timers ()
  "Return active indentation idle timers."
  (seq-filter (lambda (timer)
                (eq (timer--function timer) #'org-indent-initialize-agent))
              timer-idle-list))

(ert-deftest my/org-indent-empty-queue-retires-only-owned-idle-timers ()
  (my/org-indent-test--isolated
    (let ((other (my/org-indent-test--timer t #'ignore))
          (regular (run-at-time 3600 3600 #'org-indent-initialize-agent)))
      (setq org-indent-agent-timer (my/org-indent-test--timer t)
            org-indent-agent-resume-timer (my/org-indent-test--timer))
      (dotimes (_ 17) (my/org-indent-test--timer t))
      (my/org-indent--normalize-timers)
      (should (equal timer-idle-list (list other)))
      (should (memq regular timer-list))
      (should-not org-indent-agent-timer)
      (should-not org-indent-agent-resume-timer))))

(ert-deftest my/org-indent-pending-work-keeps-native-agent-and-resume ()
  (my/org-indent-test--isolated
    (my/org-indent-test--with-buffer
      (org-indent-mode 1)
      (let ((agent org-indent-agent-timer))
        (setq org-indent-agent-resume-timer (my/org-indent-test--timer))
        (let ((resume org-indent-agent-resume-timer)
              (delay (timer--time agent)))
          (dotimes (_ 3) (my/org-indent-test--timer t))
          (my/org-indent-test--timer)
          (my/org-indent--normalize-timers)
          (should (= 2 (length (my/org-indent-test--timers))))
          (should (eq org-indent-agent-timer agent))
          (should (eq org-indent-agent-resume-timer resume))
          (should (equal delay (timer--time agent))))))))

(ert-deftest my/org-indent-stale-reference-adopts-existing-agent ()
  (my/org-indent-test--isolated
    (my/org-indent-test--with-buffer
      (org-indent-mode 1)
      (let ((agent org-indent-agent-timer))
        (setq org-indent-agent-timer (timer-create)
              org-indent-agent-resume-timer (timer-create))
        (my/org-indent-test--timer)
        (my/org-indent--normalize-timers)
        (should (eq org-indent-agent-timer agent))
        (should-not org-indent-agent-resume-timer)
        (should (equal (my/org-indent-test--timers) (list agent)))))))

(ert-deftest my/org-indent-empty-callback-skips-valign-after-advice ()
  (my/org-indent-test--isolated
    (let ((alignments 0))
      (should (advice-member-p #'valign--org-indent-advice
                               #'org-indent-initialize-agent))
      (cl-letf (((symbol-function 'valign-region)
                 (lambda (&rest _) (cl-incf alignments))))
        (let ((valign-mode t))
          (dotimes (_ 3) (org-indent-initialize-agent))))
      (should (zerop alignments))
      (should-not (my/org-indent-test--timers)))))

(ert-deftest my/org-indent-multiple-buffers-and-repeated-enabling ()
  (my/org-indent-test--isolated
    (my/org-indent-test--with-buffer
      (org-indent-mode 1)
      (let ((first (current-buffer)))
        (my/org-indent-test--with-buffer
          (dotimes (_ 3) (org-indent-mode 1))
          (should (= 1 (length (my/org-indent-test--timers))))
          (cl-letf (((symbol-function 'input-pending-p) (lambda (&rest _) nil)))
            (org-indent-initialize-agent)
            (should (equal org-indent-agentized-buffers (list first)))
            (with-current-buffer first (org-indent-initialize-agent)))
          (should-not org-indent-agentized-buffers)
          (should-not (my/org-indent-test--timers)))))))

(ert-deftest my/org-indent-interrupted-initialization-resumes-and-cleans-up ()
  (my/org-indent-test--isolated
    (my/org-indent-test--with-buffer
      (org-indent-mode 1)
      (cl-letf (((symbol-function 'input-pending-p) (lambda (&rest _) t)))
        (org-indent-initialize-agent))
      (should org-indent-agentized-buffers)
      (should (= 1 (length (my/org-indent-test--timers))))
      (let ((org-indent-agent-active-delay '(0 0 0)))
        (cl-letf (((symbol-function 'input-pending-p) (lambda (&rest _) nil))
                  ((symbol-function 'current-idle-time)
                   (lambda () (seconds-to-time 1))))
          (dotimes (_ 2) (org-indent-initialize-agent))))
      (should (= 2 (length (my/org-indent-test--timers))))
      (should (memq org-indent-agent-resume-timer timer-idle-list))
      (cl-letf (((symbol-function 'input-pending-p) (lambda (&rest _) nil)))
        (org-indent-initialize-agent))
      (should-not org-indent-agentized-buffers)
      (should-not (my/org-indent-test--timers)))))

(ert-deftest my/org-indent-disabled-or-killed-buffers-retire-timers ()
  (my/org-indent-test--isolated
    (my/org-indent-test--with-buffer
      (org-indent-mode 1)
      (org-indent-mode -1)
      (should-not (my/org-indent-test--timers)))
    (my/org-indent-test--with-buffer
      (org-indent-mode 1))
    (should org-indent-agentized-buffers)
    (org-indent-initialize-agent)
    (should-not org-indent-agentized-buffers)
    (should-not (my/org-indent-test--timers))))

(ert-deftest my/org-indent-error-path-reconciles-timers ()
  (my/org-indent-test--isolated
    (my/org-indent-test--with-buffer
      (org-indent-mode 1)
      (should-error
       (my/org-indent--guard-agent-a
        (lambda ()
          (my/org-indent-test--timer t)
          (error "Test pending initialization failure"))))
      (should (= 1 (length (my/org-indent-test--timers))))
      (should-error
       (my/org-indent--guard-agent-a
        (lambda ()
          (setq org-indent-agentized-buffers nil)
          (error "Test completed initialization failure"))))
      (should-not (my/org-indent-test--timers)))))

(ert-deftest my/org-indent-installation-is-idempotent ()
  (my/org-indent-test--isolated
    (dotimes (_ 3) (my/org-indent--install-h))
    (should (= 1 (cl-count #'my/org-indent--normalize-timers
                           org-indent-mode-hook)))
    (let ((guards 0))
      (advice-mapc (lambda (function _properties)
                     (when (eq function #'my/org-indent--guard-agent-a)
                       (cl-incf guards)))
                   #'org-indent-initialize-agent)
      (should (= guards 1)))
    (should-not (my/org-indent-test--timers))))

(defun my/org-indent-test--table-columns ()
  "Return visible bar positions for each non-separator table row."
  (save-excursion
    (goto-char (point-min))
    (let (rows)
      (while (re-search-forward "^[ \t]*|" nil t)
        (beginning-of-line)
        (unless (org-at-table-hline-p)
          (let ((end (line-end-position)) columns)
            (while (search-forward "|" end t)
              (push (valign--pixel-x (1- (point))) columns))
            (push (nreverse columns) rows)))
        (forward-line 1))
      (nreverse rows))))

(defun my/org-indent-test--assert-table-aligned ()
  "Fontify and assert equal bar positions across the test table."
  (font-lock-ensure (point-min) (point-max))
  (let* ((rows (my/org-indent-test--table-columns))
         (reference (car rows)))
    (should (= 3 (length rows)))
    (should (= 3 (length reference)))
    (dolist (row (cdr rows))
      (should (= (length reference) (length row)))
      (cl-mapc (lambda (expected actual)
                 (should (<= (abs (- expected actual)) 1)))
               reference row))))

(ert-deftest my/org-indent-chinese-table-stays-aligned-in-gui ()
  (let ((window
         (seq-some (lambda (frame)
                     (when (and (display-graphic-p frame)
                                (not (frame-parent frame)))
                       (frame-selected-window frame)))
                   (frame-list))))
    (skip-unless window)
    (my/org-indent-test--isolated
      (with-selected-window window
        (save-window-excursion
          (my/org-indent-test--with-buffer
            ;; Font Lock refuses buffers whose names start with a space.
            (rename-buffer (generate-new-buffer-name "*Org table alignment test*"))
            (erase-buffer)
            (insert "* 表格\n| 名称 | 说明 |\n|------+----------|\n| 中文 | abc |\n| abc | 混合中文 |\n")
            (set-window-buffer window (current-buffer))
            (font-lock-mode 1)
            (valign-mode 1)
            (org-indent-mode 1)
            (cl-letf (((symbol-function 'input-pending-p) (lambda (&rest _) nil)))
              (org-indent-initialize-agent))
            (should font-lock-mode)
            (ert-info ("after initialization")
              (my/org-indent-test--assert-table-aligned))
            (goto-char (point-min))
            (search-forward "| 中文")
            (replace-match "| 更长的中文名称")
            (ert-info ("after editing")
              (my/org-indent-test--assert-table-aligned))
            (goto-char (point-min))
            (org-cycle)
            (org-cycle)
            (should-not (org-invisible-p (line-beginning-position 2)))
            (ert-info ("after folding and expanding")
              (my/org-indent-test--assert-table-aligned))
            (should valign-mode)
            (should-not (my/org-indent-test--timers))))))))

;;; org-indent-timers-test.el ends here
