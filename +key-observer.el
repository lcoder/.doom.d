;;; +key-observer.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'button)
(require 'subr-x)

(use-package! keycast :commands (keycast-invisible-mode keycast-format))
(use-package! keyfreq :commands keyfreq-show)

(defgroup my/key-observer nil
  "Local observation of Emacs commands with daily log files."
  :group 'convenience)

(defcustom my/key-observer-directory
  (expand-file-name "doom-key-observer/"
                    (or (getenv "XDG_STATE_HOME") "~/.local/state/"))
  "Directory for local observation sessions, outside the Doom repository."
  :type 'directory)

(defcustom my/key-observer-duration nil
  "Session duration in seconds; nil records continuously until manually stopped."
  :type '(choice (const :tag "Continuous observation" nil)
          (number :tag "Time limit in seconds")))

(defcustom my/key-observer-save-interval 60
  "Interval between batched saves of observed commands, in seconds."
  :type 'number)

(defvar my/key-observer--session nil)
(defvar my/key-observer--pending nil)
(defvar my/key-observer--frequencies nil)
(defvar my/key-observer--contexts (make-hash-table :test #'eql))
(defvar my/key-observer--save-timer nil)
(defvar my/key-observer--deadline-timer nil)
(defvar my/key-observer--finish-timer nil)
(defvar my/key-observer--owns-keycast nil)
(defvar my/key-observer--inhibit nil)
(defvar my/key-observer--replay-source nil)
(defvar my/key-observer--source-buffer nil)
(defvar my/key-observer--control-state nil)
(defconst my/key-observer--panel "*按键观察*")
(defconst my/key-observer--replay-commands
  '(evil-repeat evil-repeat-pop evil-repeat-pop-next evil-execute-macro
    kmacro-call-macro kmacro-end-or-call-macro kmacro-end-and-call-macro
    call-last-kbd-macro execute-kbd-macro))
(defvar my/key-observer--mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'my/key-observer)
    map))

(defun my/key-observer--active-p ()
  "Return non-nil when the current session can still be continued."
  (member (plist-get my/key-observer--session :status)
          '("recording" "paused")))

(defun my/key-observer--path (name)
  "Return the current session's file path for NAME."
  (expand-file-name name (plist-get my/key-observer--session :directory)))

(defun my/key-observer--expired-p ()
  "Return non-nil if the current session has reached a finite deadline."
  (when-let* ((deadline (plist-get my/key-observer--session :deadline)))
    (>= (float-time) deadline)))

(defun my/key-observer--log-name (&optional timestamp)
  "Return the log filename for TIMESTAMP, using daily files in continuous mode."
  (if (eq (plist-get my/key-observer--session :continuous) t)
      (concat "commands-"
              (format-time-string "%Y-%m-%d"
                                  (and timestamp (seconds-to-time timestamp))
                                  "Asia/Singapore")
              ".jsonl")
    "commands.jsonl"))

(defun my/key-observer--set-control (enabled paused)
  "Remember recording intent without performing I/O in command hooks."
  (setq my/key-observer--control-state
        (list :version 1 :enabled (if (eq enabled t) t :false)
              :paused (if (eq paused t) t :false)
              :error (plist-get my/key-observer--session :error)
              :updated-at (float-time))))

(defun my/key-observer--save-control ()
  "Persist the remembered intent for automatic recovery after daemon startup."
  (when my/key-observer--control-state
    (my/key-observer--write-json
     (expand-file-name "control.json" my/key-observer-directory)
     my/key-observer--control-state)))

(defun my/key-observer--write-json (file data)
  "Atomically write DATA as private UTF-8 JSON to FILE."
  (let ((temporary (make-temp-file (concat file ".")))
        (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (progn
          (set-file-modes temporary #o600)
          (with-temp-file temporary
            (insert (json-serialize data :null-object nil :false-object :false)
                    "\n"))
          (rename-file temporary file t))
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun my/key-observer--frequency-data ()
  "Return frequency rows, separating manual input from automatic replay."
  (let (rows)
    (when my/key-observer--frequencies
      (maphash
       (lambda (key count)
         (push (list :mode (symbol-name (nth 0 key))
                     :command (nth 1 key) :source (nth 2 key) :count count)
               rows))
       my/key-observer--frequencies))
    (vconcat (sort rows (lambda (a b)
                          (> (plist-get a :count) (plist-get b :count)))))))

(defun my/key-observer--save ()
  "Save queued commands, frequencies and session state in a single batch."
  (when my/key-observer--session
    (let ((my/key-observer--inhibit t)
          (coding-system-for-write 'utf-8-unix))
      (when my/key-observer--pending
        ;; Remove only successfully saved prefixes so a retry cannot duplicate
        ;; yesterday's records when today's daily file could not be written.
        (let ((records (reverse my/key-observer--pending)))
          (setq my/key-observer--pending nil)
          (unwind-protect
              (while records
                (let* ((name (my/key-observer--log-name
                              (plist-get (car records) :time)))
                       (rest records)
                       (count 0)
                       (file (my/key-observer--path name)))
                  (with-temp-buffer
                    (while (and rest
                                (equal name (my/key-observer--log-name
                                             (plist-get (car rest) :time))))
                      (insert (json-serialize (car rest)
                                              :null-object nil :false-object :false)
                              "\n")
                      (cl-incf count)
                      (setq rest (cdr rest)))
                    (unless (file-exists-p file)
                      (with-temp-file file)
                      (set-file-modes file #o600))
                    (write-region (point-min) (point-max) file t 'silent))
                  (setq records (nthcdr count records))))
            (setq my/key-observer--pending (reverse records)))))
      (my/key-observer--write-json
       (my/key-observer--path "frequencies.json")
       (list :rows (my/key-observer--frequency-data)))
      (setf (plist-get my/key-observer--session :saved-at) (float-time))
      (my/key-observer--write-json (my/key-observer--path "session.json")
                                   my/key-observer--session)
      (my/key-observer--write-json
       (expand-file-name "latest.json" my/key-observer-directory)
       my/key-observer--session)
      (my/key-observer--save-control))))

(defun my/key-observer--deactivate ()
  "Remove only the collector and Keycast mode owned by this observer."
  (remove-hook 'pre-command-hook #'my/key-observer--pre-h)
  (remove-hook 'post-command-hook #'my/key-observer--post-h)
  (advice-remove 'evil-execute-repeat-info #'my/key-observer--repeat-a)
  (advice-remove 'read-passwd #'my/key-observer--password-a)
  (when my/key-observer--owns-keycast
    (keycast-invisible-mode -1)
    (setq my/key-observer--owns-keycast nil))
  (clrhash my/key-observer--contexts))

(defun my/key-observer--cancel-timer (symbol)
  "Cancel the timer held by SYMBOL, and clear its value."
  (when (timerp (symbol-value symbol)) (cancel-timer (symbol-value symbol)))
  (set symbol nil))

(defun my/key-observer--save-h ()
  "Checkpoint data, pausing the collector on a storage failure."
  (condition-case error-data
      (progn
        (my/key-observer--save)
        (my/key-observer--refresh))
    (error
     (my/key-observer--deactivate)
     (my/key-observer--cancel-timer 'my/key-observer--save-timer)
     (when (my/key-observer--active-p)
       (setf (plist-get my/key-observer--session :status) "paused"))
     (setf (plist-get my/key-observer--session :error)
           (error-message-string error-data))
     (my/key-observer--set-control
      (and (my/key-observer--active-p)
           (eq (plist-get my/key-observer--session :continuous) t)) t)
     (ignore-errors (my/key-observer--save-control))
     (my/key-observer--refresh)
     (display-warning 'my/key-observer
                      (concat "按键观察保存失败，已停止采集；内存中未保存的数据保留。"
                              (error-message-string error-data))
                      :warning))))

(defun my/key-observer--boundary (event)
  "Queue a lifecycle EVENT without counting it as an editing command."
  (push (list :type "boundary" :time (float-time) :event event)
        my/key-observer--pending))

(defun my/key-observer--finish (status &optional defer-save)
  "Finish the session with STATUS; DEFER-SAVE avoids I/O in command hooks."
  (when (my/key-observer--active-p)
    (my/key-observer--set-control
     (and (equal status "interrupted")
          (eq (plist-get my/key-observer--session :continuous) t))
     (equal (plist-get my/key-observer--session :status) "paused"))
    (my/key-observer--deactivate)
    (my/key-observer--cancel-timer 'my/key-observer--save-timer)
    (my/key-observer--cancel-timer 'my/key-observer--deadline-timer)
    (setf (plist-get my/key-observer--session :status) status
          (plist-get my/key-observer--session :ended-at) (float-time))
    (my/key-observer--boundary status)
    (if defer-save
        (setq my/key-observer--finish-timer
              (run-at-time 0 nil #'my/key-observer--save-h))
      (my/key-observer--save-h))
    (force-mode-line-update t)))

(defun my/key-observer--deadline-h ()
  "End observation at the original wall-clock deadline."
  (when (my/key-observer--expired-p)
    (my/key-observer--finish "completed")))

(defun my/key-observer--eligible-p ()
  "Return non-nil for observable Emacs input, including minibuffers."
  (and (not my/key-observer--inhibit)
       (not (derived-mode-p 'comint-mode 'term-mode 'vterm-mode
                            'eat-mode 'eshell-mode))
       (not (equal (buffer-name) my/key-observer--panel))
       (not (and (symbolp this-original-command)
                 (string-prefix-p "my/key-observer"
                                  (symbol-name this-original-command))))))

(defun my/key-observer--source ()
  "Return the source of this command, distinguishing replay from manual input."
  (or my/key-observer--replay-source
      (and executing-kbd-macro "macro")
      "manual"))

(defun my/key-observer--queue-command (record mode command source)
  "Queue RECORD and increment the frequency for MODE, COMMAND and SOURCE."
  (let ((key (list mode command source)))
    (push record my/key-observer--pending)
    (puthash key (1+ (gethash key my/key-observer--frequencies 0))
             my/key-observer--frequencies)
    (cl-incf (plist-get my/key-observer--session :events))))

(defun my/key-observer--repeat-a (fn &rest args)
  "Label commands executed by FN as Evil repeat playback."
  (let ((my/key-observer--replay-source "repeat"))
    (apply fn args)))

(defun my/key-observer--password-a (fn &rest args)
  "Invalidate the enclosing command context when FN reads a password."
  ;; Keycast protects the minibuffer itself. Also omit the enclosing command's
  ;; full input vector, which can otherwise include events read inside it.
  (puthash (recursion-depth) nil my/key-observer--contexts)
  (apply fn args))

(defun my/key-observer--pre-h ()
  "Remember command context; do not read files or start processes."
  (when (equal (plist-get my/key-observer--session :status) "recording")
    (if (my/key-observer--expired-p)
        (my/key-observer--finish "completed" t)
      (let ((context
             (when (my/key-observer--eligible-p)
               (list :mode major-mode :state (bound-and-true-p evil-state)
                     :buffer (buffer-name) :file buffer-file-name
                     :invocation (key-description (this-command-keys-vector))))))
        (puthash (recursion-depth) context my/key-observer--contexts)
        ;; Playback can replace the command loop's outer context. Keep its
        ;; invocation before any replayed commands overwrite that context.
        (when (and context
                   (or (memq this-original-command my/key-observer--replay-commands)
                       (and (arrayp this-original-command)
                            (not (symbolp this-original-command)))))
          (let ((command (if (symbolp this-original-command)
                             (symbol-name this-original-command)
                           "<keyboard-macro>"))
                (source (my/key-observer--source)))
            (my/key-observer--queue-command
             (list :type "command" :phase "invocation" :time (float-time)
                   :keys (plist-get context :invocation)
                   :invocation (plist-get context :invocation)
                   :command command :source source :mode (symbol-name major-mode)
                   :state-before (when (bound-and-true-p evil-state)
                                   (symbol-name evil-state))
                   :buffer (buffer-name) :file buffer-file-name
                   :prefix (when current-prefix-arg
                             (prin1-to-string current-prefix-arg)))
             major-mode command source)))))))

(defun my/key-observer--post-h ()
  "Queue Keycast's resolved command and original keys with editing context."
  (when (equal (plist-get my/key-observer--session :status) "recording")
    (if (my/key-observer--expired-p)
        (my/key-observer--finish "completed" t)
      (condition-case error-data
          (let* ((context (gethash (recursion-depth) my/key-observer--contexts))
                 (keys (and context (keycast-format "%K")))
                 (command (and keys (keycast-format "%C"))))
            (when (and command (not (string-empty-p keys))
                       (not (memq this-original-command
                                  my/key-observer--replay-commands))
                       (my/key-observer--eligible-p))
              (let* ((source (my/key-observer--source))
                     (mode (plist-get context :mode))
                     (command (string-trim command))
                     (record
                      (list :type "command" :time (float-time)
                            :keys keys :command command :source source
                            :invocation (plist-get context :invocation)
                            :command-keys (key-description
                                           (this-command-keys-vector))
                            :original-command
                            (when (symbolp this-original-command)
                              (symbol-name this-original-command))
                            :mode (symbol-name mode)
                            :state-before
                            (when (plist-get context :state)
                              (symbol-name (plist-get context :state)))
                            :state-after
                            (when (bound-and-true-p evil-state)
                              (symbol-name evil-state))
                            :buffer (plist-get context :buffer)
                            :file (plist-get context :file)
                            :result-buffer (buffer-name)
                            :prefix (when current-prefix-arg
                                      (prin1-to-string current-prefix-arg)))))
                (my/key-observer--queue-command record mode command source))))
        (error
         (my/key-observer--deactivate)
         (my/key-observer--cancel-timer 'my/key-observer--save-timer)
         (setf (plist-get my/key-observer--session :status) "paused"
               (plist-get my/key-observer--session :error)
               (error-message-string error-data))
         (my/key-observer--set-control
          (eq (plist-get my/key-observer--session :continuous) t) t)
         (setq my/key-observer--finish-timer
               (run-at-time 0 nil #'my/key-observer--save-h))
         (message "按键观察出现错误，已暂停：%s"
                  (error-message-string error-data)))))))

(defun my/key-observer--activate ()
  "Enable the shared collector without opening a Keycast display frame."
  (unless (bound-and-true-p keycast-invisible-mode)
    (keycast-invisible-mode 1)
    (setq my/key-observer--owns-keycast t))
  (add-hook 'pre-command-hook #'my/key-observer--pre-h 80)
  ;; Keycast appends its update hook at depth 90. Sample its resolved value after it.
  (add-hook 'post-command-hook #'my/key-observer--post-h 100)
  (when (fboundp 'evil-execute-repeat-info)
    (advice-add 'evil-execute-repeat-info :around #'my/key-observer--repeat-a))
  (advice-add 'read-passwd :around #'my/key-observer--password-a)
  (my/key-observer--cancel-timer 'my/key-observer--save-timer)
  (setq my/key-observer--save-timer
        (run-at-time my/key-observer-save-interval my/key-observer-save-interval
                     #'my/key-observer--save-h)))

(defun my/key-observer-start ()
  "Start a fresh observation session, shared by all Emacs clients."
  (interactive)
  (when (my/key-observer--active-p)
    (user-error "已有观察会话，请继续或结束当前会话"))
  (when my/key-observer--pending
    (user-error "尚有未保存数据，请先解决保存错误并查看日志"))
  (require 'keycast)
  (require 'keyfreq)
  (unless (and (or (null my/key-observer-duration)
                   (and (numberp my/key-observer-duration)
                        (> my/key-observer-duration 0)))
               (> my/key-observer-save-interval 0))
    (user-error "观察时长和保存间隔必须大于零"))
  (my/key-observer--cancel-timer 'my/key-observer--finish-timer)
  (make-directory my/key-observer-directory t)
  (set-file-modes my/key-observer-directory #o700)
  (let* ((started (float-time))
         (directory (make-temp-file
                     (expand-file-name (format-time-string "%Y%m%d-%H%M%S-")
                                       my/key-observer-directory) t)))
    (set-file-modes directory #o700)
    (setq my/key-observer--frequencies (make-hash-table :test #'equal)
          my/key-observer--session
          (list :version 2 :id (file-name-nondirectory directory)
                :host (system-name)
                :directory directory :status "recording" :started-at started
                :continuous (if my/key-observer-duration :false t)
                :deadline (when my/key-observer-duration
                            (+ started my/key-observer-duration))
                :ended-at nil :saved-at nil :events 0 :error nil))
    (my/key-observer--set-control (null my/key-observer-duration) nil)
    (my/key-observer--boundary "started")
    (my/key-observer--save-h)
    (unless (plist-get my/key-observer--session :error)
      (my/key-observer--activate)
      (when my/key-observer-duration
        (setq my/key-observer--deadline-timer
              (run-at-time my/key-observer-duration nil
                           #'my/key-observer--deadline-h))))
    (my/key-observer--refresh)
    (when (called-interactively-p 'interactive)
      (message (if my/key-observer-duration
                   "按键观察已开始，到期自动停止。"
                 "持续按键观察已开始，日志按天保存。")))))

(defun my/key-observer-pause ()
  "Pause collection and checkpoint data without extending the deadline."
  (interactive)
  (unless (equal (plist-get my/key-observer--session :status) "recording")
    (user-error "当前没有正在记录的观察"))
  (my/key-observer--deactivate)
  (my/key-observer--cancel-timer 'my/key-observer--save-timer)
  (setf (plist-get my/key-observer--session :status) "paused")
  (my/key-observer--set-control
   (eq (plist-get my/key-observer--session :continuous) t) t)
  (my/key-observer--boundary "paused")
  (my/key-observer--save-h))

(defun my/key-observer-resume ()
  "Continue a paused session until its existing deadline."
  (interactive)
  (unless (equal (plist-get my/key-observer--session :status) "paused")
    (user-error "当前没有可继续的观察"))
  (if (my/key-observer--expired-p)
      (my/key-observer--finish "completed")
    (setf (plist-get my/key-observer--session :error) nil)
    (my/key-observer--save-h)
    (unless (plist-get my/key-observer--session :error)
      (setf (plist-get my/key-observer--session :status) "recording")
      (my/key-observer--set-control
       (eq (plist-get my/key-observer--session :continuous) t) nil)
      (my/key-observer--boundary "resumed")
      (my/key-observer--activate)
      (my/key-observer--save-h))))

(defun my/key-observer-stop ()
  "End collection and save the current session."
  (interactive)
  (unless (my/key-observer--active-p) (user-error "当前没有正在观察的会话"))
  (my/key-observer--finish "stopped"))

(defun my/key-observer-status ()
  "Return a copy of the current session metadata without changing editor state."
  (copy-tree my/key-observer--session))

(defun my/key-observer-log ()
  "Save pending records and open this session's raw log read-only."
  (interactive)
  (unless my/key-observer--session (user-error "还没有观察会话"))
  (my/key-observer--save-h)
  (find-file-read-only (my/key-observer--path (my/key-observer--log-name))))

(defun my/key-observer-statistics ()
  "Show manual command frequencies using Keyfreq's native display."
  (interactive)
  (unless my/key-observer--session (user-error "还没有观察会话"))
  (require 'keyfreq)
  (let ((keyfreq-table (make-hash-table :test #'equal))
        (inhibit-read-only t)
        ;; Do not merge the user's historical Keyfreq data into this session.
        (keyfreq-file (my/key-observer--path "unused-keyfreq-history"))
        (keyfreq-buffer "*按键观察频率*"))
    (maphash
     (lambda (key count)
       (when (equal (nth 2 key) "manual")
         (puthash (cons (nth 0 key) (intern (nth 1 key))) count keyfreq-table)))
     my/key-observer--frequencies)
    (with-current-buffer (if (buffer-live-p my/key-observer--source-buffer)
                             my/key-observer--source-buffer
                           (current-buffer))
      (keyfreq-show))
    (when-let* ((buffer (get-buffer keyfreq-buffer)))
      (with-current-buffer buffer
        (setq-local buffer-read-only t)))))

(defun my/key-observer--label ()
  "Return the Chinese status label for the current observation."
  (pcase (plist-get my/key-observer--session :status)
    ("recording" "观察中")
    ("paused" "已暂停")
    ("completed" "已完成")
    ("stopped" "已结束")
    ("interrupted" "会话已中断")
    (_ "未开始")))

(defun my/key-observer--mode-line ()
  "Render a small clickable status indicator using the current theme."
  (when (my/key-observer--active-p)
    (propertize (concat " [按键:" (my/key-observer--label) "]")
                'face 'mode-line 'mouse-face 'mode-line-highlight
                'help-echo "点击打开按键观察面板"
                'local-map my/key-observer--mode-line-map)))

(defun my/key-observer--button (label command)
  "Insert a clickable LABEL invoking COMMAND without a custom key binding."
  (insert-text-button label 'follow-link t
                      'action (lambda (_button) (funcall command)))
  (insert "  "))

(defun my/key-observer--render ()
  "Render the current session and native text-button controls."
  (let ((inhibit-read-only t)
        (point-before (point)))
    (erase-buffer)
    (insert "按键观察与每日复盘\n\n状态：" (my/key-observer--label) "\n")
    (when my/key-observer--session
      (insert (format "操作记录：%d\n" (plist-get my/key-observer--session :events))
              "记录方式："
              (if (my/key-observer--expired-p)
                  "本次限时观察已到期"
                (if (plist-get my/key-observer--session :deadline)
                    (concat "截止至 "
                            (format-time-string
                             "%Y-%m-%d %H:%M:%S %Z"
                             (seconds-to-time
                              (plist-get my/key-observer--session :deadline))))
                  "持续记录，直到手动结束"))
              "\n日志目录：" (plist-get my/key-observer--session :directory) "\n")
      (when (plist-get my/key-observer--session :error)
        (insert "保存／采集错误："
                (plist-get my/key-observer--session :error) "\n")))
    (insert "\n")
    (unless (my/key-observer--active-p)
      (my/key-observer--button "[开始]" #'my/key-observer-start))
    (pcase (plist-get my/key-observer--session :status)
      ("recording" (my/key-observer--button "[暂停]" #'my/key-observer-pause))
      ("paused" (my/key-observer--button "[继续]" #'my/key-observer-resume)))
    (when (my/key-observer--active-p)
      (my/key-observer--button "[结束]" #'my/key-observer-stop))
    (when my/key-observer--session
      (my/key-observer--button "[查看日志]" #'my/key-observer-log)
      (my/key-observer--button "[查看统计]" #'my/key-observer-statistics))
    (insert "\n\n记录各个 Emacs 客户端的操作，包含原始输入键序列。\n"
            "持续观察按天保存，Emacs 重启后恢复；手动暂停和结束仍生效。\n"
            "终端进程输入不记录；限时观察的暂停不会延长截止时间。\n"
            "手动操作、宏回放和 Evil 重复分别统计。\n"
            "Keyfreq 页面展示当前会话累计手动操作；按键提示来自原编辑模式。\n"
            "每日复盘由 Codex 当前聊天汇报，汇报后继续记录。\n")
    (goto-char (min point-before (point-max)))))

(defun my/key-observer--refresh ()
  "Refresh the panel if it exists, without changing focus or window layout."
  (when-let* ((buffer (get-buffer my/key-observer--panel)))
    (with-current-buffer buffer (my/key-observer--render)))
  (force-mode-line-update t))

(defun my/key-observer ()
  "Open observation controls; recording starts only with the Start button."
  (interactive)
  (unless (equal (buffer-name) my/key-observer--panel)
    (setq my/key-observer--source-buffer (current-buffer)))
  (with-current-buffer (get-buffer-create my/key-observer--panel)
    (unless (derived-mode-p 'special-mode) (special-mode))
    (my/key-observer--render))
  (pop-to-buffer my/key-observer--panel))

(defun my/key-observer--exit-h ()
  "Checkpoint an interrupted session before the shared daemon exits."
  (when (my/key-observer--active-p)
    (my/key-observer--finish "interrupted")))

(defun my/key-observer-ensure-running ()
  "Restore continuous recording intent, respecting manual pause and stop."
  (interactive)
  (unless (my/key-observer--active-p)
    (let ((file (expand-file-name "control.json" my/key-observer-directory)))
      (when (file-exists-p file)
        (condition-case error-data
            (let ((control
                   (with-temp-buffer
                     (insert-file-contents file)
                     (json-parse-buffer :object-type 'plist
                                        :null-object nil :false-object nil))))
              (when (eq (plist-get control :enabled) t)
                (let ((my/key-observer-duration nil))
                  (my/key-observer-start))
                (when (and (plist-get control :paused)
                           (equal (plist-get my/key-observer--session :status)
                                  "recording"))
                  (my/key-observer-pause)
                  (when (plist-get control :error)
                    (setf (plist-get my/key-observer--session :error)
                          (plist-get control :error))
                    (my/key-observer--set-control t t)
                    (my/key-observer--save-h)))))
          (error
           (display-warning 'my/key-observer
                            (concat "无法恢复持续按键观察："
                                    (error-message-string error-data))
                            :warning)))))))

;; Re-evaluation preserves an active session. Startup follows saved local intent.
(add-hook 'kill-emacs-hook #'my/key-observer--exit-h)
(add-hook 'after-init-hook #'my/key-observer-ensure-running)
(add-to-list 'global-mode-string '(:eval (my/key-observer--mode-line)) t)
(when after-init-time (my/key-observer-ensure-running))

(provide 'my-key-observer)
