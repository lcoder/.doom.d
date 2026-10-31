;;; pyim-gc-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gcmh)
(require 'pyim)

(defconst my/pyim-test--config-file
  (expand-file-name "../config.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Configuration source used to verify Pyim GC reload behavior.")

(defmacro my/pyim-test--with-gcmh (&rest body)
  "Run BODY with private GCMH timer state and a low initial GC threshold."
  (declare (indent 0) (debug t))
  `(let ((gcmh-mode t)
         (gcmh-idle-timer nil)
         (gcmh-idle-delay 3600)
         (gcmh-high-cons-threshold 134217728)
         (gc-cons-threshold 800000))
     (unwind-protect
         (progn ,@body)
       (when (timerp gcmh-idle-timer)
         (cancel-timer gcmh-idle-timer)))))

(ert-deftest my/pyim-gc-uses-editing-threshold-and-restores-caller ()
  (my/pyim-test--with-gcmh
   (should
    (equal (my/pyim--with-active-gc-a
            (lambda (&rest args)
              (should (= gc-cons-threshold gcmh-high-cons-threshold))
              args)
            'first 'second)
           '(first second)))
   (should (= gc-cons-threshold 800000))
   (should (memq gcmh-idle-timer timer-list))))

(ert-deftest my/pyim-gc-keeps-a-higher-caller-threshold ()
  (my/pyim-test--with-gcmh
   (let ((gc-cons-threshold 268435456))
     (my/pyim--with-active-gc-a
      (lambda () (should (= gc-cons-threshold 268435456))))
     (should (= gc-cons-threshold 268435456)))))

(ert-deftest my/pyim-gc-postpones-only-the-pending-gcmh-timer ()
  (my/pyim-test--with-gcmh
   (let ((pending (run-with-timer 3600 nil #'ignore))
         (other (run-with-timer 3600 nil #'ignore)))
     (setq gcmh-idle-timer pending)
     (unwind-protect
         (progn
           (my/pyim--with-active-gc-a
            (lambda ()
              (should-not gcmh-idle-timer)
              (should-not (memq pending timer-list))
              (should (memq other timer-list))))
           (should-not (eq gcmh-idle-timer pending))
           (should (memq gcmh-idle-timer timer-list))
           (should (memq other timer-list)))
       (cancel-timer pending)
       (cancel-timer other)))))

(ert-deftest my/pyim-gc-restores-idle-scheduling-after-error-or-quit ()
  (dolist (condition '(error quit))
    (my/pyim-test--with-gcmh
     (should
      (eq (condition-case caught
              (my/pyim--with-active-gc-a
               (lambda () (signal condition '("Test input interruption"))))
            (error (car caught))
            (quit (car caught)))
          condition))
     (should (= gc-cons-threshold 800000))
     (should (memq gcmh-idle-timer timer-list)))))

(ert-deftest my/pyim-gc-disabled-gcmh-leaves-threshold-and-timer-alone ()
  (my/pyim-test--with-gcmh
   (let ((gcmh-mode nil)
         (pending (run-with-timer 3600 nil #'ignore)))
     (setq gcmh-idle-timer pending)
     (should
      (eq (my/pyim--with-active-gc-a
           (lambda ()
             (should (= gc-cons-threshold 800000))
             'original-result))
          'original-result))
     (should (eq gcmh-idle-timer pending))
     (should (memq pending timer-list)))))

(ert-deftest my/pyim-gc-unavailable-gcmh-helper-falls-back ()
  (my/pyim-test--with-gcmh
   (cl-letf (((symbol-function 'gcmh-register-idle-gc) nil))
     (should
      (eq (my/pyim--with-active-gc-a
           (lambda ()
             (should (= gc-cons-threshold 800000))
             'original-result))
          'original-result))
     (should-not gcmh-idle-timer))))

(ert-deftest my/pyim-gc-does-not-restart-gcmh-disabled-during-input ()
  (my/pyim-test--with-gcmh
   (my/pyim--with-active-gc-a (lambda () (setq gcmh-mode nil)))
   (should-not gcmh-idle-timer)
   (should (= gc-cons-threshold 800000))))

(ert-deftest my/pyim-gc-configuration-reload-keeps-one-advice ()
  (with-temp-buffer
    (insert-file-contents my/pyim-test--config-file)
    (goto-char (point-min))
    (let ((form (catch 'found
                  (while t
                    (let ((candidate (read (current-buffer))))
                      (when (and (consp candidate)
                                 (eq (car candidate) 'after!)
                                 (eq (cadr candidate) 'pyim))
                        (throw 'found candidate)))))))
      ;; Reload only the actual guard and registration, without reloading dictionaries.
      (dotimes (_ 3)
        (dolist (item (cddr form))
          (when (or (and (consp item) (eq (car item) 'defun)
                         (eq (cadr item) 'my/pyim--with-active-gc-a))
                    (and (consp item) (eq (car item) 'advice-add)
                         (equal (cadr item) '(quote pyim-input-method))))
            (eval item t))))))
  (let ((count 0))
    (advice-mapc (lambda (function _properties)
                   (when (eq function #'my/pyim--with-active-gc-a)
                     (cl-incf count)))
                 'pyim-input-method)
    (should (= count 1))))

;;; pyim-gc-test.el ends here
