;;; pyim-dart-input-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'pyim)
(require 'dart-ts-mode)

(defmacro my/pyim-dart-test--at (text &rest body)
  "Run BODY at the | marker in Dart TEXT, without starting language services."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert "void main() {\n  " ,text "\n}\n")
     (goto-char (point-min))
     (search-forward "|")
     (delete-char -1)
     (let ((position (point)))
       (delay-mode-hooks (dart-ts-mode))
       (goto-char position)
       ,@body)))

(ert-deftest my/pyim-dart-nested-string-literals-allow-chinese ()
  (dolist (text '("Text('总数 ${pieces == null ? '-' : '$pieces |件'}');"
                  "Text('总|数 ${pieces == null ? '-' : '$pieces 件'}');"
                  "Text('总数 ${pieces == null ? '|-' : '$pieces 件'}');"
                  "Text('总数 ${pieces == null ? '|' : '$pieces 件'}');"))
    (my/pyim-dart-test--at text
      (should-not (my/pyim-probe-program-mode)))))

(ert-deftest my/pyim-dart-interpolation-expressions-stay-english ()
  (dolist (text '("Text('总数 ${|pieces == null ? '-' : '$pieces 件'}');"
                  "Text('总数 ${pieces == |null ? '-' : '$pieces 件'}');"
                  "Text('总数 ${pieces == null ? '-' : '$|pieces 件'}');"
                  "Text('总数 ${pieces == null ? '-' : '$pieces| 件'}');"))
    (my/pyim-dart-test--at text
      (should (my/pyim-probe-program-mode)))))

(ert-deftest my/pyim-dart-insertion-boundaries-follow-the-left-context ()
  (dolist (text '("Text('总数 |${pieces}');"
                  "Text('总数 ${pieces}|件');"
                  "Text('|');"
                  "Text('文字|');"))
    (my/pyim-dart-test--at text
      (should-not (my/pyim-probe-program-mode))))
  (dolist (text '("Text(|'文字');"
                  "Text('文字'|);"
                  "Text('文字'); |"))
    (my/pyim-dart-test--at text
      (should (my/pyim-probe-program-mode)))))

(ert-deftest my/pyim-dart-comments-raw-and-multiline-strings-allow-chinese ()
  (dolist (text '("// 注释|"
                  "/// 文档|"
                  "/* 注|释 */"
                  "/* 注释 */|"
                  "Text('总数 ${format(/* 注|释 */ pieces)}');"
                  "Text(r'原|文');"
                  "Text('''多行\n文|字''');"
                  "Text(\"双|引号\");"))
    (my/pyim-dart-test--at text
      (if (equal text "/* 注释 */|")
          (should (my/pyim-probe-program-mode))
        (should-not (my/pyim-probe-program-mode))))))

(ert-deftest my/pyim-dart-probe-keeps-other-modes-and-parser-fallback ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(message \"中文\")")
    (goto-char (point-min))
    (should (my/pyim-probe-program-mode))
    (search-forward "中文")
    (should-not (my/pyim-probe-program-mode)))
  (my/pyim-dart-test--at "Text('文|字');"
    (cl-letf (((symbol-function 'treesit-parser-list) (lambda () nil)))
      (should-not (my/pyim-probe-program-mode)))))

(ert-deftest my/pyim-dart-toggle-and-conversion-work-in-nested-string ()
  (my/pyim-dart-test--at "Text('总数 ${pieces == null ? '-' : '$pieces |件'}');"
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (evil-local-mode 1)
      (evil-normal-state)
      (setq-local evil-input-method nil)
      (let ((unread-command-events nil))
        (execute-kbd-macro (kbd "C-\\"))
        (should-not current-input-method)
        (should (equal evil-input-method "pyim"))
        (evil-insert-state)
        (should (equal current-input-method "pyim"))
        (execute-kbd-macro (kbd "C-\\"))
        (should-not current-input-method)
        (should-not evil-input-method)
        (execute-kbd-macro (kbd "C-\\"))
        (should (equal current-input-method "pyim"))
        (setq-local pyim-english-input-switch-functions
                    '(my/pyim-probe-program-mode pyim-probe-isearch-mode))
        (let ((before (buffer-string))
              (pyim-process--input-ascii nil)
              (unread-command-events (list ?i ?a ?n 32)))
          (should (cl-some (lambda (event) (> event 127))
                           (pyim-input-method ?j)))
          (should (equal before (buffer-string))))
        (evil-normal-state)))))

;;; pyim-dart-input-test.el ends here
