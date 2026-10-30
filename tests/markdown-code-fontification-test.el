;;; markdown-code-fontification-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'markdown-mode)

(defconst my/markdown-test--config-file
  (expand-file-name "../+ui.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Configuration source used to verify Markdown reload behavior.")

(defun my/markdown-test--configuration-form ()
  "Read the Markdown configuration form without loading unrelated settings."
  (with-temp-buffer
    (insert-file-contents my/markdown-test--config-file)
    (goto-char (point-min))
    (catch 'found
      (while t
        (let ((form (read (current-buffer))))
          (when (and (consp form)
                     (eq (car form) 'after!)
                     (eq (cadr form) 'markdown-mode))
            (throw 'found form)))))))

(ert-deftest my/markdown-explicit-mode-skips-grammar-probes ()
  (let ((markdown-code-lang-modes '(("bash" . sh-mode))))
    (cl-letf (((symbol-function 'treesit-language-available-p)
               (lambda (&rest _) (ert-fail "Unexpected grammar probe"))))
      (should (eq (markdown-get-lang-mode "bash") 'sh-mode))
      (should (eq (markdown-get-lang-mode "BASH") 'sh-mode)))))

(ert-deftest my/markdown-explicit-mode-preserves-case-priority ()
  (let ((markdown-code-lang-modes (copy-tree '(("BASH" . emacs-lisp-mode)
                                               ("bash" . sh-mode)))))
    (should (eq (markdown-get-lang-mode "BASH") 'emacs-lisp-mode))
    (should (eq (markdown-get-lang-mode "Bash") 'sh-mode))
    (setcdr (assoc "BASH" markdown-code-lang-modes)
            'my/markdown-test-missing-mode)
    (should (eq (markdown-get-lang-mode "BASH") 'sh-mode))))

(ert-deftest my/markdown-explicit-mode-observes-mapping-changes ()
  (let ((markdown-code-lang-modes (list (cons "bash" 'sh-mode))))
    (should (eq (markdown-get-lang-mode "bash") 'sh-mode))
    (setcdr (car markdown-code-lang-modes) 'emacs-lisp-mode)
    (should (eq (markdown-get-lang-mode "bash") 'emacs-lisp-mode))))

(ert-deftest my/markdown-missing-or-invalid-mapping-calls-original-once ()
  (dolist (mappings '(nil (("bash" . my/markdown-test-missing-mode))))
    (let ((markdown-code-lang-modes mappings)
          (language (copy-sequence "bash"))
          (calls 0))
      (should
       (eq (my/markdown--prefer-explicit-mode-a
            (lambda (lang)
              (cl-incf calls)
              (should (eq lang language))
              'original-result)
            language)
           'original-result))
      (should (= calls 1)))))

(ert-deftest my/markdown-ineligible-tree-sitter-mapping-falls-back ()
  (let ((markdown-code-lang-modes '(("bash" . my/markdown-test-ts-mode)))
        (major-mode-remap-alist nil)
        (auto-mode-alist nil)
        (calls 0))
    (cl-letf (((symbol-function 'my/markdown-test-ts-mode) #'ignore))
      (should
       (eq (my/markdown--prefer-explicit-mode-a
            (lambda (_lang) (cl-incf calls) 'original-result)
            "bash")
           'original-result))
      (should (= calls 1)))))

(ert-deftest my/markdown-unavailable-predicate-calls-original ()
  (let ((markdown-code-lang-modes '(("bash" . sh-mode)))
        (calls 0))
    (cl-letf (((symbol-function 'markdown--lang-mode-predicate) nil))
      (should
       (eq (my/markdown--prefer-explicit-mode-a
            (lambda (_lang) (cl-incf calls) 'original-result)
            "bash")
           'original-result))
      (should (= calls 1)))))

(ert-deftest my/markdown-configuration-reload-keeps-one-advice ()
  (let ((form (my/markdown-test--configuration-form))
        (count 0))
    (dotimes (_ 3) (eval form t))
    (advice-mapc (lambda (function _properties)
                   (when (eq function #'my/markdown--prefer-explicit-mode-a)
                     (cl-incf count)))
                 'markdown-get-lang-mode)
    (should (= count 1))))

(defun my/markdown-test--highlighting (language code)
  "Return the selected mode and character faces for LANGUAGE and CODE."
  (with-temp-buffer
    (rename-buffer (generate-new-buffer-name "*Markdown fontification test*"))
    (insert "```" language "\n" code "```\n")
    (delay-mode-hooks (gfm-mode))
    (setq-local markdown-fontify-code-blocks-natively t)
    (font-lock-mode 1)
    (font-lock-ensure)
    (list (markdown-get-lang-mode language)
          (cl-loop for position from (point-min) below (point-max)
                   collect (list (get-text-property position 'face)
                                 (get-text-property position 'font-lock-face))))))

(ert-deftest my/markdown-code-highlighting-matches-original ()
  (dolist (example '(("bash" . "# 中文注释\nprintf '%s\\n' \"$HOME\"\n")
                     ("elisp" . ";; 中文注释\n(message \"hello\")\n")
                     ("rust" . "// 中文注释\nfn main() { let count = 42; }\n")))
    (let (original)
      (unwind-protect
          (progn
            (advice-remove 'markdown-get-lang-mode
                           #'my/markdown--prefer-explicit-mode-a)
            (setq original (my/markdown-test--highlighting
                            (car example) (cdr example))))
        (advice-add 'markdown-get-lang-mode :around
                    #'my/markdown--prefer-explicit-mode-a))
      (should (equal original
                     (my/markdown-test--highlighting
                      (car example) (cdr example)))))))

;;; markdown-code-fontification-test.el ends here
