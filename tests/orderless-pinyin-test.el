;;; orderless-pinyin-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'orderless)
(require 'pyim)
(require 'evil-pinyin)
(require 'consult)

(defconst my/orderless-pinyin-test--config-file
  (expand-file-name "../config.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Configuration source used to verify Orderless Pinyin reload behavior.")

(defmacro my/orderless-pinyin-test--with-completion (&rest body)
  "Run BODY with ordinary Orderless matching in a private buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((my/orderless-pinyin-max-query-length 8)
           (evil-pinyin-scheme 'simplified-quanpin-all)
           (orderless-matching-styles '(orderless-literal orderless-regexp))
           (orderless-style-dispatchers nil)
           (completion-styles '(orderless basic))
           (completion-category-defaults nil)
           (completion-category-overrides nil)
           (completion-ignore-case t))
       ,@body)))

(ert-deftest my/orderless-pinyin-long-identifier-filters-and-highlights ()
  (my/orderless-pinyin-test--with-completion
   (let* ((query "rejects_identifier_after_decimal_integer")
          (line (concat "fn " query "() {"))
          (matches (consult--completion-filter
                    query (list line "fn lexes_decimal_integer_tokens() {")
                    'consult-location t)))
     (should (equal (mapcar #'substring-no-properties matches) (list line)))
     (should (get-text-property 3 'face (car matches))))))

(ert-deftest my/orderless-pinyin-preview-finds-the-identifier-position ()
  (my/orderless-pinyin-test--with-completion
   (let ((query "rejects_identifier_after_decimal_integer")
         (consult-point-placement 'match-beginning))
     (insert "fn other_test() {}\nfn " query "() {}\n")
     (goto-char (point-min))
     (search-forward query)
     (let* ((expected-position (- (point) (length query)))
            (candidates (consult--line-candidates t 1))
            (selected (cadr candidates))
            ;; Consult's preview drops properties before resolving the location.
            (location (consult--line-match
                       (substring-no-properties selected) candidates query)))
       (should (= (car location) expected-position))
       (should (equal (cdr location) (list (cons 0 (length query)))))))))

(ert-deftest my/orderless-pinyin-eight-letter-components-remain-searchable ()
  (my/orderless-pinyin-test--with-completion
   (cl-loop for character from ?a to ?z do
            (let* ((query (make-string 8 character))
                   (regexps (cdr (orderless-compile query)))
                   (matches (consult--completion-filter
                             query (list query "unrelated")
                             'consult-location t)))
              (should regexps)
              (dolist (regexp regexps)
                (should (string-match-p regexp query)))
              (should (equal (mapcar #'substring-no-properties matches)
                             (list query)))))))

(ert-deftest my/orderless-pinyin-short-initials-keep-chinese-and-english ()
  (my/orderless-pinyin-test--with-completion
   (let ((matches (consult--completion-filter
                   "sz" '("数组" "sz" "ordinary") 'consult-location t)))
     (should (equal (mapcar #'substring-no-properties matches) '("数组" "sz")))
     (dolist (match matches)
       (should (get-text-property 0 'face match))))))

(ert-deftest my/orderless-pinyin-invalid-expansion-keeps-original-matches ()
  (my/orderless-pinyin-test--with-completion
   (dolist (expansion '(nil "" "["))
     (cl-letf (((symbol-function 'evil-pinyin--build-regexp-string)
                (lambda (&rest _) expansion)))
       (should (equal (mapcar #'substring-no-properties
                              (consult--completion-filter
                               "sz" '("sz" "unrelated") 'consult-location t))
                      '("sz")))))))

(ert-deftest my/orderless-pinyin-multiple-components-keep-unordered-matching ()
  (my/orderless-pinyin-test--with-completion
   (let ((matches (consult--completion-filter
                   "sz rejects_identifier_after_decimal_integer"
                   '("rejects_identifier_after_decimal_integer 数组"
                     "sz rejects_identifier_after_decimal_integer"
                     "rejects_identifier_after_decimal_integer"
                     "数组 other_test")
                   'consult-location t)))
     (should (equal (mapcar #'substring-no-properties matches)
                    '("rejects_identifier_after_decimal_integer 数组"
                      "sz rejects_identifier_after_decimal_integer"))))))

(ert-deftest my/orderless-pinyin-preserves-explicit-and-invalid-regexps ()
  (my/orderless-pinyin-test--with-completion
   (let ((regexp "^rejects_.*_integer$"))
     (should (equal (orderless-regexp regexp) regexp))
     (dolist (compiled (cdr (orderless-compile regexp nil '(ignore))))
       (should (string-match-p compiled "rejects_identifier_after_decimal_integer"))
       (should-not (string-match-p compiled "unrelated"))))
   (should-not (orderless-regexp "["))
   (should-not (my/orderless-pinyin--expand-regexp nil))
   ;; Invalid regexp input still has Orderless's ordinary literal alternative.
   (should (equal (mapcar #'substring-no-properties
                          (consult--completion-filter
                           "[" '("[value]" "other") 'consult-location t))
                  '("[value]")))))

(ert-deftest my/orderless-pinyin-configuration-reload-keeps-one-advice ()
  (let ((form
         (with-temp-buffer
           (insert-file-contents my/orderless-pinyin-test--config-file)
           (goto-char (point-min))
           (catch 'found
             (while t
               (let ((candidate (read (current-buffer))))
                 (when (and (consp candidate)
                            (eq (car candidate) 'after!)
                            (equal (cadr candidate) '(pyim orderless)))
                   (throw 'found candidate)))))))
        (count 0))
    (dotimes (_ 3) (eval form t))
    (advice-mapc (lambda (function _properties)
                   (when (eq function #'my/orderless-pinyin--expand-regexp)
                     (cl-incf count)))
                 'orderless-regexp)
    (should (= count 1))
    (should-not (advice-member-p #'evil-pinyin--build-regexp-string 'orderless-regexp))
    (should-not (advice-member-p #'pyim-cregexp-build 'orderless-regexp))))

;;; orderless-pinyin-test.el ends here
