;;; stpl-tests.el --- Tests for stpl template engine  -*- lexical-binding: t; -*-

(require 'ert)
(require 'stpl)

(ert-deftest stpl-plain-text ()
  (should (equal (stpl-render "Hello, world") "Hello, world"))
  (should (equal (stpl-render "") "")))

(ert-deftest stpl-simple-expression ()
  (should (equal (stpl-render "{{ 42 }}") "42"))
  (should (equal (stpl-render "{{ (+ 1 2) }}") "3"))
  (should (equal (stpl-render "{{ \"foo\" }}") "foo"))
  (should (equal (stpl-render "{{ nil }}") "nil"))
  (should (equal (stpl-render "{{ }}") "")))

(ert-deftest stpl-expression-with-text ()
  (should (equal (stpl-render "The answer is {{ 42 }}!") "The answer is 42!"))
  (should (equal (stpl-render "{{ (+ 1 2) }} + {{ 3 }} = {{ 6 }}")
                 "3 + 3 = 6")))

(ert-deftest stpl-variable-environment ()
  (should (equal (stpl-render-with-env "{{ x }}" '((x . 10))) "10"))
  (should (equal (stpl-render-with-env "{{ (concat \"Hello, \" name) }}"
                                       '((name . "World")))
                 "Hello, World"))
  (should (equal (stpl-render-with-env "{{ (format \"%s-%s\" a b) }}"
                                       '((a . "foo") (b . "bar")))
                 "foo-bar")))

(ert-deftest stpl-let-binding ()
  (should (equal (stpl-render-with-env
                   "{{ (let ((x 10)) (+ x 5)) }}" nil)
                 "15")))

(ert-deftest stpl-simple-sub-template ()
  (should (equal (stpl-render "{%x%}") "x"))
  (should (equal (stpl-render "hello {%world%}") "hello world"))
  ;; Spaces inside {% %} are preserved (like "{% {{ 1 }} %}" → " 1 ")
  (should (equal (stpl-render "{% {{ 1 }} %}") " 1 ")))

(ert-deftest stpl-nested-sub-in-expression ()
  ;; No extra spaces inside {% %} to get a clean quoted value
  (should (equal (stpl-render-with-env
                   "{{ (concat \"a\" {%{{ x }}%}) }}"
                   '((x . "b")))
                 "ab")))

(ert-deftest stpl-recursive-nesting ()
  ;; No extra spaces for clean let binding
  (should (equal (stpl-render
                   "{{ (let ((inner {%{{ 10 }}%})) (concat inner inner)) }}")
                 "1010")))

(ert-deftest stpl-unmatched-double-braces ()
  (should-error (stpl-render "{{ 1"))
  (should-error (stpl-render "{{ 1 }")))

(ert-deftest stpl-unmatched-percent-braces ()
  (should-error (stpl-render "{% 1"))
  (should-error (stpl-render "{% 1 %")))

(ert-deftest stpl-mixed-nesting-error ()
  (should-error (stpl-render "{{ {% }}"))
  (should-error (stpl-render "{% {{ %}")))

(ert-deftest stpl-empty-input ()
  (should (equal (stpl-render "") "")))

(ert-deftest stpl-only-spaces ()
  (should (equal (stpl-render "   ") "   ")))

(ert-deftest stpl-expression-returning-nil ()
  (should (equal (stpl-render "{{ (progn nil) }}") "nil")))

(ert-deftest stpl-expression-returning-symbol ()
  (should (equal (stpl-render "{{ 'hello }}") "hello")))

(defvar my-var)
(ert-deftest stpl-dynamic-environment ()
  (let ((my-var 42))
    (declare (special my-var))
    (should (equal (stpl-render "{{ my-var }}") "42"))))

(ert-deftest stpl-multiple-expressions ()
  (should (equal (stpl-render "{{ 1 }}{{ 2 }}{{ 3 }}") "123")))

(ert-deftest stpl-sub-template-with-spaces ()
  ;; Spaces around {% %} remain
  (should (equal (stpl-render "  {% x %}  ") "   x   ")))

(provide 'stpl-tests)
;;; stpl-tests.el ends here
