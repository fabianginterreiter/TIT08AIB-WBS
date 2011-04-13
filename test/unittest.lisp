(load "libs/vendor/lisp-unit.lisp")
(load "../src/libs/vendor/versionspace.lsp")

(load "../src/libs/aq.lisp")

(use-package :lisp-unit)

(define-test test-remove-covered-elements
	(assert-equal '() (remove-covered-elements '("*") '(("1") ("2") ("3"))))
	(assert-equal '(("2" "1") ("3" "4")) (remove-covered-elements '("1" "*") '(("1" "2") ("1" "3") ("2" "1") ("3" "4") ("1" "5"))))
)

(define-test test-count-stars
	(assert-equal 0 (count-stars '("1" "2")))
	(assert-equal 1 (count-stars '("*")))
	(assert-equal 1 (count-stars '("1" "*" "2")))
	(assert-equal 3 (count-stars '("*" "*" "*")))
)

(define-test test-choose-best-generalisation
	(assert-equal '("*" "1" "2") (choose-best-generalisation '(("*" "1" "2") ("*" "*" "2") ("2" "*" "*") ("*" "2" "*"))))
	(assert-equal '("*" "1" "2") (choose-best-generalisation '(("*" "1" "2") ("*" "3" "2") ("2" "*" "*") ("*" "2" "*"))))
	(assert-equal '("*" "1" "2") (choose-best-generalisation '(("*" "*" "2") ("2" "*" "*") ("*" "1" "2") ("*" "2" "*"))))
	(assert-equal nil (choose-best-generalisation nil))
)

(define-test test-choose-less-stars
	(assert-equal '("*" "1" "2") (choose-less-stars '(("*" "1" "2") ("*" "*" "2") ("2" "*" "*") ("*" "2" "*"))))
	(assert-equal '("*" "1" "2") (choose-less-stars '(("*" "1" "2") ("*" "3" "2") ("2" "*" "*") ("*" "2" "*"))))
	(assert-equal '("*" "1" "2") (choose-less-stars '(("*" "*" "2") ("2" "*" "*") ("*" "1" "2") ("*" "2" "*"))))
	(assert-equal nil (choose-less-stars nil))
)

(define-test test-isMoreGeneral
	(assert-equal T (isMoreGeneral '("*") '("1")))
	(assert-equal T (isMoreGeneral '("*" "2" "*") '("1" "2" "3")))
	(assert-equal T (isMoreGeneral '("*" "2" "3") '("1" "2" "3")))
	(assert-equal nil (isMoreGeneral '("*" "2" "3") '("1" "3" "3")))	
	(assert-equal nil (isMoreGeneral '("2") '("1")))
	(assert-equal nil (isMoreGeneral '("2") '("*")))
	(assert-equal T (isMoreGeneral '("2") '("2")))
)

(define-test test-is-element-good
	(assert-equal T (is-element-good '("1") '(("*"))))
	(assert-equal T (is-element-good '("1") '(("1"))))
	(assert-equal nil (is-element-good '("1") '(("2"))))
	(assert-equal T (is-element-good '("1" "2" "3") '(("1" "5" "*") ( "1" "*" "*"))))
	(assert-equal nil (is-element-good '("1" "2" "3") '(("1" "5" "*") ( "2" "*" "*"))))
)


(run-tests)
