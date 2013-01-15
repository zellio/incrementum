


(system "make runtime")

(load "test/scheme/tests-driver.scm")
(load "test/scheme/tests-1.1-req.scm")
(load "test/scheme/tests-1.2-req.scm")
(load "test/scheme/tests-1.3-req.scm")
(load "test/scheme/tests-1.4-req.scm")

(load "src/scheme/compiler.scm")

(test-all)

(system "make test_clean")
