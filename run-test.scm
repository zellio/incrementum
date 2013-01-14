


(load "test/scheme/tests-driver.scm")
(load "test/scheme/tests-1.1-req.scm")

(load "src/scheme/compiler.scm")

(test-all)

(system "make test_clean")
