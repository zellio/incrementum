

(system "make runtime")

(load "load.scm")

(test-all)

(system "make test_clean")
