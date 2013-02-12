(system "make runtime.o")

(load "load.scm")

(test-all)

(system "make test_clean")
