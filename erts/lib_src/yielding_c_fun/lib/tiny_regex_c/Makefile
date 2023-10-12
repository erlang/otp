# Compiler to use - can be replaced by clang for instance
CC := gcc

# Number of random text expressions to generate, for random testing
NRAND_TESTS := 1000

PYTHON != if (python --version 2>&1 | grep -q 'Python 2\..*'); then \
            echo 'python';                                          \
          elif command -v python2 >/dev/null 2>&1; then             \
            echo 'python2';                                         \
          else                                                      \
            echo 'Error: no compatible python version found.' >&2;  \
            exit 1;                                                 \
          fi

# Flags to pass to compiler
CFLAGS := -O3 -Wall -Wextra -std=c99 -I.

all:
	@$(CC) $(CFLAGS) re.c tests/test1.c     -o tests/test1
	@$(CC) $(CFLAGS) re.c tests/test2.c     -o tests/test2
	@$(CC) $(CFLAGS) re.c tests/test_rand.c -o tests/test_rand
	@$(CC) $(CFLAGS) re.c tests/test_rand_neg.c -o tests/test_rand_neg

clean:
	@rm -f tests/test1 tests/test2 tests/test_rand
	@#@$(foreach test_bin,$(TEST_BINS), rm -f $(test_bin) ; )
	@rm -f a.out
	@rm -f *.o


test: all
	@$(test $(PYTHON))
	@echo
	@echo Testing hand-picked regex\'s:
	@./tests/test1
	@echo Testing patterns against $(NRAND_TESTS) random strings matching the Python implementation and comparing:
	@echo
	@$(PYTHON) ./scripts/regex_test.py \\d+\\w?\\D\\d             $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py \\s+[a-zA-Z0-9?]*          $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py \\w*\\d?\\w\\?             $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [^\\d]+\\\\?\\s            $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [^\\w][^-1-4]              $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [^\\w]                     $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [^1-4]                     $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [^-1-4]                    $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [^\\d]+\\s?[\\w]*          $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py a+b*[ac]*.+.*.[\\.].       $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py a?b[ac*]*.?[\\]+[?]?       $(NRAND_TESTS)
	@#python ./scripts/regex_test.py [1-5-]+[-1-2]-[-]         $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [-1-3]-[-]+                $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [1-5]+[-1-2]-[\\-]         $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [-1-2]*                    $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py \\s?[a-fKL098]+-?          $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [\\-]*                     $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [\\\\]+                    $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [0-9a-fA-F]+               $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [1379][2468][abcdef]       $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [012345-9]?[0123-789]      $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [012345-9]                 $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [0-56789]                  $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [abc-zABC-Z]               $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [a\d]?1234                 $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py .*123faerdig               $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py .?\\w+jsj$                 $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [?to][+to][?ta][*ta]       $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py \\d+                       $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [a-z]+                     $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py \\s+[a-zA-Z0-9?]*          $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py \\w                        $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py \\d                        $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [\\d]                      $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test.py [^\\d]                     $(NRAND_TESTS)
	@#python ./scripts/regex_test.py [^-1-4]                    $(NRAND_TESTS)
	@echo
	@echo
	@echo
	@echo Testing rejection of patterns against $(NRAND_TESTS) random strings also rejected by the Python implementation:
	@echo
	@$(PYTHON) ./scripts/regex_test_neg.py \\d+                   $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [a-z]+                 $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py \\s+[a-zA-Z0-9?]*      $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py ^\\w                   $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py ^\\d                   $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [\\d]                  $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py ^[^\\d]                $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [^\\w]+                $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py ^[\\w]+                $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py ^[^0-9]                $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [a-z].[A-Z]            $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [-1-3]-[-]+            $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [1-5]+[-1-2]-[\\-]     $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [-0-9]+                $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [\\-]+                 $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [\\\\]+                $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [0-9a-fA-F]+           $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [1379][2468][abcdef]   $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [012345-9]             $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py [0-56789]              $(NRAND_TESTS)
	@$(PYTHON) ./scripts/regex_test_neg.py .*123faerdig           $(NRAND_TESTS)
	@echo
	@echo
	@./tests/test2
	@echo
	@echo

