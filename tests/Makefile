TESTDIRS = locate no-escape recovery

all:
	@for dir in $(TESTDIRS); do \
	    $(MAKE) one DIR=$$dir; \
	done

promote:
	@for dir in $(TESTDIRS); do \
	    $(MAKE) promote-one DIR=$$dir; \
	done

clean:
	@for dir in $(TESTDIRS); do \
	    $(MAKE) clean-one DIR=$$dir; \
	done

.PHONY: one
one:
	@for TESTDIR in $(DIR)/*; do \
	    echo "Running tests for '$$TESTDIR' ..." ; \
	    $(MAKE) --no-print-directory -C $$TESTDIR ; \
	done

.PHONY: promote-one
promote-one:
	@for TESTDIR in $(DIR)/*; do \
	    $(MAKE) -C $$TESTDIR promote ; \
	done

.PHONY: clean-one
clean-one:
	@for TESTDIR in $(DIR)/*; do \
	    $(MAKE) -C $$TESTDIR cleane ; \
	done
