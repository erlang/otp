
ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")


.PHONY: deps test build

all: build test docs

build: $(REBAR3)
	@$(REBAR3) compile

deps:
	@$(REBAR3) get-deps

clean:
	@$(REBAR3) clean

distclean: clean
	@$(REBAR3) delete-deps

docs:
	@$(REBAR3) edoc


test:
	@$(REBAR3) ct


release: test
	@$(REBAR3) release
