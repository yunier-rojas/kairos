
.PHONY: format
format:
	$(foreach file, $(wildcard src/*.erl), rebar3 as test fmt $(file);)
	$(foreach file, $(wildcard src/*.src), rebar3 as test fmt $(file);)
	$(foreach file, $(wildcard test/*.erl), rebar3 as test fmt $(file);)
	$(foreach file, $(wildcard test/*.src), rebar3 as test fmt $(file);)
	rebar3 as test fmt kairos.config
	rebar3 as test fmt kairos.config.example


.PHONY: tests
tests:
	rebar3 as test ct

.PHONY: cover
cover:
	rebar3 as test ct --cover --dir test
	rebar3 as test cover
	python -m webbrowser "file://${CURDIR}/_build/test/cover/index.html"

.PHONY: types
types:
	rebar3 dialyzer

.PHONY: shell
shell:
	rebar3 shell


.PHONY: docs
docs:
	rebar3 edoc


.PHONY: clean
clean:
	rm -rf src/*.beam erl_crash.dump


