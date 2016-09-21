.PHONY: all compile rel run test clean
.PHONY: dialyzer

REBAR=./rebar3

all: $(REBAR) compile

compile:
				$(REBAR) compile

rel: compile
				$(REBAR) release
				$(REBAR) tar

run:
				erl -pa _build/default/lib/*/ebin -config config/sys.config -args_file config/vm.args -boot start_sasl -s sync -s insomnia

test:
				$(REBAR) eunit skip_deps=true verbose=3
				$(REBAR) ct skip_deps=true verbose=3

clean:
				$(REBAR) clean
				rm -rf ./log
				rm -rf ./erl_crash.dump

dialyzer:
				$(REBAR) dialyzer

