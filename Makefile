.PHONY: all app tests clean distclean check_plt build_plt dialyzer

all: app

app:
	rebar compile

test:
	rebar eunit

clean:
	rebar clean

distclean: clean
	rm -rf .rebar .eunit $(PLT)

APPS = kernel stdlib erts
PLT = .dialyzer.plt
DIALYZER_FLAGS ?= -Wrace_conditions

check_plt:
	dialyzer --check_plt --plt $(PLT) --apps $(APPS)

build_plt:
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS)

dialyzer: app
	if ! test -e $(PLT); then $(MAKE) build_plt; fi
	dialyzer $(DIALYZER_FLAGS) --plt $(PLT) ebin
