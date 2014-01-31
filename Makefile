#
 ERLANG_HOME ?= /opt/erlang/release/latest

 GROONGA_HOME ?= /opt/groonga/release/latest

 CC ?= /opt/gnu/gcc/4.7.3/bin/gcc

#
 REBAR_BIN  = ./rebar

 REBAR_ENV  =
 REBAR_ENV += PATH=$(ERLANG_HOME)/bin:$(PATH)
 REBAR_ENV += GROONGA_HOME="$(GROONGA_HOME)"
 REBAR_ENV += CC="$(CC)"
 REBAR_ENV += ERL_LIBS=..

 REBAR_OPT  =
#REBAR_OPT += --verbose 3

#
 ERL_ENV  =
 ERL_ENV += ERL_LIBS=..:deps:deps/baseline/sub_dirs

 ERL_OPT  =
 ERL_OPT += -config priv/conf/$(1)

 PLT = .dialyzer_plt.local

 DIALYZER_OPT  =
 DIALYZER_OPT += --no_native
 DIALYZER_OPT += --plts $(ERLANG_HOME)/.dialyzer_plt $(PLT)
 DIALYZER_OPT += --src src
 DIALYZER_OPT += -I deps
 DIALYZER_OPT += -I ..

#
default: compile

#
delete-deps get-deps:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@

compile ct:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@ skip_deps=true


all: build

build: get-deps
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) compile

build_plt:
	@$(ERLANG_HOME)/bin/dialyzer --$@ --output_plt $(PLT) --apps deps/*/ebin

clean: delete-autosave
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@ skip_deps=true

delete-autosave:
	@-find . -name "*~" | xargs rm -f

dialyzer:
	@$(ERLANG_HOME)/bin/dialyzer $(DIALYZER_OPT)

distclean: clean delete-deps
	@-rm -rf deps $(PLT)

test: compile ct

#
client driver: compile
	@$(ERL_ENV) $(ERLANG_HOME)/bin/erl $(call ERL_OPT,$@)

x%: compile
	@$(ERL_ENV) $(ERLANG_HOME)/bin/escript escript/$@.escript
#
start:
	$(GROONGA_HOME)/bin/groonga -p 10041 -d /tmp/groonga/x1 localhost
stop:
	$(GROONGA_HOME)/bin/groonga -p 10041 -c localhost shutdown
data:
	@mkdir -p /tmp/groonga
	$(GROONGA_HOME)/bin/groonga -n /tmp/groonga/x3 < files/x3.txt
