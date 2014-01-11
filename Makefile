#
 ERLANG_HOME ?= /opt/erlang/release/latest

 GROONGA_HOME ?= /opt/groonga/release/latest

#
 CC = /opt/gnu/gcc/4.7.3/bin/gcc

 CFLAGS =
 CFLAGS += -std=c99
#CFLAGS += -D__unix
#CFLAGS += -D__STDC_FORMAT_MACROS -D__STDC_CONSTANT_MACROS
 CFLAGS += -g
 CFLAGS += -Wall
 CFLAGS += -Wextra
 CFLAGS += -Wstrict-prototypes
 CFLAGS += -fPIC
 CFLAGS += -fno-common

 LDFLAGS  =

#
 REBAR_BIN  = ./rebar

 REBAR_ENV  =
 REBAR_ENV += PATH=$(ERLANG_HOME)/bin:$(PATH)
 REBAR_ENV += ERL_LIB=..
 REBAR_ENV += GROONGA_HOME="$(GROONGA_HOME)"
 REBAR_ENV += CC="$(CC)"
 REBAR_ENV += CFLAGS="$(CFLAGS)"
 REBAR_ENV += LDFLAGS="$(LDFLAGS)"

 REBAR_OPT  =
#REBAR_OPT += --verbose 3

#
 ERL_OPT  =
 ERL_OPT += -pa ebin deps/*/ebin

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
all: build

delete-deps get-deps:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@

clean compile ct:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@ skip_deps=true

build: get-deps
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) compile

cleanall:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) clean

build_plt:
	@$(ERLANG_HOME)/bin/dialyzer --$@ --output_plt $(PLT) --apps deps/*/ebin

dialyzer:
	@$(ERLANG_HOME)/bin/dialyzer $(DIALYZER_OPT)

client driver: compile
	@$(ERLANG_HOME)/bin/erl $(ERL_OPT) -config files/$@

test: compile ct

distclean: clean delete-deps
	@-rm -f deps $(PLT)

#
x%: compile
	@ERL_FLAGS="" $(ERLANG_HOME)/bin/escript escript/$@.escript
#
start:
	$(GROONGA_HOME)/bin/groonga -p 10041 -d /tmp/groonga/x1 localhost
stop:
	$(GROONGA_HOME)/bin/groonga -p 10041 -c localhost shutdown
data:
	@mkdir -p /tmp/groonga
	$(GROONGA_HOME)/bin/groonga -n /tmp/groonga/x3 < files/x3.txt
