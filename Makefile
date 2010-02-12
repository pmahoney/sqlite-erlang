## usage:
## just "make" or "make NOTEST=true" for release build.

VERSION=1.0.2

ERL=$(shell which erl)
ERLC=$(shell which erlc)
BASE_PROG='io:format("~s~n", [code:lib_dir()])'
BASE=$(shell erl -eval $(BASE_PROG) -s init stop -noshell)

DEPS="[kernel, stdlib, crypto, compiler, syntax_tools, hipe]"
PROG='lists:foreach(fun(M) -> io:format("~s", [code:lib_dir(M)]) end)'
PATHS=`erl -eval '$(PROG)' -s init stop -noshell`

INSTALL_DEST=$(BASE)/sqlite-$(VERSION)

ifdef NOTEST
EMAKE_SRC = Emakefile.release
else
EMAKE_SRC = Emakefile.devel
NOTEST=
endif

all: compile docs

compile:
	cp $(EMAKE_SRC) Emakefile
	$(ERL) -make
	cd priv && make NOTEST=$(NOTEST)

install: compile
	mkdir -p $(INSTALL_DEST)/ebin
	install -m 644 ebin/sqlite.beam ebin/sqlite_lib.beam $(INSTALL_DEST)/ebin
	mkdir -p $(INSTALL_DEST)/priv
	install priv/sqlite_port $(INSTALL_DEST)/priv

static:
	dialyzer --build_plt --output_plt sqlite.plt -r ebin $(PATHS)

clean:
	- rm -rf ebin/*.beam doc/* sqlite.plt src/test/*.beam
	- rm -rf ct.db
	- find . -name "*~" | xargs rm -f
	cd priv && make clean
	- rm Emakefile

docs:
	$(ERL) -noshell -run edoc_run application "'sqlite'" '"."' '[{title,"Welcome to sqlite"},{hidden,false},{private,false}]' -s erlang halt

test: compile
	- rm -rf ct.db
	$(ERL) -pa ./ebin -noshell -run test unit -s erlang halt
