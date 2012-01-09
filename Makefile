EBIN_DIR=ebin
ERLC=erlc -W0
ERL=erl -noshell -pa $(EBIN_DIR)

.PHONY: setup test clean

compile: ebin src/dynamo_router_parser.erl

setup:
	git submodule update --init
	cd deps/ibrowse && make
	cd deps/misultin && make

src/dynamo_router_parser.erl: src/dynamo_router_parser.yrl
	@ echo Compiling parser ...
	$(ERL) -eval 'yecc:file("$<"), halt().'
	@ mkdir -p $(EBIN_DIR)
	$(ERLC) -o $(EBIN_DIR) $@
	@ echo

ebin: lib/*.ex lib/*/*.ex lib/*/*/*.ex
	@ rm -f ebin/::*.beam
	@ echo Compiling ...
	@ mkdir -p $(EBIN_DIR)
	@ touch $(EBIN_DIR)
	elixirc lib/*/*/*.ex lib/*/*.ex lib/*.ex -o ebin
	@ echo

test: compile
	@ echo Running tests ...
	time exunit -pa ebin -- test/**/*_test.exs
	@ echo

clean:
	rm -f src/dynamo_router_scanner.erl
	rm -f src/dynamo_router_parser.erl
	rm -rf $(EBIN_DIR)
	@ echo
