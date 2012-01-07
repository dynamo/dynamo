EBIN_DIR=ebin
ERLC=erlc -W0
ERL=erl -noshell -pa $(EBIN_DIR)

.PHONY: test clean

compile: ebin src/dynamo_router_scanner.erl src/dynamo_router_parser.erl

src/dynamo_router_scanner.erl: src/dynamo_router_scanner.xrl
	@ echo Compiling lexer ...
	$(ERL) -eval 'leex:file("$<"), halt().'
	@ mkdir -p $(EBIN_DIR)
	$(ERLC) -o $(EBIN_DIR) $@
	@ echo

src/dynamo_router_parser.erl: src/dynamo_router_parser.yrl
	@ echo Compiling parser ...
	$(ERL) -eval 'yecc:file("$<"), halt().'
	@ mkdir -p $(EBIN_DIR)
	$(ERLC) -o $(EBIN_DIR) $@
	@ echo

ebin: lib/*.ex
	@ echo Compiling ...
	@ mkdir -p $(EBIN_DIR)
	@ touch $(EBIN_DIR)
	elixirc $< -o ebin
	@ echo

test: compile
	@ echo Running tests ...
	time exunit -- test/**/*_test.exs
	@ echo

clean:
	rm -f src/dynamo_router_scanner.erl
	rm -f src/dynamo_router_parser.erl
	rm -rf $(EBIN_DIR)
	@ echo
