EBIN_DIR=ebin
ERLC=erlc -W0
ERL=erl -noshell -pa $(EBIN_DIR)

.PHONY: setup test compile clean

compile: $(EBIN_DIR)

setup:
	git submodule update --init
	cd deps/ibrowse && make
	cd deps/cowboy && make

$(EBIN_DIR): $(shell find lib -type f -name "*.ex")
	@ rm -rf ebin
	@ echo Compiling ...
	@ mkdir -p $(EBIN_DIR)
	@ touch $(EBIN_DIR)
	elixirc -pa deps/cowboy/ebin lib -o ebin
	@ echo

test: compile
	@ echo Running tests ...
	time elixir -pa ebin -r "test/**/*_test.exs"
	@ echo

clean:
	rm -rf $(EBIN_DIR)
	@ echo
