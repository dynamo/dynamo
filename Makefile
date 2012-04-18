EBIN_DIR=ebin
ERLC=erlc -W0
ERL=erl -noshell -pa $(EBIN_DIR)

.PHONY: setup test clean

compile: ebin

setup:
	git submodule update --init
	cd deps/ibrowse && make
	cd deps/cowboy && make

ebin: lib/*.ex lib/*/*.ex lib/*/*/*.ex
	@ rm -f ebin/::*.beam
	@ echo Compiling ...
	@ mkdir -p $(EBIN_DIR)
	@ touch $(EBIN_DIR)
	elixirc -pa deps/cowboy/ebin lib/*/*/*.ex lib/*/*.ex lib/*.ex -o ebin
	@ echo

test: compile
	@ echo Running tests ...
	time elixir -pa ebin -r "test/**/*_test.exs"
	@ echo

clean:
	rm -rf $(EBIN_DIR)
	@ echo
