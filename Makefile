all: compile

app: compile
	@rebar generate force=1

compile:
	@rebar get-deps compile

tests: compile
	@rebar eunit

clean:
	@rebar clean
	rm -f erl_crash.dump

dist-clean: clean

