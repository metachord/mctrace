all: compile

compile:
	rebar compile

clean:
	rebar clean

run: compile
	ERL_LIBS=../../../ erl -pa ebin -s mctrace
