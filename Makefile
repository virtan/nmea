compile: get-deps
	@./rebar compile

get-deps:
	@./rebar get-deps

generate: compile
	@./rebar generate

clean:
	@./rebar clean ; rm -rf rel/nmea

console: generate
	cd rel/nmea && bin/nmea console ; cd ../../
