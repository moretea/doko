all:
	@rebar compile

clean:
	@rebar clean

eunit:
	@rebar eunit skip_deps=true apps=doko_query,doko_utf8

ct:
	@ERL_LIBS=apps rebar ct apps=doko_stemming

systest:
	@./systest.pl

test: clean all eunit ct systest
