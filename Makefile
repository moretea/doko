all:
	@rebar compile

clean:
	@rebar clean

eunit: all
	@rebar eunit skip_deps=true apps=doko_query,doko_utf8

ct: all
	@ERL_LIBS=apps/ rebar ct apps=doko_stemming

test: eunit ct
