all:
	rebar compile
	-(cd c_src;$(MAKE) -k)

clean:
	(cd c_src;$(MAKE) clean)
	rebar clean

test: all
	rebar eunit

build_plt: all
	rebar build-plt

analyze: all
	rebar dialyze

doc: all
	rebar doc

xref: all
	rebar xref
	
run: all
	erl -pa ../fd_server/ebin
