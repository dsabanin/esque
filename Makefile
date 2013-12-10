NAME=esque

all:
	rebar get-deps compile

quick:
	rebar skip_deps=true compile

release:
	rebar get-deps compile generate

clean:
	rm -f test/*.beam
	rebar clean

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref

test: quick
	mkdir -p ./log/ct
	rebar -v skip_deps=true ct ${CT_ARGS}; open logs/index.html\

run: all
	rel/${NAME}/bin/${NAME} console

run_local: all
	erl -boot rel/files/esque -mode embedded -config rel/files/sys.config -args_file rel/files/vm.args -- console

dialyze:
	rebar -DDIALYZER skip_deps=true clean compile && \
	dialyzer --verbose --plt .dialyzer-R15B01.plt \
		-Wunmatched_returns -Werror_handling \
		ebin/

soft_dialyze:
	rebar -DDIALYZER skip_deps=true clean compile && \
	dialyzer --verbose --plt .dialyzer-R15B01.plt \
		-Werror_handling ebin/ | grep -v "Callback info"

dialyze_plt: all
	dialyzer --verbose --build_plt \
	  --output_plt .dialyzer-R15B01.plt -Werror_handling \
		--apps kernel stdlib sasl erts ssl \
		  tools os_mon runtime_tools crypto \
			inets xmerl webtool eunit syntax_tools \
			compiler edoc hipe mnesia otp_mibs public_key \
			snmp -pa ebin/

warn:
	rebar -DDIALYZER skip_deps=true clean compile | grep Warning
