-module(main_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	{ok, CurrentDir} = file:get_cwd(),
	SslDir = filename:join([CurrentDir, "priv", "ssl"]),
	TransportOpts = [
		{port,      8443},
		{cacertfile, filename:join([SslDir, "cowboy-ca.crt"])},
		{certfile,   filename:join([SslDir, "server.crt"])},
		{keyfile,    filename:join([SslDir, "server.key"])}
	],
	{ok, ListenerPid} = cowboy:start_tls(
		https,
		TransportOpts,
		#{middlewares => [controller, cowmachine]}),
	{ok, ListenerPid}.

stop(_State) ->
    ok.