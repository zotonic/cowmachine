-module(controller).

-export([
    execute/2,
    process/4
]).

%% Cowmachine API

%% Middleware export, place this module in the request as controller.
execute(Req, Env) ->
	{ok, Req, Env#{ cowmachine_controller => ?MODULE }}.

% Controller export
process(<<"GET">>, _AcceptedCT, _ProvidedCT, Context) ->
	{<<"Hello World!">>, Context}.
