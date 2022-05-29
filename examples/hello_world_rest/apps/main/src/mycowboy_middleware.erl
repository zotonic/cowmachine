-module(mycowboy_middleware).

-export([execute/2]).

%% @doc Call cowmachine to handle the request with the given controller. 

-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
    when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env) ->
	% Replace below with your own controller module
    Controller = mycontroller,
	
	Context = #{
		controller => Controller,
		cowreq => Req,
        cowenv => Env
		},
	
    % Set options for the cowmachine and handle the request
    Options = #{
        on_welformed =>
            fun(Ctx) ->
                % Perform anything after well-formedness check of your request
                % Examples are parsing the query args, or authentication
				Ctx
            end
    },
	cowmachine:request(Context, Options).	

