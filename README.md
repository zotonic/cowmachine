# Cowmachine

Webmachine for Zotonic and Cowboy

This is an adaptation of https://github.com/webmachine/webmachine for the Cowboy web server.

Cowmachine is a request handler for Cowboy.

Main differences with Basho’s Webmachine are:

 * Use cowboy instead of mochiweb for the http server
 * Separate dispatching
 * Simplified callbacks
 * Caching of some callbacks (like `modified`)
 * More streaming options for returned data
 * Better support for range requests

## Installation

Cowmachine is at Hex, in your `rebar.config` file use:

```erlang
{deps, [
    {cowmachine, "1.0.0"}
]}.
```

You can also use the direct Git url and use the development version:

```erlang
{deps, [
    {cowmachine, {git, "", {branch, "master"}}}
]}.
```

## Calling Cowmachine

Cowmachine can be called from your Cowboy middleware:

```erlang
-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
    when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env) ->
    % Replace below with your own controller module
    Controller = mycontroller,
    ControllerOpts = [ ],
    % Set options for the cowmachine and handle the request
    Options = #{
        on_welformed =>
            fun(Ctx) ->
                % Perform anything after well-formedness check of your request
                % Examples are parsing the query args, or authentication
            end
    },
    cowmachine:request(Controller, ControllerOpts, Req, Env, Options, Context3).
```

## Dispatching

You can use the [Zotonic Dispatch Compiler](https://github.com/zotonic/dispatch_compiler) to match your controller paths against the request.


## Controller

The controller provides the callbacks to handle the request.

The controller is an Erlang module implementing callback functions, only functions that return some non-default value need to be implemented.

For the controller callbacks and more documentation, check the [wiki pages](https://github.com/zotonic/cowmachine/wiki).

