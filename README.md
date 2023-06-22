[![Test](https://github.com/zotonic/cowmachine/actions/workflows/test.yml/badge.svg)](https://github.com/zotonic/cowmachine/actions/workflows/test.yml)
[![Hex.pm version][hexpm version]][hexpm]
[![Hex.pm Downloads][hexpm downloads]][hexpm]
[![Hex.pm Documentation][hexdocs documentation]][hexdocs]
[![Erlang Versions][erlang version badge]][gh]
[![License][license]](https://www.apache.org/licenses/LICENSE-2.0)


# Cowmachine

Webmachine for Zotonic and Cowboy

This is an adaptation of https://github.com/webmachine/webmachine for the Cowboy web server.

Cowmachine is a request handler for Cowboy.

Main differences with Basho’s Webmachine are:

 * Use cowboy instead of mochiweb for the http server
 * Separate dispatching
 * Simplified callbacks
 * Single `process/4` callback for all http methods
 * Caching of some callbacks (like `modified`)
 * More streaming options for returned data
 * Better support for range requests

## Installation

Cowmachine is at Hex, in your `rebar.config` file use:

```erlang
{deps, [
    cowmachine
]}.
```

You can also use the direct Git url and use the development version:

```erlang
{deps, [
    {cowmachine, {git, "https://github.com/zotonic/cowmachine.git", {branch, "master"}}}
]}.
```

## Calling Cowmachine

Cowmachine can be called from your Cowboy middleware:

```erlang
-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
    when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env) ->
    % Replace below with your own controller module and optionally wrap
    % the request in your own request-context record or map.
    EnvController = Env#{
        cowmachine_controller => mycontroller
    },
    RequestContext = cowmachine_req:init_context(Req, EnvController, #{}),
    % Set options for the cowmachine
    Options = #{
        on_request =>
            fun(Ctx) ->
                % Perform anything after initialization of your request
                % Examples are checking special cookies, changing headers, etc.
                Ctx
            end,
        on_welformed =>
            fun(Ctx) ->
                % Perform anything after well-formedness check of your request
                % Examples are parsing the query args, or authentication
                Ctx
            end,
        on_handled =>
            fun(Ctx) ->
                % Perform anything after processing, before sending the result.
                Ctx
            end
    },
    % Handle the request, returns updated Req and Env for next Cowboy middleware
    cowmachine:request(RequestContext, Options).
```

Or just use the default Cowmachine middleware:

```erlang
    #{
        middlewares => [
            % ... add your dispatcher middlware
            cowmachine
        ],
        request_timeout => 60000,
        env => #{
            % If no dispatcher, default to `mycontroller` as the cowmachine
            % controller.
            cowmachine_controller => mycontroller
        }
    }.
```

## Dispatching

You can use the [Zotonic Dispatch Compiler](https://github.com/zotonic/dispatch_compiler) to match your controller paths against the request.


## Controller

The controller provides the callbacks to handle the request.

The controller is an Erlang module implementing callback functions, only functions that return some non-default value need to be implemented.

For the controller callbacks and more documentation, check the [wiki pages](https://github.com/zotonic/cowmachine/wiki).

## Documentation generation

### Edoc

#### Generate public API
`rebar3 edoc`

#### Generate private API
`rebar3 as edoc_private edoc`

### ExDoc

`rebar3 ex_doc --logo doc/img/logo.png --output edoc -f html`

<!-- Badges -->
[hexpm]: https://hex.pm/packages/cowmachine
[hexpm version]: https://img.shields.io/hexpm/v/cowmachine.svg?style=flat-curcle "Hex version"
[hexpm downloads]: https://img.shields.io/hexpm/dt/cowmachine.svg?style=flat-curcle
[hexdocs documentation]: https://img.shields.io/badge/hex-docs-purple.svg?style=flat-curcle
[hexdocs]: https://hexdocs.pm/cowmachine
[gh]: https://github.com/zotonic/cowmachine/actions/workflows/test.yaml
[gh badge]: https://img.shields.io/github/workflow/status/zotonic/cowmachine/Test?style=flat-curcle
[erlang version badge]: https://img.shields.io/badge/Supported%20Erlang%2FOTP-22.0%20to%2024.0-blue.svg?style=flat-curcle
[license]: https://img.shields.io/badge/License-Apache_2.0-blue.svg?logo=apache&logoColor=red "Apache 2.0"

