# Websocket example


[Cowboy original example](https://github.com/ninenines/cowboy/tree/master/examples/websocket)

An OTP application

## Build

`rebar3 escriptize`

## Run

`escript _build/default/bin/launcher`
	
## Build and run

`rebar3 escriptize && escript _build/default/bin/launcher`

### How to check working with WebSocket possibilities

To check WebSocket connections install Chrome addon ["WebSocket King Client"](https://chrome.google.com/webstore/detail/websocket-king-client/cbcbkhdmedgianpaifchdaddpnmgnknn/) then enter the link ws://localhost:1234/websocket to input field.

Then point your browser to http://localhost:1234


## How to work with the demo
	
1. Build and run.
2. Point your browser to http://localhost:1234.
2. Press "connection" button.

## How to switch Cowboy/Comachine working flow

Note that you get two working flow (Cowboy and Comachine). By default flow selection is random. 
For instance, to use Cowboy way only please edit `apps/main/main_app.erl` file content. 
Find out `cowboy_options(Dispatch)` function and set `TypeOfCallingCowmachine` value to `1` into it.

## Stop web server

Type in `exit.` command and press `Enter`.
