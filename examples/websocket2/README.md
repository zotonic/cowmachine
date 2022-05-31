# Websocket example


[Cowboy original example](https://github.com/ninenines/cowboy/tree/master/examples/websocket)

This variant of implementation use `priv file`, `priv dir` features of Cowboy Web Server as a source location for static content.

## Build

`$ rebar3 compile`

## Run

`rebar3 shell`
	
## Build and run

`rebar3 compile && rebar3 shell`

### Request HTML:

`$ curl -i https://localhost:1234`

### How to check working with WebSocket possibilities

To check WebSocket connections install Chrome addon ["WebSocket King Client"](https://chrome.google.com/webstore/detail/websocket-king-client/cbcbkhdmedgianpaifchdaddpnmgnknn/) then enter the link ws://localhost:1234/websocket to input field.

Then point your browser to http://localhost:1234


## How to work with the demo
	
1. Build and run.
2. Point your browser to http://localhost:1234.
3. Press "connection" button.

## How to switch Cowboy/Comachine working flow

Note that you get two working flow (Cowboy and Comachine). By default flow selection is random. 
For instance, to use Cowboy way only please edit `main.erl` file content. 
Find out `cowboy_options(Dispatch)` function and set `TypeOfCallingCowmachine` value to `1` into it.

## How stop the application

Type in `q().` command and press `Enter`. 