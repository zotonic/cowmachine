# File server implementation


[Cowboy original example](https://github.com/ninenines/cowboy/tree/master/examples/file_server)

## Build

`$ rebar3 get-deps && rebar3 compile`

## Run

`rebar3 shell`
	
## Build and run

`rebar3 get-deps && rebar3 compile && rebar3 shell`

Point your browser to http://localhost:1234


## How to work with the demo
	
1. Build and run.
2. Point your browser to http://localhost:1234.


## How to switch Cowboy/Comachine working flow

Note that you get two working flow (Cowboy and Comachine). By default flow selection is random. 
For instance, to use Cowboy way only please edit `main.erl` file content. 
Find out `cowboy_options(Dispatch)` function and set `TypeOfCallingCowmachine` value to `1` into it.

## How stop the application

Type in `q().` command and press `Enter`. 