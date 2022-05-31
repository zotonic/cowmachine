# Chunked hello world example

Demonstrate chunked data transfer with two one-second delays.

[Cowboy original example](https://github.com/ninenines/cowboy/tree/master/examples/chunked_hello_world)

> While Cowmachine is suitable for a great many Web applications it is 
> not a good fit for an application that will gradually or continuously stream 
> responses back to clients inside the context of a single HTTP Request.
[Decision Core](https://github.com/zotonic/cowmachine/wiki/Mechanics#decision-core)

An OTP application

## Build

`rebar3 escriptize`

## Run

`escript _build/default/bin/launcher`
	
## Build and run

`rebar3 escriptize && escript _build/default/bin/launcher`	


## HTTP/1.1 example output

### Using Cowboy router only (without middlewar)

```
$ time curl -i http://localhost:1234
HTTP/1.1 200 OK
date: Fri, 13 May 2022 13:27:31 GMT
server: Cowboy
transfer-encoding: chunked

Hello
World
Chunked!

real    0m2,158s
user    0m0,000s
sys     0m0,015s
```

### Using Cowmachine

The solition for Cowmachine API is not implemented.

```
$ time curl -i http://localhost:1234
HTTP/1.1 200 OK
content-length: 0
content-type: text/html;charset=utf-8
date: Wed, 18 May 2022 11:02:37 GMT
server: CowMachine/1.9.0+build.115.ref71f4831


real    0m0,176s
user    0m0,000s
sys     0m0,015s


```

## Stop web server

Type in `exit.` command and press `Enter`.
