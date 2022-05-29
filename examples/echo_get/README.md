# GET parameter echo example


[Cowboy original example](https://github.com/ninenines/cowboy/tree/master/examples/echo_get)

An OTP application

## Build

`rebar3 escriptize`

## Run

`escript _build/default/bin/launcher`
	
## Build and run

`rebar3 escriptize && escript _build/default/bin/launcher`	

	
Point your browser to http://localhost:1234/?echo=hello

You can replace the echo parameter with another to check that the handler is echoing it back properly.

Or point your browser to `http://localhost:1234` in your browser and get a result.

## How to switch Cowboy/Comachine working flow

Note that you get two working flow (Cowboy and Comachine). By default flow selection is random. 
For instance, to use Cowboy way only please edit `apps/main/main_app.erl` file content. 
Find out `cowboy_options(Dispatch)` function and set `TypeOfCallingCowmachine` value to `1` into it.


## HTTP/1.1 example output

### Using Cowboy router only (without middlewar)

```
$ curl -i "http://localhost:1234/?echo=saymyname"
HTTP/1.1 200 OK
content-length: 9
content-type: text/plain; charset=utf-8
date: Sun, 15 May 2022 17:41:42 GMT
server: Cowboy

saymyname

```

```
$ curl -i "http://localhost:1234"
HTTP/1.1 400 Bad Request
content-length: 23
date: Sun, 15 May 2022 19:23:48 GMT
server: Cowboy

Missing echo parameter.

```

```
$ curl -i "http://localhost:1234/?echo1=1"
HTTP/1.1 400 Bad Request
content-length: 23
date: Sun, 15 May 2022 19:26:49 GMT
server: Cowboy

Missing echo parameter.

```

POST:

```
$ curl -i -d echo=saymyname http://localhost:1234
HTTP/1.1 405 Method Not Allowed
content-length: 0
date: Sun, 15 May 2022 19:34:16 GMT
server: Cowboy
```

### Using Cowmachine

```
$ curl -i "http://localhost:1234/?echo=saymyname"
HTTP/1.1 200 OK
content-length: 9
content-type: text/html;charset=utf-8
date: Sun, 15 May 2022 19:11:55 GMT
server: CowMachine/1.9.0+build.115.ref71f4831

saymyname

```

```
$ curl -i -H http://localhost:1234
HTTP/1.1 406 Not Acceptable
content-length: 0
date: Sun, 15 May 2022 19:41:05 GMT
server: CowMachine/1.9.0+build.115.ref71f4831

```

```
$ $ curl -i -d echo=saymyname http://localhost:1234
HTTP/1.1 405 Method Not Allowed
allow: GET, HEAD
content-length: 0
date: Sun, 15 May 2022 19:42:04 GMT
server: CowMachine/1.9.0+build.115.ref71f4831
```

## Stop web server

Enter `exit.` and press `Enter`.
