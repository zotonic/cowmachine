# POST parameter echo example


[Cowboy original example](https://github.com/ninenines/cowboy/tree/master/examples/echo_post)

An OTP application

## Build

`rebar3 escriptize`

## Run

`escript _build/default/bin/launcher`
	
## Build and run

`rebar3 escriptize && escript _build/default/bin/launcher`

## How to switch Cowboy/Comachine working flow

Note that you get two working flow (Cowboy and Comachine). By default flow selection is random. 
For instance, to use Cowboy way only please edit `apps/main/main_app.erl` file content. 
Find out `cowboy_options(Dispatch)` function and set `TypeOfCallingCowmachine` value to `1` into it.


## HTTP/1.1 example output

### Using Cowboy router only (without middlewar)

POST with data:
```
$ curl -i -d echo=echomeplz http://localhost:1234
HTTP/1.1 200 OK
content-length: 9
content-type: text/plain; charset=utf-8
date: Mon, 16 May 2022 09:14:31 GMT
server: Cowboy

echomeplz

```

POST with missing echo parameter:
```
$ curl -i -d echo1=1 http://localhost:1234
HTTP/1.1 400 Bad Request
content-length: 23
date: Mon, 16 May 2022 11:10:49 GMT
server: Cowboy

Missing echo parameter.

```

POST with missing without body:
```
$ curl -X POST http://localhost:1234
Missing body.
```

```
$ cat /dev/null | curl --data @- http://localhost:1234
Missing body!
```

Send GET method:
```
$ curl -i http://localhost:1234
HTTP/1.1 405 Method Not Allowed
content-length: 0
date: Mon, 16 May 2022 11:27:25 GMT
server: Cowboy
```

### Using Cowmachine

POST with data:
```
$ curl -i -d echo=echomeplz http://localhost:1234
HTTP/1.1 200 OK
content-length: 9
content-type: text/html;charset=utf-8
date: Mon, 16 May 2022 16:39:47 GMT
server: CowMachine/1.9.0+build.115.ref71f4831

echomeplz

```

POST with missing echo parameter:
```
$ curl -i -d echo1=1 http://localhost:1234
HTTP/1.1 400 Bad Request
content-length: 23
content-type: text/plain;charset=utf-8
date: Mon, 16 May 2022 18:47:56 GMT
server: CowMachine/1.9.0+build.115.ref71f4831

Missing echo parameter.

```

POST with missing without body:
```
$ curl -X POST http://localhost:1234
Missing body.
```

```
$ cat /dev/null | curl --data @- http://localhost:1234
Missing body!
```

Send GET method:
```
$ curl -i http://localhost:1234
HTTP/1.1 405 Method Not Allowed
content-length: 0
content-type: text/plain;charset=utf-8
date: Mon, 16 May 2022 18:55:47 GMT
server: CowMachine/1.9.0+build.115.ref71f4831

```

## Stop web server

Enter `exit.` and press `Enter`.
