# REST hello world example

Return the data type that matches the request type (ex: html, text, json).

[Cowboy original example](https://github.com/ninenines/cowboy/tree/master/examples/rest_hello_world)

An OTP application

## Build

`$ rebar3 escriptize`

## Run

`$ escript _build/default/bin/launcher`
	
## Build and run

`$ rebar3 escriptize && escript _build/default/bin/launcher`	

## How to switch Cowboy/Comachine working flow

Note that you get two working flow (Cowboy and Comachine). By default flow selection is random. 
For instance, to use Cowboy way only please edit `apps/main/main_app.erl` file content. 
Find out `cowboy_options(Dispatch)` function and set `TypeOfCallingCowmachine` value to `1` into it.


## HTTP/1.1 example output

### Using Cowboy router only (without middlewar)

#### Request HTML:

```
$ curl -i http://localhost:1234
HTTP/1.1 200 OK
content-length: 144
content-type: text/html
date: Fri, 13 May 2022 18:15:14 GMT
server: Cowboy
vary: accept

<html>
<head>
        <meta charset="utf-8">
        <title>REST Hello World!</title>
</head>
<body>
        <p>REST Hello World as HTML!</p>
</body>
</html>

```

#### Request JSON:

```
$ curl -i -H "Accept: application/json" http://localhost:1234
HTTP/1.1 200 OK
content-length: 24
content-type: application/json
date: Fri, 13 May 2022 18:16:36 GMT
server: Cowboy
vary: accept

{"rest": "Hello World!"}
```

#### Request plain text:

```
$ curl -i -H "Accept: text/plain" http://localhost:1234
HTTP/1.1 200 OK
content-length: 25
content-type: text/plain
date: Fri, 13 May 2022 18:17:51 GMT
server: Cowboy
vary: accept

REST Hello World as text!
```

#### Request a non acceptable content-type:

```
$ curl -i -H "Accept: text/css" http://localhost:1234
HTTP/1.1 406 Not Acceptable
content-length: 0
date: Fri, 13 May 2022 18:18:47 GMT
server: Cowboy
```

### Using Cowmachine

#### Request HTML:

```
$ curl -i http://localhost:1234
$ curl -i -H "Accept: text/html" http://localhost:1234
HTTP/1.1 200 OK
content-length: 144
content-type: text/html;charset=utf-8
date: Sat, 14 May 2022 14:13:19 GMT
server: CowMachine/1.9.0+build.115.ref71f4831
vary: accept

<html>
        <head>
                <meta charset="utf-8">
                <title>REST Hello World!</title>
        </head>
        <body>
                <p>REST Hello World as HTML!</p>
        </body>
</html>
```

#### Request JSON:

```
$ curl -i -H "Accept: application/json" http://localhost:1234
HTTP/1.1 200 OK
content-length: 24
content-type: application/json
date: Sat, 14 May 2022 14:15:33 GMT
server: CowMachine/1.9.0+build.115.ref71f4831
vary: accept

{"rest": "Hello World!"}
```

#### Request plain text:

```
$ curl -i -H "Accept: text/plain" http://localhost:1234
HTTP/1.1 200 OK
content-length: 25
content-type: text/plain;charset=utf-8
date: Sat, 14 May 2022 14:16:30 GMT
server: CowMachine/1.9.0+build.115.ref71f4831
vary: accept

REST Hello World as text!
```

#### Request a non acceptable content-type:

```
$ curl -i -H "Accept: text/css" http://localhost:1234
HTTP/1.1 406 Not Acceptable
content-length: 0
date: Sat, 14 May 2022 14:17:10 GMT
server: CowMachine/1.9.0+build.115.ref71f4831


```

Or enter address string `http://localhost:1234` in your browser and get a result.

## Stop web server

Type in `exit.` command and press `Enter`.
