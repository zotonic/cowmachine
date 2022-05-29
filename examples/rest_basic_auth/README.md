# Basic authorization example using REST

[Cowboy original example](https://github.com/ninenines/cowboy/tree/master/examples/rest_basic_auth)

An OTP application

## Build

`rebar3 escriptize`

## Run

`escript _build/default/bin/launcher`
	
## Build and run

`rebar3 escriptize && escript _build/default/bin/launcher`	


## HTTP/1.1 example output

### Using Cowboy router only (without middlewar)

#### Request with no authentication:

```
$ curl -i http://localhost:1234
HTTP/1.1 401 Unauthorized
content-length: 0
date: Sat, 28 May 2022 09:25:29 GMT
server: Cowboy
www-authenticate: Basic realm="cowboy"
```

#### Request with authentication:

```
$ curl -i -u "Alladin:open sesame" http://localhost:1234
HTTP/1.1 200 OK
content-length: 16
content-type: text/plain
date: Sat, 28 May 2022 09:33:46 GMT
server: Cowboy

Hello, Alladin!
```
### Using Cowmachine

#### Request with no authentication:

```
$  curl -i -u "Alladin:open sesame" http://localhost:1234
HTTP/1.1 401 Unauthorized
content-length: 0
content-type: text/html;charset=utf-8
date: Sat, 28 May 2022 18:30:06 GMT
server: CowMachine/1.9.0+build.115.ref71f4831
```

#### Request with authentication:

```
$ curl -i -u "Alladin:open sesame" http://localhost:1234
HTTP/1.1 200 OK
content-length: 16
content-type: text/plain;charset=utf-8
date: Sat, 28 May 2022 19:17:19 GMT
server: CowMachine/1.9.0+build.115.ref71f4831
user: Alladin

Hello, Alladin!

```

Or enter address string `Alladin:open sesame@localhost:1234` in your browser.

## Using browser

Point your browser to http://localhost:1234. You will see entering `Username` and `Password` window.
Try to enter something. For instance `Username` `test` and `Password` `test`. You will that window again. Now try to enter `Username` `Alladin` and `Password` `open sesame` and you will be pointed to the authorised page.


### How to convert/rewerse base64 texts

Basic authentication use Base64 encoding/decoding while you user such kind of authorisation.

This is example how it encoding/decoding text data while basic authorisation process:
```
echo -n "Alladin:open sesame" | base64
QWxsYWRpbjpvcGVuIHNlc2FtZQ==

echo -n "QWxsYWRpbjpvcGVuIHNlc2FtZQ==" | base64 --decode
Alladin:open sesame
```

## Stop web server

Type in `exit.` command and press `Enter`.
