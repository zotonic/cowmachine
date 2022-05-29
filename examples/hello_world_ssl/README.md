# Secure hello world example

Simplest SSL application.

[Cowboy original example](https://github.com/ninenines/cowboy/tree/master/examples/ssl_hello_world)

An OTP application

## Build

`rebar3 escriptize`

## Run

`escript _build/default/bin/launcher`
	
## Build and run

`rebar3 escriptize && escript _build/default/bin/launcher`	


## Verify that a certificate was issued by a specific CA, given that CA's certificate:

```
$ openssl verify -verbose -CAfile priv/ssl/cowboy-ca.crt  priv/ssl/server.crt
priv/ssl/server.crt: OK

```

## Add trusted root certificates to the server ([Howto](https://manuals.gfi.com/en/kerio/connect/content/server-configuration/ssl-certificates/adding-trusted-root-certificates-to-the-server-1605.html)).


### Mac OS X

#### Add	

`sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain ~/new-root-certificate.crt`

### Remove

`sudo security delete-certificate -c "<name of existing certificate>"`

### Windows

#### Add

`certutil -addstore -f "ROOT" priv\ssl\server.crt`

### Remove

`certutil -delstore "ROOT" priv\ssl\server.crt`

### Linux (Ubuntu, Debian)

#### Add

1. Copy your CA to dir `/usr/local/share/ca-certificates/`
2. Use command: `sudo cp foo.crt /usr/local/share/ca-certificates/server.crt`
3. Update the CA store: `sudo update-ca-certificates`

### Remove

1. Remove your CA.
2. Update the CA store: `sudo update-ca-certificates --fresh`


## How to switch Cowboy/Comachine working flow

Note that you get two working flow (Cowboy and Comachine). By default flow selection is random. 
For instance, to use Cowboy way only please edit `apps/main/main_app.erl` file content. 
Find out `cowboy_options(Dispatch)` function and set `TypeOfCallingCowmachine` value to `1` into it.



## HTTP/1.1 example output

### Using Cowboy router only (without middlewar)

#### Request HTML:

```
$ curl --cacert priv/ssl/cowboy-ca.crt -i https://localhost:8443 --http1.1
HTTP/1.1 200 OK
content-length: 12
content-type: text/plain
date: Sun, 15 May 2022 15:36:00 GMT
server: Cowboy

Hello world!
```

```
$ curl --cacert priv/ssl/cowboy-ca.crt -i https://localhost:8443
HTTP/2 200
content-length: 12
content-type: text/plain
date: Sun, 15 May 2022 16:00:02 GMT
server: Cowboy

Hello world!

```

### Using Cowmachine

#### Request HTML:

```
$ curl --cacert priv/ssl/cowboy-ca.crt -i https://localhost:8443 --http1.1
HTTP/1.1 200 OK
content-length: 12
content-type: text/html;charset=utf-8
date: Sun, 15 May 2022 16:01:38 GMT
server: CowMachine/1.9.0+build.115.ref71f4831

Hello World!
```

```
$ curl --cacert priv/ssl/cowboy-ca.crt -i https://localhost:8443
HTTP/2 200
content-length: 12
content-type: text/html;charset=utf-8
date: Sun, 15 May 2022 16:02:01 GMT
server: CowMachine/1.9.0+build.115.ref71f4831

Hello World!
```

Or enter address string `http://localhost:1234` in your browser and get a result.

## Stop web server

Enter `exit.` and press `Enter`
