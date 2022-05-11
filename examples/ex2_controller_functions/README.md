ex2_chunked_hello_world
=====

# Controller functions implementaion

Demonstrate controller with default controller functions.


An OTP application

Build
-----

    $ rebar3 escriptize

Run
---

    $ escript _build/default/bin/launcher
	
Build and run
---

    $ rebar3 escriptize && escript _build/default/bin/launcher
	

## HTTP/1.1 example output

```
$ curl -i http://localhost:1234
HTTP/1.1 200 OK
content-length: 11
content-type: text/html;charset=utf-8
date: Wed, 11 May 2022 18:03:53 GMT
server: CowMachine/1.8.3

Hello World
```

Or enter address string `http://localhost:1234` in your browser and get `Hello World` as a result.

Stop web server
---
```
Enter `exit.` and press `Enter`
```