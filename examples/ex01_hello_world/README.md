ex01_hello_world
=====

An OTP application

Build
-----

    $ rebar3 escriptize

Run
---

    $ escript _build/default/bin/launcher

## HTTP/1.1 example output

```
$ curl -i http://localhost:1234
HTTP/1.1 200 OK
content-length: 11
content-type: text/html;charset=utf-8
date: Sun, 08 May 2022 20:46:05 GMT
server: CowMachine/1.8.3

Hello World
```

Or enter address string `http://localhost:1234` in your browser and get `Hello World` as a result.

Stop web server
---
```
Enter `exit.` and press `Enter`
```