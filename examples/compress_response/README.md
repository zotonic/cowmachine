# Compressed response


[Cowboy original example](https://github.com/ninenines/cowboy/tree/master/examples/compress_response)

An OTP application

## Build

`rebar3 escriptize`

## Run

`escript _build/default/bin/launcher`
	
## Build and run

`rebar3 escriptize && escript _build/default/bin/launcher`	

	
Point your browser to http://localhost:1234

## How to switch Cowboy/Comachine working flow

Note that you get two working flow (Cowboy and Comachine). By default flow selection is random. 
For instance, to use Cowboy way only please edit `apps/main/main_app.erl` file content. 
Find out `cowboy_options(Dispatch)` function and set `TypeOfCallingCowmachine` value to `1` into it.


## HTTP/1.1 example output

### Using Cowboy router only (without middlewar)

#### Without compression:

```bash
$ curl -i http://localhost:1234
HTTP/1.1 200 OK
content-length: 920
date: Sun, 22 May 2022 07:16:07 GMT
server: Cowboy

A cowboy is an animal herder who tends cattle on ranches in North America,
traditionally on horseback, and often performs a multitude of other ranch-
related tasks. The historic American cowboy of the late 19th century arose
from the vaquero traditions of northern Mexico and became a figure of special
significance and legend. A subtype, called a wrangler, specifically tends the
horses used to work cattle. In addition to ranch work, some cowboys work for
or participate in rodeos. Cowgirls, first defined as such in the late 19th
century, had a less-well documented historical role, but in the modern world
have established the ability to work at virtually identical tasks and obtained
considerable respect for their achievements. There are also cattle handlers
in many other parts of the world, particularly South America and Australia,
who perform work similar to the cowboy in their respective nations.
```

#### With compression:

```bash
$ curl -i --compressed http://localhost:1234
HTTP/1.1 200 OK
content-encoding: gzip
content-length: 521
date: Sun, 22 May 2022 07:18:47 GMT
server: Cowboy
vary: accept-encoding

A cowboy is an animal herder who tends cattle on ranches in North America,
traditionally on horseback, and often performs a multitude of other ranch-
related tasks. The historic American cowboy of the late 19th century arose
from the vaquero traditions of northern Mexico and became a figure of special
significance and legend. A subtype, called a wrangler, specifically tends the
horses used to work cattle. In addition to ranch work, some cowboys work for
or participate in rodeos. Cowgirls, first defined as such in the late 19th
century, had a less-well documented historical role, but in the modern world
have established the ability to work at virtually identical tasks and obtained
considerable respect for their achievements. There are also cattle handlers
in many other parts of the world, particularly South America and Australia,
who perform work similar to the cowboy in their respective nations.
```

### Using Cowmachine

#### Without compression:

```bash
$ curl -i http://localhost:1234
HTTP/1.1 200 OK
content-length: 920
content-type: text/plain
date: Sun, 22 May 2022 13:20:07 GMT
server: CowMachine/1.9.0+build.115.ref71f4831
vary: acceptaccept-encoding

A cowboy is an animal herder who tends cattle on ranches in North America,
traditionally on horseback, and often performs a multitude of other ranch-
related tasks. The historic American cowboy of the late 19th century arose
from the vaquero traditions of northern Mexico and became a figure of special
significance and legend. A subtype, called a wrangler, specifically tends the
horses used to work cattle. In addition to ranch work, some cowboys work for
or participate in rodeos. Cowgirls, first defined as such in the late 19th
century, had a less-well documented historical role, but in the modern world
have established the ability to work at virtually identical tasks and obtained
considerable respect for their achievements. There are also cattle handlers
in many other parts of the world, particularly South America and Australia,
who perform work similar to the cowboy in their respective nations.
```

#### With compression:

```bash
$ curl -i --compressed http://localhost:1234
HTTP/1.1 200 OK
content-encoding: gzip
content-length: 521
content-type: text/plain
date: Sun, 22 May 2022 13:18:55 GMT
server: CowMachine/1.9.0+build.115.ref71f4831
vary: acceptaccept-encoding

A cowboy is an animal herder who tends cattle on ranches in North America,
traditionally on horseback, and often performs a multitude of other ranch-
related tasks. The historic American cowboy of the late 19th century arose
from the vaquero traditions of northern Mexico and became a figure of special
significance and legend. A subtype, called a wrangler, specifically tends the
horses used to work cattle. In addition to ranch work, some cowboys work for
or participate in rodeos. Cowgirls, first defined as such in the late 19th
century, had a less-well documented historical role, but in the modern world
have established the ability to work at virtually identical tasks and obtained
considerable respect for their achievements. There are also cattle handlers
in many other parts of the world, particularly South America and Australia,
who perform work similar to the cowboy in their respective nations.
```


## Stop web server

Type in `exit.` command and press `Enter`.
