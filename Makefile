ERL       ?= erl
ERLC      ?= $(ERL)c
REBAR     := ./rebar3
REBAR_URL := https://s3-eu-west-1.amazonaws.com/zotonic-rebar/rebar3

.PHONY: compile test

all: compile

compile: $(REBAR)
	$(REBAR) compile

test: $(REBAR)
	$(REBAR) eunit

clean:
	$(REBAR) clean

$(REBAR):
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "$(REBAR)"}])' \
	  -s init stop
	chmod +x $(REBAR)
