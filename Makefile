ERL       ?= erl
ERLC      ?= $(ERL)c
REBAR     := ./rebar3
REBAR_URL := https://github.com/erlang/rebar3/releases/download/3.15.1/rebar3

.PHONY: compile test dialyzer xref clean

all: compile

compile: $(REBAR)
	$(REBAR) compile

test: $(REBAR)
	$(REBAR) as test eunit

dialyzer: $(REBAR)
	$(REBAR) dialyzer

xref: $(REBAR)
	$(REBAR) as test xref

clean:
	$(REBAR) clean

$(REBAR):
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "$(REBAR)"}])' \
	  -s init stop
	chmod +x $(REBAR)
