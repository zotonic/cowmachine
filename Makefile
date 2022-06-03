ERL       ?= erl
ERLC      ?= $(ERL)c
REBAR     := ./rebar3
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3

.PHONY: compile test dialyzer xref clean

all: compile

compile: $(REBAR)
	$(REBAR) compile

test: $(REBAR)
	$(REBAR) as test eunit
	$(REBAR) as test proper

dialyzer: $(REBAR)
	$(REBAR) as check dialyzer

xref: $(REBAR)
	$(REBAR) as test xref

clean: $(REBAR) clean_doc
	$(REBAR) clean

clean_doc:
	@rm -f doc/*.html
	@rm -f doc/erlang.png
	@rm -f doc/edoc-info

##
## Doc targets
##
edoc: $(REBAR)
	$(REBAR) edoc

edoc_private: $(REBAR)	
	$(REBAR) as edoc_private edoc

exdoc: $(REBAR)	
	$(REBAR) ex_doc --logo doc/img/logo.png --output edoc

$(REBAR):
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "$(REBAR)"}])' \
	  -s init stop
	chmod +x $(REBAR)
