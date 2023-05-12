%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2009 Basho Technologies
%% @end
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(cowmachine_controller).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-author('Marc Worrell <marc@worrell.nl>').

-export([
    do/3,
    do_process/3
]).

-include("cowmachine_state.hrl").

-export_type([cmstate/0]).

%% @doc Get default value by Key.
-spec default(DefaultID, Context) -> Result when
	DefaultID:: service_available |	resource_exists |	auth_required |	is_authorized |	forbidden |	upgrades_provided |	allow_missing_post |	malformed_request |	uri_too_long |	known_content_type |	valid_content_headers |	valid_entity_length |	options |	allowed_methods |	known_methods |	validate_content_checksum |	content_types_provided |	content_types_accepted |	delete_resource |	delete_completed |	post_is_create |	create_path |	base_uri |	process_post |	language_available |	charsets_provided |	content_encodings_provided |	transfer_encodings_provided |	variances |	is_conflict |	multiple_choices |	previously_existed |	moved_permanently |	moved_temporarily |	last_modified |	expires |	generate_etag |	finish_request,
	Context :: cowmachine_req:context(),
	Result :: no_charset | no_default | undefined | boolean() | list(binary()).
default(service_available, _Context) ->
    true;
default(resource_exists, _Context) ->
    true;
default(auth_required, _Context) ->
    true;
default(is_authorized, _Context) ->
    true;
default(forbidden, _Context) ->
    false;
default(upgrades_provided, _Context) ->
    [];
default(allow_missing_post, _Context) ->
    false;
default(malformed_request, _Context) ->
    false;
default(uri_too_long, _Context) ->
    false;
default(known_content_type, _Context) ->
    true;
default(valid_content_headers, _Context) ->
    true;
default(valid_entity_length, _Context) ->
    true;
default(options, _Context) ->
    [];
default(allowed_methods, _Context) ->
    [ <<"GET">>, <<"HEAD">> ];
default(known_methods, _Context) ->
    [ <<"GET">>, <<"HEAD">>,
      <<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>,
      <<"TRACE">>, <<"CONNECT">>, <<"OPTIONS">> ];
default(validate_content_checksum, _Context) ->
    not_validated;
default(content_types_provided, _Context) ->
    [ {<<"text">>, <<"html">>, []} ];
default(content_types_accepted, _Context) ->
    [];
default(delete_resource, _Context) ->
    false;
default(delete_completed, _Context) ->
    true;
default(post_is_create, _Context) ->
    false;
default(create_path, _Context) ->
    undefined;
default(base_uri, _Context) ->
    undefined;
default(process_post, _Context) ->
    false;
default(language_available, _Context) ->
    true;

% The default setting is needed for non-charset responses such as image/png
% An example of how one might do actual negotiation:
%    [ <<"iso-8859-1">>, <<"utf-8">> ];
default(charsets_provided, Context) ->
    case is_text( cowmachine_req:resp_content_type(Context) ) of
        true -> [ <<"utf-8">> ];
        false -> no_charset
    end;

% The content variations available to the controller.
default(content_encodings_provided, _Context) ->
    [<<"identity">>];

% How the content is transferred, this is handy for auto-gzip of GET-only resources.
% "identity" and "chunked" are always available to HTTP/1.1 clients.
% Example:
%    [{"gzip", fun(X) -> zlib:gzip(X) end}];
default(transfer_encodings_provided, _Context) ->
    [];

default(variances, _Context) ->
    [];
default(is_conflict, _Context) ->
    false;
default(multiple_choices, _Context) ->
    false;
default(previously_existed, _Context) ->
    false;
default(moved_permanently, _Context) ->
    false;
default(moved_temporarily, _Context) ->
    false;
default(last_modified, _Context) ->
    undefined;
default(expires, _Context) ->
    undefined;
default(generate_etag, _Context) ->
    undefined;
default(finish_request, _Context) ->
    true;
default(_, _Context) ->
    no_default.


%% @doc Content types that are textual and should have a charset defined.

-spec is_text(ContentType) -> Result when
	ContentType :: cow_http_hd:media_type(),
	Result :: boolean().
is_text({<<"text">>, _, _}) -> true;
is_text({<<"application">>, <<"json">>, _}) -> true;
is_text({<<"application">>, <<"ld+json">>, _}) -> true;
is_text({<<"application">>, <<"x-javascript">>, _}) -> true;
is_text({<<"application">>, <<"javascript">>, _}) -> true;
is_text({<<"application">>, <<"xhtml">>, _}) -> true;
is_text({<<"application">>, <<"xhtml+xml">>, _}) -> true;
is_text({<<"application">>, <<"xml">>, _}) -> true;
is_text({<<"application">>, Sub, _}) ->
    case binary:split(Sub, <<"+">>) of
        [_, <<"json">>] -> true;
        [_, <<"xml">>] -> true;
        _ -> false
    end;
is_text(_) ->
    false.


%% @doc Export and run function `Fun'.

-spec do(Fun, State, Context) -> Result when
	Fun :: atom(),
	State :: cmstate(),
	Context :: cowmachine_req:context(),
	Result :: {ContentType, Context},
	ContentType :: cow_http_hd:media_type().
do(Fun, #cmstate{ controller = Controller }, Context) when is_atom(Fun) ->
    case erlang:function_exported(Controller, Fun, 1) of
        true ->
            Controller:Fun(Context);
        false ->
            case default(Fun, Context) of
                no_default -> Controller:Fun(Context);
                Default -> {Default, Context}
            end
    end.

%% @doc Export and process `State' with `Context'.

-spec do_process(ContentType, State, Context) -> Result when
	ContentType :: cow_http_hd:media_type(),
	State :: cmstate(),
	Context :: cowmachine_req:context(),
	Result :: {Res, Context},
	Res :: boolean() | cowmachine_req:halt() | {error, any(), any()} | {error, any()} |
			cowmachine_req:resp_body().
do_process(ContentType, #cmstate{ controller = Controller }, Context) ->
    case erlang:function_exported(Controller, process, 4) of
        true ->
            Controller:process(
                cowmachine_req:method(Context),
                ContentType,
                cowmachine_req:resp_content_type(Context),
                Context);
        false ->
            {true, Context}
    end.
