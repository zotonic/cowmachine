%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2009 Basho Technologies
%% Based on mochiweb_request.erl, which is Copyright 2007 Mochi Media, Inc.
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

%% @doc Response functions, generate the response inclusive body and headers.
%% The body can be sourced from multiple sources. These sources include files,
%% binary files and functions.
%% The response body function handles range requests.

-module(cowmachine_response).
-author('Marc Worrell <marc@worrell.nl>').

-export([
     send_response/1,
     server_header/0
     ]).

% Debugging
-export([
    send_stream_body/2
]).

-define(IDLE_TIMEOUT, infinity).
-define(FILE_CHUNK_LENGTH, 65536).

server_header() ->
    case application:get_env(cowmachine, server_header) of
        {ok, Server} -> z_convert:to_binary(Server);
        undefined ->
            case application:get_key(cowmachine, vsn) of
                {ok, Version} -> <<"CowMachine/", (z_convert:to_binary(Version))/binary>>;
                undefined -> <<"CowMachine">>
            end
    end.

-spec send_response(cowmachine_req:context()) -> {ok, Req, Env} | {stop, Req}
    when Req :: cowboy_req:req(),
         Env :: cowboy_middleware:env().
send_response(Context) ->
    ControllerOptions = cowmachine_req:controller_options(Context),
    HttpStatusCode = case proplists:get_value(http_status_code, ControllerOptions) of
                         Code when is_integer(Code) -> Code;
                         _ -> cowmachine_req:response_code(Context)
                     end,
    Context1 = send_response_range(HttpStatusCode, Context),
    {ok, cowmachine_req:req(Context1), cowmachine_req:env(Context1)}.

%% --------------------------------------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------------------------------------

send_response_range(200, Context) ->
    {Range, Context1} = get_range(Context),
    case Range of
        undefined ->
            send_response_code(200, all, Context1);
        Ranges ->
            case get_resp_body_size(cowmachine_req:resp_body(Context1)) of
                {ok, Size, Body1} ->
                    Context2 = cowmachine_req:set_resp_body(Body1, Context1),
                    case range_parts(Ranges, Size) of
                        [] ->
                            % no valid ranges
                            % could be 416, for now we'll just return 200 and the whole body
                            send_response_code(200, all, Context2);
                        PartList ->
                            % error_logger:info_msg("PARTS ~p (~p)", [PartList, wrq:path(ReqData)]),
                            ContentType = cowmachine_req:get_resp_header(<<"content-type">>, Context2),
                            {RangeHeaders, Boundary} = make_range_headers(PartList, Size, ContentType),
                            Context3 = cowmachine_req:set_resp_headers(RangeHeaders, Context2),
                            send_response_code(206, {PartList, Size, Boundary, ContentType}, Context3)
                    end;
                {error, nosize} ->
                    send_response_code(200, all, Context1)
            end
    end;
send_response_range(Code, Context) ->
    send_response_code(Code, all, Context).

send_response_code(Code, Parts, Context) ->
    send_response_bodyfun(cowmachine_req:resp_body(Context), Code, Parts, Context).


% send_response_bodyfun(undefined, Code, Parts, Context) ->
%     send_response_bodyfun(<<>>, Code, Parts, Context);
send_response_bodyfun({device, IO}, Code, Parts, Context) ->
    Length = iodevice_size(IO),
    send_response_bodyfun({device, Length, IO}, Code, Parts, Context);
send_response_bodyfun({device, Length, IO}, Code, all, Context) ->
    Writer = fun(FunContext) ->
                send_device_body(FunContext, Length, IO),
                _ = file:close(IO),
                FunContext
             end,
    start_response_stream(Code, Length, Writer, Context);
send_response_bodyfun({device, _Length, IO}, Code, Parts, Context) ->
    Writer = fun(FunContext) ->
                FunContext1 = send_device_body_parts(FunContext, Parts, IO),
                _ = file:close(IO),
                FunContext1
             end,
    start_response_stream(Code, undefined, Writer, Context);
send_response_bodyfun({file, Filename}, Code, Parts, Context) ->
    Length = filelib:file_size(Filename),
    send_response_bodyfun({file, Length, Filename}, Code, Parts, Context);
send_response_bodyfun({file, Length, Filename}, Code, all, Context) ->
    Writer = fun(FunContext) -> send_file_body(FunContext, Length, Filename, fin) end,
    start_response_stream(Code, Length, Writer, Context);
send_response_bodyfun({file, _Length, Filename}, Code, Parts, Context) ->
    Writer = fun(FunContext) -> send_file_body_parts(FunContext, Parts, Filename) end,
    start_response_stream(Code, undefined, Writer, Context);
send_response_bodyfun({stream, Fun}, Code, all, Context) ->
    Writer = fun(FunContext) -> send_stream_body(FunContext, Fun(0, all)) end,
    start_response_stream(Code, undefined, Writer, Context);
send_response_bodyfun({stream, Size, Fun}, Code, all, Context) ->
    Writer = fun(FunContext) -> send_stream_body(FunContext, Fun(0, Size-1)) end,
    start_response_stream(Code, undefined, Writer, Context);
send_response_bodyfun({writer, WriterFun}, Code, all, Context) ->
    Writer = fun(FunContext) -> send_writer_body(FunContext, WriterFun) end,
    start_response_stream(Code, undefined, Writer, Context);
send_response_bodyfun(Body, Code, all, Context) ->
    Length = iolist_size(Body),
    start_response_stream(Code, Length, Body, Context);
send_response_bodyfun(undefined, Code, _Parts, Context) ->
    start_response_stream(Code, 0, <<>>, Context);
send_response_bodyfun(Body, Code, Parts, Context) ->
    Writer = fun(FunContext) -> send_parts(FunContext, Body, Parts) end,
    start_response_stream(Code, undefined, Writer, Context).


start_response_stream(Code, Length, Fun, Context) when is_function(Fun) ->
    Headers = response_headers(Code, Length, Context),
    Req = cowmachine_req:req(Context),
    Req1 = cowboy_req:stream_reply(Code, Headers, Req),
    Fun( cowmachine_req:set_req(Req1, Context) );
start_response_stream(Code, Length, Body, Context) when is_list(Body); is_binary(Body) ->
    Headers = response_headers(Code, Length, Context),
    Req = cowmachine_req:req(Context),
    Req1 = cowboy_req:reply(Code, Headers, Body, Req),
    cowmachine_req:set_req(Req1, Context).

%% @todo Add the cookies!
response_headers(Code, Length, Context) ->
    Hdrs = cowmachine_req:get_resp_headers(Context),
    Hdrs1 = case Code of
        304 ->
            Hdrs;
        _ when is_integer(Length) ->
            Hdrs#{<<"content-length">> => integer_to_binary(Length)};
        _ ->
            Hdrs
    end,
    Hdrs1#{
        <<"server">> => server_header(),
        <<"date">> => cowboy_clock:rfc1123()
    }.

send_stream_body(Context, {<<>>, done}) ->
    % @TODO: in cowboy this give a wrong termination with two 0 size chunks
    send_chunk(Context, <<>>, fin);
send_stream_body(Context, {{file, Filename}, Next}) ->
    Length = filelib:file_size(Filename),
    send_stream_body(Context, {{file, Length, Filename}, Next});
send_stream_body(Context, {{file, 0, _Filename}, Next}) ->
    send_stream_body(Context, {<<>>, Next});
send_stream_body(Context, {{file, Size, Filename}, Next}) ->
    Context1 = send_file_body(Context, Size, Filename, nofin),
    send_stream_body(Context1, {<<>>, Next});
send_stream_body(Context, {Data, done}) ->
    send_chunk(Context, Data, fin);
send_stream_body(Context, {<<>>, Next}) ->
    send_stream_body(Context, Next());
send_stream_body(Context, {[], Next}) ->
    send_stream_body(Context, Next());
send_stream_body(Context, {Data, Next}) ->
    Context1 = send_chunk(Context, Data, nofin),
    send_stream_body(Context1, Next());
send_stream_body(Context, Fun) when is_function(Fun, 0) ->
    send_stream_body(Context, Fun()).


send_device_body(Context, Length, IO) ->
    send_file_body_loop(Context, 0, Length, IO, fin).

send_file_body(Context, Length, Filename, FinNoFin) ->
    {ok, FD} = file:open(Filename, [raw,binary]),
    try
        send_file_body_loop(Context, 0, Length, FD, FinNoFin)
    after
        file:close(FD)
    end.

send_device_body_parts(Context, {[{From,Length}], _Size, _Boundary, _ContentType}, IO) ->
    {ok, _} = file:position(IO, From), 
    send_file_body_loop(Context, 0, Length, IO, fin);
send_device_body_parts(Context, {Parts, Size, Boundary, ContentType}, IO) ->
    lists:foreach(
        fun({From,Length}) ->
            {ok, _} = file:position(IO, From), 
            send_chunk(Context, part_preamble(Boundary, ContentType, From, Length, Size), nofin),
            send_file_body_loop(Context, 0, Length, IO, nofin),
            send_chunk(Context, <<"\r\n">>, nofin)
        end,
        Parts),
    send_chunk(Context, end_boundary(Boundary), fin).

send_file_body_parts(Context, Parts, Filename) ->
    {ok, FD} = file:open(Filename, [raw,binary]),
    try
        send_device_body_parts(Context, Parts, FD)
    after
        file:close(FD)
    end.

send_parts(Context, Bin, {[{From,Length}], _Size, _Boundary, _ContentType}) ->
    send_chunk(Context, binary:part(Bin,From,Length), nofin);
send_parts(Context, Bin, {Parts, Size, Boundary, ContentType}) ->
    lists:foreach(
        fun({From,Length}) ->
            Part = [
                part_preamble(Boundary, ContentType, From, Length, Size),
                Bin,
                <<"\r\n">>
            ],
            send_chunk(Context, Part, nofin)
        end,
        Parts),
    send_chunk(Context, end_boundary(Boundary), fin).


send_file_body_loop(Context, Offset, Size, _Device, _FinNoFin) when Offset =:= Size ->
    Context;
send_file_body_loop(Context, Offset, Size, Device, FinNoFin) when Size - Offset =< ?FILE_CHUNK_LENGTH ->
    {ok, Data} = file:read(Device, Size - Offset),
    send_chunk(Context, Data, FinNoFin);
send_file_body_loop(Context, Offset, Size, Device, FinNoFin) ->
    {ok, Data} = file:read(Device, ?FILE_CHUNK_LENGTH),
    send_chunk(Context, Data, nofin),
    send_file_body_loop(Context, Offset+?FILE_CHUNK_LENGTH, Size, Device, FinNoFin).

send_writer_body(Context, BodyFun) ->
    BodyFun(fun(Data, false, ContextW) ->
                    send_chunk(ContextW, Data, nofin);
               (Data, true, ContextW) ->
                    send_chunk(ContextW, Data, fin)
            end,
            Context).

send_chunk(Context, <<>>, nofin) ->
    Context;
send_chunk(Context, [], nofin) ->
    Context;
send_chunk(Context, Data, FinNoFin) ->
    Data1 = iolist_to_binary(Data),
    Req = cowmachine_req:req(Context),
    ok = cowboy_req:stream_body(Data1, FinNoFin, Req),
    Context.

-spec get_range(cowmachine_req:context()) -> {Range, cowmachine_req:context()}
    when Range :: undefined | [ {integer()|none, integer()|none} ].
get_range(Context) ->
    Env = cowmachine_req:env(Context),
    Range = case maps:get(cowmachine_range_ok, Env) of
        false ->
            undefined;
        true ->
            Req = cowmachine_req:req(Context),
            case cowboy_req:header(<<"range">>, Req) of
                undefined ->
                    undefined;
                RawRange ->
                    parse_range_request(RawRange)
            end
    end,
    Env1 = Env#{ cowmachine_range => Range },
    {Range, cowmachine_req:set_env(Env1, Context)}.

-spec parse_range_request(binary()|undefined) -> undefined | [{integer()|none,integer()|none}].
parse_range_request(<<"bytes=", RangeString/binary>>) ->
    try
        Ranges = binary:split(RangeString, <<",">>, [global]),
        lists:map(
            fun (<<"-", V/binary>>)  ->
                   {none, binary_to_integer(V)};
                (R) ->
                    case binary:split(R, <<"-">>) of
                        [S1, S2] -> {binary_to_integer(S1), binary_to_integer(S2)};
                        [S] -> {binary_to_integer(S), none}
                    end
          end,
          Ranges)
    catch
        _:_ ->
            % Invalid range header, ignore silently
            undefined
    end;
parse_range_request(_) ->
    undefined.

% Map request ranges to byte ranges, taking the total body length into account.
-spec range_parts([{integer()|none,integer()|none}], integer()) -> [{integer(),integer()}].
range_parts(Ranges, Size) ->
    Ranges1 = [ range_skip_length(Spec, Size) || Spec <- Ranges ],
    [ R || R <- Ranges1, R =/= invalid_range ].

range_skip_length({none, R}, Size) when R =< Size, R >= 0 ->
    {Size - R, R};
range_skip_length({none, _OutOfRange}, Size) ->
    {0, Size};
range_skip_length({R, none}, Size) when R >= 0, R < Size ->
    {R, Size - R};
range_skip_length({_OutOfRange, none}, _Size) ->
    invalid_range;
range_skip_length({Start, End}, Size) when 0 =< Start, Start =< End, End < Size ->
    {Start, End - Start + 1};
range_skip_length({_OutOfRange, _End}, _Size) ->
    invalid_range.

-spec get_resp_body_size(cowmachine_req:resp_body()) -> 
          {ok, integer(), cowmachine_req:resp_body()}
        | {error, nosize}.
get_resp_body_size({device, Size, _} = Body) ->
    {ok, Size, Body};
get_resp_body_size({device, IO}) ->
    Length = iodevice_size(IO),
    {ok, Length, {device, Length, IO}};
get_resp_body_size({file, Size, _} = Body) ->
    {ok, Size, Body};
get_resp_body_size({file, Filename}) ->
    Length = filelib:file_size(Filename),
    {ok, Length, {file, Length, Filename}};
get_resp_body_size(B) when is_binary(B) ->
    {ok, size(B), B};
get_resp_body_size(L) when is_list(L) ->
    B = iolist_to_binary(L),
    {ok, size(B), B};
get_resp_body_size(_) ->
    {error, nosize}.

make_range_headers([{Start, Length}], Size, _ContentType) ->
    HeaderList = [{<<"accept-ranges">>, <<"bytes">>},
                  {<<"content-range">>, iolist_to_binary([
                            "bytes ", make_io(Start), "-", make_io(Start+Length-1), "/", make_io(Size)])},
                  {<<"content-length">>, integer_to_binary(Length)}],
    {HeaderList, none};
make_range_headers(Parts, Size, ContentType) when is_list(Parts) ->
    Boundary = boundary(),
    Lengths = [
        iolist_size(part_preamble(Boundary, ContentType, Start, Length, Size)) + Length + 2
        || {Start,Length} <- Parts
    ],
    TotalLength = lists:sum(Lengths) + iolist_size(end_boundary(Boundary)), 
    HeaderList = [{<<"accept-ranges">>, <<"bytes">>},
                  {<<"content-type">>, <<"multipart/byteranges; boundary=", Boundary/binary>>},
                  {<<"content-length">>, integer_to_binary(TotalLength)}],
    {HeaderList, Boundary}.

part_preamble(Boundary, CType, Start, Length, Size) ->
    [boundary(Boundary),
     <<"content-type: ">>, CType,
     <<"\r\ncontent-range: bytes ">>, 
        integer_to_binary(Start), <<"-">>, integer_to_binary(Start+Length-1), 
        <<"/">>, integer_to_binary(Size),
     <<"\r\n\r\n">>].

boundary() ->
    A = rand:uniform(100000000),
    B = rand:uniform(100000000),
    <<(integer_to_binary(A))/binary, $_, (integer_to_binary(B))/binary>>.

boundary(B)     -> <<"--", B/binary, "\r\n">>.
end_boundary(B) -> <<"--", B/binary, "--\r\n">>.

make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer).
% make_io(Atom) when is_atom(Atom) ->
%     atom_to_list(Atom);
% make_io(Io) when is_list(Io); is_binary(Io) ->
%     Io.


-spec iodevice_size(file:io_device()) -> integer().
iodevice_size(IoDevice) ->
    {ok, Size} = file:position(IoDevice, eof),
    {ok, 0} = file:position(IoDevice, bof),
    Size.

