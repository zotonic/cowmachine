%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2009 Basho Technologies, 2018-2022 Marc Worrell
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

-define(FILE_CHUNK_LENGTH, 16#80000). % 512KB

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
    start_response_stream(Code, Length, Writer, all, Context);
send_response_bodyfun({device, _Length, IO}, Code, Parts, Context) ->
    Writer = fun(FunContext, WParts) ->
                FunContext1 = send_device_body_parts(FunContext, WParts, IO),
                _ = file:close(IO),
                FunContext1
             end,
    start_response_stream(Code, undefined, Writer, Parts, Context);
% File
send_response_bodyfun({file, Filename}, Code, Parts, Context) ->
    Length = filelib:file_size(Filename),
    send_response_bodyfun({file, Length, Filename}, Code, Parts, Context);
send_response_bodyfun({file, Length, Filename}, Code, all, Context) ->
    Writer = fun(FunContext) ->
        send_file_body(FunContext, Length, Filename, fin)
    end,
    start_response_stream(Code, Length, Writer, all, Context);
send_response_bodyfun({file, _Length, Filename}, Code, Parts, Context) ->
    Writer = fun(FunContext, WParts) ->
        send_file_body_parts(FunContext, WParts, Filename)
    end,
    start_response_stream(Code, undefined, Writer, Parts, Context);
% Stream functions with continuation
send_response_bodyfun({stream, {_, _} = InitialStream}, Code, Parts, Context) ->
    start_response_stream(Code, undefined, InitialStream, Parts, Context);
send_response_bodyfun({stream, Size, {_, _} = InitialStream}, Code, Parts, Context) ->
    start_response_stream(Code, Size, InitialStream, Parts, Context);
send_response_bodyfun({stream, StreamFun}, Code, Parts, Context) when is_function(StreamFun) ->
    start_response_stream(Code, undefined, {<<>>, StreamFun}, Parts, Context);
send_response_bodyfun({stream, Size, StreamFun}, Code, Parts, Context) when is_function(StreamFun) ->
    start_response_stream(Code, Size, {<<>>, StreamFun}, Parts, Context);
% send_response_bodyfun({stream, {_Data, Fun} = Initial, Code, Parts, Context) when is_function(Fun, 1) ->
%     Writer = fun(FunContext) -> send_stream_body(FunContext, Fun(Parts)) end,
%     start_response_stream(Code, undefined, Writer, Context);
% % Stream without parts - remove parts response header
% send_response_bodyfun({stream, Fun}, Code, Parts, Context) when is_function(Fun, 0) ->
%     Writer = fun(FunContext) -> send_stream_body(FunContext, Fun()) end,
%     start_response_stream(Code, undefined, Writer, Context);
% send_response_bodyfun({stream, Size, Fun}, Code, all, Context) when is_function(Fun, 0) ->
%     Writer = fun(FunContext) -> send_stream_body(FunContext, Fun()) end,
%     start_response_stream(Code, undefined, Writer, Context);
% % Stream with parts (managed by the streaming function)
% send_response_bodyfun({stream, Fun}, Code, Parts, Context) when is_function(Fun, 1) ->
%     Writer = fun(FunContext) -> send_stream_body(FunContext, Fun(Parts)) end,
%     start_response_stream(Code, undefined, Writer, Context);
% send_response_bodyfun({stream, Size, Fun}, Code, Parts, Context) when is_function(Fun, 1) ->
%     Writer = fun(FunContext) -> send_stream_body(FunContext, Fun(Parts) end,
%     start_response_stream(Code, undefined, Writer, Context);
% Writer
send_response_bodyfun({writer, WriterFun}, Code, all, Context) ->
    Writer = fun(FunContext) ->
        send_writer_body(FunContext, WriterFun)
    end,
    start_response_stream(Code, undefined, Writer, all, Context);
% Data
send_response_bodyfun(undefined, Code, Parts, Context) ->
    send_response_bodyfun(<<>>, Code, Parts, Context);
send_response_bodyfun(Body, Code, all, Context) ->
    Length = iolist_size(Body),
    Headers = response_headers(Code, Length, Context),
    Req = cowmachine_req:req(Context),
    Req1 = cowboy_req:reply(Code, Headers, Body, Req),
    cowmachine_req:set_req(Req1, Context);
send_response_bodyfun(Body, Code, Parts, Context) ->
    Headers = response_headers(Context),
    Req = cowmachine_req:req(Context),
    Req1 = cowboy_req:stream_reply(Code, Headers, Req),
    Context1 = cowmachine_req:set_req(Req1, Context),
    send_parts(Context1, Parts, iolist_to_binary(Body)).


start_response_stream(Code, Length, InitialStream, Parts, Context) ->
    {Code1, Context1, Parts1} = case is_streaming_range(InitialStream) of
        false when Parts =/= all ->
            % Drop range response header
            C1 = cowmachine_req:remove_resp_header(<<"accept-ranges">>, Context),
            C2 = cowmachine_req:remove_resp_header(<<"content-range">>, C1),
            C3 = cowmachine_req:remove_resp_header(<<"content-length">>, C2),
            {200, C3, all};
        false ->
            {Code, Context, all};
        true ->
            {Code, Context, Parts}
    end,
    Headers = response_headers(Code1, Length, Context1),
    Req = cowmachine_req:req(Context1),
    Req1 = cowboy_req:stream_reply(Code, Headers, Req),
    Context2 = cowmachine_req:set_req(Req1, Context1),
    FirstHunk = case InitialStream of
        {InitialData, InitialFun} ->
            {InitialData, stream_initial_fun(InitialFun, Parts1)};
        InitialFun ->
            stream_initial_fun(InitialFun, Parts1)
    end,
    send_stream_body(FirstHunk, Context2).

stream_initial_fun(F, Parts) when is_function(F, 2) ->
    fun(Ctx) -> F(Ctx, Parts) end;
stream_initial_fun(F, _Parts) when is_function(F) ->
    F;
stream_initial_fun(done, _Parts) ->
    done.


%% Check if we support ranges on the data stream (body or function)
is_streaming_range(Fun) when is_function(Fun, 2) -> true;
is_streaming_range(Fun) when is_function(Fun) -> false;
is_streaming_range({<<>>, Fun}) when is_function(Fun, 2) ->
    % Only support ranges if the initial binary is a range, until we
    % add functionality to stream the data from the binary in accordance
    % with the requested ranges (and update the range given to the range function)
    true;
is_streaming_range({_, Fun}) when is_function(Fun) -> false;
is_streaming_range({_, done}) -> false.


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

response_headers(Context) ->
    Hdrs = cowmachine_req:get_resp_headers(Context),
    Hdrs#{
        <<"server">> => server_header(),
        <<"date">> => cowboy_clock:rfc1123()
    }.


% With continuation
send_stream_body({<<>>, done}, Context) ->
    % @TODO: in cowboy this give a wrong termination with two 0 size chunks
    send_chunk(Context, <<>>, fin);
send_stream_body({<<>>, Next}, Context) ->
    next(Next, Context);
send_stream_body({Data, Next}, Context) when is_binary(Data); is_list(Data) ->
    send_chunk(Context, Data, fin(Next)),
    next(Next, Context);
send_stream_body({{file, Filename}, Next}, Context) ->
    Length = filelib:file_size(Filename),
    send_stream_body({{file, Length, Filename}, Next}, Context);
send_stream_body({{file, 0, _Filename}, Next}, Context) ->
    send_stream_body(Context, {<<>>, Next});
send_stream_body({{file, Size, Filename}, Next}, Context) ->
    Context1 = send_file_body(Context, Size, Filename, fin(Next)),
    next(Next, Context1);
% Without continuation
send_stream_body(done, Context) ->
    send_chunk(Context, <<>>, fin);
send_stream_body(WriterFun, Context) when is_function(WriterFun, 1) ->
    WriterFun(Context);
send_stream_body(WriterFun, Context) when is_function(WriterFun, 0) ->
    _ = WriterFun(),
    Context.

next(Fun, Context) when is_function(Fun, 1) ->
    send_stream_body(Fun(Context), Context);
next(Fun, Context) when is_function(Fun, 0) ->
    send_stream_body(Fun(), Context);
next(done, Context) ->
    Context.

fin(done) -> fin;
fin(_) -> nofin.

send_device_body(Context, Length, IO) ->
    send_file_body_loop(Context, 0, Length, IO, fin).

send_file_body(Context, Length, Filename, FinNoFin) ->
    {ok, FD} = file:open(Filename, [read,raw,binary]),
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

-spec send_file_body_parts( cowmachine_req:context(), cowmachine_req:parts(), file:filename_all() ) -> ok | {error, term()}.
send_file_body_parts(Context, Parts, Filename) ->
    {ok, FD} = file:open(Filename, [raw,binary]),
    try
        send_device_body_parts(Context, Parts, FD)
    after
        file:close(FD)
    end.

-spec send_parts( cowmachine_req:context(), cowmachine_req:parts(), binary() ) -> cowmachine_req:context().
send_parts(Context, {[{From,Length}], _Size, _Boundary, _ContentType}, Bin) ->
    send_chunk(Context, binary:part(Bin,From,Length), fin);
send_parts(Context, {Parts, Size, Boundary, ContentType}, Bin) ->
    lists:foreach(
        fun({From,Length}) ->
            Part = [
                part_preamble(Boundary, ContentType, From, Length, Size),
                binary:part(Bin,From,Length),
                <<"\r\n">>
            ],
            send_chunk(Context, Part, nofin)
        end,
        Parts),
    send_chunk(Context, end_boundary(Boundary), fin).


send_file_body_loop(Context, Offset, Size, _Device, FinNoFin) when Offset =:= Size ->
    send_chunk(Context, <<>>, FinNoFin);
send_file_body_loop(Context, Offset, Size, Device, FinNoFin) when Size - Offset =< ?FILE_CHUNK_LENGTH ->
    {ok, Data} = file:read(Device, Size - Offset),
    send_chunk(Context, Data, FinNoFin);
send_file_body_loop(Context, Offset, Size, Device, FinNoFin) ->
    {ok, Data} = file:read(Device, ?FILE_CHUNK_LENGTH),
    send_chunk(Context, Data, nofin),
    send_file_body_loop(Context, Offset+iolist_size(Data), Size, Device, FinNoFin).

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
        Ranges = binary:split(binary:replace(RangeString, <<" ">>, <<>>, [global]), <<",">>, [global]),
        lists:map(
            fun (<<"-", V/binary>>)  ->
                   {none, binary_to_integer(V)};
                (R) ->
                    case binary:split(R, <<"-">>) of
                        [S1, <<>>] -> {binary_to_integer(S1), none};
                        [<<>>, S2] -> {none, binary_to_integer(S2)};
                        [S1, S2] -> {binary_to_integer(S1), binary_to_integer(S2)};
                        [S1] -> {binary_to_integer(S1), none}
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

