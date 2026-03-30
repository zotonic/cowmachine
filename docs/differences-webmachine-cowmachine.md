# Differences between Webmachine and Cowmachine callbacks

Cowmachine is a rewrite of [Webmachine](https://github.com/webmachine/webmachine) for
[Cowboy](https://github.com/ninenines/cowboy). Although it follows the same HTTP decision
flow, several callbacks were renamed, removed, merged, or added. This document lists all
differences.

## Startup

| Webmachine | Cowmachine |
|---|---|
| `init(Args) -> {ok, State} \| {{trace,Dir}, State}` | `execute(Req, Env) -> {ok, Req, Env#{cowmachine_controller => Module}}` |

Webmachine resources were registered via a dispatcher and initialised with `init/2`.
Cowmachine controllers are Cowboy **middlewares**: the `execute/2` function inserts the
controller module into the Cowboy environment so that the `cowmachine` middleware can find
it. This aligns with how all other Cowboy middlewares work.

## Callback arity and return value

Every callback signature changed:

| Webmachine | Cowmachine |
|---|---|
| `Fun(ReqData, State) -> {Result, ReqData, State}` | `Fun(Context) -> {Result, Context}` |

Webmachine carried two separate arguments for the request data (`ReqData`) and the
resource state (`State`). Cowmachine merges both into a single opaque `Context` map.

## Renamed callbacks

| Webmachine | Cowmachine | Notes |
|---|---|---|
| `encodings_provided/2` | `content_encodings_provided/1` | Name made consistent with `content_types_provided` |

## Removed callbacks (replaced by `process/4`)

Webmachine used named handler functions that were looked up at content negotiation time.
Cowmachine replaces all of them with a single, unified `process/4` callback:

```erlang
-spec process(Method, AcceptedContentType, ProvidedContentType, Context) ->
    {true | RespBody | halt(), Context}.
```

The controller pattern-matches on the HTTP method and the negotiated content types.

| Webmachine mechanism | What it did | Cowmachine replacement |
|---|---|---|
| `delete_resource/2` | Called at `v3m20` to perform DELETE | `process(<<"DELETE">>, ...)` |
| `process_post/2` | Called at `v3n11` to handle POST body | `process(<<"POST">>, ...)` |
| Named functions in `content_types_provided` list | Called to generate the GET/HEAD response body for a given media type | `process(<<"GET">>, _, ProvidedCT, ...)` |
| Named functions in `content_types_accepted` list | Called to process a PUT/PATCH/POST body for a given media type | `process(Method, AcceptedCT, _, ...)` |

### `content_types_provided` format change

Webmachine expected a list of `{MediaTypeString, HandlerFunName}` tuples, where
`HandlerFunName` was an atom naming a callback on the resource module:

```erlang
%% Webmachine
content_types_provided(Req, State) ->
    {[{"text/html", to_html}, {"application/json", to_json}], Req, State}.

to_html(Req, State) -> {<<"<html>…</html>">>, Req, State}.
to_json(Req, State) -> {<<"{…}">>,            Req, State}.
```

Cowmachine expects a list of normalised `{Type, SubType, Params}` tuples and dispatches
all body generation to `process/4`:

```erlang
%% Cowmachine
content_types_provided(Context) ->
    {[{<<"text">>, <<"html">>, []}, {<<"application">>, <<"json">>, []}], Context}.

process(<<"GET">>, _AcceptedCT, {<<"text">>,        <<"html">>,   _}, Context) -> {<<"<html>…</html>">>, Context};
process(<<"GET">>, _AcceptedCT, {<<"application">>, <<"json">>,   _}, Context) -> {<<"{…}">>,            Context}.
```

### `content_types_accepted` format change

Same change: no handler function name, only the normalised media type tuple. The body
processing logic lives in `process/4`.

## New callbacks (not in Webmachine)

| Callback | Description |
|---|---|
| `process/4` | Unified handler for all HTTP methods; replaces `delete_resource`, `process_post`, and named content handlers |
| `transfer_encodings_provided/1` | List of `{Encoding, Fun}` pairs for transfer-encoding negotiation (e.g. chunked) |
| `upgrades_provided/1` | List of `{Protocol, UpgradeFun}` pairs used for protocol upgrade (e.g. WebSocket) |
| `execute/2` | Cowboy middleware entry point; sets the controller module in the request environment |

## Changed callback behaviour

### `is_authorized/1`

| Webmachine | Cowmachine |
|---|---|
| Any non-`true` return value is used as the `WWW-Authenticate` header value and 401 is returned | `false` returns 401 with a default `z.auth` challenge; a binary returns 401 with that binary as the `WWW-Authenticate` value; `true` continues |

### `known_methods/1` and `allowed_methods/1`

| Webmachine | Cowmachine |
|---|---|
| Returns a list of atoms: `['GET', 'HEAD', 'POST', …]` | Returns a list of binaries: `[<<"GET">>, <<"HEAD">>, <<"POST">>, …]` |

### `charsets_provided/1`

| Webmachine | Cowmachine |
|---|---|
| `[{CharsetName, ConversionFun}]` or `no_charset` | `[CharsetName]` or `no_charset` — no conversion function |

### `content_encodings_provided/1` (was `encodings_provided/2`)

| Webmachine | Cowmachine |
|---|---|
| `[{EncodingName, EncodingFun}]` — encoding function applied inline | `[EncodingName]` — a list of binary content-coding names used for `Accept-Encoding` negotiation; cowmachine sets the `Content-Encoding` header based on the chosen encoding, but it does not transform the response body itself (that must be done by the controller or other middleware/handlers) |

## Full callback comparison table

| Callback | Webmachine arity | Cowmachine arity | Notes |
|---|---|---|---|
| `service_available` | 2 | 1 | |
| `known_methods` | 2 | 1 | Returns binaries instead of atoms in cowmachine |
| `uri_too_long` | 2 | 1 | |
| `allowed_methods` | 2 | 1 | Returns binaries instead of atoms in cowmachine |
| `validate_content_checksum` | 2 | 1 | |
| `malformed_request` | 2 | 1 | |
| `is_authorized` | 2 | 1 | See behaviour change above |
| `forbidden` | 2 | 1 | |
| `upgrades_provided` | — | 1 | **New in cowmachine** |
| `valid_content_headers` | 2 | 1 | |
| `known_content_type` | 2 | 1 | |
| `valid_entity_length` | 2 | 1 | |
| `options` | 2 | 1 | |
| `content_types_provided` | 2 | 1 | Format changed; no handler function names |
| `language_available` | 2 | 1 | |
| `charsets_provided` | 2 | 1 | No conversion function in cowmachine |
| `encodings_provided` | 2 | — | **Renamed** to `content_encodings_provided` |
| `content_encodings_provided` | — | 1 | Renamed from `encodings_provided`; no encoding function |
| `transfer_encodings_provided` | — | 1 | **New in cowmachine** — carries the encoding functions |
| `variances` | 2 | 1 | |
| `resource_exists` | 2 | 1 | |
| `generate_etag` | 2 | 1 | |
| `last_modified` | 2 | 1 | |
| `moved_permanently` | 2 | 1 | |
| `previously_existed` | 2 | 1 | |
| `moved_temporarily` | 2 | 1 | |
| `allow_missing_post` | 2 | 1 | |
| `delete_resource` | 2 | — | **Removed**; logic moved to `process/4` |
| `delete_completed` | 2 | 1 | |
| `post_is_create` | 2 | 1 | |
| `create_path` | 2 | 1 | |
| `base_uri` | 2 | 1 | |
| `content_types_accepted` | 2 | 1 | Format changed; no handler function names |
| `process_post` | 2 | — | **Removed**; logic moved to `process/4` |
| `is_conflict` | 2 | 1 | |
| `multiple_choices` | 2 | 1 | |
| `expires` | 2 | 1 | |
| `finish_request` | 2 | 1 | |
| `process` | — | 4 | **New in cowmachine** — `process(Method, AcceptedCT, ProvidedCT, Context)` |
| `execute` | — | 2 | **New in cowmachine** — Cowboy middleware entry point |

## HTTP decision flow differences

### Added node: `v3b6_upgrade`

Cowmachine inserts a protocol-upgrade check between `forbidden?` (`v3b7`) and
`valid_content_headers?` (`v3b6`). If the request carries an `Upgrade` header together
with `Connection: Upgrade`, the `upgrades_provided/1` callback is consulted. If a
matching protocol is found, the request is handed off to the upgrade handler (e.g.
WebSocket). This node does not exist in Webmachine.

### `v3h7` — If-Match on a missing resource

| Webmachine | Cowmachine |
|---|---|
| Any `If-Match` header present when the resource does not exist → 412 | Only `If-Match: *` when the resource does not exist → 412; a specific ETag is still checked |

### `v3m20` — DELETE processing

| Webmachine | Cowmachine |
|---|---|
| Calls `delete_resource/2` directly | Calls `process(<<"DELETE">>, undefined, ProvidedCT, Context)` |

### `v3n11` — POST processing

| Webmachine | Cowmachine |
|---|---|
| `post_is_create=false` → calls `process_post/2` directly | `post_is_create=false` → calls `process(<<"POST">>, AcceptedCT, ProvidedCT, Context)` via `accept_process_helper` |

### `v3o18` — GET/HEAD body generation

| Webmachine | Cowmachine |
|---|---|
| Looks up the handler function name in the `content_types_provided` list and calls it as a resource callback | Calls `process(<<"GET">> \| <<"HEAD">>, undefined, ProvidedCT, Context)` |
