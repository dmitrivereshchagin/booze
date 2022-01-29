-module(booze_message).

-export([request/2,
         request/3,
         request/4]).

-export([method/1,
         set_method/2]).

-export([uri/1,
         set_uri/2,
         target/1]).

-export([response/1,
         response/2,
         response/3]).

-export([status/1,
         set_status/2]).

-export([headers/1,
         has_header/2,
         header_values/2,
         header_line/2,
         set_header/3,
         add_header/3,
         remove_header/2]).

-export([body/1,
         set_body/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type request() :: #{method  := iodata(),
                     uri     := iodata(),
                     headers := request_headers(),
                     body    := iodata()}.
-export_type([request/0]).

-type request_headers() :: booze_message_headers:headers(iodata()).
-export_type([request_headers/0]).

-type response() :: #{status  := status(),
                      headers := response_headers(),
                      body    := binary()}.
-export_type([response/0]).

-type response_headers() :: booze_message_headers:headers(binary()).
-export_type([response_headers/0]).

-type status() :: 100..599.
-export_type([status/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec request(iodata(), iodata()) -> request().
request(Method, URI) ->
    request(Method, URI, []).

-spec request(iodata(), iodata(), request_headers()) -> request().
request(Method, URI, Headers) ->
    request(Method, URI, Headers, <<>>).

-spec request(iodata(), iodata(), request_headers(), iodata()) -> request().
request(Method, URI, Headers, Body) ->
    #{method => Method, uri => URI, headers => Headers, body => Body}.

-spec method(request()) -> iodata().
method(#{method := Method}) ->
    Method.

-spec set_method(iodata(), request()) -> request().
set_method(Method, Request) ->
    Request#{method := Method}.

-spec uri(request()) -> iodata().
uri(#{uri := URI}) ->
    URI.

-spec set_uri(iodata(), request()) -> request().
set_uri(URI, Request) ->
    Request#{uri := URI}.

-spec target(request()) -> booze_uri:uri_string().
target(#{uri := URI}) ->
    %% Maybe store URI as booze_uri:uri/0?
    URIMap = booze_uri:normalize(URI, [return_map]),
    booze_uri:recompose(maps:with([path, query], URIMap)).

-spec response(status()) -> response().
response(Status) ->
    response(Status, []).

-spec response(status(), response_headers()) -> response().
response(Status, Headers) ->
    response(Status, Headers, <<>>).

-spec response(status(), response_headers(), binary()) -> response().
response(Status, Headers, Body) ->
    #{status => Status, headers => Headers, body => Body}.

-spec status(response()) -> status().
status(#{status := Status}) ->
    Status.

-spec set_status(status(), response()) -> response().
set_status(Status, Response) ->
    Response#{status := Status}.

-spec headers(request())  -> request_headers();
             (response()) -> response_headers().
headers(#{headers := Headers}) ->
    Headers.

-spec has_header(binary(), request() | response()) -> boolean().
has_header(Name, #{headers := Headers}) ->
    booze_message_headers:has(Name, Headers).

-spec header_values(binary(), request())  -> [iodata()];
                   (binary(), response()) -> [binary()].
header_values(Name, #{headers := Headers}) ->
    booze_message_headers:values(Name, Headers).

-spec header_line(binary(), request() | response()) -> binary().
header_line(Name, #{headers := Headers}) ->
    Values = booze_message_headers:values(Name, Headers),
    iolist_to_binary(lists:join(", ", Values)).

-spec set_header(binary(), iodata(), request())  -> request();
                (binary(), binary(), response()) -> response().
set_header(Name, Value, #{headers := Headers} = Message) ->
    NewHeaders = booze_message_headers:set(Name, Value, Headers),
    Message#{headers := NewHeaders}.

-spec add_header(binary(), iodata(), request())  -> request();
                (binary(), binary(), response()) -> response().
add_header(Name, Value, #{headers := Headers} = Message) ->
    NewHeaders = booze_message_headers:add(Name, Value, Headers),
    Message#{headers := NewHeaders}.

-spec remove_header(binary(), request())  -> request();
                   (binary(), response()) -> response().
remove_header(Name, #{headers := Headers} = Message) ->
    NewHeaders = booze_message_headers:remove(Name, Headers),
    Message#{headers := NewHeaders}.

-spec body(request())  -> iodata();
          (response()) -> binary().
body(#{body := Body}) ->
    Body.

-spec set_body(iodata(), request())  -> request();
              (binary(), response()) -> response().
set_body(Body, Message) ->
    Message#{body := Body}.
