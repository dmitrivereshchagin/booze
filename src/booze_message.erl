-module(booze_message).

-export([request/1,
         request/2,
         request/3,
         request/4,
         method/1,
         set_method/2,
         uri/1,
         set_uri/2]).

-export([response/3,
         code/1,
         set_code/2]).

-export([headers/1,
         set_headers/2,
         add_header/3,
         remove_header/2,
         body/1,
         set_body/2]).

-export_type([message/0,
              request/0,
              response/0,
              headers/0,
              code/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type message() :: request() | response().

-type request() :: #{method  := binary(),
                     uri     := binary(),
                     headers := headers(),
                     body    := binary()}.

-type response() :: #{code    := code(),
                      headers := headers(),
                      body    := binary()}.

-type headers() :: [{binary(), binary()}].

-type code() :: 100..599.

%%%===================================================================
%%% API
%%%===================================================================

-spec request(URI) -> request() when
      URI :: binary().
request(URI) ->
    request(<<"GET">>, URI).

-spec request(Method, URI) -> request() when
      Method :: binary(), URI :: binary().
request(Method, URI) ->
    request(Method, URI, []).

-spec request(Method, URI, headers()) -> request() when
      Method :: binary(), URI :: binary().
request(Method, URI, Headers) ->
    request(Method, URI, Headers, <<>>).

-spec request(Method, URI, headers(), Body) -> request() when
      Method :: binary(), URI :: binary(), Body :: binary().
request(Method, URI, Headers, Body) ->
    #{method  => Method,
      uri     => URI,
      headers => Headers,
      body    => Body}.

-spec method(request()) -> binary().
method(#{method := Method}) ->
    Method.

-spec set_method(Method, request()) -> request() when
      Method :: binary().
set_method(Method, Req) ->
    Req#{method := Method}.

-spec uri(request()) -> binary().
uri(#{uri := URI}) ->
    URI.

-spec set_uri(URI, request()) -> request() when
      URI :: binary().
set_uri(URI, Req) ->
    Req#{uri := URI}.

-spec response(code(), headers(), Body) -> response() when
      Body :: binary().
response(Code, Headers, Body) ->
    #{code    => Code,
      headers => Headers,
      body    => Body}.

-spec code(response()) -> code().
code(#{code := Code}) ->
    Code.

-spec set_code(code(), response()) -> response().
set_code(Code, Resp) ->
    Resp#{code := Code}.

-spec headers(message()) -> headers().
headers(#{headers := Headers}) ->
    Headers.

-spec set_headers(headers(), message()) -> message().
set_headers(Headers, Req) ->
    Req#{headers := Headers}.

-spec add_header(Name, Value, message()) -> message() when
      Name :: binary(), Value :: binary().
add_header(Name, Value, #{headers := Headers} = Req) ->
    Req#{headers := [{Name, Value} | Headers]}.

-spec remove_header(Name, message()) -> message() when
      Name :: binary().
remove_header(Name, #{headers := Headers} = Req) ->
    F = fun({N, _}) -> not string:equal(N, Name, true) end,
    Req#{headers := lists:filter(F, Headers)}.

-spec body(message()) -> binary().
body(#{body := Body}) ->
    Body.

-spec set_body(Body, message()) -> message() when
      Body :: binary().
set_body(Body, Req) ->
    Req#{body := Body}.
