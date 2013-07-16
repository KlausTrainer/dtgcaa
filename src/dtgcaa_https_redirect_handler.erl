%% @doc dtgcaa_https_redirect_handler.
-module(dtgcaa_https_redirect_handler).

%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(REDIRECT_HEADERS, [
    {<<"content-type">>, <<"text/html">>},
    {<<"strict-transport-security">>, <<"max-age=315360000">>},
    {<<"access-control-allow-origin">>, <<"*">>},
    {<<"access-control-allow-methods">>, <<"GET, PUT, POST, OPTIONS">>},
    {<<"access-control-max-age">>, <<"86400">>}
]).

-define(REDIRECT_BODY,
    <<"<html><head><title>301 Moved Permanently</title></head>"
      "<body bgcolor=\"white\"><center><h1>301 Moved Permanently</h1>"
      "</center><hr><center>Cowboy</center></body></html>">>).

-spec init({tcp | ssl, http}, cowboy_req:req(), []) -> {ok, cowboy_req:req(), undefined}.
init({_TransportName, http}, Req, []) ->
	{ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
	ok.

-spec handle(cowboy_req:req(), undefined) -> {ok, cowboy_req:req(), undefined}.
handle(Req, undefined) ->
    {Url, _} = cowboy_req:url(Req),
    <<_:5/binary,Uri/binary>> = Url,
    HttpsUrl = <<"https:",Uri/binary>>,
    ResponseHeaders = [{<<"location">>, HttpsUrl} | ?REDIRECT_HEADERS],
    {ok, Req2} = cowboy_req:reply(301, ResponseHeaders, ?REDIRECT_BODY, Req),
    {ok, Req2, undefined}.
