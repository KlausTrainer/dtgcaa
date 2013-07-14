%% @doc dtgcaa_handler.
-module(dtgcaa_handler).

%% API
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

%-export([provide_html/2]).
-export([provide_json/2]).

-define(IBROWSE_OPTS, [{response_format, binary}, {stream_to, {self(), once}}]).

%% External API

-spec init({tcp | ssl, http}, cowboy_req:req(), []) -> {upgrade, protocol, cowboy_rest}.
init({_TransportName, http}, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
	{[
%		{<<"text/html">>, provide_html},
		{<<"application/json">>, provide_json}
	], Req, State}.


%% Internal API

-spec provide_json(cowboy_req:req(), undefined) -> {halt, cowboy_req:req(), undefined}.
provide_json(Req, undefined) ->
    {ok, _Response} = case cowboy_req:bindings(Req) of
    {[{uri, EscapedUri}], _} ->
        Uri = http_uri:decode(binary_to_list(EscapedUri)),
        case check_gif(Uri) of
        true ->
            send_response(200, <<"{\"containsananimation\":true}">>, Req);
        false ->
            send_response(200, <<"{\"containsananimation\":false}">>, Req);
        {error, no_gif} ->
            send_response(400, <<"{\"error\":\"This is no gif!\"}">>, Req);
        {error, Reason} ->
            Error = case Reason of
            _ when is_atom(Reason) ->
                list_to_binary(atom_to_list(Reason));
            _ when is_tuple(Reason) ->
                list_to_binary(atom_to_list(element(1, Reason)))
            end,
            send_response(400, <<"{\"error\":\"",Error/binary,"\"}">>, Req)
        end;
    _ ->
        send_response(400, <<"{\"error\":\"bad request\"}">>, Req)
    end,
    {halt, Req, undefined}.

send_response(StatusCode, Body, Req) ->
    case cowboy_req:qs_val(<<"callback">>, Req) of
    {Value, _} when Value =:= undefined; Value =:= true->
        ResponseHeaders = [
            {<<"content-type">>, <<"application/json; charset=utf-8">>}
        ],
        cowboy_req:reply(StatusCode, ResponseHeaders, Body, Req);
    {Value, _} when is_binary(Value) ->
        ResponseHeaders = [
            {<<"content-type">>, <<"application/json-p; charset=utf-8">>}
        ],
        JsonpBody = <<Value/binary,"(",Body/binary,");">>,
        cowboy_req:reply(StatusCode, ResponseHeaders, JsonpBody, Req)
    end.

check_gif(Uri) ->
    case ibrowse:send_req(Uri, [], get, <<>>, ?IBROWSE_OPTS) of
    {error, Reason} -> {error, Reason};
    {ibrowse_req_id, ReqId} -> check_gif(ReqId, 0, <<>>, 0, 0)
    end.

check_gif(_ReqId, 6, _Data, _Offset, _PixmapCount) ->
    {error, too_many_redirects};
check_gif(ReqId, RedirectCount, Data0, Offset, PixmapCount) ->
    receive
        {ibrowse_async_headers, ReqId, Status, Headers} ->
            case list_to_integer(Status) of
            200 ->
                ok = ibrowse:stream_next(ReqId),
                check_gif(ReqId, RedirectCount, Data0, Offset, PixmapCount);
            StatusCode when 301 =< StatusCode, StatusCode =< 303 -> % redirect
                ok = ibrowse:stream_close(ReqId),
                CanonicalizedHeaders = canonicalize_headers(Headers),
                case proplists:get_value("location", CanonicalizedHeaders) of
                undefined ->
                    {error, missing_redirect_location};
                RedirectUri ->
                    case ibrowse:send_req(RedirectUri, [], get, <<>>, ?IBROWSE_OPTS) of
                    {error, Reason} ->
                        {error, Reason};
                    {ibrowse_req_id, NewReqId} ->
                        check_gif(NewReqId, RedirectCount + 1, <<>>, 0, 0)
                    end
                end;
            StatusCode ->
                ok = ibrowse:stream_close(ReqId),
                {error, ibrowse_lib:status_code(StatusCode)}
            end;
        {ibrowse_async_response, ReqId, Data1} when byte_size(Data0) + byte_size(Data1) =< Offset ->
            ok = ibrowse:stream_next(ReqId),
            check_gif(ReqId, <<Data0/binary,Data1/binary>>, RedirectCount, Offset, PixmapCount);
        {ibrowse_async_response, ReqId, Data1} ->
            Data2 = <<Data0/binary,Data1/binary>>,
            case dtgcaa_gif:contains_animation(Data2, Offset, PixmapCount) of
            {stream_next, Offset2, PixmapCount2} ->
                ok = ibrowse:stream_next(ReqId),
                check_gif(ReqId, RedirectCount, Data2, Offset2, PixmapCount2);
            {error, no_gif} = Error ->
                ok = ibrowse:stream_close(ReqId),
                Error;
            {ok, ContainsAnimation} ->
                ContainsAnimation
            end;
        {ibrowse_async_response_end, ReqId} ->
            false
    end.

canonicalize_headers(Headers) ->
    [{string:to_lower(K), V} || {K, V} <- Headers].
