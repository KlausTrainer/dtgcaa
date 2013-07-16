-module(dtgcaa_http).

%% API
-export([start/1, stop/1]).

-spec start(inet:port_number()) -> {ok, pid()}.
start(Port) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', dtgcaa_https_redirect_handler, []}
        ]}
    ]),
    cowboy:start_http(?MODULE, 64, [
        {port, Port}
    ], [{env, [{dispatch, Dispatch}]}]).

stop(Pid) ->
    cowboy:stop_listener(Pid).

