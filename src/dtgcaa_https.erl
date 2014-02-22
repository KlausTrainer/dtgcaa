-module(dtgcaa_https).

%% API
-export([start/1, stop/1]).

-spec start(inet:port_number()) -> {ok, pid()}.
start(Port) ->
    PrivDir = code:priv_dir(dtgcaa),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static,
                {file, PrivDir ++ "/www/index.html", [{mimetypes,
                    {<<"text">>, <<"html">>, []}}]}
            },
            {"/favicon.gif", cowboy_static,
                {file, PrivDir ++ "/www/favicon.gif", [{mimetypes,
                    {<<"image">>, <<"gif">>, []}}]}
            },
            {"/:uri", dtgcaa_handler, []}
        ]}
    ]),
    cowboy:start_https(?MODULE, 64, [
        {port, Port},
        {cacertfile, PrivDir ++ "/ssl/ca.crt"},
        {certfile, PrivDir ++ "/ssl/server.crt"},
        {keyfile, PrivDir ++ "/ssl/server.key"}
    ], [{env, [{dispatch, Dispatch}]}]).

stop(Pid) ->
    cowboy:stop_listener(Pid).
