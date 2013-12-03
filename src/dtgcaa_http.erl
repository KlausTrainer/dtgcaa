-module(dtgcaa_http).

%% API
-export([start/0, stop/1]).

-spec start() -> {ok, pid()}.
start() ->
    Port = dtgcaa:get_app_env(port, 8000),
    CouchUri = dtgcaa:get_app_env(couch_uri, "http://127.0.0.1:5984"),
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
            {"/:uri", dtgcaa_handler, [CouchUri]}
        ]}
    ]),
    cowboy:start_http(?MODULE, 64, [
        {port, Port}
    ], [{env, [{dispatch, Dispatch}]}]).

stop(Pid) ->
    cowboy:stop_listener(Pid).
