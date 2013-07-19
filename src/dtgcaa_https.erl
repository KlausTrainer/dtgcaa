-module(dtgcaa_https).

%% API
-export([start/1, stop/1]).

-spec start(inet:port_number()) -> {ok, pid()}.
start(Port) ->
    PrivDir = code:priv_dir(dtgcaa),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, [
                {directory, PrivDir ++ "/www"},
                {file, <<"index.html">>}
            ]},
            {"/:uri", dtgcaa_handler, []}
        ]}
    ]),
    UnBrokenCiphers = lists:filter(fun(Suite) ->
        string:left(atom_to_list(element(1, Suite)), 4) =/= "ecdh"
    end, ssl:cipher_suites()),
    cowboy:start_https(?MODULE, 64, [
        {port, Port},
        {cacertfile, PrivDir ++ "/ssl/ca.crt"},
        {certfile, PrivDir ++ "/ssl/server.crt"},
        {keyfile, PrivDir ++ "/ssl/server.key"},
        {ciphers, UnBrokenCiphers}
    ], [{env, [{dispatch, Dispatch}]}]).

stop(Pid) ->
    cowboy:stop_listener(Pid).
