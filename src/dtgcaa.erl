-module(dtgcaa).

%% API
-export([start/0, get_app_env/1, get_app_env/2]).

start() ->
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ibrowse),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(sasl),
    ok = application:start(dtgcaa).

%% @doc The official way to get the values set in dtgcaa's environment.
%% Will return `undefined' if the given option is unset.
-spec get_app_env(atom()) -> term().
get_app_env(Opt) ->
    get_app_env(Opt, undefined).

%% @doc The official way to get the values set in dtgcaa's environment.
%% Will return `Default' if the given option is unset.
-spec get_app_env(atom(), term()) -> term().
get_app_env(Opt, Default) ->
    case application:get_env(?MODULE, Opt) of
    undefined -> Default;
    {ok, Value} -> Value
    end.
