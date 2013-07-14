-module(dtgcaa).

%% API
-export([start/0]).

start() ->
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ibrowse),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(sasl),
    ok = application:start(dtgcaa).
