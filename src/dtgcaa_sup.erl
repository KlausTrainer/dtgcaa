-module(dtgcaa_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Processes = [
        {
            dtgcaa_http,
            {dtgcaa_http, start, []},
            permanent, 2000, worker, dynamic
        }
    ],
    {ok, {{one_for_one, 5, 10}, Processes}}.
