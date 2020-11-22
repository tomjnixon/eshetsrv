-module(eshethttp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [#{id => eshetnet_registry,
               start =>
                   {eshetnet_registry, start_link, [{local, eshethttp_registry}, eshetsrv_state]},
               modules => [eshetnet_registry]}],
    {ok, {{one_for_one, 1, 5}, Procs}}.
