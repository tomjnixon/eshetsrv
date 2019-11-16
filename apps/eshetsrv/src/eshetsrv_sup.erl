-module(eshetsrv_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
             #{id => eshetsrv_state,
               start => {eshetsrv_state, start_link, [{local, eshetsrv_state}]},
               modules => [eshetsrv_state]
              },
             #{id => eshetsrv_meta,
               start => {eshetsrv_meta, start_link, [eshetsrv_state]},
               modules => [eshetsrv_meta]
              }
            ],
    {ok, {{one_for_one, 5, 5}, Procs}}.
