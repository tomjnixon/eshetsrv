-module(systemd_support_sup).

-behaviour(supervisor).

-export([
    start_link/1,
    init/1
]).

start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

init(_Opts) ->
    SupFlags = #{
        strategy => one_for_one
    },
    Children = [
        systemd:ready()
    ],

    {ok, {SupFlags, Children}}.
