-module(systemd_support_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    systemd_support_sup:start_link([]).

stop(_State) ->
    ok.
