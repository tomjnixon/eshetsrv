-module(eshethttp_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Port = application:get_env(eshethttp, port, 11237),

    Dispatch = cowboy_router:compile([{'_',
                                       [{"/call/[...]",
                                         eshethttp_action_call,
                                         [eshetsrv_state]}]}]),
    {ok, _} = cowboy:start_clear(eshethttp,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    eshethttp_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(eshethttp).
