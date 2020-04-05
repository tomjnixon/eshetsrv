-module(eshethttp_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/call/[...]",
                                         eshethttp_action_call,
                                         [eshetsrv_state]}]}]),
    {ok, _} = cowboy:start_clear(eshethttp,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    eshethttp_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(eshethttp).
