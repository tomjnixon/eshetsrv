-module(eshetproto_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, _} = ranch:start_listener(tcp_eshetnet, 100,
                                   ranch_tcp, [{port, 11236}],
                                   eshetnet_proto, [eshetsrv_state]),
    eshetproto_sup:start_link().

stop(_State) ->
    ok.
