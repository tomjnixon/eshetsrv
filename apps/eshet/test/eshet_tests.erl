-module(eshet_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    {ok, Server} = eshetsrv_state:start_link(),
    {ok, Client} = eshet_test_server:start_link(Server),
    {Server, Client}.

stop({Server, Client}) ->
    ok =gen_server:stop(Client),
    ok =gen_server:stop(Server).

eshet_test_() ->
    [
     {"action",
      ?setup({with, [fun action/1]})},
     {"prop",
      ?setup({with, [fun prop/1]})},
     {"event",
      ?setup({with, [fun event/1]})}
    ].

action({Server, _Client}) ->
    {ok, action_result} = eshet:action_call(Server, <<"/action">>, []).

prop({Server, _Client}) ->
    ok = eshet:prop_set(Server, <<"/prop">>, prop_value),
    {ok, prop_value} = eshet:prop_get(Server, <<"/prop">>).

event({Server, Client}) ->
    ok = gen_server:call(Client, {set_test_client, self()}),
    ok = eshet:event_register(Server, <<"/event">>),
    ok = eshet:event_emit(Server, <<"/event">>, event_value),
    ok = receive
             {event, event_value} -> ok
         after
             100 -> timeout
         end.
