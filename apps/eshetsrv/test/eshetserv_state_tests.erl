-module(eshetserv_state_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    {ok, Pid} = eshetsrv_state:start_link(),
    Pid.

stop(Pid) ->
    gen_server:stop(Pid).

start_stop_test_() ->
    {"the server can be started and stopped",
     ?setup(fun nothing/1)}.

nothing(_Pid) ->
    [].

register_test_() ->
    [
     {"register action",
      ?setup(fun register_action/1)},
     {"register prop",
      ?setup(fun register_prop/1)},
     {"register event then listener",
      ?setup(fun register_event_first/1)},
     {"register listener then event",
      ?setup(fun register_listener_first/1)}
    ].

register_action(Pid) ->
    Self = self(),
    [?_assertEqual(eshetsrv_state:register(Pid, <<"/foo">>, action_owner, Self),
                   ok),
     ?_assertEqual(eshetsrv_state:lookup(Pid, <<"/foo">>, action_owner),
                   [Self]),
     ?_assertEqual(eshetsrv_state:register(Pid, <<"/foo">>, action_owner, Self),
                   {error, already_registered})
    ].


register_prop(Pid) ->
    Self = self(),
    [?_assertEqual(eshetsrv_state:register(Pid, <<"/foo">>, prop_owner, Self),
                   ok),
     ?_assertEqual(eshetsrv_state:lookup(Pid, <<"/foo">>, prop_owner),
                   [Self]),
     ?_assertEqual(eshetsrv_state:register(Pid, <<"/foo">>, prop_owner, Self),
                   {error, already_registered})
    ].


register_event_first(Pid) ->
    Self = self(),
    [?_assertEqual(eshetsrv_state:register(Pid, <<"/foo">>, event_owner, Self),
                   ok),
     ?_assertEqual(eshetsrv_state:lookup(Pid, <<"/foo">>, event_owner),
                   [Self]),
     ?_assertEqual(eshetsrv_state:register(Pid, <<"/foo">>, event_owner, Self),
                   {error, already_registered}),
     ?_assertEqual(eshetsrv_state:register(Pid, <<"/foo">>, event_listener, Self),
                   ok),
     ?_assertEqual(eshetsrv_state:lookup(Pid, <<"/foo">>, event_listener),
                   [Self]),
     ?_assertEqual(eshetsrv_state:register(Pid, <<"/foo">>, event_listener, Self),
                   ok),
     ?_assertEqual(eshetsrv_state:lookup(Pid, <<"/foo">>, event_listener),
                   [Self, Self])
    ].

register_listener_first(Pid) ->
    Self = self(),
    [
     ?_assertEqual(eshetsrv_state:register(Pid, <<"/foo">>, event_listener, Self),
                   ok),
     ?_assertEqual(eshetsrv_state:lookup(Pid, <<"/foo">>, event_listener),
                   [Self]),
     ?_assertEqual(eshetsrv_state:register(Pid, <<"/foo">>, event_owner, Self),
                   ok),
     ?_assertEqual(eshetsrv_state:lookup(Pid, <<"/foo">>, event_owner),
                   [Self])
    ].

link_test_() ->
    [
     {"register then exit immediately",
      ?setup(fun register_exit/1)},
     {"register, check, then exit",
      ?setup(fun register_check_exit/1)}
    ].

register_exit(Pid) ->
    F = fun () ->
                eshetsrv_state:register(Pid, <<"/foo">>, action_owner, self())
        end,
    {_Pid, MonitorRef} = spawn_monitor(F),
    receive
        {_Tag, MonitorRef, _Type, _Object, _Info} -> 
            ?_assertEqual(eshetsrv_state:lookup(Pid, <<"/foo">>, action_owner), [])
    end.


register_check_exit(Pid) ->
    F = fun () ->
                eshetsrv_state:register(Pid, <<"/foo">>, action_owner, self()),
                ?assertEqual(eshetsrv_state:lookup(Pid, <<"/foo">>, action_owner), [self()])
        end,
    {_Pid, MonitorRef} = spawn_monitor(F),
    receive
        {_Tag, MonitorRef, _Type, _Object, _Info} -> 
            ?_assertEqual(eshetsrv_state:lookup(Pid, <<"/foo">>, action_owner), [])
    end.
