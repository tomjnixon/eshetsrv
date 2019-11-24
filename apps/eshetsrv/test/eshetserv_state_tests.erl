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
      ?setup({with, [fun register_event_first/1]})},
     {"register listener then event",
      ?setup(fun register_listener_first/1)}
    ].

register_action(Pid) ->
    Self = self(),
    [?_assertEqual(ok,
                   eshetsrv_state:register(Pid, action_owner, <<"/foo">>, Self)),
     ?_assertEqual({ok, Self},
                   eshetsrv_state:lookup(Pid, action_owner, <<"/foo">>)),
     ?_assertEqual({error, path_already_exists},
                   eshetsrv_state:register(Pid, action_owner, <<"/foo">>, Self))
    ].


register_prop(Pid) ->
    Self = self(),
    [?_assertEqual(ok,
                   eshetsrv_state:register(Pid, prop_owner, <<"/foo">>, Self)),
     ?_assertEqual({ok, Self},
                   eshetsrv_state:lookup(Pid, prop_owner, <<"/foo">>)),
     ?_assertEqual({error, path_already_exists},
                   eshetsrv_state:register(Pid, prop_owner, <<"/foo">>, Self))
    ].


register_event_first(Pid) ->
    Self = self(),
    ?assertEqual(ok,
                  eshetsrv_state:register(Pid, event_owner, <<"/foo">>, Self)),
    ?assertEqual({error, path_already_exists},
                  eshetsrv_state:register(Pid, event_owner, <<"/foo">>, Self)),
     ?_assertEqual({ok, Self},
                   eshetsrv_state:lookup(Pid, event_owner, <<"/foo">>)),

    % listen once
    ?assertEqual(ok,
                  eshetsrv_state:register(Pid, event_listener, <<"/foo">>, Self)),
    ?assertEqual({ok, [Self]},
                  eshetsrv_state:lookup(Pid, event_listeners, <<"/foo">>)),

    % listen again; still listening
    ?assertEqual(ok,
                  eshetsrv_state:register(Pid, event_listener, <<"/foo">>, Self)),
    ?assertEqual({ok, [Self]},
                  eshetsrv_state:lookup(Pid, event_listeners, <<"/foo">>)),

    % listen from another process
    Other = spawn_link(fun() ->
                               receive
                                   exit -> exit
                               end
                       end),
    ?assertEqual(ok,
                  eshetsrv_state:register(Pid, event_listener, <<"/foo">>, Other)),
    {ok, Listeners} = eshetsrv_state:lookup(Pid, event_listeners, <<"/foo">>),
    ?assertEqual(lists:sort([Self, Other]), lists:sort(Listeners)),

    Other ! exit.

register_listener_first(Pid) ->
    Self = self(),
    [
     ?_assertEqual(ok,
                   eshetsrv_state:register(Pid, event_listener, <<"/foo">>, Self)),
     ?_assertEqual({ok, [Self]},
                   eshetsrv_state:lookup(Pid, event_listeners, <<"/foo">>)),
     ?_assertEqual({error, no_owner},
                   eshetsrv_state:lookup(Pid, event_owner, <<"/foo">>)),
     ?_assertEqual(ok,
                   eshetsrv_state:register(Pid, event_owner, <<"/foo">>, Self)),
     ?_assertEqual({ok, Self},
                   eshetsrv_state:lookup(Pid, event_owner, <<"/foo">>))
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
                eshetsrv_state:register(Pid, action_owner, <<"/foo">>, self())
        end,
    {_Pid, MonitorRef} = spawn_monitor(F),
    receive
        {_Tag, MonitorRef, _Type, _Object, _Info} -> 
            ?_assertEqual([], eshetsrv_state:lookup(Pid, action_owner, <<"/foo">>))
    end.


register_check_exit(Pid) ->
    F = fun () ->
                eshetsrv_state:register(Pid, action_owner, <<"/foo">>, self()),
                ?assertEqual([self()], eshetsrv_state:lookup(Pid, action_owner, <<"/foo">>))
        end,
    {_Pid, MonitorRef} = spawn_monitor(F),
    receive
        {_Tag, MonitorRef, _Type, _Object, _Info} -> 
            ?_assertEqual([], eshetsrv_state:lookup(Pid, action_owner, <<"/foo">>))
    end.
