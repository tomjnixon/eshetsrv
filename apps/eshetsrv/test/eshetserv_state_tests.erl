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
    ?assertEqual({ok, Self},
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

state_test_() ->
    [
     {"register state",
      ?setup({with, [fun register_state/1]})},
     {"observe before register state",
      ?setup({with, [fun observe_before_register_state/1]})}
    ].

register_state(Pid) ->
    Self = self(),
    ?assertEqual(ok,
                 eshetsrv_state:register(Pid, state_owner, <<"/foo">>, Self)),

    ?assertEqual({ok, unknown},
                 eshetsrv_state:register(Pid, state_observer, <<"/foo">>, Self)),

    ?assertEqual(ok,
                 eshetsrv_state:state_changed(Pid, <<"/foo">>, Self, some_state)),
    ok = receive
             {'$gen_cast', {state_changed, <<"/foo">>, {known, some_state}}} -> ok
         after
             100 -> timeout
         end.

observe_before_register_state(Pid) ->
    Self = self(),
    ?assertEqual({ok, unknown},
                 eshetsrv_state:register(Pid, state_observer, <<"/foo">>, Self)),

    ?assertEqual(ok,
                 eshetsrv_state:register(Pid, state_owner, <<"/foo">>, Self)),

    ?assertEqual(ok,
                 eshetsrv_state:state_changed(Pid, <<"/foo">>, Self, some_state)),
    ok = receive
        {'$gen_cast', {state_changed, <<"/foo">>, {known, some_state}}} ->
            ok
    after
        100 -> timeout
    end.



link_test_() ->
    [
     {"register then exit immediately",
      ?setup({with, [fun register_exit/1]})},
     {"register, check, then exit",
      ?setup({with, [fun register_check_exit/1]})},
     {"state",
      ?setup({with, [fun link_state/1]})}
    ].

register_exit(Pid) ->
    F = fun () ->
                eshetsrv_state:register(Pid, action_owner, <<"/foo">>, self())
        end,
    {_Pid, MonitorRef} = spawn_monitor(F),
    receive
        {_Tag, MonitorRef, _Type, _Object, _Info} -> 
            timer:sleep(50),
            ?_assertEqual({error,path_not_found}, eshetsrv_state:lookup(Pid, action_owner, <<"/foo">>))
    end.


register_check_exit(Pid) ->
    F = fun () ->
                eshetsrv_state:register(Pid, action_owner, <<"/foo">>, self()),
                ?assertEqual({ok, self()}, eshetsrv_state:lookup(Pid, action_owner, <<"/foo">>))
        end,
    {_Pid, MonitorRef} = spawn_monitor(F),
    receive
        {_Tag, MonitorRef, _Type, _Object, _Info} -> 
            timer:sleep(50),
            ?_assertEqual({error,path_not_found}, eshetsrv_state:lookup(Pid, action_owner, <<"/foo">>))
    end.


link_state(Pid) ->
    Self = self(),
    F = fun () ->
                eshetsrv_state:register(Pid, state_owner, <<"/foo">>, self()),
                eshetsrv_state:state_changed(Pid, <<"/foo">>, self(), some_state),
                Self ! setup_done,
                receive
                    exit -> exit
                end

        end,
    {TestPid, MonitorRef} = spawn_monitor(F),

    ok = receive
             setup_done -> ok
         after
             100 -> timeout
         end,

    ?assertEqual({ok, {known, some_state}},
                 eshetsrv_state:register(Pid, state_observer, <<"/foo">>, self())),

    TestPid ! exit,

    ok = receive
             {_Tag, MonitorRef, _Type, _Object, _Info} ->
                 receive
                     {'$gen_cast', {state_changed, _P, unknown}} -> ok
                 after
                     100 -> state_changed_timeout
                 end
         after
             100 -> monitor_timeout
         end.
