-module(eshethttp_ws_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([check_call/1]).
-export([check_observe/1]).
-export([check_observe_time/1]).

init_per_suite(Config) ->
    application:set_env(eshethttp, port, 11238),
    {ok, _} = application:ensure_all_started(eshetsrv),
    {ok, _} = application:ensure_all_started(eshethttp),
    {ok, _} = application:ensure_all_started(gun),
    {ok, Client} = gen_server:start(eshet_test_server, [eshetsrv_state], []),
    [{client, Client} | Config].

end_per_suite(Config) ->
    ok = gen_server:stop(proplists:get_value(client, Config)),
    application:stop(eshethttp),
    application:stop(gun),
    ok.

all() ->
    [check_call, check_observe, check_observe_time].

connect() ->
    {ok, Pid} = gun:open("127.0.0.1", 11238, #{retry => 0}),
    {ok, http} = gun:await_up(Pid),
    MRef = monitor(process, Pid),
    StreamRef = gun:ws_upgrade(Pid, "/ws", [], #{compress => true}),
    receive
        {gun_upgrade, Pid, StreamRef, [<<"websocket">>], _} ->
            ok;
        Msg ->
            ct:pal("Unexpected message ~p", [Msg]),
            error(failed)
    end,
    {Pid, MRef, StreamRef}.

send_json(Pid, StreamRef, Json) ->
    gun:ws_send(Pid, StreamRef, {text, jiffy:encode(Json)}).

recv_json(Pid, StreamRef) ->
    receive
        {gun_ws, Pid, StreamRef, {text, Text}} ->
            jiffy:decode(Text, [return_maps]);
        Msg ->
            ct:pal("Unexpected message ~p", [Msg]),
            error(failed)
    after 1000 ->
        error(timeout)
    end.

check_call(_Config) ->
    {Pid, _MRef, StreamRef} = connect(),
    ok = send_json(Pid, StreamRef, [<<"hello">>, 1, 30]),
    [<<"hello_id">>, _Id] = recv_json(Pid, StreamRef),

    ok = send_json(Pid, StreamRef, [<<"action_call">>, 0, <<"/action">>, []]),
    [<<"reply">>, 0, [<<"ok">>, <<"action_result">>]] = recv_json(Pid, StreamRef),

    ok = gun:close(Pid).

check_observe(_Config) ->
    {Pid, _MRef, StreamRef} = connect(),
    ok = send_json(Pid, StreamRef, [<<"hello">>, 1, 30]),
    [<<"hello_id">>, _Id] = recv_json(Pid, StreamRef),

    ok = send_json(Pid, StreamRef, [<<"state_observe">>, 1, <<"/server_state">>]),
    [<<"reply_state">>, 1, [<<"ok">>, <<"unknown">>]] = recv_json(Pid, StreamRef),

    ok = eshet:set(eshetsrv_state, <<"/server_state">>, <<"foo">>),
    [<<"state_changed">>, <<"/server_state">>, [<<"known">>, <<"foo">>]] = recv_json(
        Pid, StreamRef
    ),

    ok = gun:close(Pid).

check_observe_time(_Config) ->
    {Pid, _MRef, StreamRef} = connect(),
    ok = send_json(Pid, StreamRef, [<<"hello">>, 1, 30]),
    [<<"hello_id">>, _Id] = recv_json(Pid, StreamRef),

    ok = eshet:set(eshetsrv_state, <<"/server_state">>, <<"foo">>),

    timer:sleep(200),

    ok = send_json(Pid, StreamRef, [<<"state_observe_t">>, 1, <<"/server_state">>]),
    [<<"reply_state">>, 1, [<<"ok">>, [<<"known">>, <<"foo">>], T]] = recv_json(Pid, StreamRef),

    true = erlang:abs(T - 0.2) < 0.05,

    ok = gun:close(Pid).
