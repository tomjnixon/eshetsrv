-module(eshet_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
    {ok, Server} = eshetsrv_state:start_link(),
    {ok, Client} = eshet_test_server:start_link(Server),
    {Server, Client}.

stop({Server, Client}) ->
    ok = gen_server:stop(Client),
    ok = gen_server:stop(Server).

eshet_test_() ->
    [
        {"action", ?setup({with, [fun action/1]})},
        {"action_error", fun action_error/0},
        {"prop", ?setup({with, [fun prop/1]})},
        {"event", ?setup({with, [fun event/1]})},
        {"state", ?setup({with, [fun state/1]})},
        {"state_register_before", ?setup({with, [fun state_register_before/1]})},
        {"state_set", ?setup({with, [fun state_set/1]})},
        {"state_errors", ?setup({with, [fun state_errors/1]})}
    ].

action({Server, _Client}) ->
    {ok, action_result} = eshet:action_call(Server, <<"/action">>, []).

action_error() ->
    {ok, Server} = eshetsrv_state:start_link(),
    {ok, Client} = eshet_test_server:start_link(Server),
    unlink(Client),

    % check that if the other client exits, we get a reasonable response
    {error, client_exited} = eshet:action_call(Server, <<"/action_error">>, []),

    ok = gen_server:stop(Server).

prop({Server, _Client}) ->
    ok = eshet:set(Server, <<"/prop">>, prop_value),
    {ok, prop_value} = eshet:get(Server, <<"/prop">>).

event({Server, Client}) ->
    ok = gen_server:call(Client, {set_test_client, self()}),
    ok = eshet:event_register(Server, <<"/event">>),
    ok = eshet:event_emit(Server, <<"/event">>, event_value),
    ok =
        receive
            {event, event_value} -> ok
        after 100 -> timeout
        end.

state({Server, Client}) ->
    ok = gen_server:call(Client, {set_test_client, self()}),

    {ok, unknown} = gen_server:call(Client, observe_state),
    {error, state_unknown} = eshet:get(Server, <<"/state">>),

    ok = eshet:state_register(Server, <<"/state">>),
    {error, state_unknown} = eshet:get(Server, <<"/state">>),

    ok = eshet:state_changed(Server, <<"/state">>, new_state),
    ok =
        receive
            {state, {known, new_state}} -> ok
        after 100 -> timeout
        end,
    {ok, new_state} = eshet:get(Server, <<"/state">>).

state_register_before({Server, Client}) ->
    ok = gen_server:call(Client, {set_test_client, self()}),
    ok = eshet:state_register(Server, <<"/state">>),
    ok = eshet:state_changed(Server, <<"/state">>, start_state),
    {ok, {known, start_state}} = gen_server:call(Client, observe_state),
    ok = eshet:state_changed(Server, <<"/state">>, new_state),
    ok =
        receive
            {state, {known, new_state}} -> ok
        after 100 -> timeout
        end.

state_set({Server, _Client}) ->
    ok = eshet:set(Server, <<"/server_state">>, server_state),
    {ok, server_state} = eshet:get(Server, <<"/server_state">>).

state_errors({Server, _Client}) ->
    % generic errors
    {error, path_should_be_binary} = eshet:state_changed(Server, "/state", new_state),
    {error, invalid_path} = eshet:state_changed(Server, <<"state">>, new_state),

    {error, no_such_node} = eshet:state_changed(Server, <<"/state">>, new_state),

    ok = eshet:event_register(Server, <<"/event">>),
    {error, wrong_type_of_node} = eshet:state_changed(Server, <<"/event">>, new_state),

    {error, not_owner} = eshet:state_changed(Server, <<"/server_state">>, new_state),

    {error, path_already_exists} = eshet:state_register(Server, <<"/server_state">>).
