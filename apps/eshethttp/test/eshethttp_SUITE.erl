-module(eshethttp_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([check_call/1, check_get/1, check_set/1]).

init_per_suite(Config) ->
    application:set_env(eshethttp, port, 11238),
    {ok, _} = application:ensure_all_started(eshetsrv),
    {ok, _} = application:ensure_all_started(eshethttp),
    {ok, Client} = gen_server:start(eshet_test_server, [eshetsrv_state], []),
    [{client, Client} | Config].

end_per_suite(Config) ->
    ok = gen_server:stop(proplists:get_value(client, Config)),
    application:stop(eshethttp),
    ok.

all() ->
    [check_call, check_get, check_set].

check_call(_Config) ->
    <<"action_result">> = http_post("http://localhost:11238/call/action", <<"[]">>),
    ok.

check_get(_Config) ->
    eshet:set(eshetsrv_state, <<"/server_state">>, 5),
    5 = http_get("http://localhost:11238/get/server_state"),
    ok.

check_set(_Config) ->
    null = http_post("http://localhost:11238/set/server_state", <<"7">>),
    {ok, 7} = eshet:get(eshetsrv_state, <<"/server_state">>),
    ok.

% utils

http_post(Url, JSON) ->
    {ok, Resp} = httpc:request(post, {Url, [], "application/json", JSON}, [], []),
    {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Resp,
    jiffy:decode(Body).

http_get(Url) ->
    {ok, Resp} = httpc:request(get, {Url, []}, [], []),
    {{_Version, 200, _ReasonPhrase}, _Headers, Body} = Resp,
    jiffy:decode(Body).

