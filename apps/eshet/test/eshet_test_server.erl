-module(eshet_test_server).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          server,
          test_client=null,
          prop_value=null
         }).

%% API.

-spec start_link(pid()) -> {ok, pid()}.
start_link(Server) ->
    gen_server:start_link(?MODULE, [Server], []).

%% gen_server.

init([Server]) ->
    ok = eshet:action_register(Server, <<"/action">>),
    ok = eshet:prop_register(Server, <<"/prop">>),
    ok = eshet:event_listen(Server, <<"/event">>),
    ok = eshet:state_register(Server, <<"/server_state">>),
    {ok, #state{server=Server}}.

handle_call({set_test_client, Client}, _From, State) ->
    {reply, ok, State#state{test_client=Client}};

handle_call({action_call, <<"/action">>, []}, _From, State) ->
    {reply, {ok, action_result}, State};

handle_call({prop_set, <<"/prop">>, NewValue}, _From, State) ->
    {reply, ok, State#state{prop_value=NewValue}};

handle_call({prop_get, <<"/prop">>}, _From, State=#state{prop_value=Value}) ->
    {reply, {ok, Value}, State};

handle_call(observe_state, _From, State=#state{server=Server}) ->
    Ret = eshet:state_observe(Server, <<"/state">>),
    {reply, Ret, State};

handle_call({state_set, <<"/server_state">>, NewState},
            _From, State=#state{server=Server}) ->
    ok = eshet:state_changed(Server, <<"/server_state">>, NewState),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignored_in_test_server, State}.

handle_cast({event_notify, <<"/event">>, Value}, State=#state{test_client=Client}) ->
    Client ! {event, Value},
    {noreply, State};

handle_cast({state_changed, <<"/state">>, Value}, State=#state{test_client=Client}) ->
    Client ! {state, Value},
    {noreply, State};

handle_cast(Msg, State) ->
    erlang:display({message, Msg}),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
