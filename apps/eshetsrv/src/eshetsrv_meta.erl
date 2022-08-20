-module(eshetsrv_meta).
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
    server
}).

%% API.

-spec start_link(term()) -> {ok, pid()}.
start_link(Srv) ->
    gen_server:start_link(?MODULE, [Srv], []).

%% gen_server.

init([Srv]) ->
    eshet:action_register(Srv, <<"/meta/ls">>),
    {ok, #state{server = Srv}}.

handle_call({action_call, <<"/meta/ls">>, [Path]}, _From, State = #state{server = Srv}) when
    is_binary(Path)
->
    Res = eshetsrv_state:lookup(Srv, node, Path),
    {reply, Res, State};
handle_call({action_call, _, _}, _From, State) ->
    {reply, {error, bad_args}, State};
handle_call(_Request, _From, State = #state{server = _Srv}) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
