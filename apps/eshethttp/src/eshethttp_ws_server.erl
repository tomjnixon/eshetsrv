-module(eshethttp_ws_server).

% used by eshethttp_ws to interact with the rest of the system using gen_server
% messages

-behaviour(gen_server).

%% API.
-export([start_link/3]).
%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {ws, gen_state}).

start_link(WS, Srv, Registry) ->
    gen_server:start_link(?MODULE, [WS, Srv, Registry], []).

init([WS, Srv, Registry]) ->
    {ok, State} = eshetproto_generic:init(Srv, Registry),
    {ok, #state{ws = WS, gen_state = State}}.

handle_call(Request, From, State = #state{gen_state = GState}) ->
    reply(eshetproto_generic:handle_call(Request, From, GState), State).

handle_cast({messages, Messages}, State = #state{gen_state = GState}) ->
    reply(eshetproto_generic:handle_messages(Messages, GState), State);
handle_cast(Msg, State = #state{gen_state = GState}) ->
    reply(eshetproto_generic:handle_cast(Msg, GState), State).

handle_info(Msg, State = #state{gen_state = GState}) ->
    reply(eshetproto_generic:handle_info(Msg, GState), State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% impl

reply({ok, Messages, NewState}, State = #state{ws = WS}) ->
    WS ! {send, Messages},
    {noreply, State#state{gen_state = NewState}};
reply(stop, State) ->

    {stop, timeout, State}.
