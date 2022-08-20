-module(eshetproto_generic).

-export([init/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([handle_messages/2]).

-record(state, {server, registry, next_id = 0, wait = #{}, timeout_s = 0, timeout_ref = none}).

init(Server, Registry) ->
    {ok, #state{server = Server, registry = Registry}}.

handle_call({action_call, Path, Args}, From, State) ->
    {Id, State1} = wait_reply(State, From),
    Msg = {action_call, Id, Path, Args},
    {ok, [Msg], State1};
handle_call({prop_get, Path}, From, State) ->
    {Id, State1} = wait_reply(State, From),
    Msg = {prop_get, Id, Path},
    {ok, [Msg], State1};
handle_call({prop_set, Path, Value}, From, State) ->
    {Id, State1} = wait_reply(State, From),
    Msg = {prop_set, Id, Path, Value},
    {ok, [Msg], State1};
handle_call(_Request, _From, _State) ->
    ignored.

handle_cast({send, Message}, State) ->
    {ok, [Message], State};
handle_cast({event_notify, Path, Value}, State) ->
    {ok, [{event_notify, Path, Value}], State};
handle_cast({state_changed, Path, Value}, State) ->
    {ok, [{state_changed, Path, Value}], State};
handle_cast(_Msg, _State) ->
    ignored.

handle_info(timeout, _State) ->
    stop;
handle_info(_Msg, _State) ->
    ignored.

handle_messages(Messages, State) ->
    {ok, Responses, State1} = do_handle_messages(Messages, State),
    % messages might have changed the idle timeout; resetting after handling
    % messages makes this take effect
    {ok, Responses, reset_timeout(State1)}.

do_handle_messages([Message | Messages], State) ->
    {ok, FirstResponses, State1} = handle_message(Message, State),
    {ok, RestResponses, State2} = do_handle_messages(Messages, State1),
    {ok, FirstResponses ++ RestResponses, State2};
do_handle_messages([], State) ->
    {ok, [], State}.

% impl

reset_timeout(State = #state{timeout_ref = TRef, timeout_s = TimeoutS}) ->
    _ =
        case TRef of
            none ->
                ok;
            _ ->
                erlang:cancel_timer(TRef)
        end,
    NewTRef =
        case TimeoutS of
            0 ->
                none;
            _ ->
                erlang:send_after(1000 * TimeoutS, self(), timeout)
        end,
    State#state{timeout_ref = NewTRef}.

handle_message({hello, 1, TimeoutS}, State = #state{registry = Registry}) ->
    Id = erlang:unique_integer(),
    eshetnet_registry:register(Registry, Id),
    {ok, [{hello_id, Id}], State#state{timeout_s = TimeoutS}};
handle_message({hello_id, 1, TimeoutS, Id}, State = #state{registry = Registry}) ->
    eshetnet_registry:register(Registry, Id),
    {ok, [{hello}], State#state{timeout_s = TimeoutS}};
handle_message({ping, Id}, State) ->
    {ok, [{reply, Id, {ok, null}}], State};
handle_message({reply, Id, Response}, State = #state{wait = Wait}) ->
    {From, NewWait} = maps:take(Id, Wait),
    gen_server:reply(From, Response),
    {ok, [], State#state{wait = NewWait}};
handle_message({action_register, Id, Path}, State = #state{server = Srv, registry = Registry}) ->
    reply(eshetnet_registry:proxy(Registry, action_register, [Srv, Path]), Id, State);
handle_message({action_call, Id, Path, Msg}, State = #state{server = Srv}) ->
    Self = self(),
    spawn_link(fun() ->
        Result = eshet:action_call(Srv, Path, Msg),
        gen_server:cast(Self, {send, {reply, Id, Result}})
    end),
    {ok, [], State};
handle_message({prop_register, Id, Path}, State = #state{server = Srv}) ->
    reply(eshet:prop_register(Srv, Path), Id, State);
handle_message({get, Id, Path}, State = #state{server = Srv}) ->
    Self = self(),
    spawn_link(fun() ->
        Result = eshet:get(Srv, Path),
        gen_server:cast(Self, {send, {reply, Id, Result}})
    end),
    {ok, [], State};
handle_message({set, Id, Path, Msg}, State = #state{server = Srv}) ->
    Self = self(),
    spawn_link(fun() ->
        Result = eshet:set(Srv, Path, Msg),
        gen_server:cast(Self, {send, {reply, Id, Result}})
    end),
    {ok, [], State};
handle_message({event_register, Id, Path}, State = #state{server = Srv, registry = Registry}) ->
    reply(eshetnet_registry:proxy(Registry, event_register, [Srv, Path]), Id, State);
handle_message({event_emit, Id, Path, Value}, State = #state{server = Srv}) ->
    reply(eshet:event_emit(Srv, Path, Value), Id, State);
handle_message({event_listen, Id, Path}, State = #state{server = Srv}) ->
    reply(eshet:event_listen(Srv, Path), Id, State);
handle_message({state_register, Id, Path}, State = #state{server = Srv, registry = Registry}) ->
    reply(eshetnet_registry:proxy(Registry, state_register, [Srv, Path]), Id, State);
handle_message({state_changed, Id, Path, Value}, State = #state{server = Srv}) ->
    reply(eshet:state_changed(Srv, Path, Value), Id, State);
handle_message({state_unknown, Id, Path}, State = #state{server = Srv}) ->
    reply(eshet:state_unknown(Srv, Path), Id, State);
handle_message({state_observe, Id, Path}, State = #state{server = Srv}) ->
    reply_state(eshet:state_observe(Srv, Path), Id, State).

wait_reply(State = #state{next_id = Id, wait = Wait}, From) ->
    {Id, State#state{next_id = (Id + 1) band 16#ffff, wait = Wait#{Id => From}}}.

% replies with Result, which might be ok, {ok, Value} or {error, Value}.
reply(ok, Id, State) ->
    {ok, [{reply, Id, {ok, null}}], State};
reply({ok, Value}, Id, State) ->
    {ok, [{reply, Id, {ok, Value}}], State};
reply({error, Value}, Id, State) ->
    {ok, [{reply, Id, {error, Value}}], State}.

% replies with Result, which might be {ok, {known, Value}}, {ok, unknown} or
% {error, Value}.
reply_state({ok, Value}, Id, State) ->
    {ok, [{reply_state, Id, {ok, Value}}], State};
reply_state({error, Value}, Id, State) ->
    {ok, [{reply, Id, {error, Value}}], State}.
