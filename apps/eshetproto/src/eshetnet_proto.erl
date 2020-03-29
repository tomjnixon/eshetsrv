-module(eshetnet_proto).
-behaviour(ranch_protocol).
-behaviour(gen_server).

-export([start_link/4]).
-export([init/4]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-type opts() :: [any()].
-export_type([opts/0]).

-record(state, {
    socket :: inet:socket(),
    transport :: module(),
    server,
    data=(<<>>),
    next_id=0,
    wait=#{}
}).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

-spec init(ranch:ref(), inet:socket(), module(), opts()) -> ok.
init(Ref, Socket, Transport, [Server]) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {keepalive, true}]),
	gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport, server=Server}).

init(State) ->
    {ok, State}.

handle_call({action_call, Path, Args}, From, State) ->
    {Id, State1} = wait_reply(State, From),
    ok = send_message({action_call, Id, Path, Args}, State),
    {noreply, State1};

handle_call({prop_get, Path}, From, State) ->
    {Id, State1} = wait_reply(State, From),
    ok = send_message({prop_get, Id, Path}, State),
    {noreply, State1};

handle_call({prop_set, Path, Value}, From, State) ->
    {Id, State1} = wait_reply(State, From),
    ok = send_message({prop_set, Id, Path, Value}, State),
    {noreply, State1};

handle_call(_Request, _From, State=#state{server=_Srv}) ->
    {reply, ignored, State}.

handle_cast({send, Message}, State) ->
    send_message(Message, State),
    {noreply, State};

handle_cast({event_notify, Path, Value}, State) ->
    ok = send_message({event_notify, Path, Value}, State),
    {noreply, State};

handle_cast({state_changed, Path, Value}, State) ->
    ok = send_message({state_changed, Path, Value}, State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, NewData},
            State=#state{socket=Socket, transport=Transport, data=OldData}) ->
    Data = <<OldData/binary, NewData/binary>>,
    {Messages, Rest} = eshetnet_proto_impl:parse(Data),
    {ok, NewState} = handle_messages(Messages, State#state{data=Rest}),
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, NewState};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% impl

wait_reply(State=#state{next_id=Id, wait=Wait}, From) ->
    {Id,
     State#state{next_id=(Id + 1) band 16#ffff,
                 wait=Wait#{Id => From}}}.

% replies with Result, which might be ok, {ok, Value} or {error, Value}.
reply(Result, Id, State) ->
    ok = case Result of
             ok -> send_message({reply, Id, {ok, null}}, State);
             {ok, Value} -> send_message({reply, Id, {ok, Value}}, State);
             {error, Value} -> send_message({reply, Id, {error, Value}}, State)
         end,
    {ok, State}.

% replies with Result, which might be {ok, {known, Value}}, {ok, unknown} or
% {error, Value}.
reply_state(Result, Id, State) ->
    ok = case Result of
             {ok, Value} -> send_message({reply_state, Id, {ok, Value}}, State);
             {error, Value} -> send_message({reply, Id, {error, Value}}, State)
         end,
    {ok, State}.

send_message(Message, #state{socket=Socket, transport=Transport}) ->
    {ok, Packed} = eshetnet_proto_impl:pack(Message),
    Transport:send(Socket, Packed).


handle_messages([Message | Messages], State) ->
    {ok, NewState} = handle_message(Message, State),
    handle_messages(Messages, NewState);
handle_messages([], State) ->
    {ok, State}.

handle_message({hello, 1}, State) ->
    Id = erlang:unique_integer(),
    eshetnet_registry:register(Id),
    ok = send_message({hello_id, Id}, State),
    {ok, State};

handle_message({hello_id, 1, Id}, State) ->
    eshetnet_registry:register(Id),
    ok = send_message({hello}, State),
    {ok, State};

handle_message({ping, Id}, State) ->
    ok = send_message({reply, Id, {ok, null}}, State),
    {ok, State};

handle_message({reply, Id, Response}, State=#state{wait=Wait}) ->
    {From, NewWait} = maps:take(Id, Wait),
    gen_server:reply(From, Response),
    {ok, State#state{wait=NewWait}};

handle_message({action_register, Id, Path}, State=#state{server=Srv}) ->
    reply(eshetnet_registry:proxy(action_register, [Srv, Path]), Id, State);

handle_message({action_call, Id, Path, Msg}, State=#state{server=Srv}) ->
    Self = self(),
    spawn_link(fun () ->
                       Result = eshet:action_call(Srv, Path, Msg),
                       gen_server:cast(Self, {send, {reply, Id, Result}})
               end),
    {ok, State};

handle_message({prop_register, Id, Path}, State=#state{server=Srv}) ->
    reply(eshet:prop_register(Srv, Path), Id, State);

handle_message({get, Id, Path}, State=#state{server=Srv}) ->
    Self = self(),
    spawn_link(fun () ->
                       Result = eshet:get(Srv, Path),
                       gen_server:cast(Self, {send, {reply, Id, Result}})
               end),
    {ok, State};

handle_message({set, Id, Path, Msg}, State=#state{server=Srv}) ->
    Self = self(),
    spawn_link(fun () ->
                       Result = eshet:set(Srv, Path, Msg),
                       gen_server:cast(Self, {send, {reply, Id, Result}})
               end),
    {ok, State};

handle_message({event_register, Id, Path}, State=#state{server=Srv}) ->
    reply(eshetnet_registry:proxy(event_register, [Srv, Path]), Id, State);

handle_message({event_emit, Id, Path, Value}, State=#state{server=Srv}) ->
    reply(eshet:event_emit(Srv, Path, Value), Id, State);

handle_message({event_listen, Id, Path}, State=#state{server=Srv}) ->
    reply(eshet:event_listen(Srv, Path), Id, State);

handle_message({state_register, Id, Path}, State=#state{server=Srv}) ->
    reply(eshetnet_registry:proxy(state_register, [Srv, Path]), Id, State);

handle_message({state_changed, Id, Path, Value}, State=#state{server=Srv}) ->
    reply(eshet:state_changed(Srv, Path, Value), Id, State);

handle_message({state_unknown, Id, Path}, State=#state{server=Srv}) ->
    reply(eshet:state_unknown(Srv, Path), Id, State);

handle_message({state_observe, Id, Path}, State=#state{server=Srv}) ->
    reply_state(eshet:state_observe(Srv, Path), Id, State).
