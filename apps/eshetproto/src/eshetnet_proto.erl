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
    wait=[]
}).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

-spec init(ranch:ref(), inet:socket(), module(), opts()) -> ok.
init(Ref, Socket, Transport, [Server]) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
	gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport, server=Server}).

init(State) ->
    {ok, State}.

handle_call({action_call, Path, Args}, From,
            State=#state{server=_Srv, next_id=Id, wait=Wait}) ->
    ok = send_message({action_call, Id, Path, Args}, State),
    {noreply, State#state{next_id=(Id + 1) band 16#ffff,
                          wait=[{Id, From} | Wait]}};
handle_call(_Request, _From, State=#state{server=_Srv}) ->
    {reply, ignored, State}.

handle_cast({send, Message}, State) ->
    send_message(Message, State),
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

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% impl

send_message(Message, #state{socket=Socket, transport=Transport}) ->
    {ok, Packed} = eshetnet_proto_impl:pack(Message),
    Transport:send(Socket, Packed).


handle_messages([Message | Messages], State) ->
    {ok, NewState} = handle_message(Message, State),
    handle_messages(Messages, NewState);
handle_messages([], State) ->
    {ok, State}.

handle_message({ping, Id}, State) ->
    ok = send_message({reply, Id, {ok, null}}, State),
    {ok, State};

handle_message({reply, Id, Response}, State=#state{wait=Wait}) ->
    {[[{Id, From}]], NewWait} = proplists:split(Wait, [Id]),
    gen_server:reply(From, Response),
    {ok, State#state{wait=NewWait}};

handle_message({action_register, Id, Path}, State=#state{server=Srv}) ->
    case eshet:action_register(Srv, Path) of
        ok ->
            ok = send_message({reply, Id, {ok, null}}, State);
        {error, Error} ->
            ok = send_message({reply, Id, {error, Error}}, State)
    end,
    {ok, State};

handle_message({action_call, Id, Path, Msg}, State=#state{server=Srv}) ->
    Self = self(),
    spawn_link(fun () ->
                       Result = eshet:action_call(Srv, Path, Msg),
                       gen_server:cast(Self, {send, {reply, Id, Result}})
               end),
    {ok, State}.
