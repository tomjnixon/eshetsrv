-module(eshetnet_registry).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([register/1]).
-export([proxy/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          server,
          pid_to_id=#{}
}).

%% API.

start_link(Srv) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Srv], []).

register(Id) ->
    gen_server:call(?MODULE, {register, Id}).

proxy(Function, Args) ->
    % gen_server:call(?MODULE, {proxy, Function, Args}).
    case gen_server:call(?MODULE, check_registered) of
        true ->
            erlang:apply(eshet, Function, Args);
        false ->
            {error, unregistered}
    end.

%% gen_server.

init([Srv]) ->
    {ok, #state{server=Srv}}.

handle_call({register, Id}, {Pid, _Tag}, State=#state{server=Srv, pid_to_id=PidToId}) ->
    NewPidToId = case lookup_value(PidToId, Id) of
                     [OtherPid] ->
                         ok = try gen_server:stop(OtherPid) catch exit:noproc -> ok end,
                         ok = gen_server:call(Srv, {deregister, OtherPid}),
                         maps:remove(OtherPid, PidToId);
                     [] ->
                         PidToId
                 end,
    {reply, ok, State#state{pid_to_id=NewPidToId#{Pid => Id}}};

handle_call(check_registered, {Pid, _Tag}, State=#state{pid_to_id=PidToId}) ->
    case PidToId of
        #{Pid := _Id} ->
            {reply, true, State};
        #{} ->
            {reply, false, State}
    end;

handle_call({proxy, Function, Args}, {Pid, _Tag}, State=#state{pid_to_id=PidToId}) ->
    case PidToId of
        #{Pid := _Id} ->
            Result = erlang:apply(eshet, Function, Args),
            {reply, Result, State};
        #{} ->
            {reply, {error, unregistered}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal

lookup_value(Map, Value) ->
    [Key || {Key, KeyValue} <- maps:to_list(Map), KeyValue =:= Value].
