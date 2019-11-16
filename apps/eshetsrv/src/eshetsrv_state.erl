-module(eshetsrv_state).
-behaviour(gen_server).

%% API.
-export([start_link/0, start_link/1]).
-export([register/4]).
-export([lookup/3]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          table
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).


-spec start_link(term()) -> {ok, pid()}.
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).


register(Srv, Path, Type, Pid) ->
    gen_server:call(Srv, {register, Path, Type, Pid}).


lookup(Srv, Path, Type) ->
    gen_server:call(Srv, {lookup, Path, Type}).


%% gen_server.

init([]) ->
    process_flag(trap_exit, true),
    Table = ets:new(eshetsrv_state, [named_table, duplicate_bag]),
    {ok, #state{table=Table}}.


handle_call({register, Path, Type, Pid}, _From, State=#state{table=Table})
  when is_binary(Path), is_pid(Pid) ->
    case eshet:path_valid(Path) of
        false ->
            {reply, {error, invalid_path}, State};
        true ->
            Parts = eshet:path_split(Path),
            Existing = ets:select(Table, [{{Parts, '$1', '_'}, [], ['$1']}]),
            case check_existing(Type, Existing) of
                ok ->
                    ets:insert(Table, {Parts, Type, Pid}),
                    link(Pid),
                    {reply, ok, State};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end
    end;

handle_call({lookup, Path, Type}, _From, State=#state{table=Table}) ->
    case eshet:path_valid(Path) of
        true ->
            PathParts = eshet:path_split(Path),
            Nodes = ets:select(Table, [{{PathParts, Type, '$1'}, [], ['$1']}]),
            {reply, Nodes, State};
        false ->
            {reply, {error, invalid_path}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    {noreply, deregister(Pid, State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal

deregister(Pid, State=#state{table=Table}) ->
    unlink(Pid),
    ets:match_delete(Table, {'_', '_', Pid}),
    State.


check_existing(action_owner, []) -> ok;
check_existing(action_owner, _) -> {error, already_registered};

check_existing(prop_owner, []) -> ok;
check_existing(prop_owner, _) -> {error, already_registered};

check_existing(event_owner, L) ->
    case [T || T <- L, T =/= event_listener] of
        [] -> ok;
        _ -> {error, already_registered}
    end;

check_existing(event_listener, L) ->
    case [T || T <- L, T =/= event_listener] of
        [] -> ok;
        [event_owner] -> ok;
        _ -> {error, invalid_type_for_action}
    end;

check_existing(_, _) ->
    {error, invalid_type}.
