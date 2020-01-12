-module(eshetsrv_state).
-behaviour(gen_server).

%% API.
-export([start_link/0, start_link/1]).
-export([register/4]).
-export([lookup/3]).
-export([state_changed/4]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          tree
         }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).


-spec start_link(term()) -> {ok, pid()}.
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).


register(Srv, Type, Path, Pid) ->
    gen_server:call(Srv, {register, Type, Path, Pid}).


lookup(Srv, Type, Path) ->
    gen_server:call(Srv, {lookup, Type, Path}).

state_changed(Srv, Path, Pid, NewState) ->
    gen_server:call(Srv, {state_changed, Path, Pid, NewState}).

%% gen_server.

init([]) ->
    process_flag(trap_exit, true),
    Tree = eshetsrv_tree:new(),
    {ok, #state{tree=Tree}}.


% actions

handle_call({register, action_owner, Path, Pid}, _From, State=#state{tree=Tree}) ->
    case check(Path, Pid) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            monitor(process, Pid),
            to_reply(State,
                     eshetsrv_tree:insert(Tree, Parts, #{type => action,
                                                         owner => Pid}))
    end;
handle_call({lookup, action_owner, Path}, _From, State=#state{tree=Tree}) ->
    case check(Path) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            case lookup_node_type(Tree, Parts, action) of
                {leaf, #{owner := Pid}} -> {reply, {ok, Pid}, State};
                {error, E} -> {reply, {error, E}, State}
            end
    end;

% props

handle_call({register, prop_owner, Path, Pid}, _From, State=#state{tree=Tree}) ->
    case check(Path, Pid) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            monitor(process, Pid),
            to_reply(State,
                     eshetsrv_tree:insert(Tree, Parts, #{type => prop,
                                                         owner => Pid}))
    end;
handle_call({lookup, prop_owner, Path}, _From, State=#state{tree=Tree}) ->
    case check(Path) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            case lookup_node_type(Tree, Parts, prop) of
                {leaf, #{owner := Pid}} -> {reply, {ok, Pid}, State};
                {error, E} -> {reply, {error, E}, State}
            end
    end;

% events

handle_call({register, event_owner, Path, Pid}, _From, State) ->
    case check(Path, Pid) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            monitor(process, Pid),
            update_generic(State, Parts, event, #{owner => Pid, listeners => sets:new()},
                           fun (L =  #{owner := none}) ->
                                   {leaf, L#{owner => Pid}};
                               (#{owner := _}) ->
                                   {error, path_already_exists}
                           end)
    end;
handle_call({register, event_listener, Path, Pid}, _From, State) ->
    case check(Path, Pid) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            monitor(process, Pid),
            update_generic(State, Parts, event, #{owner => none, listeners => sets:from_list([Pid])},
                           fun (L =  #{listeners := Listeners}) ->
                                   {leaf, L#{listeners := sets:add_element(Pid, Listeners)}}
                           end)
    end;
handle_call({lookup, event_owner, Path}, _From, State=#state{tree=Tree}) ->
    case check(Path) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            case lookup_node_type(Tree, Parts, event) of
                {leaf, #{owner := none}} -> {reply, {error, no_owner}, State};
                {leaf, #{owner := Pid}} -> {reply, {ok, Pid}, State};
                {error, E} -> {reply, {error, E}, State}
            end
    end;
handle_call({lookup, event_listeners, Path}, _From, State=#state{tree=Tree}) ->
    case check(Path) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            case lookup_node_type(Tree, Parts, event) of
                {leaf, #{listeners := Listeners}} -> {reply, {ok, sets:to_list(Listeners)}, State};
                {error, E} -> {reply, {error, E}, State}
            end
    end;

% states

% state can change like:
%     start(register observer) -> #{owner => none, state => unknown}
%     start(register owner) -> #{owner => pid(), state => unknown}
%     update: #{owner => pid(), state => unknown} -> #{owner => pid(), state => {known, any()}}
%     remove: #{owner => pid(), state => unknown} ->#{owner => none, state => unknown}
%           : #{owner => pid(), state => {known, any()} ->#{owner => none, state => unknown}
%
% therefore the possible states are:
%     #{owner => none, state => unknown}
%     #{owner => pid(), state => unknown}
%     #{owner => pid(), state => {known, any()}}

handle_call({register, state_owner, Path, Pid}, _From, State) ->
    case check(Path, Pid) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            monitor(process, Pid),
            update_generic(State, Parts, state, #{owner => Pid, observers => sets:new(), state => unknown},
                           fun (L = #{owner := none, state := unknown}) ->
                                   % still unknown, no need to update
                                   {leaf, L#{owner => Pid}};
                               (#{owner := _}) ->
                                   {error, path_already_exists}
                           end)
    end;

handle_call({register, state_observer, Path, Pid}, _From, State) ->
    case check(Path, Pid) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            monitor(process, Pid),
            update_generic(
              State, Parts, state,
              {#{owner => none,
                observers => sets:from_list([Pid]),
                state => unknown},
               unknown},
              fun (L=#{type := state, observers := Observers, state := S}) ->
                      {leaf, L#{observers => sets:add_element(Pid, Observers)}, S}
              end)
    end;

handle_call({state_changed, Path, Pid, NewState}, _From, State=#state{tree=Tree}) ->
    case check(Path, Pid) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            to_reply(
              State,
              eshetsrv_tree:update(
                Tree, Parts,
                fun (nothing) -> {error, no_such_node};
                    ({leaf, L=#{type := state}}) ->
                        case L of
                            #{owner := Pid, observers := Observers} ->
                                ok = send_state_changed(Observers, Path, NewState),
                                {leaf, L#{state => NewState}};
                            #{owner := _} ->
                                {error, not_owner}
                        end;
                    ({leaf, #{type := _}}) ->
                        {error, wrong_type_of_node}
                end))
    end;

handle_call({state_get, Path}, _From, State=#state{tree=Tree}) ->
    case check(Path) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            case lookup_node_type(Tree, Parts, state) of
                {leaf, #{state := S}} -> {reply, {ok, S}, State};
                {error, E} -> {reply, {error, E}, State}
            end
    end;

% props and states

handle_call({lookup, prop_state_owner, Path}, _From, State=#state{tree=Tree}) ->
    case check(Path) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            case lookup_node(Tree, Parts) of
                {leaf, #{type := prop, owner := Pid}} -> {reply, {ok, prop, Pid}, State};
                {leaf, #{type := state, owner := Pid}} -> {reply, {ok, state, Pid}, State};
                {leaf, #{type := _}} -> {reply, {error, path_is_wrong_type}, State};
                {error, E} -> {reply, {error, E}, State}
            end
    end;

% for ls

handle_call({lookup, node, Path}, _From, State=#state{tree=Tree}) ->
    case eshet:path_or_dir_valid(Path) of
        false -> {reply, {error, invalid_path}, State};
        true ->
            Parts = eshet:path_split(Path),
            Reply = case eshetsrv_tree:lookup(Tree, Parts) of
                {leaf, #{type := Type}} ->
                    {ok, Type};
                directory ->
                    {ok, Entries} = eshetsrv_tree:list_dir(Tree, Parts),
                    EntriesFmt = [{Name, case Entry of
                                             dir -> dir;
                                             {leaf, #{type := Type}} -> Type
                                         end}
                                  || {Name, Entry} <- Entries],
                    {ok, {dir, EntriesFmt}};
                nothing -> {error, path_not_found};
                {error, E} -> {error, E}
            end,
            {reply, Reply, State}
    end;

handle_call({deregister, Pid}, _From, State) ->
    {reply, ok, deregister(Pid, State)};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, _Type, Pid, _Reason}, State) ->
    {noreply, deregister(Pid, State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal

deregister_from_node(_Path, #{type := action, owner := Pid}, Pid) -> nothing;
deregister_from_node(_Path, V=#{type := action, owner := _}, _) -> {leaf, V};
deregister_from_node(_Path, #{type := prop, owner := Pid}, Pid) -> nothing;
deregister_from_node(_Path, V=#{type := prop, owner := _}, _) -> {leaf, V};

deregister_from_node(_Path, V=#{type := event, owner := Owner, listeners := Listeners}, Pid) ->
    NewOwner = case Owner of
                    Pid -> none;
                    _ -> Owner
                end,
    NewListeners = sets:del_element(Pid, Listeners),

    case {NewOwner, sets:is_empty(NewListeners)} of
        {none, true} -> nothing;
        _ -> {leaf, V#{owner => NewOwner, listeners => NewListeners}}
    end;

deregister_from_node(Path, V=#{type := state,
                               owner := Owner,
                               state := S,
                               observers := Observers}, Pid) ->
    {NewOwner, NewState} = case {Owner, S} of
                               {Pid, {known, _}} ->
                                   ok = send_state_changed(Observers, eshet:path_unsplit(Path), unknown),
                                   {none, unknown};
                               {Pid, unknown} ->
                                   % already unknown, no message
                                   {none, unknown};
                               {_, _} ->
                                   {Owner, S}
                           end,

    NewObservers = sets:del_element(Pid, Observers),

    case {NewOwner, sets:is_empty(NewObservers)} of
        {none, true} -> nothing;
        _ -> {leaf, V#{owner => NewOwner, observers => NewObservers, state => NewState}}
    end.


deregister(Pid, State=#state{tree=Tree}) ->
    NewTree = eshetsrv_tree:map(Tree,
                                fun (Path, Value) ->
                                        deregister_from_node(Path, Value, Pid)
                                end),
    State#state{tree=NewTree}.

check(Path) when is_binary(Path) ->
    case eshet:path_valid(Path) of
        true ->
            {ok,  eshet:path_split(Path)};
        false ->
            {error, invalid_path}
    end;
check(_Path) ->
    {error, path_should_be_binary}.

check(Path, Pid) when is_pid(Pid) ->
    check(Path);
check(_Path, _Pid) ->
    {error, path_should_be_binary}.

to_reply(State, {error, E}) ->
    {reply, {error, E}, State};
to_reply(State, {ok, NewTree}) ->
    {reply, ok, State#state{tree=NewTree}};
to_reply(State, {ok, NewTree, Ret}) ->
    {reply, {ok, Ret}, State#state{tree=NewTree}}.

update_generic(State=#state{tree=Tree}, Parts, Type, Initial, Existing) ->
    to_reply(State,
             eshetsrv_tree:update(Tree, Parts, fun (nothing) ->
                                                       case Initial of
                                                           Leaf when is_map(Leaf) ->
                                                               {leaf, Leaf#{type => Type}};
                                                           {Leaf, Ret} ->
                                                               {leaf, Leaf#{type => Type}, Ret}
                                                       end;
                                                   ({leaf, L}) ->
                                                       case L of
                                                           #{type := Type} -> Existing(L);
                                                           #{type := _} -> {error, path_already_exists}
                                                       end
                                               end)).

% same as lookup, but turn directory and nothing results into errors
lookup_node(Tree, Path) ->
    case eshetsrv_tree:lookup(Tree, Path) of
        {leaf, Leaf} -> {leaf, Leaf};
        directory -> {error, path_is_directory};
        nothing -> {error, path_not_found};
        {error, E} -> {error, E}
    end.

% same as lookup_node, but also check that the node is the right type
lookup_node_type(Tree, Path, Type) ->
    case lookup_node(Tree, Path) of
        {leaf, Leaf=#{type := Type}} -> {leaf, Leaf};
        {leaf, _} -> {error, path_is_wrong_type};
        {error, E} -> {error, E}
    end.

send_state_changed(Observers, Path, State) ->
    [gen_server:cast(Observer, {state_changed, Path, State})
     || Observer <- sets:to_list(Observers)],
    ok.
