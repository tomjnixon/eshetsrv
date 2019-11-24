-module(eshetsrv_state).
-behaviour(gen_server).

%% API.
-export([start_link/0, start_link/1]).
-export([register/4]).
-export([lookup/3]).
-export([update_state/4]).

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

update_state(Srv, Path, State, Pid) ->
    gen_server:call(Srv, {update_state, Path, State, Pid}).

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

handle_call({register, state_owner, Path, Pid}, _From, State) ->
    case check(Path, Pid) of
        {error, E} -> {reply, {error, E}, State};
        {ok, Parts} ->
            update_generic(State, Parts, state, #{owner => Pid, listeners => sets:new(), state => unknown},
                           fun (L = #{owner := none}) ->
                                   {leaf, L#{owner => Pid}};
                               (#{owner := _}) ->
                                   {error, path_already_exists}
                           end)
    end;

handle_call({update_state, Path, Pid, State}, _From, State=#state{tree=Tree}) ->
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
                            #{owner := Pid} ->
                                % send out updates
                                {node, L#{state => {known, State}}};
                            #{owner := _} ->
                                {error, not_owner}
                        end;
                    ({leaf, #{type := _}}) ->
                        {error, wrong_type_of_node}
                end))
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    % {noreply, deregister(Pid, State)};
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal

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
    {reply, ok, State#state{tree=NewTree}}.

update_generic(State=#state{tree=Tree}, Parts, Type, Initial, Existing) ->
    to_reply(State,
             eshetsrv_tree:update(Tree, Parts, fun (nothing) ->
                                                       {leaf, Initial#{type => Type}};
                                                   ({leaf, L}) ->
                                                       case L of
                                                           #{type := Type} -> Existing(L);
                                                           #{type := _} -> {error, path_already_exists}
                                                       end
                                               end)).

% same as lookup, but turn durectory and nothing results into errors
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
