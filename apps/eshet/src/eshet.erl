-module(eshet).

-export([link/1]).

-export([path_valid/1, path_or_dir_valid/1]).
-export([path_split/1, path_unsplit/1]).

-export([action_register/2, action_call/3]).
-export([prop_register/2]).
-export([event_register/2, event_emit/3, event_listen/2]).
-export([state_register/2, state_changed/3, state_unknown/2, state_observe/2, state_observe_t/2]).
-export([set/3, get/2]).

link(Srv) ->
    eshetsrv_state_api:link(Srv, self()).

path_valid(Path) ->
    case re:run(Path, <<"^(/\\w+)+$">>) of
        {match, _} -> true;
        nomatch -> false
    end.

path_or_dir_valid(Path) ->
    case re:run(Path, <<"^(/\\w+)*/\\w*$">>) of
        {match, _} -> true;
        nomatch -> false
    end.

path_split(Path) ->
    binary:split(Path, <<"/">>, [trim_all, global]).

path_unsplit(Parts) ->
    list_to_binary([<<"/">> | lists:join(<<"/">>, Parts)]).

action_register(Srv, Path) ->
    eshetsrv_state_api:register(Srv, action_owner, Path, self()).

action_call(Srv, Path, Args) ->
    case eshetsrv_state_api:lookup(Srv, action_owner, Path) of
        {ok, Owner} ->
            try_gen_call(Owner, {action_call, Path, Args});
        {error, E} ->
            {error, E}
    end.

prop_register(Srv, Path) ->
    eshetsrv_state_api:register(Srv, prop_owner, Path, self()).

set(Srv, Path, Value) ->
    case eshetsrv_state_api:lookup(Srv, prop_state_owner, Path) of
        {ok, prop, Owner} ->
            try_gen_call(Owner, {prop_set, Path, Value});
        {ok, state, none} ->
            {error, no_owner};
        {ok, state, Owner} ->
            try_gen_call(Owner, {state_set, Path, Value});
        {error, E} ->
            {error, E}
    end.

get(Srv, Path) ->
    case eshetsrv_state_api:lookup(Srv, prop_state_owner, Path) of
        {ok, prop, Owner} ->
            try_gen_call(Owner, {prop_get, Path});
        {ok, state, _Owner} ->
            case try_gen_call(Srv, {state_get, Path}) of
                {ok, {known, Value}} -> {ok, Value};
                {ok, unknown} -> {error, state_unknown};
                Error -> Error
            end;
        {error, E} ->
            {error, E}
    end.

event_register(Srv, Path) ->
    eshetsrv_state_api:register(Srv, event_owner, Path, self()).

event_emit(Srv, Path, Args) ->
    % XXX: check owner?
    case eshetsrv_state_api:lookup(Srv, event_listeners, Path) of
        {ok, Listeners} ->
            [
                gen_server:cast(Listener, {event_notify, Path, Args})
             || Listener <- Listeners
            ],
            ok;
        {error, E} ->
            {error, E}
    end.

event_listen(Srv, Path) ->
    eshetsrv_state_api:register(Srv, event_listener, Path, self()).

state_register(Srv, Path) ->
    eshetsrv_state_api:register(Srv, state_owner, Path, self()).

state_changed(Srv, Path, NewState) ->
    eshetsrv_state_api:state_changed(Srv, Path, self(), {known, NewState}).

state_unknown(Srv, Path) ->
    eshetsrv_state_api:state_changed(Srv, Path, self(), unknown).

state_observe(Srv, Path) ->
    eshetsrv_state_api:register(Srv, state_observer, Path, self()).

state_observe_t(Srv, Path) ->
    case eshetsrv_state_api:register(Srv, state_observer_t, Path, self()) of
        % tuple format could be ambiguous with {known, V}
        {ok, {S, T}} -> {ok, S, T};
        Error -> Error
    end.

% internal

try_gen_call(Pid, Arg, Timeout) ->
    try
        gen_server:call(Pid, Arg, Timeout)
    catch
        throw:{noproc, _} -> {error, no_such_node};
        exit:timeout -> {error, timeout};
        exit:{nodedown, _Node} -> {error, node_down};
        exit:_Reason -> {error, client_exited}
    end.

try_gen_call(Pid, Arg) -> try_gen_call(Pid, Arg, infinity).
