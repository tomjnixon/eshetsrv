-module(eshet).

-export([path_valid/1]).
-export([path_split/1, path_unsplit/1]).

-export([action_register/2, action_call/3]).
-export([prop_register/2, prop_set/3, prop_get/2]).
-export([event_register/2, event_dispatch/3, event_listen/2]).


path_valid(Path) ->
    case re:run(Path, <<"^(/\\w+)+$">>) of 
        {match, _} -> true;
        nomatch -> false
    end.


path_split(Path) ->
    binary:split(Path, <<"/">>, [trim_all, global]).


path_unsplit(Parts) ->
    list_to_binary([<<"/">> | lists:join(<<"/">>, Parts)]).


action_register(Srv, Path) ->
    eshetsrv_state:register(Srv, Path, action_owner, self()).


action_call(Srv, Path, Args) ->
    case eshetsrv_state:lookup(Srv, Path, action_owner) of
        [Owner] ->
            try
                gen_server:call(Owner, {action_call, Path, Args})
            catch
                {noproc, _} -> {error, no_such_action}
            end;
        [] ->
            {error, no_such_action}
    end.


prop_register(Srv, Path) ->
    eshetsrv_state:register(Srv, Path, prop_owner, self()).


prop_set(Srv, Path, Value) ->
    case eshetsrv_state:lookup(Srv, Path, prop_owner) of
        [Owner] ->
            try
                gen_server:call(Owner, {prop_set, Path, Value})
            catch
                {noproc, _} -> {error, no_such_prop}
            end;
        [] ->
            {error, no_such_prop}
    end.


prop_get(Srv, Path) ->
    case eshetsrv_state:lookup(Srv, Path, prop_owner) of
        [Owner] ->
            try
                gen_server:call(Owner, {prop_get, Path})
            catch
                {noproc, _} -> {error, no_such_prop}
            end;
        [] ->
            {error, no_such_prop}
    end.


event_register(Srv, Path) ->
    eshetsrv_state:register(Srv, Path, event_owner, self()).


event_dispatch(Srv, Path, Args) ->
    % XXX: check owner; check that it's actually an event...
    Listeners = eshetsrv_state:lookup(Srv, Path, event_listener),
    [gen_server:cast(Listener, {event_notify, Path, Args})
     || Listener <- Listeners].


event_listen(Srv, Path) ->
    eshetsrv_state:register(Srv, Path, event_listener, self()).
